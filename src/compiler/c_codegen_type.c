#include "c_codegen_internal.h"

Decl *c_decl_unwrap(Decl *decl)
{
	if (!decl)
	{
		return NULL;
	}
	while (decl->replacement)
	{
		decl = decl->replacement;
	}
	while (decl->decl_kind == DECL_ALIAS)
	{
		decl = decl->define_decl.alias;
		while (decl && decl->replacement)
		{
			decl = decl->replacement;
		}
	}
	if (decl->decl_kind != DECL_VAR || decl->var.kind != VARDECL_UNWRAPPED)
	{
		return decl;
	}
	decl = decl->var.alias;
	while (decl && decl->replacement)
	{
		decl = decl->replacement;
	}
	return decl;
}

Type *c_unwrap_alias(Type *type)
{
	while (type->type_kind == TYPE_ALIAS)
	{
		ASSERT(type->canonical != type);
		type = type->canonical;
	}
	return type;
}

// The C representation of a type: type_lowering (codegen_internal.h), except that anyfault and typeid keep their pointer
// types c3fault_t / c3typeid_t where LLVM lowers them to uptr. C static initializers need them as address constants:
// `(uintptr_t)&sym` is not a constant expression in ISO C.
Type *c_safe_type_lower(Type *type)
{
	switch (type->canonical->type_kind)
	{
		case TYPE_ALIAS:
		case TYPE_POISONED:
		case TYPE_TYPEINFO:
		case TYPE_INFERRED_ARRAY:
		case TYPE_INFERRED_VECTOR:
		case TYPE_MEMBER:
		case TYPE_REFLECTION:
		case TYPE_UNTYPEDLIST:
			UNREACHABLE
		default:
			return type_lowering_ex(type, true);
	}
}

Type *c_decl_type(Decl *decl)
{
	decl = c_decl_unwrap(decl);
	ASSERT(decl->type);
	return c_safe_type_lower(decl->type);
}

Type *c_expr_type(Expr *expr)
{
	ASSERT(expr->type);
	return c_safe_type_lower(expr->type);
}

bool c_type_is_aggregate(Type *t)
{
	t = c_safe_type_lower(t);
	return t->type_kind == TYPE_STRUCT || t->type_kind == TYPE_UNION ||
	       c_type_is_vec_or_arr(t) || t->type_kind == TYPE_SLICE ||
	       t->type_kind == TYPE_FLEXIBLE_ARRAY ||
	       t->type_kind == TYPE_ANY || t->type_kind == TYPE_INTERFACE;
}

// Whether the type is a finished runtime type. compiler.context.type also holds aliases and typedefs that sema never
// resolved (unused, generic templates, or erased by @if), which have no C form.
bool c_type_is_resolved(Type *type)
{
	switch (type->type_kind)
	{
		case TYPE_ALIAS:
		{
			Decl *d = type->decl;
			if (d->decl_kind != DECL_TYPE_ALIAS || d->is_template || d->resolve_status != RESOLVE_DONE) return false;
			return c_type_is_resolved(type->canonical);
		}
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_BITSTRUCT:
		case TYPE_ENUM:
		case TYPE_CONSTSET:
		case TYPE_TYPEDEF:
		case TYPE_INTERFACE:
			return type->decl->decl_kind != DECL_ERASED && !type->decl->is_template && type->decl->resolve_status == RESOLVE_DONE;
		case TYPE_POINTER:
			return c_type_is_resolved(type->pointer);
		case TYPE_SLICE:
		case TYPE_ARRAY:
		case TYPE_VECTOR:
		case TYPE_SIMD_VECTOR:
		case TYPE_FLEXIBLE_ARRAY:
			return c_type_is_resolved(type->array.base);
		case TYPE_OPTIONAL:
			if (!type->optional || type->optional->type_kind == TYPE_VOID || type->optional->type_kind == TYPE_WILDCARD)
			{
				return false;
			}
			return c_type_is_resolved(type->optional);
		case TYPE_FUNC_PTR:
			return true;
		case TYPE_VOID:
		case TYPE_BOOL:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_ANY:
		case TYPE_ANYFAULT:
		case TYPE_TYPEID:
			return true;
		default:
			return false;
	}
}

size_t c_get_type_size(Type *type)
{
	type = c_unwrap_alias(type);
	if (!c_type_is_resolved(type))
	{
		return 0;
	}
	switch (type->type_kind)
	{
		case TYPE_VOID:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_FUNC_RAW:
		case TYPE_POISONED:
		case TYPE_WILDCARD:
		case TYPE_UNTYPEDLIST:
		case TYPE_TYPEINFO:
		case TYPE_MEMBER:
		case TYPE_REFLECTION:
		case TYPE_INFERRED_ARRAY:
		case TYPE_INFERRED_VECTOR:
			return 0;
		default:
			return (size_t)type_size(type);
	}
}

// Types whose C form is defined by the file; the scalar ones come from the runtime header.
static void c_record_used_type(GenContext *c, Type *type)
{
	switch (type->type_kind)
	{
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_SLICE:
		case TYPE_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_VECTOR:
		case TYPE_SIMD_VECTOR:
		case TYPE_FUNC_PTR:
			if (htable_get(&c->used_type_set, type)) return;
			htable_set(&c->used_type_set, type, type);
			vec_add(c->used_types, type);
			return;
		default:
			return;
	}
}

static Signature *c_fnptr_signature(Type *fnptr)
{
	Type *raw = fnptr->pointer;
	if (!raw || raw->type_kind != TYPE_FUNC_RAW) return NULL;
	if (raw->function.prototype) raw = raw->function.prototype->raw_type;
	return raw->function.signature;
}

static HTable c_fnptr_name_by_sig;
static HTable c_fnptr_text_by_name;

void c_type_names_init(void)
{
	htable_init(&c_fnptr_name_by_sig, 1024);
	htable_init(&c_fnptr_text_by_name, 1024);
}

// Hash of the C signature: deterministic, and equal signatures share a typedef.
static const char *c_fnptr_name(Signature *sig)
{
	const char *name = htable_get(&c_fnptr_name_by_sig, sig);
	if (name) return name;
	const char *text = c_signature_str(NULL, sig, "(*)", (CSigOpts){0});
	name = c_intern(str_printf("__c3_fnptr_%016" PRIx64, a5hash(text, (uint32_t)strlen(text), 0)));
	const char *known = htable_get(&c_fnptr_text_by_name, (void *)name);
	if (!known)
	{
		htable_set(&c_fnptr_text_by_name, (void *)name, (void *)text);
	}
	else if (strcmp(known, text) != 0)
	{
		error_exit("C backend: '%s' and '%s' hash to the same typedef name %s.", known, text, name);
	}
	htable_set(&c_fnptr_name_by_sig, sig, (void *)name);
	return name;
}

const char *c_type_name(GenContext *c, Type *type)
{
	type = c_safe_type_lower(type);
	// Without a file (runtime header accessors) nothing is recorded.
	if (c && c->record_types) c_record_used_type(c, type);
	switch (type->type_kind)
	{
		case TYPE_VOID: return "void";
		case TYPE_BOOL: return "bool";
		case TYPE_I8: return "int8_t";
		case TYPE_I16: return "int16_t";
		case TYPE_I32: return "int32_t";
		case TYPE_I64: return "int64_t";
		case TYPE_I128: return "__c3_int128";
		case TYPE_U8: return "uint8_t";
		case TYPE_U16: return "uint16_t";
		case TYPE_U32: return "uint32_t";
		case TYPE_U64: return "uint64_t";
		case TYPE_U128: return "__c3_uint128";
		case TYPE_F16: return "__c3_f16";
		case TYPE_BF16: return "__c3_bf16";
		case TYPE_F32: return "float";
		case TYPE_F64:
		case TYPE_F128: return "double";
		case TYPE_ANYFAULT: return "c3fault_t";
		case TYPE_TYPEID: return "c3typeid_t";
		case TYPE_ANY:
		case TYPE_INTERFACE: return "__c3_any__";
		case TYPE_TYPEDEF:
			if (type->decl && type->decl->distinct)
			{
				return c_type_name(c, type->decl->distinct->type);
			}
			return "void*";
		case TYPE_POINTER: return c_intern(str_printf("%s*", c_type_name(c, type->pointer)));
		case TYPE_STRUCT:
		case TYPE_UNION:
			ASSERT(type->decl);
			return c_get_decl_name(type->decl);
		case TYPE_BITSTRUCT:
			return c_type_name(c, type->decl->strukt.container_type->type);
		case TYPE_SLICE:
		{
			const char *base = c_type_name(c, type->array.base);
			return c_intern(str_printf("__c3_slice_%s", c_sanitize_name(base)));
		}
		case TYPE_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_VECTOR:
		case TYPE_SIMD_VECTOR:
		{
			const char *base       = c_type_name(c, type->array.base);
			unsigned long long len = type->type_kind == TYPE_FLEXIBLE_ARRAY ? 0 : (unsigned long long)type->array.len;
			if (len == 0 && type->type_kind != TYPE_FLEXIBLE_ARRAY)
			{
				len = 1;
			}
			// @simd vectors are over-aligned, so they can't share the array's definition.
			const char *kind = type->type_kind == TYPE_SIMD_VECTOR ? "simd" : "array";
			return c_intern(str_printf("__c3_%s_%s_%llu", kind, c_sanitize_name(base), len));
		}
		case TYPE_FUNC_PTR:
		{
			Signature *sig = c_fnptr_signature(type);
			if (!sig) return "__c3_fnptr_void";
			return c_fnptr_name(sig);
		}
		default: return "void*";
	}
}

const char *c_type_zero_literal(Type *t)
{
	t = c_safe_type_lower(t);
	if (c_type_is_aggregate(t) || c_type_is_int128(t) || c_type_is_half(t))
	{
		return "{0}";
	}
	if (type_is_pointer(t) || t->type_kind == TYPE_FUNC_PTR ||
	    t->type_kind == TYPE_ANYFAULT || t->type_kind == TYPE_TYPEID)
	{
		return "NULL";
	}
	if (type_is_float(t))
	{
		return "0.0";
	}
	return "0";
}

static bool c_emit_struct_or_union_forward_decl(GenContext *c, Type *type, const char *keyword)
{
	const char *name = c_type_name(c, type);
	if (htable_get(&c->gen_decl, (void *)name))
	{
		return false;
	}
	htable_set(&c->gen_decl, (void *)name, (void *)1);
	Decl *d = type->decl;
	PRINTF("typedef %s %s__ %s;\n", keyword, name, name);
	if (d && d->strukt.members)
	{
		FOREACH(Decl *, m, d->strukt.members)
		{
			if (m)
			{
				c_emit_type_forward_decl(c, m->type);
			}
		}
	}
	return true;
}

// Whether a C3 name can't be a C identifier as is: a C keyword, a lowercase macro of the C headers, or a type
// name like size_t. Such names get a trailing `_`; a macro missing here fails to compile.
bool c_name_is_reserved(const char *name)
{
	static const char *reserved[] = {
		"auto", "break", "case", "char", "const", "continue", "default", "do", "double", "else", "enum", "extern",
		"float", "for", "goto", "if", "inline", "int", "long", "register", "restrict", "return", "short", "signed",
		"sizeof", "static", "struct", "switch", "typedef", "union", "unsigned", "void", "volatile", "while", "bool",
		"true", "false", "alignas", "alignof", "constexpr", "nullptr", "static_assert", "thread_local", "typeof",
		"typeof_unqual", "asm", "errno", "stdin", "stdout", "stderr", "assert", "offsetof", "setjmp", "longjmp",
		"va_start", "va_end", "va_arg", "va_copy", "alloca", "linux", "unix", "i386", "min", "max", "main" };
	size_t len = strlen(name);
	if (len > 2 && name[len - 2] == '_' && name[len - 1] == 't') return true;
	for (size_t i = 0; i < ELEMENTLEN(reserved); i++)
	{
		if (strcmp(name, reserved[i]) == 0) return true;
	}
	return false;
}

// The C name of member `index` of `d`: its C3 name (c_name_is_reserved adds `_`), or __m<index> when anonymous.
const char *c_member_name(Decl *d, int index)
{
	const char *name = d->strukt.members[index]->name;
	if (!name) return c_intern(str_printf("__m%d", index));
	return c_name_is_reserved(name) ? c_intern(str_printf("%s_", name)) : name;
}

// The alignment C gives the C form of `type`. It is above C3's for a bitstruct with a lower `@align`, or an array of one.
AlignSize c_type_alignment(Type *type)
{
	type = c_safe_type_lower(type);
	switch (type->type_kind)
	{
		case TYPE_BITSTRUCT:
			return c_type_alignment(type->decl->strukt.container_type->type);
		case TYPE_ARRAY:
		case TYPE_VECTOR:
		case TYPE_FLEXIBLE_ARRAY:
			return c_type_alignment(type->array.base);
		default:
			return type_abi_alignment(type);
	}
}

// The alignment C gives `d` from its members alone.
static AlignSize c_struct_natural_alignment(Decl *d)
{
	AlignSize natural = 1;
	FOREACH(Decl *, m, d->strukt.members)
	{
		if (m && c_type_alignment(m->type) > natural) natural = c_type_alignment(m->type);
	}
	return natural;
}

// Whether C must lay out `d` under `#pragma pack(1)`: it has unaligned members, or is less aligned than they are.
bool c_struct_is_packed(Decl *d)
{
	return d->strukt.is_packed || d->alignment < c_struct_natural_alignment(d);
}

// Whether the C form of `type` has an alignment attribute, at any depth. On Windows `#pragma pack` can't lower that
// alignment, so a packed struct keeps such members as bytes.
static bool c_type_has_align_attr(Type *type)
{
	type = c_safe_type_lower(type);
	switch (type->type_kind)
	{
		case TYPE_I128:
		case TYPE_U128:
		case TYPE_SIMD_VECTOR:
			return true;
		case TYPE_BITSTRUCT:
			return c_type_has_align_attr(type->decl->strukt.container_type->type);
		case TYPE_ARRAY:
		case TYPE_VECTOR:
		case TYPE_FLEXIBLE_ARRAY:
			return c_type_has_align_attr(type->array.base);
		case TYPE_STRUCT:
		case TYPE_UNION:
		{
			Decl *d = type->decl;
			if (c_struct_is_packed(d)) return d->alignment > 1;
			if (d->alignment > c_struct_natural_alignment(d)) return true;
			FOREACH(Decl *, m, d->strukt.members)
			{
				if (m && (m->alignment > c_type_alignment(m->type) || c_type_has_align_attr(m->type))) return true;
			}
			return false;
		}
		default:
			return false;
	}
}

// Whether `member` of `d` is kept as bytes in C, see c_type_has_align_attr.
bool c_member_is_bytes(Decl *d, Decl *member)
{
	return c_struct_is_packed(d) && c_type_has_align_attr(member->type);
}

static bool c_emit_struct_or_union_decl(GenContext *c, Type *type, const char *keyword)
{
	const char *name = c_type_name(c, type);
	if (htable_get(&c->gen_def, (void *)name))
	{
		return false;
	}
	htable_set(&c->gen_def, (void *)name, (void *)1);

	Decl *d         = type->decl;
	int field_count = 0;
	if (d && d->strukt.members)
	{
		FOREACH(Decl *, m, d->strukt.members)
		{
			if (!m)
			{
				continue;
			}
			Type *mt = c_safe_type_lower(m->type);
			if (!mt || mt->type_kind == TYPE_VOID)
			{
				continue;
			}
			if (mt->type_kind != TYPE_POINTER)
			{
				c_emit_type_decl(c, mt);
			}
			field_count++;
		}
	}

	// C gives a struct its members' alignment and offsets; attributes can only raise them. A packed struct is
	// laid out byte by byte, with C3's gaps as explicit padding, because compilers disagree on aligned members
	// under `#pragma pack`.
	AlignSize align   = d->alignment;
	AlignSize natural = c_struct_natural_alignment(d);
	bool packed       = c_struct_is_packed(d);
	bool checked = packed || align > natural;
	if (packed) PRINT("#pragma pack(push, 1)\n");
	if (packed ? align > 1 : align > natural)
	{
		PRINTF("%s __c3_aligned(%u) %s__ {\n", keyword, align, name);
	}
	else
	{
		PRINTF("%s %s__ {\n", keyword, name);
	}
	if (field_count == 0)
	{
		PRINT("\tchar __empty;\n");
	}
	else
	{
		FOREACH_IDX(i, Decl *, m, d->strukt.members)
		{
			if (!m)
			{
				continue;
			}
			Type *mt = c_safe_type_lower(m->type);
			if (!mt || mt->type_kind == TYPE_VOID)
			{
				mt = type_char;
			}
			// C99 flexible array member: sema keeps it last and never alone.
			bool flexible     = mt->type_kind == TYPE_FLEXIBLE_ARRAY;
			const char *mtn   = c_type_name(c, flexible ? mt->array.base : mt);
			const char *dims  = flexible ? "[]" : "";
			checked |= flexible;
			if (packed && m->padding) PRINTF("\tuint8_t __pad%d[%u];\n", (int)i, m->padding);
			if (packed && c_type_has_align_attr(mt))
			{
				PRINTF("\tuint8_t %s[%s];\n", c_member_name(d, i), flexible ? "" : str_printf("%llu", (unsigned long long)type_size(mt)));
			}
			else if (!packed && m->alignment > c_type_alignment(mt))
			{
				PRINTF("\t__c3_aligned(%u) %s %s%s;\n", m->alignment, mtn, c_member_name(d, i), dims);
			}
			else
			{
				PRINTF("\t%s %s%s;\n", mtn, c_member_name(d, i), dims);
			}
		}
	}
	PRINT("};\n");
	if (packed) PRINT("#pragma pack(pop)\n");
	if (!checked) return true;
	PRINTF("__C3_ASSERT_LAYOUT(sizeof(%s) == %llu, %s_size);\n", name, (unsigned long long)type_size(type), name);
	PRINTF("__C3_ASSERT_LAYOUT(__C3_ALIGNOF(%s) == %u, %s_align);\n", name, align, name);
	FOREACH_IDX(i, Decl *, m, d->strukt.members)
	{
		if (!m) continue;
		const char *member = c_member_name(d, i);
		PRINTF("__C3_ASSERT_LAYOUT(offsetof(%s, %s) == %u, %s_%s);\n", name, member, m->offset, name, member);
	}
	return true;
}

static bool c_emit_type_forward_decl_here(GenContext *c, Type *type);

bool c_emit_type_forward_decl(GenContext *c, Type *type)
{
	ByteBuffer *prev = c_decl_stream_enter(c, c->type_decl_buffer);
	bool emitted     = c_emit_type_forward_decl_here(c, type);
	c->buffer        = prev;
	return emitted;
}

static bool c_emit_type_forward_decl_here(GenContext *c, Type *type)
{
	type = c_safe_type_lower(type);

	if (type == type_u128 || type == type_i128)
	{
		htable_set(&c->gen_decl, (void *)c_type_name(c, type), (void *)1);
		return true;
	}
	switch (type->type_kind)
	{
		case TYPE_POINTER:
			return c_emit_type_forward_decl(c, type->pointer);
		case TYPE_STRUCT:
			return c_emit_struct_or_union_forward_decl(c, type, "struct");
		case TYPE_UNION:
			return c_emit_struct_or_union_forward_decl(c, type, "union");
		case TYPE_SLICE:
		{
			const char *slice_name = c_type_name(c, type);
			if (htable_get(&c->gen_decl, (void *)slice_name))
			{
				return false;
			}
			htable_set(&c->gen_decl, (void *)slice_name, (void *)1);
			c_emit_type_forward_decl(c, type->array.base);
			const char *base_tname = c_type_name(c, type->array.base);
			PRINTF("#ifndef %s_defined__\n#define %s_defined__\n", slice_name, slice_name);
			PRINTF("typedef struct { %s* ptr; size_t len; } %s;\n", base_tname, slice_name);
			PRINT("#endif\n");
			return true;
		}
		case TYPE_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_SIMD_VECTOR:
		case TYPE_VECTOR:
		{
			const char *arr_name = c_type_name(c, type);
			if (htable_get(&c->gen_decl, (void *)arr_name))
			{
				return false;
			}
			htable_set(&c->gen_decl, (void *)arr_name, (void *)1);
			c_emit_type_forward_decl(c, type->array.base);
			PRINTF("#ifndef %s_defined__\n#define %s_defined__\n", arr_name, arr_name);
			PRINTF("typedef struct %s__ %s;\n", arr_name, arr_name);
			PRINT("#endif\n");
			return true;
		}
		case TYPE_FUNC_PTR:
		{
			const char *fnptr_name = c_type_name(c, type);
			if (htable_get(&c->gen_decl, (void *)fnptr_name))
			{
				return false;
			}
			htable_set(&c->gen_decl, (void *)fnptr_name, (void *)1);

			Signature *sig = c_fnptr_signature(type);
			const char *declarator = str_printf("(*%s)", fnptr_name);
			const char *signature  = sig ? c_signature_str(c, sig, declarator, (CSigOpts){0}) : str_printf("void %s(void)", declarator);
			PRINTF("#ifndef %s_defined__\n#define %s_defined__\n", fnptr_name, fnptr_name);
			PRINTF("typedef %s;\n#endif\n", signature);
			return true;
		}
		default:
			return false;
	}
}

bool c_emit_type_decl(GenContext *c, Type *type)
{
	type = c_safe_type_lower(type);

	switch (type->type_kind)
	{
		case TYPE_STRUCT:
			return c_emit_struct_or_union_decl(c, type, "struct");
		case TYPE_UNION:
			return c_emit_struct_or_union_decl(c, type, "union");
		case TYPE_FLEXIBLE_ARRAY:
			// Emitted as `T m[]`: only the element type needs a definition.
			return c_emit_type_decl(c, type->array.base);
		case TYPE_ARRAY:
		case TYPE_SIMD_VECTOR:
		case TYPE_VECTOR:
		{
			const char *arr_name = c_type_name(c, type);
			if (htable_get(&c->gen_def, (void *)arr_name))
			{
				return false;
			}
			htable_set(&c->gen_def, (void *)arr_name, (void *)1);

			c_emit_type_decl(c, type->array.base);
			const char *base_tname = c_type_name(c, type->array.base);
			unsigned long long len = type->array.len ? (unsigned long long)type->array.len : 1;
			AlignSize vec_align    = type_abi_alignment(type);
			if (vec_align > type_abi_alignment(type->array.base))
			{
				PRINTF("struct __c3_aligned(%u) %s__ { %s ptr[%llu]; };\n", vec_align, arr_name, base_tname, len);
			}
			else
			{
				PRINTF("struct %s__ { %s ptr[%llu]; };\n", arr_name, base_tname, len);
			}
			return true;
		}
		case TYPE_BITSTRUCT:
		{
			Decl *d = type->decl;
			if (!d)
			{
				return false;
			}
			const char *bs_name = c_get_decl_name(d);
			if (htable_get(&c->gen_def, (void *)bs_name))
			{
				return false;
			}
			htable_set(&c->gen_def, (void *)bs_name, (void *)1);
			return true;
		}
		default:
			return false;
	}
}

HTable c_needed_typeids;
Type **c_needed_typeids_vec = NULL;
bool c_recording_typeids = false;

void c_record_needed_typeid(Type *type, const char *sym)
{
	if (!c_recording_typeids || !type || !sym) return;
	if (strcmp(sym, "__c3_typeid_void") == 0) return;
	if (htable_get(&c_needed_typeids, (void *)sym)) return;
	htable_set(&c_needed_typeids, (void *)sym, (void *)1);
	vec_add(c_needed_typeids_vec, type);
}

static void c_append_decl_name(Decl *decl)
{
	scratch_buffer_append(c_get_decl_name(decl));
}

// The typeid symbol: LLVM's introspection name made a C identifier, with declarations named by
// c_get_decl_name. `$` is not portable C, so it is escaped injectively: `_` -> `__`, `$` -> `_d`,
// any other non-alphanumeric byte -> `_x` and two hex digits.
const char *c_typeid_name(Type *type)
{
	scratch_buffer_push();
	scratch_buffer_clear();
	type_mangle_introspect_name_with(type->canonical, c_append_decl_name);
	const char *mangled = scratch_buffer_copy();
	scratch_buffer_clear();
	scratch_buffer_append("__c3_typeid_");
	for (const char *p = mangled; *p; p++)
	{
		unsigned char ch = (unsigned char)*p;
		if (ch == '_')
		{
			scratch_buffer_append("__");
		}
		else if (ch == '$')
		{
			scratch_buffer_append("_d");
		}
		else if (char_is_alphanum_(ch))
		{
			scratch_buffer_append_char(ch);
		}
		else
		{
			scratch_buffer_printf("_x%02x", ch);
		}
	}
	const char *sym = scratch_buffer_interned();
	scratch_buffer_pop();
	c_record_needed_typeid(type->canonical, sym);
	return sym;
}

const char *c_typeid_ref(GenContext *c, Type *type)
{
	const char *sym = c_typeid_name(type);
	if (strcmp(sym, "__c3_typeid_void") == 0 || htable_get(&c->declared_typeids, (void *)sym)) return sym;
	ASSERT(c->decl_buffer);
	htable_set(&c->declared_typeids, (void *)sym, (void *)1);
	ByteBuffer *prev = c_decl_stream_enter(c, c->decl_buffer);
	PRINTF("extern const c3type_info_t %s;\n", sym);
	c->buffer = prev;
	return sym;
}

const char *c_fault_symbol_name(Decl *decl)
{
	if (!decl)
	{
		return "NULL";
	}
	const char *name = c_get_decl_name(decl);
	return c_intern(str_printf("__c3_fault_%s", name));
}

const char *c_fault_ref(GenContext *c, Decl *decl)
{
	const char *sym = c_fault_symbol_name(decl);
	if (htable_get(&c->declared_faults, (void *)sym)) return sym;
	ASSERT(c->decl_buffer);
	htable_set(&c->declared_faults, (void *)sym, (void *)1);
	ByteBuffer *prev = c_decl_stream_enter(c, c->decl_buffer);
	PRINTF("extern const char %s[];\n", sym);
	c->buffer = prev;
	return sym;
}

static Type *c_unwrap_target_type(Type *t)
{
	t = c_unwrap_alias(t);
	if (t && type_is_pointer(t) && t->pointer)
	{
		t = c_unwrap_alias(t->pointer);
	}
	return t;
}

Type *c_get_method_target_type(Decl *m)
{
	if (!m || m->decl_kind != DECL_FUNC)
	{
		return NULL;
	}
	if (m->func_decl.type_parent)
	{
		TypeInfo *ti = type_infoptr(m->func_decl.type_parent);
		if (ti)
		{
			return c_unwrap_target_type(ti->type);
		}
	}
	Signature *sig = &m->func_decl.signature;
	if (sig->params && vec_size(sig->params) > 0)
	{
		Decl *self_p = sig->params[0];
		if (self_p && (self_p->var.is_self || (self_p->name && strcmp(self_p->name, "self") == 0)))
		{
			return c_unwrap_target_type(self_p->type ? self_p->type : c_decl_type(self_p));
		}
	}
	return NULL;
}