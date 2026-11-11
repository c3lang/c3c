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

int c_get_type_introspection_kind(Type *type)
{
	type = c_unwrap_alias(type);
	switch (type->type_kind)
	{
		case TYPE_VOID: return INTROSPECT_TYPE_VOID;
		case TYPE_BOOL: return INTROSPECT_TYPE_BOOL;
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
		case TYPE_I128: return INTROSPECT_TYPE_SIGNED_INT;
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
		case TYPE_U128: return INTROSPECT_TYPE_UNSIGNED_INT;
		case TYPE_F16:
		case TYPE_BF16:
		case TYPE_F32:
		case TYPE_F64:
		case TYPE_F128: return INTROSPECT_TYPE_FLOAT;
		case TYPE_TYPEID: return INTROSPECT_TYPE_TYPEID;
		case TYPE_ANYFAULT: return INTROSPECT_TYPE_ANYFAULT;
		case TYPE_ANY: return INTROSPECT_TYPE_ANY;
		case TYPE_ENUM: return INTROSPECT_TYPE_ENUM;
		case TYPE_CONSTSET: return INTROSPECT_TYPE_CONSTSET;
		case TYPE_STRUCT: return INTROSPECT_TYPE_STRUCT;
		case TYPE_UNION: return INTROSPECT_TYPE_UNION;
		case TYPE_BITSTRUCT: return INTROSPECT_TYPE_BITSTRUCT;
		case TYPE_FUNC_PTR:
		case TYPE_FUNC_RAW: return INTROSPECT_TYPE_FUNC;
		case TYPE_OPTIONAL: return INTROSPECT_TYPE_OPTIONAL;
		case TYPE_ARRAY: return INTROSPECT_TYPE_ARRAY;
		case TYPE_SLICE: return INTROSPECT_TYPE_SLICE;
		case TYPE_VECTOR:
		case TYPE_SIMD_VECTOR: return INTROSPECT_TYPE_VECTOR;
		case TYPE_TYPEDEF: return INTROSPECT_TYPE_DISTINCT;
		case TYPE_POINTER: return INTROSPECT_TYPE_POINTER;
		case TYPE_INTERFACE: return INTROSPECT_TYPE_INTERFACE;
		default: return INTROSPECT_TYPE_VOID;
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

const char *c_type_name(GenContext *c, Type *type)
{
	type = c_safe_type_lower(type);
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
		case TYPE_F16:
		case TYPE_BF16:
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
		{
			Decl *d = type->decl;
			return d ? c_get_decl_name(d) : c_intern(str_printf("__c3_struct_%" PRIxPTR, (uintptr_t)type));
		}
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
			return c_intern(str_printf("__c3_array_%s_%llu", c_sanitize_name(base), len));
		}
		case TYPE_FUNC_PTR:
		{
			// Equal signatures share one interned prototype type (sema_types.c), so they get one C name.
			Type *raw = type->pointer;
			if (raw && raw->type_kind == TYPE_FUNC_RAW && raw->function.prototype) raw = raw->function.prototype->raw_type;
			return c_intern(str_printf("__c3_fnptr_%" PRIxPTR, (uintptr_t)raw));
		}
		default: return "void*";
	}
}

const char *c_type_zero_literal(Type *t)
{
	t = c_safe_type_lower(t);
	if (c_type_is_aggregate(t))
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
	if (htable_get(c->gen_decl, (void *)name))
	{
		return false;
	}
	htable_set(c->gen_decl, (void *)name, (void *)1);
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

static bool c_emit_struct_or_union_decl(GenContext *c, Type *type, const char *keyword)
{
	const char *name = c_type_name(c, type);
	if (htable_get(c->gen_def, (void *)name))
	{
		return false;
	}
	htable_set(c->gen_def, (void *)name, (void *)1);

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

	AlignSize align = d ? d->alignment : 0;
	bool packed     = d ? d->strukt.is_packed : false;
	if (packed)
	{
		PRINT("#pragma pack(push, 1)\n");
	}
	if (align)
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
			if (m->alignment && m->alignment > type_abi_alignment(mt))
			{
				PRINTF("\t__c3_aligned(%u) %s m%d;\n", m->alignment, c_type_name(c, mt), (int)i);
			}
			else
			{
				PRINTF("\t%s m%d;\n", c_type_name(c, mt), (int)i);
			}
		}
	}
	PRINT("};\n");
	if (packed)
	{
		PRINT("#pragma pack(pop)\n");
	}
	return true;
}

bool c_emit_type_forward_decl(GenContext *c, Type *type)
{
	type = c_safe_type_lower(type);

	if (type == type_u128 || type == type_i128)
	{
		htable_set(c->gen_decl, (void *)c_type_name(c, type), (void *)1);
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
			if (htable_get(c->gen_decl, (void *)slice_name))
			{
				return false;
			}
			htable_set(c->gen_decl, (void *)slice_name, (void *)1);
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
			if (htable_get(c->gen_decl, (void *)arr_name))
			{
				return false;
			}
			htable_set(c->gen_decl, (void *)arr_name, (void *)1);
			c_emit_type_forward_decl(c, type->array.base);
			PRINTF("#ifndef %s_defined__\n#define %s_defined__\n", arr_name, arr_name);
			PRINTF("typedef struct %s__ %s;\n", arr_name, arr_name);
			PRINT("#endif\n");
			return true;
		}
		case TYPE_FUNC_PTR:
		{
			const char *fnptr_name = c_type_name(c, type);
			if (htable_get(c->gen_decl, (void *)fnptr_name))
			{
				return false;
			}
			htable_set(c->gen_decl, (void *)fnptr_name, (void *)1);

			Type *raw_func = type->pointer;
			Signature *sig = (raw_func->type_kind == TYPE_FUNC_RAW) ? raw_func->function.signature : NULL;
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
		case TYPE_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_SIMD_VECTOR:
		case TYPE_VECTOR:
		{
			const char *arr_name = c_type_name(c, type);
			if (htable_get(c->gen_def, (void *)arr_name))
			{
				return false;
			}
			htable_set(c->gen_def, (void *)arr_name, (void *)1);

			c_emit_type_decl(c, type->array.base);
			const char *base_tname = c_type_name(c, type->array.base);
			unsigned long long len = type->type_kind == TYPE_FLEXIBLE_ARRAY ? 0 : (unsigned long long)type->array.len;
			if (len == 0 && type->type_kind != TYPE_FLEXIBLE_ARRAY)
			{
				len = 1;
			}
			AlignSize vec_align = type_abi_alignment(type);
			const char *ext     = (len == 0) ? "__extension__ " : "";
			if (vec_align > type_abi_alignment(type->array.base))
			{
				PRINTF("%sstruct __c3_aligned(%u) %s__ { %s ptr[%llu]; };\n", ext, vec_align, arr_name, base_tname, len);
			}
			else
			{
				PRINTF("%sstruct %s__ { %s ptr[%llu]; };\n", ext, arr_name, base_tname, len);
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
			if (htable_get(c->gen_def, (void *)bs_name))
			{
				return false;
			}
			htable_set(c->gen_def, (void *)bs_name, (void *)1);
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

// The typeid symbol: LLVM's introspection name (type_mangle_introspect_name_to_buffer) made a C identifier. That name
// uses `$` as separator, which C does not allow portably, so it is escaped injectively: `_` -> `__`, `$` -> `_d`, any
// other non-alphanumeric byte -> `_x` and two hex digits.
const char *c_typeid_name(Type *type)
{
	scratch_buffer_push();
	scratch_buffer_clear();
	type_mangle_introspect_name_to_buffer(type->canonical);
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

const char *c_fault_symbol_name(Decl *decl)
{
	if (!decl)
	{
		return "NULL";
	}
	const char *name = c_get_decl_name(decl);
	return c_intern(str_printf("__c3_fault_%s", name));
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