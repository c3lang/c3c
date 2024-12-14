// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"

static Type *flatten_raw_function_type(Type *type);

static struct
{
	Type u0, u1, i8, i16, i32, i64, i128;
	Type u8, u16, u32, u64, u128;
	Type bf16, f16, f32, f64, f128;
	Type usz, isz, uptr, iptr;
	Type string;
	Type voidstar, typeid, anyfault, member, typeinfo, untyped_list;
	Type any, wildcard;
} t;

Type *type_bool = &t.u1;
Type *type_void = &t.u0;
Type *type_voidptr = &t.voidstar;
Type *type_float16 = &t.f16;
Type *type_bfloat = &t.bf16;
Type *type_float = &t.f32;
Type *type_double = &t.f64;
Type *type_f128 = &t.f128;
Type *type_typeid = &t.typeid;
Type *type_any = &t.any;
Type *type_typeinfo = &t.typeinfo;
Type *type_ichar = &t.i8;
Type *type_short = &t.i16;
Type *type_int = &t.i32;
Type *type_long = &t.i64;
Type *type_i128 = &t.i128;
Type *type_iptr = &t.iptr;
Type *type_isz = &t.isz;
Type *type_char = &t.u8;
Type *type_ushort = &t.u16;
Type *type_uint = &t.u32;
Type *type_ulong = &t.u64;
Type *type_u128 = &t.u128;
Type *type_uptr = &t.uptr;
Type *type_usz = &t.usz;
Type *type_anyfault = &t.anyfault;
Type *type_untypedlist = &t.untyped_list;
Type *type_wildcard = &t.wildcard;
Type *type_member = &t.member;
Type *type_chars = NULL;
Type *type_wildcard_optional = NULL;
Type *type_string = &t.string;
Type *type_reflected_param = NULL;
Type *type_cint;
Type *type_cuint;

static unsigned size_slice;
static AlignSize alignment_slice;
static AlignSize max_alignment_vector;

#define PTR_OFFSET 0
#define INFERRED_ARRAY_OFFSET 1
#define FLEXIBLE_ARRAY_OFFSET 2
#define SLICE_OFFSET 3
#define INFERRED_VECTOR_OFFSET 4
#define OPTIONAL_OFFSET 5
#define ARRAY_OFFSET 6

static void type_append_func_to_scratch(FunctionPrototype *prototype);

void type_init_cint(void)
{
	type_cint = type_int_signed_by_bitsize(compiler.platform.width_c_int);
	type_cuint = type_int_unsigned_by_bitsize(compiler.platform.width_c_int);
}

Type *type_int_signed_by_bitsize(BitSize bitsize)
{
	switch (bitsize)
	{
		case 8: return type_ichar;
		case 16: return type_short;
		case 32: return type_int;
		case 64: return type_long;
		case 128: return type_i128;
		default: FATAL_ERROR("Illegal bitsize %d", bitsize);
	}
}
Type *type_int_unsigned_by_bitsize(BitSize bit_size)
{
	switch (bit_size)
	{
		case 8: return type_char;
		case 16: return type_ushort;
		case 32: return type_uint;
		case 64: return type_ulong;
		case 128: return type_u128;
		default: FATAL_ERROR("Illegal bitsize");
	}
}

const char *type_quoted_error_string(Type *type)
{
	if (type->canonical != type)
	{
		return str_printf("'%s' (%s)", type_to_error_string(type), type_to_error_string(type->canonical));
	}
	return str_printf("'%s'", type_to_error_string(type));
}

void type_append_name_to_scratch(Type *type)
{
	type = type->canonical;
	switch (type->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_TYPEDEF:
			UNREACHABLE;
		case TYPE_FAULTTYPE:
		case TYPE_ENUM:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_DISTINCT:
		case TYPE_BITSTRUCT:
		case TYPE_INTERFACE:
			scratch_buffer_append(type->decl->name);
			break;
		case TYPE_POINTER:
			type_append_name_to_scratch(type->pointer);
			scratch_buffer_append_char('*');
			break;
		case TYPE_OPTIONAL:
			if (type->optional)
			{
				type_append_name_to_scratch(type->optional);
			}
			else
			{
				scratch_buffer_append("void");
			}
			scratch_buffer_append_char('!');
			break;
		case TYPE_SLICE:
			type_append_name_to_scratch(type->array.base);
			scratch_buffer_append("[]");
			break;
		case TYPE_FLEXIBLE_ARRAY:
			type_append_name_to_scratch(type->array.base);
			scratch_buffer_append("[*]");
			break;
		case TYPE_VOID:
		case TYPE_BOOL:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_TYPEID:
		case TYPE_ANYFAULT:
		case TYPE_ANY:
		case TYPE_VECTOR:
			scratch_buffer_append(type->name);
			break;
		case TYPE_UNTYPED_LIST:
		case TYPE_INFERRED_ARRAY:
		case TYPE_INFERRED_VECTOR:
		case TYPE_TYPEINFO:
		case TYPE_MEMBER:
		case TYPE_WILDCARD:
			UNREACHABLE
			break;
		case TYPE_FUNC_PTR:
			type = type->pointer;
			FALLTHROUGH;
		case TYPE_FUNC_RAW:
			type_append_func_to_scratch(type->function.prototype);
			break;
		case TYPE_ARRAY:
			type_append_name_to_scratch(type->array.base);
			scratch_buffer_append_char('[');
			scratch_buffer_append_signed_int(type->array.len);
			scratch_buffer_append_char(']');
			break;
	}
}

static void type_append_func_to_scratch(FunctionPrototype *prototype)
{
	type_append_name_to_scratch(prototype->rtype);
	scratch_buffer_append_char('(');
	unsigned elements = vec_size(prototype->param_types);
	for (unsigned i = 0; i < elements; i++)
	{
		if (i > 0)
		{
			scratch_buffer_append_char(',');
		}
		type_append_name_to_scratch(prototype->param_types[i]);
	}
	if (prototype->raw_variadic && elements > 0)
	{
		scratch_buffer_append_char(',');
	}
	scratch_buffer_append_char(')');
}

bool type_is_inner_type(Type *type)
{
	switch (type->type_kind)
	{
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_BITSTRUCT:
			return type->decl->strukt.parent != 0;
		default:
			return false;
	}
}

static void type_add_parent_to_scratch(Decl *decl)
{
	switch (decl->decl_kind)
	{
		case DECL_STRUCT:
		case DECL_UNION:
		case DECL_BITSTRUCT:
			if (decl->strukt.parent)
			{
				Decl *parent = declptr(decl->strukt.parent);
				type_add_parent_to_scratch(parent);
				if (!parent->name) return;
				scratch_buffer_append(parent->name);
				scratch_buffer_append_char('.');
			}
			return;
		default:
			return;
	}
}
const char *type_to_error_string(Type *type)
{
	switch (type->type_kind)
	{
		case TYPE_POISONED:
			return "poisoned";
		case TYPE_VOID:
		case TYPE_BOOL:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_ANYFAULT:
		case TYPE_UNTYPED_LIST:
		case TYPE_ANY:
		case TYPE_MEMBER:
		case TYPE_WILDCARD:
			return type->name;
		case TYPE_ENUM:
		case TYPE_FAULTTYPE:
		case TYPE_TYPEDEF:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_DISTINCT:
		case TYPE_BITSTRUCT:
		case TYPE_INTERFACE:
		{
			Decl *decl = type->decl;
			const char *suffix = decl->unit->module->generic_suffix;
			if (!suffix && !type_is_inner_type(type)) return type->name;
			scratch_buffer_clear();
			type_add_parent_to_scratch(decl);
			scratch_buffer_append(decl->name);
			if (suffix) scratch_buffer_append(suffix);
			return scratch_buffer_copy();
		}
		case TYPE_FUNC_PTR:
			type = type->pointer;
			FALLTHROUGH;
		case TYPE_FUNC_RAW:
			if (!type->function.prototype) return type->name;
			scratch_buffer_clear();
			scratch_buffer_append("fn ");
			type_append_func_to_scratch(type->function.prototype);
			return scratch_buffer_copy();
		case TYPE_INFERRED_VECTOR:
			return str_printf("%s[<*>]", type_to_error_string(type->array.base));
		case TYPE_VECTOR:
			return str_printf("%s[<%llu>]", type_to_error_string(type->array.base), (unsigned long long)type->array.len);
		case TYPE_TYPEINFO:
			return "typeinfo";
		case TYPE_TYPEID:
			return "typeid";
		case TYPE_POINTER:
			return str_printf("%s*", type_to_error_string(type->pointer));
		case TYPE_OPTIONAL:
			if (!type->optional) return "void!";
			return str_printf("%s!", type_to_error_string(type->optional));
		case TYPE_ARRAY:
			return str_printf("%s[%llu]", type_to_error_string(type->array.base), (unsigned long long)type->array.len);
		case TYPE_INFERRED_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
			return str_printf("%s[*]", type_to_error_string(type->array.base));
		case TYPE_SLICE:
			return str_printf("%s[]", type_to_error_string(type->array.base));
	}
	UNREACHABLE
}


bool type_is_matching_int(CanonicalType *type1, CanonicalType *type2)
{
	ASSERT0(type1->canonical == type1 && type2->canonical == type2);
	TypeKind typekind1 = type1->type_kind;
	TypeKind typekind2 = type2->type_kind;
	if (typekind1 == typekind2) return type_kind_is_any_integer(typekind1);
	if (type_kind_is_signed(typekind1)) return typekind1 + (TYPE_U8 - TYPE_I8) == typekind2;
	if (type_kind_is_unsigned(typekind1)) return typekind2 + (TYPE_U8 - TYPE_I8) == typekind1;
	return false;
}

TypeSize type_size(Type *type)
{
RETRY:
	switch (type->type_kind)
	{
		case TYPE_BITSTRUCT:
			ASSERT0(type->decl->resolve_status == RESOLVE_DONE);
			type = type->decl->strukt.container_type->type;
			goto RETRY;
		case TYPE_DISTINCT:
			ASSERT0(type->decl->resolve_status == RESOLVE_DONE);
			type = type->decl->distinct->type;
			goto RETRY;
		case TYPE_VECTOR:
		{
			TypeSize width = type_size(type->array.base) * type->array.len;
			if (!is_power_of_two(width)) return next_highest_power_of_2(width);
			return width;
		}
		case CT_TYPES:
		case TYPE_FUNC_RAW:
			UNREACHABLE;
		case TYPE_FLEXIBLE_ARRAY:
			return 0;
		case TYPE_OPTIONAL:
			type = type->optional;
			goto RETRY;
		case TYPE_TYPEDEF:
			type = type->canonical;
			goto RETRY;
		case TYPE_FAULTTYPE:
			type = type_iptr->canonical;
			goto RETRY;
		case TYPE_ENUM:
			ASSERT0(type->decl->enums.type_info->resolve_status == RESOLVE_DONE);
			type = type->decl->enums.type_info->type->canonical;
			goto RETRY;
		case TYPE_STRUCT:
		case TYPE_UNION:
			ASSERT0(type->decl->resolve_status == RESOLVE_DONE);
			return type->decl->strukt.size;
		case TYPE_VOID:
			return 1;
		case TYPE_BOOL:
		case TYPE_TYPEID:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_ANYFAULT:
			return type->builtin.bytesize;
		case TYPE_INTERFACE:
		case TYPE_ANY:
			return t.iptr.canonical->builtin.bytesize * 2;
		case TYPE_FUNC_PTR:
		case TYPE_POINTER:
			return t.iptr.canonical->builtin.bytesize;
		case TYPE_ARRAY:
			return type_size(type->array.base) * type->array.len;
		case TYPE_SLICE:
			return size_slice;
	}
	UNREACHABLE
}

FunctionPrototype *type_get_resolved_prototype(Type *type)
{
	ASSERT0(type->type_kind == TYPE_FUNC_RAW);
	FunctionPrototype *prototype = type->function.prototype;
	if (!prototype->is_resolved) c_abi_func_create(prototype);
	return prototype;
}

bool type_flat_is_numlike(Type *type)
{
	type = type_flatten(type);
	if (type->type_kind == TYPE_VECTOR) type = type->array.base;
	TypeKind kind = type->type_kind;
	return kind >= TYPE_NUM_FIRST && kind <= TYPE_NUM_LAST;
}

bool type_flat_is_floatlike(Type *type)
{
	type = type_flatten(type);
	if (type->type_kind == TYPE_VECTOR) type = type->array.base;
	TypeKind kind = type->type_kind;
	return kind >= TYPE_FLOAT_FIRST && kind <= TYPE_FLOAT_LAST;
}

bool type_flat_is_intlike(Type *type)
{
	type = type_flatten(type);
	if (type->type_kind == TYPE_VECTOR) type = type->array.base;
	TypeKind kind = type->type_kind;
	return kind >= TYPE_INTEGER_FIRST && kind <= TYPE_INTEGER_LAST;
}

bool type_flat_is_boolintlike(Type *type)
{
	type = type_flatten(type);
	if (type->type_kind == TYPE_VECTOR) type = type->array.base;
	TypeKind kind = type->type_kind;
	return kind == TYPE_BOOL || (kind >= TYPE_INTEGER_FIRST && kind <= TYPE_INTEGER_LAST);
}


bool type_is_int128(Type *type)
{
	TypeKind kind = type->canonical->type_kind;
	return kind == TYPE_U128 || kind == TYPE_I128;
}

bool type_is_abi_aggregate(Type *type)
{
	RETRY:
	switch (type->type_kind)
	{
		case TYPE_OPTIONAL:
			type = type->optional;
			goto RETRY;
		case TYPE_DISTINCT:
			type = type->decl->distinct->type;
			goto RETRY;
		case TYPE_TYPEDEF:
			type = type->canonical;
			goto RETRY;
		case CT_TYPES:
		case TYPE_FLEXIBLE_ARRAY:
			return false;
		case TYPE_BITSTRUCT:
		case ALL_FLOATS:
		case TYPE_VOID:
		case ALL_INTS:
		case TYPE_BOOL:
		case TYPE_TYPEID:
		case TYPE_POINTER:
		case TYPE_ENUM:
		case TYPE_FUNC_PTR:
		case TYPE_FUNC_RAW:
		case TYPE_VECTOR:
		case TYPE_ANYFAULT:
		case TYPE_FAULTTYPE:
			return false;
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_SLICE:
		case TYPE_ARRAY:
		case TYPE_ANY:
		case TYPE_INTERFACE:
			return true;
	}
	UNREACHABLE
}

Type *type_find_largest_union_element(Type *type)
{
	ASSERT0(type->type_kind == TYPE_UNION);
	ByteSize largest = 0;
	Type *largest_type = NULL;
	FOREACH(Decl *, member, type->decl->strukt.members)
	{
		Type *member_type = member->type;
		if (type_size(member_type) > largest)
		{
			largest = type_size(member_type);
			largest_type = type->canonical;
		}
	}
	return largest_type;
}

bool type_is_ordered(Type *type)
{
	RETRY:
	switch (type->type_kind)
	{
		case ALL_FLOATS:
		case ALL_INTS:
		case TYPE_POINTER:
		case TYPE_BOOL:
		case TYPE_ENUM:
			return true;
		case TYPE_TYPEDEF:
			type = type->canonical;
			goto RETRY;
		case TYPE_DISTINCT:
			type = type->decl->distinct->type;
			goto RETRY;
		default:
			return false;
	}
}

bool type_is_comparable(Type *type)
{
	RETRY:
	switch (type->type_kind)
	{
		case TYPE_INFERRED_VECTOR:
		case TYPE_INFERRED_ARRAY:
		case TYPE_POISONED:
			UNREACHABLE
		case TYPE_VOID:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_OPTIONAL:
		case TYPE_MEMBER:
			return false;
		case TYPE_UNION:
		case TYPE_STRUCT:
			return type->decl->attr_compact;
		case TYPE_BITSTRUCT:
			type = type->decl->strukt.container_type->type;
			goto RETRY;
		case TYPE_TYPEDEF:
			type = type->canonical;
			goto RETRY;
		case TYPE_SLICE:
		case TYPE_ARRAY:
			// Arrays are comparable if elements are
			type = type->array.base;
			goto RETRY;
		case TYPE_DISTINCT:
			type = type->decl->distinct->type;
			goto RETRY;
		case TYPE_BOOL:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_ANY:
		case TYPE_INTERFACE:
		case TYPE_ANYFAULT:
		case TYPE_TYPEID:
		case TYPE_POINTER:
		case TYPE_ENUM:
		case TYPE_FUNC_PTR:
		case TYPE_FUNC_RAW:
		case TYPE_FAULTTYPE:
		case TYPE_UNTYPED_LIST:
		case TYPE_TYPEINFO:
		case TYPE_VECTOR:
		case TYPE_WILDCARD:
			return true;
	}
	UNREACHABLE
}

void type_mangle_introspect_name_to_buffer(Type *type)
{
	switch (type->type_kind)
	{
		case CT_TYPES:
			UNREACHABLE
		case TYPE_ANY:
			scratch_buffer_append("any$");
			return;
		case TYPE_VOID:
		case TYPE_BOOL:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_ANYFAULT:
		case TYPE_TYPEID:
			scratch_buffer_append(type->name);
			return;
		case TYPE_POINTER:
			scratch_buffer_append("p$");
			type_mangle_introspect_name_to_buffer(type->pointer);
			return;
		case TYPE_SLICE:
			scratch_buffer_append("sa$");
			type_mangle_introspect_name_to_buffer(type->array.base);
			return;
		case TYPE_FLEXIBLE_ARRAY:
			scratch_buffer_append("a0$");
			type_mangle_introspect_name_to_buffer(type->array.base);
			return;
		case TYPE_OPTIONAL:
			scratch_buffer_append("f$");
			type_mangle_introspect_name_to_buffer(type->optional);
			return;
		case TYPE_VECTOR:
			scratch_buffer_append_char('v');
			scratch_buffer_append_unsigned_int(type->array.len);
			scratch_buffer_append_char('$');
			type_mangle_introspect_name_to_buffer(type->array.base);
			return;
		case TYPE_ARRAY:
			scratch_buffer_append_char('a');
			scratch_buffer_append_unsigned_int(type->array.len);
			scratch_buffer_append_char('$');
			type_mangle_introspect_name_to_buffer(type->array.base);
			return;
		case TYPE_FUNC_PTR:
			type = type->pointer;
			FALLTHROUGH;
		case TYPE_FUNC_RAW:
			type = type->function.prototype->raw_type;
			if (type->function.decl)
			{
				scratch_buffer_append_module(type->function.decl->unit->module, true);
				scratch_buffer_append("$");
				scratch_buffer_append(type->name);
			}
			else
			{
				size_t len = strlen(type->name);
				for (size_t i = 0; i < len; i++)
				{
					char c = type->name[i];
					if (char_is_alphanum_(c))
					{
						scratch_buffer_append_char(c);
						continue;
					}
					if (c == '$')
					{
						scratch_buffer_append("$$");
						continue;
					}
					else
					{
						scratch_buffer_append_char('$');
					}
				}
			}
			return;
		case TYPE_ENUM:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_BITSTRUCT:
		case TYPE_FAULTTYPE:
		case TYPE_DISTINCT:
		case TYPE_INTERFACE:
			scratch_buffer_set_extern_decl_name(type->decl, false);
			return;
		case TYPE_TYPEDEF:
			type_mangle_introspect_name_to_buffer(type->canonical);
			return;
	}
	UNREACHABLE
}

bool type_func_match(Type *fn_type, Type *rtype, unsigned arg_count, ...)
{
	ASSERT0(type_is_func_ptr(fn_type));
	Signature *sig = fn_type->pointer->function.signature;
	if (rtype->canonical != typeget(sig->rtype)->canonical) return false;
	if (vec_size(sig->params) != arg_count) return false;
	va_list ap;
	va_start(ap, arg_count);
	FOREACH(Decl *, decl, sig->params)
	{
		Type *arg = va_arg(ap, Type*);
		if (decl->type->canonical != arg->canonical)
		{
			va_end(ap);
			return false;
		}
	}
	va_end(ap);
	return true;
}

AlignSize type_abi_alignment(Type *type)
{
	RETRY:
	switch (type->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_TYPEINFO:
		case TYPE_UNTYPED_LIST:
		case TYPE_MEMBER:
		case TYPE_WILDCARD:
			UNREACHABLE;
		case TYPE_BITSTRUCT:
			type = type->decl->strukt.container_type->type;
			goto RETRY;
		case TYPE_INFERRED_VECTOR:
		case TYPE_VECTOR:
		{
			ArraySize len = type->array.len;
			if (!len) len = 1;
			ByteSize width = type_size(type->array.base) * len;
			AlignSize alignment = (AlignSize)(int32_t)width;
			if (max_alignment_vector && alignment > max_alignment_vector) return max_alignment_vector;
			if (!is_power_of_two(alignment))
			{
				alignment = (AlignSize)next_highest_power_of_2((uint32_t)alignment);
			}
			return alignment;
		}
		case TYPE_VOID:
			return 1;
		case TYPE_OPTIONAL:
			type = type->optional;
			goto RETRY;
		case TYPE_DISTINCT:
			type = type->decl->distinct->type;
			goto RETRY;
		case TYPE_TYPEDEF:
			type = type->canonical;
			goto RETRY;
		case TYPE_ENUM:
			type = type->decl->enums.type_info->type->canonical;
			goto RETRY;
		case TYPE_STRUCT:
		case TYPE_UNION:
			ASSERT0(type->decl->resolve_status == RESOLVE_DONE);
			return type->decl->alignment;
			UNREACHABLE
		case TYPE_BOOL:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_ANYFAULT:
			return type->builtin.abi_alignment;
		case TYPE_FUNC_RAW:
		case TYPE_FUNC_PTR:
		case TYPE_INTERFACE:
		case TYPE_ANY:
		case TYPE_POINTER:
		case TYPE_TYPEID:
		case TYPE_FAULTTYPE:
			return t.iptr.canonical->builtin.abi_alignment;
		case TYPE_ARRAY:
		case TYPE_INFERRED_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
			type = type->array.base;
			goto RETRY;
		case TYPE_SLICE:
			return alignment_slice;
	}
	UNREACHABLE
}

static inline void create_type_cache(Type *type)
{
	ASSERT0(type->type_cache == NULL);
	for (int i = 0; i < ARRAY_OFFSET; i++)
	{
		vec_add(type->type_cache, NULL);
	}
}

static Type *type_generate_ptr(Type *ptr_type, bool canonical)
{
	if (canonical) ptr_type = ptr_type->canonical;
	if (!ptr_type->type_cache)
	{
		create_type_cache(ptr_type);
	}

	Type *ptr = ptr_type->type_cache[PTR_OFFSET];
	if (ptr == NULL)
	{
		ptr = type_new(TYPE_POINTER, str_printf("%s*", ptr_type->name));
		ptr->pointer = ptr_type;
		ptr_type->type_cache[PTR_OFFSET] = ptr;
		if (ptr_type == ptr_type->canonical)
		{
			ptr->canonical = ptr;
		}
		else
		{
			ptr->canonical = type_generate_ptr(ptr_type->canonical, true);
		}
	}
	return ptr;
}

static Type *type_generate_optional(Type *optional_type, bool canonical)
{
	if (canonical) optional_type = optional_type->canonical;
	if (!optional_type->type_cache)
	{
		create_type_cache(optional_type);
	}
	Type *optional = optional_type->type_cache[OPTIONAL_OFFSET];
	if (optional == NULL)
	{
		optional = type_new(TYPE_OPTIONAL, str_printf("%s!", optional_type->name));
		optional->pointer = optional_type;
		optional_type->type_cache[OPTIONAL_OFFSET] = optional;
		if (optional_type == optional_type->canonical)
		{
			optional->canonical = optional;
		}
		else
		{
			optional->canonical = type_generate_optional(optional_type->canonical, true);
		}
	}
	return optional;
}

static Type *type_generate_slice(Type *arr_type, bool canonical)
{
	if (canonical) arr_type = arr_type->canonical;
	if (!arr_type->type_cache)
	{
		create_type_cache(arr_type);
	}

	Type *arr = arr_type->type_cache[SLICE_OFFSET];
	if (arr == NULL)
	{
		arr = type_new(TYPE_SLICE, str_printf("%s[]", arr_type->name));
		arr->array.base = arr_type;
		arr_type->type_cache[SLICE_OFFSET] = arr;
		if (arr_type == arr_type->canonical)
		{
			arr->canonical = arr;
		}
		else
		{
			arr->canonical = type_generate_slice(arr_type->canonical, true);
		}
	}
	return arr;
}

static Type *type_generate_inferred_array(Type *arr_type, bool canonical)
{
	if (canonical) arr_type = arr_type->canonical;
	if (!arr_type->type_cache)
	{
		create_type_cache(arr_type);
	}

	Type *arr = arr_type->type_cache[INFERRED_ARRAY_OFFSET];
	if (arr == NULL)
	{
		arr = type_new(TYPE_INFERRED_ARRAY, str_printf("%s[*]", arr_type->name));
		arr->array.base = arr_type;
		arr_type->type_cache[INFERRED_ARRAY_OFFSET] = arr;
		if (arr_type == arr_type->canonical)
		{
			arr->canonical = arr;
		}
		else
		{
			arr->canonical = type_generate_inferred_array(arr_type->canonical, true);
		}
	}
	return arr;
}

static Type *type_generate_inferred_vector(Type *arr_type, bool canonical)
{
	if (canonical) arr_type = arr_type->canonical;
	if (!arr_type->type_cache)
	{
		create_type_cache(arr_type);
	}

	Type *arr = arr_type->type_cache[INFERRED_VECTOR_OFFSET];
	if (arr == NULL)
	{
		arr = type_new(TYPE_INFERRED_VECTOR, str_printf("%s[<*>]", arr_type->name));
		arr->array.base = arr_type;
		arr_type->type_cache[INFERRED_VECTOR_OFFSET] = arr;
		if (arr_type == arr_type->canonical)
		{
			arr->canonical = arr;
		}
		else
		{
			arr->canonical = type_generate_inferred_vector(arr_type->canonical, true);
		}
	}
	return arr;
}

static Type *type_generate_flexible_array(Type *arr_type, bool canonical)
{
	if (canonical) arr_type = arr_type->canonical;
	if (!arr_type->type_cache)
	{
		create_type_cache(arr_type);
	}

	Type *arr = arr_type->type_cache[FLEXIBLE_ARRAY_OFFSET];
	if (arr == NULL)
	{
		arr = type_new(TYPE_FLEXIBLE_ARRAY, str_printf("%s[*]", arr_type->name));
		arr->array.base = arr_type;
		arr->array.len = 0;
		arr_type->type_cache[FLEXIBLE_ARRAY_OFFSET] = arr;
		if (arr_type == arr_type->canonical)
		{
			arr->canonical = arr;
		}
		else
		{
			arr->canonical = type_generate_flexible_array(arr_type->canonical, true);
		}
	}
	return arr;
}



Type *type_get_ptr_recurse(Type *ptr_type)
{
	if (ptr_type->type_kind == TYPE_OPTIONAL)
	{
		ptr_type = ptr_type->optional;
		return type_get_optional(type_get_ptr(ptr_type));
	}
	return type_get_ptr(ptr_type);

}
Type *type_get_ptr(Type *ptr_type)
{
	ASSERT0(ptr_type->type_kind != TYPE_FUNC_RAW);
	ASSERT0(!type_is_optional(ptr_type));
	return type_generate_ptr(ptr_type, false);
}

Type *type_get_func_ptr(Type *func_type)
{
	ASSERT0(func_type->type_kind == TYPE_FUNC_RAW);
	if (func_type->func_ptr) return func_type->func_ptr;
	Type *type = func_type->func_ptr = type_new(TYPE_FUNC_PTR, func_type->name);
	type->pointer = func_type;
	type->canonical = type;
	return type;
}

Type *type_get_optional(Type *optional_type)
{
	ASSERT0(!type_is_optional(optional_type));
	return type_generate_optional(optional_type, false);
}

Type *type_get_slice(Type *arr_type)
{
	ASSERT0(type_is_valid_for_array(arr_type));
	return type_generate_slice(arr_type, false);
}

Type *type_get_inferred_array(Type *arr_type)
{
	ASSERT0(type_is_valid_for_array(arr_type));
	return type_generate_inferred_array(arr_type, false);
}

Type *type_get_inferred_vector(Type *arr_type)
{
	ASSERT0(type_is_valid_for_array(arr_type));
	return type_generate_inferred_vector(arr_type, false);
}

Type *type_get_flexible_array(Type *arr_type)
{
	ASSERT0(type_is_valid_for_array(arr_type));
	return type_generate_flexible_array(arr_type, false);
}


static inline bool array_structurally_equivalent_to_struct(Type *array, Type *type)
{
	ASSERT0(array->type_kind == TYPE_ARRAY);

	ArrayIndex len = (ArrayIndex)array->array.len;
	if (!len) return type_size(type) == 0;

	Type *base = array->array.base;

	if (len == 1 && type_is_structurally_equivalent(base, type)) return true;

	ASSERT0(type->type_kind != TYPE_UNION && "Does not work on unions");

	if (!type_is_union_or_strukt(type)) return false;

	Decl **members = type->decl->strukt.members;

	// For structs / errors, all members must match.
	ArrayIndex offset = 0;
	AlignSize align_size = type_abi_alignment(array);
	Type *array_base = array->array.base;
	FOREACH(Decl *, member, members)
	{
		if (!type_is_structurally_equivalent(array_base, member->type)) return false;
		if (member->offset != offset) return false;
		offset += align_size;
	}
	return true;
}

bool type_is_structurally_equivalent(Type *type1, Type *type2)
{
	type1 = type_flatten(type1);
	type2 = type_flatten(type2);

	if (type1 == type2) return true;

	// If the other type is a union, we check against every member
	// noting that there is only structural equivalence if it fills out the
	if (type2->type_kind == TYPE_UNION)
	{
		// If any member is structurally equivalent, then
		// the cast is valid.
		FOREACH(Decl *, member, type2->decl->strukt.members)
		{
			if (type_is_structurally_equivalent(type1, member->type)) return true;
		}
		// In this case we can't get a match.
		return false;
	}

	if (type1->type_kind == TYPE_ARRAY)
	{
		return array_structurally_equivalent_to_struct(type1, type2);
	}

	if (type2->type_kind == TYPE_ARRAY)
	{
		return array_structurally_equivalent_to_struct(type2, type1);
	}

	if (!type_is_union_or_strukt(type1)) return false;

	Decl **members = type1->decl->strukt.members;
	if (type1->type_kind == TYPE_UNION)
	{
		// If any member is structurally equivalent, then
		// the cast is valid.
		FOREACH(Decl *, member, members)
		{
			if (type_is_structurally_equivalent(member->type, type2)) return true;
		}
		return false;
	}

	// The only thing we have left is to check against another structlike.
	if (!type_is_union_or_strukt(type2)) return false;

	Decl **other_members = type2->decl->strukt.members;

	// For structs / errors, all members must match.
	FOREACH_IDX(i, Decl *, member, members)
	{
		if (!type_is_structurally_equivalent(member->type, other_members[i]->type)) return false;
		if (member->offset != other_members[i]->offset) return false;
	}

	return true;
}

bool type_is_user_defined(Type *type)
{
	switch (type->type_kind)
	{
		case TYPE_ENUM:
		case TYPE_FUNC_RAW:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_FAULTTYPE:
		case TYPE_DISTINCT:
		case TYPE_BITSTRUCT:
		case TYPE_TYPEDEF:
		case TYPE_INTERFACE:
			return true;
		default:
			return false;
	}
}

Type *type_get_indexed_type(Type *type)
{
	RETRY:
	if (type == type_voidptr) return NULL;
	switch (type->type_kind)
	{
		case TYPE_POINTER:
			return type->pointer->canonical;
		case TYPE_ARRAY:
		case TYPE_SLICE:
		case TYPE_INFERRED_ARRAY:
		case TYPE_INFERRED_VECTOR:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_VECTOR:
			return type->array.base->canonical;
		case TYPE_DISTINCT:
			type = type->decl->distinct->type;
			goto RETRY;
		case TYPE_OPTIONAL:
			type = type->optional;
			goto RETRY;
		case TYPE_TYPEDEF:
			type = type->canonical;
			goto RETRY;
		default:
			return NULL;
	}
}

static Type *type_create_array(Type *element_type, ArraySize len, bool vector, bool canonical)
{
	if (canonical) element_type = element_type->canonical;
	if (!element_type->type_cache)
	{
		create_type_cache(element_type);
	}
	int entries = (int)vec_size(element_type->type_cache);
	for (int i = ARRAY_OFFSET; i < entries; i++)
	{
		Type *ptr_vec = element_type->type_cache[i];
		if (vector)
		{
			if (ptr_vec->type_kind != TYPE_VECTOR) continue;
			if (ptr_vec->array.len == len) return ptr_vec;
		}
		else
		{
			if (ptr_vec->type_kind == TYPE_VECTOR) continue;
			if (ptr_vec->array.len == len) return ptr_vec;
		}
	}
	Type *vec_arr;
	if (vector)
	{
		vec_arr = type_new(TYPE_VECTOR, str_printf("%s[<%u>]", element_type->name, len));
		vec_arr->array.base = element_type;
		vec_arr->array.len = len;
	}
	else
	{
		vec_arr = type_new(TYPE_ARRAY, str_printf("%s[%u]", element_type->name, len));
		vec_arr->array.base = element_type;
		vec_arr->array.len = len;
	}
	if (element_type->canonical == element_type)
	{
		vec_arr->canonical = vec_arr;
	}
	else
	{
		vec_arr->canonical = type_create_array(element_type, len, vector, true);
	}
	vec_add(element_type->type_cache, vec_arr);
	return vec_arr;
}

Type *type_get_array(Type *arr_type, ArraySize len)
{
	ASSERT(len > 0, "Created a zero length array");
	ASSERT0(type_is_valid_for_array(arr_type));
	return type_create_array(arr_type, len, false, false);
}

bool type_is_valid_for_vector(Type *type)
{
	RETRY:
	switch (type->type_kind)
	{
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_BOOL:
		case TYPE_POINTER:
		case TYPE_ENUM:
		case TYPE_TYPEID:
		case TYPE_FAULTTYPE:
		case TYPE_ANYFAULT:
			return true;
		case TYPE_DISTINCT:
			ASSERT0(type->decl->resolve_status == RESOLVE_DONE);
			type = type->decl->distinct->type;
			goto RETRY;
		case TYPE_TYPEDEF:
			type = type->canonical;
			goto RETRY;
		default:
			return false;
	}
}

bool type_is_valid_for_array(Type *type)
{
	RETRY:
	switch (type->type_kind)
	{
		case TYPE_DISTINCT:
			ASSERT0(type->decl->resolve_status == RESOLVE_DONE);
			type = type->decl->distinct->type;
			goto RETRY;
		case TYPE_ANY:
		case TYPE_INTERFACE:
		case TYPE_ANYFAULT:
		case TYPE_TYPEID:
		case TYPE_POINTER:
		case TYPE_ENUM:
		case TYPE_FUNC_PTR:
		case TYPE_FUNC_RAW:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_BITSTRUCT:
		case TYPE_FAULTTYPE:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_BOOL:
		case TYPE_ARRAY:
		case TYPE_SLICE:
		case TYPE_VECTOR:
			return true;
		case TYPE_TYPEDEF:
			ASSERT0(type->decl->resolve_status == RESOLVE_DONE);
			type = type->canonical;
			goto RETRY;
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_INFERRED_ARRAY:
		case TYPE_INFERRED_VECTOR:
			type = type->array.base;
			goto RETRY;
		case TYPE_UNTYPED_LIST:
		case TYPE_OPTIONAL:
		case TYPE_WILDCARD:
		case TYPE_TYPEINFO:
		case TYPE_MEMBER:
		case TYPE_POISONED:
		case TYPE_VOID:
			return false;
	}
	UNREACHABLE
}

Type *type_get_vector_bool(Type *original_type)
{
	Type *type = type_flatten(original_type);
	ByteSize size = type_size(type->array.base);
	return type_get_vector(type_int_signed_by_bitsize((unsigned)size * 8), (unsigned)original_type->array.len);
}

Type *type_get_vector(Type *vector_type, unsigned len)
{
	ASSERT0(type_is_valid_for_vector(vector_type));
	return type_create_array(vector_type, len, true, false);
}

static void type_create(const char *name, Type *location, TypeKind kind, unsigned bitsize,
						unsigned align, unsigned pref_align)
{
	ASSERT0(align);
	unsigned byte_size = (bitsize + 7) / 8;
	*location = (Type) {
		.type_kind = kind,
		.builtin.bytesize = byte_size,
		.builtin.bitsize = bitsize,
		.builtin.abi_alignment = align,
		.builtin.pref_alignment = pref_align ? pref_align : align,
		.name = name,
		.canonical = location,
	};
	location->name = name;
	location->canonical = location;
	global_context_add_type(location);
}

static void type_init(const char *name, Type *location, TypeKind kind, unsigned bitsize, AlignData align)
{
	ASSERT0(align.align);
	unsigned byte_size = (bitsize + 7) / 8;
	*location = (Type) {
		.type_kind = kind,
		.builtin.bytesize = byte_size,
		.builtin.bitsize = bitsize,
		.builtin.abi_alignment = align.align / 8,
		.builtin.pref_alignment = (align.pref_align ? align.pref_align : align.align) / 8,
		.name = name,
		.canonical = location,
		};
	location->name = name;
	location->canonical = location;
	global_context_add_type(location);
}

static void type_create_alias(const char *name, Type *location, Type *canonical)
{
	Decl *decl = decl_new(DECL_TYPEDEF, name, INVALID_SPAN);
	decl->resolve_status = RESOLVE_DONE;
	decl->typedef_decl.type_info = type_info_new_base(canonical, INVALID_SPAN);
	decl->unit = compiler.context.core_unit;
	decl->is_export = true;
	*location = (Type) {
		.decl = decl,
		.type_kind = TYPE_TYPEDEF,
		.name = name,
		.canonical = canonical
	};
	decl->type = location;
	global_context_add_type(location);
}


Type *type_new_func(Decl *decl, Signature *sig)
{
	Type *type = type_new(TYPE_FUNC_RAW, decl->name);
	type->canonical = type;
	type->function.signature = sig;
	type->function.decl = decl;
	return type;
}

static inline void type_init_int(const char *name, Type *type, TypeKind kind, BitSizes bits)
{
	unsigned actual_bits = bits ? (unsigned int)(8 << (bits - 1)) : 1;
	type_init(name, type, kind, actual_bits, compiler.platform.integers[bits]);
}

static inline void type_create_float(const char *name, Type *type, TypeKind kind, BitSizes bits)
{
	unsigned actual_bits = bits ? (unsigned int)(8 << (bits - 1)) : 1;
	type_init(name, type, kind, actual_bits, compiler.platform.floats[bits]);
}

Type *type_create_struct(const char *name, Type **types, const char **names, int count)
{
	Decl *decl = decl_new_with_type(symtab_preset(name, TOKEN_TYPE_IDENT), INVALID_SPAN, DECL_STRUCT);
	decl->unit = compiler.context.core_unit;
	decl->extname = decl->name;
	AlignSize offset = 0;
	AlignSize max_align = 0;
	for (int i = 0; i < count; i++)
	{
		Type *member_type = types[i];
		Decl *member = decl_new_var(symtab_preset(names[i], TOKEN_IDENT), INVALID_SPAN, type_info_new_base(member_type, INVALID_SPAN), VARDECL_MEMBER);
		member->unit = compiler.context.core_unit;
		member->type = member_type;
		member->resolve_status = RESOLVE_DONE;
		AlignSize align = type_abi_alignment(member_type);
		if (align > max_align) max_align = align;
		member->offset = aligned_offset(offset, align);
		offset = member->offset + type_size(member_type);
		member->alignment = align;
		vec_add(decl->strukt.members, member);
	}
	decl->strukt.size = aligned_offset(offset, max_align);
	decl->alignment = max_align;
	decl->resolve_status = RESOLVE_DONE;
	global_context_add_type(decl->type);
	global_context_add_decl(decl);
	return decl->type;
}
void type_setup(PlatformTarget *target)
{
	max_alignment_vector = (AlignSize)target->align_max_vector;

	type_create_float("float16", &t.f16, TYPE_F16, BITS16);
	type_create_float("bfloat", &t.bf16, TYPE_BF16, BITS16);
	type_create_float("float", &t.f32, TYPE_F32, BITS32);
	type_create_float("double", &t.f64, TYPE_F64, BITS64);
	type_create_float("float128", &t.f128, TYPE_F128, BITS128);


	type_init_int("ichar", &t.i8, TYPE_I8, BITS8);
	type_init_int("short", &t.i16, TYPE_I16, BITS16);
	type_init_int("int", &t.i32, TYPE_I32, BITS32);
	type_init_int("long", &t.i64, TYPE_I64, BITS64);
	type_init_int("int128", &t.i128, TYPE_I128, BITS128);

	type_init_int("bool", &t.u1, TYPE_BOOL, BITS8);
	type_init_int("char", &t.u8, TYPE_U8, BITS8);
	type_init_int("ushort", &t.u16, TYPE_U16, BITS16);
	type_init_int("uint", &t.u32, TYPE_U32, BITS32);
	type_init_int("ulong", &t.u64, TYPE_U64, BITS64);
	type_init_int("uint128", &t.u128, TYPE_U128, BITS128);

	type_init_int("void", &t.u0, TYPE_VOID, BITS8);

	type_create("typeinfo", &t.typeinfo, TYPE_TYPEINFO, 1, 1, 1);
	type_create("member_ref", &t.member, TYPE_MEMBER, 1, 1, 1);
	type_create("untyped_list", &t.untyped_list, TYPE_UNTYPED_LIST, 1, 1, 1);
	type_create("void", &t.wildcard, TYPE_WILDCARD, 1, 1, 1);
	type_init("typeid", &t.typeid, TYPE_TYPEID, target->width_pointer, target->align_pointer);
	type_init("void*", &t.voidstar, TYPE_POINTER, target->width_pointer, target->align_pointer);
	create_type_cache(type_void);
	type_void->type_cache[0] = &t.voidstar;
	t.voidstar.pointer = type_void;
	type_create("any", &t.any, TYPE_ANY, target->width_pointer * 2, target->align_pointer.align, target->align_pointer.pref_align);

	type_create_alias("usz", &t.usz, type_int_unsigned_by_bitsize(target->width_pointer));
	type_create_alias("isz", &t.isz, type_int_signed_by_bitsize(target->width_pointer));
	type_create_alias("uptr", &t.uptr, type_int_unsigned_by_bitsize(target->width_pointer));
	type_create_alias("iptr", &t.iptr, type_int_signed_by_bitsize(target->width_pointer));

	alignment_slice = MAX(type_abi_alignment(&t.voidstar), type_abi_alignment(t.usz.canonical));
	size_slice = (unsigned)(alignment_slice * 2);
	type_init("anyfault", &t.anyfault, TYPE_ANYFAULT, target->width_pointer, target->align_pointer);
	type_chars = type_get_slice(type_char);
	type_wildcard_optional = type_get_optional(type_wildcard);
	Decl *string_decl = decl_new_with_type(symtab_preset("String", TOKEN_TYPE_IDENT), INVALID_SPAN, DECL_DISTINCT);
	string_decl->unit = compiler.context.core_unit;
	string_decl->extname = string_decl->name;
	string_decl->is_substruct = true;
	string_decl->distinct = type_info_new_base(type_chars, INVALID_SPAN);
	string_decl->resolve_status = RESOLVE_DONE;
	type_string = string_decl->type;

	global_context_add_type(string_decl->type);
	global_context_add_decl(string_decl);

	Type* types[2] = { type_string, type_typeid };
	const char* names[2] = { "name", "type" };
	type_reflected_param = type_create_struct("ReflectedParam", types, names, 2);
}

int type_kind_bitsize(TypeKind kind)
{
	switch (kind)
	{
		case TYPE_I8:
		case TYPE_U8:
			return 8;
		case TYPE_I16:
		case TYPE_U16:
		case TYPE_F16:
		case TYPE_BF16:
			return 16;
		case TYPE_I32:
		case TYPE_U32:
		case TYPE_F32:
			return 32;
		case TYPE_I64:
		case TYPE_U64:
		case TYPE_F64:
			return 64;
		case TYPE_I128:
		case TYPE_U128:
		case TYPE_F128:
			return 128;
		default:
			UNREACHABLE;
	}
}
bool type_is_scalar(Type *type)
{
	RETRY:
	switch (type->type_kind)
	{
		case CT_TYPES:
			UNREACHABLE
		case TYPE_VOID:
		case TYPE_FUNC_RAW:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_ARRAY:
		case TYPE_SLICE:
		case TYPE_VECTOR:
		case TYPE_INTERFACE:
		case TYPE_ANY:
		case TYPE_FLEXIBLE_ARRAY:
			return false;
		case TYPE_BOOL:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_TYPEID:
		case TYPE_POINTER:
		case TYPE_FUNC_PTR:
		case TYPE_ENUM:
		case TYPE_FAULTTYPE:
		case TYPE_ANYFAULT:
			return true;
		case TYPE_BITSTRUCT:
			type = type->decl->strukt.container_type->type;
			goto RETRY;
		case TYPE_DISTINCT:
			type = type->decl->distinct->type;
			goto RETRY;
		case TYPE_OPTIONAL:
			type = type->optional;
			if (!type) return false;
			goto RETRY;
		case TYPE_TYPEDEF:
			type = type->canonical;
			goto RETRY;
	}
	UNREACHABLE
}

Type *type_find_parent_type(Type *type)
{
	ASSERT0(type->canonical);
	switch (type->type_kind)
	{
		case TYPE_DISTINCT:
		{
			Decl *decl = type->decl;
			return decl->is_substruct ? decl->distinct->type : NULL;
		}
		case TYPE_STRUCT:
		{
			Decl *decl = type->decl;
			return decl->is_substruct ? decl->strukt.members[0]->type : NULL;
		}
		default:
			return NULL;
	}
}
/**
 * Check if a type is contained in another type.
 *
 * @param type canonical type
 * @param possible_subtype is this a subtype of the former, i.e. type is the parent.
 * @return true if it is a subtype
 */
bool type_is_subtype(Type *type, Type *possible_subtype)
{
	ASSERT0(type == type->canonical);
	while (possible_subtype)
	{
		possible_subtype = possible_subtype->canonical;
		if (type == possible_subtype) return true;
		possible_subtype = type_find_parent_type(possible_subtype);
	}
	return false;
}

Type *type_from_token(TokenType type)
{
	switch (type)
	{
		case TOKEN_ANY:
			return type_any;
		case TOKEN_ANYFAULT:
			return type_anyfault;
		case TOKEN_VOID:
			return type_void;
		case TOKEN_BOOL:
			return type_bool;
		case TOKEN_CHAR:
			return type_char;
		case TOKEN_FLOAT16:
			return type_float16;
		case TOKEN_BFLOAT:
			return type_bfloat;
		case TOKEN_DOUBLE:
			return type_double;
		case TOKEN_FLOAT:
			return type_float;
		case TOKEN_FLOAT128:
			return type_f128;
		case TOKEN_INT128:
			return type_i128;
		case TOKEN_ICHAR:
			return type_ichar;
		case TOKEN_INT:
			return type_int;
		case TOKEN_IPTR:
			return type_iptr;
		case TOKEN_ISZ:
			return type_isz;
		case TOKEN_LONG:
			return type_long;
		case TOKEN_SHORT:
			return type_short;
		case TOKEN_UINT128:
			return type_u128;
		case TOKEN_UINT:
			return type_uint;
		case TOKEN_ULONG:
			return type_ulong;
		case TOKEN_UPTR:
			return type_uptr;
		case TOKEN_USHORT:
			return type_ushort;
		case TOKEN_USZ:
			return type_usz;
		case TOKEN_TYPEID:
			return type_typeid;
		default:
			UNREACHABLE
	}
}

static TypeCmpResult type_array_is_equivalent(SemaContext *context, Type *from, Type *to, bool is_explicit)
{
	TypeKind to_kind = to->type_kind;
	switch (from->type_kind)
	{
		case TYPE_INFERRED_ARRAY:
			ASSERT0(to_kind != TYPE_INFERRED_ARRAY);
			if (to_kind != TYPE_ARRAY) return TYPE_MISMATCH;
			return type_array_element_is_equivalent(context, from->array.base, to->array.base, is_explicit);
		case TYPE_ARRAY:
			if (to_kind != TYPE_ARRAY && to_kind != TYPE_INFERRED_ARRAY) return TYPE_MISMATCH;
			if (to->type_kind == TYPE_ARRAY && from->array.len != to->array.len) return TYPE_MISMATCH;
			return type_array_element_is_equivalent(context, from->array.base, to->array.base, is_explicit);
		case TYPE_INFERRED_VECTOR:
			ASSERT0(to_kind != TYPE_INFERRED_VECTOR);
			if (to->type_kind != TYPE_VECTOR) return TYPE_MISMATCH;
			return type_array_element_is_equivalent(context, from->array.base, to->array.base, is_explicit);
		case TYPE_VECTOR:
			if (to_kind != TYPE_VECTOR && to_kind != TYPE_INFERRED_VECTOR) return TYPE_MISMATCH;
			if (to->type_kind == TYPE_VECTOR && from->array.len != to->array.len) return TYPE_MISMATCH;
			return type_array_element_is_equivalent(context, from->array.base, to->array.base, is_explicit);
		default:
			return TYPE_MISMATCH;
	}
}


TypeCmpResult type_array_element_is_equivalent(SemaContext *context, Type *element1, Type *element2, bool is_explicit)
{
	if (is_explicit)
	{
		element1 = type_flatten(element1);
		element2 = type_flatten(element2);
	}
	else
	{
		element1 = element1->canonical;
		element2 = element2->canonical;
	}
	if (element1 == element2) return TYPE_SAME;
	if ((type_is_void(element1) && element2 == type_char) || (element1 == type_char && type_is_void(element2)))
	{
		return TYPE_SAME;
	}
	switch (element1->type_kind)
	{
		case TYPE_FUNC_PTR:
			if (element2 == type_voidptr) return TYPE_SAME;
			if (element1->type_kind != TYPE_FUNC_PTR) return TYPE_MISMATCH;
			if (element1->pointer->function.prototype->raw_type == element2->pointer->function.prototype->raw_type) return TYPE_SAME;
			return TYPE_MISMATCH;
		case TYPE_POINTER:
			if (element2->type_kind == TYPE_FUNC_PTR && type_voidptr == element1) return TYPE_SAME;
			if (!type_is_pointer(element2)) return TYPE_MISMATCH;
			return type_is_pointer_equivalent(context, element1, element2, is_explicit);
		case TYPE_STRUCT:
			if (is_explicit) return type_is_structurally_equivalent(element1, element2) ? TYPE_SAME : TYPE_MISMATCH;
			return TYPE_MISMATCH;
		case TYPE_VECTOR:
		case TYPE_ARRAY:
		case TYPE_INFERRED_ARRAY:
		case TYPE_INFERRED_VECTOR:
			return type_array_is_equivalent(context, element1, element2, is_explicit);
		case ALL_INTS:
			if (type_is_matching_int(element1, element2)) return TYPE_SAME_INT_SIZE;
			FALLTHROUGH;
		default:
			return TYPE_MISMATCH;
	}
}

TypeCmpResult type_is_pointer_equivalent(SemaContext *context, Type *to_pointer, Type *from_pointer, bool flatten_distinct)
{
RETRY:
	if (flatten_distinct)
	{
		to_pointer = type_flatten(to_pointer);
		from_pointer = type_flatten(from_pointer);
	}
	// Are they the same?
	if (to_pointer == from_pointer) return TYPE_SAME;

	// Is one of them a void*?
	if (to_pointer == type_voidptr || from_pointer == type_voidptr) return TYPE_SAME;

	// Look at the pointees.
	Type *to_pointee = to_pointer->pointer->canonical;
	Type *from_pointee = from_pointer->pointer->canonical;
	if (flatten_distinct)
	{
		to_pointee = type_flatten(to_pointee);
		from_pointee = type_flatten(from_pointee);
	}

	// Are pointees same after flattening?
	if (to_pointee == from_pointee) return TYPE_SAME;
	if (type_is_subtype(to_pointee, from_pointee)) return TYPE_SAME;

	if (type_is_matching_int(to_pointee, from_pointee)) return TYPE_SAME_INT_SIZE;

	if (type_is_any_arraylike(from_pointee))
	{
		TypeCmpResult res_current = TYPE_SAME;
		if (type_abi_alignment(to_pointee) > type_abi_alignment(from_pointee)) res_current = TYPE_ALIGNMENT_INCREASE;

		// Try array equivalence.
		if (type_is_any_arraylike(to_pointee))
		{
			TypeCmpResult res = type_array_is_equivalent(context, from_pointee, to_pointee, flatten_distinct);
			if (res != TYPE_MISMATCH) return res == TYPE_SAME ? res_current : res;
			if (to_pointee->array.len == from_pointee->array.len && to_pointee->array.base->canonical == from_pointee->array.base->canonical)
			{
				return res_current;
			}
		}
		// A possible int[4]* -> int* decay?
		TypeCmpResult res = type_is_pointer_equivalent(context, to_pointer, type_get_ptr(from_pointee->array.base), flatten_distinct);
		if (res == TYPE_SAME) return res_current;
	}

	if (to_pointee->type_kind != from_pointee->type_kind) return TYPE_MISMATCH;

	if (to_pointee->type_kind == TYPE_FUNC_RAW && from_pointee->type_kind == TYPE_FUNC_RAW)
	{
		if (!sema_resolve_type_decl(context, to_pointee)) return TYPE_ERROR;
		if (!sema_resolve_type_decl(context, from_pointee)) return TYPE_ERROR;
		return to_pointee->function.prototype->raw_type == from_pointee->function.prototype->raw_type;
	}
	if (to_pointee->type_kind == TYPE_POINTER || to_pointee->type_kind == TYPE_FUNC_PTR)
	{
		to_pointer = to_pointee;
		from_pointer = from_pointee;
		goto RETRY;
	}
	return false;
}

bool type_may_have_method(Type *type)
{
	DECL_TYPE_KIND_REAL(kind, type)
	switch (kind)
	{
		case TYPE_DISTINCT:
		case TYPE_UNION:
		case TYPE_STRUCT:
		case TYPE_ENUM:
		case TYPE_FAULTTYPE:
		case TYPE_BITSTRUCT:
		case ALL_FLOATS:
		case ALL_INTS:
		case TYPE_ANY:
		case TYPE_ANYFAULT:
		case TYPE_TYPEID:
		case TYPE_ARRAY:
		case TYPE_SLICE:
		case TYPE_INFERRED_ARRAY:
		case TYPE_INFERRED_VECTOR:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_VECTOR:
		case TYPE_BOOL:
		case TYPE_INTERFACE:
			return true;
		case TYPE_TYPEDEF:
			UNREACHABLE
		case TYPE_POINTER:
			return type == type_voidptr;
		case TYPE_POISONED:
		case TYPE_VOID:
		case TYPE_FUNC_PTR:
		case TYPE_FUNC_RAW:
		case TYPE_UNTYPED_LIST:
		case TYPE_OPTIONAL:
		case TYPE_TYPEINFO:
		case TYPE_MEMBER:
		case TYPE_WILDCARD:
			return false;
	}
	UNREACHABLE
}

bool type_may_have_sub_elements(Type *type)
{
	DECL_TYPE_KIND_REAL(kind, type)
	switch (kind)
	{
		case TYPE_DISTINCT:
		case TYPE_UNION:
		case TYPE_STRUCT:
		case TYPE_ENUM:
		case TYPE_FAULTTYPE:
		case TYPE_BITSTRUCT:
		case TYPE_INTERFACE:
			return true;
		default:
			return false;
	}
}

Type *type_find_max_num_type(Type *num_type, Type *other_num)
{
	TypeKind kind = num_type->type_kind;
	TypeKind other_kind = other_num->type_kind;
	ASSERT0(kind <= other_kind && "Expected ordering");
	ASSERT0(kind != other_kind);

	// 1. The only conversions need to happen if the other type is a number.
	if (other_kind < TYPE_INTEGER_FIRST || other_kind > TYPE_FLOAT_LAST) return NULL;

	// 2. First check the float case.
	if (other_kind >= TYPE_FLOAT_FIRST && other_kind <= TYPE_FLOAT_LAST)
	{
		switch (other_kind)
		{
			case TYPE_BF16:
			case TYPE_F16:
			case TYPE_F32:
			case TYPE_F64:
			case TYPE_F128:
				// Pick the biggest, which will be in other_num due to ordering.
				return other_num;
			default:
				UNREACHABLE
		}
	}

	// Handle integer <=> integer conversions.
	ASSERT0(type_kind_is_any_integer(other_kind) && type_is_integer(num_type));

	// 4. Check the bit sizes.
	unsigned other_bit_size = other_num->builtin.bitsize;
	unsigned bit_size = num_type->builtin.bitsize;

	// 5. The other type is unsigned
	if (type_kind_is_unsigned(other_kind))
	{
		if (type_kind_is_signed(kind))
		{
			// 5a. Signed + Unsigned -> Signed
			return bit_size >= other_bit_size ? num_type : type_int_signed_by_bitsize(other_bit_size);
		}
		// 5b. Unsigned + Unsigned -> return other_num which is the bigger due to ordering.
		return other_num;
	}

	// 6. The other type is signed, then pick other_num which is bigger due to ordering.
	return other_num;
}

/**
 * max(Foo[:], Bar*)  -> max(Foo*, Bar*)
 * max(Foo[], Bar*)   -> max(Foo*, Bar*)
 * max(Foo[n]*, Bar*) -> max(Foo*, Bar*)
 * max(void*, Foo*)   -> void*
 * max(Foo*, Bar*)    -> max(Foo, Bar)*
 * max(other, Foo*)   -> NULL
 *
 * @param type
 * @param other
 * @return the max pointer type or NULL if none can be found.
 */
static inline Type *type_find_max_ptr_type(Type *type, Type *other)
{
	// Slice and vararray can implicitly convert to a pointer.
	if (other->type_kind == TYPE_SLICE)
	{
		Type *max_type = type_find_max_type(type->pointer, other->pointer);
		if (!max_type) return NULL;
		return type_get_ptr(max_type);
	}

	// Neither slice, vararray nor pointer? Then no max
	if (other->type_kind != TYPE_POINTER) return NULL;

	Type* other_pointer_type = other->pointer;
	Type* pointer_type = type->pointer;

	// Reorder if needed
	if (other_pointer_type->type_kind < pointer_type->type_kind)
	{
		pointer_type = other_pointer_type;
		other_pointer_type = type->pointer;
	}

	// void * is always max.
	if (type_is_void(pointer_type)) return type_voidptr;

	if (pointer_type->type_kind == TYPE_POINTER && other_pointer_type->type_kind == TYPE_ARRAY)
	{
		// Decay foo[n]* to foo*
		other_pointer_type = type_get_ptr(other_pointer_type->array.base);
	}
	if (type_is_subtype(pointer_type->canonical, other_pointer_type->canonical))
	{
		return type;
	}
	if (type_is_subtype(other_pointer_type->canonical, pointer_type->canonical))
	{
		return other;
	}
	Type *max_type = type_find_max_type(pointer_type, other_pointer_type);
	if (!max_type) return NULL;
	return type_get_ptr(max_type);
}


Type *type_decay_array_pointer(Type *type)
{
	ASSERT0(type->type_kind == TYPE_POINTER);
	Type *ptr = type->pointer;
	switch (ptr->type_kind)
	{
		case TYPE_ARRAY:
		case TYPE_VECTOR:
			return type_get_ptr(ptr->array.base->canonical);
		default:
			return type;
	}
}
#define MAX_SEARCH_DEPTH 512
static inline Type *type_find_max_distinct_type(Type *left, Type *right)
{
	ASSERT0(left == left->canonical && right == right->canonical);
	ASSERT0(left != right);
	ASSERT0(left->type_kind == TYPE_DISTINCT && right->type_kind == TYPE_DISTINCT);
	static Type *left_types[MAX_SEARCH_DEPTH];
	int depth = 0;
	while (depth < MAX_SEARCH_DEPTH)
	{
		left_types[depth++] = left;
		if (left->type_kind != TYPE_DISTINCT || !left->decl->is_substruct) break;
		left = left->decl->distinct->type;
		if (left == right) return right;
	}
	if (depth == MAX_SEARCH_DEPTH)
	{
		error_exit("Common ancestor search depth %d exceeded.", MAX_SEARCH_DEPTH);
	}
	ASSERT0(left != right);
	while (true)
	{
		for (int i = 0; i < depth; i++)
		{
			if (right == left_types[i]) return right;
		}
		if (right->type_kind != TYPE_DISTINCT || !right->decl->is_substruct) return NULL;
		right = right->decl->distinct->type;
		if (left == right) return right;
	}
}

Type *type_find_max_type(Type *type, Type *other)
{
	type = type->canonical;
	other = other->canonical;
	ASSERT0(!type_is_optional(type) && !type_is_optional(other));

RETRY_DISTINCT:
	if (type == other) return type;

	if (type == type_wildcard) return other;
	if (other == type_wildcard) return type;

	// Sort types
	if (type->type_kind > other->type_kind)
	{
		Type *temp = type;
		type = other;
		other = temp;
	}

	// The following relies on type kind ordering
	switch (type->type_kind)
	{
		case TYPE_INFERRED_ARRAY:
		case TYPE_INFERRED_VECTOR:
		case TYPE_POISONED:
		case TYPE_OPTIONAL:
		case TYPE_WILDCARD:
			UNREACHABLE
		case TYPE_VOID:
		case TYPE_BOOL:
		case TYPE_TYPEINFO:
		case TYPE_BITSTRUCT:
		case TYPE_FLEXIBLE_ARRAY:
			return NULL;
		case ALL_INTS:
			if (other->type_kind == TYPE_DISTINCT && type_underlying_is_numeric(other)) return other;
			if (other->type_kind == TYPE_ENUM) return type_find_max_type(type, other->decl->enums.type_info->type->canonical);
			if (other->type_kind == TYPE_VECTOR) return other;
			return type_find_max_num_type(type, other);
		case ALL_FLOATS:
			if (other->type_kind == TYPE_DISTINCT && type_is_float(type_flatten(other))) return other;
			if (other->type_kind == TYPE_VECTOR) return other;
			return type_find_max_num_type(type, other);
		case TYPE_ANY:
			// any + interface => any
			if (other == type_voidptr) return other;
			return other->type_kind == TYPE_INTERFACE ? type : NULL;
		case TYPE_INTERFACE:
			// interface + void* => void*
			return other == type_voidptr ? type_voidptr : NULL;
		case TYPE_POINTER:
			if (type->pointer->type_kind == TYPE_ARRAY)
			{
				Type *array_base = type->pointer->array.base->canonical;
				if (other->type_kind == TYPE_SLICE &&
					array_base == other->array.base->canonical)
				{
					return other;
				}
				if (other->type_kind == TYPE_POINTER)
				{
					Type *other_pointer = other->pointer;
					if (other_pointer->type_kind == TYPE_ARRAY && other_pointer->array.base->canonical == array_base)
					{
						return type_get_slice(array_base);
					}
				}
			}
			if (type->pointer->type_kind == TYPE_VECTOR)
			{
				Type *vector_base = type->pointer->array.base->canonical;
				if (other->type_kind == TYPE_SLICE && vector_base == other->array.base->canonical)
				{
					return other;
				}
			}
			// We need to decay the pointer
			type = type_decay_array_pointer(type);
			// And possibly the other pointer as well
			if (other->type_kind == TYPE_POINTER) other = type_decay_array_pointer(other);
			return type_find_max_ptr_type(type, other);
		case TYPE_ENUM:
			// IMPROVE: should there be implicit conversion between one enum and the other in
			// some way?
			return NULL;
		case TYPE_FAULTTYPE:
			if (other->type_kind == TYPE_FAULTTYPE) return type_anyfault;
			return NULL;
		case TYPE_ANYFAULT:
			return type_anyfault;
		case TYPE_FUNC_PTR:
			if (other == type_voidptr) return other;
			if (other->type_kind != TYPE_FUNC_PTR) return NULL;
			if (other->pointer->function.prototype->raw_type != type->pointer->function.prototype->raw_type) return NULL;
			return type;
		case TYPE_FUNC_RAW:
			UNREACHABLE
		case TYPE_UNTYPED_LIST:
			if (other->type_kind == TYPE_ARRAY) return other;
			if (other->type_kind == TYPE_VECTOR) return other;
			if (other->type_kind == TYPE_STRUCT) return other;
			if (other->type_kind == TYPE_SLICE) return other;
			return NULL;
		case TYPE_UNION:
		case TYPE_STRUCT:
			// union/struct + anything else => no
			// even if the struct has an inline type, this should not
			// be implicit
			return NULL;
		case TYPE_TYPEID:
		case TYPE_MEMBER:
			return NULL;
		case TYPE_TYPEDEF:
			UNREACHABLE
		case TYPE_SLICE:
			// slice + [array, vector of the same type] => yes
			if (type_is_arraylike(other) && (other->array.base->canonical == type->array.base->canonical))
			{
				return type;
			}
			// otherwise no
			return NULL;
		case TYPE_ARRAY:
			// array + [other array, vector] => no
			return NULL;
		case TYPE_VECTOR:
			// No implicit conversion between vectors
			return NULL;
		case TYPE_DISTINCT:
			if (other->type_kind == TYPE_DISTINCT)
			{
				return type_find_max_distinct_type(type, other);
			}
			// Try matching with its inline type
			if (type->decl->is_substruct)
			{
				type = type->decl->distinct->type;
				goto RETRY_DISTINCT;
			}
			// distinct + any other type => no
			return NULL;
	}
	UNREACHABLE
}


unsigned type_get_introspection_kind(TypeKind kind)
{
	switch (kind)
	{
		case TYPE_POISONED:
			return 0;
		case TYPE_VOID:
			return INTROSPECT_TYPE_VOID;
		case TYPE_BOOL:
			return INTROSPECT_TYPE_BOOL;
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
		case TYPE_I128:
			return INTROSPECT_TYPE_SIGNED_INT;
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
		case TYPE_U128:
			return INTROSPECT_TYPE_UNSIGNED_INT;
		case TYPE_F16:
		case TYPE_BF16:
		case TYPE_F32:
		case TYPE_F64:
		case TYPE_F128:
			return INTROSPECT_TYPE_FLOAT;
		case TYPE_ANY:
			return INTROSPECT_TYPE_ANY;
		case TYPE_ANYFAULT:
			return INTROSPECT_TYPE_ANYFAULT;
		case TYPE_INTERFACE:
			return INTROSPECT_TYPE_INTERFACE;
		case TYPE_TYPEID:
			return INTROSPECT_TYPE_TYPEID;
		case TYPE_POINTER:
			return INTROSPECT_TYPE_POINTER;
		case TYPE_ENUM:
			return INTROSPECT_TYPE_ENUM;
		case TYPE_FUNC_PTR:
			return INTROSPECT_TYPE_FUNC;
		case TYPE_STRUCT:
			return INTROSPECT_TYPE_STRUCT;
		case TYPE_UNION:
			return INTROSPECT_TYPE_UNION;
		case TYPE_BITSTRUCT:
			return INTROSPECT_TYPE_BITSTRUCT;
		case TYPE_FAULTTYPE:
			return INTROSPECT_TYPE_FAULT;
		case TYPE_FUNC_RAW:
		case TYPE_TYPEDEF:
			UNREACHABLE
		case TYPE_DISTINCT:
			return INTROSPECT_TYPE_DISTINCT;
		case TYPE_ARRAY:
		case TYPE_INFERRED_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
			return INTROSPECT_TYPE_ARRAY;
		case TYPE_SLICE:
			return INTROSPECT_TYPE_SLICE;
		case TYPE_VECTOR:
		case TYPE_INFERRED_VECTOR:
			return INTROSPECT_TYPE_VECTOR;
		case TYPE_OPTIONAL:
			return INTROSPECT_TYPE_OPTIONAL;
		case TYPE_UNTYPED_LIST:
		case TYPE_TYPEINFO:
		case TYPE_MEMBER:
		case TYPE_WILDCARD:
			UNREACHABLE
			return 0;
	}
	UNREACHABLE
}

Module *type_base_module(Type *type)
{
	RETRY:
	switch (type->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_VOID:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_BOOL:
		case TYPE_ANY:
		case TYPE_ANYFAULT:
		case TYPE_TYPEID:
		case TYPE_WILDCARD:
			return NULL;
		case TYPE_POINTER:
			type = type->pointer;
			goto RETRY;
		case TYPE_FUNC_PTR:
			type = type->pointer;
			FALLTHROUGH;
		case TYPE_FUNC_RAW:
			return type->function.decl ? type->function.decl->unit->module : NULL;
		case TYPE_ENUM:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_BITSTRUCT:
		case TYPE_FAULTTYPE:
		case TYPE_DISTINCT:
		case TYPE_INTERFACE:
			return type->decl->unit ? type->decl->unit->module : NULL;
		case TYPE_TYPEDEF:
			type = type->canonical;
			goto RETRY;
		case TYPE_ARRAY:
		case TYPE_SLICE:
		case TYPE_INFERRED_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_VECTOR:
		case TYPE_INFERRED_VECTOR:
			type = type->array.base;
			goto RETRY;
		case TYPE_OPTIONAL:
			type = type->optional;
			goto RETRY;
		case TYPE_UNTYPED_LIST:
		case TYPE_TYPEINFO:
		case TYPE_MEMBER:
			UNREACHABLE
	}
	UNREACHABLE
}
