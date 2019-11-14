// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "compiler_internal.h"

Type *type_bool, *type_void, *type_string, *type_voidptr;
Type *type_float, *type_double;
Type *type_char, *type_short, *type_int, *type_long, *type_isize;
Type *type_byte, *type_ushort, *type_uint, *type_ulong, *type_usize;
Type *type_compint, *type_compuint, *type_compfloat;
Type *type_c_short, *type_c_int, *type_c_long, *type_c_longlong;
Type *type_c_ushort, *type_c_uint, *type_c_ulong, *type_c_ulonglong;

Type t_u0, t_str;
Type t_u1, t_i8, t_i16, t_i32, t_i64, t_ixx;
Type t_u8, t_u16, t_u32, t_u64, t_uxx;
Type t_f32, t_f64, t_fxx;
Type t_usz, t_isz;
Type t_cus, t_cui, t_cul, t_cull;
Type t_cs, t_ci, t_cl, t_cll;
Type t_voidstar;

Type *type_signed_int_by_size(int bitsize)
{
	switch (bitsize)
	{
		case 1: return type_char;
		case 2: return type_short;
		case 4: return type_int;
		case 8: return type_long;
		default: FATAL_ERROR("Illegal bitsize %d", bitsize);
	}
}
Type *type_unsigned_int_by_size(int bitsize)
{
	switch (bitsize)
	{
		case 1: return type_byte;
		case 2: return type_ushort;
		case 4: return type_uint;
		case 8: return type_ulong;
		default: FATAL_ERROR("Illegal bitsize %d", bitsize);
	}
}

const char *type_to_error_string(Type *type)
{
	char *buffer = NULL;
	switch (type->type_kind)
	{
		case TYPE_POISONED:
			return "poisoned";
		case TYPE_USER_DEFINED:
			return type->name_loc.string;
		case TYPE_VOID:
		case TYPE_BOOL:
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
		case TYPE_IXX:
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
		case TYPE_UXX:
		case TYPE_F32:
		case TYPE_F64:
		case TYPE_FXX:
		case TYPE_FUNC:
			return type->name_loc.string;
		case TYPE_POINTER:
			asprintf(&buffer, "%s*", type_to_error_string(type->base));
			return buffer;
		case TYPE_STRING:
			return "string";
		case TYPE_ARRAY:
			if (type->resolve_status == RESOLVE_DONE)
			{
				asprintf(&buffer, "%s[%zu]", type_to_error_string(type->base), type->len);
			}
			else
			{
				asprintf(&buffer, "%s[]", type_to_error_string(type->base));
			}
			return buffer;
		case TYPE_VARARRAY:
			asprintf(&buffer, "%s[]", type_to_error_string(type->base));
			return buffer;
		case TYPE_INC_ARRAY:
			asprintf(&buffer, "%s[+]", type_to_error_string(type->base));
			return buffer;
		case TYPE_EXPRESSION:
			return "type(...)";
	}
}

static void type_append_signature_name_user_defined(Decl *decl, char *dst, size_t *offset)
{
	switch (decl->decl_kind)
	{
		case DECL_FUNC:
		{
			assert(decl->func.function_signature.mangled_signature);
			int len = sprintf(dst + *offset, "func %s", decl->func.function_signature.mangled_signature);
			*offset += len;
			return;
		}
		case DECL_POISONED:
		case DECL_VAR:
		case DECL_ENUM_CONSTANT:
		case DECL_TYPEDEF:
		case DECL_ERROR_CONSTANT:
		case DECL_ARRAY_VALUE:
		case DECL_IMPORT:
		case DECL_MACRO:
		case DECL_MULTI_DECL:
		case DECL_GENERIC:
		case DECL_CT_IF:
		case DECL_CT_ELSE:
		case DECL_CT_ELIF:
		case DECL_ATTRIBUTE:
			UNREACHABLE
		case DECL_STRUCT:
		case DECL_UNION:
		case DECL_ENUM:
		case DECL_ERROR:
			memcpy(dst + *offset, decl->name.string, decl->name.span.length);
			*offset += decl->name.span.length;
			return;
	}
	UNREACHABLE
}
void type_append_signature_name(Type *type, char *dst, size_t *offset)
{
	assert(*offset < 2000);
	assert(type->resolve_status == RESOLVE_DONE && type->canonical && type_ok(type));
	Type *canonical_type = type->canonical;
	switch (type->type_kind)
	{
		case TYPE_POISONED:
			UNREACHABLE
		case TYPE_USER_DEFINED:
			type_append_signature_name_user_defined(canonical_type->decl, dst, offset);
			return;
		case TYPE_VOID:
		case TYPE_BOOL:
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
		case TYPE_IXX:
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
		case TYPE_UXX:
		case TYPE_F32:
		case TYPE_F64:
		case TYPE_FXX:
		case TYPE_FUNC:
			memcpy(dst + *offset, type->name_loc.string, type->name_loc.span.length);
			*offset += type->name_loc.span.length;
			return;
		case TYPE_POINTER:
			type_append_signature_name(type->base, dst, offset);
			return;
		case TYPE_STRING:
			TODO
			return;
		case TYPE_ARRAY:
			type_append_signature_name(type->base, dst, offset);
			{
				int len = sprintf(dst + *offset, "[%zu]", type->len);
				*offset += len;
			}
			return;
		case TYPE_VARARRAY:
			type_append_signature_name(type->base, dst, offset);
			dst[*offset++] = '[';
			dst[*offset] = ']';
			*offset += 1;
			return;
		case TYPE_INC_ARRAY:
			TODO
			return;
		case TYPE_EXPRESSION:
			UNREACHABLE
	}
}


Type *type_new(TypeKind type_kind)
{
	Type *type = malloc_arena(sizeof(Type));
	memset(type, 0, sizeof(Type));
	type->type_kind = type_kind;
	return type;
}


size_t type_size(Type *canonical)
{
	assert(canonical && canonical->canonical == canonical);
	switch (canonical->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_INC_ARRAY:
		case TYPE_EXPRESSION:
			UNREACHABLE;
		case TYPE_USER_DEFINED:
			TODO
		case TYPE_VOID:
			return 1;
		case TYPE_BOOL:
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
		case TYPE_F32:
		case TYPE_F64:
			return canonical->builtin.bytesize;
		case TYPE_IXX:
		case TYPE_UXX:
		case TYPE_FXX:
			return 8;
		case TYPE_FUNC:
		case TYPE_POINTER:
		case TYPE_VARARRAY:
		case TYPE_STRING:
			return t_usz.canonical->builtin.bytesize;
		case TYPE_ARRAY:
			return type_size(canonical->base) * canonical->len;
	}
	TODO
}

static inline void create_ptr_live_canonical(Type *canonical_type)
{
	assert(canonical_type->canonical == canonical_type);
	canonical_type->ptr_like_canonical = VECADD(canonical_type->ptr_like_canonical, NULL);
	canonical_type->ptr_like_canonical = VECADD(canonical_type->ptr_like_canonical, NULL);
}

Type *type_get_canonical_ptr(Type *ptr_type)
{
	Type *canonical_base = ptr_type->base->canonical;

	assert(canonical_base);

	if (!canonical_base->ptr_like_canonical)
	{
		create_ptr_live_canonical(canonical_base);
	}

	Type *canonical_ptr = canonical_base->ptr_like_canonical[0];
	if (canonical_ptr == NULL)
	{
		canonical_ptr = malloc_arena(sizeof(Type));
		*canonical_ptr = *ptr_type;
		canonical_ptr->base = canonical_base;
		canonical_ptr->canonical = canonical_ptr;
		canonical_ptr->resolve_status = RESOLVE_DONE;
		canonical_base->ptr_like_canonical[0] = canonical_ptr;
		canonical_base->resolve_status = RESOLVE_DONE;
	}

	return canonical_ptr;
}

Type *type_get_canonical_array(Type *arr_type)
{
	Type *canonical_base = arr_type->base->canonical;
	if (!arr_type->ptr_like_canonical)
	{
		create_ptr_live_canonical(canonical_base);
	}

	// Dynamic array
	if (arr_type->len == 0)
	{
		Type *canonical = canonical_base->ptr_like_canonical[1];
		if (canonical == NULL)
		{
			canonical = malloc_arena(sizeof(Type));
			*canonical = *arr_type;
			canonical->canonical = canonical_base;
			canonical_base->ptr_like_canonical[1] = canonical;
			canonical->resolve_status = RESOLVE_DONE;
		}
		return canonical;
	}

	int entries = (int)vec_size(canonical_base->ptr_like_canonical);
	for (int i = 1; i < entries; i++)
	{
		Type *ptr = canonical_base->ptr_like_canonical[i];
		if (ptr->len == arr_type->len)
		{
			return ptr;
		}
	}
	Type *canonical = malloc_arena(sizeof(Type));
	*canonical = *arr_type;
	canonical->base = canonical_base;
	canonical->resolve_status = RESOLVE_DONE;
	canonical_base->ptr_like_canonical = VECADD(canonical_base->ptr_like_canonical, canonical);
	return canonical;
}

static void type_create(const char *name, Type *location, Type **ptr, TypeKind kind, unsigned bytesize, unsigned bitsize)
{
	*location = (Type) { .type_kind = kind, .resolve_status = RESOLVE_DONE, .builtin.bytesize = bytesize, .builtin.bitsize = bitsize };
	location->name_loc.string = name;
	location->name_loc.span.length = strlen(name);
	location->canonical = location;
	*ptr = location;
}

static void type_create_alias(const char *name, Type *location, Type **ptr, Type *canonical)
{
	*location = (Type) { .type_kind = TYPE_USER_DEFINED, .resolve_status = RESOLVE_DONE };
	location->name_loc.string = name;
	location->name_loc.span.length = strlen(name);
	location->canonical = canonical;
	*ptr = location;
}


void builtin_setup()
{
	type_create("void", &t_u0, &type_void, TYPE_VOID, 1, 8);
	type_create("string", &t_str, &type_string, TYPE_STRING, build_options.pointer_size, build_options.pointer_size * 8);
	create_ptr_live_canonical(type_void);
	type_void->ptr_like_canonical[0] = &t_voidstar;
	type_create("void*", &t_voidstar, &type_voidptr, TYPE_POINTER, 0, 0);
	t_voidstar.base = type_void;

	/*TODO
 * decl_string = (Decl) { .decl_kind = DECL_BUILTIN, .name.string = "string" };
	create_type(&decl_string, &type_string);
	type_string.type_kind = TYPE_STRING;
*/
#define DEF_TYPE(_name, _shortname, _type, _bits) \
type_create(#_name, &_shortname, &type_ ## _name, _type, (_bits + 7) / 8, _bits);

	DEF_TYPE(compint, t_ixx, TYPE_IXX, 64);
	DEF_TYPE(compuint, t_uxx, TYPE_UXX, 64);
	DEF_TYPE(compfloat, t_fxx, TYPE_FXX, 64);
	DEF_TYPE(bool, t_u1, TYPE_BOOL, 1);

	DEF_TYPE(float, t_f32, TYPE_F32, 32);
	DEF_TYPE(double, t_f64, TYPE_F64, 64);

	DEF_TYPE(char, t_i8, TYPE_I8, 8);
	DEF_TYPE(short, t_i16, TYPE_I16, 16);
	DEF_TYPE(int, t_i32, TYPE_I32, 32);
	DEF_TYPE(long, t_i64, TYPE_I64, 64);

	DEF_TYPE(byte, t_u8, TYPE_U8, 8);
	DEF_TYPE(ushort, t_u16, TYPE_U16, 16);
	DEF_TYPE(uint, t_u32, TYPE_U32, 32);
	DEF_TYPE(ulong, t_u64, TYPE_U64, 64);

	type_create_alias("usize", &t_usz, &type_usize, type_unsigned_int_by_size(build_options.pointer_size));
	type_create_alias("isize", &t_isz, &type_isize, type_signed_int_by_size(build_options.pointer_size));

	type_create_alias("c_ushort", &t_cus, &type_c_ushort, type_unsigned_int_by_size(build_options.cshort_size));
	type_create_alias("c_uint", &t_cui, &type_c_uint, type_unsigned_int_by_size(build_options.cint_size));
	type_create_alias("c_ulong", &t_cul, &type_c_ulong, type_unsigned_int_by_size(build_options.clong_size));
	type_create_alias("c_ulonglong", &t_cull, &type_c_ulonglong, type_unsigned_int_by_size(build_options.clonglong_size));

	type_create_alias("c_short", &t_cs, &type_c_short, type_signed_int_by_size(build_options.cshort_size));
	type_create_alias("c_int", &t_ci, &type_c_int, type_signed_int_by_size(build_options.cint_size));
	type_create_alias("c_long", &t_cl, &type_c_long, type_signed_int_by_size(build_options.clong_size));
	type_create_alias("c_longlong", &t_cll, &type_c_longlong, type_signed_int_by_size(build_options.clonglong_size));


#undef DEF_TYPE

}

/**
 * Check if a type is contained in another type.
 *
 * @param type canonical type
 * @param possible_subtype canonical type
 * @return true if it is a subtype
 */
bool type_is_subtype(Type *type, Type *possible_subtype)
{
	assert(type == type->canonical && possible_subtype == possible_subtype->canonical);
	if (type == possible_subtype) return true;
	if (type->type_kind != possible_subtype->type_kind) return false;
	if (type->type_kind != TYPE_USER_DEFINED || type->decl->decl_kind != DECL_STRUCT) return false;

	if (!possible_subtype->decl->strukt.members) return false;

	Decl *first_element = possible_subtype->decl->strukt.members[0];

	if (first_element->decl_kind != DECL_VAR) return false;

	return type_is_subtype(type, first_element->var.type->canonical);
}


bool type_may_have_method_functions(Type *type)
{
	// An alias is not ok.
	if (type->type_kind != TYPE_USER_DEFINED) return false;
	Decl *decl = type->decl;
	switch (decl->decl_kind)
	{
		case DECL_UNION:
		case DECL_STRUCT:
		case DECL_ERROR:
		case DECL_ENUM:
			return true;
		default:
			return false;
	}
}