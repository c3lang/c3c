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
		case TYPE_ENUM:
		case TYPE_TYPEDEF:
		case TYPE_STRUCT:
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
		case TYPE_UNION:
		case TYPE_ERROR:
			return type->name;
		case TYPE_POINTER:
			asprintf(&buffer, "%s*", type_to_error_string(type->pointer));
			return buffer;
		case TYPE_STRING:
			return "string";
		case TYPE_ARRAY:
			asprintf(&buffer, "%s[%zu]", type_to_error_string(type->array.base), type->array.len);
			return buffer;
		case TYPE_VARARRAY:
			asprintf(&buffer, "%s[]", type_to_error_string(type->array.base));
			return buffer;
		case TYPE_SUBARRAY:
			asprintf(&buffer, "%s[:]", type_to_error_string(type->array.base));
			return buffer;

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
	memcpy(dst + *offset, type->name, strlen(type->name));
	*offset += strlen(type->name);
}



size_t type_size(Type *canonical)
{
	assert(canonical && canonical->canonical == canonical);
	switch (canonical->type_kind)
	{
		case TYPE_POISONED:
			UNREACHABLE;
		case TYPE_ENUM:
		case TYPE_TYPEDEF:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_ERROR:
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
			return type_size(canonical->array.base) * canonical->array.len;
		case TYPE_SUBARRAY:
			TODO
	}
	TODO
}

static inline void create_ptr_cache(Type *canonical_type)
{
	assert(canonical_type->canonical == canonical_type);
	canonical_type->ptr_cache = VECADD(canonical_type->ptr_cache, NULL);
	canonical_type->ptr_cache = VECADD(canonical_type->ptr_cache, NULL);
}

static Type *type_generate_ptr(Type *ptr_type, bool canonical)
{
	if (canonical) ptr_type = ptr_type->canonical;
	if (!ptr_type->ptr_cache)
	{
		create_ptr_cache(ptr_type);
	}

	Type *ptr = ptr_type->ptr_cache[0];
	if (ptr == NULL)
	{
		ptr = type_new(TYPE_POINTER, strformat("%s*", ptr_type->name));
		ptr->pointer = ptr_type;
		ptr_type->ptr_cache[0] = ptr;
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

Type *type_get_ptr(Type *ptr_type)
{
	return type_generate_ptr(ptr_type, false);
}


Type *type_create_array(Type *arr_type, uint64_t len, bool canonical)
{
	if (canonical) arr_type = arr_type->canonical;
	if (!arr_type->ptr_cache)
	{
		create_ptr_cache(arr_type);
	}

	// Dynamic array
	if (len == 0)
	{
		Type *array = arr_type->ptr_cache[1];
		if (array == NULL)
		{
			array = type_new(TYPE_VARARRAY, strformat("%s[]", arr_type->name));
			array->array.base = arr_type;
			array->array.len = 0;
			if (arr_type->canonical == arr_type)
			{
				array->canonical = array;
			}
			else
			{
				array->canonical = type_create_array(arr_type, len, true);
			}
			arr_type->ptr_cache[1] = array;
		}
		return array;
	}

	int entries = (int)vec_size(arr_type->ptr_cache);
	for (int i = 1; i < entries; i++)
	{
		Type *ptr = arr_type->ptr_cache[i];
		if (ptr->array.len == arr_type->array.len)
		{
			return ptr;
		}
	}
	Type *array = type_new(TYPE_ARRAY, strformat("%s[%llu]", arr_type->name, len));
	array->array.base = arr_type;
	array->array.len = 0;
	if (arr_type->canonical == arr_type)
	{
		array->canonical = array;
	}
	else
	{
		array->canonical = type_create_array(arr_type, len, true);
	}
	VECADD(arr_type->ptr_cache, array);
	return array;
}

Type *type_get_array(Type *arr_type, uint64_t len)
{
	return type_create_array(arr_type, len, false);
}

static void type_create(const char *name, Type *location, Type **ptr, TypeKind kind, unsigned bytesize, unsigned bitsize)
{
	*location = (Type) {
		.type_kind = kind,
		.builtin.bytesize = bytesize,
		.builtin.bitsize = bitsize,
		.name = name,
		.canonical = location
	};
	location->name = name;
	location->canonical = location;
	*ptr = location;
}

static void type_create_alias(const char *name, Type *location, Type **ptr, Type *canonical)
{
	*location = (Type) {
		.type_kind = TYPE_TYPEDEF,
		.name = name,
		.canonical = canonical
	};
	*ptr = location;
}


void builtin_setup()
{
	type_create("void", &t_u0, &type_void, TYPE_VOID, 1, 8);
	type_create("string", &t_str, &type_string, TYPE_STRING, build_options.pointer_size, build_options.pointer_size * 8);
	create_ptr_cache(type_void);
	type_void->ptr_cache[0] = &t_voidstar;
	type_create("void*", &t_voidstar, &type_voidptr, TYPE_POINTER, 0, 0);
	t_voidstar.pointer = type_void;

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
	if (type->decl->decl_kind != DECL_STRUCT) return false;

	if (!possible_subtype->decl->strukt.members) return false;

	Decl *first_element = possible_subtype->decl->strukt.members[0];

	if (first_element->decl_kind != DECL_VAR) return false;

	return type_is_subtype(type, first_element->type->canonical);
}


bool type_may_have_method_functions(Type *type)
{
	// An alias is not ok.
	switch (type->type_kind)
	{
		case TYPE_UNION:
		case TYPE_STRUCT:
		case TYPE_ENUM:
			return true;
		default:
			return false;
	}
}