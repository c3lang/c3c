// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "compiler_internal.h"

Type type_bool;
Type type_void, type_string;

Type type_half, type_float, type_double, type_quad;
Type type_char, type_short, type_int, type_long, type_isize;
Type type_byte, type_ushort, type_uint, type_ulong, type_usize;
Type type_compint, type_compuint, type_compfloat;
Type type_c_short, type_c_int, type_c_long, type_c_longlong;
Type type_c_ushort, type_c_uint, type_c_ulong, type_c_ulonglong;

Type *type_signed_int_by_size(int bitsize)
{
	switch (bitsize)
	{
		case 1: return &type_char;
		case 2: return &type_short;
		case 4: return &type_int;
		case 8: return &type_long;
		default: FATAL_ERROR("Illegal bitsize %d", bitsize);
	}
}
Type *type_unsigned_int_by_size(int bitsize)
{
	switch (bitsize)
	{
		case 1: return &type_byte;
		case 2: return &type_ushort;
		case 4: return &type_uint;
		case 8: return &type_ulong;
		default: FATAL_ERROR("Illegal bitsize %d", bitsize);
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
		case TYPE_POINTER:
		case TYPE_VARARRAY:
		case TYPE_STRING:
			return type_isize.canonical->builtin.bytesize;
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

	Type *canonical_ptr = canonical_base->ptr_like_canonical[(int)ptr_type->nullable];
	if (canonical_ptr == NULL)
	{
		canonical_ptr = malloc_arena(sizeof(Type));
		*canonical_ptr = *ptr_type;
		canonical_ptr->base = canonical_base;
		canonical_base->ptr_like_canonical[(int)ptr_type->nullable] = canonical_ptr;
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
		Type *canonical = canonical_base->ptr_like_canonical[2];
		if (canonical == NULL)
		{
			canonical = malloc_arena(sizeof(Type));
			*canonical = *arr_type;
			canonical->canonical = canonical_base;
			canonical_base->ptr_like_canonical[2] = canonical;
			canonical->resolve_status = RESOLVE_DONE;
		}
		return canonical;
	}

	int entries = (int)vec_size(canonical_base->ptr_like_canonical);
	for (int i = 3; i < entries; i++)
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

static void create_type(const char *name, Type *location, TypeKind kind, unsigned bytesize, unsigned bitsize)
{
	*location = (Type) { .type_kind = kind, .resolve_status = RESOLVE_DONE, .builtin.bytesize = bytesize, .builtin.bitsize = bitsize };
	location->name_loc.string = name;
	location->name_loc.span.length = strlen(name);
	location->canonical = location;
}

static void type_create_alias(const char *name, Type *location, Type *canonical)
{
	*location = (Type) { .type_kind = TYPE_USER_DEFINED, .resolve_status = RESOLVE_DONE };
	location->name_loc.string = name;
	location->name_loc.span.length = strlen(name);
	location->canonical = canonical;
}


void builtin_setup()
{
	create_type("void", &type_void, TYPE_VOID, 1, 8);

	/*TODO
 * decl_string = (Decl) { .decl_kind = DECL_BUILTIN, .name.string = "string" };
	create_type(&decl_string, &type_string);
	type_string.type_kind = TYPE_STRING;
*/
#define DEF_TYPE(_name, _type, _bits) \
create_type(#_name, &type_ ## _name, _type, (_bits + 7) / 8, _bits);

	DEF_TYPE(compint, TYPE_IXX, 64);
	DEF_TYPE(compuint, TYPE_UXX, 64);
	DEF_TYPE(compfloat, TYPE_FXX, 64);
	DEF_TYPE(bool, TYPE_BOOL, 1);

//	DEF_TYPE(half, 2, 16, NUMBER_TYPE_FLOAT, "half", T_F16);
	DEF_TYPE(float, TYPE_F32, 32);
	DEF_TYPE(double, TYPE_F64, 64);
//  DEF_TYPE(quad, 16, 128, NUMBER_TYPE_FLOAT, "long double", T_F128);

	DEF_TYPE(char, TYPE_I8, 8);
	DEF_TYPE(short, TYPE_I16, 16);
	DEF_TYPE(int, TYPE_I32, 32);
	DEF_TYPE(long, TYPE_I64, 64);

	DEF_TYPE(byte, TYPE_U8, 8);
	DEF_TYPE(ushort, TYPE_U16, 16);
	DEF_TYPE(uint, TYPE_U32, 32);
	DEF_TYPE(ulong, TYPE_U64, 64);

	type_create_alias("usize", &type_usize, type_unsigned_int_by_size(build_options.pointer_size));
	type_create_alias("isize", &type_isize, type_signed_int_by_size(build_options.pointer_size));

	type_create_alias("c_ushort", &type_c_ushort, type_unsigned_int_by_size(build_options.cshort_size));
	type_create_alias("c_uint", &type_c_uint, type_unsigned_int_by_size(build_options.cint_size));
	type_create_alias("c_ulong", &type_c_ulong, type_unsigned_int_by_size(build_options.clong_size));
	type_create_alias("c_ulonglong", &type_c_ulonglong, type_unsigned_int_by_size(build_options.clonglong_size));

	type_create_alias("c_short", &type_c_short, type_signed_int_by_size(build_options.cshort_size));
	type_create_alias("c_int", &type_c_int, type_signed_int_by_size(build_options.cint_size));
	type_create_alias("c_long", &type_c_long, type_signed_int_by_size(build_options.clong_size));
	type_create_alias("c_longlong", &type_c_longlong, type_signed_int_by_size(build_options.clonglong_size));


#undef DEF_TYPE

}