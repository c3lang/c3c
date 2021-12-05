// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"

static STable function_types;
static struct
{
	Type u0, u1, i8, i16, i32, i64, i128, ixx;
	Type u8, u16, u32, u64, u128;
	Type f16, f32, f64, f128, fxx;
	Type usz, isz, uptr, iptr, uptrdiff, iptrdiff;
	Type voidstar, typeid, anyerr, typeinfo, ctlist;
	Type str, any, anyfail;
} t;

Type *type_bool = &t.u1;
Type *type_void = &t.u0;
Type *type_voidptr = &t.voidstar;
Type *type_half = &t.f16;
Type *type_float = &t.f32;
Type *type_double = &t.f64;
Type *type_quad = &t.f128;
Type *type_typeid = &t.typeid;
Type *type_any = &t.any;
Type *type_typeinfo = &t.typeinfo;
Type *type_ichar = &t.i8;
Type *type_short = &t.i16;
Type *type_int = &t.i32;
Type *type_long = &t.i64;
Type *type_i128 = &t.i128;
Type *type_iptr = &t.iptr;
Type *type_iptrdiff = &t.iptrdiff;
Type *type_isize = &t.isz;
Type *type_char = &t.u8;
Type *type_ushort = &t.u16;
Type *type_uint = &t.u32;
Type *type_ulong = &t.u64;
Type *type_u128 = &t.u128;
Type *type_uptr = &t.uptr;
Type *type_uptrdiff = &t.uptrdiff;
Type *type_usize = &t.usz;
Type *type_compstr = &t.str;
Type *type_anyerr = &t.anyerr;
Type *type_complist = &t.ctlist;
Type *type_anyfail = &t.anyfail;

static unsigned size_subarray;
static AlignSize alignment_subarray;
static AlignSize max_alignment_vector;

#define PTR_OFFSET 0
#define INFERRED_ARRAY_OFFSET 1
#define SUB_ARRAY_OFFSET 2
#define FAILABLE_OFFSET 3
#define ARRAY_OFFSET 4

Type *type_cint(void)
{
	return type_int_signed_by_bitsize(platform_target.width_c_int);
}

Type *type_cuint(void)
{
	return type_int_unsigned_by_bitsize(platform_target.width_c_int);
}

Type *type_int_signed_by_bitsize(unsigned bitsize)
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
Type *type_int_unsigned_by_bitsize(unsigned bytesize)
{
	switch (bytesize)
	{
		case 8: return type_char;
		case 16: return type_ushort;
		case 32: return type_uint;
		case 64: return type_ulong;
		case 128: return type_u128;
		default: FATAL_ERROR("Illegal bitsize %d", bytesize);
	}
}

const char *type_quoted_error_string(Type *type)
{
	char *buffer = NULL;
	if (type->canonical != type)
	{
		asprintf(&buffer, "'%s' (%s)", type_to_error_string(type), type_to_error_string(type->canonical));
		return buffer;
	}
	asprintf(&buffer, "'%s'", type_to_error_string(type));
	return buffer;
}
const char *type_to_error_string(Type *type)
{
	char *buffer = NULL;
	switch (type->type_kind)
	{
		case TYPE_POISONED:
			return "poisoned";
		case TYPE_ENUM:
		case TYPE_ERRTYPE:
		case TYPE_TYPEDEF:
		case TYPE_STRUCT:
		case TYPE_VOID:
		case TYPE_BOOL:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_UNION:
		case TYPE_DISTINCT:
		case TYPE_BITSTRUCT:
		case TYPE_ANYERR:
		case TYPE_UNTYPED_LIST:
		case TYPE_ANY:
			return type->name;
		case TYPE_FUNC:
			return strcat_arena("func ", type->func.mangled_function_signature);
		case TYPE_VECTOR:
			asprintf(&buffer, "%s[<%llu>]", type_to_error_string(type->array.base), (unsigned long long)type->array.len);
			return buffer;
		case TYPE_TYPEINFO:
			return "typeinfo";
		case TYPE_TYPEID:
			return "typeid";
		case TYPE_FAILABLE_ANY:
			return "void!";
		case TYPE_POINTER:
			if (type->pointer->type_kind == TYPE_FUNC)
			{
				return type_to_error_string(type->pointer);
			}
			asprintf(&buffer, "%s*", type_to_error_string(type->pointer));
			return buffer;
		case TYPE_FAILABLE:
			if (!type->failable) return "void!";
			asprintf(&buffer, "%s!", type_to_error_string(type->failable));
			return buffer;
		case TYPE_STRLIT:
			return "string literal";
		case TYPE_ARRAY:
			asprintf(&buffer, "%s[%llu]", type_to_error_string(type->array.base), (unsigned long long)type->array.len);
			return buffer;
		case TYPE_INFERRED_ARRAY:
			asprintf(&buffer, "%s[*]", type_to_error_string(type->array.base));
			return buffer;
		case TYPE_SUBARRAY:
			asprintf(&buffer, "%s[]", type_to_error_string(type->array.base));
			return buffer;
	}
	UNREACHABLE
}

void type_append_signature_name(Type *type, char *dst, size_t *offset)
{
	type = type->canonical;
	assert(*offset < MAX_FUNCTION_SIGNATURE_SIZE);
	const char *name;
	switch (type->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_TYPEDEF:
			UNREACHABLE;
		case TYPE_ERRTYPE:
		case TYPE_ENUM:
		case TYPE_STRUCT:
		case TYPE_UNION:
			name = type->decl->external_name;
			break;
		default:
			name = type->name;
			break;
	}
	memcpy(dst + *offset, name, strlen(name));
	*offset += strlen(name);
}



TypeSize type_size(Type *type)
{
RETRY:
	switch (type->type_kind)
	{
		case TYPE_BITSTRUCT:
			type = type->decl->bitstruct.base_type->type;
			goto RETRY;
		case TYPE_DISTINCT:
			type = type->decl->distinct_decl.base_type;
			goto RETRY;
		case TYPE_VECTOR:
		{
			TypeSize width = type_size(type->vector.base) * type->vector.len;
			if (width & (width - 1))
			{
				AlignSize alignment = next_highest_power_of_2((uint32_t) width);
				width = aligned_offset((AlignSize)width, alignment);
			}
			return width;
		}
		case CT_TYPES:
			UNREACHABLE;
		case TYPE_FAILABLE:
			type = type->failable;
			goto RETRY;
		case TYPE_TYPEDEF:
			type = type->canonical;
			goto RETRY;
		case TYPE_ERRTYPE:
			type = type_iptr->canonical;
			goto RETRY;
		case TYPE_ENUM:
			return type->decl->enums.type_info->type->canonical->builtin.bytesize;
		case TYPE_STRUCT:
		case TYPE_UNION:
			assert(type->decl->resolve_status == RESOLVE_DONE);
			return type->decl->strukt.size;
		case TYPE_VOID:
		case TYPE_FAILABLE_ANY:
			return 1;
		case TYPE_BOOL:
		case TYPE_TYPEID:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_ANYERR:
		case TYPE_ANY:
			return type->builtin.bytesize;
		case TYPE_STRLIT:
		case TYPE_FUNC:
		case TYPE_POINTER:
			return t.iptr.canonical->builtin.bytesize;
		case TYPE_ARRAY:
			return type_size(type->array.base) * type->array.len;
		case TYPE_SUBARRAY:
			return size_subarray;
	}
	UNREACHABLE
}

const char *type_generate_qname(Type *type)
{
	if (type_is_builtin(type->type_kind)) return type->name;
	return strformat("%s::%s", type->decl->module->name->module, type->name);
}


bool type_is_float_or_float_vector(Type *type)
{
	type = type_flatten(type);
	if (type->type_kind == TYPE_VECTOR) type = type->vector.base;
	TypeKind kind = type->type_kind;
	return kind >= TYPE_FLOAT_FIRST && kind <= TYPE_FLOAT_LAST;
}

bool type_is_union_struct(Type *type)
{
	TypeKind kind = type->canonical->type_kind;
	return kind == TYPE_STRUCT || kind == TYPE_UNION;
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
		case TYPE_POISONED:
			return false;
		case TYPE_FAILABLE:
			type = type->failable;
			goto RETRY;
		case TYPE_DISTINCT:
			type = type->decl->distinct_decl.base_type;
			goto RETRY;
		case TYPE_TYPEDEF:
			type = type->canonical;
			goto RETRY;
		case TYPE_BITSTRUCT:
		case ALL_FLOATS:
		case TYPE_VOID:
		case TYPE_FAILABLE_ANY:
		case ALL_INTS:
		case TYPE_BOOL:
		case TYPE_TYPEID:
		case TYPE_POINTER:
		case TYPE_ENUM:
		case TYPE_FUNC:
		case TYPE_STRLIT:
		case TYPE_VECTOR:
		case TYPE_ANYERR:
		case TYPE_ERRTYPE:
			return false;
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_SUBARRAY:
		case TYPE_ARRAY:
		case TYPE_ANY:
			return true;
		case TYPE_TYPEINFO:
		case TYPE_INFERRED_ARRAY:
		case TYPE_UNTYPED_LIST:
			UNREACHABLE
	}
	UNREACHABLE
}




Type *type_find_largest_union_element(Type *type)
{
	assert(type->type_kind == TYPE_UNION);
	ByteSize largest = 0;
	Type *largest_type = NULL;
	Decl **members = type->decl->strukt.members;
	VECEACH(members, i)
	{
		if (type_size(type) > largest)
		{
			largest = type_size(type);
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
		case TYPE_VECTOR:
			return true;
		case TYPE_TYPEDEF:
			type = type->canonical;
			goto RETRY;
		case TYPE_DISTINCT:
			type = type->decl->distinct_decl.base_type;
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
		case TYPE_INFERRED_ARRAY:
		case TYPE_POISONED:
			UNREACHABLE
		case TYPE_VOID:
		case TYPE_UNION:
		case TYPE_STRUCT:
		case TYPE_BITSTRUCT:
			return false;
		case TYPE_TYPEDEF:
			type = type->canonical;
			goto RETRY;
		case TYPE_SUBARRAY:
		case TYPE_ARRAY:
			// Arrays are comparable if elements are
			type = type->array.base;
			goto RETRY;
		case TYPE_DISTINCT:
			type = type->decl->distinct_decl.base_type;
			goto RETRY;
		default:
			return true;
	}
}

AlignSize type_abi_alignment(Type *type)
{
	RETRY:
	switch (type->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_TYPEINFO:
		case TYPE_UNTYPED_LIST:
			UNREACHABLE;
		case TYPE_BITSTRUCT:
			type = type->decl->bitstruct.base_type->type;
			goto RETRY;
		case TYPE_VECTOR:
		{
			ByteSize width = type_size(type->vector.base) * (uint32_t)type->vector.len;
			AlignSize alignment = (AlignSize)(int32_t)width;
			if (alignment & (alignment - 1))
			{
				alignment = (AlignSize)next_highest_power_of_2((uint32_t)alignment);
			}
			if (max_alignment_vector && alignment > max_alignment_vector) alignment = max_alignment_vector;
			return alignment;
		}
		case TYPE_VOID:
		case TYPE_FAILABLE_ANY:
			return 1;
		case TYPE_FAILABLE:
			type = type->failable;
			goto RETRY;
		case TYPE_DISTINCT:
			type = type->decl->distinct_decl.base_type;
			goto RETRY;
		case TYPE_TYPEDEF:
			type = type->canonical;
			goto RETRY;
		case TYPE_ENUM:
			return type->decl->enums.type_info->type->canonical->builtin.abi_alignment;
		case TYPE_ERRTYPE:
			return t.iptr.canonical->builtin.abi_alignment;
		case TYPE_STRUCT:
		case TYPE_UNION:
			return type->decl->alignment;
		case TYPE_BOOL:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_ANY:
		case TYPE_ANYERR:
			return type->builtin.abi_alignment;
		case TYPE_FUNC:
		case TYPE_POINTER:
		case TYPE_STRLIT:
		case TYPE_TYPEID:
			return t.iptr.canonical->builtin.abi_alignment;
		case TYPE_ARRAY:
		case TYPE_INFERRED_ARRAY:
			type = type->array.base;
			goto RETRY;
		case TYPE_SUBARRAY:
			return alignment_subarray;
	}
	UNREACHABLE
}

static inline void create_type_cache(Type *type)
{
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
		ptr = type_new(TYPE_POINTER, strformat("%s*", ptr_type->name));
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

static Type *type_generate_failable(Type *failable_type, bool canonical)
{
	if (canonical) failable_type = failable_type->canonical;
	if (!failable_type->type_cache)
	{
		create_type_cache(failable_type);
	}
	Type *failable = failable_type->type_cache[FAILABLE_OFFSET];
	if (failable == NULL)
	{
		failable = type_new(TYPE_FAILABLE, strformat("%s!", failable_type->name));
		failable->pointer = failable_type;
		failable_type->type_cache[FAILABLE_OFFSET] = failable;
		if (failable_type == failable_type->canonical)
		{
			failable->canonical = failable;
		}
		else
		{
			failable->canonical = type_generate_failable(failable_type->canonical, true);
		}
	}
	return failable;
}

static Type *type_generate_subarray(Type *arr_type, bool canonical)
{
	if (canonical) arr_type = arr_type->canonical;
	if (!arr_type->type_cache)
	{
		create_type_cache(arr_type);
	}

	Type *arr = arr_type->type_cache[SUB_ARRAY_OFFSET];
	if (arr == NULL)
	{
		arr = type_new(TYPE_SUBARRAY, strformat("%s[]", arr_type->name));
		arr->array.base = arr_type;
		arr_type->type_cache[SUB_ARRAY_OFFSET] = arr;
		if (arr_type == arr_type->canonical)
		{
			arr->canonical = arr;
		}
		else
		{
			arr->canonical = type_generate_subarray(arr_type->canonical, true);
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
		arr = type_new(TYPE_INFERRED_ARRAY, strformat("%s[*]", arr_type->name));
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


Type *type_get_ptr_recurse(Type *ptr_type)
{
	assert(ptr_type->type_kind != TYPE_FAILABLE_ANY);
	if (ptr_type->type_kind == TYPE_FAILABLE)
	{
		ptr_type = ptr_type->failable;
		return type_get_failable(type_get_ptr(ptr_type));
	}
	return type_get_ptr(ptr_type);

}
Type *type_get_ptr(Type *ptr_type)
{
	assert(!type_is_failable(ptr_type));
	return type_generate_ptr(ptr_type, false);
}

Type *type_get_failable(Type *failable_type)
{
	assert(!type_is_failable(failable_type));
	return type_generate_failable(failable_type, false);
}

Type *type_get_subarray(Type *arr_type)
{
	return type_generate_subarray(arr_type, false);
}

Type *type_get_inferred_array(Type *arr_type)
{
	return type_generate_inferred_array(arr_type, false);
}

static inline bool array_structurally_equivalent_to_struct(Type *array, Type *type)
{
	assert(array->type_kind == TYPE_ARRAY);

	MemberIndex len = (MemberIndex)array->array.len;
	if (!len) return type_size(type) == 0;

	Type *base = array->array.base;

	if (len == 1 && type_is_structurally_equivalent(base, type)) return true;

	assert(type->type_kind != TYPE_UNION && "Does not work on unions");

	if (!type_is_structlike(type)) return false;

	Decl **members = type->decl->strukt.members;

	// For structs / errors, all members must match.
	MemberIndex  offset = 0;
	AlignSize align_size = type_abi_alignment(array);
	Type *array_base = array->array.base;
	VECEACH(members, i)
	{
		if (!type_is_structurally_equivalent(array_base, members[i]->type)) return false;
		if (members[i]->offset != offset) return false;
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
		Decl **members = type2->decl->strukt.members;
		// If any member is structurally equivalent, then
		// the cast is valid.
		VECEACH(members, i)
		{
			if (type_is_structurally_equivalent(type1, members[i]->type)) return true;
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

	if (!type_is_structlike(type1)) return false;

	Decl **members = type1->decl->strukt.members;
	if (type1->type_kind == TYPE_UNION)
	{
		// If any member is structurally equivalent, then
		// the cast is valid.
		VECEACH(members, i)
		{
			if (type_is_structurally_equivalent(members[i]->type, type2)) return true;
		}
		return false;
	}

	// The only thing we have left is to check against another structlike.
	if (!type_is_structlike(type2)) return false;

	Decl **other_members = type2->decl->strukt.members;

	// For structs / errors, all members must match.
	VECEACH(members, i)
	{
		if (!type_is_structurally_equivalent(members[i]->type, other_members[i]->type)) return false;
		if (members[i]->offset != other_members[i]->offset) return false;
	}
	return true;
}

bool type_is_user_defined(Type *type)
{
	switch (type->type_kind)
	{
		case TYPE_ENUM:
		case TYPE_FUNC:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_ERRTYPE:
		case TYPE_TYPEDEF:
		case TYPE_DISTINCT:
			return true;
		default:
			return false;
	}
}

Type *type_get_indexed_type(Type *type)
{
	RETRY:
	switch (type->type_kind)
	{
		case TYPE_POINTER:
			return type->pointer;
		case TYPE_ARRAY:
		case TYPE_SUBARRAY:
		case TYPE_INFERRED_ARRAY:
		case TYPE_VECTOR:
			return type->array.base;
		case TYPE_STRLIT:
			return type_char;
		case TYPE_DISTINCT:
			type = type->decl->distinct_decl.base_type;
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
			if (ptr_vec->vector.len == len) return ptr_vec;
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
		vec_arr = type_new(TYPE_VECTOR, strformat("%s[<%u>]", element_type->name, len));
		vec_arr->vector.base = element_type;
		vec_arr->vector.len = len;
	}
	else
	{
		vec_arr = type_new(TYPE_ARRAY, strformat("%s[%u]", element_type->name, len));
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
			return true;
		case TYPE_TYPEDEF:
			type = type->canonical;
			goto RETRY;
		default:
			return false;
	}
}

Type *type_get_vector_bool(Type *original_type)
{
	Type *type = type_flatten(original_type);
	ByteSize size = type_size(type->vector.base);
	return type_get_vector(type_int_signed_by_bitsize((unsigned)size * 8), (unsigned)original_type->vector.len);
}

Type *type_get_vector(Type *vector_type, unsigned len)
{
	return type_create_array(vector_type, len, true, false);
}

static void type_create(const char *name, Type *location, TypeKind kind, unsigned bitsize,
                        unsigned align, unsigned pref_align)
{
	assert(align);
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
	assert(align.align);
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
	*location = (Type) {
		.type_kind = TYPE_TYPEDEF,
		.name = name,
		.canonical = canonical
	};
	global_context_add_type(location);
}


static void type_append_name_to_scratch(Type *type)
{
	type = type->canonical;
	switch (type->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_TYPEDEF:
			UNREACHABLE;
		case TYPE_ERRTYPE:
		case TYPE_ENUM:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_DISTINCT:
		case TYPE_BITSTRUCT:
			scratch_buffer_append(type->decl->external_name);
			break;
		case TYPE_POINTER:
			type_append_name_to_scratch(type->pointer);
			scratch_buffer_append_char('*');
			break;
		case TYPE_FAILABLE_ANY:
			scratch_buffer_append("void!");
			break;
		case TYPE_FAILABLE:
			if (type->failable)
			{
				type_append_name_to_scratch(type->failable);
			}
			else
			{
				scratch_buffer_append("void");
			}
			scratch_buffer_append_char('!');
			break;
		case TYPE_SUBARRAY:
			type_append_name_to_scratch(type->pointer);
			scratch_buffer_append("[]");
		case TYPE_VOID:
		case TYPE_BOOL:
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
		case TYPE_I128:
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
		case TYPE_U128:
		case TYPE_F16:
		case TYPE_F32:
		case TYPE_F64:
		case TYPE_F128:
		case TYPE_TYPEID:
		case TYPE_ANYERR:
		case TYPE_ANY:
		case TYPE_VECTOR:
			scratch_buffer_append(type->name);
			break;
		case TYPE_STRLIT:
		case TYPE_UNTYPED_LIST:
		case TYPE_INFERRED_ARRAY:
		case TYPE_TYPEINFO:
			UNREACHABLE
			break;
		case TYPE_FUNC:
			scratch_buffer_append(type->func.mangled_function_signature);
			break;
		case TYPE_ARRAY:
			type_append_name_to_scratch(type->array.base);
			scratch_buffer_append_char('[');
			scratch_buffer_append_signed_int(type->array.len);
			scratch_buffer_append_char(']');
			break;
	}
}

Type *type_find_function_type(FunctionSignature *signature)
{
	scratch_buffer_clear();
	type_append_name_to_scratch(signature->rtype->type);
	scratch_buffer_append_char('(');
	unsigned elements = vec_size(signature->params);
	for (unsigned i = 0; i < elements; i++)
	{
		if (i > 0)
		{
			scratch_buffer_append_char(',');
		}
		type_append_name_to_scratch(signature->params[i]->var.type_info->type);
	}
	if (signature->variadic == VARIADIC_RAW && elements > 0)
	{
		scratch_buffer_append_char(',');
	}
	if (signature->variadic != VARIADIC_NONE)
	{
		scratch_buffer_append("...");
	}
	scratch_buffer_append_char(')');
	const char *mangled_signature = scratch_buffer_interned();
	Type *func_type = stable_get(&function_types, mangled_signature);
	if (!func_type)
	{
		func_type = type_new(TYPE_FUNC, mangled_signature);
		func_type->canonical = func_type;
		func_type->func.signature = signature;
		func_type->func.mangled_function_signature = mangled_signature;
		stable_set(&function_types, mangled_signature, func_type);
	}
	return func_type;
}

static inline void type_init_int(const char *name, Type *type, TypeKind kind, BitSizes bits)
{
	unsigned actual_bits = bits ? (unsigned int)(8 << (bits - 1)) : 1;
	type_init(name, type, kind, actual_bits, platform_target.integers[bits]);
}

static inline void type_create_float(const char *name, Type *type, TypeKind kind, BitSizes bits)
{
	unsigned actual_bits = bits ? (unsigned int)(8 << (bits - 1)) : 1;
	type_init(name, type, kind, actual_bits, platform_target.floats[bits]);
}

void type_setup(PlatformTarget *target)
{
	stable_init(&function_types, 0x1000);
	max_alignment_vector = (AlignSize)target->align_max_vector;

	type_create_float("float16", &t.f16, TYPE_F16, BITS16);
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

	type_init("string", &t.str, TYPE_STRLIT, target->width_pointer, target->align_pointer);


	type_create("typeinfo", &t.typeinfo, TYPE_TYPEINFO, 1, 1, 1);
	type_create("complist", &t.ctlist, TYPE_UNTYPED_LIST, 1, 1, 1);
	type_create("void!", &t.anyfail, TYPE_FAILABLE_ANY, 1, 1, 1);
	type_init("typeid", &t.typeid, TYPE_TYPEID, target->width_pointer, target->align_pointer);

	type_init("void*", &t.voidstar, TYPE_POINTER, target->width_pointer, target->align_pointer);
	create_type_cache(type_void);
	type_void->type_cache[0] = &t.voidstar;
	t.voidstar.pointer = type_void;
	type_init("variant", &t.any, TYPE_ANY, target->width_pointer * 2, target->align_pointer);

	type_create_alias("usize", &t.usz, type_int_unsigned_by_bitsize(target->width_pointer));
	type_create_alias("isize", &t.isz, type_int_signed_by_bitsize(target->width_pointer));

	type_create_alias("uptr", &t.uptr, type_int_unsigned_by_bitsize(target->width_pointer));
	type_create_alias("iptr", &t.iptr, type_int_signed_by_bitsize(target->width_pointer));

	type_create_alias("uptrdiff", &t.uptrdiff, type_int_unsigned_by_bitsize(target->width_pointer));
	type_create_alias("iptrdiff", &t.iptrdiff, type_int_signed_by_bitsize(target->width_pointer));

	alignment_subarray = MAX(type_abi_alignment(&t.voidstar), type_abi_alignment(t.usz.canonical));
	size_subarray = (unsigned)(alignment_subarray * 2);
	type_init("anyerr", &t.anyerr, TYPE_ANYERR, target->width_pointer, target->align_pointer);
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
		case TYPE_FUNC:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_ARRAY:
		case TYPE_SUBARRAY:
		case TYPE_VECTOR:
		case TYPE_FAILABLE_ANY:
		case TYPE_ANY:
			return false;
		case TYPE_BOOL:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_STRLIT:
		case TYPE_TYPEID:
		case TYPE_POINTER:
		case TYPE_ENUM:
		case TYPE_ERRTYPE:
		case TYPE_ANYERR:
			return true;
		case TYPE_BITSTRUCT:
			type = type->decl->bitstruct.base_type->type;
			goto RETRY;
		case TYPE_DISTINCT:
			type = type->decl->distinct_decl.base_type;
			goto RETRY;
		case TYPE_FAILABLE:
			type = type->failable;
			if (!type) return false;
			goto RETRY;
		case TYPE_TYPEDEF:
			type = type->canonical;
			goto RETRY;
	}
	UNREACHABLE
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
	while (1)
	{
		if (type == possible_subtype) return true;
		if (type->type_kind != possible_subtype->type_kind) return false;
		if (type->decl->decl_kind != DECL_STRUCT) return false;

		if (!possible_subtype->decl->strukt.members) return false;

		Decl *first_element = possible_subtype->decl->strukt.members[0];

		if (first_element->decl_kind != DECL_VAR) return false;

		possible_subtype = first_element->type->canonical;
	}

}

Type *type_from_token(TokenType type)
{
	switch (type)
	{
		case TOKEN_VARIANT:
			return type_any;
		case TOKEN_FAULT:
		case TOKEN_ANYERR:
			return type_anyerr;
		case TOKEN_VOID:
			return type_void;
		case TOKEN_BOOL:
			return type_bool;
		case TOKEN_CHAR:
			return type_char;
		case TOKEN_DOUBLE:
			return type_double;
		case TOKEN_FLOAT:
			return type_float;
		case TOKEN_INT128:
			return type_i128;
		case TOKEN_ICHAR:
			return type_ichar;
		case TOKEN_INT:
			return type_int;
		case TOKEN_IPTR:
			return type_iptr;
		case TOKEN_IPTRDIFF:
			return type_iptrdiff;
		case TOKEN_ISIZE:
			return type_isize;
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
		case TOKEN_UPTRDIFF:
			return type_uptrdiff;
		case TOKEN_USHORT:
			return type_ushort;
		case TOKEN_USIZE:
			return type_usize;
		case TOKEN_TYPEID:
			return type_typeid;
		default:
			UNREACHABLE
	}
}

bool type_may_have_sub_elements(Type *type)
{
	// An alias is not ok.
	switch (type->type_kind)
	{
		case TYPE_DISTINCT:
		case TYPE_UNION:
		case TYPE_STRUCT:
		case TYPE_ENUM:
		case TYPE_ERRTYPE:
		case TYPE_BITSTRUCT:
			return true;
		default:
			return false;
	}
}

Type *type_find_max_num_type(Type *num_type, Type *other_num)
{
	TypeKind kind = num_type->type_kind;
	TypeKind other_kind = other_num->type_kind;
	assert(kind <= other_kind && "Expected ordering");
	assert(kind != other_kind);

	// 1. The only conversions need to happen if the other type is a number.
	if (other_kind < TYPE_INTEGER_FIRST || other_kind > TYPE_FLOAT_LAST) return NULL;

	// 2. First check the float case.
	if (other_kind >= TYPE_FLOAT_FIRST && other_kind <= TYPE_FLOAT_LAST)
	{
		switch (other_kind)
		{
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
	assert(type_kind_is_any_integer(other_kind) && type_is_integer(num_type));

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
	// Subarray and vararray can implicitly convert to a pointer.
	if (other->type_kind == TYPE_SUBARRAY)
	{
		Type *max_type = type_find_max_type(type->pointer, other->pointer);
		if (!max_type) return NULL;
		return type_get_ptr(max_type);
	}

	// Neither subarray, vararray or pointer? Then no max
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
	if (pointer_type->type_kind == TYPE_VOID) return type_voidptr;

	if (pointer_type->type_kind == TYPE_POINTER && other_pointer_type->type_kind == TYPE_ARRAY)
	{
		// Decay foo[n]* to foo*
		other_pointer_type = type_get_ptr(other_pointer_type->array.base);
	}

	Type *max_type = type_find_max_type(pointer_type, other_pointer_type);
	if (!max_type) return NULL;
	return type_get_ptr(max_type);
}


Type *type_find_max_type(Type *type, Type *other)
{
	if (type == type_anyfail)
	{
		return type_get_opt_fail(other, true);
	}
	if (other == type_anyfail)
	{
		return type_get_failable(type);
	}

	type = type->canonical;
	other = other->canonical;

	assert(!type_is_failable(type) && !type_is_failable(other));

	if (type == other) return type;

	// Sort types
	if (type->type_kind > other->type_kind)
	{
		Type *temp = type;
		type = other;
		other = temp;
	}

	switch (type->type_kind)
	{
		case TYPE_INFERRED_ARRAY:
		case TYPE_POISONED:
		case TYPE_FAILABLE:
		case TYPE_FAILABLE_ANY:
			UNREACHABLE
		case TYPE_VOID:
		case TYPE_BOOL:
		case TYPE_TYPEINFO:
		case TYPE_ANY:
		case TYPE_BITSTRUCT:
			return NULL;
		case ALL_INTS:
			if (other->type_kind == TYPE_DISTINCT && type_underlying_is_numeric(other)) return other;
			if (other->type_kind == TYPE_ENUM) return type_find_max_type(type, other->decl->enums.type_info->type->canonical);
			return type_find_max_num_type(type, other);
		case ALL_FLOATS:
			if (other->type_kind == TYPE_DISTINCT && type_is_float(other->decl->distinct_decl.base_type)) return other;
			return type_find_max_num_type(type, other);
		case TYPE_POINTER:
			return type_find_max_ptr_type(type, other);
		case TYPE_ENUM:
			// IMPROVE: should there be implicit conversion between one enum and the other in
			// some way?
			return NULL;
		case TYPE_ERRTYPE:
			if (other->type_kind == TYPE_ERRTYPE) return type_anyerr;
			return NULL;
		case TYPE_FUNC:
		case TYPE_UNION:
		case TYPE_ANYERR:
		case TYPE_TYPEID:
		case TYPE_STRUCT:
		case TYPE_UNTYPED_LIST:
			TODO
		case TYPE_TYPEDEF:
			UNREACHABLE
		case TYPE_STRLIT:
			if (other->type_kind == TYPE_DISTINCT)
			{
				// In this case we only react to the flattened type.
				Type *flatten_other = type_flatten(other);
				if (flatten_other->type_kind == TYPE_SUBARRAY && flatten_other->array.base->type_kind == TYPE_U8) return other;
				if (flatten_other->type_kind == TYPE_POINTER && flatten_other->pointer->type_kind == TYPE_U8) return other;
			}
			if (other->type_kind == TYPE_SUBARRAY && other->array.base->type_kind == TYPE_U8) return other;
			if (other->type_kind == TYPE_POINTER && other->pointer->type_kind == TYPE_U8) return other;
			return NULL;
		case TYPE_DISTINCT:
			return NULL;
		case TYPE_ARRAY:
			return NULL;
		case TYPE_SUBARRAY:
			TODO
		case TYPE_VECTOR:
			// No implicit conversion between vectors
			return NULL;
	}
	UNREACHABLE
}

#define MAX_SEARCH_DEPTH 512

Type *type_find_common_ancestor(Type *left, Type *right)
{
	if (left == right) return left;
	left = left->canonical;
	right = right->canonical;
	if (left == right) return left;
	if (left->type_kind != right->type_kind) return NULL;
	if (left->type_kind == TYPE_POINTER)
	{
		Type *common = type_find_common_ancestor(left->pointer, right->pointer);
		return common ? type_get_ptr(common) : NULL;
	}
	if (left->type_kind != TYPE_STRUCT) return NULL;

	static Type *left_types[MAX_SEARCH_DEPTH];
	int depth = 0;
	while (depth < MAX_SEARCH_DEPTH)
	{
		if (!left->decl->strukt.members) break;
		Decl *first_element = left->decl->strukt.members[0];
		if (first_element->decl_kind != DECL_VAR) break;
		if (first_element->type->canonical == right) return right;
		left = first_element->type->canonical;
		left_types[depth++] = left;
	}
	if (depth == MAX_SEARCH_DEPTH)
	{
		error_exit("Struct type depth %d exceeded.", MAX_SEARCH_DEPTH);
	}
	while (true)
	{
		if (!right->decl->strukt.members) return NULL;
		Decl *first_element = right->decl->strukt.members[0];
		if (first_element->decl_kind != DECL_VAR) return NULL;
		right = first_element->type->canonical;
		for (int i = 0; i < depth; i++)
		{
			if (right == left_types[i]) return right;
		}
	}
}