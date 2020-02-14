// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

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

#define META_OFFSET 0
#define PTR_OFFSET 1
#define VAR_ARRAY_OFFSET 2
#define ARRAY_OFFSET 3

Type *type_signed_int_by_bitsize(unsigned bytesize)
{
	switch (bytesize)
	{
		case 8: return type_char;
		case 16: return type_short;
		case 32: return type_int;
		case 64: return type_long;
		default: FATAL_ERROR("Illegal bitsize %d", bytesize);
	}
}
Type *type_unsigned_int_by_bitsize(unsigned bytesize)
{
	switch (bytesize)
	{
		case 8: return type_byte;
		case 16: return type_ushort;
		case 32: return type_uint;
		case 64: return type_ulong;
		default: FATAL_ERROR("Illegal bitsize %d", bytesize);
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
		case TYPE_META_TYPE:
			asprintf(&buffer, "type %s", type_to_error_string(type->child));
			return buffer;
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
		case TYPE_ERROR_UNION:
			TODO
	}
	UNREACHABLE
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
		case DECL_THROWS:
		case DECL_POISONED:
		case DECL_VAR:
		case DECL_ENUM_CONSTANT:
		case DECL_TYPEDEF:
		case DECL_ERROR_CONSTANT:
		case DECL_ARRAY_VALUE:
		case DECL_IMPORT:
		case DECL_MACRO:
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
		{
			unsigned len = source_range_len(decl->name_span);
			memcpy(dst + *offset, decl->name, len);
			*offset += len;
			return;

		}
	}
	UNREACHABLE
}
void type_append_signature_name(Type *type, char *dst, size_t *offset)
{
	assert(*offset < MAX_FUNCTION_SIGNATURE_SIZE);
	const char *name;
	switch (type->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_TYPEDEF:
			UNREACHABLE;
		case TYPE_ERROR:
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



size_t type_size(Type *canonical)
{
	assert(canonical && canonical->canonical == canonical);
	switch (canonical->type_kind)
	{
		case TYPE_POISONED:
			UNREACHABLE;
		case TYPE_META_TYPE:
			return 0;
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
		case TYPE_ERROR_UNION:
			TODO
	}
	TODO
}

static inline void create_type_cache(Type *canonical_type)
{
	assert(canonical_type->canonical == canonical_type);
	for (int i = 0; i < ARRAY_OFFSET; i++)
	{
		vec_add(canonical_type->type_cache, NULL);
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

static Type *type_generate_meta(Type *type, bool canonical)
{
	if (canonical) type = type->canonical;
	if (!type->type_cache)
	{
		create_type_cache(type);
	}

	Type *meta = type->type_cache[META_OFFSET];
	if (meta == NULL)
	{
		meta = type_new(TYPE_META_TYPE, strformat("type %s", type->name));
		meta->child = type;
		type->type_cache[META_OFFSET] = meta;
		if (type == type->canonical)
		{
			meta->canonical = meta;
		}
		else
		{
			meta->canonical = type_generate_meta(type->canonical, true);
		}
	}
	return meta;
}


Type *type_get_ptr(Type *ptr_type)
{
	return type_generate_ptr(ptr_type, false);
}

Type *type_get_meta(Type *meta_type)
{
	return type_generate_meta(meta_type, false);
}


Type *type_create_array(Type *arr_type, uint64_t len, bool canonical)
{
	if (canonical) arr_type = arr_type->canonical;
	if (!arr_type->type_cache)
	{
		create_type_cache(arr_type);
	}

	// Dynamic array
	if (len == 0)
	{
		Type *array = arr_type->type_cache[VAR_ARRAY_OFFSET];
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
			arr_type->type_cache[VAR_ARRAY_OFFSET] = array;
		}
		return array;
	}

	int entries = (int)vec_size(arr_type->type_cache);
	for (int i = ARRAY_OFFSET; i < entries; i++)
	{
		Type *ptr = arr_type->type_cache[i];
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
	VECADD(arr_type->type_cache, array);
	return array;
}

Type *type_get_array(Type *arr_type, uint64_t len)
{
	return type_create_array(arr_type, len, false);
}

static void type_create(const char *name, Type *location, Type **ptr, TypeKind kind, unsigned bitsize,
                        unsigned align, unsigned pref_align)
{
	*location = (Type) {
		.type_kind = kind,
		.builtin.bytesize = (bitsize + 7) / 8,
		.builtin.bitsize = bitsize,
		.builtin.min_alignment = align,
		.builtin.pref_alignment = pref_align,
		.name = name,
		.canonical = location,
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


void builtin_setup(Target *target)
{

	/*TODO
 * decl_string = (Decl) { .decl_kind = DECL_BUILTIN, .name.string = "string" };
	create_type(&decl_string, &type_string);
	type_string.type_kind = TYPE_STRING;
*/
#define DEF_TYPE(_name, _shortname, _type, _bits, _align) \
type_create(#_name, &_shortname, &type_ ## _name, _type, _bits, target->align_min_ ## _align, target->align_ ## _align)

	DEF_TYPE(bool, t_u1, TYPE_BOOL, 1, byte);
	DEF_TYPE(float, t_f32, TYPE_F32, 32, float);
	DEF_TYPE(double, t_f64, TYPE_F64, 64, double);

	DEF_TYPE(char, t_i8, TYPE_I8, 8, byte);
	DEF_TYPE(short, t_i16, TYPE_I16, 16, short);
	DEF_TYPE(int, t_i32, TYPE_I32, 32, int);
	DEF_TYPE(long, t_i64, TYPE_I64, 64, long);

	DEF_TYPE(byte, t_u8, TYPE_U8, 8, byte);
	DEF_TYPE(ushort, t_u16, TYPE_U16, 16, short);
	DEF_TYPE(uint, t_u32, TYPE_U32, 32, int);
	DEF_TYPE(ulong, t_u64, TYPE_U64, 64, long);

	DEF_TYPE(void, t_u0, TYPE_VOID, 8, byte);
	DEF_TYPE(string, t_str, TYPE_STRING, target->width_pointer, pointer);

#undef DEF_TYPE

	type_create("void*", &t_voidstar, &type_voidptr, TYPE_POINTER, target->width_pointer, target->align_min_pointer, target->align_pointer);
	create_type_cache(type_void);
	type_void->type_cache[0] = &t_voidstar;
	t_voidstar.pointer = type_void;
	type_create("compint", &t_ixx, &type_compint, TYPE_IXX, 64, 0, 0);
	type_create("compuint", &t_uxx, &type_compuint, TYPE_UXX, 64, 0, 0);
	type_create("compfloat", &t_fxx, &type_compfloat, TYPE_FXX, 64, 0, 0);

	type_create_alias("usize", &t_usz, &type_usize, type_unsigned_int_by_bitsize(target->width_pointer));
	type_create_alias("isize", &t_isz, &type_isize, type_signed_int_by_bitsize(target->width_pointer));

	type_create_alias("c_ushort", &t_cus, &type_c_ushort, type_unsigned_int_by_bitsize(target->width_c_short));
	type_create_alias("c_uint", &t_cui, &type_c_uint, type_unsigned_int_by_bitsize(target->width_c_int));
	type_create_alias("c_ulong", &t_cul, &type_c_ulong, type_unsigned_int_by_bitsize(target->width_c_long));
	type_create_alias("c_ulonglong", &t_cull, &type_c_ulonglong, type_unsigned_int_by_bitsize(target->width_c_long_long));

	type_create_alias("c_short", &t_cs, &type_c_short, type_signed_int_by_bitsize(target->width_c_short));
	type_create_alias("c_int", &t_ci, &type_c_int, type_signed_int_by_bitsize(target->width_c_int));
	type_create_alias("c_long", &t_cl, &type_c_long, type_signed_int_by_bitsize(target->width_c_long));
	type_create_alias("c_longlong", &t_cll, &type_c_longlong, type_signed_int_by_bitsize(target->width_c_long_long));


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
		case TYPE_ERROR:
			return true;
		default:
			return false;
	}
}

typedef enum
{
	L,
	LS,
	R,
	RS,
	FL,
} MaxType;

Type *type_find_max_num_type(Type *num_type, Type *other_num)
{
	if (other_num->type_kind < TYPE_BOOL || other_num->type_kind > TYPE_FXX) return NULL;
	assert(num_type->type_kind >= TYPE_BOOL && num_type->type_kind <= TYPE_FXX);
	static MaxType max_conv[TYPE_FXX - TYPE_BOOL + 1][TYPE_FXX - TYPE_BOOL + 1] = {
		//Bool  I8 I16 I32 I64 IXX U8 U16 U32 U64 UXX  F32  F64 FXX
		{   L,  R,  R,  R,  R,  L,  R,  R,  R,  R,  L,  R,  R, FL }, // Bool
		{   L,  L,  R,  R,  R,  L,  L, RS, RS, RS,  L,  R,  R, FL }, // I8
		{   L,  L,  L,  R,  R,  L,  L,  L, RS, RS,  L,  R,  R, FL }, // I16
		{   L,  L,  L,  L,  R,  L,  L,  L,  L, RS,  L,  R,  R, FL }, // I32
		{   L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  R,  R, FL }, // I64
		{   R,  R,  R,  R,  R,  L, RS, RS, RS, RS,  L,  R,  R,  R }, // IXX
		{   L,  R,  R,  R,  R, LS,  L,  R,  R,  R,  L,  R,  R, FL }, // U8
		{   L, LS,  R,  R,  R, LS,  L,  L,  R,  R,  L,  R,  R, FL }, // U16
		{   L, LS, LS,  R,  R,  L,  L,  L,  L,  R,  L,  R,  R, FL }, // U32
		{   L, LS, LS, LS,  R,  L,  L,  L,  L,  L,  L,  R,  R, FL }, // U64
		{   R,  R,  R,  R,  R,  R,  R,  R,  R,  R,  L,  R,  R,  R }, // UXX
		{   L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  R,  L }, // F32
		{   L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L }, // F32
		{  FL, FL, FL, FL, FL, FL, FL, FL, FL, FL,  L,  R,  R,  L }, // FXX
	};
	MaxType conversion = max_conv[num_type->type_kind - TYPE_BOOL][other_num->type_kind - TYPE_BOOL];
	switch (conversion)
	{
		case L:
			assert (num_type->type_kind != TYPE_FXX);
			return num_type;
		case R:
			assert (other_num->type_kind != TYPE_FXX);
			return other_num;
		case LS:
			return type_signed_int_by_bitsize(num_type->builtin.bytesize * 8);
		case RS:
			return type_signed_int_by_bitsize(other_num->builtin.bytesize * 8);
		case FL:
			return type_double;
		default:
			UNREACHABLE
	}
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
	if (other->type_kind == TYPE_SUBARRAY || other->type_kind == TYPE_VARARRAY)
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

/**
 * Find the maximum vararray type. Due to ordering the other type fullfils
 * other->type_kind >= TYPE_VARARRAY
 *
 * @param type
 * @param other
 * @return maximum type or NULL if none is found.
 */
static inline Type *type_find_max_vararray_type(Type *type, Type *other)
{
	assert(other->canonical != type->canonical && "Expected different types");
	assert(other->type_kind >= type->type_kind && "Expected sorted types");
	switch (other->type_kind)
	{
		case TYPE_VARARRAY:
			// Because of the stride being different, it's not safe to implictly
			// convert one vararray to another. However, it is fine if they are both pointers
			// since the stride is the same.
			if (type->array.base->type_kind == TYPE_POINTER && other->array.base->type_kind == TYPE_POINTER)
			{
				// Jolly nice. Let's create the max from these:
				Type *max_type = type_find_max_ptr_type(type->array.base, other->array.base);
				if (max_type == NULL) return NULL;
				return type_get_array(max_type, 0);
			}
			// If it's not a pointer then there's no real way of converting them.
			return NULL;
		case TYPE_SUBARRAY:
			TODO; // Will return the subarray
		default:
			UNREACHABLE
	}
}

Type *type_find_max_type(Type *type, Type *other)
{
	assert(type->canonical == type);
	assert(other->canonical == other);
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
		case TYPE_POISONED:
		case TYPE_VOID:
			return NULL;
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
			return type_find_max_num_type(type, other);
		case TYPE_POINTER:
			return type_find_max_ptr_type(type, other);
		case TYPE_ENUM:
			// IMPROVE: should there be implicit conversion between one enum and the other in
			// some way?
			return NULL;
		case TYPE_ERROR:
			TODO
		case TYPE_FUNC:
		case TYPE_UNION:
		case TYPE_ERROR_UNION:
		case TYPE_META_TYPE:
			return NULL;
		case TYPE_STRUCT:
			TODO
		case TYPE_TYPEDEF:
			UNREACHABLE
		case TYPE_STRING:
			TODO
		case TYPE_ARRAY:
			return NULL;
		case TYPE_VARARRAY:
			return type_find_max_vararray_type(type, other);
		case TYPE_SUBARRAY:
			TODO
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
		return common ? type_generate_ptr(common, true) : NULL;
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