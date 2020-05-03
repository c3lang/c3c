// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"

static Type t_u0, t_str, t_u1, t_i8, t_i16, t_i32, t_i64, t_ixx;
static Type t_u8, t_u16, t_u32, t_u64;
static Type t_f32, t_f64, t_fxx;
static Type t_usz, t_isz;
static Type t_cus, t_cui, t_cul, t_cull;
static Type t_cs, t_ci, t_cl, t_cll;
static Type t_voidstar, t_typeid, t_error_union;

Type *type_bool = &t_u1;
Type *type_void = &t_u0;
Type *type_string = &t_str;
Type *type_voidptr = &t_voidstar;
Type *type_float = &t_f32;
Type *type_double = &t_f64;
Type *type_typeid = &t_typeid;
Type *type_char = &t_i8;
Type *type_short = &t_i16;
Type *type_int = &t_i32;
Type *type_long = &t_i64;
Type *type_isize = &t_isz;
Type *type_byte = &t_u8;
Type *type_ushort = &t_u16;
Type *type_uint = &t_u32;
Type *type_ulong = &t_u64;
Type *type_usize = &t_usz;
Type *type_compint = &t_ixx;
Type *type_compfloat = &t_fxx;
Type *type_c_short = &t_cs;
Type *type_c_int = &t_ci;
Type *type_c_long = &t_cl;
Type *type_c_longlong = &t_cll;
Type *type_c_ushort = &t_cus;
Type *type_c_uint = &t_cui;
Type *type_c_ulong = &t_cul;
Type *type_c_ulonglong = &t_cull;
Type *type_error_union = &t_error_union;
Type *type_error_base = &t_ci;

static unsigned size_subarray;
static unsigned alignment_subarray;
unsigned size_error_code;
unsigned alignment_error_code;

#define PTR_OFFSET 0
#define VAR_ARRAY_OFFSET 1
#define ARRAY_OFFSET 2

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
			return "error";
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
		case TYPE_TYPEDEF:
			UNREACHABLE;
		case TYPE_ENUM:
			return canonical->decl->enums.type_info->type->canonical->builtin.bytesize;
		case TYPE_ERROR:
			return alignment_error_code;
		case TYPE_STRUCT:
		case TYPE_UNION:
			return canonical->decl->strukt.size;
		case TYPE_VOID:
			return 1;
		case TYPE_BOOL:
		case TYPE_META_TYPE:
		case ALL_INTS:
		case ALL_FLOATS:
			return canonical->builtin.bytesize;
		case TYPE_FUNC:
		case TYPE_POINTER:
		case TYPE_VARARRAY:
		case TYPE_STRING:
		case TYPE_ERROR_UNION:
			return t_usz.canonical->builtin.bytesize;
		case TYPE_ARRAY:
			return type_size(canonical->array.base) * canonical->array.len;
		case TYPE_SUBARRAY:
			return size_subarray;
	}
	UNREACHABLE
}

unsigned int type_abi_alignment(Type *canonical)
{
	assert(canonical && canonical->canonical == canonical);
	switch (canonical->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_TYPEDEF:
		case TYPE_VOID:
			UNREACHABLE;
		case TYPE_ENUM:
			return canonical->decl->enums.type_info->type->canonical->builtin.abi_alignment;
		case TYPE_ERROR:
			return alignment_error_code;
		case TYPE_STRUCT:
		case TYPE_UNION:
			return canonical->decl->strukt.abi_alignment;
		case TYPE_META_TYPE:
		case TYPE_BOOL:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_ERROR_UNION:
			return canonical->builtin.abi_alignment;
		case TYPE_FUNC:
		case TYPE_POINTER:
		case TYPE_VARARRAY:
		case TYPE_STRING:
			return t_usz.canonical->builtin.abi_alignment;
		case TYPE_ARRAY:
			return type_abi_alignment(canonical->array.base);
		case TYPE_SUBARRAY:
			return alignment_subarray;
	}
	UNREACHABLE
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



Type *type_get_ptr(Type *ptr_type)
{
	return type_generate_ptr(ptr_type, false);
}

Type *type_get_indexed_type(Type *type)
{
	switch (type->type_kind)
	{
		case TYPE_POINTER:
			return type->pointer;
		case TYPE_VARARRAY:
		case TYPE_ARRAY:
		case TYPE_SUBARRAY:
			return type->array.base;
		case TYPE_STRING:
			return type_char;
		default:
			break;
	}
	Type *canonical = type->canonical;
	if (canonical != type) return type_get_indexed_type(type);
	return NULL;

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
	array->array.len = len;
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

static void type_create(const char *name, Type *location, TypeKind kind, unsigned bitsize,
                        unsigned align, unsigned pref_align)
{
	*location = (Type) {
		.type_kind = kind,
		.builtin.bytesize = (bitsize + 7) / 8,
		.builtin.bitsize = bitsize,
		.builtin.abi_alignment = align,
		.builtin.pref_alignment = pref_align,
		.name = name,
		.canonical = location,
	};
	location->name = name;
	location->canonical = location;
}

static void type_create_alias(const char *name, Type *location, Type *canonical)
{
	*location = (Type) {
		.type_kind = TYPE_TYPEDEF,
		.name = name,
		.canonical = canonical
	};
}


void builtin_setup(Target *target)
{

	/*TODO
 * decl_string = (Decl) { .decl_kind = DECL_BUILTIN, .name.string = "string" };
	create_type(&decl_string, &type_string);
	type_string.type_kind = TYPE_STRING;
*/
#define DEF_TYPE(_name, _shortname, _type, _bits, _align) \
type_create(#_name, &_shortname, _type, _bits, target->align_ ## _align, target->align_pref_ ## _align)

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

	type_create("typeid", &t_typeid, TYPE_META_TYPE, target->width_pointer, target->align_pref_pointer, target->align_pointer);
	type_create("void*", &t_voidstar, TYPE_POINTER, target->width_pointer, target->align_pref_pointer, target->align_pointer);
	create_type_cache(type_void);
	type_void->type_cache[0] = &t_voidstar;
	t_voidstar.pointer = type_void;
	type_create("compint", &t_ixx, TYPE_IXX, 32, 0, 0);
	type_create("compfloat", &t_fxx, TYPE_FXX, 64, 0, 0);

	type_create_alias("usize", &t_usz, type_unsigned_int_by_bitsize(target->width_pointer));
	type_create_alias("isize", &t_isz, type_signed_int_by_bitsize(target->width_pointer));

	type_create_alias("c_ushort", &t_cus, type_unsigned_int_by_bitsize(target->width_c_short));
	type_create_alias("c_uint", &t_cui, type_unsigned_int_by_bitsize(target->width_c_int));
	type_create_alias("c_ulong", &t_cul, type_unsigned_int_by_bitsize(target->width_c_long));
	type_create_alias("c_ulonglong", &t_cull, type_unsigned_int_by_bitsize(target->width_c_long_long));

	type_create_alias("c_short", &t_cs, type_signed_int_by_bitsize(target->width_c_short));
	type_create_alias("c_int", &t_ci, type_signed_int_by_bitsize(target->width_c_int));
	type_create_alias("c_long", &t_cl, type_signed_int_by_bitsize(target->width_c_long));
	type_create_alias("c_longlong", &t_cll, type_signed_int_by_bitsize(target->width_c_long_long));

	alignment_subarray = MAX(type_abi_alignment(&t_voidstar), type_abi_alignment(t_usz.canonical));
	size_subarray = alignment_subarray * 2;
	type_create("error", &t_error_union, TYPE_ERROR_UNION, target->width_pointer, target->align_pointer, target->align_pref_pointer);
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
	R,
	FL,
	X,
} MaxType;

Type *type_find_max_num_type(Type *num_type, Type *other_num)
{
	if (other_num->type_kind < TYPE_I8 || other_num->type_kind > TYPE_FXX) return NULL;
	assert(num_type->type_kind >= TYPE_I8 && num_type->type_kind <= TYPE_FXX);
	static MaxType max_conv[TYPE_FXX - TYPE_I8 + 1][TYPE_FXX - TYPE_BOOL + 1] = {
		// I8 I16 I32 I64  U8 U16 U32 U64 IXX F32 F64 FXX
		{   L,  R,  R,  R,  X,  X,  X,  X,  L,  R,  R,  FL }, // I8
		{   L,  L,  R,  R,  L,  X,  X,  X,  L,  R,  R,  FL }, // I16
		{   L,  L,  L,  R,  L,  L,  X,  X,  L,  R,  R,  FL }, // I32
		{   L,  L,  L,  L,  L,  L,  L,  X,  L,  R,  R,  FL }, // I64
		{   X,  R,  R,  R,  L,  R,  R,  R,  L,  R,  R,  FL }, // U8
		{   X,  X,  R,  R,  L,  L,  R,  R,  L,  R,  R,  FL }, // U16
		{   X,  X,  X,  R,  L,  L,  L,  R,  L,  R,  R,  FL }, // U32
		{   X,  X,  X,  X,  L,  L,  L,  L,  L,  R,  R,  FL }, // U64
		{   R,  R,  R,  R,  R,  R,  R,  R,  L,  R,  R,  R  }, // IXX
		{   L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  R,  L  }, // F32
		{   L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L  }, // F64
		{  FL, FL, FL, FL, FL, FL, FL, FL, FL,  R,  R,  L  }, // FXX
	};
	MaxType conversion = max_conv[num_type->type_kind - TYPE_I8][other_num->type_kind - TYPE_I8];
	switch (conversion)
	{
		case X:
			return NULL;
		case L:
			return num_type;
		case R:
			return other_num;
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
		case TYPE_BOOL:
			return NULL;
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
		case TYPE_IXX:
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
			if (other->type_kind == TYPE_ENUM) return type_find_max_type(type, other->decl->enums.type_info->type->canonical);
			FALLTHROUGH;
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
			if (other->type_kind == TYPE_ERROR) return type_error_union;
			return NULL;
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