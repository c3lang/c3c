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
	Type voidstar, typeid, error, typeinfo;
	Type str, varheader, virtual, virtual_generic;

} t;

Type *type_bool = &t.u1;
Type *type_void = &t.u0;
Type *type_voidptr = &t.voidstar;
Type *type_virtual = &t.virtual;
Type *type_virtual_generic = &t.virtual_generic;
Type *type_half = &t.f16;
Type *type_float = &t.f32;
Type *type_double = &t.f64;
Type *type_quad = &t.f128;
Type *type_typeid = &t.typeid;
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
Type *type_compint = &t.ixx;
Type *type_compfloat = &t.fxx;
Type *type_compstr = &t.str;
Type *type_error = &t.error;
Type *type_varheader = &t.varheader;

static unsigned size_subarray;
static AlignSize alignment_subarray;
unsigned size_error_code;
unsigned alignment_error_code;

#define PTR_OFFSET 0
#define INFERRED_ARRAY_OFFSET 1
#define SUB_ARRAY_OFFSET 2
#define ARRAY_OFFSET 3

Type *type_cint(void)
{
	return type_int_signed_by_bitsize(platform_target.width_c_int);
}

Type *type_cuint(void)
{
	return type_int_unsigned_by_bitsize(platform_target.width_c_int);
}

Type *type_int_signed_by_bitsize(unsigned bytesize)
{
	switch (bytesize)
	{
		case 8: return type_ichar;
		case 16: return type_short;
		case 32: return type_int;
		case 64: return type_long;
		case 128: return type_i128;
		default: FATAL_ERROR("Illegal bitsize %d", bytesize);
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
		case TYPE_TYPEDEF:
		case TYPE_STRUCT:
		case TYPE_VOID:
		case TYPE_BOOL:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_UNION:
		case TYPE_ERRTYPE:
		case TYPE_DISTINCT:
		case TYPE_VIRTUAL_ANY:
		case TYPE_VIRTUAL:
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
		case TYPE_POINTER:
			if (type->pointer->type_kind == TYPE_FUNC)
			{
				return type_to_error_string(type->pointer);
			}
			asprintf(&buffer, "%s*", type_to_error_string(type->pointer));
			return buffer;
		case TYPE_STRLIT:
			return "compile time string";
		case TYPE_ARRAY:
			asprintf(&buffer, "%s[%llu]", type_to_error_string(type->array.base), (unsigned long long)type->array.len);
			return buffer;
		case TYPE_INFERRED_ARRAY:
			asprintf(&buffer, "%s[*]", type_to_error_string(type->array.base));
			return buffer;
		case TYPE_SUBARRAY:
			asprintf(&buffer, "%s[]", type_to_error_string(type->array.base));
			return buffer;
		case TYPE_ERR_UNION:
			return "error";
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



ByteSize type_size(Type *type)
{
	switch (type->type_kind)
	{
		case TYPE_DISTINCT:
			return type_size(type->decl->distinct_decl.base_type);
		case TYPE_VECTOR:
			return type_size(type->vector.base) * type->vector.len;
		case TYPE_POISONED:
		case TYPE_TYPEINFO:
		case TYPE_INFERRED_ARRAY:
			UNREACHABLE;
		case TYPE_TYPEDEF:
			return type_size(type->canonical);
		case TYPE_ENUM:
			return type->decl->enums.type_info->type->canonical->builtin.bytesize;
		case TYPE_ERRTYPE:
			return type_size(type_usize->canonical);
		case TYPE_STRUCT:
		case TYPE_UNION:
			assert(type->decl->resolve_status == RESOLVE_DONE);
			return type->decl->strukt.size;
		case TYPE_VOID:
			return 1;
		case TYPE_BOOL:
		case TYPE_TYPEID:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_ERR_UNION:
		case TYPE_VIRTUAL_ANY:
			return type->builtin.bytesize;
		case TYPE_VIRTUAL:
			return type_virtual_generic->builtin.bytesize;
		case TYPE_STRLIT:
		case TYPE_FUNC:
		case TYPE_POINTER:
			return t.usz.canonical->builtin.bytesize;
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



bool type_is_union_struct(Type *type)
{
	TypeKind kind = type->canonical->type_kind;
	return kind == TYPE_STRUCT || kind == TYPE_UNION;
}

bool type_is_empty_field(Type *type, bool allow_array)
{
	type = type_flatten(type);
	if (allow_array)
	{
		while (type->type_kind == TYPE_ARRAY)
		{
			if (type->array.len == 0) return true;
			type = type_flatten(type->array.base);
		}
	}
	return type_is_empty_record(type, allow_array);
}

bool type_is_empty_record(Type *type, bool allow_array)
{
	if (!type_is_union_struct(type)) return false;

	Decl *decl = type->decl;
	if (decl->has_variable_array) return false;

	Decl **members = decl->strukt.members;
	VECEACH(members, i)
	{
		if (!type_is_empty_field(members[i]->type, allow_array)) return false;
	}
	return true;
}

bool type_is_int128(Type *type)
{
	TypeKind kind = type->canonical->type_kind;
	return kind == TYPE_U128 || kind == TYPE_I128;
}

/**
 * Based on isSingleElementStruct in Clang
 */
Type *type_abi_find_single_struct_element(Type *type)
{
	if (!type_is_structlike(type)) return NULL;

	// Elements with a variable array? If so no.
	if (type->decl->has_variable_array) return NULL;

	Type *found = NULL;
	Decl **members = type->decl->strukt.members;
	VECEACH(members, i)
	{
		Type *field_type = type_flatten(members[i]->type);

		// Ignore empty arrays
		if (type_is_empty_field(field_type, true)) continue;

		// Already one field found, not single field.
		if (found) return NULL;

		// Flatten single element arrays.
		while (field_type->type_kind == TYPE_ARRAY)
		{
			if (field_type->array.len != 1) break;
			field_type = field_type->array.base;
		}

		if (type_is_structlike(field_type))
		{
			field_type = type_abi_find_single_struct_element(field_type);
			if (!field_type) return NULL;
		}
		found = field_type;
	}
	// If there is some padding? Then ignore.
	if (found && type_size(type) != type_size(found)) found = NULL;
	return found;
}



bool type_is_abi_aggregate(Type *type)
{
	switch (type->type_kind)
	{
		case TYPE_POISONED:
			return false;
		case TYPE_DISTINCT:
			return type_is_abi_aggregate(type->decl->distinct_decl.base_type);
		case TYPE_TYPEDEF:
			return type_is_abi_aggregate(type->canonical);
		case ALL_FLOATS:
		case TYPE_VOID:
		case ALL_INTS:
		case TYPE_BOOL:
		case TYPE_TYPEID:
		case TYPE_POINTER:
		case TYPE_ENUM:
		case TYPE_FUNC:
		case TYPE_STRLIT:
		case TYPE_VECTOR:
			return false;
		case TYPE_ERRTYPE:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_SUBARRAY:
		case TYPE_ARRAY:
		case TYPE_ERR_UNION:
		case TYPE_VIRTUAL:
		case TYPE_VIRTUAL_ANY:
			return true;
		case TYPE_TYPEINFO:
		case TYPE_INFERRED_ARRAY:
			UNREACHABLE
	}
	UNREACHABLE
}
bool type_is_homogenous_base_type(Type *type)
{
	type = type->canonical;
	switch (platform_target.abi)
	{
		case ABI_PPC64_SVR4:
			switch (type->type_kind)
			{
				case TYPE_F128:
					if (!platform_target.float128) return false;
					FALLTHROUGH;
				case TYPE_F32:
				case TYPE_F64:
					return !platform_target.ppc64.is_softfp;
				case TYPE_VECTOR:
					return type_size(type) == 128 / 8;
				default:
					return false;
			}
		case ABI_X64:
		case ABI_WIN64:
		case ABI_X86:
			switch (type->type_kind)
			{
				case TYPE_F64:
				case TYPE_F32:
					return true;
				case TYPE_VECTOR:
					switch (type_size(type))
					{
						case 16:
						case 32:
						case 64:
							// vec128 256 512 ok
							return true;
						default:
							return false;
					}
				default:
					return false;
			}
		case ABI_AARCH64:
			switch (type->type_kind)
			{
				case ALL_FLOATS:
					return true;
				case TYPE_VECTOR:
					switch (type_size(type))
					{
						case 8:
						case 16:
							// vector 64, 128 => true
							return true;
						default:
							return false;
					}
				default:
					return false;
			}
		case ABI_ARM:
			switch (type->type_kind)
			{
				case TYPE_F32:
				case TYPE_F64:
				case TYPE_F128:
					return true;
				case TYPE_VECTOR:
					switch (type_size(type))
					{
						case 8:
						case 16:
							return true;
						default:
							break;
					}
					FALLTHROUGH;
				default:
					return false;
			}
		case ABI_UNKNOWN:
		case ABI_WASM:
		case ABI_PPC32:
		case ABI_RISCV:
			return false;
	}
	UNREACHABLE
}

bool type_homogenous_aggregate_small_enough(Type *type, unsigned members)
{
	switch (platform_target.abi)
	{
		case ABI_PPC64_SVR4:
			if (type->type_kind == TYPE_F128 && platform_target.float128) return members <= 8;
			if (type->type_kind == TYPE_VECTOR) return members <= 8;
			// Use max 8 registers.
			return ((type_size(type) + 7) / 8) * members <= 8;
		case ABI_X64:
		case ABI_WIN64:
		case ABI_X86:
		case ABI_AARCH64:
		case ABI_ARM:
			return members <= 4;
		case ABI_UNKNOWN:
		case ABI_WASM:
		case ABI_PPC32:
		case ABI_RISCV:
			return false;
	}
	UNREACHABLE
}

/**
 * Calculate whether this is a homogenous aggregate for the ABI.
 * // Based on bool ABIInfo::isHomogeneousAggregate in Clang
 * @param type the (flattened) type to check.
 * @param base the base type of the aggregate
 * @param elements the elements found
 * @return true if it is an aggregate, false otherwise.
 */
bool type_is_homogenous_aggregate(Type *type, Type **base, unsigned *elements)
{
	*elements = 0;
	RETRY:
	switch (type->type_kind)
	{
		case TYPE_DISTINCT:
			type = type->decl->distinct_decl.base_type;
			goto RETRY;
		case TYPE_FXX:
		case TYPE_POISONED:
		case TYPE_IXX:
		case TYPE_VOID:
		case TYPE_TYPEINFO:
		case TYPE_TYPEID:
		case TYPE_FUNC:
		case TYPE_STRLIT:
		case TYPE_SUBARRAY:
		case TYPE_INFERRED_ARRAY:
			return false;
		case TYPE_ERR_UNION:
			DEBUG_LOG("Should error be passed as homogenous aggregate?");
			FALLTHROUGH;
		case TYPE_VIRTUAL:
		case TYPE_VIRTUAL_ANY:
			*base = type_iptr->canonical;
			*elements = 2;
			return true;
		case TYPE_ERRTYPE:
			*base = type_iptr->canonical;
			*elements = 1;
			return true;
		case TYPE_TYPEDEF:
			type = type->canonical;
			goto RETRY;
		case TYPE_STRUCT:
		case TYPE_UNION:
			if (type->decl->has_variable_array) return false;
			*elements = 0;
			{
				Decl **members = type->decl->strukt.members;
				VECEACH(members, i)
				{
					unsigned member_mult = 1;
					// Flatten the type.
					Type *member_type = type_lowering(members[i]->type);
					// Go down deep into  a nester array.
					while (member_type->type_kind == TYPE_ARRAY)
					{
						// If we find a zero length array, this is not allowed.
						if (member_type->array.len == 0) return false;
						member_mult *= member_type->array.len;
						member_type = member_type->array.base;
					}
					unsigned member_members = 0;
					// Skip any empty record.
					if (type_is_empty_record(member_type, true)) continue;

					// Check recursively if the field member is homogenous
					if (!type_is_homogenous_aggregate(member_type, base, &member_members)) return false;
					member_members *= member_mult;
					// In the case of a union, grab the bigger set of elements.
					if (type->type_kind == TYPE_UNION)
					{
						*elements = MAX(*elements, member_members);
					}
					else
					{
						*elements += member_members;
					}
				}
				assert(base);
				if (!*base) return false;

				// Ensure no padding
				if (type_size(*base) * *elements != type_size(type)) return false;
			}
			goto TYPECHECK;
		case TYPE_ARRAY:
			// Empty arrays? Not homogenous.
			if (type->array.len == 0) return false;
			// Check the underlying type and multiply by length.
			if (!type_is_homogenous_aggregate(type->array.base, base, elements)) return false;
			*elements *= type->array.len;
			goto TYPECHECK;
		case TYPE_ENUM:
			type = type->decl->enums.type_info->type;
			goto RETRY;
		case TYPE_BOOL:
			// Lower bool to unsigned char
			type = type_char;
			break;
		case ALL_SIGNED_INTS:
			// Lower signed to unsigned
			type = type_int_unsigned_by_bitsize(type->builtin.bytesize);
			break;
		case ALL_UNSIGNED_INTS:
		case ALL_REAL_FLOATS:
		case TYPE_VECTOR:
			break;
		case TYPE_POINTER:
			// All pointers are the same.
			type = type_voidptr;
			break;
	}
	// The common case:
	*elements = 1;
	// Is it a valid base type?
	if (!type_is_homogenous_base_type(type)) return false;
	// If we don't have a base type yet, set it.
	if (!*base)
	{
		*base = type;
		// Special handling of non-power-of-2 vectors
		if (type->type_kind == TYPE_VECTOR)
		{
			// Widen the type with elements.
			unsigned vec_elements = type_size(type) / type_size(type->vector.base);
			*base = type_get_vector(type->vector.base, vec_elements);
		}
	}
	// One is vector - other isn't => failure
	if (((*base)->type_kind == TYPE_VECTOR) != (type->type_kind == TYPE_VECTOR)) return false;

	// Size does not match => failure
	if (type_size(*base) != type_size(type)) return false;

	TYPECHECK:
	if (*elements == 0) return false;
	return type_homogenous_aggregate_small_enough(type, *elements);
}

AlignSize type_alloca_alignment(Type *type)
{
	if (platform_target.abi == ABI_X64)
	{
		type = type_flatten(type);
		if (type->type_kind == TYPE_ARRAY && type_size(type) >= 16) return 16;
	}
	return type_abi_alignment(type);
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

AlignSize type_abi_alignment(Type *type)
{
	switch (type->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_TYPEINFO:
		case TYPE_INFERRED_ARRAY:
			UNREACHABLE;
		case TYPE_VECTOR:
			TODO
		case TYPE_VOID:
			return 1;
		case TYPE_DISTINCT:
			return type_abi_alignment(type->decl->distinct_decl.base_type);
		case TYPE_TYPEDEF:
			return type_abi_alignment(type->canonical);
		case TYPE_ENUM:
			return type->decl->enums.type_info->type->canonical->builtin.abi_alignment;
		case TYPE_ERRTYPE:
			return t.usz.canonical->builtin.abi_alignment;
		case TYPE_STRUCT:
		case TYPE_UNION:
			return type->decl->alignment;
		case TYPE_TYPEID:
			return type_abi_alignment(type_usize);
		case TYPE_BOOL:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_ERR_UNION:
		case TYPE_VIRTUAL_ANY:
			return type->builtin.abi_alignment;
		case TYPE_VIRTUAL:
			return type_virtual_generic->builtin.abi_alignment;
		case TYPE_FUNC:
		case TYPE_POINTER:
		case TYPE_STRLIT:
			return t.usz.canonical->builtin.abi_alignment;
		case TYPE_ARRAY:
			return type_abi_alignment(type->array.base);
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



Type *type_get_ptr(Type *ptr_type)
{
	return type_generate_ptr(ptr_type, false);
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

	ByteSize len = array->array.len;
	if (!len) return type_size(type) == 0;

	Type *base = array->array.base;

	if (len == 1 && type_is_structurally_equivalent(base, type)) return true;

	assert(type->type_kind != TYPE_UNION && "Does not work on unions");

	if (!type_is_structlike(type)) return false;

	Decl **members = type->decl->strukt.members;

	// For structs / errors, all members must match.
	ArrayIndex  offset = 0;
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

	if (type_size(type1) != type_size(type2)) return false;

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


static Type *type_create_array(Type *element_type, uint64_t len, bool vector, bool canonical)
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
		vec_arr = type_new(TYPE_VECTOR, strformat("%s[<%llu>]", element_type->name, len));
		vec_arr->vector.base = element_type;
		vec_arr->vector.len = len;
	}
	else
	{
		vec_arr = type_new(TYPE_ARRAY, strformat("%s[%llu]", element_type->name, len));
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
	VECADD(element_type->type_cache, vec_arr);
	return vec_arr;
}

Type *type_get_array(Type *arr_type, ByteSize len)
{
	return type_create_array(arr_type, len, false, false);
}

Type *type_get_vector(Type *vector_type, unsigned len)
{
	return type_create_array(vector_type, len, true, false);
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
			scratch_buffer_append(type->decl->external_name);
			break;
		case TYPE_POINTER:
			type_append_name_to_scratch(type->pointer);
			scratch_buffer_append_char('*');
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
		case TYPE_ERR_UNION:
		case TYPE_VIRTUAL_ANY:
		case TYPE_VECTOR:
			scratch_buffer_append(type->name);
			break;
		case TYPE_IXX:
		case TYPE_FXX:
		case TYPE_STRLIT:
		case TYPE_INFERRED_ARRAY:
		case TYPE_TYPEINFO:
			UNREACHABLE
			break;
		case TYPE_FUNC:
			scratch_buffer_append(type->func.mangled_function_signature);
			break;
		case TYPE_ARRAY:
			TODO
		case TYPE_VIRTUAL:
			scratch_buffer_append("virtual ");
			scratch_buffer_append(type->decl->name);
	}
}

Type *type_find_function_type(FunctionSignature *signature)
{
	scratch_buffer_clear();
	type_append_name_to_scratch(signature->rtype->type);
	if (signature->failable) scratch_buffer_append_char('!');
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
	if (signature->variadic && elements > 0)
	{
		scratch_buffer_append_char(',');
	}
	if (signature->variadic || signature->typed_variadic)
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


void type_setup(PlatformTarget *target)
{
	stable_init(&function_types, 0x1000);

	/*TODO
 * decl_string = (Decl) { .decl_kind = DECL_BUILTIN, .name.string = "string" };
	create_type(&decl_string, &type_string);
	type_string.type_kind = TYPE_STRING;
*/
#define DEF_TYPE(name_, shortname_, type_, bits_, aligned_) \
type_create(#name_, &(shortname_), type_, bits_, target->align_ ## aligned_, target->align_pref_ ## aligned_)

	DEF_TYPE(bool, t.u1, TYPE_BOOL, 1, byte);
	DEF_TYPE(float, t.f32, TYPE_F32, 32, float);
	DEF_TYPE(double, t.f64, TYPE_F64, 64, double);

	DEF_TYPE(ichar, t.i8, TYPE_I8, 8, byte);
	DEF_TYPE(short, t.i16, TYPE_I16, 16, short);
	DEF_TYPE(int, t.i32, TYPE_I32, 32, int);
	DEF_TYPE(long, t.i64, TYPE_I64, 64, long);
	DEF_TYPE(i128, t.i128, TYPE_I128, 128, i128);

	DEF_TYPE(char, t.u8, TYPE_U8, 8, byte);
	DEF_TYPE(ushort, t.u16, TYPE_U16, 16, short);
	DEF_TYPE(uint, t.u32, TYPE_U32, 32, int);
	DEF_TYPE(ulong, t.u64, TYPE_U64, 64, long);
	DEF_TYPE(u128, t.u128, TYPE_U128, 128, i128);

	DEF_TYPE(void, t.u0, TYPE_VOID, 8, byte);
	DEF_TYPE(string, t.str, TYPE_STRLIT, target->width_pointer, pointer);

#undef DEF_TYPE

	type_create("typeinfo", &t.typeinfo, TYPE_TYPEINFO, 0, 0, 0);
	type_create("typeid", &t.typeid, TYPE_TYPEID, target->width_pointer, target->align_pref_pointer, target->align_pointer);
	type_create("void*", &t.voidstar, TYPE_POINTER, target->width_pointer, target->align_pref_pointer, target->align_pointer);
	create_type_cache(type_void);
	type_void->type_cache[0] = &t.voidstar;
	t.voidstar.pointer = type_void;
	type_create("virtual*", &t.virtual, TYPE_VIRTUAL_ANY, target->width_pointer * 2, target->align_pref_pointer, target->align_pointer);
	type_create("virtual_generic", &t.virtual_generic, TYPE_VIRTUAL, target->width_pointer * 2, target->align_pref_pointer, target->align_pointer);

	type_create("compint", &t.ixx, TYPE_IXX, 0, 0, 0);
	type_create("compfloat", &t.fxx, TYPE_FXX, 0, 0, 0);

	type_create_alias("usize", &t.usz, type_int_unsigned_by_bitsize(target->width_pointer));
	type_create_alias("isize", &t.isz, type_int_signed_by_bitsize(target->width_pointer));

	type_create_alias("uptr", &t.uptr, type_int_unsigned_by_bitsize(target->width_pointer));
	type_create_alias("iptr", &t.iptr, type_int_signed_by_bitsize(target->width_pointer));

	type_create_alias("uptrdiff", &t.uptrdiff, type_int_unsigned_by_bitsize(target->width_pointer));
	type_create_alias("iptrdiff", &t.iptrdiff, type_int_signed_by_bitsize(target->width_pointer));

	alignment_subarray = MAX(type_abi_alignment(&t.voidstar), type_abi_alignment(t.usz.canonical));
	size_subarray = alignment_subarray * 2;
	type_create("error", &t.error, TYPE_ERR_UNION, target->width_pointer * 2, target->align_pointer, target->align_pref_pointer);
}

bool type_is_scalar(Type *type)
{
	RETRY:
	switch (type->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_TYPEINFO:
		case TYPE_INFERRED_ARRAY:
			UNREACHABLE
		case TYPE_VOID:
		case TYPE_FUNC:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_ARRAY:
		case TYPE_SUBARRAY:
		case TYPE_VECTOR:
			return false;
		case TYPE_BOOL:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_STRLIT:
		case TYPE_TYPEID:
		case TYPE_POINTER:
		case TYPE_ENUM:
		case TYPE_ERRTYPE:
		case TYPE_ERR_UNION:
		case TYPE_VIRTUAL:
		case TYPE_VIRTUAL_ANY:
			return true;
		case TYPE_DISTINCT:
			type = type->decl->distinct_decl.base_type;
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
		case TOKEN_ERR:
			return type_error;
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
		case TOKEN_I128:
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
		case TOKEN_U128:
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
	if (other_kind < TYPE_I8 || other_kind > TYPE_FXX) return NULL;

	// 2. First check the float case.
	if (other_kind >= TYPE_F16 && other_kind <= TYPE_FXX)
	{
		switch (other_kind)
		{
			case TYPE_FXX:
				return kind <= TYPE_IXX ? type_double : num_type;
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

	// 3. If the other type is IXX, return the current type.
	if (other_kind == TYPE_IXX) return num_type;

	// 4. Check the bit sizes.
	unsigned other_bit_size = other_num->builtin.bitsize;
	unsigned bit_size = num_type->builtin.bitsize;

	// 5. The other type is unsigned
	if (type_kind_is_unsigned(other_kind))
	{
		if (type_kind_is_signed(kind))
		{
			// 5a. Signed + Unsigned -> Signed
			return bit_size >= other_bit_size ? num_type : NULL;
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
		case TYPE_INFERRED_ARRAY:
		case TYPE_POISONED:
			UNREACHABLE
		case TYPE_VOID:
		case TYPE_BOOL:
		case TYPE_TYPEINFO:
		case TYPE_VIRTUAL:
		case TYPE_VIRTUAL_ANY:
			return NULL;
		case TYPE_IXX:
			if (other->type_kind == TYPE_DISTINCT && type_is_numeric(other->decl->distinct_decl.base_type)) return other;
			FALLTHROUGH;
		case ALL_SIGNED_INTS:
		case ALL_UNSIGNED_INTS:
			if (other->type_kind == TYPE_ENUM) return type_find_max_type(type, other->decl->enums.type_info->type->canonical);
			return type_find_max_num_type(type, other);
		case TYPE_FXX:
			if (other->type_kind == TYPE_DISTINCT && type_is_float(other->decl->distinct_decl.base_type)) return other;
			FALLTHROUGH;
		case ALL_REAL_FLOATS:
			return type_find_max_num_type(type, other);
		case TYPE_POINTER:
			return type_find_max_ptr_type(type, other);
		case TYPE_ENUM:
			// IMPROVE: should there be implicit conversion between one enum and the other in
			// some way?
			return NULL;
		case TYPE_ERRTYPE:
			if (other->type_kind == TYPE_ERRTYPE) return type_error;
			return NULL;
		case TYPE_FUNC:
		case TYPE_UNION:
		case TYPE_ERR_UNION:
		case TYPE_TYPEID:
		case TYPE_STRUCT:
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