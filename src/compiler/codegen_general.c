#include "codegen_internal.h"


const char * const benchmark_fns_var_name = "__$C3_BENCHMARK_FN_LIST";
const char * const benchmark_names_var_name = "__$C3_BENCHMARK_NAMES_LIST";
const char * const test_fns_var_name = "__$C3_TEST_FN_LIST";
const char * const test_names_var_name = "__$C3_TEST_NAMES_LIST";
/**
 * Based on isSingleElementStruct in Clang
 */
Type *type_abi_find_single_struct_element(Type *type, bool in_abi)
{
	if (!type_is_union_or_strukt(type)) return NULL;

	// Elements with a variable array? If so no.
	if (type->decl->has_variable_array) return NULL;

	Type *found = NULL;
	FOREACH(Decl *, member, type->decl->strukt.members)
	{
		// Already one field found, not single field.
		if (found) return NULL;

		Type *field_type = type_lowering(member->type);

		// Flatten single element arrays.
		while (field_type->type_kind == TYPE_ARRAY)
		{
			if (field_type->array.len != 1) break;
			field_type = field_type->array.base;
		}

		if (type_is_union_or_strukt(field_type))
		{
			field_type = type_abi_find_single_struct_element(field_type, in_abi);
			if (!field_type) return NULL;
		}
		found = field_type;
	}
	// If there is some padding? Then ignore.
	if (found && type_size(type) != type_size(found)) found = NULL;
	return found;
}

bool type_is_homogenous_base_type(Type *type)
{
	type = type->canonical;
	switch (compiler.platform.abi)
	{
		case ABI_PPC64_SVR4:
			switch (type->type_kind)
			{
				case TYPE_F128:
					if (!compiler.platform.float128) return false;
					FALLTHROUGH;
				case TYPE_F32:
				case TYPE_F64:
					return !compiler.platform.ppc64.is_softfp;
				case TYPE_SIMD_VECTOR:
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
				case TYPE_SIMD_VECTOR:
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
				case TYPE_SIMD_VECTOR:
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
				case TYPE_SIMD_VECTOR:
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
		case ABI_XTENSA:
			return false;
	}
	UNREACHABLE
}


bool type_homogenous_aggregate_small_enough(Type *type, unsigned members)
{
	switch (compiler.platform.abi)
	{
		case ABI_PPC64_SVR4:
			if (type->type_kind == TYPE_F128 && compiler.platform.float128) return members <= 8;
			if (type->type_kind == TYPE_SIMD_VECTOR) return members <= 8;
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
		case ABI_XTENSA:
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
bool type_is_homogenous_aggregate(LoweredType *type, Type **base, unsigned *elements)
{
	ASSERT(base && type && elements);
	*elements = 0;
	switch (type->type_kind)
	{
		case LOWERED_TYPES:
			UNREACHABLE
		case TYPE_VECTOR:
			// Converted in ABI
			UNREACHABLE
		case TYPE_VOID:
		case TYPE_FUNC_RAW:
		case TYPE_SLICE:
			return false;
		case TYPE_ANY:
			*base = type_iptr->canonical;
			*elements = 2;
			return true;
		case TYPE_STRUCT:
		case TYPE_UNION:
			if (type->decl->has_variable_array) return false;
			*elements = 0;
			{
				FOREACH(Decl *, member, type->decl->strukt.members)
				{
					unsigned member_mult = 1;
					// Flatten the type.
					LoweredType *member_type = lowered_member_type(member);
					// Go down deep into  a nester array.
					while (member_type->type_kind == TYPE_ARRAY)
					{
						ASSERT(member_type->array.len && "Zero length arrays not allowed");
						member_mult *= member_type->array.len;
						member_type = member_type->array.base;
					}
					unsigned member_members = 0;

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
				ASSERT(base);
				if (!*base) return false;

				// Ensure no padding
				if (type_size(*base) * *elements != type_size(type)) return false;
			}
			goto TYPECHECK;
		case TYPE_FLEXIBLE_ARRAY:
			// Same with scaled vectors
			return false;
		case TYPE_ARRAY:
			// Empty arrays? Not homogenous.
			if (type->array.len == 0) return false;
			// Check the underlying type and multiply by length.
			if (!type_is_homogenous_aggregate(lowered_array_element_type(type), base, elements)) return false;
			*elements *= type->array.len;
			goto TYPECHECK;
		case TYPE_BOOL:
			// Lower bool to unsigned char
			type = type_char;
			break;
		case ALL_SIGNED_INTS:
			// Lower signed to unsigned
			type = type_int_unsigned_by_bitsize(type->builtin.bitsize);
			break;
		case ALL_UNSIGNED_INTS:
		case ALL_FLOATS:
		case TYPE_SIMD_VECTOR:
			break;
		case TYPE_POINTER:
		case TYPE_FUNC_PTR:
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
		// If we allowed it
		/*
		if (type->type_kind == TYPE_SIMD_VECTOR)
		{
			// Widen the type with elements.
			unsigned vec_elements = type_size(type) / type_size(type->array.base);
			*base = type_get_vector(type->array.base, vec_elements);
		}
		*/
	}
	// One is vector - other isn't => failure
	if (((*base)->type_kind == TYPE_SIMD_VECTOR) != (type->type_kind == TYPE_SIMD_VECTOR)) return false;

	// Size does not match => failure
	if (type_size(*base) != type_size(type)) return false;

	TYPECHECK:
	if (*elements == 0) return false;
	return type_homogenous_aggregate_small_enough(type, *elements);
}



bool codegen_single_obj_output()
{
	if (!compiler.build.output_name) return false;
	if (compiler.build.type != TARGET_TYPE_OBJECT_FILES) return false;
	return compiler.build.single_module == SINGLE_MODULE_ON;
}

void codegen_setup_object_names(Module *module, const char **base_name, const char **ir_filename, const char **asm_filename, const char **object_filename)
{
	const char *result = module_create_object_file_name(module);
	*base_name = scratch_buffer_copy();
	assert(compiler.build.object_file_dir);
	if (codegen_single_obj_output())
	{
		const char *res = compiler.build.output_name;
		if (!res) res = compiler.build.name;
		if (!res) res = result;
		const char *ext = get_object_extension();
		if (!str_has_suffix(res, ext))
		{
			res = str_printf("%s%s", result, ext);
		}
		compiler.obj_output = *object_filename = (file_path_is_relative(res) ? file_append_path(compiler.build.output_dir, res) : res);
		char *dir_path = NULL;
		char *filename = NULL;
		file_get_dir_and_filename_from_full(compiler.obj_output, &filename, &dir_path);
		if (dir_path && strlen(dir_path) && !file_is_dir(dir_path))
		{
			error_exit("Can't output '%s', the directory '%s' could not be found.", compiler.obj_output, dir_path);
		}
	}
	else
	{
		*object_filename = file_append_path(compiler.build.object_file_dir, str_printf("%s%s", result, get_object_extension()));
	}

	*ir_filename = file_append_path(compiler.build.ir_file_dir, str_printf(compiler.build.backend == BACKEND_LLVM ? "%s.ll" : "%s.ir", result));
	if (compiler.build.emit_asm)
	{
		*asm_filename = str_printf("%s.s", result);
		if (compiler.build.asm_file_dir) *asm_filename = file_append_path(compiler.build.asm_file_dir, *asm_filename);
	}
}
