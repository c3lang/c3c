// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "c_abi_internal.h"

ABIArgInfo *aarch64_illegal_vector(Type *type)
{
	// Need to look up SVE vectors.
	return false;
}

ABIArgInfo *aarch64_coerce_illegal_vector(Type *type)
{
	TODO
}

ABIArgInfo *aarch64_classify_argument_type(Type *type)
{
	type = type_lowering(type);

	if (type->type_kind == TYPE_VOID) return abi_arg_ignore();

	if (type->type_kind == TYPE_VECTOR && aarch64_illegal_vector(type))
	{
		return aarch64_coerce_illegal_vector(type);
	}

	TypeSize size = type_size(type);

	if (!type_is_abi_aggregate(type))
	{
		// Over 128 bits should be indirect, but
		// we don't have that (yet?)
		if (type_is_promotable_integer(type) && platform_target.aarch64.is_darwin_pcs)
		{
			return abi_arg_new_direct_int_ext(type);
		}
		return abi_arg_new_direct();
	}

	// Is empty
	if (!size) return abi_arg_ignore();

	// Homogeneous Floating-point Aggregates (HFAs) need to be expanded.
	Type *base = NULL;
	unsigned members = 0;
	if (type_is_homogenous_aggregate(type, &base, &members))
	{
		assert(members < 128);
		return abi_arg_new_direct_coerce_array_type(base, (int8_t)members);
	}

	// Aggregates <= in registers
	if (size <= 16)
	{
		// For RenderScript <= 16 needs to be coerced.
		AlignSize alignment = type_abi_alignment(type);
		if (platform_target.aarch64.is_aapcs)
		{
			alignment = alignment < 16 ? 8 : 16;
		}
		else
		{
			if (alignment < type_abi_alignment(type_voidptr))
			{
				alignment = type_abi_alignment(type_voidptr);
			}
		}
		size = aligned_offset(size, alignment);
		// We use a pair of i64 for 16-byte aggregate with 8-byte alignment.
		// For aggregates with 16-byte alignment, we use i128.
		assert(alignment == 8 || alignment == 16);
		assert(size / alignment < 128);
		return abi_arg_new_direct_coerce_array_type(alignment == 8 ? type_ulong : type_u128, (int8_t)(size / alignment));
	}

	return abi_arg_new_indirect_not_by_val(type);
}

ABIArgInfo *aarch64_classify_return_type(Type *type, bool variadic)
{
	type = type_lowering(type);

	if (type->type_kind == TYPE_VOID) return abi_arg_ignore();

	if (type->type_kind == TYPE_VECTOR && aarch64_illegal_vector(type))
	{
		return aarch64_coerce_illegal_vector(type);
	}

	TypeSize size = type_size(type);

	// Large vectors by mem.
	if (type->type_kind == TYPE_VECTOR && size > 16)
	{
		return abi_arg_new_direct_coerce_type(type);
	}

	if (!type_is_abi_aggregate(type))
	{
		if (type_is_promotable_integer(type) && platform_target.aarch64.is_darwin_pcs)
		{
			return abi_arg_new_direct_int_ext(type);
		}
		return abi_arg_new_direct();
	}

	// Abi aggregate:

	// Is empty
	if (!size) return abi_arg_ignore();

	Type *base = NULL;
	unsigned members = 0;
	if (type_is_homogenous_aggregate(type, &base, &members) &&
	    !(platform_target.arch == ARCH_TYPE_AARCH64_32 && variadic))
	{
		return abi_arg_new_direct();
	}

	// Aggregates <= in registers
	if (size <= 16)
	{
		// For RenderScript <= 16 needs to be coerced.
		unsigned alignment = type_abi_alignment(type);
		// Align to multiple of 8.
		unsigned aligned_size = aligned_offset(size, 8);
		if (alignment < 16 && size == 16)
		{
			return abi_arg_new_direct_coerce_type(type_get_array(type_ulong, size / 8));
		}
		return abi_arg_new_direct_coerce_type(type_int_unsigned_by_bitsize(aligned_size * 8));
	}

	return abi_arg_new_indirect_by_val(type);
}


void c_abi_func_create_aarch64(FunctionPrototype *prototype)
{

	prototype->ret_abi_info = aarch64_classify_return_type(prototype->abi_ret_type, prototype->variadic == VARIADIC_RAW);
	if (prototype->ret_by_ref)
	{
		prototype->ret_by_ref_abi_info = aarch64_classify_argument_type(type_get_ptr(type_flatten(prototype->ret_by_ref_type)));
	}

	Type **params = prototype->params;
	unsigned param_count = vec_size(prototype->params);
	if (param_count)
	{
		ABIArgInfo **args = MALLOC(sizeof(ABIArgInfo) * param_count);
		for (unsigned i = 0; i < param_count; i++)
		{
			args[i] = aarch64_classify_argument_type(params[i]);
		}
		prototype->abi_args = args;
	}
}
