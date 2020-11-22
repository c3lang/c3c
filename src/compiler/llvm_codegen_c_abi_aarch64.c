// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_c_abi_internal.h"

ABIArgInfo *aarch64_illegal_vector(Type *type)
{
	// Need to look up SVE vectors.
	return false;
}
ABIArgInfo *aarch64_coerce_illegal_vector(Type *type)
{
	TODO
}

ABIArgInfo *aarch64_classify_argument_type(GenContext *context, Type *type)
{
	type = type_lowering(type);

	if (type->type_kind == TYPE_VOID) return abi_arg_new(ABI_ARG_IGNORE);

	if (type->type_kind == TYPE_VECTOR && aarch64_illegal_vector(type))
	{
		return aarch64_coerce_illegal_vector(type);
	}

	size_t size = type_size(type);

	if (!type_is_abi_aggregate(type))
	{
		// Over 128 bits should be indirect, but
		// we don't have that (yet?)
		if (type_is_promotable_integer(type) && build_target.aarch64.is_darwin_pcs)
		{
			return abi_arg_new_direct_int_ext(type);
		}
		return abi_arg_new(ABI_ARG_DIRECT_COERCE);
	}

	// Is empty
	if (!size) return abi_arg_new(ABI_ARG_IGNORE);

	// Homogeneous Floating-point Aggregates (HFAs) need to be expanded.
	Type *base = NULL;
	unsigned members = 0;
	if (type_is_homogenous_aggregate(type, &base, &members))
	{
		ABIArgInfo *info = abi_arg_new_direct_coerce(abi_type_new_plain(base));
		info->direct_coerce.elements = members;
		return info;
	}

	// Aggregates <= in registers
	if (size <= 16)
	{
		// For RenderScript <= 16 needs to be coerced.
		unsigned alignment = type_abi_alignment(type);
		if (build_target.aarch64.is_aapcs)
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
		ABIArgInfo *info = abi_arg_new_direct_coerce(abi_type_new_int_bits(alignment * 8));
		info->direct_coerce.elements = size / alignment;
		return info;
	}

	return abi_arg_new_indirect_not_by_val();
}

ABIArgInfo *aarch64_classify_return_type(GenContext *context, Type *type, bool variadic)
{
	type = type_lowering(type);

	if (type->type_kind == TYPE_VOID) return abi_arg_new(ABI_ARG_IGNORE);

	if (type->type_kind == TYPE_VECTOR && aarch64_illegal_vector(type))
	{
		return aarch64_coerce_illegal_vector(type);
	}

	size_t size = type_size(type);

	// Large vectors by mem.
	if (type->type_kind == TYPE_VECTOR && size > 16)
	{
		return abi_arg_new_direct_coerce(abi_type_new_plain(type));
	}

	if (!type_is_abi_aggregate(type))
	{
		if (type_is_promotable_integer(type) && build_target.aarch64.is_darwin_pcs)
		{
			return abi_arg_new_direct_int_ext(type);
		}
		return abi_arg_new(ABI_ARG_DIRECT_COERCE);
	}

	// Abi aggregate:

	// Is empty
	if (!size) return abi_arg_new(ABI_ARG_IGNORE);

	Type *base = NULL;
	unsigned members = 0;
	if (type_is_homogenous_aggregate(type, &base, &members) &&
	    !(build_target.arch == ARCH_TYPE_AARCH64_32 && variadic))
	{
		return abi_arg_new(ABI_ARG_DIRECT_COERCE);
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
			return abi_arg_new_direct_coerce(abi_type_new_plain(type_get_array(type_ulong, size / 8)));
		}
		return abi_arg_new_direct_coerce(abi_type_new_int_bits(aligned_size * 8));
	}

	return abi_arg_new_indirect_by_val();
}


void c_abi_func_create_aarch64(GenContext *context, FunctionSignature *signature)
{
	if (signature->failable)
	{
		signature->failable_abi_info = aarch64_classify_return_type(context, signature->rtype->type, signature->variadic);
		if (signature->rtype->type->type_kind != TYPE_VOID)
		{
			signature->ret_abi_info = aarch64_classify_argument_type(context, type_get_ptr(type_lowering(signature->rtype->type)));
		}
	}
	else
	{
		signature->ret_abi_info = aarch64_classify_return_type(context, signature->rtype->type, signature->variadic);
	}
	Decl **params = signature->params;
	VECEACH(params, i)
	{
		params[i]->var.abi_info = aarch64_classify_argument_type(context, params[i]->type);
	}

}
