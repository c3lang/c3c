// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "compiler/c_abi_internal.h"

INLINE bool is_aarch64_illegal_vector(Type *type)
{
	if (type->type_kind != TYPE_VECTOR)
	{
		// Return true if scaled vector
		return false;
	}
	ArraySize len = type->array.len;
	if (!is_power_of_two(len)) return true;
	switch (type_size(type))
	{
		case 8:
			return false;
		case 16:
			return len == 1;
		default:
			return true;
	}
}

ABIArgInfo *aarch64_coerce_illegal_vector(Type *type)
{
	if (false /*type->type_kind == TYPE_SCALED_VECTOR*/)
	{
		/*
		Type *base_type = type->array.base;
		if (base_type == type_bool) return abi_arg_new_direct_coerce_type(type_get_scaled_vector(type_bool, 16));
		switch (type->type_kind)
		{
			case TYPE_U8:
			case TYPE_I8:
				return abi_arg_new_direct_coerce_type(type_get_scaled_vector(type_char, 16));
			case TYPE_U16:
			case TYPE_I16:
				return abi_arg_new_direct_coerce_type(type_get_scaled_vector(type_ushort, 8));
			case TYPE_I32:
			case TYPE_U32:
				return abi_arg_new_direct_coerce_type(type_get_scaled_vector(type_uint, 4));
			case TYPE_I64:
			case TYPE_U64:
				return abi_arg_new_direct_coerce_type(type_get_scaled_vector(type_uint, 2));
			case TYPE_F16:
				return abi_arg_new_direct_coerce_type(type_get_scaled_vector(type_float16, 8));
			case TYPE_F32:
				return abi_arg_new_direct_coerce_type(type_get_scaled_vector(type_float, 4));
			case TYPE_F64:
				return abi_arg_new_direct_coerce_type(type_get_scaled_vector(type_double, 4));
			case TYPE_BF16:
				return abi_arg_new_direct_coerce_type(type_get_scaled_vector(type_bfloat16, 8));
			default:
				UNREACHABLE
		}*/
	}
	ASSERT0(type->type_kind == TYPE_VECTOR);
	TypeSize size = type_size(type);

	// CLANG: Android promotes char[<2>] to ushort, not uint
	if (compiler.platform.environment_type == ENV_TYPE_ANDROID && size <= 2)
	{
		return abi_arg_new_direct_coerce_type(type_ushort);
	}
	// 32 bits or fewer? Put in int.
	if (size <= 4) return abi_arg_new_direct_coerce_type(type_uint);

	// 64 bits or less? Put in uint[<2>]
	if (size <= 8) return abi_arg_new_direct_coerce_type(type_get_vector(type_uint, 2));
	// 128 bits in a single val? Put in uint[<4>]
	if (size == 128) return abi_arg_new_direct_coerce_type(type_get_vector(type_uint, 4));
	return abi_arg_new_indirect_not_by_val(type);
}

ABIArgInfo *aarch64_classify_argument_type(Type *type)
{
	type = type_lowering(type);

	if (type_is_void(type)) return abi_arg_ignore();

	if (is_aarch64_illegal_vector(type))
	{
		return aarch64_coerce_illegal_vector(type);
	}

	TypeSize size = type_size(type);

	if (!type_is_abi_aggregate(type))
	{
		// Over 128 bits should be indirect, but
		// we don't have that (yet?)
		if (type_is_promotable_int_bool(type) && compiler.platform.aarch64.is_darwin_pcs)
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
		ASSERT0(members < 128);
		if (members > 1)
		{
			return abi_arg_new_direct_coerce_type(type_get_array(base, members));
		}
		return abi_arg_new_direct_coerce_type(base);
	}

	// Aggregates <= in registers
	if (size <= 16)
	{
		// For RenderScript <= 16 needs to be coerced.
		AlignSize alignment = type_abi_alignment(type);
		if (compiler.platform.aarch64.is_aapcs)
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
		ASSERT0(alignment == 8 || alignment == 16);

		if (alignment == 16) return abi_arg_new_direct_coerce_type(type_u128);
		ArraySize m = size / alignment;
		if (m > 1) return abi_arg_new_direct_coerce_type(type_get_array(type_ulong, m));
		return abi_arg_new_direct_coerce_type(type_ulong);

	}

	return abi_arg_new_indirect_not_by_val(type);
}

ABIArgInfo *aarch64_classify_return_type(Type *type, bool variadic)
{
	type = type_lowering(type);

	if (type_is_void(type)) return abi_arg_ignore();

	if (is_aarch64_illegal_vector(type))
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
		if (type_is_promotable_int_bool(type) && compiler.platform.aarch64.is_darwin_pcs)
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
		!(compiler.platform.arch == ARCH_TYPE_AARCH64_32 && variadic))
	{
		return abi_arg_new_direct();
	}

	// Aggregates <= in registers
	if (size <= 16)
	{
		// For RenderScript <= 16 needs to be coerced to ints
		// this is case is ignored here but needs to be added
		// in case it is to be supported.

		if (size <= 8 && !compiler.platform.big_endian)
		{
			return abi_arg_new_direct_coerce_int();
		}

		unsigned alignment = type_abi_alignment(type);
		// Align to multiple of 8.
		size = aligned_offset(size, 8);
		if (alignment < 16 && size == 16)
		{
			return abi_arg_new_direct_coerce_type(type_get_array(type_ulong, size / 8));
		}
		return abi_arg_new_direct_coerce_type(type_int_unsigned_by_bitsize(size * 8));
	}

	return abi_arg_new_indirect_by_val(type);
}


void c_abi_func_create_aarch64(FunctionPrototype *prototype)
{

	prototype->ret_abi_info = aarch64_classify_return_type(prototype->abi_ret_type, prototype->raw_variadic);
	if (prototype->ret_by_ref)
	{
		prototype->ret_by_ref_abi_info = aarch64_classify_argument_type(type_get_ptr(type_flatten(prototype->ret_by_ref_type)));
	}

	Type **params = prototype->param_types;
	unsigned param_count = vec_size(prototype->param_types);
	if (param_count)
	{
		ABIArgInfo **args = MALLOC(sizeof(ABIArgInfo) * param_count);
		for (unsigned i = 0; i < param_count; i++)
		{
			args[i] = aarch64_classify_argument_type(params[i]);
		}
		prototype->abi_args = args;
	}
	Type **va_params = prototype->varargs;
	unsigned va_param_count = vec_size(va_params);
	if (va_param_count)
	{
		ABIArgInfo **args = MALLOC(sizeof(ABIArgInfo) * va_param_count);
		for (unsigned i = 0; i < va_param_count; i++)
		{
			args[i] = aarch64_classify_argument_type(va_params[i]);
		}
		prototype->abi_varargs = args;
	}
}
