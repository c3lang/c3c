// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "compiler/c_abi_internal.h"

ABIArgInfo *win64_classify(Regs *regs, Type *type, bool is_return, bool is_vector_call)
{
	if (type_is_void(type)) return abi_arg_ignore();
	
	// Lower enums etc.
	type = type_lowering(type);

	// Variable array has to be passed indirectly.
	if (type_is_union_or_strukt(type) && type->decl->has_variable_array)
	{
		return abi_arg_new_indirect_not_by_val(type);
	}

	Type *base = NULL;
	unsigned elements = 0;
	if (is_vector_call && type_is_homogenous_aggregate(type, &base, &elements))
	{
		// Enough registers AND return / builtin / vector
		if (regs->float_regs >= elements &&
			(is_return || type_is_builtin(type->type_kind) || type->type_kind == TYPE_VECTOR))
		{
			regs->float_regs -= elements;
			return abi_arg_new_direct();
		}
		// HVAs are handled later.
		if (is_return || (!type_is_builtin(type->type_kind) && type->type_kind != TYPE_VECTOR))
		{
			return abi_arg_new_indirect_not_by_val(type);
		}
		// => to main handling.
	}
	ByteSize size = type_size(type);
	bool type_is_vector_to_pass_as_array = compiler.build.feature.pass_win64_simd_as_arrays && type_flat_is_vector(type);
	if (type_is_vector_to_pass_as_array || type_is_abi_aggregate(type))
	{
		// Not 1, 2, 4, 8? Pass indirect.
		if (size > 8 || !is_power_of_two(size))
		{
			return abi_arg_new_indirect_not_by_val(type);
		}
		// Coerce to integer.
		return abi_arg_new_direct_coerce_type(type_int_unsigned_by_bitsize(size * 8));
	}
	if (type_is_builtin(type->type_kind))
	{
		switch (type->type_kind)
		{
			case TYPE_BOOL:
				return abi_arg_new_direct_int_ext(type_bool);
			case TYPE_U128:
			case TYPE_I128:
				// Pass by val since greater than 8 bytes.
				if (!is_return) return abi_arg_new_indirect_not_by_val(type);
				// Make i128 return in XMM0
				return abi_arg_new_direct_coerce_type(type_get_vector(type_long, 2));
			default:
				break;
		}
	}
	if (size > 8)
	{
		return abi_arg_new_indirect_not_by_val(type);
	}
	return abi_arg_new_direct();
}

ABIArgInfo *win64_reclassify_hva_arg(Regs *regs, Type *type, ABIArgInfo *info)
{
	// Assumes vectorCall calling convention.
	Type *base = NULL;
	unsigned elements = 0;
	type = type_lowering(type);
	if (!type_is_builtin(type->type_kind) && type->type_kind != TYPE_VECTOR && type_is_homogenous_aggregate(type, &base, &elements))
	{
		if (regs->float_regs >= elements)
		{
			regs->float_regs -= elements;
			ABIArgInfo *new_info = abi_arg_new_direct_by_reg(true);
			return new_info;
		}
	}
	return info;
}

static void win64_vector_call_args(Regs *regs, FunctionPrototype *prototype, bool is_vector)
{
	static const unsigned max_param_vector_calls_as_reg = 6;
	unsigned count = 0;
	Type **params = prototype->param_types;
	unsigned param_count = vec_size(prototype->param_types);
	if (param_count)
	{
		ABIArgInfo **args = MALLOC(sizeof(ABIArgInfo) * param_count);
		for (unsigned i = 0; i < param_count; i++)
		{
			Type *type = params[i];
			if (count < max_param_vector_calls_as_reg)
			{
				args[i] = win64_classify(regs, type, false, is_vector);
			}
			else
			{
				// Cannot be passed in registers pretend no registers.
				unsigned float_regs = regs->float_regs;
				regs->float_regs = 0;
				args[i] = win64_classify(regs, type, false, is_vector);
				regs->float_regs = float_regs;
			}
			count++;
		}
		for (unsigned i = 0; i < param_count; i++)
		{
			args[i] = win64_reclassify_hva_arg(regs, params[i], args[i]);
		}
		prototype->abi_args = args;
	}
}

ABIArgInfo **win64_create_params(Type **params, Regs *regs, bool is_vector_call)
{
	unsigned param_count = vec_size(params);
	if (!param_count) return NULL;
	ABIArgInfo **args = MALLOC(sizeof(ABIArgInfo) * param_count);
	for (unsigned i = 0; i < param_count; i++)
	{
		args[i] = win64_classify(regs, params[i], false, is_vector_call);
	}
	return args;
}

void c_abi_func_create_win64(FunctionPrototype *prototype)
{
	// allow calling sysv?

	// Set up return registers.
	Regs regs = { 0, 0 };
	bool is_vector_call = false;
	switch (prototype->call_abi)
	{
		case CALL_X64_VECTOR:
			regs.float_regs = 4;
			is_vector_call = true;
			break;
		default:
			regs.float_regs = 0;
			break;
	}

	prototype->ret_abi_info = win64_classify(&regs, prototype->abi_ret_type, true, is_vector_call);
	if (prototype->ret_by_ref)
	{
		prototype->ret_by_ref_abi_info = win64_classify(&regs,
														type_get_ptr(type_lowering(prototype->ret_by_ref_type)),
														false,
														is_vector_call);
	}

	// Set up parameter registers.
	switch (prototype->call_abi)
	{
		case CALL_X64_VECTOR:
			regs.float_regs = 6;
			is_vector_call = true;
			break;
		default:
			regs.float_regs = 0;
			break;
	}
	if (is_vector_call)
	{
		win64_vector_call_args(&regs, prototype, is_vector_call);
		return;
	}

	prototype->abi_args = win64_create_params(prototype->param_types, &regs, is_vector_call);
	prototype->abi_varargs = win64_create_params(prototype->varargs, &regs, is_vector_call);
}