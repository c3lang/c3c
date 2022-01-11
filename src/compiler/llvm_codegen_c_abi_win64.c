// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "c_abi_internal.h"

ABIArgInfo *win64_classify(Regs *regs, Type *type, bool is_return, bool is_vector, bool is_reg)
{
	if (type->type_kind == TYPE_VOID) return abi_arg_ignore();

	// Lower enums etc.
	type = type_lowering(type);

	// Variable array has to be passed indirectly.
	if (type_is_structlike(type) && type->decl->has_variable_array)
	{
		return abi_arg_new_indirect_not_by_val(type);
	}

	Type *base = NULL;
	unsigned elements = 0;
	if ((is_vector || is_reg) && type_is_homogenous_aggregate(type, &base, &elements))
	{
		if (is_reg)
		{
			// Enough registers? Then use direct/expand
			if (regs->float_regs >= elements)
			{
				regs->float_regs -= elements;
				// Direct if return / builtin / vector
				if (is_return || type_is_builtin(type->type_kind) || type->type_kind == TYPE_VECTOR)
				{
					return abi_arg_new_direct();
				}
				return abi_arg_new_expand();
			}
			// Otherwise use indirect
			return abi_arg_new_indirect_not_by_val(type);
		}
		if (is_vector)
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
	}
	ByteSize size = type_size(type);
	if (type_is_abi_aggregate(type))
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

void win64_vector_call_args(Regs *regs, FunctionPrototype *prototype, bool is_vector, bool is_reg)
{
	static const unsigned max_param_vector_calls_as_reg = 6;
	unsigned count = 0;
	Type **params = prototype->params;
	unsigned param_count = vec_size(prototype->params);
	if (param_count)
	{
		ABIArgInfo **args = MALLOC(sizeof(ABIArgInfo) * param_count);
		for (unsigned i = 0; i < param_count; i++)
		{
			Type *type = params[i];
			if (count < max_param_vector_calls_as_reg)
			{
				args[i] = win64_classify(regs, type, false, is_vector, is_reg);
			}
			else
			{
				// Cannot be passed in registers pretend no registers.
				unsigned float_regs = regs->float_regs;
				regs->float_regs = 0;
				args[i] = win64_classify(regs, type, false, is_vector, is_reg);
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

void c_abi_func_create_win64(FunctionPrototype *prototype)
{
	// allow calling sysv?

	// Set up return registers.
	Regs regs = { 0, 0 };
	bool is_reg_call = false;
	bool is_vector_call = false;
	switch (prototype->call_abi)
	{
		case CALL_X86_VECTOR:
			regs.float_regs = 4;
			is_vector_call = true;
			break;
		case CALL_X86_REG:
			regs.float_regs = 16;
			is_reg_call = true;
			break;
		default:
			regs.float_regs = 0;
			break;
	}

	prototype->ret_abi_info = win64_classify(&regs, prototype->abi_ret_type, true, is_vector_call, is_reg_call);
	if (prototype->ret_by_ref)
	{
		prototype->ret_by_ref_abi_info = win64_classify(&regs, type_get_ptr(type_lowering(prototype->ret_by_ref_type)), false, is_vector_call, is_reg_call);
	}

	// Set up parameter registers.
	switch (prototype->call_abi)
	{
		case CALL_X86_VECTOR:
			regs.float_regs = 6;
			is_vector_call = true;
			break;
		case CALL_X86_REG:
			regs.float_regs = 16;
			is_reg_call = true;
			break;
		default:
			regs.float_regs = 0;
			break;
	}
	if (is_vector_call)
	{
		win64_vector_call_args(&regs, prototype, is_vector_call, is_reg_call);
		return;
	}

	Type **params = prototype->params;
	unsigned param_count = vec_size(prototype->params);
	if (param_count)
	{
		ABIArgInfo **args = MALLOC(sizeof(ABIArgInfo) * param_count);
		for (unsigned i = 0; i < param_count; i++)
		{
			args[i] = win64_classify(&regs, params[i], false, is_vector_call, is_reg_call);
		}
		prototype->abi_args = args;
	}
}