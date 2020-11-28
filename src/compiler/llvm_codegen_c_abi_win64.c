// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_c_abi_internal.h"

ABIArgInfo *win64_classify(GenContext *context, Type *type, bool is_return, bool is_vector, bool is_reg)
{
	if (type->type_kind == TYPE_VOID) return abi_arg_ignore();

	// Lower enums etc.
	type = type_lowering(type);

	Type *base = NULL;
	unsigned elements = 0;
	if ((is_vector || is_reg) && type_is_homogenous_aggregate(type, &base, &elements))
	{
		if (is_reg)
		{
			// Enough registers? Then use direct/expand
			if (context->abi.sse_registers >= elements)
			{
				context->abi.sse_registers -= elements;
				// Direct if return / builtin / vector
				if (is_return || type_is_builtin(type->type_kind) || type->type_kind == TYPE_VECTOR)
				{
					return abi_arg_new_direct();
				}
				return abi_arg_new_expand();
			}
			// Otherwise use indirect
			return abi_arg_new_indirect_not_by_val();
		}
		if (is_vector)
		{
			// Enough registers AND return / builtin / vector
			if (context->abi.sse_registers >= elements &&
				(is_return || type_is_builtin(type->type_kind) || type->type_kind == TYPE_VECTOR))
			{
				context->abi.sse_registers -= elements;
				return abi_arg_new_direct();
			}
			// HVAs are handled later.
			if (is_return || (!type_is_builtin(type->type_kind) && type->type_kind != TYPE_VECTOR))
			{
				return abi_arg_new_indirect_not_by_val();
			}
			// => to main handling.
		}
	}
	size_t size = type_size(type);
	if (type_is_abi_aggregate(type))
	{
		// Not 1, 2, 4, 8? Pass indirect.
		if (size > 8 || !is_power_of_two(size))
		{
			return abi_arg_new_indirect_not_by_val();
		}
		// Coerce to integer.
		return abi_arg_new_direct_coerce(abi_type_new_int_bits(size * 8));
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
				if (!is_return) return abi_arg_new_indirect_not_by_val();
				// Make i128 return in XMM0
				return abi_arg_new_direct_coerce(abi_type_new_plain(type_get_vector(type_long, 2)));
			default:
				break;
		}
	}
	if (size > 8)
	{
		return abi_arg_new_indirect_not_by_val();
	}
	return abi_arg_new_direct();
}

ABIArgInfo *win64_reclassify_hva_arg(GenContext *context, Type *type, ABIArgInfo *info)
{
	// Assumes vectorCall calling convention.
	Type *base = NULL;
	unsigned elements = 0;
	type = type_lowering(type);
	if (!type_is_builtin(type->type_kind) && type->type_kind != TYPE_VECTOR && type_is_homogenous_aggregate(type, &base, &elements))
	{
		if (context->abi.sse_registers >= elements)
		{
			context->abi.sse_registers -= elements;
			ABIArgInfo *new_info = abi_arg_new_direct();
			new_info->attributes.by_reg = true;
			return new_info;
		}
	}
	return info;
}

void win64_vector_call_args(GenContext *context, FunctionSignature *signature, bool is_vector, bool is_reg)
{
	static const unsigned MaxParamVectorCallsAsReg = 6;
	unsigned count = 0;
	Decl **params = signature->params;
	VECEACH(params, i)
	{
		Decl *param = params[i];
		if (count < MaxParamVectorCallsAsReg)
		{
			param->var.abi_info = win64_classify(context, param->type, false, is_vector, is_reg);
		}
		else
		{
			// Cannot be passed in registers pretend no registers.
			unsigned regs = context->abi.sse_registers;
			context->abi.sse_registers = 0;
			param->var.abi_info = win64_classify(context, param->type, false, is_vector, is_reg);
			context->abi.sse_registers = regs;
		}
		count++;
	}
	VECEACH(params, i)
	{
		Decl *param = params[i];
		param->var.abi_info = win64_reclassify_hva_arg(context, param->type, param->var.abi_info);
	}

}
void c_abi_func_create_win64(GenContext *context, FunctionSignature *signature)
{
	// allow calling sysv?

	// Set up return registers.
	context->abi.int_registers = 0;
	bool is_reg_call = false;
	bool is_vector_call = false;
	switch (context->abi.call_convention)
	{
		case CALL_CONVENTION_VECTOR:
			context->abi.sse_registers = 4;
			is_vector_call = true;
			break;
		case CALL_CONVENTION_REGCALL:
			context->abi.sse_registers = 16;
			is_reg_call = true;
			break;
		default:
			context->abi.sse_registers = 0;
			break;
	}

	if (signature->failable)
	{
		signature->failable_abi_info = win64_classify(context, type_error, true, is_vector_call, is_reg_call);
		if (signature->rtype->type->type_kind != TYPE_VOID)
		{
			signature->ret_abi_info = win64_classify(context, type_get_ptr(type_lowering(signature->rtype->type)), false, is_vector_call, is_reg_call);
		}
	}
	else
	{
		signature->ret_abi_info = win64_classify(context, signature->rtype->type, true, is_vector_call, is_reg_call);
	}

	// Set up parameter registers.
	switch (context->abi.call_convention)
	{
		case CALL_CONVENTION_VECTOR:
			context->abi.sse_registers = 6;
			is_vector_call = true;
			break;
		case CALL_CONVENTION_REGCALL:
			context->abi.sse_registers = 16;
			is_reg_call = true;
			break;
		default:
			context->abi.sse_registers = 0;
			break;
	}
	if (is_vector_call)
	{
		win64_vector_call_args(context, signature, is_vector_call, is_reg_call);
		return;
	}
	Decl **params = signature->params;
	VECEACH(params, i)
	{
		params[i]->var.abi_info = win64_classify(context, params[i]->type, false, is_vector_call, is_reg_call);
	}
}