// Copyright (c) 2022 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "tilde_internal.h"

#if TB_BACKEND

TB_Register tilde_get_zero(TbContext *c, Type *type)
{
	type = type_lowering(type);
	TB_DataType data_type = tbtype(type);
	if (type_is_float(type))
	{
		return tb_inst_float(c->f, data_type, 0);
	}
	return type_is_signed(type) ? tb_inst_sint(c->f, data_type, 0) : tb_inst_uint(c->f, data_type, 0);
}

TBEValue tilde_emit_assign_expr(TbContext *c, TBEValue *ref, Expr *expr, TB_Reg failable)
{
	assert(ref->kind == TBE_ADDRESS || ref->kind == TBE_ADDRESS_FAILABLE);
	TB_Label assign_block = 0;

	PUSH_ERROR();

	TBEValue result;
	if (failable)
	{
		if (IS_FAILABLE(expr))
		{
			if (expr->expr_kind == EXPR_FAILABLE)
			{
				c->error_var = 0;
				c->catch_block = 0;
				tilde_emit_expr(c, &result, expr->inner_expr);
				tilde_store_value_aligned(c, failable, &result, type_abi_alignment(type_anyerr));
				POP_ERROR();
				return (TBEValue) { .kind = TBE_VALUE };
			}
			assign_block = tb_inst_new_label_id(c->f);
			c->error_var = failable;
			c->catch_block = assign_block;
		}
		else
		{
			c->error_var = 0;
			c->catch_block = 0;
		}
	}
	if (type_is_vector(expr->type))
	{
		tilde_emit_expr(c, &result, expr);
		tilde_store_value(c, ref, &result);
	}
	else if (expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_LIST)
	{
		TODO
		//llvm_emit_const_initialize_reference(c, ref, expr);
		//value = *ref;
	}
	else if (expr_is_init_list(expr))
	{
		TODO
		//llvm_emit_initialize_reference(c, ref, expr);
		//value = *ref;
	}
	else
	{
		tilde_emit_expr(c, &result, expr);
		tilde_store_value(c, ref, &result);
	}

	if (failable)
	{
		tilde_store_zero(c, type_anyerr, failable, type_alloca_alignment(type_anyerr));
	}
	POP_ERROR();

	if (assign_block)
	{
		tb_inst_goto(c->f, assign_block);
		tb_inst_label(c->f, assign_block);
	}

	return result;
}

static void tilde_emit_parameter(TbContext *c, TB_Reg **args, ABIArgInfo *info, TBEValue *be_value, Type *type)
{
	type = type_lowering(type);
	assert(be_value->type->canonical == type);
	switch (info->kind)
	{
		case ABI_ARG_IGNORE:
			// Skip.
			return;
		case ABI_ARG_DIRECT_SPLIT_STRUCT:
		{
			TODO
			/*
			LLVMTypeRef coerce_type = llvm_get_coerce_type(c, info);
			assert(coerce_type && coerce_type != llvm_get_type(c, type));
			AlignSize target_alignment = llvm_abi_alignment(c, coerce_type);

			AlignSize alignment;
			LLVMValueRef cast = llvm_emit_coerce_alignment(c, be_value, coerce_type, target_alignment, &alignment);
			LLVMTypeRef element = llvm_get_type(c, info->direct_struct_expand.type);
			for (unsigned idx = 0; idx < info->direct_struct_expand.elements; idx++)
			{
				AlignSize load_align;
				LLVMValueRef element_ptr = llvm_emit_struct_gep_raw(c, cast, coerce_type, idx, alignment, &load_align);
				vec_add(*args, llvm_load(c, element, element_ptr, load_align, ""));
			}
			return;*/
		}
		case ABI_ARG_INDIRECT:
		{
			// If we want we could optimize for structs by doing it by reference here.
			assert(info->indirect.alignment == type_abi_alignment(type) || info->attributes.realign);
			TB_Reg indirect = tb_inst_local(c->f, type_size(type), info->indirect.alignment);
			tilde_store_value_aligned(c, indirect, be_value, info->indirect.alignment);
			vec_add(*args, indirect);
			return;
		}
		case ABI_ARG_DIRECT:
			vec_add(*args, tilde_load_value(c, be_value));
			return;
		case ABI_ARG_DIRECT_COERCE:
		{
			TB_DataType coerce_type = tbtype(info->direct_coerce_type);
			if (coerce_type.type == tbtype(type).type)
			{
				vec_add(*args, tilde_load_value(c, be_value));
				return;
			}
			TODO
			//vec_add(*args, llvm_emit_coerce(c, coerce_type, be_value, type));
			//return;
		}
		case ABI_ARG_DIRECT_COERCE_INT:
		{
			TB_DataType coerce_type = tilde_get_int_type_of_bytesize(type_size(type));
			if (coerce_type.type == tbtype(type).type)
			{
				vec_add(*args, tilde_load_value(c, be_value));
				return;
			}
			TODO
			//vec_add(*args, llvm_emit_coerce(c, coerce_type, be_value, type));
			//return;
		}
		case ABI_ARG_DIRECT_PAIR:
		{
			TODO
			/*
			llvm_value_addr(c, be_value);
			REMINDER("Handle invalid alignment");
			// Here we do the following transform:
			// struct -> { lo, hi } -> lo, hi
			LLVMTypeRef lo = llvm_abi_type(c, info->direct_pair.lo);
			LLVMTypeRef hi = llvm_abi_type(c, info->direct_pair.hi);
			LLVMTypeRef struct_type = llvm_get_coerce_type(c, info);

			AlignSize struct_align;
			LLVMValueRef cast = llvm_emit_coerce_alignment(c, be_value, struct_type, llvm_abi_alignment(c, struct_type), &struct_align);
			// Get the lo value.

			AlignSize alignment;
			LLVMValueRef lo_ptr = llvm_emit_struct_gep_raw(c, cast, struct_type, 0, struct_align, &alignment);
			vec_add(*args, llvm_load(c, lo, lo_ptr, alignment, "lo"));
			// Get the hi value.
			LLVMValueRef hi_ptr = llvm_emit_struct_gep_raw(c, cast, struct_type, 1, struct_align, &alignment);
			vec_add(*args, llvm_load(c, hi, hi_ptr, alignment, "hi"));
			return;*/
		}
		case ABI_ARG_EXPAND_COERCE:
		{
			/*
			// Create the expand type:
			LLVMTypeRef coerce_type = llvm_get_coerce_type(c, info);
			LLVMValueRef temp = LLVMBuildBitCast(c->builder, decl->backend_ref, LLVMPointerType(coerce_type, 0), "coerce");
			llvm_emit_and_set_decl_alloca(c, decl);

			AlignSize alignment = decl->alignment;
			AlignSize element_align;
			LLVMValueRef gep_first = llvm_emit_struct_gep_raw(c, temp, coerce_type, info->coerce_expand.lo_index, alignment, &element_align);
			llvm_store(c, gep_first, llvm_get_next_param(c, index), element_align);
			if (abi_type_is_valid(info->coerce_expand.hi))
			{
				LLVMValueRef gep_second = llvm_emit_struct_gep_raw(c, temp, coerce_type, info->coerce_expand.hi_index, alignment, &element_align);
				llvm_store(c, gep_second, llvm_get_next_param(c, index), element_align);
			}
			break;*/
			case ABI_ARG_EXPAND:
			{
				TODO
				/*
				// Move this to an address (if needed)
				llvm_value_addr(c, be_value);
				llvm_expand_type_to_args(c, type, be_value->value, args, be_value->alignment);
				// Expand the padding here.
				if (info->expand.padding_type)
				{
					vec_add(*args, LLVMGetUndef(llvm_get_type(c, info->expand.padding_type)));
				}
				return;*/
			}
		}
	}

}

void tilde_emit_call_expr(TbContext *c, TBEValue *result_value, Expr *expr)
{
	if (expr->call_expr.is_builtin)
	{
		TODO
		// llvm_emit_builtin_call(c, result_value, expr);
		return;
	}
	TB_DataType func_type;
	TB_Reg func;
	TBEValue temp_value;

	bool always_inline = false;

	FunctionPrototype *prototype;
	// 1. Call through a pointer.
	if (expr->call_expr.is_pointer_call)
	{
		Expr *function = exprptr(expr->call_expr.function);

		// 1a. Find the pointee type for the function pointer:
		Type *type = function->type->canonical->pointer;

		// 1b. Find the type signature using the underlying pointer.
		prototype = type->func.prototype;

		// 1c. Evaluate the pointer expression.
		TBEValue func_value;
		tilde_emit_expr(c, &func_value, function);

		// 1d. Load it as a value
		func = tilde_load_value(c, &func_value);

		// 1e. Calculate the function type
		func_type = tbtype(type);
	}
	else
	{
		// 2a. Get the function declaration

		Decl *function_decl = declptr(expr->call_expr.func_ref);
		always_inline = function_decl->func_decl.attr_inline;

		// 2b. Set signature, function and function type
		prototype = function_decl->type->func.prototype;
		func = function_decl->tb_register;
		assert(func);
		func_type = tbtype(function_decl->type);
	}

	(void)func_type;
	TB_Reg *values = NULL;
	Type **params = prototype->params;
	ABIArgInfo **abi_args = prototype->abi_args;
	unsigned param_count = vec_size(params);
	unsigned non_variadic_params = param_count;
	if (prototype->variadic == VARIADIC_TYPED) non_variadic_params--;
	ABIArgInfo *ret_info = prototype->ret_abi_info;
	Type *call_return_type = prototype->abi_ret_type;

	// 5. In the case of a failable, the error is replacing the regular return abi.
	TB_Reg error_var = 0;

	*result_value = (TBEValue){ .kind = TBE_VALUE, .reg = 0 };
	// 6. Generate data for the return value.
	switch (ret_info->kind)
	{
		case ABI_ARG_INDIRECT:
			// 6a. We can use the stored error var if there is no redirect.
			if (prototype->is_failable && c->error_var && !ret_info->attributes.realign)
			{
				error_var = c->error_var;
				vec_add(values, error_var);
				break;
			}
			// 6b. Return true is indirect, in this case we allocate a local, using the desired alignment on the caller side.
			assert(ret_info->attributes.realign || ret_info->indirect.alignment == type_abi_alignment(call_return_type));
			AlignSize alignment = ret_info->indirect.alignment;
			value_set_address(result_value, tilde_emit_alloca(c, call_return_type), call_return_type, alignment);

			// 6c. Add the pointer to the list of arguments.
			vec_add(values, result_value->reg);
			break;
		case ABI_ARG_EXPAND:
			UNREACHABLE
		case ABI_ARG_DIRECT_PAIR:
		case ABI_ARG_IGNORE:
		case ABI_ARG_DIRECT_COERCE:
		case ABI_ARG_DIRECT:
		case ABI_ARG_EXPAND_COERCE:
		case ABI_ARG_DIRECT_COERCE_INT:
		case ABI_ARG_DIRECT_SPLIT_STRUCT:
			TODO
	}


	// 7. We might have a failable indirect return and a normal return.
	//    In this case we need to add it by hand.
	TBEValue synthetic_return_param = { 0 };
	if (prototype->ret_by_ref)
	{
		// 7b. Create the address to hold the return.
		Type *actual_return_type = type_lowering(prototype->ret_by_ref_type);
		value_set(&synthetic_return_param, tilde_emit_alloca(c, actual_return_type), type_get_ptr(actual_return_type));
		// 7c. Emit it as a parameter as a pointer (will implicitly add it to the value list)
		tilde_emit_parameter(c,
		                    &values,
		                    prototype->ret_by_ref_abi_info,
		                    &synthetic_return_param,
		                    synthetic_return_param.type);
		// 7d. Update the be_value to actually be an address.
		value_set_address_abi_aligned(&synthetic_return_param, synthetic_return_param.reg, actual_return_type);
	}


	// 8. Add all other arguments.
	unsigned arguments = vec_size(expr->call_expr.arguments);
	assert(arguments >= non_variadic_params);
	for (unsigned i = 0; i < non_variadic_params; i++)
	{
		// 8a. Evaluate the expression.
		Expr *arg_expr = expr->call_expr.arguments[i];
		tilde_emit_expr(c, &temp_value, arg_expr);

		// 8b. Emit the parameter according to ABI rules.
		Type *param = params[i];
		ABIArgInfo *info = abi_args[i];
		tilde_emit_parameter(c, &values, info, &temp_value, param);
	}

	// 9. Typed varargs
	if (prototype->variadic == VARIADIC_TYPED)
	{
		REMINDER("All varargs should be called with non-alias!");
		Type *vararg_param = params[non_variadic_params];
		ABIArgInfo *vararg_info = abi_args[non_variadic_params];

		TBEValue subarray;

		value_set_address_abi_aligned(&subarray, tilde_emit_alloca(c, vararg_param), vararg_param);

		// 9a. Special case, empty argument
		if (arguments == non_variadic_params)
		{
			// Just set the size to zero.
			TBEValue len_addr;
			TODO
			// llvm_emit_subarray_len(c, &subarray, &len_addr);
			// llvm_store_value_raw(c, &len_addr, llvm_get_zero(c, type_usize));
		}
		else if (arguments == non_variadic_params + 1 && expr->call_expr.unsplat_last)
		{
			// 9b. We unpack the last type which is either a slice, an array or a dynamic array.
			TODO
			// llvm_emit_unpacked_variadic_arg(c, expr->call_expr.arguments[non_variadic_params], &subarray);
		}
		else
		{
			// 9b. Otherwise we also need to allocate memory for the arguments:
			Type *pointee_type = vararg_param->array.base;
			Type *array = type_get_array(pointee_type, arguments - non_variadic_params);
			TB_DataType llvm_array_type = tbtype(array);
			AlignSize alignment = type_alloca_alignment(array);
			TB_Reg array_ref = tilde_emit_alloca(c, array);
			for (unsigned i = non_variadic_params; i < arguments; i++)
			{
				Expr *arg_expr = expr->call_expr.arguments[i];
				tilde_emit_expr(c, &temp_value, arg_expr);
				AlignSize store_alignment;
				TODO
				/*
				LLVMValueRef slot = llvm_emit_array_gep_raw(c, array_ref, llvm_array_type, i - non_variadic_params, alignment, &store_alignment);
				llvm_store_value_aligned(c, slot, &temp_value, store_alignment);*/
			}
			TODO
			/*
			BEValue len_addr;
			llvm_emit_subarray_len(c, &subarray, &len_addr);
			llvm_store_value_raw(c, &len_addr, llvm_const_int(c, type_usize, arguments - non_variadic_params));
			BEValue pointer_addr;
			llvm_emit_subarray_pointer(c, &subarray, &pointer_addr);
			Type *array_as_pointer_type = type_get_ptr(pointee_type);
			llvm_store_value_raw(c, &pointer_addr, llvm_emit_bitcast(c, array_ref, array_as_pointer_type));*/
		}
		tilde_emit_parameter(c, &values, vararg_info, &subarray, vararg_param);
	}
	else
	{
		// 9. Emit varargs.
		for (unsigned i = param_count; i < arguments; i++)
		{
			Expr *arg_expr = expr->call_expr.arguments[i];
			tilde_emit_expr(c, &temp_value, arg_expr);
			REMINDER("Varargs should be expanded correctly");
			vec_add(values, tilde_load_value(c, &temp_value));
		}
	}


	// 10. Create the actual call (remember to emit a loc, because we might have shifted loc emitting the params)
	//EMIT_LOC(c, expr);
	TODO
	TB_Reg call_value;
	// = tb_inst_call(c->f, func_type, func, vec_size(values), values);
	if (prototype->call_abi)
	{
	//	LLVMSetInstructionCallConv(call_value, llvm_call_convention_from_call(prototype->call_abi, platform_target.arch, platform_target.os));
	}
	if (expr->call_expr.attr_force_noinline)
	{
	//	llvm_attribute_add_call(c, call_value, attribute_id.noinline, -1, 0);
	}
	else
	{
		if (expr->call_expr.attr_force_inline || always_inline)
		{
	//		llvm_attribute_add_call(c, call_value, attribute_id.alwaysinline, -1, 0);
		}
	}

	/*
	for (unsigned i = 0; i < non_variadic_params; i++)
	{
		Type *param = params[i];
		ABIArgInfo *info = abi_args[i];
		switch (info->kind)
		{
			case ABI_ARG_INDIRECT:
				if (info->attributes.by_val)
				{
					// llvm_attribute_add_call_type(c, call_value, attribute_id.byval, (int)i + 1, llvm_get_type(c, info->indirect.type));
				}
				llvm_attribute_add_call(c, call_value, attribute_id.align, (int)i + 1, info->indirect.alignment);
				break;
				default:
					break;
		}
	}

	// 11. Process the return value.
	switch (ret_info->kind)
	{
		case ABI_ARG_EXPAND:
			UNREACHABLE
			case ABI_ARG_IGNORE:
				// 12. Basically void returns or empty structs.
				//     Here we know we don't have a failable or any return value that can be used.
				assert(!prototype->is_failable && "Failable should have produced a return value.");
				*result_value = (BEValue) { .type = type_void, .kind = BE_VALUE };
				return;
				case ABI_ARG_INDIRECT:
					llvm_attribute_add_call_type(c, call_value, attribute_id.sret, 1, llvm_get_type(c, ret_info->indirect.type));
					llvm_attribute_add_call(c, call_value, attribute_id.align, 1, ret_info->indirect.alignment);
					// 13. Indirect, that is passing the result through an out parameter.

					// 13a. In the case of an already present error_var, we don't need to do a load here.
					if (error_var) break;

					// 13b. Otherwise it will be contained in a be_value that is an address
					//      so we don't need to do anything more.
					assert(result_value->kind == BE_ADDRESS);

					break;
					case ABI_ARG_DIRECT_PAIR:
					{
						// 14. A direct pair, in this case the data is stored like { lo, hi }
						//     For example we might have { int, int, short, short, int },
						//     this then gets bitcast to { long, long }, so we recover it by loading
						//     { long, long } into memory, then performing a bitcast to { int, int, short, short, int }

						// 14a. Generate the type.
						LLVMTypeRef lo = llvm_abi_type(c, ret_info->direct_pair.lo);
						LLVMTypeRef hi = llvm_abi_type(c, ret_info->direct_pair.hi);
						LLVMTypeRef struct_type = llvm_get_twostruct(c, lo, hi);

						// 14b. Use the coerce method to go from the struct to the actual type
						//      by storing the { lo, hi } struct to memory, then loading it
						//      again using a bitcast.
						llvm_emit_convert_value_from_coerced(c, result_value, struct_type, call_value, call_return_type);
						break;
					}
					case ABI_ARG_EXPAND_COERCE:
					{
						// 15. Expand-coerce, this is similar to "direct pair", but looks like this:
						//     { lo, hi } set into { pad, lo, pad, hi } -> original type.

						// 15a. Create memory to hold the return type.
						LLVMValueRef ret = llvm_emit_alloca_aligned(c, call_return_type, "");
						llvm_value_set_address_abi_aligned(result_value, ret, call_return_type);

						// 15b. "Convert" this return type pointer in memory to our coerce type which is { pad, lo, pad, hi }
						LLVMTypeRef coerce_type = llvm_get_coerce_type(c, ret_info);
						LLVMValueRef coerce = LLVMBuildBitCast(c->builder, ret, coerce_type, "");

						// 15d. Find the address to the low value
						AlignSize alignment;
						LLVMValueRef lo = llvm_emit_struct_gep_raw(c, coerce, coerce_type, ret_info->coerce_expand.lo_index,
																   type_abi_alignment(call_return_type), &alignment);

						// 15e. If there is only a single field, we simply store the value,
						//      so { lo } set into { pad, lo, pad } -> original type.
						if (!abi_type_is_valid(ret_info->coerce_expand.hi))
						{
							// Here we do a store to call -> lo (leaving the rest undefined)
							llvm_store(c, lo, call_value, alignment);
							break;
						}

						// 15g. We can now extract { lo, hi } to lo_value and hi_value.
						LLVMValueRef lo_value = LLVMBuildExtractValue(c->builder, call_value, 0, "");
						LLVMValueRef hi_value = LLVMBuildExtractValue(c->builder, call_value, 1, "");

						// 15h. Store lo_value into the { pad, lo, pad, hi } struct.
						llvm_store(c, lo, lo_value, alignment);

						// 15i. Calculate the address to the high value (like for the low in 15d.
						LLVMValueRef hi = llvm_emit_struct_gep_raw(c, coerce, coerce_type, ret_info->coerce_expand.hi_index,
																   type_abi_alignment(call_return_type), &alignment);

						// 15h. Store the high value.
						llvm_store(c, hi, hi_value, alignment);

						break;
					}
					case ABI_ARG_DIRECT:
						llvm_value_set(result_value, call_value, call_return_type);
						break;
						case ABI_ARG_DIRECT_COERCE:
						{
							// 16. A direct coerce, this is basically "call result" bitcast return type.

							// 16a. Get the type of the return.
							LLVMTypeRef coerce = llvm_get_coerce_type(c, ret_info);

							// 16b. If we don't have any coerce type, or the actual LLVM types are the same, we're done.
							if (!coerce || coerce == llvm_get_type(c, call_return_type))
							{
								// 16c. We just set as a value in be_value.
								llvm_value_set(result_value, call_value, call_return_type);
								break;
							}
							// 16c. We use a normal bitcast coerce.
							assert(!abi_info_should_flatten(ret_info) && "Did not expect flattening on return types.");
							llvm_emit_convert_value_from_coerced(c, result_value, coerce, call_value, call_return_type);
							break;
						}
	}

	// 17. Handle failables.
	if (prototype->is_failable)
	{
		BEValue no_err;

		// 17a. If we used the error var as the indirect recipient, then that will hold the error.
		//      otherwise it's whatever value in be_value.
		BEValue error_holder = *result_value;
		if (error_var)
		{
			llvm_value_set_address_abi_aligned(&error_holder, c->error_var, type_anyerr);
		}
		// 17b. Generate a boolean switch.
		llvm_value_set_bool(&no_err, llvm_emit_is_no_error(c, llvm_load_value(c, &error_holder)));

		// 17c. If we have an error var, or we aren't interested in the error variable
		//      - then it's straightforward. We just jump to the catch block.
		LLVMBasicBlockRef after_block = llvm_basic_block_new(c, "after.errcheck");
		if (error_var || !c->error_var)
		{
			llvm_emit_cond_br(c, &no_err, after_block, c->catch_block);
		}
		else
		{
			// 17d. If we have an error var we need to assign, then we need to
			//      first jump to an error block, where we do the copy.
			LLVMBasicBlockRef error_block = llvm_basic_block_new(c, "error");
			llvm_emit_cond_br(c, &no_err, after_block, error_block);
			llvm_emit_block(c, error_block);
			llvm_store_value_aligned(c, c->error_var, result_value, 0);
			// 17e. Then jump to the catch.
			llvm_emit_br(c, c->catch_block);
		}

		// 17f. Emit the "after" block.
		llvm_emit_block(c, after_block);

		// 17g. If void, be_value contents should be skipped.
		if (!prototype->ret_by_ref)
		{
			*result_value = (BEValue) { .type = type_void, .kind = BE_VALUE };
			return;
		}

		// 17h. Assign the return param to be_value.
		*result_value = synthetic_return_param;
		return;
	}
*/
	// 17i. The simple case here is where there is a normal return.
	//      In this case be_value already holds the result
	return;
}

void tilde_emit_expr(TbContext *c, TBEValue *value, Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_CALL:
			tilde_emit_call_expr(c, value, expr);
			return;
		default:
			TODO
		/*
		case EXPR_DESIGNATOR:
		case EXPR_POISONED:
		case EXPR_COND:
		case EXPR_TYPEINFO:
		case EXPR_MACRO_EXPANSION:
		case EXPR_CT_IDENT:
		case EXPR_HASH_IDENT:
		case EXPR_PLACEHOLDER:
		case EXPR_CT_CALL:
		case EXPR_FLATPATH:
		case EXPR_VARIANTSWITCH:
			UNREACHABLE
		case EXPR_ARGV_TO_SUBARRAY:
			llvm_emit_argv_to_subarray(c, value, expr);
			return;
		case EXPR_TRY_UNWRAP_CHAIN:
			llvm_emit_try_unwrap_chain(c, value, expr);
			return;
		case EXPR_TRY_UNWRAP:
			llvm_emit_try_unwrap(c, value, expr);
			return;
		case EXPR_CATCH_UNWRAP:
			llvm_emit_catch_unwrap(c, value, expr);
			return;
		case EXPR_UNDEF:
			// Should never reach this.
			UNREACHABLE
		case EXPR_PTR:
			llvm_emit_ptr(c, value, expr);
			return;
		case EXPR_BUILTIN:
			UNREACHABLE;
		case EXPR_DECL:
			llvm_emit_local_decl(c, expr->decl_expr);
			llvm_value_set_decl_address(value, expr->decl_expr);
			return;
		case EXPR_SLICE_ASSIGN:
			llvm_emit_slice_assign(c, value, expr);
			return;
		case EXPR_SLICE:
			gencontext_emit_slice(c, value, expr);
			return;
		case EXPR_LEN:
			llvm_emit_len(c, value, expr);
			return;
		case EXPR_FAILABLE:
			llvm_emit_failable(c, value, expr);
			return;
		case EXPR_TRY:
			llvm_emit_try_expr(c, value, expr);
			return;
		case EXPR_CATCH:
			llvm_emit_catch_expr(c, value, expr);
			return;
		case EXPR_NOP:
			llvm_value_set(value, NULL, type_void);
			return;
		case EXPR_OR_ERROR:
			gencontext_emit_or_error(c, value, expr);
			return;
		case EXPR_MACRO_BLOCK:
			llvm_emit_macro_block(c, value, expr);
			return;
		case EXPR_COMPOUND_LITERAL:
			UNREACHABLE
		case EXPR_INITIALIZER_LIST:
		case EXPR_DESIGNATED_INITIALIZER_LIST:
			llvm_emit_initializer_list_expr(c, value, expr);
			return;
		case EXPR_EXPR_BLOCK:
			llvm_emit_expr_block(c, value, expr);
			return;
		case EXPR_SCOPED_EXPR:
			gencontext_emit_scoped_expr(c, value, expr);
			return;
		case EXPR_UNARY:
			gencontext_emit_unary_expr(c, value, expr);
			return;
		case EXPR_CONST:
			llvm_emit_const_expr(c, value, expr);
			return;
		case EXPR_MACRO_BODY_EXPANSION:
			llvm_emit_macro_body_expansion(c, value, expr);
			return;
		case EXPR_BITASSIGN:
			llvm_emit_bitassign_expr(c, value, expr);
			return;
		case EXPR_BINARY:
			llvm_emit_binary_expr(c, value, expr);
			return;
		case EXPR_TERNARY:
			gencontext_emit_ternary_expr(c, value, expr);
			return;
		case EXPR_POST_UNARY:
			llvm_emit_post_unary_expr(c, value, expr);
			return;
		case EXPR_FORCE_UNWRAP:
			llvm_emit_force_unwrap_expr(c, value, expr);
			return;
		case EXPR_RETHROW:
			llvm_emit_rethrow_expr(c, value, expr);
			return;
		case EXPR_TYPEOFANY:
			llvm_emit_typeofany(c, value, expr);
			return;
		case EXPR_TYPEID:
		case EXPR_GROUP:
			// These are folded in the semantic analysis step.
			UNREACHABLE
		case EXPR_IDENTIFIER:
		case EXPR_CONST_IDENTIFIER:
			llvm_value_set_decl(value, expr->identifier_expr.decl);
			return;
		case EXPR_SUBSCRIPT:
		case EXPR_SUBSCRIPT_ADDR:
			gencontext_emit_subscript(c, value, expr);
			return;
		case EXPR_ACCESS:
			gencontext_emit_access_addr(c, value, expr);
			return;
		case EXPR_EXPRESSION_LIST:
			gencontext_emit_expression_list_expr(c, value, expr);
			return;
		case EXPR_CAST:
			gencontext_emit_cast_expr(c, value, expr);
			return;
		case EXPR_BITACCESS:
			llvm_emit_bitaccess(c, value, expr);
			return;*/
	}
	UNREACHABLE
}

#endif