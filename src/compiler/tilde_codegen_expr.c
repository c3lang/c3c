// Copyright (c) 2022 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "tilde_internal.h"

TB_Reg tilde_get_const_int(TildeContext *c, Type *type, uint64_t i)
{
	return type_is_signed(type)
		? tb_inst_sint(c->f, tildetype(type), (int64_t)i)
		: tb_inst_uint(c->f, tildetype(type), i);
}

TB_Reg tilde_get_const_float(TildeContext *c, Type *type, double d)
{
	return type_size(type) == 4 ? tb_inst_float32(c->f, (float)d) : tb_inst_float64(c->f, d);
}

TB_Register tilde_get_zero(TildeContext *c, Type *type)
{
	type = type_lowering(type);
	TB_DataType data_type = tildetype(type);
	switch (data_type.type)
	{
		case TB_INT:
			return type_is_signed(type) ? tb_inst_sint(c->f, data_type, 0) : tb_inst_uint(c->f, data_type, 0);
		case TB_FLOAT:
			return type->type_kind == TYPE_F32 ? tb_inst_float32(c->f, 0.0f) : tb_inst_float64(c->f, 0.0);
		case TB_PTR:
			return tb_inst_ptr(c->f, 0);
		default:
			UNREACHABLE;
	}
}

static void tilde_emit_const_expr(TildeContext *c, TBEValue *value, Expr *expr)
{
	Type *type = type_reduced_from_expr(expr)->canonical;
	switch (expr->const_expr.const_kind)
	{

		case CONST_BYTES:
			TODO
		case CONST_INTEGER:
		{
			Int128 i = expr->const_expr.ixx.i;
			switch (expr->const_expr.ixx.type)
			{
				case TYPE_I128:
				case TYPE_U128:
				{
					uint64_t words[2] = { i.low, i.high };
					TODO
				}
				default:
					value_set(value, tilde_get_const_int(c, type, i.low), type);
					return;
			}
		}
		case CONST_FLOAT:
			value_set(value, tilde_get_const_float(c, type, expr->const_expr.fxx.f), type);
			return;
		case CONST_POINTER:
			if (!expr->const_expr.ptr)
			{
				value_set(value, tb_inst_ptr(c->f, 0), type);
			}
			else
			{
				value_set(value, tb_inst_ptr(c->f, expr->const_expr.ptr), type);
			}
			return;
		case CONST_BOOL:
			value_set(value, tb_inst_bool(c->f, expr->const_expr.b), type_bool);
			return;
		case CONST_STRING:
		{
			TODO
			/*
			Type *str_type = type_lowering(expr->type);
			bool is_array = type_flat_is_char_array(str_type);
			if (llvm_is_local_eval(c) || !is_array)
			{
				ArraySize strlen = expr->const_expr.bytes.len;
				ArraySize size = expr->const_expr.bytes.len + 1;
				if (type_flat_is_char_array(expr->type) && type->array.len > size) size = type->array.len;
				LLVMValueRef global_name = llvm_add_global_raw(c,
				                                               ".str",
				                                               LLVMArrayType(llvm_get_type(c, type_char), size),
				                                               1);
				llvm_set_private_linkage(global_name);
				LLVMSetUnnamedAddress(global_name, LLVMGlobalUnnamedAddr);
				LLVMSetGlobalConstant(global_name, 1);
				LLVMValueRef string = llvm_get_zstring(c, expr->const_expr.bytes.ptr, expr->const_expr.bytes.len);
				if (size > strlen + 1)
				{
					LLVMValueRef trailing_zeros = llvm_get_zero_raw(LLVMArrayType(c->byte_type, size - strlen - 1));
					LLVMValueRef values[2] = { string, trailing_zeros };
					string = llvm_get_packed_struct(c, values, 2);
				}
				LLVMSetInitializer(global_name, string);
				if (is_array)
				{
					global_name = llvm_emit_bitcast_ptr(c, global_name, type);
					llvm_value_set_address(be_value, global_name, type, 1);
				}
				else
				{
					global_name = llvm_emit_bitcast(c, global_name, type);
					llvm_value_set(be_value, global_name, type);
				}
				return;
			}
			ArraySize array_len = type->array.len;
			ArraySize size = expr->const_expr.bytes.len + 1;
			bool zero_terminate = array_len == size;
			LLVMValueRef string;
			if (array_len <= size)
			{
				if (zero_terminate)
				{
					string = llvm_get_zstring(c, expr->const_expr.bytes.ptr, expr->const_expr.bytes.len);
				}
				else
				{
					string = llvm_get_bytes(c, expr->const_expr.bytes.ptr, array_len);
				}
			}
			else
			{
				char *buffer = ccalloc(1, array_len);
				memcpy(buffer, expr->const_expr.bytes.ptr, expr->const_expr.bytes.len);
				string = llvm_get_bytes(c, buffer, array_len);
			}
			llvm_value_set(be_value, string, type);*/
			return;
		}
		case CONST_TYPEID:
			TODO
			//llvm_emit_typeid(c, be_value, expr->const_expr.typeid);
			return;
		case CONST_ERR:
		{
			TODO
			/*
			Decl *decl = expr->const_expr.enum_err_val;

			LLVMValueRef value;
			if (decl)
			{
				value = LLVMBuildPtrToInt(c->builder, llvm_get_ref(c, decl), llvm_get_type(c, type_anyfault), "");
			}
			else
			{
				value = llvm_get_zero(c, type_anyfault);
			}
			llvm_value_set(be_value, value, type_anyfault);*/
			return;
		}
		case CONST_ENUM:
			value_set(value, tilde_get_const_int(c, type, expr->const_expr.enum_err_val->enum_constant.ordinal), type);
			return;
		case CONST_INITIALIZER:
			TODO
			//llvm_emit_const_initializer_list_expr(c, be_value, expr);
			return;
		case CONST_MEMBER:
		case CONST_UNTYPED_LIST:
			UNREACHABLE
	}
	UNREACHABLE

}

void tilde_emit_parameter(TildeContext *c, TB_Reg *args, unsigned *arg_count_ref, ABIArgInfo *info, TBEValue *be_value, Type *type)
{
	type = type_lowering(type);
	assert(be_value->type->canonical == type);
	switch (info->kind)
	{
		case ABI_ARG_IGNORE:
			// Skip.
			return;
		case ABI_ARG_INDIRECT:
		{
			// If we want we could optimize for structs by doing it by reference here.
			assert(info->indirect.alignment == type_abi_alignment(type) || info->attributes.realign);
			if (info->attributes.by_val && value_is_addr(be_value) && info->indirect.alignment <= be_value->alignment)
			{
				value_fold_optional(c, be_value);
				args[(*arg_count_ref)++] = be_value->reg;
				return;
			}
			TB_Reg indirect = tilde_emit_alloca(c, type, info->indirect.alignment);
			tilde_store_to_ptr_aligned(c, indirect, be_value, info->indirect.alignment);
			args[(*arg_count_ref)++] = indirect;
			return;
		}
		case ABI_ARG_DIRECT:
			args[(*arg_count_ref)++] = tilde_load_value_store(c, be_value);
			return;
		case ABI_ARG_DIRECT_SPLIT_STRUCT:
		{
			TODO; /*--
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
				args[(*arg_count_ref)++] = llvm_load(c, element, element_ptr, load_align, "");
			}
			return;*/
		}
		case ABI_ARG_DIRECT_COERCE:
		{
			TODO; /*
			LLVMTypeRef coerce_type = llvm_get_type(c, info->direct_coerce_type);
			if (coerce_type == llvm_get_type(c, type))
			{
				args[(*arg_count_ref)++] = llvm_load_value_store(c, be_value);
				return;
			}
			args[(*arg_count_ref)++] = llvm_emit_coerce(c, coerce_type, be_value, type);
			return;--*/
		}
		case ABI_ARG_DIRECT_COERCE_INT:
		{
			TODO; /*--
			LLVMTypeRef coerce_type = LLVMIntTypeInContext(c->context, type_size(type) * 8);
			if (coerce_type == llvm_get_type(c, type))
			{
				args[(*arg_count_ref)++] = llvm_load_value_store(c, be_value);
				return;
			}
			args[(*arg_count_ref)++] = llvm_emit_coerce(c, coerce_type, be_value, type);
			return; --*/
		}
		case ABI_ARG_DIRECT_PAIR:
		{
			TODO; /*-
			assert(type_flatten(be_value->type) == be_value->type);
			LLVMTypeRef original_type = llvm_get_type(c, be_value->type);
			LLVMTypeRef struct_type = llvm_get_coerce_type(c, info);
			AlignSize alignment;
			if (llvm_types_are_similar(original_type, struct_type))
			{
				// Optimization
				assert(LLVMGetTypeKind(original_type) == LLVMStructTypeKind && LLVMCountStructElementTypes(original_type) == 2);
				if (llvm_value_is_addr(be_value))
				{
					LLVMValueRef ptr = llvm_emit_struct_gep_raw(c, be_value->value, original_type, 0, be_value->alignment, &alignment);
					args[(*arg_count_ref)++] = llvm_load(c, LLVMStructGetTypeAtIndex(original_type, 0), ptr, alignment, "lo");
					ptr = llvm_emit_struct_gep_raw(c, be_value->value, original_type, 1, be_value->alignment, &alignment);
					args[(*arg_count_ref)++] = llvm_load(c, LLVMStructGetTypeAtIndex(original_type, 1), ptr, alignment, "hi");
					return;
				}
				LLVMValueRef val = be_value->value;
				// Maybe it's just created? Let's optimize codegen.
				if (!LLVMGetFirstUse(val) && LLVMIsAInsertValueInst(val) && LLVMIsAInsertValueInst(
						LLVMGetPreviousInstruction(val)))
				{
					LLVMValueRef prev = LLVMGetPreviousInstruction(val);
					// Isn't this a second insert?
					if (LLVMGetOperand(val, 0) != prev) goto NO_OPT;
					// Is it used in between?
					if (LLVMGetNextUse(LLVMGetFirstUse(prev))) goto NO_OPT;
					// No, then we can replace the instructions with the values.
					LLVMValueRef first_val = LLVMGetOperand(prev, 1);
					LLVMValueRef second_val = LLVMGetOperand(val, 1);
					LLVMInstructionEraseFromParent(val);
					LLVMInstructionEraseFromParent(prev);
					args[(*arg_count_ref)++] = first_val;
					args[(*arg_count_ref)++] = second_val;
					return;
				}
				NO_OPT:
				args[(*arg_count_ref)++] = llvm_emit_extract_value(c, be_value->value, 0);
				args[(*arg_count_ref)++] = llvm_emit_extract_value(c, be_value->value, 1);
				return;
			}
			llvm_value_addr(c, be_value);
			REMINDER("Handle invalid alignment");
			// Here we do the following transform:
			// struct -> { lo, hi } -> lo, hi
			LLVMTypeRef lo = llvm_abi_type(c, info->direct_pair.lo);
			LLVMTypeRef hi = llvm_abi_type(c, info->direct_pair.hi);

			AlignSize struct_align;
			LLVMValueRef cast = llvm_emit_coerce_alignment(c, be_value, struct_type, llvm_abi_alignment(c, struct_type), &struct_align);
			// Get the lo value.

			LLVMValueRef lo_ptr = llvm_emit_struct_gep_raw(c, cast, struct_type, 0, struct_align, &alignment);
			args[(*arg_count_ref)++] = llvm_load(c, lo, lo_ptr, alignment, "lo");
			// Get the hi value.
			LLVMValueRef hi_ptr = llvm_emit_struct_gep_raw(c, cast, struct_type, 1, struct_align, &alignment);
			args[(*arg_count_ref)++] = llvm_load(c, hi, hi_ptr, alignment, "hi");
			return;--*/
		}
		case ABI_ARG_EXPAND_COERCE:
		{
			TODO

			/*--
			// Move this to an address (if needed)
			llvm_value_addr(c, be_value);
			LLVMTypeRef coerce_type = llvm_get_coerce_type(c, info);
			AlignSize alignment;
			LLVMValueRef temp = llvm_emit_coerce_alignment(c, be_value, coerce_type, llvm_abi_alignment(c, coerce_type), &alignment);

			AlignSize align;
			LLVMValueRef gep_first = llvm_emit_struct_gep_raw(c, temp, coerce_type, info->coerce_expand.lo_index, alignment, &align);
			args[(*arg_count_ref)++] = llvm_load(c, llvm_abi_type(c, info->coerce_expand.lo), gep_first, align, "");
			if (abi_type_is_valid(info->coerce_expand.hi))
			{
				LLVMValueRef gep_second = llvm_emit_struct_gep_raw(c, temp, coerce_type, info->coerce_expand.hi_index, alignment, &align);
				args[(*arg_count_ref)++] = llvm_load(c, llvm_abi_type(c, info->coerce_expand.hi), gep_second, align, "");
			}
			return;--*/
		}
		case ABI_ARG_EXPAND:
		{
			// Move this to an address (if needed)
			value_addr(c, be_value);
			TODO /*--tilde_expand_type_to_args(c, type, be_value.reg, args, arg_count_ref, be_value->alignment);
			// Expand the padding here.
			if (info->expand.padding_type)
			{
				args[(*arg_count_ref)++] = TB_NULL_REG;
			}
			return;--*/
		}
	}
}

void tilde_emit_raw_call(TildeContext *c, TBEValue *result_value, FunctionPrototype *prototype, TB_FunctionPrototype *func_type,
						 TB_Function *func, TB_Reg func_ptr, TB_Reg *args, unsigned arg_count, int inline_flag, TB_Reg error_var,
						 bool sret_return, TBEValue *synthetic_return_param)
{
	ABIArgInfo *ret_info = prototype->ret_abi_info;
	Type *call_return_type = prototype->abi_ret_type;

	TB_Reg call_value;
	if (func_ptr)
	{
		TODO
	}
	else
	{
		call_value = tb_inst_call(c->f, tildetype(call_return_type), (TB_Symbol *)func, arg_count, args);
	}

	switch (inline_flag)
	{
		case -1:
			TODO // llvm_attribute_add_call(c, call_value, attribute_id.noinline, -1, 0);
			break;
		case 1:
			TODO // llvm_attribute_add_call(c, call_value, attribute_id.alwaysinline, -1, 0);
			break;
		default:
			break;
	}

	assert(!prototype->ret_by_ref || prototype->ret_by_ref_abi_info->kind != ABI_ARG_INDIRECT);

	/*
	llvm_add_abi_call_attributes(c, call_value, vec_size(prototype->param_types), prototype->abi_args);
	if (prototype->abi_varargs)
	{
		llvm_add_abi_call_attributes(c,
		                             call_value,
		                             vec_size(prototype->varargs),
		                             prototype->abi_varargs);
	}*/

	// 11. Process the return value.
	switch (ret_info->kind)
	{
		case ABI_ARG_EXPAND:
		case ABI_ARG_DIRECT_SPLIT_STRUCT:
			UNREACHABLE
		case ABI_ARG_IGNORE:
			// 12. Basically void returns or empty structs.
			//     Here we know we don't have an optional or any return value that can be used.
			assert(!prototype->is_optional && "Optional should have produced a return value.");
			*result_value = (TBEValue) { .type = type_void, .kind = TBE_VALUE };
			return;
		case ABI_ARG_INDIRECT:
			TODO /*
			llvm_attribute_add_call_type(c, call_value, attribute_id.sret, 1, llvm_get_type(c, ret_info->indirect.type));
			llvm_attribute_add_call(c, call_value, attribute_id.align, 1, ret_info->indirect.alignment);
			// 13. Indirect, that is passing the result through an out parameter.

			// 13a. In the case of an already present error_var, we don't need to do a load here.
			if (error_var || sret_return) break;

			// 13b. If not it will be contained in a be_value that is an address
			//      so we don't need to do anything more.
			assert(result_value->kind == BE_ADDRESS);

			break; --*/
		case ABI_ARG_DIRECT_PAIR:
		{
			TODO
			/*
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
			break; --*/
		}
		case ABI_ARG_EXPAND_COERCE:
		{
			TODO
			/*
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
				llvm_store_to_ptr_raw_aligned(c, lo, call_value, alignment);
				break;
			}

			// 15g. We can now extract { lo, hi } to lo_value and hi_value.
			LLVMValueRef lo_value = llvm_emit_extract_value(c, call_value, 0);
			LLVMValueRef hi_value = llvm_emit_extract_value(c, call_value, 1);

			// 15h. Store lo_value into the { pad, lo, pad, hi } struct.
			llvm_store_to_ptr_raw_aligned(c, lo, lo_value, alignment);

			// 15i. Calculate the address to the high value (like for the low in 15d.
			LLVMValueRef hi = llvm_emit_struct_gep_raw(c, coerce, coerce_type, ret_info->coerce_expand.hi_index,
			                                           type_abi_alignment(call_return_type), &alignment);

			// 15h. Store the high value.
			llvm_store_to_ptr_raw_aligned(c, hi, hi_value, alignment);
*/
			break;
		}
		case ABI_ARG_DIRECT:
			value_set(result_value, call_value, call_return_type);
			break;
		case ABI_ARG_DIRECT_COERCE_INT:
		{
			// 16. A direct coerce, this is basically "call result" bitcast return type.

			// 16a. Get the type of the return.
			TB_DataType coerce = tilde_get_int_type_of_bytesize(type_size(call_return_type));

			// 16b. If we don't have any coerce type, or the actual LLVM types are the same, we're done.
			TB_DataType ret_type = tildetype(call_return_type);
			if (coerce.raw == ret_type.raw)
			{
				// 16c. We just set as a value in be_value.
				value_set(result_value, call_value, call_return_type);
				break;
			}
			// 16c. We use a normal bitcast coerce.
			TODO // tilde_emit_convert_value_from_coerced(c, result_value, coerce, call_value, call_return_type);
			break;
		}
		case ABI_ARG_DIRECT_COERCE:
		{
			TODO /*---
			// 16. A direct coerce, this is basically "call result" bitcast return type.

			// 16a. Get the type of the return.
			LLVMTypeRef coerce = llvm_get_type(c, ret_info->direct_coerce_type);

			// 16b. If we don't have any coerce type, or the actual LLVM types are the same, we're done.
			if (coerce == llvm_get_type(c, call_return_type))
			{
				// 16c. We just set as a value in be_value.
				llvm_value_set(result_value, call_value, call_return_type);
				break;
			}
			// 16c. We use a normal bitcast coerce.
			llvm_emit_convert_value_from_coerced(c, result_value, coerce, call_value, call_return_type);
			break; */
		}
	}

	// 17. Handle optionals.
	if (sret_return)
	{
		*result_value = (TBEValue) { .type = type_void, .kind = TBE_VALUE };
		return;
	}
	if (prototype->is_optional)
	{
		TBEValue no_err;

		// Emit the current stack into the thread local or things will get messed up.
		if (c->debug.last_ptr)
			tilde_store_to_ptr_raw_aligned(c,
										  type_voidptr,
			                              c->debug.last_ptr,
			                              c->debug.stack_slot,
			                              type_alloca_alignment(type_voidptr));

		// 17a. If we used the error var as the indirect recipient, then that will hold the error.
		//      otherwise it's whatever value in be_value.
		TBEValue error_holder = *result_value;
		if (error_var)
		{
			value_set_address_abi_aligned(&error_holder, c->opt_var, type_anyfault);
		}

		TB_Reg stored_error;

		if (error_var)
		{
			stored_error = c->opt_var;
			c->opt_var = TB_NULL_REG;
		}
		tilde_emit_jump_to_optional_exit(c, tilde_load_value(c, &error_holder));
		if (error_var)
		{
			c->opt_var = stored_error;
		}


		// 17g. If void, be_value contents should be skipped.
		if (!prototype->ret_by_ref)
		{
			*result_value = (TBEValue) { .type = type_void, .kind = TBE_VALUE };
			return;
		}

		// 17h. Assign the return param to be_value.
		*result_value = *synthetic_return_param;
		return;
	}

	// Emit the current stack into the thread local or things will get messed up.
	if (c->debug.last_ptr)
		tilde_store_to_ptr_raw_aligned(c,
									  type_voidptr,
		                              c->debug.last_ptr,
		                              c->debug.stack_slot,
		                              type_alloca_alignment(type_voidptr));

	// 17i. The simple case here is where there is a normal return.
	//      In this case be_value already holds the result
}

static void tilde_emit_call_expr(TildeContext *c, TBEValue *result_value, Expr *expr, TBEValue *target)
{
	if (expr->call_expr.is_builtin)
	{
		TODO // llvm_emit_builtin_call(c, result_value, expr);
		return;
	}

	REMINDER("Debug stack");
	/*
	if (c->debug.stack_slot_row)
	{
		llvm_store_to_ptr_raw_aligned(c,
		                              c->debug.stack_slot_row,
		                              llvm_const_int(c, type_uint, expr->span.row),
		                              type_abi_alignment(type_uint));
	}*/

	TB_FunctionPrototype *func_type;
	TB_Function *func = NULL;
	TB_Reg func_ptr = TB_NULL_REG;
	TBEValue temp_value;

	bool always_inline = false;

	FunctionPrototype *prototype;
	// 1. Call through a pointer.
	if (!expr->call_expr.is_func_ref)
	{
		Expr *function = exprptr(expr->call_expr.function);

		// 1a. Find the pointee type for the function pointer:
		Type *type = function->type->canonical->pointer;

		// 1b. Find the type signature using the underlying pointer.
		prototype = type->function.prototype;

		// 1c. Evaluate the pointer expression.
		TBEValue func_value;
		tilde_emit_expr(c, &func_value, function);

		// 1d. Load it as a value
		func_ptr = tilde_load_value(c, &func_value);

		// 1e. Calculate the function type
		func_type = tilde_get_func_prototype(c, prototype);
	}
	else
	{
		// 2a. Get the function declaration

		Decl *function_decl = declptr(expr->call_expr.func_ref);
		always_inline = function_decl->func_decl.attr_inline;

		// 2b. Set signature, function and function type
		prototype = function_decl->type->function.prototype;
		func = tilde_get_function(c, function_decl);
		assert(func);
		func_type = tilde_get_func_prototype(c, prototype);
	}

	TB_Reg arg_values[512];
	unsigned arg_count = 0;
	Type **params = prototype->param_types;
	ABIArgInfo **abi_args = prototype->abi_args;
	unsigned param_count = vec_size(params);
	Expr **args = expr->call_expr.arguments;
	Expr **varargs = NULL;
	Expr *vararg_splat = NULL;
	if (prototype->variadic != VARIADIC_NONE)
	{
		if (expr->call_expr.splat_vararg)
		{
			vararg_splat = expr->call_expr.splat;
		}
		else
		{
			varargs = expr->call_expr.varargs;
		}
	}
	FunctionPrototype copy;
	if (prototype->variadic == VARIADIC_RAW)
	{
		if (varargs || vararg_splat)
		{
			assert(!vararg_splat);
			copy = *prototype;
			copy.varargs = NULL;

			foreach(Expr*, varargs)
			{
				vec_add(copy.varargs, type_flatten(val->type));
			}
			copy.ret_abi_info = NULL;
			copy.ret_by_ref_abi_info = NULL;
			copy.abi_args = NULL;
			c_abi_func_create(&copy);
			prototype = &copy;
			TB_DataType *params_type = NULL;
			tilde_update_prototype_abi(c, prototype, &params_type);
		}
	}
	ABIArgInfo *ret_info = prototype->ret_abi_info;
	Type *call_return_type = prototype->abi_ret_type;

	// 5. In the case of an optional, the error is replacing the regular return abi.
	TB_Reg error_var = TB_NULL_REG;

	*result_value = (TBEValue){ .kind = TBE_VALUE, .reg = TB_NULL_REG };
	// 6. Generate data for the return value.
	bool sret_return = false;
	switch (ret_info->kind)
	{
		case ABI_ARG_INDIRECT:
			// 6a. We can use the stored error var if there is no redirect.
			if (prototype->is_optional && c->opt_var && !ret_info->attributes.realign)
			{
				error_var = c->opt_var;
				arg_values[arg_count++] = error_var;
				break;
			}
			// 6b. Return true is indirect, in this case we allocate a local, using the desired alignment on the caller side.
			assert(ret_info->attributes.realign || ret_info->indirect.alignment == type_abi_alignment(call_return_type));
			AlignSize alignment = ret_info->indirect.alignment;
			// If we have a target, then use it.
			if (target && alignment <= target->alignment)
			{
				assert(target->kind == TBE_ADDRESS);
				arg_values[arg_count++] = target->reg;
				sret_return = true;
				break;
			}
			value_set_address(result_value,
			                  tilde_emit_alloca(c, call_return_type, alignment),
							  call_return_type, alignment);

			// 6c. Add the pointer to the list of arguments.
			arg_values[arg_count++] = result_value->reg;
			break;
		case ABI_ARG_EXPAND:
		case ABI_ARG_DIRECT_SPLIT_STRUCT:
			UNREACHABLE
		case ABI_ARG_DIRECT_PAIR:
		case ABI_ARG_IGNORE:
		case ABI_ARG_DIRECT_COERCE_INT:
		case ABI_ARG_DIRECT_COERCE:
		case ABI_ARG_DIRECT:
		case ABI_ARG_EXPAND_COERCE:
			break;
	}


	// 7. We might have an optional indirect return and a normal return.
	//    In this case we need to add it by hand.
	TBEValue synthetic_return_param = { 0 };
	if (prototype->ret_by_ref)
	{
		// 7b. Create the address to hold the return.
		Type *actual_return_type = type_lowering(prototype->ret_by_ref_type);
		value_set(&synthetic_return_param, tilde_emit_alloca(c, actual_return_type, 0), type_get_ptr(actual_return_type));
		// 7c. Emit it as a parameter as a pointer (will implicitly add it to the value list)
		tilde_emit_parameter(c, arg_values, &arg_count, prototype->ret_by_ref_abi_info, &synthetic_return_param, synthetic_return_param.type);
		// 7d. Update the be_value to actually be an address.
		value_set_address_abi_aligned(&synthetic_return_param, synthetic_return_param.reg, actual_return_type);
	}


	// 8. Add all other arguments.
	for (unsigned i = 0; i < param_count; i++)
	{
		// 8a. Evaluate the expression.
		Expr *arg_expr = args[i];
		Type *param = params[i];
		ABIArgInfo *info = abi_args[i];

		if (arg_expr)
		{
			tilde_emit_expr(c, &temp_value, arg_expr);
		}
		else
		{
			assert(prototype->variadic == VARIADIC_TYPED || prototype->variadic == VARIADIC_ANY);
			TODO // tilde_emit_vararg_parameter(c, &temp_value, param, info, varargs, vararg_splat);
		}

		// 8b. Emit the parameter according to ABI rules.
		tilde_emit_parameter(c, arg_values, &arg_count, info, &temp_value, param);
	}

	// 9. Typed varargs

	if (prototype->variadic == VARIADIC_RAW)
	{
		if (prototype->abi_varargs)
		{
			// 9. Emit varargs.
			unsigned index = 0;
			ABIArgInfo **abi_varargs = prototype->abi_varargs;
			foreach(Expr*, varargs)
			{
				tilde_emit_expr(c, &temp_value, val);
				ABIArgInfo *info = abi_varargs[index];
				tilde_emit_parameter(c, arg_values, &arg_count, info, &temp_value, prototype->varargs[index]);
				index++;
			}
		}
		else
		{
			// 9. Emit varargs.
			foreach(Expr*, varargs)
			{
				tilde_emit_expr(c, &temp_value, val);
				REMINDER("Varargs should be expanded correctly");
				arg_values[arg_count++] = tilde_load_value(c, &temp_value);
			}
		}
	}


	// 10. Create the actual call (remember to emit a loc, because we might have shifted loc emitting the params)
	EMIT_LOC(c, expr);
	int inline_flag = 0;
	if (expr->call_expr.attr_force_noinline)
	{
		inline_flag = -1;
	}
	else
	{
		inline_flag = expr->call_expr.attr_force_inline || always_inline ? 1 : 0;
	}
	tilde_emit_raw_call(c, result_value, prototype, func_type, func, func_ptr, arg_values, arg_count, inline_flag, error_var, sret_return, &synthetic_return_param);

	// Emit the current stack into the thread local or things will get messed up.
	if (c->debug.last_ptr)
	{
		tilde_store_to_ptr_raw_aligned(c, type_voidptr, c->debug.last_ptr, c->debug.stack_slot,
									   type_alloca_alignment(type_voidptr));
	}

	// 17i. The simple case here is where there is a normal return.
	//      In this case be_value already holds the result
	return;
}

TBEValue tilde_emit_assign_expr(TildeContext *c, TBEValue *ref, Expr *expr, TB_Reg optional)
{
	assert(ref->kind == TBE_ADDRESS || ref->kind == TBE_ADDRESS_OPTIONAL);

	assert(optional || !IS_OPTIONAL(expr));
	// Special optimization of handling of optional
	if (expr->expr_kind == EXPR_OPTIONAL)
	{
		PUSH_OPT();

		c->opt_var = TB_NULL_REG;
		c->catch_block = TB_NULL_REG;
		TBEValue result;
		// Emit the fault type.
		tilde_emit_expr(c, &result, expr->inner_expr);
		TB_Reg err_val = result.reg;
		// Store it in the optional
		tilde_store_to_ptr(c, optional, &result);
		// Set the result to an undef value
		value_set(&result, TB_NULL_REG, ref->type);

		POP_OPT();

		// If we had a catch block outside then we want to jump to that exit.
		if (c->catch_block) tilde_emit_jump_to_optional_exit(c, err_val);

		// This return value will not be used.
		return result;
	}

	PUSH_OPT();


	TB_Label assign_block = 0;
	TB_Label rejump_block = 0;

	if (IS_OPTIONAL(expr))
	{
		assign_block = tb_basic_block_create(c->f);
		assert(optional);
		if (c->opt_var)
		{
			c->catch_block = rejump_block = tb_basic_block_create(c->f);
		}
		else
		{
			c->catch_block = assign_block;
		}
		c->opt_var = optional;
	}
	else
	{
		c->opt_var = 0;
		c->catch_block = 0;
	}

	TBEValue value;
	if (type_flat_is_vector(expr->type))
	{
		tilde_emit_expr(c, &value, expr);
		tilde_store(c, ref, &value);
	}
	else if (expr_is_const_initializer(expr))
	{
		TODO
		//llvm_emit_const_initialize_reference(c, ref, expr);
		value = *ref;
	}
	else if (expr_is_init_list(expr))
	{
		TODO
		//llvm_emit_initialize_reference(c, ref, expr);
		value = *ref;
	}
	else
	{
		if (expr->expr_kind == EXPR_CALL)
		{
			tilde_emit_call_expr(c, &value, expr, ref);
		}
		else
		{
			tilde_emit_expr(c, &value, expr);
		}
		if (value.type != type_void) tilde_store(c, ref, &value);
	}

	if (optional)
	{
		tilde_store_to_ptr_raw(c, optional, tilde_get_zero(c, type_anyfault), type_anyfault);
	}
	POP_OPT();

	if (assign_block)
	{
		tb_inst_goto(c->f, assign_block);
		if (rejump_block)
		{
			tilde_emit_block(c, rejump_block);
			TB_Reg error = tilde_load_abi_alignment(c, type_anyfault, optional);
			tilde_store_to_ptr_raw(c, c->opt_var, error, type_anyfault);
			tb_inst_goto(c->f, c->catch_block);
		}
		tilde_emit_block(c, assign_block);
	}

	return value;

}

static inline TB_Reg tilde_emit_add_int(TildeContext *c, Type *type, TB_Reg left, TB_Reg right, SourceSpan loc)
{
	if (active_target.feature.trap_on_wrap)
	{
		REMINDER("Unable to trap on wrap");
		TODO /*--
		LLVMTypeRef type_to_use = llvm_get_type(c, type->canonical);
		LLVMValueRef args[2] = { left, right };
		assert(type->canonical == type);
		LLVMValueRef add_res;
		if (type_is_unsigned(type))
		{
			add_res = llvm_emit_call_intrinsic(c, intrinsic_id.uadd_overflow, &type_to_use, 1, args, 2);
		}
		else
		{
			add_res = llvm_emit_call_intrinsic(c, intrinsic_id.sadd_overflow, &type_to_use, 1, args, 2);
		}
		LLVMValueRef result = llvm_emit_extract_value(c, add_res, 0);
		LLVMValueRef ok = llvm_emit_extract_value(c, add_res, 1);
		llvm_emit_panic_on_true(c, ok, "Addition overflow", loc);
		return result; --*/
	}

	return tb_inst_add(c->f, left, right, (TB_ArithmaticBehavior)0);
}

static inline TB_Reg tilde_emit_mult_int(TildeContext *c, Type *type, TB_Reg left, TB_Reg right, SourceSpan loc)
{
	if (active_target.feature.trap_on_wrap)
	{
		TODO /*
		LLVMTypeRef type_to_use = llvm_get_type(c, type);
		LLVMValueRef args[2] = { left, right };
		LLVMTypeRef types[2] = { type_to_use, type_to_use };
		unsigned operation = type_is_integer_unsigned(type) ? intrinsic_id.umul_overflow
		                                                    : intrinsic_id.smul_overflow;
		LLVMValueRef call_res = llvm_emit_call_intrinsic(c,
		                                                 operation,
		                                                 types,
		                                                 1,
		                                                 args,
		                                                 2);
		LLVMValueRef val = llvm_emit_extract_value(c, call_res, 0);
		LLVMValueRef ok = llvm_emit_extract_value(c, call_res, 1);
		llvm_emit_panic_on_true(c, ok, "Integer multiplication overflow", loc);
		return val;*/
	}
	return tb_inst_mul(c->f, left, right, (TB_ArithmaticBehavior)0);
}

void tilde_emit_int_comp_raw(TildeContext *c, TBEValue *result, Type *lhs_type, Type *rhs_type, TB_Reg lhs_value, TB_Reg rhs_value, BinaryOp binary_op)
{
	bool lhs_signed, rhs_signed;
	Type *vector_type = type_vector_type(lhs_type);
	if (vector_type)
	{
		lhs_signed = type_is_signed(vector_type);
		rhs_signed = type_is_signed(type_vector_type(rhs_type));
	}
	else
	{
		assert(type_is_integer_or_bool_kind(lhs_type));
		lhs_signed = type_is_signed(lhs_type);
		rhs_signed = type_is_signed(rhs_type);
	}
	if (lhs_signed != rhs_signed)
	{
		// Swap sides if needed.
		if (!lhs_signed)
		{
			Type *temp = lhs_type;
			lhs_type = rhs_type;
			rhs_type = temp;
			lhs_signed = true;
			rhs_signed = false;
			TB_Reg temp_val = lhs_value;
			lhs_value = rhs_value;
			rhs_value = temp_val;
			switch (binary_op)
			{
				case BINARYOP_GE:
					binary_op = BINARYOP_LE;
					break;
				case BINARYOP_GT:
					binary_op = BINARYOP_LT;
					break;
				case BINARYOP_LE:
					binary_op = BINARYOP_GE;
					break;
				case BINARYOP_LT:
					binary_op = BINARYOP_GT;
					break;
				default:
					break;
			}
		}
	}

	if (!lhs_signed)
	{
		assert(lhs_signed == rhs_signed);
		// Right and left side are both unsigned.
		TB_Reg value;
		switch (binary_op)
		{
			case BINARYOP_EQ:
				value = tb_inst_cmp_eq(c->f, lhs_value, rhs_value);
				break;
			case BINARYOP_NE:
				value = tb_inst_cmp_ne(c->f, lhs_value, rhs_value);
				break;
			case BINARYOP_GE:
				value = tb_inst_cmp_ige(c->f, lhs_value, rhs_value, false);
				break;
			case BINARYOP_GT:
				value = tb_inst_cmp_igt(c->f, lhs_value, rhs_value, false);
				break;
			case BINARYOP_LE:
				value = tb_inst_cmp_ile(c->f, lhs_value, rhs_value, false);
				break;
			case BINARYOP_LT:
				value = tb_inst_cmp_ilt(c->f, lhs_value, rhs_value, false);
				break;
			default:
				UNREACHABLE
		}
		if (vector_type)
		{
			TODO // llvm_convert_vector_comparison(c, result, value, lhs_type, binary_op == BINARYOP_EQ);
			return;
		}
		value_set(result, value, type_bool);
		return;
	}


	// Left side is signed.
	TB_Reg comp_value;
	TB_Reg check_value;

	switch (binary_op)
	{
		case BINARYOP_EQ:
			comp_value = tb_inst_cmp_eq(c->f, lhs_value, rhs_value);
			break;
		case BINARYOP_NE:
			comp_value = tb_inst_cmp_ne(c->f, lhs_value, rhs_value);
			break;
		case BINARYOP_GE:
			comp_value = tb_inst_cmp_ige(c->f, lhs_value, rhs_value, true);
			break;
		case BINARYOP_GT:
			comp_value = tb_inst_cmp_igt(c->f, lhs_value, rhs_value, true);
			break;
		case BINARYOP_LE:
			comp_value = tb_inst_cmp_ile(c->f, lhs_value, rhs_value, true);
			break;
		case BINARYOP_LT:
			comp_value = tb_inst_cmp_ilt(c->f, lhs_value, rhs_value, true);
			break;
		default:
			UNREACHABLE
	}

	// If right side is also signed then this is fine.
	if (rhs_signed)
	{
		if (vector_type)
		{
			TODO // tilde_convert_vector_comparison(c, result, comp_value, lhs_type, binary_op == BINARYOP_EQ);
			return;
		}
		value_set(result, comp_value, type_bool);
		return;
	}

	// Otherwise, special handling for left side signed, right side unsigned.
	TB_Reg zero = tilde_get_zero(c, lhs_type);
	switch (binary_op)
	{
		case BINARYOP_EQ:
			// Only true if lhs >= 0
			check_value = tb_inst_cmp_ige(c->f, lhs_value, zero, true);
			comp_value = tb_inst_and(c->f, check_value, comp_value);
			break;
		case BINARYOP_NE:
			// Always true if lhs < 0
			check_value = tb_inst_cmp_ilt(c->f, lhs_value, zero, true);
			comp_value = tb_inst_and(c->f, check_value, comp_value);
			break;
		case BINARYOP_GE:
			// Only true if rhs >= 0 when regarded as a signed integer
			check_value = tb_inst_cmp_ige(c->f, rhs_value, zero, true);
			comp_value = tb_inst_and(c->f, check_value, comp_value);
			break;
		case BINARYOP_GT:
			// Only true if rhs >= 0 when regarded as a signed integer
			check_value = tb_inst_cmp_ige(c->f, rhs_value, zero, true);
			comp_value = tb_inst_and(c->f, check_value, comp_value);
			break;
		case BINARYOP_LE:
			// Always true if rhs < 0 when regarded as a signed integer
			check_value = tb_inst_cmp_ilt(c->f, rhs_value, zero, true);
			comp_value = tb_inst_or(c->f, check_value, comp_value);
			break;
		case BINARYOP_LT:
			// Always true if rhs < 0 when regarded as a signed integer
			check_value = tb_inst_cmp_ilt(c->f, rhs_value, zero, true);
			comp_value = tb_inst_or(c->f, check_value, comp_value);
			break;
		default:
			UNREACHABLE
	}
	if (vector_type)
	{
		TODO // tilde_convert_vector_comparison(c, result, comp_value, lhs_type, BINARYOP_EQ == binary_op);
		return;
	}
	value_set(result, comp_value, type_bool);
}


void tilde_emit_comp(TildeContext *c, TBEValue *result, TBEValue *lhs, TBEValue *rhs, BinaryOp binary_op)
{
	assert(binary_op >= BINARYOP_GT && binary_op <= BINARYOP_EQ);
	value_rvalue(c, lhs);
	value_rvalue(c, rhs);
	if (type_is_integer_or_bool_kind(lhs->type))
	{
		tilde_emit_int_comp_raw(c, result, lhs->type, rhs->type, lhs->reg, rhs->reg, binary_op);
		return;
	}
	if (type_is_pointer(lhs->type))
	{
		TODO // tilde_emit_ptr_comparison(c, result, lhs, rhs, binary_op);
		return;
	}
	if (type_is_float(lhs->type))
	{
		TODO // tilde_emit_float_comp(c, result, lhs, rhs, binary_op, NULL);
		return;
	}
	if (lhs->type->type_kind == TYPE_INFO_SLICE)
	{
		TODO // tilde_emit_subarray_comp(c, result, lhs, rhs, binary_op);
		return;
	}
	if (lhs->type->type_kind == TYPE_VECTOR)
	{
		Type *type = type_vector_type(lhs->type);
		if (type_is_float(type))
		{
			TODO // tilde_emit_float_comp(c, result, lhs, rhs, binary_op, lhs->type);
		}
		else
		{
			TODO // tilde_emit_int_comp_raw(c, result, lhs->type, rhs->type, lhs->reg, rhs->reg, binary_op);
		}
		return;
	}
	if (lhs->type->type_kind == TYPE_ARRAY)
	{
		TODO // tilde_emit_array_comp(c, result, lhs, rhs, binary_op);
		return;
	}
	TODO
}


void tilde_emit_binary(TildeContext *c, TBEValue *be_value, Expr *expr, TBEValue *lhs_loaded, BinaryOp binary_op)
{
	// foo ?? bar
	if (binary_op == BINARYOP_ELSE)
	{
		TODO // llvm_emit_else(c, be_value, expr);
		return;
	}

	// foo || bar and foo && bar
	if (binary_op == BINARYOP_AND || binary_op == BINARYOP_OR)
	{
		TODO // llvm_emit_logical_and_or(c, be_value, expr, binary_op);
		return;
	}

	// Load if needed, otherwise use the already loaded.
	TBEValue lhs;
	if (lhs_loaded)
	{
		lhs = *lhs_loaded;
	}
	else
	{
		if (type_is_float(type_flatten(expr->type)) && (binary_op == BINARYOP_ADD || binary_op == BINARYOP_SUB))
		{
			TODO // if (tilde_emit_fmuladd_maybe(c, be_value, expr, binary_op)) return;
		}
		tilde_emit_expr(c, &lhs, exprptr(expr->binary_expr.left));
	}
	// We need the rvalue.
	value_rvalue(c, &lhs);

	// Evaluate rhs
	TBEValue rhs;
	tilde_emit_expr(c, &rhs, exprptr(expr->binary_expr.right));
	value_rvalue(c, &rhs);

	EMIT_LOC(c, expr);
	// Comparison <=>
	if (binary_op >= BINARYOP_GT && binary_op <= BINARYOP_EQ)
	{
		tilde_emit_comp(c, be_value, &lhs, &rhs, binary_op);
		return;
	}

	Type *lhs_type = lhs.type;
	Type *rhs_type = rhs.type;
	Type *vector_type = lhs_type->type_kind == TYPE_VECTOR ? lhs_type->array.base : NULL;
	bool is_float = type_is_float(lhs_type) || (vector_type && type_is_float(vector_type));
	TB_Reg val = TB_NULL_REG;
	TB_Reg lhs_value = lhs.reg;
	TB_Reg rhs_value = rhs.reg;
	switch (binary_op)
	{
		case BINARYOP_ERROR:
			UNREACHABLE
		case BINARYOP_MULT:
			if (is_float)
			{
				val = tb_inst_fmul(c->f, lhs_value, rhs_value);
				break;
			}
			val = tilde_emit_mult_int(c, lhs_type, lhs_value, rhs_value, expr->span);
			break;
		case BINARYOP_SUB:
			if (lhs_type->type_kind == TYPE_POINTER)
			{
				TODO /*---
				if (lhs_type == rhs_type)
				{
					LLVMTypeRef int_type = llvm_get_type(c, type_isz);
					val = LLVMBuildSub(c->builder, LLVMBuildPtrToInt(c->builder, lhs_value, int_type, ""),
					                   LLVMBuildPtrToInt(c->builder, rhs_value, int_type, ""), "");
					val = LLVMBuildExactSDiv(c->builder, val, llvm_const_int(c, type_isz, type_abi_alignment(lhs_type->pointer)), "");
					break;
				}
				rhs_value = LLVMBuildNeg(c->builder, rhs_value, "");
				val = llvm_emit_pointer_gep_raw(c, llvm_get_pointee_type(c, lhs_type), lhs_value, rhs_value);
				*/
				break;
			}
			if (is_float)
			{
				val = tb_inst_fsub(c->f, lhs_value, rhs_value);
				break;
			}
			TODO // val = tilde_emit_sub_int(c, lhs_type, lhs_value, rhs_value, expr->span);
			break;
		case BINARYOP_ADD:
			if (lhs_type->type_kind == TYPE_POINTER)
			{
				assert(type_is_integer(rhs_type));
				TODO // val = llvm_emit_pointer_gep_raw(c, llvm_get_pointee_type(c, lhs_type), lhs_value, rhs_value);
				break;
			}
			if (is_float)
			{
				val = tb_inst_fadd(c->f, lhs_value, rhs_value);
				break;
			}
			val = tilde_emit_add_int(c, lhs_type, lhs_value, rhs_value, expr->span);
			break;
		case BINARYOP_DIV:
			TODO // tilde_emit_trap_zero(c, rhs_type, rhs_value, "Division by zero.", expr->span);
			if (is_float)
			{
				val = tb_inst_fdiv(c->f, lhs_value, rhs_value);
				break;
			}
			val = tb_inst_div(c->f, lhs_value, rhs_value, type_is_signed(lhs_type));
			break;
		case BINARYOP_MOD:
			TODO // tilde_emit_trap_zero(c, rhs_type, rhs_value, "% by zero.", expr->span);
			if (type_is_float(lhs_type))
			{
				TODO // val = LLVMBuildFRem(c->builder, lhs_value, rhs_value, "fmod");
				break;
			}
			val = tb_inst_mod(c->f, lhs_value, rhs_value, type_is_signed(lhs_type));
			break;
		case BINARYOP_SHR:
			TODO /*---
			rhs_value = llvm_zext_trunc(c, rhs_value, LLVMTypeOf(lhs_value));
			llvm_emit_trap_invalid_shift(c, rhs_value, lhs_type, "Shift amount out of range.", expr->span);
			val = type_is_unsigned(lhs_type)
			      ? LLVMBuildLShr(c->builder, lhs_value, rhs_value, "lshr")
			      : LLVMBuildAShr(c->builder, lhs_value, rhs_value, "ashr");
			val = LLVMBuildFreeze(c->builder, val, ""); --*/
			break;
		case BINARYOP_SHL:
			/*---
			rhs_value = llvm_zext_trunc(c, rhs_value, LLVMTypeOf(lhs_value));
			llvm_emit_trap_invalid_shift(c, rhs_value, lhs_type, "Shift amount out of range.", expr->span);
			val = LLVMBuildShl(c->builder, lhs_value, rhs_value, "shl");
			val = LLVMBuildFreeze(c->builder, val, ""); --*/
			break;
		case BINARYOP_BIT_AND:
			val = tb_inst_and(c->f, lhs_value, rhs_value);
			break;
		case BINARYOP_BIT_OR:
			val = tb_inst_or(c->f, lhs_value, rhs_value);
			break;
		case BINARYOP_BIT_XOR:
			val = tb_inst_xor(c->f, lhs_value, rhs_value);
			break;
		case BINARYOP_ELSE:
		case BINARYOP_EQ:
		case BINARYOP_NE:
		case BINARYOP_GE:
		case BINARYOP_GT:
		case BINARYOP_LE:
		case BINARYOP_LT:
		case BINARYOP_AND:
		case BINARYOP_OR:
		case BINARYOP_ASSIGN:
		case BINARYOP_MULT_ASSIGN:
		case BINARYOP_ADD_ASSIGN:
		case BINARYOP_SUB_ASSIGN:
		case BINARYOP_DIV_ASSIGN:
		case BINARYOP_MOD_ASSIGN:
		case BINARYOP_BIT_AND_ASSIGN:
		case BINARYOP_BIT_OR_ASSIGN:
		case BINARYOP_BIT_XOR_ASSIGN:
		case BINARYOP_SHR_ASSIGN:
		case BINARYOP_SHL_ASSIGN:
			// Handled elsewhere.
			UNREACHABLE
	}
	assert(val);
	value_set(be_value, val, expr->type);
}

static void tilde_emit_binary_expr(TildeContext *c, TBEValue *be_value, Expr *expr)
{
	BinaryOp binary_op = expr->binary_expr.operator;
	if (binary_op >= BINARYOP_ASSIGN && expr_is_vector_index(exprptr(expr->binary_expr.left)))
	{
		TODO // llvm_emit_vector_assign_expr(c, be_value, expr);
		return;
	}
	if (binary_op > BINARYOP_ASSIGN)
	{
		BinaryOp base_op = binaryop_assign_base_op(binary_op);
		assert(base_op != BINARYOP_ERROR);
		TBEValue addr;
		tilde_emit_expr(c, &addr, exprptr(expr->binary_expr.left));
		value_addr(c, &addr);
		tilde_emit_binary(c, be_value, expr, &addr, base_op);
		tilde_store(c, &addr, be_value);
		return;
	}
	if (binary_op == BINARYOP_ASSIGN)
	{
		Expr *left = exprptr(expr->binary_expr.left);
		tilde_emit_expr(c, be_value, left);
		assert(value_is_addr(be_value));
		TB_Reg optional_ref = TB_NULL_REG;
		if (left->expr_kind == EXPR_IDENTIFIER)
		{
			TODO
			/*---
			optional_ref = decl_optional_ref(left->identifier_expr.decl);
			be_value->kind = BE_ADDRESS;*/
		}
		*be_value = tilde_emit_assign_expr(c, be_value, exprptr(expr->binary_expr.right), optional_ref);
		return;
	}

	tilde_emit_binary(c, be_value, expr, NULL, binary_op);
}

void tilde_emit_expr(TildeContext *c, TBEValue *value, Expr *expr)
{
	EMIT_LOC(c, expr);
	switch (expr->expr_kind)
	{
		case NON_RUNTIME_EXPR:
		case EXPR_COND:
		case EXPR_CT_ARG:
		case EXPR_ASM:
		case EXPR_VASPLAT:
		case EXPR_CT_CHECKS:
		case EXPR_BUILTIN:
			UNREACHABLE
		case EXPR_CONST:
			tilde_emit_const_expr(c, value, expr);
			return;
		case EXPR_BINARY:
			tilde_emit_binary_expr(c, value, expr);
			return;
		case EXPR_NOP:
			value_set(value, TB_NULL_REG, type_void);
			return;
		case EXPR_IDENTIFIER:
			value_set_decl(c, value, expr->identifier_expr.decl);
			return;
		case EXPR_CALL:
			tilde_emit_call_expr(c, value, expr, NULL);
			return;

		default:
			break;
	}
	TODO
}

void tilde_emit_cast(TildeContext *c, CastKind cast_kind, Expr *expr, TBEValue *value, Type *to_type, Type *from_type)
{
	Type *to_type_original = to_type;
	to_type = type_flatten(to_type);
	from_type = type_flatten(from_type);

	switch (cast_kind)
	{
		case CAST_NUMVEC:
			TODO //	llvm_emit_num_to_vec_cast(c, value, to_type, from_type);
			return;
		case CAST_BOOLVECINT:
			TODO // llvm_emit_bool_to_intvec_cast(c, value, to_type, from_type);
			return;
		case CAST_ARRVEC:
			TODO // llvm_emit_array_to_vector_cast(c, value, to_type, from_type);
			return;
		case CAST_PTRANY:
		{
			value_rvalue(c, value);
			TODO /*--
			LLVMValueRef pointer = llvm_emit_bitcast(c, value->value, type_voidptr);
			BEValue typeid;
			llvm_emit_typeid(c, &typeid, from_type->pointer);
			llvm_value_aggregate_two(c, value, to_type, pointer, typeid.value);
			return;--*/
		}
		case CAST_BSARRY:
			TODO
			/*--
			llvm_value_addr(c, value);
			llvm_value_bitcast(c, value, to_type);
			llvm_value_rvalue(c, value);--*/
			return;
			/*
		case CAST_BSINT:
			llvm_value_addr(c, value);
			llvm_value_bitcast(c, value, to_type);
			llvm_value_rvalue(c, value);
			return;
		case CAST_EUINT:
		case CAST_ERINT:
			to_type = type_lowering(to_type);
			from_type = type_lowering(from_type);
			llvm_value_rvalue(c, value);
			if (type_convert_will_trunc(to_type, from_type))
			{
				value->value = LLVMBuildTrunc(c->builder, value->value, llvm_get_type(c, to_type), "errinttrunc");
			}
			else
			{
				value->value = type_is_signed(to_type)
				               ? LLVMBuildSExt(c->builder, value->value, llvm_get_type(c, to_type), "errsiext")
				               : LLVMBuildZExt(c->builder, value->value, llvm_get_type(c, to_type), "erruiext");

			}
			break;
		case CAST_ANYPTR:
			llvm_emit_any_pointer(c, value, value);
			if (llvm_value_is_addr(value))
			{
				value->value = LLVMBuildBitCast(c->builder, value->value, llvm_get_ptr_type(c, to_type), "");
			}
			else
			{
				value->value = LLVMBuildBitCast(c->builder, value->value, llvm_get_type(c, to_type), "");
			}
			break;
		case CAST_XIERR:
			to_type = type_lowering(to_type);
			llvm_value_rvalue(c, value);
			value->value = llvm_zext_trunc(c, value->value, llvm_get_type(c, to_type));
			break;
		case CAST_ERROR:
			UNREACHABLE
		case CAST_STRPTR:
		case CAST_PTRPTR:
			llvm_value_rvalue(c, value);
			value->value = LLVMBuildPointerCast(c->builder, value->value, llvm_get_type(c, to_type), "ptrptr");
			break;
		case CAST_PTRXI:
			llvm_value_rvalue(c, value);
			value->value = LLVMBuildPtrToInt(c->builder, value->value, llvm_get_type(c, to_type), "ptrxi");
			break;
		case CAST_APTSA:
			llvm_emit_arr_to_subarray_cast(c, value, to_type);
			break;
		case CAST_SASA:
			assert(type_is_pointer(value->type->array.base));
			llvm_value_addr(c, value);
			llvm_value_bitcast(c, value, to_type);
			break;
		case CAST_SAPTR:
			llvm_emit_subarray_pointer(c, value, value);
			if (value->type != to_type)
			{
				if (llvm_value_is_addr(value))
				{
					value->value = LLVMBuildPointerCast(c->builder, value->value, llvm_get_ptr_type(c, to_type), "saptr");
				}
				else
				{
					value->value = LLVMBuildPointerCast(c->builder, value->value, llvm_get_ptr_type(c, to_type), "saptr");
				}
			}
			break;
		case CAST_EREU:
			// This is a no op.
			assert(type_lowering(to_type) == type_lowering(from_type));
			break;
		case CAST_VECARR:
			llvm_emit_vector_to_array_cast(c, value, to_type, from_type);
			break;
		case CAST_EUER:
			TODO // gencontext_emit_value_bitcast(c, value->value, to_type, from_type);
		case CAST_ERBOOL:
		case CAST_EUBOOL:
		{
			BEValue zero;
			llvm_value_set_int(c, &zero, type_anyfault, 0);
			llvm_emit_int_comp(c, value, value, &zero, BINARYOP_NE);
			break;
		}
		case CAST_PTRBOOL:
			llvm_value_rvalue(c, value);
			value->value = LLVMBuildIsNotNull(c->builder, value->value, "ptrbool");
			value->kind = BE_BOOLEAN;
			break;
		case CAST_BOOLINT:
			llvm_value_rvalue(c, value);
			value->value =  LLVMBuildZExt(c->builder, value->value, llvm_get_type(c, to_type), "boolsi");
			value->kind = BE_VALUE;
			break;
		case CAST_FPBOOL:
			llvm_value_rvalue(c, value);
			value->value =  LLVMBuildFCmp(c->builder, LLVMRealUNE, value->value, llvm_get_zero(c, from_type), "fpbool");
			value->kind = BE_BOOLEAN;
			break;
		case CAST_BOOLBOOL:
			value->value = LLVMBuildTrunc(c->builder, value->value, c->bool_type, "boolbool");
			value->kind = BE_BOOLEAN;
			break;
		case CAST_BOOLFP:
			llvm_value_rvalue(c, value);
			value->value =  LLVMBuildUIToFP(c->builder, value->value, llvm_get_type(c, to_type), "boolfp");
			value->kind = BE_VALUE;
			break;
		case CAST_INTBOOL:
			llvm_value_rvalue(c, value);
			value->value = LLVMBuildICmp(c->builder, LLVMIntNE, value->value, llvm_get_zero(c, from_type), "intbool");
			value->kind = type_kind_is_any_vector(value->type->type_kind) ? BE_BOOLVECTOR : BE_BOOLEAN;
			break;
		case CAST_FPFP:
			llvm_value_rvalue(c, value);
			value->value = type_convert_will_trunc(to_type, from_type)
			               ? LLVMBuildFPTrunc(c->builder, value->value, llvm_get_type(c, to_type), "fpfptrunc")
			               : LLVMBuildFPExt(c->builder, value->value, llvm_get_type(c, to_type), "fpfpext");
			break;
		case CAST_FPSI:
			llvm_value_rvalue(c, value);
			value->value = LLVMBuildFPToSI(c->builder, value->value, llvm_get_type(c, to_type), "fpsi");
			break;
		case CAST_FPUI:
			llvm_value_rvalue(c, value);
			value->value = LLVMBuildFPToUI(c->builder, value->value, llvm_get_type(c, to_type), "fpui");
			break;
		case CAST_SISI:
			llvm_value_rvalue(c, value);
			value->value = type_convert_will_trunc(to_type, from_type)
			               ? LLVMBuildTrunc(c->builder, value->value, llvm_get_type(c, to_type), "sisitrunc")
			               : LLVMBuildSExt(c->builder, value->value, llvm_get_type(c, to_type), "sisiext");
			break;
		case CAST_SIUI:
			llvm_value_rvalue(c, value);
			value->value = type_convert_will_trunc(to_type, from_type)
			               ? LLVMBuildTrunc(c->builder, value->value, llvm_get_type(c, to_type), "siuitrunc")
			               : LLVMBuildSExt(c->builder, value->value, llvm_get_type(c, to_type), "siuiext");
			break;
		case CAST_SIFP:
			llvm_value_rvalue(c, value);
			value->value = LLVMBuildSIToFP(c->builder, value->value, llvm_get_type(c, to_type), "sifp");
			break;
		case CAST_XIPTR:
			llvm_value_rvalue(c, value);
			value->value = LLVMBuildIntToPtr(c->builder, value->value, llvm_get_type(c, to_type), "xiptr");
			break;
		case CAST_UISI:
			llvm_value_rvalue(c, value);
			value->value = type_convert_will_trunc(to_type, from_type)
			               ? LLVMBuildTrunc(c->builder, value->value, llvm_get_type(c, to_type), "uisitrunc")
			               : LLVMBuildZExt(c->builder, value->value, llvm_get_type(c, to_type), "uisiext");
			break;
		case CAST_UIUI:
			llvm_value_rvalue(c, value);
			value->value = llvm_zext_trunc(c, value->value, llvm_get_type(c, to_type));
			break;
		case CAST_UIFP:
			llvm_value_rvalue(c, value);
			value->value = LLVMBuildUIToFP(c->builder, value->value, llvm_get_type(c, to_type), "uifp");
			break;
		case CAST_ENUMLOW:
			llvm_value_rvalue(c, value);
			break;
		case CAST_STST:
			llvm_value_addr(c, value);
			value->value = LLVMBuildBitCast(c->builder, value->value, llvm_get_ptr_type(c, to_type), "");
			value->type = to_type;
			return;
		case CAST_INTENUM:
			if (active_target.feature.safe_mode && c->builder != c->global_builder)
			{
				llvm_value_rvalue(c, value);
				BEValue check;
				Decl *decl = to_type_original->canonical->decl;
				unsigned max = vec_size(decl->enums.values);
				if (type_is_signed(value->type))
				{
					scratch_buffer_clear();
					scratch_buffer_printf("Conversion to enum '%s' failed - tried to convert a negative value.", decl->name);
					llvm_emit_int_comp_zero(c, &check, value, BINARYOP_LT);
					llvm_emit_panic_on_true(c, check.value, scratch_buffer_to_string(), expr->span);
				}
				scratch_buffer_clear();
				scratch_buffer_printf("Conversion to enum '%s' failed - the value was greater than %u.", decl->name, max - 1);
				LLVMValueRef val = llvm_const_int(c, value->type, max);
				llvm_emit_int_comp_raw(c, &check, value->type, value->type, value->value, val, BINARYOP_GE);
				llvm_emit_panic_on_true(c, check.value,scratch_buffer_to_string(), expr->span);
			}
			return;
		case CAST_SABOOL:
			llvm_value_fold_optional(c, value);
			if (llvm_value_is_addr(value))
			{
				value->value = llvm_emit_struct_gep_raw(c,
				                                        value->value,
				                                        llvm_get_type(c, value->type),
				                                        1,
				                                        value->alignment,
				                                        &value->alignment);
			}
			else
			{
				value->value = llvm_emit_extract_value(c, value->value, 1);
			}
			value->type = type_usz->canonical;
			llvm_value_rvalue(c, value);
			llvm_emit_int_comp_zero(c, value, value, BINARYOP_NE);
			break;*/
		default:
			TODO
	}
	value->type = to_type;
}
