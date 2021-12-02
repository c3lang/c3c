// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.


#include "llvm_codegen_internal.h"

static void llvm_emit_param_attributes(GenContext *context, LLVMValueRef function, ABIArgInfo *info, bool is_return, int index, int last_index);
static inline void llvm_emit_return_value(GenContext *context, LLVMValueRef value);
static void llvm_expand_from_args(GenContext *c, Type *type, LLVMValueRef ref, unsigned *index, AlignSize alignment);
static inline void llvm_process_parameter_value(GenContext *c, Decl *decl, unsigned *index);

bool llvm_emit_check_block_branch(GenContext *context)
{
	if (!context->current_block) return false;
	// If it's not used, we can delete the previous block and skip the branch.
	// Unless it is the entry block or a label target for jumps
	// These empty blocks will occur when doing branches.
	// Consider:
	// while (1)
	// {
	//   break;
	//   break;
	// }
	// Naively we'd output
	// br label %for.cond  - 1st break
	// br label %for.cond  - 2nd break
	// br label %for.cond  - end of scope
	//
	// The fix is to introduce a new block after a break:
	// br label %for.cond
	// jmp:
	// br label %for.cond
	// jmp.1:
	// br label %for.cond
	//
	// But this leaves us with blocks that have no parent.
	// Consequently we will delete those and realize that
	// we then have no need for emitting a br.
	if (!context->current_block_is_target
	    && !LLVMGetFirstUse(LLVMBasicBlockAsValue(context->current_block)))
	{
		LLVMDeleteBasicBlock(context->current_block);
		context->current_block = NULL;
		return false;
	}
	return true;
};

void llvm_emit_br(GenContext *c, LLVMBasicBlockRef next_block)
{
	if (!llvm_emit_check_block_branch(c)) return;
	c->current_block = NULL;
	LLVMBuildBr(c->builder, next_block);
}



void llvm_emit_cond_br(GenContext *context, BEValue *value, LLVMBasicBlockRef then_block, LLVMBasicBlockRef else_block)
{
	assert(context->current_block);
	assert(value->kind == BE_BOOLEAN);
	LLVMBuildCondBr(context->builder, value->value, then_block, else_block);
	LLVMClearInsertionPosition(context->builder);
	context->current_block = NULL;
	context->current_block_is_target = false;
}


void llvm_emit_block(GenContext *context, LLVMBasicBlockRef next_block)
{
	assert(context->current_block == NULL);
	LLVMAppendExistingBasicBlock(context->function, next_block);
	LLVMPositionBuilderAtEnd(context->builder, next_block);
	context->current_block = next_block;
	context->current_block_is_target = false;
}

static void llvm_expand_from_args(GenContext *c, Type *type, LLVMValueRef ref, unsigned *index, AlignSize alignment)
{
	switch (type->type_kind)
	{
		case TYPE_ARRAY:
		{
			LLVMTypeRef array_type = llvm_get_type(c, type);
			for (unsigned i = 0; i < type->array.len; i++)
			{
				AlignSize element_align;
				LLVMValueRef target = llvm_emit_array_gep_raw(c, ref, array_type, i, alignment, &element_align);
				llvm_expand_from_args(c, type->array.base, target, index, element_align);
			}
			return;
		}
		case TYPE_STRUCT:
		{
			LLVMTypeRef struct_type = llvm_get_type(c, type);
			Decl **members = type->decl->strukt.members;
			VECEACH(members, i)
			{
				AlignSize element_align;
				LLVMValueRef target = llvm_emit_struct_gep_raw(c, ref, struct_type, i, alignment, &element_align);
				llvm_expand_from_args(c, members[i]->type, target, index, element_align);
			}
			return;
		}
		case TYPE_UNION:
		{
			Type *largest_type = type_find_largest_union_element(type);
			LLVMValueRef cast_addr = llvm_emit_bitcast(c, ref, type_get_ptr(largest_type));
			llvm_expand_from_args(c, largest_type, cast_addr, index, alignment);
			return;
		}
		default:
			llvm_store_aligned(c, ref, llvm_get_next_param(c, index), alignment);
			return;
	}
}

LLVMValueRef llvm_get_next_param(GenContext *context, unsigned *index)
{
	return LLVMGetParam(context->function, (*index)++);
}


static inline void llvm_process_parameter_value(GenContext *c, Decl *decl, unsigned *index)
{
	ABIArgInfo *info = decl->var.abi_info;
	switch (info->kind)
	{
		case ABI_ARG_IGNORE:
			return;
		case ABI_ARG_INDIRECT:
		{
			// A simple memcopy, with alignment respected.
			LLVMValueRef pointer = llvm_get_next_param(c, index);
			llvm_emit_memcpy_to_decl(c, decl, pointer, info->indirect.alignment);
			return;
		}
		case ABI_ARG_EXPAND_COERCE:
		{
			// Create the expand type:
			LLVMTypeRef coerce_type = llvm_get_coerce_type(c, info);
			LLVMValueRef temp = LLVMBuildBitCast(c->builder, decl->backend_ref, LLVMPointerType(coerce_type, 0), "coerce");

			AlignSize alignment = decl->alignment;
			AlignSize element_align;
			LLVMValueRef gep_first = llvm_emit_struct_gep_raw(c, temp, coerce_type, info->coerce_expand.lo_index, alignment, &element_align);
			llvm_store_aligned(c, gep_first, llvm_get_next_param(c, index), element_align);
			if (info->coerce_expand.hi)
			{
				LLVMValueRef gep_second = llvm_emit_struct_gep_raw(c, temp, coerce_type, info->coerce_expand.hi_index, alignment, &element_align);
				llvm_store_aligned(c, gep_second, llvm_get_next_param(c, index), element_align);
			}
			break;
		}
		case ABI_ARG_DIRECT_PAIR:
		{
			// Here we do the following transform:
			// lo, hi -> { lo, hi } -> struct
			LLVMTypeRef lo = llvm_abi_type(c, info->direct_pair.lo);
			LLVMTypeRef hi = llvm_abi_type(c, info->direct_pair.hi);
			LLVMTypeRef struct_type = llvm_get_twostruct(c, lo, hi);
			AlignSize decl_alignment = decl->alignment;
			// Cast to { lo, hi }
			LLVMValueRef cast = LLVMBuildBitCast(c->builder, decl->backend_ref, LLVMPointerType(struct_type, 0), "pair");
			// Point to the lo value.
			AlignSize element_align;
			LLVMValueRef lo_ptr = llvm_emit_struct_gep_raw(c, cast, struct_type, 0, decl_alignment, &element_align);
			// Store it in the struct.
			llvm_store_aligned(c, lo_ptr, llvm_get_next_param(c, index), element_align);
			// Point to the hi value.
			LLVMValueRef hi_ptr = llvm_emit_struct_gep_raw(c, cast, struct_type, 1, decl_alignment, &element_align);
			// Store it in the struct.
			llvm_store_aligned(c, hi_ptr, llvm_get_next_param(c, index), element_align);
			return;
		}
		case ABI_ARG_DIRECT_COERCE:
		{
			LLVMTypeRef coerce_type = llvm_get_coerce_type(c, info);
			if (!coerce_type || coerce_type == llvm_get_type(c, decl->type))
			{
				llvm_store_aligned_decl(c, decl, llvm_get_next_param(c, index));
				return;
			}

			// If we're not flattening, we simply do a store.
			if (!abi_info_should_flatten(info))
			{
				LLVMValueRef param = llvm_get_next_param(c, index);
				// Store it with the alignment of the decl.
				llvm_emit_coerce_store(c, decl->backend_ref, decl->alignment, coerce_type, param, llvm_get_type(c, decl->type));
				return;
			}

			// In this case we've been flattening the parameter into multiple registers.
			LLVMTypeRef element_type = llvm_abi_type(c, info->direct_coerce.type);

			// Cast to the coerce type.
			LLVMValueRef cast = LLVMBuildBitCast(c->builder, decl->backend_ref, LLVMPointerType(coerce_type, 0), "coerce");

			AlignSize decl_alignment = decl->alignment;
			// Store each expanded parameter.
			for (unsigned idx = 0; idx < info->direct_coerce.elements; idx++)
			{
				AlignSize align;
				LLVMValueRef element_ptr = llvm_emit_struct_gep_raw(c, cast, coerce_type, idx, decl_alignment, &align);
				LLVMValueRef value = llvm_get_next_param(c, index);
				llvm_store_aligned(c, element_ptr, value, align);
			}
			return;
		}
		case ABI_ARG_EXPAND:
		{
			llvm_expand_from_args(c, decl->type, decl->backend_ref, index, decl->alignment);
			if (info->expand.padding_type)
			{
				// Skip the pad.
				llvm_get_next_param(c, index);
			}
		}
	}
}
static inline void llvm_emit_parameter(GenContext *context, Decl *decl, unsigned *index, unsigned real_index)
{
	assert(decl->decl_kind == DECL_VAR && decl->var.kind == VARDECL_PARAM);

	// Allocate room on stack, but do not copy.
	llvm_emit_and_set_decl_alloca(context, decl);
	llvm_process_parameter_value(context, decl, index);
	if (llvm_use_debug(context))
	{
		llvm_emit_debug_parameter(context, decl, real_index);
	}
}

static inline void llvm_emit_return_value(GenContext *context, LLVMValueRef value)
{
	if (!value)
	{
		LLVMBuildRetVoid(context->builder);
	}
	else
	{
		LLVMBuildRet(context->builder, value);
	}
	context->current_block = NULL;
	context->current_block_is_target = false;
}

void llvm_emit_return_abi(GenContext *c, BEValue *return_value, BEValue *failable)
{
	FunctionSignature *signature = &c->cur_func_decl->func_decl.function_signature;
	ABIArgInfo *info = signature->ret_abi_info;

	// If we have a failable it's always the return argument, so we need to copy
	// the return value into the return value holder.
	LLVMValueRef return_out = c->return_out;
	bool is_failable = IS_FAILABLE(signature->rtype);
	Type *return_type = signature->rtype->type;
	if (is_failable) return_type = return_type->failable;

	BEValue no_fail;

	// In this case we use the failable as the actual return.
	if (is_failable)
	{
		if (return_value && return_value->value)
		{

			llvm_store_bevalue_aligned(c, c->return_out, return_value, 0);
		}
		return_out = c->failable_out;
		return_type = type_anyerr;
		if (!failable)
		{
			llvm_value_set(&no_fail, LLVMConstNull(llvm_get_type(c, type_anyerr)), type_anyerr);
			failable = &no_fail;
		}
		return_value = failable;
		info = signature->failable_abi_info;
	}

	switch (info->kind)
	{
		case ABI_ARG_INDIRECT:
			llvm_store_bevalue_aligned(c, return_out, return_value, info->indirect.alignment);
			llvm_emit_return_value(c, NULL);
			return;
		case ABI_ARG_IGNORE:
			llvm_emit_return_value(c, NULL);
			return;
		case ABI_ARG_EXPAND:
			// Expands to multiple slots -
			// Not applicable to return values.
			UNREACHABLE
		case ABI_ARG_EXPAND_COERCE:
		{
			// Pick the return as an address.
			llvm_value_addr(c, return_value);
			// Get the coerce type.
			LLVMTypeRef coerce_type = llvm_get_coerce_type(c, info);
			// Create the new pointer
			LLVMValueRef coerce = LLVMBuildBitCast(c->builder, return_value->value, coerce_type, "");
			// We might have only one value, in that case, build a GEP to that one.
			LLVMValueRef lo_val;
			AlignSize alignment;
			LLVMValueRef lo = llvm_emit_struct_gep_raw(c, coerce, coerce_type, info->coerce_expand.lo_index,
			                                           return_value->alignment, &alignment);
			LLVMTypeRef lo_type = llvm_abi_type(c, info->coerce_expand.lo);
			lo_val = llvm_emit_load_aligned(c, lo_type, lo, alignment, "");

			// We're done if there's a single field.
			if (!info->coerce_expand.hi)
			{
				llvm_emit_return_value(c, lo_val);
				return;
			}

			// Let's make a first class aggregate
			LLVMValueRef hi = llvm_emit_struct_gep_raw(c, coerce, coerce_type, info->coerce_expand.hi_index,
			                                           return_value->alignment, &alignment);
			LLVMTypeRef hi_type = llvm_abi_type(c, info->coerce_expand.hi);
			LLVMValueRef hi_val = llvm_emit_load_aligned(c, hi_type, hi, alignment, "");

			LLVMTypeRef unpadded_type = llvm_get_twostruct(c, lo_type, hi_type);
			LLVMValueRef composite = LLVMGetUndef(unpadded_type);

			composite = LLVMBuildInsertValue(c->builder, composite, lo_val, 0, "");
			composite = LLVMBuildInsertValue(c->builder, composite, hi_val, 1, "");

			// And return that unpadded result
			llvm_emit_return_value(c, composite);
			break;
		}
		case ABI_ARG_DIRECT_PAIR:
		case ABI_ARG_DIRECT_COERCE:
		{
			LLVMTypeRef coerce_type = llvm_get_coerce_type(c, info);
			if (!coerce_type || coerce_type == llvm_get_type(c, return_type))
			{
				// The normal return
				llvm_emit_return_value(c, llvm_value_rvalue_store(c, return_value));
				return;
			}
			assert(!abi_info_should_flatten(info));
			llvm_emit_return_value(c, llvm_emit_coerce(c, coerce_type, return_value, return_type));
			return;
		}
	}
}

void llvm_emit_return_implicit(GenContext *c)
{
	Type *rtype_real = c->cur_func_decl->func_decl.function_signature.rtype->type;
	if (type_lowering(type_no_fail(rtype_real)) != type_void)
	{
		LLVMBuildUnreachable(c->builder);
		return;
	}
	if (type_is_failable(rtype_real))
	{
		llvm_emit_return_abi(c, NULL, NULL);
		return;
	}
	BEValue value;
	llvm_value_set(&value, llvm_get_zero(c, type_anyerr), type_anyerr);
	llvm_emit_return_abi(c, NULL, &value);
}

void llvm_emit_function_body(GenContext *context, Decl *decl)
{
	DEBUG_LOG("Generating function %s.", decl->external_name);
	assert(decl->backend_ref);

	bool emit_debug = llvm_use_debug(context);
	LLVMValueRef prev_function = context->function;
	LLVMBuilderRef prev_builder = context->builder;

	context->error_var = NULL;
	context->catch_block = NULL;

	context->function = decl->backend_ref;
	if (emit_debug)
	{
		context->debug.function = LLVMGetSubprogram(context->function);
	}

	context->cur_func_decl = decl;

	LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(context->context, context->function, "entry");
	context->current_block = entry;
	context->current_block_is_target = true;
	context->block_return_exit = NULL;
	context->in_block = 0;
	context->builder = LLVMCreateBuilderInContext(context->context);
	LLVMPositionBuilderAtEnd(context->builder, entry);

	LLVMValueRef alloca_point = LLVMBuildAlloca(context->builder, LLVMInt32TypeInContext(context->context), "alloca_point");
	context->alloca_point = alloca_point;

	FunctionSignature *signature = &decl->func_decl.function_signature;
	unsigned arg = 0;

	if (emit_debug)
	{
		llvm_debug_scope_push(context, context->debug.function);
	}

	bool is_failable = IS_FAILABLE(signature->rtype);
	if (is_failable && signature->failable_abi_info->kind == ABI_ARG_INDIRECT)
	{
		context->failable_out = LLVMGetParam(context->function, arg++);
	}
	else
	{
		context->failable_out = NULL;
	}
	if (signature->ret_abi_info && signature->ret_abi_info->kind == ABI_ARG_INDIRECT)
	{
		context->return_out = LLVMGetParam(context->function, arg++);
	}
	else
	{
		context->return_out = NULL;
		if (signature->ret_abi_info && is_failable)
		{
			context->return_out = LLVMGetParam(context->function, arg++);
		}
	}


	if (!decl->func_decl.attr_naked)
	{
		// Generate LLVMValueRef's for all parameters, so we can use them as local vars in code
		VECEACH(decl->func_decl.function_signature.params, i)
		{
			llvm_emit_parameter(context, decl->func_decl.function_signature.params[i], &arg, i);
		}
	}

	LLVMSetCurrentDebugLocation2(context->builder, NULL);

	VECEACH(decl->func_decl.body->compound_stmt.stmts, i)
	{
		llvm_emit_stmt(context, decl->func_decl.body->compound_stmt.stmts[i]);
	}

	if (context->current_block && llvm_basic_block_is_unused(context->current_block))
	{
		LLVMBasicBlockRef prev_block = LLVMGetPreviousBasicBlock(context->current_block);
		LLVMDeleteBasicBlock(context->current_block);
		context->current_block = prev_block;
		LLVMPositionBuilderAtEnd(context->builder, context->current_block);
	}
	// Insert a return (and defer) if needed.
	if (context->current_block && !LLVMGetBasicBlockTerminator(context->current_block))
	{
		assert(!decl->func_decl.body->compound_stmt.defer_list.end);
		llvm_emit_defer(context, decl->func_decl.body->compound_stmt.defer_list.start, 0);
		llvm_emit_return_implicit(context);
	}

	// erase alloca point
	if (LLVMGetInstructionParent(alloca_point))
	{
		context->alloca_point = NULL;
		LLVMInstructionEraseFromParent(alloca_point);
	}

	LLVMDisposeBuilder(context->builder);

	if (llvm_use_debug(context))
	{
		llvm_debug_scope_pop(context);
	}

	context->builder = prev_builder;
	context->function = prev_function;
}

static void llvm_emit_param_attributes(GenContext *context, LLVMValueRef function, ABIArgInfo *info, bool is_return, int index, int last_index)
{
	assert(last_index == index || info->kind == ABI_ARG_DIRECT_PAIR || info->kind == ABI_ARG_IGNORE
	       || info->kind == ABI_ARG_EXPAND || info->kind == ABI_ARG_DIRECT_COERCE);

	if (info->attributes.zeroext)
	{
		// Direct only
		assert(index == last_index);
		llvm_attribute_add(context, function, attribute_id.zext, index);
	}
	if (info->attributes.signext)
	{
		// Direct only
		assert(index == last_index);
		llvm_attribute_add(context, function, attribute_id.sext, index);
	}
	if (info->attributes.by_reg)
	{
		llvm_attribute_add_range(context, function, attribute_id.inreg, index, last_index);
	}
	switch (info->kind)
	{
		case ABI_ARG_EXPAND:
		case ABI_ARG_IGNORE:
		case ABI_ARG_DIRECT_COERCE:
		case ABI_ARG_DIRECT_PAIR:
		case ABI_ARG_EXPAND_COERCE:
			break;
		case ABI_ARG_INDIRECT:
			if (is_return)
			{
				assert(info->indirect.type);
				llvm_attribute_add_type(context, function, attribute_id.sret, llvm_get_type(context, info->indirect.type), 1);
				llvm_attribute_add_int(context, function, attribute_id.align, info->indirect.alignment, 1);
			}
			else
			{
				if (info->attributes.by_val) llvm_attribute_add_type(context, function, attribute_id.byval, llvm_get_type(context, info->indirect.type), index);
				llvm_attribute_add_int(context, function, attribute_id.align, info->indirect.alignment, index);
			}
			break;

	}

}
void llvm_emit_function_decl(GenContext *c, Decl *decl)
{
	assert(decl->decl_kind == DECL_FUNC);
	// Resolve function backend type for function.
	LLVMValueRef function = LLVMAddFunction(c->module, decl->extname ? decl->extname : decl->external_name, llvm_get_type(c, decl->type));
	decl->backend_ref = function;
	FunctionSignature *signature = &decl->func_decl.function_signature;
	FunctionSignature *type_signature = decl->type->func.signature;

	// We only resolve 1 function signature, so we might have functions
	// with the same signature (but different default values!)
	// that we have in common. So overwrite the data from the type here.
	if (signature != type_signature)
	{
		// Store the params.
		Decl **params = signature->params;
		// Copy the rest.
		*signature = *type_signature;
		signature->params = params;
		VECEACH(params, i)
		{
			Decl *sig_param = type_signature->params[i];
			Decl *param = params[i];
			param->var.abi_info = sig_param->var.abi_info;
		}
		signature->params = params;
	}

	ABIArgInfo *ret_abi_info = signature->failable_abi_info ? signature->failable_abi_info : signature->ret_abi_info;
	llvm_emit_param_attributes(c, function, ret_abi_info, true, 0, 0);
	Decl **params = signature->params;
	if (signature->failable_abi_info && signature->ret_abi_info)
	{
		ABIArgInfo *info = signature->ret_abi_info;
		llvm_emit_param_attributes(c, function, info, false, info->param_index_start + 1, info->param_index_end);
	}
	VECEACH(params, i)
	{
		Decl *param = params[i];
		ABIArgInfo *info = param->var.abi_info;
		llvm_emit_param_attributes(c, function, info, false, info->param_index_start + 1, info->param_index_end);
	}
	// We ignore decl->func_decl.attr_inline and place it in every call instead.
	if (decl->func_decl.attr_noinline)
	{
		llvm_attribute_add(c, function, attribute_id.noinline, -1);
	}
	if (decl->func_decl.attr_noreturn)
	{
		llvm_attribute_add(c, function, attribute_id.noreturn, -1);
	}
	if (decl->alignment)
	{
		llvm_set_alignment(function, decl->alignment);
	}
	if (decl->section)
	{
		LLVMSetSection(function, decl->section);
	}
	llvm_attribute_add(c, function, attribute_id.nounwind, -1);
	if (decl->func_decl.attr_naked)
	{
		llvm_attribute_add(c, function, attribute_id.naked, -1);
	}
	if (decl->func_decl.function_signature.call_abi == CALL_X86_STD)
	{
		if (platform_target.os == OS_TYPE_WIN32)
		{
			LLVMSetDLLStorageClass(function, LLVMDLLImportStorageClass);
		}
	}
	LLVMSetFunctionCallConv(function, llvm_call_convention_from_call(decl->func_decl.function_signature.call_abi, platform_target.arch, platform_target.os));

	switch (decl->visibility)
	{
		case VISIBLE_EXTERN:
			LLVMSetLinkage(function, decl->func_decl.attr_weak ? LLVMExternalWeakLinkage : LLVMExternalLinkage);
			LLVMSetVisibility(function, LLVMDefaultVisibility);
			break;
		case VISIBLE_PUBLIC:
		case VISIBLE_MODULE:
			if (decl->func_decl.attr_weak) LLVMSetLinkage(function, LLVMWeakAnyLinkage);
			LLVMSetVisibility(function, LLVMDefaultVisibility);
			break;
		case VISIBLE_LOCAL:
			LLVMSetLinkage(function, decl->func_decl.attr_weak ? LLVMLinkerPrivateWeakLinkage : LLVMInternalLinkage);
			LLVMSetVisibility(function, LLVMDefaultVisibility);
			break;;
	}
	if (llvm_use_debug(c))
	{
		llvm_emit_debug_function(c, decl);
	}
}


void llvm_emit_methods(GenContext *c, Decl **methods)
{
	VECEACH(methods, i)
	{
		Decl *decl = methods[i];
		if (decl->decl_kind == DECL_MACRO) continue;
		llvm_emit_function_decl(c, decl);
	}
}

void llvm_emit_extern_decl(GenContext *context, Decl *decl)
{
	switch (decl->decl_kind)
	{
		case DECL_POISONED:
			UNREACHABLE;
		case DECL_FUNC:
			decl->backend_ref = LLVMAddFunction(context->module, decl->extname ? decl->extname : decl->external_name,
			                                    llvm_get_type(context, decl->type));
			LLVMSetVisibility(decl->backend_ref, LLVMDefaultVisibility);
			break;
		case DECL_VAR:
			decl->backend_ref = LLVMAddGlobal(context->module, llvm_get_type(context, decl->type), decl->extname ? decl->extname : decl->external_name);
			LLVMSetVisibility(decl->backend_ref, LLVMDefaultVisibility);
			break;
		case DECL_BITSTRUCT:
		case DECL_STRUCT:
		case DECL_UNION:
			llvm_get_type(context, decl->type);
			// TODO // Fix typeid
			break;
		case DECL_ENUM:
			break;
		case DECL_ERRTYPE:
			gencontext_emit_introspection_type(context, decl);
			// TODO // Fix typeid
			return;
		case DECL_TYPEDEF:
		case DECL_DISTINCT:
		case NON_TYPE_DECLS:
		case DECL_ENUM_CONSTANT:
		case DECL_ERRVALUE:
			return;
	}
}
