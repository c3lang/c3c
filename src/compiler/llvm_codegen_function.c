// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.


#include "llvm_codegen_internal.h"





bool gencontext_check_block_branch_emit(GenContext *context)
{
	assert(context->current_block);
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

void gencontext_emit_br(GenContext *context, LLVMBasicBlockRef next_block)
{
	if (!gencontext_check_block_branch_emit(context)) return;
	context->current_block = NULL;
	LLVMBuildBr(context->builder, next_block);
}

void gencontext_emit_cond_br(GenContext *context, LLVMValueRef value, LLVMBasicBlockRef thenBlock, LLVMBasicBlockRef elseBlock)
{
	assert(context->current_block);
	LLVMBuildCondBr(context->builder, value, thenBlock, elseBlock);
	LLVMClearInsertionPosition(context->builder);
	context->current_block = NULL;
	context->current_block_is_target = false;
}


void gencontext_emit_block(GenContext *context, LLVMBasicBlockRef next_block)
{
	assert(context->current_block == NULL);
	LLVMAppendExistingBasicBlock(context->function, next_block);
	LLVMPositionBuilderAtEnd(context->builder, next_block);
	context->current_block = next_block;
	context->current_block_is_target = false;
}


static inline void gencontext_emit_parameter(GenContext *context, Decl *decl, unsigned index)
{
	assert(decl->decl_kind == DECL_VAR && decl->var.kind == VARDECL_PARAM);

	// Allocate room on stack and copy.
	decl->var.backend_ref = gencontext_emit_alloca(context, llvm_type(decl->type), decl->name);
	LLVMBuildStore(context->builder, LLVMGetParam(context->function, index), decl->var.backend_ref);
}

void gencontext_emit_implicit_return(GenContext *context)
{
	switch (context->cur_func_decl->func.function_signature.error_return)
	{
		case ERROR_RETURN_NONE:
			LLVMBuildRetVoid(context->builder);
			return;
		case ERROR_RETURN_ANY:
		case ERROR_RETURN_MANY:
			LLVMBuildRet(context->builder, llvm_int(type_usize, 0));
			return;
		case ERROR_RETURN_ONE:
			LLVMBuildRet(context->builder, llvm_int(type_error_base, 0));
			return;
	}
	UNREACHABLE
}

void gencontext_emit_function_body(GenContext *context, Decl *decl)
{
	DEBUG_LOG("Generating function %s.", decl->external_name);
	assert(decl->func.backend_value);

	LLVMValueRef prev_function = context->function;
	LLVMBuilderRef prev_builder = context->builder;

	context->function = decl->func.backend_value;
	context->cur_func_decl = decl;

	LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(context->context, context->function, "entry");
	context->current_block = entry;
	context->current_block_is_target = true;
	context->expr_block_exit = NULL;
	context->builder = LLVMCreateBuilderInContext(context->context);
	LLVMPositionBuilderAtEnd(context->builder, entry);

	LLVMValueRef alloca_point = LLVMBuildAlloca(context->builder, LLVMInt32TypeInContext(context->context), "alloca_point");
	context->alloca_point = alloca_point;

	FunctionSignature *signature = &decl->func.function_signature;
	int arg = 0;

	if (signature->return_param)
	{
		context->return_out = LLVMGetParam(context->function, arg++);
	}
	else
	{
		context->return_out = NULL;
	}

	// Generate LLVMValueRef's for all parameters, so we can use them as local vars in code
	VECEACH(decl->func.function_signature.params, i)
	{
		gencontext_emit_parameter(context, decl->func.function_signature.params[i], arg++);
	}

	VECEACH(decl->func.labels, i)
	{
		Ast *label = decl->func.labels[i];
		label->label_stmt.backend_value = gencontext_create_free_block(context, label->label_stmt.name);
	}

	VECEACH(decl->func.body->compound_stmt.stmts, i)
	{
		gencontext_emit_stmt(context, decl->func.body->compound_stmt.stmts[i]);
	}

	if (context->current_block && !LLVMGetFirstInstruction(context->current_block) && !LLVMGetFirstUse(LLVMBasicBlockAsValue(context->current_block)))
	{
		LLVMBasicBlockRef prev_block = LLVMGetPreviousBasicBlock(context->current_block);
		LLVMDeleteBasicBlock(context->current_block);
		context->current_block = prev_block;
		LLVMPositionBuilderAtEnd(context->builder, context->current_block);
	}
	// Insert a return (and defer) if needed.
	if (context->current_block && !LLVMGetBasicBlockTerminator(context->current_block))
	{
		assert(decl->func.body->compound_stmt.defer_list.end == NULL);
		gencontext_emit_defer(context, decl->func.body->compound_stmt.defer_list.start, NULL);
		gencontext_emit_implicit_return(context);
	}

	// erase alloca point
	if (LLVMGetInstructionParent(alloca_point))
	{
		context->alloca_point = NULL;
		LLVMInstructionEraseFromParent(alloca_point);
	}

	LLVMDisposeBuilder(context->builder);

	context->builder = prev_builder;
	context->function = prev_function;
}

void gencontext_emit_function_decl(GenContext *context, Decl *decl)
{
	assert(decl->decl_kind == DECL_FUNC);
	// Resolve function backend type for function.
	decl->func.backend_value = LLVMAddFunction(context->module, decl->external_name,
	                                           llvm_type(decl->type));


	// Specify appropriate storage class, visibility and call convention
	// extern functions (linkedited in separately):
	/*
	if (glofn->flags & FlagSystem) {
		LLVMSetFunctionCallConv(glofn->llvmvar, LLVMX86StdcallCallConv);
		LLVMSetDLLStorageClass(glofn->llvmvar, LLVMDLLImportStorageClass);
	}*/
	if (decl->visibility == VISIBLE_LOCAL)
	{
		LLVMSetVisibility(decl->func.backend_value, LLVMHiddenVisibility);
	}

	if (context->debug.builder)
	{
		LLVMDIFlags flags = LLVMDIFlagZero;
		if (!decl->func.body) flags |= LLVMDIFlagPrototyped;
		switch (decl->visibility)
		{
			case VISIBLE_LOCAL:
			case VISIBLE_EXTERN:
				flags |= LLVMDIFlagPrivate;
				break;
			case VISIBLE_MODULE:
				flags |= LLVMDIFlagProtected;
				break;
			case VISIBLE_PUBLIC:
				flags |= LLVMDIFlagPublic;
				break;
		}
		SourcePosition decl_position = source_file_find_position(decl->name_span.loc);
	/*	context->debug.function = LLVMDIBuilderCreateFunction(context->debug.builder,
		                                                      context->debug.compile_unit,
		                                                      decl->name, source_range_len(decl->name_span),
		                                                      decl->name, source_range_len(decl->name_span),
		                                                      context->debug.file,
		                                                      decl_position.line,
		                                                      decl->type->backend_type,
		                                                      decl->visibility == VISIBLE_LOCAL,
		                                                      1,
		                                                      decl_position.line,
		                                                      flags,
		                                                      build_options.optimization_level != OPTIMIZATION_NONE);
		LLVMSetSubprogram(decl->func.backend_value, context->debug.function);*/
	}
}



void gencontext_emit_extern_decl(GenContext *context, Decl *decl)
{
	switch (decl->decl_kind)
	{
		case DECL_POISONED:
			UNREACHABLE;
		case DECL_FUNC:
			decl->func.backend_value = LLVMAddFunction(context->module, decl->external_name,
			                                           llvm_type(decl->type));
			LLVMSetVisibility(decl->func.backend_value, LLVMDefaultVisibility);
			break;
		case DECL_VAR:
			decl->var.backend_ref = LLVMAddGlobal(context->module, llvm_type(decl->type), decl->external_name);
			LLVMSetVisibility(decl->var.backend_ref, LLVMDefaultVisibility);
			break;
		case DECL_TYPEDEF:
			UNREACHABLE
		case DECL_ENUM_CONSTANT:
			TODO
		case DECL_STRUCT:
		case DECL_UNION:
			llvm_type(decl->type);
			TODO // Fix typeid
			break;
		case DECL_ENUM:
			TODO
		case DECL_ERROR:
			TODO
		case DECL_ERROR_CONSTANT:
			TODO
		case DECL_ARRAY_VALUE:
		case DECL_IMPORT:
		case DECL_MACRO:
		case DECL_GENERIC:
		case DECL_CT_IF:
		case DECL_CT_ELSE:
		case DECL_CT_ELIF:
		case DECL_ATTRIBUTE:
		case DECL_THROWS:
			UNREACHABLE
	}
}

