// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.


#include "llvm_codegen_internal.h"
#include "bigint.h"

bool gencontext_check_block_branch_emit(GenContext *context)
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
	decl->ref = gencontext_emit_alloca(context, llvm_type(decl->type), decl->name);
	gencontext_emit_store(context, decl, LLVMGetParam(context->function, index));
}

void gencontext_emit_implicit_return(GenContext *context)
{
	if (context->cur_func_decl->func.function_signature.failable)
	{
		LLVMBuildRet(context->builder, gencontext_emit_no_error_union(context));
	}
	else
	{
		if (context->cur_func_decl->func.function_signature.rtype->type != type_void && !context->cur_func_decl->func.function_signature.return_param)
		{
			LLVMBuildUnreachable(context->builder);
			return;
		}
		LLVMBuildRetVoid(context->builder);
	}
}

void gencontext_emit_function_body(GenContext *context, Decl *decl)
{
	DEBUG_LOG("Generating function %s.", decl->external_name);
	assert(decl->ref);

	LLVMValueRef prev_function = context->function;
	LLVMBuilderRef prev_builder = context->builder;

	context->error_var = NULL;
	context->catch_block = NULL;

	context->function = decl->ref;
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
		assert(!decl->func.body->compound_stmt.defer_list.end);
		gencontext_emit_defer(context, decl->func.body->compound_stmt.defer_list.start, 0);
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
	LLVMValueRef function = LLVMAddFunction(context->module, decl->cname ?: decl->external_name, llvm_type(decl->type));
	decl->ref = function;
	if (decl->func.function_signature.return_param)
	{
		if (!decl->func.function_signature.failable)
		{
			gencontext_add_attribute(context, function, sret_attribute, 1);
		}
		gencontext_add_attribute(context, function, noalias_attribute, 1);
	}
	if (decl->func.attr_inline)
	{
		gencontext_add_attribute(context, function, alwaysinline_attribute, -1);
	}
	if (decl->func.attr_noinline)
	{
		gencontext_add_attribute(context, function, noinline_attribute, -1);
	}
	if (decl->func.attr_noreturn)
	{
		gencontext_add_attribute(context, function, noreturn_attribute, -1);
	}
	if (decl->alignment)
	{
		LLVMSetAlignment(function, decl->alignment);
	}
	if (decl->section)
	{
		LLVMSetSection(function, decl->section);
	}
	gencontext_add_attribute(context, function, nounwind_attribute, -1);

	if (decl->func.attr_stdcall && (build_target.os == OS_TYPE_WIN32))
	{
		LLVMSetFunctionCallConv(function, LLVMX86StdcallCallConv);
		LLVMSetDLLStorageClass(function, LLVMDLLImportStorageClass);
	}

	switch (decl->visibility)
	{
		case VISIBLE_EXTERN:
			LLVMSetLinkage(function, decl->func.attr_weak ? LLVMExternalWeakLinkage : LLVMExternalLinkage);
			LLVMSetVisibility(function, LLVMDefaultVisibility);
			break;
		case VISIBLE_PUBLIC:
		case VISIBLE_MODULE:
			if (decl->func.attr_weak) LLVMSetLinkage(function, LLVMWeakAnyLinkage);
			LLVMSetVisibility(function, LLVMDefaultVisibility);
			break;
		case VISIBLE_LOCAL:
			LLVMSetLinkage(function, decl->func.attr_weak ? LLVMLinkerPrivateWeakLinkage : LLVMInternalLinkage);
			LLVMSetVisibility(function, LLVMDefaultVisibility);
			break;;
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
			decl->ref = LLVMAddFunction(context->module, decl->cname ?: decl->external_name,
			                                           llvm_type(decl->type));
			LLVMSetVisibility(decl->ref, LLVMDefaultVisibility);
			break;
		case DECL_VAR:
			decl->ref = LLVMAddGlobal(context->module, llvm_type(decl->type), decl->cname ?: decl->external_name);
			LLVMSetVisibility(decl->ref, LLVMDefaultVisibility);
			break;
		case DECL_TYPEDEF:
			UNREACHABLE
		case DECL_ENUM_CONSTANT:
			TODO
		case DECL_STRUCT:
		case DECL_UNION:
		case DECL_ERR:
			llvm_type(decl->type);
			TODO // Fix typeid
			break;
		case DECL_ENUM:
			TODO
		case DECL_MEMBER:
		case DECL_ARRAY_VALUE:
		case DECL_IMPORT:
		case DECL_MACRO:
		case DECL_GENERIC:
		case DECL_CT_IF:
		case DECL_CT_ELSE:
		case DECL_CT_ELIF:
		case DECL_ATTRIBUTE:
		case DECL_LABEL:
			UNREACHABLE
	}
}

