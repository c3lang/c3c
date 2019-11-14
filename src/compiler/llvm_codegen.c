// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "llvm_codegen_internal.h"



static void gencontext_init(GenContext *context, Context *ast_context)
{
	memset(context, 0, sizeof(GenContext));
	context->context = LLVMContextCreate();
	context->ast_context = ast_context;
}

static void gencontext_destroy(GenContext *context)
{
	LLVMContextDispose(context->context);
}


static LLVMValueRef gencontext_emit_null_constant(GenContext *context, Type *type)
{
	TODO
}

static LLVMValueRef gencontext_emit_initializer(GenContext *context, Expr *expr)
{
	TODO
}
static void gencontext_emit_global_variable_definition(GenContext *context, Decl *decl, bool is_tentative)
{
	assert(decl->var.kind == VARDECL_GLOBAL);

	LLVMValueRef init = NULL;

	if (!decl->var.init_expr)
	{
		// Tentative definition, initialized to zero, but only
		// emitted at the end of the translation unit.
		init = gencontext_emit_null_constant(context, decl->var.type);
	}
	else
	{
		init = gencontext_emit_initializer(context, decl->var.init_expr);
	}

	// TODO fix name
	decl->var.backend_ref = LLVMAddGlobal(context->module, gencontext_get_llvm_type(context, decl->var.type), decl->name.string);

	// If read only: LLVMSetGlobalConstant(decl->var.backend_ref, 1);

	switch (decl->visibility)
	{
		case VISIBLE_MODULE:
			LLVMSetVisibility(decl->var.backend_ref, LLVMProtectedVisibility);
			break;
		case VISIBLE_PUBLIC:
			LLVMSetVisibility(decl->var.backend_ref, LLVMDefaultVisibility);
			break;
		case VISIBLE_LOCAL:
			LLVMSetVisibility(decl->var.backend_ref, LLVMHiddenVisibility);
			break;
	}

	int alignment = 64; // TODO
	// Should we set linkage here?
	if (context->debug.builder)
	{
		decl->var.backend_debug_ref = LLVMDIBuilderCreateGlobalVariableExpression(context->debug.builder,
		                                                                          NULL /*scope*/,
		                                                                          decl->name.string,
		                                                                          decl->name.span.length,
		                                                                          "linkagename",
		                                                                          2,
		                                                                          context->debug.file,
		                                                                          12 /* lineno */,
		                                                                          decl->var.type->backend_debug_type,
		                                                                          decl->visibility ==
		                                                                          VISIBLE_LOCAL, /* expr */
		                                                                          NULL, /** declaration **/
		                                                                          NULL,
		                                                                          alignment);
	}
}
static void gencontext_verify_ir(GenContext *context)
{
	char *error = NULL;
	assert(context->module);
	LLVMVerifyModule(context->module, LLVMPrintMessageAction, &error);
	if (error)
	{
		if (*error)
		{
			error_exit("Could not verify IR: %s", error);
		}
		error_exit("Could not verify module IR.");
	}
}
void gencontext_print_llvm_ir(GenContext *context)
{
	char *err = NULL;
	char *filename = strformat("xx.llvmir" /*, context->module_name*/);
	if (LLVMPrintModuleToFile(context->module, filename, &err) != 0)
	{
		error_exit("Could not emit ir to file: %s", err);
	}
}



LLVMValueRef gencontext_emit_alloca(GenContext *context, Decl *decl)
{
	LLVMBasicBlockRef current_block = LLVMGetInsertBlock(context->builder);
	LLVMPositionBuilderBefore(context->builder, context->alloca_point);
	LLVMValueRef alloca = LLVMBuildAlloca(context->builder, gencontext_get_llvm_type(context, decl->var.type->canonical), decl->name.string);
	LLVMPositionBuilderAtEnd(context->builder, current_block);
	return alloca;
}

void llvm_codegen(Context *context)
{
	GenContext gen_context;
	gencontext_init(&gen_context, context);
	gencontext_begin_module(&gen_context);
	// EmitDeferred()
	VECEACH(context->functions, i)
	{
		gencontext_emit_function_decl(&gen_context, context->functions[i]);
	}
	LLVMDumpModule(gen_context.module);
	gencontext_print_llvm_ir(&gen_context);
	LLVMDumpModule(gen_context.module);
	gencontext_end_module(&gen_context);
	gencontext_destroy(&gen_context);
}
