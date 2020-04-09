// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

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
		init = gencontext_emit_null_constant(context, decl->type);
	}
	else
	{
		init = gencontext_emit_initializer(context, decl->var.init_expr);
	}

	// TODO fix name
	decl->var.backend_ref = LLVMAddGlobal(context->module, llvm_type(decl->type), decl->name);

	// If read only: LLVMSetGlobalConstant(decl->var.backend_ref, 1);

	switch (decl->visibility)
	{
		case VISIBLE_MODULE:
			LLVMSetVisibility(decl->var.backend_ref, LLVMProtectedVisibility);
			break;
		case VISIBLE_PUBLIC:
			LLVMSetVisibility(decl->var.backend_ref, LLVMDefaultVisibility);
			break;
		case VISIBLE_EXTERN:
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
		                                                                          decl->name,
		                                                                          source_range_len(decl->name_span),
		                                                                          "linkagename",
		                                                                          2,
		                                                                          context->debug.file,
		                                                                          12 /* lineno */,
		                                                                          decl->type->backend_debug_type,
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

void gencontext_emit_object_file(GenContext *context)
{
	char *err = "";
	LLVMSetTarget(context->module, build_options.target);
	char *layout = LLVMCopyStringRepOfTargetData(target_data_layout());
	LLVMSetDataLayout(context->module, layout);
	LLVMDisposeMessage(layout);

	// Generate .o or .obj file
	char *filename = strformat("%.*s.o", (int)strlen(context->ast_context->file->name) - 3, context->ast_context->file->name);
	if (LLVMTargetMachineEmitToFile(target_machine(), context->module, filename, LLVMObjectFile, &err) != 0)
	{
		error_exit("Could not emit object file: %s", err);
	}
}

void gencontext_print_llvm_ir(GenContext *context)
{
	char *err = NULL;
	char *filename = strformat("%.*s.ll", (int)strlen(context->ast_context->file->name) - 3, context->ast_context->file->name);
	if (LLVMPrintModuleToFile(context->module, filename, &err) != 0)
	{
		error_exit("Could not emit ir to file: %s", err);
	}
}


LLVMValueRef gencontext_emit_alloca(GenContext *context, LLVMTypeRef type, const char *name)
{
	LLVMBasicBlockRef current_block = LLVMGetInsertBlock(context->builder);
	LLVMPositionBuilderBefore(context->builder, context->alloca_point);
	LLVMValueRef alloca = LLVMBuildAlloca(context->builder, type, name);
	LLVMPositionBuilderAtEnd(context->builder, current_block);
	return alloca;
}

/**
 * Values here taken from LLVM.
 * @return return the inlining threshold given the build options.
 */
static int get_inlining_threshold(void)
{
	if (build_options.optimization_level == OPTIMIZATION_AGGRESSIVE)
	{
		return 250;
	}
	switch (build_options.size_optimization_level)
	{
		case SIZE_OPTIMIZATION_TINY:
			return 5;
		case SIZE_OPTIMIZATION_SMALL:
			return 50;
		default:
			return 250;
	}
}


static inline unsigned lookup_intrinsic(const char *name)
{
	return LLVMLookupIntrinsicID(name, strlen(name));
}

static bool intrinsics_setup = false;
unsigned ssub_overflow_intrinsic_id;
unsigned usub_overflow_intrinsic_id;
unsigned sadd_overflow_intrinsic_id;
unsigned uadd_overflow_intrinsic_id;
unsigned smul_overflow_intrinsic_id;
unsigned umul_overflow_intrinsic_id;
unsigned trap_intrinsic_id;

void llvm_codegen_setup()
{
	assert(intrinsics_setup == false);
	ssub_overflow_intrinsic_id = lookup_intrinsic("llvm.ssub.with.overflow");
	usub_overflow_intrinsic_id = lookup_intrinsic("llvm.usub.with.overflow");
	sadd_overflow_intrinsic_id = lookup_intrinsic("llvm.sadd.with.overflow");
	uadd_overflow_intrinsic_id = lookup_intrinsic("llvm.uadd.with.overflow");
	smul_overflow_intrinsic_id = lookup_intrinsic("llvm.smul.with.overflow");
	umul_overflow_intrinsic_id = lookup_intrinsic("llvm.umul.with.overflow");
	trap_intrinsic_id = lookup_intrinsic("llvm.trap");

	intrinsics_setup = true;
}

void llvm_codegen(Context *context)
{
	assert(intrinsics_setup);
	GenContext gen_context;
	gencontext_init(&gen_context, context);
	gencontext_begin_module(&gen_context);
	// EmitDeferred()
	VECEACH(context->external_symbol_list, i)
	{
		gencontext_emit_extern_decl(&gen_context, context->external_symbol_list[i]);
	}
	VECEACH(context->functions, i)
	{
		gencontext_emit_function_decl(&gen_context, context->functions[i]);
	}


	gencontext_print_llvm_ir(&gen_context);

	// Starting from here we could potentially thread this:
	LLVMPassManagerBuilderRef pass_manager_builder = LLVMPassManagerBuilderCreate();
	LLVMPassManagerBuilderSetOptLevel(pass_manager_builder, build_options.optimization_level);
	LLVMPassManagerBuilderSetSizeLevel(pass_manager_builder, build_options.size_optimization_level);
	LLVMPassManagerBuilderSetDisableUnrollLoops(pass_manager_builder, build_options.optimization_level == OPTIMIZATION_NONE);
	LLVMPassManagerBuilderUseInlinerWithThreshold(pass_manager_builder, get_inlining_threshold());
	LLVMPassManagerRef pass_manager = LLVMCreatePassManager();
	LLVMPassManagerRef function_pass_manager = LLVMCreateFunctionPassManagerForModule(gen_context.module);
	LLVMAddAnalysisPasses(target_machine(), pass_manager);
	LLVMAddAnalysisPasses(target_machine(), function_pass_manager);
	LLVMPassManagerBuilderPopulateModulePassManager(pass_manager_builder, pass_manager);
	LLVMPassManagerBuilderPopulateFunctionPassManager(pass_manager_builder, function_pass_manager);

	// IMPROVE
	// In LLVM Opt, LoopVectorize and SLPVectorize settings are part of the PassManagerBuilder
	// Anything else we need to manually add?

	LLVMPassManagerBuilderDispose(pass_manager_builder);

	// Run function passes
	LLVMInitializeFunctionPassManager(function_pass_manager);
	LLVMValueRef current_function = LLVMGetFirstFunction(gen_context.module);
	while (current_function)
	{
		LLVMRunFunctionPassManager(function_pass_manager, current_function);
		current_function = LLVMGetNextFunction(current_function);
	}
	LLVMFinalizeFunctionPassManager(function_pass_manager);
	LLVMDisposePassManager(function_pass_manager);

	// Run module pass
	LLVMRunPassManager(pass_manager, gen_context.module);
	LLVMDisposePassManager(pass_manager);

	// Serialize the LLVM IR, if requested
	if (build_options.emit_llvm) gencontext_print_llvm_ir(&gen_context);

	if (build_options.emit_bitcode) gencontext_emit_object_file(&gen_context);

	gencontext_end_module(&gen_context);
	gencontext_destroy(&gen_context);
}
