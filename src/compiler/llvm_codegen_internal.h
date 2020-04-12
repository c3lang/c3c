#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"
#include <llvm-c/Core.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Target.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/BitWriter.h>
#include <llvm-c/DebugInfo.h>
#include <llvm-c/Transforms/PassManagerBuilder.h>
#include <llvm-c/Transforms/InstCombine.h>
#include <llvm-c/Transforms/Vectorize.h>
#include <llvm-c/Transforms/Scalar.h>
#include <llvm-c/Transforms/IPO.h>
#include <llvm-c/Transforms/Utils.h>
#include "dwarf.h"

typedef struct
{
	LLVMBasicBlockRef continue_block;
	LLVMBasicBlockRef break_block;
	LLVMBasicBlockRef next_block;
}  BreakContinue;

typedef struct
{
	LLVMDIBuilderRef builder;
	LLVMMetadataRef file;
	LLVMMetadataRef compile_unit;
	LLVMMetadataRef function;
	SourceRange current_range;
	LLVMMetadataRef *lexical_block_stack;
	LLVMMetadataRef inlined_at;
} DebugContext;

#define BREAK_STACK_MAX 256

typedef struct
{
	LLVMModuleRef module;
	LLVMContextRef context;
	LLVMValueRef function;
	LLVMValueRef alloca_point;
	LLVMBuilderRef builder;
	LLVMBasicBlockRef current_block;
	Decl *cur_code_decl;
	Decl *cur_func_decl;
	TypeInfo *current_return_type;
	int block_global_unique_count;
	int ast_alloca_addr_space;
	BreakContinue return_block;
	int simple_return_expressions;
	unsigned pointer_alignment;
	int return_expressions;
	Ast **defer_stack;
	DebugContext debug;
	Context *ast_context;
	BreakContinue break_continue_stack[BREAK_STACK_MAX];
	size_t break_continue_stack_index;
	LLVMValueRef return_out;
	LLVMBasicBlockRef expr_block_exit;
	bool current_block_is_target : 1;
	bool did_call_stack_save : 1;
} GenContext;

extern unsigned sadd_overflow_intrinsic_id;
extern unsigned uadd_overflow_intrinsic_id;
extern unsigned ssub_overflow_intrinsic_id;
extern unsigned usub_overflow_intrinsic_id;
extern unsigned smul_overflow_intrinsic_id;
extern unsigned umul_overflow_intrinsic_id;
extern unsigned trap_intrinsic_id;

void gencontext_begin_module(GenContext *context);
void gencontext_end_module(GenContext *context);
void gencontext_emit_stmt(GenContext *context, Ast *ast);
LLVMValueRef gencontext_emit_call_intrinsic(GenContext *context, unsigned intrinsic_id, LLVMTypeRef *types,
                                            LLVMValueRef *values, unsigned arg_count);
void gencontext_emit_panic_on_true(GenContext *context, LLVMValueRef value, const char *panic_name);
void gencontext_emit_defer(GenContext *context, Ast *defer_start, Ast *defer_end);
LLVMValueRef gencontext_emit_expr(GenContext *context, Expr *expr);
LLVMValueRef gencontext_emit_ast_expr(GenContext *context, Ast *expr);
LLVMMetadataRef gencontext_get_debug_type(GenContext *context, Type *type);
void gencontext_emit_debug_location(GenContext *context, SourceRange location);
LLVMMetadataRef gencontext_create_builtin_debug_type(GenContext *context, Type *builtin_type);
LLVMValueRef gencontext_emit_alloca(GenContext *context, LLVMTypeRef type, const char *name);
void gencontext_emit_compound_stmt(GenContext *context, Ast *ast);
void gencontext_emit_block(GenContext *context, LLVMBasicBlockRef next_block);
void gencontext_emit_br(GenContext *context, LLVMBasicBlockRef next_block);
bool gencontext_check_block_branch_emit(GenContext *context);
void gencontext_emit_cond_br(GenContext *context, LLVMValueRef value, LLVMBasicBlockRef thenBlock, LLVMBasicBlockRef elseBlock);
static inline LLVMBasicBlockRef gencontext_create_free_block(GenContext *context, const char *name)
{
	return LLVMCreateBasicBlockInContext(context->context, name);
}
void gencontext_emit_function_body(GenContext *context, Decl *decl);
void gencontext_emit_implicit_return(GenContext *context);
void gencontext_emit_function_decl(GenContext *context, Decl *decl);
void gencontext_emit_extern_decl(GenContext *context, Decl *decl);
LLVMValueRef gencontext_emit_address(GenContext *context, Expr *expr);
#define LLVMTYPE(type) type->backend_type
static inline LLVMValueRef gencontext_load_expr(GenContext *context, LLVMValueRef value)
{
	return LLVMBuildLoad(context->builder, value, "");
}
LLVMTypeRef gencontext_get_llvm_type(GenContext *context, Type *type);
LLVMValueRef gencontext_emit_cast(GenContext *context, CastKind cast_kind, LLVMValueRef value, Type *type, Type *target_type);
static inline bool gencontext_use_debug(GenContext *context)
{
	return context && context->debug.builder != NULL;
}

