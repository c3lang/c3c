#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "compiler_internal.h"
#include <llvm-c/Core.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Target.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/BitWriter.h>
#include <llvm-c/Transforms/Scalar.h>
#include <llvm-c/Transforms/IPO.h>
#include <llvm-c/Transforms/Utils.h>
#include <llvm-c/DebugInfo.h>
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
	const char *filename;
	const char *dirname;
	LLVMModuleRef module;
	LLVMContextRef context;
	LLVMValueRef function;
	LLVMValueRef alloca_point;
	LLVMBuilderRef builder;
	LLVMBasicBlockRef current_block;
	bool current_block_is_target;
	Decl *cur_code_decl;
	Decl *cur_func_decl;
	bool did_call_stack_save;
	Type *current_return_type;
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

} GenContext;


void gencontext_begin_module(GenContext *context);
void gencontext_end_module(GenContext *context);
void gencontext_emit_stmt(GenContext *context, Ast *ast);
LLVMValueRef gencontext_emit_expr(GenContext *context, Expr *expr);
void gencontext_emit_debug_location(GenContext *context, SourceRange location);
LLVMMetadataRef gencontext_create_builtin_debug_type(GenContext *context, Type *builtin_type);
LLVMValueRef gencontext_emit_alloca(GenContext *context, Decl *decl);
void gencontext_emit_compound_stmt(GenContext *context, Ast *ast);
void gencontext_emit_block(GenContext *context, LLVMBasicBlockRef next_block);
void gencontext_emit_br(GenContext *context, LLVMBasicBlockRef next_block);
void gencontext_emit_cond_br(GenContext *context, LLVMValueRef value, LLVMBasicBlockRef thenBlock, LLVMBasicBlockRef elseBlock);
static inline LLVMBasicBlockRef gencontext_create_free_block(GenContext *context, const char *name)
{
	return LLVMCreateBasicBlockInContext(context->context, name);
}

void gencontext_emit_function_decl(GenContext *context, Decl *decl);
LLVMValueRef gencontext_emit_address(GenContext *context, Expr *expr);
#define LLVMTYPE(type) gencontext_get_llvm_type(context, type->canonical)

LLVMTypeRef gencontext_get_llvm_type(GenContext *context, Type *type);

static inline bool gencontext_use_debug(GenContext *context)
{
	return context->debug.builder != NULL;
}

