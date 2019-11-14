// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "llvm_codegen_internal.h"

static void gencontext_pop_break_continue(GenContext *context);
static void
gencontext_push_break_continue(GenContext *context, LLVMBasicBlockRef break_block, LLVMBasicBlockRef continue_block,
                               LLVMBasicBlockRef next_block);

void gencontext_emit_compound_stmt(GenContext *context, Ast *ast)
{
	assert(ast->ast_kind == AST_COMPOUND_STMT);
	VECEACH(ast->compound_stmt.stmts, i)
	{
		gencontext_emit_stmt(context, ast->compound_stmt.stmts[i]);
	}
}

static inline void gencontext_emit_stmt_list(GenContext *context, Ast *ast)
{
	assert(ast->ast_kind == AST_STMT_LIST);
	VECEACH(ast->stmt_list, i)
	{
		gencontext_emit_stmt(context, ast->stmt_list[i]);
	}
}

static inline void gencontext_emit_return(GenContext *context, Ast *ast)
{
	if (!ast->return_stmt.expr)
	{
		LLVMBuildRetVoid(context->builder);
		return;
	}
	LLVMValueRef returnValue = gencontext_emit_expr(context, ast->return_stmt.expr);
	LLVMBuildRet(context->builder, returnValue);
}

static inline LLVMValueRef gencontext_emit_cond(GenContext *context, Ast *ast)
{
	assert(ast->ast_kind == AST_COND_STMT);
	VECEACH(ast->cond_stmt.stmts, i)
	{
		gencontext_emit_stmt(context, ast->cond_stmt.stmts[i]);
	}
	return gencontext_emit_expr(context, ast->cond_stmt.expr);
}

static LLVMValueRef gencontext_emit_decl(GenContext *context, Ast *ast)
{
	Decl *decl = ast->declare_stmt;

	decl->var.backend_ref = gencontext_emit_alloca(context, decl);
	// TODO NRVO
	// TODO debug info
	/*
	if (EmitDebugInfo && HaveInsertPoint()) {
		Address DebugAddr = address;
		bool UsePointerValue = NRVO && ReturnValuePointer.isValid();
		DI->setLocation(D.getLocation());

		// If NRVO, use a pointer to the return address.
		if (UsePointerValue)
			DebugAddr = ReturnValuePointer;

		(void)DI->EmitDeclareOfAutoVariable(&D, DebugAddr.getPointer(), Builder,
		                                    UsePointerValue);
	}
	*/
	if (decl->var.init_expr)
	{
		LLVMValueRef value = gencontext_emit_expr(context, decl->var.init_expr);
		LLVMBuildStore(context->builder, value, decl->var.backend_ref);
		return value;
	}
	return decl->var.backend_ref;
}


void gencontext_emit_if(GenContext *context, Ast *ast)
{
	// In the case of something like if (foo = 1, int bar = 2; b > 0) { ... }
	// Let's first emit foo = 1, int bar = 2
	// IMPROVE Consider whether these should be lowered or not.
	Ast **stmts = ast->if_stmt.cond->cond_stmt.stmts;
	VECEACH(stmts, i)
	{
		gencontext_emit_stmt(context, stmts[i]);
	}

	// We need at least the exit block and the "then" block.
	LLVMBasicBlockRef exit_block = LLVMCreateBasicBlockInContext(context->context, "if.exit");
	LLVMBasicBlockRef then_block = LLVMCreateBasicBlockInContext(context->context, "if.then");
	LLVMBasicBlockRef else_block = NULL;

	// We have an optional else block.
	if (ast->if_stmt.else_body)
	{
		else_block = LLVMCreateBasicBlockInContext(context->context, "if.else");
	}

	assert(ast->if_stmt.cond->cond_stmt.expr->type->type_kind == TYPE_BOOL);

	// Output boolean value and switch.
	LLVMValueRef value = gencontext_emit_expr(context, ast->if_stmt.cond->cond_stmt.expr);
	gencontext_emit_cond_br(context, value, then_block, else_block ? else_block : exit_block);

	// Emit the 'then' code.
	gencontext_emit_block(context, then_block);
	gencontext_emit_stmt(context, ast->if_stmt.then_body);

	// Jump to exit.
	gencontext_emit_br(context, exit_block);

	// Emit the 'else' branch if present.
	if (else_block)
	{
		gencontext_emit_block(context, else_block);
		gencontext_emit_stmt(context, ast->if_stmt.else_body);
		gencontext_emit_br(context, exit_block);
	}

	// And now we just emit the exit block.
	gencontext_emit_block(context, exit_block);
}


static void gencontext_push_next(GenContext *context, LLVMBasicBlockRef nextBlock)
{
	// TODO
}
static void
gencontext_push_break_continue(GenContext *context, LLVMBasicBlockRef break_block, LLVMBasicBlockRef continue_block,
                               LLVMBasicBlockRef next_block)
{
	size_t index = context->break_continue_stack_index++;
	if (index == BREAK_STACK_MAX - 1)
	{
		error_exit("Exhausted break/continue stack - exceeded %d entries.", BREAK_STACK_MAX);
	}
	if (!index)
	{
		context->break_continue_stack[index].continue_block = continue_block;
		context->break_continue_stack[index].break_block = break_block;
		context->break_continue_stack[index].next_block = next_block;
		return;
	}

	context->break_continue_stack[index].continue_block = continue_block ? continue_block : context->break_continue_stack[index - 1].continue_block;
	context->break_continue_stack[index].next_block = next_block ? next_block : context->break_continue_stack[index - 1].next_block;
	context->break_continue_stack[index].break_block = break_block ? break_block : context->break_continue_stack[index - 1].break_block;
}

static void gencontext_pop_break_continue(GenContext *context)
{
	assert(context->break_continue_stack_index);
	context->break_continue_stack_index--;
}

void gencontext_emit_for_stmt(GenContext *context, Ast *ast)
{
	// First, emit all inits.

	Ast **init_stmts = ast->for_stmt.init;

	VECEACH(init_stmts, i)
	{
		gencontext_emit_stmt(context, init_stmts[i]);
	}

	// We have 3 optional parts, which makes this code bit complicated.
	LLVMBasicBlockRef exit_block = gencontext_create_free_block(context, "for.exit");
	LLVMBasicBlockRef inc_block = ast->for_stmt.incr ? gencontext_create_free_block(context, "for.inc") : NULL;
	LLVMBasicBlockRef body_block = ast->for_stmt.body->compound_stmt.stmts ? gencontext_create_free_block(context, "for.body") : NULL;
	LLVMBasicBlockRef cond_block = ast->for_stmt.cond ? gencontext_create_free_block(context, "for.cond") : NULL;

	// A loop must either have a body or an inc.
	// This type of for loop is forbidden:
	// for (;;);
	assert(cond_block || inc_block || body_block && "For has no body, no inc and no cond.");

	// Break is simple it always jumps out.
	// For continue:
	// 1. If there is inc, jump to the condition
	// 2. If there is no condition, jump to the body.
	gencontext_push_break_continue(context,
	                               exit_block,
	                               inc_block ? inc_block : (cond_block ? cond_block : body_block),
	                               NULL);

	LLVMValueRef value = NULL;

	if (cond_block)
	{
		// Emit cond
		gencontext_emit_br(context, cond_block);
		gencontext_emit_block(context, cond_block);
		value = gencontext_emit_expr(context, ast->for_stmt.cond->cond_stmt.expr);
		// If we have a body, conditionally jump to it.
		if (body_block)
		{
			gencontext_emit_cond_br(context, value, body_block, exit_block);
		}
		else
		{
			// Otherwise jump to inc or cond depending on what's available.
			gencontext_emit_cond_br(context, value, inc_block ? inc_block : cond_block, exit_block);
		}
	}

	if (body_block)
	{
		if (!cond_block)
		{
			// We don't have a cond, so we need to unconditionally jump here.
			gencontext_emit_br(context, body_block);
		}
		gencontext_emit_block(context, body_block);
		gencontext_emit_stmt(context, ast->for_stmt.body);
		// IMPROVE handle continue/break.
		if (inc_block)
		{
			gencontext_emit_br(context, inc_block);
		}
	}

	if (inc_block)
	{
		// Emit the block
		gencontext_emit_block(context, inc_block);
		gencontext_emit_stmt(context, ast->for_stmt.incr);
	}

	// Loop back.
	gencontext_emit_br(context, cond_block ? cond_block : (body_block ? body_block : inc_block));

	// And insert exit block
	gencontext_emit_block(context, exit_block);
	gencontext_pop_break_continue(context);
}

void gencontext_emit_do_stmt(GenContext *context, Ast *ast)
{
	LLVMBasicBlockRef exit_block = gencontext_create_free_block(context, "do.exit");
	LLVMBasicBlockRef cond_block = ast->do_stmt.expr ? gencontext_create_free_block(context, "do.cond") : NULL;
	LLVMBasicBlockRef body_block = ast->do_stmt.body ? gencontext_create_free_block(context, "do.body") : NULL;

	// A loop must either have a body or an inc.
	// This type do-while for loop is forbidden:
	// do { } while (1);
	assert(cond_block || body_block && "Do has no body and no cond.");

	// Break is simple it always jumps out.
	// For continue: if there is no condition, jump to the body.
	gencontext_push_break_continue(context, exit_block, cond_block ? cond_block : body_block, NULL);

	if (body_block)
	{
		// Emit the body
		gencontext_emit_br(context, body_block);
		gencontext_emit_block(context, body_block);
		gencontext_emit_stmt(context, ast->do_stmt.body);
	}

	if (cond_block)
	{
		gencontext_emit_br(context, cond_block);
		gencontext_emit_block(context, cond_block);
		LLVMValueRef value = gencontext_emit_expr(context, ast->do_stmt.expr);
		gencontext_emit_cond_br(context, value, body_block ? body_block : cond_block, exit_block);
	}
	else
	{
		// Branch to the beginning of the block
		gencontext_emit_br(context, body_block);
	}

	// Emit the exit block.
	gencontext_emit_block(context, exit_block);

	gencontext_pop_break_continue(context);
}

void gencontext_emit_jmp(GenContext *context, LLVMBasicBlockRef block)
{
	gencontext_emit_br(context, block);
	LLVMBasicBlockRef post_jump_block = gencontext_create_free_block(context, "jmp");
	gencontext_emit_block(context, post_jump_block);
}

void gencontext_emit_label(GenContext *context, Ast *ast)
{
	gencontext_emit_br(context, ast->label_stmt.backend_value);
	gencontext_emit_block(context, ast->label_stmt.backend_value);
	context->current_block_is_target = true;
}

void gencontext_emit_switch(GenContext *context, Ast *ast)
{
	LLVMValueRef switch_value = gencontext_emit_cond(context, ast->switch_stmt.cond);

	size_t cases = vec_size(ast->switch_stmt.cases);
	if (!cases)
	{
		// No body or default is empty, just exit after the value.
		return;
	}

	Ast *default_case = NULL;
	VECEACH(ast->switch_stmt.cases, i)
	{
		Ast *case_stmt = ast->switch_stmt.cases[i];
		if (case_stmt->case_stmt.value_type == CASE_VALUE_DEFAULT)
		{
			if (case_stmt->case_stmt.block)
			{
				case_stmt->case_stmt.backend_value = gencontext_create_free_block(context, "switch.default");
			}
			default_case = case_stmt;
		}
		else if (case_stmt->case_stmt.block)
		{
			case_stmt->case_stmt.backend_value = gencontext_create_free_block(context, "switch.case");
		}
	}

	LLVMBasicBlockRef exit_block = gencontext_create_free_block(context, "switch.exit");

	// We will now treat the fallthrough cases:
	// switch (i)
	// {
	//    case 1:
	//    case 2:
	//      do_something();
	//    default:
	// }
	VECEACH(ast->switch_stmt.cases, i)
	{
		Ast *case_stmt = ast->switch_stmt.cases[i];
		if (case_stmt->case_stmt.backend_value != NULL) continue;

		// Look forward for a block
		for (size_t j = i + 1; j < cases; j++)
		{
			Ast *other_case = ast->switch_stmt.cases[j];
			if (other_case->case_stmt.backend_value != NULL)
			{
				case_stmt->case_stmt.backend_value = other_case->case_stmt.backend_value;
				break;
			}
		}
		// No block found? Then the block is the exit block.
		if (!case_stmt->case_stmt.backend_value)
		{
			case_stmt->case_stmt.backend_value = exit_block;
		}
	}

	gencontext_push_break_continue(context, exit_block, NULL, NULL);

	LLVMValueRef switch_stmt = LLVMBuildSwitch(context->builder, switch_value, default_case ? default_case->case_stmt.backend_value : exit_block, cases);
	context->current_block = NULL;
	VECEACH(ast->switch_stmt.cases, i)
	{
		Ast *case_stmt = ast->switch_stmt.cases[i];
		LLVMBasicBlockRef block = case_stmt->case_stmt.backend_value;
		if (case_stmt != default_case)
		{
			LLVMValueRef case_value = LLVMConstInt(LLVMTypeOf(switch_value), case_stmt->case_stmt.val, case_stmt->case_stmt.value_type == CASE_VALUE_INT);
			LLVMAddCase(switch_stmt, case_value, block);
		}

		// Skip fallthroughs.
		if (!case_stmt->case_stmt.block) continue;

		gencontext_emit_block(context, block);
		// IMPORTANT!
		context->current_block_is_target = true;
		gencontext_push_break_continue(context, NULL, NULL, i < cases - 1 ? ast->switch_stmt.cases[i + 1]->case_stmt.backend_value : exit_block);
		gencontext_emit_stmt(context, case_stmt->case_stmt.block);
		gencontext_pop_break_continue(context);
		gencontext_emit_br(context, exit_block);
	}
	gencontext_pop_break_continue(context);
	gencontext_emit_block(context, exit_block);
}


void gencontext_emit_stmt(GenContext *context, Ast *ast)
{
	switch (ast->ast_kind)
	{
		case AST_POISONED:
			UNREACHABLE
		case AST_EXPR_STMT:
			gencontext_emit_expr(context, ast->expr_stmt);
			break;
		case AST_DECLARE_STMT:
			gencontext_emit_decl(context, ast);
			break;
		case AST_BREAK_STMT:
			assert(context->break_continue_stack_index);
			gencontext_emit_jmp(context, context->break_continue_stack[context->break_continue_stack_index - 1].break_block);
			break;
		case AST_CONTINUE_STMT:
			assert(context->break_continue_stack_index);
			gencontext_emit_jmp(context, context->break_continue_stack[context->break_continue_stack_index - 1].continue_block);
			break;
		case AST_IF_STMT:
			gencontext_emit_if(context, ast);
			break;
		case AST_RETURN_STMT:
			gencontext_emit_return(context, ast);
			break;
		case AST_COMPOUND_STMT:
			gencontext_emit_compound_stmt(context, ast);
			break;
		case AST_FOR_STMT:
			gencontext_emit_for_stmt(context, ast);
			break;
		case AST_DO_STMT:
			gencontext_emit_do_stmt(context, ast);
			break;
		case AST_NEXT_STMT:
			assert(context->break_continue_stack_index);
			gencontext_emit_jmp(context, context->break_continue_stack[context->break_continue_stack_index - 1].next_block);
			break;
		case AST_NOP_STMT:
			break;
		case AST_WHILE_STMT:
		case AST_CATCH_STMT:
		case AST_DEFER_STMT:
		case AST_TRY_STMT:
		case AST_THROW_STMT:
			// Should have been lowered.
			UNREACHABLE
		case AST_ASM_STMT:
			TODO
		case AST_ATTRIBUTE:
		case AST_COND_STMT:
		case AST_CT_IF_STMT:
		case AST_CT_ELIF_STMT:
		case AST_CT_ELSE_STMT:
		case AST_CT_FOR_STMT:
		case AST_CT_SWITCH_STMT:
		case AST_CT_DEFAULT_STMT:
		case AST_CT_CASE_STMT:
		case AST_GENERIC_CASE_STMT:
		case AST_GENERIC_DEFAULT_STMT:
			UNREACHABLE
		case AST_LABEL:
			gencontext_emit_label(context, ast);
			break;
		case AST_GOTO_STMT:
			gencontext_emit_jmp(context, ast->goto_stmt.label->label_stmt.backend_value);
			break;
		case AST_STMT_LIST:
			gencontext_emit_stmt_list(context, ast);
			break;
		case AST_SWITCH_STMT:
			gencontext_emit_switch(context, ast);
			break;
		case AST_CASE_STMT:
		case AST_DEFAULT_STMT:
			TODO
		case AST_VOLATILE_STMT:
			TODO
	}
}
