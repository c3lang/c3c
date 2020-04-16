// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"

static void gencontext_pop_break_continue(GenContext *context);
static void gencontext_push_break_continue(GenContext *context, LLVMBasicBlockRef break_block, LLVMBasicBlockRef continue_block,
                               LLVMBasicBlockRef next_block);

void gencontext_emit_compound_stmt(GenContext *context, Ast *ast)
{
	assert(ast->ast_kind == AST_COMPOUND_STMT);
	VECEACH(ast->compound_stmt.stmts, i)
	{
		gencontext_emit_stmt(context, ast->compound_stmt.stmts[i]);
	}
	gencontext_emit_defer(context, ast->compound_stmt.defer_list.start, ast->compound_stmt.defer_list.end);
}


static LLVMValueRef gencontext_emit_decl(GenContext *context, Ast *ast)
{
	Decl *decl = ast->declare_stmt;

	decl->var.backend_ref = gencontext_emit_alloca(context, llvm_type(type_reduced(decl->type)), decl->name);
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
		gencontext_emit_assign_expr(context, decl->var.backend_ref, decl->var.init_expr);
		return decl->var.backend_ref;
	}
	return decl->var.backend_ref;
}

void gencontext_emit_decl_expr_list_ignore_result(GenContext *context, Ast *ast)
{
	assert(ast->ast_kind == AST_DECL_EXPR_LIST);
	VECEACH(ast->decl_expr_stmt, i)
	{
		gencontext_emit_stmt(context, ast->decl_expr_stmt[i]);
	}
}

LLVMValueRef gencontext_emit_decl_expr_list(GenContext *context, Ast *ast, bool bool_cast)
{
	assert(ast->ast_kind == AST_DECL_EXPR_LIST);
	size_t size = vec_size(ast->decl_expr_stmt);
	size_t last_index = size - 1;
	for (size_t i = 0; i < last_index; i++)
	{
		gencontext_emit_stmt(context, ast->decl_expr_stmt[i]);
	}
	Ast *last = ast->decl_expr_stmt[last_index];
	LLVMValueRef result;
	Type *type;
	switch (last->ast_kind)
	{
		case AST_EXPR_STMT:
			type = last->expr_stmt->type;
			result = gencontext_emit_expr(context, last->expr_stmt);
			break;
		case AST_DECLARE_STMT:
			type = last->declare_stmt->var.type_info->type;
			result = gencontext_load_expr(context, gencontext_emit_decl(context, last));
			break;
		default:
			UNREACHABLE
	}
	if (bool_cast)
	{
		type = type->canonical;
		if (type->type_kind != TYPE_BOOL)
		{
			CastKind cast = cast_to_bool_kind(type);
			result = gencontext_emit_cast(context, cast, result, type, type_bool);
		}
	}
	return result;
}

void gencontext_emit_jmp(GenContext *context, LLVMBasicBlockRef block)
{
	gencontext_emit_br(context, block);
	LLVMBasicBlockRef post_jump_block = gencontext_create_free_block(context, "jmp");
	gencontext_emit_block(context, post_jump_block);
}

static inline void gencontext_emit_return(GenContext *context, Ast *ast)
{
	// Ensure we are on a branch that is non empty.
	if (!gencontext_check_block_branch_emit(context)) return;

	LLVMValueRef ret_value = ast->return_stmt.expr ? gencontext_emit_expr(context, ast->return_stmt.expr) : NULL;
	gencontext_emit_defer(context, ast->return_stmt.defer, NULL);

	// Are we in an expression block?
	if (context->expr_block_exit)
	{
		if (context->return_out)
		{
			LLVMBuildStore(context->builder, ret_value, context->return_out);
		}
		gencontext_emit_jmp(context, context->expr_block_exit);
		return;
	}

	if (!ret_value)
	{
		gencontext_emit_implicit_return(context);
	}
	else
	{
		if (context->return_out)
		{
			LLVMBuildStore(context->builder, ret_value, context->return_out);
			gencontext_emit_implicit_return(context);
		}
		else
		{
			LLVMBuildRet(context->builder, ret_value);
		}
	}
	context->current_block = NULL;
	LLVMBasicBlockRef post_ret_block = gencontext_create_free_block(context, "ret");
	gencontext_emit_block(context, post_ret_block);
}

static inline void gencontext_emit_throw(GenContext *context, Ast *ast)
{
	// Ensure we are on a branch that is non empty.
	if (!gencontext_check_block_branch_emit(context)) return;

	gencontext_emit_defer(context, ast->throw_stmt.defers.start, ast->throw_stmt.defers.end);
	// TODO handle throw if simply a jump
	LLVMBuildRet(context->builder, LLVMConstInt(llvm_type(type_ulong), 10 + ast->throw_stmt.throw_value->identifier_expr.decl->error_constant.value, false));

	context->current_block = NULL;
	LLVMBasicBlockRef post_ret_block = gencontext_create_free_block(context, "ret");
	gencontext_emit_block(context, post_ret_block);
}



void gencontext_emit_if(GenContext *context, Ast *ast)
{
	if (ast->if_stmt.decl) gencontext_emit_decl_expr_list_ignore_result(context, ast->if_stmt.decl);

	// We need at least the exit block and the "then" block.
	LLVMBasicBlockRef exit_block = LLVMCreateBasicBlockInContext(context->context, "if.exit");
	LLVMBasicBlockRef then_block = LLVMCreateBasicBlockInContext(context->context, "if.then");
	LLVMBasicBlockRef else_block = NULL;

	// We have an optional else block.
	if (ast->if_stmt.else_body)
	{
		else_block = LLVMCreateBasicBlockInContext(context->context, "if.else");
	}

	// Output boolean value and switch.
	LLVMValueRef value = gencontext_emit_decl_expr_list(context, ast->if_stmt.cond, true);
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


void gencontext_push_catch(GenContext *context, Decl *error_type, LLVMBasicBlockRef catch_block)
{
	size_t index = context->catch_stack_index++;
	if (index == CATCH_STACK_MAX - 1)
	{
		error_exit("Exhausted catch stack - exceeded %d entries.", CATCH_STACK_MAX);
	}
	context->catch_stack[index].decl = error_type;
	context->catch_stack[index].catch_block = catch_block;
}

void gencontext_pop_catch(GenContext *context)
{
	assert(context->catch_stack_index > 0);
	context->catch_stack_index--;
}

static void gencontext_push_break_continue(GenContext *context, LLVMBasicBlockRef break_block,
		LLVMBasicBlockRef continue_block, LLVMBasicBlockRef next_block)
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

	if (ast->for_stmt.init) gencontext_emit_decl_expr_list_ignore_result(context, ast->for_stmt.init);

	// We have 3 optional parts, which makes this code bit complicated.
	LLVMBasicBlockRef exit_block = gencontext_create_free_block(context, "for.exit");
	LLVMBasicBlockRef inc_block = ast->for_stmt.incr ? gencontext_create_free_block(context, "for.inc") : NULL;
	LLVMBasicBlockRef body_block = ast->for_stmt.body->compound_stmt.stmts ? gencontext_create_free_block(context, "for.body") : NULL;
	LLVMBasicBlockRef cond_block = ast->for_stmt.cond ? gencontext_create_free_block(context, "for.cond") : NULL;

	// A loop must either have a body or an inc.
	// This type of for loop is forbidden:
	// for (;;);
	assert((cond_block || inc_block || body_block) && "For has no body, no inc and no cond.");

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
		value = gencontext_emit_expr(context, ast->for_stmt.cond);
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
		gencontext_emit_expr(context, ast->for_stmt.incr);
	}

	// Loop back.
	gencontext_emit_br(context, cond_block ? cond_block : (body_block ? body_block : inc_block));

	// And insert exit block
	gencontext_emit_block(context, exit_block);
	gencontext_pop_break_continue(context);
}

void gencontext_emit_while_stmt(GenContext *context, Ast *ast)
{
	// First, emit all inits.

	if (ast->while_stmt.decl) gencontext_emit_decl_expr_list_ignore_result(context, ast->while_stmt.decl);


	LLVMBasicBlockRef exit_block = gencontext_create_free_block(context, "while.exit");
	LLVMBasicBlockRef begin_block = gencontext_create_free_block(context, "while.begin");
	LLVMBasicBlockRef body_block = ast->while_stmt.body->compound_stmt.stmts ? gencontext_create_free_block(context, "while.body") : NULL;

	gencontext_push_break_continue(context, exit_block, begin_block, NULL);

	// Emit cond
	gencontext_emit_br(context, begin_block);
	gencontext_emit_block(context, begin_block);
	DeferList defers = { NULL, NULL };
	Ast *cond = ast->while_stmt.cond;
	if (cond->ast_kind == AST_SCOPED_STMT)
	{
		defers = cond->scoped_stmt.defers;
		cond = cond->scoped_stmt.stmt;
	}
	LLVMValueRef value = gencontext_emit_decl_expr_list(context, cond, true);
	// If we have a body, conditionally jump to it.
	if (body_block)
	{
		gencontext_emit_cond_br(context, value, body_block, exit_block);
	}
	else
	{
		// Emit defers
		gencontext_emit_defer(context, defers.start, defers.end);

		// Otherwise jump to inc or cond depending on what's available.
		gencontext_emit_cond_br(context, value, begin_block, exit_block);
	}

	if (body_block)
	{
		gencontext_emit_block(context, body_block);
		gencontext_emit_stmt(context, ast->while_stmt.body);

		// Emit defers
		gencontext_emit_defer(context, defers.start, defers.end);
	}

	// Loop back.
	gencontext_emit_br(context, begin_block);

	// And insert exit block
	gencontext_emit_block(context, exit_block);

	// Emit defers
	gencontext_emit_defer(context, defers.start, defers.end);
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


void gencontext_emit_label(GenContext *context, Ast *ast)
{
	gencontext_emit_br(context, ast->label_stmt.backend_value);
	gencontext_emit_block(context, ast->label_stmt.backend_value);
	context->current_block_is_target = true;
}

void gencontext_emit_switch(GenContext *context, Ast *ast)
{
	// TODO check defer correctness
	if (ast->switch_stmt.decl) gencontext_emit_decl_expr_list_ignore_result(context, ast->switch_stmt.decl);
	LLVMValueRef switch_value = gencontext_emit_decl_expr_list(context, ast->switch_stmt.cond, false);
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
		if (!case_stmt->case_stmt.expr)
		{
			if (case_stmt->case_stmt.body)
			{
				case_stmt->case_stmt.backend_value = gencontext_create_free_block(context, "switch.default");
			}
			default_case = case_stmt;
		}
		else if (case_stmt->case_stmt.body)
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
			LLVMValueRef case_value = gencontext_emit_expr(context, case_stmt->case_stmt.expr);
			LLVMAddCase(switch_stmt, case_value, block);
		}

		// Skip fallthroughs.
		if (!case_stmt->case_stmt.body) continue;

		gencontext_emit_block(context, block);
		// IMPORTANT!
		context->current_block_is_target = true;
		gencontext_push_break_continue(context, NULL, NULL, i < cases - 1 ? ast->switch_stmt.cases[i + 1]->case_stmt.backend_value : exit_block);
		gencontext_emit_stmt(context, case_stmt->case_stmt.body);
		gencontext_pop_break_continue(context);
		gencontext_emit_br(context, exit_block);
	}
	gencontext_pop_break_continue(context);
	gencontext_emit_block(context, exit_block);
}

LLVMValueRef gencontext_get_defer_bool(GenContext *context, Ast *defer)
{
	assert(defer->ast_kind == AST_DEFER_STMT && defer->defer_stmt.emit_boolean);
	if (!defer->defer_stmt.bool_var)
	{
		defer->defer_stmt.bool_var = gencontext_emit_alloca(context, llvm_type(type_bool), "defer");
	}
	return defer->defer_stmt.bool_var;
}

void gencontext_emit_defer(GenContext *context, Ast *defer_start, Ast *defer_end)
{
	if (defer_start == defer_end) return;
	Ast *defer = defer_start;
	while (defer && defer != defer_end)
	{
		if (defer->defer_stmt.emit_boolean)
		{

			// We need at least the exit block and the "then" block.
			LLVMBasicBlockRef exit_block = LLVMCreateBasicBlockInContext(context->context, "skip.defer");
			LLVMBasicBlockRef defer_block = LLVMCreateBasicBlockInContext(context->context, "do.defer");

			LLVMValueRef value = LLVMBuildLoad2(context->builder, llvm_type(type_bool), gencontext_get_defer_bool(context, defer), "will.defer");

			gencontext_emit_cond_br(context, value, defer_block, exit_block);

			// Emit the defer.
			gencontext_emit_block(context, defer_block);
			gencontext_emit_stmt(context, defer->defer_stmt.body);

			// Jump to exit.
			gencontext_emit_br(context, exit_block);

			// And now we just emit the exit block.
			gencontext_emit_block(context, exit_block);
		}
		else
		{
			gencontext_emit_stmt(context, defer->defer_stmt.body);
		}
		defer = defer->defer_stmt.prev_defer;
	}
}

void gencontext_emit_goto(GenContext *context, Ast *ast)
{
	gencontext_emit_defer(context, ast->goto_stmt.defer.start, ast->goto_stmt.defer.end);
	Ast *defer = ast->goto_stmt.label->label_stmt.defer;
	while (defer != ast->goto_stmt.defer.end)
	{
		LLVMBuildStore(context->builder, LLVMConstInt(llvm_type(type_bool), 0, false),
		               gencontext_get_defer_bool(context, defer));
		defer = defer->defer_stmt.prev_defer;
	}
	gencontext_emit_jmp(context, ast->goto_stmt.label->label_stmt.backend_value);
}

void gencontext_emit_break(GenContext *context, Ast *ast)
{
	gencontext_emit_defer(context, ast->break_stmt.defers.start, ast->break_stmt.defers.end);

	assert(context->break_continue_stack_index);
	gencontext_emit_jmp(context, context->break_continue_stack[context->break_continue_stack_index - 1].break_block);
}

void gencontext_emit_continue(GenContext *context, Ast *ast)
{
	gencontext_emit_defer(context, ast->continue_stmt.defers.start, ast->continue_stmt.defers.end);

	assert(context->break_continue_stack_index);
	gencontext_emit_jmp(context, context->break_continue_stack[context->break_continue_stack_index - 1].continue_block);
}

void gencontext_emit_next_stmt(GenContext *context, Ast *ast)
{
	gencontext_emit_defer(context, ast->next_stmt.defers.start, ast->next_stmt.defers.end);

	assert(context->break_continue_stack_index);
	gencontext_emit_jmp(context, context->break_continue_stack[context->break_continue_stack_index - 1].next_block);
}

void gencontext_emit_scoped_stmt(GenContext *context, Ast *ast)
{
	gencontext_emit_stmt(context, ast->scoped_stmt.stmt);
	gencontext_emit_defer(context, ast->scoped_stmt.defers.start, ast->scoped_stmt.defers.end);
}

void gencontext_emit_panic_on_true(GenContext *context, LLVMValueRef value, const char *panic_name)
{
	LLVMBasicBlockRef panic_block = gencontext_create_free_block(context, "panic");
	LLVMBasicBlockRef ok_block = gencontext_create_free_block(context, "checkok");
	gencontext_emit_cond_br(context, value, panic_block, ok_block);
	gencontext_emit_block(context, panic_block);
	gencontext_emit_call_intrinsic(context, trap_intrinsic_id, NULL, NULL, 0);
	gencontext_emit_br(context, ok_block);
	gencontext_emit_block(context, ok_block);
}


void gencontext_emit_stmt(GenContext *context, Ast *ast)
{
	switch (ast->ast_kind)
	{
		case AST_POISONED:
			UNREACHABLE
		case AST_SCOPED_STMT:
			gencontext_emit_scoped_stmt(context, ast);
			break;
		case AST_EXPR_STMT:
		{
			gencontext_emit_expr(context, ast->expr_stmt);
		}
			break;
		case AST_DECLARE_STMT:
			gencontext_emit_decl(context, ast);
			break;
		case AST_BREAK_STMT:
			gencontext_emit_break(context, ast);
			break;
		case AST_CONTINUE_STMT:
			gencontext_emit_continue(context, ast);
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
		case AST_WHILE_STMT:
			gencontext_emit_while_stmt(context, ast);
			break;
		case AST_DO_STMT:
			gencontext_emit_do_stmt(context, ast);
			break;
		case AST_NEXT_STMT:
			gencontext_emit_next_stmt(context, ast);
			break;
		case AST_DEFER_STMT:
			if (ast->defer_stmt.emit_boolean)
			{
				LLVMBuildStore(context->builder, LLVMConstInt(llvm_type(type_bool), 1, false),
				               gencontext_get_defer_bool(context, ast));
			}
			break;
		case AST_NOP_STMT:
			break;
		case AST_CATCH_STMT:
		case AST_TRY_STMT:
			// Should have been lowered.
			UNREACHABLE
		case AST_THROW_STMT:
			gencontext_emit_throw(context, ast);
			break;
		case AST_ASM_STMT:
			TODO
		case AST_ATTRIBUTE:
		case AST_CT_IF_STMT:
		case AST_CT_ELIF_STMT:
		case AST_CT_ELSE_STMT:
		case AST_CT_FOR_STMT:
		case AST_CT_SWITCH_STMT:
		case AST_CT_DEFAULT_STMT:
		case AST_CT_CASE_STMT:
		case AST_GENERIC_CASE_STMT:
		case AST_GENERIC_DEFAULT_STMT:
		case AST_DECL_EXPR_LIST:
			UNREACHABLE
		case AST_LABEL:
			gencontext_emit_label(context, ast);
			break;
		case AST_GOTO_STMT:
			gencontext_emit_goto(context, ast);
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
