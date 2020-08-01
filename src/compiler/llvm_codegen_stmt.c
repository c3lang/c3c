// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"

void gencontext_emit_try_stmt(GenContext *context, Ast *pAst);

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

	decl->ref = gencontext_emit_alloca(context, llvm_type(type_reduced(decl->type)), decl->name);
	if (decl->var.failable)
	{
		decl->var.failable_ref = gencontext_emit_alloca(context, llvm_type(type_error), decl->name);
		LLVMBuildStore(context->builder, gencontext_emit_no_error_union(context), decl->var.failable_ref);
	}
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
		gencontext_emit_assign_expr(context, decl->ref, decl->var.init_expr, decl->var.failable_ref);
	}
	return decl->ref;
}

void gencontext_emit_decl_expr_list_ignore_result(GenContext *context, Expr *expr)
{
	assert(expr->expr_kind == EXPR_DECL_LIST);
	VECEACH(expr->dexpr_list_expr, i)
	{
		gencontext_emit_stmt(context, expr->dexpr_list_expr[i]);
	}
}

LLVMValueRef gencontext_emit_decl_expr_list(GenContext *context, Expr *expr, bool bool_cast)
{
	assert(expr->expr_kind == EXPR_DECL_LIST);
	size_t size = vec_size(expr->dexpr_list_expr);
	size_t last_index = size - 1;
	for (size_t i = 0; i < last_index; i++)
	{
		Ast *ast = expr->dexpr_list_expr[i];
		if (ast->ast_kind == AST_DECLARE_STMT)
		{
			gencontext_emit_decl(context, ast);
		}
		else
		{
			assert(ast->ast_kind == AST_EXPR_STMT);
			gencontext_emit_expr(context, ast->expr_stmt);
		}
	}
	Ast *last = expr->dexpr_list_expr[last_index];
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
			{
				LLVMValueRef value = gencontext_emit_decl(context, last);
				if (bool_cast && last->declare_stmt->var.unwrap)
				{
					return llvm_int(type_bool, 1);
				}
				result = gencontext_emit_load(context, type, value);
			}
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

	bool in_expression_block = context->expr_block_exit != NULL;

	PUSH_ERROR();

	LLVMBasicBlockRef error_return_block = NULL;
	LLVMValueRef error_out = NULL;
	if (!in_expression_block && context->cur_func_decl->func.function_signature.failable)
	{
		error_return_block = gencontext_create_free_block(context, "errretblock");
		error_out = gencontext_emit_alloca(context, llvm_type(type_error), "reterr");
		context->error_var = error_out;
		context->catch_block = error_return_block;
	}
	LLVMValueRef ret_value = ast->return_stmt.expr ? gencontext_emit_expr(context, ast->return_stmt.expr) : NULL;

	POP_ERROR();

	gencontext_emit_defer(context, ast->return_stmt.defer, 0);

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
	if (error_return_block && LLVMGetFirstUse(LLVMBasicBlockAsValue(error_return_block)))
	{
		gencontext_emit_block(context, error_return_block);
		LLVMBuildRet(context->builder, gencontext_emit_load(context, type_error, error_out));
		context->current_block = NULL;
	}
	LLVMBasicBlockRef post_ret_block = gencontext_create_free_block(context, "ret");
	gencontext_emit_block(context, post_ret_block);
}



void gencontext_emit_if(GenContext *context, Ast *ast)
{
	// We need at least the exit block and the "then" block.
	LLVMBasicBlockRef exit_block = LLVMCreateBasicBlockInContext(context->context, "if.exit");
	LLVMBasicBlockRef then_block = LLVMCreateBasicBlockInContext(context->context, "if.then");
	LLVMBasicBlockRef else_block = NULL;

	// We have an optional else block.
	if (ast->if_stmt.else_body)
	{
		else_block = LLVMCreateBasicBlockInContext(context->context, "if.else");
	}
	else
	{
		else_block = exit_block;
	}

	ast->if_stmt.break_block = exit_block;
	// Output boolean value and switch.

	PUSH_ERROR();

	context->catch_block = else_block ?: exit_block;
	context->error_var = NULL;

	LLVMValueRef value = gencontext_emit_decl_expr_list(context, ast->if_stmt.cond, true);

	if (LLVMIsConstant(value))
	{
		unsigned long v =  LLVMConstIntGetZExtValue(value);
		if (v)
		{
			gencontext_emit_br(context, then_block);
			else_block = NULL;
		}
		else
		{
			gencontext_emit_br(context, else_block);
			then_block = NULL;
		}
	}
	else
	{
		gencontext_emit_cond_br(context, value, then_block, else_block);
	}

	POP_ERROR();

	Decl *label = ast->if_stmt.flow.label;
	if (label)
	{
		label->label.break_target = exit_block;
	}

	// Emit the 'then' code.
	if (then_block)
	{
		gencontext_emit_block(context, then_block);
		gencontext_emit_stmt(context, ast->if_stmt.then_body);

		// Jump to exit.
		gencontext_emit_br(context, exit_block);
	}


	// Emit the 'else' branch if present.
	if (else_block && else_block != exit_block)
	{
		gencontext_emit_block(context, else_block);
		gencontext_emit_stmt(context, ast->if_stmt.else_body);
		gencontext_emit_br(context, exit_block);
	}

	// And now we just emit the exit block.
	gencontext_emit_block(context, exit_block);
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

	// Break is simple it always jumps out.
	// For continue:
	// 1. If there is inc, jump to the condition
	// 2. If there is no condition, jump to the body.
	LLVMBasicBlockRef continue_block = inc_block ? inc_block : (cond_block ? cond_block : body_block);

	ast->for_stmt.continue_block = continue_block;
	ast->for_stmt.exit_block = exit_block;
	LLVMValueRef value = NULL;

	LLVMBasicBlockRef loopback_block = cond_block;
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
			loopback_block = body_block;
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
		if (!body_block && !cond_block)
		{
			// We have neither cond nor body, so jump here
			loopback_block = inc_block;
			gencontext_emit_br(context, inc_block);
		}
		// Emit the block
		gencontext_emit_block(context, inc_block);
		gencontext_emit_expr(context, ast->for_stmt.incr);
	}

	if (!loopback_block)
	{
		loopback_block = gencontext_create_free_block(context, "infiniteloop");
		gencontext_emit_br(context, loopback_block);
		gencontext_emit_block(context, loopback_block);
	}
	// Loop back.
	gencontext_emit_br(context, loopback_block);

	// And insert exit block
	gencontext_emit_block(context, exit_block);
}

void gencontext_emit_while_stmt(GenContext *context, Ast *ast)
{
	// First, emit all inits.
	LLVMBasicBlockRef exit_block = gencontext_create_free_block(context, "while.exit");
	LLVMBasicBlockRef begin_block = gencontext_create_free_block(context, "while.begin");
	LLVMBasicBlockRef body_block = ast->while_stmt.body->compound_stmt.stmts ? gencontext_create_free_block(context, "while.body") : NULL;

	ast->while_stmt.continue_block = begin_block;
	ast->while_stmt.break_block = exit_block;

	// Emit cond
	gencontext_emit_br(context, begin_block);
	gencontext_emit_block(context, begin_block);
	DeferList defers = { 0, 0 };
	Expr *cond = ast->while_stmt.cond;
	if (cond->expr_kind == EXPR_SCOPED_EXPR)
	{
		defers = cond->expr_scope.defers;
		cond = cond->expr_scope.expr;
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
}

void gencontext_emit_do_stmt(GenContext *context, Ast *ast)
{
	LLVMBasicBlockRef exit_block = gencontext_create_free_block(context, "do.exit");
	LLVMBasicBlockRef cond_block = ast->do_stmt.expr ? gencontext_create_free_block(context, "do.cond") : NULL;
	LLVMBasicBlockRef body_block = gencontext_create_free_block(context, "do.body");

	// Break is simple it always jumps out.
	// For continue: if there is no condition, exit.
	LLVMBasicBlockRef cont_block = cond_block ? cond_block : exit_block;

	Expr *do_expr = ast->do_stmt.expr;
	Ast *do_body = ast->do_stmt.body;

	// Overwrite:
	ast->do_stmt.break_block = exit_block;
	ast->do_stmt.continue_block = cont_block;

	// Emit the body
	gencontext_emit_br(context, body_block);
	gencontext_emit_block(context, body_block);
	gencontext_emit_stmt(context, do_body);

	if (cond_block)
	{
		gencontext_emit_br(context, cond_block);
		gencontext_emit_block(context, cond_block);
		LLVMValueRef value = gencontext_emit_expr(context, do_expr);
		if (LLVMIsConstant(value))
		{
			unsigned long v =  LLVMConstIntGetZExtValue(value);
			gencontext_emit_br(context, v ? body_block : exit_block);
		}
		else
		{
			gencontext_emit_cond_br(context, value, body_block, exit_block);
		}
	}
	else
	{
		// Branch to the exit
		gencontext_emit_br(context, exit_block);
	}

	// Emit the exit block.
	gencontext_emit_block(context, exit_block);

}




void gencontext_emit_if_switch_body(GenContext *context, LLVMValueRef switch_value, Ast **cases)
{
	size_t case_count = vec_size(cases);
	if (!case_count)
	{
		// No body or default is empty, just exit after the value.
		return;
	}

	Ast *default_case = NULL;
	for (unsigned i = 0; i < case_count; i++)
	{
		Ast *case_stmt = cases[i];
		if (!case_stmt->case_stmt.expr)
		{
			if (case_stmt->case_stmt.body)
			{
				case_stmt->case_stmt.backend_block = gencontext_create_free_block(context, "switch.default");
			}
			default_case = case_stmt;
		}
		else if (case_stmt->case_stmt.body)
		{
			case_stmt->case_stmt.backend_block = gencontext_create_free_block(context, "switch.case");
		}
	}


	LLVMBasicBlockRef exit_block = gencontext_create_free_block(context, "switch.exit");


	for (unsigned i = 0; i < case_count; i++)
	{
		Ast *case_stmt = cases[i];
		if (case_stmt == default_case) continue;
		LLVMValueRef case_value;
		LLVMBasicBlockRef case_block = case_stmt->case_stmt.backend_block;
		// TODO handle string.
		if (case_stmt->case_stmt.is_type)
		{
			case_value = case_stmt->case_stmt.type_info->type->backend_typeid;
		}
		else
		{
			case_value = gencontext_emit_expr(context, case_stmt->case_stmt.expr);
		}
		LLVMValueRef cmp = LLVMBuildICmp(context->builder, LLVMIntNE, switch_value, case_value, "");
		LLVMBasicBlockRef next_block = gencontext_create_free_block(context, "");
		gencontext_emit_cond_br(context, cmp, next_block, case_block);
		gencontext_emit_block(context, next_block);
	}
	if (default_case)
	{
		gencontext_emit_br(context, default_case->case_stmt.backend_block);
	}
	else
	{
		gencontext_emit_br(context, exit_block);
	}
	for (unsigned i = 0; i < case_count; i++)
	{
		Ast *case_stmt = cases[i];
		LLVMBasicBlockRef case_block = case_stmt->case_stmt.backend_block;
		gencontext_emit_block(context, case_block);
		if (!case_stmt->case_stmt.body)
		{
			gencontext_emit_br(context, i == case_count - 1 ? exit_block : cases[i + 1]->case_stmt.backend_block);
			continue;
		}

		// IMPORTANT!
		context->current_block_is_target = true;
		gencontext_emit_stmt(context, case_stmt->case_stmt.body);
		gencontext_emit_br(context, exit_block);

	}
	gencontext_emit_block(context, exit_block);
}

void gencontext_emit_switch_body(GenContext *context, LLVMValueRef switch_value, Ast *switch_ast)
{
	Ast **cases = switch_ast->switch_stmt.cases;
	size_t case_count = vec_size(cases);
	if (!case_count)
	{
		// No body or default is empty, just exit after the value.
		return;
	}

	Ast *default_case = NULL;
	for (unsigned i = 0; i < case_count; i++)
	{
		Ast *case_stmt = cases[i];
		if (!case_stmt->case_stmt.expr)
		{
			if (case_stmt->case_stmt.body)
			{
				case_stmt->case_stmt.backend_block = gencontext_create_free_block(context, "switch.default");
			}
			default_case = case_stmt;
		}
		else if (case_stmt->case_stmt.body)
		{
			case_stmt->case_stmt.backend_block = gencontext_create_free_block(context, "switch.case");
		}
	}

	LLVMBasicBlockRef exit_block = gencontext_create_free_block(context, "switch.exit");
	LLVMBasicBlockRef switch_block = gencontext_create_free_block(context, "switch.entry");
	switch_ast->switch_stmt.codegen.retry_block = switch_block;
	switch_ast->switch_stmt.codegen.exit_block = exit_block;

	// We will now treat the fallthrough cases:
	// switch (i)
	// {
	//    case 1:
	//    case 2:
	//      do_something();
	//    default:
	// }
	LLVMBasicBlockRef next_block = exit_block;
	for (unsigned i = case_count; i > 0; i--)
	{
		Ast *case_stmt = cases[i - 1];
		if (case_stmt->case_stmt.backend_block)
		{
			next_block = case_stmt->case_stmt.backend_block;
			continue;
		}

		case_stmt->case_stmt.backend_block = next_block;
	}


	Type *switch_type = switch_ast->switch_stmt.cond->type;
	LLVMValueRef switch_var = gencontext_emit_alloca(context, llvm_type(switch_type), "");
	switch_ast->switch_stmt.codegen.retry_var = switch_var;
	LLVMBuildStore(context->builder, switch_value, switch_var);

	gencontext_emit_br(context, switch_block);
	gencontext_emit_block(context, switch_block);

	LLVMValueRef switch_load = gencontext_emit_load(context, switch_type, switch_var);

	LLVMValueRef switch_stmt = LLVMBuildSwitch(context->builder, switch_load, default_case ? default_case->case_stmt.backend_block : exit_block, case_count);
	context->current_block = NULL;
	for (unsigned i = 0; i < case_count; i++)
	{
		Ast *case_stmt = cases[i];
		LLVMBasicBlockRef block = case_stmt->case_stmt.backend_block;
		if (case_stmt != default_case)
		{
			LLVMValueRef case_value;
			if (case_stmt->case_stmt.is_type)
			{
				case_value = case_stmt->case_stmt.type_info->type->backend_typeid;
			}
			else
			{
				case_value = gencontext_emit_expr(context, case_stmt->case_stmt.expr);
			}
			LLVMAddCase(switch_stmt, case_value, block);
		}

		// Skip fallthroughs.
		if (!case_stmt->case_stmt.body) continue;

		gencontext_emit_block(context, block);
		// IMPORTANT!
		context->current_block_is_target = true;

		gencontext_emit_stmt(context, case_stmt->case_stmt.body);
		gencontext_emit_br(context, exit_block);
	}
	gencontext_emit_block(context, exit_block);
}

void gencontext_emit_switch(GenContext *context, Ast *ast)
{
	LLVMValueRef switch_value = gencontext_emit_decl_expr_list(context, ast->switch_stmt.cond, false);
	return gencontext_emit_switch_body(context, switch_value, ast);
}

void gencontext_emit_defer(GenContext *context, AstId defer_start, AstId defer_end)
{
	if (defer_start == defer_end) return;
	AstId defer = defer_start;
	while (defer && defer != defer_end)
	{
		Ast *def = astptr(defer);
		LLVMBasicBlockRef exit = gencontext_create_free_block(context, "exit");
		Ast *body = def->defer_stmt.body;
		def->defer_stmt.codegen.exit_block = exit;
		gencontext_emit_stmt(context, body);
		gencontext_emit_br(context, exit);
		gencontext_emit_block(context, exit);
		defer = def->defer_stmt.prev_defer;
	}
}


void gencontext_emit_break(GenContext *context, Ast *ast)
{
	gencontext_emit_defer(context, ast->contbreak_stmt.defers.start, ast->contbreak_stmt.defers.end);
	Ast *jump_target = astptr(ast->contbreak_stmt.ast);
	LLVMBasicBlockRef jump;
	switch (jump_target->ast_kind)
	{
		case AST_IF_STMT:
			jump = jump_target->if_stmt.break_block;
			break;
		case AST_WHILE_STMT:
			jump = jump_target->while_stmt.break_block;
			break;
		case AST_FOR_STMT:
			jump = jump_target->for_stmt.exit_block;
			break;
		case AST_DO_STMT:
			jump = jump_target->do_stmt.break_block;
			break;
		case AST_SWITCH_STMT:
			jump = jump_target->switch_stmt.codegen.exit_block;
			break;
		case AST_DEFER_STMT:
			jump = jump_target->defer_stmt.codegen.exit_block;
			break;
		default:
			UNREACHABLE
	}
	gencontext_emit_jmp(context, jump);
}

void gencontext_emit_continue(GenContext *context, Ast *ast)
{	gencontext_emit_defer(context, ast->contbreak_stmt.defers.start, ast->contbreak_stmt.defers.end);
	Ast *jump_target = astptr(ast->contbreak_stmt.ast);
	LLVMBasicBlockRef jump;
	switch (jump_target->ast_kind)
	{
		case AST_IF_STMT:
		case AST_SWITCH_STMT:
			UNREACHABLE
			break;
		case AST_WHILE_STMT:
			jump = jump_target->while_stmt.continue_block;
			break;
		case AST_DO_STMT:
			jump = jump_target->do_stmt.continue_block;
			break;
		case AST_FOR_STMT:
			jump = jump_target->for_stmt.continue_block;
			break;
		default:
			UNREACHABLE
	}
	gencontext_emit_jmp(context, jump);
}

void gencontext_emit_next_stmt(GenContext *context, Ast *ast)
{
	Ast *jump_target = astptr(ast->next_stmt.case_switch_stmt);
	if (jump_target->ast_kind != AST_SWITCH_STMT)
	{
		gencontext_emit_defer(context, ast->next_stmt.defers.start, ast->next_stmt.defers.end);
		gencontext_emit_jmp(context, jump_target->case_stmt.backend_block);
		return;
	}
	LLVMValueRef value = gencontext_emit_expr(context, ast->next_stmt.switch_expr);
	LLVMBuildStore(context->builder, value, jump_target->switch_stmt.codegen.retry_var);
	gencontext_emit_defer(context, ast->next_stmt.defers.start, ast->next_stmt.defers.end);
	gencontext_emit_jmp(context, jump_target->switch_stmt.codegen.retry_block);
}

void gencontext_emit_scoped_stmt(GenContext *context, Ast *ast)
{
	gencontext_emit_stmt(context, ast->scoped_stmt.stmt);
	gencontext_emit_defer(context, ast->scoped_stmt.defers.start, ast->scoped_stmt.defers.end);
}

void gencontext_generate_catch_block_if_needed(GenContext *context, Ast *ast)
{
	LLVMBasicBlockRef block = ast->catch_stmt.block;
	if (block) return;
	block = gencontext_create_free_block(context, "catchblock");
	ast->catch_stmt.block = block;
	LLVMTypeRef type;
	TODO
#if 0
	if (ast->catch_stmt.error_param->type == type_error_union)
	{
		type = llvm_type(type_error_union);
	}
	else
	{
		type = llvm_type(type_error_base);
	}
	ast->catch_stmt.error_param->ref = gencontext_emit_alloca(context, type, "");
#endif
}

void gencontext_emit_try_stmt(GenContext *context, Ast *ast)
{
	// Create after try block
	LLVMBasicBlockRef after_try = gencontext_create_free_block(context, "after_try");

	// Store catch/error var
	PUSH_ERROR();

	// Set the catch/error var
	context->error_var = NULL;
	context->catch_block = after_try;

	// Emit the checks, which will create jumps like we want them.
	Ast **decl_expr = ast->try_stmt.decl_expr->dexpr_list_expr;
	VECEACH(decl_expr, i)
	{
		Ast *dexpr = decl_expr[i];
		switch (dexpr->ast_kind)
		{
			case AST_EXPR_STMT:
				gencontext_emit_expr(context, dexpr->expr_stmt);
				break;
			case AST_DECLARE_STMT:
				gencontext_emit_decl(context, dexpr);
				break;
			default:
				UNREACHABLE
		}
	}

	// Restore.
	POP_ERROR();

	// Emit the statement
	gencontext_emit_stmt(context, ast->try_stmt.body);

	// Jump to after.
	gencontext_emit_br(context, after_try);
	gencontext_emit_block(context, after_try);

}


static inline void gencontext_emit_assume(GenContext *context, Expr *expr)
{
	// 1. Convert x > 0 && y > 2 => llvm.assume(x > 0) + llvm.assume(y > 2)
	if (expr->expr_kind == EXPR_BINARY && expr->binary_expr.operator == BINARYOP_AND)
	{
		gencontext_emit_assume(context, expr->binary_expr.left);
		gencontext_emit_assume(context, expr->binary_expr.right);
		return;
	}

	// 2. Convert !(x > 0 || y > 2) => llvm.assume(!(x > 0)) + llvm.assume(!(y > 2))
	if (expr->expr_kind == EXPR_UNARY && expr->unary_expr.operator == UNARYOP_NOT)
	{
		Expr *inner = expr->unary_expr.expr;
		if (inner->expr_kind == EXPR_BINARY && inner->binary_expr.operator == BINARYOP_OR)
		{
			Expr *left = inner->binary_expr.left;
			Expr *right = inner->binary_expr.right;

			expr->unary_expr.expr = left;
			gencontext_emit_assume(context, expr);


			expr->unary_expr.expr = right;
			gencontext_emit_assume(context, expr);

			return;
		}
	}

	// 3. Check if pure, if so we emit the assume.
	if (expr->pure)
	{
		LLVMValueRef value = gencontext_emit_expr(context, expr);
		gencontext_emit_call_intrinsic(context, assume_intrinsic_id, NULL, 0, &value, 1);
	}

}
static inline void gencontext_emit_assert_stmt(GenContext *context, Ast *ast)
{
	if (build_options.debug_mode)
	{
		LLVMValueRef value = gencontext_emit_expr(context, ast->assert_stmt.expr);
		LLVMBasicBlockRef on_fail = gencontext_create_free_block(context, "assert_fail");
		LLVMBasicBlockRef on_ok = gencontext_create_free_block(context, "assert_ok");
		gencontext_emit_cond_br(context, value, on_fail, on_ok);
		gencontext_emit_block(context, on_fail);
		// TODO emit message
		gencontext_emit_call_intrinsic(context, trap_intrinsic_id, NULL, 0, NULL, 0);
		gencontext_emit_block(context, on_ok);
		return;
	}
	gencontext_emit_assume(context, ast->assert_stmt.expr);
}

static inline void gencontext_emit_unreachable_stmt(GenContext *context, Ast *ast)
{
	// TODO emit message
	gencontext_emit_call_intrinsic(context, trap_intrinsic_id, NULL, 0, NULL, 0);
	LLVMBuildUnreachable(context->builder);
	LLVMBasicBlockRef block = gencontext_create_free_block(context, "unreachable_block");
	context->current_block = NULL;
	context->current_block_is_target = false;
	gencontext_emit_block(context, block);
}

void gencontext_emit_expr_stmt(GenContext *context, Ast *ast)
{
	if (ast->expr_stmt->failable)
	{
		PUSH_ERROR();
		LLVMBasicBlockRef discard_fail = gencontext_create_free_block(context, "voiderr");
		context->catch_block = discard_fail;
		context->error_var = NULL;
		gencontext_emit_expr(context, ast->expr_stmt);
		gencontext_emit_br(context, discard_fail);
		gencontext_emit_block(context, discard_fail);
		POP_ERROR();
		return;
	}
	gencontext_emit_expr(context, ast->expr_stmt);
}

void gencontext_emit_catch_stmt(GenContext *context, Ast *ast)
{
	Expr *catch_expr;
	LLVMValueRef error_result = NULL;
	if (ast->catch_stmt.has_err_var)
	{
		Decl *error_var = ast->catch_stmt.err_var;
		assert(error_var->type->canonical == type_error);
		error_result = gencontext_emit_alloca(context, llvm_type(type_error), error_var->name);
		error_var->ref = error_result;
		catch_expr = error_var->var.init_expr;

	}
	else
	{
		if (ast->catch_stmt.is_switch)
		{
			error_result = gencontext_emit_alloca(context, llvm_type(type_error), "catchval");
		}
		catch_expr = ast->catch_stmt.catchable;
	}

	// Create catch block.
	LLVMBasicBlockRef catch_block = gencontext_create_free_block(context, "catch");

	// Store catch/error var
	PUSH_ERROR();

	// Set the catch/error var
	context->error_var = error_result;
	context->catch_block = catch_block;

	// Emit the catch, which will create jumps like we want them.
	gencontext_emit_expr(context, catch_expr);

	// Restore.
	POP_ERROR();

	// Create the bloch after and jump to it.
	LLVMBasicBlockRef after_catch = gencontext_create_free_block(context, "after_catch");
	gencontext_emit_br(context, after_catch);

	// Emit catch.
	gencontext_emit_block(context, catch_block);

	if (ast->catch_stmt.is_switch)
	{
		LLVMValueRef ref = gencontext_emit_bitcast(context, error_result, type_get_ptr(type_typeid));
		gencontext_emit_if_switch_body(context, gencontext_emit_load(context, type_typeid, ref), ast->catch_stmt.cases);
	}
	else
	{
		gencontext_emit_stmt(context, ast->catch_stmt.body);
	}

	// Jump to after.
	gencontext_emit_br(context, after_catch);
	gencontext_emit_block(context, after_catch);
}

void gencontext_emit_panic_on_true(GenContext *context, LLVMValueRef value, const char *panic_name)
{
	LLVMBasicBlockRef panic_block = gencontext_create_free_block(context, "panic");
	LLVMBasicBlockRef ok_block = gencontext_create_free_block(context, "checkok");
	gencontext_emit_cond_br(context, value, panic_block, ok_block);
	gencontext_emit_block(context, panic_block);
	gencontext_emit_call_intrinsic(context, trap_intrinsic_id, NULL, 0, NULL, 0);
	gencontext_emit_br(context, ok_block);
	gencontext_emit_block(context, ok_block);
}


void gencontext_emit_stmt(GenContext *context, Ast *ast)
{
	if (context->catch_block == NULL)
	{
		context->catch_block = gencontext_create_free_block(context, "stmt_catch");
	}
	switch (ast->ast_kind)
	{
		case AST_POISONED:
		case AST_DEFINE_STMT:
			UNREACHABLE
		case AST_TRY_STMT:
			gencontext_emit_try_stmt(context, ast);
			break;
		case AST_SCOPED_STMT:
			gencontext_emit_scoped_stmt(context, ast);
			break;
		case AST_EXPR_STMT:
			gencontext_emit_expr_stmt(context, ast);
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
		case AST_NOP_STMT:
			break;
		case AST_CATCH_STMT:
			gencontext_emit_catch_stmt(context, ast);
			break;
		case AST_ASM_STMT:
			TODO
		case AST_ASSERT_STMT:
			gencontext_emit_assert_stmt(context, ast);
			break;;
		case AST_CT_ASSERT:
		case AST_CT_IF_STMT:
		case AST_CT_ELIF_STMT:
		case AST_CT_ELSE_STMT:
		case AST_CT_FOR_STMT:
		case AST_CT_SWITCH_STMT:
		case AST_CASE_STMT:
		case AST_DEFAULT_STMT:
			UNREACHABLE
		case AST_SWITCH_STMT:
			gencontext_emit_switch(context, ast);
			break;
		case AST_UNREACHABLE_STMT:
			gencontext_emit_unreachable_stmt(context, ast);
			break;
		case AST_VOLATILE_STMT:
			TODO
	}
}

