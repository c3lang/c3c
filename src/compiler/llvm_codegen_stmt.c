// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"

static void llvm_emit_switch_body(GenContext *c, BEValue *switch_value, Ast *switch_ast, bool is_typeid);

// Emit a regular compound statement.
void llvm_emit_compound_stmt(GenContext *c, Ast *ast)
{
	ASSERT0(ast->ast_kind == AST_COMPOUND_STMT);

	DebugScope *old_block = NULL;
	if (ast->compound_stmt.parent_defer && llvm_use_debug(c))
	{
		old_block = c->debug.block_stack;
		ASSERT0(ast->compound_stmt.parent_defer);
		c->debug.block_stack = astptr(ast->compound_stmt.parent_defer)->defer_stmt.scope;
	}
	// Push the lexical scope if in debug.
	DEBUG_PUSH_LEXICAL_SCOPE(c, ast->span);

	// Emit the statement chain
	llvm_emit_statement_chain(c, ast->compound_stmt.first_stmt);

	// Pop lexical scope.
	DEBUG_POP_LEXICAL_SCOPE(c);
	if (old_block) c->debug.block_stack = old_block;
}

void llvm_emit_local_static(GenContext *c, Decl *decl, BEValue *value)
{
	// In defers we might already have generated this variable.
	if (decl->backend_ref)
	{
		llvm_value_set_decl(c, value, decl);
		return;
	}

	// Push the builder
	void *builder = c->builder;
	c->builder = c->global_builder;

	// Emit the global.
	decl->backend_ref = llvm_add_global(c, "temp", decl->type, decl->alignment);
	if (IS_OPTIONAL(decl))
	{
		LLVMTypeRef anyfault = llvm_get_type(c, type_anyfault);
		scratch_buffer_append(c->cur_func.name);
		scratch_buffer_append_char('.');
		scratch_buffer_append(decl->name);
		scratch_buffer_append(".f");
		decl->var.optional_ref = llvm_add_global_raw(c, scratch_buffer_to_string(), anyfault, 0);
	}
	llvm_emit_global_variable_init(c, decl);
	// Pop the builder
	c->builder = builder;
	llvm_value_set_decl(c, value, decl);
}
/**
 * This emits a local declaration.
 */
void llvm_emit_local_decl(GenContext *c, Decl *decl, BEValue *value)
{
	switch (decl->var.kind)
	{
		case VARDECL_CONST:
			llvm_emit_local_static(c, decl, value);
			return;
		case VARDECL_LOCAL:
			if (decl->var.is_static)
			{
				llvm_emit_local_static(c, decl, value);
				return;
			}
			break;
		case VARDECL_PARAM_CT:
		case VARDECL_PARAM_CT_TYPE:
		case VARDECL_PARAM_EXPR:
		case VARDECL_GLOBAL:
		case VARDECL_MEMBER:
		case VARDECL_BITMEMBER:
			UNREACHABLE;
		case VARDECL_PARAM_REF: // DEPRECATED
		case VARDECL_PARAM:
		{
			Expr *init_expr = decl->var.init_expr;
			llvm_emit_expr(c, value, init_expr);
			if (llvm_value_is_addr(value) || decl->var.is_written || decl->var.is_addr || llvm_use_accurate_debug_info(c))
			{
				llvm_emit_and_set_decl_alloca(c, decl);
				llvm_store_decl(c, decl, value);
				return;
			}

			decl->is_value = true;
			decl->backend_value = value->value;
			return;
		}
		case VARDECL_UNWRAPPED:
		case VARDECL_ERASE:
		case VARDECL_REWRAPPED:
			return;
		case VARDECL_LOCAL_CT:
		case VARDECL_LOCAL_CT_TYPE:
			UNREACHABLE
	}

	// Get the declaration and the LLVM type.
	Type *var_type = type_lowering(decl->type);
	LLVMTypeRef alloc_type = llvm_get_type(c, var_type);

	// Create a local alloca
	ASSERT0(!decl->backend_ref);
	Type *type_low = type_lowering(decl->type);
	if (decl->var.is_temp && !IS_OPTIONAL(decl) && !decl->var.is_addr && !decl->var.is_written && !type_is_user_defined(
			type_low) && type_low->type_kind != TYPE_ARRAY)
	{
		ASSERT0(decl->var.init_expr);
		llvm_emit_expr(c, value, decl->var.init_expr);
		llvm_value_rvalue(c, value);
		decl->backend_value = value->value;
		decl->is_value = true;
		return;
	}
	llvm_emit_local_var_alloca(c, decl);

	// Create optional storage
	bool is_optional = IS_OPTIONAL(decl);
	if (is_optional)
	{
		scratch_buffer_clear();
		scratch_buffer_append(decl->name ? decl->name : "anon");
		scratch_buffer_append(".f");
		decl->var.optional_ref = llvm_emit_alloca_aligned(c, type_anyfault, scratch_buffer_to_string());
		// Only clear out the result if the assignment isn't an optional.
	}

	// Grab the init expression
	Expr *init = decl->var.init_expr;
	if (init)
	{
		llvm_value_set_decl_address(c, value, decl);
		value->kind = BE_ADDRESS;
		BEValue res = llvm_emit_assign_expr(c, value, decl->var.init_expr, decl->var.optional_ref, true);
		if (!is_optional) *value = res;
		return;
	}

	// If the variable has a no-init, then set the value to undef.
	if (decl->var.no_init)
	{
		llvm_value_set(value, LLVMGetUndef(alloc_type), decl->type);
		if (is_optional)
		{
			llvm_store_to_ptr_raw(c, decl->var.optional_ref, llvm_get_undef(c, type_anyfault), type_anyfault);
		}
		return;
	}


	// Normal case, zero init.
	llvm_value_set_decl_address(c, value, decl);
	if (is_optional)
	{
		llvm_store_to_ptr_zero(c, decl->var.optional_ref, type_anyfault);

		// Prevent accidental optional folding in "llvm_store_zero"!
		value->kind = BE_ADDRESS;
	}

	llvm_store_zero(c, value);
	llvm_value_set(value, llvm_get_zero(c, var_type), var_type);
}

/**
 * Emit the 'cond' in something like: "while (int a = foo(), int bar = a * a, bar > baz)"
 * But also the switch value in switches.
 *
 * @param c the context to use
 * @param be_value the value to return the value of the cond
 * @param expr the expression of type EXPR_COND
 * @param bool_cast true if in while/for/etc false if it is a switch
 */
static void llvm_emit_cond(GenContext *c, BEValue *be_value, Expr *expr, bool bool_cast)
{
	ASSERT0(expr->expr_kind == EXPR_COND);
	ByteSize size = vec_size(expr->cond_expr);

	// First emit everything up to the last element.
	ByteSize last_index = size - 1;
	for (ByteSize i = 0; i < last_index; i++)
	{
		BEValue value;
		llvm_emit_expr(c, &value, expr->cond_expr[i]);
	}

	// Emit the last element.
	Expr *last = expr->cond_expr[last_index];
	llvm_emit_expr(c, be_value, last);

	// If it is a declaration, set it to the address of the variable.
	if (last->expr_kind == EXPR_DECL)
	{
		llvm_value_set_decl_address(c, be_value, last->decl_expr);
	}

	// Cast the result to bool if needed.
	ASSERT0(!bool_cast || be_value->type == type_bool);
}

void llvm_emit_jmp(GenContext *context, LLVMBasicBlockRef block)
{
	llvm_emit_br(context, block);
	LLVMBasicBlockRef post_jump_block = llvm_basic_block_new(context, "unreachable");
	llvm_emit_block(context, post_jump_block);
}

static inline void llvm_emit_return(GenContext *c, Ast *ast)
{

	Expr *expr = ast->return_stmt.expr;
	if (expr && expr->expr_kind == EXPR_OPTIONAL)
	{
		BEValue be_value;
		llvm_emit_expr(c, &be_value, expr->inner_expr);
		if (ast->return_stmt.cleanup_fail)
		{
			llvm_value_rvalue(c, &be_value);
			LLVMValueRef error_out = llvm_emit_alloca_aligned(c, type_anyfault, "reterr");
			llvm_store_to_ptr(c, error_out, &be_value);
			PUSH_DEFER_ERROR(error_out);
			llvm_emit_statement_chain(c, ast->return_stmt.cleanup_fail);
			POP_DEFER_ERROR();
		}
		llvm_emit_return_abi(c, NULL, &be_value);
		return;
	}

	PUSH_CATCH();

	LLVMBasicBlockRef error_return_block = NULL;
	LLVMValueRef error_out = NULL;
	if (c->cur_func.prototype && type_is_optional(c->cur_func.prototype->rtype))
	{
		error_return_block = llvm_basic_block_new(c, "err_retblock");
		error_out = llvm_emit_alloca_aligned(c, type_anyfault, "reterr");
		c->catch = (OptionalCatch) { error_out, error_return_block };
	}

	bool has_return_value = ast->return_stmt.expr != NULL;
	BEValue return_value = { 0 };
	if (has_return_value)
	{
		llvm_emit_expr(c, &return_value, ast->return_stmt.expr);
		llvm_value_fold_optional(c, &return_value);
		c->retval = return_value;
	}

	POP_CATCH();

	if (ast->return_stmt.cleanup || ast->return_stmt.cleanup_fail)
	{
		if (has_return_value)
		{
			if (llvm_temp_as_address(c, return_value.type))
			{
				LLVMValueRef temp = llvm_emit_alloca_aligned(c, return_value.type, "ret$temp");
				llvm_store_to_ptr(c, temp, &return_value);
				llvm_value_set_address_abi_aligned(&return_value, temp, return_value.type);
			}
			else
			{
				llvm_value_rvalue(c, &return_value);
			}
		}
		llvm_emit_statement_chain(c, ast->return_stmt.cleanup);
	}

	if (llvm_get_current_block_if_in_use(c))
	{
		// Are we in an expression block?
		if (!has_return_value)
		{
			llvm_emit_return_implicit(c);
		}
		else
		{
			llvm_emit_return_abi(c, &return_value, NULL);
		}
	}
	if (error_return_block && LLVMGetFirstUse(LLVMBasicBlockAsValue(error_return_block)))
	{
		llvm_emit_block(c, error_return_block);
		PUSH_DEFER_ERROR(error_out);
		llvm_emit_statement_chain(c, ast->return_stmt.cleanup_fail);
		POP_DEFER_ERROR();
		BEValue value;
		llvm_value_set_address_abi_aligned(&value, error_out, type_anyfault);
		llvm_emit_return_abi(c, NULL, &value);
	}
}

static inline void llvm_emit_block_exit_return(GenContext *c, Ast *ast)
{

	LLVMBasicBlockRef error_return_block = NULL;
	LLVMValueRef error_out = NULL;
	BlockExit *exit = *ast->return_stmt.block_exit_ref;

	PUSH_CATCH_VAR_BLOCK(exit->block_error_var, exit->block_optional_exit);

	LLVMBasicBlockRef err_cleanup_block = NULL;
	Expr *ret_expr = ast->return_stmt.expr;

	BEValue return_value = { 0 };
	if (ret_expr)
	{
		if (ast->return_stmt.cleanup_fail && IS_OPTIONAL(ret_expr))
		{
			ASSERT0(c->catch.block);
			err_cleanup_block = llvm_basic_block_new(c, "opt_block_cleanup");
			c->catch.block = err_cleanup_block;
		}
		llvm_emit_expr(c, &return_value, ast->return_stmt.expr);
		llvm_value_fold_optional(c, &return_value);
	}

	POP_CATCH();

	AstId cleanup = ast->return_stmt.cleanup;
	AstId cleanup_fail = ast->return_stmt.cleanup_fail;
	AstId err_cleanup = err_cleanup_block && cleanup_fail ? astid(copy_ast_defer(astptr(cleanup_fail))) : 0;
	if (exit->block_return_out && return_value.value)
	{
		llvm_store_to_ptr_aligned(c, exit->block_return_out, &return_value, type_alloca_alignment(return_value.type));
	}
	llvm_emit_statement_chain(c, cleanup);

	if (err_cleanup_block)
	{
		llvm_emit_br(c, exit->block_return_exit);
		llvm_emit_block(c, err_cleanup_block);
		PUSH_DEFER_ERROR(exit->block_error_var);
		llvm_emit_statement_chain(c, err_cleanup);
		POP_DEFER_ERROR();
		llvm_emit_jmp(c, exit->block_optional_exit);
	}
	else
	{
		llvm_emit_jmp(c, exit->block_return_exit);
	}
}


/**
 * Emit if (...) { ... } else { ... }
 *
 * This code is slightly optimized to omit branches when not needed. This is something LLVM
 * will optimize as well, but it is convenient to make the code slightly smaller for LLVM to work with:
 * 1. If the "then" branch is empty, replace it with "exit".
 * 2. If the "else" branch is empty or missing, replace if with "exit".
 * 3. If both "else" and "then" branches are empty, replace it with just the condition and remove the "exit"
 */
static void llvm_emit_if_stmt(GenContext *c, Ast *ast)
{
	// We need at least the exit block and the "then" block.
	LLVMBasicBlockRef exit_block = llvm_basic_block_new(c, "if.exit");
	LLVMBasicBlockRef then_block = exit_block;
	LLVMBasicBlockRef else_block = exit_block;

	Ast *then_body = astptr(ast->if_stmt.then_body);
	// Only generate a target if
	if (ast_is_not_empty(then_body))
	{
		then_block = llvm_basic_block_new(c, "if.then");
	}

	// We have an optional else block.
	AstId else_id = ast->if_stmt.else_body;
	Ast *else_body = else_id ? astptr(else_id) : NULL;
	if (ast_is_not_empty(else_body))
	{
		else_block = llvm_basic_block_new(c, "if.else");
	}

	Expr *cond = exprptr(ast->if_stmt.cond);
	ast->if_stmt.codegen.break_block = exit_block;

	// Output boolean value and switch.

	Decl *label = declptrzero(ast->if_stmt.flow.label);
	if (label)
	{
		label->label.break_target = exit_block;
	}

	BEValue be_value = { 0 };

	bool exit_in_use = true;

	if (then_body->ast_kind == AST_IF_CATCH_SWITCH_STMT)
	{
		llvm_emit_cond(c, &be_value, cond, false);
		llvm_value_rvalue(c, &be_value);
		BEValue comp;
		llvm_emit_int_comp_zero(c, &comp, &be_value, BINARYOP_NE);
		llvm_emit_cond_br(c, &comp, then_block, else_block);
		llvm_emit_br(c, then_block);
		llvm_emit_block(c, then_block);
		llvm_emit_switch_body(c, &be_value, then_body, false);
		llvm_emit_br(c, exit_block);
		goto EMIT_ELSE;
	}

	llvm_emit_cond(c, &be_value, cond, true);

	llvm_value_rvalue(c, &be_value);

	ASSERT0(llvm_value_is_bool(&be_value));

	if (llvm_value_is_const(&be_value) && then_block != else_block)
	{
		if (LLVMConstIntGetZExtValue(be_value.value))
		{
			llvm_emit_br(c, then_block);
			else_block = exit_block;
		}
		else
		{
			llvm_emit_br(c, else_block);
			then_block = exit_block;
		}
	}
	else
	{
		if (then_block != else_block)
		{
			llvm_emit_cond_br(c, &be_value, then_block, else_block);
		}
		else
		{
			exit_in_use = LLVMGetFirstUse(LLVMBasicBlockAsValue(exit_block)) != NULL;
			if (exit_in_use) llvm_emit_br(c, exit_block);
		}
	}

	// Emit the 'then' code.
	if (then_block != exit_block)
	{
		llvm_emit_block(c, then_block);
		llvm_emit_stmt(c, then_body);

		// Jump to exit.
		llvm_emit_br(c, exit_block);
	}

	EMIT_ELSE:
	// Emit the 'else' branch if present.
	if (else_block != exit_block)
	{
		llvm_emit_block(c, else_block);
		llvm_emit_stmt(c, else_body);
		llvm_emit_br(c, exit_block);
	}

	// And now we just emit the exit block.
	if (exit_in_use)
	{
		llvm_emit_block(c, exit_block);
	}
}

typedef enum
{
	LOOP_NORMAL,
	LOOP_INFINITE,
	LOOP_NONE
}  LoopType;

static inline LoopType loop_type_for_cond(Expr *cond, bool do_while)
{
	if (!cond)
	{
		// We may have do-while (0)
		if (do_while) return LOOP_NONE;

		// OR we have for (int x;;x++)
		return LOOP_INFINITE;
	}

	// Fold simple conds
	if (cond->expr_kind == EXPR_COND && vec_size(cond->cond_expr) == 1)
	{
		cond = cond->cond_expr[0];
	}

	// Do we have a constant cond?
	if (expr_is_const(cond))
	{
		ASSERT0(cond->const_expr.const_kind == CONST_BOOL);
		// The result is either infinite or no loop
		return cond->const_expr.b ? LOOP_INFINITE : LOOP_NONE;
	}

	// Otherwise we have a normal loop.
	return LOOP_NORMAL;
}

void llvm_emit_for_stmt(GenContext *c, Ast *ast)
{
	DEBUG_PUSH_LEXICAL_SCOPE(c, ast->span);
	// First, emit all inits.
	BEValue value;
	if (ast->for_stmt.init) llvm_emit_expr(c, &value, exprptr(ast->for_stmt.init));

	bool no_exit = ast->for_stmt.flow.no_exit;
	ExprId incr = ast->for_stmt.incr;

	LLVMBasicBlockRef inc_block = incr ? llvm_basic_block_new(c, "loop.inc") : NULL;
	Ast *body = astptr(ast->for_stmt.body);
	LLVMBasicBlockRef body_block = ast_is_not_empty(body) ? llvm_basic_block_new(c, "loop.body") : NULL;
	LLVMBasicBlockRef cond_block = NULL;

	// Skipping first cond? This is do-while semantics
	bool skip_first = ast->for_stmt.flow.skip_first;

	ExprId cond_id = ast->for_stmt.cond;
	Expr *cond = cond_id ? exprptr(cond_id) : NULL;
	LoopType loop = loop_type_for_cond(cond, skip_first);

	// This is the starting block to loop back to, and may either be cond, body or inc
	LLVMBasicBlockRef loop_start_block = body_block ? body_block : inc_block;

	// We only emit a cond block if we have a normal loop.
	if (loop == LOOP_NORMAL)
	{
		cond_block = llvm_basic_block_new(c, "loop.cond");
		loop_start_block = cond_block;
	}

	// In the case that *none* of the blocks exist.
	if (!inc_block && !body_block && !cond_block)
	{
		if (loop == LOOP_INFINITE)
		{
			SourceSpan loc = ast->span;

			llvm_emit_panic(c, "Infinite loop found", loc, NULL, NULL);
			llvm_emit_block(c, llvm_basic_block_new(c, "unreachable_block"));
			DEBUG_POP_LEXICAL_SCOPE(c);
			return;
		}
		DEBUG_POP_LEXICAL_SCOPE(c);
		return;
	}

	ASSERT0(loop_start_block != NULL);

	LLVMBasicBlockRef exit_block = llvm_basic_block_new(c, "loop.exit");

	// Break is simple it always jumps out.
	// For continue:
	// 1. If there is inc, jump to the condition
	// 2. If this is not looping, jump to the exit, otherwise go to cond/body depending on what the start is.
	LLVMBasicBlockRef continue_block = inc_block;
	if (!continue_block)
	{
		continue_block = loop == LOOP_NONE ? exit_block : loop_start_block;
	}

	ast->for_stmt.codegen.continue_block = continue_block;
	ast->for_stmt.codegen.exit_block = exit_block;

	// We have a normal loop, so we emit a cond.
	if (loop == LOOP_NORMAL)
	{
		// Emit a jump for do-while semantics, to skip the initial cond.
		if (skip_first)
		{
			LLVMBasicBlockRef do_while_start = body_block ? body_block : inc_block;
			// Only jump if we have a body / inc
			// if the case is do {} while (...) then we basically can treat this as while (...) {}
			llvm_emit_br(c, do_while_start ? do_while_start : cond_block);
		}
		else
		{
			llvm_emit_br(c, cond_block);
		}

		// Emit the block
		llvm_emit_block(c, cond_block);
		BEValue be_value;
		ASSERT0(cond);
		if (cond->expr_kind == EXPR_COND)
		{
			llvm_emit_cond(c, &be_value, cond, true);
		}
		else
		{
			llvm_emit_expr(c, &be_value, cond);
		}
		llvm_value_rvalue(c, &be_value);
		ASSERT0(llvm_value_is_bool(&be_value));

		// If we have a body, conditionally jump to it.
		LLVMBasicBlockRef cond_success = body_block ? body_block : inc_block;
		// If there is a while (...) { } we need to set the success to this block
		if (!cond_success) cond_success = cond_block;
		// Otherwise jump to inc or cond depending on what's available.
		llvm_emit_cond_br(c, &be_value, cond_success, exit_block);
	}

	// The optional cond is emitted, so emit the body
	if (body_block)
	{
		// If we have LOOP_NONE, then we don't need a new block here
		// since we will just exit. That leaves the infinite loop.
		switch (loop)
		{
			case LOOP_NORMAL:
				// If we have LOOP_NORMAL, we already emitted a br to the body.
				// so emit the block
				llvm_emit_block(c, body_block);
				break;
			case LOOP_INFINITE:
				// In this case we have no cond, so we need to emit the br and
				// then the block
				llvm_emit_br(c, body_block);
				llvm_emit_block(c, body_block);
			case LOOP_NONE:
				// If there is no loop, then we will just fall through and the
				// block is needed.
				body_block = NULL;
				break;
		}
		// Now emit the body
		llvm_emit_stmt(c, body);

		// Did we have a jump to inc yet?
		if (inc_block && !llvm_basic_block_is_unused(inc_block))
		{
			// If so we emit the jump to the inc block.
			llvm_emit_br(c, inc_block);
		}
		else
		{
			inc_block = NULL;
		}
	}

	if (incr)
	{
		// We might have neither body nor cond
		// In that case we do a jump from the init.
		if (loop_start_block == inc_block)
		{
			llvm_emit_br(c, inc_block);
		}
		if (inc_block)
		{
			// Emit the block if it exists.
			// The inc block might also be the end of the body block.
			llvm_emit_block(c, inc_block);
		}
		if (llvm_get_current_block_if_in_use(c))
		{
			BEValue dummy;
			llvm_emit_expr(c, &dummy, incr ? exprptr(incr) : NULL);
		}
	}

	// Loop back.
	if (loop != LOOP_NONE)
	{
		llvm_emit_br(c, loop_start_block);
	}
	else
	{
		// If the exit block is unused, just skip it.
		if (llvm_basic_block_is_unused(exit_block))
		{
			DEBUG_POP_LEXICAL_SCOPE(c);
			return;
		}
		llvm_emit_br(c, exit_block);
	}

	// And insert exit block
	llvm_emit_block(c, exit_block);
	DEBUG_POP_LEXICAL_SCOPE(c);
}






static void llvm_emit_switch_body_if_chain(GenContext *c,
										   Ast **cases,
										   Ast *default_case,
										   BEValue *switch_value,
										   LLVMBasicBlockRef exit_block,
										   bool is_type_switch)
{
	LLVMBasicBlockRef next = NULL;
	FOREACH(Ast *, case_stmt, cases)
	{
		LLVMBasicBlockRef block = case_stmt->case_stmt.backend_block;
		if (case_stmt == default_case) continue;
		BEValue be_value;
		Expr *expr = exprptr(case_stmt->case_stmt.expr);
		llvm_emit_expr(c, &be_value, expr);
		llvm_value_rvalue(c, &be_value);
		BEValue equals;
		Expr *to_expr = exprptrzero(case_stmt->case_stmt.to_expr);
		if (to_expr)
		{
			ASSERT0(!is_type_switch);
			BEValue to_value;
			llvm_emit_expr(c, &to_value, to_expr);
			llvm_value_rvalue(c, &to_value);
			BEValue le;
			llvm_emit_comp(c, &le, &be_value, switch_value, BINARYOP_LE);
			BEValue ge;
			llvm_emit_comp(c, &ge, &to_value, switch_value, BINARYOP_GE);
			llvm_value_set(&equals, llvm_emit_and(c, &le, &ge), type_bool);
		}
		else
		{
			if (is_type_switch)
			{
				llvm_emit_lhs_is_subtype(c, &equals, &be_value, switch_value);
			}
			else
			{
				llvm_emit_comp(c, &equals, &be_value, switch_value, BINARYOP_EQ);
			}
		}
		next = llvm_basic_block_new(c, "next_if");
		llvm_emit_cond_br(c, &equals, block, next);
		if (case_stmt->case_stmt.body)
		{
			llvm_emit_block(c, block);
			llvm_emit_stmt(c, case_stmt->case_stmt.body);
			llvm_emit_br(c, exit_block);
		}
		llvm_emit_block(c, next);
	}
	if (default_case && default_case->case_stmt.body)
	{
		llvm_emit_br(c, default_case->case_stmt.backend_block);
		llvm_emit_block(c, default_case->case_stmt.backend_block);
		llvm_emit_stmt(c, default_case->case_stmt.body);
		llvm_emit_br(c, exit_block);
	}
	else
	{
		llvm_emit_br(c, exit_block);
	}
	llvm_emit_block(c, exit_block);
	return;
}

static LLVMValueRef llvm_emit_switch_jump_stmt(GenContext *c,
                                       Ast *switch_ast,
                                       Ast **cases,
									   uint64_t count,
									   int min_index,
									   LLVMValueRef jump_table,
									   LLVMBasicBlockRef default_block,
									   BEValue *switch_value)
{
	unsigned case_count = vec_size(cases);
	BEValue min_val;
	llvm_emit_expr(c, &min_val, exprptr(cases[min_index]->case_stmt.expr));
	ASSERT0(llvm_value_is_const(&min_val));
	llvm_value_rvalue(c, switch_value);
	llvm_value_rvalue(c, &min_val);
	LLVMValueRef min = min_val.value;
	if (!LLVMIsConstant(min) || !LLVMIsNull(min))
	{
		switch_value->value = LLVMBuildSub(c->builder, switch_value->value, min_val.value, "");
	}
	LLVMValueRef is_valid = LLVMBuildICmp(c->builder, LLVMIntUGT, switch_value->value, llvm_const_int(c, switch_value->type, count - 1), "");
	LLVMBasicBlockRef switch_block = llvm_basic_block_new(c, "jumpblock");
	LLVMBuildCondBr(c->builder, is_valid, default_block, switch_block);
	c->current_block = NULL;
	llvm_emit_block(c, switch_block);
	AlignSize align;
	LLVMTypeRef type = LLVMArrayType(c->ptr_type, count);
	LLVMValueRef index = llvm_emit_array_gep_raw_index(c, jump_table, type, switch_value, llvm_abi_alignment(c, type), &align);
	LLVMValueRef addr = llvm_load(c, c->ptr_type, index, align, "target");
	LLVMValueRef instr = LLVMBuildIndirectBr(c->builder, addr, case_count);
	c->current_block = NULL;
	return instr;
}

static void llvm_set_jump_table_values(ExprId from, ExprId to, Int *from_ref, Int *to_ref)
{
	Expr *from_expr = exprptr(from);
	Expr *to_expr = exprptrzero(to);
	Type *type_flat = type_flatten(from_expr->type);
	ASSERT(from, expr_is_const(from_expr) && (!to_expr || expr_is_const(to_expr)));
	if (type_flat->type_kind == TYPE_ENUM)
	{
		Type *low = type_lowering(type_flat);
		*from_ref = (Int) { .i.low = from_expr->const_expr.enum_err_val->enum_constant.ordinal, .type = low->type_kind };
		if (to)
		{
			*to_ref = (Int) { .i.low = to_expr->const_expr.enum_err_val->enum_constant.ordinal, .type = low->type_kind };
		}
		else
		{
			*to_ref = *from_ref;
		}
	}
	else
	{
		ASSERT(from, type_is_integer(type_flat));
		*from_ref = from_expr->const_expr.ixx;
		*to_ref = to_expr ? to_expr->const_expr.ixx : *from_ref;
	}

}
static void llvm_emit_switch_jump_table(GenContext *c,
										Ast *switch_ast,
										Ast **cases,
										Ast *default_case,
										BEValue *switch_value,
										LLVMBasicBlockRef exit_block)
{
	unsigned case_count = vec_size(cases);
	if (!case_count) return;
	Int min = { .type = TYPE_VOID };
	Int max = { .type = TYPE_VOID };
	int min_index = -1;
	int default_index = -1;
	LLVMBasicBlockRef default_block = exit_block;
	for (unsigned i = 0; i < case_count; i++)
	{
		Ast *case_ast = cases[i];
		if (case_ast->ast_kind == AST_DEFAULT_STMT)
		{
			default_block = case_ast->case_stmt.backend_block;
			default_index = i;
			continue;
		}
		Int value, to_value;
		llvm_set_jump_table_values(case_ast->case_stmt.expr, case_ast->case_stmt.to_expr, &value, &to_value);
		if (min.type == TYPE_VOID)
		{
			min = value;
			max = to_value;
			min_index = i;
		}
		else if (int_comp(value, min, BINARYOP_LT))
		{
			min = value;
			min_index = i;
		}
		else if (int_comp(to_value, max, BINARYOP_GT))
		{
			max = to_value;
		}
	}
	switch_ast->switch_stmt.codegen.jump.default_index = default_index;
	switch_ast->switch_stmt.codegen.jump.min_index = min_index;
	max = int_sub(max, min);
	ASSERT0(max.i.low <= 0xFFFF);
	uint64_t count = switch_ast->switch_stmt.codegen.jump.count = max.i.low + 1;
	ASSERT0(!max.i.high && "Should never exceed 64 bytes");

	Type *goto_array_type = type_get_array(type_voidptr, count);
	LLVMTypeRef llvm_array_type = llvm_get_type(c, goto_array_type);
	AlignSize alignment = type_alloca_alignment(switch_value->type);

	LLVMValueRef jmptable = llvm_add_global_raw(c, "jumptable", llvm_array_type, alignment);
	switch_ast->switch_stmt.codegen.jump.jmptable = jmptable;

	llvm_set_private_declaration(jmptable);
	LLVMSetGlobalConstant(jmptable, 1);
	BEValue array_value;

	LLVMValueRef instr = llvm_emit_switch_jump_stmt(c, switch_ast, cases, count, min_index, jmptable, default_block, switch_value);

	static LLVMValueRef refs[DEFAULT_SWITCHRANGE_MAX_SIZE];
	LLVMValueRef default_block_address = LLVMBlockAddress(c->cur_func.ref, default_block);
	ASSERT0(count < DEFAULT_SWITCHRANGE_MAX_SIZE);
	memset(refs, 0, sizeof(LLVMValueRef) * count);
	for (unsigned i = 0; i < case_count; i++)
	{
		Ast *case_stmt = cases[i];
		LLVMBasicBlockRef block = case_stmt->case_stmt.backend_block;
		if (case_stmt->ast_kind != AST_DEFAULT_STMT)
		{
			Int value, to_value;
			llvm_set_jump_table_values(case_stmt->case_stmt.expr, case_stmt->case_stmt.to_expr, &value, &to_value);
			value = int_sub(value, min);
			to_value = int_sub(to_value, min);
			uint64_t from_val = value.i.low;
			uint64_t to_val = to_value.i.low;
			for (uint64_t j = from_val; j <= to_val; j++)
			{
				refs[j] = LLVMBlockAddress(c->cur_func.ref, block);
			}
			// No fallthrough
			if (!case_stmt->case_stmt.body) continue;
			LLVMAddDestination(instr, block);
		}

		llvm_emit_block(c, block);
		llvm_emit_stmt(c, case_stmt->case_stmt.body);
		llvm_emit_br(c, exit_block);
	}

	bool found = false;
	for (uint64_t i = 0; i < count; i++)
	{
		if (refs[i]) continue;
		refs[i] = default_block_address;
		if (found) continue;
		found = true;
		LLVMAddDestination(instr, default_block);
	}

	LLVMSetInitializer(jmptable, LLVMConstArray(c->ptr_type, refs, count));
	llvm_emit_block(c, exit_block);
}

static void llvm_emit_switch_body(GenContext *c, BEValue *switch_value, Ast *switch_ast, bool is_typeid)
{
	bool is_if_chain = switch_ast->switch_stmt.flow.if_chain;
	Type *switch_type = switch_ast->ast_kind == AST_IF_CATCH_SWITCH_STMT ? type_lowering(type_anyfault) : switch_value->type;
	Ast **cases = switch_ast->switch_stmt.cases;
	ArraySize case_count = vec_size(cases);
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
				case_stmt->case_stmt.backend_block = llvm_basic_block_new(c, "switch.default");
			}
			default_case = case_stmt;
		}
		else if (case_stmt->case_stmt.body)
		{
			case_stmt->case_stmt.backend_block = llvm_basic_block_new(c, "switch.case");
		}
	}

	LLVMBasicBlockRef exit_block = llvm_basic_block_new(c, "switch.exit");
	LLVMBasicBlockRef switch_block = llvm_basic_block_new(c, "switch.entry");
	switch_ast->switch_stmt.codegen.retry.block = switch_block;
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

	BEValue switch_var;
	llvm_value_set_address_abi_aligned(&switch_var, llvm_emit_alloca_aligned(c, switch_type, "switch"), switch_type);
	switch_ast->switch_stmt.codegen.retry.var = &switch_var;
	llvm_store(c, &switch_var, switch_value);

	llvm_emit_br(c, switch_block);
	llvm_emit_block(c, switch_block);

	BEValue switch_current_val = switch_var;
	llvm_value_rvalue(c, &switch_current_val);

	if (is_if_chain)
	{
		llvm_emit_switch_body_if_chain(c, cases, default_case, &switch_current_val, exit_block, is_typeid);
		return;
	}

	if (switch_ast->switch_stmt.flow.jump)
	{
		llvm_emit_switch_jump_table(c, switch_ast, cases, default_case, &switch_current_val, exit_block);
		return;
	}
	ASSERT0(!is_typeid);

	LLVMValueRef switch_stmt = LLVMBuildSwitch(c->builder, switch_current_val.value, default_case ? default_case->case_stmt.backend_block : exit_block, case_count);
	c->current_block = NULL;
	for (unsigned i = 0; i < case_count; i++)
	{
		Ast *case_stmt = cases[i];
		LLVMBasicBlockRef block = case_stmt->case_stmt.backend_block;
		if (case_stmt != default_case)
		{
			LLVMValueRef case_value;
			BEValue be_value;
			Expr *from = exprptr(case_stmt->case_stmt.expr);
			ASSERT0(expr_is_const(from));
			llvm_emit_expr(c, &be_value, from);
			llvm_value_rvalue(c, &be_value);
			case_value = be_value.value;
			LLVMAddCase(switch_stmt, case_value, block);
			Expr *to_expr = exprptrzero(case_stmt->case_stmt.to_expr);
			if (to_expr)
			{
				BEValue to_value;
				llvm_emit_expr(c, &to_value, to_expr);
				llvm_value_rvalue(c, &to_value);
				LLVMValueRef to = to_value.value;
				ASSERT0(LLVMIsAConstant(to));
				LLVMValueRef one = llvm_const_int(c, to_value.type, 1);
				while (LLVMConstIntGetZExtValue(LLVMBuildICmp(c->builder, LLVMIntEQ, to, case_value, "")) != 1)
				{
					case_value = LLVMBuildAdd(c->builder, case_value, one, "");
					LLVMAddCase(switch_stmt, case_value, block);
				}
			}
		}

		// Skip fallthroughs.
		if (!case_stmt->case_stmt.body) continue;

		llvm_emit_block(c, block);

		llvm_emit_stmt(c, case_stmt->case_stmt.body);
		llvm_emit_br(c, exit_block);
	}
	llvm_emit_block(c, exit_block);
}

void llvm_emit_switch(GenContext *c, Ast *ast)
{
	DEBUG_PUSH_LEXICAL_SCOPE(c, ast->span);
	BEValue switch_value;
	Expr *expr = exprptrzero(ast->switch_stmt.cond);
	bool is_typeid = expr && expr->type->canonical == type_typeid;
	if (expr)
	{
		// Regular switch
		llvm_emit_cond(c, &switch_value, expr, false);
	}
	else
	{
		// Match switch, so set the value to true
		llvm_value_set(&switch_value, llvm_const_int(c, type_bool, 1), type_bool);
	}
	llvm_emit_switch_body(c, &switch_value, ast, is_typeid);
	DEBUG_POP_LEXICAL_SCOPE(c);
}


void llvm_emit_break(GenContext *c, Ast *ast)
{
	llvm_emit_statement_chain(c, ast->contbreak_stmt.defers);
	Ast *jump_target = astptr(ast->contbreak_stmt.ast);
	LLVMBasicBlockRef jump;
	switch (jump_target->ast_kind)
	{
		case AST_IF_STMT:
			jump = jump_target->if_stmt.codegen.break_block;
			break;
		case AST_FOR_STMT:
			jump = jump_target->for_stmt.codegen.exit_block;
			break;
		case AST_IF_CATCH_SWITCH_STMT:
		case AST_SWITCH_STMT:
			jump = jump_target->switch_stmt.codegen.exit_block;
			break;
		case AST_FOREACH_STMT:
		default:
			UNREACHABLE
	}
	llvm_emit_jmp(c, jump);
}

void llvm_emit_continue(GenContext *c, Ast *ast)
{
	ASSERT0(ast->contbreak_stmt.is_resolved);
	llvm_emit_statement_chain(c, ast->contbreak_stmt.defers);
	Ast *jump_target = astptr(ast->contbreak_stmt.ast);
	LLVMBasicBlockRef jump;
	switch (jump_target->ast_kind)
	{
		case AST_IF_STMT:
		case AST_SWITCH_STMT:
		case AST_FOREACH_STMT:
			UNREACHABLE
			break;
		case AST_FOR_STMT:
			jump = jump_target->for_stmt.codegen.continue_block;
			break;
		default:
			UNREACHABLE
	}
	llvm_emit_jmp(c, jump);
}

void gencontext_emit_next_stmt(GenContext *context, Ast *ast)
{
	Ast *jump_target = astptr(ast->nextcase_stmt.case_switch_stmt);
	if (jump_target->ast_kind != AST_SWITCH_STMT)
	{
		llvm_emit_statement_chain(context, ast->nextcase_stmt.defer_id);
		llvm_emit_jmp(context, jump_target->case_stmt.backend_block);
		return;
	}
	BEValue be_value;
	llvm_emit_expr(context, &be_value, ast->nextcase_stmt.switch_expr);
	if (jump_target->switch_stmt.flow.jump)
	{
		llvm_emit_statement_chain(context, ast->nextcase_stmt.defer_id);
		Ast **cases = jump_target->switch_stmt.cases;
		int default_index = jump_target->switch_stmt.codegen.jump.default_index;
		LLVMBasicBlockRef exit_block = jump_target->switch_stmt.codegen.exit_block;
		LLVMValueRef instr = llvm_emit_switch_jump_stmt(context, jump_target, cases,
		                                                jump_target->switch_stmt.codegen.jump.count,
		                                                jump_target->switch_stmt.codegen.jump.min_index,
		                                                jump_target->switch_stmt.codegen.jump.jmptable,
		                                                default_index < 0
														? exit_block
		                                                : cases[default_index]->case_stmt.backend_block,
		                                                &be_value);
		FOREACH(Ast *, case_ast, cases)
		{
			if (!case_ast->case_stmt.body) continue;
			LLVMAddDestination(instr, case_ast->case_stmt.backend_block);
		}
		if (default_index < 0) LLVMAddDestination(instr, exit_block);

		return;
	}
	llvm_store(context, jump_target->switch_stmt.codegen.retry.var, &be_value);
	llvm_emit_statement_chain(context, ast->nextcase_stmt.defer_id);
	llvm_emit_jmp(context, jump_target->switch_stmt.codegen.retry.block);
}


static inline void llvm_emit_assume(GenContext *c, Expr *expr)
{
	// 1. Convert x > 0 && y > 2 => llvm.assume(x > 0) + llvm.assume(y > 2)
	if (expr->expr_kind == EXPR_BINARY && expr->binary_expr.operator == BINARYOP_AND)
	{
		llvm_emit_assume(c, exprptr(expr->binary_expr.left));
		llvm_emit_assume(c, exprptr(expr->binary_expr.right));
		return;
	}

	// 2. Convert !(x > 0 || y > 2) => llvm.assume(!(x > 0)) + llvm.assume(!(y > 2))
	if (expr->expr_kind == EXPR_UNARY && expr->unary_expr.operator == UNARYOP_NOT)
	{
		Expr *inner = expr->unary_expr.expr;
		if (inner->expr_kind == EXPR_BINARY && inner->binary_expr.operator == BINARYOP_OR)
		{
			Expr *left = exprptr(inner->binary_expr.left);
			Expr *right = exprptr(inner->binary_expr.right);

			expr->unary_expr.expr = left;
			llvm_emit_assume(c, expr);


			expr->unary_expr.expr = right;
			llvm_emit_assume(c, expr);

			return;
		}
	}

	// 3. Check if pure, if so we emit the assume.
	if (expr_is_pure(expr))
	{
		BEValue value;
		llvm_emit_expr(c, &value, expr);
		llvm_value_rvalue(c, &value);
		ASSERT0(value.kind == BE_BOOLEAN);
		EMIT_EXPR_LOC(c, expr);
		llvm_emit_assume_true(c, &value);
	}
}

static inline void llvm_emit_assert_stmt(GenContext *c, Ast *ast)
{
	ExprId exprid = ast->assert_stmt.expr;
	Expr *assert_expr = exprptr(exprid);
	if (compile_asserts())
	{
		BEValue value;
		llvm_emit_expr(c, &value, assert_expr);
		llvm_value_rvalue(c, &value);
		LLVMBasicBlockRef on_fail = llvm_basic_block_new(c, "assert_fail");
		LLVMBasicBlockRef on_ok = llvm_basic_block_new(c, "assert_ok");
		ASSERT0(value.kind == BE_BOOLEAN);
		llvm_emit_cond_br(c, &value, on_ok, on_fail);
		llvm_emit_block(c, on_fail);
		SourceSpan loc = assert_expr->span;
		const char *error = "Assert violation";
		const char *fmt = NULL;
		Expr *message_expr = exprptrzero(ast->assert_stmt.message);
		BEValue *values = NULL;
		if (message_expr)
		{
			const char *err_msg = exprptr(ast->assert_stmt.message)->const_expr.bytes.ptr;
			Expr **args = ast->assert_stmt.args;
			if (vec_size(args))
			{
				fmt = err_msg;
				FOREACH(Expr *, arg, args)
				{
					BEValue var;
					llvm_emit_expr(c, &var, arg);
					llvm_emit_any_from_value(c, &var, arg->type);
					vec_add(values, var);
				}
			}
			else
			{
				error = err_msg;
			}
		}
		llvm_emit_panic(c, error, loc, fmt, values);
		llvm_emit_block(c, on_ok);
		EMIT_LOC(c, ast);
		return;
	}
	llvm_emit_assume(c, exprptr(ast->assert_stmt.expr));
}

static inline void add_target_clobbers_to_buffer(GenContext *c)
{
	switch (compiler.platform.arch)
	{
		case ARCH_TYPE_X86_64:
		case ARCH_TYPE_X86:
			scratch_buffer_append("~{dirflag},~{fpsr},~{flags}");
			break;
		case ARCH_TYPE_MIPS:
		case ARCH_TYPE_MIPS64:
		case ARCH_TYPE_MIPS64EL:
		case ARCH_TYPE_MIPSEL:
			// Currently Clang does this
			scratch_buffer_append("~{$1}");
			break;
		default:
			// In Clang no other platform has automatic clobbers
			break;
	}
}

static void codegen_append_constraints(ClobberList *clobber_list, const char *str)
{
	char *string = clobber_list->string;
	unsigned len = clobber_list->constraint_len;
	while (*str)
	{
		if (len > 1022) error_exit("Constraint list exceeded max length.");
		string[len++] = *(str++);
	}
	clobber_list->constraint_len = len;
}

static void codegen_new_constraint(ClobberList *clobber_list)
{
	if (clobber_list->constraint_len) codegen_append_constraints(clobber_list, ",");
}

static inline void llvm_emit_asm_block_stmt(GenContext *c, Ast *ast)
{
	const char *data;
	scratch_buffer_clear();
	add_target_clobbers_to_buffer(c);
	char *clobbers = scratch_buffer_copy();
	ClobberList clobber_list = { .constraint_len = 0 };
	LLVMTypeRef param_types[512];
	LLVMTypeRef pointer_type[512];
	LLVMValueRef args[512];
	LLVMTypeRef result_types[512];
	Decl *result_decls[512];
	unsigned result_count = 0;
	unsigned param_count = 0;
	AsmInlineBlock *block = ast->asm_block_stmt.block;
	if (ast->asm_block_stmt.is_string)
	{
		data = exprptr(ast->asm_block_stmt.asm_string)->const_expr.bytes.ptr;
	}
	else
	{
		data = codegen_create_asm(ast);
		clobbers = clobber_list.string;
		FOREACH(ExprAsmArg *,  var, block->output_vars)
		{
			codegen_new_constraint(&clobber_list);
			if (var->kind == ASM_ARG_MEMVAR)
			{
				if (var->ident.early_clobber)
				{
					codegen_append_constraints(&clobber_list, "=*&m");
				}
				else
				{
					codegen_append_constraints(&clobber_list, "=*m");
				}
				BEValue value;
				llvm_value_set_decl(c, &value, var->ident.ident_decl);
				llvm_value_addr(c, &value);
				value.kind = BE_VALUE;
				pointer_type[param_count] = llvm_get_type(c, value.type);
				value.type = type_get_ptr(value.type);
				llvm_value_rvalue(c, &value);
				param_types[param_count] = LLVMTypeOf(value.value);
				args[param_count++] = value.value;
				continue;
			}
			ASSERT0(var->kind == ASM_ARG_REGVAR);
			if (var->ident.early_clobber)
			{
				codegen_append_constraints(&clobber_list, "=&r");
			}
			else
			{
				codegen_append_constraints(&clobber_list, "=r");
			}
			Decl *decl = result_decls[result_count] = var->ident.ident_decl;
			result_types[result_count++] = llvm_get_type(c, decl->type);
		}

		FOREACH(ExprAsmArg *, val, block->input)
		{
			BEValue value;
			codegen_new_constraint(&clobber_list);
			pointer_type[param_count] = NULL;
			switch (val->kind)
			{
				case ASM_ARG_MEMVAR:
					llvm_value_set_decl(c, &value, val->ident.ident_decl);
					llvm_value_addr(c, &value);
					value.kind = BE_VALUE;
					pointer_type[param_count] = llvm_get_type(c, value.type);
					value.type = type_get_ptr(value.type);
					ASSERT0(!val->ident.copy_output);
					codegen_append_constraints(&clobber_list, "*m");
					break;
				case ASM_ARG_REGVAR:
					llvm_value_set_decl(c, &value, val->ident.ident_decl);
					if (val->ident.copy_output)
					{
						char buf[10];
						snprintf(buf, 10, "%d", val->index);
						codegen_append_constraints(&clobber_list, buf);
					}
					else
					{
						codegen_append_constraints(&clobber_list, "r");
					}
					break;
				case ASM_ARG_VALUE:
					llvm_emit_exprid(c, &value, val->expr_id);
					codegen_append_constraints(&clobber_list, "r");
					break;
				default:
					TODO
			}
			llvm_value_rvalue(c, &value);
			param_types[param_count] = LLVMTypeOf(value.value);
			args[param_count++] = value.value;
		}


		for (int i = 0; i < CLOBBER_FLAG_ELEMENTS; i++)
		{
			uint64_t clobber_mask = block->clobbers.mask[i];
			if (!clobber_mask) continue;
			uint64_t mask = 1;
			for (int j = 0; j < 64; j++)
			{
				if (mask & clobber_mask)
				{
					unsigned clobber_index = i * 64 + j;
					codegen_new_constraint(&clobber_list);
					codegen_append_constraints(&clobber_list, "~{");
					codegen_append_constraints(&clobber_list, asm_clobber_by_index(clobber_index));
					codegen_append_constraints(&clobber_list, "}");
				}
				mask <<= 1;
			}
		}
		if (compiler.platform.extra_clobbers)
		{
			codegen_new_constraint(&clobber_list);
			codegen_append_constraints(&clobber_list, compiler.platform.extra_clobbers);
		}
	}
	DEBUG_LOG("Asm: %s (%s)", data, clobbers);
	LLVMTypeRef result_type;
	if (result_count)
	{
		result_type = result_count == 1 ? result_types[0] : LLVMStructTypeInContext(c->context, result_types, result_count, false);
	}
	else
	{
		result_type = llvm_get_type(c, type_void);
	}
	LLVMTypeRef asm_fn_type = LLVMFunctionType(result_type, param_types, param_count, 0);
	LLVMValueRef asm_fn = LLVMGetInlineAsm(asm_fn_type,
										   (char*)data,
										   strlen(data),
										   clobbers,
										   strlen(clobbers),
										   ast->asm_block_stmt.is_volatile,
										   true,
										   LLVMInlineAsmDialectATT,
										   /* can throw */ false
										   );
	LLVMValueRef res = LLVMBuildCall2(c->builder, asm_fn_type, asm_fn, args, param_count, "");
	for (unsigned i = 0; i < param_count; i++)
	{
		if (pointer_type[i])
		{
			llvm_attribute_add_call_type(c, res, attribute_id.elementtype, i + 1, pointer_type[i]);
		}
	}
	if (!result_count) return;
	if (result_count == 1)
	{
		Decl *decl = block->output_vars[0]->ident.ident_decl;
		llvm_store_decl_raw(c, decl, res);
		return;
	}
	for (unsigned i = 0; i < result_count; i++)
	{
		Decl *decl = result_decls[i];
		LLVMValueRef res_val = LLVMBuildExtractValue(c->builder, res, i, "");
		llvm_store_decl_raw(c, decl, res_val);
	}
}


static void llvm_emit_expr_stmt(GenContext *c, Ast *ast)
{
	llvm_emit_ignored_expr(c, ast->expr_stmt);
}

LLVMValueRef llvm_emit_string_const(GenContext *c, const char *str, const char *extname)
{
	size_t len = str ? strlen(str) : 0;
	if (!len) return llvm_emit_empty_string_const(c);
	LLVMValueRef val = llvm_emit_zstring_named(c, str, extname);
	LLVMValueRef data[2] = { val, llvm_const_int(c, type_usz, strlen(str)) };
	return llvm_get_struct_named(c->chars_type, data, 2);
}

LLVMValueRef llvm_emit_empty_string_const(GenContext *c)
{
	return LLVMConstNull(c->chars_type);
}

LLVMValueRef llvm_emit_zstring_named(GenContext *c, const char *str, const char *extname)
{
	FOREACH(ReusableConstant, constant, c->reusable_constants)
	{
		if (str_eq(str, constant.string) && str_eq(extname, constant.name))
		{
			return constant.value;
		}
	}

	unsigned len = (unsigned)strlen(str);
	LLVMTypeRef char_array_type = LLVMArrayType(c->byte_type, len + 1);
	LLVMValueRef global_string = llvm_add_global_raw(c, extname, char_array_type, 0);
	llvm_set_internal_linkage(global_string);
	LLVMSetGlobalConstant(global_string, 1);
	LLVMSetInitializer(global_string, llvm_get_zstring(c, str, len));
	AlignSize alignment;
	LLVMValueRef string = llvm_emit_array_gep_raw(c, global_string, char_array_type, 0, 1, &alignment);
	ReusableConstant reuse = { .string = str_copy(str, len), .name = str_copy(extname, strlen(extname)), .value = string };
	vec_add(c->reusable_constants, reuse);
	return string;
}

void llvm_emit_unreachable(GenContext *c)
{
	LLVMBuildUnreachable(c->builder);
	c->current_block = NULL;
}

void llvm_emit_panic(GenContext *c, const char *message, SourceSpan loc, const char *fmt, BEValue *varargs)
{
	if (c->debug.builder) llvm_emit_debug_location(c, loc);

	Decl *panic_var = c->panic_var;
	if (no_panic() || !panic_var )
	{
		if (safe_mode_enabled())
		{
			llvm_emit_call_intrinsic(c, intrinsic_id.trap, NULL, 0, NULL, 0);
		}
		llvm_emit_unreachable(c);
		return;
	}

	File *file = source_file_by_id(loc.file_id);

	Decl *panicf = fmt ? c->panicf : NULL;

	LLVMValueRef panic_args[5] = {
			llvm_emit_string_const(c, panicf ? fmt : message, ".panic_msg"),
			llvm_emit_string_const(c, file->name, ".file"),
			llvm_emit_string_const(c, c->cur_func.name, ".func"),
			llvm_const_int(c, type_uint, loc.row)
	};
	FunctionPrototype *prototype = panicf
			? type_get_resolved_prototype(panicf->type)
			: type_get_resolved_prototype(panic_var->type->canonical->pointer);
	LLVMValueRef actual_args[16];
	unsigned count = 0;
	ABIArgInfo **abi_args = prototype->abi_args;
	Type **types = prototype->param_types;
	for (unsigned i = 0; i < 4; i++)
	{
		Type *type = type_lowering(types[i]);
		BEValue value = { .value = panic_args[i], .type = type };
		llvm_emit_parameter(c, actual_args, &count, abi_args[i], &value, type);
	}

	if (panicf)
	{
		unsigned elements = vec_size(varargs);
		Type *any_slice = type_get_slice(type_any);
		Type *any_array = type_get_array(type_any, elements);
		LLVMTypeRef llvm_array_type = llvm_get_type(c, any_array);
		AlignSize alignment = type_alloca_alignment(any_array);
		LLVMValueRef array_ref = llvm_emit_alloca(c, llvm_array_type, alignment, varargslots_name);
		unsigned vacount = vec_size(varargs);
		for (unsigned i = 0; i < vacount; i++)
		{
			AlignSize store_alignment;
			LLVMValueRef slot = llvm_emit_array_gep_raw(c,
			                                            array_ref,
			                                            llvm_array_type,
			                                            i,
			                                            alignment,
			                                            &store_alignment);
			llvm_store_to_ptr_aligned(c, slot, &varargs[i], store_alignment);
		}
		BEValue value;
		llvm_value_aggregate_two(c, &value, any_slice, array_ref, llvm_const_int(c, type_usz, elements));
		LLVMSetValueName2(value.value, temp_name, 6);

		llvm_emit_parameter(c, actual_args, &count, abi_args[4], &value, any_slice);

		BEValue res;
		if (c->debug.builder) llvm_emit_debug_location(c, loc);
		llvm_emit_raw_call(c, &res, prototype, llvm_func_type(c, prototype), llvm_get_ref(c, panicf), actual_args,
						   count, 0, NULL, false, NULL);
		llvm_emit_unreachable(c);
		return;
	}

	BEValue val;
	llvm_value_set_decl(c, &val, panic_var);
	llvm_value_rvalue(c, &val);

	BEValue res;
	if (c->debug.builder) llvm_emit_debug_location(c, loc);
	llvm_emit_raw_call(c, &res, prototype, llvm_func_type(c, prototype), val.value, actual_args,
					   count, 0, NULL, false, NULL);
	llvm_emit_unreachable(c);
}

void llvm_emit_panic_if_true(GenContext *c, BEValue *value, const char *panic_name, SourceSpan loc, const char *fmt, BEValue *value_1,
							 BEValue *value_2)
{
	if (LLVMIsAConstantInt(value->value))
	{
		ASSERT0(!LLVMConstIntGetZExtValue(value->value) && "Unexpected bounds check failed.");
		return;
	}
	LLVMBasicBlockRef panic_block = llvm_basic_block_new(c, "panic");
	LLVMBasicBlockRef ok_block = llvm_basic_block_new(c, "checkok");
	value->value = llvm_emit_expect_false(c, value);
	llvm_emit_cond_br(c, value, panic_block, ok_block);

	llvm_emit_block(c, panic_block);
	vec_add(c->panic_blocks, panic_block);
	BEValue *values = NULL;
	if (value_1)
	{
		BEValue var = *value_1;
		llvm_emit_any_from_value(c, &var, var.type);
		vec_add(values, var);
		if (value_2)
		{
			var = *value_2;
			llvm_emit_any_from_value(c, &var, var.type);
			vec_add(values, var);
		}
	}
	llvm_emit_panic(c, panic_name, loc, fmt, values);
	llvm_emit_block(c, ok_block);
	EMIT_SPAN(c, loc);
}

void llvm_emit_panic_on_true(GenContext *c, LLVMValueRef value, const char *panic_name, SourceSpan loc,
							 const char *fmt, BEValue *value_1, BEValue *value_2)
{
	BEValue be_value;
	llvm_value_set(&be_value, value, type_bool);
	llvm_emit_panic_if_true(c, &be_value, panic_name, loc, fmt, value_1, value_2);
}


void llvm_emit_stmt(GenContext *c, Ast *ast)
{
	if (!c->current_block) return;
	if (ast->ast_kind != AST_COMPOUND_STMT) EMIT_LOC(c, ast);
	switch (ast->ast_kind)
	{
		case AST_POISONED:
		case AST_IF_CATCH_SWITCH_STMT:
		case AST_FOREACH_STMT:
		case AST_CONTRACT:
		case AST_ASM_STMT:
		case AST_ASM_LABEL:
		case AST_CONTRACT_FAULT:
		case AST_CASE_STMT:
		case AST_DEFAULT_STMT:
		case CT_AST:
			UNREACHABLE
		case AST_EXPR_STMT:
			llvm_emit_expr_stmt(c, ast);
			break;
		case AST_DECLARE_STMT:
		{
			BEValue value;
			llvm_emit_local_decl(c, ast->declare_stmt, &value);
			break;
		}
		case AST_DECLS_STMT:
		{
			BEValue value;
			FOREACH(Decl *, decl, ast->decls_stmt)
			{
				if (!decl) continue;
				llvm_emit_local_decl(c, decl, &value);
			}
			break;
		}
		case AST_BREAK_STMT:
			llvm_emit_break(c, ast);
			break;
		case AST_CONTINUE_STMT:
			llvm_emit_continue(c, ast);
			break;
		case AST_IF_STMT:
			llvm_emit_if_stmt(c, ast);
			break;
		case AST_RETURN_STMT:
			llvm_emit_return(c, ast);
			break;
		case AST_BLOCK_EXIT_STMT:
			llvm_emit_block_exit_return(c, ast);
			break;
		case AST_COMPOUND_STMT:
			llvm_emit_compound_stmt(c, ast);
			break;
		case AST_FOR_STMT:
			llvm_emit_for_stmt(c, ast);
			break;
		case AST_NEXTCASE_STMT:
			gencontext_emit_next_stmt(c, ast);
			break;
		case AST_DEFER_STMT:
			if (llvm_use_debug(c)) ast->defer_stmt.scope = c->debug.block_stack;
			break;
		case AST_NOP_STMT:
			break;
		case AST_ASM_BLOCK_STMT:
			llvm_emit_asm_block_stmt(c, ast);
			break;
		case AST_ASSERT_STMT:
			llvm_emit_assert_stmt(c, ast);
			break;
		case AST_SWITCH_STMT:
			llvm_emit_switch(c, ast);
			break;
	}
}

