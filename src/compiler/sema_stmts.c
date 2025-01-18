// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"

static inline bool sema_analyse_asm_stmt(SemaContext *context, Ast *stmt);
static inline bool sema_analyse_assert_stmt(SemaContext *context, Ast *statement);
static inline bool sema_analyse_break_stmt(SemaContext *context, Ast *statement);
static inline bool sema_analyse_compound_stmt(SemaContext *context, Ast *statement);
static inline bool sema_analyse_continue_stmt(SemaContext *context, Ast *statement);
static inline bool sema_analyse_ct_for_stmt(SemaContext *context, Ast *statement);
static inline bool sema_analyse_ct_foreach_stmt(SemaContext *context, Ast *statement);
static inline bool sema_analyse_ct_if_stmt(SemaContext *context, Ast *statement);
static inline bool sema_analyse_ct_switch_stmt(SemaContext *context, Ast *statement);
static inline bool sema_analyse_declare_stmt(SemaContext *context, Ast *statement);
static inline bool sema_analyse_defer_stmt(SemaContext *context, Ast *statement);
static inline bool sema_analyse_expr_stmt(SemaContext *context, Ast *statement);
static inline bool sema_analyse_for_stmt(SemaContext *context, Ast *statement);
static inline bool sema_analyse_foreach_stmt(SemaContext *context, Ast *statement);
static inline bool sema_analyse_if_stmt(SemaContext *context, Ast *statement);
static inline bool sema_analyse_nextcase_stmt(SemaContext *context, Ast *statement);
static inline bool sema_analyse_return_stmt(SemaContext *context, Ast *statement);
static inline bool sema_analyse_switch_stmt(SemaContext *context, Ast *statement);

static inline bool sema_check_return_matches_opt_returns(SemaContext *context, Expr *ret_expr);
static inline bool sema_defer_has_try_or_catch(AstId defer_top, AstId defer_bottom);
static inline bool sema_analyse_block_exit_stmt(SemaContext *context, Ast *statement);
static inline bool sema_analyse_defer_stmt_body(SemaContext *context, Ast *statement);
static inline bool sema_analyse_for_cond(SemaContext *context, ExprId *cond_ref, bool *infinite);
static inline bool assert_create_from_contract(SemaContext *context, Ast *directive, AstId **asserts, SourceSpan evaluation_location);
static bool sema_analyse_asm_string_stmt(SemaContext *context, Ast *stmt);
static void sema_unwrappable_from_catch_in_else(SemaContext *c, Expr *cond);
static inline bool sema_analyse_try_unwrap(SemaContext *context, Expr *expr);
static inline bool sema_analyse_try_unwrap_chain(SemaContext *context, Expr *expr, CondType cond_type, CondResult *result);
static void sema_remove_unwraps_from_try(SemaContext *c, Expr *cond);
static inline bool sema_analyse_last_cond(SemaContext *context, Expr *expr, CondType cond_type, CondResult *result);
static inline bool sema_analyse_cond_list(SemaContext *context, Expr *expr, CondType cond_type, CondResult *result);
static inline bool sema_analyse_cond(SemaContext *context, Expr *expr, CondType cond_type, CondResult *result);
static inline Decl *sema_analyse_label(SemaContext *context, Ast *stmt);
static bool context_labels_exist_in_scope(SemaContext *context);
static inline bool sema_analyse_then_overwrite(SemaContext *context, Ast *statement, AstId replacement);
static inline bool sema_analyse_catch_unwrap(SemaContext *context, Expr *expr);
static inline bool sema_analyse_compound_statement_no_scope(SemaContext *context, Ast *compound_statement);
static inline bool sema_check_type_case(SemaContext *context, Type *switch_type, Ast *case_stmt, Ast **cases, unsigned index);
static inline bool sema_check_value_case(SemaContext *context, Type *switch_type, Ast *case_stmt, Ast **cases,
                                         unsigned index,
                                         bool *if_chained, bool *max_ranged, int *actual_cases_ref);
static bool sema_analyse_switch_body(SemaContext *context, Ast *statement, SourceSpan expr_span, Type *switch_type, Ast **cases, ExprAnySwitch *any_switch, Decl *var_holder);

static inline bool sema_analyse_statement_inner(SemaContext *context, Ast *statement);
static bool sema_analyse_require(SemaContext *context, Ast *directive, AstId **asserts, SourceSpan source);
static bool sema_analyse_ensure(SemaContext *context, Ast *directive);
static bool sema_analyse_optional_returns(SemaContext *context, Ast *directive);

static inline bool sema_analyse_asm_label(SemaContext *context, AsmInlineBlock *block, Ast *label)
{
	const char *name = label->asm_label;
	FOREACH(Ast *, other, block->labels)
	{
		if (name == other->asm_label)
		{
			SEMA_ERROR(label, "Duplicate ASM label '%s'.", name);
			SEMA_NOTE(other, "The previous definition was here.");
			return false;
		}
	}
	vec_add(block->labels, label);
	return true;
}

static inline bool sema_analyse_asm_stmt(SemaContext *context, Ast *stmt)
{
	if (stmt->asm_block_stmt.is_string) return sema_analyse_asm_string_stmt(context, stmt);
	// Check for support
	if (!asm_is_supported(compiler.platform.arch))
	{
		RETURN_SEMA_ERROR(stmt, "This architecture does not support inline asm.");
	}
	// Init as needed.
	init_asm(&compiler.platform);
	AsmInlineBlock *block = stmt->asm_block_stmt.block;
	AstId ast_id = block->asm_stmt;
	scratch_buffer_clear();
	while (ast_id)
	{
		Ast *ast = astptr(ast_id);
		ast_id = ast->next;
		if (ast->ast_kind == AST_ASM_LABEL)
		{
			sema_analyse_asm_label(context, block, ast);
			continue;
		}
		if (!sema_analyse_asm(context, block, ast)) return false;
	}
	return true;
}

/**
 * assert(foo), assert(foo, message, ...)
 *
 * - assert(false) is a compile time error.
 */
static inline bool sema_analyse_assert_stmt(SemaContext *context, Ast *statement)
{
	Expr *expr = exprptr(statement->assert_stmt.expr);

	// Verify that the message is a string if it exists.
	Expr *message_expr = exprptrzero(statement->assert_stmt.message);
	if (message_expr)
	{
		if (!sema_analyse_ct_expr(context, message_expr)) return false;
		if (!expr_is_const_string(message_expr)) RETURN_SEMA_ERROR(message_expr, "Expected a constant string as the error message.");
		FOREACH(Expr *, e, statement->assert_stmt.args)
		{
			if (!sema_analyse_expr(context, e)) return false;
			if (IS_OPTIONAL(e)) RETURN_SEMA_ERROR(e, "Optionals cannot be used as assert arguments, use '?"
													 "?', '!' or '!!' to fix this.");
			if (type_is_void(e->type)) RETURN_SEMA_ERROR(e, "This expression is of type 'void', did you make a mistake?");
		}
	}

	CondResult result_no_resolve = COND_MISSING;
	if (expr->resolve_status == RESOLVE_DONE && expr_is_const_bool(expr))
	{
		result_no_resolve = expr->const_expr.b ? COND_TRUE : COND_FALSE;
	}

	// Check the conditional inside
	CondResult result = COND_MISSING;
	if (!sema_analyse_cond_expr(context, expr, &result)) return false;

	// If it's constant, we process it differently.
	switch (result)
	{
		case COND_TRUE:
			// It's true, then replace the statement with a nop.
			statement->ast_kind = AST_NOP_STMT;
			return true;
		case COND_FALSE:
			// Was this 'assert(false)'?
			if (result_no_resolve == COND_FALSE)
			{
				// If this is a test, then assert(false) is permitted.
				if (context->call_env.current_function && context->call_env.current_function->func_decl.attr_test)
				{
					context->active_scope.jump_end = true;
					return true;
				}
				// Otherwise, require unreachable.
				RETURN_SEMA_ERROR(expr, "Use 'unreachable' instead of 'assert(false)'.");
			}
			// Otherwise we print an error.
			if (!context->active_scope.jump_end && !context->active_scope.is_dead)
			{
				if (message_expr && sema_cast_const(message_expr) && vec_size(statement->assert_stmt.args))
				{
					RETURN_SEMA_ERROR(expr, "%.*s", EXPAND_EXPR_STRING(message_expr));
				}
				if (statement->assert_stmt.is_ensure) RETURN_SEMA_ERROR(expr, "Contract violated.");
				RETURN_SEMA_ERROR(expr, "This expression will always be 'false'.");
			}
			// Otherwise, continue, this is fine as it can't be reached.
			return true;
		case COND_MISSING:
			// If the assert isn't compile time resolvable, we keep the assert.
			return true;
	}
	UNREACHABLE
}

/**
 * break and break LABEL;
 */
static inline bool sema_analyse_break_stmt(SemaContext *context, Ast *statement)
{
	// If there is no break target and there is no label,
	// we skip.
	if (!context->break_target && !statement->contbreak_stmt.is_label)
	{
		if (context_labels_exist_in_scope(context))
		{
			RETURN_SEMA_ERROR(statement, "Unlabelled 'break' is not allowed here.");
		}
		RETURN_SEMA_ERROR(statement, "There is no valid target for 'break', did you make a mistake?");
	}

	// Is jump, and set it as resolved.
	context->active_scope.jump_end = true;
	statement->contbreak_stmt.is_resolved = true;

	AstId defer_begin;
	Ast *parent;

	if (statement->contbreak_stmt.label.name)
	{
		// If we have a label, pick it and set the parent astid to that target.
		ASSIGN_DECL_OR_RET(Decl *target, sema_analyse_label(context, statement), false);
		// We don't need to do any checking since all(!) label constructs support break.
		parent = astptr(target->label.parent);
		defer_begin = target->label.defer;
	}
	else
	{
		// Jump to the default break target.
		parent = context->break_target;
		defer_begin = context->break_defer;
	}

	ASSERT0(parent);
	parent->flow.has_break = true;
	statement->contbreak_stmt.ast = astid(parent);

	// Append the defers.
	statement->contbreak_stmt.defers = context_get_defers(context, context->active_scope.defer_last, defer_begin, true);
	return true;
}

/**
 * The regular { }
 */
static inline bool sema_analyse_compound_stmt(SemaContext *context, Ast *statement)
{
	bool success;
	bool ends_with_jump;
	SCOPE_START
		success = sema_analyse_compound_statement_no_scope(context, statement);
		ends_with_jump = context->active_scope.jump_end;
	SCOPE_END;
	// If this ends with a jump, then we know we don't need to certain analysis.
	context->active_scope.jump_end = ends_with_jump;
	return success;
}

/**
 * continue and continue FOO;
 */
static inline bool sema_analyse_continue_stmt(SemaContext *context, Ast *statement)
{
	// If we have a plain continue and no continue label, we just failed.
	if (!context->continue_target && !statement->contbreak_stmt.label.name)
	{
		RETURN_SEMA_ERROR(statement, "'continue' is not allowed here.");
	}

	AstId defer_id;
	Ast *parent;
	if (statement->contbreak_stmt.label.name)
	{
		// If we have a label grab it.
		ASSIGN_DECL_OR_RET(Decl *target, sema_analyse_label(context, statement), false);
		defer_id = target->label.defer;
		parent = astptr(target->label.parent);

		// Continue can only be used with "for" statements, skipping the "do {  };" statement
		if (!ast_supports_continue(parent))
		{
			RETURN_SEMA_ERROR(statement, "'continue' may only be used with 'for', 'while' and 'do-while' statements.");
		}
	}
	else
	{
		// Use default defer and ast.
		defer_id = context->continue_defer;
		parent = context->continue_target;
	}

	// This makes the active scope jump.
	context->active_scope.jump_end = true;

	// Link the parent and add the defers.
	statement->contbreak_stmt.ast = astid(parent);
	statement->contbreak_stmt.is_resolved = true;
	statement->contbreak_stmt.defers = context_get_defers(context, context->active_scope.defer_last, defer_id, true);
	return true;
}


static inline Expr *sema_dive_into_expression(Expr *expr)
{
	// Dive into any cast, because it might have been cast into boolean.
	while (true)
	{
		switch (expr->expr_kind)
		{
			case EXPR_RVALUE:
			case EXPR_RECAST:
				expr = expr->inner_expr;
				continue;
			case EXPR_MAKE_SLICE:
				expr = expr->make_slice_expr.ptr;
				continue;
			case EXPR_MAKE_ANY:
				expr = expr->make_any_expr.inner;
				continue;
			case EXPR_INT_TO_BOOL:
				expr = expr->int_to_bool_expr.inner;
				continue;
			case EXPR_CAST:
				expr = exprptr(expr->cast_expr.expr);
				continue;
			default:
				return expr;
		}
	}

}
/**
 * If we have "if (catch x)", then we want to unwrap x in the else clause.
 **/
static void sema_unwrappable_from_catch_in_else(SemaContext *c, Expr *cond)
{
	ASSERT0(cond->expr_kind == EXPR_COND && "Assumed cond");

	Expr *last = VECLAST(cond->cond_expr);
	ASSERT0(last);
	last = sema_dive_into_expression(last);

	// Skip any non-unwraps
	if (last->expr_kind != EXPR_CATCH_UNWRAP) return;

	// If we have "if (catch x)" then this will unwrap x in the
	// else branch.
	FOREACH(Expr *, expr, last->catch_unwrap_expr.exprs)
	{
		if (expr->expr_kind != EXPR_IDENTIFIER) continue;

		Decl *decl = expr->identifier_expr.decl;
		if (decl->decl_kind != DECL_VAR) continue;
		ASSERT0(decl->type->type_kind == TYPE_OPTIONAL && "The variable should always be optional at this point.");

		// Note that we could possibly have "if (catch x, x)" and in this case we'd
		// unwrap twice, but that isn't really a problem.

		// 5. Locals and globals may be unwrapped
		switch (decl->var.kind)
		{
			case VARDECL_LOCAL:
			case VARDECL_GLOBAL:
				sema_unwrap_var(c, decl);
				break;
			default:
				continue;
		}
	}

}

// --- Sema analyse stmts

/**
 * Turn a "require" or "ensure" into a contract in the callee.
 */
static inline bool assert_create_from_contract(SemaContext *context, Ast *directive, AstId **asserts, SourceSpan evaluation_location)
{
	directive = copy_ast_single(directive);
	Expr *declexpr = directive->contract_stmt.contract.decl_exprs;
	ASSERT0(declexpr->expr_kind == EXPR_EXPRESSION_LIST);

	FOREACH(Expr *, expr, declexpr->expression_list)
	{
		if (expr->expr_kind == EXPR_DECL) RETURN_SEMA_ERROR(expr, "Only expressions are allowed in contracts.");
		CondResult result = COND_MISSING;
		if (!sema_analyse_cond_expr(context, expr, &result)) return false;

		const char *comment = directive->contract_stmt.contract.comment;
		if (!comment) comment = directive->contract_stmt.contract.expr_string;
		switch (result)
		{
			case COND_TRUE:
				continue;
			case COND_FALSE:
				sema_error_at(context, evaluation_location.a ? evaluation_location : expr->span, "%s", comment);
				return false;
			case COND_MISSING:
				break;
		}
		Ast *assert = new_ast(AST_ASSERT_STMT, expr->span);
		assert->assert_stmt.is_ensure = true;
		assert->assert_stmt.expr = exprid(expr);
		Expr *comment_expr = expr_new_const_string(expr->span, comment);
		assert->assert_stmt.message = exprid(comment_expr);
		ast_append(asserts, assert);
	}
	return true;
}

// Check whether a defer chain contains a try or a catch.
static inline bool sema_defer_has_try_or_catch(AstId defer_top, AstId defer_bottom)
{
	AstId first = 0;
	while (defer_bottom != defer_top)
	{
		Ast *defer = astptr(defer_top);
		if (defer->defer_stmt.is_catch || defer->defer_stmt.is_try) return true;
		defer_top = defer->defer_stmt.prev_defer;
	}
	return false;
}

// Print defers at return (from macro/block or from function)
static inline void sema_inline_return_defers(SemaContext *context, Ast *stmt, AstId defer_top, AstId defer_bottom)
{
	// Store the cleanup defers, which will happen on try.
	stmt->return_stmt.cleanup = context_get_defers(context, defer_top, defer_bottom, true);

	// If we have an optional return, then we create a cleanup_fail
	if (stmt->return_stmt.expr && IS_OPTIONAL(stmt->return_stmt.expr)
		&& sema_defer_has_try_or_catch(context->active_scope.defer_last, context->block_return_defer))
	{
		stmt->return_stmt.cleanup_fail = context_get_defers(context, context->active_scope.defer_last, context->block_return_defer, false);
		return;
	}
	// Otherwise we make the cleanup fail be the same as the cleanup.
	stmt->return_stmt.cleanup_fail = stmt->return_stmt.cleanup ? astid(copy_ast_defer(astptr(stmt->return_stmt.cleanup))) : 0;
}

/**
 * Check that an optional returned actually matches the "returns!" declared
 * by the contract.
 */
static inline bool sema_check_return_matches_opt_returns(SemaContext *context, Expr *ret_expr)
{
	if (!IS_OPTIONAL(ret_expr) || !context->call_env.opt_returns) return true;

	// TODO if this is a call, then we should check against
	// the "return!" in that call.
	// But for now we
	if (ret_expr->expr_kind != EXPR_OPTIONAL) return true;
	Expr *inner = ret_expr->inner_expr;
	if (!sema_cast_const(inner)) return true;

	// Here we have a const optional return.
	ASSERT0(ret_expr->inner_expr->const_expr.const_kind == CONST_ERR);
	Decl *fault = ret_expr->inner_expr->const_expr.enum_err_val;

	// Check that we find it.
	FOREACH(Decl *, opt, context->call_env.opt_returns)
	{
		if (opt->decl_kind == DECL_FAULT)
		{
			if (fault->type->decl == opt) return true;
			continue;
		}
		if (opt == fault) return true;
	}
	// No match
	RETURN_SEMA_ERROR(ret_expr, "This value does not match declared optional returns, it needs to be declared with the other optional returns.");
}

static bool sema_analyse_macro_constant_ensures(SemaContext *context, Expr *ret_expr)
{
	ASSERT0(context->current_macro);
	// This is a per return check, so we don't do it if the return expression is missing,
	// or if it is optional, or – obviously - if there are no '@ensure'.
	if (!ret_expr || !context->macro_has_ensures || IS_OPTIONAL(ret_expr)) return true;

	// If the return expression can't be flattened to a constant value, then
	// we won't be able to do any constant ensure checks anyway, so skip.
	if (!sema_cast_const(ret_expr)) return true;

	AstId doc_directive = context->current_macro->func_decl.docs;
	// We store the old return_expr for retval
	Expr *return_expr_old = context->return_expr;
	// And set our new one.
	context->return_expr = ret_expr;
	bool success = true;
	SCOPE_START_WITH_FLAGS(SCOPE_ENSURE_MACRO);
		while (doc_directive)
		{
			Ast *directive = astptr(doc_directive);
			doc_directive = directive->next;
			if (directive->contract_stmt.kind != CONTRACT_ENSURE) continue;
			Expr *checks = copy_expr_single(directive->contract_stmt.contract.decl_exprs);
			ASSERT0(checks->expr_kind == EXPR_EXPRESSION_LIST);
			Expr **exprs = checks->expression_list;
			FOREACH(Expr *, expr, exprs)
			{
				if (expr->expr_kind == EXPR_DECL)
				{
					SEMA_ERROR(expr, "Only expressions are allowed.");
					success = false;
					goto END;
				}
				CondResult result = COND_MISSING;
				if (!sema_analyse_cond_expr(context, expr, &result))
				{
					success = false;
					goto END;
				}
				// Skipping non-const.
				if (result == COND_MISSING) continue;
				if (result == COND_TRUE) continue;
				const char *comment = directive->contract_stmt.contract.comment;
				if (!comment) comment = directive->contract_stmt.contract.expr_string;
				SEMA_ERROR(ret_expr, "%s", comment);
				success = false;
				goto END;
			}
		}
END:
	SCOPE_END;
	context->return_expr = return_expr_old;
	return success;
}
/**
 * Handle exit in a macro or in an expression block.
 * @param context
 * @param statement
 * @return
 */
static inline bool sema_analyse_block_exit_stmt(SemaContext *context, Ast *statement)
{
	bool is_macro = (context->active_scope.flags & SCOPE_MACRO) != 0;
	ASSERT0(context->active_scope.flags & (SCOPE_EXPR_BLOCK | SCOPE_MACRO));
	statement->ast_kind = AST_BLOCK_EXIT_STMT;
	context->active_scope.jump_end = true;
	Type *block_type = context->expected_block_type;
	Expr *ret_expr = statement->return_stmt.expr;
	if (ret_expr)
	{
		if (block_type)
		{
			if (!sema_analyse_expr_rhs(context, block_type, ret_expr, true, NULL, false)) return false;
		}
		else
		{
			if (!sema_analyse_expr(context, ret_expr)) return false;
		}
		if (is_macro && !sema_check_return_matches_opt_returns(context, ret_expr)) return false;

	}
	else
	{
		if (block_type && type_no_optional(block_type) != type_void)
		{
			SEMA_ERROR(statement, "Expected a return value of type %s here.", type_quoted_error_string(block_type));
			return false;
		}
	}
	statement->return_stmt.block_exit_ref = context->block_exit_ref;
	sema_inline_return_defers(context, statement, context->active_scope.defer_last, context->block_return_defer);

	if (is_macro && !sema_analyse_macro_constant_ensures(context, ret_expr)) return false;
	vec_add(context->returns, statement);
	return true;
}

/**
 * Prevent the common mistake of `return &a` where "a" is a local.
 * @return true if the check is ok (no such escape)
 */
INLINE bool sema_check_not_stack_variable_escape(SemaContext *context, Expr *expr)
{
	Expr *outer = expr;
	expr = sema_dive_into_expression(expr);
	// We only want && and &
	if (expr->expr_kind == EXPR_SUBSCRIPT_ADDR)
	{
		expr = exprptr(expr->subscript_expr.expr);
		goto CHECK_ACCESS;
	}
	if (expr->expr_kind != EXPR_UNARY) return true;
	if (expr->unary_expr.operator == UNARYOP_TADDR)
	{
		RETURN_SEMA_ERROR(outer, "A pointer to a temporary value will be invalid once the function returns. Try copying the value to the heap or the temp memory instead.");
	}
	if (expr->unary_expr.operator != UNARYOP_ADDR) return true;
	expr = expr->unary_expr.expr;
CHECK_ACCESS:
	while (expr->expr_kind == EXPR_ACCESS) expr = expr->access_expr.parent;
	if (expr->expr_kind != EXPR_IDENTIFIER) return true;
	Decl *decl = expr->identifier_expr.decl;
	if (decl->decl_kind != DECL_VAR) return true;
	switch (decl->var.kind)
	{
		case VARDECL_LOCAL:
			if (decl->var.is_static) return true;
			switch (type_flatten(decl->type)->type_kind)
			{
				case TYPE_POINTER:
				case TYPE_SLICE:
					// &foo[2] is fine if foo is a pointer or slice.
					return true;
				default:
					break;
			}
			FALLTHROUGH;
		case VARDECL_PARAM:
			break;
		default:
			return true;
	}
	SEMA_ERROR(outer, "A pointer to a local variable will be invalid once the function returns. "
					  "Allocate the data on the heap or temp memory to return a pointer.");
	return false;
}

/**
 * We have the following possibilities:
 *  1. return
 *  2. return <non void expr>
 *  3. return <void expr>
 *
 * If we are in a block or a macro expansion we need to handle it differently.
 *
 * @param context
 * @param statement
 * @return
 */
static inline bool sema_analyse_return_stmt(SemaContext *context, Ast *statement)
{
	if (context->active_scope.in_defer)
	{
		RETURN_SEMA_ERROR(statement, "Return is not allowed inside of a defer.");
	}

	// This might be a return in a function block or a macro which must be treated differently.
	if (context->active_scope.flags & (SCOPE_EXPR_BLOCK | SCOPE_MACRO))
	{
		return sema_analyse_block_exit_stmt(context, statement);
	}

	// 1. We mark that the current scope ends with a jump.
	context->active_scope.jump_end = true;

	Type *expected_rtype = context->rtype;
	ASSERT0(expected_rtype && "We should always have known type from a function return.");

	Expr *return_expr = statement->return_stmt.expr;
	if (return_expr)
	{
		if (!sema_analyse_expr_rhs(context, expected_rtype, return_expr, type_is_optional(expected_rtype), NULL, false)) return false;
		if (!sema_check_not_stack_variable_escape(context, return_expr)) return false;
		if (!sema_check_return_matches_opt_returns(context, return_expr)) return false;
	}
	else
	{
		if (type_no_optional(expected_rtype)->canonical != type_void)
		{
			SEMA_ERROR(statement, "Expected to return a result of type %s.", type_to_error_string(expected_rtype));
			return false;
		}
		statement->return_stmt.cleanup = context_get_defers(context, context->active_scope.defer_last, 0, true);
		return true;
	}

	// Process any ensures.
	sema_inline_return_defers(context, statement, context->active_scope.defer_last, 0);
	if (context->call_env.ensures)
	{
		// Never generate an expression.
		if (return_expr && return_expr->expr_kind == EXPR_OPTIONAL) goto SKIP_ENSURE;
		AstId first = 0;
		AstId *append_id = &first;
		// Creating an assign statement
		AstId doc_directive = context->call_env.current_function->func_decl.docs;
		context->return_expr = return_expr;
		while (doc_directive)
		{
			Ast *directive = astptr(doc_directive);
			if (directive->contract_stmt.kind == CONTRACT_ENSURE)
			{
				bool success;
				SCOPE_START_WITH_FLAGS(SCOPE_ENSURE);
					success = assert_create_from_contract(context, directive, &append_id, statement->span);
				SCOPE_END;
				if (!success) return false;
			}
			doc_directive = directive->next;
		}
		if (!first) goto SKIP_ENSURE;
		if (statement->return_stmt.cleanup)
		{
			Ast *last = ast_last(astptr(statement->return_stmt.cleanup));
			last->next = first;
		}
		else
		{
			statement->return_stmt.cleanup = first;
		}
	}
SKIP_ENSURE:;

	ASSERT0(type_no_optional(statement->return_stmt.expr->type)->canonical == type_no_optional(expected_rtype)->canonical);
	return true;
}

static inline bool sema_expr_valid_try_expression(Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_BITASSIGN:
		case EXPR_CATCH_UNWRAP:
		case EXPR_COND:
		case EXPR_POISONED:
		case EXPR_CT_AND_OR:
		case EXPR_CT_CONCAT:
		case EXPR_CT_ARG:
		case EXPR_CT_APPEND:
		case EXPR_CT_CALL:
		case EXPR_CT_CASTABLE:
		case EXPR_CT_IS_CONST:
		case EXPR_CT_DEFINED:
		case EXPR_CT_EVAL:
		case EXPR_CT_IDENT:
		case EXPR_NAMED_ARGUMENT:
			UNREACHABLE
		case EXPR_BINARY:
		case EXPR_POINTER_OFFSET:
		case EXPR_CAST:
		case EXPR_UNARY:
		case EXPR_POST_UNARY:
		case EXPR_TERNARY:
		case EXPR_LAST_FAULT:
		case EXPR_TYPECALL:
		case EXPR_MEMBER_GET:
		case EXPR_SPLAT:
		case EXPR_MAKE_ANY:
		case EXPR_DISCARD:
			return false;
		case EXPR_BITACCESS:
		case EXPR_BUILTIN:
		case EXPR_BUILTIN_ACCESS:
		case EXPR_CALL:
		case EXPR_COMPILER_CONST:
		case EXPR_COMPOUND_LITERAL:
		case EXPR_CONST:
		case EXPR_DECL:
		case EXPR_DESIGNATED_INITIALIZER_LIST:
		case EXPR_DESIGNATOR:
		case EXPR_EMBED:
		case EXPR_EXPRESSION_LIST:
		case EXPR_EXPR_BLOCK:
		case EXPR_MACRO_BLOCK:
		case EXPR_OPTIONAL:
		case EXPR_FORCE_UNWRAP:
		case EXPR_GENERIC_IDENT:
		case EXPR_HASH_IDENT:
		case EXPR_IDENTIFIER:
		case EXPR_INITIALIZER_LIST:
		case EXPR_LAMBDA:
		case EXPR_MACRO_BODY_EXPANSION:
		case EXPR_NOP:
		case EXPR_OPERATOR_CHARS:
		case EXPR_OTHER_CONTEXT:
		case EXPR_RETHROW:
		case EXPR_RETVAL:
		case EXPR_SLICE:
		case EXPR_SLICE_ASSIGN:
		case EXPR_SLICE_COPY:
		case EXPR_STRINGIFY:
		case EXPR_SUBSCRIPT:
		case EXPR_SWIZZLE:
		case EXPR_SUBSCRIPT_ADDR:
		case EXPR_SUBSCRIPT_ASSIGN:
		case EXPR_BENCHMARK_HOOK:
		case EXPR_TEST_HOOK:
		case EXPR_TRY_UNWRAP:
		case EXPR_TRY_UNWRAP_CHAIN:
		case EXPR_TYPEID:
		case EXPR_TYPEID_INFO:
		case EXPR_TYPEINFO:
		case EXPR_ANYSWITCH:
		case EXPR_VASPLAT:
		case EXPR_MACRO_BODY:
		case EXPR_ACCESS:
		case EXPR_ASM:
		case EXPR_DEFAULT_ARG:
		case EXPR_EXT_TRUNC:
		case EXPR_INT_TO_BOOL:
		case EXPR_VECTOR_TO_ARRAY:
		case EXPR_SLICE_TO_VEC_ARRAY:
		case EXPR_MAKE_SLICE:
		case EXPR_SCALAR_TO_VECTOR:
		case EXPR_PTR_ACCESS:
		case EXPR_FLOAT_TO_INT:
		case EXPR_INT_TO_FLOAT:
		case EXPR_INT_TO_PTR:
		case EXPR_PTR_TO_INT:
		case EXPR_SLICE_LEN:
		case EXPR_ANYFAULT_TO_FAULT:
		case EXPR_VECTOR_FROM_ARRAY:
		case EXPR_RVALUE:
		case EXPR_RECAST:
		case EXPR_ADDR_CONVERSION:
		case EXPR_ENUM_FROM_ORD:
			return true;
	}
	UNREACHABLE
}
static inline bool sema_analyse_try_unwrap(SemaContext *context, Expr *expr)
{
	ASSERT0(expr->expr_kind == EXPR_TRY_UNWRAP);
	Expr *ident = expr->try_unwrap_expr.variable;
	Expr *optional = expr->try_unwrap_expr.init;

	// Case A. Unwrapping a single variable.
	if (!optional)
	{
		if (!sema_analyse_expr(context, ident)) return false;
		// The `try foo()` case.
		if (ident->expr_kind != EXPR_IDENTIFIER)
		{
			if (!sema_expr_valid_try_expression(ident))
			{
				RETURN_SEMA_ERROR(ident, "You need to assign this expression to something in order to use it with 'if (try ...)'.");
			}
			expr->try_unwrap_expr.optional = ident;
			expr->try_unwrap_expr.lhs = NULL;
			expr->try_unwrap_expr.assign_existing = true;
			expr->resolve_status = RESOLVE_DONE;
			expr->type = type_bool;
			return true;
		}
		Decl *decl = ident->identifier_expr.decl;
		if (decl->decl_kind != DECL_VAR)
		{
			RETURN_SEMA_ERROR(ident, "Expected this to be the name of an optional variable, but it isn't. Did you mistype?");
		}
		if (!IS_OPTIONAL(decl))
		{
			if (decl->var.kind == VARDECL_UNWRAPPED)
			{
				SEMA_ERROR(ident, "This variable is already unwrapped, so you cannot use 'try' on it again, please remove the 'try'.");
				return false;
			}
			SEMA_ERROR(ident, "Expected this variable to be an optional, otherwise it can't be used for unwrap, maybe you didn't intend to use 'try'?");
			return false;
		}
		expr->try_unwrap_expr.decl = decl;
		expr->type = type_bool;
		sema_unwrap_var(context, decl);
		expr->resolve_status = RESOLVE_DONE;
		return true;
	}

	// Case B. We are unwrapping to a variable that may or may not exist.
	TypeInfo *var_type = expr->try_unwrap_expr.type;

	// 2. If we have a type for the variable, resolve it.
	if (var_type)
	{
		if (!sema_resolve_type_info(context, var_type, RESOLVE_TYPE_DEFAULT)) return false;
		if (IS_OPTIONAL(var_type))
		{
			RETURN_SEMA_ERROR(var_type, "Only non-optional types may be used as types for 'try', please remove the '!'.");
		}
	}

	// 3. We are creating a new variable

	// 3a. If we had a variable type, then our expression must be an identifier.
	if (ident->expr_kind != EXPR_IDENTIFIER) RETURN_SEMA_ERROR(ident, "A variable name was expected here.");
	ASSERT0(ident->resolve_status != RESOLVE_DONE);
	if (ident->identifier_expr.path) RETURN_SEMA_ERROR(ident->identifier_expr.path, "The variable may not have a path.");
	if (ident->identifier_expr.is_const) RETURN_SEMA_ERROR(ident, "Expected a variable starting with a lower case letter.");
	const char *ident_name = ident->identifier_expr.ident;

	// Special check for `if (try a = a)`
	if (optional->expr_kind == EXPR_IDENTIFIER && optional->resolve_status == RESOLVE_NOT_DONE
		&& !optional->identifier_expr.path && optional->identifier_expr.ident == ident_name)
	{
		RETURN_SEMA_ERROR(ident, "If you want to unwrap the same variable, use 'if (try %s)' { ... } instead.", ident_name);
	}

	// 3b. Evaluate the expression
	if (!sema_analyse_expr(context, optional)) return false;

	if (!IS_OPTIONAL(optional))
	{
		RETURN_SEMA_ERROR(optional, "Expected an optional expression to 'try' here. If it isn't an optional, remove 'try'.");
		return false;
	}

	if (var_type)
	{
		if (!cast_implicit(context, optional, var_type->type, false)) return false;
	}

	// 4c. Create a type_info if needed.
	if (!var_type)
	{
		var_type = type_info_new_base(optional->type->optional, optional->span);
	}

	// 4d. A new declaration is created.
	Decl *decl = decl_new_var(ident->identifier_expr.ident, ident->span, var_type, VARDECL_LOCAL);

	// 4e. Analyse it
	if (!sema_analyse_var_decl(context, decl, true)) return false;

	expr->try_unwrap_expr.decl = decl;

	expr->try_unwrap_expr.optional = optional;
	expr->type = type_bool;
	expr->resolve_status = RESOLVE_DONE;
	return true;
}


static inline bool sema_analyse_try_unwrap_chain(SemaContext *context, Expr *expr, CondType cond_type, CondResult *result)
{
	ASSERT0(cond_type == COND_TYPE_UNWRAP_BOOL || cond_type == COND_TYPE_UNWRAP);

	ASSERT0(expr->expr_kind == EXPR_TRY_UNWRAP_CHAIN);

	FOREACH(Expr *, chain_element, expr->try_unwrap_chain_expr)
	{
		if (chain_element->expr_kind == EXPR_TRY_UNWRAP)
		{
			if (!sema_analyse_try_unwrap(context, chain_element)) return false;
			continue;
		}
		bool old_is_fail = *result == COND_FALSE;
		if (!sema_analyse_cond_expr(context, chain_element, result)) return false;
		if (old_is_fail) *result = COND_FALSE;
	}
	expr->type = type_bool;
	expr->resolve_status = RESOLVE_DONE;
	return true;
}
static inline bool sema_analyse_catch_unwrap(SemaContext *context, Expr *expr)
{
	Expr *ident = expr->catch_unwrap_expr.variable;

	TypeInfo *type = expr->catch_unwrap_expr.type;

	if (!type && !ident)
	{
		expr->catch_unwrap_expr.lhs = NULL;
		expr->catch_unwrap_expr.decl = NULL;
		goto RESOLVE_EXPRS;
	}
	type = type ? type : type_info_new_base(type_anyfault, expr->span);

	if (!sema_resolve_type_info(context, type, RESOLVE_TYPE_DEFAULT)) return false;

	if (type->type->canonical != type_anyfault)
	{
		RETURN_SEMA_ERROR(type, "Expected the type to be %s, not %s.", type_quoted_error_string(type_anyfault),
		                  type_quoted_error_string(type->type));
	}
	if (ident->expr_kind != EXPR_IDENTIFIER)
	{
		RETURN_SEMA_ERROR(ident, "A variable name was expected here.");
	}

	ASSERT0(ident->resolve_status != RESOLVE_DONE);
	if (ident->identifier_expr.path) RETURN_SEMA_ERROR(ident->identifier_expr.path, "The variable may not have a path.");
	if (ident->identifier_expr.is_const) RETURN_SEMA_ERROR(ident, "Expected a variable starting with a lower case letter.");

	// 4d. A new declaration is created.
	Decl *decl = decl_new_var(ident->identifier_expr.ident, ident->span, type, VARDECL_LOCAL);
	decl->var.no_init = true;

	// 4e. Analyse it
	if (!sema_analyse_var_decl(context, decl, true)) return false;

	expr->catch_unwrap_expr.decl = decl;
	expr->catch_unwrap_expr.lhs = NULL;

RESOLVE_EXPRS:;
	FOREACH(Expr *, fail, expr->catch_unwrap_expr.exprs)
	{
		if (!sema_analyse_expr(context, fail)) return false;
		if (!type_is_optional(fail->type))
		{
			RETURN_SEMA_ERROR(fail, "This expression is not optional, did you add it by mistake?");
		}
	}
	expr->type = type_anyfault;
	expr->resolve_status = RESOLVE_DONE;
	return true;
}

static void sema_remove_unwraps_from_try(SemaContext *c, Expr *cond)
{
	ASSERT0(cond->expr_kind == EXPR_COND);
	Expr *last = VECLAST(cond->cond_expr);
	if (!last || last->expr_kind != EXPR_TRY_UNWRAP_CHAIN) return;
	FOREACH(Expr *, expr, last->try_unwrap_chain_expr)
	{
		if (expr->expr_kind != EXPR_TRY_UNWRAP) continue;
		if (expr->try_unwrap_expr.assign_existing) continue;
		if (expr->try_unwrap_expr.optional)
		{
			sema_erase_var(c, expr->try_unwrap_expr.decl);
		}
		else
		{
			sema_erase_unwrapped(c, expr->try_unwrap_expr.decl);
		}
	}
}

static inline bool sema_analyse_last_cond(SemaContext *context, Expr *expr, CondType cond_type, CondResult *result)
{
	switch (expr->expr_kind)
	{
		case EXPR_TRY_UNWRAP_CHAIN:
			if (cond_type != COND_TYPE_UNWRAP_BOOL && cond_type != COND_TYPE_UNWRAP)
			{
				SEMA_ERROR(expr, "Try unwrapping is only allowed inside of a 'while' or 'if' conditional.");
				return false;
			}
			return sema_analyse_try_unwrap_chain(context, expr, cond_type, result);
		case EXPR_CATCH_UNWRAP:
			if (cond_type != COND_TYPE_UNWRAP_BOOL && cond_type != COND_TYPE_UNWRAP)
			{
				RETURN_SEMA_ERROR(expr, "Catch unwrapping is only allowed inside of a 'while' or 'if' conditional, maybe '@catch(<expr>)' will do what you need?");
			}
			return sema_analyse_catch_unwrap(context, expr);
		default:
			break;
	}

	if (cond_type != COND_TYPE_EVALTYPE_VALUE) goto NORMAL_EXPR;

	// Now we're analysing the last expression in a switch.
	// Case 1: switch (var = variant_expr)
	if (expr->expr_kind == EXPR_BINARY && expr->binary_expr.operator == BINARYOP_ASSIGN)
	{
		// No variable on the lhs? Then it can't be an any unwrap.
		Expr *left = exprptr(expr->binary_expr.left);
		if (left->resolve_status ==  RESOLVE_DONE || left->expr_kind != EXPR_IDENTIFIER || left->identifier_expr.path) goto NORMAL_EXPR;

		// Does the identifier exist in the parent scope?
		// then again it can't be an any unwrap.
		BoolErr defined_in_scope = sema_symbol_is_defined_in_scope(context, left->identifier_expr.ident);
		if (defined_in_scope == BOOL_ERR) return false;
		if (defined_in_scope == BOOL_TRUE) goto NORMAL_EXPR;

		Expr *right = exprptr(expr->binary_expr.right);
		bool is_deref = right->expr_kind == EXPR_UNARY && right->unary_expr.operator == UNARYOP_DEREF;
		if (is_deref) right = right->unary_expr.expr;
		if (!sema_analyse_expr_rhs(context, NULL, right, false, NULL, false)) return false;
		Type *type = right->type->canonical;
		if (type == type_get_ptr(type_any) && is_deref)
		{
			is_deref = false;
			right = exprptr(expr->binary_expr.right);
			if (!sema_analyse_expr_rhs(context, NULL, right, false, NULL, false)) return false;
		}
		if (type != type_any) goto NORMAL_EXPR;
		// Found an expansion here
		expr->expr_kind = EXPR_ANYSWITCH;
		expr->any_switch.new_ident = left->identifier_expr.ident;
		expr->any_switch.span = left->span;
		expr->any_switch.any_expr = right;
		expr->any_switch.is_deref = is_deref;
		expr->any_switch.is_assign = true;
		expr->resolve_status = RESOLVE_DONE;
		expr->type = type_typeid;
		return true;
	}
	if (!sema_analyse_expr(context, expr)) return false;
	Type *type = expr->type->canonical;
	if (type != type_any) return true;
	if (expr->expr_kind == EXPR_IDENTIFIER)
	{
		Decl *decl = expr->identifier_expr.decl;
		expr->expr_kind = EXPR_ANYSWITCH;
		expr->any_switch.is_deref = false;
		expr->any_switch.is_assign = false;
		expr->any_switch.variable = decl;
		expr->type = type_typeid;
		expr->resolve_status = RESOLVE_DONE;
		return true;
	}
	return true;

NORMAL_EXPR:;
	return sema_analyse_expr(context, expr);
}
/**
 * An decl-expr-list is a list of a mixture of declarations and expressions.
 * The last declaration or expression is propagated. So for example:
 *
 *   int a = 3, b = 4, float c = 4.0
 *
 * In this case the final value is 4.0 and the type is float.
 */
static inline bool sema_analyse_cond_list(SemaContext *context, Expr *expr, CondType cond_type, CondResult *result)
{
	ASSERT0(expr->expr_kind == EXPR_COND);

	Expr **dexprs = expr->cond_expr;
	unsigned entries = vec_size(dexprs);

	// 1. Special case, there are no entries, so the type is void
	if (entries == 0)
	{
		expr->type = type_void;
		return true;
	}

	// 2. Walk through each of our declarations / expressions as if they were regular expressions.
	for (unsigned i = 0; i < entries - 1; i++)
	{
		if (!sema_analyse_expr(context, dexprs[i])) return false;
	}

	if (!sema_analyse_last_cond(context, dexprs[entries - 1], cond_type, result)) return false;

	expr->type = dexprs[entries - 1]->type;
	expr->resolve_status = RESOLVE_DONE;
	return true;
}


/**
 * Analyse a conditional expression:
 *
 *  1. The middle statement in a for loop
 *  2. The expression in an if, while
 *  3. The expression in a switch
 *
 * @param context the current context
 * @param expr the conditional to evaluate
 * @param cast_to_bool if the result is to be cast to bool after
 * @return true if it passes analysis.
 */
static inline bool sema_analyse_cond(SemaContext *context, Expr *expr, CondType cond_type, CondResult *result)
{
	bool cast_to_bool = cond_type == COND_TYPE_UNWRAP_BOOL;
	ASSERT0(expr->expr_kind == EXPR_COND && "Conditional expressions should always be of type EXPR_DECL_LIST");

	// 1. Analyse the declaration list.
	ScopeFlags current_flags = context->active_scope.flags;
	context->active_scope.flags |= SCOPE_COND;
	bool success = sema_analyse_cond_list(context, expr, cond_type, result);
	context->active_scope.flags = current_flags;
	if (!success) return false;

	// 2. If we get "void", either through a void call or an empty list,
	//    signal that.
	if (type_is_void(expr->type))
	{
		SEMA_ERROR(expr, cast_to_bool ? "Expected a boolean expression." : "Expected an expression resulting in a value.");
		return false;
	}

	// 3. We look at the last element (which is guaranteed to exist because
	//    the type was not void.
	Expr *last = VECLAST(expr->cond_expr);

	if (last->expr_kind == EXPR_DECL)
	{
		// 3c. The declaration case
		Decl *decl = last->decl_expr;
		Expr *init = decl->var.init_expr;
		// 3d. We expect an initialization for the last declaration.
		if (!init)
		{
			SEMA_ERROR(last, "Expected a declaration with initializer.");
			return false;
		}
		// 3e. Expect that it isn't an optional
		if (IS_OPTIONAL(init))
		{
			return sema_error_failed_cast(context, last, last->type, cast_to_bool ? type_bool : init->type);
		}
		if (cast_to_bool)
		{
			if (!may_cast(context, init, type_bool, true, true))
			{
				RETURN_SEMA_ERROR(last->decl_expr->var.init_expr, "The expression needs to be convertible to a boolean.");
			}
			cast_no_check(context, last, type_bool, false);
		}
		if (cast_to_bool && expr_is_const_bool(init))
		{
			*result = init->const_expr.b ? COND_TRUE : COND_FALSE;
		}

		return true;
	}

	// 3a. Check for optional in case of an expression.
	if (IS_OPTIONAL(last))
	{
		if (type_is_void(type_no_optional(last->type)) && cast_to_bool)
		{
			SEMA_ERROR(last, "Use '@ok(<expr>)' or '@catch(<expr>)' to explicitly convert a 'void!' to a boolean.");
			return false;
		}
		SEMA_ERROR(last, "The expression may not be an optional, but was %s.", type_quoted_error_string(last->type));
		return false;
	}
	// 3b. Cast to bool if that is needed
	if (cast_to_bool)
	{
		if (!cast_explicit(context, last, type_bool)) return false;
	}
	if (expr_is_const_bool(last))
	{
		*result = last->const_expr.b ? COND_TRUE : COND_FALSE;
	}
	return true;
}


static inline bool sema_analyse_decls_stmt(SemaContext *context, Ast *statement)
{
	bool should_nop = true;
	FOREACH_IDX(i, Decl *, decl, statement->decls_stmt)
	{
		VarDeclKind kind = decl->var.kind;
		if (kind == VARDECL_LOCAL_CT_TYPE || kind == VARDECL_LOCAL_CT)
		{
			if (!sema_analyse_var_decl_ct(context, decl)) return false;
			statement->decls_stmt[i] = NULL;
		}
		else
		{
			if (!sema_analyse_var_decl(context, decl, true)) return false;
			should_nop = false;
		}
	}
	if (should_nop) statement->ast_kind = AST_NOP_STMT;
	return true;
}

static inline bool sema_analyse_declare_stmt(SemaContext *context, Ast *statement)
{
	VarDeclKind kind = statement->declare_stmt->var.kind;
	bool erase = kind == VARDECL_LOCAL_CT_TYPE || kind == VARDECL_LOCAL_CT;
	if (!sema_analyse_var_decl(context, statement->declare_stmt, true)) return false;
	if (erase) statement->ast_kind = AST_NOP_STMT;
	return true;
}

static inline bool sema_analyse_expr_stmt(SemaContext *context, Ast *statement)
{
	Expr *expr = statement->expr_stmt;
	if (!sema_analyse_expr(context, expr)) return false;
	if (!sema_expr_check_discard(context, expr)) return false;
	switch (expr->expr_kind)
	{
		case EXPR_RETHROW:
			if (expr->rethrow_expr.inner->expr_kind == EXPR_OPTIONAL)
			{
				context->active_scope.jump_end = true;
			}
			break;
		case EXPR_FORCE_UNWRAP:
			if (expr->inner_expr->expr_kind == EXPR_OPTIONAL)
			{
				context->active_scope.jump_end = true;
			}
			break;
		case EXPR_POST_UNARY:
			if (expr->rethrow_expr.inner->expr_kind == EXPR_OPTIONAL)
			{
				context->active_scope.jump_end = true;
			}
			break;
		case EXPR_CALL:
			if (expr->call_expr.no_return) context->active_scope.jump_end = true;
			break;
		case EXPR_MACRO_BLOCK:
			if (expr->macro_block.is_noreturn) context->active_scope.jump_end = true;
			break;
		case EXPR_CONST:
			// Remove all const statements.
			statement->ast_kind = AST_NOP_STMT;
			break;
		default:
			break;
	}
	return true;
}

bool sema_analyse_defer_stmt_body(SemaContext *context, Ast *statement)
{
	Ast *body = astptr(statement->defer_stmt.body);
	if (body->ast_kind == AST_DEFER_STMT)
	{
		RETURN_SEMA_ERROR(body, "A defer may not have a body consisting of a raw 'defer', this looks like a mistake.");
	}
	if (body->ast_kind != AST_COMPOUND_STMT)
	{
		Ast *new_body = new_ast(AST_COMPOUND_STMT, body->span);
		new_body->compound_stmt.first_stmt = astid(body);
		body = new_body;
		statement->defer_stmt.body = astid(body);
	}
	body->compound_stmt.parent_defer = astid(statement);
	bool success = true;
	SCOPE_START

	context->active_scope.defer_last = 0;
	context->active_scope.defer_start = 0;
	context->active_scope.in_defer = statement;

	PUSH_BREAKCONT(NULL);
	PUSH_NEXT(NULL, NULL);

	// Only ones allowed.
	context->active_scope.flags = 0;

	success = sema_analyse_statement(context, body);

	POP_BREAKCONT();
	POP_NEXT();

	// We should never need to replace any defers here.

	SCOPE_END;

	return success;

}
static inline bool sema_analyse_defer_stmt(SemaContext *context, Ast *statement)
{

	if (!sema_analyse_defer_stmt_body(context, statement)) return false;

	statement->defer_stmt.prev_defer = context->active_scope.defer_last;
	context->active_scope.defer_last = astid(statement);

	return true;
}

static inline bool sema_analyse_for_cond(SemaContext *context, ExprId *cond_ref, bool *infinite)
{
	ExprId cond_id = *cond_ref;
	if (!cond_id)
	{
		*infinite = true;
		return true;
	}
	Expr *cond = exprptr(cond_id);
	CondResult result = COND_MISSING;
	if (cond->expr_kind == EXPR_COND)
	{
		if (!sema_analyse_cond(context, cond, COND_TYPE_UNWRAP_BOOL, &result)) return false;
	}
	else
	{
		if (!sema_analyse_cond_expr(context, cond, &result)) return false;
	}

	// If this is const true, then set this to infinite and remove the expression.
	if (result == COND_TRUE)
	{
		if (cond->expr_kind != EXPR_COND || vec_size(cond->cond_expr) == 1)
		{
			cond = NULL;
		}
		*infinite = true;
	}
	else
	{
		*infinite = false;
	}
	*cond_ref = cond ? exprid(cond) : 0;
	return true;
}
static inline bool sema_analyse_for_stmt(SemaContext *context, Ast *statement)
{
	bool success = true;
	bool is_infinite = false;

	Ast *body = astptr(statement->for_stmt.body);
	ASSERT0(body);
	if (body->ast_kind == AST_DEFER_STMT)
	{
		RETURN_SEMA_ERROR(body, "Looping over a raw 'defer' is not allowed, was this a mistake?");
	}
	bool do_loop = statement->for_stmt.flow.skip_first;
	if (body->ast_kind != AST_COMPOUND_STMT && do_loop)
	{
		RETURN_SEMA_ERROR(body, "A do loop must use { } around its body.");
	}
	// Enter for scope
	SCOPE_OUTER_START

		if (statement->for_stmt.init)
		{
			success = sema_analyse_expr(context, exprptr(statement->for_stmt.init));
		}

		// Conditional scope start
		SCOPE_START_WITH_LABEL(statement->for_stmt.flow.label)

			if (!do_loop)
			{
				if (!sema_analyse_for_cond(context, &statement->for_stmt.cond, &is_infinite) || !success)
				{
					SCOPE_ERROR_END_OUTER();
					return false;
				}
			}


			PUSH_BREAKCONT(statement);
				success = sema_analyse_statement(context, body);
				statement->for_stmt.flow.no_exit = context->active_scope.jump_end;
			POP_BREAKCONT();

			// End for body scope
			context_pop_defers_and_replace_ast(context, body);

		SCOPE_END;

		if (statement->for_stmt.flow.skip_first)
		{
			SCOPE_START
				if (!sema_analyse_for_cond(context, &statement->for_stmt.cond, &is_infinite) || !success)
				{
					SCOPE_ERROR_END_OUTER();
					return false;
				}
			SCOPE_END;
			// Rewrite do { } while(true) to while(true) { }
			if (is_infinite)
			{
				ASSERT0(!statement->for_stmt.cond);
				statement->for_stmt.flow.skip_first = false;
			}
		}

		if (success && statement->for_stmt.incr)
		{
			// Incr scope start
			SCOPE_START
				success = sema_analyse_expr(context, exprptr(statement->for_stmt.incr));
				// Incr scope end
			SCOPE_END;
		}


		// End for body scope
		context_pop_defers_and_replace_ast(context, statement);

	SCOPE_OUTER_END;

	if (is_infinite && !statement->for_stmt.flow.has_break)
	{
		context->active_scope.jump_end = true;
	}
	return success;
}

/**
 * foreach_stmt ::= foreach
 * @param context
 * @param statement
 * @return
 */
static inline bool sema_analyse_foreach_stmt(SemaContext *context, Ast *statement)
{
	// Pull out the relevant data.
	Decl *var = declptr(statement->foreach_stmt.variable);
	Decl *index = declptrzero(statement->foreach_stmt.index);
	Expr *enumerator = exprptr(statement->foreach_stmt.enumeration);
	AstId body = statement->foreach_stmt.body;
	AstId first_stmt = 0;
	AstId *succ = &first_stmt;
	Expr **expressions = NULL;
	bool is_reverse = statement->foreach_stmt.is_reverse;
	bool value_by_ref = statement->foreach_stmt.value_by_ref;
	bool success = true;
	bool iterator_based = false;
	bool iterator_was_initializer = enumerator->expr_kind == EXPR_INITIALIZER_LIST;
	// Check the type if needed
	TypeInfo *variable_type_info = vartype(var);
	if (variable_type_info && !sema_resolve_type_info(context, variable_type_info, RESOLVE_TYPE_DEFAULT)) return false;

	// Conditional scope start
	SCOPE_START


		// In the case of foreach (int x : { 1, 2, 3 }) we will infer the int[] type, so pick out the number of elements.
		Type *inferred_type = variable_type_info ? type_get_inferred_array(type_no_optional(variable_type_info->type)) : NULL;

		if (variable_type_info)
		{
			if (!sema_analyse_inferred_expr(context, inferred_type, enumerator)) return SCOPE_POP_ERROR();
		}
		else
		{
			if (!sema_analyse_expr(context, enumerator)) return SCOPE_POP_ERROR();
		}
		// And pop the cond scope.
	SCOPE_END;

	if (IS_OPTIONAL(enumerator))
	{
		RETURN_SEMA_ERROR(enumerator, "The foreach iterable expression may not be optional.");
	}

	if (statement->foreach_stmt.index_by_ref)
	{
		ASSERT0(index);
		RETURN_SEMA_ERROR(index, "The index cannot be held by reference, did you accidentally add a '&'?");
	}

	// Insert a single deref as needed.
	Type *canonical = enumerator->type->canonical;
	if (canonical->type_kind == TYPE_UNTYPED_LIST)
	{
		if (variable_type_info || !iterator_was_initializer)
		{
			RETURN_SEMA_ERROR(enumerator, "It is not possible to enumerate a compile time 'untyped' list at runtime, but you can use the compile time `$foreach` with the list.");
		}
		else
		{
			RETURN_SEMA_ERROR(var, "Add an explicit type to the variable if you want to iterate over an initializer list.");
		}
	}
	if (canonical->type_kind == TYPE_POINTER)
	{
		// Something like Foo** will not be dereferenced, only Foo*
		if (canonical->pointer->type_kind == TYPE_POINTER)
		{
			RETURN_SEMA_ERROR(enumerator, "It is not possible to enumerate an expression of type %s.", type_quoted_error_string(enumerator->type));
		}
		expr_rewrite_insert_deref(enumerator);
	}

	// At this point we should have dereferenced any pointer or bailed.
	ASSERT0(!type_is_pointer(enumerator->type));

	// Check that we can even index this expression.

	Type *value_type = type_get_indexed_type(enumerator->type);
	if (canonical->type_kind == TYPE_DISTINCT && type_flatten(canonical)->type_kind == TYPE_POINTER)
	{
		value_type = NULL;
	}

	if (value_type && value_by_ref) value_type = type_get_ptr(value_type);

	Decl *len = NULL;
	Decl *index_macro = NULL;
	Type *index_type = type_usz;

	if (!value_type || canonical->type_kind == TYPE_DISTINCT)
	{
		len = sema_find_operator(context, enumerator->type, OVERLOAD_LEN);
		Decl *by_val = sema_find_operator(context, enumerator->type, OVERLOAD_ELEMENT_AT);
		Decl *by_ref = sema_find_operator(context, enumerator->type, OVERLOAD_ELEMENT_REF);
		if (!len || (!by_val && !by_ref))
		{
			if (value_type) goto SKIP_OVERLOAD;
			RETURN_SEMA_ERROR(enumerator, "It's not possible to enumerate an expression of type %s.", type_quoted_error_string(enumerator->type));
		}
		if (!by_ref && value_by_ref)
		{
			RETURN_SEMA_ERROR(enumerator, "%s does not support 'foreach' by reference, but you iterate by value.", type_quoted_error_string(enumerator->type));
		}
		if (!decl_ok(len) || !decl_ok(by_val) || !decl_ok(by_ref)) return false;
		index_macro = value_by_ref ? by_ref : by_val;
		ASSERT0(index_macro);
		index_type = index_macro->func_decl.signature.params[1]->type;
		if (!type_is_integer(index_type))
		{
			RETURN_SEMA_ERROR(enumerator, "Only integer indexed types may be used with foreach.");
		}
		TypeInfoId rtype = index_macro->func_decl.signature.rtype;
		value_type = rtype ? type_infoptr(rtype)->type : NULL;
	}

SKIP_OVERLOAD:;

	TypeInfo *type_info = vartype(var);
	// Set up the value, assigning the type as needed.
	// Element *value @noinit
	if (!type_info)
	{
		type_info = type_info_new_base(value_type, var->span);
		var->var.type_info = type_infoid(type_info);
	}
	if (!sema_resolve_type_info(context, type_info, RESOLVE_TYPE_DEFAULT)) return false;

	// Set up the optional index parameter
	Type *index_var_type = NULL;
	if (index)
	{
		TypeInfo *idx_type_info = vartype(index);
		if (!idx_type_info)
		{
			idx_type_info = type_info_new_base(index_type, enumerator->span);
			index->var.type_info = type_infoid(idx_type_info);
		}
		if (!sema_resolve_type_info(context, idx_type_info, RESOLVE_TYPE_DEFAULT)) return false;
		index_var_type = idx_type_info->type;
		if (type_is_optional(index_var_type))
		{
			RETURN_SEMA_ERROR(idx_type_info, "The index may not be an optional.");
		}
		if (!type_is_integer(type_flatten(index_var_type)))
		{
			RETURN_SEMA_ERROR(idx_type_info,
			                  "Index must be an integer type, '%s' is not valid.",
			                  type_to_error_string(index_var_type));
		}
	}


	// We either have "foreach (x : some_var)" or "foreach (x : some_call())"
	// So we grab the former by address (implicit &) and the latter as the value.
	ASSERT0(enumerator->resolve_status == RESOLVE_DONE);
	bool is_addr = false;
	bool is_variable = false;
	if (enumerator->expr_kind == EXPR_IDENTIFIER)
	{
		enumerator->identifier_expr.decl->var.is_written = true;
		is_variable = true;
	}
	else if (expr_may_addr(enumerator))
	{
		is_addr = true;
		expr_insert_addr(enumerator);
	}

	Decl *temp = NULL;
	if (is_variable)
	{
		temp = enumerator->identifier_expr.decl;
	}
	else
	{
		// Store either "Foo* __enum$ = &some_var;" or "Foo __enum$ = some_call()"
		temp = decl_new_generated_var(enumerator->type, VARDECL_LOCAL, enumerator->span);
		vec_add(expressions, expr_generate_decl(temp, enumerator));
	}

	// Create @__enum$.len() or @(*__enum$).len()
	Expr *enum_val = expr_variable(temp);
	enum_val->span = enumerator->span;
	if (is_addr) expr_rewrite_insert_deref(enum_val);
	Type *enumerator_type = type_flatten(enum_val->type);
	Expr *len_call;
	ArraySize array_len = 0;
	if (len)
	{
		len_call = expr_new(EXPR_CALL, enumerator->span);
		if (!sema_insert_method_call(context, len_call, len, enum_val, NULL)) return false;
	}
	else
	{
		switch (enumerator_type->type_kind)
		{
			case TYPE_ARRAY:
			case TYPE_VECTOR:
				array_len = enumerator_type->array.len;
				len_call = NULL;
				break;
			case TYPE_SLICE:
				if (!sema_analyse_expr(context, enum_val)) return false;
				len_call = expr_new_expr(EXPR_SLICE_LEN, enumerator);
				expr_rewrite_slice_len(len_call, enum_val, type_isz);
				break;
			default:
				UNREACHABLE
		}
	}
	bool is_single_pass = array_len == 1;
	if (is_single_pass)
	{
		is_reverse = false;
	}

	Decl *idx_decl = decl_new_generated_var(index_type, VARDECL_LOCAL, index ? index->span : enumerator->span);

	// IndexType __len$ = (IndexType)(@__enum$.len())
	Decl *len_decl = NULL;


	if (is_reverse)
	{
		if (!len_call)
		{
			// Create const len if missing.
			len_call = expr_new_const_int(enumerator->span, type_isz, array_len);
		}
		if (!cast_implicit(context, len_call, index_type, false)) return false;
		// __idx$ = (IndexType)(@__enum$.len()) (or const)
		vec_add(expressions, expr_generate_decl(idx_decl, len_call));
	}
	else
	{
		if (len_call)
		{
			len_decl = decl_new_generated_var(index_type, VARDECL_LOCAL, enumerator->span);
			if (!cast_implicit_silent(context, len_call, index_type, false))
			{
				SEMA_ERROR(enumerator,
				           "'foreach' is not supported, as the length %s cannot "
				           "be cast implicitly cast to %s - please update your definition.",
				           type_quoted_error_string(len_call->type), type_quoted_error_string(index_type));
				if (len)
				{
					SEMA_NOTE(len, "The definition of 'len()' is here.");
					decl_poison(len);
				}
				if (index_macro)
				{
					SEMA_NOTE(index_macro, "The index definition is here.");
					decl_poison(index_macro);
				}
				return false;
			}
			vec_add(expressions, expr_generate_decl(len_decl, len_call));
		}
		Expr *idx_init = expr_new_const_int(idx_decl->span, index_type, 0);
		vec_add(expressions, expr_generate_decl(idx_decl, idx_init));
	}

	// Add all declarations to the init
	Expr *init_expr = expr_new(EXPR_EXPRESSION_LIST, var->span);
	init_expr->expression_list = expressions;

	Expr *update = NULL;
	Expr *cond;
	if (is_reverse)
	{
		// Create __idx$ > 0
		cond = expr_new(EXPR_BINARY, idx_decl->span);
		cond->binary_expr.operator = BINARYOP_GT;
		cond->binary_expr.left = exprid(expr_variable(idx_decl));
		Expr *rhs = expr_new_const_int(enumerator->span, index_type, 0);
		cond->binary_expr.right = exprid(rhs);

		// Create --__idx$
		Expr *dec = expr_new(EXPR_UNARY, idx_decl->span);
		dec->unary_expr.expr = expr_variable(idx_decl);
		dec->unary_expr.operator = UNARYOP_DEC;
		dec->unary_expr.no_wrap = true;
		Ast *update_stmt = new_ast(AST_EXPR_STMT, idx_decl->span);
		update_stmt->expr_stmt = dec;
		ast_append(&succ, update_stmt);
	}
	else if (is_single_pass)
	{
		cond = expr_new_const_bool(idx_decl->span, type_bool, false);
	}
	else
	{
		// Create __idx$ < __len$
		cond = expr_new(EXPR_BINARY, idx_decl->span);
		cond->binary_expr.operator = BINARYOP_LT;
		cond->binary_expr.left = exprid(expr_variable(idx_decl));
		if (len_decl)
		{
			cond->binary_expr.right = exprid(expr_variable(len_decl));
		}
		else
		{
			Expr *rhs = expr_new_const_int(enumerator->span, type_isz, array_len);
			cond->binary_expr.right = exprid(rhs);
		}

		// Create ++__idx$
		update = expr_new(EXPR_UNARY, idx_decl->span);
		update->unary_expr.expr = expr_variable(idx_decl);
		update->unary_expr.operator = UNARYOP_INC;
		update->unary_expr.no_wrap = true;
	}

	// Create IndexType index = __idx$
	if (index)
	{
		Ast *declare_ast = new_ast(AST_DECLARE_STMT, var->span);
		declare_ast->declare_stmt = index;
		Expr *load_idx = expr_variable(idx_decl);
		if (!cast_explicit(context, load_idx, index_var_type)) return false;
		index->var.init_expr = load_idx;
		ast_append(&succ, declare_ast);
	}

	// Create value = (*__$enum)[__idx$]
	Ast *value_declare_ast = new_ast(AST_DECLARE_STMT, var->span);
	value_declare_ast->declare_stmt = var;

	Expr *subscript = expr_new(EXPR_SUBSCRIPT, var->span);
	enum_val = expr_variable(temp);
	enum_val->span = enumerator->span;
	if (is_addr) expr_rewrite_insert_deref(enum_val);
	subscript->subscript_expr.expr = exprid(enum_val);
	if (array_len == 1)
	{
		subscript->subscript_expr.index.expr = exprid(expr_new_const_int(var->span, idx_decl->type, 0));
	}
	else
	{
		subscript->subscript_expr.index.expr = exprid(expr_variable(idx_decl));
	}
	if (value_by_ref)
	{
		Expr *addr = expr_new(EXPR_UNARY, subscript->span);
		addr->unary_expr.operator = UNARYOP_ADDR;
		addr->unary_expr.expr = subscript;
		subscript = addr;
	}
	var->var.init_expr = subscript;
	ast_append(&succ, value_declare_ast);
	Ast *body_ast = astptr(body);
	ast_append(&succ, body_ast);
	Ast *compound_stmt = new_ast(AST_COMPOUND_STMT, body_ast->span);
	compound_stmt->compound_stmt.first_stmt = first_stmt;
	FlowCommon flow = statement->foreach_stmt.flow;
	flow.skip_first = is_single_pass;
	statement->for_stmt = (AstForStmt){ .init = exprid(init_expr),
										.cond = exprid(cond),
										.incr = update ? exprid(update) : 0,
										.flow = flow,
										.body = astid(compound_stmt),
	};
	statement->ast_kind = AST_FOR_STMT;
	return sema_analyse_for_stmt(context, statement);

}

static inline bool sema_analyse_if_stmt(SemaContext *context, Ast *statement)
{
	// IMPROVE
	// convert
	// if (!x) A(); else B();
	// into
	// if (x) B(); else A();

	bool else_jump;
	bool then_jump;
	bool success;

	Expr *cond = exprptr(statement->if_stmt.cond);
	Ast *then = astptr(statement->if_stmt.then_body);
	if (then->ast_kind == AST_DEFER_STMT)
	{
		RETURN_SEMA_ERROR(then, "An 'if' statement may not be followed by a raw 'defer' statement, this looks like a mistake.");
	}
	AstId else_id = statement->if_stmt.else_body;
	Ast *else_body = else_id ? astptr(else_id) : NULL;
	SCOPE_OUTER_START
		CondType cond_type = then->ast_kind == AST_IF_CATCH_SWITCH_STMT
							 ? COND_TYPE_UNWRAP : COND_TYPE_UNWRAP_BOOL;
		CondResult result = COND_MISSING;
		success = sema_analyse_cond(context, cond, cond_type, &result);

		if (success && !ast_ok(then))
		{
			SEMA_ERROR(then,
					   "The 'then' part of a single line if-statement must start on the same line as the 'if' or use '{ }'");
			success = false;
		}

		if (success && else_body)
		{
			bool then_has_braces = then->ast_kind == AST_COMPOUND_STMT || then->ast_kind == AST_IF_CATCH_SWITCH_STMT;
			if (!then_has_braces)
			{
				SEMA_ERROR(then, "if-statements with an 'else' must use '{ }' even around a single statement.");
				success = false;
			}
			if (success && else_body->ast_kind != AST_COMPOUND_STMT &&
				else_body->ast_kind != AST_IF_STMT)
			{
				SEMA_ERROR(else_body,
						   "An 'else' must use '{ }' even around a single statement.");
				success = false;
			}
		}
		if (context->active_scope.jump_end && !context->active_scope.allow_dead_code)
		{
			if (!SEMA_WARN(then, "This code will never execute."))
			{
				success = false;
			}
		}

		if (then->ast_kind == AST_IF_CATCH_SWITCH_STMT)
		{
			DeclId label_id = statement->if_stmt.flow.label;
			then->switch_stmt.flow.label = label_id;
			statement->if_stmt.flow.label = 0;
			Decl *label = declptrzero(label_id);
			if (label) label->label.parent = astid(then);
			SCOPE_START_WITH_LABEL(label_id);
				success = success && sema_analyse_switch_stmt(context, then);
				then_jump = context->active_scope.jump_end;
			SCOPE_END;
		}
		else
		{
			SCOPE_START_WITH_LABEL(statement->if_stmt.flow.label);
				if (result == COND_FALSE) context->active_scope.is_dead = true;
				success = success && sema_analyse_statement(context, then);
				then_jump = context->active_scope.jump_end;
			SCOPE_END;
		}

		if (!success) goto END;
		else_jump = false;
		if (statement->if_stmt.else_body)
		{
			SCOPE_START_WITH_LABEL(statement->if_stmt.flow.label);
				if (result == COND_TRUE) context->active_scope.is_dead = true;
				sema_remove_unwraps_from_try(context, cond);
				sema_unwrappable_from_catch_in_else(context, cond);
				success = success && sema_analyse_statement(context, else_body);
				else_jump = context->active_scope.jump_end;
			SCOPE_END;
		}

END:
		context_pop_defers_and_replace_ast(context, statement);

	SCOPE_OUTER_END;
	if (!success) return false;
	if (then_jump)
	{
		sema_unwrappable_from_catch_in_else(context, cond);
	}
	if (then_jump && else_jump && !statement->flow.has_break)
	{
		context->active_scope.jump_end = true;
	}
	return true;
}

static bool sema_analyse_asm_string_stmt(SemaContext *context, Ast *stmt)
{
	Expr *body = exprptr(stmt->asm_block_stmt.asm_string);
	if (!sema_analyse_ct_expr(context, body)) return false;
	if (!expr_is_const_string(body))
	{
		SEMA_ERROR(body, "The asm statement expects a constant string.");
		return false;
	}
	return true;
}


/**
 * When jumping to a label
 * @param context
 * @param stmt
 * @return
 */
static inline Decl *sema_analyse_label(SemaContext *context, Ast *stmt)
{
	Label *label = stmt->ast_kind == AST_NEXTCASE_STMT ? &stmt->nextcase_stmt.label : &stmt->contbreak_stmt.label;
	const char *name = label->name;
	Decl *target = sema_find_label_symbol(context, name);
	if (!target)
	{
		target = sema_find_label_symbol_anywhere(context, name);
		if (target && target->decl_kind == DECL_LABEL)
		{
			if (context->active_scope.flags & SCOPE_EXPR_BLOCK)
			{
				switch (stmt->ast_kind)
				{
					case AST_BREAK_STMT:
						SEMA_ERROR(stmt, "You cannot break out of an expression block.");
						return poisoned_decl;
					case AST_CONTINUE_STMT:
						SEMA_ERROR(stmt, "You cannot use continue out of an expression block.");
						return poisoned_decl;
					case AST_NEXTCASE_STMT:
						SEMA_ERROR(stmt, "You cannot use nextcase to exit an expression block.");
						return poisoned_decl;
					default:
						UNREACHABLE
				}
			}
			if (target->label.scope_defer != astid(context->active_scope.in_defer))
			{
				switch (stmt->ast_kind)
				{
					case AST_BREAK_STMT:
						SEMA_ERROR(stmt, "You cannot break out of a defer.");
						return poisoned_decl;
					case AST_CONTINUE_STMT:
						SEMA_ERROR(stmt, "You cannot use continue out of a defer.");
						return poisoned_decl;
					case AST_NEXTCASE_STMT:
						SEMA_ERROR(stmt, "You cannot use nextcase out of a defer.");
						return poisoned_decl;
					default:
						UNREACHABLE
				}
			}
			SEMA_ERROR(stmt, "'%s' cannot be reached from the current scope.", name);
			return poisoned_decl;
		}
		SEMA_ERROR(stmt, "A labelled statement with the name '%s' can't be found in the current scope.", name);
		return poisoned_decl;
	}
	if (target->decl_kind != DECL_LABEL)
	{
		SEMA_ERROR(label, "Expected the name to match a label, not a constant.");
		return poisoned_decl;
	}
	if (context->active_scope.in_defer)
	{
		if (target->label.scope_defer != astid(context->active_scope.in_defer))
		{
			switch (stmt->ast_kind)
			{
				case AST_BREAK_STMT:
					SEMA_ERROR(stmt, "You cannot break out of a defer.");
					return poisoned_decl;
				case AST_CONTINUE_STMT:
					SEMA_ERROR(stmt, "You cannot use continue out of a defer.");
					return poisoned_decl;
				case AST_NEXTCASE_STMT:
					SEMA_ERROR(stmt, "You cannot use nextcase out of a defer.");
					return poisoned_decl;
				default:
					UNREACHABLE
			}
		}
	}
	return target;
}

static bool context_labels_exist_in_scope(SemaContext *context)
{
	Decl **locals = context->locals;
	for (size_t local = context->active_scope.current_local; local > 0; local--)
	{
		if (locals[local - 1]->decl_kind == DECL_LABEL) return true;
	}
	return false;
}


static bool sema_analyse_nextcase_stmt(SemaContext *context, Ast *statement)
{
	context->active_scope.jump_end = true;
	if (!context->next_target && !statement->nextcase_stmt.label.name && !statement->nextcase_stmt.expr && !statement->nextcase_stmt.is_default)
	{
		if (context->next_switch)
		{
			SEMA_ERROR(statement, "A plain 'nextcase' is not allowed on the last case.");
		}
		else
		{
			SEMA_ERROR(statement, "'nextcase' can only be used inside of a switch.");
		}
		return false;
	}

	Ast *parent = context->next_switch;
	if (statement->nextcase_stmt.label.name)
	{
		Decl *target = sema_analyse_label(context, statement);
		if (!decl_ok(target)) return false;
		parent = astptr(target->label.parent);
		AstKind kind = parent->ast_kind;
		if (kind != AST_SWITCH_STMT && kind != AST_IF_CATCH_SWITCH_STMT)
		{
			RETURN_SEMA_ERROR(&statement->nextcase_stmt.label, "Expected the label to match a 'switch' or 'if-catch' statement.");
		}
	}
	if (!parent)
	{
		RETURN_SEMA_ERROR(statement, "No matching switch could be found.");
	}

	// Handle jump to default.
	Ast **cases = parent->switch_stmt.cases;
	if (statement->nextcase_stmt.is_default)
	{
		Ast *default_ast = NULL;
		FOREACH(Ast *, cs, cases)
		{
			if (cs->ast_kind == AST_DEFAULT_STMT)
			{
				default_ast = cs;
				break;
			}
		}
		if (!default_ast) RETURN_SEMA_ERROR(statement, "There is no 'default' in the switch to jump to.");
		statement->nextcase_stmt.defer_id = context_get_defers(context, context->active_scope.defer_last, parent->switch_stmt.defer, true);
		statement->nextcase_stmt.case_switch_stmt = astid(default_ast);
		statement->nextcase_stmt.switch_expr = NULL;
		return true;
	}

	Expr *value = exprptrzero(statement->nextcase_stmt.expr);
	statement->nextcase_stmt.switch_expr = NULL;
	if (!value)
	{
		ASSERT0(context->next_target);
		statement->nextcase_stmt.defer_id = context_get_defers(context, context->active_scope.defer_last, parent->switch_stmt.defer, true);
		statement->nextcase_stmt.case_switch_stmt = astid(context->next_target);
		return true;
	}

	Expr *cond = exprptrzero(parent->switch_stmt.cond);

	if (!cond)
	{
		RETURN_SEMA_ERROR(statement, "'nextcase' cannot be used with an expressionless switch.");
	}

	if (value->expr_kind == EXPR_TYPEINFO)
	{
		TypeInfo *type_info = value->type_expr;
		if (!sema_resolve_type_info(context, type_info, RESOLVE_TYPE_DEFAULT)) return false;
		statement->nextcase_stmt.defer_id = context_get_defers(context, context->active_scope.defer_last, parent->switch_stmt.defer, true);
		if (cond->type->canonical != type_typeid)
		{
			SEMA_ERROR(statement, "Unexpected 'type' in as an 'nextcase' destination.");
			SEMA_NOTE(statement, "The 'switch' here uses expected a type '%s'.", type_to_error_string(cond->type));
			return false;
		}
		Type *type = type_info->type->canonical;
		FOREACH(Ast *, case_stmt, parent->switch_stmt.cases)
		{
			if (case_stmt->ast_kind == AST_DEFAULT_STMT) continue;
			Expr *expr = exprptr(case_stmt->case_stmt.expr);
			if (sema_cast_const(expr) && expr->const_expr.typeid == type)
			{
				statement->nextcase_stmt.case_switch_stmt = astid(case_stmt);
				return true;
			}
		}
		SEMA_ERROR(type_info, "There is no case for type '%s'.", type_to_error_string(type_info->type));
		return false;
	}

	Type *expected_type = parent->ast_kind == AST_SWITCH_STMT ? cond->type : type_anyfault;

	if (!sema_analyse_expr_rhs(context, expected_type, value, false, NULL, false)) return false;

	statement->nextcase_stmt.defer_id = context_get_defers(context, context->active_scope.defer_last, parent->switch_stmt.defer, true);

	if (sema_cast_const(value))
	{
		FOREACH(Ast *, case_stmt, parent->switch_stmt.cases)
		{
			if (case_stmt->ast_kind == AST_DEFAULT_STMT) continue;
			Expr *from = exprptr(case_stmt->case_stmt.expr);
			if (!sema_cast_const(from)) goto VARIABLE_JUMP;
			ExprConst *const_expr = &from->const_expr;
			ExprConst *to_const_expr = case_stmt->case_stmt.to_expr ? &exprptr(case_stmt->case_stmt.to_expr)->const_expr : const_expr;
			if (expr_const_in_range(&value->const_expr, const_expr, to_const_expr))
			{
				statement->nextcase_stmt.case_switch_stmt = astid(case_stmt);
				return true;
			}
		}
		SEMA_ERROR(value, "There is no 'case %s' in the switch, please check if a case is missing or if this value is incorrect.", expr_const_to_error_string(&value->const_expr));
		return false;
	}
VARIABLE_JUMP:
	statement->nextcase_stmt.case_switch_stmt = astid(parent);
	statement->nextcase_stmt.switch_expr = value;
	return true;
}



static inline bool sema_analyse_then_overwrite(SemaContext *context, Ast *statement, AstId replacement)
{
	if (!replacement)
	{
		statement->ast_kind = AST_NOP_STMT;
		return true;
	}
	Ast *last = NULL;
	AstId next = statement->next;
	*statement = *astptr(replacement);
	AstId current = astid(statement);
	ASSERT0(current);
	while (current)
	{
		Ast *ast = ast_next(&current);
		if (!sema_analyse_statement(context, ast)) return false;
		last = ast;
	}
	last = ast_last(last);
	last->next = next;
	return true;
}


static inline bool sema_analyse_ct_if_stmt(SemaContext *context, Ast *statement)
{
	unsigned ct_context = sema_context_push_ct_stack(context);
	CondResult res = sema_check_comp_time_bool(context, statement->ct_if_stmt.expr);
	if (res == COND_MISSING) goto FAILED;
	if (res == COND_TRUE)
	{
		if (sema_analyse_then_overwrite(context, statement, statement->ct_if_stmt.then)) goto SUCCESS;
		goto FAILED;
	}
	Ast *elif = astptrzero(statement->ct_if_stmt.elif);
	while (1)
	{
		if (!elif)
		{
			// Turn into NOP!
			statement->ast_kind = AST_NOP_STMT;
			goto SUCCESS;
		}
		// We found else, then just replace with that.
		if (elif->ast_kind == AST_CT_ELSE_STMT)
		{
			if (sema_analyse_then_overwrite(context, statement, elif->ct_else_stmt)) goto SUCCESS;
			goto FAILED;
		}
		ASSERT0(elif->ast_kind == AST_CT_IF_STMT);

		res = sema_check_comp_time_bool(context, elif->ct_if_stmt.expr);
		if (res == COND_MISSING) goto FAILED;
		if (res == COND_TRUE)
		{
			if (sema_analyse_then_overwrite(context, statement, elif->ct_if_stmt.then)) goto SUCCESS;
			goto FAILED;
		}
		elif = astptrzero(elif->ct_if_stmt.elif);
	}
SUCCESS:
	sema_context_pop_ct_stack(context, ct_context);
	return true;
FAILED:
	sema_context_pop_ct_stack(context, ct_context);
	return false;
}

static inline bool sema_analyse_compound_statement_no_scope(SemaContext *context, Ast *compound_statement)
{
	bool all_ok = ast_ok(compound_statement);
	AstId current = compound_statement->compound_stmt.first_stmt;
	Ast *ast = NULL;
	while (current)
	{
		ast = ast_next(&current);
		if (!sema_analyse_statement(context, ast))
		{
			ast_poison(ast);
			all_ok = false;
		}
	}
	AstId *next = ast ? &ast_last(ast)->next : &compound_statement->compound_stmt.first_stmt;
	context_pop_defers(context, next);
	return all_ok;
}

static inline bool sema_check_type_case(SemaContext *context, Type *switch_type, Ast *case_stmt, Ast **cases, unsigned index)
{
	Expr *expr = exprptr(case_stmt->case_stmt.expr);
	if (!sema_analyse_expr_rhs(context, type_typeid, expr, false, NULL, false)) return false;

	if (sema_cast_const(expr))
	{
		Type *my_type = expr->const_expr.typeid;
		for (unsigned i = 0; i < index; i++)
		{
			Ast *other = cases[i];
			if (other->ast_kind != AST_CASE_STMT) continue;
			Expr *other_expr = exprptr(other->case_stmt.expr);
			if (sema_cast_const(other_expr) && other_expr->const_expr.typeid == my_type)
			{
				SEMA_ERROR(case_stmt, "The same type appears more than once.");
				SEMA_NOTE(other, "Here is the case with that type.");
				return false;
			}
		}
	}
	return true;
}

static inline bool sema_check_value_case(SemaContext *context, Type *switch_type, Ast *case_stmt, Ast **cases,
                                         unsigned index, bool *if_chained, bool *max_ranged, int *actual_cases_ref)
{
	ASSERT0(switch_type);
	Expr *expr = exprptr(case_stmt->case_stmt.expr);
	Expr *to_expr = exprptrzero(case_stmt->case_stmt.to_expr);

	// 1. Try to do implicit conversion to the correct type.
	if (!sema_analyse_expr_rhs(context, switch_type, expr, false, NULL, false)) return false;
	if (to_expr && !sema_analyse_expr_rhs(context, switch_type, to_expr, false, NULL, false)) return false;

	bool is_range = to_expr != NULL;
	bool first_is_const = sema_cast_const(expr);
	(*actual_cases_ref)++;
	if (!is_range && !first_is_const)
	{
		*if_chained = true;
		return true;
	}
	if (is_range && (!first_is_const || !(expr_is_const_int(expr) || expr_is_const_enum(expr))))
	{
		sema_error_at(context, extend_span_with_token(expr->span, to_expr->span), "Ranges must be constant integers.");
		return false;
	}
	bool is_enum = expr_is_const_enum(expr);
	ExprConst *const_expr = &expr->const_expr;
	ExprConst *to_const_expr = to_expr ? &to_expr->const_expr : const_expr;

	if (!*max_ranged && is_range)
	{
		if (is_enum)
		{
			uint32_t ord1 = const_expr->enum_err_val->enum_constant.ordinal;
			uint32_t ord2 = to_const_expr->enum_err_val->enum_constant.ordinal;
			if (ord1 > ord2)
			{
				sema_error_at(context, extend_span_with_token(expr->span, to_expr->span),
				              "The range is not valid because the first enum (%s) has a lower ordinal than the second (%s). "
				              "It would work if you swapped their order.",
				              const_expr->enum_err_val->name,
							  to_const_expr->enum_err_val->name);
				return false;
			}
			(*actual_cases_ref) += ord2 - ord1;
			if (compiler.build.switchrange_max_size < ord2 - ord1)
			{
				*max_ranged = true;
			}
		}
		else
		{
			if (int_comp(const_expr->ixx, to_const_expr->ixx, BINARYOP_GT))
			{
				sema_error_at(context, extend_span_with_token(expr->span, to_expr->span),
				              "The range is not valid because the first value (%s) is greater than the second (%s). "
				              "It would work if you swapped their order.",
				              int_to_str(const_expr->ixx, 10, false),
				              int_to_str(to_const_expr->ixx, 10, false));
				return false;
			}
			Int128 range = int_sub(to_const_expr->ixx, const_expr->ixx).i;
			Int128 max_range = { .low = compiler.build.switchrange_max_size };
			if (i128_comp(range, max_range, type_i128) == CMP_GT)
			{
				*max_ranged = true;
			}
		}
	}
	for (unsigned i = 0; i < index; i++)
	{
		Ast *other = cases[i];
		if (other->ast_kind != AST_CASE_STMT) continue;
		Expr *other_expr = exprptr(other->case_stmt.expr);
		if (!sema_cast_const(other_expr)) continue;
		ExprConst *other_const = &other_expr->const_expr;
		ExprConst *other_to_const = other->case_stmt.to_expr ? &exprptr(other->case_stmt.to_expr)->const_expr : other_const;
		if (expr_const_in_range(const_expr, other_const, other_to_const))
		{
			SEMA_ERROR(case_stmt, "The same case value appears more than once.");
			SEMA_NOTE(other, "Here is the previous use of that value.");
			return false;
		}
	}
	return true;
}

INLINE const char *create_missing_enums_in_switch_error(Ast **cases, unsigned found_count, Decl **enums)
{
	uint32_t enum_count = vec_size(enums);
	unsigned missing = enum_count - found_count;
	scratch_buffer_clear();
	if (missing == 1)
	{
		scratch_buffer_append("Enum value ");
	}
	else
	{
		scratch_buffer_printf("%u enum values were not handled in the switch: ", missing);
	}
	unsigned case_count = vec_size(cases);
	unsigned printed = 0;
	for (unsigned i = 0; i < enum_count; i++)
	{
		for (unsigned j = 0; j < case_count; j++)
		{
			Expr *e = exprptr(cases[j]->case_stmt.expr);
			Expr *e_to = exprptrzero(cases[j]->case_stmt.to_expr);
			ASSERT(e, expr_is_const_enum(e));
			uint32_t ordinal_from = e->const_expr.enum_err_val->enum_constant.ordinal;
			uint32_t ordinal_to = e_to ? e_to->const_expr.enum_err_val->enum_constant.ordinal : ordinal_from;
			if (i >= ordinal_from && i <= ordinal_to) goto CONTINUE;
		}
		if (++printed != 1)
		{
			scratch_buffer_append(printed == missing ? " and " : ", ");
		}
		scratch_buffer_append(enums[i]->name);
		if (printed > 2 && missing > 3)
		{
			scratch_buffer_append(", ...");
			goto DONE;
		}
		if (printed == missing) goto DONE;
CONTINUE:;
	}
DONE:;
	if (missing == 1)
	{
		scratch_buffer_append(" was not handled in the switch - either add it or add 'default'.");
		return scratch_buffer_to_string();
	}
	scratch_buffer_append(" - either add them or use 'default'.");
	return scratch_buffer_to_string();
}
static bool sema_analyse_switch_body(SemaContext *context, Ast *statement, SourceSpan expr_span, Type *switch_type, Ast **cases, ExprAnySwitch *any_switch, Decl *var_holder)
{
	bool use_type_id = false;
	if (!type_is_comparable(switch_type))
	{
		sema_error_at(context, expr_span, "You cannot test '%s' for equality, and only values that supports '==' for comparison can be used in a switch.", type_to_error_string(switch_type));
		return false;
	}
	// We need an if-chain if this isn't an enum/integer type.
	Type *flat = type_flatten(switch_type);
	TypeKind flat_switch_type_kind = flat->type_kind;
	bool is_enum_switch = flat_switch_type_kind == TYPE_ENUM;
	bool if_chain = !is_enum_switch && !type_kind_is_any_integer(flat_switch_type_kind);

	Ast *default_case = NULL;
	ASSERT0(context->active_scope.defer_start == context->active_scope.defer_last);

	bool exhaustive = false;
	unsigned case_count = vec_size(cases);
	bool success = true;
	bool max_ranged = false;
	bool type_switch = switch_type == type_typeid;
	int actual_enum_cases = 0;
	for (unsigned i = 0; i < case_count; i++)
	{
		if (!success) break;
		Ast *stmt = cases[i];
		Ast *next = (i < case_count - 1) ? cases[i + 1] : NULL;
		PUSH_NEXT(next, statement);
		switch (stmt->ast_kind)
		{
			case AST_CASE_STMT:
				if (type_switch)
				{
					if (!sema_check_type_case(context, switch_type, stmt, cases, i))
					{
						success = false;
						break;
					}
				}
				else
				{
					if (!sema_check_value_case(context, switch_type, stmt, cases, i, &if_chain, &max_ranged, &actual_enum_cases))
					{
						success = false;
						break;
					}
				}
				break;
			case AST_DEFAULT_STMT:
				exhaustive = true;
				if (default_case)
				{
					SEMA_ERROR(stmt, "'default' may only appear once in a single 'switch', please remove one.");
					SEMA_NOTE(default_case, "Here is the previous use.");
					success = false;
				}
				default_case = stmt;
				break;
			default:
				UNREACHABLE;
		}
		POP_NEXT();
	}

	if (!exhaustive && is_enum_switch) exhaustive = actual_enum_cases == vec_size(flat->decl->enums.values);
	bool all_jump_end = exhaustive;
	for (unsigned i = 0; i < case_count; i++)
	{
		Ast *stmt = cases[i];
		SCOPE_START
			PUSH_BREAK(statement);
			Ast *next = (i < case_count - 1) ? cases[i + 1] : NULL;
			PUSH_NEXT(next, statement);
			Ast *body = stmt->case_stmt.body;
			if (stmt->ast_kind == AST_CASE_STMT && body && type_switch && var_holder && sema_cast_const(exprptr(stmt->case_stmt.expr)))
			{
				if (any_switch->is_assign)
				{
					Type *real_type = type_get_ptr(exprptr(stmt->case_stmt.expr)->const_expr.typeid);
					Decl *new_var = decl_new_var(any_switch->new_ident, any_switch->span,
												 type_info_new_base(any_switch->is_deref
												 ? real_type->pointer : real_type, any_switch->span),
												 VARDECL_LOCAL);
					Expr *var_result = expr_variable(var_holder);
					if (!cast_explicit(context, var_result, real_type)) return false;
					if (any_switch->is_deref)
					{
						expr_rewrite_insert_deref(var_result);
					}
					new_var->var.init_expr = var_result;
					Ast *decl_ast = new_ast(AST_DECLARE_STMT, new_var->span);
					decl_ast->declare_stmt = new_var;
					ast_prepend(&body->compound_stmt.first_stmt, decl_ast);
				}
				else
				{
					Expr *expr = exprptr(stmt->case_stmt.expr);
					Type *type = type_get_ptr(expr->const_expr.typeid);
					Decl *alias = decl_new_var(var_holder->name, var_holder->span,
											   type_info_new_base(type, expr->span),
											   VARDECL_LOCAL);
					Expr *ident_converted = expr_variable(var_holder);
					if (!cast_explicit(context, ident_converted, type)) return false;
					alias->var.init_expr = ident_converted;
					alias->var.shadow = true;
					Ast *decl_ast = new_ast(AST_DECLARE_STMT, alias->span);
					decl_ast->declare_stmt = alias;
					ast_prepend(&body->compound_stmt.first_stmt, decl_ast);
				}
			}
			success = success && (!body || sema_analyse_compound_statement_no_scope(context, body));
			POP_BREAK();
			POP_NEXT();
			if (!body && i < case_count - 1) continue;
			all_jump_end &= context->active_scope.jump_end;
		SCOPE_END;
	}
	if (is_enum_switch && !exhaustive && success)
	{
		RETURN_SEMA_ERROR(statement, create_missing_enums_in_switch_error(cases, actual_enum_cases, flat->decl->enums.values));
	}
	if ((if_chain || max_ranged) && statement->flow.jump)
	{
		RETURN_SEMA_ERROR(statement, "Switch cannot use a jump table, please remove '@jump'.");
	}

	statement->flow.no_exit = all_jump_end;
	statement->switch_stmt.flow.if_chain = if_chain || max_ranged;
	return success;
}

static inline bool sema_analyse_ct_switch_stmt(SemaContext *context, Ast *statement)
{
	unsigned ct_context = sema_context_push_ct_stack(context);
	// Evaluate the switch statement
	Expr *cond = exprptrzero(statement->ct_switch_stmt.cond);
	if (cond && !sema_analyse_ct_expr(context, cond)) goto FAILED;

	// If we have a type, then we do different evaluation
	// compared to when it is a value.
	Type *type = cond ? cond->type : type_bool;
	bool is_type = false;
	switch (type_flatten(type)->type_kind)
	{
		case TYPE_TYPEID:
			is_type = true;
			FALLTHROUGH;
		case TYPE_ENUM:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_BOOL:
			break;
		case TYPE_SLICE:
			if (expr_is_const_string(cond)) break;
			FALLTHROUGH;
		default:
			ASSERT0(cond);
			SEMA_ERROR(cond, "Only types, strings, enums, integers, floats and booleans may be used with '$switch'."); // NOLINT
			goto FAILED;
	}

	ExprConst *switch_expr_const = cond ? &cond->const_expr : NULL;
	Ast **cases = statement->ct_switch_stmt.body;

	unsigned case_count = vec_size(cases);
	ASSERT0(case_count <= INT32_MAX);
	int matched_case = (int)case_count;
	int default_case = (int)case_count;

	// Go through each case
	for (unsigned i = 0; i < case_count; i++)
	{
		Ast *stmt = cases[i];
		switch (stmt->ast_kind)
		{
			case AST_CASE_STMT:
			{
				Expr *expr = exprptr(stmt->case_stmt.expr);
				Expr *to_expr = exprptrzero(stmt->case_stmt.to_expr);
				if (to_expr && !type_is_integer(type))
				{
					SEMA_ERROR(to_expr, "$case ranges are only allowed for integers.");
					goto FAILED;
				}
				// Do not evaluate if found.
				if (matched_case != case_count && expr && expr->resolve_status != RESOLVE_DONE)
				{
					continue;
				}
				if (is_type)
				{
					if (!sema_analyse_ct_expr(context, expr)) goto FAILED;
					if (expr->type != type_typeid)
					{
						SEMA_ERROR(expr, "A type was expected here not %s.", type_quoted_error_string(expr->type));
						goto FAILED;
					}
				}
				else
				{
					// Do not evaluate if found.
					if (matched_case != case_count && expr->resolve_status != RESOLVE_DONE) continue;
					if (!sema_analyse_expr_rhs(context, type, expr, false, NULL, false)) goto FAILED;
					if (to_expr && !sema_analyse_expr_rhs(context, type, to_expr, false, NULL, false)) goto FAILED;
				}
				if (!sema_cast_const(expr))
				{
					SEMA_ERROR(expr, "The $case must have a constant expression.");
					goto FAILED;
				}
				if (!cond)
				{
					if (!expr->const_expr.b) continue;
					if (matched_case == case_count) matched_case = (int)i;
					continue;
				}
				if (to_expr && !sema_cast_const(to_expr))
				{
					SEMA_ERROR(to_expr, "The $case must have a constant expression.");
					goto FAILED;
				}
				ExprConst *const_expr = &expr->const_expr;
				ExprConst *const_to_expr = to_expr ? &to_expr->const_expr : const_expr;
				if (to_expr && expr_const_compare(const_expr, const_to_expr, BINARYOP_GT))
				{
					SEMA_ERROR(to_expr, "The end of a range must be less or equal to the beginning.");
					goto FAILED;
				}
				// Check that it is unique.
				for (unsigned j = 0; j < i; j++)
				{
					Ast *other_stmt = cases[j];
					if (other_stmt->ast_kind == AST_DEFAULT_STMT) continue;
					if (exprptr(other_stmt->case_stmt.expr)->resolve_status != RESOLVE_DONE) continue;
					ExprConst *other_const = &exprptr(other_stmt->case_stmt.expr)->const_expr;
					ExprConst *other_const_to = other_stmt->case_stmt.to_expr ? &exprptr(other_stmt->case_stmt.to_expr)->const_expr : other_const;
					if (expr_const_in_range(const_expr, other_const, other_const_to))
					{
						SEMA_ERROR(stmt, "'%s' appears more than once.", expr_const_to_error_string(const_expr));
						SEMA_NOTE(exprptr(cases[j]->case_stmt.expr), "The previous $case was here.");
						goto FAILED;
					}
				}
				if (is_type)
				{
					ASSERT0(const_expr == const_to_expr);
					Type *switch_type = switch_expr_const->typeid;
					Type *case_type = const_expr->typeid;
					if (matched_case > i && type_is_subtype(case_type->canonical, switch_type->canonical))
					{
						matched_case = (int)i;
					}
				}
				else if (expr_const_in_range(switch_expr_const, const_expr, const_to_expr))
				{
					matched_case = (int)i;
				}
				break;
			}
			case AST_DEFAULT_STMT:
				if (i != case_count - 1)
				{
					SEMA_ERROR(stmt, "$default must be last in a $switch.");
					goto FAILED;
				}
				default_case = (int)i;
				continue;
			default:
				UNREACHABLE;
		}
	}

	if (matched_case == case_count) matched_case = default_case;

	Ast *body = NULL;
	for (int i = matched_case; i < case_count; i++)
	{
		body = cases[i]->case_stmt.body;
		if (body) break;
	}
	if (!body)
	{
		statement->ast_kind = AST_NOP_STMT;
		goto SUCCESS;
	}
	if (!sema_analyse_then_overwrite(context, statement, body->compound_stmt.first_stmt)) goto FAILED;
SUCCESS:
	sema_context_pop_ct_stack(context, ct_context);
	return true;
FAILED:
	sema_context_pop_ct_stack(context, ct_context);
	return false;
}


static inline bool sema_analyse_ct_foreach_stmt(SemaContext *context, Ast *statement)
{
	unsigned ct_context = sema_context_push_ct_stack(context);
	Expr *collection = exprptr(statement->ct_foreach_stmt.expr);
	if (!sema_analyse_ct_expr(context, collection)) return false;
	if (!expr_is_const(collection)) goto FAILED_NO_LIST;
	unsigned count;
	ConstInitializer *initializer = NULL;
	Expr **expressions = NULL;
	Type *const_list_type = NULL;
	const char *bytes = NULL;
	Type *bytes_type;
	switch (collection->const_expr.const_kind)
	{
		case CONST_FLOAT:
		case CONST_INTEGER:
		case CONST_BOOL:
		case CONST_ENUM:
		case CONST_ERR:
		case CONST_POINTER:
		case CONST_TYPEID:
		case CONST_REF:
		case CONST_MEMBER:
			goto FAILED_NO_LIST;
		case CONST_SLICE:
			if (!collection->const_expr.slice_init)
			{
				sema_context_pop_ct_stack(context, ct_context);
				statement->ast_kind = AST_NOP_STMT;
				return true;
			}
			initializer = collection->const_expr.slice_init;
			goto INITIALIZER;
		case CONST_INITIALIZER:
			initializer = collection->const_expr.initializer;
		INITIALIZER:;
			ConstInitType init_type = initializer->kind;
			const_list_type = type_flatten(collection->type);
			if (const_list_type->type_kind == TYPE_ARRAY || const_list_type->type_kind == TYPE_VECTOR)
			{
				count = const_list_type->array.len;
			}
			else
			{
				// Empty list
				if (init_type == CONST_INIT_ZERO)
				{
					sema_context_pop_ct_stack(context, ct_context);
					statement->ast_kind = AST_NOP_STMT;
					return true;
				}
				if (init_type != CONST_INIT_ARRAY_FULL)
				{
					SEMA_ERROR(collection, "Only regular arrays are allowed here.");
					goto FAILED;
				}
				count = vec_size(initializer->init_array_full);
			}
			break;
		case CONST_UNTYPED_LIST:
			expressions = collection->const_expr.untyped_list;
			count = vec_size(expressions);
			break;
		case CONST_BYTES:
		case CONST_STRING:
			bytes = collection->const_expr.bytes.ptr;
			count = collection->const_expr.bytes.len;
			bytes_type = type_get_indexed_type(collection->type);
			break;
	}
	Decl *index = declptrzero(statement->ct_foreach_stmt.index);

	AstId start = 0;
	if (index)
	{
		index->type = type_int;
		if (!sema_add_local(context, index)) goto FAILED;
	}
	Decl *value = declptr(statement->ct_foreach_stmt.value);
	if (!sema_add_local(context, value)) goto FAILED;
	// Get the body
	Ast *body = astptr(statement->ct_foreach_stmt.body);
	AstId *current = &start;
	unsigned loop_context = sema_context_push_ct_stack(context);
	for (unsigned i = 0; i < count; i++)
	{
		sema_context_pop_ct_stack(context, loop_context);
		Ast *compound_stmt = copy_ast_single(body);
		if (expressions)
		{
			value->var.init_expr = expressions[i];
		}
		else if (bytes)
		{
			value->var.init_expr = expr_new(EXPR_CONST, collection->span);
			expr_rewrite_const_int(value->var.init_expr, bytes_type, bytes[i]);
		}
		else
		{
			Expr *expr = expr_new(EXPR_CONST, collection->span);
			if (!expr_rewrite_to_const_initializer_index(const_list_type, initializer, expr, i, false))
			{
				SEMA_ERROR(collection, "Complex expressions are not allowed.");
				goto FAILED;
			}
			value->var.init_expr = expr;
		}
		if (index)
		{
			index->var.init_expr = expr_new_const_int(index->span, type_int, i);
			index->type = type_int;
		}
		if (!sema_analyse_compound_statement_no_scope(context, compound_stmt)) goto FAILED;
		*current = astid(compound_stmt);
		current = &compound_stmt->next;
	}
	sema_context_pop_ct_stack(context, ct_context);
	statement->ast_kind = AST_COMPOUND_STMT;
	statement->compound_stmt = (AstCompoundStmt) { .first_stmt = start };
	return true;
FAILED_NO_LIST:
	SEMA_ERROR(collection, "Expected a list to iterate over, but this was a non-list expression of type %s.",
	           type_quoted_error_string(collection->type));
FAILED:
	sema_context_pop_ct_stack(context, ct_context);
	return false;
}

static inline bool sema_analyse_switch_stmt(SemaContext *context, Ast *statement)
{
	statement->switch_stmt.scope_defer = context->active_scope.in_defer;

	SCOPE_START_WITH_LABEL(statement->switch_stmt.flow.label);

		Expr *cond = exprptrzero(statement->switch_stmt.cond);
		Type *switch_type;

		ExprAnySwitch var_switch;
		Decl *any_decl = NULL;
		if (statement->ast_kind == AST_SWITCH_STMT)
		{
			CondResult res = COND_MISSING;
			if (cond && !sema_analyse_cond(context, cond, COND_TYPE_EVALTYPE_VALUE, &res)) return false;
			Expr *last = cond ? VECLAST(cond->cond_expr) : NULL;
			switch_type = last ? last->type->canonical : type_bool;
			if (last && last->expr_kind == EXPR_ANYSWITCH)
			{
				var_switch = last->any_switch;
				Expr *inner;
				if (var_switch.is_assign)
				{
					inner = expr_new(EXPR_DECL, last->span);
					any_decl = decl_new_generated_var(type_any, VARDECL_LOCAL, last->span);
					any_decl->var.init_expr = var_switch.any_expr;
					inner->decl_expr = any_decl;
					if (!sema_analyse_expr(context, inner)) return false;
				}
				else
				{
					inner = expr_new(EXPR_IDENTIFIER, last->span);
					any_decl = var_switch.variable;
					expr_resolve_ident(inner, any_decl);
					inner->type = type_any;
				}
				expr_rewrite_to_builtin_access(last, inner, ACCESS_TYPEOFANY, type_typeid);
				switch_type = type_typeid;
				cond->type = type_typeid;
			}

		}
		else
		{
			switch_type = type_anyfault;
		}

		statement->switch_stmt.defer = context->active_scope.defer_last;
		if (!sema_analyse_switch_body(context, statement, cond ? cond->span : statement->span,
									  switch_type->canonical,
									  statement->switch_stmt.cases, any_decl ? &var_switch : NULL, any_decl))
		{
			return SCOPE_POP_ERROR();
		}
		context_pop_defers_and_replace_ast(context, statement);
	SCOPE_END;

	if (statement->flow.no_exit && !statement->flow.has_break)
	{
		context->active_scope.jump_end = true;
	}
	return true;
}

bool sema_analyse_ct_assert_stmt(SemaContext *context, Ast *statement)
{
	Expr *expr = exprptrzero(statement->assert_stmt.expr);
	ExprId message = statement->assert_stmt.message;
	const char *msg = NULL;
	Expr *message_expr = message ? exprptr(message) : NULL;
	if (message_expr)
	{
		if (!sema_analyse_expr(context, message_expr)) return false;
		if (message_expr->expr_kind != EXPR_CONST || message_expr->const_expr.const_kind != CONST_STRING)
		{
			SEMA_ERROR(message_expr, "Expected a string as the error message.");
		}
	}
	CondResult res = expr ? sema_check_comp_time_bool(context, expr) : COND_FALSE;

	if (res == COND_MISSING) return false;
	SourceSpan span = expr ? expr->span : statement->span;
	if (res == COND_FALSE)
	{
		if (message_expr)
		{
			sema_error_at(context, span, "%.*s", EXPAND_EXPR_STRING(message_expr));
		}
		else
		{
			sema_error_at(context, span, "Compile time assert failed.");
		}
		return false;
	}
	statement->ast_kind = AST_NOP_STMT;
	return true;
}

bool sema_analyse_ct_echo_stmt(SemaContext *context, Ast *statement)
{
	Expr *message = statement->expr_stmt;
	if (!sema_analyse_expr(context, message)) return false;
	if (message->expr_kind != EXPR_CONST)
	{
		SEMA_ERROR(message, "Expected a constant value.");
		return false;
	}
	printf("] ");
	switch (message->const_expr.const_kind)
	{
		case CONST_FLOAT:
			printf("%f\n", (double)message->const_expr.fxx.f);
			break;
		case CONST_INTEGER:
			puts(int_to_str(message->const_expr.ixx, 10, false));
			break;
		case CONST_BOOL:
			puts(message->const_expr.b ? "true" : "false");
			break;
		case CONST_REF:
			puts(message->const_expr.global_ref->name);
			break;
		case CONST_ENUM:
		case CONST_ERR:
			puts(message->const_expr.enum_err_val->name);
			break;
		case CONST_STRING:
			printf("%.*s\n", EXPAND_EXPR_STRING(message));
			break;
		case CONST_POINTER:
			printf("%p\n", (void*)(intptr_t)message->const_expr.ptr);
			break;
		case CONST_TYPEID:
			puts(type_to_error_string(message->const_expr.typeid));
			break;
		case CONST_BYTES:
		case CONST_SLICE:
		case CONST_INITIALIZER:
		case CONST_UNTYPED_LIST:
		case CONST_MEMBER:
			RETURN_SEMA_ERROR(message, "Unsupported type for '$echo'");
	}
	statement->ast_kind = AST_NOP_STMT;
	return true;
}

/**
 * $for(<list of ct decl/expr>, <cond>, <incr>):
 */
static inline bool sema_analyse_ct_for_stmt(SemaContext *context, Ast *statement)
{
	unsigned for_context = sema_context_push_ct_stack(context);
	bool success = false;
	ExprId init;
	if ((init = statement->for_stmt.init))
	{
		Expr *init_expr = exprptr(init);
		ASSERT0(init_expr->expr_kind == EXPR_EXPRESSION_LIST);

		// Check the list of expressions.
		FOREACH(Expr *, expr, init_expr->expression_list)
		{
			// Only a subset of declarations are allowed. We check this here.
			if (expr->expr_kind == EXPR_DECL)
			{
				Decl *decl = expr->decl_expr;
				if (decl->decl_kind != DECL_VAR || (decl->var.kind != VARDECL_LOCAL_CT && decl->var.kind != VARDECL_LOCAL_CT_TYPE))
				{
					SEMA_ERROR(expr, "Only 'var $foo' and 'var $Type' declarations are allowed in '$for'");
					goto FAILED;
				}
				if (!sema_analyse_var_decl_ct(context, decl)) goto FAILED;
				continue;
			}
			// If expression evaluate it and make sure it is constant.
			if (!sema_analyse_ct_expr(context, expr)) goto FAILED;
		}
	}
	ExprId condition = statement->for_stmt.cond;
	ExprId incr = statement->for_stmt.incr;
	Ast *body = astptr(statement->for_stmt.body);
	AstId start = 0;
	AstId *current = &start;
	Expr **incr_list = incr ? exprptr(incr)->expression_list : NULL;
	ASSERT0(condition);
	// We set a maximum of macro iterations.
	// we might consider reducing this.
	unsigned current_ct_scope = sema_context_push_ct_stack(context);
	for (int i = 0; i < MAX_MACRO_ITERATIONS; i++)
	{
		sema_context_pop_ct_stack(context, current_ct_scope);
		// First evaluate the cond, which we note that we *must* have.
		// we need to make a copy
		Expr *copy = copy_expr_single(exprptr(condition));
		CondResult result = COND_MISSING;
		if (!sema_analyse_cond_expr(context, copy, &result)) goto FAILED;
		if (result == COND_MISSING)
		{
			SEMA_ERROR(copy, "Expected a value that can be evaluated at compile time.");
			goto FAILED;
		}
		// Break if we reached "false"
		if (result == COND_FALSE) break;

		// Otherwise we copy the body.
		Ast *compound_stmt = copy_ast_single(body);

		// Analyse the body
		if (!sema_analyse_compound_statement_no_scope(context, compound_stmt)) goto FAILED;

		// Append it.
		*current = astid(compound_stmt);
		current = &compound_stmt->next;

		// Copy and evaluate all the expressions in "incr"
		FOREACH(Expr *, expr, incr_list)
		{
			if (!sema_analyse_ct_expr(context, copy_expr_single(expr))) goto FAILED;
		}
	}
	// Analysis is done turn the generated statements into a compound statement for lowering.
	statement->ast_kind = AST_COMPOUND_STMT;
	statement->compound_stmt = (AstCompoundStmt) { .first_stmt = start };
	return true;
FAILED:
	sema_context_pop_ct_stack(context, for_context);
	return false;
}


static inline bool sema_analyse_statement_inner(SemaContext *context, Ast *statement)
{
	switch (statement->ast_kind)
	{
		case AST_POISONED:
		case AST_IF_CATCH_SWITCH_STMT:
		case AST_CONTRACT:
		case AST_ASM_STMT:
		case AST_ASM_LABEL:
		case AST_CONTRACT_FAULT:
			UNREACHABLE
		case AST_DECLS_STMT:
			return sema_analyse_decls_stmt(context, statement);
		case AST_ASM_BLOCK_STMT:
			return sema_analyse_asm_stmt(context, statement);
		case AST_ASSERT_STMT:
			return sema_analyse_assert_stmt(context, statement);
		case AST_BREAK_STMT:
			return sema_analyse_break_stmt(context, statement);
		case AST_CASE_STMT:
			RETURN_SEMA_ERROR(statement, "Unexpected 'case' outside of switch");
		case AST_COMPOUND_STMT:
			return sema_analyse_compound_stmt(context, statement);
		case AST_CONTINUE_STMT:
			return sema_analyse_continue_stmt(context, statement);
		case AST_CT_ASSERT:
			return sema_analyse_ct_assert_stmt(context, statement);
		case AST_CT_IF_STMT:
			return sema_analyse_ct_if_stmt(context, statement);
		case AST_CT_ECHO_STMT:
			return sema_analyse_ct_echo_stmt(context, statement);
		case AST_DECLARE_STMT:
			return sema_analyse_declare_stmt(context, statement);
		case AST_DEFAULT_STMT:
			RETURN_SEMA_ERROR(statement, "Unexpected 'default' outside of switch");
		case AST_DEFER_STMT:
			return sema_analyse_defer_stmt(context, statement);
		case AST_EXPR_STMT:
			return sema_analyse_expr_stmt(context, statement);
		case AST_FOREACH_STMT:
			return sema_analyse_foreach_stmt(context, statement);
		case AST_FOR_STMT:
			return sema_analyse_for_stmt(context, statement);
		case AST_IF_STMT:
			return sema_analyse_if_stmt(context, statement);
		case AST_NOP_STMT:
			return true;
		case AST_BLOCK_EXIT_STMT:
			UNREACHABLE
		case AST_RETURN_STMT:
			return sema_analyse_return_stmt(context, statement);
		case AST_SWITCH_STMT:
			return sema_analyse_switch_stmt(context, statement);
		case AST_NEXTCASE_STMT:
			return sema_analyse_nextcase_stmt(context, statement);
		case AST_CT_SWITCH_STMT:
			return sema_analyse_ct_switch_stmt(context, statement);
		case AST_CT_ELSE_STMT:
			UNREACHABLE
		case AST_CT_FOREACH_STMT:
			return sema_analyse_ct_foreach_stmt(context, statement);
		case AST_CT_FOR_STMT:
			return sema_analyse_ct_for_stmt(context, statement);
	}

	UNREACHABLE
}


bool sema_analyse_statement(SemaContext *context, Ast *statement)
{
	if (statement->ast_kind == AST_POISONED) return false;
	bool dead_code = context->active_scope.jump_end;
	if (!sema_analyse_statement_inner(context, statement)) return ast_poison(statement);
	if (dead_code)
	{
		if (!context->active_scope.allow_dead_code)
		{
			context->active_scope.allow_dead_code = true;
			// If we start with an don't start with an assert AND the scope is a macro, then it's bad.
			if (statement->ast_kind != AST_ASSERT_STMT && statement->ast_kind != AST_NOP_STMT && !(context->active_scope.flags & SCOPE_MACRO))
			{
				if (!SEMA_WARN(statement, "This code will never execute."))
				{
					return ast_poison(statement);
				}
			}
			// Remove it
			statement->ast_kind = AST_NOP_STMT;
		}
	}
	return true;
}


static bool sema_analyse_require(SemaContext *context, Ast *directive, AstId **asserts, SourceSpan span)
{
	return assert_create_from_contract(context, directive, asserts, span);
}

static bool sema_analyse_ensure(SemaContext *context, Ast *directive)
{
	Expr *declexpr = directive->contract_stmt.contract.decl_exprs;
	ASSERT0(declexpr->expr_kind == EXPR_EXPRESSION_LIST);

	FOREACH(Expr *, expr, declexpr->expression_list)
	{
		if (expr->expr_kind == EXPR_DECL)
		{
			SEMA_ERROR(expr, "Only expressions are allowed.");
			return false;
		}
	}
	return true;
}

static bool sema_analyse_optional_returns(SemaContext *context, Ast *directive)
{
	Ast **returns = NULL;
	FOREACH(Ast *, ret, directive->contract_stmt.faults)
	{
		if (ret->contract_fault.resolved) continue;
		TypeInfo *type_info = ret->contract_fault.type;
		const char *ident = ret->contract_fault.ident;
		if (type_info->kind != TYPE_INFO_IDENTIFIER) RETURN_SEMA_ERROR(type_info, "Expected a fault name here.");
		if (!sema_resolve_type_info(context, type_info, RESOLVE_TYPE_DEFAULT)) return false;
		Type *type = type_info->type;
		if (type->type_kind != TYPE_FAULTTYPE) RETURN_SEMA_ERROR(type_info, "A fault type is required.");
		if (!ident)
		{
			ret->contract_fault.decl = type->decl;
			ret->contract_fault.resolved = true;
			goto NEXT;
		}
		Decl *decl = type->decl;
		Decl **enums = decl->enums.values;
		FOREACH(Decl *, opt_value, enums)
		{
			if (opt_value->name == ident)
			{
				ret->contract_fault.decl = opt_value;
				ret->contract_fault.resolved = true;
				goto NEXT;
			}
		}
		RETURN_SEMA_ERROR(ret, "No fault value '%s' found.", ident);
NEXT:;
		Decl *d = ret->contract_fault.decl;
		vec_add(context->call_env.opt_returns, d);
	}
	return true;
}


void sema_append_contract_asserts(AstId assert_first, Ast* compound_stmt)
{
	ASSERT0(compound_stmt->ast_kind == AST_COMPOUND_STMT);
	if (!assert_first) return;
	Ast *ast = new_ast(AST_COMPOUND_STMT, compound_stmt->span);
	ast->compound_stmt.first_stmt = assert_first;
	ast_prepend(&compound_stmt->compound_stmt.first_stmt, ast);
}

bool sema_analyse_contracts(SemaContext *context, AstId doc, AstId **asserts, SourceSpan call_span, bool *has_ensures)
{
	context->call_env.opt_returns = NULL;
	while (doc)
	{
		Ast *directive = astptr(doc);
		switch (directive->contract_stmt.kind)
		{
			case CONTRACT_UNKNOWN:
			case CONTRACT_PURE:
			case CONTRACT_COMMENT:
				break;
			case CONTRACT_REQUIRE:
				if (!sema_analyse_require(context, directive, asserts, call_span)) return false;
				break;
			case CONTRACT_PARAM:
				break;
			case CONTRACT_OPTIONALS:
				if (!sema_analyse_optional_returns(context, directive)) return false;
				break;
			case CONTRACT_ENSURE:
				if (!sema_analyse_ensure(context, directive)) return false;
				*has_ensures = true;
				break;
		}
		doc = directive->next;
	}
	return true;
}

bool sema_analyse_function_body(SemaContext *context, Decl *func)
{
	if (!decl_ok(func)) return false;

	Signature *signature = &func->func_decl.signature;
	if (signature->variadic == VARIADIC_RAW)
	{
		RETURN_SEMA_ERROR(func, "C-style variadic arguments '...' are not supported for regular functions,"
						  " please use typed vaargs on the form 'int... args' or "
						  "untyped vaargs on the form 'args...' instead.");
	}
	FunctionPrototype *prototype = func->type->function.prototype;
	ASSERT0(prototype);
	context->original_inline_line = 0;
	context->original_module = NULL;
	context->call_env = (CallEnv) {
		.current_function = func,
		.kind = CALL_ENV_FUNCTION,
		.pure = func->func_decl.signature.attrs.is_pure,
	};
	context->rtype = prototype->rtype;
	context->macro_call_depth = 0;
	context->active_scope = (DynamicScope) {
			.scope_id = 0,
			.depth = 0,
			.label_start = 0,
			.current_local = 0
	};
	vec_resize(context->ct_locals, 0);

	// Clear returns
	vec_resize(context->returns, 0);
	context->scope_id = 0;
	context->continue_target = NULL;
	context->next_target = 0;
	context->next_switch = 0;
	context->break_target = 0;
	ASSERT0(func->func_decl.body);
	Ast *body = astptr(func->func_decl.body);
	Decl **lambda_params = NULL;
	SCOPE_START
		ASSERT0(context->active_scope.depth == 1);
		FOREACH(Decl *, param, signature->params)
		{
			if (!sema_add_local(context, param)) return false;
		}
		if (func->func_decl.is_lambda)
		{
			lambda_params = copy_decl_list_single(func->func_decl.lambda_ct_parameters);
			FOREACH(Decl *, ct_param, lambda_params)
			{
				ct_param->var.is_read = false;
				if (!sema_add_local(context, ct_param)) return false;
			}
		}
		AstId assert_first = 0;
		AstId *next = &assert_first;
		bool has_ensures = false;
		if (!sema_analyse_contracts(context, func->func_decl.docs, &next, INVALID_SPAN, &has_ensures)) return false;
		context->call_env.ensures = has_ensures;
		bool is_naked = func->func_decl.attr_naked;
		if (!is_naked) sema_append_contract_asserts(assert_first, body);
		Type *canonical_rtype = type_no_optional(prototype->rtype)->canonical;
		if (!sema_analyse_compound_statement_no_scope(context, body)) return false;
		ASSERT(func,context->active_scope.depth == 1);
		if (!context->active_scope.jump_end && canonical_rtype != type_void)
		{
			RETURN_SEMA_ERROR(func, "Missing return statement at the end of the function.");
		}
	SCOPE_END;
	if (lambda_params)
	{
		FOREACH_IDX(i, Decl *, ct_param, lambda_params)
		{
			func->func_decl.lambda_ct_parameters[i]->var.is_read = ct_param->var.is_read;
		}
	}
	return true;
}

