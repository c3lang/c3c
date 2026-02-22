// Copyright (c) 2020-2026 Christoffer Lerno. All rights reserved.
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
static inline bool sema_defer_has_try_or_catch(AstId defer_top, AstId defer_bottom, bool *has_catch_ref);
static inline bool sema_analyse_block_exit_stmt(SemaContext *context, Ast *statement);
static inline bool sema_analyse_defer_stmt_body(SemaContext *context, Ast *statement);
static inline bool sema_analyse_for_cond(SemaContext *context, ExprId *cond_ref, bool *infinite);
static inline bool assert_create_from_contract(SemaContext *context, Expr *directive, AstId **asserts, SourceSpan evaluation_location);
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
static inline bool sema_check_type_case(SemaContext *context, Ast *case_stmt, Ast **cases, unsigned index);
static inline bool sema_check_value_case(SemaContext *context, Type *switch_type, Ast *case_stmt, Ast **cases,
                                         unsigned index, bool *if_chained, bool *max_ranged, uint64_t *actual_cases_ref,
										 Int *low, Int *high);
static bool sema_analyse_switch_body(SemaContext *context, Ast *statement, SourceSpan expr_span, CanonicalType *switch_type, Ast **cases);

static inline bool sema_analyse_statement_inner(SemaContext *context, Ast *statement);
static bool sema_analyse_require(SemaContext *context, Expr *directive, AstId **asserts, SourceSpan span);
static bool sema_analyse_ensure(SemaContext *context, Expr *directive);

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
	assert(expr != NULL);

	// Verify that the message is a string if it exists.
	Expr *message_expr = exprptrzero(statement->assert_stmt.message);
	if (message_expr)
	{
		if (!sema_analyse_ct_expr(context, message_expr)) return false;
		if (!expr_is_const_string(message_expr)) RETURN_SEMA_ERROR(message_expr, "Expected a constant string as the error message.");

		FOREACH(Expr *, e, statement->assert_stmt.args)
		{
			if (!sema_analyse_expr_rvalue(context, e)) return false;
			if (IS_OPTIONAL(e)) RETURN_SEMA_ERROR(e, "Optionals cannot be used as assert arguments, use '?""?', '!' or '!!' to fix this.");
			switch (sema_resolve_storage_type(context, e->type))
			{
				case STORAGE_ERROR:
					return false;
				case STORAGE_NORMAL:
					break;
				case STORAGE_WILDCARD:
					RETURN_SEMA_ERROR(e, "This value is always rethrown and doesn't have a definite type. This is not valid.");
				case STORAGE_VOID:
					RETURN_SEMA_ERROR(e, "This expression is of type 'void', did you make a mistake?");
				case STORAGE_COMPILE_TIME:
					if (e->type == type_untypedlist)
					{
						RETURN_SEMA_ERROR(e, "The type of an untyped list cannot be inferred, you can try adding an explicit type to solve this.");
					}
					RETURN_SEMA_ERROR(e, "You can't use a compile time type (%s) as an assert argument.", type_invalid_storage_type_name(e->type));
				case STORAGE_UNKNOWN:
					RETURN_SEMA_ERROR(e, "You can't use an argument of type %s in an assert.", type_quoted_error_string(e->type));
			}
		}
	}

	// We might have `assert(false)` or `assert(true)`
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
					SET_JUMP_END(context, statement);
					return true;
				}
				// Otherwise, require unreachable.
				RETURN_SEMA_ERROR(expr, "Use 'unreachable' instead of 'assert(false)'.");
			}
			// Otherwise we print an error.
			if (!context->active_scope.end_jump.active && !context->active_scope.is_dead)
			{
				if (message_expr && sema_cast_const(message_expr) && !vec_size(statement->assert_stmt.args))
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
			if (!message_expr)
			{
				scratch_buffer_clear();
				scratch_buffer_append("Assert \"");
				span_to_scratch(expr->span);
				scratch_buffer_append("\" failed.");
				message_expr = expr_new_const_string(expr->span, scratch_buffer_copy());
				statement->assert_stmt.message = exprid(message_expr);
			}
			return true;
	}
	UNREACHABLE
}

/**
 * break and break LABEL;
 */
static inline bool sema_analyse_break_stmt(SemaContext *context, Ast *statement)
{
	ASSERT(!statement->contbreak_stmt.is_resolved);
	// If there is no break target and there is no label,
	// we skip.
	if (!context->break_jump.target && !statement->contbreak_stmt.is_label)
	{
		if (context_labels_exist_in_scope(context))
		{
			RETURN_SEMA_ERROR(statement, "Unlabelled 'break' is not allowed here.");
		}
		RETURN_SEMA_ERROR(statement, "There is no valid target for 'break', did you make a mistake?");
	}

	// Is jump, and set it as resolved.
	SET_JUMP_END(context, statement);
	statement->contbreak_stmt.is_resolved = true;

	JumpTarget jump_target = { .target = NULL };

	if (statement->contbreak_stmt.label.name)
	{
		// If we have a label, pick it and set the parent astid to that target.
		ASSIGN_DECL_OR_RET(Decl *target, sema_analyse_label(context, statement), false);
		// We don't need to do any checking since all(!) label constructs support break.

		jump_target = (JumpTarget) { astptr(target->label.parent), target->label.defer };
	}
	else
	{
		// Jump to the default break target.
		jump_target = context->break_jump;
	}

	ASSERT(jump_target.target);
	jump_target.target->flow.has_break = true;
	statement->contbreak_stmt.ast = astid(jump_target.target);

	// Append the defers.
	statement->contbreak_stmt.defers = context_get_defers(context, jump_target.defer, true);
	return true;
}

/**
 * The regular { }
 */
static inline bool sema_analyse_compound_stmt(SemaContext *context, Ast *statement)
{
	bool success;
	EndJump ends_with_jump;
	SCOPE_START(statement->span)
		success = sema_analyse_compound_statement_no_scope(context, statement);
		ends_with_jump = context->active_scope.end_jump;
	SCOPE_END;
	// If this ends with a jump, then we know we don't need to certain analysis.
	context->active_scope.end_jump = ends_with_jump;
	return success;
}

/**
 * Ct compound statement
 */
static inline bool sema_analyse_ct_compound_stmt(SemaContext *context, Ast *statement)
{
	if (!ast_ok(statement)) return false;
	AstId current = statement->ct_compound_stmt;
	Ast *ast = NULL;
	bool all_ok = true;
	while (current)
	{
		ast = ast_next(&current);
		if (!sema_analyse_statement(context, ast))
		{
			ast_poison(ast);
			all_ok = false;
		}
	}
	return all_ok;
}

/**
 * continue and continue FOO;
 */
static inline bool sema_analyse_continue_stmt(SemaContext *context, Ast *statement)
{
	// If we have a plain continue and no continue label, we just failed.
	if (!context->continue_jump.target && !statement->contbreak_stmt.label.name)
	{
		RETURN_SEMA_ERROR(statement, "'continue' is not allowed here.");
	}

	JumpTarget jump_target = { .target = NULL };
	if (statement->contbreak_stmt.label.name)
	{
		// If we have a label grab it.
		ASSIGN_DECL_OR_RET(Decl *target, sema_analyse_label(context, statement), false);
		jump_target = (JumpTarget) { astptr(target->label.parent), target->label.defer };

		// Continue can only be used with "for" statements, skipping the "do {  };" statement
		if (!ast_supports_continue(jump_target.target))
		{
			RETURN_SEMA_ERROR(statement, "'continue' may only be used with 'for', 'foreach', 'while' and 'do-while' statements.");
		}
	}
	else
	{
		// Use default defer and ast.
		jump_target = context->continue_jump;
	}

	// This makes the active scope jump.
	SET_JUMP_END(context, statement);

	// Link the parent and add the defers.
	statement->contbreak_stmt.ast = astid(jump_target.target);
	statement->contbreak_stmt.is_resolved = true;
	statement->contbreak_stmt.defers = context_get_defers(context, jump_target.defer, true);
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
			case EXPR_PTR_ACCESS:
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

static bool sema_catch_in_cond(Expr *cond)
{
	ASSERT(cond->expr_kind == EXPR_COND && "Assumed cond");

	Expr *last = VECLAST(cond->cond_expr);
	ASSERT(last);
	last = sema_dive_into_expression(last);

	// Skip any non-unwraps
	return last->expr_kind == EXPR_CATCH;
}

/**
 * If we have "if (catch x)", then we want to unwrap x in the else clause.
 **/
static void sema_unwrappable_from_catch_in_else(SemaContext *c, Expr *cond)
{
	ASSERT(cond->expr_kind == EXPR_COND && "Assumed cond");

	Expr *last = VECLAST(cond->cond_expr);
	ASSERT(last);
	last = sema_dive_into_expression(last);

	// Skip any non-unwraps
	if (last->expr_kind != EXPR_CATCH) return;

	// If we have "if (catch x)" then this will unwrap x in the
	// else branch.
	FOREACH(Expr *, expr, last->unresolved_catch_expr.exprs)
	{
		if (expr->expr_kind != EXPR_IDENTIFIER) continue;

		Decl *decl = expr->ident_expr;
		if (decl->decl_kind != DECL_VAR) continue;
		ASSERT(decl->type->type_kind == TYPE_OPTIONAL && "The variable should always be optional at this point.");

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
				break;
		}
	}

}

// --- Sema analyse stmts

/**
 * Turn a "require" or "ensure" into a contract in the callee.
 */
static inline bool assert_create_from_contract(SemaContext *context, Expr *directive, AstId **asserts, SourceSpan evaluation_location)
{
	Expr *declexpr = directive->contract_expr.decl_exprs;
	ASSERT(declexpr->expr_kind == EXPR_EXPRESSION_LIST);

	FOREACH(Expr *, expr, declexpr->expression_list)
	{
		if (expr->expr_kind == EXPR_DECL) RETURN_SEMA_ERROR(expr, "Only expressions are allowed in contracts.");
		if (!sema_analyse_expr_rhs(context, type_bool, expr, false, NULL, false)) return false;

		if (evaluation_location.a) expr->span = evaluation_location;

		const char *comment = directive->contract_expr.comment;
		if (!comment) comment = directive->contract_expr.expr_string;
		if (expr_is_const_bool(expr))
		{
			if (expr->const_expr.b) continue;
			sema_error_at(context, expr->span, "%s", comment);
			return false;
		}

		Ast *assert = new_ast(AST_ASSERT_STMT, expr->span);
		assert->assert_stmt.is_ensure = true;
		assert->assert_stmt.expr = exprid(expr);
		Expr *comment_expr = expr_new_const_string(assert->span, comment);
		assert->assert_stmt.message = exprid(comment_expr);
		ast_append(asserts, assert);
	}
	return true;
}

// Check whether a defer chain contains a try or a catch.
static inline bool sema_defer_has_try_or_catch(AstId defer_top, AstId defer_bottom, bool *has_catch_ref)
{
	bool has_try = false;
	while (defer_bottom != defer_top)
	{
		Ast *defer = astptr(defer_top);
		if (defer->defer_stmt.is_catch)
		{
			if (has_catch_ref) *has_catch_ref = true;
			return true;
		}
		if (defer->defer_stmt.is_try)
		{
			has_try = true;
		}
		defer_top = defer->defer_stmt.prev_defer;
	}
	return has_try;
}


// Print defers at return (from macro/block or from function)
static inline void sema_inline_return_defers(SemaContext *context, Ast *stmt, AstId defer_bottom)
{
	// Store the cleanup defers, which will happen on try.
	stmt->return_stmt.cleanup = context_get_defers(context, defer_bottom, true);

	// If we have an optional return, then we create a cleanup_fail
	bool has_catch = false;
	if (stmt->return_stmt.expr && IS_OPTIONAL(stmt->return_stmt.expr)
		&& sema_defer_has_try_or_catch(context->active_scope.defer_last, context->block_return_defer, &has_catch))
	{
		stmt->return_stmt.cleanup_catch = has_catch;
		stmt->return_stmt.cleanup_fail = context_get_defers(context, context->block_return_defer, false);
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
	ASSERT(ret_expr->inner_expr->const_expr.const_kind == CONST_FAULT);
	Decl *fault = ret_expr->inner_expr->const_expr.enum_val;

	// Check that we find it.
	FOREACH(Decl *, opt, context->call_env.opt_returns)
	{
		assert(opt->decl_kind == DECL_FAULT);
		if (opt == fault) return true;
	}
	// No match
	FOREACH(Decl *, opt, context->call_env.opt_returns)
	{
		assert(opt->decl_kind == DECL_FAULT);
		if (opt == fault) return true;
	}
	RETURN_SEMA_ERROR(ret_expr, "This value does not match declared optional returns, it needs to be declared with the other optional returns.");
}

static bool sema_analyse_macro_constant_ensures(SemaContext *context, Expr *ret_expr)
{
	if (!context->current_macro) return true;
	ASSERT(context->current_macro);
	// This is a per return check, so we don't do it if the return expression is missing,
	// or if it is optional, or – obviously - if there are no '@ensure'.
	if (!ret_expr || !context->macro_has_ensures || IS_OPTIONAL(ret_expr)) return true;

	// If the return expression can't be flattened to a constant value, then
	// we won't be able to do any constant ensure checks anyway, so skip.
	if (!sema_cast_const(ret_expr)) return true;

	Decl *contracts = declptrzero(context->current_macro->func_decl.docs);
	Expr **ensures = contracts ? contracts->contracts_decl.ensures : NULL;

	// We store the old return_expr for retval
	Expr *return_expr_old = context->return_expr;
	// And set our new one.
	context->return_expr = ret_expr;
	bool success = true;
	SCOPE_START_WITH_FLAGS(SCOPE_ENSURE_MACRO, ret_expr->span);
		FOREACH(Expr *, directive, ensures)
		{
			Expr *checks = copy_expr_single(directive->contract_expr.decl_exprs);
			ASSERT(checks->expr_kind == EXPR_EXPRESSION_LIST);
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
				const char *comment = directive->contract_expr.comment;
				if (!comment) comment = directive->contract_expr.expr_string;
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
	ASSERT(context->active_scope.flags & SCOPE_MACRO);
	statement->ast_kind = AST_BLOCK_EXIT_STMT;
	SET_JUMP_END(context, statement);
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
			if (!sema_analyse_expr_rvalue(context, ret_expr)) return false;
		}
		if (!sema_check_return_matches_opt_returns(context, ret_expr)) return false;
		if (ret_expr->expr_kind == EXPR_CALL && ret_expr->call_expr.no_return)
		{
			statement->ast_kind = AST_EXPR_STMT;
			statement->expr_stmt = ret_expr;
			sema_inline_return_defers(context, statement, context->block_return_defer);
			return true;
		}
	}
	else
	{
		// What if we already had an expected type and we do an empty return.
		if (block_type && !type_is_void(type_no_optional(block_type)))
		{
			RETURN_SEMA_ERROR(statement, "Expected a return value of type %s here.", type_quoted_error_string(block_type));
		}
	}
	statement->return_stmt.block_exit_ref = context->block_exit_ref;
	sema_inline_return_defers(context, statement, context->block_return_defer);
	if (!sema_analyse_macro_constant_ensures(context, ret_expr)) return false;
	vec_add(context->block_returns, statement);
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
	bool allow_pointer = false;
	if (expr_is_const_slice(expr) && expr->const_expr.slice_init)
	{
		RETURN_SEMA_ERROR(outer, "A slice literal is backed by a stack allocated array which will be invalid once the function returns. "
						  "However, you can place the literal in a global or 'static' variable and safely return that value as long "
		                  "as the caller of the function won't modify the slice.");

	}
	// We only want && and &
	if (expr->expr_kind == EXPR_SUBSCRIPT_ADDR)
	{
		expr = exprptr(expr->subscript_expr.expr);
		allow_pointer = true;
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
	ASSERT_SPAN(expr, expr->resolve_status == RESOLVE_DONE);
	// &foo.bar.baz => foo
	while (expr->expr_kind == EXPR_ACCESS_RESOLVED)
	{
		// If we indexed into something, like &foo.bar.baz[3]
		if (allow_pointer)
		{
			// Then if foo.bar.baz was a pointer or slice, that's ok.
			TypeKind kind = type_flatten(expr->type)->type_kind;
			if (kind == TYPE_POINTER || kind == TYPE_SLICE) return true;
		}
		expr = expr->access_resolved_expr.parent;
	}
	if (expr->expr_kind != EXPR_IDENTIFIER) return true;
	Decl *decl = expr->ident_expr;
	if (decl->decl_kind != DECL_VAR) return true;
	switch (decl->var.kind)
	{
		case VARDECL_LOCAL:
			if (decl->var.is_static) return true;
			FALLTHROUGH;
		case VARDECL_PARAM:
			switch (type_flatten(decl->type)->type_kind)
			{
				case TYPE_POINTER:
				case TYPE_SLICE:
					// &foo[2] is fine if foo is a pointer or slice.
					if (allow_pointer) return true;
					break;
				default:
					break;
			}
			break;
		default:
			return true;
	}
	RETURN_SEMA_ERROR(outer, "A pointer to a local variable will be invalid once the function returns. "
					  "Allocate the data on the heap or temp memory to return a pointer.");
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
	if (context->active_scope.flags & SCOPE_MACRO)
	{
		return sema_analyse_block_exit_stmt(context, statement);
	}

	// 1. We mark that the current scope ends with a jump.
	SET_JUMP_END(context, statement);

	if (context->call_env.is_naked_fn)
	{
		RETURN_SEMA_ERROR(statement, "'return' is not allowed in '@naked' functions.");
	}
	Type *expected_rtype = context->rtype;
	ASSERT(expected_rtype && "We should always have known type from a function return.");

	Expr *return_expr = statement->return_stmt.expr;
	if (return_expr)
	{
		if (!sema_analyse_expr_rhs(context, expected_rtype, return_expr, type_is_optional(expected_rtype), NULL, false)) return false;
		if (!sema_check_not_stack_variable_escape(context, return_expr)) return false;
		if (!sema_check_return_matches_opt_returns(context, return_expr)) return false;
		// Process any ensures.
		sema_inline_return_defers(context, statement, 0);
	}
	else
	{
		if (type_no_optional(expected_rtype)->canonical != type_void)
		{
			SEMA_ERROR(statement, "Expected to return a result of type %s.", type_to_error_string(expected_rtype));
			return false;
		}
		statement->return_stmt.cleanup = context_get_defers(context, 0, true);
	}

	if (context->call_env.ensures)
	{
		// Never generate an expression.
		if (return_expr && return_expr->expr_kind == EXPR_OPTIONAL) goto SKIP_ENSURE;
		AstId first = 0;
		AstId *append_id = &first;
		// Creating an assign statement
		Decl *contracts = declptrzero(context->call_env.current_function->func_decl.docs);
		Expr **ensures = contracts ? contracts->contracts_decl.ensures : NULL;
		context->return_expr = return_expr;
		FOREACH(Expr *, ensure, ensures)
		{
			bool success;
			SCOPE_START_WITH_FLAGS(SCOPE_ENSURE, statement->span);
				success = assert_create_from_contract(context, ensure, &append_id, statement->span);
			SCOPE_END;
			if (!success) return false;
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

	ASSERT(!return_expr || type_no_optional(statement->return_stmt.expr->type)->canonical == type_no_optional(expected_rtype)->canonical);
	return true;
}

static inline bool sema_expr_valid_try_expression(Expr *expr)
{
	ASSERT_SPAN(expr, expr->resolve_status == RESOLVE_DONE);
	switch (expr->expr_kind)
	{
		case UNRESOLVED_EXPRS:
		case EXPR_BITASSIGN:
		case EXPR_CATCH:
		case EXPR_COND:
		case EXPR_POISONED:
		case EXPR_CT_ARG:
		case EXPR_CT_CALL:
		case EXPR_CT_ASSIGNABLE:
		case EXPR_CT_IS_CONST:
		case EXPR_CT_DEFINED:
		case EXPR_CT_EVAL:
		case EXPR_CONTRACT:
		case EXPR_NAMED_ARGUMENT:
			UNREACHABLE
		case EXPR_BINARY:
		case EXPR_POINTER_OFFSET:
		case EXPR_UNARY:
		case EXPR_POST_UNARY:
		case EXPR_TERNARY:
		case EXPR_LAST_FAULT:
		case EXPR_TYPECALL:
		case EXPR_MEMBER_GET:
		case EXPR_MEMBER_SET:
		case EXPR_SPLAT:
		case EXPR_MAKE_ANY:
		case EXPR_DISCARD:
			return false;
		case EXPR_BITACCESS:
		case EXPR_BUILTIN:
		case EXPR_BUILTIN_ACCESS:
		case EXPR_CALL:
		case EXPR_COMPILER_CONST:
		case EXPR_CONST:
		case EXPR_DECL:
		case EXPR_DESIGNATED_INITIALIZER_LIST:
		case EXPR_DESIGNATOR:
		case EXPR_EXPRESSION_LIST:
		case EXPR_MACRO_BLOCK:
		case EXPR_OPTIONAL:
		case EXPR_FORCE_UNWRAP:
		case EXPR_HASH_IDENT:
		case EXPR_IDENTIFIER:
		case EXPR_INITIALIZER_LIST:
		case EXPR_LAMBDA:
		case EXPR_MACRO_BODY_EXPANSION:
		case EXPR_NOP:
		case EXPR_OPERATOR_CHARS:
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
		case EXPR_TRY:
		case EXPR_TRY_UNWRAP_CHAIN:
		case EXPR_TYPEID_INFO:
		case EXPR_TYPEINFO:
		case EXPR_ACCESS_RESOLVED:
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
		case EXPR_VECTOR_FROM_ARRAY:
		case EXPR_RVALUE:
		case EXPR_RECAST:
		case EXPR_ADDR_CONVERSION:
		case EXPR_ENUM_FROM_ORD:
		case EXPR_MAYBE_DEREF:
			return true;
		case EXPR_TWO:
			return sema_expr_valid_try_expression(expr->two_expr.last);

	}
	UNREACHABLE
}
static inline bool sema_analyse_try_unwrap(SemaContext *context, Expr *expr)
{
	ASSERT(expr->expr_kind == EXPR_TRY_UNRESOLVED);
	Expr *ident = expr->unresolved_try_expr.variable;
	Expr *optional = expr->unresolved_try_expr.init;

	// Case A. Unwrapping a single variable.
	if (!optional)
	{
		if (!sema_analyse_expr_rvalue(context, ident)) return false;
		// The `try foo()` case.
		if (ident->expr_kind != EXPR_IDENTIFIER)
		{
			if (!sema_expr_valid_try_expression(ident))
			{
				RETURN_SEMA_ERROR(ident, "You need to assign this expression to something in order to use it with 'if (try ...)'.");
			}
			expr->expr_kind = EXPR_TRY;
			expr->try_expr = (ExprTry) { .optional = ident, .lhs = NULL, .assign_existing = true };
			expr->resolve_status = RESOLVE_DONE;
			expr->type = type_bool;
			return true;
		}
		Decl *decl = ident->ident_expr;
		if (decl->decl_kind != DECL_VAR)
		{
			RETURN_SEMA_ERROR(ident, "Expected this to be the name of an optional variable, but it isn't. Did you mistype?");
		}
		if (!IS_OPTIONAL(decl))
		{
			if (decl->var.kind == VARDECL_UNWRAPPED)
			{
				RETURN_SEMA_ERROR(ident, "This variable is already unwrapped, so you cannot use 'try' on it again, please remove the 'try'.");
			}
			RETURN_SEMA_ERROR(ident, "Expected this variable to be an optional, otherwise it can't be used for unwrap, maybe you didn't intend to use 'try'?");
		}
		expr->expr_kind = EXPR_TRY;
		expr->try_expr = (ExprTry) { .decl = decl, .assign_existing = false };
		expr->type = type_bool;
		sema_unwrap_var(context, decl);
		expr->resolve_status = RESOLVE_DONE;
		return true;
	}

	// Case B. We are unwrapping to a variable that may or may not exist.
	TypeInfo *var_type = expr->unresolved_try_expr.type;

	// 2. If we have a type for the variable, resolve it.
	if (var_type)
	{
		if (!sema_resolve_type_info(context, var_type, RESOLVE_TYPE_DEFAULT)) return false;
		if (IS_OPTIONAL(var_type))
		{
			RETURN_SEMA_ERROR(var_type, "Only non-optional types may be used as types for 'try', please remove the '?'.");
		}
	}

	// 3. We are creating a new variable

	// 3a. If we had a variable type, then our expression must be an identifier.
	if (ident->expr_kind != EXPR_UNRESOLVED_IDENTIFIER) RETURN_SEMA_ERROR(ident, "A variable name was expected here.");
	ASSERT(ident->resolve_status != RESOLVE_DONE);
	if (ident->unresolved_ident_expr.path) RETURN_SEMA_ERROR(ident->unresolved_ident_expr.path, "The variable may not have a path.");
	if (ident->unresolved_ident_expr.is_const) RETURN_SEMA_ERROR(ident, "Expected a variable starting with a lower case letter.");
	const char *ident_name = ident->unresolved_ident_expr.ident;

	// Special check for `if (try a = a)`
	if (optional->expr_kind == EXPR_UNRESOLVED_IDENTIFIER
		&& !optional->unresolved_ident_expr.path && optional->unresolved_ident_expr.ident == ident_name)
	{
		RETURN_SEMA_ERROR(ident, "If you want to unwrap the same variable, use 'if (try %s)' { ... } instead.", ident_name);
	}

	// 3b. Evaluate the expression
	if (!sema_analyse_expr_rvalue(context, optional)) return false;

	if (!IS_OPTIONAL(optional))
	{
		RETURN_SEMA_ERROR(optional, "Expected an optional expression to 'try' here. If it isn't an optional, remove 'try'.");
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
	Decl *decl = decl_new_var(ident->unresolved_ident_expr.ident, ident->span, var_type, VARDECL_LOCAL);

	// 4e. Analyse it
	if (!sema_analyse_var_decl(context, decl, true, NULL)) return false;

	expr->expr_kind = EXPR_TRY;
	expr->try_expr = (ExprTry) { .decl = decl, .optional = optional, .assign_existing = false };
	expr->type = type_bool;
	expr->resolve_status = RESOLVE_DONE;
	return true;
}


static inline bool sema_analyse_try_unwrap_chain(SemaContext *context, Expr *expr, CondType cond_type, CondResult *result)
{
	ASSERT_SPAN(expr, cond_type == COND_TYPE_UNWRAP_BOOL);
	ASSERT_SPAN(expr, expr->expr_kind == EXPR_TRY_UNWRAP_CHAIN);

	FOREACH(Expr *, chain_element, expr->try_unwrap_chain_expr)
	{
		if (chain_element->expr_kind == EXPR_TRY_UNRESOLVED)
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
	Expr *ident = expr->unresolved_catch_expr.variable;
	TypeInfo *type = expr->unresolved_catch_expr.type;

	Decl *decl = NULL;
	if (!type && !ident) goto RESOLVE_EXPRS;
	type = type ? type : type_info_new_base(type_fault, expr->span);

	if (!sema_resolve_type_info(context, type, RESOLVE_TYPE_DEFAULT)) return false;

	if (type->type->canonical != type_fault)
	{
		RETURN_SEMA_ERROR(type, "Expected the type to be %s, not %s.", type_quoted_error_string(type_fault),
		                  type_quoted_error_string(type->type));
	}
	if (ident->expr_kind != EXPR_UNRESOLVED_IDENTIFIER)
	{
		RETURN_SEMA_ERROR(ident, "A variable name was expected here.");
	}

	if (ident->unresolved_ident_expr.path) RETURN_SEMA_ERROR(ident->unresolved_ident_expr.path, "The variable may not have a path.");
	if (ident->unresolved_ident_expr.is_const) RETURN_SEMA_ERROR(ident, "Expected a variable starting with a lower case letter.");

	// 4d. A new declaration is created.
	decl = decl_new_var(ident->unresolved_ident_expr.ident, ident->span, type, VARDECL_LOCAL);
	decl->var.no_init = true;

	// 4e. Analyse it
	if (!sema_analyse_var_decl(context, decl, true, NULL)) return false;

RESOLVE_EXPRS:;
	Expr **exprs = expr->unresolved_catch_expr.exprs;
	FOREACH(Expr *, fail, exprs)
	{
		if (!sema_analyse_expr_rvalue(context, fail)) return false;
		if (!type_is_optional(fail->type))
		{
			RETURN_SEMA_ERROR(fail, "This expression is not optional, did you add it by mistake?");
		}
	}
	expr->catch_expr = (ExprCatch) { .exprs = exprs, .decl = decl };
	expr->expr_kind = EXPR_CATCH;
	expr->type = type_fault;
	expr->resolve_status = RESOLVE_DONE;
	return true;
}

static void sema_remove_unwraps_from_try(SemaContext *c, Expr *cond)
{
	ASSERT(cond->expr_kind == EXPR_COND);
	Expr *last = VECLAST(cond->cond_expr);
	if (!last || last->expr_kind != EXPR_TRY_UNWRAP_CHAIN) return;
	FOREACH(Expr *, expr, last->try_unwrap_chain_expr)
	{
		if (expr->expr_kind != EXPR_TRY) continue;
		if (expr->try_expr.assign_existing) continue;
		if (expr->try_expr.optional)
		{
			sema_erase_var(c, expr->try_expr.decl);
		}
		else
		{
			sema_erase_unwrapped(c, expr->try_expr.decl);
		}
	}
}

static inline bool sema_analyse_last_cond(SemaContext *context, Expr *expr, CondType cond_type, CondResult *result)
{
	switch (expr->expr_kind)
	{
		case EXPR_TRY_UNWRAP_CHAIN:
			if (cond_type != COND_TYPE_UNWRAP_BOOL)
			{
				RETURN_SEMA_ERROR(expr, "Try unwrapping is only allowed inside of a 'while' or 'if' conditional.");
			}
			return sema_analyse_try_unwrap_chain(context, expr, cond_type, result);
		case EXPR_CATCH_UNRESOLVED:
			if (cond_type != COND_TYPE_UNWRAP_BOOL)
			{
				RETURN_SEMA_ERROR(expr, "Catch unwrapping is only allowed inside of a 'while' or 'if' conditional, maybe '@catch(<expr>)' will do what you need?");
			}
			if (!sema_analyse_catch_unwrap(context, expr))
			{
				context->active_scope.is_poisoned = true;
				return false;
			}
		default:
			break;
	}
	return sema_analyse_expr_rvalue(context, expr);
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
	ASSERT(expr->expr_kind == EXPR_COND);

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
		Expr *dexpr = dexprs[i];
		if (!sema_analyse_expr_rvalue(context, dexpr)) return false;
		if (!sema_expr_check_discard(context, dexpr)) return false;
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
 * @param cond_type the type of conditional unwrap / unwrap_bool / type
 * @param result the result passed back to the caller if known at runtime
 * @return true if it passes analysis.
 */
static inline bool sema_analyse_cond(SemaContext *context, Expr *expr, CondType cond_type, CondResult *result)
{
	bool cast_to_bool = cond_type == COND_TYPE_UNWRAP_BOOL;
	ASSERT(expr->expr_kind == EXPR_COND && "Conditional expressions should always be of type EXPR_DECL_LIST");

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
		RETURN_SEMA_ERROR(expr, cast_to_bool ? "Expected a boolean expression." : "Expected an expression resulting in a value.");
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

			cast_no_check(last, type_bool, false);
		}
		if (cast_to_bool && expr_is_const_bool(init))
		{
			*result = init->const_expr.b ? COND_TRUE : COND_FALSE;
		}
		return true;
	}
	if (cast_to_bool && last->expr_kind == EXPR_BINARY && last->binary_expr.operator >= BINARYOP_ASSIGN && !last->binary_expr.grouped)
	{
		if (vec_size(expr->cond_expr) > 1)
		{
			RETURN_SEMA_ERROR(last, "An assignment in the last conditional must be parenthesized - did you mean to use '==' instead?");
		}
		else
		{
			RETURN_SEMA_ERROR(last, "An assignment in a conditional must have an extra parenthesis - did you mean to use '==' instead?");
		}
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

static inline bool sema_analyse_ct_type_assign_stmt(SemaContext *context, Ast *statement)
{
	Expr *right = statement->ct_type_assign_stmt.type_expr;
	if (!sema_analyse_expr(context, right)) return false;
	if (right->expr_kind == EXPR_TYPEINFO)
	{
		expr_rewrite_const_typeid(right, right->type_expr->type);
	}
	if (!expr_is_const_typeid(right)) RETURN_SEMA_ERROR(right, "Expected a type or constant typeid here.");

	Decl *decl = sema_find_symbol(context, statement->ct_type_assign_stmt.var_name);
	if (!decl) RETURN_SEMA_ERROR(statement, "'%s' is not defined in this scope yet.", statement->ct_type_assign_stmt.var_name);
	if (decl_is_defaulted_var(decl)) RETURN_SEMA_ERROR(statement, "The parameter '%s' was not provided by the caller.", decl->name);
	decl->var.init_expr = right;
	statement->ast_kind = AST_NOP_STMT;

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
			if (!sema_analyse_var_decl_ct(context, decl, NULL)) return false;
			statement->decls_stmt[i] = NULL;
		}
		else
		{
			if (!sema_analyse_var_decl(context, decl, true, NULL)) return false;
			should_nop = false;
		}
	}
	if (should_nop) statement->ast_kind = AST_NOP_STMT;
	return true;
}

static inline bool sema_analyse_declare_stmt(SemaContext *context, Ast *statement)
{
	Decl *decl = statement->declare_stmt;
	VarDeclKind kind = decl->var.kind;
	bool erase = kind == VARDECL_LOCAL_CT_TYPE || kind == VARDECL_LOCAL_CT;
	if (!sema_analyse_var_decl(context, decl, true, NULL))
	{
		if (!decl_ok(decl)) context->active_scope.is_poisoned = true;
		return false;
	}
	if (erase || decl->decl_kind == DECL_ERASED) statement->ast_kind = AST_NOP_STMT;
	return true;
}

static inline bool sema_analyse_expr_stmt(SemaContext *context, Ast *statement)
{
	Expr *expr = statement->expr_stmt;
	if (!sema_analyse_expr_rvalue(context, expr)) return false;
	if (!sema_expr_check_discard(context, expr)) return false;
	switch (expr->expr_kind)
	{
		case EXPR_RETHROW:
			if (expr->rethrow_expr.inner->expr_kind == EXPR_OPTIONAL)
			{
				SET_JUMP_END(context, expr);
			}
			break;
		case EXPR_FORCE_UNWRAP:
			if (expr->inner_expr->expr_kind == EXPR_OPTIONAL)
			{
				SET_JUMP_END(context, expr);
			}
			break;
		case EXPR_POST_UNARY:
			if (expr->rethrow_expr.inner->expr_kind == EXPR_OPTIONAL)
			{
				SET_JUMP_END(context, expr);
			}
			break;
		case EXPR_CALL:
			if (expr->call_expr.no_return) SET_JUMP_END(context, expr);
			break;
		case EXPR_MACRO_BLOCK:
			if (expr->macro_block.is_noreturn) SET_JUMP_END(context, expr);
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
	SCOPE_START(statement->span)

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
	ASSERT(body);
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
	SCOPE_OUTER_START(statement->span)

		if (statement->for_stmt.init)
		{
			success = sema_analyse_expr_rvalue(context, exprptr(statement->for_stmt.init));
		}

		// Conditional scope start
		SCOPE_START_WITH_LABEL(statement->for_stmt.flow.label, statement->span)

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
				statement->for_stmt.flow.no_exit = context->active_scope.end_jump.active;
			POP_BREAKCONT();

			// End for body scope
			context_pop_defers_and_replace_ast(context, body);

		SCOPE_END;

		if (statement->for_stmt.flow.skip_first)
		{
			SCOPE_START(statement->span)
				if (!sema_analyse_for_cond(context, &statement->for_stmt.cond, &is_infinite) || !success)
				{
					SCOPE_ERROR_END_OUTER();
					return false;
				}
			SCOPE_END;
			// Rewrite do { } while(true) to while(true) { }
			if (is_infinite)
			{
				ASSERT(!statement->for_stmt.cond);
				statement->for_stmt.flow.skip_first = false;
			}
		}

		if (success && statement->for_stmt.incr)
		{
			// Incr scope start
			SCOPE_START(statement->span)
				success = sema_analyse_expr_rvalue(context, exprptr(statement->for_stmt.incr));
				// Incr scope end
			SCOPE_END;
		}


		// End for body scope
		context_pop_defers_and_replace_ast(context, statement);

	SCOPE_OUTER_END;

	if (is_infinite && !statement->for_stmt.flow.has_break)
	{
		if (!success) context->active_scope.is_poisoned = true;
		SET_JUMP_END(context, statement);
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
	bool iterator_was_initializer = enumerator->expr_kind == EXPR_INITIALIZER_LIST;

	// Check the type if needed
	TypeInfo *variable_type_info = vartype(var);
	if (variable_type_info && !sema_resolve_type_info(context, variable_type_info, RESOLVE_TYPE_DEFAULT)) return false;
	if (variable_type_info)
	{
		switch (sema_resolve_storage_type(context, variable_type_info->type))
		{
			case STORAGE_ERROR: return false;
			case STORAGE_NORMAL: break;
			default: RETURN_SEMA_ERROR(var, "%s is not a valid type for '%s', only runtime types with a known size may be used.", type_quoted_error_string(variable_type_info->type), var->name);
		}
	}
	// Conditional scope start
	SCOPE_START(statement->span)

		// In the case of foreach (int x : { 1, 2, 3 }) we will infer the int[] type, so pick out the number of elements.
		Type *inferred_type = variable_type_info ? type_get_inferred_array(type_no_optional(variable_type_info->type)) : NULL;

		if (variable_type_info)
		{
			if (!sema_analyse_inferred_expr(context, inferred_type, enumerator,NULL)) return SCOPE_POP_ERROR();
		}
		else
		{
			if (!sema_analyse_expr_rvalue(context, enumerator)) return SCOPE_POP_ERROR();
		}
		// And pop the cond scope.
	SCOPE_END;

	// Trying to iterate over an optional is meaningless, it should always be handled
	// So check if it's a foreach (x : may_fail())
	if (IS_OPTIONAL(enumerator))
	{
		RETURN_SEMA_ERROR(enumerator, "The foreach iterable expression may not be optional.");
	}

	// We handle the case of `foreach(&i, v : foo)` here, as it gives the chance
	// to give better errors
	if (statement->foreach_stmt.index_by_ref)
	{
		ASSERT_SPAN(statement, index);
		RETURN_SEMA_ERROR(index, "The index cannot be held by reference, did you accidentally add a '&'?");
	}

	// We might have an untyped list, if we failed the conversion.
	Type *canonical = enumerator->type->canonical;
	if (canonical->type_kind == TYPE_UNTYPED_LIST)
	{
		if (variable_type_info || !iterator_was_initializer)
		{
			RETURN_SEMA_ERROR(enumerator, "It is not possible to enumerate a compile time 'untyped' list at runtime, but you can use the compile time `$foreach` with the list.");
		}
		RETURN_SEMA_ERROR(var, "Add an explicit type to the variable if you want to iterate over an initializer list.");
	}

	Type *original_type = enumerator->type;
	// In the case of a single `*`, then we will implicitly dereference that pointer.
	if (canonical->type_kind == TYPE_POINTER)
	{
		// Something like Foo** will not be dereferenced, only Foo*
		if (canonical->pointer->type_kind == TYPE_POINTER)
		{
			RETURN_SEMA_ERROR(enumerator, "It is not possible to enumerate an expression of type %s.", type_quoted_error_string(enumerator->type));
		}
		if (!sema_expr_rewrite_insert_deref(context, enumerator)) return false;
		canonical = enumerator->type->canonical;
	}

	// At this point we should have dereferenced any pointer or bailed.
	ASSERT_SPAN(enumerator, !type_is_pointer(enumerator->type));

	if (enumerator->type->type_kind == TYPE_FLEXIBLE_ARRAY)
	{
		RETURN_SEMA_ERROR(enumerator, "It is not possible to enumerate over a flexible array member.", type_quoted_error_string(enumerator->type));
	}
	// Check that we can even index this expression, this will dig into the flattened type.
	Type *value_type = type_get_indexed_type(enumerator->type);

	// However, if we have something distinct, that flattens to a pointer, we should never take the
	// the underlying pointee type.
	if (canonical->type_kind == TYPE_TYPEDEF && type_flatten(canonical)->type_kind == TYPE_POINTER)
	{
		value_type = NULL;
	}

	// If we have a value type and it's by ref, then we get the pointer to that type.
	if (value_type && value_by_ref) value_type = type_get_ptr(value_type);

	Decl *len = NULL;
	Decl *index_function = NULL;
	Type *index_type = type_usz;
	bool is_enum_iterator = false;
	bool need_deref = false;
	// Now we lower the foreach...
	// If we can't find a value, or this is distinct, then we assume there is an overload.
	if (!value_type || canonical->type_kind == TYPE_TYPEDEF)
	{
		// Get the overload for .len
		len = sema_find_untyped_operator(enumerator->type, OVERLOAD_LEN, NULL);
		// For foo[]
		Decl *by_val = sema_find_untyped_operator(enumerator->type, OVERLOAD_ELEMENT_AT, NULL);
		// For &foo[]
		Decl *by_ref = sema_find_untyped_operator(enumerator->type, OVERLOAD_ELEMENT_REF, NULL);

		// If we don't have .len, or there is neither by val nor by ref
		if (!len || (!by_val && !by_ref))
		{
			// If we found an underlying type we can iterate over, use that.
			if (value_type) goto SKIP_OVERLOAD;

			// Otherwise this is an error.
			RETURN_SEMA_ERROR(enumerator, "It's not possible to enumerate an expression of type %s.", type_quoted_error_string(original_type));
		}
		// If we want the value "by ref" and there isn't a &[], then this is an error.
		if (!by_ref && value_by_ref)
		{
			RETURN_SEMA_ERROR(enumerator, "%s does not support 'foreach' by reference, but you iterate by value.", type_quoted_error_string(enumerator->type));
		}

		// If there was an error in either of those declarations,
		// bail here.
		if (!decl_ok(len) || !decl_ok(by_val) || !decl_ok(by_ref)) return false;

		// Get the proper macro
		index_function = value_by_ref ? by_ref : by_val;
		if (!index_function)
		{
			assert(!value_by_ref);
			need_deref = true;
			index_function = by_ref;
		}
		ASSERT_SPAN(statement, index_function);

		// The index type is the second parameter.
		index_type = index_function->func_decl.signature.params[1]->type;

		// If it's an enum this is handled in a special way.
		is_enum_iterator = index_type->canonical->type_kind == TYPE_ENUM;
		// We check that the index is either using integer or enums.
		if (!type_is_integer(index_type) && !is_enum_iterator)
		{
			RETURN_SEMA_ERROR(enumerator, "Only types indexed by integers or enums may be used with foreach.");
		}

		// The return type is the value type if it is known (it's inferred for macros)
		TypeInfoId rtype = index_function->func_decl.signature.rtype;
		value_type = rtype ? type_infoptr(rtype)->type : NULL;
		if (need_deref && value_type)
		{
			if (value_type->type_kind != TYPE_POINTER) RETURN_SEMA_ERROR(enumerator, "Expected the index function to return a pointer.");
			value_type = value_type->pointer;
		}
	}

SKIP_OVERLOAD:;

	// Get the type of the variable, if available (e.g. foreach (Foo x : y) has a type
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
		// The index may or may not have a type, infer it as needed.
		TypeInfo *idx_type_info = vartype(index);
		if (!idx_type_info)
		{
			idx_type_info = type_info_new_base(index_type, enumerator->span);
			index->var.type_info = type_infoid(idx_type_info);
		}
		if (!sema_resolve_type_info(context, idx_type_info, RESOLVE_TYPE_DEFAULT)) return false;

		// The resulting type is our "index_var_type"
		index_var_type = idx_type_info->type;

		// Check that we don't have `foreach(int? i, y : z)`
		if (type_is_optional(index_var_type))
		{
			RETURN_SEMA_ERROR(idx_type_info, "The index may not be an optional.");
		}
		// If we have an enum iterator, the enums must match.
		if (is_enum_iterator)
		{
			if (index_var_type->canonical != index_type->canonical)
			{
				RETURN_SEMA_ERROR(idx_type_info, "The index value must be the enum %s.", type_quoted_error_string(index_type));
			}
		}
		else if (!type_is_integer(type_flatten(index_var_type)))
		{
			// Otherwise make sure it's an integer.
			RETURN_SEMA_ERROR(idx_type_info,
			                  "The index must be an integer type, '%s' is not valid.",
			                  type_to_error_string(index_var_type));
		}
	}


	// We either have "foreach (x : some_var)" or "foreach (x : some_call())"
	// So we grab the former by address (implicit &) and the latter as the value.
	// Generate the temp as needed.
	ASSERT(enumerator->resolve_status == RESOLVE_DONE);
	bool is_addr = false;
	Decl *temp = NULL;
	if (enumerator->expr_kind == EXPR_IDENTIFIER)
	{
		enumerator->ident_expr->var.is_written = true;
		temp = enumerator->ident_expr;
	}
	else
	{
		if (expr_may_addr(enumerator))
		{
			is_addr = true;
			expr_insert_addr(enumerator);
		}
		// Store either "Foo* __enum$ = &some_var;" or "Foo __enum$ = some_call()"
		temp = decl_new_generated_var(enumerator->type, VARDECL_LOCAL, enumerator->span);
		vec_add(expressions, expr_generate_decl(temp, enumerator));
	}

	// Create @__enum$.len() or @(*__enum$).len()
	Expr *enum_val = expr_variable(temp);
	enum_val->span = enumerator->span;
	if (is_addr && !sema_expr_rewrite_insert_deref(context, enum_val)) return false;
	Type *enumerator_type = type_flatten(enum_val->type);
	Expr *len_call;
	ArraySize array_len = 0;
	if (len)
	{
		len_call = expr_new(EXPR_CALL, enumerator->span);
		if (!sema_insert_method_call(context, len_call, len, enum_val, NULL, false)) return false;
	}
	else
	{
		switch (enumerator_type->type_kind)
		{
			case TYPE_ARRAY:
			case VECTORS:
				array_len = enumerator_type->array.len;
				len_call = NULL;
				break;
			case TYPE_SLICE:
				if (!sema_analyse_expr_rvalue(context, enum_val)) return false;
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

	// Find the actual index type -> flattening the enum
	Type *actual_index_type = is_enum_iterator ? enum_inner_type(index_type->canonical) : index_type;

	// Generate the index variable
	Decl *idx_decl = decl_new_generated_var(actual_index_type, VARDECL_LOCAL, index ? index->span : enumerator->span);

	// IndexType __len$ = (ActualIndexType)(@__enum$.len())
	Decl *len_decl = NULL;

	if (is_reverse)
	{
		if (!len_call)
		{
			// Create const len if missing.
			len_call = expr_new_const_int(enumerator->span, type_isz, array_len);
		}
		if (is_enum_iterator)
		{
			if (!cast_explicit(context, len_call, actual_index_type)) return false;
		}
		else
		{
			if (!cast_implicit(context, len_call, actual_index_type, false)) return false;
		}
		// __idx$ = (ActualIndexType)(@__enum$.len()) (or const)
		vec_add(expressions, expr_generate_decl(idx_decl, len_call));
	}
	else
	{
		if (len_call)
		{
			len_decl = decl_new_generated_var(actual_index_type, VARDECL_LOCAL, enumerator->span);
			bool success;
			if (is_enum_iterator)
			{
				success = cast_explicit_silent(context, len_call, actual_index_type);
			}
			else
			{
				success = cast_implicit_silent(context, len_call, actual_index_type, false);
			}
			if (!success)
			{
				SEMA_ERROR(enumerator,
				           "'foreach' is not supported, as the length %s cannot "
				           "be %s to %s - please update your definition.",
				           type_quoted_error_string(len_call->type), is_enum_iterator ? "cast" : "implicitly cast",
				           type_quoted_error_string(actual_index_type));
				if (len)
				{
					SEMA_NOTE(len, "The definition of 'len()' is here.");
					decl_poison(len);
				}
				if (index_function)
				{
					SEMA_NOTE(index_function, "The index definition is here.");
					decl_poison(index_function);
				}
				return false;
			}
			vec_add(expressions, expr_generate_decl(len_decl, len_call));
		}
		Expr *idx_init = expr_new_const_int(idx_decl->span, actual_index_type, 0);
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
		Expr *rhs = expr_new_const_int(enumerator->span, index_type, 0);
		cond = expr_new_binary(idx_decl->span, expr_variable(idx_decl), rhs, BINARYOP_GT);

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
		if (is_enum_iterator)
		{
			load_idx->type = index_var_type;
		}
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
	if (is_addr && !sema_expr_rewrite_insert_deref(context, enum_val)) return false;
	subscript->subscript_expr.expr = exprid(enum_val);
	Expr *index_expr = array_len == 1 ? expr_new_const_int(var->span, idx_decl->type, 0) : expr_variable(idx_decl);
	if (is_enum_iterator)
	{
		expr_rewrite_enum_from_ord(index_expr, index_type);
	}
	subscript->subscript_expr.index.expr = exprid(index_expr);
	if (value_by_ref || need_deref)
	{
		Expr *addr = expr_new(EXPR_UNARY, subscript->span);
		addr->unary_expr.operator = UNARYOP_ADDR;
		addr->unary_expr.expr = subscript;
		subscript = addr;
	}
	if (need_deref)
	{
		Expr *deref = expr_new(EXPR_UNARY, subscript->span);
		deref->unary_expr.operator = UNARYOP_DEREF;
		deref->unary_expr.expr = subscript;
		subscript = deref;
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
	bool else_jump = false;
	bool then_jump = false;
	bool success;

	Expr *cond = exprptr(statement->if_stmt.cond);
	Ast *then = astptr(statement->if_stmt.then_body);
	if (then->ast_kind == AST_DEFER_STMT)
	{
		RETURN_SEMA_ERROR(then, "An 'if' statement may not be followed by a raw 'defer' statement, this looks like a mistake.");
	}
	AstId else_id = statement->if_stmt.else_body;
	Ast *else_body = else_id ? astptr(else_id) : NULL;
	CondResult result = COND_MISSING;
	bool is_invalid = false;
	bool reverse = false;
	SCOPE_OUTER_START(statement->span)

		success = sema_analyse_cond(context, cond, COND_TYPE_UNWRAP_BOOL, &result);
		if (success && cond->expr_kind == EXPR_COND)
		{
			Expr **list = cond->cond_expr;
			reverse = vec_size(list) == 1 && list[0]->expr_kind == EXPR_UNARY && list[0]->unary_expr.operator == UNARYOP_NOT;
		}
		if (success && !ast_ok(then))
		{
			SEMA_ERROR(then,
					   "The 'then' part of a single line if-statement must start on the same line as the 'if' or use '{ }'");
			success = false;
		}

		if (success && else_body)
		{
			bool then_has_braces = then->ast_kind == AST_COMPOUND_STMT;
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
		if (context->active_scope.end_jump.active && !context->active_scope.allow_dead_code)
		{
			context->active_scope.allow_dead_code = true;
			bool warn = SEMA_WARN(statement, dead_code, "This code will never execute.");
			if (compiler.build.warnings.dead_code > WARNING_SILENT) sema_note_prev_at(context->active_scope.end_jump.span, "This code is preventing it from exectuting");
			if (!warn)
			{
				success = false;
				goto END;
			}
		}

		SCOPE_START_WITH_LABEL(statement->if_stmt.flow.label, then->span);
			if (result == COND_FALSE) context->active_scope.is_dead = true;
			success = success && sema_analyse_statement(context, then);
			then_jump = context->active_scope.end_jump.active && !(statement->if_stmt.flow.label && statement->if_stmt.flow.has_break);
		SCOPE_END;

		if (!success) goto END;
		else_jump = false;
		if (statement->if_stmt.else_body)
		{
			bool store_break = statement->if_stmt.flow.has_break;
			statement->if_stmt.flow.has_break = false;
			SCOPE_START_WITH_LABEL(statement->if_stmt.flow.label, statement->span);
				if (result == COND_TRUE) context->active_scope.is_dead = true;
				sema_remove_unwraps_from_try(context, cond);
				sema_unwrappable_from_catch_in_else(context, cond);
				success = sema_analyse_statement(context, else_body);
				else_jump = context->active_scope.end_jump.active && !(statement->if_stmt.flow.label && statement->if_stmt.flow.has_break);
			SCOPE_END;
			statement->if_stmt.flow.has_break |= store_break;
		}

END:
		context_pop_defers_and_replace_ast(context, statement);

		is_invalid = context->active_scope.is_poisoned;
	SCOPE_OUTER_END;
	if (is_invalid) context->active_scope.is_poisoned = true;
	if (!success)
	{
		if (then_jump && sema_catch_in_cond(cond)) context->active_scope.is_poisoned = true;
		return false;
	}
	if (then_jump)
	{
		sema_unwrappable_from_catch_in_else(context, cond);
	}
	if (then_jump && else_jump && !statement->flow.has_break)
	{

		SET_JUMP_END(context, statement);
	}
	else if (then_jump && result == COND_TRUE)
	{
		SET_JUMP_END(context, statement);
	}
	else if (else_jump && result == COND_FALSE)
	{
		SET_JUMP_END(context, statement);
	}
	if (reverse)
	{
		cond->cond_expr[0] = cond->cond_expr[0]->unary_expr.expr;
		statement->if_stmt.else_body = statement->if_stmt.then_body;
		statement->if_stmt.then_body = else_id;
	}
	return true;
}

static bool sema_analyse_asm_string_stmt(SemaContext *context, Ast *stmt)
{
	Expr *body = exprptr(stmt->asm_block_stmt.asm_string);
	if (!sema_analyse_ct_expr(context, body)) return false;
	if (!expr_is_const_string(body))
	{
		RETURN_SEMA_ERROR(body, "The asm statement expects a constant string.");
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
	SET_JUMP_END(context, statement);
	if (!context->next_jump.target && !statement->nextcase_stmt.label.name && !statement->nextcase_stmt.expr && !statement->nextcase_stmt.is_default)
	{
		if (context->next_switch)
		{
			RETURN_SEMA_ERROR(statement, "A plain 'nextcase' is not allowed on the last case.");
		}
		RETURN_SEMA_ERROR(statement, "'nextcase' can only be used inside of a switch.");
	}

	Ast *parent = context->next_switch;
	if (statement->nextcase_stmt.label.name)
	{
		Decl *target = sema_analyse_label(context, statement);
		if (!decl_ok(target)) return false;
		parent = astptr(target->label.parent);
		AstKind kind = parent->ast_kind;
		if (kind != AST_SWITCH_STMT)
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
		statement->nextcase_stmt.defer_id = context_get_defers(context, parent->switch_stmt.defer, true);
		statement->nextcase_stmt.case_switch_stmt = astid(default_ast);
		statement->nextcase_stmt.switch_expr = NULL;
		return true;
	}

	Expr *value = exprptrzero(statement->nextcase_stmt.expr);
	statement->nextcase_stmt.switch_expr = NULL;
	if (!value)
	{
		ASSERT(context->next_jump.target);
		statement->nextcase_stmt.defer_id = context_get_defers(context, parent->switch_stmt.defer, true);
		statement->nextcase_stmt.case_switch_stmt = astid(context->next_jump.target);
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
		statement->nextcase_stmt.defer_id = context_get_defers(context, parent->switch_stmt.defer, true);
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

	Type *expected_type = parent->ast_kind == AST_SWITCH_STMT ? cond->type : type_fault;

	if (!sema_analyse_expr_rhs(context, expected_type, value, false, NULL, false)) return false;

	statement->nextcase_stmt.defer_id = context_get_defers(context, parent->switch_stmt.defer, true);

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
		RETURN_SEMA_ERROR(value, "There is no 'case %s' in the switch, please check if a case is missing or if this value is incorrect.", expr_const_to_error_string(&value->const_expr));
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
	statement->ast_kind = AST_NOP_STMT;
	AstId next = statement->next;
	statement->next = replacement;
	AstId current = replacement;
	ASSERT(current);
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
		ASSERT(elif->ast_kind == AST_CT_IF_STMT);

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
			// Don't continue inside a macro, since we get too many "inlined" errors.
			if (context->current_macro) break;
		}
	}
	AstId *next = ast ? &ast_last(ast)->next : &compound_statement->compound_stmt.first_stmt;
	context_pop_defers(context, next);
	return all_ok;
}

static inline bool sema_check_type_case(SemaContext *context, Ast *case_stmt, Ast **cases, unsigned index)
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
                                         unsigned index, bool *if_chained, bool *max_ranged, uint64_t *actual_cases_ref, Int *low, Int *high)
{
	ASSERT(switch_type);
	Expr *expr = exprptr(case_stmt->case_stmt.expr);
	Expr *to_expr = exprptrzero(case_stmt->case_stmt.to_expr);

	// 1. Try to do implicit conversion to the correct type.
	if (!sema_analyse_expr_rhs(context, switch_type, expr, false, NULL, false)) return false;
	if (to_expr && !sema_analyse_expr_rhs(context, switch_type, to_expr, false, NULL, false)) return false;

	bool is_range = to_expr != NULL;
	bool first_is_const = sema_cast_const(expr) && (expr_is_const_int(expr) || expr_is_const_enum(expr));
	(*actual_cases_ref)++;
	if (!is_range && !first_is_const)
	{
		*if_chained = true;
		return true;
	}
	if (is_range)
	{
		bool second_is_const = sema_cast_const(to_expr) && (expr_is_const_int(to_expr) || expr_is_const_enum(to_expr));
		if (!first_is_const || !second_is_const) RETURN_SEMA_ERROR(first_is_const ? to_expr : expr, "Ranges must be constant integers or enum values, but this is not an integer / enum constant.");
	}
	bool is_enum = expr_is_const_enum(expr);
	ExprConst *const_expr = &expr->const_expr;
	ExprConst *to_const_expr = to_expr ? &to_expr->const_expr : const_expr;

	if (const_expr->const_kind == CONST_INTEGER)
	{
		if (low->type == TYPE_POISONED || int_comp(*low, const_expr->ixx, BINARYOP_GT)) *low = const_expr->ixx;
		if (high->type == TYPE_POISONED || int_comp(*high, to_const_expr->ixx, BINARYOP_LT)) *high = to_const_expr->ixx;
	}
	if (!*max_ranged && is_range)
	{
		if (is_enum)
		{
			uint32_t ord1 = const_expr->enum_val->enum_constant.inner_ordinal;
			uint32_t ord2 = to_const_expr->enum_val->enum_constant.inner_ordinal;
			if (ord1 > ord2)
			{
				sema_error_at(context, extend_span_with_token(expr->span, to_expr->span),
				              "The range is not valid because the first enum (%s) has a lower ordinal than the second (%s). "
				              "It would work if you swapped their order.",
				              const_expr->enum_val->name,
							  to_const_expr->enum_val->name);
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
			*actual_cases_ref += range.low;
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
			ASSERT_SPAN(e, expr_is_const_enum(e));
			uint32_t ordinal_from = e->const_expr.enum_val->enum_constant.inner_ordinal;
			uint32_t ordinal_to = e_to ? e_to->const_expr.enum_val->enum_constant.inner_ordinal : ordinal_from;
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
static bool sema_analyse_switch_body(SemaContext *context, Ast *statement, SourceSpan expr_span, CanonicalType *switch_type, Ast **cases)
{
	bool is_enum_switch = false;
	bool if_chain = true;
	Decl **enum_values = NULL;
	if (type_is_user_defined(switch_type) && switch_type->type_kind != TYPE_ENUM)
	{
		BoolErr res = sema_type_has_equality_overload(context, switch_type);
		if (res == BOOL_ERR) return false;
		if (res == BOOL_TRUE) goto FOUND;
	}
	if (type_is_comparable(switch_type))
	{
		Type *flat = type_flatten(switch_type);
		TypeKind flat_switch_type_kind = flat->type_kind;
		is_enum_switch = flat_switch_type_kind == TYPE_ENUM;
		if (is_enum_switch) enum_values = flat->decl->enums.values;
		// We need an if-chain if this isn't an enum/integer type.
		if_chain = !is_enum_switch && !type_kind_is_any_integer(flat_switch_type_kind);
	}
	else
	{
		RETURN_SEMA_ERROR_AT(expr_span, "You cannot test '%s' for equality, and only values that supports '==' for comparison can be used in a switch.", type_to_error_string(switch_type));
	}
FOUND:;
	Ast *default_case = NULL;
	ASSERT(context->active_scope.defer_start == context->active_scope.defer_last);

	bool exhaustive = false;
	unsigned case_count = vec_size(cases);
	bool success = true;
	bool max_ranged = false;
	bool type_switch = switch_type == type_typeid;
	uint64_t actual_enum_cases = 0;
	Int low = { .type = TYPE_POISONED };
	Int high = { .type = TYPE_POISONED };
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
					if (!sema_check_type_case(context, stmt, cases, i))
					{
						success = false;
						break;
					}
				}
				else
				{
					if (!sema_check_value_case(context, switch_type, stmt, cases, i, &if_chain, &max_ranged, &actual_enum_cases, &low, &high))
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

	if (!exhaustive && is_enum_switch) exhaustive = actual_enum_cases == vec_size(enum_values);
	bool all_jump_end = exhaustive;
	for (unsigned i = 0; i < case_count; i++)
	{
		Ast *stmt = cases[i];
		SCOPE_START(statement->span)
			PUSH_BREAK(statement);
			Ast *next = (i < case_count - 1) ? cases[i + 1] : NULL;
			PUSH_NEXT(next, statement);
			Ast *body = stmt->case_stmt.body;
			success = success && (!body || sema_analyse_compound_statement_no_scope(context, body));
			POP_BREAK();
			POP_NEXT();
			if (!body && i < case_count - 1) continue;
			all_jump_end &= context->active_scope.end_jump.active;
		SCOPE_END;
	}
	if (is_enum_switch && !exhaustive && success && !if_chain)
	{
		RETURN_SEMA_ERROR(statement, create_missing_enums_in_switch_error(cases, actual_enum_cases, enum_values));
	}
	if (statement->flow.jump)
	{
		if (if_chain) RETURN_SEMA_ERROR(statement, "The switch cannot use a jump table because it cannot be translated into a jump, please remove '@jump'.");
		// Ignore max ranged
		max_ranged = false;
		if (low.type != TYPE_POISONED)
		{
			Int range = int_sub(high, low);
			Int max = { .i.low = compiler.build.switchjump_max_size, .type = range.type };
			if (int_comp(range, max, BINARYOP_GE))
			{
				RETURN_SEMA_ERROR(statement, "The switch cannot use a jump table size of the table would exceed "
				                             "the maximum allowed (%u), please remove '@jump'.", compiler.build.switchjump_max_size);
			}
		}
		// Do not generate a jump table if we only have a default statement.
		if (default_case && case_count == 1)
		{
			statement->flow.jump = false;
		}
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
		case TYPE_CONST_ENUM:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_BOOL:
			break;
		case TYPE_SLICE:
			if (expr_is_const_string(cond)) break;
			FALLTHROUGH;
		default:
			ASSERT(cond);
			SEMA_ERROR(cond, "Only types, strings, enums, integers, floats and booleans may be used with '$switch'."); // NOLINT
			goto FAILED;
	}

	ExprConst *switch_expr_const = cond ? &cond->const_expr : NULL;
	Ast **cases = statement->ct_switch_stmt.body;

	unsigned case_count = vec_size(cases);
	ASSERT(case_count <= INT32_MAX);
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
				assert(expr);
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
					ASSERT(const_expr == const_to_expr);
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
	unsigned count = (unsigned)-1; // To fix invalid "maybe uninitialized" on GCC
	ConstInitializer *initializer = NULL;
	Expr **expressions = NULL;
	Type *const_list_type = NULL;
	const char *bytes = NULL;
    Type *bytes_type = NULL;
    switch (collection->const_expr.const_kind)
	{
		case CONST_FLOAT:
		case CONST_INTEGER:
		case CONST_BOOL:
		case CONST_ENUM:
		case CONST_FAULT:
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
			if (const_list_type->type_kind == TYPE_ARRAY || type_kind_is_real_vector(const_list_type->type_kind))
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
		if (!sema_analyse_statement(context, compound_stmt)) goto FAILED;
		*current = astid(compound_stmt);
		current = &compound_stmt->next;
	}
	sema_context_pop_ct_stack(context, ct_context);
	if (!start)
	{
		statement->ast_kind = AST_NOP_STMT;
	}
	else
	{
		statement->ast_kind = AST_CT_COMPOUND_STMT;
		statement->ct_compound_stmt = start;
	}
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

	SCOPE_START_WITH_LABEL(statement->switch_stmt.flow.label, statement->span);

		Expr *cond = exprptrzero(statement->switch_stmt.cond);
		Type *switch_type;

		if (statement->ast_kind == AST_SWITCH_STMT)
		{
			CondResult res = COND_MISSING;
			if (cond && !sema_analyse_cond(context, cond, COND_TYPE_EVALTYPE_VALUE, &res)) return false;
			Expr *last = cond ? VECLAST(cond->cond_expr) : NULL;
			switch_type = last ? last->type->canonical : type_bool;
		}
		else
		{
			switch_type = type_fault;
		}

		statement->switch_stmt.defer = context->active_scope.defer_last;
		if (!sema_analyse_switch_body(context, statement, cond ? cond->span : statement->span,
									  switch_type->canonical,
									  statement->switch_stmt.cases))
		{
			return SCOPE_POP_ERROR();
		}
		context_pop_defers_and_replace_ast(context, statement);
	SCOPE_END;

	if (statement->flow.no_exit && !statement->flow.has_break)
	{
		SET_JUMP_END(context, statement);
	}
	return true;
}

bool sema_analyse_ct_assert_stmt(SemaContext *context, Ast *statement)
{
	Expr *expr = exprptrzero(statement->assert_stmt.expr);
	ExprId message = statement->assert_stmt.message;
	Expr *message_expr = message ? exprptr(message) : NULL;
	if (message_expr)
	{
		if (!sema_analyse_expr_rvalue(context, message_expr)) return false;
		if (message_expr->expr_kind != EXPR_CONST || message_expr->const_expr.const_kind != CONST_STRING)
		{
			RETURN_SEMA_ERROR(message_expr, "Expected a string as the error message.");
		}
	}
	CondResult res = expr ? sema_check_comp_time_bool(context, expr) : COND_FALSE;

	if (res == COND_MISSING) return false;
	SourceSpan span = expr ? expr->span : statement->span;
	if (res == COND_FALSE)
	{
		if (message_expr)
		{
			unsigned len = vec_size(statement->assert_stmt.args);
			if (len && !sema_expr_analyse_sprintf(context, message_expr, message_expr, statement->assert_stmt.args, len)) return false;
			sema_error_at(context, span, "%.*s", EXPAND_EXPR_STRING(message_expr));
		}
		else
		{
			sema_error_at(context, span, "Compile time assert failed.");
		}
		context->active_scope.is_poisoned = true;
		return false;
	}
	statement->ast_kind = AST_NOP_STMT;
	return true;
}

bool sema_analyse_ct_echo_stmt(SemaContext *context, Ast *statement)
{
	Expr *message = statement->expr_stmt;
	if (!sema_analyse_expr_rvalue(context, message)) return false;
	if (!sema_cast_const(message))
	{
		RETURN_SEMA_ERROR(message, "Expected a constant value.");
	}
	const char *prefix = compiler.build.echo_prefix ? compiler.build.echo_prefix : "c3c:";
	while (prefix[0] != 0)
	{
		const char *next_file = strstr(prefix, "{FILE}");
		const char *next_line = strstr(prefix, "{LINE}");
		const char *next = next_line;
		if (next_file)
		{
			if (next_line && next_line < next_file)
			{
				next = next_line;
				next_file = NULL;
			}
			else
			{
				next = next_file;
				next_line = NULL;
			}
		}
		if (!next)
		{
			printf("%s", prefix);
			break;
		}
		if (prefix != next)
		{
			printf("%.*s", (int)(next - prefix), prefix);
		}
		prefix = next + 6;
		if (next_line)
		{
			printf("%d", statement->span.row);
		}
		else
		{
			File *file = source_file_by_id(statement->span.file_id);
			printf("%s", file->name);
		}
	}
	printf(" ");
	scratch_buffer_clear();
	expr_const_to_scratch_buffer(&message->const_expr);
	puts(scratch_buffer_to_string());
	statement->ast_kind = AST_NOP_STMT;
	return true;
}

/**
 * $for(<list of ct decl/expr>, <cond>, <incr>):
 */
static inline bool sema_analyse_ct_for_stmt(SemaContext *context, Ast *statement)
{
	unsigned for_context = sema_context_push_ct_stack(context);
	ExprId init;
	if ((init = statement->for_stmt.init))
	{
		Expr *init_expr = exprptr(init);
		ASSERT(init_expr->expr_kind == EXPR_EXPRESSION_LIST);

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
				if (!sema_analyse_var_decl_ct(context, decl, NULL)) goto FAILED;
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
	ASSERT(condition);
	// We set a maximum of macro iterations.
	// we might consider reducing this.
	unsigned current_ct_scope = sema_context_push_ct_stack(context);
	for (int i = 0; ; i++)
	{
		if (i >= compiler.build.max_macro_iterations)
		{
			SEMA_ERROR(statement, "Too many iterations in '$for' (it exceeded %d), you can change this limit using '--max-macro-iterations'.", compiler.build.max_macro_iterations);
			goto FAILED;
		}
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
		if (!sema_analyse_statement(context, compound_stmt)) goto FAILED;

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
	statement->ast_kind = AST_CT_COMPOUND_STMT;
	statement->ct_compound_stmt = start;
	sema_context_pop_ct_stack(context, for_context);
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
		case AST_ASM_STMT:
		case AST_ASM_LABEL:
			UNREACHABLE
		case AST_CT_TYPE_ASSIGN_STMT:
			return sema_analyse_ct_type_assign_stmt(context, statement);
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
		case AST_CT_COMPOUND_STMT:
			return sema_analyse_ct_compound_stmt(context, statement);
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
	if (context->active_scope.is_poisoned) return false;
	if (statement->ast_kind == AST_POISONED) return false;
	EndJump end_jump = context->active_scope.end_jump;
	unsigned returns = vec_size(context->block_returns);
	if (!sema_analyse_statement_inner(context, statement)) return ast_poison(statement);
	if (end_jump.active)
	{
		if (!context->active_scope.allow_dead_code)
		{
			// If we start with an don't start with an assert AND the scope is a macro, then it's bad.
			if (statement->ast_kind != AST_ASSERT_STMT && statement->ast_kind != AST_NOP_STMT && !(context->active_scope.flags & SCOPE_MACRO))
			{
				context->active_scope.allow_dead_code = true;
				bool warn = SEMA_WARN(statement, dead_code, "This code will never execute.");
				if (compiler.build.warnings.dead_code > WARNING_SILENT) sema_note_prev_at(end_jump.span, "No code will execute after this statement.");
				if (!warn) return ast_poison(statement);
			}
			// Remove it
		}
		vec_resize(context->block_returns, returns);
		statement->ast_kind = AST_NOP_STMT;
	}
	return true;
}


static bool sema_analyse_require(SemaContext *context, Expr *directive, AstId **asserts, SourceSpan span)
{
	return assert_create_from_contract(context, directive, asserts, span);
}

static bool sema_analyse_ensure(SemaContext *context, Expr *directive)
{
	Expr *declexpr = directive->contract_expr.decl_exprs;
	ASSERT(declexpr->expr_kind == EXPR_EXPRESSION_LIST);

	FOREACH(Expr *, expr, declexpr->expression_list)
	{
		if (expr->expr_kind == EXPR_DECL)
		{
			RETURN_SEMA_ERROR(expr, "Only expressions are allowed.");
		}
	}
	return true;
}

static bool sema_analyse_call_optional_returns(SemaContext *context, Decl *contract)
{
	if (!contract || !contract->contracts_decl.opt_returns)
	{
		context->call_env.opt_returns = NULL;
		return true;
	}
	if (!sema_analyse_optional_returns(context, contract)) return false;
	context->call_env.opt_returns = contract->contracts_decl.opt_returns_resolved;
	return true;
}


void sema_append_contract_asserts(AstId assert_first, Ast* compound_stmt)
{
	ASSERT(compound_stmt->ast_kind == AST_COMPOUND_STMT);
	if (!assert_first) return;
	Ast *ast = new_ast(AST_COMPOUND_STMT, compound_stmt->span);
	ast->compound_stmt.first_stmt = assert_first;
	ast_prepend(&compound_stmt->compound_stmt.first_stmt, ast);
}

bool sema_analyse_contracts(SemaContext *context, Decl *contract, Expr **requires, Expr **ensures, AstId **asserts, SourceSpan call_span, bool *has_ensures)
{
	context->call_env.opt_returns = NULL;
	if (has_ensures)
	{
		if (!sema_analyse_call_optional_returns(context, contract)) return false;
	}

	FOREACH(Expr *, require, requires)
	{
		if (!sema_analyse_require(context, require, asserts, call_span)) return false;
	}
	if (!has_ensures) return true;
	FOREACH(Expr *, ensure, ensures)
	{
		if (!sema_analyse_ensure(context, ensure)) return false;
		*has_ensures = true;
	}
	return true;
}

bool sema_analyse_function_body(SemaContext *context, Decl *func)
{
	// Stop if it's already poisoned.
	if (!decl_ok(func)) return false;
	if (func->is_body_checked) return true;
	func->is_body_checked = true;
	context->generic_instance = func->is_templated ? declptr(func->instance_id) : NULL;
	// Check the signature here we test for variadic raw, since we don't support it.
	Signature *signature = &func->func_decl.signature;
	if (signature->variadic == VARIADIC_RAW)
	{
		RETURN_SEMA_ERROR(func, "C-style variadic arguments '...' are not supported for regular functions,"
						  " please use typed vaargs on the form 'int... args' or "
						  "untyped vaargs on the form 'args...' instead.");
	}

	// Pull out the prototype
	FunctionPrototype *prototype = func->type->function.prototype;
	ASSERT_SPAN(func, prototype);

	// Set up the context for analysis
	context->original_module = NULL;
	CallEnv env = {
		.current_function = func,
		.is_naked_fn = func->func_decl.attr_naked,
		.kind = CALL_ENV_FUNCTION,
		.pure = func->func_decl.signature.attrs.is_pure,
		.ignore_deprecation = func->allow_deprecated || decl_is_deprecated(func)
	};
	context->call_env = env;

	Type *rtype = context->rtype = typeget(signature->rtype);
	context->macro_call_depth = 0;
	DynamicScope new_scope = {
		.depth = 0,
		.label_start = 0,
		.current_local = 0
	};

	context->active_scope = new_scope;
	vec_resize(context->ct_locals, 0);
	// Clear returns
	vec_resize(context->block_returns, 0);
	// Zero out any jumps
	context->break_jump = context->continue_jump = context->next_jump = (JumpTarget) { .target = NULL };
	ASSERT_SPAN(func, func->func_decl.body);

	Ast *body = astptr(func->func_decl.body);
	Decl **lambda_params = NULL;
	SCOPE_START(func->span)
		ASSERT(context->active_scope.depth == 1);
		FOREACH(Decl *, param, signature->params)
		{
			if (!sema_add_local(context, param)) return false;
		}
		if (func->func_decl.is_lambda)
		{
			// If we're a lambda we need to pass on the compile time values that will
			// be baked into the function.
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
		Decl *contracts = declptrzero(func->func_decl.docs);
		if (contracts)
		{
			copy_begin();
			Expr **requires = copy_exprlist_macro(contracts->contracts_decl.requires);
			Expr **ensures = copy_exprlist_macro(contracts->contracts_decl.ensures);
			copy_end();
			if (!sema_analyse_contracts(context, contracts, requires, ensures, &next, INVALID_SPAN, &has_ensures)) return false;
		}
		context->call_env.ensures = has_ensures;
		bool is_naked = func->func_decl.attr_naked;
		if (!is_naked) sema_append_contract_asserts(assert_first, body);
		Type *canonical_rtype = type_no_optional(rtype)->canonical;
		if (!is_naked && has_ensures && type_is_void(canonical_rtype))
		{
			AstId* append_pos = &body->compound_stmt.first_stmt;
			if (*append_pos)
			{
				Ast *last = ast_last(astptr(*append_pos));
				if (last->ast_kind == AST_RETURN_STMT) goto NEXT;
				append_pos = &last->next;
			}
			Ast *ret = ast_calloc();
			ret->ast_kind = AST_RETURN_STMT;
			ret->span = body->span;
			*append_pos = astid(ret);
		}
NEXT:
		if (!sema_analyse_compound_statement_no_scope(context, body)) return false;
		ASSERT_SPAN(func,context->active_scope.depth == 1);
		if (!context->active_scope.end_jump.active)
		{
			if (canonical_rtype != type_void && !is_naked)
			{
				RETURN_SEMA_ERROR(func, "Missing return statement at the end of the function.");
			}
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

