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

static inline bool sema_analyse_block_exit_stmt(SemaContext *context, Ast *statement);
static inline bool sema_analyse_defer_stmt_body(SemaContext *context, Ast *statement, Ast *body);
static inline bool sema_analyse_for_cond(SemaContext *context, ExprId *cond_ref, bool *infinite);
static inline bool assert_create_from_contract(SemaContext *context, Ast *directive, AstId **asserts, SourceSpan evaluation_location);
static bool sema_analyse_asm_string_stmt(SemaContext *context, Ast *stmt);
static void sema_unwrappable_from_catch_in_else(SemaContext *c, Expr *cond);
static inline bool sema_analyse_try_unwrap(SemaContext *context, Expr *expr);
static inline bool sema_analyse_try_unwrap_chain(SemaContext *context, Expr *expr, CondType cond_type);
static void sema_remove_unwraps_from_try(SemaContext *c, Expr *cond);
static inline bool sema_analyse_last_cond(SemaContext *context, Expr *expr, CondType cond_type);
static inline bool sema_analyse_cond_list(SemaContext *context, Expr *expr, CondType cond_type);
static inline bool sema_analyse_cond(SemaContext *context, Expr *expr, CondType cond_type);
static inline Decl *sema_analyse_label(SemaContext *context, Ast *stmt);
static bool context_labels_exist_in_scope(SemaContext *context);
static inline bool sema_analyse_then_overwrite(SemaContext *context, Ast *statement, AstId replacement);
static inline bool sema_analyse_catch_unwrap(SemaContext *context, Expr *expr);
static inline bool sema_analyse_compound_statement_no_scope(SemaContext *context, Ast *compound_statement);
static inline bool sema_check_type_case(SemaContext *context, Type *switch_type, Ast *case_stmt, Ast **cases, unsigned index);
static inline bool sema_check_value_case(SemaContext *context, Type *switch_type, Ast *case_stmt, Ast **cases, unsigned index, bool *if_chained, bool *max_ranged);
static bool sema_analyse_switch_body(SemaContext *context, Ast *statement, SourceSpan expr_span, Type *switch_type, Ast **cases, ExprVariantSwitch *variant, Decl *var_holder);

static inline bool sema_analyse_statement_inner(SemaContext *context, Ast *statement);
static bool sema_analyse_require(SemaContext *context, Ast *directive, AstId **asserts);
static bool sema_analyse_ensure(SemaContext *context, Ast *directive);
static bool sema_analyse_errors(SemaContext *context, Ast *directive);
static bool sema_analyse_checked(SemaContext *context, Ast *directive, AstId **asserts);

static inline bool sema_analyse_asm_stmt(SemaContext *context, Ast *stmt)
{
	if (stmt->asm_block_stmt.is_string) return sema_analyse_asm_string_stmt(context, stmt);
	AsmInlineBlock *block = stmt->asm_block_stmt.block;
	AstId ast_id = block->asm_stmt;
	scratch_buffer_clear();
	while (ast_id)
	{
		Ast *ast = astptr(ast_id);
		ast_id = ast->next;
		if (!sema_analyse_asm(context, block, ast)) return false;
	}
	return true;
}

/**
 * assert(foo), assert(foo, message), assert(try foo), assert(try foo, message)
 *
 * - Using the try construct we implicitly unpack the variable if present.
 * - assert(false) will implicitly make the rest of the code marked as unreachable.
 * - assert might also be used for 'ensure' and in this case violating it is a compile time error.
 */
static inline bool sema_analyse_assert_stmt(SemaContext *context, Ast *statement)
{
	Expr *expr = exprptr(statement->assert_stmt.expr);

	// Verify that the message is a string if it exists.
	Expr *message_expr = exprptrzero(statement->assert_stmt.message);
	if (message_expr)
	{
		if (!sema_analyse_expr(context, message_expr)) return false;
		if (!expr_is_const_string(message_expr))
		{
			SEMA_ERROR(message_expr, "Expected a string as the error message.");
			return false;
		}
	}


	// Handle force unwrapping using assert, e.g. assert(try x)
	if (expr->expr_kind == EXPR_TRY_UNWRAP_CHAIN)
	{
		return sema_analyse_try_unwrap_chain(context, expr, COND_TYPE_UNWRAP_BOOL);
	}

	// Check the conditional inside
	if (!sema_analyse_cond_expr(context, expr)) return false;

	// If it's constant, we process it differently.
	if (expr_is_const(expr))
	{
		// It's true, then replace the statement with a nop.
		if (expr->const_expr.b)
		{
			statement->ast_kind = AST_NOP_STMT;
			return true;
		}
		// If it's ensure (and an error) we print an error.
		if (statement->assert_stmt.is_ensure)
		{
			if (message_expr)
			{
				SEMA_ERROR(expr, "%.*s", EXPAND_EXPR_STRING(message_expr));
			}
			else
			{
				SEMA_ERROR(expr, "Contract violated.");
			}
			return false;
		}
		// assert(false) means this can't be reached.
		context->active_scope.jump_end = true;
	}
	return true;
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
			SEMA_ERROR(statement, "Unlabelled 'break' is not allowed here.");
		}
		else
		{
			SEMA_ERROR(statement, "There is no valid target for 'break', did you make a mistake?");
		}
		return false;
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

	assert(parent);
	parent->flow.has_break = true;
	statement->contbreak_stmt.ast = astid(parent);

	// Append the defers.
	statement->contbreak_stmt.defers = context_get_defers(context, context->active_scope.defer_last, defer_begin);
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
		SEMA_ERROR(statement, "'continue' is not allowed here.");
		return false;
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
			SEMA_ERROR(statement, "'continue' may only be used with 'for', 'while' and 'do-while' statements.");
			return false;
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
	statement->contbreak_stmt.defers = context_get_defers(context, context->active_scope.defer_last, defer_id);
	return true;
}

static void sema_unwrappable_from_catch_in_else(SemaContext *c, Expr *cond)
{
	assert(cond->expr_kind == EXPR_COND);

	Expr *last = VECLAST(cond->cond_expr);
	while (last->expr_kind == EXPR_CAST)
	{
		last = exprptr(last->cast_expr.expr);
	}
	if (!last || last->expr_kind != EXPR_CATCH_UNWRAP) return;

	Expr **unwrapped = last->catch_unwrap_expr.exprs;

	VECEACH(unwrapped, i)
	{
		Expr *expr = unwrapped[i];
		if (expr->expr_kind != EXPR_IDENTIFIER) continue;
		Decl *decl = expr->identifier_expr.decl;
		if (decl->decl_kind != DECL_VAR) continue;
		assert(decl->type->type_kind == TYPE_OPTIONAL && "The variable should always be failable at this point.");

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


static inline bool assert_create_from_contract(SemaContext *context, Ast *directive, AstId **asserts, SourceSpan evaluation_location)
{
	Expr *declexpr = directive->doc_stmt.contract.decl_exprs;
	assert(declexpr->expr_kind == EXPR_EXPRESSION_LIST);

	Expr **exprs = declexpr->expression_list;
	VECEACH(exprs, j)
	{
		Expr *expr = exprs[j];
		if (expr->expr_kind == EXPR_DECL)
		{
			SEMA_ERROR(expr, "Only expressions are allowed.");
			return false;
		}
		if (!sema_analyse_cond_expr(context, expr)) return false;

		const char *comment = directive->doc_stmt.contract.comment;
		if (!comment) comment = directive->doc_stmt.contract.expr_string;
		if (expr_is_const(expr))
		{
			assert(expr->const_expr.const_kind == CONST_BOOL);
			if (expr->const_expr.b)
			{
				// Verified, so we can skip introducing it.
				continue;
			}
			sema_error_at(evaluation_location.a ? evaluation_location : expr->span, "%s", comment);
			return false;
		}
		Ast *assert = new_ast(AST_ASSERT_STMT, expr->span);
		assert->assert_stmt.is_ensure = true;
		assert->assert_stmt.expr = exprid(expr);
		Expr *comment_expr = expr_new(EXPR_CONST, expr->span);
		expr_rewrite_to_string(comment_expr, comment);
		assert->assert_stmt.message = exprid(comment_expr);
		ast_append(asserts, assert);
	}
	return true;
}

/**
 * Handle exit in a macro or in an expression block.
 * @param context
 * @param statement
 * @return
 */
static inline bool sema_analyse_block_exit_stmt(SemaContext *context, Ast *statement)
{
	assert(context->active_scope.flags & (SCOPE_EXPR_BLOCK | SCOPE_MACRO));
	statement->ast_kind = AST_BLOCK_EXIT_STMT;
	context->active_scope.jump_end = true;
	Type *block_type = context->expected_block_type;
	if (statement->return_stmt.expr)
	{
		if (block_type)
		{
			if (!sema_analyse_expr_rhs(context, block_type, statement->return_stmt.expr, type_is_optional(block_type))) return false;
		}
		else
		{
			if (!sema_analyse_expr(context, statement->return_stmt.expr)) return false;
		}
	}
	else
	{
		if (block_type && block_type != type_void)
		{
			SEMA_ERROR(statement, "Expected a return value of type %s here.", type_quoted_error_string(block_type));
			return false;
		}
	}
	statement->return_stmt.block_exit_ref = context->block_exit_ref;
	statement->return_stmt.cleanup = context_get_defers(context, context->active_scope.defer_last, context->block_return_defer);
	vec_add(context->returns, statement);
	return true;
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
		SEMA_ERROR(statement, "Return is not allowed inside of a defer.");
		return false;
	}

	// This might be a return in a function block or a macro which must be treated differently.
	if (context->active_scope.flags & (SCOPE_EXPR_BLOCK | SCOPE_MACRO))
	{
		return sema_analyse_block_exit_stmt(context, statement);
	}
	// 1. We mark that the current scope ends with a jump.
	context->active_scope.jump_end = true;

	Type *expected_rtype = context->rtype;
	assert(expected_rtype && "We should always have known type from a function return.");

	Expr *return_expr = statement->return_stmt.expr;

	if (return_expr)
	{
		if (!sema_analyse_expr_rhs(context, expected_rtype, return_expr, type_is_optional(expected_rtype))) return false;
	}
	else
	{
		if (type_no_optional(expected_rtype)->canonical != type_void)
		{
			SEMA_ERROR(statement, "Expected to return a result of type %s.", type_to_error_string(expected_rtype));
			return false;
		}
		statement->return_stmt.cleanup = context_get_defers(context, context->active_scope.defer_last, 0);
		return true;
	}

	// Process any ensures.
	AstId cleanup = context_get_defers(context, context->active_scope.defer_last, 0);
	if (context->call_env.ensures)
	{
		AstId first = 0;
		AstId *append_id = &first;
		// Creating an assign statement
		AstId doc_directive = context->call_env.current_function->func_decl.docs;
		context->return_expr = return_expr;
		while (doc_directive)
		{
			Ast *directive = astptr(doc_directive);
			if (directive->doc_stmt.kind == DOC_DIRECTIVE_ENSURE)
			{
				if (!assert_create_from_contract(context, directive, &append_id, statement->span)) return false;
			}
			doc_directive = directive->next;
		}
		if (cleanup)
		{
			Ast *last = ast_last(astptr(cleanup));
			last->next = first;
		}
		else
		{
			cleanup = first;
		}
	}

	statement->return_stmt.cleanup = cleanup;

	assert(type_no_optional(statement->return_stmt.expr->type)->canonical == type_no_optional(expected_rtype)->canonical);

	return true;
}


static inline bool sema_analyse_try_unwrap(SemaContext *context, Expr *expr)
{
	assert(expr->expr_kind == EXPR_TRY_UNWRAP);
	Expr *ident = expr->try_unwrap_expr.variable;
	Expr *failable = expr->try_unwrap_expr.init;

	// Case A. Unwrapping a single variable.
	if (!failable)
	{
		if (!sema_analyse_expr(context, ident)) return false;
		if (ident->expr_kind != EXPR_IDENTIFIER)
		{
			SEMA_ERROR(ident, "Only single identifiers may be unwrapped using 'try var', maybe you wanted 'try (expr)' instead?");
			return false;
		}
		Decl *decl = ident->identifier_expr.decl;
		if (decl->decl_kind != DECL_VAR)
		{
			SEMA_ERROR(ident, "Expected this to be the name of a failable variable, but it isn't. Did you mistype?");
			return false;
		}
		if (!IS_OPTIONAL(decl))
		{
			if (decl->var.kind == VARDECL_UNWRAPPED)
			{
				SEMA_ERROR(ident, "This variable is already unwrapped, so you cannot use 'try' on it again, please remove the 'try'.");
				return false;
			}
			SEMA_ERROR(ident, "Expected this variable to be a failable, otherwise it can't be used for unwrap, maybe you didn't intend to use 'try'?");
			return false;
		}
		expr->try_unwrap_expr.decl = decl;
		expr->type = type_bool;
		sema_unwrap_var(context, decl);
		expr->resolve_status = RESOLVE_DONE;
		return true;
	}

	// Case B. We are unwrapping to a variable that may or may not exist.
	bool implicit_declaration = false;
	TypeInfo *var_type = expr->try_unwrap_expr.type;

	// 1. Check if we are doing an implicit declaration.
	if (!var_type && ident->expr_kind == EXPR_IDENTIFIER)
	{
		implicit_declaration = !sema_symbol_is_defined_in_scope(context, ident->identifier_expr.ident);
	}

	// 2. If we have a type for the variable, resolve it.
	if (var_type)
	{
		if (!sema_resolve_type_info(context, var_type)) return false;
		if (IS_OPTIONAL(var_type))
		{
			SEMA_ERROR(var_type, "Only non-failable types may be used as types for 'try', please remove the '!'.");
			return false;
		}
	}

	// 3. We interpret this as an assignment to an existing variable.
	if (!var_type && !implicit_declaration)
	{
		// 3a. Resolve the identifier.
		if (!sema_analyse_expr_lvalue(context, ident)) return false;

		// 3b. Make sure it's assignable
		if (!sema_expr_check_assign(context, ident)) return false;

		// 3c. It can't be failable either.
		if (IS_OPTIONAL(ident))
		{
			if (ident->expr_kind == EXPR_IDENTIFIER)
			{
				SEMA_ERROR(ident, "This is a failable variable, you should only have non-failable variables on the left side unless you use 'try' without '='.");
			}
			else
			{
				SEMA_ERROR(ident, "This is a failable expression, it can't go on the left hand side of a 'try'.");
			}
			return false;
		}

		// 3d. We can now analyse the expression using the variable type.
		if (!sema_analyse_expr(context, failable)) return false;

		if (!IS_OPTIONAL(failable))
		{
			SEMA_ERROR(failable, "Expected a failable expression to 'try' here. If it isn't a failable, remove 'try'.");
			return false;
		}

		if (!cast_implicit(context, failable, ident->type)) return false;

		expr->try_unwrap_expr.assign_existing = true;
		expr->try_unwrap_expr.lhs = ident;
	}
	else
	{
		// 4. We are creating a new variable

		// 4a. If we had a variable type, then our expression must be an identifier.
		if (ident->expr_kind != EXPR_IDENTIFIER)
		{
			SEMA_ERROR(ident, "A variable name was expected here.");
			return false;
		}

		assert(ident->resolve_status != RESOLVE_DONE);
		if (ident->identifier_expr.path)
		{
			SEMA_ERROR(ident->identifier_expr.path, "The variable may not have a path.");
			return false;
		}

		if (ident->identifier_expr.is_const)
		{
			SEMA_ERROR(ident, "Expected a variable starting with a lower case letter.");
			return false;
		}

		// 4b. Evaluate the expression
		if (!sema_analyse_expr(context, failable)) return false;

		if (!IS_OPTIONAL(failable))
		{
			SEMA_ERROR(failable, "Expected a failable expression to 'try' here. If it isn't a failable, remove 'try'.");
			return false;
		}

		if (var_type)
		{
			if (!cast_implicit(context, failable, var_type->type)) return false;
		}

		// 4c. Create a type_info if needed.
		if (!var_type)
		{
			var_type = type_info_new_base(failable->type->failable, failable->span);
		}

		// 4d. A new declaration is created.
		Decl *decl = decl_new_var(ident->identifier_expr.ident, ident->span, var_type, VARDECL_LOCAL, VISIBLE_LOCAL);

		// 4e. Analyse it
		if (!sema_analyse_var_decl(context, decl, true)) return false;

		expr->try_unwrap_expr.decl = decl;
	}

	expr->try_unwrap_expr.failable = failable;
	expr->type = type_bool;
	expr->resolve_status = RESOLVE_DONE;
	return true;
}


static inline bool sema_analyse_try_unwrap_chain(SemaContext *context, Expr *expr, CondType cond_type)
{
	assert(cond_type == COND_TYPE_UNWRAP_BOOL || cond_type == COND_TYPE_UNWRAP);

	assert(expr->expr_kind == EXPR_TRY_UNWRAP_CHAIN);
	Expr **chain = expr->try_unwrap_chain_expr;

	VECEACH(expr->try_unwrap_chain_expr, i)
	{
		Expr *chain_element = chain[i];
		if (chain_element->expr_kind == EXPR_TRY_UNWRAP)
		{
			if (!sema_analyse_try_unwrap(context, chain_element)) return false;
			continue;
		}
		if (!sema_analyse_cond_expr(context, chain_element)) return false;
	}
	expr->type = type_bool;
	expr->resolve_status = RESOLVE_DONE;
	return true;
}
static inline bool sema_analyse_catch_unwrap(SemaContext *context, Expr *expr)
{
	Expr *ident = expr->catch_unwrap_expr.variable;

	bool implicit_declaration = false;
	TypeInfo *type = expr->catch_unwrap_expr.type;

	if (!type && !ident)
	{
		expr->catch_unwrap_expr.lhs = NULL;
		expr->catch_unwrap_expr.decl = NULL;
		goto RESOLVE_EXPRS;
	}
	if (!type && ident->expr_kind == EXPR_IDENTIFIER)
	{
		implicit_declaration = !sema_symbol_is_defined_in_scope(context, ident->identifier_expr.ident);
	}

	if (!type && !implicit_declaration)
	{
		if (!sema_analyse_expr_lvalue(context, ident)) return false;

		if (!sema_expr_check_assign(context, ident)) return false;

		if (ident->type->canonical != type_anyerr)
		{
			SEMA_ERROR(ident, "Expected the variable to have the type %s, not %s.", type_quoted_error_string(type_anyerr),
			           type_quoted_error_string(ident->type));
			return false;
		}

		expr->catch_unwrap_expr.lhs = ident;
		expr->catch_unwrap_expr.decl = NULL;
	}
	else
	{
		type = type ? type : type_info_new_base(type_anyerr, expr->span);

		if (!sema_resolve_type_info(context, type)) return false;

		if (type->type->canonical != type_anyerr)
		{
			SEMA_ERROR(type, "Expected the type to be %s, not %s.", type_quoted_error_string(type_anyerr),
			           type_quoted_error_string(type->type));
			return false;
		}
		if (ident->expr_kind != EXPR_IDENTIFIER)
		{
			SEMA_ERROR(ident, "A variable name was expected here.");
			return false;
		}

		assert(ident->resolve_status != RESOLVE_DONE);
		if (ident->identifier_expr.path)
		{
			SEMA_ERROR(ident->identifier_expr.path, "The variable may not have a path.");
			return false;
		}

		if (ident->identifier_expr.is_const)
		{
			SEMA_ERROR(ident, "Expected a variable starting with a lower case letter.");
			return false;
		}

		// 4d. A new declaration is created.
		Decl *decl = decl_new_var(ident->identifier_expr.ident, ident->span, type, VARDECL_LOCAL, VISIBLE_LOCAL);
		decl->var.no_init = true;

		// 4e. Analyse it
		if (!sema_analyse_var_decl(context, decl, true)) return false;

		expr->catch_unwrap_expr.decl = decl;
		expr->catch_unwrap_expr.lhs = NULL;
	}
RESOLVE_EXPRS:;
	Expr **exprs = expr->catch_unwrap_expr.exprs;
	VECEACH(exprs, i)
	{
		Expr *fail = exprs[i];
		if (!sema_analyse_expr(context, fail)) return false;
		if (!type_is_optional(fail->type))
		{
			SEMA_ERROR(fail, "This expression is not failable, did you add it by mistake?");
			return false;
		}
	}
	expr->type = type_anyerr;
	expr->resolve_status = RESOLVE_DONE;
	return true;
}

static void sema_remove_unwraps_from_try(SemaContext *c, Expr *cond)
{
	assert(cond->expr_kind == EXPR_COND);
	Expr *last = VECLAST(cond->cond_expr);
	if (!last || last->expr_kind != EXPR_TRY_UNWRAP_CHAIN) return;
	Expr **chain = last->try_unwrap_chain_expr;
	VECEACH(chain, i)
	{
		Expr *expr = chain[i];
		if (expr->expr_kind != EXPR_TRY_UNWRAP) continue;
		if (expr->try_unwrap_expr.assign_existing) continue;
		if (expr->try_unwrap_expr.failable)
		{
			sema_erase_var(c, expr->try_unwrap_expr.decl);
		}
		else
		{
			sema_erase_unwrapped(c, expr->try_unwrap_expr.decl);
		}
	}
}

static inline bool sema_analyse_last_cond(SemaContext *context, Expr *expr, CondType cond_type)
{
	switch (expr->expr_kind)
	{
		case EXPR_TRY_UNWRAP_CHAIN:
			if (cond_type != COND_TYPE_UNWRAP_BOOL && cond_type != COND_TYPE_UNWRAP)
			{
				SEMA_ERROR(expr, "Try unwrapping is only allowed inside of a 'while' or 'if' conditional.");
				return false;
			}
			return sema_analyse_try_unwrap_chain(context, expr, cond_type);
		case EXPR_CATCH_UNWRAP:
			if (cond_type != COND_TYPE_UNWRAP_BOOL && cond_type != COND_TYPE_UNWRAP)
			{
				SEMA_ERROR(expr, "Catch unwrapping is only allowed inside of a 'while' or 'if' conditional, maybe catch(...) will do what you need?");
				return false;
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
		// No variable on the lhs? Then it can't be a variant unwrap.
		Expr *left = exprptr(expr->binary_expr.left);
		if (left->resolve_status ==  RESOLVE_DONE || left->expr_kind != EXPR_IDENTIFIER || left->identifier_expr.path) goto NORMAL_EXPR;

		// Does the identifier exist in the parent scope?
		// then again it can't be a variant unwrap.
		if (sema_symbol_is_defined_in_scope(context, left->identifier_expr.ident)) goto NORMAL_EXPR;

		Expr *right = exprptr(expr->binary_expr.right);
		bool is_deref = right->expr_kind == EXPR_UNARY && right->unary_expr.operator == UNARYOP_DEREF;
		if (is_deref) right = right->unary_expr.expr;
		if (!sema_analyse_expr_rhs(context, NULL, right, false)) return false;
		if (right->type == type_get_ptr(type_any) && is_deref)
		{
			is_deref = false;
			right = exprptr(expr->binary_expr.right);
			if (!sema_analyse_expr_rhs(context, NULL, right, false)) return false;
		}
		if (right->type != type_any) goto NORMAL_EXPR;
		// Found an expansion here
		expr->expr_kind = EXPR_VARIANTSWITCH;
		expr->variant_switch.new_ident = left->identifier_expr.ident;
		expr->variant_switch.span = left->span;
		expr->variant_switch.variant_expr = right;
		expr->variant_switch.is_deref = is_deref;
		expr->variant_switch.is_assign = true;
		expr->resolve_status = RESOLVE_DONE;
		expr->type = type_typeid;
		return true;
	}
	if (!sema_analyse_expr(context, expr)) return false;
	if (expr->type != type_any) return true;
	if (expr->expr_kind == EXPR_IDENTIFIER)
	{
		Decl *decl = expr->identifier_expr.decl;
		expr->expr_kind = EXPR_VARIANTSWITCH;
		expr->variant_switch.is_deref = false;
		expr->variant_switch.is_assign = false;
		expr->variant_switch.variable = decl;
		expr->type = type_typeid;
		expr->resolve_status = RESOLVE_DONE;
		return true;
	}
	return true;

NORMAL_EXPR:
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
static inline bool sema_analyse_cond_list(SemaContext *context, Expr *expr, CondType cond_type)
{
	assert(expr->expr_kind == EXPR_COND);

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

	if (!sema_analyse_last_cond(context, dexprs[entries - 1], cond_type)) return false;

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
static inline bool sema_analyse_cond(SemaContext *context, Expr *expr, CondType cond_type)
{
	bool cast_to_bool = cond_type == COND_TYPE_UNWRAP_BOOL;
	assert(expr->expr_kind == EXPR_COND && "Conditional expressions should always be of type EXPR_DECL_LIST");

	// 1. Analyse the declaration list.
	if (!sema_analyse_cond_list(context, expr, cond_type)) return false;

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
		// 3e. Expect that it isn't a failable
		if (IS_OPTIONAL(init) && !decl->var.unwrap)
		{
			return sema_error_failed_cast(last, last->type, cast_to_bool ? type_bool : init->type);
			return false;
		}
		// TODO document
		if (!decl->var.unwrap && cast_to_bool && cast_to_bool_kind(decl->var.type_info->type) == CAST_ERROR)
		{
			SEMA_ERROR(last->decl_expr->var.init_expr, "The expression needs to be convertible to a boolean.");
			return false;
		}
		return true;
	}
	// 3a. Check for failables in case of an expression.
	if (IS_OPTIONAL(last))
	{
		if (!cast_to_bool || cast_may_implicit(type_no_optional(last->type), type_bool, CAST_OPTION_NONE))
		{
			SEMA_ERROR(last, "The expression may not be a failable, but was %s.", type_quoted_error_string(last->type));
			return false;
		}
		sema_error_failed_cast(last, type_no_optional(last->type), type_bool);
		return false;
	}
	// 3b. Cast to bool if that is needed
	if (cast_to_bool)
	{
		if (!cast_implicit(context, last, type_bool)) return false;
	}
	return true;
}


static inline bool sema_analyse_declare_stmt(SemaContext *context, Ast *statement)
{
	VarDeclKind kind = statement->declare_stmt->var.kind;
	if (kind == VARDECL_LOCAL_CT_TYPE || kind == VARDECL_LOCAL_CT)
	{
		if (!sema_analyse_var_decl_ct(context, statement->declare_stmt)) return false;
		statement->ast_kind = AST_NOP_STMT;
		return true;
	}
	return sema_analyse_var_decl(context, statement->declare_stmt, true);
}

static inline bool sema_analyse_expr_stmt(SemaContext *context, Ast *statement)
{
	Expr *expr = statement->expr_stmt;
	if (expr->expr_kind == EXPR_CALL)
	{
		expr->call_expr.result_unused = true;
	}
	if (!sema_analyse_expr(context, expr)) return false;
	// Remove all const statements.
	if (expr_is_const(expr))
	{
		statement->ast_kind = AST_NOP_STMT;
	}
	return true;
}

bool sema_analyse_defer_stmt_body(SemaContext *context, Ast *statement, Ast *body)
{
	if (body->ast_kind == AST_DEFER_STMT)
	{
		SEMA_ERROR(body, "A defer may not have a body consisting of a raw 'defer', this looks like a mistake.");
		return false;
	}
	// TODO special parsing of "catch"
	bool success;
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
	// TODO special parsing of "catch"
	if (!sema_analyse_defer_stmt_body(context, statement, astptr(statement->defer_stmt.body))) return false;

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
	if (cond->expr_kind == EXPR_COND)
	{
		if (!sema_analyse_cond(context, cond, COND_TYPE_UNWRAP_BOOL)) return false;
	}
	else
	{
		if (!sema_analyse_cond_expr(context, cond)) return false;
	}

	// If this is const true, then set this to infinite and remove the expression.
	Expr *cond_last = cond->expr_kind == EXPR_COND ? VECLAST(cond->cond_expr) : cond;
	assert(cond_last);
	if (cond_last->expr_kind == EXPR_CONST && cond_last->const_expr.b)
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
	assert(body);
	if (body->ast_kind == AST_DEFER_STMT)
	{
		SEMA_ERROR(body, "Looping over a raw 'defer' is not allowed, was this a mistake?");
		return false;
	}
	bool do_loop = statement->for_stmt.flow.skip_first;
	if (body->ast_kind != AST_COMPOUND_STMT && do_loop)
	{
		SEMA_ERROR(body, "A do loop must use { } around its body.");
		return false;
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
				assert(!statement->for_stmt.cond);
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

	if (statement->for_stmt.flow.no_exit || (is_infinite && !statement->for_stmt.flow.has_break))
	{
		context->active_scope.jump_end = true;
	}
	return success;
}

static inline bool sema_analyse_foreach_stmt(SemaContext *context, Ast *statement)
{
	// Pull out the relevant data.
	DeclId index_id = statement->foreach_stmt.index;
	DeclId var_id = statement->foreach_stmt.variable;
	Decl *var = var_id ? declptr(var_id) : NULL;
	Decl *index = index_id ? declptr(index_id) : NULL;
	Expr *enumerator = exprptr(statement->foreach_stmt.enumeration);
	AstId body = statement->foreach_stmt.body;
	AstId first_stmt = 0;
	AstId *succ = &first_stmt;
	Expr **expressions = NULL;
	bool is_reverse = statement->foreach_stmt.is_reverse;
	bool value_by_ref = statement->foreach_stmt.value_by_ref;
	bool success = true;

	// First fold the enumerator expression, removing any () around it.
	while (enumerator->expr_kind == EXPR_GROUP) enumerator = enumerator->inner_expr;

	bool iterator_based = false;

	// Conditional scope start
	SCOPE_START

		// In the case of foreach (int x : { 1, 2, 3 }) we will infer the int[] type, so pick out the number of elements.
		Type *inferred_type = NULL;

		// We may have an initializer list, in this case we rely on an inferred type.
		if (expr_is_init_list(enumerator) || (enumerator->expr_kind == EXPR_CONST && enumerator->const_expr.const_kind == CONST_INITIALIZER))
		{
			bool may_be_array;
			bool is_const_size;
			MemberIndex size = sema_get_initializer_const_array_size(context, enumerator, &may_be_array, &is_const_size);
			if (!may_be_array)
			{
				SEMA_ERROR(enumerator,
				           "This initializer appears to be a struct initializer when, an array initializer was expected.");
				return SCOPE_POP_ERROR();
			}
			if (!is_const_size)
			{
				SEMA_ERROR(enumerator, "Only constant sized initializers may be implicitly initialized.");
				return SCOPE_POP_ERROR();
			}
			if (size < 0)
			{
				SEMA_ERROR(enumerator, "The initializer mixes designated initialization with array initialization.");
				return SCOPE_POP_ERROR();
			}
			assert(size >= 0);

			TypeInfo *variable_type_info = var->var.type_info;

			if (!variable_type_info)
			{
				SEMA_ERROR(var, "Add the type of your variable here if you want to iterate over an initializer list.");
				return SCOPE_POP_ERROR();
			}
			// First infer the type of the variable.
			if (!sema_resolve_type_info(context, variable_type_info)) return false;
			// And create the inferred type:
			inferred_type = type_get_array(var->var.type_info->type, (ArraySize)size);
		}

		// because we don't want the index + variable to move into the internal scope
		if (!sema_analyse_inferred_expr(context, inferred_type, enumerator))
		{
			// Exit early here, because semantic checking might be messed up otherwise.
			return SCOPE_POP_ERROR();
		}

		// And pop the cond scope.
	SCOPE_END;

	if (IS_OPTIONAL(enumerator))
	{
		SEMA_ERROR(enumerator, "The expression may not be failable.");
		return false;
	}

	if (statement->foreach_stmt.index_by_ref)
	{
		assert(index);
		SEMA_ERROR(index, "The index cannot be held by reference, did you accidentally add a '&'?");
		return false;
	}

	// Insert a single deref as needed.
	Type *flattened_type = enumerator->type->canonical;
	if (flattened_type->type_kind == TYPE_POINTER)
	{
		// Something like Foo** will not be dereferenced, only Foo*
		if (flattened_type->pointer->type_kind == TYPE_POINTER)
		{
			SEMA_ERROR(enumerator, "It is not possible to enumerate an expression of type %s.", type_quoted_error_string(enumerator->type));
			return false;
		}
		expr_rewrite_insert_deref(enumerator);
	}

	// At this point we should have dereferenced any pointer or bailed.
	assert(!type_is_pointer(enumerator->type));

	// Check that we can even index this expression.

	Type *value_type = type_get_indexed_type(enumerator->type);
	if (value_type && value_by_ref) value_type = type_get_ptr(value_type);

	Decl *len = NULL;
	Decl *index_macro = NULL;
	Type *index_type = type_usize;

	if (!value_type)
	{
		len = sema_find_operator(context, enumerator, OVERLOAD_LEN);
		Decl *by_val = sema_find_operator(context, enumerator, OVERLOAD_ELEMENT_AT);
		Decl *by_ref = sema_find_operator(context, enumerator, OVERLOAD_ELEMENT_REF);
		if (!len || (!by_val && !by_ref))
		{
			SEMA_ERROR(enumerator, "It's not possible to enumerate an expression of type %s.", type_quoted_error_string(enumerator->type));
			return false;
		}
		if (!by_ref && value_by_ref)
		{
			SEMA_ERROR(enumerator, "%s does not support 'foreach' with the value by reference.", type_quoted_error_string(enumerator->type));
			return false;
		}
		index_macro = value_by_ref ? by_ref : by_val;
		assert(index_macro);
		index_type = index_macro->func_decl.signature.params[1]->type;
		if (!type_is_integer(index_type))
		{
			SEMA_ERROR(enumerator, "Only integer indexed types may be used with foreach.");
			return false;
		}
		TypeInfoId rtype = index_macro->func_decl.signature.rtype;
		value_type = rtype ? type_infoptr(rtype)->type : NULL;
	}


	// Set up the value, assigning the type as needed.
	// Element *value = void
	if (!var->var.type_info)
	{
		var->var.type_info = type_info_new_base(value_type, var->span);
	}
	if (!sema_resolve_type_info(context, var->var.type_info)) return false;

	if (type_is_optional(var->var.type_info->type))
	{
		SEMA_ERROR(var->var.type_info, "The variable may not be a failable.");
		return false;
	}

	// Set up the optional index parameter
	Type *index_var_type = NULL;
	if (index)
	{
		if (!index->var.type_info) index->var.type_info = type_info_new_base(index_type, enumerator->span);
		if (!sema_resolve_type_info(context, index->var.type_info)) return false;
		index_var_type = index->var.type_info->type;
		if (type_is_optional(index_var_type))
		{
			SEMA_ERROR(index->var.type_info, "The index may not be a failable.");
			return false;
		}
		if (!type_is_integer(type_flatten(index_var_type)))
		{
			SEMA_ERROR(index->var.type_info,
			           "Index must be an integer type, '%s' is not valid.",
			           type_to_error_string(index_var_type));
			return false;
		}
	}


	// We either have "foreach (x : some_var)" or "foreach (x : some_call())"
	// So we grab the former by address (implicit &) and the latter as the value.
	assert(enumerator->resolve_status == RESOLVE_DONE);
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
		if (enumerator_type->type_kind == TYPE_ARRAY)
		{
			array_len = enumerator_type->array.len;
			len_call = NULL;
		}
		else
		{
			len_call = expr_new(EXPR_BUILTIN_ACCESS, enumerator->span);
			if (!sema_analyse_expr(context, enum_val)) return false;
			len_call->builtin_access_expr.inner = exprid(enum_val);
			len_call->builtin_access_expr.kind = ACCESS_LEN;
			len_call->resolve_status = RESOLVE_DONE;
			len_call->type = type_isize;
		}
	}
	Decl *idx_decl = decl_new_generated_var(index_type, VARDECL_LOCAL, index ? index->span : enumerator->span);

	// IndexType __len$ = (IndexType)(@__enum$.len())
	Decl *len_decl = NULL;

	if (is_reverse)
	{
		if (!len_call)
		{
			// Create const len if missing.
			len_call = expr_new_const_int(enumerator->span, type_isize, array_len, true);
		}
		if (!cast_implicit(context, len_call, index_type)) return false;
		// __idx$ = (IndexType)(@__enum$.len()) (or const)
		vec_add(expressions, expr_generate_decl(idx_decl, len_call));
	}
	else
	{
		if (len_call)
		{
			len_decl = decl_new_generated_var(index_type, VARDECL_LOCAL, enumerator->span);
			if (!cast_implicit(context, len_call, index_type)) return false;
			vec_add(expressions, expr_generate_decl(len_decl, len_call));
		}
		Expr *idx_init = expr_new_const_int(idx_decl->span, index_type, 0, true);
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
		Expr *rhs = expr_new_const_int(enumerator->span, index_type, 0, true);
		cond->binary_expr.right = exprid(rhs);

		// Create --__idx$
		Expr *dec = expr_new(EXPR_UNARY, idx_decl->span);
		dec->unary_expr.expr = expr_variable(idx_decl);
		dec->unary_expr.operator = UNARYOP_DEC;
		Ast *update_stmt = new_ast(AST_EXPR_STMT, idx_decl->span);
		update_stmt->expr_stmt = dec;
		ast_append(&succ, update_stmt);
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
			Expr *rhs = expr_new_const_int(enumerator->span, type_isize, array_len, true);
			cond->binary_expr.right = exprid(rhs);
		}

		// Create ++__idx$
		update = expr_new(EXPR_UNARY, idx_decl->span);
		update->unary_expr.expr = expr_variable(idx_decl);
		update->unary_expr.operator = UNARYOP_INC;
	}

	// Create IndexType index = __idx$
	if (index)
	{
		Ast *declare_ast = new_ast(AST_DECLARE_STMT, var->span);
		declare_ast->declare_stmt = index;
		Expr *load_idx = expr_variable(idx_decl);
		if (!cast(load_idx, index_var_type)) return false;
		index->var.init_expr = load_idx;
		ast_append(&succ, declare_ast);
	}

	// Create value = (*__$enum)[__idx$]
	Ast *value_declare_ast = new_ast(AST_DECLARE_STMT, var->span);
	value_declare_ast->declare_stmt = var;

	Expr *subscript = expr_new(EXPR_SUBSCRIPT, var->span);
	enum_val = expr_variable(temp);
	if (is_addr) expr_rewrite_insert_deref(enum_val);
	subscript->subscript_expr.expr = exprid(enum_val);
	subscript->subscript_expr.range.start = exprid(expr_variable(idx_decl));
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
	statement->for_stmt = (AstForStmt){ .init = exprid(init_expr),
										.cond = exprid(cond),
										.incr = update ? exprid(update) : 0,
										.flow = flow,
										.body = astid(compound_stmt)
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
		SEMA_ERROR(then, "An 'if' statement may not be followed by a raw 'defer' statement, this looks like a mistake.");
		return false;
	}
	AstId else_id = statement->if_stmt.else_body;
	Ast *else_body = else_id ? astptr(else_id) : NULL;
	SCOPE_OUTER_START
		CondType cond_type = then->ast_kind == AST_IF_CATCH_SWITCH_STMT
		                     ? COND_TYPE_UNWRAP : COND_TYPE_UNWRAP_BOOL;
		success = sema_analyse_cond(context, cond, cond_type);

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
			SEMA_ERROR(then, "This code can never be executed.");
			success = false;
		}

		if (then->ast_kind == AST_IF_CATCH_SWITCH_STMT)
		{
			Decl *label = statement->if_stmt.flow.label;
			then->switch_stmt.flow.label = label;
			statement->if_stmt.flow.label = NULL;
			if (label) label->label.parent = astid(then);
			SCOPE_START_WITH_LABEL(statement->if_stmt.flow.label);
				success = success && sema_analyse_switch_stmt(context, then);
				then_jump = context->active_scope.jump_end;
			SCOPE_END;
		}
		else
		{
			SCOPE_START_WITH_LABEL(statement->if_stmt.flow.label);
				success = success && sema_analyse_statement(context, then);
				then_jump = context->active_scope.jump_end;
			SCOPE_END;
		}

		else_jump = false;
		if (statement->if_stmt.else_body)
		{
			SCOPE_START_WITH_LABEL(statement->if_stmt.flow.label);
				sema_remove_unwraps_from_try(context, cond);
				sema_unwrappable_from_catch_in_else(context, cond);
				success = success && sema_analyse_statement(context, else_body);
				else_jump = context->active_scope.jump_end;
			SCOPE_END;
		}

		context_pop_defers_and_replace_ast(context, statement);

	SCOPE_OUTER_END;
	if (then_jump)
	{
		sema_unwrappable_from_catch_in_else(context, cond);
	}
	if (then_jump && else_jump && !statement->flow.has_break)
	{
		context->active_scope.jump_end = true;
	}
	return success;
}

static bool sema_analyse_asm_string_stmt(SemaContext *context, Ast *stmt)
{
	Expr *body = exprptr(stmt->asm_block_stmt.asm_string);
	if (!sema_analyse_expr(context, body)) return false;
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
	Label *label = stmt->ast_kind == AST_NEXT_STMT ? &stmt->nextcase_stmt.label : &stmt->contbreak_stmt.label;
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
					case AST_NEXT_STMT:
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
					case AST_NEXT_STMT:
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
				case AST_NEXT_STMT:
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
	if (!context->next_target && !statement->nextcase_stmt.label.name && !statement->nextcase_stmt.expr)
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

	// TODO test that nextcase out of a switch using label correctly creates defers.
	Ast *parent = context->next_switch;
	if (statement->nextcase_stmt.label.name)
	{
		Decl *target = sema_analyse_label(context, statement);
		if (!decl_ok(target)) return false;
		parent = astptr(target->label.parent);
		AstKind kind = parent->ast_kind;
		if (kind != AST_SWITCH_STMT && kind != AST_IF_CATCH_SWITCH_STMT)
		{
			SEMA_ERROR(&statement->nextcase_stmt.label, "Expected the label to match a 'switch' or 'if-catch' statement.");
			return false;
		}
	}
	if (!parent)
	{
		SEMA_ERROR(statement, "No matching switch could be found.");
		return false;
	}

	// Plain next.
	if (!statement->nextcase_stmt.expr)
	{
		assert(context->next_target);
		statement->nextcase_stmt.defer_id = context_get_defers(context, context->active_scope.defer_last, parent->switch_stmt.defer);
		statement->nextcase_stmt.case_switch_stmt = astid(context->next_target);
		return true;
	}

	Expr *cond = exprptr(parent->switch_stmt.cond);
	if (statement->nextcase_stmt.expr->expr_kind == EXPR_TYPEINFO)
	{
		TypeInfo *type_info = statement->nextcase_stmt.expr->type_expr;
		if (!sema_resolve_type_info(context, type_info)) return false;
		Ast **cases;
		statement->nextcase_stmt.defer_id = context_get_defers(context, context->active_scope.defer_last, parent->switch_stmt.defer);
		if (cond->type->canonical != type_typeid)
		{
			SEMA_ERROR(statement, "Unexpected 'type' in as an 'nextcase' destination.");
			SEMA_NOTE(statement, "The 'switch' here uses expected a type '%s'.", type_to_error_string(cond->type));
			return false;
		}
		cases = parent->switch_stmt.cases;

		Ast *default_stmt = NULL;
		Type *type = type_info->type->canonical;
		VECEACH(cases, i)
		{
			Ast *case_stmt = cases[i];
			if (case_stmt->ast_kind == AST_DEFAULT_STMT)
			{
				default_stmt = case_stmt;
				break;
			}
			Expr *expr = case_stmt->case_stmt.expr;
			if (expr->expr_kind == EXPR_CONST && expr->const_expr.typeid == type)
			{
				statement->nextcase_stmt.case_switch_stmt = astid(case_stmt);
				return true;
			}
		}
		if (default_stmt)
		{
			statement->nextcase_stmt.case_switch_stmt = astid(default_stmt);
			return true;
		}
		SEMA_ERROR(type_info, "There is no case for type '%s'.", type_to_error_string(type_info->type));
		return false;
	}

	Expr *target = statement->nextcase_stmt.expr;

	Type *expected_type = parent->ast_kind == AST_SWITCH_STMT ? cond->type : type_anyerr;

	if (!sema_analyse_expr_rhs(context, expected_type, target, false)) return false;

	statement->nextcase_stmt.defer_id = context_get_defers(context, context->active_scope.defer_last, parent->switch_stmt.defer);

	if (target->expr_kind == EXPR_CONST)
	{
		Ast *default_stmt = NULL;
		VECEACH(parent->switch_stmt.cases, i)
		{
			Ast *case_stmt = parent->switch_stmt.cases[i];
			if (case_stmt->ast_kind == AST_DEFAULT_STMT)
			{
				default_stmt = case_stmt;
				break;
			}
			ExprConst *const_expr = &case_stmt->case_stmt.expr->const_expr;
			ExprConst *to_const_expr = case_stmt->case_stmt.to_expr ? &case_stmt->case_stmt.to_expr->const_expr : const_expr;
			if (expr_const_in_range(&target->const_expr, const_expr, to_const_expr))
			{
				statement->nextcase_stmt.case_switch_stmt = astid(case_stmt);
				return true;
			}
		}
		if (default_stmt)
		{
			statement->nextcase_stmt.case_switch_stmt = astid(default_stmt);
			return true;
		}
		SEMA_ERROR(statement, "'nextcase' needs to jump to an exact case statement.");
		return false;
	}

	statement->nextcase_stmt.case_switch_stmt = astid(parent);
	statement->nextcase_stmt.switch_expr = target;
	return true;
}



static inline bool sema_analyse_then_overwrite(SemaContext *context, Ast *statement, AstId replacement)
{
	statement->ast_kind = AST_NOP_STMT;
	if (!replacement)
	{
		return true;
	}
	AstId current = replacement;
	Ast *last;
	while (1)
	{
		Ast *curr_ast = astptr(current);
		if (!sema_analyse_statement(context, curr_ast)) return false;
		current = curr_ast->next;
		if (!current)
		{
			last = curr_ast;
			break;
		}
	}
	// NOP and insert after.
	last->next = statement->next;
	statement->next = replacement;
	return true;
}


static inline bool sema_analyse_ct_if_stmt(SemaContext *context, Ast *statement)
{
	unsigned ct_context = sema_context_push_ct_stack(context);
	int res = sema_check_comp_time_bool(context, statement->ct_if_stmt.expr);
	if (res == -1) goto FAILED;
	if (res)
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
		assert(elif->ast_kind == AST_CT_IF_STMT);

		res = sema_check_comp_time_bool(context, elif->ct_if_stmt.expr);
		if (res == -1) goto FAILED;
		if (res)
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
	AstId *next = ast ? &ast->next : &compound_statement->compound_stmt.first_stmt;
	context_pop_defers(context, next);
	return all_ok;
}

static inline bool sema_check_type_case(SemaContext *context, Type *switch_type, Ast *case_stmt, Ast **cases, unsigned index)
{
	Expr *expr = case_stmt->case_stmt.expr;
	if (!sema_analyse_expr_rhs(context, type_typeid, expr, false)) return false;

	if (expr->expr_kind == EXPR_CONST)
	{
		Type *my_type = expr->const_expr.typeid;
		for (unsigned i = 0; i < index; i++)
		{
			Ast *other = cases[i];
			if (other->ast_kind != AST_CASE_STMT) continue;
			Expr *other_expr = other->case_stmt.expr;
			if (other_expr->expr_kind == EXPR_CONST && other_expr->const_expr.typeid == my_type)
			{
				SEMA_ERROR(case_stmt, "The same type appears more than once.");
				SEMA_NOTE(other, "Here is the case with that type.");
				return false;
			}
		}
	}
	return true;
}

static inline bool sema_check_value_case(SemaContext *context, Type *switch_type, Ast *case_stmt, Ast **cases, unsigned index, bool *if_chained, bool *max_ranged)
{
	assert(switch_type);
	Expr *expr = case_stmt->case_stmt.expr;
	Expr *to_expr = case_stmt->case_stmt.to_expr;

	// 1. Try to do implicit conversion to the correct type.
	if (!sema_analyse_expr_rhs(context, switch_type, expr, false)) return false;
	if (to_expr && !sema_analyse_expr_rhs(context, switch_type, to_expr, false)) return false;

	if (expr->expr_kind != EXPR_CONST || (to_expr && to_expr->expr_kind != EXPR_CONST))
	{
		*if_chained = true;
		return true;
	}
	ExprConst *const_expr = &expr->const_expr;
	ExprConst *to_const_expr = to_expr ? &to_expr->const_expr : const_expr;

	if (!*max_ranged && to_const_expr != const_expr)
	{
		if (const_expr->const_kind == CONST_ENUM)
		{
			assert(to_const_expr->const_kind == CONST_ENUM);
			if (to_const_expr->enum_err_val->enum_constant.ordinal < const_expr->enum_err_val->enum_constant.ordinal)
			{
				sema_error_at(extend_span_with_token(expr->span, to_expr->span),
				              "A enum range must be have the enum with a lower ordinal followed by the one with a higher ordinal. "
				              "It would work if you swapped their order.");
				return false;
			}
		}
		else if (type_is_integer(expr->type))
		{
			if (int_comp(const_expr->ixx, to_const_expr->ixx, BINARYOP_GT))
			{
				sema_error_at(extend_span_with_token(expr->span, to_expr->span),
				              "The range is not valid because the first value (%s) is greater than the second (%s). "
				              "It would work if you swapped their order.",
				              int_to_str(const_expr->ixx, 10),
				              int_to_str(to_const_expr->ixx, 10));
				return false;
			}
			Int128 range = int_sub(to_const_expr->ixx, const_expr->ixx).i;
			Int128 max_range = { .low = active_target.switchrange_max_size };
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
		ExprConst *other_const = &other->case_stmt.expr->const_expr;
		ExprConst *other_to_const = other->case_stmt.to_expr ? &other->case_stmt.to_expr->const_expr : other_const;
		if (expr_const_in_range(const_expr, other_const, other_to_const))
		{
			SEMA_ERROR(case_stmt, "The same case value appears more than once.");
			SEMA_NOTE(other, "Here is the previous use of that value.");
			return false;
		}
	}
	return true;
}

static bool sema_analyse_switch_body(SemaContext *context, Ast *statement, SourceSpan expr_span, Type *switch_type, Ast **cases, ExprVariantSwitch *variant, Decl *var_holder)
{
	bool use_type_id = false;
	if (!type_is_comparable(switch_type))
	{
		sema_error_at(expr_span, "You cannot test '%s' for equality, and only values that supports '==' for comparison can be used in a switch.", type_to_error_string(switch_type));
		return false;
	}
	// We need an if chain if this isn't an integer type.
	bool if_chain = !type_is_integer(type_flatten(switch_type));

	Ast *default_case = NULL;
	assert(context->active_scope.defer_start == context->active_scope.defer_last);

	bool exhaustive = false;
	unsigned case_count = vec_size(cases);
	bool success = true;
	bool max_ranged = false;
	bool type_switch = switch_type == type_typeid;
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
						break;;
					}
				}
				else
				{
					if (!sema_check_value_case(context, switch_type, stmt, cases, i, &if_chain, &max_ranged))
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

	bool all_jump_end = exhaustive;
	for (unsigned i = 0; i < case_count; i++)
	{
		Ast *stmt = cases[i];
		SCOPE_START
			PUSH_BREAK(statement);
			Ast *next = (i < case_count - 1) ? cases[i + 1] : NULL;
			PUSH_NEXT(next, statement);
			Ast *body = stmt->case_stmt.body;
			if (stmt->ast_kind == AST_CASE_STMT && body && type_switch && var_holder && stmt->case_stmt.expr->expr_kind == EXPR_CONST)
			{
				if (variant->is_assign)
				{
					Type *real_type = type_get_ptr(stmt->case_stmt.expr->const_expr.typeid);
					Decl *new_var = decl_new_var(variant->new_ident, variant->span,
												 type_info_new_base(variant->is_deref
					                             ? real_type->pointer : real_type, variant->span),
					                             VARDECL_LOCAL, VISIBLE_LOCAL);
					Expr *var_result = expr_variable(var_holder);
					if (!cast(var_result, real_type)) return false;
					if (variant->is_deref)
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
					Type *type = type_get_ptr(stmt->case_stmt.expr->const_expr.typeid);
					Decl *alias = decl_new_var(var_holder->name, var_holder->span,
											   type_info_new_base(type, stmt->case_stmt.expr->span),
											   VARDECL_LOCAL, VISIBLE_LOCAL);
					Expr *ident_converted = expr_variable(var_holder);
					if (!cast(ident_converted, type)) return false;
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
			all_jump_end &= (!body | context->active_scope.jump_end);
		SCOPE_END;
	}
	statement->flow.no_exit = all_jump_end;
	statement->switch_stmt.flow.if_chain = if_chain || max_ranged;
	if (!success) return false;
	return success;
}

static inline bool sema_analyse_ct_switch_stmt(SemaContext *context, Ast *statement)
{
	unsigned ct_context = sema_context_push_ct_stack(context);
	// Evaluate the switch statement
	Expr *cond = exprptr(statement->ct_switch_stmt.cond);
	if (!sema_analyse_ct_expr(context, cond)) goto FAILED;

	// If we have a type, then we do different evaluation
	// compared to when it is a value.
	Type *type = cond->type;
	bool is_type = false;
	switch (type_flatten(type)->type_kind)
	{
		case TYPE_TYPEID:
			is_type = true;
			FALLTHROUGH;
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_BOOL:
			break;
		case TYPE_POINTER:
			if (expr_is_const_string(cond)) break;
			FALLTHROUGH;
		default:
			SEMA_ERROR(cond, "Only types, strings, integers, floats and booleans may be used with '$switch'.");
			goto FAILED;
	}

	ExprConst *switch_expr_const = &cond->const_expr;
	Ast **cases = statement->ct_switch_stmt.body;

	unsigned case_count = vec_size(cases);
	assert(case_count <= INT32_MAX);
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

				Expr *expr = stmt->case_stmt.expr;
				Expr *to_expr = stmt->case_stmt.to_expr;
				if (to_expr)
				{
					if (!type_is_integer(type) && !type_is_float(type))
					{
						SEMA_ERROR(to_expr, "$case ranges are only allowed for floats and integers.");
						goto FAILED;
					}
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
					if (!sema_analyse_expr_rhs(context, type, expr, false)) goto FAILED;
					if (to_expr && !sema_analyse_expr_rhs(context, type, to_expr, false)) goto FAILED;
				}
				if (!expr_is_const(expr))
				{
					SEMA_ERROR(expr, "The $case must have a constant expression.");
					goto FAILED;
				}
				if (to_expr && !expr_is_const(to_expr))
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
					ExprConst *other_const = &other_stmt->case_stmt.expr->const_expr;
					ExprConst *other_const_to = other_stmt->case_stmt.to_expr ? &other_stmt->case_stmt.to_expr->const_expr : other_const;
					if (expr_const_in_range(const_expr, other_const, other_const_to))
					{
						SEMA_ERROR(stmt, "'%s' appears more than once.", expr_const_to_error_string(const_expr));
						SEMA_NOTE(cases[j]->case_stmt.expr, "The previous $case was here.");
						goto FAILED;
					}
				}
				if (expr_const_in_range(switch_expr_const, const_expr, const_to_expr))
				{
					matched_case = (int) i;
				}
				break;
			}
			case AST_DEFAULT_STMT:
				if (default_case < case_count)
				{
					SEMA_ERROR(stmt, "More than one $default is not allowed.");
					SEMA_NOTE(cases[default_case], "The previous $default was here.");
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
	if (!expr_is_const_untyped_list(collection) && !expr_is_const_initializer(collection))
	{
		SEMA_ERROR(collection, "Expected a list to iterate over");
		goto FAILED;
	}
	unsigned count;
	ConstInitializer *initializer = NULL;
	Expr **expressions = NULL;
	Type *const_list_type = NULL;
	if (expr_is_const_initializer(collection))
	{
		initializer = collection->const_expr.initializer;
		ConstInitType init_type = initializer->kind;
		const_list_type = type_flatten(collection->type);
		if (const_list_type->type_kind == TYPE_ARRAY)
		{
			count = const_list_type->array.len;
		}
		else
		{
			// Empty list
			if (init_type == CONST_INIT_ZERO)
			{
				sema_context_pop_ct_stack(context, ct_context);
				return true;
			}
			if (init_type != CONST_INIT_ARRAY_FULL)
			{
				SEMA_ERROR(collection, "Only regular arrays are allowed here.");
				goto FAILED;
			}
			count = vec_size(initializer->init_array_full);
		}
	}
	else
	{
		expressions = collection->const_expr.untyped_list;
		count = vec_size(expressions);
	}
	Decl *index = NULL;
	const char *index_name = statement->ct_foreach_stmt.index_name;

	AstId start = 0;
	if (index_name)
	{
		index = decl_new_var(index_name, statement->ct_foreach_stmt.index_span, NULL, VARDECL_LOCAL_CT, VISIBLE_LOCAL);
		index->type = type_int;
		if (!sema_add_local(context, index)) goto FAILED;
	}
	Decl *value = decl_new_var(statement->ct_foreach_stmt.value_name, statement->ct_foreach_stmt.value_span, NULL, VARDECL_LOCAL_CT, VISIBLE_LOCAL);
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
		else
		{
			Expr *expr = expr_new(EXPR_CONST, collection->span);
			if (!expr_rewrite_to_const_initializer_index(const_list_type, initializer, expr, i))
			{
				SEMA_ERROR(collection, "Complex expressions are not allowed.");
				goto FAILED;
			}
			value->var.init_expr = expr;
		}
		if (index)
		{
			index->var.init_expr = expr_new_const_int(index->span, type_int, i, true);
			index->type = type_int;
		}
		if (!sema_analyse_compound_stmt(context, compound_stmt)) goto FAILED;
		*current = astid(compound_stmt);
		current = &compound_stmt->next;
	}
	sema_context_pop_ct_stack(context, ct_context);
	statement->ast_kind = AST_COMPOUND_STMT;
	statement->compound_stmt.first_stmt = start;
	return true;
FAILED:
	sema_context_pop_ct_stack(context, ct_context);
	return false;
}

static inline bool sema_analyse_switch_stmt(SemaContext *context, Ast *statement)
{
	statement->switch_stmt.scope_defer = context->active_scope.in_defer;

	SCOPE_START_WITH_LABEL(statement->switch_stmt.flow.label);

		Expr *cond = exprptr(statement->switch_stmt.cond);
		Type *switch_type;

		ExprVariantSwitch var_switch;
		Decl *variant_decl = NULL;
		if (statement->ast_kind == AST_SWITCH_STMT)
		{
			if (!sema_analyse_cond(context, cond, COND_TYPE_EVALTYPE_VALUE)) return false;
			Expr *last = VECLAST(cond->cond_expr);
			switch_type = last->type->canonical;
			if (last->expr_kind == EXPR_VARIANTSWITCH)
			{
				var_switch = last->variant_switch;


				Expr *inner;
				if (var_switch.is_assign)
				{
					inner = expr_new(EXPR_DECL, last->span);
					variant_decl = decl_new_generated_var(type_any, VARDECL_LOCAL, last->span);
					variant_decl->var.init_expr = var_switch.variant_expr;
					inner->decl_expr = variant_decl;
					if (!sema_analyse_expr(context, inner)) return false;
				}
				else
				{
					inner = expr_new(EXPR_IDENTIFIER, last->span);
					variant_decl = var_switch.variable;
					inner->identifier_expr.decl = variant_decl;
					inner->type = type_any;
					inner->resolve_status = RESOLVE_DONE;
				}
				expr_rewrite_to_builtin_access(last, inner, ACCESS_TYPEOFANY, type_typeid);
				switch_type = type_typeid;
				cond->type = type_typeid;
			}

		}
		else
		{
			switch_type = type_anyerr;
		}

		statement->switch_stmt.defer = context->active_scope.defer_last;
		if (!sema_analyse_switch_body(context, statement, cond ? cond->span : statement->span,
		                              switch_type->canonical,
		                              statement->switch_stmt.cases, variant_decl ? &var_switch : NULL, variant_decl))
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
	Expr *expr = exprptr(statement->assert_stmt.expr);
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
	int res = sema_check_comp_time_bool(context, expr);

	if (res == -1) return false;
	if (!res)
	{
		if (message_expr)
		{
			SEMA_ERROR(expr, "Compile time assert - %.*s", EXPAND_EXPR_STRING(message_expr));
		}
		else
		{
			SEMA_ERROR(expr, "Compile time assert failed.");
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
			puts(int_to_str(message->const_expr.ixx, 10));
			break;
		case CONST_BOOL:
			puts(message->const_expr.b ? "true" : "false");
			break;
		case CONST_ENUM:
		case CONST_ERR:
			puts(message->const_expr.enum_err_val->name);
			break;
		case CONST_STRING:
			printf("%.*s\n", EXPAND_EXPR_STRING(message));
			break;
		case CONST_POINTER:
			printf("%p\n", (void*)message->const_expr.ptr);
			break;
		case CONST_TYPEID:
			puts(type_to_error_string(message->const_expr.typeid));
			break;
		case CONST_BYTES:
		case CONST_INITIALIZER:
		case CONST_UNTYPED_LIST:
		case CONST_MEMBER:
			SEMA_ERROR(message, "Unsupported type for '$echo'");
			break;
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
		assert(init_expr->expr_kind == EXPR_EXPRESSION_LIST);

		// Check the list of expressions.
		FOREACH_BEGIN(Expr *expr, init_expr->expression_list)
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
		FOREACH_END();
	}
	ExprId condition = statement->for_stmt.cond;
	ExprId incr = statement->for_stmt.incr;
	Ast *body = astptr(statement->for_stmt.body);
	AstId start = 0;
	AstId *current = &start;
	Expr **incr_list = incr ? exprptr(incr)->expression_list : NULL;
	assert(condition);
	// We set a maximum of macro iterations.
	// we might consider reducing this.
	unsigned current_ct_scope = sema_context_push_ct_stack(context);
	for (int i = 0; i < MAX_MACRO_ITERATIONS; i++)
	{
		sema_context_pop_ct_stack(context, current_ct_scope);
		// First evaluate the cond, which we note that we *must* have.
		// we need to make a copy
		Expr *copy = copy_expr_single(exprptr(condition));
		if (!sema_analyse_cond_expr(context, copy)) goto FAILED;
		if (!expr_is_const(copy))
		{
			SEMA_ERROR(copy, "Expected a value that can be evaluated at compile time.");
			goto FAILED;
		}
		// This is simple, since we know we have a boolean, just break if we reached "false"
		if (!copy->const_expr.b) break;

		// Otherwise we copy the body.
		Ast *compound_stmt = copy_ast_single(body);

		// Analyse the body
		if (!sema_analyse_compound_statement_no_scope(context, compound_stmt)) goto FAILED;

		// Append it.
		*current = astid(compound_stmt);
		current = &compound_stmt->next;

		// Copy and evaluate all the expressions in "incr"
		FOREACH_BEGIN(Expr *expr, incr_list)
			if (!sema_analyse_ct_expr(context, copy_expr_single(expr))) goto FAILED;
		FOREACH_END();
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
	if (statement->ast_kind == AST_POISONED)
	{
		return false;
	}
	if (context->active_scope.jump_end && !context->active_scope.allow_dead_code)
	{
		if (statement->ast_kind == AST_ASSERT_STMT)
		{
			context->active_scope.allow_dead_code = true;
			return true;
		}
		//ERROR_NODE(statement, "This code will never execute.");
		context->active_scope.allow_dead_code = true;
		//return false;
	}
	switch (statement->ast_kind)
	{
		case AST_POISONED:
		case AST_IF_CATCH_SWITCH_STMT:
		case AST_DOC_STMT:
		case AST_ASM_STMT:
			UNREACHABLE
		case AST_ASM_BLOCK_STMT:
			return sema_analyse_asm_stmt(context, statement);
		case AST_ASSERT_STMT:
			return sema_analyse_assert_stmt(context, statement);
		case AST_BREAK_STMT:
			return sema_analyse_break_stmt(context, statement);
		case AST_CASE_STMT:
			SEMA_ERROR(statement, "Unexpected 'case' outside of switch");
			return false;
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
			SEMA_ERROR(statement, "Unexpected 'default' outside of switch");
			return false;
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
		case AST_NEXT_STMT:
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
	if (sema_analyse_statement_inner(context, statement)) return true;
	return ast_poison(statement);
}


static bool sema_analyse_require(SemaContext *context, Ast *directive, AstId **asserts)
{
	return assert_create_from_contract(context, directive, asserts, INVALID_SPAN);
}

static bool sema_analyse_ensure(SemaContext *context, Ast *directive)
{
	Expr *declexpr = directive->doc_stmt.contract.decl_exprs;
	assert(declexpr->expr_kind == EXPR_EXPRESSION_LIST);

	VECEACH(declexpr->expression_list, j)
	{
		Expr *expr = declexpr->expression_list[j];
		if (expr->expr_kind == EXPR_DECL)
		{
			SEMA_ERROR(expr, "Only expressions are allowed.");
			return false;
		}
	}
	return true;
}

static bool sema_analyse_errors(SemaContext *context, Ast *directive)
{
	DocOptReturn *returns = directive->doc_stmt.optreturns;
	VECEACH(returns, i)
	{
		TypeInfo *type_info = returns[i].type;
		const char *ident = returns[i].ident;
		if (type_info->kind != TYPE_INFO_IDENTIFIER)
		{
			SEMA_ERROR(type_info, "Expected a fault name here.");
			return false;
		}
		if (!sema_resolve_type_info(context, type_info)) return false;
		Type *type = type_info->type;
		if (type->type_kind != TYPE_FAULTTYPE)
		{
			SEMA_ERROR(type_info, "A fault type is required.");
			return false;
		}
		if (!ident)
		{
			returns[i].decl = type->decl;
			goto NEXT;
		}
		Decl *decl = type->decl;
		Decl **enums = decl->enums.values;
		VECEACH(enums, j)
		{
			Decl *opt_value = enums[j];
			if (opt_value->name == ident)
			{
				returns[i].decl = opt_value;
				goto NEXT;
			}
		}
		sema_error_at(returns[i].span, "No fault value '%s' found.", ident);
		return false;
NEXT:;
	}
	return true;
}

static bool sema_analyse_checked(SemaContext *context, Ast *directive, AstId **asserts)
{
	Expr *declexpr = directive->doc_stmt.contract.decl_exprs;
	bool success = true;
	bool suppress_error = global_context.suppress_errors;
	global_context.suppress_errors = true;
	SCOPE_START_WITH_FLAGS(SCOPE_CHECKS)
		VECEACH(declexpr->cond_expr, j)
		{
			Expr *expr = declexpr->cond_expr[j];
			const char *comment = directive->doc_stmt.contract.comment;
			global_context.suppress_errors = comment != NULL;
			if (!sema_analyse_cond_expr(context, expr))
			{
				if (comment)
				{
					global_context.suppress_errors = false;
					SEMA_ERROR(expr, comment);
				}
				success = false;
				goto END;
			}
		}
END:
	SCOPE_END;
	global_context.suppress_errors = suppress_error;
	return success;
}

void sema_append_contract_asserts(AstId assert_first, Ast* compound_stmt)
{
	assert(compound_stmt->ast_kind == AST_COMPOUND_STMT);
	if (!assert_first) return;
	Ast *ast = new_ast(AST_COMPOUND_STMT, compound_stmt->span);
	ast->compound_stmt.first_stmt = assert_first;
	ast_prepend(&compound_stmt->compound_stmt.first_stmt, ast);
}

bool sema_analyse_contracts(SemaContext *context, AstId doc, AstId **asserts)
{
	while (doc)
	{
		Ast *directive = astptr(doc);
		switch (directive->doc_stmt.kind)
		{
			case DOC_DIRECTIVE_UNKNOWN:
			case DOC_DIRECTIVE_PURE:
				break;
			case DOC_DIRECTIVE_REQUIRE:
				if (!sema_analyse_require(context, directive, asserts)) return false;
				break;
			case DOC_DIRECTIVE_CHECKED:
				if (!sema_analyse_checked(context, directive, asserts)) return false;
				break;
			case DOC_DIRECTIVE_PARAM:
				break;
			case DOC_DIRECTIVE_ERRORS:
				if (!sema_analyse_errors(context, directive)) return false;
				break;
			case DOC_DIRECTIVE_ENSURE:
				if (!sema_analyse_ensure(context, directive)) return false;
				context->call_env.ensures = true;
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
	FunctionPrototype *prototype = func->type->function.prototype;
	context->call_env = (CallEnv) {
		.current_function = func,
		.kind = CALL_ENV_FUNCTION,
		.pure = func->func_decl.signature.attrs.is_pure
	};
	context->rtype = prototype->rtype;
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
	assert(func->func_decl.body);
	Ast *body = astptr(func->func_decl.body);
	SCOPE_START
		assert(context->active_scope.depth == 1);
		Decl **params = signature->params;
		VECEACH(params, i)
		{
			if (!sema_add_local(context, params[i])) return false;
		}
		AstId assert_first = 0;
		AstId *next = &assert_first;
		if (!sema_analyse_contracts(context, func->func_decl.docs, &next)) return false;
		if (func->func_decl.attr_naked)
		{
			AstId current = body->compound_stmt.first_stmt;
			while (current)
			{
				Ast *stmt = ast_next(&current);
				if (stmt->ast_kind != AST_ASM_STMT)
				{
					SEMA_ERROR(stmt, "Only asm statements are allowed inside of a naked function.");
					return false;
				}
			}
			assert_first = 0;
		}
		else
		{
			sema_append_contract_asserts(assert_first, body);
			Type *canonical_rtype = type_no_optional(prototype->rtype)->canonical;
			// Insert an implicit return
			if (canonical_rtype == type_void)
			{
				AstId *next_id = &body->compound_stmt.first_stmt;
				SourceSpan span = body->span;
				if (*next_id)
				{
					Ast *last = ast_last(astptr(*next_id));
					// Cleanup later
					if (last->ast_kind == AST_RETURN_STMT) goto SKIP_NEW_RETURN;
					span = last->span;
					next_id = &last->next;
				}
				Ast *ret = new_ast(AST_RETURN_STMT, span);
				ast_append(&next_id, ret);
				SKIP_NEW_RETURN:;
			}
			if (!sema_analyse_compound_statement_no_scope(context, body)) return false;
			assert(context->active_scope.depth == 1);
			if (!context->active_scope.jump_end)
			{
				SEMA_ERROR(func, "Missing return statement at the end of the function.");
				return false;
			}
		}
	SCOPE_END;
	return true;
}

