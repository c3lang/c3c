// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"

// --- Helper functions

static bool sema_analyse_compound_stmt(Context *context, Ast *statement);

typedef enum
{
	COND_TYPE_UNWRAP_BOOL,
	COND_TYPE_UNWRAP,
	COND_TYPE_EVALTYPE_VALUE,
} CondType;

static void sema_unwrappable_from_catch_in_else(Context *c, Expr *cond)
{
	assert(cond->expr_kind == EXPR_COND);

	Expr *last = VECLAST(cond->cond_expr);
	while (last->expr_kind == EXPR_CAST)
	{
		last = last->cast_expr.expr;
	}
	if (!last || last->expr_kind != EXPR_CATCH_UNWRAP) return;

	Expr **unwrapped = last->catch_unwrap_expr.exprs;

	VECEACH(unwrapped, i)
	{
		Expr *expr = unwrapped[i];
		if (expr->expr_kind != EXPR_IDENTIFIER) continue;
		Decl *decl = expr->identifier_expr.decl;
		if (decl->decl_kind != DECL_VAR) continue;
		assert(decl->type->type_kind == TYPE_FAILABLE && "The variable should always be failable at this point.");

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



static inline bool sema_analyse_block_return_stmt(Context *context, Ast *statement)
{
	assert(context->active_scope.flags & (SCOPE_EXPR_BLOCK | SCOPE_MACRO));
	context->active_scope.jump_end = true;
	if (statement->return_stmt.expr)
	{
		if (!sema_analyse_expr(context, statement->return_stmt.expr))
		{
			return false;
		}
	}
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
static inline bool sema_analyse_return_stmt(Context *context, Ast *statement)
{
	// This might be a return in a function block or a macro which must be treated differently.
	if (context->active_scope.flags & (SCOPE_EXPR_BLOCK | SCOPE_MACRO))
	{
		return sema_analyse_block_return_stmt(context, statement);
	}
	// 1. We mark that the current scope ends with a jump.
	context->active_scope.jump_end = true;

	Type *expected_rtype = context->rtype;
	assert(expected_rtype && "We should always have known type from a function return.");

	Expr *return_expr = statement->return_stmt.expr;
	statement->return_stmt.defer = context->active_scope.defer_last;

	// 2. First handle the plain return.
	if (return_expr == NULL)
	{
		if (type_no_fail(expected_rtype)->canonical != type_void)
		{
			SEMA_ERROR(statement, "Expected to return a result of type %s.", type_to_error_string(expected_rtype));
			return false;
		}
		return true;
	}

	// 3. Evaluate the return value to be the expected return type.
	if (!sema_analyse_expr_rhs(context, expected_rtype, return_expr, type_is_failable(expected_rtype))) return false;

	assert(type_no_fail(statement->return_stmt.expr->type)->canonical == type_no_fail(expected_rtype)->canonical);

	return true;
}

static inline bool sema_analyse_unreachable_stmt(Context *context)
{
	context->active_scope.jump_end = true;
	return true;
}

static inline bool sema_analyse_try_unwrap(Context *context, Expr *expr)
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
		if (!IS_FAILABLE(decl))
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
		Decl *decl = sema_resolve_normal_symbol(context, ident->identifier_expr.identifier, NULL, false);
		if (!decl) implicit_declaration = true;
	}

	// 2. If we have a type for the variable, resolve it.
	if (var_type)
	{
		if (!sema_resolve_type_info(context, var_type)) return false;
		if (IS_FAILABLE(var_type))
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
		if (!expr_is_ltype(ident))
		{
			SEMA_ERROR(ident, "'try' expected an assignable variable or expression here, did you make a mistake?");
			return false;
		}

		// 3c. It can't be failable either.
		if (IS_FAILABLE(ident))
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

		if (!IS_FAILABLE(failable))
		{
			SEMA_ERROR(failable, "Expected a failable expression to 'try' here. If it isn't a failable, remove 'try'.");
			return false;
		}

		if (!cast_implicit(failable, ident->type)) return false;

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

		if (ident->identifier_expr.path)
		{
			sema_error_range(ident->identifier_expr.path->span, "The variable may not have a path.");
			return false;
		}

		TokenId ident_token = ident->identifier_expr.identifier;

		if (TOKTYPE(ident_token) != TOKEN_IDENT)
		{
			SEMA_ERROR(ident, "Expected a variable starting with a lower case letter.");
			return false;
		}

		// 4b. Evaluate the expression
		if (!sema_analyse_expr(context, failable)) return false;

		if (!IS_FAILABLE(failable))
		{
			SEMA_ERROR(failable, "Expected a failable expression to 'try' here. If it isn't a failable, remove 'try'.");
			return false;
		}

		if (var_type)
		{
			if (!cast_implicit(failable, var_type->type)) return false;
		}

		// 4c. Create a type_info if needed.
		if (!var_type)
		{
			var_type = type_info_new_base(failable->type->failable, failable->span);
		}

		// 4d. A new declaration is created.
		Decl *decl = decl_new_var(ident_token, var_type, VARDECL_LOCAL, VISIBLE_LOCAL);

		// 4e. Analyse it
		if (!sema_analyse_var_decl(context, decl, true)) return false;

		expr->try_unwrap_expr.decl = decl;
	}

	expr->try_unwrap_expr.failable = failable;
	expr->type = type_bool;
	expr->resolve_status = RESOLVE_DONE;
	return true;
}


static inline bool sema_analyse_try_unwrap_chain(Context *context, Expr *expr, CondType cond_type)
{
	assert(cond_type == COND_TYPE_UNWRAP_BOOL || cond_type == COND_TYPE_UNWRAP);

	assert(expr->expr_kind == EXPR_TRY_UNWRAP_CHAIN);
	Expr **chain = expr->try_unwrap_chain_expr;
	unsigned elements = vec_size(chain);

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
static inline bool sema_analyse_catch_unwrap(Context *context, Expr *expr)
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
		Decl *decl = sema_resolve_normal_symbol(context, ident->identifier_expr.identifier, NULL, false);
		if (!decl) implicit_declaration = true;
	}

	if (!type && !implicit_declaration)
	{
		if (!sema_analyse_expr_lvalue(context, ident)) return false;

		if (!expr_is_ltype(ident))
		{
			SEMA_ERROR(ident, "'catch' expected an assignable variable or expression here, did you make a mistake?");
			return false;
		}

		if (ident->type->canonical != type_anyerr)
		{
			SEMA_ERROR(ident, "Expected the variable to have the type %s, not %s.", type_quoted_error_string(type_anyerr),
					   type_quoted_error_string(type->type));
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

		if (ident->identifier_expr.path)
		{
			sema_error_range(ident->identifier_expr.path->span, "The variable may not have a path.");
			return false;
		}

		TokenId ident_token = ident->identifier_expr.identifier;

		if (TOKTYPE(ident_token) != TOKEN_IDENT)
		{
			SEMA_ERROR(ident, "Expected a variable starting with a lower case letter.");
			return false;
		}

		// 4d. A new declaration is created.
		Decl *decl = decl_new_var(ident_token, type, VARDECL_LOCAL, VISIBLE_LOCAL);
		decl->var.init_expr = expr_new(EXPR_UNDEF, decl->span);

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
		if (!type_is_failable(fail->type))
		{
			SEMA_ERROR(fail, "This expression is not failable, did you add it by mistake?");
			return false;
		}
	}
	expr->type = type_anyerr;
	expr->resolve_status = RESOLVE_DONE;
	return true;
}

static void sema_remove_unwraps_from_try(Context *c, Expr *cond)
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

static inline bool sema_analyse_last_cond(Context *context, Expr *expr, CondType cond_type)
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
		Expr *left = expr->binary_expr.left;
		if (left->resolve_status ==  RESOLVE_DONE || left->expr_kind != EXPR_IDENTIFIER || left->identifier_expr.path) goto NORMAL_EXPR;

		// Does the identifier exist in the parent scope?
		// then again it can't be a variant unwrap.
		Decl *decl_for_ident = sema_resolve_normal_symbol(context, left->identifier_expr.identifier, NULL, false);
		if (decl_for_ident) goto NORMAL_EXPR;

		Expr *right = expr->binary_expr.right;
		bool is_deref = right->expr_kind == EXPR_UNARY && right->unary_expr.operator == UNARYOP_DEREF;
		if (is_deref) right = right->unary_expr.expr;
		if (!sema_analyse_expr_rhs(context, NULL, right, false)) return false;
		if (right->type == type_get_ptr(type_any) && is_deref)
		{
			is_deref = false;
			right = expr->binary_expr.right;
			if (!sema_analyse_expr_rhs(context, NULL, right, false)) return false;
		}
		if (right->type != type_any) goto NORMAL_EXPR;
		// Found an expansion here
		expr->expr_kind = EXPR_VARIANTSWITCH;
		expr->variant_switch.new_ident = left->identifier_expr.identifier;
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
static inline bool sema_analyse_cond_list(Context *context, Expr *expr, CondType cond_type)
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
static inline bool sema_analyse_cond(Context *context, Expr *expr, CondType cond_type)
{
	bool cast_to_bool = cond_type == COND_TYPE_UNWRAP_BOOL;
	assert(expr->expr_kind == EXPR_COND && "Conditional expressions should always be of type EXPR_DECL_LIST");

	// 1. Analyse the declaration list.
	if (!sema_analyse_cond_list(context, expr, cond_type)) return false;

	// 2. If we get "void", either through a void call or an empty list,
	//    signal that.
	if (expr->type->type_kind == TYPE_VOID)
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
		if (IS_FAILABLE(init) && !decl->var.unwrap)
		{
			return sema_failed_cast(last, last->type, cast_to_bool ? type_bool : init->type);
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
	if (IS_FAILABLE(last))
	{
		if (!cast_to_bool || cast_may_implicit(type_no_fail(last->type), type_bool, false, false))
		{
			SEMA_ERROR(last, "The expression may not be a failable, but was %s.", type_quoted_error_string(last->type));
			return false;
		}
		sema_failed_cast(last, type_no_fail(last->type), type_bool);
		return false;
	}
	// 3b. Cast to bool if that is needed
	if (cast_to_bool)
	{
		if (!cast_implicit(last, type_bool)) return false;
	}
	return true;
}

static inline bool sema_analyse_stmt_placement(Expr *cond, Ast *stmt)
{
	if (stmt->ast_kind == AST_COMPOUND_STMT) return true;
	SourceLocation *end_of_cond = TOKLOC(cond->span.end_loc);
	SourceLocation *start_of_then = TOKLOC(stmt->span.loc);
	return end_of_cond->row == start_of_then->row;
}

/**
 * Check "while" statement, including end of line placement of a single statement.
 */
static inline bool sema_analyse_while_stmt(Context *context, Ast *statement)
{
	Expr *cond = statement->while_stmt.cond;
	Ast *body = statement->while_stmt.body;

	bool success;

	// 1. Begin our scope, this is relevant in case we have something like
	//    while (File *f = @getFileAndClose!()) where the macro pushes a defer into the scope.
	SCOPE_START_WITH_LABEL(statement->while_stmt.flow.label)

		// 2. Analyze the condition
		if (!sema_analyse_cond(context, cond, COND_TYPE_UNWRAP_BOOL))
		{
			// 2a. In case of error, pop context and exit.
			return SCOPE_POP_ERROR();
		}

		// 4. Push break / continue - which is independent of the scope.
		PUSH_BREAKCONT(statement);

		// 6. Analyse the statement
		success = sema_analyse_statement(context, body);

		// 7. Pop break / continue
		POP_BREAKCONT();

		// 8. Check placement, in case of a single statement, it must be placed on the same line.
		if (success && !sema_analyse_stmt_placement(cond, body))
		{
			SEMA_ERROR(body, "A single statement after 'while' must be placed on the same line, or be enclosed in {}.");
			return SCOPE_POP_ERROR();
		}

		// 9. Pop defers attach them to the statement if needed
		context_pop_defers_and_replace_ast(context, body);

	// 10. Pop the while scope.
	SCOPE_END;
	statement->ast_kind = AST_FOR_STMT;
	AstForStmt for_stmt = {
			.cond = cond,
			.flow = statement->while_stmt.flow,
			.incr = NULL,
			.body = statement->while_stmt.body,
	};
	statement->for_stmt = for_stmt;
	return success;
}

/**
 * Check the do ... while (...) statement.
 */
static inline bool sema_analyse_do_stmt(Context *context, Ast *statement)
{
	Expr *expr = statement->do_stmt.expr;
	Ast *body = statement->do_stmt.body;
	bool success;

	// 1. Begin pushing the scope and break / continue.
	SCOPE_START_WITH_LABEL(statement->do_stmt.flow.label)

		PUSH_BREAKCONT(statement);

		// 2. We analyze the statement.
		success = sema_analyse_statement(context, body);

		// 3. Pop break / continue
		POP_BREAKCONT();

		// 4. Pop any defers
		context_pop_defers_and_replace_ast(context, body);

		// 5. If the current scope ended in a jump, then
		//    the do statement has no exit.
		statement->do_stmt.flow.no_exit = context->active_scope.jump_end;

	// 6. Pop the scope
	SCOPE_END;

	// 7. We can now exit if there was an error further up.
	if (!success) return false;

	// 8. Handle the do { } expression
	if (!statement->do_stmt.expr)
	{
		goto END;
	}

	// 9. We next handle the while test. This is its own scope.
	SCOPE_START

		// 10. Try to evaluate and implicitly cast to boolean.
		if (!sema_analyse_cond_expr(context, expr))
		{
			// 10a. On failure, pop and return false.
			return SCOPE_POP_ERROR();
		}

		// 11. Pop any defers in the expression.
		statement->do_stmt.expr = context_pop_defers_and_wrap_expr(context, expr);

	SCOPE_END;

	// 13. Check for infinite loops using do ... while (1).
	//     If it has no break then that means we've reached a statement which ends with a jump.
	if (statement->do_stmt.expr->expr_kind == EXPR_CONST && statement->do_stmt.expr->const_expr.b)
	{
		// Unless there is a break, this won't ever exit.
		context->active_scope.jump_end = !statement->do_stmt.flow.has_break;
	}

END:;
	FlowCommon flow = statement->do_stmt.flow;
	flow.skip_first = true;
	statement->ast_kind = AST_FOR_STMT;
	AstForStmt for_stmt = {
			.cond = expr,
			.flow = flow,
			.incr = NULL,
			.body = body,
			};
	statement->for_stmt = for_stmt;
	return true;
}


static inline bool sema_analyse_declare_stmt(Context *context, Ast *statement)
{
	return sema_analyse_var_decl(context, statement->declare_stmt, true);
}

/**
 * Check "var $foo = ... " and "var $Foo = ..."
 */
static inline bool sema_analyse_var_stmt(Context *context, Ast *statement)
{
	// 1. Pick the declaration.
	Decl *decl = statement->var_stmt;

	// 2. Convert it to a NOP early
	statement->ast_kind = AST_NOP_STMT;

	assert(decl->decl_kind == DECL_VAR);
	switch (decl->var.kind)
	{
		case VARDECL_LOCAL_CT_TYPE:
			// Locally declared compile time type.
			if (decl->var.type_info)
			{
				SEMA_ERROR(decl->var.type_info, "Compile time type variables may not have a type.");
				return false;
			}
			Expr *init = decl->var.init_expr;
			if (init)
			{
				if (!sema_analyse_expr_lvalue(context, init)) return false;
				if (init->expr_kind != EXPR_TYPEINFO)
				{
					SEMA_ERROR(decl->var.init_expr, "Expected a type assigned to %s.", decl->name);
					return false;
				}
			}
			break;
		case VARDECL_LOCAL_CT:
			if (decl->var.type_info && !sema_resolve_type_info(context, decl->var.type_info)) return false;
			if (decl->var.type_info)
			{
				decl->type = decl->var.type_info->type->canonical;
				if (!type_is_builtin(decl->type->type_kind))
				{
					SEMA_ERROR(decl->var.type_info, "Compile time variables may only be built-in types.");
					return false;
				}
				if (decl->var.init_expr)
				{
					if (!sema_analyse_expr_rhs(context, decl->type, decl->var.init_expr, false)) return false;
					if (!expr_is_constant_eval(decl->var.init_expr, CONSTANT_EVAL_ANY))
					{
						SEMA_ERROR(decl->var.init_expr, "Expected a constant expression assigned to %s.", decl->name);
						return false;
					}
				}
				else
				{
					TODO // generate.
					// decl->var.init_expr =
				}
			}
			else
			{
				Expr *decl_init = decl->var.init_expr;
				if (decl_init)
				{
					if (!sema_analyse_expr(context, decl_init)) return false;
					if (!expr_is_constant_eval(decl_init, CONSTANT_EVAL_ANY))
					{
						SEMA_ERROR(decl->var.init_expr, "Expected a constant expression assigned to %s.", decl->name);
						return false;
					}
					decl->type = decl->var.init_expr->type;
				}
				else
				{
					decl->type = type_void;
				}
			}
			break;
		default:
			UNREACHABLE
	}

	decl->var.scope_depth = context->active_scope.depth;
	return sema_add_local(context, decl);
}

static inline bool sema_analyse_expr_stmt(Context *context, Ast *statement)
{
	if (!sema_analyse_expr(context, statement->expr_stmt)) return false;
	return true;
}

static inline bool sema_analyse_defer_stmt(Context *context, Ast *statement)
{
	// TODO special parsing of "catch"
	bool success;
	SCOPE_START_WITH_FLAGS(SCOPE_DEFER)

		context->active_scope.defer_last = 0;
		context->active_scope.defer_start = 0;
		context->active_scope.in_defer = statement;

		PUSH_CONTINUE(NULL);
		PUSH_BREAK(statement);
		PUSH_NEXT(NULL, NULL);

		// Only ones allowed.
		context->active_scope.flags &= SCOPE_DEFER;

		success = sema_analyse_statement(context, statement->defer_stmt.body);

		POP_BREAKCONT();
		POP_NEXT();

		context_pop_defers_and_replace_ast(context, statement->defer_stmt.body);

	SCOPE_END;

	if (!success) return false;

	statement->defer_stmt.prev_defer = context->active_scope.defer_last;
	context->active_scope.defer_last = astid(statement);
	return true;
}


static inline bool sema_analyse_for_stmt(Context *context, Ast *statement)
{
	bool success = true;
	bool is_infinite;

	// Enter for scope
	SCOPE_OUTER_START

		is_infinite = statement->for_stmt.cond == NULL;
		if (statement->for_stmt.init)
		{
			success = sema_analyse_expr(context, statement->for_stmt.init);
		}

		if (success && statement->for_stmt.cond)
		{
			// Conditional scope start
			SCOPE_START
				Expr *cond = statement->for_stmt.cond;
				if (cond->expr_kind == EXPR_COND)
				{
					success = sema_analyse_cond(context, cond, COND_TYPE_UNWRAP_BOOL);
				}
				else
				{
					success = sema_analyse_cond_expr(context, cond);
				}
				statement->for_stmt.cond = context_pop_defers_and_wrap_expr(context, cond);
				// If this is const true, then set this to infinite and remove the expression.
				if (statement->for_stmt.cond->expr_kind == EXPR_CONST && statement->for_stmt.cond->const_expr.b)
				{
					statement->for_stmt.cond = NULL;
					is_infinite = true;
				}
				// Conditional scope end
			SCOPE_END;
		}
		if (success && statement->for_stmt.incr)
		{
			// Incr scope start
			SCOPE_START
				Expr *incr = statement->for_stmt.incr;
				success = sema_analyse_expr(context, incr);
				statement->for_stmt.incr = context_pop_defers_and_wrap_expr(context, incr);
				// Incr scope end
			SCOPE_END;
		}
		if (!success)
		{
			SCOPE_ERROR_END_OUTER();
			return false;
		}

		// Create the for body scope.
		SCOPE_START_WITH_LABEL(statement->for_stmt.flow.label)

			PUSH_BREAKCONT(statement);
			success = sema_analyse_statement(context, statement->for_stmt.body);
			statement->for_stmt.flow.no_exit = context->active_scope.jump_end;
			POP_BREAKCONT();
			// End for body scope
			context_pop_defers_and_replace_ast(context, statement->for_stmt.body);
		SCOPE_END;

		context_pop_defers_and_replace_ast(context, statement);
		// End for scope

	SCOPE_OUTER_END;

	if (statement->for_stmt.flow.no_exit && is_infinite && !statement->for_stmt.flow.has_break)
	{
		context->active_scope.jump_end = true;
	}
	return success;
}


static inline bool sema_inline_default_iterator(Context *context, Expr *expr, Decl *decl)
{
	Expr *inner = expr_copy(expr);
	expr_insert_addr(inner);
	expr->expr_kind = EXPR_CALL;
	expr->call_expr = (ExprCall) {
		.is_type_method = true,
	};
	REMINDER("Failability");
	return sema_expr_analyse_general_call(context, expr, decl, inner, decl->decl_kind == DECL_MACRO, false);
}

static Decl *find_iterator(Context *context, Expr *enumerator)
{
	if (!type_may_have_sub_elements(enumerator->type))
	{
		SEMA_ERROR(enumerator, "It's not possible to enumerate an expression of type %s.", type_quoted_error_string(enumerator->type));
		return NULL;
	}
	Decl *ambiguous = NULL;
	Decl *private = NULL;
	Decl *method = sema_resolve_method(context, enumerator->type->decl, kw_iterator, &ambiguous, &private);
	if (!decl_ok(method)) return NULL;
	if (!method)
	{
		if (ambiguous)
		{
			SEMA_ERROR(enumerator,
			           "It's not possible to find a definition for 'iterator' on %s that is not ambiguous.",
			           type_quoted_error_string(enumerator->type));
			return NULL;
		}
		if (private)
		{
			SEMA_ERROR(enumerator,
			           "It's not possible to find a public definition for 'iterator' with '%s'.",
			           type_quoted_error_string(enumerator->type));
			return NULL;
		}
		SEMA_ERROR(enumerator,
		           "This type cannot be iterated over, implement a method or method macro called 'iterator'.",
		           type_to_error_string(enumerator->type));
		return NULL;
	}
	Decl **parameters;
	TypeInfo *iterator_type;
	switch (method->decl_kind)
	{
		case DECL_GENERIC:
			parameters = method->generic_decl.parameters;
			iterator_type = method->generic_decl.rtype;
			break;
		case DECL_MACRO:
			parameters = method->macro_decl.parameters;
			iterator_type = method->macro_decl.rtype;
			break;
		case DECL_FUNC:
			parameters = method->func_decl.function_signature.params;
			iterator_type = method->func_decl.function_signature.rtype;
			break;
		default:
			UNREACHABLE
	}
	if (vec_size(parameters) > 1)
	{
		SEMA_ERROR(enumerator, "'iterator()' takes parameters and can't be used for 'foreach'.");
		return NULL;
	}
	if (!iterator_type)
	{
		SEMA_ERROR(enumerator, "This type has an iterator without a declared result type, this can't be used with 'foreach'.");
		return NULL;
	}
	assert(iterator_type->resolve_status == RESOLVE_DONE);
	Type *it_type = iterator_type->type->canonical;
	if (it_type->type_kind != TYPE_STRUCT)
	{
		SEMA_ERROR(enumerator, "This type has an implementation of 'iterator()' that doesn't return a struct, so it can't be used with 'foreach'.");
		return NULL;
	}
	return method;
}

static Decl *find_iterator_next(Context *context, Expr *enumerator)
{
	Type *type = enumerator->type->canonical;
	assert(type->type_kind == TYPE_STRUCT);
	Decl *ambiguous = NULL;
	Decl *private = NULL;
	Decl *method = sema_resolve_method(context, type->decl, kw_next, &ambiguous, &private);
	if (!decl_ok(method)) return NULL;
	if (!method)
	{
		if (ambiguous)
		{
			SEMA_ERROR(enumerator, "The iterator %s has ambiguous 'next' definitions.", type_quoted_error_string(type));
			return NULL;
		}
		if (private)
		{
			SEMA_ERROR(enumerator,
					   "The iterator %s has a private 'next' definition.",
					   type_quoted_error_string(type));
			return NULL;
		}
		SEMA_ERROR(enumerator,
				   "The iterator %s is missing a definition for 'next()'.",
				   type_quoted_error_string(type));
		return NULL;
	}
	Decl **parameters;
	TypeInfo *rtype;
	switch (method->decl_kind)
	{
		case DECL_GENERIC:
			parameters = method->generic_decl.parameters;
			rtype = method->generic_decl.rtype;
			break;
		case DECL_MACRO:
			parameters = method->macro_decl.parameters;
			rtype = method->macro_decl.rtype;
			break;
		case DECL_FUNC:
			parameters = method->func_decl.function_signature.params;
			rtype = method->func_decl.function_signature.rtype;
			break;
		default:
			UNREACHABLE
	}
	if (vec_size(parameters) != 2)
	{
		SEMA_ERROR(enumerator, "An iterator with a 'next()' that take takes %d parameters can't be used for 'foreach', it should have 1.", vec_size(parameters) - 1);
		return NULL;
	}
	if (!rtype)
	{
		SEMA_ERROR(enumerator, "This type has an iterator without a declared return type, this can't be used with 'foreach'.");
		return NULL;
	}
	return method;
}

static Expr *sema_insert_method_macro_call(Context *context, SourceSpan span, Decl *macro_decl, Expr *parent, Expr **arguments)
{
	Expr *len_call = expr_new(EXPR_CALL, span);
	len_call->resolve_status = RESOLVE_RUNNING;
	len_call->call_expr.func_ref = macro_decl;
	len_call->call_expr.arguments = arguments;
	len_call->call_expr.body = NULL;
	len_call->call_expr.unsplat_last = false;
	len_call->call_expr.is_type_method = true;
	if (!sema_expr_analyse_macro_call(context, len_call, parent, macro_decl, false)) return poisoned_expr;
	len_call->resolve_status = RESOLVE_DONE;
	return len_call;
}

static inline bool sema_analyse_foreach_stmt(Context *context, Ast *statement)
{
	// Pull out the relevant data.
	Decl *index = statement->foreach_stmt.index;
	Decl *var = statement->foreach_stmt.variable;
	Expr *enumerator = statement->foreach_stmt.enumeration;
	Ast *body = statement->foreach_stmt.body;
	Ast **stmts = NULL;
	Expr **expressions = NULL;

	bool value_by_ref = statement->foreach_stmt.value_by_ref;
	bool success = true;

	// First fold the enumerator expression, removing any () around it.
	while (enumerator->expr_kind == EXPR_GROUP) enumerator = enumerator->inner_expr;

	bool iterator_based = false;

	// Conditional scope start
	SCOPE_START_WITH_FLAGS(SCOPE_COND)

		// In the case of foreach (int x : { 1, 2, 3 }) we will infer the int[] type, so pick out the number of elements.
		Type *inferred_type = NULL;

		// We may have an initializer list, in this case we rely on an inferred type.
		if (expr_is_init_list(enumerator) || (enumerator->expr_kind == EXPR_CONST && enumerator->const_expr.const_kind == CONST_LIST))
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

		// Pop any possible defers.
		enumerator = context_pop_defers_and_wrap_expr(context, enumerator);

		// And pop the cond scope.
	SCOPE_END;

	if (IS_FAILABLE(enumerator))
	{
		SEMA_ERROR(enumerator, "The expression may not be failable.");
		return false;
	}

	if (statement->foreach_stmt.index_by_ref)
	{
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
		expr_insert_deref(enumerator);
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
		len = sema_find_operator(context, enumerator, kw_operator_len);
		Decl *by_val = sema_find_operator(context, enumerator, kw_operator_element_at);
		Decl *by_ref = sema_find_operator(context, enumerator, kw_operator_element_at_ref);
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
		index_type = index_macro->macro_decl.parameters[1]->type;
		value_type = index_macro->macro_decl.rtype->type;
	}


	// Set up the value, assigning the type as needed.
	// Element *value = void
	Decl *value = statement->foreach_stmt.variable;
	if (!value->var.type_info)
	{
		value->var.type_info = type_info_new_base(value_type, value->span);
	}
	if (!sema_resolve_type_info(context, value->var.type_info)) return false;

	if (type_is_failable(value->var.type_info->type))
	{
		SEMA_ERROR(value->var.type_info, "The variable may not be a failable.");
		return false;
	}

	// Set up the optional index parameter
	Decl *index_decl = statement->foreach_stmt.index;
	Type *index_var_type = NULL;
	if (index_decl)
	{
		if (!index_decl->var.type_info) index_decl->var.type_info = type_info_new_base(index_type, enumerator->span);
		if (!sema_resolve_type_info(context, index_decl->var.type_info)) return false;
		index_var_type = index_decl->var.type_info->type;
		if (type_is_failable(index_var_type))
		{
			SEMA_ERROR(index_decl->var.type_info, "The index may not be a failable.");
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

	// IndexType __idx$ = 0
	Decl *idx_decl = decl_new_generated_var("__idx$", index_type, VARDECL_LOCAL, index_decl ? index_decl->span : enumerator->span);
	Expr *idx_init = expr_new(EXPR_CONST, idx_decl->span);
	expr_rewrite_to_int_const(idx_init, index_type, 0, true);
	vec_add(expressions, expr_generate_decl(idx_decl, idx_init));

	// We either have "foreach (x : some_var)" or "foreach (x : some_call())"
	// So we grab the former by address (implicit &) and the latter as the value.
	assert(enumerator->resolve_status == RESOLVE_DONE);
	bool is_addr = false;
	bool is_variable = false;
	if (expr_is_ltype(enumerator))
	{
		if (enumerator->expr_kind == EXPR_IDENTIFIER)
		{
			is_variable = true;
		}
		else
		{
			is_addr = true;
			expr_insert_addr(enumerator);
		}
	}

	Decl *temp = NULL;
	if (is_variable)
	{
		temp = enumerator->identifier_expr.decl;
	}
	else
	{
		// Store either "Foo* __enum$ = &some_var;" or "Foo __enum$ = some_call()"
		temp = decl_new_generated_var("__enum$", enumerator->type, VARDECL_LOCAL, enumerator->span);
		vec_add(expressions, expr_generate_decl(temp, enumerator));
	}

	// Create @__enum$.len() or @(*__enum$).len()
	Expr *enum_val = expr_variable(temp);
	if (is_addr) expr_insert_deref(enum_val);
	Type *enumerator_type = type_flatten(enum_val->type);
	Expr *len_call;
	ArraySize array_len = 0;
	if (len)
	{
		ASSIGN_EXPR_ELSE(len_call, sema_insert_method_macro_call(context, enumerator->span, len, enum_val, NULL), false);
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
			len_call = expr_new(EXPR_LEN, enumerator->span);
			if (!sema_analyse_expr(context, enum_val)) return false;
			len_call->len_expr.inner = enum_val;
			len_call->resolve_status = RESOLVE_DONE;
			len_call->type = type_isize;
		}
	}

	// IndexType __len$ = (IndexType)(@__enum$.len())
	Decl *len_decl = NULL;
	if (len_call)
	{
		len_decl = decl_new_generated_var("__len$", idx_init->type, VARDECL_LOCAL, enumerator->span);
		if (!cast_implicit(len_call, idx_init->type)) return false;
		vec_add(expressions, expr_generate_decl(len_decl, len_call));
	}


	// Add all declarations to the init
	Expr *init_expr = expr_new(EXPR_EXPRESSION_LIST, value->span);
	init_expr->expression_list = expressions;

	// Create __idx$ < __len$
	Expr *binary = expr_new(EXPR_BINARY, idx_decl->span);
	binary->binary_expr.operator = BINARYOP_LT;
	binary->binary_expr.left = expr_variable(idx_decl);
	if (len_decl)
	{
		binary->binary_expr.right = expr_variable(len_decl);
	}
	else
	{
		Expr *rhs = expr_new(EXPR_CONST, enumerator->span);
		expr_rewrite_to_int_const(rhs, type_isize, array_len, true);
		binary->binary_expr.right = rhs;
	}

	// Create __idx$++
	Expr *inc = expr_new(EXPR_UNARY, idx_decl->span);
	inc->unary_expr.expr = expr_variable(idx_decl);
	inc->unary_expr.operator = UNARYOP_INC;

	// Create IndexType index = __idx$++
	if (index_decl)
	{
		Ast *declare_ast = new_ast(AST_DECLARE_STMT, value->span);
		declare_ast->declare_stmt = index_decl;
		Expr *load_idx = expr_variable(idx_decl);
		if (!cast(load_idx, index_var_type)) return false;
		index_decl->var.init_expr = load_idx;
		vec_add(stmts, declare_ast);
	}

	// Create value = (*__$enum)[__idx$]
	Ast *value_declare_ast = new_ast(AST_DECLARE_STMT, value->span);
	value_declare_ast->declare_stmt = value;

	Expr *subscript = expr_new(EXPR_SUBSCRIPT, value->span);
	enum_val = expr_variable(temp);
	if (is_addr) expr_insert_deref(enum_val);
	subscript->subscript_expr.expr = enum_val;
	subscript->subscript_expr.index = expr_variable(idx_decl);
	if (value_by_ref)
	{
		Expr *addr = expr_new(EXPR_UNARY, subscript->span);
		addr->unary_expr.operator = UNARYOP_ADDR;
		addr->unary_expr.expr = subscript;
		subscript = addr;
	}
	value->var.init_expr = subscript;
	vec_add(stmts, value_declare_ast);
	vec_add(stmts, body);
	Ast *compound_stmt = new_ast(AST_COMPOUND_STMT, body->span);
	compound_stmt->compound_stmt.stmts = stmts;
	statement->for_stmt = (AstForStmt){ .init = init_expr,
										.cond = binary,
										.incr = inc,
										.flow = statement->foreach_stmt.flow,
										.body = compound_stmt
	};
	statement->ast_kind = AST_FOR_STMT;
	return sema_analyse_for_stmt(context, statement);

}


static bool sema_analyse_switch_stmt(Context *context, Ast *statement);

static inline bool sema_analyse_if_stmt(Context *context, Ast *statement)
{
	// IMPROVE
	// convert
	// if (!x) A(); else B();
	// into
	// if (x) B(); else A();

	bool else_jump;
	bool then_jump;
	bool success;

	Expr *cond = statement->if_stmt.cond;
	SCOPE_OUTER_START
		CondType cond_type = statement->if_stmt.then_body->ast_kind == AST_IF_CATCH_SWITCH_STMT
		                     ? COND_TYPE_UNWRAP : COND_TYPE_UNWRAP_BOOL;
		success = sema_analyse_cond(context, cond, cond_type);

		Ast *then = statement->if_stmt.then_body;
		bool then_has_braces = then->ast_kind == AST_COMPOUND_STMT || then->ast_kind == AST_IF_CATCH_SWITCH_STMT;

		if (statement->if_stmt.else_body)
		{
			if (!then_has_braces)
			{
				SEMA_ERROR(then, "if-statements with an 'else' must use '{ }' even around a single statement.");
				success = false;
			}
			if (success && statement->if_stmt.else_body->ast_kind != AST_COMPOUND_STMT &&
			    statement->if_stmt.else_body->ast_kind != AST_IF_STMT)
			{
				SEMA_ERROR(statement->if_stmt.else_body,
				           "An 'else' must use '{ }' even around a single statement.");
				success = false;
			}
		}
		if (success && !then_has_braces)
		{
			SourceLocation *end_of_cond = TOKLOC(cond->span.end_loc);
			SourceLocation *start_of_then = TOKLOC(statement->if_stmt.then_body->span.loc);
			if (end_of_cond->row != start_of_then->row)
			{
				SEMA_ERROR(statement->if_stmt.then_body,
				           "The 'then' part of a single line if-statement must start on the same line as the 'if' or use '{ }'");
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
				sema_remove_unwraps_from_try(context, statement->if_stmt.cond);
				sema_unwrappable_from_catch_in_else(context, statement->if_stmt.cond);
				success = success && sema_analyse_statement(context, statement->if_stmt.else_body);
				else_jump = context->active_scope.jump_end;
			SCOPE_END;
		}

		context_pop_defers_and_replace_ast(context, statement);

	SCOPE_OUTER_END;
	if (then_jump)
	{
		sema_unwrappable_from_catch_in_else(context, statement->if_stmt.cond);
	}
	if (then_jump && else_jump && !statement->flow.has_break)
	{
		context->active_scope.jump_end = true;
	}
	return success;
}





static bool sema_analyse_asm_stmt(Context *context, Ast *stmt)
{
	if (!sema_analyse_expr(context, stmt->asm_stmt.body)) return false;
	if (stmt->asm_stmt.body->expr_kind != EXPR_CONST
		|| stmt->asm_stmt.body->const_expr.const_kind != CONST_STRING)
	{
		SEMA_ERROR(stmt->asm_stmt.body, "The asm statement requires a constant string here.");
		return false;
	}
	return true;
}

static inline Decl *sema_analyse_label(Context *context, Ast *stmt)
{
	Decl *ambiguous;
	Decl *dummy;
	Decl *target = sema_resolve_normal_symbol(context, stmt->contbreak_stmt.label.span, NULL, false);
	if (!target)
	{
		SEMA_ERROR(stmt, "Cannot find a labelled statement with the name '%s'.", stmt->contbreak_stmt.label.name);
		return poisoned_decl;
	}
	if (target->decl_kind != DECL_LABEL)
	{
		SEMA_TOKID_ERROR(stmt->contbreak_stmt.label.span, "Expected the name to match a label, not a constant.");
		return poisoned_decl;
	}
	if (context->active_scope.in_defer)
	{
		if (target->label.scope_defer != astid(context->active_scope.in_defer))
		{
			SEMA_ERROR(stmt, stmt->ast_kind == AST_BREAK_STMT ? "You cannot break out of a defer." : "You cannot use continue out of a defer.");
			return poisoned_decl;
		}
	}
	return target;
}

static bool context_labels_exist_in_scope(Context *context)
{
	Decl **last = context->active_scope.current_local;
	for (Decl **from = &context->locals[0]; from < last; from++)
	{
		if ((*from)->decl_kind == DECL_LABEL) return true;
	}
	return false;
}

static bool sema_analyse_break_stmt(Context *context, Ast *statement)
{
	context->active_scope.jump_end = true;
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

	statement->contbreak_stmt.defers.start = context->active_scope.defer_last;
	if (statement->contbreak_stmt.label.name)
	{
		Decl *target = sema_analyse_label(context, statement);
		if (!decl_ok(target)) return false;

		astptr(target->label.parent)->flow.has_break = true;
		statement->contbreak_stmt.ast = target->label.parent;
		statement->contbreak_stmt.defers.end = target->label.defer;
		return true;
	}
	statement->contbreak_stmt.defers.end = context->break_defer;
	statement->contbreak_stmt.ast = context->break_target;
	astptr(context->break_target)->flow.has_break = true;
	return true;
}

static bool sema_analyse_nextcase_stmt(Context *context, Ast *statement)
{
	context->active_scope.jump_end = true;

	if (!context->next_target && !statement->next_stmt.label.name && !statement->next_stmt.expr_or_type_info)
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
	if (statement->next_stmt.label.name)
	{
		Decl *target = sema_resolve_normal_symbol(context, statement->next_stmt.label.span, NULL, false);
		if (!target)
		{
			SEMA_ERROR(statement, "Cannot find a switch statement with the name '%s'.", statement->next_stmt.label.name);
			return false;
		}
		if (target->decl_kind != DECL_LABEL)
		{
			SEMA_TOKID_ERROR(statement->next_stmt.label.span, "Expected the name to match a label, not a constant.");
			return false;
		}
		parent = astptr(target->label.parent);
		AstKind kind = parent->ast_kind;
		if (kind != AST_SWITCH_STMT && kind != AST_IF_CATCH_SWITCH_STMT)
		{
			SEMA_TOKID_ERROR(statement->next_stmt.label.span, "Expected the label to match a 'switch' or 'if-catch' statement.");
			return false;
		}
		bool defer_mismatch = false;
		defer_mismatch = context->active_scope.in_defer != parent->switch_stmt.scope_defer;
		if (defer_mismatch)
		{
			SEMA_ERROR(statement, "This 'nextcase' would jump out of a defer which is not allowed.");
			return false;
		}
		assert(statement->next_stmt.expr_or_type_info);
	}

	statement->next_stmt.defers.start = context->active_scope.defer_last;
	statement->next_stmt.defers.end = parent->switch_stmt.defer;

	// Plain next.
	if (!statement->next_stmt.expr_or_type_info)
	{
		assert(context->next_target);
		statement->next_stmt.case_switch_stmt = context->next_target;
		return true;
	}

	if (statement->next_stmt.is_type)
	{
		TypeInfo *type_info = statement->next_stmt.expr_or_type_info;
		if (!sema_resolve_type_info(context, type_info)) return false;
		Ast **cases;
		statement->next_stmt.defers.end = parent->switch_stmt.defer;
		if (parent->switch_stmt.cond->type->canonical != type_typeid)
		{
			SEMA_ERROR(statement, "Unexpected 'type' in as an 'nextcase' destination.");
			SEMA_PREV(statement, "The 'switch' here uses expected a type '%s'.", type_to_error_string(parent->switch_stmt.cond->type));
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
				statement->next_stmt.case_switch_stmt = astid(case_stmt);
				return true;
			}
		}
		if (default_stmt)
		{
			statement->next_stmt.case_switch_stmt = astid(default_stmt);
			return true;
		}
		SEMA_ERROR(type_info, "There is no case for type '%s'.", type_to_error_string(type_info->type));
		return false;
	}

	Expr *target = statement->next_stmt.expr_or_type_info;

	Type *expected_type = parent->ast_kind == AST_SWITCH_STMT ? parent->switch_stmt.cond->type : type_anyerr;

	if (!sema_analyse_expr_rhs(context, expected_type, target, false)) return false;

	if (target->expr_kind == EXPR_CONST)
	{
		Ast *default_stmt = NULL;
		statement->next_stmt.defers.end = parent->switch_stmt.defer;
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
			if (expr_const_compare(&target->const_expr, const_expr, BINARYOP_GE) &&
				expr_const_compare(&target->const_expr, to_const_expr, BINARYOP_LE))
			{
				statement->next_stmt.case_switch_stmt = astid(case_stmt);
				return true;
			}
		}
		if (default_stmt)
		{
			statement->next_stmt.case_switch_stmt = astid(default_stmt);
			return true;
		}
		SEMA_ERROR(statement, "'nextcase' needs to jump to an exact case statement.");
		return false;
	}

	statement->next_stmt.case_switch_stmt = astid(parent);
	statement->next_stmt.switch_expr = target;
	return true;
}

static bool sema_analyse_continue_stmt(Context *context, Ast *statement)
{
	context->active_scope.jump_end = true;
	statement->contbreak_stmt.defers.start = context->active_scope.defer_last;

	if (!context->continue_target && !statement->contbreak_stmt.label.name)
	{
		SEMA_ERROR(statement, "'continue' is not allowed here.");
		return false;
	}

	if (statement->contbreak_stmt.label.name)
	{
		Decl *target = sema_analyse_label(context, statement);
		if (!decl_ok(target)) return false;
		Ast *parent = astptr(target->label.parent);
		switch (parent->ast_kind)
		{
			case AST_FOR_STMT:
			case AST_WHILE_STMT:
				break;
			case AST_DO_STMT:
				if (parent->do_stmt.expr) break;
				FALLTHROUGH;
			default:
				SEMA_ERROR(statement, "'continue' may only be used with 'for', 'while' and 'do-while' statements.");
				return false;
		}
		statement->contbreak_stmt.ast = target->label.parent;
		statement->contbreak_stmt.defers.end = target->label.defer;
		return true;
	}
	statement->contbreak_stmt.defers.end = context->continue_defer;
	statement->contbreak_stmt.ast = context->continue_target;
	return true;
}

static inline bool sema_analyse_then_overwrite(Context *context, Ast *statement, Ast *replacement)
{
	if (!sema_analyse_statement(context, replacement)) return false;
	// Overwrite
	*statement = *replacement;
	return true;
}


static bool sema_analyse_ct_if_stmt(Context *context, Ast *statement)
{
	int res = sema_check_comp_time_bool(context, statement->ct_if_stmt.expr);
	if (res == -1) return false;
	if (res)
	{
		return sema_analyse_then_overwrite(context, statement, statement->ct_if_stmt.then);
	}

	Ast *elif = statement->ct_if_stmt.elif;
	while (1)
	{
		if (!elif)
		{
			// Turn into NOP!
			statement->ast_kind = AST_NOP_STMT;
			return true;
		}
		// We found else, then just replace with that.
		if (elif->ast_kind == AST_CT_ELSE_STMT)
		{
			return sema_analyse_then_overwrite(context, statement, elif->ct_else_stmt);
		}
		assert(elif->ast_kind == AST_CT_ELIF_STMT);

		res = sema_check_comp_time_bool(context, elif->ct_elif_stmt.expr);
		if (res == -1) return false;
		if (res)
		{
			return sema_analyse_then_overwrite(context, statement, elif->ct_elif_stmt.then);
		}
		elif = elif->ct_elif_stmt.elif;
	}
}


static inline bool sema_analyse_compound_statement_no_scope(Context *context, Ast *compound_statement)
{
	bool all_ok = ast_ok(compound_statement);
	VECEACH(compound_statement->compound_stmt.stmts, i)
	{
		if (!sema_analyse_statement(context, compound_statement->compound_stmt.stmts[i]))
		{
			ast_poison(compound_statement->compound_stmt.stmts[i]);
			all_ok = false;
		}
	}

	context_pop_defers_to(context, &compound_statement->compound_stmt.defer_list);
	return all_ok;
}

static inline bool sema_check_type_case(Context *context, Type *switch_type, Ast *case_stmt, Ast **cases, unsigned index)
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
				SEMA_PREV(other, "Here is the case with that type.");
				return false;
			}
		}
	}
	return true;
}

static inline ExprConst *flatten_enum_const(Expr *expr)
{
	ExprConst *const_expr = &expr->const_expr;
	if (const_expr->const_kind == CONST_ENUM)
	{
		const_expr->const_kind = CONST_INTEGER;
		Decl *enum_val = const_expr->enum_val;
		Expr *enum_expr = enum_val->enum_constant.expr;
		assert(enum_expr->expr_kind == EXPR_CONST);
		ExprConst *enum_const = &enum_expr->const_expr;
		assert(enum_const->const_kind == CONST_INTEGER);
		*const_expr = *enum_const;
	}
	return const_expr;
}
static inline bool sema_check_value_case(Context *context, Type *switch_type, Ast *case_stmt, Ast **cases, unsigned index, bool *if_chained, bool *max_ranged)
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
	ExprConst *const_expr = flatten_enum_const(expr);
	ExprConst *to_const_expr = to_expr ? flatten_enum_const(to_expr) : const_expr;

	if (!*max_ranged && type_is_integer(expr->type) && to_const_expr != const_expr)
	{
		Int128 range = int_sub(to_const_expr->ixx, const_expr->ixx).i;
		Int128 max_range = { .low = active_target.switchrange_max_size };
		if (i128_comp(range, max_range, type_i128) == CMP_GT)
		{
			*max_ranged = true;
		}
	}
	for (unsigned i = 0; i < index; i++)
	{
		Ast *other = cases[i];
		if (other->ast_kind != AST_CASE_STMT) continue;
		ExprConst *other_const = &other->case_stmt.expr->const_expr;
		ExprConst *other_to_const = other->case_stmt.to_expr ? &other->case_stmt.to_expr->const_expr : other_const;
		if (expr_const_compare(const_expr, other_to_const, BINARYOP_LE) && expr_const_compare(to_const_expr, other_const, BINARYOP_GE))
		{
			SEMA_ERROR(case_stmt, "The same case value appears more than once.");
			SEMA_PREV(other, "Here is the previous use of that value.");
			return false;
		}
	}
	return true;
}

static bool sema_analyse_switch_body(Context *context, Ast *statement, SourceSpan expr_span, Type *switch_type, Ast **cases, ExprVariantSwitch *variant, Decl *var_holder)
{
	bool use_type_id = false;
	if (!type_is_comparable(switch_type))
	{
		sema_error_range(expr_span, "You cannot test '%s' for equality, and only values that supports '==' for comparison can be used in a switch.", type_to_error_string(switch_type));
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
					SEMA_PREV(default_case, "Here is the previous use.");
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
					TokenId name = variant->new_ident;
					Decl *new_var = decl_new_var(name,
												 type_info_new_base(variant->is_deref
					                             ? real_type->pointer : real_type, source_span_from_token_id(name)),
					                             VARDECL_LOCAL, VISIBLE_LOCAL);
					Expr *var_result = expr_variable(var_holder);
					if (!cast(var_result, real_type)) return false;
					if (variant->is_deref)
					{
						expr_insert_deref(var_result);
					}
					new_var->var.init_expr = var_result;
					Ast *decl_ast = new_ast(AST_DECLARE_STMT, new_var->span);
					decl_ast->declare_stmt = new_var;
					vec_insert_first(body->compound_stmt.stmts, decl_ast);
				}
				else
				{
					Type *type = type_get_ptr(stmt->case_stmt.expr->const_expr.typeid);
					Decl *alias = decl_new_var(var_holder->name_token,
											   type_info_new_base(type, stmt->case_stmt.expr->span),
											   VARDECL_LOCAL, VISIBLE_LOCAL);
					Expr *ident_converted = expr_variable(var_holder);
					if (!cast(ident_converted, type)) return false;
					alias->var.init_expr = ident_converted;
					alias->var.shadow = true;
					Ast *decl_ast = new_ast(AST_DECLARE_STMT, alias->span);
					decl_ast->declare_stmt = alias;
					vec_insert_first(body->compound_stmt.stmts, decl_ast);
				}
			}
			success = success && (!body || sema_analyse_compound_statement_no_scope(context, body));
			POP_BREAK();
			POP_NEXT();
			all_jump_end &= (!body | context->active_scope.jump_end);
		SCOPE_END;
	}
	statement->flow.no_exit = all_jump_end;
	statement->switch_stmt.if_chain = if_chain || max_ranged;
	if (!success) return false;
	return success;
}

static bool sema_analyse_ct_switch_body(Context *context, Ast *statement)
{
	Expr *cond = statement->ct_switch_stmt.cond;
	Type *type = cond->type;
	bool is_type = type == type_typeid;
	ExprConst *switch_expr_const = &cond->const_expr;
	Ast **cases = statement->ct_switch_stmt.body;
	unsigned case_count = vec_size(cases);
	int matched_case = (int)case_count;
	int default_case = (int)case_count;
	for (unsigned i = 0; i < case_count; i++)
	{
		Ast *stmt = cases[i];
		switch (stmt->ast_kind)
		{
			case AST_CASE_STMT:
			{
				Expr *expr = stmt->case_stmt.expr;
				Expr *to_expr = stmt->case_stmt.to_expr;
				if (is_type)
				{
					if (!sema_analyse_expr(context, expr)) return false;
					if (expr->type != type_typeid)
					{
						SEMA_ERROR(expr, "A type was expected here not %s.", type_quoted_error_string(expr->type));
						return false;
					}
				}
				else
				{
					if (!sema_analyse_expr_rhs(context, type, expr, false)) return false;
					if (to_expr && !sema_analyse_expr_rhs(context, type, to_expr, false)) return false;
				}
				if (expr->expr_kind != EXPR_CONST)
				{
					SEMA_ERROR(expr, "The $case must have a constant expression.");
					return false;
				}
				if (to_expr && to_expr->expr_kind != EXPR_CONST)
				{
					SEMA_ERROR(to_expr, "The $case must have a constant expression.");
					return false;
				}
				ExprConst *const_expr = &expr->const_expr;
				ExprConst *const_to_expr = to_expr ? &to_expr->const_expr : const_expr;
				if (to_expr && expr_const_compare(const_expr, const_to_expr, BINARYOP_GT))
				{
					SEMA_ERROR(to_expr, "The end of a range must be less or equal to the beginning.");
					return false;
				}
				// Check that it is unique.
				for (unsigned j = 0; j < i; j++)
				{
					Ast *other_stmt = cases[j];
					if (other_stmt->ast_kind == AST_DEFAULT_STMT) continue;
					ExprConst *other_const = &other_stmt->case_stmt.expr->const_expr;
					ExprConst *other_const_to = other_stmt->case_stmt.to_expr ? &other_stmt->case_stmt.to_expr->const_expr : other_const;
					if (expr_const_compare(const_expr, other_const_to, BINARYOP_LE) && expr_const_compare(const_to_expr, other_const, BINARYOP_GE))
					{
						SEMA_ERROR(stmt, "'%s' appears more than once.", expr_const_to_error_string(const_expr));
						SEMA_PREV(cases[j]->case_stmt.expr, "The previous $case was here.");
						return false;
					}
				}
				if (expr_const_compare(switch_expr_const, const_expr, BINARYOP_GE) && expr_const_compare(switch_expr_const, const_to_expr, BINARYOP_LE))
				{
					matched_case = (int) i;
				}
				break;
			}
			case AST_DEFAULT_STMT:
				if (default_case < case_count)
				{
					SEMA_ERROR(stmt, "More than one $default is not allowed.");
					SEMA_PREV(cases[default_case], "The previous $default was here.");
					return false;
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
		return true;
	}
	if (!sema_analyse_statement(context, body)) return false;

	*statement = *body;
	return true;
}

static bool sema_analyse_ct_switch_stmt(Context *context, Ast *statement)
{
	Expr *cond = statement->ct_switch_stmt.cond;
	if (!sema_analyse_expr(context, cond)) return false;
	if (cond->expr_kind != EXPR_CONST)
	{
		SEMA_ERROR(cond, "A compile time $switch must be over a constant value.");
		return false;
	}
	return sema_analyse_ct_switch_body(context, statement);
}

static bool sema_analyse_switch_stmt(Context *context, Ast *statement)
{
	statement->switch_stmt.scope_defer = context->active_scope.in_defer;

	SCOPE_START_WITH_LABEL(statement->switch_stmt.flow.label);

		Expr *cond = statement->switch_stmt.cond;
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
					variant_decl = decl_new_generated_var(".variant", type_any, VARDECL_LOCAL, last->span);
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
				last->type = type_typeid;
				last->expr_kind = EXPR_TYPEOFANY;
				last->inner_expr = inner;
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




bool sema_analyse_ct_assert_stmt(Context *context, Ast *statement)
{
	Expr *expr = statement->ct_assert_stmt.expr;
	Expr *message = statement->ct_assert_stmt.message;
	if (message)
	{
		if (!sema_analyse_expr(context, message)) return false;
		if (message->expr_kind != EXPR_CONST || message->const_expr.const_kind != CONST_STRING)
		{
			SEMA_ERROR(message, "Expected a string as the error message.");
		}
	}
	int res = sema_check_comp_time_bool(context, expr);

	if (res == -1) return false;
	if (!res)
	{
		if (message)
		{
			SEMA_ERROR(expr, "Compile time assert - %.*s", message->const_expr.string.len, message->const_expr.string.chars);
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

static inline bool sema_analyse_scoping_stmt(Context *context, Ast *statement)
{
	Expr **exprs = statement->scoping_stmt.scoped->expression_list;
	unsigned scoped_count = vec_size(exprs);
	Ast **stmts = 0;
	for (unsigned i = 0; i < scoped_count; i++)
	{
		Expr *expr = exprs[i];
		if (!sema_analyse_expr_lvalue(context, expr)) return false;
		if (!expr_is_ltype(expr))
		{
			SEMA_ERROR(expr, "Expected an assignable value.");
			return false;
		}
		if (!expr_is_pure(expr))
		{
			SEMA_ERROR(expr, "A value with side effects (e.g. function calls) is not allowed here.");
			return false;
		}
		Decl *new_decl = decl_new_generated_var(".scope", expr->type, VARDECL_LOCAL, expr->span);
		new_decl->var.init_expr = expr;
		Ast *declare = new_ast(AST_DECLARE_STMT, expr->span);
		declare->declare_stmt = new_decl;
		vec_add(stmts, declare);
		Ast *defer_restore = new_ast(AST_DEFER_STMT, expr->span);

		Expr *restore_expr = expr_new(EXPR_BINARY, expr->span);
		Expr *rhs = expr_new(EXPR_IDENTIFIER, expr->span);
		rhs->resolve_status = RESOLVE_DONE;
		rhs->identifier_expr.decl = new_decl;
		rhs->type = expr->type;

		restore_expr->binary_expr = (ExprBinary) { .left = MACRO_COPY_EXPR(expr), .right = rhs, .operator = BINARYOP_ASSIGN };
		Ast *restore_stmt = new_ast(AST_EXPR_STMT, expr->span);
		restore_stmt->expr_stmt = restore_expr;

		defer_restore->defer_stmt.body = restore_stmt;
		vec_add(stmts, defer_restore);
	}
	vec_add(stmts, statement->scoping_stmt.stmt);
	statement->ast_kind = AST_COMPOUND_STMT;
	statement->compound_stmt = (AstCompoundStmt) { .stmts = stmts };
	return sema_analyse_compound_stmt(context, statement);
}

bool sema_analyse_assert_stmt(Context *context, Ast *statement)
{
	Expr *expr = statement->assert_stmt.expr;
	Expr *message = statement->assert_stmt.message;
	if (message)
	{
		if (!sema_analyse_expr(context, message)) return false;
		if (message->expr_kind != EXPR_CONST || message->const_expr.const_kind != CONST_STRING)
		{
			SEMA_ERROR(message, "Expected a string as the error message.");
		}
	}
	if (expr->expr_kind == EXPR_TRY_UNWRAP_CHAIN)
	{
		if (!sema_analyse_try_unwrap_chain(context, expr, COND_TYPE_UNWRAP_BOOL)) return false;
	}
	else
	{
		if (!sema_analyse_cond_expr(context, expr)) return false;
	}
	return true;
}

static bool sema_analyse_compound_stmt(Context *context, Ast *statement)
{
	bool success;
	bool ends_with_jump;
	SCOPE_START
		success = sema_analyse_compound_statement_no_scope(context, statement);
		ends_with_jump = context->active_scope.jump_end;
	SCOPE_END;
	context->active_scope.jump_end = ends_with_jump;
	return success;
}

static bool sema_analyse_ct_compound_stmt(Context *context, Ast *statement)
{
	bool all_ok = ast_ok(statement);
	VECEACH(statement->ct_compound_stmt, i)
	{
		if (!sema_analyse_statement(context, statement->ct_compound_stmt[i]))
		{
			ast_poison(statement->ct_compound_stmt[i]);
			all_ok = false;
		}
	}
	return all_ok;
}


static inline bool sema_analyse_statement_inner(Context *context, Ast *statement)
{
	if (statement->ast_kind == AST_POISONED)
	{
		return false;
	}
	if (context->active_scope.jump_end && !context->active_scope.allow_dead_code)
	{
		if (statement->ast_kind == AST_UNREACHABLE_STMT)
		{
			context->active_scope.allow_dead_code = true;
			return true;
		}
		//SEMA_ERROR(statement, "This code will never execute.");
		context->active_scope.allow_dead_code = true;
		//return false;
	}
	switch (statement->ast_kind)
	{
		case AST_POISONED:
		case AST_SCOPED_STMT:
		case AST_DOCS:
		case AST_DOC_DIRECTIVE:
		case AST_IF_CATCH_SWITCH_STMT:
			UNREACHABLE
		case AST_SCOPING_STMT:
			return sema_analyse_scoping_stmt(context, statement);
		case AST_ASM_STMT:
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
		case AST_CT_COMPOUND_STMT:
			return sema_analyse_ct_compound_stmt(context, statement);
		case AST_CT_IF_STMT:
			return sema_analyse_ct_if_stmt(context, statement);
		case AST_DECLARE_STMT:
			return sema_analyse_declare_stmt(context, statement);
		case AST_DEFAULT_STMT:
			SEMA_ERROR(statement, "Unexpected 'default' outside of switch");
			return false;
		case AST_DEFER_STMT:
			return sema_analyse_defer_stmt(context, statement);
		case AST_VAR_STMT:
			return sema_analyse_var_stmt(context, statement);
		case AST_DO_STMT:
			return sema_analyse_do_stmt(context, statement);
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
		case AST_RETURN_STMT:
			return sema_analyse_return_stmt(context, statement);
		case AST_SWITCH_STMT:
			return sema_analyse_switch_stmt(context, statement);
		case AST_NEXT_STMT:
			return sema_analyse_nextcase_stmt(context, statement);
		case AST_UNREACHABLE_STMT:
			return sema_analyse_unreachable_stmt(context);
		case AST_WHILE_STMT:
			return sema_analyse_while_stmt(context, statement);
		case AST_CT_SWITCH_STMT:
			return sema_analyse_ct_switch_stmt(context, statement);
		case AST_CT_ELIF_STMT:
		case AST_CT_ELSE_STMT:
			UNREACHABLE
		case AST_CT_FOR_STMT:
			TODO
	}

	UNREACHABLE
}


bool sema_analyse_statement(Context *context, Ast *statement)
{
	if (sema_analyse_statement_inner(context, statement)) return true;
	return ast_poison(statement);
}


static bool sema_analyse_requires(Context *context, Ast *docs, Ast ***asserts)
{
	if (!docs) return true;
	Ast **directives = docs->directives;
	Ast **output = NULL;
	VECEACH(directives, i)
	{
		Ast *directive = directives[i];
		if (directive->doc_directive.kind != DOC_DIRECTIVE_REQUIRE) continue;
		Expr *comment = directive->doc_directive.contract.comment;
		Expr *declexpr = directive->doc_directive.contract.decl_exprs;
		if (comment)
		{
			if (comment->expr_kind != EXPR_CONST || comment->const_expr.const_kind != CONST_STRING)
			{
				SEMA_ERROR(comment, "Expected a string here.");
				return false;
			}
		}
		assert(declexpr->expr_kind == EXPR_COND);

		VECEACH(declexpr->cond_expr, j)
		{
			Expr *expr = declexpr->cond_expr[j];
			if (expr->expr_kind == EXPR_DECL)
			{
				SEMA_ERROR(expr, "Only expressions are allowed.");
				return false;
			}
			if (!sema_analyse_cond_expr(context, expr)) return false;
			Ast *assert = new_ast(AST_ASSERT_STMT, expr->span);
			assert->assert_stmt.expr = expr;
			assert->assert_stmt.message = comment;
			vec_add(output, assert);
		}
	}
	*asserts = output;
	return true;
}

bool sema_analyse_function_body(Context *context, Decl *func)
{
	if (!decl_ok(func)) return false;
	FunctionSignature *signature = &func->func_decl.function_signature;
	context->active_function_for_analysis = func;
	context->rtype = signature->rtype->type;
	context->active_scope = (DynamicScope) {
			.scope_id = 0,
			.depth = 0,
			.local_decl_start = &context->locals[0],
			.current_local = &context->locals[0]
	};
	context->macro_scope = (MacroScope) { 0 };

	// Clear returns
	vec_resize(context->returns, 0);
	context->scope_id = 0;
	context->continue_target = 0;
	context->next_target = 0;
	context->next_switch = 0;
	context->break_target = 0;
	func->func_decl.annotations = CALLOCS(FuncAnnotations);
	SCOPE_START
		assert(context->active_scope.depth == 1);
		Decl **params = signature->params;
		VECEACH(params, i)
		{
			if (!sema_add_local(context, params[i])) return false;
		}
		Ast **asserts = NULL;
		if (!sema_analyse_requires(context, func->docs, &asserts)) return false;
		if (func->func_decl.attr_naked)
		{
			Ast **stmts = func->func_decl.body->compound_stmt.stmts;
			VECEACH(stmts, i)
			{
				if (stmts[i]->ast_kind != AST_ASM_STMT)
				{
					SEMA_ERROR(stmts[i], "Only asm statements are allowed inside of a naked function.");
					return false;
				}
			}
			asserts = NULL;
		}
		else
		{
			if (!sema_analyse_compound_statement_no_scope(context, func->func_decl.body)) return false;
			assert(context->active_scope.depth == 1);
			if (!context->active_scope.jump_end)
			{
				Type *canonical_rtype = type_no_fail(signature->rtype->type)->canonical;
				if (canonical_rtype != type_void)
				{
					// IMPROVE better pointer to end.
					SEMA_TOKID_ERROR(func->name_token, "Missing return statement at the end of the function.");
					return false;
				}
			}
		}
		if (asserts)
		{
			Ast *ast = new_ast(AST_COMPOUND_STMT, func->func_decl.body->span);
			ast->compound_stmt.stmts = asserts;
			vec_add(ast->compound_stmt.stmts, func->func_decl.body);
			func->func_decl.body = ast;
		}
	SCOPE_END;
	return true;
}

