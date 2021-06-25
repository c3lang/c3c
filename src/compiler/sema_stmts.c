// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"

#pragma mark --- Helper functions



#pragma mark --- Sema analyse stmts


static inline bool sema_analyse_block_return_stmt(Context *context, Ast *statement)
{
	assert(context->active_scope.flags & (SCOPE_EXPR_BLOCK | SCOPE_MACRO));
	context->active_scope.jump_end = true;
	if (statement->return_stmt.expr)
	{
		if (!sema_analyse_expr_of_required_type(context,
	                                           context->expected_block_type,
	                                           statement->return_stmt.expr, true)) return false;
		context->expr_failable_return |= statement->return_stmt.expr->failable;
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
		if (expected_rtype->canonical != type_void)
		{
			SEMA_ERROR(statement, "Expected to return a result of type %s.", type_to_error_string(expected_rtype));
			return false;
		}
		return true;
	}

	// 3. Evaluate the return value to be the expected return type.
	if (!sema_analyse_expr_of_required_type(context, expected_rtype, return_expr, context->failable_return)) return false;

	assert(statement->return_stmt.expr->type->canonical == expected_rtype->canonical);

	return true;
}

static inline bool sema_analyse_unreachable_stmt(Context *context)
{
	context->active_scope.jump_end = true;
	return true;
}

/**
 * An decl-expr-list is a list of a mixture of declarations and expressions.
 * The last declaration or expression is propagated. So for example:
 *
 *   int a = 3, b = 4, float c = 4.0
 *
 * In this case the final value is 4.0 and the type is float.
 */
static inline bool sema_analyse_decl_expr_list(Context *context, Expr *expr)
{
	assert(expr->expr_kind == EXPR_DECL_LIST);

	Ast **dexprs = expr->dexpr_list_expr;
	unsigned entries = vec_size(dexprs);

	// 1. Special case, there are no entries, so the type is void
	if (entries == 0)
	{
		expr_set_type(expr, type_void);
		return true;
	}

	// 2. Walk through each of our declarations / expressions as if they were regular statements.
	for (unsigned i = 0; i < entries; i++)
	{
		if (!sema_analyse_statement(context, dexprs[i])) return false;
	}

	// 3. We now look at the final expression and copy its type as the type of the entire list.
	//    There is a subtle difference here. The expression might have a different original type
	//    than the expression's type. In most (all?) cases this is isn't something one uses,
	//    but this makes sure that analysis is correct.
	Ast *last = dexprs[entries - 1];
	switch (last->ast_kind)
	{
		case AST_DECLARE_STMT:
			// In the declaration case, the value is the type of the declaration.
			expr_set_type(expr, last->declare_stmt->type);
			break;
		case AST_EXPR_STMT:
			// In the expression case, *copy* the expression's types.
			expr_copy_types(expr, last->expr_stmt);
			break;
		default:
			UNREACHABLE
	}
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
static inline bool sema_analyse_cond(Context *context, Expr *expr, bool cast_to_bool)
{
	assert(expr->expr_kind == EXPR_DECL_LIST && "Conditional expressions should always be of type EXPR_DECL_LIST");

	// 1. Analyse the declaration list.
	if (!sema_analyse_decl_expr_list(context, expr)) return false;

	// 2. If we get "void", either through a void call or an empty list,
	//    signal that.
	if (expr->type->type_kind == TYPE_VOID)
	{
		SEMA_ERROR(expr, cast_to_bool ? "Expected a boolean expression." : "Expected an expression resulting in a value.");
		return false;
	}

	// 3. We look at the last element (which is guaranteed to exist because
	//    the type was not void.
	Ast *last = VECLAST(expr->dexpr_list_expr);

	switch (last->ast_kind)
	{
		case AST_EXPR_STMT:
			// 3a. Check for failables in case of an expression.
			if (last->expr_stmt->failable)
			{
				SEMA_ERROR(last, "'%s!' cannot be converted into '%s'.",
				               type_to_error_string(last->expr_stmt->type),
				               cast_to_bool ? "bool" : type_to_error_string(last->expr_stmt->type));
			}
			// 3b. Cast to bool if that is needed
			if (cast_to_bool)
			{
				if (!cast_implicit(last->expr_stmt, type_bool)) return false;
			}
			return true;
		case AST_DECLARE_STMT:
		{
			// 3c. The declaration case
			Decl *decl = last->declare_stmt;
			Expr *init = decl->var.init_expr;
			// 3d. We expect an initialization for the last declaration.
			if (!init)
			{
				SEMA_ERROR(last, "Expected a declaration with initializer.");
				return false;
			}
			// 3e. Expect that it isn't a failable
			if (init->failable && !decl->var.unwrap)
			{
				SEMA_ERROR(last, "'%s!' cannot be converted into '%s'.",
				               type_to_error_string(last->expr_stmt->type),
				           cast_to_bool ? "bool" : type_to_error_string(init->type));
			}
			// TODO document
			if (!decl->var.unwrap && cast_to_bool && cast_to_bool_kind(decl->var.type_info->type) == CAST_ERROR)
			{
				SEMA_ERROR(last->declare_stmt->var.init_expr, "The expression needs to be convertible to a boolean.");
				return false;
			}
			return true;
		}
		default:
			UNREACHABLE
	}
}

static inline bool sema_analyse_stmt_placement(Expr *cond, Ast *stmt)
{
	if (stmt->ast_kind == AST_COMPOUND_STMT) return true;
	SourceLocation *end_of_cond = TOKLOC(cond->span.end_loc);
	SourceLocation *start_of_then = TOKLOC(stmt->span.loc);
	return end_of_cond->line == start_of_then->line;
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
		if (!sema_analyse_cond(context, cond, true))
		{
			// 2a. In case of error, pop context and exit.
			return SCOPE_POP_ERROR();
		}

		// 4. Push break / continue - which is independent of the scope.
		PUSH_BREAKCONT(statement);

		// 5. Analyse the statement
		success = sema_analyse_statement(context, body);

		// 6. Pop break / continue
		POP_BREAKCONT();

		// 7. Check placement, in case of a single statement, it must be placed on the same line.
		if (success && !sema_analyse_stmt_placement(cond, body))
		{
			SEMA_ERROR(body, "A single statement after 'while' must be placed on the same line, or be enclosed in {}.");
			return SCOPE_POP_ERROR();
		}

		// 8. Pop defers attach them to the statement if needed
		context_pop_defers_and_replace_ast(context, body);

	// 9. Pop the while scope.
	SCOPE_END;

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
	if (!statement->do_stmt.expr) return true;

	// 9. We next handle the while test. This is its own scope.
	SCOPE_START

		// 10. Try to evaluate and implicitly cast to boolean.
		if (!sema_analyse_expr_of_required_type(context, type_bool, expr, false))
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
	return true;
}


/**
 * Analyse a regular local declaration, e.g. int x = 123
 */
static inline bool sema_analyse_local_decl(Context *context, Decl *decl)
{
	assert(decl->decl_kind == DECL_VAR && "Unexpected declaration type");

	// TODO unify with global decl analysis
	// Add a local to the current context, will throw error on shadowing.
	if (!sema_add_local(context, decl)) return decl_poison(decl);

	// 1. Local constants: const int FOO = 123.
	if (decl->var.kind == VARDECL_CONST)
	{
		Expr *init_expr = decl->var.init_expr;
		// 1a. We require an init expression.
		if (!init_expr)
		{
			SEMA_ERROR(decl, "Constants need to have an initial value.");
			return false;
		}
		// 1b. We require defined constants
		if (init_expr->expr_kind == EXPR_UNDEF)
		{
			SEMA_ERROR(decl, "Constants cannot be undefined.");
			return false;
		}
		if (!decl->var.type_info)
		{
			if (!sema_analyse_expr(context, NULL, init_expr)) return false;
			decl->type = init_expr->type;
			// Skip further evaluation.
			goto EXIT_OK;
		}
	}
	if (!sema_resolve_type_info_maybe_inferred(context, decl->var.type_info, decl->var.init_expr != NULL)) return decl_poison(decl);
	decl->type = decl->var.type_info->type;
	if (decl->var.init_expr)
	{
		bool type_is_inferred = decl->type->type_kind == TYPE_INFERRED_ARRAY;
		Expr *init = decl->var.init_expr;
		// Handle explicit undef
		if (init->expr_kind == EXPR_UNDEF)
		{
			if (type_is_inferred)
			{
				SEMA_ERROR(decl->var.type_info, "Size of the array cannot be inferred with explicit undef.");
				return false;
			}
			goto EXIT_OK;
		}

		if (!sema_expr_analyse_assign_right_side(context, NULL, decl->type, init, decl->var.failable || decl->var.unwrap ? FAILABLE_YES : FAILABLE_NO)) return decl_poison(decl);

		if (decl->type)
		{
			expr_set_type(decl->var.init_expr, decl->type);
		}

		if (type_is_inferred)
		{
			Type *right_side_type = init->type->canonical;
			assert(right_side_type->type_kind == TYPE_ARRAY);
			decl->type = type_get_array(decl->type->array.base, right_side_type->array.len);
		}

		if (decl->var.unwrap && !init->failable)
		{
			SEMA_ERROR(decl->var.init_expr, "A failable expression was expected here.");
			return decl_poison(decl);
		}

	}
	EXIT_OK:
	if (decl->var.is_static)
	{
		scratch_buffer_clear();
		scratch_buffer_append(context->active_function_for_analysis->name);
		scratch_buffer_append_char('.');
		scratch_buffer_append(decl->name);
		decl->external_name = scratch_buffer_interned();
	}
	if (decl->var.init_expr && decl->var.is_static)
	{
		if (!expr_is_constant_eval(decl->var.init_expr))
		{
			SEMA_ERROR(decl->var.init_expr, "Static variable initialization must be constant.");
			return false;
		}
	}
	if (!decl->alignment) decl->alignment = type_alloca_alignment(decl->type);
	return true;
}

static inline bool sema_analyse_declare_stmt(Context *context, Ast *statement)
{
	return sema_analyse_local_decl(context, statement->declare_stmt);
}

static inline bool sema_analyse_define_stmt(Context *context, Ast *statement)
{
	Decl *decl = statement->declare_stmt;
	statement->ast_kind = AST_NOP_STMT;
	assert(decl->decl_kind == DECL_VAR);
	switch (decl->var.kind)
	{
		case VARDECL_LOCAL_CT_TYPE:
			if (decl->var.type_info && !sema_resolve_type_info(context, decl->var.type_info)) return false;
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
					if (!sema_analyse_expr_of_required_type(context, decl->type, decl->var.init_expr, false)) return false;
					if (decl->var.init_expr->expr_kind != EXPR_CONST)
					{
						SEMA_ERROR(decl->var.init_expr, "Expected a constant expression here.");
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
				if (decl->var.init_expr)
				{
					if (!sema_analyse_expr(context, NULL, decl->var.init_expr)) return false;
					if (decl->var.init_expr->expr_kind != EXPR_CONST)
					{
						SEMA_ERROR(decl->var.init_expr, "Expected a constant expression here.");
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
	if (!sema_analyse_expr(context, NULL, statement->expr_stmt)) return false;
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
			success = sema_analyse_decl_expr_list(context, statement->for_stmt.init);
		}

		if (success && statement->for_stmt.cond)
		{
			// Conditional scope start
			SCOPE_START
				Expr *cond = statement->for_stmt.cond;
				success = sema_analyse_expr_of_required_type(context, type_bool, cond, false);
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
				success = sema_analyse_expr(context, NULL, incr);
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


static inline bool sema_analyse_foreach_stmt(Context *context, Ast *statement)
{
	// Pull out the relevant data.
	Decl *index = statement->foreach_stmt.index;
	Decl *var = statement->foreach_stmt.variable;
	Expr *enumerator = statement->foreach_stmt.enumeration;
	Ast *body = statement->foreach_stmt.body;
	bool success = true;
	Type *indexed_type;

	// First fold the enumerator expression, removing any () around it.
	while (enumerator->expr_kind == EXPR_GROUP) enumerator = enumerator->group_expr;


	// First analyse the enumerator.

	// Conditional scope start
	SCOPE_START_WITH_FLAGS(SCOPE_COND)

		// In the case of foreach (int x : { 1, 2, 3 }) we will infer the int[] type, so pick out the number of elements.
		Type *inferred_type = NULL;

		// We may have an initializer list, in this case we rely on an inferred type.
		if (enumerator->expr_kind == EXPR_INITIALIZER_LIST)
		{
			bool may_be_array;
			bool is_const_size;
			ArrayIndex size = sema_get_initializer_const_array_size(context, enumerator, &may_be_array, &is_const_size);
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
			inferred_type = type_get_array(var->var.type_info->type, size);
		}

		// because we don't want the index + variable to move into the internal scope
		if (!sema_analyse_expr(context, inferred_type, enumerator))
		{
			// Exit early here, because semantic checking might be messed up otherwise.
			return SCOPE_POP_ERROR();
		}

		// Check that we can even index this expression.
		indexed_type = type_get_indexed_type(enumerator->type);
		if (!indexed_type)
		{
			SEMA_ERROR(enumerator, "It's not possible to enumerate an expression of type '%s'.", type_to_error_string(enumerator->type));
			return SCOPE_POP_ERROR();
		}

		// Pop any possible defers.
		enumerator = context_pop_defers_and_wrap_expr(context, enumerator);

		// And pop the cond scope.
	SCOPE_END;

	// Enter foreach scope, to put index + variable into the internal scope.
	// the foreach may be labelled.
	SCOPE_START_WITH_LABEL(statement->foreach_stmt.flow.label)

		if (index)
		{
			if (statement->foreach_stmt.index_by_ref)
			{
				SEMA_ERROR(index, "The index cannot be held by reference, did you accidentally add a '&'?");
				return SCOPE_POP_ERROR();
			}
			// Set type to isize if missing.
			if (!index->var.type_info)
			{
				index->var.type_info = type_info_new_base(type_isize, index->span);
			}
			// Analyse the declaration.
			if (!sema_analyse_local_decl(context, index)) return SCOPE_POP_ERROR();

			// Make sure that the index is an integer.
			if (!type_is_integer(type_flatten(index->type)))
			{
				SEMA_ERROR(index->var.type_info,
				           "Index must be an integer type, '%s' is not valid.",
				           type_to_error_string(index->type));
				return SCOPE_POP_ERROR();
			}
			if (index->var.failable)
			{
				SEMA_ERROR(index->var.type_info, "The index may not be a failable.");
				return SCOPE_POP_ERROR();
			}
		}

		// Find the expected type for the value.
		Type *expected_var_type = indexed_type;

		// If by ref, then the pointer.
		if (statement->foreach_stmt.value_by_ref) expected_var_type = type_get_ptr(expected_var_type);

		// If we type infer, then the expected type is the same as the indexed type.
		if (!var->var.type_info)
		{
			var->var.type_info = type_info_new_base(expected_var_type, var->span);
		}

		// Analyse the value declaration.
		if (!sema_analyse_local_decl(context, var)) return SCOPE_POP_ERROR();

		if (var->var.failable)
		{
			SEMA_ERROR(var->var.type_info, "The variable may not be a failable.");
			return SCOPE_POP_ERROR();
		}
		// TODO consider failables.

		// We may have an implicit cast happening.
		statement->foreach_stmt.cast = CAST_ERROR;

		// If the type is different, attempt an implicit cast.
		if (var->type != expected_var_type)
		{
			// This is hackish, replace when cast is refactored.
			Expr dummy = { .resolve_status = RESOLVE_DONE, .span = { var->var.type_info->span.loc,
			                                                         var->span.end_loc }, .expr_kind = EXPR_IDENTIFIER, .type = expected_var_type, .original_type = expected_var_type };
			if (!cast_implicit(&dummy, var->type)) return SCOPE_POP_ERROR();

			assert(dummy.expr_kind == EXPR_CAST);
			statement->foreach_stmt.cast = dummy.cast_expr.kind;
		}

		// Push the statement on the break/continue stack.
		PUSH_BREAKCONT(statement);

		success = sema_analyse_statement(context, body);
		statement->foreach_stmt.flow.no_exit = context->active_scope.jump_end;

		// Pop the stack.
		POP_BREAKCONT();

		// End foreach scope
		context_pop_defers_and_replace_ast(context, body);

	SCOPE_END;

	return success;


}



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
		success = sema_analyse_cond(context, cond, true);
		if (statement->if_stmt.else_body)
		{
			if (statement->if_stmt.then_body->ast_kind != AST_COMPOUND_STMT)
			{
				SEMA_ERROR(statement->if_stmt.then_body,
				           "if-statements with an 'else' must use '{ }' even around a single statement.");
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
		if (success && statement->if_stmt.then_body->ast_kind != AST_COMPOUND_STMT)
		{
			SourceLocation *end_of_cond = TOKLOC(cond->span.end_loc);
			SourceLocation *start_of_then = TOKLOC(statement->if_stmt.then_body->span.loc);
			if (end_of_cond->line != start_of_then->line)
			{
				SEMA_ERROR(statement->if_stmt.then_body,
				           "The 'then' part of a single line if-statement must start on the same line as the 'if' or use '{ }'");
				success = false;
			}
		}
		if (context->active_scope.jump_end && !context->active_scope.allow_dead_code)
		{
			SEMA_ERROR(statement->if_stmt.then_body, "This code can never be executed.");
			success = false;
		}

		SCOPE_START_WITH_LABEL(statement->if_stmt.flow.label);

			success = success && sema_analyse_statement(context, statement->if_stmt.then_body);
			then_jump = context->active_scope.jump_end;

		SCOPE_END;

		else_jump = false;
		if (statement->if_stmt.else_body)
		{
			SCOPE_START_WITH_LABEL(statement->if_stmt.flow.label);
				success = success && sema_analyse_statement(context, statement->if_stmt.else_body);
				else_jump = context->active_scope.jump_end;
			SCOPE_END;
		}

		context_pop_defers_and_replace_ast(context, statement);

	SCOPE_OUTER_END;

	if (then_jump && else_jump && !statement->flow.has_break)
	{
		context->active_scope.jump_end = true;
	}
	return success;
}





static bool sema_analyse_asm_stmt(Context *context __unused, Ast *statement __unused)
{
	TODO
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
			SEMA_ERROR(statement, "'break' is not allowed here.");
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

static bool sema_analyse_next_stmt(Context *context, Ast *statement)
{
	context->active_scope.jump_end = true;
	if (!context->next_target && !statement->next_stmt.label.name)
	{
		SEMA_ERROR(statement, "'next' is not allowed here.");
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
		if (kind != AST_SWITCH_STMT && kind != AST_CATCH_STMT)
		{
			SEMA_TOKID_ERROR(statement->next_stmt.label.span, "Expected the label to match a 'switch' or 'catch' statement.");
			return false;
		}
		bool defer_mismatch = false;
		if (parent->ast_kind == AST_SWITCH_STMT)
		{
			defer_mismatch = context->active_scope.in_defer != parent->switch_stmt.scope_defer;
		}
		else
		{
			defer_mismatch = context->active_scope.in_defer != parent->catch_stmt.scope_defer;
		}
		if (defer_mismatch)
		{
			SEMA_ERROR(statement, "This 'next' would jump out of a defer which is not allowed.");
			return false;
		}
		assert(statement->next_stmt.target);
	}

	statement->next_stmt.defers.start = context->active_scope.defer_last;
	statement->next_stmt.defers.end = parent->switch_stmt.defer;
	// Plain next.
	if (!statement->next_stmt.target)
	{
		if (!context->next_target)
		{
			SEMA_ERROR(statement, "Unexpected 'next' statement outside of a switch.");
			return false;
		}
		statement->next_stmt.case_switch_stmt = context->next_target;
		return true;
	}

	if (statement->next_stmt.is_type)
	{
		if (!sema_resolve_type_info(context, statement->next_stmt.type_info)) return false;
		Ast **cases;
		if (parent->ast_kind == AST_SWITCH_STMT)
		{
			statement->next_stmt.defers.end = parent->switch_stmt.defer;
			if (parent->switch_stmt.cond->type->canonical != type_typeid)
			{
				SEMA_ERROR(statement, "Unexpected 'type' in as an 'next' destination.");
				SEMA_PREV(statement, "The 'switch' here uses expected a type '%s'.", type_to_error_string(parent->switch_stmt.cond->type));
				return false;
			}
			cases = parent->switch_stmt.cases;
		}
		else
		{
			statement->next_stmt.defers.end = parent->catch_stmt.defer;
			cases = parent->catch_stmt.cases;
		}

		Ast *default_stmt = NULL;
		VECEACH(cases, i)
		{
			Ast *case_stmt = cases[i];
			if (case_stmt->ast_kind == AST_DEFAULT_STMT)
			{
				default_stmt = case_stmt;
				break;
			}
			if (case_stmt->case_stmt.type_info->type->canonical == statement->next_stmt.type_info->type->canonical)
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
		SEMA_ERROR(statement->next_stmt.type_info, "There is no case for type '%s'.", type_to_error_string(statement->next_stmt.type_info->type));
		return false;
	}

	if (parent->ast_kind != AST_SWITCH_STMT)
	{
		SEMA_ERROR(statement, "The 'next' expected a type.");
	}
	Expr *target = statement->next_stmt.target;

	if (!sema_analyse_expr(context, parent->switch_stmt.cond->type, target)) return false;

	if (!cast_implicit(target, parent->switch_stmt.cond->type)) return false;

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
			if (expr_const_compare(&target->const_expr, &case_stmt->case_stmt.expr->const_expr, BINARYOP_EQ))
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
		SEMA_ERROR(statement, "The 'next' needs to jump to an exact case statement.");
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

/**
 * Cast the case expression to the switch type and ensure it is constant.
 *
 * @return true if the analysis succeeds.
 */
static bool sema_analyse_case_expr(Context *context, Type* to_type, Ast *case_stmt)
{
	assert(to_type);
	Expr *case_expr = case_stmt->case_stmt.expr;

	// TODO string switch.

	// 1. Try to do implicit conversion to the correct type.
	if (!sema_analyse_expr(context, to_type, case_expr)) return false;

	// 2. Skip continued analysis if it's not constant.
	if (case_expr->expr_kind != EXPR_CONST)
	{
		SEMA_ERROR(case_expr, "A case value must always be constant at compile time.");
		return false;
	}

	Type *case_type = case_expr->original_type->canonical;
	Type *to_type_canonical = to_type->canonical;

	// 3. If we already have the same type we're done.
	if (to_type_canonical == case_type) return true;

	// 4. Otherwise check if we have an enum receiving type and a number on
	//    in the case. In that case we do an implicit conversion.
	if (to_type_canonical->type_kind == TYPE_ENUM && type_is_any_integer(case_type))
	{
		return cast(case_expr, to_type);
	}

	return cast_implicit(case_expr, to_type);
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

static inline Type *ast_cond_type(Expr *expr)
{
	assert(expr->expr_kind == EXPR_DECL_LIST);
	Ast *last = VECLAST(expr->dexpr_list_expr);
	switch (last->ast_kind)
	{
		case AST_EXPR_STMT:
			return last->expr_stmt->type;
		case AST_DECLARE_STMT:
			return last->declare_stmt->var.type_info->type;
		default:
			UNREACHABLE
	}
}

static inline bool sema_check_type_case(Context *context, Type *switch_type, Ast *case_stmt, Ast **cases, unsigned index, bool use_type_id)
{
	if (!sema_resolve_type_info(context, case_stmt->case_stmt.type_info)) return false;
	Type *case_type = case_stmt->case_stmt.type_info->type;
	if (!use_type_id)
	{
		SEMA_ERROR(case_stmt, "Unexpected '%s' given case when a normal expression was expected.", type_to_error_string(case_type));
		return false;
	}
	if (switch_type == type_error && case_type->canonical->type_kind != TYPE_ERRTYPE)
	{
		if (case_type->canonical == type_error)
		{
			SEMA_ERROR(case_stmt, "In a catch, only use specific error types, never 'error'.");
			return false;
		}
		SEMA_ERROR(case_stmt, "Expected an error type here, not '%s'", type_to_error_string(case_type));
		return false;
	}
	for (unsigned i = 0; i < index; i++)
	{
		Ast *other = cases[i];
		if (other->ast_kind == AST_CASE_STMT && other->case_stmt.type_info->type->canonical == case_stmt->case_stmt.type_info->type->canonical)
		{
			SEMA_ERROR(case_stmt, "The same type appears more than once.");
			SEMA_PREV(other, "Here is the case with that type.");
			return false;
		}
	}
	return true;
}

static inline bool sema_check_value_case(Context *context, Type *switch_type, Ast *case_stmt, Ast **cases, unsigned index, bool use_type_id)
{
	if (!sema_analyse_case_expr(context, switch_type, case_stmt)) return false;
	Expr *expr = case_stmt->case_stmt.expr;
	if (use_type_id)
	{
		SEMA_ERROR(case_stmt, "Unexpected value of type '%s' when expecting a value.", type_to_error_string(expr->type));
		return false;
	}
	for (unsigned i = 0; i < index; i++)
	{
		Ast *other = cases[i];
		if (other->ast_kind == AST_CASE_STMT && expr_const_compare(&other->case_stmt.expr->const_expr, &expr->const_expr, BINARYOP_EQ))
		{
			SEMA_ERROR(case_stmt, "The same case value appears more than once.");
			SEMA_PREV(other, "Here is the previous use of that value.");
			return false;
		}
	}
	return true;
}

static bool sema_analyse_switch_body(Context *context, Ast *statement, SourceSpan expr_span, Type *switch_type, Ast **cases)
{
	bool use_type_id = false;
	Type *switch_type_flattened = type_flatten(switch_type);
	switch (switch_type_flattened->type_kind)
	{
		case TYPE_TYPEID:
		case TYPE_ERR_UNION:
			use_type_id = true;
			break;
		case ALL_INTS:
			assert(switch_type->type_kind != TYPE_IXX);
		case TYPE_BOOL:
		case TYPE_ENUM:
			break;
		case TYPE_DISTINCT:
			UNREACHABLE
		case TYPE_SUBARRAY:
			// Allow switching over char[] and String
			if (switch_type_flattened->array.base->type_kind == TYPE_U8) break;
			FALLTHROUGH;
		default:
			sema_error_range(expr_span, "It is not possible to switch over '%s'.", type_to_error_string(switch_type));
			return false;
	}
	Ast *default_case = NULL;
	assert(context->active_scope.defer_start == context->active_scope.defer_last);

	bool exhaustive = false;
	unsigned case_count = vec_size(cases);
	bool success = true;
	for (unsigned i = 0; i < case_count; i++)
	{
		Ast *stmt = cases[i];
		switch (stmt->ast_kind)
		{
			case AST_CASE_STMT:
				if (stmt->case_stmt.is_type)
				{
					if (!sema_check_type_case(context, switch_type, stmt, cases, i, use_type_id))
					{
						success = false;
						break;;
					}
				}
				else
				{
					if (!sema_check_value_case(context, switch_type, stmt, cases, i, use_type_id))
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
			success = success && (!body || sema_analyse_compound_statement_no_scope(context, body));
			POP_BREAK();
			POP_NEXT();
			all_jump_end &= (!body | context->active_scope.jump_end);
		SCOPE_END;
	}
	statement->flow.no_exit = all_jump_end;
	if (!success) return false;
	return success;
}

static bool sema_analyse_ct_switch_body(Context *context, Ast *statement)
{
	Expr *cond = statement->ct_switch_stmt.cond;
	bool use_type_id = cond->expr_kind == EXPR_TYPEID;
	Type *type = use_type_id ? cond->typeid_expr->type->canonical : NULL;
	Ast **cases = statement->ct_switch_stmt.body;
	unsigned case_count = vec_size(cases);

	Ast *match = NULL;
	for (unsigned i = 0; i < case_count; i++)
	{
		Ast *stmt = cases[i];
		switch (stmt->ast_kind)
		{
			case AST_CASE_STMT:
				if (use_type_id)
				{
					if (!stmt->case_stmt.is_type)
					{
						SEMA_ERROR(stmt, "Unexpectedly encountered a value rather than a type in the $case");
						return false;
					}
					TypeInfo *case_type = stmt->case_stmt.type_info;
					if (!sema_resolve_type_info(context, case_type)) return false;
					Type *case_canonical = case_type->type->canonical;
					if (case_canonical == type)
					{
						// Is this a better match?
						if (!match || match->case_stmt.type_info->type->canonical != type)
						{
							match = stmt;
						}
					}
					// TODO only do suptyping when explicit
					/*
					else if (!match && type_is_subtype(case_canonical, type))
					{
						match = stmt;
					}*/
				}
				else
				{
					if (stmt->case_stmt.is_type)
					{
						SEMA_ERROR(stmt, "Unexpectedly encountered a type rather than a value in the $case");
						return false;
					}
					if (!sema_analyse_expr_of_required_type(context,
					                                        cond->type,
					                                        stmt->case_stmt.expr,
					                                        false))
					{
						return false;
					}
					if (!match && expr_const_compare(&stmt->case_stmt.expr->const_expr, &cond->const_expr, BINARYOP_EQ))
					{
						match = stmt;
					}
				}
				break;
			case AST_DEFAULT_STMT:
				if (!match) match = stmt;
				break;
			default:
				UNREACHABLE;
		}
	}

	if (!match)
	{
		statement->ast_kind = AST_NOP_STMT;
		return true;
	}

	match = match->case_stmt.body;
	if (!sema_analyse_statement(context, match)) return false;

	*statement = *match;
	return true;
}

static bool sema_analyse_ct_switch_stmt(Context *context, Ast *statement)
{
	Expr *cond = statement->ct_switch_stmt.cond;
	if (!sema_analyse_expr(context, NULL, cond)) return false;
	if (cond->expr_kind != EXPR_CONST && cond->expr_kind != EXPR_TYPEID)
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
		if (!sema_analyse_cond(context, cond, false)) return false;


		Type *switch_type = ast_cond_type(cond)->canonical;
		statement->switch_stmt.defer = context->active_scope.defer_last;
		if (!sema_analyse_switch_body(context, statement, cond->span,
		                              switch_type->canonical,
		                              statement->switch_stmt.cases))
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

/**
 * Handle the catch statement
 * @return true if error checking succeeds.
 */
static bool sema_analyse_catch_stmt(Context *context, Ast *statement)
{
	Expr *catch_expr = statement->catch_stmt.catchable;
	Decl *error_var = NULL;
	Expr *error_expr = catch_expr;
	Decl *unwrapped = NULL;

	statement->catch_stmt.scope_defer = context->active_scope.in_defer;
	SCOPE_START_WITH_LABEL(statement->catch_stmt.flow.label);

		statement->catch_stmt.defer = context->active_scope.defer_last;

		if (catch_expr->expr_kind == EXPR_BINARY && catch_expr->binary_expr.operator == BINARYOP_ASSIGN)
		{
			Expr *left = catch_expr->binary_expr.left;
			if (left->expr_kind == EXPR_IDENTIFIER)
			{
				Decl *ambiguous_decl;
				Decl *dummy;
				Decl *error_var_decl = sema_resolve_normal_symbol(context,
				                                                  left->identifier_expr.identifier,
				                                                  left->identifier_expr.path, false);
				if (!error_var_decl)
				{
					error_var = decl_new_var(left->span.loc, type_info_new_base(type_error, left->span), VARDECL_LOCAL,
					                         VISIBLE_LOCAL);
					error_var->type = type_error;
					Expr *right = catch_expr->binary_expr.right;
					error_var->var.init_expr = right;
					error_expr = right;
					statement->catch_stmt.has_err_var = true;
					statement->catch_stmt.err_var = error_var;
				}
			}
		}

		if (!sema_analyse_expr(context, NULL, error_expr)) return SCOPE_POP_ERROR();

		if (error_var)
		{
			sema_add_local(context, error_var);
		}

		if (!error_expr->failable)
		{
			const char *error_type = type_to_error_string(error_expr->type);
			if (error_expr->expr_kind == EXPR_IDENTIFIER
			    && error_expr->identifier_expr.decl->decl_kind == DECL_VAR
			    && error_expr->identifier_expr.decl->var.kind == VARDECL_ALIAS)
			{
				SEMA_ERROR(error_expr,
				           "'%s' is unwrapped to '%s' here, so it cannot be caught.",
				           error_expr->identifier_expr.decl->name,
				           error_type);
			}
			else
			{
				SEMA_ERROR(error_expr, "Expected a failable '%s!' not '%s'.", error_type, error_type);
			}
			return SCOPE_POP_ERROR();
		}

		if (catch_expr->expr_kind == EXPR_IDENTIFIER)
		{
			unwrapped = catch_expr->identifier_expr.decl;
		}
		else if (error_var)
		{
			Expr *right = catch_expr->binary_expr.right;
			if (right->expr_kind == EXPR_IDENTIFIER) unwrapped = right->identifier_expr.decl;
		}

		if (statement->catch_stmt.is_switch)
		{
			if (!sema_analyse_switch_body(context,
			                              statement,
			                              error_expr->span,
			                              type_error,
			                              statement->catch_stmt.cases))
			{
				return SCOPE_POP_ERROR();
			}
		}
		else
		{
			if (!sema_analyse_statement(context, statement->catch_stmt.body)) return SCOPE_POP_ERROR();
			if (context->active_scope.jump_end) statement->flow.no_exit = true;
		}
		context_pop_defers_and_replace_ast(context, statement);

	SCOPE_END;

	if (unwrapped && !statement->flow.has_break && statement->flow.no_exit)
	{
		Decl *decl = decl_copy(unwrapped);
		decl->var.kind = VARDECL_ALIAS;
		decl->var.alias = unwrapped;
		decl->var.failable = false;
		sema_unwrap_var(context, decl);
	}
	return true;
}


static bool sema_analyse_try_stmt(Context *context, Ast *stmt)
{
	assert(stmt->try_stmt.decl_expr->expr_kind == EXPR_DECL_LIST);

	Ast **dexprs = stmt->try_stmt.decl_expr->dexpr_list_expr;
	SCOPE_START
		unsigned entries = vec_size(dexprs);
		for (unsigned i = 0; i < entries; i++)
		{
			Ast *ast = dexprs[i];
			if (ast->ast_kind == AST_DECLARE_STMT)
			{
				ast->declare_stmt->var.unwrap = true;
				if (!sema_analyse_statement(context, ast)) return SCOPE_POP_ERROR();
				continue;
			}

			if (!sema_analyse_statement(context, ast)) return SCOPE_POP_ERROR();

			Expr *expr = ast->expr_stmt;
			if (!expr->failable)
			{
				SEMA_ERROR(expr, "The expression to 'try' must be failable.");
				return SCOPE_POP_ERROR();
			}
			if (expr->expr_kind == EXPR_IDENTIFIER)
			{
				Decl *var = expr->identifier_expr.decl;
				Decl *decl = decl_copy(var);
				decl->var.kind = VARDECL_ALIAS;
				decl->var.alias = var;
				decl->var.failable = false;
				sema_unwrap_var(context, decl);
			}
		}
		if (!sema_analyse_statement(context, stmt->try_stmt.body))
		{
			return SCOPE_POP_ERROR();
		}
	SCOPE_END;
	return true;
}

static bool sema_analyse_volatile_stmt(Context *context, Ast *statement)
{
	context->in_volatile_section++;
	bool result = sema_analyse_statement(context, statement->volatile_stmt);
	context->in_volatile_section--;
	return result;
}

bool sema_analyse_ct_assert_stmt(Context *context, Ast *statement)
{
	Expr *expr = statement->ct_assert_stmt.expr;
	Expr *message = statement->ct_assert_stmt.message;
	if (message)
	{
		if (!sema_analyse_expr(context, type_compstr, message)) return false;
		if (message->type->type_kind != TYPE_STRLIT)
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

bool sema_analyse_assert_stmt(Context *context, Ast *statement)
{
	Expr *expr = statement->ct_assert_stmt.expr;
	Expr *message = statement->ct_assert_stmt.message;
	if (message)
	{
		if (!sema_analyse_expr(context, type_compstr, message)) return false;
		if (message->type->type_kind != TYPE_STRLIT)
		{
			SEMA_ERROR(message, "Expected a string as the error message.");
		}
	}
	if (!sema_analyse_expr_of_required_type(context, type_bool, expr, false)) return false;
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

static bool sema_analyse_yield_stmt(Context *context, Ast *stmt)
{
	Decl *macro = context->macro_scope.macro;
	if (!macro)
	{
		SEMA_ERROR(stmt, "'yield' can only be used in macros.");
		return false;
	}

	if (!macro->has_body_param)
	{
		SEMA_ERROR(stmt, "'yield' can only be used in macros that takes trailing bodies, use ';' after the regular parameters.");
		return false;
	}
	unsigned expressions = vec_size(stmt->yield_stmt.values);
	if (expressions != vec_size(macro->macro_decl.body_parameters))
	{
		SEMA_ERROR(stmt, "Expected %d parameter(s) for 'yield'.", vec_size(macro->macro_decl.body_parameters));
		return false;
	}
	for (unsigned i = 0; i < expressions; i++)
	{
		Expr *expr = stmt->yield_stmt.values[i];
		Decl *param = context->macro_scope.yield_args[i];
		if (!sema_analyse_expr(context, param->type, expr)) return false;
	}
	stmt->yield_stmt.declarations = context->macro_scope.yield_args;
	bool in_yield = context->macro_scope.in_yield;
	context->macro_scope.in_yield = true;
	stmt->yield_stmt.ast = copy_ast(context->macro_scope.yield_body);
	bool success = sema_analyse_statement(context, stmt->yield_stmt.ast);
	context->macro_scope.in_yield = in_yield;
	return success;
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
			UNREACHABLE
		case AST_YIELD_STMT:
			return sema_analyse_yield_stmt(context, statement);
		case AST_ASM_STMT:
			return sema_analyse_asm_stmt(context, statement);
		case AST_ASSERT_STMT:
			return sema_analyse_assert_stmt(context, statement);
		case AST_BREAK_STMT:
			return sema_analyse_break_stmt(context, statement);
		case AST_CASE_STMT:
			SEMA_ERROR(statement, "Unexpected 'case' outside of switch");
			return false;
		case AST_CATCH_STMT:
			return sema_analyse_catch_stmt(context, statement);
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
		case AST_DEFINE_STMT:
			return sema_analyse_define_stmt(context, statement);
		case AST_DO_STMT:
			return sema_analyse_do_stmt(context, statement);
		case AST_EXPR_STMT:
			return sema_analyse_expr_stmt(context, statement);
		case AST_FOREACH_STMT:
			return sema_analyse_foreach_stmt(context, statement);
		case AST_FOR_STMT:
			return sema_analyse_for_stmt(context, statement);
		case AST_TRY_STMT:
			return sema_analyse_try_stmt(context, statement);
		case AST_IF_STMT:
			return sema_analyse_if_stmt(context, statement);
		case AST_NOP_STMT:
			return true;
		case AST_RETURN_STMT:
			return sema_analyse_return_stmt(context, statement);
		case AST_SWITCH_STMT:
			return sema_analyse_switch_stmt(context, statement);
		case AST_NEXT_STMT:
			return sema_analyse_next_stmt(context, statement);
		case AST_UNREACHABLE_STMT:
			return sema_analyse_unreachable_stmt(context);
		case AST_VOLATILE_STMT:
			return sema_analyse_volatile_stmt(context, statement);
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
	context->macro_scope = (MacroScope) {};
	context->failable_return = signature->failable;

	// Clear returns
	vec_resize(context->returns, 0);
	context->scope_id = 0;
	context->expected_block_type = NULL;
	context->in_volatile_section = 0;
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
		if (!sema_analyse_compound_statement_no_scope(context, func->func_decl.body)) return false;
		assert(context->active_scope.depth == 1);
		if (!context->active_scope.jump_end)
		{
			Type *canonical_rtype = signature->rtype->type->canonical;
			if (canonical_rtype != type_void)
			{
				// IMPROVE better pointer to end.
				SEMA_ERROR(func, "Missing return statement at the end of the function.");
				return false;
			}
		}
	SCOPE_END;
	return true;
}

