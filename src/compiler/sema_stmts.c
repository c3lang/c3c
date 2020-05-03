// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"
#include "bigint.h"

#pragma mark --- Context help functions

void context_push_scope_with_flags(Context *context, ScopeFlags flags)
{
	if (context->current_scope == &context->scopes[MAX_SCOPE_DEPTH - 1])
	{
		FATAL_ERROR("Too deeply nested scopes.");
	}
	ScopeFlags previous_flags = context->current_scope->flags;
	Ast *parent_defer = context->current_scope->defers.start;
	context->current_scope++;
	context->current_scope->exit = EXIT_NONE;
	context->current_scope->local_decl_start = context->last_local;
	context->current_scope->defers.start = parent_defer;
	context->current_scope->defers.end = parent_defer;
	if (flags & (SCOPE_DEFER | SCOPE_EXPR_BLOCK | SCOPE_NEXT))
	{
		context->current_scope->flags = flags;
	}
	else
	{
		context->current_scope->flags = previous_flags | flags;
	}
	context->current_scope->flags_created = flags;
}

static inline void context_push_scope(Context *context)
{
	context_push_scope_with_flags(context, SCOPE_NONE);
}

static inline void context_pop_defers(Context *context)
{
	context->current_scope->defers.start = context->current_scope->defers.end;
}

static inline void context_pop_defers_to(Context *context, DeferList *list)
{
	*list = context->current_scope->defers;
	context_pop_defers(context);
}

static inline void context_add_exit(Context *context, ExitType exit)
{
	if (!context->current_scope->exit) context->current_scope->exit = exit;
}

void context_pop_scope(Context *context)
{
	assert(context->current_scope != &context->scopes[0]);
	context->last_local = context->current_scope->local_decl_start;
	ExitType exit_type = context->current_scope->exit;
	assert (context->current_scope->defers.end == context->current_scope->defers.start);
	context->current_scope--;
	if (!context->current_scope->exit && exit_type)
	{
		context->current_scope->exit = exit_type;
	}
}

static Expr *context_pop_defers_and_wrap_expr(Context *context, Expr *expr)
{
	DeferList defers = { NULL, NULL };
	context_pop_defers_to(context, &defers);
	if (defers.end == defers.start) return expr;
	Expr *wrap = expr_new(EXPR_SCOPED_EXPR, expr->span);
	wrap->type = expr->type;
	wrap->resolve_status = RESOLVE_DONE;
	wrap->expr_scope.expr = expr;
	wrap->expr_scope.defers = defers;
	return expr;
}

static void context_pop_defers_and_replace_ast(Context *context, Ast *ast)
{
	DeferList defers = { NULL, NULL };
	context_pop_defers_to(context, &defers);
	if (defers.end == defers.start) return;
	if (ast->ast_kind == AST_DEFER_STMT)
	{
		assert(defers.start == ast);
		*ast = *ast->defer_stmt.body;
		return;
	}
	assert(ast->ast_kind != AST_COMPOUND_STMT);
	Ast *replacement = malloc_arena(sizeof(Ast));
	*replacement = *ast;
	ast->ast_kind = AST_SCOPED_STMT;
	ast->scoped_stmt.stmt = replacement;
	ast->scoped_stmt.defers = defers;
}

#pragma mark --- Helper functions

#define UPDATE_EXIT(exit_type) \
 do { if (!context->current_scope->exit) context->current_scope->exit = exit_type; } while(0)

#pragma mark --- Sema analyse stmts


static inline bool sema_analyse_block_return_stmt(Context *context, Ast *statement)
{
	assert(context->current_scope->flags & SCOPE_EXPR_BLOCK);
	UPDATE_EXIT(EXIT_RETURN);
	if (statement->return_stmt.expr
	    && !sema_analyse_expr_of_required_type(context,
	                                           context->expected_block_type,
	                                           statement->return_stmt.expr))
	{
		return false;
	}
	vec_add(context->returns, statement);
	return true;
}

static inline bool sema_analyse_return_stmt(Context *context, Ast *statement)
{
	// This might be a return in a function block or a macro which must be treated differently.
	if (context->current_scope->flags & SCOPE_EXPR_BLOCK)
	{
		return sema_analyse_block_return_stmt(context, statement);
	}

	UPDATE_EXIT(EXIT_RETURN);

	Type *expected_rtype = context->rtype;
	Expr *return_expr = statement->return_stmt.expr;
	statement->return_stmt.defer = context->current_scope->defers.start;
	if (return_expr == NULL)
	{
		if (!expected_rtype)
		{
			assert(context->evaluating_macro);
			context->rtype = type_void;
			return true;
		}
		if (expected_rtype->canonical != type_void)
		{
			SEMA_ERROR(statement, "Expected to return a result of type %s.", type_to_error_string(expected_rtype));
			return false;
		}
		return true;
	}
	if (expected_rtype == type_void)
	{
		SEMA_ERROR(statement, "You can't return a value from a void function, you need to add a return type.");
		return false;
	}
	if (!sema_analyse_expr_of_required_type(context, expected_rtype, return_expr)) return false;
	if (!expected_rtype)
	{
		assert(context->evaluating_macro);
		context->rtype = type_void;
		context->active_function_for_analysis->func.function_signature.rtype->type->canonical = statement->return_stmt.expr->type->canonical;
		return true;
	}
	assert(statement->return_stmt.expr->type->canonical == expected_rtype->canonical);
	return true;
}


static inline bool sema_analyse_var_decl(Context *context, Decl *decl)
{
	assert(decl->decl_kind == DECL_VAR);
	if (!sema_resolve_type_info(context, decl->var.type_info)) return decl_poison(decl);
	decl->type = decl->var.type_info->type;
	if (decl->var.init_expr)
	{
		if (!sema_analyse_expr_of_required_type(context, decl->type, decl->var.init_expr)) return decl_poison(decl);
	}
	if (!sema_add_local(context, decl)) return decl_poison(decl);
	return true;
}


static inline bool sema_analyse_decl_expr_list(Context *context, Ast *stmt)
{
	assert(stmt->ast_kind == AST_DECL_EXPR_LIST);

	VECEACH(stmt->decl_expr_stmt, i)
	{
		if (!sema_analyse_statement(context, stmt->decl_expr_stmt[i])) return false;
	}

	return true;
}

static inline bool sema_analyse_cond(Context *context, Ast *stmt, bool cast_to_bool)
{
	assert(stmt->ast_kind == AST_DECL_EXPR_LIST);

	size_t size = vec_size(stmt->decl_expr_stmt);
	if (!size)
	{
		SEMA_ERROR(stmt, "Expected a boolean expression");
		return false;
	}

	if (!sema_analyse_decl_expr_list(context, stmt)) return false;

	Ast *last = stmt->decl_expr_stmt[size - 1];
	switch (last->ast_kind)
	{
		case AST_EXPR_STMT:
			if (cast_to_bool)
			{
				if (!cast_implicit(last->expr_stmt, type_bool)) return false;
			}
			return true;
		case AST_DECLARE_STMT:
		{
			Expr *init = last->declare_stmt->var.init_expr;
			if (!init)
			{
				SEMA_ERROR(last, "Expected a declaration with initializer.");
				return false;
			}
			if (cast_to_bool && init->type->type_kind != TYPE_BOOL &&
			    cast_to_bool_kind(last->declare_stmt->var.type_info->type) == CAST_ERROR)
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

static inline bool sema_analyse_while_stmt(Context *context, Ast *statement)
{
	Ast *decl = statement->while_stmt.decl;
	Ast *cond = statement->while_stmt.cond;
	Ast *body = statement->while_stmt.body;
	context_push_scope(context);
	bool success = !decl || sema_analyse_statement(context, decl);
	context_push_scope(context);

	success = success && sema_analyse_cond(context, cond, true);
	context_push_scope_with_flags(context, SCOPE_BREAK | SCOPE_CONTINUE); // NOLINT(hicpp-signed-bitwise)
	success = success && sema_analyse_statement(context, body);
	context_pop_defers_and_replace_ast(context, body);
	context_pop_scope(context);
	context_pop_defers_and_replace_ast(context, cond);
	context_pop_scope(context);
	context_pop_defers_and_replace_ast(context, statement);
	context_pop_scope(context);
	if (!success) return false;
	return success;
}

static inline bool sema_analyse_do_stmt(Context *context, Ast *statement)
{
	Expr *expr = statement->do_stmt.expr;
	Ast *body = statement->do_stmt.body;
	bool success;
	context_push_scope_with_flags(context, SCOPE_BREAK | SCOPE_CONTINUE); // NOLINT(hicpp-signed-bitwise)
	success = sema_analyse_statement(context, body);
	context_pop_defers_and_replace_ast(context, body);
	context_pop_scope(context);
	if (!success) return false;
	context_push_scope(context);
	success = sema_analyse_expr_of_required_type(context, type_bool, expr);
	statement->do_stmt.expr = context_pop_defers_and_wrap_expr(context, expr);
	context_pop_scope(context);
	return success;
}


static inline bool sema_analyse_declare_stmt(Context *context, Ast *statement)
{
	Decl *decl = statement->declare_stmt;
	return sema_analyse_var_decl(context, decl);
}

static inline bool sema_analyse_expr_stmt(Context *context, Ast *statement)
{
	if (!sema_analyse_expr(context, NULL, statement->expr_stmt)) return false;
	return true;
}

static inline bool sema_analyse_defer_stmt(Context *context, Ast *statement)
{
	// TODO special parsing of "catch"
	context_push_scope_with_flags(context, SCOPE_DEFER | SCOPE_CONTINUE); // NOLINT(hicpp-signed-bitwise)
	// Only ones allowed.
	context->current_scope->flags &= SCOPE_DEFER | SCOPE_CONTINUE; // NOLINT(hicpp-signed-bitwise)

	bool success = sema_analyse_statement(context, statement->defer_stmt.body);

	context_pop_scope(context);

	if (!success) return false;

	statement->defer_stmt.prev_defer = context->current_scope->defers.start;
	context->current_scope->defers.start = statement;
	return true;
}


static inline bool sema_analyse_for_stmt(Context *context, Ast *statement)
{
	bool success = true;

	// Enter for scope
	context_push_scope(context);
	if (statement->for_stmt.init)
	{
		success = sema_analyse_statement(context, statement->for_stmt.init);
	}
	if (success && statement->for_stmt.cond)
	{
		// Conditional scope start
		context_push_scope(context);
		Expr *cond = statement->for_stmt.cond;
		success = sema_analyse_expr_of_required_type(context, type_bool, cond);
		statement->for_stmt.cond = context_pop_defers_and_wrap_expr(context, cond);
		// Conditional scope end
		context_pop_scope(context);
	}
	if (success && statement->for_stmt.incr)
	{
		// Incr scope start
		context_push_scope(context);
		Expr *incr = statement->for_stmt.incr;
		success = sema_analyse_expr(context, NULL, incr);
		statement->for_stmt.incr = context_pop_defers_and_wrap_expr(context, incr);
		// Incr scope end
		context_pop_scope(context);
	}
	if (!success) return false;

	// Create the for body scope.
	context_push_scope_with_flags(context, SCOPE_BREAK | SCOPE_CONTINUE); // NOLINT(hicpp-signed-bitwise)
	success = sema_analyse_statement(context, statement->for_stmt.body);
	// End for body scope
	context_pop_defers_and_replace_ast(context, statement->for_stmt.body);
	context_pop_scope(context);
	context_pop_defers_and_replace_ast(context, statement);
	// End for scope
	context_pop_scope(context);
	return success;
}

static inline bool sema_analyse_goto_stmt(Context *context, Ast *statement)
{

	statement->goto_stmt.defer = context->current_scope->defers;
	VECEACH(context->labels, i)
	{
		Ast *label = context->labels[i];
		if (statement->goto_stmt.label_name == label->label_stmt.name)
		{
			label->label_stmt.is_used = true;
			statement->goto_stmt.label = label;
		}
	}
	vec_add(context->gotos, statement);
	context_add_exit(context, EXIT_GOTO);
	return true;
}

static inline bool sema_analyse_if_stmt(Context *context, Ast *statement)
{
	// IMPROVE
	// convert
	// if (!x) A(); else B();
	// into
	// if (x) B(); else A();
	Ast *cond = statement->if_stmt.cond;
	context_push_scope(context);
	bool success = sema_analyse_cond(context, cond, true);
	if (statement->if_stmt.else_body)
	{
		if (statement->if_stmt.then_body->ast_kind != AST_COMPOUND_STMT)
		{
			SEMA_ERROR(statement->if_stmt.then_body,
			           "if-statements with an 'else' must use '{ }' even around a single statement.");
			success = false;
		}
		if (success && statement->if_stmt.else_body->ast_kind != AST_COMPOUND_STMT)
		{
			SEMA_ERROR(statement->if_stmt.else_body,
			           "An 'else' must use '{ }' even around a single statement.");
			success = false;
		}
	}
	ExitType prev_exit = context->current_scope->exit;
	success = success && sema_analyse_statement(context, statement->if_stmt.then_body);
	ExitType if_exit = context->current_scope->exit;
	ExitType else_exit = prev_exit;
	if (statement->if_stmt.else_body)
	{
		context->current_scope->exit = prev_exit;
		success = success && sema_analyse_statement(context, statement->if_stmt.else_body);
		else_exit = context->current_scope->exit;
	}
	context->current_scope->exit = else_exit < if_exit ? else_exit : if_exit;
	context_pop_defers_and_replace_ast(context, statement);
	context_pop_scope(context);
	return success;
}

static inline bool sema_analyse_label(Context *context, Ast *statement)
{
	statement->label_stmt.defer = context->current_scope->defers.start;
	VECEACH(context->labels, i)
	{
		Ast *label = context->labels[i];
		if (label->label_stmt.name == statement->label_stmt.name)
		{
			SEMA_ERROR(statement, "This duplicate label '%s'.", statement->label_stmt.name);
			sema_prev_at_range(label->span, "The previous declaration was here.");
			ast_poison(label);
			ast_poison(statement);
			return false;
		}
	}
	context->current_scope->exit = EXIT_NONE;
	vec_add(context->labels, statement);
	VECEACH(context->gotos, i)
	{
		Ast *the_goto = context->gotos[i];
		if (the_goto->goto_stmt.label_name == statement->label_stmt.name)
		{
			the_goto->goto_stmt.label = statement;
			statement->label_stmt.is_used = true;
			break;
		}
	}
	return true;
}


static inline bool throw_completely_handled_call_throw_many(Throw *throw)
{
	assert(throw->kind == THROW_TYPE_CALL_THROW_MANY && "Only for throw many");
	assert(!throw->throw_info->is_completely_handled && "Expected unhandled");
	Decl **throws = throw->throws;
	CatchInfo *catched = throw->throw_info->catches;
	unsigned catches = 0;
	unsigned throw_count = vec_size(throws);
	for (unsigned i = 0; i < throw_count; i++)
	{
		Decl *throw_decl = throws[i];
		if (throw_completely_caught(throw_decl, catched))
		{
			catches++;
		}
	}
	return catches == throw_count;
}

/**
 * Handle the catch statement
 * @return true if error checking succeeds.
 */
static bool sema_analyse_catch_stmt(Context *context, Ast *statement)
{
	unsigned throws = vec_size(context->error_calls);

	// 1. If no errors are found we don't have a try to match.
	if (throws <= context->current_scope->throws)
	{
		SEMA_ERROR(statement, "Unexpected 'catch' without a matching 'try'.");
		return false;
	}

	// 2. Let's check that we haven't caught all errors.
	bool found = false;
	for (unsigned i = context->current_scope->throws; i < throws; i++)
	{
		if (!context->error_calls[i].throw_info->is_completely_handled)
		{
			found = true;
			break;;
		}
	}
	// IMPROVE: Suppress for macro?
	if (!found)
	{
		SEMA_ERROR(statement, "All errors are already caught, so this catch will not handle any errors.");
		return false;
	}

	// 3. Resolve variable
	Decl *variable = statement->catch_stmt.error_param;
	assert(variable->var.kind == VARDECL_LOCAL);
	if (!sema_resolve_type_info(context, variable->var.type_info)) return false;
	variable->type = variable->var.type_info->type;
	Type *error_type = variable->type->canonical;

	CatchInfo catch = { .kind = CATCH_REGULAR, .catch = statement };
	// 4. Absorb all errors in case of a type error union.
	if (error_type == type_error_union)
	{
		for (unsigned i = context->current_scope->throws; i < throws; i++)
		{
			Throw *throw = &context->error_calls[i];
			// Skip handled errors
			if (throw->throw_info->is_completely_handled) continue;
			vec_add(throw->throw_info->catches, catch);
			throw->throw_info->is_completely_handled = true;
		}
		// Resize to remove the throws from consideration.
		vec_resize(context->error_calls, context->current_scope->throws);
	}
	else
	{
		// 5. Otherwise, go through the list of errors and null the errors matching the current type.
		for (unsigned i = context->current_scope->throws; i < throws; i++)
		{
			Throw *throw = &context->error_calls[i];
			// Skip handled errors
			if (throw->throw_info->is_completely_handled) continue;

			switch (throw->kind)
			{
				case THROW_TYPE_CALL_ANY:
					vec_add(throw->throw_info->catches, catch);
					// An error union can never be completely handled.
					break;
				case THROW_TYPE_CALL_THROW_ONE:
					// If there is no match, ignore.
					if (throw->throw != error_type) continue;
					// Otherwise add and set to completely handled.
					vec_add(throw->throw_info->catches, catch);
					throw->throw_info->is_completely_handled = true;
					break;
				case THROW_TYPE_CALL_THROW_MANY:
					// The most complex situation, add and handle below.
					vec_add(throw->throw_info->catches, catch);
					throw->throw_info->is_completely_handled = throw_completely_handled_call_throw_many(throw);
					break;
			}
		}
	}
	context_push_scope(context);
	// Push the error variable
	if (!sema_add_local(context, variable)) goto ERR_END_SCOPE;
	if (!sema_analyse_statement(context, statement->catch_stmt.body)) goto ERR_END_SCOPE;
	context_pop_scope(context);
	return true;

	ERR_END_SCOPE:
	context_pop_scope(context);
	return false;
}

static bool sema_analyse_asm_stmt(Context *context __unused, Ast *statement __unused)
{
	TODO
}


static bool sema_analyse_break_stmt(Context *context, Ast *statement)
{
	if (!(context->current_scope->flags & SCOPE_BREAK))  // NOLINT(hicpp-signed-bitwise)
	{
		SEMA_ERROR(statement, "'break' is not allowed here.");
		return false;
	}
	UPDATE_EXIT(EXIT_BREAK);
	DynamicScope *scope = context->current_scope;
	statement->break_stmt.defers.start = scope->defers.start;
	while (!(scope->flags_created & SCOPE_BREAK)) // NOLINT(hicpp-signed-bitwise)
	{
		scope--;
	}
	statement->break_stmt.defers.end = scope->defers.end;
	return true;
}

static bool sema_analyse_next_stmt(Context *context, Ast *statement)
{
	if (!(context->current_scope->flags & SCOPE_NEXT))  // NOLINT(hicpp-signed-bitwise)
	{
		SEMA_ERROR(statement, "'next' is not allowed here.");
		return false;
	}
	UPDATE_EXIT(EXIT_NEXT);
	DynamicScope *scope = context->current_scope;
	statement->next_stmt.defers.start = scope->defers.start;
	while (!(scope->flags_created & SCOPE_NEXT)) // NOLINT(hicpp-signed-bitwise)
	{
		scope--;
	}
	statement->next_stmt.defers.end = scope->defers.end;
	return true;
}


static bool sema_analyse_continue_stmt(Context *context, Ast *statement)
{
	if (!(context->current_scope->flags & SCOPE_CONTINUE))  // NOLINT(hicpp-signed-bitwise)
	{
		SEMA_ERROR(statement, "'continue' is not allowed here.");
		return false;
	}
	UPDATE_EXIT(EXIT_CONTINUE);
	DynamicScope *scope = context->current_scope;
	statement->continue_stmt.defers.start = scope->defers.start;
	while (!(scope->flags_created & SCOPE_CONTINUE)) // NOLINT(hicpp-signed-bitwise)
	{
		scope--;
	}
	statement->continue_stmt.defers.end = scope->defers.end;
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

	// 1. Try to do implicit conversion to the correct type.
	if (!sema_analyse_expr(context, to_type, case_expr)) return false;

	// 2. Skip continued analysis if it's not constant.
	if (case_expr->expr_kind != EXPR_CONST)
	{
		SEMA_ERROR(case_expr, "A case value must always be constant at compile time.");
		return false;
	}

	Type *case_type = case_expr->type->canonical;
	Type *to_type_canonical = to_type->canonical;

	// 3. If we already have the same type we're done.
	if (to_type_canonical == case_type) return true;

	// 4. Otherwise check if we have an enum receiving type and a number on
	//    in the case. In that case we do an implicit conversion.
	if (to_type_canonical->type_kind == TYPE_ENUM && type_is_any_integer(case_expr->type))
	{
		return cast(case_expr, to_type, CAST_TYPE_EXPLICIT);
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

static inline Type *ast_cond_type(Ast *ast)
{
	assert(ast->ast_kind == AST_DECL_EXPR_LIST);
	Ast *last = VECLAST(ast->decl_expr_stmt);
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


static bool sema_analyse_switch_stmt(Context *context, Ast *statement)
{
	context_push_scope(context);
	Ast *cond = statement->switch_stmt.cond;
	bool success = sema_analyse_cond(context, cond, false);

	Type *switch_type = ast_cond_type(cond)->canonical;
	switch (switch_type->type_kind)
	{
		case ALL_INTS:
			assert(switch_type->type_kind != TYPE_IXX);
		case TYPE_BOOL:
		case TYPE_ERROR:
		case TYPE_META_TYPE:
		case TYPE_ENUM:
		case TYPE_STRING:
			break;
		default:
			SEMA_ERROR(cond, "It is not possible to switch over '%s'.", type_to_error_string(switch_type));
			return false;
	}
	Ast *default_case = NULL;
	assert(context->current_scope->defers.start == context->current_scope->defers.end);

	ExitType prev_exit = context->current_scope->exit;
	bool exhaustive = false;
	ExitType lowest_exit = EXIT_NONE;
	unsigned cases = vec_size(statement->switch_stmt.cases);
	for (unsigned i = 0; i < cases; i++)
	{
		context->current_scope->exit = prev_exit;
		Ast *stmt = statement->switch_stmt.cases[i];
		switch (stmt->ast_kind)
		{
			case AST_CASE_STMT:
				if (!sema_analyse_case_expr(context, switch_type, stmt))
				{
					success = false;
					break;
				}
				for (unsigned j = 0; j < i; j++)
				{
					Ast *other = statement->switch_stmt.cases[j];
					if (other->ast_kind == AST_CASE_STMT && expr_const_compare(&other->case_stmt.expr->const_expr, &stmt->case_stmt.expr->const_expr, BINARYOP_EQ))
					{
						SEMA_ERROR(stmt, "The same case value appears more than once.");
						SEMA_PREV(other, "Here is the previous use of that value.");
						success = false;
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
		if (i == cases - 1)
		{
			context_push_scope_with_flags(context, SCOPE_BREAK);
		}
		else
		{
			context_push_scope_with_flags(context, SCOPE_NEXT | SCOPE_BREAK);
		}
		success = success && (!stmt->case_stmt.body || sema_analyse_compound_statement_no_scope(context, stmt->case_stmt.body));
		ExitType case_exit = context->current_scope->exit;
		if (case_exit != lowest_exit)
		{
			switch (case_exit)
			{
				case EXIT_NONE:
				case EXIT_BREAK:
					lowest_exit = EXIT_BREAK;
					break;
				case EXIT_NEXT:
					// We ignore this completely
					break;
				default:
					if (!lowest_exit || lowest_exit > case_exit) lowest_exit = case_exit;
					break;
			}
		}
		context_pop_scope(context);
	}
	context_pop_defers_and_replace_ast(context, statement);
	if (lowest_exit <= EXIT_BREAK) lowest_exit = prev_exit;
	// Check exhaustive use.
	context->current_scope->exit = exhaustive ? lowest_exit : EXIT_NONE;
	context_pop_scope(context);
	if (!success) return false;
	return success;
}


static bool sema_analyse_throw_stmt(Context *context, Ast *statement)
{
	Expr *throw_value = statement->throw_stmt.throw_value;
	UPDATE_EXIT(EXIT_THROW);
	if (!sema_analyse_expr(context, NULL, throw_value)) return false;
	Type *type = throw_value->type->canonical;
	if (type->type_kind != TYPE_ERROR && type->type_kind != TYPE_ERROR_UNION)
	{
		SEMA_ERROR(throw_value, "Only 'error' types can be thrown, this is a '%s'.", type->name);
		return false;
	}
	FunctionSignature *sig = &context->active_function_for_analysis->func.function_signature;
	if (sig->error_return == ERROR_RETURN_NONE)
	{
		SEMA_ERROR(statement, "This throw requires that the function adds 'throws %s' to its declaration.", type->name);
		return false;
	}

	// Check if the error is actually in the list.
	if (sig->error_return == ERROR_RETURN_MANY || sig->error_return == ERROR_RETURN_ONE)
	{
		bool found = false;
		VECEACH(sig->throws, i)
		{
			if (sig->throws[i]->type == type)
			{
				found = true;
			}
		}
		if (!found)
		{
			if (type != type_error_union)
			{
				SEMA_ERROR(statement->throw_stmt.throw_value, "'%s' must be added to the list of errors after 'throws'.", type->name);
			}
			else
			{
				SEMA_ERROR(statement, "This throw requires the function to use a wildcard 'throws' without types.", type->name);
			}
			return false;
		}
	}

	vec_add(context->throw, statement);
	return true;
}


static bool sema_analyse_volatile_stmt(Context *context, Ast *statement)
{
	context->in_volatile_section++;
	bool result = sema_analyse_statement(context, statement->volatile_stmt);
	context->in_volatile_section--;
	return result;
}

static bool sema_analyse_compound_stmt(Context *context, Ast *statement)
{
	context_push_scope(context);
	bool success = sema_analyse_compound_statement_no_scope(context, statement);
	context_pop_scope(context);
	return success;
}


static inline bool sema_analyse_statement_inner(Context *context, Ast *statement)
{
	switch (statement->ast_kind)
	{
		case AST_POISONED:
			return false;
		case AST_ATTRIBUTE:
		case AST_SCOPED_STMT:
			UNREACHABLE
		case AST_ASM_STMT:
			return sema_analyse_asm_stmt(context, statement);
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
		case AST_CT_IF_STMT:
			return sema_analyse_ct_if_stmt(context, statement);
		case AST_DECLARE_STMT:
			return sema_analyse_declare_stmt(context, statement);
		case AST_DEFAULT_STMT:
			SEMA_ERROR(statement, "Unexpected 'default' outside of switch");
			return false;
		case AST_DEFER_STMT:
			return sema_analyse_defer_stmt(context, statement);
		case AST_DO_STMT:
			return sema_analyse_do_stmt(context, statement);
		case AST_EXPR_STMT:
			return sema_analyse_expr_stmt(context, statement);
		case AST_FOR_STMT:
			return sema_analyse_for_stmt(context, statement);
		case AST_GOTO_STMT:
			return sema_analyse_goto_stmt(context, statement);
		case AST_IF_STMT:
			return sema_analyse_if_stmt(context, statement);
		case AST_LABEL:
			return sema_analyse_label(context, statement);
		case AST_NOP_STMT:
			return true;
		case AST_RETURN_STMT:
			return sema_analyse_return_stmt(context, statement);
		case AST_SWITCH_STMT:
			return sema_analyse_switch_stmt(context, statement);
		case AST_THROW_STMT:
			return sema_analyse_throw_stmt(context, statement);
		case AST_NEXT_STMT:
			return sema_analyse_next_stmt(context, statement);
		case AST_VOLATILE_STMT:
			return sema_analyse_volatile_stmt(context, statement);
		case AST_WHILE_STMT:
			return sema_analyse_while_stmt(context, statement);
		case AST_DECL_EXPR_LIST:
			return sema_analyse_decl_expr_list(context, statement);
		case AST_CT_ELIF_STMT:
		case AST_CT_ELSE_STMT:
			UNREACHABLE
		case AST_CT_FOR_STMT:
		case AST_CT_SWITCH_STMT:
		case AST_CT_DEFAULT_STMT:
		case AST_CT_CASE_STMT:
		case AST_GENERIC_CASE_STMT:
		case AST_GENERIC_DEFAULT_STMT:
			TODO
	}
	UNREACHABLE
}

bool sema_analyse_statement(Context *context, Ast *statement)
{
	if (sema_analyse_statement_inner(context, statement)) return true;
	return ast_poison(statement);
}

static inline int defer_depth(Ast *defer_stmt)
{
	int depth = 0;
	while (defer_stmt)
	{
		defer_stmt = defer_stmt->defer_stmt.prev_defer;
		depth++;
	}
	return depth;
}

static inline void defer_list_walk_to_common_depth(Ast **defer_stmt, int this_depth, int other_depth)
{
	int steps = this_depth - other_depth;
	for (int i = 0; i < steps; i++)
	{
		*defer_stmt = (*defer_stmt)->defer_stmt.prev_defer;
	}
}

static inline bool throw_add_error_return_catch(Throw *throw, Decl **func_throws)
{
	assert(throw->kind != THROW_TYPE_CALL_ANY);
	Decl **throws;
	unsigned throw_count;
	if (throw->kind == THROW_TYPE_CALL_THROW_MANY)
	{
		throws = throw->throws;
		throw_count = vec_size(throws);
	}
	else
	{
		throws = &throw->throw->decl;
		throw_count = 1;
	}
	unsigned func_throw_count = vec_size(func_throws);
	assert(func_throw_count);
	bool catch_added = false;
	for (unsigned i = 0; i < func_throw_count; i++)
	{
		Decl *func_throw = func_throws[i];
		for (unsigned j = 0; j < throw_count; j++)
		{
			if (throws[j] == func_throw->type->decl)
			{
				// If the throw was already caught, ignore it.
				if (throw_completely_caught(throws[j], throw->throw_info->catches)) continue;

				// One of the throws was caught
				if (func_throw_count > 1)
				{
					CatchInfo info = { .kind = CATCH_RETURN_MANY, .error = func_throw->type->decl };
					vec_add(throw->throw_info->catches, info);
				}
				else
				{
					CatchInfo info = { .kind = CATCH_RETURN_ONE, .error = func_throw->type->decl };
					vec_add(throw->throw_info->catches, info);
				}
				// If we only have one count, then we're done!
				if (throw_count == 1)
				{
					throw->throw_info->is_completely_handled = true;
					return true;
				}
				// Otherwise we simply continue.
			}
		}
	}
	// If we have already caught some, then we might have completely caught all throws.
	if (throw_count > 1 && catch_added)
	{
		throw->throw_info->is_completely_handled = throw_completely_handled_call_throw_many(throw);
	}
	return catch_added;
}

bool sema_analyse_function_body(Context *context, Decl *func)
{
	FunctionSignature *signature = &func->func.function_signature;
	context->active_function_for_analysis = func;
	context->rtype = signature->rtype->type;
	context->current_scope = &context->scopes[0];
	// Clean out the current scope.
	memset(context->current_scope, 0, sizeof(*context->current_scope));

	// Clear try handling
	vec_resize(context->throw, 0);
	vec_resize(context->error_calls, 0);
	// Clear returns
	vec_resize(context->returns, 0);
	context->try_nesting = 0;
	context->labels = NULL;
	context->gotos = NULL;
	context->returns = NULL;
	context->expected_block_type = NULL;
	context->last_local = &context->locals[0];
	context->in_volatile_section = 0;
	func->func.annotations = CALLOCS(*func->func.annotations);
	context_push_scope(context);
	Decl **params = signature->params;
	assert(context->current_scope == &context->scopes[1]);
	VECEACH(params, i)
	{
		if (!sema_add_local(context, params[i])) return false;
	}
	if (!sema_analyse_compound_statement_no_scope(context, func->func.body)) return false;
	assert(context->current_scope == &context->scopes[1]);
	if (context->current_scope->exit != EXIT_RETURN && context->current_scope->exit != EXIT_THROW && context->current_scope->exit != EXIT_GOTO)
	{
		if (signature->rtype->type->canonical != type_void)
		{
			// IMPROVE better pointer to end.
			SEMA_ERROR(func, "Missing return statement at the end of the function.");
			return false;
		}
	}


	VECEACH(context->gotos, i)
	{
		Ast *goto_stmt = context->gotos[i];
		Ast *label_target = goto_stmt->goto_stmt.label;
		if (!label_target)
		{
			SEMA_ERROR(goto_stmt, "Goto to a missing label %s.", goto_stmt->goto_stmt.label_name);
			return false;
		}

		// If there are no defers, then that's fine.
		if (!goto_stmt->goto_stmt.defer.start && !label_target->label_stmt.defer) continue;

		Ast *common_depth_label = label_target->label_stmt.defer;
		Ast *common_depth_goto = goto_stmt->goto_stmt.defer.start;

		// First we need to search for the common depth.
		int label_depth = defer_depth(common_depth_label);
		int goto_depth = defer_depth(common_depth_goto);


		// Now walk up to the common depth.
		defer_list_walk_to_common_depth(&common_depth_label, label_depth, goto_depth);
		defer_list_walk_to_common_depth(&common_depth_goto, goto_depth, label_depth);

		// We might still not match, so walk upwards until we have a match:
		while (common_depth_goto != common_depth_label)
		{
			assert(common_depth_goto && common_depth_label);
			common_depth_goto = common_depth_goto->defer_stmt.prev_defer;
			common_depth_label = common_depth_label->defer_stmt.prev_defer;
		}

		// We now know the top defer (which we won't actually generate)
		goto_stmt->goto_stmt.defer.end = common_depth_goto;

		// Mark all defers that occur on the way "up" to the common depth conditional.
		Ast *current = label_target->label_stmt.defer;
		while (current != common_depth_goto)
		{
			current->defer_stmt.emit_boolean = true;
			current = current->defer_stmt.prev_defer;
		}
	}
	bool error_was_useful = vec_size(context->throw) > 0;
	VECEACH(context->error_calls, i)
	{
		Throw *throw = &context->error_calls[i];
		if (throw->throw_info->is_completely_handled) continue;

		switch (signature->error_return)
		{
			case ERROR_RETURN_NONE:
				// Nothing to do, will result in error.
				break;
			case ERROR_RETURN_ANY:
				// Any return, then any throw is ok, add
				// an implicit catch.
				vec_add(throw->throw_info->catches, (CatchInfo) { .kind = CATCH_RETURN_ANY });
				throw->throw_info->is_completely_handled = true;
				error_was_useful = true;
				continue;
			case ERROR_RETURN_MANY:
			case ERROR_RETURN_ONE:
				// Try to add a catch.
				if (throw_add_error_return_catch(throw, signature->throws))
				{
					error_was_useful = true;
				}
				break;
		}
		// If it's fully catched, then fine.
		if (throw->throw_info->is_completely_handled) continue;
		// Otherwise error.
		SEMA_ERROR(throw, "The errors returned by the call must be completely caught in a catch or else the function current must be declared to throw.");
		return false;
	}

	if (!error_was_useful)
	{
		// Warning here?
	}
	func->func.labels = context->labels;
	context_pop_scope(context);
	context->current_scope = NULL;
	return true;
}

