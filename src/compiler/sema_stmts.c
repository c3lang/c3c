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
	Ast *previous_defer = context->current_scope->in_defer;
	AstId parent_defer = context->current_scope->defer_last;
	context->current_scope++;
	context->current_scope->scope_id = ++context->scope_id;
	context->current_scope->allow_dead_code = false;
	context->current_scope->jump_end = false;
	if (context->scope_id == 0)
	{
		FATAL_ERROR("Too many scopes.");
	}
	context->current_scope->local_decl_start = context->last_local;
	context->current_scope->in_defer = previous_defer;
	context->current_scope->defer_last = parent_defer;

	if (flags & (SCOPE_DEFER | SCOPE_EXPR_BLOCK))
	{
		context->current_scope->flags = flags;
	}
	else
	{
		context->current_scope->flags = previous_flags | flags;
	}
	if (previous_flags & SCOPE_MACRO)
	{
		context->current_scope->flags = previous_flags | SCOPE_MACRO;
	}
}

void context_push_scope_with_label(Context *context, Decl *label)
{
	context_push_scope_with_flags(context, SCOPE_NONE);

	if (label)
	{
		label->label.defer = context->current_scope->defer_last;
		sema_add_local(context, label);
		label->label.scope_id = context->current_scope->scope_id;
	}
}



static inline void context_pop_defers_to(Context *context, DeferList *list)
{
	list->end = context_start_defer(context);
	list->start = context->current_scope->defer_last;
	context->current_scope->defer_last = list->end;
}



void context_pop_scope(Context *context)
{
	assert(context->current_scope != &context->scopes[0]);
	context->last_local = context->current_scope->local_decl_start;

	assert(context_start_defer(context) == context->current_scope->defer_last);

	context->current_scope--;
}

static Expr *context_pop_defers_and_wrap_expr(Context *context, Expr *expr)
{
	DeferList defers = { 0, 0 };
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
	DeferList defers = { 0, 0 };
	context_pop_defers_to(context, &defers);
	if (defers.end == defers.start) return;
	if (ast->ast_kind == AST_DEFER_STMT)
	{
		assert(defers.start == astid(ast));
		*ast = *ast->defer_stmt.body;
		return;
	}
	assert(ast->ast_kind != AST_COMPOUND_STMT);
	Ast *replacement = COPY(ast);
	ast->ast_kind = AST_SCOPED_STMT;
	ast->scoped_stmt.stmt = replacement;
	ast->scoped_stmt.defers = defers;
}

AstId context_start_defer(Context *context)
{
	if (context->current_scope == context->scopes) return 0;
	if (context->current_scope->in_defer != context->current_scope[-1].in_defer) return 0;
	return context->current_scope[-1].defer_last;
}

#pragma mark --- Helper functions



#pragma mark --- Sema analyse stmts


static inline bool sema_analyse_block_return_stmt(Context *context, Ast *statement)
{
	assert(context->current_scope->flags & (SCOPE_EXPR_BLOCK | SCOPE_MACRO));
	context->current_scope->jump_end = true;
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

static inline bool sema_analyse_return_stmt(Context *context, Ast *statement)
{
	// This might be a return in a function block or a macro which must be treated differently.
	if (context->current_scope->flags & (SCOPE_EXPR_BLOCK | SCOPE_MACRO))
	{
		return sema_analyse_block_return_stmt(context, statement);
	}
	context->current_scope->jump_end = true;
	Type *expected_rtype = context->rtype;
	Expr *return_expr = statement->return_stmt.expr;
	statement->return_stmt.defer = context->current_scope->defer_last;
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
	if (expected_rtype == type_void && !context->failable_return)
	{
		SEMA_ERROR(statement, "You can't return a value from a void function, you need to add a return type.");
		return false;
	}
	if (!sema_analyse_expr_of_required_type(context, expected_rtype, return_expr, context->failable_return)) return false;
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

static inline bool sema_analyse_unreachable_stmt(Context *context, Ast *statement)
{
	context->current_scope->jump_end = true;
	return true;
}

static inline bool sema_analyse_decl_expr_list(Context *context, Expr *expr)
{
	assert(expr->expr_kind == EXPR_DECL_LIST);

	Ast **dexprs = expr->dexpr_list_expr;
	unsigned entries = vec_size(dexprs);
	for (unsigned i = 0; i < entries; i++)
	{
		Ast *ast = dexprs[i];
		if (!sema_analyse_statement(context, ast))
		{
			return false;
		}
	}
	if (entries == 0)
	{
		expr->type = type_void;
		return true;
	}
	Ast *last = dexprs[entries - 1];
	switch (last->ast_kind)
	{
		case AST_DECLARE_STMT:
			expr->type = last->declare_stmt->type;
			break;
		case AST_EXPR_STMT:
			expr->type = last->expr_stmt->type;
			break;
		default:
			UNREACHABLE
	}
	return true;
}


static inline bool sema_analyse_cond(Context *context, Expr *expr, bool cast_to_bool)
{
	assert(expr->expr_kind == EXPR_DECL_LIST);
	size_t size = vec_size(expr->dexpr_list_expr);
	if (!size)
	{
		SEMA_ERROR(expr, "Expected a boolean expression");
		return false;
	}

	if (!sema_analyse_decl_expr_list(context, expr)) return false;

	Ast *last = expr->dexpr_list_expr[size - 1];
	switch (last->ast_kind)
	{
		case AST_EXPR_STMT:
			if (last->expr_stmt->failable)
			{
				SEMA_ERROR(last, "'%s!' cannot be converted into '%s'.",
				               type_to_error_string(last->expr_stmt->type),
						cast_to_bool ? "bool" : type_to_error_string(last->expr_stmt->type));
			}
			if (cast_to_bool)
			{
				if (!cast_implicit(context, last->expr_stmt, type_bool)) return false;
			}
			return true;
		case AST_DECLARE_STMT:
		{
			Decl *decl = last->declare_stmt;
			Expr *init = decl->var.init_expr;
			if (!init)
			{
				SEMA_ERROR(last, "Expected a declaration with initializer.");
				return false;
			}
			if (init->failable && !decl->var.unwrap)
			{
				SEMA_ERROR(last, "'%s!' cannot be converted into '%s'.",
				               type_to_error_string(last->expr_stmt->type),
				           cast_to_bool ? "bool" : type_to_error_string(init->type));
			}
			if (!decl->var.unwrap && cast_to_bool && init->type->type_kind != TYPE_BOOL &&
			    cast_to_bool_kind(decl->var.type_info->type) == CAST_ERROR)
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
	Expr *cond = statement->while_stmt.cond;
	Ast *body = statement->while_stmt.body;
	context_push_scope(context);
	bool success = sema_analyse_cond(context, cond, true);
	context_push_scope_with_label(context, statement->while_stmt.flow.label);

	PUSH_BREAKCONT(statement);
	success = success && sema_analyse_statement(context, body);
	context_pop_defers_and_replace_ast(context, body);
	POP_BREAKCONT();

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
	context_push_scope_with_label(context, statement->do_stmt.flow.label);
	PUSH_BREAKCONT(statement);
	success = sema_analyse_statement(context, body);
	context_pop_defers_and_replace_ast(context, body);
	POP_BREAKCONT();
	statement->do_stmt.flow.no_exit = context->current_scope->jump_end;
	context_pop_scope(context);
	if (!success) return false;
	if (!statement->do_stmt.expr) return success;

	context_push_scope(context);
	success = sema_analyse_expr_of_required_type(context, type_bool, expr, false);
	statement->do_stmt.expr = context_pop_defers_and_wrap_expr(context, expr);
	context_pop_scope(context);

	// while (1) with no break means that we've reached a statement which ends with a jump.
	if (statement->do_stmt.expr->expr_kind == EXPR_CONST && statement->do_stmt.expr->const_expr.b)
	{
		if (statement->do_stmt.flow.no_exit && !statement->do_stmt.flow.has_break)
		{
			context->current_scope->jump_end = true;
		}
	}
	return success;
}


static inline bool sema_analyse_declare_stmt(Context *context, Ast *statement)
{
	Decl *decl = statement->declare_stmt;
	assert(decl->decl_kind == DECL_VAR);
	if (!sema_add_local(context, decl)) return decl_poison(decl);
	if (decl->var.kind == VARDECL_CONST)
	{
		Expr *init_expr = decl->var.init_expr;
		if (!init_expr)
		{
			SEMA_ERROR(decl, "Constants need to have an initial value.");
			return false;
		}
		if (init_expr->expr_kind == EXPR_TYPEINFO && init_expr->type_expr->resolve_status == RESOLVE_DONE
			   && init_expr->type_expr->type->type_kind == TYPE_VOID)
		{
			SEMA_ERROR(decl, "Constants cannot be undefined.");
			return false;
		}
		if (!decl->var.type_info)
		{
			// Skip further evaluation.
			return true;
		}
	}
	if (!sema_resolve_type_info(context, decl->var.type_info)) return decl_poison(decl);
	decl->type = decl->var.type_info->type;
	if (decl->var.init_expr)
	{
		Expr *init = decl->var.init_expr;
		// Handle explicit undef
		if (init->expr_kind == EXPR_TYPEINFO && init->type_expr->resolve_status == RESOLVE_DONE
			&& init->type_expr->type->type_kind == TYPE_VOID)
		{
			init->expr_kind = EXPR_UNDEF;
			init->resolve_status = RESOLVE_DONE;
			return true;
		}
		if (!sema_expr_analyse_assign_right_side(context, NULL, decl->type, init, decl->var.failable || decl->var.unwrap ? FAILABLE_YES : FAILABLE_NO)) return decl_poison(decl);
		if (decl->var.unwrap && !init->failable)
		{
			SEMA_ERROR(decl->var.init_expr, "A failable expression was expected here.");
			return decl_poison(decl);
		}
	}
	return true;
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
	decl->var.scope = context->current_scope;
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
	context_push_scope_with_flags(context, SCOPE_DEFER); // NOLINT(hicpp-signed-bitwise)

	context->current_scope->defer_last = 0;
	context->current_scope->in_defer = statement;

	PUSH_CONTINUE(NULL);
	PUSH_BREAK(statement);
	PUSH_NEXT(NULL, NULL);

	// Only ones allowed.
	context->current_scope->flags &= SCOPE_DEFER;

	bool success = sema_analyse_statement(context, statement->defer_stmt.body);

	POP_BREAKCONT();
	POP_NEXT();

	context_pop_defers_and_replace_ast(context, statement->defer_stmt.body);

	context_pop_scope(context);

	if (!success) return false;

	statement->defer_stmt.prev_defer = context->current_scope->defer_last;
	context->current_scope->defer_last = astid(statement);
	return true;
}


static inline bool sema_analyse_for_stmt(Context *context, Ast *statement)
{
	bool success = true;

	// Enter for scope
	context_push_scope(context);
	bool is_infinite = statement->for_stmt.cond == NULL;
	if (statement->for_stmt.init)
	{
		success = sema_analyse_decl_expr_list(context, statement->for_stmt.init);
	}
	if (success && statement->for_stmt.cond)
	{
		// Conditional scope start
		context_push_scope(context);
		Expr *cond = statement->for_stmt.cond;
		success = sema_analyse_expr_of_required_type(context, type_bool, cond, false);
		statement->for_stmt.cond = context_pop_defers_and_wrap_expr(context, cond);
		// If this is const true, then set this to infinte and remove the expression.
		if (statement->for_stmt.cond->expr_kind == EXPR_CONST && statement->for_stmt.cond->const_expr.b)
		{
			statement->for_stmt.cond = NULL;
			is_infinite = true;
		}
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
	if (!success)
	{
		context_pop_scope(context);
		return false;
	}

	// Create the for body scope.
	context_push_scope_with_label(context, statement->for_stmt.flow.label);
	PUSH_BREAKCONT(statement);
	success = sema_analyse_statement(context, statement->for_stmt.body);
	statement->for_stmt.flow.no_exit = context->current_scope->jump_end;
	POP_BREAKCONT();
	// End for body scope
	context_pop_defers_and_replace_ast(context, statement->for_stmt.body);
	context_pop_scope(context);
	context_pop_defers_and_replace_ast(context, statement);
	// End for scope
	context_pop_scope(context);
	if (statement->for_stmt.flow.no_exit && is_infinite && !statement->for_stmt.flow.has_break)
	{
		context->current_scope->jump_end = true;
	}
	return success;
}




static inline bool sema_analyse_if_stmt(Context *context, Ast *statement)
{
	// IMPROVE
	// convert
	// if (!x) A(); else B();
	// into
	// if (x) B(); else A();
	Expr *cond = statement->if_stmt.cond;
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
		if (success && statement->if_stmt.else_body->ast_kind != AST_COMPOUND_STMT  && statement->if_stmt.else_body->ast_kind != AST_IF_STMT)
		{
			SEMA_ERROR(statement->if_stmt.else_body,
			               "An 'else' must use '{ }' even around a single statement.");
			success = false;
		}
	}
	if (success && statement->if_stmt.then_body->ast_kind != AST_COMPOUND_STMT)
	{
		SourceLocation *end_of_cond = TOKILOC(cond->span.end_loc);
		SourceLocation *start_of_then = TOKILOC(statement->if_stmt.then_body->span.loc);
		if (end_of_cond->line != start_of_then->line)
		{
			SEMA_ERROR(statement->if_stmt.then_body,
			           "The 'then' part of a single line if-statement must start on the same line as the 'if' or use '{ }'");
			success = false;
		}
	}
	if (context->current_scope->jump_end && !context->current_scope->allow_dead_code)
	{
		SEMA_ERROR(statement->if_stmt.then_body, "This code can never be executed.");
		success = false;
	}

	context_push_scope_with_label(context, statement->if_stmt.flow.label);

	success = success && sema_analyse_statement(context, statement->if_stmt.then_body);
	bool then_jump = context->current_scope->jump_end;
	context_pop_scope(context);
	bool else_jump = false;
	if (statement->if_stmt.else_body)
	{
		context_push_scope_with_label(context, statement->if_stmt.flow.label);
		success = success && sema_analyse_statement(context, statement->if_stmt.else_body);
		else_jump = context->current_scope->jump_end;
		context_pop_scope(context);
	}
	context_pop_defers_and_replace_ast(context, statement);
	context_pop_scope(context);

	if (then_jump && else_jump && !statement->flow.has_break)
	{
		context->current_scope->jump_end = true;
	}
	return success;
}





static bool sema_analyse_asm_stmt(Context *context __unused, Ast *statement __unused)
{
	TODO
}

static DynamicScope *context_find_scope_by_id(Context *context, ScopeId scope_id)
{
	DynamicScope *scope = context->current_scope;
	while (1)
	{
		if (scope->scope_id == scope_id) return scope;
		assert(scope != &context->scopes[0]);
		scope--;
	}
	UNREACHABLE
}

static inline Decl *sema_analyse_label(Context *context, Ast *stmt)
{
	Decl *ambiguous;
	Decl *dummy;
	Decl *target = sema_resolve_symbol(context, stmt->contbreak_stmt.label.name, NULL, &ambiguous, &dummy);
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
	if (context->current_scope->in_defer)
	{
		DynamicScope *scope = context_find_scope_by_id(context, target->label.scope_id);
		if (scope->in_defer != context->current_scope->in_defer)
		{
			SEMA_ERROR(stmt, stmt->ast_kind == AST_BREAK_STMT ? "You cannot break out of a defer." : "You cannot use continue out of a defer.");
			return poisoned_decl;
		}
	}
	return target;
}

static bool context_labels_exist_in_scope(Context *context)
{
	for (Decl **from = &context->locals[0]; from < context->last_local; from++)
	{
		if ((*from)->decl_kind == DECL_LABEL) return true;
	}
	return false;
}

static bool sema_analyse_break_stmt(Context *context, Ast *statement)
{
	context->current_scope->jump_end = true;
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

	statement->contbreak_stmt.defers.start = context->current_scope->defer_last;
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
	context->current_scope->jump_end = true;
	if (!context->next_target && !statement->next_stmt.label.name)
	{
		SEMA_ERROR(statement, "'next' is not allowed here.");
		return false;
	}
	Ast *parent = context->next_switch;

	if (statement->next_stmt.label.name)
	{
		Decl *ambiguous;
		Decl *dummy;
		Decl *target = sema_resolve_symbol(context, statement->next_stmt.label.name, NULL, &ambiguous, &dummy);
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
			defer_mismatch = context->current_scope->in_defer != context_find_scope_by_id(context, parent->switch_stmt.scope_id)->in_defer;
		}
		else
		{
			defer_mismatch = context->current_scope->in_defer != context_find_scope_by_id(context, parent->catch_stmt.scope_id)->in_defer;
		}
		if (defer_mismatch)
		{
			SEMA_ERROR(statement, "This 'next' would jump out of a defer which is not allowed.");
			return false;
		}
		assert(statement->next_stmt.target);
	}

	statement->next_stmt.defers.start = context->current_scope->defer_last;
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

	if (!cast_implicit(context, target, parent->switch_stmt.cond->type)) return false;

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
	context->current_scope->jump_end = true;
	statement->contbreak_stmt.defers.start = context->current_scope->defer_last;

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
		return cast(context, case_expr, to_type, CAST_TYPE_EXPLICIT);
	}

	return cast_implicit(context, case_expr, to_type);
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
	switch (switch_type->type_kind)
	{
		case TYPE_TYPEID:
		case TYPE_ERR_UNION:
			use_type_id = true;
			break;
		case ALL_INTS:
			assert(switch_type->type_kind != TYPE_IXX);
		case TYPE_BOOL:
		case TYPE_ENUM:
		case TYPE_STRING:
			break;
		default:
			sema_error_range3(expr_span, "It is not possible to switch over '%s'.", type_to_error_string(switch_type));
			return false;
	}
	Ast *default_case = NULL;
	assert(context_start_defer(context) == context->current_scope->defer_last);

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
		context_push_scope(context);
		PUSH_BREAK(statement);
		Ast *next = (i < case_count - 1) ? cases[i + 1] : NULL;
		PUSH_NEXT(next, statement);
		Ast *body = stmt->case_stmt.body;
		success = success && (!body || sema_analyse_compound_statement_no_scope(context, body));
		POP_BREAK();
		POP_NEXT();
		all_jump_end &= (!body | context->current_scope->jump_end);
		context_pop_scope(context);
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
	statement->switch_stmt.scope_id = context->current_scope->scope_id;
	context_push_scope_with_label(context, statement->switch_stmt.flow.label);
	Expr *cond = statement->switch_stmt.cond;
	if (!sema_analyse_cond(context, cond, false)) return false;


	Type *switch_type = ast_cond_type(cond)->canonical;
	statement->switch_stmt.defer = context->current_scope->defer_last;
	bool success = sema_analyse_switch_body(context, statement, cond->span,
	                                switch_type->canonical,
	                                statement->switch_stmt.cases);
	if (success) context_pop_defers_and_replace_ast(context, statement);
	context_pop_scope(context);
	if (statement->flow.no_exit && !statement->flow.has_break)
	{
		context->current_scope->jump_end = true;
	}
	return success;
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

	statement->catch_stmt.scope_id = context->current_scope->scope_id;
	context_push_scope_with_label(context, statement->catch_stmt.flow.label);

	statement->catch_stmt.defer = context->current_scope->defer_last;
	if (catch_expr->expr_kind == EXPR_BINARY && catch_expr->binary_expr.operator == BINARYOP_ASSIGN)
	{
		Expr *left = catch_expr->binary_expr.left;
		if (left->expr_kind == EXPR_IDENTIFIER)
		{
			Decl *ambiguous_decl;
			Decl *dummy;
			Decl *error_var_decl = sema_resolve_symbol(context,
			                                           left->identifier_expr.identifier,
			                                           left->identifier_expr.path,
			                                           &ambiguous_decl, &dummy);
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
	bool success = sema_analyse_expr(context, NULL, error_expr);

	if (!success) goto EXIT;

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
			success = false;
			goto EXIT;
		}
		SEMA_ERROR(error_expr, "Expected a failable '%s!' not '%s'.", error_type, error_type);
		success = false;
		goto EXIT;
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
		success = sema_analyse_switch_body(context,
		                                   statement,
		                                   error_expr->span,
		                                   type_error,
		                                   statement->catch_stmt.cases);
	}
	else
	{
		success = sema_analyse_statement(context, statement->catch_stmt.body);
		if (context->current_scope->jump_end) statement->flow.no_exit = true;
	}
	if (success) context_pop_defers_and_replace_ast(context, statement);
	context_pop_scope(context);
EXIT:
	if (success)
	{
		if (unwrapped && !statement->flow.has_break && statement->flow.no_exit)
		{
			Decl *decl = COPY(unwrapped);
			decl->var.kind = VARDECL_ALIAS;
			decl->var.alias = unwrapped;
			decl->var.failable = false;
			sema_unwrap_var(context, decl);
		}
	}
	return success;

}


static bool sema_analyse_try_stmt(Context *context, Ast *stmt)
{
	assert(stmt->try_stmt.decl_expr->expr_kind == EXPR_DECL_LIST);

	Ast **dexprs = stmt->try_stmt.decl_expr->dexpr_list_expr;
	context_push_scope(context);
	unsigned entries = vec_size(dexprs);
	for (unsigned i = 0; i < entries; i++)
	{
		Ast *ast = dexprs[i];
		if (ast->ast_kind == AST_DECLARE_STMT)
		{
			ast->declare_stmt->var.unwrap = true;
			if (!sema_analyse_statement(context, ast)) goto ERR;
			continue;
		}
		if (!sema_analyse_statement(context, ast)) goto ERR;
		Expr *expr = ast->expr_stmt;
		if (!expr->failable)
		{
			SEMA_ERROR(expr, "The expression to 'try' must be failable.");
			goto ERR;
		}
		if (expr->expr_kind == EXPR_IDENTIFIER)
		{
			Decl *var = expr->identifier_expr.decl;
			Decl *decl = COPY(var);
			decl->var.kind = VARDECL_ALIAS;
			decl->var.alias = var;
			decl->var.failable = false;
			sema_unwrap_var(context, decl);
		}
	}
	if (!sema_analyse_statement(context, stmt->try_stmt.body)) goto ERR;

	context_pop_scope(context);
	return true;

	ERR:
	context_pop_scope(context);
	return false;
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
		if (!sema_analyse_expr(context, type_string, message)) return false;
		if (message->type->type_kind != TYPE_STRING)
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
		if (!sema_analyse_expr(context, type_string, message)) return false;
		if (message->type->type_kind != TYPE_STRING)
		{
			SEMA_ERROR(message, "Expected a string as the error message.");
		}
	}
	if (!sema_analyse_expr_of_required_type(context, type_bool, expr, false)) return false;
	return true;
}

static bool sema_analyse_compound_stmt(Context *context, Ast *statement)
{
	context_push_scope(context);
	bool success = sema_analyse_compound_statement_no_scope(context, statement);
	bool ends_with_jump = context->current_scope->jump_end;
	context_pop_scope(context);
	context->current_scope->jump_end = ends_with_jump;
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
	if (context->current_scope->jump_end && !context->current_scope->allow_dead_code)
	{
		if (statement->ast_kind == AST_UNREACHABLE_STMT)
		{
			context->current_scope->allow_dead_code = true;
			return true;
		}
		//SEMA_ERROR(statement, "This code will never execute.");
		context->current_scope->allow_dead_code = true;
		//return false;
	}
	switch (statement->ast_kind)
	{
		case AST_POISONED:
		case AST_SCOPED_STMT:
			UNREACHABLE
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
			return sema_analyse_unreachable_stmt(context, statement);
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
	FunctionSignature *signature = &func->func.function_signature;
	context->active_function_for_analysis = func;
	context->rtype = signature->rtype->type;
	context->current_scope = &context->scopes[0];
	context->current_scope->scope_id = 0;
	context->failable_return = signature->failable;

	// Clean out the current scope.
	memset(context->current_scope, 0, sizeof(*context->current_scope));

	// Clear returns
	vec_resize(context->returns, 0);
	context->scope_id = 0;
	context->returns = NULL;
	context->expected_block_type = NULL;
	context->last_local = &context->locals[0];
	context->in_volatile_section = 0;
	context->macro_counter = 0;
	context->macro_nesting = 0;
	context->continue_target = 0;
	context->next_target = 0;
	context->next_switch = 0;
	context->break_target = 0;
	func->func.annotations = CALLOCS(FuncAnnotations);
	context_push_scope(context);
	Decl **params = signature->params;
	assert(context->current_scope == &context->scopes[1]);
	VECEACH(params, i)
	{
		if (!sema_add_local(context, params[i])) return false;
	}
	if (!sema_analyse_compound_statement_no_scope(context, func->func.body)) return false;
	assert(context->current_scope == &context->scopes[1]);
	if (!context->current_scope->jump_end)
	{
		Type *canonical_rtype = signature->rtype->type->canonical;
		if (canonical_rtype != type_void)
		{
			// IMPROVE better pointer to end.
			SEMA_ERROR(func, "Missing return statement at the end of the function.");
			return false;
		}
	}


	context_pop_scope(context);
	context->current_scope = NULL;
	return true;
}

