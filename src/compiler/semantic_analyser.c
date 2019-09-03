// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "compiler_internal.h"

typedef bool(*AstAnalysis)(Context *, Ast*);

bool sema_analyse_stmt_list(Context *context, Ast *statement);

void sema_init(File *file)
{
	LOG_FUNC
}

/**
 * Check if a type is contained in another type.
 *
 * @param type
 * @param possible_subtype
 * @return true if it is a subtype
 */
static bool canonical_type_is_subtype(Type *type, Type *possible_subtype)
{
	assert(type == type->canonical && possible_subtype == possible_subtype->canonical);
	if (type == possible_subtype) return true;
	if (type->type_kind != possible_subtype->type_kind) return false;
	if (type->type_kind != TYPE_USER_DEFINED || type->decl->decl_kind != DECL_STRUCT) return false;

	if (!possible_subtype->decl->strukt.members) return false;

	Decl *first_element = possible_subtype->decl->strukt.members[0];

	if (first_element->decl_kind != DECL_VAR) return false;

	return canonical_type_is_subtype(type, first_element->var.type->canonical);
}


static void show_shadow_error(Decl *decl, Decl *old)
{
	sema_error_range(decl->name.span, "The '%s' would shadow a previous declaration.", decl->name.string);
	sema_prev_at_range(old->name.span, "The previous use of '%s' was here.", decl->name.string);
}

Decl *module_find_symbol(Module *module, const char *symbol)
{
	return stable_get(&module->symbols, symbol);
}


Decl *context_find_ident(Context *context, const char *symbol)
{
	Decl **first = &context->locals[0];
	Decl **current = context->last_local - 1;
	while (current >= first)
	{
		if (current[0]->name.string == symbol) return current[0];
		current--;
	}
	Decl *found = module_find_symbol(context->module, symbol);
	if (found) return found;
	// TODO check imports
	return NULL;
}


static inline void context_push_scope(Context *context)
{
	if (context->current_scope == &context->scopes[MAX_SCOPE_DEPTH - 1])
	{
		FATAL_ERROR("Too deeply nested scopes.");
	}
	context->current_scope++;
	context->current_scope->exit = EXIT_NONE;
	context->current_scope->local_decl_start = context->last_local;
}

static inline void context_push_scope_with_flags(Context *context, ScopeFlags flags)
{
	ScopeFlags previous_flags = context->current_scope->flags;
	context_push_scope(context);
	context->current_scope->flags = previous_flags | flags;
	context->current_scope->flags_created = flags;
}

static inline void context_pop_scope(Context *context)
{
	assert(context->current_scope != &context->scopes[0]);
	context->last_local = context->current_scope->local_decl_start;
	ExitType exit_type = context->current_scope->exit;
	context->current_scope--;
	if (context->current_scope->exit < exit_type)
	{
		context->current_scope->exit = exit_type;
	}
}



static bool sema_resolve_ptr_type(Context *context, Type *type)
{
	if (!sema_resolve_type(context, type->base))
	{
		type_poison(type);
		return false;
	}
	type->canonical = type_get_canonical_ptr(type);
	type->resolve_status = RESOLVE_DONE;
	return true;
}

static bool sema_resolve_array_type(Context *context, Type *type)
{
	if (!sema_resolve_type(context, type->base))
	{
		type_poison(type);
		return false;
	}
	type->canonical = type_get_canonical_array(type);
	type->resolve_status = RESOLVE_DONE;
	return true;
}

bool sema_resolve_type(Context *context, Type *type)
{
    LOG_FUNC

	if (type->resolve_status == RESOLVE_DONE) return type_ok(type);

	if (type->resolve_status == RESOLVE_RUNNING)
	{
		SEMA_ERROR(type->name_loc, "Circular dependency resolving type '%s'.", type->name_loc);
		type_poison(type);
		return false;
	}

	type->resolve_status = RESOLVE_RUNNING;

	switch (type->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_INC_ARRAY:
			UNREACHABLE
		case TYPE_USER_DEFINED:
			break;
		case TYPE_POINTER:
			return sema_resolve_ptr_type(context, type);
		case TYPE_ARRAY:
			return sema_resolve_array_type(context, type);
		default:
			TODO
	}

	Decl *decl = stable_get(&context->local_symbols, type->name_loc.string);
	if (!decl)
	{
		decl = module_find_symbol(context->module, type->name_loc.string);
	}

	if (!decl)
	{
		SEMA_ERROR(type->name_loc, "Unknown type '%s'.", type->name_loc.string);
		type_poison(type);
		return false;
	}

	switch (decl->decl_kind)
	{
		case DECL_STRUCT:
		case DECL_UNION:
		case DECL_ERROR:
		case DECL_ENUM:
			type->decl = decl;
			type->canonical = decl->self_type;
			type->resolve_status = RESOLVE_DONE;
			DEBUG_LOG("Resolved %s.", type->name_loc.string);
			return true;
		case DECL_TYPEDEF:
			// TODO func
			if (!sema_resolve_type(context, decl->typedef_decl.type))
			{
				decl_poison(decl);
				type_poison(type);
				return false;
			}
			type->decl = decl;
			type->canonical = decl->typedef_decl.type->canonical;
			type->resolve_status = RESOLVE_DONE;
			DEBUG_LOG("Resolved %s.", type->name_loc.string);
			return true;
		case DECL_POISONED:
			type_poison(type);
			return false;
		case DECL_FUNC:
		case DECL_VAR:
		case DECL_ENUM_CONSTANT:
		case DECL_ERROR_CONSTANT:
		case DECL_ARRAY_VALUE:
		case DECL_IMPORT:
		case DECL_MACRO:
		case DECL_GENERIC:
			SEMA_ERROR(type->name_loc, "This is not a type.");
			type_poison(type);
			return false;
		case DECL_MULTI_DECL:
		case DECL_CT_ELSE:
		case DECL_CT_IF:
		case DECL_CT_ELIF:
			UNREACHABLE
	}
	UNREACHABLE
}

static inline bool sema_analyse_struct_member(Context *context, Decl *decl)
{
	LOG_FUNC
	assert(decl->decl_kind == DECL_VAR);
	assert(decl->var.kind == VARDECL_MEMBER);
	assert(!decl->var.init_expr);
	if (!sema_resolve_type(context, decl->var.type))
	{
		decl_poison(decl);
		return false;
	}
	assert(decl->var.type->canonical);
	return true;
}
static inline void sema_analyse_struct(Context *context, Decl *decl)
{
	LOG_FUNC
	DEBUG_LOG("Beginning analysis of %s.", decl->name.string);
	assert(decl->decl_kind == DECL_STRUCT);
	VECEACH(decl->strukt.members, i)
	{
		Decl *member = decl->strukt.members[i];
		if (!decl_ok(member))
		{
			decl_poison(decl);
			continue;
		}
		if (!sema_analyse_struct_member(context, decl->strukt.members[i]))
		{
			if (decl_ok(decl))
			{
				decl_poison(decl);
				continue;
			}
			decl_poison(decl);
		}
	}
	DEBUG_LOG("Analysing complete.");
}


/*
static inline void add_integers(ExprConst *result, uint64_t l, bool l_is_negative, uint64_t r, bool r_is_negative)
{
	if (l_is_negative == r_is_negative)
	{
		result->type = l_is_negative ? CONST_NEGATIVE_INT : CONST_POSITIVE_INT;
		result->integer.i = l + r;
		return;
	}
	if (l > r)
	{
		result->integer.i = l - r;
		result->type = l_is_negative ? CONST_NEGATIVE_INT : CONST_POSITIVE_INT;
	}
	else
	{
		result->integer.i = r - l;
		result->type = l_is_negative ? CONST_POSITIVE_INT : CONST_NEGATIVE_INT;
	}
}
*/




static inline bool sema_analyse_function_param(Context *context, Decl *param, bool is_function)
{
	if (!decl_ok(param)) return false;
	assert(param->decl_kind == DECL_VAR);
	assert(param->var.kind == VARDECL_PARAM);
	if (!sema_resolve_type(context, param->var.type))
	{
		return false;
	}
	if (param->var.init_expr && !is_function)
	{
		SEMA_ERROR(param->var.init_expr->loc, "Function types may not have default arguments.");
		return false;
	}
	if (param->var.init_expr)
	{
		Expr *expr = param->var.init_expr;
		if (!sema_expr_analysis(context, expr)) return false;
		if (expr->expr_kind != EXPR_CONST)
		{
			SEMA_ERROR(expr->loc, "Only constant expressions may be used as default values.");
			return false;
		}
		if (!cast(expr, param->var.type, CAST_TYPE_IMPLICIT_ASSIGN)) return false;
	}
	return true;
}

static inline bool sema_analyse_function_signature(Context *context, FunctionSignature *signature, bool is_function)
{
	bool all_ok = true;
	all_ok = sema_resolve_type(context, signature->rtype) && all_ok;
	// TODO check parameter name appearing more than once.
	VECEACH(signature->params, i)
	{
		if (!sema_analyse_function_param(context, signature->params[i], is_function))
		{
			decl_poison(signature->params[i]);
			all_ok = false;
		}
	}
	VECEACH(signature->throws, i)
	{
		TODO
	}
	return all_ok;
}


static inline bool sema_analyse_compound_statement_no_scope(Context *context, Ast *compound_statement)
{
	LOG_FUNC
	bool all_ok = ast_ok(compound_statement);
	VECEACH(compound_statement->compound_stmt.stmts, i)
	{
		if (!sema_analyse_statement(context, compound_statement->compound_stmt.stmts[i]))
		{
			ast_poison(compound_statement->compound_stmt.stmts[i]);
			all_ok = false;
		}
	}
	/*
	if (parent->exit < compound_statement->exit)
	{
		parent->exit = compound_statement->exit;
	}*/
	return all_ok;
}

static inline bool sema_analyse_return_stmt(Context *context, Ast *statement)
{
	LOG_FUNC
	context->current_scope->exit = EXIT_RETURN;
	Type *expected_rtype = context->rtype;
	Expr *return_expr = statement->return_stmt.expr;
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
			SEMA_ERROR(statement->token, "Expected to return a result of type %s.", expected_rtype->name_loc.string);
			return false;
		}
		return true;
	}
	if (!sema_expr_analysis(context, return_expr)) return false;
	if (!expected_rtype)
	{
		assert(context->evaluating_macro);
		context->rtype = type_void;
		context->active_function_for_analysis->func.function_signature.rtype = statement->return_stmt.expr->type;
		return true;
	}
	if (context->evaluating_macro && expected_rtype->canonical != return_expr->type->canonical)
	{
		TODO // Fix upcast
	}
	if (!cast(statement->return_stmt.expr, expected_rtype, CAST_TYPE_IMPLICIT)) return false;
	return true;
}


static inline bool sema_analyse_var_decl(Context *context, Decl *decl)
{
	assert(decl->decl_kind == DECL_VAR);
	if (!sema_resolve_type(context, decl->var.type)) return false;
	Decl *other = context_find_ident(context, decl->name.string);
	if (other)
	{
		show_shadow_error(decl, other);
		decl_poison(decl);
		decl_poison(other);
		return false;
	}
	Decl *** vars = &context->active_function_for_analysis->func.annotations->vars;
	unsigned num_vars = vec_size(*vars);
	if (num_vars == MAX_LOCALS - 1 || context->last_local == &context->locals[MAX_LOCALS - 1])
	{
		SEMA_ERROR(decl->name, "Reached the maximum number of locals.");
		return false;
	}
	decl->var.id = num_vars;
	*vars = VECADD(*vars, decl);
	if (decl->var.init_expr)
	{
		if (!sema_expr_analysis(context, decl->var.init_expr) ||
			!cast(decl->var.init_expr, decl->var.type, CAST_TYPE_IMPLICIT_ASSIGN))
		{
			decl_poison(decl);
			return false;
		}
	}
	context->last_local[0] = decl;
	context->last_local++;
	return true;
}

static inline bool sema_analyse_decl_expr_list(Context *context, Ast *statement)
{
	switch (statement->decl_expr_list.list_type)
	{
		case DECLEXPR_DECL:
			return sema_analyse_var_decl(context, statement->decl_expr_list.decl);
		case DECLEXPR_EXPR:
			return sema_expr_analysis(context, statement->decl_expr_list.expr);
	}
	UNREACHABLE
}


static inline bool sema_insert_decl_expr_list_in_compound_stmt(Context *context, Ast *compound_statement, Ast *decl_expr_list)
{
	compound_statement->compound_stmt.stmts = VECADD(compound_statement->compound_stmt.stmts, decl_expr_list);
	return true;
}

static inline Ast *convert_expr_to_ast(Expr *expr)
{
	Ast *ast = new_ast(AST_EXPR_STMT, expr->loc);
	ast->expr_stmt = expr;
	return ast;
}
static inline Expr *convert_decl_to_expr(Context *context, Decl *decl)
{
	assert(decl->decl_kind == DECL_VAR);
	assert(decl->decl_kind == VARDECL_LOCAL);
	if (!decl->var.init_expr) return NULL;
	Expr *assign_expr = expr_new(EXPR_BINARY, decl->name);
	assign_expr->resolve_status = RESOLVE_DONE;
	Expr *identifier = expr_new(EXPR_IDENTIFIER, decl->name);
	identifier->resolve_status = RESOLVE_DONE;
	identifier->identifier_expr.identifier = decl->name;
	identifier->identifier_expr.decl = decl;
	assign_expr->binary_expr.left = identifier;
	assign_expr->binary_expr.right = decl->var.init_expr;
	assign_expr->binary_expr.operator = TOKEN_EQ;
	identifier->type = decl->var.type;
	assign_expr->type = decl->var.init_expr->type;
	return assign_expr;
}

static inline bool convert_decl_for_cond(Context *context, Decl *decl, Ast*** stmt_list, Expr** last, bool is_last)
{
	Expr *expr = convert_decl_to_expr(context, decl);
	if (!expr)
	{
		if (is_last)
		{
			SEMA_ERROR(decl->name, "Expected an initializer for '%s'.", decl->name.string);
			return false;
		}
		// Simply skip declarations if they don't have an initializer, since they're already registered anyway.
		return true;
	}
	if (is_last)
	{
		*last = expr;
	}
	else
	{
		Ast *stmt = convert_expr_to_ast(expr);
		*stmt_list = VECADD(*stmt_list, stmt);
	}
	return true;
}

static inline bool convert_stmt_for_cond(Context *context, Ast *stmt, Ast*** stmt_list, Expr** last, bool is_last)
{
	if (stmt->ast_kind == AST_EXPR_STMT)
	{
		if (is_last)
		{
			*last = stmt->expr_stmt;
			return true;
		}
		*stmt_list = VECADD(*stmt_list, stmt);
		return true;
	}
	assert(stmt->ast_kind == AST_DECLARE_STMT);
	Decl *decl = stmt->declare_stmt;

	if (decl->decl_kind != DECL_MULTI_DECL)
	{
		return convert_decl_for_cond(context, decl, stmt_list, last, is_last);
	}

	Decl **decls = decl->multi_decl;
	assert(vec_size(decls) > 0);
	unsigned last_element = vec_size(decls) - 1;
	for (unsigned i = 0; i <= last_element; i++)
	{
		Decl *sub_decl = decls[i];
		if (!convert_decl_for_cond(context, sub_decl, stmt_list, last, is_last && last_element == i)) return false;
	}
	return true;
}

static inline bool decl_or_expr_to_expr_stmt(Context *context, Ast *stmt)
{
	if (stmt->ast_kind == AST_EXPR_STMT) return true;
	assert(stmt->ast_kind == AST_DECLARE_STMT);
	stmt->ast_kind = AST_EXPR_STMT;
	Decl *decl = stmt->declare_stmt;
	if (decl->decl_kind == DECL_MULTI_DECL)
	{
		Expr *list = expr_new(EXPR_EXPRESSION_LIST, stmt->token);
		Expr **exprs = NULL;
		VECEACH(decl->multi_decl, i)
		{
			Decl *var = decl->multi_decl[i];
			assert(var->decl_kind == DECL_VAR);
			assert(var->decl_kind == VARDECL_LOCAL);
			if (var->var.init_expr == NULL)
			{
				SEMA_ERROR(var->name, "'%s' needs to be assigned.", var->name.string);
				return false;
			}
			Expr *assign_expr = expr_new(EXPR_BINARY, stmt->token);
			assign_expr->resolve_status = RESOLVE_DONE;
			assign_expr->binary_expr.operator = TOKEN_EQ;
			Expr *identifier = expr_new(EXPR_IDENTIFIER, var->name);
			identifier->resolve_status = RESOLVE_DONE;
			identifier->identifier_expr.decl = var;
			identifier->type = var->var.type;
			assign_expr->binary_expr.left = identifier;
			assign_expr->binary_expr.right = var->var.init_expr;
			exprs = VECADD(exprs, assign_expr);
		}
		list->expression_list = exprs;
		stmt->expr_stmt = list;
		return true;
	}
	assert(decl->decl_kind == DECL_VAR);
	assert(decl->decl_kind == VARDECL_LOCAL);
	if (decl->var.init_expr == NULL)
	{
		SEMA_ERROR(decl->name, "'%s' needs to be assigned.", decl->name.string);
		return false;
	}
	stmt->expr_stmt = decl->var.init_expr;
	return true;
}

static inline bool sema_flatten_cond(Context *context, Ast *stmt)
{
	assert(stmt->ast_kind == AST_STMT_LIST);
	assert(vec_size(stmt->stmt_list) > 0);

	// The common case:
	if (vec_size(stmt->stmt_list) == 1 && stmt->stmt_list[0]->ast_kind == AST_EXPR_STMT)
	{
		Expr *expr = stmt->stmt_list[0]->expr_stmt;
		if (!cast(expr, type_bool, CAST_TYPE_IMPLICIT)) return false;
		stmt->ast_kind = AST_COND_STMT;
		stmt->cond_stmt.expr = expr;
		stmt->cond_stmt.stmts = NULL;
		return true;
	}
	Ast **new_list = NULL;
	Expr *last = NULL;

	unsigned last_index = vec_size(stmt->stmt_list) - 1;
	VECEACH(stmt->stmt_list, i)
	{
		if (!convert_stmt_for_cond(context, stmt->stmt_list[i], &new_list, &last, last_index == i)) return false;
	}
	if (!cast(last, type_bool, CAST_TYPE_IMPLICIT)) return false;
	stmt->ast_kind = AST_COND_STMT;
	stmt->cond_stmt.expr = last;
	stmt->cond_stmt.stmts = new_list;
	return true;
}

static inline bool sema_analyse_while_stmt(Context *context, Ast *statement)
{
	LOG_FUNC
	Ast *cond = statement->while_stmt.cond;
	Ast *body = statement->while_stmt.body;
	assert(cond && cond->ast_kind == AST_STMT_LIST);
	context_push_scope_with_flags(context, SCOPE_CONTROL);
	bool success = sema_analyse_statement(context, cond);
	sema_flatten_cond(context, cond);
	context_push_scope_with_flags(context, SCOPE_BREAK | SCOPE_CONTINUE); // NOLINT(hicpp-signed-bitwise)
	success = success && sema_analyse_statement(context, body);
	context_pop_scope(context);
	context_pop_scope(context);
	if (!success) return false;
	statement->ast_kind = AST_FOR_STMT;
	statement->for_stmt.cond = cond;
	statement->for_stmt.incr = NULL;
	statement->for_stmt.body = body;
	return success;
}

static inline bool sema_analyse_do_stmt(Context *context, Ast *statement)
{
	LOG_FUNC
	Expr *expr = statement->do_stmt.expr;
	Ast *body = statement->do_stmt.body;
	bool success;
	context_push_scope_with_flags(context, SCOPE_BREAK | SCOPE_CONTINUE); // NOLINT(hicpp-signed-bitwise)
	success = sema_analyse_statement(context, body);
	context_pop_scope(context);
	if (!success) return false;
	context_push_scope_with_flags(context, SCOPE_CONTROL);
	success = sema_expr_analysis(context, expr);
	success = success && cast(expr, type_bool, CAST_TYPE_IMPLICIT);
	context_pop_scope(context);
	return success;
}


static inline bool sema_analyse_multi_decl(Context *context, Ast *statement)
{
	Decl *decl = statement->declare_stmt;
	VECEACH(statement->declare_stmt->multi_decl, i)
	{
		if (!sema_analyse_var_decl(context, statement->declare_stmt->multi_decl[i]))
		{
			decl_poison(decl);
			return false;
		}
	}
	return true;
}

static inline bool sema_analyse_declare_stmt(Context *context, Ast *statement)
{
	Decl *decl = statement->declare_stmt;
	if (decl->decl_kind == DECL_MULTI_DECL)
	{
		return sema_analyse_multi_decl(context, statement);
	}
	return sema_analyse_var_decl(context, decl);
}

static inline bool sema_analyse_expr_stmt(Context *context, Ast *statement)
{
	return sema_expr_analysis(context, statement->expr_stmt);
}

static inline bool sema_analyse_defer_stmt(Context *context, Ast *statement)
{
	TODO
}

static inline bool sema_analyse_default_stmt(Context *context, Ast *statement)
{
	TODO
}

bool sema_analyse_stmt_list(Context *context, Ast *statement)
{
	VECEACH(statement->stmt_list, i)
	{
		if (!sema_analyse_statement(context, statement->stmt_list[i])) return false;
	}
	return true;
}

static inline bool sema_analyse_for_stmt(Context *context, Ast *statement)
{
	context_push_scope_with_flags(context, SCOPE_CONTROL);

	bool success = sema_analyse_statement(context, statement->for_stmt.cond);
	success = success && (!statement->for_stmt.incr || sema_analyse_statement(context, statement->for_stmt.incr));
	context_pop_scope(context);
	if (!success) return false;
	context_push_scope_with_flags(context, SCOPE_BREAK | SCOPE_CONTINUE); // NOLINT(hicpp-signed-bitwise)
	success = sema_analyse_statement(context, statement->for_stmt.body);
	context_pop_scope(context);
	return success;
}

static inline bool sema_analyse_goto_stmt(Context *context, Ast *statement)
{
	TODO
}

static inline bool sema_analyse_if_stmt(Context *context, Ast *statement)
{
	TODO
}

static inline bool sema_analyse_label(Context *context, Ast *statement)
{
	TODO
}

static inline bool sema_analyse_nop_stmt(Context *context, Ast *statement)
{
	TODO
}


static bool sema_analyse_catch_stmt(Context *context, Ast *statement)
{
	TODO
}

static bool sema_analyse_asm_stmt(Context *context, Ast *statement)
{
	TODO
}

static bool sema_analyse_attribute(Context *context, Ast *statement)
{
	TODO
}

static bool sema_analyse_break_stmt(Context *context, Ast *statement)
{
	TODO
}

static bool sema_analyse_case_stmt(Context *context, Ast *statement)
{
	TODO
}

static bool sema_analyse_continue_stmt(Context *context, Ast *statement)
{
	TODO
}

static bool sema_analyse_ct_if_stmt(Context *context, Ast *statement)
{
	TODO
}

static bool sema_analyse_switch_stmt(Context *context, Ast *statement)
{
	TODO
}

static bool sema_analyse_try_stmt(Context *context, Ast *statement)
{
	TODO
}

static bool sema_analyse_throw_stmt(Context *context, Ast *statement)
{
	TODO
}

static bool sema_analyse_next_stmt(Context *context, Ast *statement)
{
	TODO
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

static AstAnalysis AST_ANALYSIS[AST_WHILE_STMT + 1] =
{
	[AST_ASM_STMT] = &sema_analyse_asm_stmt,
	[AST_ATTRIBUTE] = &sema_analyse_attribute,
	[AST_BREAK_STMT] = &sema_analyse_break_stmt,
	[AST_CASE_STMT] = &sema_analyse_case_stmt,
	[AST_CATCH_STMT] = &sema_analyse_catch_stmt,
	[AST_COMPOUND_STMT] = &sema_analyse_compound_stmt,
	[AST_CONTINUE_STMT] = &sema_analyse_continue_stmt,
	[AST_CT_IF_STMT] = &sema_analyse_ct_if_stmt,
	[AST_DECLARE_STMT] = &sema_analyse_declare_stmt,
	[AST_DEFAULT_STMT] = &sema_analyse_default_stmt,
	[AST_DEFER_STMT] = &sema_analyse_defer_stmt,
	[AST_DO_STMT] = &sema_analyse_do_stmt,
	[AST_EXPR_STMT] = &sema_analyse_expr_stmt,
	[AST_FOR_STMT] = &sema_analyse_for_stmt,
	[AST_GOTO_STMT] = &sema_analyse_goto_stmt,
	[AST_IF_STMT] = &sema_analyse_if_stmt,
	[AST_LABEL] = &sema_analyse_label,
	[AST_NOP_STMT] = &sema_analyse_nop_stmt,
	[AST_RETURN_STMT] = &sema_analyse_return_stmt,
	[AST_SWITCH_STMT] = &sema_analyse_switch_stmt,
	[AST_TRY_STMT] = &sema_analyse_try_stmt,
	[AST_THROW_STMT] = &sema_analyse_throw_stmt,
	[AST_NEXT_STMT] = &sema_analyse_next_stmt,
	[AST_VOLATILE_STMT] = &sema_analyse_volatile_stmt,
	[AST_WHILE_STMT] = &sema_analyse_while_stmt,
	[AST_STMT_LIST] = &sema_analyse_stmt_list
};

bool sema_analyse_statement(Context *context, Ast *statement)
{
	LOG_FUNC
	if (AST_ANALYSIS[statement->ast_kind](context, statement)) return true;
	return ast_poison(statement);
}

static inline bool sema_analyse_function_body(Context *context, Decl *func)
{
	context->active_function_for_analysis = func;
	context->rtype = func->func.function_signature.rtype;
	context->current_scope = &context->scopes[0];
	context->current_scope->local_decl_start = 0;
	context->last_local = &context->locals[0];
	context->unique_index = 0;
	context->in_volatile_section = 0;
	func->func.annotations = CALLOCS(*func->func.annotations);
	context_push_scope(context);
	if (!sema_analyse_compound_statement_no_scope(context, func->func.body)) return false;
	if (context->current_scope->exit != EXIT_RETURN && func->func.function_signature.rtype->canonical != type_void)
	{
		SEMA_ERROR(func->name, "Missing return statement at the end of the function.");
		return false;
	}
	context_pop_scope(context);
	return true;
}
static inline bool sema_analyse_func(Context *context, Decl *decl)
{
	bool all_ok = sema_analyse_function_signature(context, &decl->func.function_signature, true);
	if (decl->func.struct_parent)
	{
		all_ok = sema_resolve_type(context, decl->func.struct_parent) && all_ok;
	}
	all_ok = all_ok && sema_analyse_function_body(context, decl);
	if (!all_ok) decl_poison(decl);
	return all_ok;
}

static inline bool sema_analyse_macro(Context *context, Decl *decl)
{
	Type *rtype = decl->macro_decl.rtype;
	if (decl->macro_decl.rtype && !sema_resolve_type(context, rtype)) return false;
	VECEACH(decl->macro_decl.parameters, i)
	{
		Decl *param = decl->macro_decl.parameters[i];
		assert(param->decl_kind == DECL_VAR);
		assert(param->var.kind == VARDECL_PARAM);
		if (param->var.type && !sema_resolve_type(context, param->var.type)) return false;
	}
	return true;
}

static inline void sema_analyse_decl(Context *context, Decl *decl)
{
	LOG_FUNC
	DEBUG_LOG("Analyse %s", decl->name.string);
	switch (decl->decl_kind)
	{
		case DECL_IMPORT:
			// TODO
			break;
		case DECL_STRUCT:
			sema_analyse_struct(context, decl);
			break;
		case DECL_FUNC:
			sema_analyse_func(context, decl);
			break;
		case DECL_MACRO:
			sema_analyse_macro(context, decl);
			break;
		default:
			TODO
	}
}


bool context_register_global(Context *context, Decl *decl)
{
	Decl *old = stable_set(&context->local_symbols, decl->name.string, decl);
	if (!old && decl->visibility != VISIBLE_LOCAL)
	{
		old = stable_set(&context->module->symbols, decl->name.string, decl);
	}
	if (!old && decl->visibility == VISIBLE_PUBLIC)
	{
		old = stable_set(&context->module->public_symbols, decl->name.string, decl);
	}
	if (old != NULL)
	{
		show_shadow_error(decl, old);
		decl_poison(decl);
		return false;
	}
	return true;
}

static inline void sema_register_declarations(Context *context)
{
	VECEACH(context->declarations, i)
	{
		context_register_global(context, context->declarations[i]);
	}
}

static inline void sema_analyse_declarations(Context *context)
{
	VECEACH(context->declarations, i)
	{
		sema_analyse_decl(context, context->declarations[i]);
	}
}

static inline void sema_process_imports(Context *context)
{
	// TODO
}
void sema_analysis(Context *context)
{
	sema_process_imports(context);
	sema_register_declarations(context);
	// Skip the ct_if for now -> assume they passed.
	sema_analyse_declarations(context);
}

