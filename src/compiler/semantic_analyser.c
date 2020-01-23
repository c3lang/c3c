// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"

typedef bool(*AstAnalysis)(Context *, Ast*);

static inline DeferId defer_id_from_ast(Ast *ast)
{
	if (!ast) return 0;
	assert(ast->ast_kind == AST_DEFER_STMT);
	return ast->defer_stmt.id;
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


void sema_shadow_error(Decl *decl, Decl *old)
{
	SEMA_ERROR(decl, "The '%s' would shadow a previous declaration.", decl->name);
	SEMA_PREV(old, "The previous use of '%s' was here.", decl->name);
}


static inline void context_push_scope_with_flags(Context *context, ScopeFlags flags)
{
	if (context->current_scope == &context->scopes[MAX_SCOPE_DEPTH - 1])
	{
		FATAL_ERROR("Too deeply nested scopes.");
	}
	ScopeFlags previous_flags = context->current_scope->flags;
	DeferId parent_defer = context->current_scope->defer_last;
	context->current_scope++;
	context->current_scope->exit = EXIT_NONE;
	context->current_scope->local_decl_start = context->last_local;
	context->current_scope->defer_top = parent_defer;
	context->current_scope->defer_last = parent_defer;
	context->current_scope->flags = previous_flags | flags;
	context->current_scope->flags_created = flags;
}

static inline void context_push_scope(Context *context)
{
	context_push_scope_with_flags(context, SCOPE_NONE);
}

static inline void context_pop_scope(Context *context, Ast **ast, Expr **expr)
{
	assert(context->current_scope != &context->scopes[0]);
	context->last_local = context->current_scope->local_decl_start;
	ExitType exit_type = context->current_scope->exit;
	if (context->current_scope->defer_top != context->current_scope->defer_last)
	{
		TODO;
	}
	context->current_scope--;
	if (context->current_scope->exit < exit_type)
	{
		context->current_scope->exit = exit_type;
	}
}



static bool sema_resolve_ptr_type(Context *context, TypeInfo *type_info)
{
	if (!sema_resolve_type_shallow(context, type_info->pointer))
	{
		return type_info_poison(type_info);
	}
	type_info->type = type_get_ptr(type_info->pointer->type);
	type_info->resolve_status = RESOLVE_DONE;
	return true;
}


static bool sema_resolve_array_type(Context *context, TypeInfo *type)
{
	if (!sema_resolve_type_info(context, type->array.base))
	{
		return type_info_poison(type);
	}
	if (type->array.len)
	{
		if (!sema_analyse_expr(context, type_usize, type->array.len)) return type_info_poison(type);
		if (type->array.len->expr_kind != EXPR_CONST)
		{
			SEMA_ERROR(type->array.len, "Expected a constant value as array size.");
			return type_info_poison(type);
		}
	}
	assert(!type->array.len || type->array.len->expr_kind == EXPR_CONST);
	type->type = type_get_array(type->array.base->type, type->array.len ? type->array.len->const_expr.i : 0);
	type->resolve_status = RESOLVE_DONE;
	return true;
}
static inline bool sema_analyse_struct_member(Context *context, Decl *decl)
{
	if (decl->decl_kind == DECL_STRUCT || decl->decl_kind == DECL_UNION)
	{
		DEBUG_LOG("Beginning analysis of inner struct/union");
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
		DEBUG_LOG("Analysis complete.");
		return decl_ok(decl);
	}
	assert(decl->decl_kind == DECL_VAR);
	assert(decl->var.kind == VARDECL_MEMBER);
	assert(!decl->var.init_expr);
	if (!sema_resolve_type_info(context, decl->var.type_info))
	{
		decl_poison(decl);
		return false;
	}
	decl->type = decl->var.type_info->type;
	assert(decl->var.type_info->type);
	return true;
}

static inline bool sema_analyse_struct_union(Context *context, Decl *decl)
{
	DEBUG_LOG("Beginning analysis of %s.", decl->name);
	assert(decl->decl_kind == DECL_STRUCT || decl->decl_kind == DECL_UNION);
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
	DEBUG_LOG("Analysis complete.");
	// Todo, resolve alignment, size etc.
	return decl_ok(decl);
}



static inline bool sema_analyse_function_param(Context *context, Decl *param, bool is_function)
{
	assert(param->decl_kind == DECL_VAR);
	assert(param->var.kind == VARDECL_PARAM);
	if (!sema_resolve_type_info(context, param->var.type_info))
	{
		return false;
	}
	param->type = param->var.type_info->type;
	if (param->var.init_expr && !is_function)
	{
		SEMA_ERROR(param->var.init_expr, "Function types may not have default arguments.");
		return false;
	}
	if (param->var.init_expr)
	{
		Expr *expr = param->var.init_expr;
		if (!sema_analyse_expr(context, param->type, expr)) return false;
		if (expr->expr_kind != EXPR_CONST)
		{
			SEMA_ERROR(expr, "Only constant expressions may be used as default values.");
			return false;
		}
	}
	return true;
}

static inline Type *sema_analyse_function_signature(Context *context, FunctionSignature *signature, bool is_function)
{
	char buffer[2048];
	size_t buffer_write_offset = 0;
	bool all_ok = true;
	all_ok = sema_resolve_type_info(context, signature->rtype) && all_ok;
	if (all_ok)
	{
		type_append_signature_name(signature->rtype->type, buffer, &buffer_write_offset);
		buffer[buffer_write_offset++] = '(';
	}
	STable *names = &context->scratch_table;
	stable_clear(names);
	VECEACH(signature->params, i)
	{
		Decl *param = signature->params[i];
		assert(param->resolve_status == RESOLVE_NOT_DONE);
		param->resolve_status = RESOLVE_RUNNING;
		if (!sema_analyse_function_param(context, param, is_function))
		{
			decl_poison(param);
			all_ok = false;
			continue;
		}
		param->resolve_status = RESOLVE_DONE;
		if (i > 0 && all_ok)
		{
			buffer[buffer_write_offset++] = ',';
		}
		type_append_signature_name(param->var.type_info->type, buffer, &buffer_write_offset);
		if (param->name)
		{
			Decl *prev = stable_set(names, param->name, param);
			if (prev)
			{
				SEMA_ERROR(param, "Duplicate parameter name %s.", param->name);
				SEMA_PREV(prev, "Previous use of the name was here.");
				decl_poison(prev);
				decl_poison(param);
				all_ok = false;
			}
		}
	}
	if (signature->variadic)
	{
		buffer[buffer_write_offset++] = ',';
		buffer[buffer_write_offset++] = '.';
		buffer[buffer_write_offset++] = '.';
		buffer[buffer_write_offset++] = '.';
	}
	buffer[buffer_write_offset++] = ')';
	if (vec_size(signature->throws))
	{
		buffer[buffer_write_offset++] = '!';
		VECEACH(signature->throws, i)
		{
			TODO
			if (i > 0 && all_ok)
			{
				buffer[buffer_write_offset++] = ',';
			}
//			type_append_signature_name(signature->tparam->var.type, buffer, &buffer_write_offset);
		}
	}

	if (!all_ok) return NULL;
	TokenType type = TOKEN_INVALID_TOKEN;
	signature->mangled_signature = symtab_add(buffer, buffer_write_offset, fnv1a(buffer, buffer_write_offset), &type);
	Type *func_type = stable_get(&context->local_symbols, signature->mangled_signature);
	if (!func_type)
	{
		func_type = type_new(TYPE_FUNC, signature->mangled_signature);
		func_type->canonical = func_type;
		func_type->func.signature = signature;
		stable_set(&context->local_symbols, signature->mangled_signature, func_type);
	}
	return func_type;

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
	/*
	if (parent->exit < compound_statement->exit)
	{
		parent->exit = compound_statement->exit;
	}*/
	return all_ok;
}

static inline bool sema_analyse_return_stmt(Context *context, Ast *statement)
{
	context->current_scope->exit = EXIT_RETURN;
	Type *expected_rtype = context->rtype;
	Expr *return_expr = statement->return_stmt.expr;
	statement->return_stmt.defer = VECLAST(context->defers);
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
	if (!sema_analyse_expr(context, expected_rtype, return_expr)) return false;
	if (!expected_rtype)
	{
		assert(context->evaluating_macro);
		context->rtype = type_void;
		context->active_function_for_analysis->func.function_signature.rtype->type->canonical = statement->return_stmt.expr->type->canonical;
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
	if (!sema_resolve_type_info(context, decl->var.type_info)) return decl_poison(decl);
	decl->type = decl->var.type_info->type;
	if (decl->var.init_expr)
	{
		if (!sema_analyse_expr(context, decl->type, decl->var.init_expr)) return decl_poison(decl);
	}
	if (!sema_add_local(context, decl)) return decl_poison(decl);
	return true;
}

static inline Ast *convert_expr_to_ast(Expr *expr)
{
	Ast *ast = AST_NEW(AST_EXPR_STMT, expr->span);
	ast->expr_stmt = expr;
	return ast;
}
static inline Expr *convert_decl_to_expr(Context *context, Decl *decl)
{
	assert(decl->decl_kind == DECL_VAR);
	assert(decl->decl_kind == VARDECL_LOCAL);
	if (!decl->var.init_expr) return NULL;
	Expr *assign_expr = expr_new(EXPR_BINARY, decl->span);
	assign_expr->resolve_status = RESOLVE_DONE;
	Expr *identifier = expr_new(EXPR_IDENTIFIER, decl->span);
	identifier->resolve_status = RESOLVE_DONE;
	identifier->identifier_expr.identifier = decl->name;
	identifier->identifier_expr.decl = decl;
	assign_expr->binary_expr.left = identifier;
	assign_expr->binary_expr.right = decl->var.init_expr;
	assign_expr->binary_expr.operator = BINARYOP_ASSIGN;
	// Possibly not right v TODO
	identifier->type = decl->var.type_info->type;
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
			SEMA_ERROR(decl, "Expected an initializer for '%s'.", decl->name);
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


static inline bool sema_analyse_function_block_stmt(Context *context, Ast *stmt)
{
	TODO
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
	context_pop_scope(context, &body, NULL);
	context_pop_scope(context, &cond, NULL);
	context_pop_scope(context, &decl, NULL);
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
	context_pop_scope(context, &body, NULL);
	if (!success) return false;
	context_push_scope(context);
	success = sema_analyse_expr(context, type_bool, expr);
	context_pop_scope(context, NULL, &expr);
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
	context_push_scope_with_flags(context, SCOPE_DEFER | SCOPE_CONTINUE); // NOLINT(hicpp-signed-bitwise)
	// Only ones allowed.
	context->current_scope->flags &= SCOPE_DEFER | SCOPE_CONTINUE; // NOLINT(hicpp-signed-bitwise)

	bool success = sema_analyse_statement(context, statement->defer_stmt.body);

	context_pop_scope(context, &statement->defer_stmt.body, NULL);

	if (!success) return false;

	statement->defer_stmt.prev_defer = VECLAST(context->defers);
	vec_add(context->defers, statement);
	statement->defer_stmt.id = vec_size(context->defers);
	return true;
}

static inline bool sema_analyse_default_stmt(Context *context, Ast *statement)
{
	SEMA_ERROR(statement, "Unexpected 'default' outside of switch");
	return false;
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
		success = sema_analyse_expr(context, type_bool, statement->for_stmt.cond);
		// Conditional scope end
		context_pop_scope(context, NULL, &statement->for_stmt.cond);
	}
	if (success && statement->for_stmt.incr)
	{
		// Incr scope start
		context_push_scope(context);
		success = sema_analyse_statement(context, statement->for_stmt.incr);
		// Incr scope end
		context_pop_scope(context, &statement->for_stmt.incr, NULL);
	}
	if (!success) return false;

	// Create the for body scope.
	context_push_scope_with_flags(context, SCOPE_BREAK | SCOPE_CONTINUE); // NOLINT(hicpp-signed-bitwise)
	success = sema_analyse_statement(context, statement->for_stmt.body);
	// End for body scope
	context_pop_scope(context, &statement->for_stmt.body, NULL);
	// End for scope
	context_pop_scope(context, &statement, NULL);
	return success;
}

static inline bool sema_analyse_goto_stmt(Context *context, Ast *statement)
{
	VECEACH(context->labels, i)
	{
		Ast *label = context->labels[i];
		if (statement->goto_stmt.label_name == label->label_stmt.name)
		{
			statement->goto_stmt.type = GOTO_JUMP_BACK;
			label->label_stmt.is_used = true;
			statement->goto_stmt.label = label;
		}
	}
	vec_add(context->gotos, statement);
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
			SEMA_ERROR(statement->if_stmt.then_body, "if-statements with an 'else' must use '{ }' even around a single statement.");
			success = false;
		}
		if (success && statement->if_stmt.else_body->ast_kind != AST_COMPOUND_STMT)
		{
			SEMA_ERROR(statement->if_stmt.else_body,
			           "An 'else' must use '{ }' even around a single statement.");
			success = false;
		}
	}
	context_push_scope(context);
	success = success && sema_analyse_statement(context, statement->if_stmt.then_body);
	context_pop_scope(context, &statement->if_stmt.then_body, NULL);
	// TODO null flowcheck
	if (statement->if_stmt.else_body)
	{
		context_push_scope(context);
		success = success && sema_analyse_statement(context, statement->if_stmt.else_body);
		context_pop_scope(context, &statement->if_stmt.else_body, NULL);
	}
	context_pop_scope(context, &statement, NULL);
	return success;
}

static inline bool sema_analyse_label(Context *context, Ast *statement)
{
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
	vec_add(context->labels, statement);
	VECEACH(context->gotos, i)
	{
		Ast *the_goto = context->gotos[i];
		if (the_goto->goto_stmt.label_name == statement->label_stmt.name)
		{
			the_goto->goto_stmt.type = GOTO_JUMP_FORWARD;
			the_goto->goto_stmt.label = statement;
			statement->label_stmt.is_used = true;
			break;
		}
	}
	return true;
}

static inline bool sema_analyse_nop_stmt(Context *context, Ast *statement)
{
	return true;
}


static bool sema_analyse_catch_stmt(Context *context, Ast *statement)
{
	TODO
}

static bool sema_analyse_asm_stmt(Context *context, Ast *statement)
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
	return true;
}

static bool sema_analyse_case_stmt(Context *context, Ast *statement)
{
	SEMA_ERROR(statement, "Unexpected 'case' outside of switch");
	return false;
}

static bool sema_analyse_continue_stmt(Context *context, Ast *statement)
{
	if (!(context->current_scope->flags & SCOPE_CONTINUE))  // NOLINT(hicpp-signed-bitwise)
	{
		SEMA_ERROR(statement, "'continue' is not allowed here.");
		return false;
	}
	return true;
}

static inline bool sema_analyse_then_overwrite(Context *context, Ast *statement, Ast *replacement)
{
	if (!sema_analyse_statement(context, replacement)) return false;
	// Overwrite
	*statement = *replacement;
	return true;
}

static int sema_check_comp_time_bool(Context *context, Expr *expr)
{
	if (!sema_analyse_expr(context, type_bool, expr)) return -1;
	if (expr->expr_kind != EXPR_CONST)
	{
		SEMA_ERROR(expr, "$if requires a compile time constant value.");
		return -1;
	}
	return expr->const_expr.b;
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

static bool sema_analyse_switch_case(Context *context, Ast*** prev_cases, Ast *case_stmt, Type *switch_type, Ast **prev_case)
{
	if (case_stmt->ast_kind == AST_CASE_STMT)
	{
		if (*prev_case)
		{
			// sema_build_defer_chain(context, prev_cases);
			context_pop_scope(context, prev_case, NULL);
			*prev_case = NULL;
		}
		Expr *case_expr = case_stmt->case_stmt.expr;
		if (!sema_analyse_expr(context, switch_type, case_expr)) return false;
		if (case_expr->expr_kind != EXPR_CONST)
		{
			SEMA_ERROR(case_expr, "This must be a constant expression.");
			return false;
		}
		assert(case_expr->const_expr.type == CONST_INT);
		case_stmt->case_stmt.value_type = type_is_signed(case_expr->type->canonical) ? CASE_VALUE_INT : CASE_VALUE_UINT;
		uint64_t val = case_expr->const_expr.i;
		case_stmt->case_stmt.val = val;
		*prev_case = case_stmt;
		VECEACH(*prev_cases, i)
		{
			if ((*prev_cases)[i]->case_stmt.val == val)
			{
				SEMA_ERROR(case_stmt, "Duplicate case value.");
				sema_prev_at_range((*prev_cases)[i]->span, "Previous use was here.");
				return false;
			}
		}
		context_push_scope_with_flags(context, SCOPE_BREAK);
		vec_add(*prev_cases, case_stmt);
		return true;
	}
	if (case_stmt->ast_kind == AST_DEFAULT_STMT)
	{
		case_stmt->ast_kind = AST_CASE_STMT;
		case_stmt->case_stmt.value_type = CASE_VALUE_DEFAULT;
		case_stmt->case_stmt.block = NULL;
		if (*prev_case)
		{
			context_pop_scope(context, prev_case, NULL);
		}
		context_push_scope(context);
		*prev_case = case_stmt;
		vec_add(*prev_cases, case_stmt);
		return true;
	}
	if (!*prev_case)
	{
		SEMA_ERROR(case_stmt, "Expected a 'case' or 'default' statement.");
		return false;
	}
	if (case_stmt->ast_kind == AST_NEXT_STMT)
	{
		case_stmt->next_stmt = *prev_case;
		(*prev_case)->case_stmt.has_next = true;
	}
	else
	{
		if (!sema_analyse_statement(context, case_stmt)) return false;
	}
	if (!(*prev_case)->case_stmt.block)
	{
		(*prev_case)->case_stmt.block = AST_NEW(AST_COMPOUND_STMT, (*prev_case)->span);
	}
	vec_add((*prev_case)->case_stmt.block->compound_stmt.stmts, case_stmt);
	return true;
}

static bool sema_analyse_switch_stmt(Context *context, Ast *statement)
{
	context_push_scope(context);
	bool success = sema_analyse_statement(context, statement->switch_stmt.cond);
	Ast *cond = statement->switch_stmt.cond;
	success = success && sema_analyse_cond(context, cond, false);
	context_push_scope_with_flags(context, SCOPE_BREAK | SCOPE_NEXT); // NOLINT(hicpp-signed-bitwise)
	Ast *body = statement->switch_stmt.body;
	assert(body->ast_kind == AST_COMPOUND_STMT);
	Type *switch_type = ast_cond_type(cond)->canonical;
	if (!type_is_integer(switch_type))
	{
		SEMA_ERROR(cond, "Expected an integer or enum type, was '%s'.", type_to_error_string(switch_type));
		return false;
	}
	Ast *in_case = NULL;
	VECEACH(body->compound_stmt.stmts, i)
	{
		success = success && sema_analyse_switch_case(context, &statement->switch_stmt.cases, body->compound_stmt.stmts[i], switch_type, &in_case);
	}
	if (in_case)
	{
		context_pop_scope(context, &in_case, NULL);
	}
	context_pop_scope(context, &cond, NULL);
	context_pop_scope(context, &statement, NULL);
	return success;
}

static bool sema_analyse_try_stmt(Context *context, Ast *statement)
{
	TODO
}

static bool sema_analyse_throw_stmt(Context *context, Ast *statement)
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
	context_pop_scope(context, &statement, NULL);
	return success;
}

static inline bool sema_analyse_statement_inner(Context *context, Ast *statement)
{
	switch (statement->ast_kind)
	{
		case AST_POISONED:
			return false;
		case AST_ATTRIBUTE:
			UNREACHABLE
		case AST_ASM_STMT:
			return sema_analyse_asm_stmt(context, statement);
		case AST_BREAK_STMT:
			return sema_analyse_break_stmt(context, statement);
		case AST_CASE_STMT:
			return sema_analyse_case_stmt(context, statement);
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
			return sema_analyse_default_stmt(context, statement);
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
			return sema_analyse_nop_stmt(context, statement);
		case AST_RETURN_STMT:
			return sema_analyse_return_stmt(context, statement);
		case AST_SWITCH_STMT:
			return sema_analyse_switch_stmt(context, statement);
		case AST_THROW_STMT:
			return sema_analyse_throw_stmt(context, statement);
		case AST_TRY_STMT:
			return sema_analyse_try_stmt(context, statement);
		case AST_NEXT_STMT:
			UNREACHABLE
		case AST_VOLATILE_STMT:
			return sema_analyse_volatile_stmt(context, statement);
		case AST_WHILE_STMT:
			return sema_analyse_while_stmt(context, statement);
		case AST_DECL_EXPR_LIST:
			return sema_analyse_decl_expr_list(context, statement);
		case AST_FUNCTION_BLOCK_STMT:
			return sema_analyse_function_block_stmt(context, statement);
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

static inline bool sema_analyse_function_body(Context *context, Decl *func)
{
	context->active_function_for_analysis = func;
	context->rtype = func->func.function_signature.rtype->type;
	context->current_scope = &context->scopes[0];
	// Clean out the current scope.
	memset(context->current_scope, 0, sizeof(*context->current_scope));
	context->labels = NULL;
	context->gotos = NULL;
	context->last_local = &context->locals[0];
	context->in_volatile_section = 0;
	func->func.annotations = CALLOCS(*func->func.annotations);
	context_push_scope(context);
	Decl **params = func->func.function_signature.params;
	VECEACH(params, i)
	{
		if (!sema_add_local(context, params[i])) return false;
	}
	if (!sema_analyse_compound_statement_no_scope(context, func->func.body)) return false;
	if (context->current_scope->exit != EXIT_RETURN)
	{
		if (func->func.function_signature.rtype->type->canonical != type_void)
		{
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
		if (!goto_stmt->goto_stmt.defer && !label_target->label_stmt.defer) continue;

		// First we need to search for the common depth.
		int label_depth = defer_depth(label_target->label_stmt.defer);
		int goto_depth = defer_depth(goto_stmt->goto_stmt.defer);

		Ast *common_depth_label = label_target->label_stmt.defer;
		Ast *common_depth_goto = goto_stmt->goto_stmt.defer;

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
		goto_stmt->goto_stmt.defer_end = common_depth_goto;

		// Mark all defers that occur on the way "up" to the common depth conditional.
		Ast *current = label_target->label_stmt.defer;
		while (current != common_depth_goto)
		{
			current->defer_stmt.emit_boolean = true;
		}
	}
	func->func.labels = context->labels;
	context_pop_scope(context, &func->func.body, NULL);
	context->current_scope = NULL;
	return true;
}


static inline bool sema_analyse_method_function(Context *context, Decl *decl)
{
	TypeInfo *parent_type = decl->func.type_parent;
	if (!sema_resolve_type_info(context, parent_type)) return false;
	if (!type_may_have_method_functions(parent_type->type))
	{
		SEMA_ERROR(decl, "Method functions can not be associated with '%s'", type_to_error_string(decl->func.type_parent->type));
		return false;
	}
	Decl *parent = parent_type->type->decl;
	VECEACH(parent->method_functions, i)
	{
		Decl *function = parent->method_functions[i];
		if (function->name == decl->name)
		{
			SEMA_ERROR(decl, "Duplicate name '%s' for method function.", function->name);
			SEMA_PREV(function, "Previous definition here.");
			return false;
		}
	}
	DEBUG_LOG("Method function '%s.%s' analysed.", parent->name, decl->name);
	vec_add(parent->method_functions, decl);
	return true;
}

static inline bool sema_analyse_func(Context *context, Decl *decl)
{
	DEBUG_LOG("Analysing function %s", decl->name);
	Type *func_type = sema_analyse_function_signature(context, &decl->func.function_signature, true);
	decl->type = func_type;
	if (!func_type) return decl_poison(decl);
	if (decl->func.type_parent)
	{
		if (!sema_analyse_method_function(context, decl)) return decl_poison(decl);
	}
	if (decl->func.body && !sema_analyse_function_body(context, decl)) return decl_poison(decl);
	if (decl->name == main_name)
	{
		if (decl->visibility == VISIBLE_LOCAL)
		{
			SEMA_ERROR(decl, "'main' cannot have local visibility.");
			return false;
		}
		decl->visibility = VISIBLE_EXTERN;
	}
	DEBUG_LOG("Function analysis done.")
	return true;
}

static inline bool sema_analyse_macro(Context *context, Decl *decl)
{
	TypeInfo *rtype = decl->macro_decl.rtype;
	if (decl->macro_decl.rtype && !sema_resolve_type_info(context, rtype)) return false;
	VECEACH(decl->macro_decl.parameters, i)
	{
		Decl *param = decl->macro_decl.parameters[i];
		assert(param->decl_kind == DECL_VAR);
		assert(param->var.kind == VARDECL_PARAM);
		if (param->var.type_info && !sema_resolve_type_info(context, param->var.type_info)) return false;
	}
	return true;
}

static inline bool sema_analyse_global(Context *context, Decl *decl)
{
	if (!sema_resolve_type_info(context, decl->var.type_info)) return false;
	if (decl->var.init_expr)
	{
		if (!sema_analyse_expr(context, decl->type, decl->var.init_expr)) return false;
		if (decl->var.init_expr->expr_kind != EXPR_CONST)
		{
			SEMA_ERROR(decl->var.init_expr, "The expression must be a constant value.");
			return false;
		}
	}
	switch (decl->var.kind)
	{
		case VARDECL_CONST:
			assert(decl->var.init_expr);
			return true;
		case VARDECL_GLOBAL:
			return true;
		default:
			UNREACHABLE
			break;
	}
}

static inline bool sema_analyse_typedef(Context *context, Decl *decl)
{
	if (decl->typedef_decl.is_func)
	{
		Type *func_type = sema_analyse_function_signature(context, &decl->typedef_decl.function_signature, false);
		if (!func_type) return false;
		decl->type->canonical = func_type;
		return true;
	}
	if (!sema_resolve_type_info(context, decl->typedef_decl.type_info)) return false;
	decl->type->canonical = decl->typedef_decl.type_info->type;
	// Do we need anything else?
	return true;
}

static inline bool sema_analyse_generic(Context *context, Decl *decl)
{
	TODO
	return true;
}

static inline bool sema_analyse_enum(Context *context, Decl *decl)
{
	if (!sema_resolve_type_info(context, decl->enums.type_info)) return false;
	uint64_t value = 0;
	Type *type = decl->enums.type_info->type;
	bool success = true;
	VECEACH(decl->enums.values, i)
	{
		Decl *enum_value = decl->enums.values[i];
		enum_value->enum_constant.ordinal = i;
		assert(enum_value->resolve_status == RESOLVE_NOT_DONE);
		assert(enum_value->decl_kind == DECL_ENUM_CONSTANT);
		enum_value->resolve_status = RESOLVE_RUNNING;
		Expr *expr = enum_value->enum_constant.expr;
		if (!expr)
		{
			expr = expr_new(EXPR_CONST, INVALID_RANGE);
			expr->type = type;
			expr->resolve_status = RESOLVE_DONE;
			expr->const_expr.type = CONST_INT;
			expr->const_expr.i = value;
			enum_value->enum_constant.expr = expr;
		}
		if (!sema_analyse_expr(context, type, expr))
		{
			success = false;
			enum_value->resolve_status = RESOLVE_DONE;
			continue;
		}
		assert(type_is_integer(expr->type->canonical));
		if (expr->expr_kind != EXPR_CONST)
		{
			SEMA_ERROR(expr, "Expected a constant expression for enum");
			success = false;
		}
		enum_value->resolve_status = RESOLVE_DONE;
		enum_value->type = decl->type;
	}
	return success;
}

static inline bool sema_analyse_error(Context *context, Decl *decl)
{
	// TODO assign numbers to constants
	return true;
}

bool sema_analyse_decl(Context *context, Decl *decl)
{
	if (decl->resolve_status == RESOLVE_DONE) return decl_ok(decl);

	DEBUG_LOG("Analyse %s", decl->name);
	if (decl->resolve_status == RESOLVE_RUNNING)
	{
		SEMA_ERROR(decl, "Recursive dependency on %s.", decl->name);
		decl_poison(decl);
		return false;
	}

	decl->resolve_status = RESOLVE_RUNNING;
	decl->module = context->module;
	switch (decl->decl_kind)
	{
		case DECL_THROWS:
			TODO
		case DECL_STRUCT:
		case DECL_UNION:
			if (!sema_analyse_struct_union(context, decl)) return decl_poison(decl);
			decl_set_external_name(decl);
			break;
		case DECL_FUNC:
			if (!sema_analyse_func(context, decl)) return decl_poison(decl);
			decl_set_external_name(decl);
			break;
		case DECL_MACRO:
			if (!sema_analyse_macro(context, decl)) return decl_poison(decl);
			break;
		case DECL_VAR:
			if (!sema_analyse_global(context, decl)) return decl_poison(decl);
			decl_set_external_name(decl);
			break;
		case DECL_TYPEDEF:
			if (!sema_analyse_typedef(context, decl)) return decl_poison(decl);
			break;
		case DECL_ENUM:
			if (!sema_analyse_enum(context, decl)) return decl_poison(decl);
			decl_set_external_name(decl);
			break;
		case DECL_ERROR:
			if (!sema_analyse_error(context, decl)) return decl_poison(decl);
			decl_set_external_name(decl);
			break;
		case DECL_GENERIC:
			if (!sema_analyse_generic(context, decl)) return decl_poison(decl);
			break;
		case DECL_ATTRIBUTE:
			TODO
		case DECL_POISONED:
		case DECL_IMPORT:
		case DECL_ENUM_CONSTANT:
		case DECL_ERROR_CONSTANT:
		case DECL_ARRAY_VALUE:
		case DECL_CT_ELSE:
		case DECL_CT_ELIF:
			UNREACHABLE
		case DECL_CT_IF:
			// Handled elsewhere
			UNREACHABLE
	}
	decl->resolve_status = RESOLVE_DONE;
	return true;
}



static void append_decls(Context *context, Decl **decls)
{
	VECEACH(decls, i)
	{
		context_register_global_decl(context, decls[i]);
	}
}
static inline bool sema_analyse_top_level_if(Context *context, Decl *ct_if)
{
	int res = sema_check_comp_time_bool(context, ct_if->ct_if_decl.expr);
	if (res == -1) return false;
	if (res)
	{
		append_decls(context, ct_if->ct_if_decl.then);
		return true;
	}
	Decl *ct_elif = ct_if->ct_if_decl.elif;
	while (ct_elif)
	{
		if (ct_elif->decl_kind == DECL_CT_ELIF)
		{
			res = sema_check_comp_time_bool(context, ct_elif->ct_elif_decl.expr);
			if (res == -1) return false;
			if (res)
			{
				append_decls(context, ct_elif->ct_elif_decl.then);
				return true;
			}
			ct_elif = ct_elif->ct_elif_decl.elif;
		}
		else
		{
			assert(ct_elif->decl_kind == DECL_CT_ELSE);
			append_decls(context, ct_elif->ct_elif_decl.then);
			return true;
		}
	}
	return true;
}

void sema_analysis_pass_process_imports(Context *context)
{
	VECEACH(context->imports, i)
	{
		Decl *import = context->imports[i];
		import->resolve_status = RESOLVE_RUNNING;
		// IMPROVE error on importing twice.
		Path *path = import->import.path;
		Module *module = stable_get(&compiler.modules, path->module);
		if (!module)
		{
			SEMA_ERROR(import, "No module named '%s' could be found.", path->module);
			decl_poison(import);
			continue;
		}
		import->module = module;
	}
}

void sema_analysis_pass_conditional_compilation(Context *context)
{
	DEBUG_LOG("Pass 1 - analyse: %s", context->file->name);
	for (unsigned i = 0; i < vec_size(context->ct_ifs); i++)
	{
		sema_analyse_top_level_if(context, context->ct_ifs[i]);
	}
}

void sema_analysis_pass_decls(Context *context)
{
	DEBUG_LOG("Pass 2 - analyse: %s", context->file->name);
	VECEACH(context->enums, i)
	{
		sema_analyse_decl(context, context->enums[i]);
	}
	VECEACH(context->types, i)
	{
		sema_analyse_decl(context, context->types[i]);
	}
	VECEACH(context->error_types, i)
	{
		sema_analyse_decl(context, context->error_types[i]);
	}
	VECEACH(context->struct_functions, i)
	{
		sema_analyse_decl(context, context->struct_functions[i]);
	}
	VECEACH(context->vars, i)
	{
		sema_analyse_decl(context, context->vars[i]);
	}
	VECEACH(context->functions, i)
	{
		sema_analyse_decl(context, context->functions[i]);
	}
}


bool sema_resolve_type_info(Context *context, TypeInfo *type_info)
{
	if (!sema_resolve_type_shallow(context, type_info)) return false;
	return true;
}

static bool sema_resolve_type_identifier(Context *context, TypeInfo *type_info)
{
	Decl *ambiguous_decl;
	Decl *decl = sema_resolve_symbol(context, type_info->unresolved.name_loc.string, type_info->unresolved.path, &ambiguous_decl);

	if (!decl)
	{
		SEMA_TOKEN_ERROR(type_info->unresolved.name_loc, "Unknown type '%s'.", type_info->unresolved.name_loc.string);
		return type_info_poison(type_info);
	}

	if (ambiguous_decl)
	{
		SEMA_TOKEN_ERROR(type_info->unresolved.name_loc, "Ambiguous type '%s' – both defined in %s and %s, please add the module name to resolve the ambiguity", type_info->unresolved.name_loc.string,
		                 decl->module->name->module, ambiguous_decl->module->name->module);
		return type_info_poison(type_info);
	}
	switch (decl->decl_kind)
	{
		case DECL_THROWS:
			TODO
		case DECL_STRUCT:
		case DECL_UNION:
		case DECL_ERROR:
		case DECL_ENUM:
			type_info->type = decl->type;
			type_info->resolve_status = RESOLVE_DONE;
			DEBUG_LOG("Resolved %s.", type_info->unresolved.name_loc.string);
			return true;
		case DECL_TYPEDEF:
			// TODO func
			if (!sema_resolve_type_info(context, decl->typedef_decl.type_info))
			{
				decl_poison(decl);
				return type_info_poison(type_info);
			}
			DEBUG_LOG("Resolved %s.", type_info->unresolved.name_loc.string);
			type_info->type = decl->type;
			type_info->resolve_status = RESOLVE_DONE;
			return true;
		case DECL_POISONED:
			return type_info_poison(type_info);
		case DECL_FUNC:
		case DECL_VAR:
		case DECL_ENUM_CONSTANT:
		case DECL_ERROR_CONSTANT:
		case DECL_ARRAY_VALUE:
		case DECL_IMPORT:
		case DECL_MACRO:
		case DECL_GENERIC:
			SEMA_TOKEN_ERROR(type_info->unresolved.name_loc, "This is not a type.");
			return type_info_poison(type_info);
		case DECL_CT_ELSE:
		case DECL_CT_IF:
		case DECL_CT_ELIF:
		case DECL_ATTRIBUTE:
			UNREACHABLE
	}
	UNREACHABLE

}

bool sema_resolve_type_shallow(Context *context, TypeInfo *type_info)
{
	if (type_info->resolve_status == RESOLVE_DONE) return type_info_ok(type_info);

	if (type_info->resolve_status == RESOLVE_RUNNING)
	{
		// TODO this is incorrect for unresolved expressions
		SEMA_TOKEN_ERROR(type_info->unresolved.name_loc, "Circular dependency resolving type '%s'.", type_info->unresolved.name_loc.string);
		return type_info_poison(type_info);
	}

	type_info->resolve_status = RESOLVE_RUNNING;

	switch (type_info->kind)
	{
		case TYPE_INFO_POISON:
		case TYPE_INFO_INC_ARRAY:
			UNREACHABLE
		case TYPE_INFO_IDENTIFIER:
			return sema_resolve_type_identifier(context, type_info);
		case TYPE_INFO_EXPRESSION:
			if (!sema_analyse_expr(context, NULL, type_info->unresolved_type_expr))
			{
				return type_info_poison(type_info);
			}
			TODO
		case TYPE_INFO_ARRAY:
			return sema_resolve_array_type(context, type_info);
		case TYPE_INFO_POINTER:
			return sema_resolve_ptr_type(context, type_info);
	}

}