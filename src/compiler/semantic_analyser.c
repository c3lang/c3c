// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "../utils/errors.h"
#include "../utils/lib.h"
#include "semantic_analyser.h"
#include "../utils/file_utils.h"
#include "symtab.h"
#include "ast.h"
#include "diagnostics.h"
#include "context.h"


void sema_init(File *file)
{
	LOG_FUNC
}

static inline Decl *module_find_symbol(Module *module, const char *symbol)
{
	return stable_get(&module->symbols, symbol);
}



static inline bool sema_resolve_unresolved_array(Type *type)
{
	TODO
}

static inline bool sema_resolve_unresolved_type_expr(Type *type)
{
	TODO
}

static inline bool sema_resolve_unresolved_type(Context *context, Type *type)
{
	assert(type->type_kind == TYPE_UNRESOLVED);
	if (type->unresolved.module.string)
	{
		TODO
	}
	Decl *decl = stable_get(&context->local_symbols, type->unresolved.name.string);
	if (!decl)
	{
		decl = module_find_symbol(context->module, type->unresolved.name.string);
	}
	if (!decl)
	{
		TODO
	}
	if (!decl)
	{
		SEMA_ERROR(type->unresolved.name, "Unknown type '%s'.", type->unresolved.name);
		type->type_kind = TYPE_POISONED;
		return false;
	}
	switch (decl->decl_kind)
	{
		case DECL_STRUCT:
			type->type_kind = TYPE_STRUCT;
			break;
		case DECL_UNION:
			type->type_kind = TYPE_UNION;
			break;
		case DECL_TYPEDEF:
			type->type_kind = TYPE_TYPEDEF;
			break;
		case DECL_FUNC_TYPE:
			type->type_kind = TYPE_FUNC_TYPE;
			break;
		case DECL_ENUM:
			type->type_kind = TYPE_ENUM;
			break;
		case DECL_ERROR:
			type->type_kind = TYPE_ERROR;
			break;
		default:
			SEMA_ERROR(type->unresolved.name, "Unknown type '%s'.", type->unresolved.name);
			type->type_kind = TYPE_POISONED;
			return false;
	}
	type->decl = decl;
	type->canonical_type = decl->type;
    return true;
}
static bool sema_resolve_type(Context *context, Type *type)
{
    LOG_FUNC
	switch (type->type_kind)
	{
		case TYPE_POISONED:
			return false;
		case TYPE_UNRESOLVED:
			return sema_resolve_unresolved_type(context, type);
		case TYPE_UNRESOLVED_EXPR:
			return sema_resolve_unresolved_type_expr(type);
		case TYPE_UNRESOLVED_ARRAY:
			return sema_resolve_unresolved_array(type);
		default:
			return true;
	}
}

static inline bool sema_analyse_struct_member(Context *context, Decl *decl)
{
	LOG_FUNC
	assert(decl->decl_kind == DECL_VAR);
	assert(decl->var.kind == VARDECL_MEMBER);
	assert(!decl->var.init_expr);
	if (!sema_resolve_type(context, decl->type))
	{
		decl_poison(decl);
		return false;
	}
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

static inline bool sema_analyse_expression(Context *context, Expr *expr)
{
	return false;
}

/**
 * Convert an expression to a given type using implicit conversion.
 *
 * @param expr the expression to be implictly converted
 * @param type the type to convert to
 * @return an expression with cast if needed, or NULL if an error has been sent and the conversion failed.
 */
Expr *expr_implicit_conversion(Expr *expr, Type *type)
{
	TODO
	return NULL;
}

static inline bool sema_analyse_function_param(Context *context, Decl *param, bool is_function)
{
	if (!decl_ok(param)) return false;
	assert(param->decl_kind == DECL_VAR);
	assert(param->var.kind == VARDECL_PARAM);
	if (!sema_resolve_type(context, param->type))
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
		sema_analyse_expression(context, expr);
		if (!expr_ok(expr)) return false;
		if (expr->expr_kind != EXPR_CONST)
		{
			SEMA_ERROR(expr->loc, "Only constant expressions may be used as default values.");
			return false;
		}
		Expr *converted_expr = expr_implicit_conversion(expr, param->var.type);
		if (!converted_expr)
		{
			return false;
		}
		param->var.init_expr = converted_expr;
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

static bool sema_analyse_statement(Context *context, Ast *parent, Ast *statement);

static inline bool sema_analyse_compound_statement(Context *context, Ast *parent, Ast *compound_statement)
{
	LOG_FUNC
	bool all_ok = ast_ok(compound_statement);
	VECEACH(compound_statement->compound_stmt.stmts, i)
	{
		if (!sema_analyse_statement(context, compound_statement, compound_statement->compound_stmt.stmts[i]))
		{
			ast_poison(compound_statement->compound_stmt.stmts[i]);
			all_ok = false;
		}
	}
	if (parent->exit < compound_statement->exit)
	{
		parent->exit = compound_statement->exit;
	}
	return all_ok;
}

static inline bool sema_analyse_return_stmt(Context *context, Ast *parent, Ast *statement)
{
	LOG_FUNC
	parent->exit = EXIT_RETURN;
	Type *expected_rtype = context->active_function_for_analysis->func.function_signature.rtype;
	if (statement->return_stmt.expr == NULL)
	{
		if (expected_rtype->type_kind != TYPE_VOID)
		{
			SEMA_ERROR(statement->token, "Expected to return a result of type %s.", type_to_string(expected_rtype));
			return false;
		}
	}
	else
	{
		if (!sema_analyse_expression(context, statement->return_stmt.expr)) return false;
		Expr *conversion = expr_implicit_conversion(statement->return_stmt.expr, expected_rtype);
		if (!conversion) return false;
		statement->return_stmt.expr = conversion;
	}
	return true;
}

static bool sema_analyse_statement(Context *context, Ast *parent, Ast *statement)
{
	LOG_FUNC
	switch (statement->ast_kind)
	{
		case AST_POISONED:
			break;
		case AST_ASM_STMT:
			break;
		case AST_ATTRIBUTE:
			break;
		case AST_BREAK_STMT:
			break;
		case AST_CASE_STMT:
			break;
		case AST_CATCH_STMT:
			break;
		case AST_COMPOUND_STMT:
			break;
		case AST_COND_STMT:
			break;
		case AST_CONTINUE_STMT:
			break;
		case AST_CT_IF_STMT:
			break;
		case AST_CT_ELIF_STMT:
			break;
		case AST_CT_ELSE_STMT:
			break;
		case AST_DECLARE_STMT:
			break;
		case AST_DECL_EXPR_LIST:
			break;
		case AST_DEFAULT_STMT:
			break;
		case AST_DEFER_STMT:
			break;
		case AST_DO_STMT:
			break;
		case AST_EXPR_STMT:
			break;
		case AST_FOR_STMT:
			break;
		case AST_GOTO_STMT:
			break;
		case AST_IF_STMT:
			break;
		case AST_LABEL:
			break;
		case AST_NOP_STMT:
			break;
		case AST_RETURN_STMT:
			return sema_analyse_return_stmt(context, parent, statement);
		case AST_SWITCH_STMT:
			break;
		case AST_THROW_STMT:
			break;
		case AST_TRY_STMT:
			break;
		case AST_NEXT_STMT:
			break;
		case AST_VOLATILE_STMT:
			break;
		case AST_WHILE_STMT:
			break;
		case AST_GENERIC_CASE_STMT:
			break;
		case AST_GENERIC_DEFAULT_STMT:
			break;
	}
	TODO
}

static inline bool sema_analyse_function_body(Context *context, Decl *func)
{
	context->active_function_for_analysis = func;
	if (!sema_analyse_compound_statement(context, func->func.body, func->func.body)) return false;
	if (func->func.body->exit != EXIT_RETURN && func->func.function_signature.rtype->type_kind != TYPE_VOID)
	{
		SEMA_ERROR(func->name, "Missing return statement at the end of the function.");
		return false;
	}
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
		default:
			TODO
	}
}

static void show_shadow_error(Decl *decl, Decl *old)
{
	sema_error_range(decl->name.span, "The '%s' would shadow a previous declaration.", decl->name.string);
	sema_prev_at_range(old->name.span, "The previous use of '%s' was here.", decl->name.string);
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
	context->declarations = VECADD(context->declarations, decl);
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

