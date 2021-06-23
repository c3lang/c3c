// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"

void sema_shadow_error(Decl *decl, Decl *old)
{
	SEMA_ERROR(decl, "'%s' would shadow a previous declaration.", decl->name);
	SEMA_PREV(old, "The previous use of '%s' was here.", decl->name);
}

bool sema_resolve_type_info_maybe_inferred(Context *context, TypeInfo *type_info, bool allow_inferred_type)
{
	if (!sema_resolve_type_shallow(context, type_info, allow_inferred_type, false)) return false;
	Type *type = type_info->type;
	// usize and similar typedefs will not have a decl.
	if (type->type_kind == TYPE_TYPEDEF && type->decl == NULL) return true;
	if (!type_is_user_defined(type)) return true;
	return sema_analyse_decl(context, type->decl);
}

bool sema_resolve_type_info(Context *context, TypeInfo *type_info)
{
	return sema_resolve_type_info_maybe_inferred(context, type_info, false);
}



void context_change_scope_with_flags(Context *context, ScopeFlags flags)
{
	unsigned depth = context->active_scope.depth + 1;
	if (depth > MAX_SCOPE_DEPTH)
	{
		FATAL_ERROR("Too deeply nested scopes.");
	}

	Ast *previous_defer = context->active_scope.in_defer;
	AstId parent_defer = context->active_scope.defer_last;
	Decl **last_local = context->active_scope.current_local;
	assert(parent_defer < 1000000);
	// Defer and expression blocks introduce their own return/break/continue
	// otherwise just merge with the old flags.
	if (!(flags & (SCOPE_DEFER | SCOPE_EXPR_BLOCK)))
	{
		flags = context->active_scope.flags | flags;
	}
	context->active_scope = (DynamicScope) {
			.scope_id = ++context->scope_id,
			.allow_dead_code = false,
			.jump_end = false,
			.depth = depth,
			.current_local = last_local,
			.local_decl_start = last_local,
			.in_defer = previous_defer,
			.defer_last = parent_defer,
			.defer_start = parent_defer,
			.flags = flags,
	};
	if (context->scope_id == 0)
	{
		FATAL_ERROR("Too many scopes.");
	}
}

void context_change_scope_for_label(Context *context, Decl *label)
{
	context_change_scope_with_flags(context, SCOPE_NONE);

	if (label)
	{
		label->label.defer = context->active_scope.defer_last;
		sema_add_local(context, label);
		label->label.scope_defer = astid(context->active_scope.in_defer);
	}
}



void context_pop_defers_to(Context *context, DeferList *list)
{
	list->end = context->active_scope.defer_start;
	assert(context->active_scope.defer_last < 10000000);
	list->start = context->active_scope.defer_last;
	context->active_scope.defer_last = list->end;
}





Expr *context_pop_defers_and_wrap_expr(Context *context, Expr *expr)
{
	DeferList defers = { 0, 0 };
	context_pop_defers_to(context, &defers);
	if (defers.end == defers.start) return expr;
	Expr *wrap = expr_new(EXPR_SCOPED_EXPR, expr->span);
	expr_copy_types(wrap, expr);
	wrap->resolve_status = RESOLVE_DONE;
	wrap->expr_scope.expr = expr;
	wrap->expr_scope.defers = defers;
	return expr;
}

void context_pop_defers_and_replace_ast(Context *context, Ast *ast)
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
	Ast *replacement = ast_copy(ast);
	ast->ast_kind = AST_SCOPED_STMT;
	ast->scoped_stmt.stmt = replacement;
	ast->scoped_stmt.defers = defers;
}

