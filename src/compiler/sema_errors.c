// Copyright (c) 2022-2025 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"

void sema_shadow_error(SemaContext *context, Decl *decl, Decl *old)
{
	SEMA_ERROR(decl, "'%s' would shadow a previous declaration.", decl->name);
	SEMA_NOTE(old, "The previous use of '%s' was here.", decl->name);
}

bool sema_type_error_on_binop(SemaContext *context, Expr *expr)
{
	const char *c = token_type_to_string(binaryop_to_token(expr->binary_expr.operator));
	SEMA_ERROR(expr, "%s is not defined in the expression %s %s %s.",
			   c, type_quoted_error_string(exprptr(expr->binary_expr.left)->type),
			   c, type_quoted_error_string(exprptr(expr->binary_expr.right)->type));
	return false;
}
