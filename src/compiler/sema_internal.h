#pragma once

// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.


#include "compiler_internal.h"

int sema_check_comp_time_bool(Context *context, Expr *expr);
bool sema_analyse_function_body(Context *context, Decl *func);
void context_pop_scope(Context *context);
void context_push_scope_with_flags(Context *context, ScopeFlags flags);
#define PUSH_X(ast, X) AstId _old_##X##_defer = context->X##_defer; AstId _old_##X = context->X##_target; context->X##_target = astid(ast); context->X##_defer = context->current_scope->defers.start
#define POP_X(X) context->X##_target = _old_##X; context->X##_defer = _old_##X##_defer
#define PUSH_CONTINUE(ast) PUSH_X(ast, continue)
#define POP_CONTINUE() POP_X(continue)
#define PUSH_BREAK(ast) PUSH_X(ast, break)
#define POP_BREAK() POP_X(break)
#define PUSH_NEXT(ast, sast) PUSH_X(ast, next); Ast *_old_next_switch = context->next_switch; context->next_switch = sast
#define POP_NEXT() POP_X(next); context->next_switch = _old_next_switch
#define PUSH_BREAKCONT(ast) PUSH_CONTINUE(ast); PUSH_BREAK(ast)
#define POP_BREAKCONT() POP_CONTINUE(); POP_BREAK()
