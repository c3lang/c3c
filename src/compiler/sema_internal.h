#pragma once

// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.


#include "compiler_internal.h"

int sema_check_comp_time_bool(Context *context, Expr *expr);
bool sema_analyse_function_body(Context *context, Decl *func);
void context_pop_scope(Context *context);
void context_push_scope_with_flags(Context *context, ScopeFlags flags);
