#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "compiler_common.h"

typedef struct
{
	Token module_name;
	File *file;
} CompilationUnit;

typedef enum {
	IMPORT_TYPE_FULL,
	IMPORT_TYPE_ALIAS,
	IMPORT_TYPE_ALIAS_LOCAL,
	IMPORT_TYPE_LOCAL
} ImportType;

void sema_init(File *file);
void sema_add_module(Token module_name);
void sema_add_module_from_filename(void);
void sema_add_import(Token module_name, Token alias, ImportType import_type);
void sema_add_errors(Token error_type_name /* todo values */);
void sema_add_macro_var(Token macro_var_name /* , expr **/ );

// If we have a public parameter, then the next one will be the real one.
void sema_mark_next_public(void);

void sema_verror_at(SourceRange range, const char *message, va_list args);
