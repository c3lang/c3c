// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <utils/errors.h>
#include <utils/lib.h>
#include "semantic_analyser.h"
#include "../utils/file_utils.h"
#include "symtab.h"

CompilationUnit current_unit;

void sema_init(File *file)
{
	LOG_FUNC
	current_unit.file = file;
	current_unit.module_name.type = INVALID_TOKEN;
}

void sema_add_module(Token module_name)
{
	LOG_FUNC
	current_unit.module_name = module_name;
}

void sema_add_module_from_filename(void)
{
	LOG_FUNC
	char buffer[MAX_IDENTIFIER_LENGTH + 1];
	int len = filename_to_module(current_unit.file->full_path, buffer);
	if (!len)
	{
		TODO
	}

	TokenType type = TOKEN_VAR_IDENT;
	const char *module_name = symtab_add(buffer, len, fnv1a(buffer, len), &type);
	if (type != TOKEN_VAR_IDENT)
	{
		TODO
	}
	current_unit.module_name.string = module_name;
	TODO
}
void sema_add_import(Token module_name, Token alias, ImportType import_type)
{
	TODO
}

void sema_add_errors(Token error_type_name /* todo values */)
{
	TODO
}
void sema_add_macro_var(Token macro_var_name /* , expr **/ )
{
	TODO
}

// If we have a public parameter, then the next one will be the real one.
void sema_mark_next_public(void)
{
	TODO
}

void sema_verror_at(SourceRange range, const char *message, va_list args)
{
	TODO
}
