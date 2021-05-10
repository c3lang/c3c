// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"

Decl *module_find_symbol(Module *module, const char *symbol)
{
	return stable_get(&module->symbols, symbol);
}

Path *path_create_from_string(const char *string, size_t len, SourceSpan span)
{
	Path *path = CALLOCS(Path);
	path->span = span;
	TokenType type = TOKEN_IDENT;
	path->module = symtab_add(string, len, fnv1a(string, len), &type);
	path->len = len;
	if (type != TOKEN_IDENT)
	{
		SEMA_ERROR(path, "A module name was expected here.");
		return NULL;
	}
	return path;
}

Path *path_find_parent_path(Context *context, Path *path)
{
	const char *last_scope_chars = strrchr(path->module, ':');
	// No parent
	if (!last_scope_chars) return NULL;

	Path *parent_path = path_create_from_string(path->module, last_scope_chars - path->module - 1, INVALID_RANGE);

	assert(parent_path && "Didn't we pass in a TOKEN_IDENT? That's the only reason this could fail.");

	return parent_path;
}