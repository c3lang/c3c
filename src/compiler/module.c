// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"

Decl *module_find_symbol(Module *module, const char *symbol, ModuleSymbolSearch search)
{
	Decl *decl = stable_get(&module->symbols, symbol);
	if (decl)
	{
		switch (decl->visibility)
		{
			case VISIBLE_LOCAL:
			case VISIBLE_EXTERN:
				decl = NULL;
				break;
			case VISIBLE_MODULE:
				if (search == MODULE_SYMBOL_SEARCH_EXTERNAL) decl = NULL;
				break;
			case VISIBLE_PUBLIC:
				break;
		}
	}
	if (!decl)
	{
		if (search == MODULE_SYMBOL_SEARCH_THIS) search = MODULE_SYMBOL_SEARCH_PARENT;
		VECEACH (module->sub_modules, i)
		{
			if ((decl = module_find_symbol(module->sub_modules[i], symbol, search))) break;
		}
	}
	return decl;
}

Path *path_create_from_string(Context *context, const char *string, size_t len, SourceSpan span)
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

	Path *parent_path = path_create_from_string(context, path->module, last_scope_chars - path->module - 1, INVALID_RANGE);

	// Should never fail.
	assert(parent_path);

	return parent_path;
}