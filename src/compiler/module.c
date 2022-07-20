// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"

Decl *module_find_symbol(Module *module, const char *symbol)
{
	return htable_get(&module->symbols, symbol);
}

const char *module_create_object_file_name(Module *module)
{
	scratch_buffer_clear();
	char c;
	const char *name = module->name->module;
	while ((c = *(name++)))
	{
		switch (c)
		{
			case '$':
				if (*name == '$') name++;
				if (*name) scratch_buffer_append_char('.');
				break;
			case ':':
				if (*name == ':') name++;
				if (*name) scratch_buffer_append_char('.');
				break;
			default:
				scratch_buffer_append_char(c);
				break;
		}
	}
	return scratch_buffer_to_string();
}

Path *path_create_from_string(const char *string, uint32_t len, SourceSpan span)
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
