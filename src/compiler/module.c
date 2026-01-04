// Copyright (c) 2019-2025 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"

Decl *module_find_symbol(Module *module, const char *symbol)
{
	Decl *decl = htable_get(&module->symbols, (void*)symbol);
	return decl && decl->visibility != VISIBLE_LOCAL ? decl : NULL;
}

Decl *module_find_symbol_in_unit(Module *module, CompilationUnit *unit, const char *symbol)
{
	Decl *decl = htable_get(&unit->local_symbols, (void*)symbol);
	if (decl) return decl;
	return module_find_symbol(module, symbol);
}

void scratch_buffer_append_module(Module *module, bool is_export)
{
	if (module->extname)
	{
		scratch_buffer_append(module->extname);
		return;
	}
	const char *name = module->name->module;
	char c;
	while ((c = *(name++)) != 0)
	{
		switch (c)
		{
			case ':':
				ASSERT(name[0] == ':');
				scratch_buffer_append_char(is_export ? '_' : '.');
				name++;
				break;
			case '.':
				if (is_export)
				{
					scratch_buffer_append_char('_');
					break;
				}
				FALLTHROUGH;
			default:
				scratch_buffer_append_char(c);
				break;
		}
	}
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
	if (scratch_buffer.len > 128)
	{
		scratch_buffer.len = 128;
		scratch_buffer_printf("@%X", module);
	}
	return scratch_buffer_to_string();
}


Path *path_create_from_string(const char *string, uint32_t len, SourceSpan span)
{
	ASSERT(string);
	Path *path = CALLOCS(Path);
	path->span = span;
	TokenType type = TOKEN_IDENT;
	path->module = symtab_add(string, len, fnv1a(string, len), &type);
	path->len = len;
	if (type != TOKEN_IDENT)
	{
		PRINT_ERROR_AT(path, "A module name was expected here.");
		return NULL;
	}
	return path;
}
