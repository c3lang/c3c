// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"

Decl *module_find_symbol(Module *module, const char *symbol)
{
	return htable_get(&module->symbols, (void*)symbol);
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
				ASSERT0(name[0] == ':');
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
	if (compiler.build.single_module == SINGLE_MODULE_ON)
	{
		if (compiler.build.output_name) return compiler.build.output_name;
		if (compiler.build.name) return compiler.build.name;
	}
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
	ASSERT0(string);
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
