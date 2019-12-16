// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"



static inline bool matches_subpath(Path *path_to_check, Path *path_to_find)
{
	// This checks the full match.
	if (path_to_find->module == path_to_check->module) return true;

	// Let's check the offset on where to start comparing to start with
	// the submatch.
	ssize_t compare_start = (ssize_t)path_to_check->len - (ssize_t)path_to_find->len;

	// The smallest match is the situation a::foo::bar vs foo::bar
	// This means that the compare_start must be 3 or more.
	if (compare_start < 3) return false;

	// We also want to make sure that the preceeding 2 characters are ::
	if (path_to_check->module[compare_start - 1] != ':' || path_to_check->module[compare_start - 2] != ':') return false;

	// Ok, now we know this is a subpath, so check:
	return 0 == memcmp(path_to_check->module + compare_start, path_to_find->module, path_to_find->len);
}

static Decl *sema_resolve_path_symbol(Context *context, const char *symbol, Path *path, Decl **ambiguous_other_decl)
{
	assert(path && "Expected path.");
	*ambiguous_other_decl = NULL;
	Decl *decl = NULL;
	VECEACH(context->imports, i)
	{
		Decl *import = context->imports[i];
		// Partial imports
		if (import->import.symbol.string && import->import.symbol.string != symbol) continue;
		// Full import, first match the subpath.
		if (path->len > import->import.path->len) continue;
		if (!matches_subpath(import->import.path, path)) continue;
		Decl *found = module_find_symbol(import->module, symbol, MODULE_SYMBOL_SEARCH_EXTERNAL);
		if (!found) continue;
		if (decl)
		{
			*ambiguous_other_decl = found;
			continue;
		}
		decl = found;
	}
	context_register_external_symbol(context, decl);
	return decl;
}

Decl *sema_resolve_symbol(Context *context, const char *symbol, Path *path, Decl **ambiguous_other_decl)
{
	if (path)
	{
		return sema_resolve_path_symbol(context, symbol, path, ambiguous_other_decl);
	}

	*ambiguous_other_decl = NULL;

	if (context->current_scope)
	{
		Decl **first = &context->locals[0];
		Decl **current = context->last_local - 1;
		while (current >= first)
		{
			if (current[0]->name.string == symbol) return current[0];
			current--;
		}
	}

	// Search in file scope.
	Decl *decl = stable_get(&context->local_symbols, symbol);

	if (decl) return decl;

	// Search in the module and child modules.
	decl = module_find_symbol(context->module, symbol, MODULE_SYMBOL_SEARCH_THIS);

	if (decl)
	{
		context_register_external_symbol(context, decl);
		return decl;
	}

	// Search in imports
	VECEACH(context->imports, i)
	{
		Decl *import = context->imports[i];
		if (!decl_ok(import)) continue;
		Decl *found = module_find_symbol(import->module, symbol, MODULE_SYMBOL_SEARCH_EXTERNAL);
		if (!found) continue;
		if (decl)
		{
			*ambiguous_other_decl = found;
			continue;
		}
		decl = found;
	}
	if (!decl) return NULL;
	context_register_external_symbol(context, decl);
	return decl;
}

bool sema_add_local(Context *context, Decl *decl)
{
	Decl *dummy;
	Decl *other = sema_resolve_symbol(context, decl->name.string, NULL, &dummy);
	if (other)
	{
		sema_shadow_error(decl, other);
		decl_poison(decl);
		decl_poison(other);
		return false;
	}
	Decl *** vars = &context->active_function_for_analysis->func.annotations->vars;
	unsigned num_vars = vec_size(*vars);
	if (num_vars == MAX_LOCALS - 1 || context->last_local == &context->locals[MAX_LOCALS - 1])
	{
		SEMA_ERROR(decl->name, "Reached the maximum number of locals.");
		return false;
	}
	decl->var.id = num_vars;
	*vars = VECADD(*vars, decl);
	context->last_local[0] = decl;
	context->last_local++;
	return true;
}


