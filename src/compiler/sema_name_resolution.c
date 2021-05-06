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

static Decl *sema_resolve_path_symbol(Context *context, const char *symbol, Path *path, Decl **ambiguous_other_decl,
                                      Decl **private_decl)
{
	assert(path && "Expected path.");
	*ambiguous_other_decl = NULL;
	Decl *decl = NULL;
	bool path_found = false;

	// 1. Do we match our own path?
	if (matches_subpath(context->module->name, path))
	{
		// 2. If so just get the symbol.
		return module_find_symbol(context->module, symbol);
	}
	// 3. Loop over imports.
	VECEACH(context->imports, i)
	{
		Decl *import = context->imports[i];
		// 4. Don't look through parameterized modules.
		if (import->module->parameters) continue;

		// TODO handle partial imports.

		// 5. Can we match a subpath?
		if (path->len > import->import.path->len) continue;
		if (!matches_subpath(import->import.path, path)) continue;

		// 6. We have a sub path match at least.
		path_found = true;

		// 7. Find the symbol
		Decl *found = module_find_symbol(import->module, symbol);

		// 8. No match, so continue
		if (!found) continue;

		// 9. If we found something private and we don't import privately?
		if (found->visibility <= VISIBLE_MODULE && !import->import.private && !decl)
		{
			// 10. Register this as a possible private decl.
			*private_decl = found;
			continue;
		}

		// 11. Did we already have a match?
		if (decl)
		{
			// 12. Then set an ambiguous match.
			*ambiguous_other_decl = found;
			continue;
		}

		// 13. We've found a match.
		decl = found;
		*private_decl = NULL;
	}
	// 14. If we didn't find one.
	if (!decl)
	{
		// 15. If the path wasn't found, then let's say that.
		if (!path_found)
		{
			SEMA_ERROR(path, "Unknown module '%.*s', did you forget to import it?", path->len, path->module);
			return poisoned_decl;
		}
		// 16. Otherwise return null.
		return NULL;
	}
	// 17. Store that this external symbol is used and return.
	context_register_external_symbol(context, decl);
	return decl;
}

Decl *sema_resolve_symbol_in_current_dynamic_scope(Context *context, const char *symbol)
{
	if (context->current_scope)
	{
		Decl **first = context->current_scope->local_decl_start;
		Decl **current = context->last_local - 1;
		while (current >= first)
		{
			if (current[0]->name == symbol) return current[0];
			current--;
		}
	}
	return NULL;
}

Decl *sema_resolve_symbol(Context *context, const char *symbol, Path *path, Decl **ambiguous_other_decl,
                          Decl **private_decl)
{
	if (path)
	{
		return sema_resolve_path_symbol(context, symbol, path, ambiguous_other_decl, private_decl);
	}

	*ambiguous_other_decl = NULL;

	if (context->current_scope)
	{
		Decl **first = &context->locals[0];
		if (context->macro_nesting) first = context->macro_locals_start;
		Decl **current = context->last_local - 1;
		while (current >= first)
		{
			if (current[0]->name == symbol) return current[0];
			current--;
		}
	}

	// Search in file scope.
	Decl *decl = stable_get(&context->local_symbols, symbol);

	if (decl) return decl;

	// Search in the module.
	decl = module_find_symbol(context->module, symbol);

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
		Decl *found = module_find_symbol(import->module, symbol);
		if (!found) continue;
		// If we found something private and we don't import privately?
		if (found->visibility <= VISIBLE_MODULE && !import->import.private && !decl)
		{
			// 10. Register this as a possible private decl.
			*private_decl = found;
			continue;
		}
		if (decl)
		{
			*ambiguous_other_decl = found;
			continue;
		}
		decl = found;
		*private_decl = found;
	}
	if (!decl) return NULL;
	context_register_external_symbol(context, decl);
	return decl;
}

static inline bool sema_append_local(Context *context, Decl *decl)
{
	if (context->last_local == &context->locals[MAX_LOCALS - 1])
	{
		SEMA_ERROR(decl, "Reached the maximum number of locals.");
		return false;
	}
	context->last_local[0] = decl;
	context->last_local++;
	return true;
}

bool sema_add_member(Context *context, Decl *decl)
{
	return sema_append_local(context, decl);
}

bool sema_add_local(Context *context, Decl *decl)
{
	Decl *dummy;
	Decl *dummy2;
	Decl *other = sema_resolve_symbol(context, decl->name, NULL, &dummy, &dummy2);
	if (other)
	{
		sema_shadow_error(decl, other);
		decl_poison(decl);
		decl_poison(other);
		return false;
	}
	Decl ***vars = &context->active_function_for_analysis->func.annotations->vars;
	unsigned num_vars = vec_size(*vars);
	if (num_vars == MAX_LOCALS - 1)
	{
		SEMA_ERROR(decl, "Reached the maximum number of locals.");
		return false;
	}
	vec_add(*vars, decl);
	decl->resolve_status = RESOLVE_DONE;
	return sema_append_local(context, decl);
}

bool sema_unwrap_var(Context *context, Decl *decl)
{
	Decl *alias = COPY(decl);
	alias->var.kind = VARDECL_ALIAS;
	alias->var.alias = decl;
	alias->var.failable = false;
	decl->resolve_status = RESOLVE_DONE;
	return sema_append_local(context, decl);
}

bool sema_rewrap_var(Context *context, Decl *decl)
{
	assert(decl->decl_kind == DECL_VAR && decl->var.kind == VARDECL_ALIAS && decl->var.alias->var.failable);
	return sema_append_local(context, decl->var.alias);
}

bool sema_add_macro_local(Context *context, Decl *decl)
{
	return sema_add_local(context, decl);
}

