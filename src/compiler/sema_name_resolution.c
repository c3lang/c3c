// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"

#if defined(_MSC_VER)
// This isn't standard apparently, so MSVC doesn't have it built in...
typedef long long int ssize_t;
#endif

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

Decl *sema_resolve_symbol_in_current_dynamic_scope(Context *context, const char *symbol)
{
	Decl **first = context->active_scope.local_decl_start;
	Decl **current = context->active_scope.current_local;
	while (current > first)
	{
		current--;
		if (current[0]->name == symbol) return current[0];
	}
	return NULL;
}

static Decl *sema_resolve_path_symbol(Context *context, const char *symbol, Path *path, Decl **ambiguous_other_decl,
                                      Decl **private_decl, bool *path_found)
{
	assert(path && "Expected path.");
	*ambiguous_other_decl = NULL;
	Decl *decl = NULL;
	*path_found = false;

	// 0. std module special handling.
	if (path->module == global_context.std_module_path.module)
	{
		return module_find_symbol(&global_context.std_module, symbol);
	}

	Context *real_context = context->macro_scope.macro ? context->macro_scope.macro->macro_decl.context : context;

	// 1. Do we match our own path?
	if (matches_subpath(real_context->module->name, path))
	{
		// 2. If so just get the symbol.
		return module_find_symbol(real_context->module, symbol);
	}

	// 3. Loop over imports.
	VECEACH(real_context->imports, i)
	{
		Decl *import = real_context->imports[i];

		if (import->module->is_generic) continue;

		// 4. Can we match a subpath?
		if (path->len > import->import.path->len) continue;
		if (!matches_subpath(import->import.path, path)) continue;

		// 5. We have a sub path match at least.
		*path_found = true;

		// 6. Find the symbol
		Decl *found = module_find_symbol(import->module, symbol);

		// 7. No match, so continue
		if (!found) continue;

		// 8. If we found something private and we don't import privately?
		if (found->visibility <= VISIBLE_MODULE && !import->import.private && !decl)
		{
			// 9. Register this as a possible private decl.
			*private_decl = found;
			continue;
		}

		// 10. Did we already have a match?
		if (decl)
		{
			// 11. Then set an ambiguous match.
			*ambiguous_other_decl = found;
			continue;
		}

		// 12. We've found a match.
		decl = found;
		*private_decl = NULL;
	}

	return decl;
}

static Decl *sema_resolve_no_path_symbol(Context *context, const char *symbol,
                                         Decl **ambiguous_other_decl, Decl **private_decl)
{
	Decl *decl = NULL;

	if (context->active_scope.current_local > &context->locals[0])
	{
		Decl **first = &context->locals[0];
		Decl **current = context->active_scope.current_local - 1;
		if (context->macro_scope.macro)
		{
			first = context->macro_scope.locals_start;
			if (context->macro_scope.in_yield)
			{
				first = context->macro_scope.yield_symbol_start;
				current = context->macro_scope.yield_symbol_end - 1;
			}
		}
		while (current >= first)
		{
			if (current[0]->name == symbol) 
			{
				// We patch special behaviour here.
				if (current[0]->decl_kind == DECL_VAR)
				{
					VarDeclKind kind = current[0]->var.kind;

					// In this case, we erase the value from parent scopes, so it isn't visible here.
					if (kind == VARDECL_ERASE) goto JUMP_ERASED;
					if (kind == VARDECL_REWRAPPED) return current[0]->var.alias;
				}
				return current[0];
			}
			current--;
		}
	}
	JUMP_ERASED:;

	Context *real_context = context->macro_scope.macro ? context->macro_scope.macro->macro_decl.context : context;

	// Search in file scope.
	decl = stable_get(&real_context->local_symbols, symbol);

	if (decl) return decl;


	// Search in the module.
	decl = module_find_symbol(real_context->module, symbol);

	if (decl) return decl;

	// Search in imports
	VECEACH(real_context->imports, i)
	{
		Decl *import = real_context->imports[i];
		if (!decl_ok(import)) continue;

		// Skip parameterized modules
		if (import->module->is_generic) continue;

		Decl *found = module_find_symbol(import->module, symbol);
		if (!found) continue;
		// If we found something private and we don't import privately?
		if (found->visibility <= VISIBLE_MODULE && !import->import.private && !decl)
		{
			// Register this as a possible private decl.
			*private_decl = found;
			continue;
		}
		if (decl)
		{
			// Register this an ambiguous decl.
			*ambiguous_other_decl = found;
			continue;
		}

		decl = found;
		private_decl = NULL;
	}
	return decl;
}


static void sema_report_error_on_decl(const char *symbol_str, SourceSpan span, Decl *found, Decl *ambiguous_decl,
                                      Decl *private_decl)
{
	if (!found && private_decl)
	{
		sema_error_range(span, "The %s '%s' is not visible from this module.", decl_to_name(private_decl), symbol_str);
		return;
	}
	if (ambiguous_decl)
	{
		assert(found);
		const char *symbol_type = decl_to_name(found);
		sema_error_range(span,
						 "The %s '%s' is defined in both '%s' and '%s', please use either %s::%s or %s::%s to resolve the ambiguity.",
		                 symbol_type,
		                 symbol_str,
		                 found->module->name->module,
		                 ambiguous_decl->module->name->module,
		                 found->module->name->module,
		                 symbol_str,
		                 ambiguous_decl->module->name->module,
		                 symbol_str);
		return;
	}
	assert(!found);
	sema_error_range(span, "'%s' could not be found, did you spell it right?", symbol_str);
}

static Decl *sema_resolve_symbol(Context *context, const char *symbol_str, SourceSpan span, Path *path, bool report_error)
{
	Decl *ambiguous_other_decl = NULL;
	Decl *private_decl = NULL;
	bool path_found = false;
	Decl *decl;
	if (path)
	{
		decl = sema_resolve_path_symbol(context, symbol_str, path, &ambiguous_other_decl, &private_decl, &path_found);
		if (!decl && !path_found)
		{
			if (report_error)
			{
				SEMA_ERROR(path, "Unknown module '%.*s', did you forget to import it?", path->len, path->module);
				return poisoned_decl;
			}
			return NULL;
		}
	}
	else
	{
		decl = sema_resolve_no_path_symbol(context, symbol_str, &ambiguous_other_decl, &private_decl);
	}

	if (!decl || ambiguous_other_decl)
	{
		if (!report_error) return NULL;
		sema_report_error_on_decl(symbol_str, span, decl, ambiguous_other_decl, private_decl);
		return poisoned_decl;
	}
	if (decl->module && decl->module != context->module)
	{
		context_register_external_symbol(context, decl);
	}
	return decl;
}

Decl *sema_find_extension_method_in_module(Module *module, Type *type, const char *method_name)
{
	Decl **extensions = module->method_extensions;
	VECEACH(extensions, i)
	{
		Decl *extension = extensions[i];
		if (extension->name != method_name) continue;
		switch (extension->decl_kind)
		{
			case DECL_FUNC:
				if (extension->func_decl.type_parent->type == type) return extension;
				break;
			case DECL_MACRO:
			case DECL_GENERIC:
				if (extension->macro_decl.type_parent->type == type) return extension;
				break;
			default:
				UNREACHABLE
		}
	}
	return NULL;
}

Decl *sema_resolve_method(Context *context, Decl *type, const char *method_name, Decl **ambiguous_ref, Decl **private_ref)
{
	// 1. Look at the previously defined ones.
	VECEACH(type->methods, i)
	{
		Decl *func = type->methods[i];
		if (method_name == func->name) return func;
	}
	// 2. Make a module lookup
	Decl *previously_found = NULL;
	Type *actual_type = type->type;
	Decl *private_type = NULL;
	Decl *result = NULL;
	VECEACH(context->imports, i)
	{
		Decl *import = context->imports[i];

		if (import->module->is_generic) continue;

		Decl *found = sema_find_extension_method_in_module(import->module, actual_type, method_name);
		if (!found) continue;
		if (found->visibility <= VISIBLE_MODULE && !import->import.private)
		{
			private_type = found;
			continue;
		}

		if (result)
		{
			*ambiguous_ref = previously_found;
			*private_ref = NULL;
			return NULL;
		}
		result = found;
	}

	if (result && private_type)
	{
		private_type = NULL;
	}
	return result;
}

Decl *sema_resolve_parameterized_symbol(Context *context, TokenId symbol, Path *path)
{
	Decl *ambiguous_other_decl = NULL;
	Decl *private_decl = NULL;
	Decl *decl = NULL;
	const char *symbol_str = TOKSTR(symbol);
	if (path)
	{
		// 3. Loop over imports.
		VECEACH(context->imports, i)
		{
			Decl *import = context->imports[i];

			// Skip any without parameters.
			if (!import->module->is_generic) continue;

			// 5. Can we match a subpath?
			if (path->len > import->import.path->len) continue;
			if (!matches_subpath(import->import.path, path)) continue;


			// 7. Find the symbol
			Decl *found = module_find_symbol(import->module, symbol_str);

			// 8. No match, so continue
			if (!found) continue;

			// 9. If we found something private and we don't import privately?
			if (found->visibility <= VISIBLE_MODULE && !import->import.private && !decl)
			{
				// 10. Register this as a possible private decl.
				private_decl = found;
				continue;
			}

			// 11. Did we already have a match?
			if (decl)
			{
				// 12. Then set an ambiguous match.
				ambiguous_other_decl = found;
				continue;
			}

			// 13. We've found a match.
			decl = found;
			private_decl = NULL;
		}
		// 14. Error report
		if (!decl || ambiguous_other_decl)
		{
			sema_report_error_on_decl(symbol_str, source_span_from_token_id(symbol), decl, ambiguous_other_decl, private_decl);
			return poisoned_decl;
		}
		return decl;
	}
	// 15. Loop over imports.
	VECEACH(context->imports, i)
	{
		Decl *import = context->imports[i];

		// Skip any without parameters.
		if (!import->module->is_generic) continue;

		// 7. Find the symbol
		Decl *found = module_find_symbol(import->module, symbol_str);

		// 8. No match, so continue
		if (!found) continue;

		// 9. If we found something private and we don't import privately?
		if (found->visibility <= VISIBLE_MODULE && !import->import.private && !decl)
		{
			// 10. Register this as a possible private decl.
			private_decl = found;
			continue;
		}

		// 11. Did we already have a match?
		if (decl)
		{
			// 12. Then set an ambiguous match.
			ambiguous_other_decl = found;
			continue;
		}

		// 13. We've found a match.
		decl = found;
		private_decl = NULL;
	}
	// 14. Error report
	if (!decl || ambiguous_other_decl)
	{
		sema_report_error_on_decl(symbol_str, source_span_from_token_id(symbol), decl, ambiguous_other_decl, private_decl);
		return poisoned_decl;
	}
	return decl;
}

Decl *sema_resolve_normal_symbol(Context *context, TokenId symbol, Path *path, bool handle_error)
{
	return sema_resolve_symbol(context, TOKSTR(symbol), source_span_from_token_id(symbol), path, handle_error);
}

Decl *sema_resolve_string_symbol(Context *context, const char *symbol, SourceSpan span, Path *path, bool report_error)
{
	return sema_resolve_symbol(context, symbol, span, path, report_error);
}

static inline bool sema_append_local(Context *context, Decl *decl)
{
	if (context->active_scope.current_local == &context->locals[MAX_LOCALS - 1])
	{
		SEMA_ERROR(decl, "Reached the maximum number of locals.");
		return false;
	}
	context->active_scope.current_local[0] = decl;
	context->active_scope.current_local++;
	return true;
}

bool sema_add_member(Context *context, Decl *decl)
{
	return sema_append_local(context, decl);
}

bool sema_add_local(Context *context, Decl *decl)
{
	decl->module = context->module;
	// Ignore synthetic locals.
	if (decl->name_token.index == NO_TOKEN_ID.index) return true;
	if (decl->decl_kind == DECL_VAR && decl->var.shadow) goto ADD_VAR;
	Decl *other = sema_resolve_normal_symbol(context, decl->name_token, NULL, false);
	assert(!other || other->module);
	if (other && other->module == context->module)
	{
		sema_shadow_error(decl, other);
		decl_poison(decl);
		decl_poison(other);
		return false;
	}
ADD_VAR:;
	Decl ***vars = &context->active_function_for_analysis->func_decl.annotations->vars;
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
	Decl *alias = decl_copy(decl);
	alias->var.kind = VARDECL_UNWRAPPED;
	alias->var.alias = decl;
	alias->type = alias->type->failable;
	alias->resolve_status = RESOLVE_DONE;
	return sema_append_local(context, alias);
}

bool sema_rewrap_var(Context *context, Decl *decl)
{
	assert(decl->decl_kind == DECL_VAR && decl->var.kind == VARDECL_UNWRAPPED && decl->var.alias->type->type_kind == TYPE_FAILABLE);
	return sema_append_local(context, decl->var.alias);
}

bool sema_erase_var(Context *context, Decl *decl)
{
	Decl *erased = decl_copy(decl);
	erased->var.kind = VARDECL_ERASE;
	erased->resolve_status = RESOLVE_DONE;
	return sema_append_local(context, erased);
}


bool sema_erase_unwrapped(Context *context, Decl *decl)
{
	assert(IS_FAILABLE(decl));
	Decl *rewrapped = decl_copy(decl);
	rewrapped->var.kind = VARDECL_REWRAPPED;
	rewrapped->var.alias = decl;
	rewrapped->type = decl->type;
	rewrapped->resolve_status = RESOLVE_DONE;
	return sema_append_local(context, rewrapped);
}
