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

Decl *sema_resolve_symbol_in_current_dynamic_scope(SemaContext *context, const char *symbol)
{
	Decl **locals = context->locals;
	size_t first = context->active_scope.local_decl_start;
	for (size_t i = context->active_scope.current_local; i > first; i--)
	{
		Decl *decl = locals[i - 1];
		if (decl->name == symbol) return decl;
	}
	return NULL;
}

static inline Decl *sema_find_decl_in_module(Module *module, Path *path, const char *symbol, bool *path_found)
{
	if (!path) return module_find_symbol(module, symbol);
	if (path->len > module->name->len) return NULL;
	if (!matches_subpath(module->name, path)) return NULL;
	*path_found = true;
	return module_find_symbol(module, symbol);
}

static Decl *sema_find_decl_in_imports(Decl **imports, const char *symbol, Path *path, Decl **ambiguous_other_decl,
                                       Decl **private_decl, bool *path_found, bool want_generic)
{
	Decl *decl = NULL;
	// 1. Loop over imports.
	VECEACH(imports, i)
	{
		Decl *import = imports[i];
		if (import->module->is_generic != want_generic) continue;

		// Is the decl in the import.
		Decl *found = sema_find_decl_in_module(import->module, path, symbol, path_found);

		// No match, so continue
		if (!found) continue;

		// If we found something private but we don't import privately?
		if (found->visibility <= VISIBLE_MODULE && !import->import.private && !decl)
		{
			// Register this as a possible private decl.
			*private_decl = found;
			continue;
		}

		// Did we already have a match?
		if (decl)
		{
			// 11. Then set an ambiguous match.
			*ambiguous_other_decl = found;
			continue;
		}

		// We've found a match.
		decl = found;
		*private_decl = NULL;
	}
	return decl;
}

static inline bool sema_is_path_found(Module **modules, Path *path, bool want_generic)
{
	VECEACH(modules, i)
	{
		Module *module = modules[i];
		if (module->is_generic != want_generic) continue;
		if (matches_subpath(module->name, path))
		{
			return true;
		}
	}
	return false;
}

static Decl *sema_find_decl_in_global(DeclTable *table, Module **module_list, const char *symbol, Path *path, Decl **ambiguous_other_decl,
									  Decl **private_decl, bool *path_found, bool want_generic)
{
	Decl *decls = decltable_get(table, symbol);

	// We might have no match at all.
	if (!decls)
	{
		// Update the path found
		if (path && !*path_found) *path_found = sema_is_path_found(module_list, path, want_generic);
		return NULL;
	}

	// There might just be a single match.
	if (decls->decl_kind != DECL_DECLARRAY)
	{
		if (path && !matches_subpath(decls->module->name, path)) return false;
		*private_decl = NULL;
		return decls;
	}

	// Else go through the list
	Decl **decl_list = decls->decl_list;
	Decl *ambiguous = NULL;
	Decl *decl = NULL;
	VECEACH(decl_list, i)
	{
		Decl *candidate = decl_list[i];
		if (!ambiguous && (!path || matches_subpath(candidate->module->name, path)))
		{
			ambiguous = decl;
			decl = candidate;
		}
	}
	if (ambiguous) *ambiguous_other_decl = ambiguous;
	*private_decl = NULL;
	return decl;
}

static Decl *sema_resolve_path_symbol(SemaContext *context, const char *symbol, Path *path, Decl **ambiguous_other_decl,
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

	CompilationUnit *unit = context->unit;

	// 1. Do we match our own path?
	if (matches_subpath(unit->module->name, path))
	{
		// 2. If so just get the symbol.
		return module_find_symbol(unit->module, symbol);
	}

	// 3. Loop over imports.
	decl = sema_find_decl_in_imports(unit->imports, symbol, path, ambiguous_other_decl, private_decl, path_found, false);

	// 4. Go to global search
	return decl ? decl : sema_find_decl_in_global(&global_context.symbols, global_context.module_list, symbol,
	                                              path, ambiguous_other_decl, private_decl, path_found, false);
}

static Decl *sema_resolve_no_path_symbol(SemaContext *context, const char *symbol,
                                         Decl **ambiguous_other_decl, Decl **private_decl)
{
	Decl *decl = NULL;

	Decl **locals = context->locals;
	if (context->active_scope.current_local > 0)
	{
		int64_t first = 0;
		int64_t current = context->active_scope.current_local - 1;
		while (current >= first)
		{
			Decl *cur = locals[current];
			if (cur->name == symbol)
			{
				// We patch special behaviour here.
				if (cur->decl_kind == DECL_VAR)
				{
					VarDeclKind kind = cur->var.kind;

					// In this case, we erase the value from parent scopes, so it isn't visible here.
					if (kind == VARDECL_ERASE) goto JUMP_ERASED;
					if (kind == VARDECL_REWRAPPED) return cur->var.alias;
				}
				return cur;
			}
			current--;
		}
	}
	JUMP_ERASED:;

	CompilationUnit *unit = context->unit;

	// Search in file scope.
	decl = htable_get(&unit->local_symbols, symbol);

	if (decl) return decl;


	// Search in the module.
	decl = module_find_symbol(unit->module, symbol);

	if (decl) return decl;

	decl = sema_find_decl_in_imports(unit->imports, symbol, NULL, ambiguous_other_decl, private_decl, NULL, false);
	return decl ? decl : sema_find_decl_in_global(&global_context.symbols, NULL, symbol, NULL, ambiguous_other_decl, private_decl, NULL, false);
}


static void sema_report_error_on_decl(Path *path, const char *symbol_str, SourceSpan span, Decl *found, Decl *ambiguous_decl,
                                      Decl *private_decl)
{
	if (!found && private_decl)
	{
		if (path)
		{
			sema_error_at(span, "The %s '%s::%s' is not visible from this module.", decl_to_name(private_decl), path->module, symbol_str);
		}
		else
		{
			sema_error_at(span, "The %s '%s' is not visible from this module.", decl_to_name(private_decl), symbol_str);
		}
		return;
	}
	if (ambiguous_decl)
	{
		assert(found);
		const char *symbol_type = decl_to_name(found);
		if (path)
		{
			sema_error_at(span,
							 "The %s '%s::%s' is defined in both '%s' and '%s', please use either %s::%s or %s::%s to resolve the ambiguity.",
							 symbol_type,
							 path->module,
							 symbol_str,
							 found->module->name->module,
							 ambiguous_decl->module->name->module,
							 found->module->name->module,
							 symbol_str,
							 ambiguous_decl->module->name->module,
							 symbol_str);
		}
		else
		{
			sema_error_at(span,
							 "The %s '%s' is defined in both '%s' and '%s', please use either %s::%s or %s::%s to resolve the ambiguity.",
							 symbol_type,
							 symbol_str,
							 found->module->name->module,
							 ambiguous_decl->module->name->module,
							 found->module->name->module,
							 symbol_str,
							 ambiguous_decl->module->name->module,
							 symbol_str);
		}
		return;
	}
	assert(!found);
	if (path)
	{
		sema_error_at(span, "'%s::%s' could not be found, did you spell it right?", path->module, symbol_str);
	}
	else
	{
		sema_error_at(span, "'%s' could not be found, did you spell it right?", symbol_str);
	}
}

static inline Decl *sema_resolve_symbol(SemaContext *context, const char *symbol_str, SourceSpan span, Path *path, bool report_error)
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
			if (!report_error) return NULL;
			SEMA_ERROR(path, "Unknown module '%.*s', did you type it right?", path->len, path->module);
			return poisoned_decl;
		}
	}
	else
	{
		decl = sema_resolve_no_path_symbol(context, symbol_str, &ambiguous_other_decl, &private_decl);
	}

	if (!decl || ambiguous_other_decl)
	{
		if (!report_error) return NULL;
		sema_report_error_on_decl(path, symbol_str, span, decl, ambiguous_other_decl, private_decl);
		return poisoned_decl;
	}
	unit_register_external_symbol(context->compilation_unit, decl);
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

Decl *sema_resolve_method(CompilationUnit *unit, Decl *type, const char *method_name, Decl **ambiguous_ref, Decl **private_ref)
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
	VECEACH(unit->imports, i)
	{
		Decl *import = unit->imports[i];

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

Decl *unit_resolve_parameterized_symbol(CompilationUnit *unit, const char *symbol, SourceSpan span, Path *path)
{
	Decl *ambiguous_other_decl = NULL;
	Decl *private_decl = NULL;
	bool path_found = false;

	Decl *decl = sema_find_decl_in_imports(unit->imports, symbol, path, &ambiguous_other_decl, &private_decl, &path_found, true);
	if (!decl)
	{
		decl = sema_find_decl_in_global(&global_context.generic_symbols,
										global_context.generic_module_list,
										symbol, path,
										&ambiguous_other_decl,
										&private_decl,
										&path_found, true);
	}
	// 14. Error report
	if (!decl || ambiguous_other_decl)
	{
		sema_report_error_on_decl(path, symbol, span, decl, ambiguous_other_decl, private_decl);
		return poisoned_decl;
	}
	if (!decl_is_user_defined_type(decl) && !path)
	{
		sema_error_at(span, "Function and variables must be prefixed with a path, e.g. 'foo::%s'.", symbol);
		return poisoned_decl;
	}

	return decl;
}

Decl *sema_resolve_normal_symbol(SemaContext *context, const char *symbol, SourceSpan span, Path *path, bool handle_error)
{
	return sema_resolve_symbol(context, symbol, span, path, handle_error);
}

Decl *sema_resolve_string_symbol(SemaContext *context, const char *symbol, SourceSpan span, Path *path, bool report_error)
{
	return sema_resolve_symbol(context, symbol, span, path, report_error);
}

static inline bool sema_append_local(SemaContext *context, Decl *decl)
{
	Decl ***locals = &context->locals;
	size_t locals_size = vec_size(*locals);
	size_t current_local = context->active_scope.current_local;
	if (locals_size <= current_local)
	{
		while (locals_size <= current_local)
		{
			vec_add(*locals, decl);
			locals_size++;
		}
	}
	else
	{
		(*locals)[current_local] = decl;
	}
	context->active_scope.current_local++;
	return true;
}

bool sema_add_member(SemaContext *context, Decl *decl)
{
	return sema_append_local(context, decl);
}

bool sema_add_local(SemaContext *context, Decl *decl)
{
	Module *current_module = decl->module = context->unit->module;
	// Ignore synthetic locals.
	if (!decl->name) return true;
	if (decl->decl_kind == DECL_VAR && decl->var.shadow) goto ADD_VAR;
	Decl *other = sema_resolve_normal_symbol(context, decl->name, decl->span, NULL, false);
	assert(!other || other->module);
	if (other && other->module == current_module)
	{
		sema_shadow_error(decl, other);
		decl_poison(decl);
		decl_poison(other);
		return false;
	}
ADD_VAR:;
	Decl ***vars = &context->current_function->func_decl.annotations->vars;
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

bool sema_unwrap_var(SemaContext *context, Decl *decl)
{
	Decl *alias = decl_copy(decl);
	alias->var.kind = VARDECL_UNWRAPPED;
	alias->var.alias = decl;
	alias->type = alias->type->failable;
	alias->resolve_status = RESOLVE_DONE;
	return sema_append_local(context, alias);
}

bool sema_rewrap_var(SemaContext *context, Decl *decl)
{
	assert(decl->decl_kind == DECL_VAR && decl->var.kind == VARDECL_UNWRAPPED && decl->var.alias->type->type_kind == TYPE_FAILABLE);
	return sema_append_local(context, decl->var.alias);
}

bool sema_erase_var(SemaContext *context, Decl *decl)
{
	Decl *erased = decl_copy(decl);
	erased->var.kind = VARDECL_ERASE;
	erased->resolve_status = RESOLVE_DONE;
	return sema_append_local(context, erased);
}


bool sema_erase_unwrapped(SemaContext *context, Decl *decl)
{
	assert(IS_FAILABLE(decl));
	Decl *rewrapped = decl_copy(decl);
	rewrapped->var.kind = VARDECL_REWRAPPED;
	rewrapped->var.alias = decl;
	rewrapped->type = decl->type;
	rewrapped->resolve_status = RESOLVE_DONE;
	return sema_append_local(context, rewrapped);
}
