// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"

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

Decl *sema_decl_stack_resolve_symbol(const char *symbol)
{
	Decl **members = global_context.decl_stack;
	Decl **current = global_context.decl_stack_top;
	Decl **end = global_context.decl_stack_bottom;
	while (current > end)
	{
		Decl *decl = *(--current);
		if (decl->name == symbol) return decl;
	}
	return NULL;
}

Decl **sema_decl_stack_store(void)
{
	Decl **current_bottom = global_context.decl_stack_bottom;
	global_context.decl_stack_bottom = global_context.decl_stack_top;
	return current_bottom;
}

void sema_decl_stack_restore(Decl **state)
{
	global_context.decl_stack_top = global_context.decl_stack_bottom;
	global_context.decl_stack_bottom = state;
}

void sema_decl_stack_push(Decl *decl)
{
	Decl **current = global_context.decl_stack_top;
	if (current == &global_context.decl_stack[MAX_GLOBAL_DECL_STACK])
	{
		error_exit("Declaration stack exhausted.");
	}
	*(current++) = decl;
	global_context.decl_stack_top = current;
}

static void add_members_to_decl_stack(Decl *decl)
{
	VECEACH(decl->methods, i)
	{
		Decl *func = decl->methods[i];
		sema_decl_stack_push(func);
	}
	while (decl->decl_kind == DECL_DISTINCT)
	{
		Type *type = decl->distinct_decl.base_type->canonical;
		if (!type_is_user_defined(type)) break;
		decl = type->decl;
	}
	if (decl_is_enum_kind(decl))
	{
		Decl **members = decl->enums.parameters;
		VECEACH(members, i)
		{
			sema_decl_stack_push(members[i]);
		}
	}
	if (decl_is_struct_type(decl) || decl->decl_kind == DECL_BITSTRUCT)
	{
		Decl **members = decl->strukt.members;
		VECEACH(members, i)
		{
			Decl *member = members[i];
			if (member->name == NULL)
			{
				add_members_to_decl_stack(member);
				continue;
			}
			sema_decl_stack_push(member);
		}
	}
}

Decl *sema_decl_stack_find_decl_member(Decl *decl_owner, const char *symbol)
{
	Decl **state = sema_decl_stack_store();
	add_members_to_decl_stack(decl_owner);
	Decl *member = sema_decl_stack_resolve_symbol(symbol);
	sema_decl_stack_restore(state);
	return member;
}

Decl *sema_resolve_symbol_in_current_dynamic_scope(SemaContext *context, const char *symbol)
{
	Decl **locals = context->locals;
	size_t first = context->active_scope.label_start;
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

static Decl *sema_find_decl_in_imports(Decl **imports, NameResolve *name_resolve, bool want_generic)
{
	Decl *decl = NULL;
	// 1. Loop over imports.
	Path *path = name_resolve->path;
	const char *symbol = name_resolve->symbol;
	VECEACH(imports, i)
	{
		Decl *import = imports[i];
		if (import->import.module->is_generic != want_generic) continue;

		// Is the decl in the import.
		Decl *found = sema_find_decl_in_module(import->import.module, path, symbol, &name_resolve->path_found);

		// No match, so continue
		if (!found) continue;

		// If we found something private but we don't import privately?
		if (found->visibility <= VISIBLE_MODULE && !import->import.private && !decl)
		{
			// Register this as a possible private decl.
			name_resolve->private_decl = found;
			continue;
		}

		// Did we already have a match?
		if (decl)
		{
			if (!path)
			{
				// Prefer already found builtin over new found no builtin
				if (decl->is_autoimport && !found->is_autoimport) continue;
				// Prefer new builtin over non-builtin
				if (found->is_autoimport && !decl->is_autoimport)
				{
					decl = found;
					name_resolve->private_decl = NULL;
					name_resolve->ambiguous_other_decl = NULL;
					continue;
				}
			}
			// 11. Then set an ambiguous match.
			name_resolve->ambiguous_other_decl = found;
			continue;
		}

		// We've found a match.
		decl = found;
		name_resolve->private_decl = NULL;
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

Decl *sema_find_decl_in_modules(Module **module_list, Path *path, const char *interned_name)
{
	bool path_found = false;
	VECEACH(module_list, i)
	{
		Module *module = module_list[i];
		Decl *decl = sema_find_decl_in_module(module, path, interned_name, &path_found);
		if (decl) return decl;
	}
	return NULL;
}
static Decl *sema_find_decl_in_global(DeclTable *table, Module **module_list, NameResolve *name_resolve, bool want_generic)
{
	const char *symbol = name_resolve->symbol;
	Path *path = name_resolve->path;
	DeclId decl_ids = decltable_get(table, symbol);

	// We might have no match at all.
	if (!decl_ids)
	{
		// Update the path found
		if (path && !name_resolve->path_found) name_resolve->path_found = sema_is_path_found(module_list, path, want_generic);
		return NULL;
	}

	Decl *decls = declptr(decl_ids);
	// There might just be a single match.
	if (decls->decl_kind != DECL_DECLARRAY)
	{
		if (path && !matches_subpath(decls->unit->module->name, path)) return false;
		name_resolve->private_decl = NULL;
		return decls;
	}

	// Else go through the list
	Decl **decl_list = decls->decl_list;
	Decl *ambiguous = NULL;
	Decl *decl = NULL;
	VECEACH(decl_list, i)
	{
		Decl *candidate = decl_list[i];
		if (!ambiguous && (!path || matches_subpath(candidate->unit->module->name, path)))
		{
			ambiguous = decl;
			decl = candidate;
		}
	}
	if (ambiguous) name_resolve->ambiguous_other_decl = ambiguous;
	name_resolve->private_decl = NULL;
	return decl;
}

static bool decl_is_visible(CompilationUnit *unit, Decl *decl)
{
	Module *module = decl->unit->module;
	// 1. Same module as unit -> ok
	if (module == unit->module) return true;
	Module *top = module->top_module;
	// 2. Same top module as unit -> ok
	if (top == unit->module->top_module) return true;

	// 3. We want to check std::core
	Module *lookup = module;
	while (lookup)
	{
		if (lookup->name->module == kw_std__core) return true;
		lookup = lookup->parent_module;
	}

	VECEACH(unit->imports, i)
	{
		Decl *import = unit->imports[i];
		Module *import_module = import->import.module;
		// 4. Same as import
		if (import_module == module) return true;
		// 5. If import and decl doesn't share a top module -> no match
		if (import_module->top_module != top) continue;
		Module *search = module->parent_module;
		// 6. Start upward from the decl module
		//    break if no parent or we reached the import module.
		while (search && search != import_module) search = search->parent_module;
		// 7. We found the import module
		if (search) return true;
		// 8. Otherwise go to next
	}
	return false;
}

static Decl *sema_find_decl_in_global_new(CompilationUnit *unit, DeclTable *table, Module **module_list, NameResolve *name_resolve, bool want_generic)
{
	const char *symbol = name_resolve->symbol;
	Path *path = name_resolve->path;
	DeclId decl_ids = decltable_get(table, symbol);
	Decl *maybe_decl = NULL;
	// We might have no match at all.
	if (!decl_ids)
	{
		// Update the path found
		if (path && !name_resolve->path_found) name_resolve->path_found = sema_is_path_found(module_list, path, want_generic);
		return NULL;
	}

	Decl *decls = declptr(decl_ids);
	// There might just be a single match.
	if (decls->decl_kind != DECL_DECLARRAY)
	{
		if (path && !matches_subpath(decls->unit->module->name, path)) return NULL;
		if (!decl_is_visible(unit, decls))
		{
			name_resolve->maybe_decl = decls;
			return NULL;
		}
		name_resolve->private_decl = NULL;
		return decls;
	}

	// Else go through the list
	Decl **decl_list = decls->decl_list;
	Decl *ambiguous = NULL;
	Decl *decl = NULL;
	VECEACH(decl_list, i)
	{
		Decl *candidate = decl_list[i];
		if (path && !matches_subpath(candidate->unit->module->name, path)) continue;
		if (!decl_is_visible(unit, candidate))
		{
			maybe_decl = candidate;
			continue;
		}
		if (!ambiguous)
		{
			ambiguous = decl;
			decl = candidate;
			if (ambiguous)
			{
				// If we have a same match but one is builtin, prefer builtin.
				if (!ambiguous->is_autoimport && decl->is_autoimport)
				{
					ambiguous = NULL;
				}
				else if (ambiguous->is_autoimport && !decl->is_autoimport)
				{
					decl = ambiguous;
					ambiguous = NULL;
				}
			}
		}
	}
	name_resolve->ambiguous_other_decl = ambiguous;
	name_resolve->private_decl = NULL;
	name_resolve->maybe_decl = maybe_decl;
	return decl;
}

static Decl *sema_resolve_path_symbol(SemaContext *context, NameResolve *name_resolve)
{
	Path *path = name_resolve->path;
	name_resolve->ambiguous_other_decl = NULL;
	Decl *decl = NULL;
	name_resolve->path_found = false;
	assert(name_resolve->path && "Expected path.");

	const char *symbol = name_resolve->symbol;
	// 0. std module special handling.
	if (path->module == global_context.std_module_path.module)
	{
		return module_find_symbol(&global_context.std_module, symbol);
	}

	CompilationUnit *unit = context->unit;

	// 1. Do we match our own path?
	if (matches_subpath(unit->module->name, path))
	{
		// 2. If so try to locally get the symbol.
		if ((decl = module_find_symbol(unit->module, symbol))) return decl;
	}

	// 3. Loop over imports.
	decl = sema_find_decl_in_imports(unit->imports, name_resolve, false);

	// 4. Go to global search
	return decl ? decl : sema_find_decl_in_global_new(unit, &global_context.symbols, global_context.module_list, name_resolve, false);
}

static inline Decl *sema_find_local(SemaContext *context, const char *symbol)
{
	Decl **locals = context->locals;
	if (!locals || !context->active_scope.current_local) return NULL;
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
				if (kind == VARDECL_ERASE) return NULL;
				if (kind == VARDECL_REWRAPPED) return cur->var.alias;
			}
			return cur;
		}
		current--;
	}
	return NULL;
}

static Decl *sema_resolve_no_path_symbol(SemaContext *context, NameResolve *name_resolve)
{
	Decl *decl = NULL;

	const char *symbol = name_resolve->symbol;
	assert(name_resolve->path == NULL);

	Decl *found = sema_find_local(context, symbol);
	if (found) return found;

	CompilationUnit *unit = context->unit;

	// Search in file scope.
	decl = htable_get(&unit->local_symbols, symbol);

	if (decl) return decl;

	// Search in the module.
	decl = module_find_symbol(unit->module, symbol);

	if (decl) return decl;

	decl = sema_find_decl_in_imports(unit->imports, name_resolve, false);

	// Special case: the declaration in import is not autoimport and is not a type (which won't be @builtin)
	// e.g we have a malloc builtin and libc::malloc
	if (decl && !decl->is_autoimport && !decl_is_user_defined_type(decl))
	{
		// Find the global
		NameResolve copy = *name_resolve;
		Decl *global = sema_find_decl_in_global_new(context->unit, &global_context.symbols, NULL, name_resolve, false);
		// If it exists and is autoimport, then prefer it.
		if (global && global->is_autoimport) return global;
		*name_resolve = copy;
		return decl;
	}
	return decl ? decl : sema_find_decl_in_global_new(context->unit, &global_context.symbols, NULL, name_resolve, false);
}


static void sema_report_error_on_decl(Decl *found, NameResolve *name_resolve)
{
	assert(!name_resolve->suppress_error);
	const char *symbol = name_resolve->symbol;
	SourceSpan span = name_resolve->span;
	const char *path_name = name_resolve->path ? name_resolve->path->module : NULL;
	if (!found && name_resolve->private_decl)
	{
		const char *private_name = decl_to_name(name_resolve->private_decl);
		if (path_name)
		{
			sema_error_at(span, "The %s '%s::%s' is not visible from this module.",
			              private_name, path_name,
			              symbol);
		}
		else
		{
			sema_error_at(span, "The %s '%s' is not visible from this module.",
			              private_name, symbol);
		}
		return;
	}
	if (!found && name_resolve->maybe_decl)
	{
		const char *maybe_name = decl_to_name(name_resolve->maybe_decl);
		const char *module_name = name_resolve->maybe_decl->unit->module->name->module;
		if (path_name)
		{
			sema_error_at(span, "Did you mean the %s '%s::%s' in module %s? If so please add 'import %s'.",
						  maybe_name, module_name, symbol, module_name, module_name);

		}
		else
		{
			sema_error_at(span, "Did you mean the %s '%s' in module %s? If so please add 'import %s'.",
			              maybe_name, symbol, module_name, module_name);
		}
		return;
	}

	if (name_resolve->ambiguous_other_decl)
	{
		assert(found);
		const char *symbol_type = decl_to_name(found);
		const char *found_path = found->unit->module->name->module;
		const char *other_path = name_resolve->ambiguous_other_decl->unit->module->name->module;
		if (path_name)
		{
			sema_error_at(span,
			              "The %s '%s::%s' is defined in both '%s' and '%s', "
						  "please use either %s::%s or %s::%s to resolve the ambiguity.",
			              symbol_type, path_name, symbol, found_path, other_path,
			              found_path, symbol, other_path, symbol);
		}
		else
		{
			sema_error_at(span,
			              "The %s '%s' is defined in both '%s' and '%s', please use either "
						  "%s::%s or %s::%s to resolve the ambiguity.",
			              symbol_type, symbol, found_path, other_path,
			              found_path, symbol, other_path, symbol);
		}
		return;
	}
	assert(!found);
	if (path_name)
	{
		sema_error_at(span, "'%s::%s' could not be found, did you spell it right?", path_name, symbol);
	}
	else
	{
		sema_error_at(span, "'%s' could not be found, did you spell it right?", symbol);
	}
}

INLINE Decl *sema_resolve_symbol_common(SemaContext *context, NameResolve *name_resolve)
{
	name_resolve->ambiguous_other_decl = NULL;
	name_resolve->private_decl = NULL;
	name_resolve->path_found = false;
	Decl *decl;
	if (name_resolve->path)
	{
		decl = sema_resolve_path_symbol(context, name_resolve);
		if (!decl && !name_resolve->maybe_decl && !name_resolve->path_found)
		{
			if (!name_resolve->suppress_error) return NULL;
			SEMA_ERROR(name_resolve->path, "Unknown module '%.*s', did you type it right?", name_resolve->path->len, name_resolve->path->module);
			return poisoned_decl;
		}
	}
	else
	{
		decl = sema_resolve_no_path_symbol(context, name_resolve);
	}

	if (!decl || name_resolve->ambiguous_other_decl)
	{
		if (name_resolve->suppress_error) return NULL;
		sema_report_error_on_decl(decl, name_resolve);
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
		if (type_infoptr(extension->func_decl.type_parent)->type->canonical == type) return extension;
	}
	return NULL;
}

typedef enum
{
	METHOD_SEARCH_SUBMODULE_CURRENT,
	METHOD_SEARCH_IMPORTED,
	METHOD_SEARCH_CURRENT,
	METHOD_SEARCH_PRIVATE_IMPORTED
} MethodSearchType;


Decl *sema_resolve_method_in_module(Module *module, Type *actual_type, const char *method_name,
									Decl **private_found, Decl **ambiguous, MethodSearchType search_type)
{
	if (module->is_generic) return NULL;
	Decl *found = sema_find_extension_method_in_module(module, actual_type, method_name);
	// The found one might not be visible
	if (found && search_type < METHOD_SEARCH_CURRENT && found->visibility < VISIBLE_PUBLIC)
	{
		*private_found = found;
		found = NULL;
	}
	if (found && search_type == METHOD_SEARCH_CURRENT) return found;
	// We are now searching submodules, so hide the private ones.
	if (search_type == METHOD_SEARCH_CURRENT) search_type = METHOD_SEARCH_SUBMODULE_CURRENT;
	VECEACH(module->sub_modules, i)
	{
		Decl *new_found = sema_resolve_method_in_module(module->sub_modules[i], actual_type, method_name, private_found, ambiguous, search_type);
		if (new_found)
		{
			*ambiguous = new_found;
			return found;
		}
		found = new_found;
	}
	// We might have it ambiguous due to searching sub modules.
	return found;
}

Decl *sema_resolve_method(CompilationUnit *unit, Decl *type, const char *method_name, Decl **ambiguous_ref, Decl **private_ref)
{
	// 1. Look at the previously defined ones.
	VECEACH(type->methods, i)
	{
		Decl *func = type->methods[i];
		if (method_name == func->name) return func;
	}

	Type *actual_type = type->type;
	Decl *private = NULL;
	Decl *ambiguous = NULL;
	Decl *found = sema_resolve_method_in_module(unit->module, actual_type, method_name, &private, &ambiguous, METHOD_SEARCH_CURRENT);
	if (ambiguous)
	{
		*ambiguous_ref = ambiguous;
		assert(found);
		return found;
	}

	// 2. Lookup in imports
	VECEACH(unit->imports, i)
	{
		Decl *import = unit->imports[i];
		if (import->import.module->is_generic) continue;

		Decl *new_found = sema_resolve_method_in_module(import->import.module, actual_type, method_name,
		                                                &private, &ambiguous,
		                                                import->import.private
		                                                ? METHOD_SEARCH_PRIVATE_IMPORTED
		                                                : METHOD_SEARCH_IMPORTED);
		if (!new_found) continue;
		if (found)
		{
			*ambiguous_ref = new_found;
			return found;
		}
		found = new_found;
		if (ambiguous)
		{
			*ambiguous_ref = ambiguous;
			return found;
		}
	}
	if (private) *private_ref = private;
	return found;
}

Decl *unit_resolve_parameterized_symbol(CompilationUnit *unit, NameResolve *name_resolve)
{
	name_resolve->ambiguous_other_decl = NULL;
	name_resolve->private_decl = NULL;
	name_resolve->path_found = false;

	Decl *decl = sema_find_decl_in_imports(unit->imports, name_resolve, true);
	if (!decl)
	{
		decl = sema_find_decl_in_global_new(unit, &global_context.generic_symbols,
		                                    global_context.generic_module_list,
		                                    name_resolve, true);
	}
	// 14. Error report
	if (!decl || name_resolve->ambiguous_other_decl)
	{
		if (name_resolve->suppress_error) return poisoned_decl;
		sema_report_error_on_decl(decl, name_resolve);
		return poisoned_decl;
	}
	if (!decl_is_user_defined_type(decl) && !name_resolve->path)
	{
		if (name_resolve->suppress_error) return poisoned_decl;
		sema_error_at(name_resolve->span, "Function and variables must be prefixed with a path, e.g. 'foo::%s'.", name_resolve->symbol);
		return poisoned_decl;
	}

	return decl;
}


Decl *sema_find_symbol(SemaContext *context, const char *symbol)
{
	NameResolve resolve = {
			.suppress_error = true,
			.symbol = symbol,
	};
	return sema_resolve_symbol_common(context, &resolve);
}

Decl *sema_find_label_symbol(SemaContext *context, const char *symbol)
{
	Decl **locals = context->locals;
	if (!locals || !context->active_scope.current_local) return NULL;
	int64_t first = context->active_scope.label_start;
	int64_t current = context->active_scope.current_local - 1;
	while (current >= first)
	{
		Decl *cur = locals[current--];
		if (cur->name == symbol) return cur;
	}
	return NULL;
}

Decl *sema_find_label_symbol_anywhere(SemaContext *context, const char *symbol)
{
	Decl **locals = context->locals;
	if (!locals || !context->active_scope.current_local) return NULL;
	int64_t first = 0;
	int64_t current = context->active_scope.current_local - 1;
	while (current >= first)
	{
		Decl *cur = locals[current--];
		if (cur->name == symbol) return cur;
	}
	return NULL;
}

bool sema_symbol_is_defined_in_scope(SemaContext *c, const char *symbol)
{
	NameResolve resolve = {
			.suppress_error = true,
			.symbol = symbol,
	};
	Decl *decl = sema_resolve_symbol_common(c, &resolve);
	// Unknown symbol => not defined
	if (!decl) return false;
	// Defined in the same module => defined
	if (decl->unit->module == c->unit->module) return true;
	// Not a variable or function => defined
	if (decl->decl_kind != DECL_VAR && decl->decl_kind != DECL_FUNC) return true;
	// Otherwise defined only if autoimport.
	return decl->is_autoimport;
}

Decl *sema_find_path_symbol(SemaContext *context, const char *symbol, Path *path)
{
	NameResolve resolve = {
			.suppress_error = true,
			.symbol = symbol,
			.path = path
	};
	return sema_resolve_symbol_common(context, &resolve);
}

Decl *sema_resolve_symbol(SemaContext *context, const char *symbol, Path *path, SourceSpan span)
{
	NameResolve resolve = {
			.symbol = symbol,
			.path = path,
			.span = span
	};
	return sema_resolve_symbol_common(context, &resolve);
}


static inline void sema_append_local(SemaContext *context, Decl *decl)
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
}

bool sema_add_local(SemaContext *context, Decl *decl)
{
	CompilationUnit *current_unit = decl->unit = context->unit;

	// Ignore synthetic locals.
	if (!decl->name) return true;
	if (decl->decl_kind == DECL_VAR && decl->var.shadow) goto ADD_VAR;

	Decl *other = sema_find_local(context, decl->name);
	assert(!other || other->unit->module);
	if (other && (other->unit->module == current_unit->module || other->is_autoimport))
	{
		sema_shadow_error(decl, other);
		decl_poison(decl);
		decl_poison(other);
		return false;
	}
ADD_VAR:
	decl->resolve_status = RESOLVE_DONE;
	sema_append_local(context, decl);
	return true;
}

void sema_unwrap_var(SemaContext *context, Decl *decl)
{
	Decl *alias = decl_copy(decl);
	alias->var.kind = VARDECL_UNWRAPPED;
	alias->var.alias = decl;
	alias->type = alias->type->failable;
	alias->resolve_status = RESOLVE_DONE;
	sema_append_local(context, alias);
}

void sema_rewrap_var(SemaContext *context, Decl *decl)
{
	assert(decl->decl_kind == DECL_VAR && decl->var.kind == VARDECL_UNWRAPPED && decl->var.alias->type->type_kind == TYPE_FAILABLE);
	sema_append_local(context, decl->var.alias);
}

void sema_erase_var(SemaContext *context, Decl *decl)
{
	Decl *erased = decl_copy(decl);
	erased->var.kind = VARDECL_ERASE;
	erased->resolve_status = RESOLVE_DONE;
	sema_append_local(context, erased);
}


void sema_erase_unwrapped(SemaContext *context, Decl *decl)
{
	assert(IS_OPTIONAL(decl));
	Decl *rewrapped = decl_copy(decl);
	rewrapped->var.kind = VARDECL_REWRAPPED;
	rewrapped->var.alias = decl;
	rewrapped->type = decl->type;
	rewrapped->resolve_status = RESOLVE_DONE;
	sema_append_local(context, rewrapped);
}
