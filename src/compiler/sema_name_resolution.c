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

	// We also want to make sure that the preceding 2 characters are ::
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
		Type *type = decl->distinct->type->canonical;
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
	if (decl->decl_kind == DECL_INTERFACE)
	{
		FOREACH_BEGIN(TypeInfo *parent_interface, decl->interfaces)
			FOREACH_BEGIN(Decl *interface, parent_interface->type->decl->interface_methods)
				sema_decl_stack_push(interface);
			FOREACH_END();
		FOREACH_END();
		FOREACH_BEGIN(Decl *interface, decl->interface_methods)
			sema_decl_stack_push(interface);
		FOREACH_END();
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

static inline Decl *sema_find_decl_in_module(Module *module, Path *path, const char *symbol, bool *path_found)
{
	if (!path) return module_find_symbol(module, symbol);
	if (path->len > module->name->len) return NULL;
	if (!matches_subpath(module->name, path)) return NULL;
	*path_found = true;
	return module_find_symbol(module, symbol);
}

static Decl *sema_find_decl_in_private_imports(Decl **imports, NameResolve *name_resolve, bool want_generic)
{
	Decl *decl = NULL;
	// 1. Loop over imports.
	Path *path = name_resolve->path;
	const char *symbol = name_resolve->symbol;
	VECEACH(imports, i)
	{
		Decl *import = imports[i];
		if (import->import.module->is_generic != want_generic) continue;
		if (!import->import.import_private_as_public) continue;
		// Is the decl in the import.
		Decl *found = sema_find_decl_in_module(import->import.module, path, symbol, &name_resolve->path_found);

		// No match, so continue
		if (!found) continue;

		assert(found->visibility != VISIBLE_LOCAL);

		// Did we already have a match?
		if (decl)
		{
			if (!path)
			{
				// Prefer already found builtin over newly found non-builtin
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

INLINE bool module_inclusion_match(Module *a, Module *b)
{
	Module *temp;
	while ((temp = a->generic_module)) a = temp;
	while ((temp = b->generic_module)) b = temp;

	// Quick check
	if (a->top_module != b->top_module) return false;
	if (a->name->len < b->name->len)
	{
		temp = a;
		a = b;
		b = temp;
	}
	while (a->name->len > b->name->len) a = a->parent_module;
	return a == b;
}

static bool decl_is_visible(CompilationUnit *unit, Decl *decl)
{
	Module *module = decl->unit->module;
	// 1. Same module as unit -> ok
	if (module == unit->module) return true;

	// 2. Module inclusion: a is submodule of b or b of a.
	if (module_inclusion_match(module, unit->module)) return true;

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
		if (import_module == module) return true;
		if (module_inclusion_match(import_module, module)) return true;
	}
	return false;
}

static bool sema_first_is_preferred(Decl *decl, Decl *decl2)
{
	return (decl->is_autoimport && !decl2->is_autoimport)
		|| (decl2->unit->module->generic_module && !decl->unit->module->generic_module);
}

static Decl *sema_find_decl_in_global(CompilationUnit *unit, DeclTable *table, Module **module_list, NameResolve *name_resolve, bool want_generic)
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
		if (path && !matches_subpath(decl_module(decls)->name, path)) return NULL;
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
		if (path && !matches_subpath(decl_module(candidate)->name, path)) continue;
		if (!decl_is_visible(unit, candidate))
		{
			maybe_decl = candidate;
			continue;
		}
		if (ambiguous)
		{
			if (sema_first_is_preferred(candidate, decl))
			{
				ambiguous = NULL;
				decl = candidate;
			}
		}
		else
		{
			ambiguous = decl;
			decl = candidate;
			if (ambiguous)
			{
				// If we have a same match but one is builtin, prefer builtin.
				if (sema_first_is_preferred(decl, ambiguous))
				{
					ambiguous = NULL;
				}
				else if (sema_first_is_preferred(ambiguous, decl))
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
		name_resolve->path_found = true;
		return module_find_symbol(&global_context.std_module, symbol);
	}

	CompilationUnit *unit = context->unit;

	// 1. Do we match our own path?
	if (matches_subpath(unit->module->name, path))
	{
		// 2. If so try to locally get the symbol.
		if ((decl = module_find_symbol(unit->module, symbol))) return decl;
		name_resolve->path_found = true;
	}

	// 3. Loop over imports.
	decl = sema_find_decl_in_private_imports(unit->imports, name_resolve, false);

	// 4. Go to global search
	return decl ? decl : sema_find_decl_in_global(unit, &global_context.symbols, global_context.module_list, name_resolve, false);
}

static inline Decl *sema_find_ct_local(SemaContext *context, const char *symbol)
{
	Decl **locals = context->ct_locals;
	FOREACH_BEGIN(Decl *cur, locals)
		if (cur->name == symbol) return cur;
	FOREACH_END();
	return NULL;
}

static inline Decl *sema_find_local(SemaContext *context, const char *symbol)
{
	if (symbol[0] == '$') return sema_find_ct_local(context, symbol);
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
	const char *symbol = name_resolve->symbol;
	assert(name_resolve->path == NULL);

	Decl *decl = sema_find_local(context, symbol);
	if (decl) return decl;

	CompilationUnit *unit = context->unit;

	// Search in file scope.
	decl = htable_get(&unit->local_symbols, (void *) symbol);

	if (decl) return decl;

	// Search in the module.
	decl = module_find_symbol(unit->module, symbol);

	if (decl) return decl;

	decl = sema_find_decl_in_private_imports(unit->imports, name_resolve, false);

	return decl ? decl : sema_find_decl_in_global(context->unit, &global_context.symbols, NULL, name_resolve, false);
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
		const char *module_name = decl_module(name_resolve->maybe_decl)->name->module;
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
		const char *found_path = decl_module(found)->name->module;
		const char *other_path = decl_module(name_resolve->ambiguous_other_decl)->name->module;
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
			if (decl_needs_prefix(found))
			{
				sema_error_at(span, "The %s needs a path prefix (e.g. '%s::%s').", symbol_type, found_path, symbol);
				return;
			}
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
			if (name_resolve->suppress_error) return NULL;
			bool path_found = false;
			Module *module_with_path = NULL;
			FOREACH_BEGIN(Module *module, global_context.module_list)
				if (matches_subpath(module->name, name_resolve->path))
				{
					module_with_path = module;
					break;
				}
			FOREACH_END();
			if (!module_with_path)
			{
				FOREACH_BEGIN(Module *module, global_context.generic_module_list)
					if (matches_subpath(module->name, name_resolve->path))
					{
						module_with_path = module;
						break;
					}
				FOREACH_END();
			}
			if (module_with_path)
			{
				sema_error_at(name_resolve->span, "'%s' could not be found in %s.", name_resolve->symbol, module_with_path->name->module);
			}
			else
			{
				SEMA_ERROR(name_resolve->path, "Unknown module '%.*s', did you type it right?", name_resolve->path->len, name_resolve->path->module);
			}
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

Decl *sema_find_extension_method_in_list(Decl **extensions, Type *type, const char *method_name)
{
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
	Decl *found = sema_find_extension_method_in_list(module->private_method_extensions, actual_type, method_name);
	// The found one might not be visible
	if (found && search_type < METHOD_SEARCH_CURRENT && found->visibility == VISIBLE_PRIVATE)
	{
		*private_found = found;
		found = NULL;
	}
	assert(!found || found->visibility != VISIBLE_LOCAL);
	if (found && search_type == METHOD_SEARCH_CURRENT) return found;
	// We are now searching submodules, so hide the private ones.
	if (search_type == METHOD_SEARCH_CURRENT) search_type = METHOD_SEARCH_SUBMODULE_CURRENT;
	FOREACH_BEGIN(Module *mod, module->sub_modules)
		Decl *new_found = sema_resolve_method_in_module(mod, actual_type, method_name, private_found, ambiguous, search_type);
		if (!new_found) continue;
		if (found)
		{
			*ambiguous = new_found;
			return found;
		}
		found = new_found;
	FOREACH_END();
	// We might have it ambiguous due to searching sub modules.
	return found;
}

Decl *sema_resolve_method(CompilationUnit *unit, Decl *type, const char *method_name, Decl **ambiguous_ref, Decl **private_ref)
{
	// Interface, prefer interface methods.
	if (type->decl_kind == DECL_INTERFACE)
	{
		FOREACH_BEGIN(Decl *method, type->interface_methods)
			if (method_name == method->name) return method;
		FOREACH_END();
	}
	// Look through natively defined methods.
	FOREACH_BEGIN(Decl *method, type->methods)
		if (method_name == method->name)
		{
			return method;
		}
	FOREACH_END();

	return sema_resolve_type_method(unit, type->type, method_name, ambiguous_ref, private_ref);
}

bool sema_check_type_variable_array(SemaContext *context, TypeInfo *type_info)
{
	if (!type_info_ok(type_info)) return false;
	Type *type = type_info->type;

	while (1)
	{
		type = type_flatten(type);
		switch (type->type_kind)
		{
			case TYPE_POINTER:
				type = type->pointer;
				continue;
			case TYPE_SLICE:
			case TYPE_ARRAY:
			case TYPE_FLEXIBLE_ARRAY:
			case TYPE_INFERRED_ARRAY:
				type = type->array.base;
				continue;
			case TYPE_STRUCT:
				break;
			default:
				UNREACHABLE;
		}
		break;
	}
	assert(type->type_kind == TYPE_STRUCT);
	if (type->decl->has_variable_array)
	{
		SEMA_ERROR(type_info, "Arrays of structs with flexible array members is not allowed.");
		return type_info_poison(type_info);
	}
	return true;
}

bool sema_resolve_type_decl(SemaContext *context, Type *type)
{
	switch (type->type_kind)
	{
		case TYPE_POISONED:
			return false;
		case TYPE_WILDCARD:
		case TYPE_VOID:
		case TYPE_BOOL:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_ANYFAULT:
		case TYPE_TYPEID:
		case TYPE_POINTER:
		case TYPE_UNTYPED_LIST:
		case TYPE_MEMBER:
		case TYPE_INFERRED_VECTOR:
		case TYPE_VECTOR:
		case TYPE_SLICE:
		case TYPE_ANY:
		case TYPE_INTERFACE:
			return true;
		case TYPE_OPTIONAL:
			return sema_resolve_type_decl(context, type->optional);
		case TYPE_TYPEINFO:
			UNREACHABLE
		case TYPE_TYPEDEF:
			return sema_resolve_type_decl(context, type->canonical);
		case TYPE_DISTINCT:
			if (!sema_analyse_decl(context, type->decl)) return false;
			return sema_resolve_type_decl(context, type->decl->distinct->type);
		case TYPE_FUNC:
			if (!type->function.prototype && type->function.decl->decl_kind == DECL_FNTYPE) return sema_analyse_decl(context, type->function.decl);
			return true;
		case TYPE_ENUM:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_BITSTRUCT:
		case TYPE_FAULTTYPE:
			return sema_analyse_decl(context, type->decl);
		case TYPE_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_INFERRED_ARRAY:
			return sema_resolve_type_decl(context, type->array.base);
	}
	UNREACHABLE
}

Decl *sema_resolve_type_method(CompilationUnit *unit, Type *type, const char *method_name, Decl **ambiguous_ref, Decl **private_ref)
{
	assert(type == type->canonical);
	Decl *private = NULL;
	Decl *ambiguous = NULL;
	Decl *found = sema_find_extension_method_in_list(unit->local_method_extensions, type, method_name);
	if (!found) found = sema_resolve_method_in_module(unit->module, type, method_name, &private, &ambiguous, METHOD_SEARCH_CURRENT);
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

		Decl *new_found = sema_resolve_method_in_module(import->import.module, type, method_name,
														&private, &ambiguous,
														import->import.import_private_as_public
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
	if (!found)
	{
		found = sema_resolve_method_in_module(global_context.core_module, type, method_name,
											  &private, &ambiguous, METHOD_SEARCH_IMPORTED);
	}
	if (found && ambiguous)
	{
		*ambiguous_ref = ambiguous;
		return found;
	}
	if (!found)
	{
		found = sema_find_extension_method_in_list(global_context.method_extensions, type, method_name);
		private = NULL;
	}
	if (private) *private_ref = private;
	if (!found)
	{
		if (type->type_kind == TYPE_ARRAY)
		{
			Type *inferred_array = type_get_inferred_array(type->array.base);
			found = sema_resolve_type_method(unit, inferred_array, method_name, ambiguous_ref, private_ref);
			if (found) *private_ref = NULL;
		}
		else if (type->type_kind == TYPE_VECTOR)
		{
			Type *inferred_vector = type_get_inferred_vector(type->array.base);
			found = sema_resolve_type_method(unit, inferred_vector, method_name, ambiguous_ref, private_ref);
			if (found) *private_ref = NULL;
		}
	}
	return found;
}

Decl *unit_resolve_parameterized_symbol(CompilationUnit *unit, NameResolve *name_resolve)
{
	name_resolve->ambiguous_other_decl = NULL;
	name_resolve->private_decl = NULL;
	name_resolve->path_found = false;

	Decl *decl = sema_find_decl_in_private_imports(unit->imports, name_resolve, true);

	if (!decl)
	{
		decl = sema_find_decl_in_global(unit, &global_context.generic_symbols,
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
	if (decl_module(decl) == c->unit->module) return true;
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
	assert(!decl_is_ct_var(decl));
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

INLINE bool sema_add_ct_local(SemaContext *context, Decl *decl)
{
	assert(decl_is_ct_var(decl));

	Decl *other = sema_find_ct_local(context, decl->name);
	if (other)
	{
		sema_shadow_error(decl, other);
		decl_poison(decl);
		decl_poison(other);
		return false;
	}
	decl->resolve_status = RESOLVE_DONE;
	vec_add(context->ct_locals, decl);
	return true;
}

bool sema_add_local(SemaContext *context, Decl *decl)
{
	CompilationUnit *current_unit = decl->unit = context->unit;

	// Ignore synthetic locals.
	if (!decl->name) return true;
	bool is_var = decl->decl_kind == DECL_VAR;
	if (is_var && decl_var_kind_is_ct(decl->var.kind)) return sema_add_ct_local(context, decl);
	if (is_var && decl->var.shadow) goto ADD_VAR;

	Decl *other = sema_find_local(context, decl->name);
	assert(!other || decl_module(other));
	if (other && (decl_module(other) == current_unit->module || other->is_autoimport))
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
	alias->type = alias->type->optional;
	alias->resolve_status = RESOLVE_DONE;
	sema_append_local(context, alias);
}

void sema_rewrap_var(SemaContext *context, Decl *decl)
{
	assert(decl->decl_kind == DECL_VAR && decl->var.kind == VARDECL_UNWRAPPED && decl->var.alias->type->type_kind == TYPE_OPTIONAL);
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
