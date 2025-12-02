// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"

#if defined(_MSC_VER)
// This isn't standard apparently, so MSVC doesn't have it built in...
typedef long long int ssize_t;
#endif

INLINE bool sema_resolve_ambiguity(SemaContext *context, Decl **current, Decl *candidate, Decl **ambiguous);
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
	Decl **current = compiler.context.decl_stack_top;
	Decl **end = compiler.context.decl_stack_bottom;
	while (current > end)
	{
		Decl *decl = *(--current);
		if (decl->name == symbol) return decl;
	}
	return NULL;
}

Decl **sema_decl_stack_store(void)
{
	Decl **current_bottom = compiler.context.decl_stack_bottom;
	compiler.context.decl_stack_bottom = compiler.context.decl_stack_top;
	return current_bottom;
}

void sema_decl_stack_restore(Decl **state)
{
	compiler.context.decl_stack_top = compiler.context.decl_stack_bottom;
	compiler.context.decl_stack_bottom = state;
}

void sema_decl_stack_push(Decl *decl)
{
	Decl **current = compiler.context.decl_stack_top;
	if (current == &compiler.context.decl_stack[MAX_GLOBAL_DECL_STACK])
	{
		error_exit("Declaration stack exhausted.");
	}
	*(current++) = decl;
	compiler.context.decl_stack_top = current;
}

static bool add_interface_to_decl_stack(SemaContext *context, Decl *decl)
{
	if (!sema_analyse_decl(context, decl)) return false;
	FOREACH(TypeInfo *, parent_interface, decl->interfaces)
	{
		ASSERT(parent_interface->resolve_status == RESOLVE_DONE);
		Decl *inf = parent_interface->type->decl;
		if (!sema_analyse_decl(context, inf)) return false;
		add_interface_to_decl_stack(context, inf);
	}
	FOREACH(Decl *, interface, decl->interface_methods) sema_decl_stack_push(interface);
	return true;
}

static bool add_members_to_decl_stack(SemaContext *context, Decl *decl, FindMember find)
{
	if (find != FIELDS_ONLY) sema_add_methods_to_decl_stack(context, decl);
	while (decl->decl_kind == DECL_TYPEDEF)
	{
		Type *type = decl->distinct->type->canonical;
		if (!type_is_user_defined(type)) break;
		decl = type->decl;
	}
	if (decl->decl_kind == DECL_ENUM)
	{
		FOREACH(Decl *, member, decl->enums.parameters) sema_decl_stack_push(member);
	}
	if (decl->decl_kind == DECL_INTERFACE && find != FIELDS_ONLY)
	{
		if (!add_interface_to_decl_stack(context, decl)) return false;
	}
	if (decl_is_struct_type(decl) || decl->decl_kind == DECL_BITSTRUCT)
	{
		FOREACH(Decl *, member, decl->strukt.members)
		{
			if (member->name == NULL)
			{
				if (!add_members_to_decl_stack(context, member, find)) return false;
				continue;
			}
			sema_decl_stack_push(member);
		}
	}
	return true;
}

Decl *sema_decl_stack_find_decl_member(SemaContext *context, Decl *decl_owner, const char *symbol, FindMember find)
{
	Decl **state = sema_decl_stack_store();
	if (!add_members_to_decl_stack(context, decl_owner, find)) return poisoned_decl;
	Decl *member = sema_decl_stack_resolve_symbol(symbol);
	sema_decl_stack_restore(state);
	if (member || find == FIELDS_ONLY) return member;
	if (find == METHODS_AND_FIELDS)
	{
		return sema_resolve_method_only(decl_owner, symbol);
	}
	return sema_resolve_method(decl_owner, symbol);
}

static inline Decl *sema_find_decl_in_module(Module *module, Path *path, const char *symbol, Module **path_found_ref)
{
	if (!path) return module_find_symbol(module, symbol);
	if (path->len > module->name->len) return NULL;
	if (!matches_subpath(module->name, path)) return NULL;
	*path_found_ref = module;
	return module_find_symbol(module, symbol);
}

static bool sema_find_decl_in_imports(SemaContext *context, NameResolve *name_resolve)
{
	Decl *decl = NULL;
	// 1. Loop over imports.
	Path *path = name_resolve->path;
	const char *symbol = name_resolve->symbol;

	FOREACH(Decl *, import, context->unit->imports)
	{
		bool is_private_import = import->import.import_private_as_public;
		if (!path && (decl || !is_private_import)) continue;
		// Is the decl in the import.
		Decl *found = sema_find_decl_in_module(import->import.module, path, symbol, &name_resolve->path_found);

		if (!decl_ok(found)) return false;

		// No match, so continue
		if (!found) continue;
		ASSERT(found->visibility != VISIBLE_LOCAL);

		if (found->visibility != VISIBLE_PUBLIC)
		{
			if (decl) continue;
			if (!is_private_import)
			{
				name_resolve->private_decl = found;
				continue;
			}
		}

		// Did we already have a match?
		if (decl)
		{
			if (!path)
			{
				if (!sema_resolve_ambiguity(context, &decl, found, &name_resolve->ambiguous_other_decl)) return false;
				continue;
			}
			// 11. Then set an ambiguous match.
			name_resolve->ambiguous_other_decl = found;
			continue;
		}

		// We've found a match.
		decl = found;
		name_resolve->private_decl = NULL;
	}
	name_resolve->found = decl;
	return true;
}

static inline Module *sema_is_path_found(Module **modules, Path *path)
{
	FOREACH(Module *, module, modules)
	{
		if (matches_subpath(module->name, path)) return module;
	}
	return NULL;
}

Decl *sema_find_decl_in_modules(Module **module_list, Path *path, const char *interned_name)
{
	Module *path_found = NULL;
	FOREACH(Module *, module, module_list)
	{
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

	// This never matches a generic module.
	if (module->generic_module) return false;

	// 2. Skip to imports for private decls
	bool is_public = decl->visibility == VISIBLE_PUBLIC;
	if (!is_public) goto IMPORT_CHECK;

	// 3. Module inclusion: a is submodule of b or b of a.

	if (module_inclusion_match(module, unit->module)) return true;

	// 4. We want to check std::core
	Module *lookup = module;
	while (lookup)
	{
		if (lookup->name->module == kw_std__core) return true;
		lookup = lookup->parent_module;
	}
IMPORT_CHECK:;
	FOREACH(Decl *, import, unit->imports)
	{
		if (!is_public && !import->import.import_private_as_public) continue;
		Module *import_module = import->import.module;
		if (import_module == module) return true;
		if (import->import.is_non_recurse) continue;
		if (module_inclusion_match(import_module, module)) return true;
	}
	return false;
}

INLINE Type *sema_fold_weak(SemaContext *context, Decl *decl)
{
	while (decl->is_weak)
	{
		if (decl->resolve_status != RESOLVE_DONE)
		{
			if (!sema_analyse_decl(context, decl)) return NULL;
		}
		Type *type = decl->type_alias_decl.type_info->type;
		if (type->type_kind != TYPE_ALIAS) return type;
		decl = type->decl;
	}
	return decl->type;
}

/**
 * We want to prefer abc::Foo over bcd::Foo if:
 * (1) abc::Foo is autoimported and bcd::Foo isn't.
 * (2) abc::Foo is from a normal module and bcd::Foo is from a generic module.
 * (3) Folding bcd::Foo to it's @weak result gives the same as folding abc::Foo to its @weak type.
 *
 * @param context
 * @param decl
 * @param decl2
 * @return
 */
static BoolErr sema_first_is_preferred(SemaContext *context, Decl *decl, Decl *decl2)
{
	// (1) and (2)
	if ((decl->is_autoimport && !decl2->is_autoimport)
		|| (decl2->unit->module->generic_module && !decl->unit->module->generic_module)) return BOOL_TRUE;
	// Now analyse common parents, we only check if this is a redef.
	if (decl2->decl_kind != DECL_TYPE_ALIAS || !decl2->is_weak) return BOOL_FALSE;

	Type *weak2 = sema_fold_weak(context, decl2);
	if (!weak2) return BOOL_ERR;

	// Fast path: do the types match?
	if (weak2 == decl->type) return BOOL_TRUE;

	// If we can't fold the decl then we're done.
	if (decl->decl_kind != DECL_TYPE_ALIAS || !decl->is_weak) return BOOL_FALSE;

	Type *weak = sema_fold_weak(context, decl);
	if (!weak) return BOOL_ERR;

	// Both fold to the same
	if (weak == weak2) return BOOL_TRUE;

	// Otherwise we fail.
	return BOOL_FALSE;
}

INLINE bool sema_resolve_ambiguity(SemaContext *context, Decl **current, Decl *candidate, Decl **ambiguous)
{
	Decl *original = *current;
	if (!original)
	{
		*current = candidate;
		return true;
	}
	// The candidate is preferred
	BoolErr preferred = sema_first_is_preferred(context, candidate, original);
	if (preferred == BOOL_ERR) return false;
	if (preferred == BOOL_TRUE)
	{
		// Clear any ambiguity
		*ambiguous = NULL;
		*current = candidate;
		return true;
	}
	// We already have something ambiguous, so keep that.
	if (*ambiguous) return true;
	// If the original is preferred over the candidate, then we just
	// keep the original and there is no ambiguity:
	switch (sema_first_is_preferred(context, original, candidate))
	{
		case BOOL_FALSE:
			// Otherwise we have an ambiguity
			*ambiguous = candidate;
			FALLTHROUGH;
		case BOOL_TRUE:
			return true;
		case BOOL_ERR:
			return false;
	}
	UNREACHABLE
}

static Decl *sema_find_decl_by_short_path(Path *path, const char *name)
{
	return pathtable_get(&compiler.context.path_symbols, (void*)path->module, (void*)name);
}

static bool sema_find_decl_in_global(SemaContext *context, DeclTable *table, Module **module_list, NameResolve *name_resolve)
{
	const char *symbol = name_resolve->symbol;
	Path *path = name_resolve->path;
	DeclId decl_ids = decltable_get(table, symbol);
	Decl *maybe_decl = NULL;
	// We might have no match at all.
	if (!decl_ids)
	{
		// Update the path found
		if (path && !name_resolve->path_found) name_resolve->path_found = sema_is_path_found(module_list, path);
		name_resolve->found = NULL;
		return true;
	}

	Decl *decls = declptr(decl_ids);
	// There might just be a single match.
	if (decls->decl_kind != DECL_DECLARRAY)
	{
		if (path && !matches_subpath(decls->unit->module->name, path)) return true;
		if (!decl_is_visible(context->unit, decls))
		{
			if (decls->visibility == VISIBLE_PRIVATE)
			{
				name_resolve->private_decl = decls;
			}
			else
			{
				name_resolve->maybe_decl = decls;
			}
		}
		else
		{
			name_resolve->private_decl = NULL;
			name_resolve->found = decls;
		}
		return true;
	}

	// Else go through the list
	Decl *ambiguous = NULL;
	Decl *decl = NULL;
	FOREACH(Decl *, candidate, decls->decl_list)
	{
		if (path && !matches_subpath(candidate->unit->module->name, path)) continue;
		if (!decl_is_visible(context->unit, candidate))
		{
			maybe_decl = candidate;
			continue;
		}
		if (!sema_resolve_ambiguity(context, &decl, candidate, &ambiguous)) return false;
	}
	name_resolve->ambiguous_other_decl = ambiguous;
	name_resolve->found = decl;
	if (maybe_decl && maybe_decl->visibility == VISIBLE_PRIVATE)
	{
		name_resolve->private_decl = maybe_decl;
		name_resolve->maybe_decl = NULL;
	}
	else
	{
		name_resolve->private_decl = NULL;
		name_resolve->maybe_decl = maybe_decl;
	}
	return true;
}

static bool sema_resolve_path_symbol(SemaContext *context, NameResolve *name_resolve)
{
	Path *path = name_resolve->path;
	ASSERT(path && "Expected path.");
	name_resolve->ambiguous_other_decl = NULL;
	name_resolve->path_found = NULL;
	name_resolve->found = NULL;

	FOREACH(Decl *, decl_alias, context->unit->module_aliases)
	{
		if (path->module == decl_alias->name)
		{
			assert(decl_alias->resolve_status == RESOLVE_DONE);
			name_resolve->path_found = decl_alias->module_alias_decl.module;
			name_resolve->path = name_resolve->path_found->name;
			break;
		}
	}

	const char *symbol = name_resolve->symbol;
	// 0. std module special handling.
	if (path->module == compiler.context.std_module_path.module)
	{
		name_resolve->path_found = &compiler.context.std_module;
		name_resolve->found = module_find_symbol(&compiler.context.std_module, symbol);
		return true;
	}

	CompilationUnit *unit = context->unit;

	// 1. Do we match our own path?
	if (matches_subpath(unit->module->name, path))
	{
		// 2. If so try to locally get the symbol.
		if ((name_resolve->found = module_find_symbol(unit->module, symbol))) return true;
		name_resolve->path_found = unit->module;
	}

	Decl *decl = sema_find_decl_by_short_path(name_resolve->path, symbol);
	if (decl && decl_ok(decl) && decl_is_visible(context->unit, decl))
	{
		name_resolve->found = decl;
		return true;
	}
	// 3. Loop over imports.
	if (!sema_find_decl_in_imports(context, name_resolve)) return false;

	// 4. Go to global search
	if (name_resolve->found) return true;
	return sema_find_decl_in_global(context, &compiler.context.symbols, compiler.context.module_list, name_resolve);
}

static inline Decl *sema_find_ct_local(SemaContext *context, const char *symbol)
{
	Decl **locals = context->ct_locals;
	FOREACH(Decl *, cur, locals)
	{
		if (cur->name == symbol) return cur;
	}
	return NULL;
}

Decl *sema_find_local(SemaContext *context, const char *symbol)
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

static bool sema_resolve_no_path_symbol(SemaContext *context, NameResolve *name_resolve)
{
	const char *symbol = name_resolve->symbol;
	ASSERT(name_resolve->path == NULL);

	Decl *decl;

	if ((decl = sema_find_local(context, symbol)))
	{
		name_resolve->found = decl;
		return true;
	}

	CompilationUnit *unit = context->unit;

	// Search in file scope.
	if ((decl = htable_get(&unit->local_symbols, (void *) symbol)))
	{
		name_resolve->found = decl;
		return true;
	}

	// Search in the module.
	if ((decl = module_find_symbol(unit->module, symbol)))
	{
		name_resolve->found = decl;
		return true;
	}

	if (!sema_find_decl_in_imports(context, name_resolve)) return false;
	if (name_resolve->found) return true;

	return sema_find_decl_in_global(context, &compiler.context.symbols, NULL, name_resolve);
}

#define MAX_TEST 256

int damerau_levenshtein_distance(const char *a, int a_len, const char *b, int b_len)
{
	if (!a_len) return b_len;
	if (!b_len) return a_len;
	if (a_len >= MAX_TEST || b_len >= MAX_TEST) return MAX_TEST;
	int score[MAX_TEST][MAX_TEST];
	memset(score, 0, (size_t)MAX_TEST *  (size_t)MAX_TEST);
	for (int i = 0; i <= a_len; i++) score[i][0] = i;
	for (int i = 0; i <= b_len; i++) score[0][i] = i;
	for (int i = 0; i < a_len; i++)
	{
		for (int j = 0; j < b_len; j++)
		{
			int cost = a[i] == b[i] ? 0 : 1;
			int del = score[i][j + 1] + 1;
			int insert = score[i + 1][j] + 1;
			int substitute = score[i][j] + cost;
			int min = del < insert ? del : insert;
			score[i + 1][j + 1] = min < substitute ? min : substitute;
			if (i > 0 && j > 0 && a[i] == b[j - 1] && a[i - 1] == b[j])
			{
				int comp = score[i - 1][j - 1] + 1;
				if (comp < score[i + 1][j + 1]) score[i + 1][j + 1] = comp;
			}
		}
	}
	return score[a_len][b_len];
}


static void find_closest(const char *name, int name_len, Decl **decls, int *count_ref, Decl* matches[3], int *best_distance_ref)
{
	int best_distance = *best_distance_ref;
	int count = *count_ref;
	bool starts_at = name[0] == '@';
	Decl *at_match = NULL;
	FOREACH(Decl *, decl, decls)
	{
		if (decl->visibility != VISIBLE_PUBLIC) continue;
		const char *decl_name = decl->name;
		if (!starts_at && decl_name[0] == '@' && str_eq(&decl_name[1], name))
		{
			at_match = decl;
			continue;
		}
		int dist = damerau_levenshtein_distance(name, name_len, decl_name, (int)strlen(decl_name));
		if (dist < best_distance)
		{
			matches[0] = decl;
			best_distance = dist;
			count = 1;
			continue;
		}
		if (dist == best_distance && count < 3)
		{
			matches[count++] = decl;
		}
	}
	if (at_match)
	{
		if (count == 3)
		{
			matches[0] = at_match;
		}
		else
		{
			matches[count++] = at_match;
		}
	}
	*count_ref = count;
	*best_distance_ref = best_distance;
}
static int module_closest_ident_names(Module *module, const char *name, Decl* matches[3])
{
	matches[0] = matches[1] = matches[2] = NULL;

	int count = 0;
	int len = (int)strlen(name);
	int distance = MAX(1, (int)(len * 0.8));
	FOREACH(CompilationUnit *, unit, module->units)
	{
		find_closest(name, len, unit->functions, &count, matches, &distance);
		find_closest(name, len, unit->macros, &count, matches, &distance);
		find_closest(name, len, unit->vars, &count, matches, &distance);
	}
	return count;
}
static void sema_report_error_on_decl(SemaContext *context, NameResolve *name_resolve)
{
	ASSERT(!name_resolve->suppress_error);
	const char *symbol = name_resolve->symbol;
	SourceSpan span = name_resolve->span;
	Decl *found = name_resolve->found;
	const char *path_name = name_resolve->path ? name_resolve->path->module : NULL;
	if (!found && name_resolve->private_decl)
	{
		const char *private_name = decl_to_name(name_resolve->private_decl);
		if (path_name)
		{
			sema_error_at(context, span, "The %s '%s::%s' is '@private' and not visible from other modules.",
			              private_name, path_name,
			              symbol);
			return;
		}
		sema_error_at(context, span, "The %s '%s' is '@private' and not visible from other modules.",
		              private_name, symbol);
		return;
	}
	if (!found && name_resolve->maybe_decl)
	{
		Decl *decl = name_resolve->maybe_decl;
		Module *module = decl->unit->module;
		const char *maybe_name = decl_to_name(decl);
		Module *generic_module = module->generic_module;
		if (!generic_module && module->is_generic) generic_module = module;
		const char *module_name = generic_module ? generic_module->name->module : module->name->module;
		if (generic_module && !name_resolve->is_parameterized)
		{
			sema_error_at(context, span, "Did you mean the %s '%s' in the generic module %s? If so, use '%s{...}' instead.",
			              maybe_name, symbol, module_name, symbol);
			return;
		}
		if (path_name)
		{
			sema_error_at(context, span, "Did you mean the %s '%s::%s' in module %s? If so please add 'import %s'.",
			              maybe_name, module_name, symbol, module_name, module_name);
			return;
		}
		sema_error_at(context, span, "Did you mean the %s '%s' in module %s? If so please add 'import %s'.",
		              maybe_name, symbol, module_name, module_name);
		return;
	}

	if (name_resolve->ambiguous_other_decl)
	{
		ASSERT(found);
		const char *symbol_type = decl_to_name(found);
		const char *found_path = found->unit->module->name->module;
		const char *other_path = name_resolve->ambiguous_other_decl->unit->module->name->module;
		if (path_name)
		{
			sema_error_at(context, span,
			              "The %s '%s::%s' is defined in both '%s' and '%s', "
			              "please use either %s::%s or %s::%s to resolve the ambiguity.",
			              symbol_type, path_name, symbol, found_path, other_path,
			              found_path, symbol, other_path, symbol);
		}
		else
		{
			if (decl_needs_prefix(found))
			{
				sema_error_at(context, span, "The %s needs a path prefix (e.g. '%s::%s').", symbol_type, found_path,
				              symbol);
				return;
			}
			sema_error_at(context, span,
			              "The %s '%s' is defined in both '%s' and '%s', please use either "
			              "%s::%s or %s::%s to resolve the ambiguity.",
			              symbol_type, symbol, found_path, other_path,
			              found_path, symbol, other_path, symbol);
		}
		return;
	}
	ASSERT(!found);
	if (path_name)
	{
		// A common mistake is to type println and printfln
		if (name_resolve->path_found)
		{
			Decl *closest[3];
			int matches = module_closest_ident_names(name_resolve->path_found, symbol, closest);
			switch (matches)
			{
				case 1:
					sema_error_at(context, span, "'%s::%s' could not be found, did you perhaps want '%s::%s'?",
					              path_name, symbol, path_name, closest[0]->name);
					return;
				case 2:
					sema_error_at(context, span, "'%s::%s' could not be found, did you perhaps want '%s::%s' or '%s::%s'?",
					              path_name, symbol, path_name, closest[0]->name, path_name, closest[1]->name);
					return;
				case 3:
					sema_error_at(context, span, "'%s::%s' could not be found, did you perhaps want '%s::%s', '%s::%s' or '%s::%s'?",
					              path_name, symbol, path_name, closest[0]->name, path_name, closest[1]->name,
					              path_name, closest[2]->name);
					return;
				default:
					break;
			}
			if (matches > 0)
			{
				return;
			}
		}
		sema_error_at(context, span, "'%s::%s' could not be found, did you spell it right?", path_name, symbol);
	}
	else
	{
		sema_error_at(context, span, "'%s' could not be found, did you spell it right?", symbol);
	}
}

INLINE Module *sema_module_matches_path(SemaContext *context, Module *module, Path *path)
{
	if (matches_subpath(module->name, path))
	{
		FOREACH(Decl *, import, context->unit->imports)
		{
			Module *mod = module;
			while (mod)
			{
				if (import->import.module == mod)
				{
					return module;
				}
				mod = mod->parent_module;
			}
		}
	}
	return NULL;
}

INLINE Module *sema_find_module_for_path(SemaContext *context, Path *path, bool prefer_generic)
{
	if (prefer_generic)
	{
		FOREACH(Module *, module, compiler.context.generic_module_list)
		{
			Module *module_match = sema_module_matches_path(context, module, path);
			if (module_match) return module_match;
		}
	}
	FOREACH(Module *, module, compiler.context.module_list)
	{
		Module *module_match = sema_module_matches_path(context, module, path);
		if (module_match) return module_match;
	}
	if (!prefer_generic)
	{
		FOREACH(Module *, module, compiler.context.generic_module_list)
		{
			Module *module_match = sema_module_matches_path(context, module, path);
			if (module_match) return module_match;
		}
	}
	return NULL;
}

INLINE bool sema_resolve_symbol_common(SemaContext *context, NameResolve *name_resolve)
{
	name_resolve->ambiguous_other_decl = NULL;
	name_resolve->private_decl = NULL;
	name_resolve->path_found = NULL;
	if (name_resolve->path)
	{
		if (!sema_resolve_path_symbol(context, name_resolve)) return false;
		if (!name_resolve->found && !name_resolve->maybe_decl && !name_resolve->private_decl && !name_resolve->path_found)
		{
			if (name_resolve->suppress_error) return true;
			Module *module_with_path = sema_find_module_for_path(context, name_resolve->path, name_resolve->is_parameterized);
			if (module_with_path)
			{
				RETURN_SEMA_ERROR(name_resolve, "'%s' could not be found in %s.", name_resolve->symbol, module_with_path->name->module);
			}
			RETURN_SEMA_ERROR(name_resolve, "'%s' could not be found, try importing the '%.*s' module.", name_resolve->symbol, name_resolve->path->len, name_resolve->path->module);
		}
	}
	else
	{
		if (!sema_resolve_no_path_symbol(context, name_resolve)) return false;
	}

	Decl *found = name_resolve->found;
	if (!found || name_resolve->ambiguous_other_decl)
	{
		if (name_resolve->suppress_error) return name_resolve->found = NULL, true;
		sema_report_error_on_decl(context, name_resolve);
		return false;
	}
	if (found->is_if && context->call_env.in_if_resolution.a)
	{
		sema_error_at(context, context->call_env.in_if_resolution, "This @if expression is dependent on '%s' which is also conditional.", found->name);
		SEMA_NOTE(found, "'%s' is defined here.", found->name);
		return false;
	}
	if (found->decl_kind != DECL_ALIAS) unit_register_external_symbol(context, found);
	if (found->unit->module->is_generic)
	{
		if (name_resolve->is_parameterized) return true;
		if (context->generic.infer)
		{
			if (context->generic.infer->generic_module != found->unit->module)
			{
				if (name_resolve->suppress_error) return name_resolve->found = NULL, true;
				RETURN_SEMA_ERROR_AT(name_resolve->span, "Found '%s' in the generic module '%s', but it doesn't match the inferred module '%s'.", found->name, found->unit->module->name->module,
					context->generic.infer->generic_module->name->module);
			}
			Decl *symbol = module_find_symbol(context->generic.infer, found->name);
			if (symbol) return name_resolve->found = symbol, true;
		}
		if (name_resolve->suppress_error) return name_resolve->found = NULL, true;
		RETURN_SEMA_ERROR_AT(name_resolve->span, "'%s' is defined in the generic module '%s', but no parameters where given.", found->name, found->unit->module->name->module);
	}
	else
	{
		if (!name_resolve->is_parameterized) return true;
		if (name_resolve->suppress_error) return name_resolve->found = NULL, true;
		bool is_type = decl_is_user_defined_type(name_resolve->found);
		const char *str = is_type ? "type" : "symbol";
		if (found->unit->module->generic_suffix)
		{
			if (found->unit->module->generic_module == context->unit->module->generic_module)
			{
				Decl *decl = module_find_symbol(context->unit->module->generic_module, found->name);
				ASSERT(decl);
				name_resolve->found = decl;
				return true;
			}
			RETURN_SEMA_ERROR_AT(name_resolve->span, "This %s was matched as '%s%s' in module '%s%s', so parameterizing it further doesn't work.", str, found->name, found->unit->module->generic_suffix, found->unit->module->generic_module->name->module, found->unit->module->generic_suffix);
		}
		if (is_type)
		{
			RETURN_SEMA_ERROR_AT(name_resolve->span, "'%s' is not a generic type. Did you want an initializer "
										   "but forgot () around the type? That is, you typed '%s { ... }' but intended '(%s) { ... }'?",
										   name_resolve->symbol, name_resolve->symbol, name_resolve->symbol);
		}
		RETURN_SEMA_ERROR_AT(name_resolve->span, "Found '%s' in module '%s', but it is not a generic %s.", found->name, found->unit->module->name->module, str);
	}
	return true;
}

Decl *sema_find_extension_method_in_list(Decl **extensions, Type *type, const char *method_name)
{
	ASSERT(type == type->canonical);
	FOREACH(Decl *, extension, extensions)
	{
		if (extension->name != method_name) continue;
		if (typeget(extension->func_decl.type_parent) == type) return extension;
	}
	return NULL;
}

Decl *sema_resolve_method_only(Decl *type, const char *method_name)
{
	Methods *methods = type->method_table;
	Decl *found = NULL;
	if (methods)
	{
		found = declptrzero(decltable_get(&methods->method_table, method_name));
	}
	return found;
}

Decl *sema_resolve_method(Decl *type, const char *method_name)
{
	// Interface, prefer interface methods.
	bool is_interface = type->decl_kind == DECL_INTERFACE;
	if (is_interface)
	{
		FOREACH(Decl *, method, type->interface_methods)
		{
			if (method_name == method->name) return method;
		}
	}

	Methods *methods = type->method_table;
	Decl *found = NULL;
	if (methods)
	{
		found = declptrzero(decltable_get(&methods->method_table, method_name));
	}
	return found;
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
				UNREACHABLE
		}
		break;
	}
	ASSERT(type->type_kind == TYPE_STRUCT);
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
		case ALL_VECTORS:
		case TYPE_ANYFAULT:
		case TYPE_TYPEID:
		case TYPE_POINTER:
		case TYPE_FUNC_PTR:
		case TYPE_UNTYPED_LIST:
		case TYPE_MEMBER:
		case TYPE_SLICE:
		case TYPE_ANY:
		case TYPE_INTERFACE:
			return true;
		case TYPE_OPTIONAL:
			return sema_resolve_type_decl(context, type->optional);
		case TYPE_TYPEINFO:
			UNREACHABLE
		case TYPE_ALIAS:
			return sema_resolve_type_decl(context, type->canonical);
		case TYPE_TYPEDEF:
			if (!sema_analyse_decl(context, type->decl)) return false;
			return sema_resolve_type_decl(context, type->decl->distinct->type);
		case TYPE_FUNC_RAW:
			if (!type->function.prototype && type->function.decl->decl_kind == DECL_FNTYPE) return sema_analyse_decl(context, type->function.decl);
			return true;
		case TYPE_ENUM:
		case TYPE_CONST_ENUM:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_BITSTRUCT:
			return sema_analyse_decl(context, type->decl);
		case TYPE_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_INFERRED_ARRAY:
			return sema_resolve_type_decl(context, type->array.base);
	}
	UNREACHABLE
}

Decl *sema_resolve_type_method(SemaContext *context, CanonicalType *type, const char *method_name)
{
	RETRY:
	if (!type_is_user_defined(type))
	{
		Decl *found = declptrzero(methodtable_get(&compiler.context.method_extensions, type, method_name));
		if (found) return found;
		switch (type->type_kind)
		{
			case TYPE_ARRAY:
				return declptrzero(methodtable_get(&compiler.context.method_extensions, type_get_inferred_array(type->array.base), method_name));
			case VECTORS:
				return declptrzero(methodtable_get(&compiler.context.method_extensions, type_get_inferred_vector(type->array.base), method_name));
			default:
				return NULL;
		}
	}
	if (!type_may_have_method(type)) return NULL;
	Decl *type_decl = type->decl;
	if (!decl_ok(type_decl)) return poisoned_decl;
	Methods *methods = type_decl->method_table;
	Decl *found = methods ? declptrzero(decltable_get(&methods->method_table, method_name)) : NULL;
	if (found || !type_decl->is_substruct) return found;
	if (!sema_analyse_decl(context, type_decl)) return poisoned_decl;
	switch (type->type_kind)
	{
		case TYPE_STRUCT:
			type = type_decl->strukt.members[0]->type->canonical;
			goto RETRY;
		case TYPE_TYPEDEF:
			type = type_decl->distinct->type->canonical;
			goto RETRY;
		case TYPE_ENUM:
		case TYPE_CONST_ENUM:
			type = enum_inner_type(type);
			goto RETRY;
		default:
			UNREACHABLE
	}
}

/**
 * Silently find a symbol, will return NULL, Poison or the value
 */
Decl *sema_find_symbol(SemaContext *context, const char *symbol)
{
	NameResolve resolve = {
			.suppress_error = true,
			.symbol = symbol,
	};
	if (!sema_resolve_symbol_common(context, &resolve)) return poisoned_decl;
	return resolve.found;
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

BoolErr sema_symbol_is_defined_in_scope(SemaContext *c, const char *symbol)
{
	NameResolve resolve = {
			.suppress_error = true,
			.symbol = symbol,
	};
	if (!sema_resolve_symbol_common(c, &resolve)) return BOOL_ERR;
	// Unknown symbol => not defined
	Decl *found = resolve.found;
	if (!found) return BOOL_FALSE;
	// Defined in the same module => defined
	if (found->unit->module == c->unit->module) return BOOL_TRUE;
	// Not a variable or function => defined
	if (found->decl_kind != DECL_VAR && found->decl_kind != DECL_FUNC) return BOOL_TRUE;
	// Otherwise defined only if autoimport.
	return found->is_autoimport ? BOOL_TRUE : BOOL_FALSE;
}

Decl *sema_find_path_symbol(SemaContext *context, const char *symbol, Path *path)
{
	NameResolve resolve = {
			.suppress_error = true,
			.symbol = symbol,
			.path = path
	};
	if (!sema_resolve_symbol_common(context, &resolve)) return poisoned_decl;
	return resolve.found;
}

/**
 * Resolves a symbol, return NULL if an error was found (and signalled),
 * otherwise the decl.
 */
Decl *sema_resolve_symbol(SemaContext *context, const char *symbol, Path *path, SourceSpan span)
{
	NameResolve resolve = {
			.symbol = symbol,
			.path = path,
			.span = span
	};
	if (!sema_resolve_symbol_common(context, &resolve)) return NULL;
	Decl *found = resolve.found;
	ASSERT(found);
	if (!decl_ok(found)) return NULL;
	return resolve.found;
}

/**
 * Resolves a symbol, return NULL if an error was found (and signalled),
 * otherwise the decl.
 */
Decl *sema_resolve_parameterized_symbol(SemaContext *context, const char *symbol, Path *path, SourceSpan span)
{
	NameResolve resolve = {
		.path = path,
		.span = span,
		.symbol = symbol,
		.is_parameterized = true
	};
	if (!sema_resolve_symbol_common(context, &resolve)) return NULL;
	Decl *found = resolve.found;
	ASSERT(found);
	if (!decl_ok(found)) return NULL;
	return resolve.found;
}

bool sema_parameterized_type_is_found(SemaContext *context, Path *decl_path, const char *name, SourceSpan span)
{
	NameResolve name_resolve = {
		.path = decl_path,
		.span = span,
		.symbol = name,
		.suppress_error = true,
		.is_parameterized = true
	};
	return sema_resolve_symbol_common(context, &name_resolve) && name_resolve.found;
}

static inline void sema_append_local(SemaContext *context, Decl *decl)
{
	ASSERT(!decl_is_ct_var(decl));
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
	ASSERT(decl_is_ct_var(decl));

	Decl *other = sema_find_ct_local(context, decl->name);
	if (other)
	{
		sema_shadow_error(context, decl, other);
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
	if (other && (other->unit->module == current_unit->module || other->is_autoimport))
	{
		sema_shadow_error(context, decl, other);
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
	ASSERT(decl->decl_kind == DECL_VAR && decl->var.kind == VARDECL_UNWRAPPED && decl->var.alias->type->type_kind == TYPE_OPTIONAL);
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
	ASSERT(IS_OPTIONAL(decl));
	Decl *rewrapped = decl_copy(decl);
	rewrapped->var.kind = VARDECL_REWRAPPED;
	rewrapped->var.alias = decl;
	rewrapped->type = decl->type;
	rewrapped->resolve_status = RESOLVE_DONE;
	sema_append_local(context, rewrapped);
}
