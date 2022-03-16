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
    if (path_to_find->module == path_to_check->module)
        return true;

    // Let's check the offset on where to start comparing to start with
    // the submatch.
    ssize_t compare_start = (ssize_t)path_to_check->len - (ssize_t)path_to_find->len;

    // The smallest match is the situation a::foo::bar vs foo::bar
    // This means that the compare_start must be 3 or more.
    if (compare_start < 3)
        return false;

    // We also want to make sure that the preceeding 2 characters are ::
    if (path_to_check->module[compare_start - 1] != ':' || path_to_check->module[compare_start - 2] != ':')
        return false;

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
        if (decl->name == symbol)
            return decl;
    }
    return NULL;
}

static inline Decl *sema_find_decl_in_module(Module *module, Path *path, const char *symbol, bool *path_found)
{
    if (!path)
        return module_find_symbol(module, symbol);
    if (path->len > module->name->len)
        return NULL;
    if (!matches_subpath(module->name, path))
        return NULL;
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
        if (import->module->is_generic != want_generic)
            continue;

        // Is the decl in the import.
        Decl *found = sema_find_decl_in_module(import->module, path, symbol, &name_resolve->path_found);

        // No match, so continue
        if (!found)
            continue;

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
        if (module->is_generic != want_generic)
            continue;
        if (matches_subpath(module->name, path))
        {
            return true;
        }
    }
    return false;
}

static Decl *sema_find_decl_in_global(DeclTable *table, Module **module_list, NameResolve *name_resolve,
                                      bool want_generic)
{
    const char *symbol = name_resolve->symbol;
    Path *path = name_resolve->path;
    DeclId decl_ids = decltable_get(table, symbol);

    // We might have no match at all.
    if (!decl_ids)
    {
        // Update the path found
        if (path && !name_resolve->path_found)
            name_resolve->path_found = sema_is_path_found(module_list, path, want_generic);
        return NULL;
    }

    Decl *decls = declptr(decl_ids);
    // There might just be a single match.
    if (decls->decl_kind != DECL_DECLARRAY)
    {
        if (path && !matches_subpath(decls->module->name, path))
            return false;
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
        if (!ambiguous && (!path || matches_subpath(candidate->module->name, path)))
        {
            ambiguous = decl;
            decl = candidate;
        }
    }
    if (ambiguous)
        name_resolve->ambiguous_other_decl = ambiguous;
    name_resolve->private_decl = NULL;
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
        // 2. If so just get the symbol.
        return module_find_symbol(unit->module, symbol);
    }

    // 3. Loop over imports.
    decl = sema_find_decl_in_imports(unit->imports, name_resolve, false);

    // 4. Go to global search
    return decl ? decl
                : sema_find_decl_in_global(&global_context.symbols, global_context.module_list, name_resolve, false);
}

static Decl *sema_resolve_no_path_symbol(SemaContext *context, NameResolve *name_resolve)
{
    Decl *decl = NULL;

    const char *symbol = name_resolve->symbol;
    assert(name_resolve->path == NULL);

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
                    if (kind == VARDECL_ERASE)
                        goto JUMP_ERASED;
                    if (kind == VARDECL_REWRAPPED)
                        return cur->var.alias;
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

    if (decl)
        return decl;

    // Search in the module.
    decl = module_find_symbol(unit->module, symbol);

    if (decl)
        return decl;

    decl = sema_find_decl_in_imports(unit->imports, name_resolve, false);
    return decl ? decl : sema_find_decl_in_global(&global_context.symbols, NULL, name_resolve, false);
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
            sema_error_at(span, "The %s '%s::%s' is not visible from this module.", private_name, path_name, symbol);
        }
        else
        {
            sema_error_at(span, "The %s '%s' is not visible from this module.", private_name, symbol);
        }
        return;
    }

    if (name_resolve->ambiguous_other_decl)
    {
        assert(found);
        const char *symbol_type = decl_to_name(found);
        const char *found_path = found->module->name->module;
        const char *other_path = name_resolve->ambiguous_other_decl->module->name->module;
        if (path_name)
        {
            sema_error_at(span,
                          "The %s '%s::%s' is defined in both '%s' and '%s', "
                          "please use either %s::%s or %s::%s to resolve the ambiguity.",
                          symbol_type, path_name, symbol, found_path, other_path, found_path, symbol, other_path,
                          symbol);
        }
        else
        {
            sema_error_at(span,
                          "The %s '%s' is defined in both '%s' and '%s', please use either "
                          "%s::%s or %s::%s to resolve the ambiguity.",
                          symbol_type, symbol, found_path, other_path, found_path, symbol, other_path, symbol);
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
        if (!decl && !name_resolve->path_found)
        {
            if (!name_resolve->suppress_error)
                return NULL;
            SEMA_ERROR(name_resolve->path, "Unknown module '%.*s', did you type it right?", name_resolve->path->len,
                       name_resolve->path->module);
            return poisoned_decl;
        }
    }
    else
    {
        decl = sema_resolve_no_path_symbol(context, name_resolve);
    }

    if (!decl || name_resolve->ambiguous_other_decl)
    {
        if (name_resolve->suppress_error)
            return NULL;
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
        if (extension->name != method_name)
            continue;
        switch (extension->decl_kind)
        {
        case DECL_FUNC:
            if (type_infoptr(extension->func_decl.type_parent)->type == type)
                return extension;
            break;
        case DECL_MACRO:
        case DECL_GENERIC:
            if (type_infoptr(extension->macro_decl.type_parent)->type == type)
                return extension;
            break;
        default:
            UNREACHABLE
        }
    }
    return NULL;
}

Decl *sema_resolve_method(CompilationUnit *unit, Decl *type, const char *method_name, Decl **ambiguous_ref,
                          Decl **private_ref)
{
    // 1. Look at the previously defined ones.
    VECEACH(type->methods, i)
    {
        Decl *func = type->methods[i];
        if (method_name == func->name)
            return func;
    }
    // 2. Make a module lookup
    Decl *previously_found = NULL;
    Type *actual_type = type->type;
    Decl *private_type = NULL;
    Decl *result = NULL;
    VECEACH(unit->imports, i)
    {
        Decl *import = unit->imports[i];

        if (import->module->is_generic)
            continue;

        Decl *found = sema_find_extension_method_in_module(import->module, actual_type, method_name);
        if (!found)
            continue;
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

Decl *unit_resolve_parameterized_symbol(CompilationUnit *unit, NameResolve *name_resolve)
{
    name_resolve->ambiguous_other_decl = NULL;
    name_resolve->private_decl = NULL;
    name_resolve->path_found = false;

    Decl *decl = sema_find_decl_in_imports(unit->imports, name_resolve, true);
    if (!decl)
    {
        decl = sema_find_decl_in_global(&global_context.generic_symbols, global_context.generic_module_list,
                                        name_resolve, true);
    }
    // 14. Error report
    if (!decl || name_resolve->ambiguous_other_decl)
    {
        if (name_resolve->suppress_error)
            return poisoned_decl;
        sema_report_error_on_decl(decl, name_resolve);
        return poisoned_decl;
    }
    if (!decl_is_user_defined_type(decl) && !name_resolve->path)
    {
        if (name_resolve->suppress_error)
            return poisoned_decl;
        sema_error_at(name_resolve->span, "Function and variables must be prefixed with a path, e.g. 'foo::%s'.",
                      name_resolve->symbol);
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

Decl *sema_find_path_symbol(SemaContext *context, const char *symbol, Path *path)
{
    NameResolve resolve = {.suppress_error = true, .symbol = symbol, .path = path};
    return sema_resolve_symbol_common(context, &resolve);
}

Decl *sema_resolve_symbol(SemaContext *context, const char *symbol, Path *path, SourceSpan span)
{
    NameResolve resolve = {.symbol = symbol, .path = path, .span = span};
    return sema_resolve_symbol_common(context, &resolve);
}

Decl *sema_resolve_normal_symbol(SemaContext *context, NameResolve *name_resolve)
{
    return sema_resolve_symbol_common(context, name_resolve);
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

void sema_add_member(SemaContext *context, Decl *decl)
{
    sema_append_local(context, decl);
}

bool sema_add_local(SemaContext *context, Decl *decl)
{
    Module *current_module = decl->module = context->unit->module;
    // Ignore synthetic locals.
    if (!decl->name)
        return true;
    if (decl->decl_kind == DECL_VAR && decl->var.shadow)
        goto ADD_VAR;
    Decl *other = sema_find_symbol(context, decl->name);
    assert(!other || other->module);
    if (other && other->module == current_module)
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
    assert(decl->decl_kind == DECL_VAR && decl->var.kind == VARDECL_UNWRAPPED &&
           decl->var.alias->type->type_kind == TYPE_FAILABLE);
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
    assert(IS_FAILABLE(decl));
    Decl *rewrapped = decl_copy(decl);
    rewrapped->var.kind = VARDECL_REWRAPPED;
    rewrapped->var.alias = decl;
    rewrapped->type = decl->type;
    rewrapped->resolve_status = RESOLVE_DONE;
    sema_append_local(context, rewrapped);
}
