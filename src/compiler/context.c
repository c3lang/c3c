//
// Created by Christoffer Lerno on 2019-08-24.
//

#include <utils/file_utils.h>
#include <utils/lib.h>
#include "context.h"
#include "diagnostics.h"

Context *current_context;

Context *context_create(File *file)
{
    Context *context = malloc_arena(sizeof(Context));
    memset(context, 0, sizeof(Context));
    context->file = file;
    stable_init(&context->local_symbols, 256);
    return context;
}

void context_push(Context *context)
{
    current_context = context;
}

static inline bool create_module_or_check_name(Context *context, Token module_name)
{
    context->module_name = module_name;
    if (context->module == NULL)
    {
        context->module = malloc_arena(sizeof(Module));
        memset(context->module, 0, sizeof(Module));
        context->module->name = module_name.string;
        stable_init(&(context->module)->symbols, 0x10000);
        return true;
    }
    else if (context->module->name != module_name.string)
    {
        SEMA_ERROR(module_name, "Module name here '%s' did not match actual module '%s'.", module_name.string, context->module->name);
        return false;
    }
    return true;
}

bool context_set_module_from_filename(Context *context)
{
    LOG_FUNC

    char buffer[MAX_IDENTIFIER_LENGTH + 1];
    int len = filename_to_module(context->file->full_path, buffer);
    if (!len)
    {
        sema_error("The filename '%s' could not be converted to a valid module name, try using an explicit module name.");
        return false;
    }

    TokenType type = TOKEN_IDENT;
    const char *module_name = symtab_add(buffer, (uint32_t) len, fnv1a(buffer, (uint32_t) len), &type);
    if (type != TOKEN_IDENT)
    {
        sema_error("Generating a filename from the file '%s' resulted in a name that is a reserved keyword, "
                   "try using an explicit module name.");
        return false;
    }
    return create_module_or_check_name(context, wrap(module_name));
}

bool context_set_module(Context *context, Token module_name, Token *generic_parameters)
{
    LOG_FUNC
    DEBUG_LOG("CONTEXT: Setting module to '%s'.", module_name.string);
    // Note that we allow the illegal name for now, to be able to parse further.
    context->module_name = module_name;
    if (!is_all_lower(module_name.string))
    {
        sema_error_range(module_name.span, "A module name may not have any upper case characters.");
        return false;
    }
    context->module_parameters = generic_parameters;

    return create_module_or_check_name(context, module_name);
}



void context_register_global_decl(Context *context, Decl *decl)
{
	if (decl->decl_kind == DECL_CT_IF)
	{
		context->ct_ifs = VECADD(context->ct_ifs, decl);
	}
	else
	{
		DEBUG_LOG("Registering %s.", decl->name.string);
		context->declarations = VECADD(context->declarations, decl);
	}
}

bool context_add_import(Context *context, Token module_name, Token alias, ImportType import_type, Expr** generic_parameters)
{
    LOG_FUNC
    DEBUG_LOG("SEMA: Add import of '%s'.", module_name.string);
    if (!is_all_lower(module_name.string))
    {
        sema_error_range(module_name.span, "A module is not expected to have any upper case characters, please change it.");
        return false;
    }
    Decl *decl = decl_new_in_module(context->module, DECL_IMPORT, module_name, VISIBLE_LOCAL);
    decl->import.type = import_type;
    decl->import.generic_parameters = generic_parameters;
    if (import_type == IMPORT_TYPE_ALIAS_LOCAL || import_type == IMPORT_TYPE_ALIAS)
    {
        decl->import.alias = alias;
        if (!is_all_lower(alias.string))
        {
            sema_error_range(alias.span, "A module alias is not expected to have any upper case characters, please change it.");
            return false;
        }
        if (alias.string == module_name.string)
        {
            sema_error_range(alias.span, "If a module alias would be the same as the alias, it wouldn't have any effect.");
            return false;
        }
        if (alias.string == context->module_name.string)
        {
            sema_error_range(alias.span, "An alias should not be the same as the name of the current module.");
            return false;
        }
    }
    else
    {
        decl->import.alias.string = NULL;
    }

    VECEACH(context->imports, i)
    {
        Decl *other_import = context->imports[i];
        if (other_import->name.string == module_name.string
            && !other_import->import.generic_parameters
            && !generic_parameters)
        {
            sema_error_range(module_name.span, "This module was imported earlier in the file.");
        }
        if (other_import->import.alias.string == module_name.string)
        {
            sema_error_range(other_import->import.alias.span,
                             "An alias should not be the same as the name of another imported module.");
            return false;
        }
        if (decl->import.alias.string == other_import->name.string)
        {
            sema_error_range(decl->import.alias.span,
                             "An alias should not be the same as the name of another imported module.");
            return false;
        }
        if (decl->import.alias.string && decl->import.alias.string == other_import->import.alias.string)
        {
            sema_error_range(decl->import.alias.span,
                             "This alias has already been used by an earlier import statement.");
            return false;
        }
    }

    context->imports = VECADD(context->imports, decl);
    return true;
}

void context_print_ast(Context *context, FILE *file)
{
	{
		VECEACH(context->declarations, i)
		{
			fprint_decl(file, context->declarations[i]);
		}
	}
	{
		VECEACH(context->ct_ifs, i)
		{
			fprint_decl(file, context->ct_ifs[i]);
		}
	}
}
