//
// Created by Christoffer Lerno on 2019-08-24.
//

#include "compiler_internal.h"

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

void context_add_header_decl(Context *context, Decl *decl)
{
	DEBUG_LOG("Adding %s to header", decl->name.string);
	vec_add(context->header_declarations, decl);
}

bool context_add_local(Context *context, Decl *decl)
{
	Decl *other = context_find_ident(context, decl->name.string);
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

static inline bool create_module_or_check_name(Context *context, Token module_name)
{
    context->module_name = module_name;
    if (context->module == NULL)
    {
    	context->module = compiler_find_or_create_module(module_name.string);
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
	decl->module = context->module;
	switch (decl->decl_kind)
	{
		case DECL_POISONED:
		case DECL_MACRO:
		case DECL_GENERIC:
			break;
		case DECL_FUNC:
			vec_add(context->functions, decl);
			break;
		case DECL_VAR:
			vec_add(context->vars, decl);
			break;
		case DECL_STRUCT:
		case DECL_UNION:
		case DECL_TYPEDEF:
			vec_add(context->types, decl);
			break;
		case DECL_ENUM:
			vec_add(context->enums, decl);
			break;
		case DECL_ERROR:
			TODO
			break;
		case DECL_ENUM_CONSTANT:
		case DECL_ERROR_CONSTANT:
		case DECL_ARRAY_VALUE:
		case DECL_IMPORT:
		case DECL_MULTI_DECL:
		case DECL_CT_ELSE:
		case DECL_CT_ELIF:
		case DECL_ATTRIBUTE:
			UNREACHABLE
			break;

		case DECL_CT_IF:
			vec_add(context->ct_ifs, decl);
			return;
	}
	DEBUG_LOG("Registering symbol  %s.", decl->name.string);

	Decl *old = stable_set(&context->local_symbols, decl->name.string, decl);
	if (!old && decl->visibility != VISIBLE_LOCAL)
	{
		old = stable_set(&context->module->symbols, decl->name.string, decl);
	}
	if (!old && decl->visibility == VISIBLE_PUBLIC)
	{
		old = stable_set(&context->module->public_symbols, decl->name.string, decl);
	}
	if (old != NULL)
	{
		sema_shadow_error(decl, old);
		decl_poison(decl);
		decl_poison(old);
	}
}

bool context_add_import(Context *context, Token module_name, Token alias, ImportType import_type, Expr** generic_parameters)
{
    DEBUG_LOG("SEMA: Add import of '%s'.", module_name.string);
    if (!is_all_lower(module_name.string))
    {
        sema_error_range(module_name.span, "A module is not expected to have any upper case characters, please change it.");
        return false;
    }
    Decl *decl = decl_new(DECL_IMPORT, module_name, VISIBLE_LOCAL);
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
	VECEACH(context->enums, i)
	{
		fprint_decl(file, context->enums[i]);
	}
	VECEACH(context->vars, i)
	{
		fprint_decl(file, context->vars[i]);
	}
	VECEACH(context->types, i)
	{
		fprint_decl(file, context->types[i]);
	}
	VECEACH(context->functions, i)
	{
		fprint_decl(file, context->functions[i]);
	}
	VECEACH(context->ct_ifs, i)
	{
		fprint_decl(file, context->ct_ifs[i]);
	}
}
