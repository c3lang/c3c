//
// Created by Christoffer Lerno on 2019-08-24.
//

#include "compiler_internal.h"


Context *context_create(File *file)
{
    Context *context = CALLOCS(Context);
    context->file = file;
    stable_init(&context->local_symbols, 256);
	stable_init(&context->external_symbols, 256);
    return context;
}


static inline bool create_module_or_check_name(Context *context, Path *module_name, TokenId *parameters, bool is_private)
{
    context->module_name = module_name;
	if (context->module == NULL)
	{
		context->module = compiler_find_or_create_module(module_name, parameters, is_private);
	}
	else
	{
		if (context->module->name->module != module_name->module)
		{
			SEMA_ERROR(module_name,
			           "Module name here '%s' did not match actual module '%s'.",
			           module_name->module,
			           context->module->name->module);
			return false;
		}
	}

    if (context->module->is_private != is_private)
    {
    	if (is_private)
	    {
		    SEMA_ERROR(module_name, "The module is declared as private here, but was declared as public elsewhere.");
	    }
    	else
	    {
		    SEMA_ERROR(module_name, "The module is declared as public here, but was declared as private elsewhere.");
	    }
	    return false;

    }
	vec_add(context->module->contexts, context);
	return true;
}

bool context_set_module_from_filename(Context *context)
{
    char buffer[MAX_IDENTIFIER_LENGTH + 1];
    int len = filename_to_module(context->file->full_path, buffer);
    if (!len)
    {
        sema_error(context, "The filename '%s' could not be converted to a valid module name, try using an explicit module name.", context->file->full_path);
        return false;
    }

    TokenType type = TOKEN_IDENT;
    const char *module_name = symtab_add(buffer, (uint32_t) len, fnv1a(buffer, (uint32_t) len), &type);
    if (type != TOKEN_IDENT)
    {
	    sema_error(context, "Generating a filename from the file '%s' resulted in a name that is a reserved keyword, "
	                        "try using an explicit module name.", context->file->full_path);
        return false;
    }
    Path *path = CALLOCS(Path);
    path->span = INVALID_RANGE;
    path->module = module_name;
    path->len = len;
    return create_module_or_check_name(context, path, NULL, true);
}

bool context_set_module(Context *context, Path *path, TokenId *generic_parameters, bool is_private)
{
    // Note that we allow the illegal name for now, to be able to parse further.
    context->module_name = path;
    if (!is_all_lower(path->module))
    {
        SEMA_ERROR(path, "A module name may not have any upper case characters.");
        return false;
    }

    return create_module_or_check_name(context, path, generic_parameters, is_private);
}


void context_register_external_symbol(Context *context, Decl *decl)
{
	assert(decl->external_name && "Missing external name");
	Decl *prev = stable_get(&context->external_symbols, decl->external_name);
	if (prev) return;
	stable_set(&context->external_symbols, decl->external_name, decl);
	vec_add(context->external_symbol_list, decl);
}



void context_register_global_decl(Context *context, Decl *decl)
{
	assert(decl->name);
	decl->module = context->module;
	switch (decl->decl_kind)
	{
		case DECL_POISONED:
			break;
		case DECL_INTERFACE:
			vec_add(context->interfaces, decl);
			decl_set_external_name(decl);
			break;
		case DECL_GENERIC:
			vec_add(context->generics, decl);
			decl_set_external_name(decl);
			break;
		case DECL_MACRO:
			if (decl->macro_decl.type_parent)
			{
				vec_add(context->macro_methods, decl);
				return;
			}
			else
			{
				vec_add(context->macros, decl);
			}
			decl_set_external_name(decl);
			break;
		case DECL_FUNC:
			if (decl->func_decl.type_parent)
			{
				vec_add(context->methods, decl);
				return;
			}
			else
			{
				vec_add(context->functions, decl);
			}
			break;
		case DECL_VAR:
			vec_add(context->vars, decl);
			decl_set_external_name(decl);
			break;
		case DECL_STRUCT:
		case DECL_UNION:
		case DECL_TYPEDEF:
		case DECL_DISTINCT:
		case DECL_ERR:
			vec_add(context->types, decl);
			decl_set_external_name(decl);
			break;
		case DECL_DEFINE:
			vec_add(context->generic_defines, decl);
			decl_set_external_name(decl);
			break;
		case DECL_ENUM:
			vec_add(context->enums, decl);
			decl_set_external_name(decl);
			break;
		case DECL_ARRAY_VALUE:
			vec_add(context->incr_array, decl);
			return;
		case DECL_ENUM_CONSTANT:
		case DECL_IMPORT:
		case DECL_CT_ELSE:
		case DECL_CT_ELIF:
		case DECL_ATTRIBUTE:
		case DECL_LABEL:
		case DECL_CT_CASE:
			UNREACHABLE
		case DECL_CT_IF:
		case DECL_CT_SWITCH:
			vec_add(context->ct_ifs, decl);
			return;
		case DECL_CT_ASSERT:
			vec_add(context->ct_asserts, decl);
			return;
	}
	DEBUG_LOG("Registering symbol '%s'.", decl->name);

	Decl *old = stable_set(&context->local_symbols, decl->name, decl);
	if (!old)
	{
		old = stable_set(&context->module->symbols, decl->name, decl);
	}
	if (!old && decl->visibility == VISIBLE_PUBLIC)
	{
		compiler_register_public_symbol(decl);
		old = stable_set(&context->module->public_symbols, decl->name, decl);
	}
	if (old != NULL)
	{
		sema_shadow_error(decl, old);
		decl_poison(decl);
		decl_poison(old);
	}
}

bool context_add_import(Context *context, Path *path, Token token, Token alias, bool private_import)
{
    DEBUG_LOG("SEMA: Add import of '%s'.", path->module);


	if (!is_all_lower(path->module))
	{
		SEMA_ERROR(path, "A module is not expected to have any upper case characters, please change it.");
		return false;
	}

	Decl *import = decl_calloc();
	import->span = path->span;
	import->decl_kind = DECL_IMPORT;
	import->visibility = VISIBLE_LOCAL;
	import->import.path = path;
	import->import.private = private_import;
	import->import.symbol = token.id;
	if (alias.type != TOKEN_INVALID_TOKEN)
    {
		const char *alias_name = TOKSTR(alias);
	    if (alias_name == TOKSTR(token))
	    {
		    SEMA_TOKEN_ERROR(alias, "If an alias would be the same as the symbol aliased, it wouldn't have any effect.");
		    return false;
	    }
	    if (alias_name == context->module_name->module)
	    {
		    SEMA_TOKEN_ERROR(alias, "An alias cannot have not have the same as the name of the current module.");
		    return false;
	    }
	    import->import.aliased = true;
	    TODO
    }

    vec_add(context->imports, import);
	DEBUG_LOG("Added import %s", path->module);
    return true;
}

void context_print_ast(Context *context, FILE *file)
{
	VECEACH(context->enums, i)
	{
		fprint_decl(context, file, context->enums[i]);
	}
	VECEACH(context->global_decls, i)
	{
		fprint_decl(context, file, context->global_decls[i]);
	}
	VECEACH(context->vars, i)
	{
		fprint_decl(context, file, context->vars[i]);
	}
	VECEACH(context->types, i)
	{
		fprint_decl(context, file, context->types[i]);
	}
	VECEACH(context->functions, i)
	{
		fprint_decl(context, file, context->functions[i]);
	}
	VECEACH(context->ct_ifs, i)
	{
		fprint_decl(context, file, context->ct_ifs[i]);
	}
}
