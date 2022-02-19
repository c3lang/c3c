//
// Created by Christoffer Lerno on 2019-08-24.
//

#include "compiler_internal.h"


CompilationUnit * unit_create(File *file)
{
	CompilationUnit *unit = CALLOCS(CompilationUnit);
    unit->file = file;
	htable_init(&unit->local_symbols, 64 * 1024);
    return unit;
}


static inline bool create_module_or_check_name(CompilationUnit *unit, Path *module_name, const char **parameters, bool is_private)
{
    Module *module = unit->module;
	if (!module)
	{
		module = unit->module = compiler_find_or_create_module(module_name, parameters, is_private);
	}
	else
	{
		if (unit->module->name->module != module_name->module)
		{
			SEMA_ERROR(module_name,
			           "Module name here '%s' did not match actual module '%s'.",
			           module_name->module,
			           module->name->module);
			return false;
		}
	}

    if (module->is_private != is_private)
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
	vec_add(module->units, unit);
	return true;
}

static bool filename_to_module_in_buffer(const char *path)
{
	int len = (int)strlen(path);
	int last_slash = 0;
	int last_dot = -1;
	for (int i = 0; i < len; i++)
	{
		if (path[i] == '/') last_slash = i;
		if (path[i] == '.') last_dot = i;
	}
	int name_len = last_dot - last_slash - 1;
	if (name_len < 1) return false;
	scratch_buffer_clear();
	for (int i = last_slash + 1; i < last_dot; i++)
	{
		char c = path[i];
		if (is_letter(c))
		{
			c = (char)(is_upper(c) ? c + 'a' - 'A' : c);
		}
		else
		{
			c = '_';
		}
		scratch_buffer_append_char(c);
	}
	return true;
}

bool context_set_module_from_filename(ParseContext *context)
{
	File *file = context->unit->file;
	if (!filename_to_module_in_buffer(file->full_path))
    {
		sema_error(context, "The filename '%s' could not be converted to a valid module name, try using an explicit module name.", file->full_path);
        return false;
    }

    TokenType type = TOKEN_IDENT;
	const char *module_name = symtab_add(global_context.scratch_buffer,
										 global_context.scratch_buffer_len,
	                                     fnv1a(global_context.scratch_buffer, (uint32_t) global_context.scratch_buffer_len),
	                                     &type);

    if (type != TOKEN_IDENT)
    {
	    sema_error(context, "Generating a filename from the file '%s' resulted in a name that is a reserved keyword, "
	                        "try using an explicit module name.", file->full_path);
        return false;
    }
    Path *path = CALLOCS(Path);
    path->span = INVALID_SPAN;
    path->module = module_name;
    path->len = global_context.scratch_buffer_len;
    return create_module_or_check_name(context->unit, path, NULL, true);
}

bool context_set_module(ParseContext *context, Path *path, const char **generic_parameters, bool is_private)
{
    // Note that we allow the illegal name for now, to be able to parse further.
    if (!is_all_lower(path->module))
    {
    	SEMA_ERROR(path, "A module name may not have any upper case characters.");
        return false;
    }

    return create_module_or_check_name(context->unit, path, generic_parameters, is_private);
}


void unit_register_external_symbol(CompilationUnit *unit, Decl *decl)
{
	if (!decl->module || decl->module == unit->module || !decl->external_name) return;
	VECEACH(unit->external_symbol_list, i)
	{
		if (decl == unit->external_symbol_list[i]) return;
	}
	vec_add(unit->external_symbol_list, decl);
}


void decl_register(Decl *decl)
{
	if (decl->visibility != VISIBLE_PUBLIC) return;
	switch (decl->decl_kind)
	{
		case DECL_POISONED:
		case DECL_CT_CASE:
		case DECL_CT_ELIF:
		case DECL_CT_ELSE:
		case DECL_CT_IF:
		case DECL_CT_SWITCH:
		case DECL_CT_ASSERT:
		case DECL_ENUM_CONSTANT:
		case DECL_OPTVALUE:
		case DECL_IMPORT:
		case DECL_LABEL:
		case DECL_DECLARRAY:
			UNREACHABLE
		case DECL_ATTRIBUTE:
		case DECL_BITSTRUCT:
		case DECL_DISTINCT:
		case DECL_OPTENUM:
		case DECL_ENUM:
		case DECL_STRUCT:
		case DECL_TYPEDEF:
		case DECL_UNION:
		case DECL_DEFINE:
		case DECL_FUNC:
		case DECL_GENERIC:
		case DECL_MACRO:
		case DECL_VAR:
			global_context_add_decl(decl);
			break;
	}
}

void unit_register_global_decl(CompilationUnit *unit, Decl *decl)
{
	decl->module = unit->module;
	switch (decl->decl_kind)
	{
		case DECL_POISONED:
			break;
		case DECL_GENERIC:
			assert(decl->name);
			if (decl->macro_decl.type_parent)
			{
				vec_add(unit->generic_methods, decl);
				return;
			}
			else
			{
				vec_add(unit->generics, decl);
			}
			decl_set_external_name(decl);
			decl_register(decl);
			break;
		case DECL_MACRO:
			assert(decl->name);
			if (decl->macro_decl.type_parent)
			{
				vec_add(unit->macro_methods, decl);
				return;
			}
			else
			{
				vec_add(unit->macros, decl);
			}
			decl_set_external_name(decl);
			decl_register(decl);
			break;
		case DECL_FUNC:
			assert(decl->name);
			if (decl->func_decl.type_parent)
			{
				vec_add(unit->methods, decl);
				return;
			}
			else
			{
				vec_add(unit->functions, decl);
			}
			decl_register(decl);
			break;
		case DECL_VAR:
			assert(decl->name);
			vec_add(unit->vars, decl);
			decl_set_external_name(decl);
			decl_register(decl);
			break;
		case DECL_DISTINCT:
		case DECL_STRUCT:
		case DECL_UNION:
		case DECL_TYPEDEF:
		case DECL_OPTENUM:
		case DECL_BITSTRUCT:
			assert(decl->name);
			vec_add(unit->types, decl);
			decl_set_external_name(decl);
			decl_register(decl);
			break;
		case DECL_DEFINE:
			assert(decl->name);
			vec_add(unit->generic_defines, decl);
			decl_set_external_name(decl);
			decl_register(decl);
			break;
		case DECL_ENUM:
			assert(decl->name);
			vec_add(unit->enums, decl);
			decl_set_external_name(decl);
			decl_register(decl);
			break;
		case DECL_OPTVALUE:
		case DECL_ENUM_CONSTANT:
		case DECL_IMPORT:
		case DECL_CT_ELSE:
		case DECL_CT_ELIF:
		case DECL_ATTRIBUTE:
		case DECL_LABEL:
		case DECL_CT_CASE:
		case DECL_DECLARRAY:
			UNREACHABLE
		case DECL_CT_IF:
		case DECL_CT_SWITCH:
			vec_add(unit->ct_ifs, decl);
			return;
		case DECL_CT_ASSERT:
			vec_add(unit->ct_asserts, decl);
			return;
	}
	DEBUG_LOG("Registering symbol '%s' in %s.", decl->name, unit->module->name->module);

	Decl *old;
	if ((old = htable_set(&unit->local_symbols, decl->name, decl))) goto ERR;
	if ((old = htable_set(&unit->module->symbols, decl->name, decl))) goto ERR;
	return;
ERR:
	assert(decl != old);
	sema_shadow_error(decl, old);
	decl_poison(decl);
	decl_poison(old);
}

bool unit_add_import(CompilationUnit *unit, Path *path, bool private_import)
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

    vec_add(unit->imports, import);
	DEBUG_LOG("Added import %s", path->module);
    return true;
}

