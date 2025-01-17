//
// Created by Christoffer Lerno on 2019-08-24.
//

#include "compiler_internal.h"

CompilationUnit *unit_create(File *file)
{
	CompilationUnit *unit = CALLOCS(CompilationUnit);
	unit->file = file;
	unit->is_interface_file = str_has_suffix(file->name, ".c3i");
	htable_init(&unit->local_symbols, 1024);
	return unit;
}


static inline bool create_module_or_check_name(CompilationUnit *unit, Path *module_name, const char **parameters)
{
	Module *module = unit->module;
	if (!module)
	{
		module = unit->module = compiler_find_or_create_module(module_name, parameters);
		if (module->is_generic != (parameters != NULL))
		{
			print_error_at(module_name->span, "'%s' is both used as regular and generic module, it can't be both.",
			               module_name->module);
			SEMA_NOTE(module->name, "The definition here is different.");
			return false;
		}
		if (!module->is_generic) goto DONE;
		if (vec_size(parameters) != vec_size(module->parameters))
		{
			PRINT_ERROR_AT(module_name, "The parameter declarations of the generic module '%s' don't match.");
			SEMA_NOTE(module->name, "A different definition can be found here.");
			return false;
		}
		FOREACH_IDX(idx, const char *, name, parameters)
		{
			bool is_type = str_is_type(name);
			if (is_type != str_is_type(module->parameters[idx]))
			{
				PRINT_ERROR_AT(module_name, "The parameter declarations of the generic module '%s' don't match.");
				SEMA_NOTE(module->name, "The other definition is here.");
				return false;
			}
		}
		goto DONE;
	}
	if (unit->module->name->module != module_name->module)
	{
		RETURN_PRINT_ERROR_AT(false,
		                      module_name,
		                      "Module name here '%s' did not match actual module '%s'.",
		                      module_name->module,
		                      module->name->module);
	}

DONE:;
	vec_add(module->units, unit);
	return true;
}

static bool filename_to_module_in_buffer(const char *path)
{
	if (str_eq("<stdin>", path))
	{
		scratch_buffer_clear();
		scratch_buffer_append("stdin_file");
		return true;
	}
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
	bool last_was_underscore = true;
	for (int i = last_slash + 1; i < last_dot; i++)
	{
		char c = path[i];
		if (char_is_letter(c) || char_is_digit(c))
		{
			last_was_underscore = false;
			c = (char)(char_is_upper(c) ? c + 'a' - 'A' : c);
		}
		else
		{
			if (last_was_underscore) continue;
			c = '_';
			last_was_underscore = true;
		}
		scratch_buffer_append_char(c);
	}
	if (last_was_underscore && scratch_buffer.len) scratch_buffer.len--;
	if (!scratch_buffer.len) return false;
	return true;
}

bool context_set_module_from_filename(ParseContext *context)
{
	File *file = context->unit->file;
	if (!filename_to_module_in_buffer(file->full_path))
	{
		print_error(context,
		            "The filename '%s' could not be converted to a valid module name, try using an explicit module name.",
		            file->full_path);
		return false;
	}

	TokenType type = TOKEN_IDENT;
	const char *module_name = scratch_buffer_interned_as(&type);
	if (type != TOKEN_IDENT)
	{
		print_error(context, "Generating a filename from the file '%s' resulted in a name that is a reserved keyword, "
		                     "try using an explicit module name.", file->full_path);
		return false;
	}
	Path *path = CALLOCS(Path);
	path->span = INVALID_SPAN;
	path->module = module_name;
	path->len = scratch_buffer.len;
	return create_module_or_check_name(context->unit, path, NULL);
}

bool context_set_module(ParseContext *context, Path *path, const char **generic_parameters)
{

	if (!check_module_name(path)) return false;
	return create_module_or_check_name(context->unit, path, generic_parameters);
}

bool context_is_macro(SemaContext *context)
{
	if (context->current_macro != NULL) return true;
	return context->call_env.current_function && context->call_env.current_function->func_decl.in_macro;
}

void unit_register_external_symbol(SemaContext *context, Decl *decl)
{
	if (decl->is_external_visible) return;
	Module *active_module = context->current_macro ? context->original_module : context->compilation_unit->module;
	if (decl->unit->module == active_module) return;
	decl->is_external_visible = true;
}


void decl_register(Decl *decl)
{
	if (decl->visibility > VISIBLE_PUBLIC) return;
	switch (decl->decl_kind)
	{
		case DECL_ERASED:
			return;
		case DECL_POISONED:
		case DECL_CT_ASSERT:
		case DECL_CT_ECHO:
		case DECL_CT_EXEC:
		case DECL_ENUM_CONSTANT:
		case DECL_FAULTVALUE:
		case DECL_IMPORT:
		case DECL_LABEL:
		case DECL_DECLARRAY:
		case DECL_BODYPARAM:
		case DECL_CT_INCLUDE:
		case DECL_GLOBALS:
			UNREACHABLE
		case DECL_ATTRIBUTE:
		case DECL_BITSTRUCT:
		case DECL_DISTINCT:
		case DECL_FAULT:
		case DECL_ENUM:
		case DECL_STRUCT:
		case DECL_TYPEDEF:
		case DECL_UNION:
		case DECL_DEFINE:
		case DECL_FUNC:
		case DECL_MACRO:
		case DECL_VAR:
		case DECL_FNTYPE:
		case DECL_INTERFACE:
			global_context_add_decl(decl);
			break;
	}
}

void unit_register_global_decl(CompilationUnit *unit, Decl *decl)
{
	ASSERT0(!decl->unit || decl->unit->module->is_generic);
	decl->unit = unit;

	switch (decl->decl_kind)
	{
		case DECL_ERASED:
			return;
		case DECL_POISONED:
			break;
		case DECL_MACRO:
			ASSERT0(decl->name);
			if (decl->func_decl.type_parent)
			{
				if (type_infoptr(decl->func_decl.type_parent)->kind == TYPE_INFO_GENERIC)
				{
					vec_add(unit->generic_methods_to_register, decl);
					return;
				}
				vec_add(unit->methods_to_register, decl);
				return;
			}
			else
			{
				vec_add(unit->macros, decl);
			}
			decl_register(decl);
			break;
		case DECL_FUNC:
			ASSERT0(decl->name);
			if (decl->func_decl.type_parent)
			{
				if (type_infoptr(decl->func_decl.type_parent)->kind == TYPE_INFO_GENERIC)
				{
					vec_add(unit->generic_methods_to_register, decl);
					return;
				}
				vec_add(unit->methods_to_register, decl);
				return;
			}
			else
			{
				vec_add(unit->functions, decl);
			}
			decl_register(decl);
			break;
		case DECL_VAR:
			ASSERT0(decl->name);
			vec_add(unit->vars, decl);
			decl_register(decl);
			break;
		case DECL_INTERFACE:
		case DECL_DISTINCT:
		case DECL_STRUCT:
		case DECL_UNION:
		case DECL_TYPEDEF:
		case DECL_FAULT:
		case DECL_BITSTRUCT:
			ASSERT0(decl->name);
			vec_add(unit->types, decl);
			decl_register(decl);
			break;
		case DECL_DEFINE:
			ASSERT0(decl->name);
			vec_add(unit->generic_defines, decl);
			decl_register(decl);
			break;
		case DECL_ENUM:
			ASSERT0(decl->name);
			vec_add(unit->enums, decl);
			decl_register(decl);
			break;
		case DECL_ATTRIBUTE:
			vec_add(unit->attributes, decl);
			decl_register(decl);
			break;
		case DECL_FAULTVALUE:
		case DECL_ENUM_CONSTANT:
		case DECL_IMPORT:
		case DECL_LABEL:
		case DECL_DECLARRAY:
		case DECL_BODYPARAM:
		case DECL_GLOBALS:
		case DECL_FNTYPE:
			UNREACHABLE
		case DECL_CT_EXEC:
		case DECL_CT_INCLUDE:
			vec_add(unit->ct_includes, decl);
			return;
		case DECL_CT_ECHO:
			vec_add(unit->ct_echos, decl);
			return;
		case DECL_CT_ASSERT:
			vec_add(unit->ct_asserts, decl);
			return;
	}
	DEBUG_LOG("Registering symbol '%s' in %s.", decl->name, unit->module->name->module);

	Decl *old;
	if ((old = htable_set(&unit->local_symbols, (void*)decl->name, decl))) goto ERR;
	if (decl->visibility < VISIBLE_LOCAL)
	{
		if ((old = htable_set(&unit->module->symbols, (void*)decl->name, decl))) goto ERR;
	}
	return;
ERR:
	ASSERT0(decl != old);
	sema_shadow_error(NULL, decl, old);
	decl_poison(decl);
	decl_poison(old);
}


bool unit_add_import(CompilationUnit *unit, Path *path, bool private_import, bool is_non_recursive)
{
	DEBUG_LOG("SEMA: Add import of '%s'.", path->module);

	if (!check_module_name(path)) return false;

	Decl *import = decl_calloc();
	import->span = path->span;
	import->decl_kind = DECL_IMPORT;
	import->import.path = path;
	import->import.import_private_as_public = private_import;
	import->import.is_non_recurse = is_non_recursive;
	vec_add(unit->imports, import);
	DEBUG_LOG("Added import %s", path->module);
	return true;
}

