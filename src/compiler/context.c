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


static inline bool create_module_or_check_name(CompilationUnit *unit, Path *module_name)
{
	Module *module = unit->module;
	if (!module) module = unit->module = compiler_find_or_create_module(module_name);
	if (unit->module->name->module != module_name->module)
	{
		RETURN_PRINT_ERROR_AT(false,
		                      module_name,
		                      "Module name here '%s' did not match actual module '%s'.",
		                      module_name->module,
		                      module->name->module);
	}
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
	if (last_was_underscore && scratch_buffer.len) scratch_buffer_delete(1);
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
	path->loc = make_loc(context->span);
	path->module = module_name;
	path->len = scratch_buffer.len;
	return create_module_or_check_name(context->unit, path);
}

bool context_set_module(ParseContext *context, Path *path)
{

	if (!check_module_name(path)) return false;
	return create_module_or_check_name(context->unit, path);
}

bool context_is_macro(SemaContext *context)
{
	if (context->current_macro != NULL) return true;
	return context->call_env.current_function && context->call_env.current_function->func_decl.in_macro; // NOLINT
}

void unit_register_external_symbol(SemaContext *context, Decl *decl)
{
	decl = decl_flatten(decl);
	if (decl->is_external_visible) return;
	Module *active_module = context->current_macro ? context->original_module : context->compilation_unit->module;
	if (decl->unit->module == active_module) return;
	decl->is_external_visible = true;
}

INLINE void weak_visibility_mismatch(Decl *weak_symbol, Decl *other_symbol)
{
	print_error_at(weak_symbol->loc, "The weak symbol '%s' has another definition with a different, visibility.", weak_symbol->name);
	SEMA_NOTE(other_symbol, "The other definition was here.");

}

INLINE bool decl_old_should_be_removed(Decl *old, Decl *decl)
{
	return old->is_weak && (!decl->is_weak || !decl->unit->is_interface_file);
}
void decl_register(CompilationUnit *unit, Decl *decl)
{
	if (decl->is_templated)
	{
		DEBUG_LOG("Registering generic symbol '%s' in %s.", decl->name, unit->module->name->module);
		Decl *instance = declptr(decl->instance_id);
		FOREACH (Decl *, other, instance->instance_decl.generated_decls)
		{
			if (other->name != decl->name) continue;
			if (other->visibility == VISIBLE_LOCAL && decl->visibility == VISIBLE_LOCAL && decl->unit != other->unit) continue;
			sema_shadow_error(NULL, decl, other);
			decl_poison(decl);
			decl_poison(other);
			return;
		}
		vec_add(declptr(decl->instance_id)->instance_decl.generated_decls, decl);
		return;
	}
	DEBUG_LOG("Registering symbol '%s' in %s.", decl->name, unit->module->name->module);

	Decl *old;
	Decl *replaced_symbol = NULL;
	if ((old = htable_set(&unit->local_symbols, (void*)decl->name, decl)))
	{
		if (old->decl_kind != decl->decl_kind || old->decl_kind == DECL_TYPE_ALIAS) goto SHADOW_LOCAL;
		// If we have a weak symbol we can replace it
		if (decl_old_should_be_removed(old, decl))
		{
			// Visibility must match
			if (old->visibility != decl->visibility)
			{
				weak_visibility_mismatch(old, decl);
				return;
			}
			replaced_symbol = old;
			old->replacement = decl;
			vec_add(unit->weak_symbols_skipped, old);
			goto WEAK_LOCAL;
		}
		if (decl->is_weak)
		{
			// If the current is weak, but the other isn't we just ignore it.
			// But we must verify visibility matching.
			if (old->visibility != decl->visibility)
			{
				weak_visibility_mismatch(old, decl);
				return;
			}
			// We restore the old one
			htable_set(&unit->local_symbols, (void*)decl->name, old);
			decl->replacement = old;
			vec_add(unit->weak_symbols_skipped, decl);
			return;
		}
SHADOW_LOCAL:
		sema_shadow_error(NULL, decl, old);
		decl_poison(decl);
		decl_poison(old);
		return;
	}
WEAK_LOCAL:
	if ((old = htable_set(&unit->module->symbols, (void*)decl->name, decl)))
	{
		if (old->visibility == VISIBLE_LOCAL && decl->visibility == VISIBLE_LOCAL) return;
		if (old->decl_kind != decl->decl_kind || old->decl_kind == DECL_TYPE_ALIAS) goto SHADOW_MODULE;
		// If we have a weak symbol we can replace it
		if (decl_old_should_be_removed(old, decl))
		{
			// Visibility must match
			if (old->visibility != decl->visibility)
			{
				weak_visibility_mismatch(old, decl);
				return;
			}
			// If we already replaced a weak symbol, it must be the same one!
			if (replaced_symbol) ASSERT(replaced_symbol == old);
			if (!replaced_symbol) vec_add(old->unit->weak_symbols_skipped, old);
			replaced_symbol = old;
			old->replacement = decl;
			if (unit != old->unit)
			{
				// We need to pretend the weak symbol is this symbol in the local scope.
				htable_set(&old->unit->local_symbols, (void*)old->name, decl);
			}
			goto WEAK_MODULE;
		}
		if (decl->is_weak)
		{
			// If the current is weak, but the other isn't we just ignore it.
			// But we must verify visibility matching.
			if (old->visibility != decl->visibility)
			{
				weak_visibility_mismatch(old, decl);
				return;
			}
			// We restore the old one
			decl->replacement = old;
			vec_add(unit->weak_symbols_skipped, decl);
			htable_set(&unit->module->symbols, (void*)decl->name, old);
			// Pretend the old is this in the local symbol scope
			htable_set(&unit->local_symbols, (void*)decl->name, old);
			return;
		}
SHADOW_MODULE:
		if (old->visibility == VISIBLE_LOCAL)
		{
			sema_shadow_error(NULL, old, decl);
		}
		else
		{
			sema_shadow_error(NULL, decl, old);
		}
		decl_poison(decl);
		decl_poison(old);
		return;
	}
WEAK_MODULE:
	if (decl->visibility < VISIBLE_LOCAL)
	{
		switch (decl->decl_kind)
		{
			case DECL_ERASED:
			case DECL_ALIAS_PATH:
			case DECL_BODYPARAM:
			case DECL_CT_ASSERT:
			case DECL_CT_ECHO:
			case DECL_CT_EXEC:
			case DECL_CT_INCLUDE:
			case DECL_DECLARRAY:
			case DECL_ENUM_CONSTANT:
			case DECL_GROUP:
			case DECL_GENERIC:
			case DECL_GENERIC_INSTANCE:
			case DECL_IMPORT:
			case DECL_LABEL:
			case DECL_POISONED:
			case DECL_CONTRACT:
				UNREACHABLE_VOID
			case DECL_ATTRIBUTE:
			case DECL_BITSTRUCT:
			case DECL_TYPEDEF:
			case DECL_ENUM:
			case DECL_CONSTDEF:
			case DECL_STRUCT:
			case DECL_TYPE_ALIAS:
			case DECL_UNION:
			case DECL_ALIAS:
			case DECL_FUNC:
			case DECL_MACRO:
			case DECL_VAR:
			case DECL_FNTYPE:
			case DECL_INTERFACE:
			case DECL_FAULT:
				if (replaced_symbol)
				{
					global_context_replace_decl(replaced_symbol, decl);
					break;
				}
				global_context_add_decl(decl);
				break;
		}
	}

}


void unit_register_global_decl(CompilationUnit *unit, Decl *decl)
{
	ASSERT_SPAN(decl, !decl->is_template);
	ASSERT_SPAN(decl, !decl->unit || decl->is_templated);
	decl->unit = unit;
	switch (decl->decl_kind)
	{
		case DECL_ERASED:
			return;
		case DECL_POISONED:
			break;
		case DECL_MACRO:
			ASSERT(decl->name);
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
			vec_add(unit->macros, decl);
			decl_register(unit, decl);
			return;
		case DECL_FUNC:
			ASSERT(decl->name);
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
			vec_add(unit->functions, decl);
			decl_register(unit, decl);
			return;
		case DECL_VAR:
			ASSERT(decl->name);
			vec_add(unit->vars, decl);
			decl_register(unit, decl);
			return;
		case DECL_INTERFACE:
		case DECL_TYPEDEF:
		case DECL_STRUCT:
		case DECL_UNION:
		case DECL_TYPE_ALIAS:
		case DECL_BITSTRUCT:
			ASSERT(decl->name);
			vec_add(unit->types, decl);
			decl_register(unit, decl);
			return;
		case DECL_FAULT:
			ASSERT(decl->name);
			vec_add(unit->faults, decl);
			decl_register(unit, decl);
			return;
		case DECL_ALIAS:
			ASSERT(decl->name);
			vec_add(unit->generic_defines, decl);
			decl_register(unit, decl);
			return;
		case DECL_CONSTDEF:
		case DECL_ENUM:
			ASSERT(decl->name);
			vec_add(unit->enums, decl);
			decl_register(unit, decl);
			return;
		case DECL_ATTRIBUTE:
			vec_add(unit->attributes, decl);
			decl_register(unit, decl);
			return;
		case DECL_ALIAS_PATH:
		case DECL_BODYPARAM:
		case DECL_DECLARRAY:
		case DECL_ENUM_CONSTANT:
		case DECL_FNTYPE:
		case DECL_GROUP:
		case DECL_GENERIC:
		case DECL_GENERIC_INSTANCE:
		case DECL_IMPORT:
		case DECL_LABEL:
		case DECL_CONTRACT:
			UNREACHABLE_VOID
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
	UNREACHABLE_VOID;
}


bool unit_add_import(CompilationUnit *unit, Path *path, bool private_import, bool is_non_recursive)
{
	DEBUG_LOG("SEMA: Add import of '%s'.", path->module);

	if (!check_module_name(path)) return false;

	Decl *import = decl_calloc();
	import->loc = path->loc;
	import->decl_kind = DECL_IMPORT;
	import->import.path = path;
	import->import.import_private_as_public = private_import;
	import->import.is_non_recurse = is_non_recursive;
	vec_add(unit->imports, import);
	if (private_import) vec_add(unit->public_imports, import);
	DEBUG_LOG("Added import %s", path->module);
	return true;
}

bool unit_add_alias(CompilationUnit *unit, Decl *alias)
{
	DEBUG_LOG("SEMA: Add module alias '%s'.", alias->name);

	if (!check_module_name(alias->module_alias_decl.alias_path)) return false;
	vec_add(unit->module_aliases, alias);
	return true;
}

