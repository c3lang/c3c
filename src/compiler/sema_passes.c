// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"

void sema_analysis_pass_process_imports(Context *context)
{
	DEBUG_LOG("Pass: Importing dependencies for %s", context->file->name);
	unsigned imports = vec_size(context->imports);
	for (unsigned i = 0; i < imports; i++)
	{
		Decl *import = context->imports[i];
		import->resolve_status = RESOLVE_RUNNING;
		Path *path = import->import.path;
		Module *module = stable_get(&compiler.modules, path->module);
		DEBUG_LOG("- Import of %s.", path->module);
		if (!module)
		{
			SEMA_ERROR(import, "No module named '%s' could be found.", path->module);
			decl_poison(import);
			continue;
		}
		import->module = module;
		for (unsigned j = 0; j < i; j++)
		{
			if (import->module == context->imports[j]->module)
			{
				SEMA_ERROR(import, "Module '%s' imported more than once.", path->module);
				SEMA_PREV(context->imports[i], "Previous import was here");
				decl_poison(import);
				break;
			}
		}
	}
	DEBUG_LOG("Pass finished with %d error(s).", diagnostics.errors);
}

void sema_analysis_pass_register_globals(Context *context)
{
	DEBUG_LOG("Pass: Register globals for %s", context->file->name);
	VECEACH(context->global_decls, i)
	{
		context_register_global_decl(context, context->global_decls[i]);
	}
	vec_resize(context->global_decls, 0);
	DEBUG_LOG("Pass finished with %d error(s).", diagnostics.errors);
}

static inline void sema_append_decls(Context *context, Decl **decls)
{
	VECEACH(decls, i)
	{
		context_register_global_decl(context, decls[i]);
	}
}

static inline bool sema_analyse_top_level_if(Context *context, Decl *ct_if)
{
	int res = sema_check_comp_time_bool(context, ct_if->ct_if_decl.expr);
	if (res == -1) return false;
	if (res)
	{
		// Append declarations
		sema_append_decls(context, ct_if->ct_if_decl.then);
		return true;
	}

	// False, so check elifs
	Decl *ct_elif = ct_if->ct_if_decl.elif;
	while (ct_elif)
	{
		if (ct_elif->decl_kind == DECL_CT_ELIF)
		{
			res = sema_check_comp_time_bool(context, ct_elif->ct_elif_decl.expr);
			if (res == -1) return false;
			if (res)
			{
				sema_append_decls(context, ct_elif->ct_elif_decl.then);
				return true;
			}
			ct_elif = ct_elif->ct_elif_decl.elif;
		}
		else
		{
			assert(ct_elif->decl_kind == DECL_CT_ELSE);
			sema_append_decls(context, ct_elif->ct_elif_decl.then);
			return true;
		}
	}
	return true;
}


void sema_analysis_pass_conditional_compilation(Context *context)
{
	DEBUG_LOG("Pass: Top level conditionals %s", context->file->name);
	for (unsigned i = 0; i < vec_size(context->ct_ifs); i++)
	{
		// Also handle switch!
		sema_analyse_top_level_if(context, context->ct_ifs[i]);
	}
	DEBUG_LOG("Pass finished with %d error(s).", diagnostics.errors);
}

void sema_analysis_pass_ct_assert(Context *context)
{
	DEBUG_LOG("Pass: $assert checks %s", context->file->name);
	VECEACH(context->ct_asserts, i)
	{
		sema_analyse_ct_assert_stmt(context, context->ct_asserts[i]);
	}
	DEBUG_LOG("Pass finished with %d error(s).", diagnostics.errors);
}

static inline bool analyse_func_body(Context *context, Decl *decl)
{
	if (!decl->func.body) return true;
	if (!sema_analyse_function_body(context, decl)) return decl_poison(decl);
	return true;
}

void sema_analysis_pass_decls(Context *context)
{
	DEBUG_LOG("Pass: Decl analysis %s", context->file->name);
	context->current_scope = &context->scopes[0];
	context->current_scope->scope_id = 0;
	context->last_local = &context->locals[0];
	VECEACH(context->enums, i)
	{
		sema_analyse_decl(context, context->enums[i]);
	}
	VECEACH(context->types, i)
	{
		sema_analyse_decl(context, context->types[i]);
	}
	VECEACH(context->macros, i)
	{
		sema_analyse_decl(context, context->macros[i]);
	}
	VECEACH(context->methods, i)
	{
		sema_analyse_decl(context, context->methods[i]);
	}
	VECEACH(context->vars, i)
	{
		sema_analyse_decl(context, context->vars[i]);
	}
	VECEACH(context->functions, i)
	{
		sema_analyse_decl(context, context->functions[i]);
	}
	VECEACH(context->generic_defines, i)
	{
		sema_analyse_decl(context, context->generic_defines[i]);
	}
	DEBUG_LOG("Pass finished with %d error(s).", diagnostics.errors);
}

void sema_analysis_pass_functions(Context *context)
{
	DEBUG_LOG("Pass: Function analysis %s", context->file->name);

	VECEACH(context->methods, i)
	{
		analyse_func_body(context, context->methods[i]);
	}
	VECEACH(context->functions, i)
	{
		analyse_func_body(context, context->functions[i]);
	}

	DEBUG_LOG("Pass finished with %d error(s).", diagnostics.errors);
}
