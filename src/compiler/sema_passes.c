// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"


void sema_analysis_pass_process_imports(Module *module)
{
	DEBUG_LOG("Pass: Importing dependencies for files in module '%s'.", module->name->module);

	unsigned import_count = 0;
	VECEACH(module->contexts, index)
	{
		// 1. Loop through each context in the module.
		Context *context = module->contexts[index];
		DEBUG_LOG("Checking imports for %s.", context->file->name);

		// 2. Loop through imports
		unsigned imports = vec_size(context->imports);

		for (unsigned i = 0; i < imports; i++)
		{
			// 3. Begin analysis
			Decl *import = context->imports[i];
			assert(import->resolve_status == RESOLVE_NOT_DONE);
			import->resolve_status = RESOLVE_RUNNING;

			// 4. Find the module.
			Path *path = import->import.path;
			Module *import_module = global_context_find_module(path->module);

			// 5. Do we find it?
			if (!import_module)
			{
				SEMA_ERROR(import, "No module named '%s' could be found, did you type the name right?", path->module);
				decl_poison(import);
				continue;
			}

			// 6. Importing itself is not allowed.
			if (import_module == module)
			{
				SEMA_ERROR(import, "Importing the current module is not allowed, you need to remove it.");
				decl_poison(import);
				continue;
			}

			// 7. Importing private is not allowed.
			if (import_module->is_private && !import->import.private)
			{
				SEMA_ERROR(import, "Importing a private module is not allowed (unless 'import private' is used).");
				decl_poison(import);
				continue;
			}

			// 8. Assign the module.
			DEBUG_LOG("* Import of %s.", path->module);
			import->module = import_module;
			for (unsigned j = 0; j < i; j++)
			{
				// 9. We might run into multiple imports of the same package.
				if (import->module == context->imports[j]->module)
				{
					SEMA_ERROR(import, "Module '%s' was imported more than once, please remove the duplicates.", path->module);
					SEMA_PREV(context->imports[j], "Previous import was here");
					decl_poison(import);
					break;
				}
			}
		}
		import_count += imports;
	}
	(void)import_count; // workaround for clang 13.0
	DEBUG_LOG("Pass finished processing %d import(s) with %d error(s).", import_count, global_context.errors_found);
}

void sema_analysis_pass_register_globals(Module *module)
{
	DEBUG_LOG("Pass: Register globals for module '%s'.", module->name->module);

	VECEACH(module->contexts, index)
	{
		Context *context = module->contexts[index];
		context->module = module;
		DEBUG_LOG("Processing %s.", context->file->name);
		Decl **decls = context->global_decls;
		VECEACH(decls, i)
		{
			context_register_global_decl(context, decls[i]);
		}
		vec_resize(context->global_decls, 0);
	}

	DEBUG_LOG("Pass finished with %d error(s).", global_context.errors_found);
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
			sema_append_decls(context, ct_elif->ct_else_decl);
			return true;
		}
	}
	return true;
}


void sema_analysis_pass_conditional_compilation(Module *module)
{

	DEBUG_LOG("Pass: Top level conditionals %s", module->name->module);
	VECEACH(module->contexts, index)
	{
		Context *context = module->contexts[index];
		for (unsigned i = 0; i < vec_size(context->ct_ifs); i++)
		{
			// Also handle switch!
			sema_analyse_top_level_if(context, context->ct_ifs[i]);
		}
	}
	DEBUG_LOG("Pass finished with %d error(s).", global_context.errors_found);
}

void sema_analysis_pass_ct_assert(Module *module)
{
	DEBUG_LOG("Pass: $assert checks %s", module->name->module);
	VECEACH(module->contexts, index)
	{
		Context *context = module->contexts[index];
		VECEACH(context->ct_asserts, i)
		{
			sema_analyse_ct_assert_stmt(context, context->ct_asserts[i]->ct_assert_decl);
		}
	}
	DEBUG_LOG("Pass finished with %d error(s).", global_context.errors_found);
}

static inline bool analyse_func_body(Context *context, Decl *decl)
{
	if (!decl->func_decl.body) return true;
	if (!sema_analyse_function_body(context, decl)) return decl_poison(decl);
	return true;
}

void sema_analysis_pass_decls(Module *module)
{
	DEBUG_LOG("Pass: Decl analysis %s", module->name->module);

	VECEACH(module->contexts, index)
	{
		Context *context = module->contexts[index];
		context->active_scope = (DynamicScope)
				{
					.depth = 0,
					.scope_id = 0,
					.local_decl_start = &context->locals[0],
					.current_local = &context->locals[0],
				};
		context->macro_scope = (MacroScope) { 0 };
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
		VECEACH(context->generics, i)
		{
			sema_analyse_decl(context, context->generics[i]);
		}
		VECEACH(context->methods, i)
		{
			sema_analyse_decl(context, context->methods[i]);
		}
		VECEACH(context->macro_methods, i)
		{
			sema_analyse_decl(context, context->macro_methods[i]);
		}
		VECEACH(context->vars, i)
		{
			sema_analyse_decl(context, context->vars[i]);
		}
		VECEACH(context->functions, i)
		{
			sema_analyse_decl(context, context->functions[i]);
		}
		if (context->main_function)
		{
			sema_analyse_decl(context, context->main_function);
		}
		VECEACH(context->generic_defines, i)
		{
			sema_analyse_decl(context, context->generic_defines[i]);
		}
	}
	DEBUG_LOG("Pass finished with %d error(s).", global_context.errors_found);
}

void sema_analysis_pass_functions(Module *module)
{
	DEBUG_LOG("Pass: Function analysis %s", module->name->module);

	VECEACH(module->contexts, index)
	{
		Context *context = module->contexts[index];
		VECEACH(context->methods, i)
		{
			analyse_func_body(context, context->methods[i]);
		}
		VECEACH(context->functions, i)
		{
			analyse_func_body(context, context->functions[i]);
		}
		if (context->main_function) analyse_func_body(context, context->main_function);

	}

	DEBUG_LOG("Pass finished with %d error(s).", global_context.errors_found);
}
