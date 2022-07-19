// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"

void parent_path(StringSlice *slice)
{
	for (int i = (int)slice->len - 1; i >= 0; i--)
	{
		if (slice->ptr[i] == ':')
		{
			slice->len = i - 1;
			return;
		}
	}
	slice->len = 0;
}

void sema_analyse_pass_top(Module *module)
{
	Module *parent = module;
	while (parent->parent_module) parent = parent->parent_module;
	module->top_module = parent;
}

void sema_analyse_pass_module_hierarchy(Module *module)
{
	const char *name = module->name->module;
	StringSlice slice = slice_from_string(name);
	// foo::bar::baz -> foo::bar
	parent_path(&slice);
	// foo -> return, no parent
	if (!slice.len) return;


	unsigned module_count = vec_size(global_context.module_list);
	for (int i = 0; i < module_count; i++)
	{
		Module *checked = global_context.module_list[i];
		Path *checked_name = checked->name;
		if (checked_name->len != slice.len) continue;
		// Found the parent! We're done, we add this parent
		// and this as a child.
		if (memcmp(checked_name->module, slice.ptr, slice.len) == 0)
		{
			module->parent_module = checked;
			vec_add(checked->sub_modules, module);
			return;
		}
	}
	// No match, so we create a synthetic module.
	Path *path = path_create_from_string(slice.ptr, slice.len, module->name->span);
	DEBUG_LOG("Creating parent module for %s: %s", module->name->module, path->module);
	Module *parent_module = compiler_find_or_create_module(path, NULL, false /* always public */);
	module->parent_module = parent_module;
	vec_add(parent_module->sub_modules, module);
	sema_analyze_stage(parent_module, ANALYSIS_MODULE_HIERARCHY);
}


void sema_analysis_pass_process_imports(Module *module)
{
	DEBUG_LOG("Pass: Importing dependencies for files in module '%s'.", module->name->module);

	unsigned import_count = 0;
	VECEACH(module->units, index)
	{
		// 1. Loop through each context in the module.
		CompilationUnit *unit = module->units[index];
		DEBUG_LOG("Checking imports for %s.", unit->file->name);

		// 2. Loop through imports
		unsigned imports = vec_size(unit->imports);

		for (unsigned i = 0; i < imports; i++)
		{
			// 3. Begin analysis
			Decl *import = unit->imports[i];
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
		}
		import_count += imports;
	}
	(void)import_count; // workaround for clang 13.0
	DEBUG_LOG("Pass finished processing %d import(s) with %d error(s).", import_count, global_context.errors_found);
}

void sema_analysis_pass_register_globals(Module *module)
{
	DEBUG_LOG("Pass: Register globals for module '%s'.", module->name->module);

	VECEACH(module->units, index)
	{
		CompilationUnit *unit = module->units[index];
		unit->module = module;
		DEBUG_LOG("Processing %s.", unit->file->name);
		Decl **decls = unit->global_decls;
		VECEACH(decls, i)
		{
			unit_register_global_decl(unit, decls[i]);
		}
		vec_resize(unit->global_decls, 0);
	}

	DEBUG_LOG("Pass finished with %d error(s).", global_context.errors_found);
}

static inline void sema_append_decls(CompilationUnit *unit, Decl **decls)
{
	VECEACH(decls, i)
	{
		unit_register_global_decl(unit, decls[i]);
	}
}

static inline bool sema_analyse_top_level_if(SemaContext *context, Decl *ct_if)
{
	int res = sema_check_comp_time_bool(context, ct_if->ct_if_decl.expr);
	if (res == -1) return false;
	if (res)
	{
		// Append declarations
		sema_append_decls(context->unit, ct_if->ct_if_decl.then);
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
				sema_append_decls(context->unit, ct_elif->ct_elif_decl.then);
				return true;
			}
			ct_elif = ct_elif->ct_elif_decl.elif;
		}
		else
		{
			assert(ct_elif->decl_kind == DECL_CT_ELSE);
			sema_append_decls(context->unit, ct_elif->ct_else_decl);
			return true;
		}
	}
	return true;
}

static inline bool sema_analyse_top_level_switch(SemaContext *context, Decl *ct_switch)
{
	Expr *cond = ct_switch->ct_switch_decl.expr;
	if (!sema_analyse_ct_expr(context, cond)) return false;
	Type *type = cond->type;
	bool is_type = type == type_typeid;
	ExprConst *switch_expr_const = &cond->const_expr;
	Decl **cases = ct_switch->ct_switch_decl.cases;

	unsigned case_count = vec_size(cases);
	int matched_case = (int)case_count;
	int default_case = (int)case_count;
	for (unsigned i = 0; i < case_count; i++)
	{
		Decl *kase = cases[i];
		Expr *expr = kase->ct_case_decl.expr;
		Expr *to_expr = kase->ct_case_decl.to_expr;
		if (expr)
		{
			if (is_type)
			{
				if (!sema_analyse_ct_expr(context, expr)) return false;
				if (expr->type != type_typeid)
				{
					SEMA_ERROR(expr, "A type was expected here not %s.", type_quoted_error_string(expr->type));
					return false;
				}
			}
			else
			{
				if (!sema_analyse_expr_rhs(context, type, expr, false)) return false;
				if (to_expr && !sema_analyse_expr_rhs(context, type, to_expr, false)) return false;
			}
			if (expr->expr_kind != EXPR_CONST)
			{
				SEMA_ERROR(expr, "The $case must have a constant expression.");
				return false;
			}
			if (to_expr && to_expr->expr_kind != EXPR_CONST)
			{
				SEMA_ERROR(to_expr, "The $case must have a constant expression.");
				return false;
			}
			ExprConst *const_expr = &expr->const_expr;
			ExprConst *const_to_expr = to_expr ? &to_expr->const_expr : const_expr;
			if (to_expr && expr_const_compare(const_expr, const_to_expr, BINARYOP_GT))
			{
				SEMA_ERROR(to_expr, "The end of a range must be less or equal to the beginning.");
				return false;
			}
			// Check that it is unique.
			for (unsigned j = 0; j < i; j++)
			{
				Decl *other_case = cases[j];

				// Default.
				if (!other_case->ct_case_decl.expr) continue;
				ExprConst *other_const = &other_case->ct_case_decl.expr->const_expr;
				ExprConst *other_const_to = other_case->ct_case_decl.to_expr
						? &other_case->ct_case_decl.to_expr->const_expr : other_const;
				if (expr_const_compare(const_expr, other_const_to, BINARYOP_LE) &&
				expr_const_compare(const_to_expr, other_const, BINARYOP_GE))
				{
					SEMA_ERROR(kase, "'%s' appears more than once.", expr_const_to_error_string(const_expr));
					SEMA_PREV(cases[j]->ct_case_decl.expr, "The previous $case was here.");
					return false;
				}
			}
			if (expr_const_compare(switch_expr_const, const_expr, BINARYOP_GE) &&
			expr_const_compare(switch_expr_const, const_to_expr, BINARYOP_LE))
			{
				matched_case = (int)i;
			}
		}
		else
		{
			if (default_case < case_count)
			{
				SEMA_ERROR(kase, "More than one $default is not allowed.");
				SEMA_PREV(cases[default_case], "The previous $default was here.");
				return false;
			}
			default_case = (int)i;
			continue;
		}
	}

	if (matched_case == case_count) matched_case = default_case;

	for (int i = matched_case; i < case_count; i++)
	{
		Decl **body = cases[i]->ct_case_decl.body;
		if (body)
		{
			sema_append_decls(context->unit, body);
			break;
		}
	}
	return true;
}

void sema_analysis_pass_conditional_compilation(Module *module)
{

	DEBUG_LOG("Pass: Top level conditionals %s", module->name->module);
	VECEACH(module->units, index)
	{
		CompilationUnit *unit = module->units[index];
		for (unsigned i = 0; i < vec_size(unit->ct_ifs); i++)
		{
			// Also handle switch!
			SemaContext context;
			sema_context_init(&context, unit);
			Decl *decl = unit->ct_ifs[i];
			switch (decl->decl_kind)
			{
				case DECL_CT_IF:
					sema_analyse_top_level_if(&context, decl);
					break;
				case DECL_CT_SWITCH:
					sema_analyse_top_level_switch(&context, decl);
					break;
				default:
					UNREACHABLE
			}
			sema_context_destroy(&context);
		}
	}
	DEBUG_LOG("Pass finished with %d error(s).", global_context.errors_found);
}

void sema_analysis_pass_ct_assert(Module *module)
{
	DEBUG_LOG("Pass: $assert checks %s", module->name->module);
	VECEACH(module->units, index)
	{
		SemaContext context;
		sema_context_init(&context, module->units[index]);
		Decl **asserts = context.unit->ct_asserts;
		VECEACH(asserts, i)
		{
			sema_analyse_ct_assert_stmt(&context, asserts[i]->ct_assert_decl);
		}
		sema_context_destroy(&context);
	}
	DEBUG_LOG("Pass finished with %d error(s).", global_context.errors_found);
}

static inline bool analyse_func_body(SemaContext *context, Decl *decl)
{
	if (!decl->func_decl.body) return true;
	if (!sema_analyse_function_body(context, decl)) return decl_poison(decl);
	return true;
}

void sema_analysis_pass_decls(Module *module)
{
	DEBUG_LOG("Pass: Decl analysis %s", module->name->module);

	VECEACH(module->units, index)
	{
		CompilationUnit *unit = module->units[index];
		SemaContext context;
		sema_context_init(&context, unit);
		context.active_scope = (DynamicScope)
				{
					.depth = 0,
					.scope_id = 0,
					.local_decl_start = 0,
					.current_local = 0,
				};
		VECEACH(unit->attributes, i)
		{
			sema_analyse_decl(&context, unit->attributes[i]);
		}
		VECEACH(unit->enums, i)
		{
			sema_analyse_decl(&context, unit->enums[i]);
		}
		VECEACH(unit->types, i)
		{
			sema_analyse_decl(&context, unit->types[i]);
		}
		VECEACH(unit->macros, i)
		{
			sema_analyse_decl(&context, unit->macros[i]);
		}
		VECEACH(unit->generics, i)
		{
			sema_analyse_decl(&context, unit->generics[i]);
		}
		VECEACH(unit->methods, i)
		{
			sema_analyse_decl(&context, unit->methods[i]);
		}
		VECEACH(unit->macro_methods, i)
		{
			sema_analyse_decl(&context, unit->macro_methods[i]);
		}
		VECEACH(unit->vars, i)
		{
			sema_analyse_decl(&context, unit->vars[i]);
		}
		VECEACH(unit->functions, i)
		{
			sema_analyse_decl(&context, unit->functions[i]);
		}
		if (unit->main_function)
		{
			sema_analyse_decl(&context, unit->main_function);
		}
		VECEACH(unit->generic_defines, i)
		{
			sema_analyse_decl(&context, unit->generic_defines[i]);
		}
		sema_context_destroy(&context);
	}
	DEBUG_LOG("Pass finished with %d error(s).", global_context.errors_found);
}

void sema_analysis_pass_functions(Module *module)
{
	DEBUG_LOG("Pass: Function analysis %s", module->name->module);

	VECEACH(module->units, index)
	{
		CompilationUnit *unit = module->units[index];
		SemaContext context;
		sema_context_init(&context, unit);
		VECEACH(unit->methods, i)
		{
			analyse_func_body(&context, unit->methods[i]);
		}
		VECEACH(unit->functions, i)
		{
			analyse_func_body(&context, unit->functions[i]);
		}
		if (unit->main_function) analyse_func_body(&context, unit->main_function);
		sema_context_destroy(&context);

	}

	DEBUG_LOG("Pass finished with %d error(s).", global_context.errors_found);
}
