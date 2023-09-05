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
	Module *parent_module = compiler_find_or_create_module(path, NULL);
	module->parent_module = parent_module;
	vec_add(parent_module->sub_modules, module);
	sema_analyze_stage(parent_module, ANALYSIS_MODULE_HIERARCHY);
}


void sema_analysis_pass_process_imports(Module *module)
{
	DEBUG_LOG("Pass: Importing dependencies for files in module '%s'.", module->name->module);

	unsigned total_import_count = 0;
	VECEACH(module->units, index)
	{
		// 1. Loop through each context in the module.
		CompilationUnit *unit = module->units[index];
		DEBUG_LOG("Checking imports for %s.", unit->file->name);

		// 2. Loop through imports
		Decl **imports = unit->imports;
		unsigned import_count = vec_size(imports);

		for (unsigned i = 0; i < import_count; i++)
		{
			// 3. Begin analysis
			Decl *import = imports[i];
			assert(import->resolve_status == RESOLVE_NOT_DONE);
			import->resolve_status = RESOLVE_RUNNING;
			// 4. Find the module.
			Path *path = import->import.path;

			for (unsigned j = 0; j < i; j++)
			{
				if (imports[j]->import.path->module == path->module)
				{
					SEMA_ERROR(import, "Module '%s' imported more than once, please remove one.", path->module);
					SEMA_NOTE(imports[j], "The previous one was here.");
					decl_poison(import);
					goto NEXT;
				}
			}

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

			// 7. Assign the module.
			DEBUG_LOG("* Import of %s.", path->module);
			import->import.module = import_module;
NEXT:;
		}
		total_import_count += import_count;
	}
	(void)total_import_count; // workaround for clang 13.0
	DEBUG_LOG("Pass finished processing %d import(s) with %d error(s).", total_import_count, global_context.errors_found);
}

INLINE void register_global_decls(CompilationUnit *unit, Decl **decls)
{
	VECEACH(decls, i)
	{
		unit_register_global_decl(unit, decls[i]);
	}
	vec_resize(decls, 0);
}

INLINE File *sema_load_file(CompilationUnit *unit, SourceSpan span, Expr *filename, const char *type, File *no_file)
{
	if (!expr_is_const_string(filename))
	{
		SEMA_ERROR(filename, "A compile time string was expected.");
		return NULL;
	}
	const char *string = filename->const_expr.bytes.ptr;
	bool loaded;
	const char *error;
	char *path;
	char *name;
	if (file_namesplit(unit->file->full_path, &name, &path))
	{
		string = file_append_path(path, string);
	}
	File *file = source_file_load(string, &loaded, &error);
	if (!file)
	{
		if (no_file) return no_file;
		sema_error_at(span, "Failed to load file %s: %s", string, error);
		return NULL;
	}
	if (global_context.errors_found) return NULL;
	return file;
}

static Decl **sema_load_include(CompilationUnit *unit, Decl *decl)
{
	if (active_target.trust_level < TRUST_INCLUDE)
	{
		SEMA_ERROR(decl, "'$include' not permitted, trust level must be set to '-t2' or '-t3' to permit it.");
		return NULL;
	}
	SemaContext context;
	sema_context_init(&context, unit);
	FOREACH_BEGIN(Attr *attr, decl->attributes)
		if (attr->attr_kind != ATTRIBUTE_IF)
		{
			SEMA_ERROR(attr, "Invalid attribute for '$include'.");
			return NULL;
		}
	FOREACH_END();
	bool success = sema_analyse_ct_expr(&context, decl->include.filename);
	sema_context_destroy(&context);
	if (success) return NULL;
	File *file = sema_load_file(unit, decl->span,  decl->include.filename, "$include", NULL);
	if (!file) return NULL;
	if (global_context.includes_used++ > MAX_INCLUDES)
	{
		SEMA_ERROR(decl, "This $include would cause the maximum number of includes (%d) to be exceeded.", MAX_INCLUDES);
		return NULL;
	}
	return parse_include_file(file, unit);
}

static Decl **sema_run_exec(CompilationUnit *unit, Decl *decl)
{
	if (active_target.trust_level < TRUST_FULL)
	{
		SEMA_ERROR(decl, "'$exec' not permitted, trust level must be set to '-t3' to permit it.");
		return NULL;
	}
	SemaContext context;
	sema_context_init(&context, unit);
	FOREACH_BEGIN(Attr *attr, decl->attributes)
		if (attr->attr_kind != ATTRIBUTE_IF)
		{
			SEMA_ERROR(attr, "Invalid attribute for '$exec'.");
			return NULL;
		}
	FOREACH_END();
	Expr *filename = decl->exec_decl.filename;
	bool success = sema_analyse_ct_expr(&context, filename);
	FOREACH_BEGIN(Expr *arg, decl->exec_decl.args)
		success &= sema_analyse_ct_expr(&context, arg);
	FOREACH_END();
	sema_context_destroy(&context);
	if (!success) return NULL;
	if (!expr_is_const_string(filename))
	{
		SEMA_ERROR(filename, "A filename was expected as the first argument to '$exec'.");
		return NULL;
	}
	scratch_buffer_clear();
	const char *file_str = filename->const_expr.bytes.ptr;
	bool c3_script = str_has_suffix(file_str, ".c3");
	if (!c3_script)
	{
		scratch_buffer_append(file_str);
		scratch_buffer_append(" ");
	}
	FOREACH_BEGIN_IDX(i, Expr *arg, decl->exec_decl.args)
		if (i) scratch_buffer_append(" ");
		assert(expr_is_const(arg));
		switch (arg->const_expr.const_kind)
		{
			case CONST_FLOAT:
				scratch_buffer_append_double(arg->const_expr.fxx.f);
				continue;
			case CONST_INTEGER:
				scratch_buffer_append(int_to_str(arg->const_expr.ixx, 10));
				continue;
			case CONST_BOOL:
				scratch_buffer_append(arg->const_expr.b ? "true" : "false");
				continue;
			case CONST_ENUM:
			case CONST_ERR:
				scratch_buffer_append(arg->const_expr.enum_err_val->name);
				continue;
			case CONST_TYPEID:
				if (!arg->const_expr.typeid->name)
				{
					SEMA_ERROR(arg, "The type '%s' has no trivial name.", type_quoted_error_string(arg->const_expr.typeid));
					return NULL;
				}
				scratch_buffer_append(arg->const_expr.typeid->name);
				continue;
			case CONST_STRING:
				scratch_buffer_append(arg->const_expr.bytes.ptr);
				continue;
			case CONST_POINTER:
				scratch_buffer_append_unsigned_int(arg->const_expr.ptr);
				continue;
			case CONST_BYTES:
			case CONST_INITIALIZER:
			case CONST_UNTYPED_LIST:
			case CONST_MEMBER:
				SEMA_ERROR(arg, "Bytes, initializers and member references may not be used as arguments.");
				return NULL;
		}
		UNREACHABLE
	FOREACH_END();
	File *file;
	if (c3_script)
	{
		file = compile_and_invoke(file_str, scratch_buffer_copy());
	}
	else
	{
		const char *output = execute_cmd(scratch_buffer_to_string());
		file = source_file_text_load(scratch_buffer_to_string(), output);
	}
	if (global_context.includes_used++ > MAX_INCLUDES)
	{
		SEMA_ERROR(decl, "This $include would cause the maximum number of includes (%d) to be exceeded.", MAX_INCLUDES);
		return NULL;
	}
	return parse_include_file(file, unit);
}

INLINE void register_includes(CompilationUnit *unit, Decl **decls)
{
	FOREACH_BEGIN(Decl *include, decls)
		Decl **include_decls = include->decl_kind == DECL_CT_EXEC ? sema_run_exec(unit, include) : sema_load_include(unit, include);
		VECEACH(include_decls, i)
		{
			Decl *decl = include_decls[i];
			if (decl->is_cond)
			{
				vec_add(unit->global_cond_decls, decl);
			}
			else
			{
				unit_register_global_decl(unit, decl);
			}
		}
	FOREACH_END();
}

void sema_process_includes(CompilationUnit *unit)
{
	while (1)
	{
		Decl **includes = unit->ct_includes;
		if (!includes) break;
		DEBUG_LOG("Processing includes in %s.", unit->file->name);
		unit->ct_includes = NULL;
		register_includes(unit, includes);
	}
}

void sema_analysis_pass_register_global_declarations(Module *module)
{
	DEBUG_LOG("Pass: Register globals for module '%s'.", module->name->module);
	if (str_eq("std::core::env", module->name->module))
	{
		printf("");
	}
	VECEACH(module->units, index)
	{
		CompilationUnit *unit = module->units[index];
		if (unit->if_attr) continue;
		assert(!unit->ct_includes);
		unit->module = module;
		DEBUG_LOG("Processing %s.", unit->file->name);
		register_global_decls(unit, unit->global_decls);

		// Process all includes
		sema_process_includes(unit);
		assert(vec_size(unit->ct_includes) == 0);
	}

	DEBUG_LOG("Pass finished with %d error(s).", global_context.errors_found);
}

void sema_analysis_pass_register_conditional_units(Module *module)
{
	DEBUG_LOG("Pass: Register conditional units for %s", module->name->module);
	VECEACH(module->units, index)
	{
		CompilationUnit *unit = module->units[index];
		// All ct_includes should already be registered.
		assert(!unit->ct_includes);

		Attr *if_attr = unit->if_attr;
		if (!if_attr) continue;
		if (vec_size(if_attr->exprs) != 1)
		{
			SEMA_ERROR(if_attr, "Expected one parameter.");
			break;
		}
		Expr *expr = if_attr->exprs[0];
		SemaContext context;
		sema_context_init(&context, unit);
		bool success = sema_analyse_ct_expr(&context, expr);
		sema_context_destroy(&context);
		if (!success) continue;
		if (!expr_is_const(expr) || expr->type->canonical != type_bool)
		{
			SEMA_ERROR(expr, "Expected a constant boolean expression.");
			break;
		}
		if (!expr->const_expr.b)
		{
			vec_resize(unit->global_decls, 0);
			vec_resize(unit->global_cond_decls, 0);
			continue;
		}
		register_global_decls(unit, unit->global_decls);
		// There may be includes, add those.
		sema_process_includes(unit);
	}
	DEBUG_LOG("Pass finished with %d error(s).", global_context.errors_found);
}

void sema_analysis_pass_register_conditional_declarations(Module *module)
{
	DEBUG_LOG("Pass: Register conditional declarations for module '%s'.", module->name->module);
	VECEACH(module->units, index)
	{
		CompilationUnit *unit = module->units[index];
		unit->module = module;
		DEBUG_LOG("Processing %s.", unit->file->name);
RETRY:;
		Decl **decls = unit->global_cond_decls;
		VECEACH(decls, i)
		{
			Decl *decl = decls[i];
			SemaContext context;
			sema_context_init(&context, unit);
			if (sema_decl_if_cond(&context, decl))
			{
				unit_register_global_decl(unit, decl);
			}
			sema_context_destroy(&context);
		}
		vec_resize(decls, 0);
RETRY_INCLUDES:
		decls = unit->ct_includes;
		unit->ct_includes = NULL;
		register_includes(unit, decls);
		if (unit->ct_includes) goto RETRY_INCLUDES;

		// We might have gotten more declarations.
		if (vec_size(unit->global_cond_decls) > 0) goto RETRY;
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
		bool success = true;
		VECEACH(asserts, i)
		{
			if (!sema_analyse_ct_assert_stmt(&context, asserts[i]->ct_assert_decl))
			{
				success = false;
				break;
			}
		}
		sema_context_destroy(&context);
		if (!success) break;
	}
	DEBUG_LOG("Pass finished with %d error(s).", global_context.errors_found);
}

void sema_analysis_pass_ct_echo(Module *module)
{
	DEBUG_LOG("Pass: $echo checks %s", module->name->module);
	VECEACH(module->units, index)
	{
		SemaContext context;
		sema_context_init(&context, module->units[index]);
		Decl **echos = context.unit->ct_echos;
		bool success = true;
		VECEACH(echos, i)
		{
			if (!sema_analyse_ct_echo_stmt(&context, echos[i]->ct_echo_decl))
			{
				success = false;
				break;
			}
		}
		sema_context_destroy(&context);
		if (!success) break;
	}
	DEBUG_LOG("Pass finished with %d error(s).", global_context.errors_found);
}

static inline bool analyse_func_body(SemaContext *context, Decl *decl)
{
	if (!decl->func_decl.body) return true;
	if (decl->is_extern)
	{
		SEMA_ERROR(decl, "'extern' functions should never have a body.");
		return decl_poison(decl);
	}
	// Don't analyse functions that are tests.
	if (decl->func_decl.attr_test && !active_target.testing) return true;

	// Don't analyse functions that are benchmarks.
	if (decl->func_decl.attr_benchmark && !active_target.benchmarking) return true;

	if (!sema_analyse_function_body(context, decl)) return decl_poison(decl);
	return true;
}

INLINE void sema_analyse_inner_func_ptr(SemaContext *c, Decl *decl)
{
	Type *inner;
	switch (decl->decl_kind)
	{
		case DECL_DISTINCT:
			inner = decl->distinct_decl.base_type;
			break;
		case DECL_TYPEDEF:
			inner = decl->type->canonical;
			break;
		default:
			return;
	}
	if (inner->type_kind != TYPE_POINTER) return;
	Type *pointer = inner->pointer;
	if (pointer->type_kind == TYPE_FUNC && !sema_resolve_type_decl(c, pointer)) decl_poison(decl);
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
					.label_start = 0,
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
			Decl *decl = unit->types[i];
			sema_analyse_decl(&context, decl);
			sema_analyse_inner_func_ptr(&context, decl);
		}
		VECEACH(unit->macros, i)
		{
			sema_analyse_decl(&context, unit->macros[i]);
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
		if (unit->main_function && unit->main_function->is_synthetic)
		{
			sema_analyse_decl(&context, unit->main_function);
		}
		VECEACH(unit->generic_defines, i)
		{
			sema_analyse_decl(&context, unit->generic_defines[i]);
		}
		FOREACH_BEGIN(TypeInfo *info, unit->check_type_variable_array)
			sema_check_type_variable_array(&context, info);
		FOREACH_END();
		sema_context_destroy(&context);
	}
	DEBUG_LOG("Pass finished with %d error(s).", global_context.errors_found);
}

void sema_analysis_pass_lambda(Module *module)
{
	DEBUG_LOG("Extra pass: Lambda analysis %s", module->name->module);

	VECEACH(module->units, index)
	{
		while (vec_size(module->lambdas_to_evaluate))
		{
			Decl *lambda = VECLAST(module->lambdas_to_evaluate);
			CompilationUnit *unit = lambda->unit;
			SemaContext context;
			sema_context_init(&context, unit);
			vec_pop(module->lambdas_to_evaluate);
			if (analyse_func_body(&context, lambda))
			{
				vec_add(unit->lambdas, lambda);
			}
			sema_context_destroy(&context);
		}
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
		VECEACH(unit->xxlizers, i)
		{
			sema_analyse_decl(&context, unit->xxlizers[i]);
		}
		VECEACH(unit->methods, i)
		{
			analyse_func_body(&context, unit->methods[i]);
		}
		VECEACH(unit->functions, i)
		{
			analyse_func_body(&context, unit->functions[i]);
		}
		if (unit->main_function && unit->main_function->is_synthetic) analyse_func_body(&context, unit->main_function);
		sema_context_destroy(&context);
	}

	DEBUG_LOG("Pass finished with %d error(s).", global_context.errors_found);
}
