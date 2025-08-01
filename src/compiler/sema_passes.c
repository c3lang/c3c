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


	unsigned module_count = vec_size(compiler.context.module_list);
	for (int i = 0; i < module_count; i++)
	{
		Module *checked = compiler.context.module_list[i];
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
	FOREACH(CompilationUnit *, unit, module->units)
	{
		// 1. Loop through each context in the module.
		DEBUG_LOG("Checking imports for %s.", unit->file->name);

		// 2. Loop through imports
		Decl **imports = unit->imports;
		unsigned import_count = vec_size(imports);

		for (unsigned i = 0; i < import_count; i++)
		{
			// 3. Begin analysis
			Decl *import = imports[i];
			ASSERT(import->resolve_status == RESOLVE_NOT_DONE);
			import->resolve_status = RESOLVE_RUNNING;
			// 4. Find the module.
			Path *path = import->import.path;

			for (unsigned j = 0; j < i; j++)
			{
				if (imports[j]->import.path->module == path->module)
				{
					PRINT_ERROR_AT(import, "Module '%s' imported more than once, please remove one.", path->module);
					SEMA_NOTE(imports[j], "The previous one was here.");
					decl_poison(import);
					goto NEXT;
				}
			}

			Module *import_module = global_context_find_module(path->module);

			// 5. Do we find it?
			if (!import_module)
			{
				PRINT_ERROR_AT(import, "No module named '%s' could be found, did you type the name right?", path->module);
				decl_poison(import);
				continue;
			}

			// 6. Importing itself is not allowed.
			if (import_module == module)
			{
				PRINT_ERROR_AT(import, "Importing the current module is not allowed, you need to remove it.");
				decl_poison(import);
				continue;
			}

			// 7. Assign the module.
			DEBUG_LOG("* Import of %s.", path->module);
			import->import.module = import_module;
NEXT:;
		}
		FOREACH_IDX(idx, Decl *, alias_module,  unit->module_aliases)
		{
			Path *path = alias_module->module_alias_decl.alias_path;
			Module *import_module = global_context_find_module(path->module);

			// 5. Do we find it?
			if (!import_module)
			{
				PRINT_ERROR_AT(path, "No module named '%s' could be found, did you type the name right?", path->module);
				continue;
			}
			alias_module->module_alias_decl.module = import_module;
			alias_module->resolve_status = RESOLVE_DONE;
			for (unsigned i = 0; i < idx; i++)
			{
				if (unit->module_aliases[i]->name == alias_module->name)
				{
					PRINT_ERROR_AT(alias_module, "The module alias must be unique.");
					break;
				}
			}
			if (alias_module->attributes)
			{
				PRINT_ERROR_AT(alias_module->attributes[0], "Module aliases cannot have attributes.");
				break;
			}
		}
		total_import_count += import_count;
	}
	(void)total_import_count; // workaround for clang 13.0
	DEBUG_LOG("Pass finished processing %d import(s) with %d error(s).", total_import_count, compiler.context.errors_found);
}

INLINE void register_global_decls(CompilationUnit *unit, Decl **decls)
{
	FOREACH(Decl *, decl, decls)
	{
		unit_register_global_decl(unit, decl);
	}
	vec_resize(decls, 0);
}

INLINE File *sema_load_file(CompilationUnit *unit, Expr *filename)
{
	if (!expr_is_const_string(filename))
	{
		RETURN_PRINT_ERROR_AT(NULL, filename, "A compile time string was expected.");
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
		print_error_at(filename->span, "Failed to load file '%s': %s.", filename->const_expr.bytes.ptr, error);
		return NULL;
	}
	if (compiler.context.errors_found) return NULL;
	return file;
}

static Decl **sema_load_include(CompilationUnit *unit, Decl *decl)
{
	if (compiler.build.trust_level < TRUST_INCLUDE)
	{
		RETURN_PRINT_ERROR_AT(NULL, decl, "'$include' not permitted, trust level must be set to '--trust=include' or '--trust=full' to permit it.");
	}
	SemaContext context;
	sema_context_init(&context, unit);
	FOREACH(Attr *, attr, decl->attributes)
	{
		if (attr->attr_kind != ATTRIBUTE_IF)
		{
			RETURN_PRINT_ERROR_AT(NULL, attr, "Invalid attribute for '$include'.");
		}
	}
	bool success = sema_analyse_ct_expr(&context, decl->include.filename);
	sema_context_destroy(&context);
	if (!success) return NULL;
	File *file = sema_load_file(unit, decl->include.filename);
	if (!file) return NULL;
	if (compiler.context.includes_used++ > MAX_INCLUDE_DIRECTIVES)
	{
		RETURN_PRINT_ERROR_AT(NULL, decl, "This $include would cause the maximum number of includes (%d) to be exceeded.", MAX_INCLUDE_DIRECTIVES);
	}
	return parse_include_file(file, unit);
}

static bool exec_arg_append_to_scratch(Expr *arg)
{
	ASSERT(expr_is_const(arg));
	switch (arg->const_expr.const_kind)
	{
		case CONST_FLOAT:
			scratch_buffer_append_double(arg->const_expr.fxx.f);
			return true;
		case CONST_INTEGER:
			scratch_buffer_append(int_to_str(arg->const_expr.ixx, 10, false));
			return true;
		case CONST_BOOL:
			scratch_buffer_append(arg->const_expr.b ? "true" : "false");
			return true;
		case CONST_REF:
			scratch_buffer_append(arg->const_expr.global_ref->name);
			return true;
		case CONST_FAULT:
			scratch_buffer_append(arg->const_expr.fault ? arg->const_expr.fault->name : "null");
		return true;
		case CONST_ENUM:
			scratch_buffer_append(arg->const_expr.enum_val->name);
			return true;
		case CONST_TYPEID:
			if (!arg->const_expr.typeid->name)
			{
				RETURN_PRINT_ERROR_AT(false, arg, "The type '%s' has no trivial name.",
				                      type_quoted_error_string(arg->const_expr.typeid));
			}
			scratch_buffer_append(arg->const_expr.typeid->name);
			return true;
		case CONST_STRING:
			scratch_buffer_append(arg->const_expr.bytes.ptr);
			return true;
		case CONST_POINTER:
			scratch_buffer_append_unsigned_int(arg->const_expr.ptr);
			return true;
		case CONST_BYTES:
		case CONST_INITIALIZER:
		case CONST_SLICE:
		case CONST_UNTYPED_LIST:
		case CONST_MEMBER:
			return false;
	}
	UNREACHABLE
}

static Decl **sema_run_exec(CompilationUnit *unit, Decl *decl)
{
	if (compiler.build.trust_level < TRUST_FULL)
	{
		RETURN_PRINT_ERROR_AT(NULL, decl, "'$exec' not permitted, trust level must be set to '--trust=full' to permit it.");
	}
	SemaContext context;
	sema_context_init(&context, unit);
	FOREACH(Attr *, attr, decl->attributes)
	{
		if (attr->attr_kind != ATTRIBUTE_IF)
		{
			RETURN_PRINT_ERROR_AT(NULL, attr, "Invalid attribute for '$exec'.");
		}
	}
	Expr *filename = decl->exec_decl.filename;
	bool success = sema_analyse_ct_expr(&context, filename);
	FOREACH(Expr *, arg, decl->exec_decl.args) success &= sema_analyse_ct_expr(&context, arg);
	Expr *stdin_expr = decl->exec_decl.stdin_string;
	if (stdin_expr) success &= sema_analyse_ct_expr(&context, stdin_expr);
	sema_context_destroy(&context);
	if (!success) return NULL;
	if (!expr_is_const_string(filename))
	{
		RETURN_PRINT_ERROR_AT(NULL, filename, "A filename was expected as the first argument to '$exec'.");
	}
	const char *stdin_string = NULL;
	if (stdin_expr)
	{
		if (!expr_is_const_string(stdin_expr)) RETURN_PRINT_ERROR_AT(NULL, stdin_expr, "Expected the stdin parameter to be a compile time string.");
		stdin_string = stdin_expr->const_expr.bytes.ptr;
	}
	scratch_buffer_clear();
	const char *file_str = filename->const_expr.bytes.ptr;
	bool c3_script = str_has_suffix(file_str, ".c3");
	if (!c3_script)
	{
		scratch_buffer_append(file_str);
		scratch_buffer_append(" ");
	}
	FOREACH_IDX(i, Expr *, arg, decl->exec_decl.args)
	{
		if (i) scratch_buffer_append(" ");
		ASSERT(expr_is_const(arg));
		if (!exec_arg_append_to_scratch(arg))
		{
			RETURN_PRINT_ERROR_AT(NULL, arg, "Bytes, initializers and member references may not be used as arguments.");
		}
	}
	File *file;
	char old_path_buffer[PATH_MAX]; // NOLINT
	char *old_path = NULL;
	if (compiler.build.script_dir)
	{
		old_path = getcwd(old_path_buffer, PATH_MAX);
		if (!dir_change(compiler.build.script_dir))
		{
			RETURN_PRINT_ERROR_AT(NULL, decl, "Failed to open script dir '%s'", compiler.build.script_dir);
		}
	}
	if (c3_script)
	{
		file = compile_and_invoke(file_str, scratch_buffer_copy(), stdin_string, 0);
	}
	else
	{
		char *output = execute_cmd(scratch_buffer_to_string(), false, stdin_string, 0);
		file = source_file_text_load(scratch_buffer_to_string(), output);
	}
	if (old_path)
	{
		success = dir_change(old_path);
		if (!success)
		{
			RETURN_PRINT_ERROR_AT(NULL, decl, "Failed to open run dir '%s'", compiler.build.script_dir);
		}
	}
	if (compiler.context.includes_used++ > MAX_INCLUDE_DIRECTIVES)
	{
		RETURN_PRINT_ERROR_AT(NULL, decl, "This $include would cause the maximum number of includes (%d) to be exceeded.", MAX_INCLUDE_DIRECTIVES);
	}
	return parse_include_file(file, unit);
}

INLINE void register_includes(CompilationUnit *unit, Decl **decls)
{
	FOREACH(Decl *, include, decls)
	{
		Decl **include_decls;
		switch (include->decl_kind)
		{
			case DECL_CT_EXEC:
				include_decls = sema_run_exec(unit, include);
				break;
			case DECL_CT_INCLUDE:
				include_decls = sema_load_include(unit, include);
				break;
			default:
				UNREACHABLE
		}
		FOREACH(Decl *, decl, include_decls)
		{
			if (decl->is_cond)
			{
				vec_add(unit->global_cond_decls, decl);
			}
			else
			{
				unit_register_global_decl(unit, decl);
			}
		}
	}
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
	FOREACH(CompilationUnit *, unit, module->units)
	{
		if (unit->if_attr) continue;
		ASSERT(!unit->ct_includes);
		unit->module = module;
		DEBUG_LOG("Processing %s.", unit->file->name);
		register_global_decls(unit, unit->global_decls);
	}
	DEBUG_LOG("Pass finished with %d error(s).", compiler.context.errors_found);
}

void sema_analysis_pass_process_includes(Module *module)
{
	DEBUG_LOG("Pass: Process includes for module '%s'.", module->name->module);
	FOREACH(CompilationUnit *, unit, module->units)
	{
		if (unit->if_attr) continue;
		// Process all includes
		sema_process_includes(unit);
		ASSERT(vec_size(unit->ct_includes) == 0);
	}

	DEBUG_LOG("Pass finished with %d error(s).", compiler.context.errors_found);
}


void sema_analysis_pass_process_methods(Module *module, bool process_generic)
{
	DEBUG_LOG("Pass: Process methods register for module '%s'.", module->name->module);
	FOREACH(CompilationUnit *, unit, module->units)
	{
		SemaContext context;
		sema_context_init(&context, unit);
		FOREACH(Decl *, method, process_generic ? unit->generic_methods_to_register : unit->methods_to_register)
		{
			TypeInfo *parent_type_info = type_infoptr(method->func_decl.type_parent);
			if (!process_generic && sema_unresolved_type_is_generic(&context, parent_type_info))
			{
				vec_add(unit->generic_methods_to_register, method);
				continue;
			}
			sema_analyse_method_register(&context, method);
			if (method->decl_kind == DECL_MACRO)
			{
				vec_add(unit->macro_methods, method);
			}
			else
			{
				vec_add(unit->methods, method);
			}
		}
		sema_context_destroy(&context);
		if (process_generic)
		{
			vec_resize(unit->generic_methods_to_register, 0);
		}
		else
		{
			vec_resize(unit->methods_to_register, 0);
		}
	}

	DEBUG_LOG("Pass finished with %d error(s).", compiler.context.errors_found);
}

void sema_analysis_pass_register_conditional_units(Module *module)
{
	DEBUG_LOG("Pass: Register conditional units for %s", module->name->module);
	FOREACH(CompilationUnit *, unit, module->units)
	{
		// All ct_includes should already be registered.
		ASSERT(!unit->ct_includes);

		Attr *if_attr = unit->if_attr;
		if (!if_attr && !unit->attr_links) continue;

		SemaContext context;
		sema_context_init(&context, unit);
		if (!if_attr) goto CHECK_LINK;
		if (vec_size(if_attr->exprs) != 1)
		{
			PRINT_ERROR_AT(if_attr, "Expected one parameter.");
			goto FAIL_CONTEXT;
		}
		Expr *expr = if_attr->exprs[0];
		if (!sema_analyse_ct_expr(&context, expr)) goto FAIL_CONTEXT;
		if (!sema_cast_const(expr) || expr->type->canonical != type_bool)
		{
			PRINT_ERROR_AT(expr, "Expected a constant boolean expression.");
			goto FAIL_CONTEXT;
		}
		if (!expr->const_expr.b)
		{
			vec_resize(unit->global_decls, 0);
			vec_resize(unit->global_cond_decls, 0);
			continue;
		}
CHECK_LINK:
		if (!unit->attr_links) goto RELEASE_CONTEXT;
		FOREACH(Attr*,  attr, unit->attr_links)
		{
			Expr **exprs = attr->exprs;
			unsigned args = vec_size(exprs);
			ASSERT(args > 0 && "Should already have been checked.");
			Expr *cond = args > 1 ? attr->exprs[0] : NULL;
			if (cond && !sema_analyse_expr(&context, cond)) goto FAIL_CONTEXT;
			bool start = cond && expr_is_const_bool(cond) ? 1 : 0;
			bool add = start == 0 ? true : cond->const_expr.b;
			for (unsigned i = start; i < args; i++)
			{
				Expr *string = attr->exprs[i];
				if (!sema_analyse_expr(&context, string)) goto FAIL_CONTEXT;
				if (!expr_is_const_string(string))
				{
					PRINT_ERROR_AT(string, "Expected a constant string here, usage is: "
					                       "'@link([cond1, ]link1, link2, ...)'.");
					goto FAIL_CONTEXT;
				}
				if (add)
				{
					vec_add(unit->links, string->const_expr.bytes.ptr);
				}
			}
		}
RELEASE_CONTEXT:
		sema_context_destroy(&context);
		register_global_decls(unit, unit->global_decls);
		// There may be includes, add those.
		sema_process_includes(unit);
		continue;
FAIL_CONTEXT:
		sema_context_destroy(&context);
		break;
	}
	DEBUG_LOG("Pass finished with %d error(s).", compiler.context.errors_found);
}

void sema_analysis_pass_register_conditional_declarations(Module *module)
{
	DEBUG_LOG("Pass: Register conditional declarations for module '%s'.", module->name->module);
	FOREACH(CompilationUnit *, unit, module->units)
	{
		unit->module = module;
		DEBUG_LOG("Processing %s.", unit->file->name);
RETRY:;
		Decl **decls = unit->global_cond_decls;
		FOREACH(Decl *, decl, decls)
		{
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
	DEBUG_LOG("Pass finished with %d error(s).", compiler.context.errors_found);
}

void sema_analysis_pass_ct_assert(Module *module)
{
	DEBUG_LOG("Pass: $assert checks %s", module->name->module);
	FOREACH(CompilationUnit *, unit, module->units)
	{
		SemaContext context;
		sema_context_init(&context, unit);
		bool success = true;
		FOREACH(Decl *, assert, context.unit->ct_asserts)
		{
			if (!sema_analyse_ct_assert_stmt(&context, assert->ct_assert_decl))
			{
				success = false;
				break;
			}
		}
		sema_context_destroy(&context);
		if (!success) break;
	}
	DEBUG_LOG("Pass finished with %d error(s).", compiler.context.errors_found);
}

void sema_analysis_pass_ct_echo(Module *module)
{
	DEBUG_LOG("Pass: $echo checks %s", module->name->module);
	FOREACH(CompilationUnit *, unit, module->units)
	{
		SemaContext context;
		sema_context_init(&context, unit);
		bool success = true;
		FOREACH(Decl *, echo, context.unit->ct_echos)
		{
			if (!sema_analyse_ct_echo_stmt(&context, echo->ct_echo_decl))
			{
				success = false;
				break;
			}
		}
		sema_context_destroy(&context);
		if (!success) break;
	}
	DEBUG_LOG("Pass finished with %d error(s).", compiler.context.errors_found);
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
	if (decl->func_decl.attr_test && !compiler.build.testing) return true;

	// Don't analyse functions that are benchmarks.
	if (decl->func_decl.attr_benchmark && !compiler.build.benchmarking) return true;

	if (!sema_analyse_function_body(context, decl)) return decl_poison(decl);
	return true;
}

INLINE void sema_analyse_inner_func_ptr(SemaContext *c, Decl *decl)
{
	Type *inner;
	switch (decl->decl_kind)
	{
		case DECL_DISTINCT:
			inner = decl->distinct->type;
			break;
		case DECL_TYPEDEF:
			inner = decl->type->canonical;
			break;
		default:
			return;
	}
	if (inner->type_kind != TYPE_FUNC_PTR) return;
	Type *func = inner->pointer;
	ASSERT(func->type_kind == TYPE_FUNC_RAW);
	if (!sema_resolve_type_decl(c, func)) decl_poison(decl);
}

INLINE void sema_analyse_decls(SemaContext *context, Decl **decls)
{
	FOREACH(Decl *, decl, decls)
	{
		sema_analyse_decl(context, decl);
	}
}

void sema_analysis_pass_decls(Module *module)
{
	DEBUG_LOG("Pass: Decl analysis %s", module->name->module);

	FOREACH(CompilationUnit *, unit, module->units)
	{
		SemaContext context;
		sema_context_init(&context, unit);
		context.active_scope = (DynamicScope)
				{
					.depth = 0,
					.scope_id = 0,
					.label_start = 0,
					.current_local = 0,
				};
		sema_analyse_decls(&context, unit->attributes);
		sema_analyse_decls(&context, unit->enums);
		FOREACH(Decl *, decl, unit->types)
		{
			sema_analyse_decl(&context, decl);
			sema_analyse_inner_func_ptr(&context, decl);
		}
		sema_analyse_decls(&context, unit->macros);
		sema_analyse_decls(&context, unit->methods);
		sema_analyse_decls(&context, unit->macro_methods);
		sema_analyse_decls(&context, unit->vars);
		sema_analyse_decls(&context, unit->functions);
		if (unit->main_function && unit->main_function->is_synthetic)
		{
			sema_analyse_decl(&context, unit->main_function);
		}
		sema_analyse_decls(&context, unit->generic_defines);
		FOREACH(TypeInfo *, info, unit->check_type_variable_array)
		{
			sema_check_type_variable_array(&context, info);
		}
		sema_context_destroy(&context);
	}
	DEBUG_LOG("Pass finished with %d error(s).", compiler.context.errors_found);
}

void sema_analysis_pass_lambda(Module *module)
{
	DEBUG_LOG("Extra pass: Lambda analysis %s", module->name->module);

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

	DEBUG_LOG("Pass finished with %d error(s).", compiler.context.errors_found);
}

static bool sema_check_interface(SemaContext *context, Decl *decl, TypeInfo *interface_type, TypeInfo *original_type)
{
	Decl *interface = interface_type->type->decl;
	FOREACH(Decl *, method, interface->interface_methods)
	{
		Decl *matching_method = sema_decl_stack_resolve_symbol(method->name);
		if (!matching_method)
		{
			if (method->func_decl.attr_optional) continue;
			if (interface_type != original_type)
			{
				RETURN_SEMA_ERROR(original_type,
				                  "'%s' was not fully implemented, the required method '%s' of interface '%s' needs to be implemented, did you forget it?",
				                  original_type->type->decl->name, method->name, interface->name);
			}
			RETURN_SEMA_ERROR(original_type,
			                  "'%s' was not fully implemented, the required method '%s' needs to be implemented, did you forget it?",
			                  interface->name, method->name);
		}
		if (matching_method->decl_kind != DECL_FUNC)
		{
			if (method->func_decl.attr_optional) continue;
			RETURN_SEMA_ERROR(matching_method, "'%s' was not fully implemented, it requires '%s' to be a function marked '@dynamic'.",
					   interface->name, method->name);
		}
		if (!matching_method->func_decl.attr_dynamic)
		{
			SEMA_ERROR(matching_method, "'%s(...)' must be marked '@dynamic' as it matches the method '%s' in interface '%s'.",
					   method->name, method->name, interface->name);
			SEMA_NOTE(method, "Here is the interface method to implement.");
			return false;
		}
	}
	FOREACH(TypeInfo *, parent_interface, interface->interfaces)
	{
		if (!sema_check_interface(context, decl, parent_interface, original_type)) return false;
	}
	return true;
}
static inline bool sema_check_interfaces(SemaContext *context, Decl *decl)
{
	Decl **store = sema_decl_stack_store();
	FOREACH(Decl *, method, decl->methods) sema_decl_stack_push(method);
	FOREACH(TypeInfo *, interface_type, decl->interfaces)
	{
		if (!sema_check_interface(context, decl, interface_type, interface_type))
		{
			sema_decl_stack_restore(store);
			return false;
		}
	}
	sema_decl_stack_restore(store);
	return true;
}

void sema_analysis_pass_interface(Module *module)
{
	DEBUG_LOG("Pass: Interface analysis %s", module->name->module);

	FOREACH(CompilationUnit *, unit, module->units)
	{
		SemaContext context;
		sema_context_init(&context, unit);
		FOREACH(Decl *, decl, unit->types)
		{
			switch (decl->decl_kind)
			{
				case DECL_DISTINCT:
				case DECL_STRUCT:
				case DECL_UNION:
				case DECL_ENUM:
				case DECL_BITSTRUCT:
					break;
				default:
					continue;
			}
			if (decl->interfaces)
			{
				sema_check_interfaces(&context, decl);
			}
		}
		sema_context_destroy(&context);
	}

	DEBUG_LOG("Pass finished with %d error(s).", compiler.context.errors_found);
}

void sema_analysis_pass_functions(Module *module)
{
	DEBUG_LOG("Pass: Function analysis %s", module->name->module);

	FOREACH(CompilationUnit *, unit, module->units)
	{
		SemaContext context;
		sema_context_init(&context, unit);
		FOREACH(Decl *, method, unit->methods)
		{
			analyse_func_body(&context, method);
		}
		FOREACH(Decl *, func, unit->functions)
		{
			analyse_func_body(&context, func);
		}
		if (unit->main_function && unit->main_function->is_synthetic) analyse_func_body(&context, unit->main_function);
		sema_context_destroy(&context);
	}

	DEBUG_LOG("Pass finished with %d error(s).", compiler.context.errors_found);
}
