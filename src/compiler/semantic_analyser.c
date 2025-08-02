// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include <compiler_tests/benchmark.h>
#include "sema_internal.h"

char swizzle[256] = { ['x'] = 0x01, ['y'] = 0x02, ['z'] = 0x03, ['w'] = 0x04,
					  ['r'] = 0x11, ['g'] = 0x12, ['b'] = 0x13, ['a'] = 0x14 };

void context_change_scope_with_flags(SemaContext *context, ScopeFlags flags)
{
	unsigned depth = context->active_scope.depth + 1;
	if (depth > MAX_SCOPE_DEPTH)
	{
		FATAL_ERROR("Too deeply nested scopes.");
	}

	bool scope_is_dead = context->active_scope.is_dead;
	bool scope_is_poisoned = context->active_scope.is_poisoned;
	Ast *previous_defer = context->active_scope.in_defer;
	AstId parent_defer = context->active_scope.defer_last;
	unsigned last_local = context->active_scope.current_local;
	// Defer and expression blocks introduce their own return/break/continue
	// otherwise just merge with the old flags.
	if (flags & SCOPE_MACRO)
	{
		previous_defer = 0;
		parent_defer = 0;
	}
	bool new_label_scope = (flags & SCOPE_MACRO) != 0;
	bool is_macro = (flags & SCOPE_MACRO) != 0;
	flags = context->active_scope.flags | flags;
	if (is_macro) flags &= ~(SCOPE_ENSURE | SCOPE_ENSURE_MACRO);

	unsigned label_start = new_label_scope ? last_local : context->active_scope.label_start;
	context->active_scope = (DynamicScope) {
			.scope_id = ++context->scope_id,
			.allow_dead_code = false,
			.is_dead = scope_is_dead,
			.is_poisoned = scope_is_poisoned,
			.depth = depth,
			.current_local = last_local,
			.label_start = label_start,
			.in_defer = previous_defer,
			.defer_last = parent_defer,
			.defer_start = parent_defer,
			.flags = flags,
	};
	if (context->scope_id == 0)
	{
		FATAL_ERROR("Too many scopes.");
	}
}

const char *context_filename(SemaContext *context)
{
	CompilationUnit *unit = context->unit;
	if (!unit) return "<unknown unit>";
	File *file = unit->file;
	if (!file || !file->full_path) return "<unknown file>";
	return file->full_path;
}

void context_change_scope_for_label(SemaContext *context, DeclId label_id)
{
	context_change_scope_with_flags(context, SCOPE_NONE);

	if (label_id)
	{
		Decl *label = declptr(label_id);
		label->label.defer = context->active_scope.defer_last;
		sema_add_local(context, label);
		label->label.scope_defer = astid(context->active_scope.in_defer);
	}
}


AstId context_get_defers(SemaContext *context, AstId defer_bottom, bool is_success)
{
	AstId defer_top = context->active_scope.defer_last;
	AstId first = 0;
	AstId *next = &first;
	while (defer_bottom != defer_top)
	{
		Ast *defer = astptr(defer_top);
		if ((is_success && defer->defer_stmt.is_catch) || (!is_success && defer->defer_stmt.is_try))
		{
			defer_top = defer->defer_stmt.prev_defer;
			continue;
		}
		Ast *defer_body = copy_ast_defer(astptr(defer->defer_stmt.body));
		*next = astid(defer_body);
		next = &defer_body->next;
		defer_top = defer->defer_stmt.prev_defer;
	}
	return first;
}

void context_pop_defers(SemaContext *context, AstId *next)
{
	AstId defer_start = context->active_scope.defer_start;
	if (next && !context->active_scope.end_jump.active)
	{
		AstId defer_current = context->active_scope.defer_last;
		while (defer_current != defer_start)
		{
			Ast *defer = astptr(defer_current);
			if (!defer->defer_stmt.is_catch)
			{
				Ast *defer_body = copy_ast_defer(astptr(defer->defer_stmt.body));
				*next = astid(defer_body);
				next = &defer_body->next;
			}
			defer_current = defer->defer_stmt.prev_defer;
		}
	}
	context->active_scope.defer_last = defer_start;
}


void context_pop_defers_and_replace_ast(SemaContext *context, Ast *ast)
{
	AstId defer_first = 0;
	context_pop_defers(context, &defer_first);
	if (!defer_first) return;
	ASSERT(ast->ast_kind != AST_COMPOUND_STMT);
	Ast *replacement = ast_copy(ast);
	ast->ast_kind = AST_COMPOUND_STMT;
	ast->compound_stmt = (AstCompoundStmt) { .first_stmt = astid(replacement) };
	replacement->next = defer_first;
}

static inline void halt_on_error(void)
{
	if (compiler.context.errors_found > 0)
	{
		if (compiler.build.lsp_output)
		{
			eprintf("> ENDLSP-ERROR\n");
			exit_compiler(COMPILER_SUCCESS_EXIT);
		}
		exit_compiler(EXIT_FAILURE);
	}
}

void sema_analyze_stage(Module *module, AnalysisStage stage)
{
	while (module->stage < stage)
	{
		compiler.context.decl_stack_bottom = compiler.context.decl_stack_top = compiler.context.decl_stack;
		module->stage++;
		switch (module->stage)
		{
			case ANALYSIS_NOT_BEGUN:
				UNREACHABLE
			case ANALYSIS_MODULE_HIERARCHY:
				sema_analyse_pass_module_hierarchy(module);
				break;
			case ANALYSIS_MODULE_TOP:
				sema_analyse_pass_top(module);
				break;
			case ANALYSIS_IMPORTS:
				sema_analysis_pass_process_imports(module);
				break;
			case ANALYSIS_REGISTER_GLOBAL_DECLARATIONS:
				sema_analysis_pass_register_global_declarations(module);
				break;
			case ANALYSIS_INCLUDES:
				sema_analysis_pass_process_includes(module);
				break;
			case ANALYSIS_REGISTER_CONDITIONAL_UNITS:
				sema_analysis_pass_register_conditional_units(module);
				break;
			case ANALYSIS_REGISTER_CONDITIONAL_DECLARATIONS:
				sema_analysis_pass_register_conditional_declarations(module);
				break;
			case ANALYSIS_METHODS_REGISTER:
				sema_analysis_pass_process_methods(module, false);
				break;
			case ANALYSIS_METHODS_REGISTER_GENERIC:
				sema_analysis_pass_process_methods(module, true);
				break;
			case ANALYSIS_METHODS_INCLUDES:
				sema_analysis_pass_process_methods(module, false);
				break;
			case ANALYSIS_METHODS_INCLUDES_GENERIC:
				sema_analysis_pass_process_methods(module, true);
				break;
			case ANALYSIS_METHODS_CONDITIONAL:
				sema_analysis_pass_process_methods(module, false);
				break;
			case ANALYSIS_METHODS_CONDITIONAL_GENERIC:
				sema_analysis_pass_process_methods(module, true);
				break;
			case ANALYSIS_POST_REGISTER:
				break;
			case ANALYSIS_DECLS:
				sema_analysis_pass_decls(module);
				break;
			case ANALYSIS_CT_ECHO:
				sema_analysis_pass_ct_echo(module);
				break;
			case ANALYSIS_CT_ASSERT:
				sema_analysis_pass_ct_assert(module);
				break;
			case ANALYSIS_FUNCTIONS:
				sema_analysis_pass_functions(module);
				break;
			case ANALYSIS_INTERFACE:
				sema_analysis_pass_interface(module);
				break;
			case ANALYSIS_FINALIZE:
				break;
		}
		if (compiler.context.errors_found) return;
	}
}

static void register_generic_decls(CompilationUnit *unit, Decl **decls)
{
	FOREACH(Decl *, decl, decls)
	{
		decl->unit = unit;
		switch (decl->decl_kind)
		{
			case DECL_ALIAS_PATH:
			case DECL_CT_ASSERT:
			case DECL_CT_ECHO:
			case DECL_CT_EXEC:
			case DECL_CT_INCLUDE:
			case DECL_FNTYPE:
			case DECL_IMPORT:
			case DECL_POISONED:
				continue;
			case DECL_FAULT:
				PRINT_ERROR_AT(decl, "Generic modules cannot use 'faultdef', place the declaration in a separate sub module or parent module instead.");
				decl_poison(decl);
				break;
			case DECL_BODYPARAM:
			case DECL_DECLARRAY:
			case DECL_ENUM_CONSTANT:
			case DECL_ERASED:
			case DECL_GROUP:
			case DECL_LABEL:
				UNREACHABLE
			case DECL_ALIAS:
			case DECL_ATTRIBUTE:
			case DECL_BITSTRUCT:
			case DECL_CONST_ENUM:
			case DECL_DISTINCT:
			case DECL_ENUM:
			case DECL_INTERFACE:
			case DECL_STRUCT:
			case DECL_TYPEDEF:
			case DECL_UNION:
			case DECL_VAR:
				break;
			case DECL_MACRO:
			case DECL_FUNC:
				if (decl->func_decl.type_parent) continue;
				break;
		}
		htable_set(&unit->module->symbols, (void *)decl->name, decl);
		if (decl->visibility == VISIBLE_PUBLIC)
		{
			global_context_add_generic_decl(decl);
		}
	}
}

static void analyze_generic_module(Module *module)
{
	ASSERT(module->parameters && module->is_generic);
	FOREACH(CompilationUnit *, unit, module->units)
	{
		register_generic_decls(unit, unit->global_decls);
		register_generic_decls(unit, unit->global_cond_decls);
	}
}

static void sema_analyze_to_stage(AnalysisStage stage)
{
	if (stage <= ANALYSIS_MODULE_TOP)
	{
		FOREACH(Module *, module, compiler.context.generic_module_list)
		{
			sema_analyze_stage(module, stage);
		}
	}
	FOREACH(Module *, module, compiler.context.module_list)
	{
		sema_analyze_stage(module, stage);
	}
	halt_on_error();
}

static bool setup_main_runner(Decl *run_function)
{
	SemaContext context;
	sema_context_init(&context, run_function->unit);
	Decl *main = sema_create_runner_main(&context, run_function);
	if (!decl_ok(main)) return false;
	if (!sema_analyse_decl(&context, main)) return false;
	if (!sema_analyse_function_body(&context, main)) return false;
	sema_context_destroy(&context);
	compiler.context.main = main;
	main->unit->main_function = main;
	main->no_strip = true;
	return true;
}
static void assign_panicfn(void)
{
	if (compiler.build.feature.panic_level == PANIC_OFF || (!compiler.build.panicfn && no_stdlib()))
	{
		compiler.context.panic_var = NULL;
		compiler.context.panicf = NULL;
		return;
	}

	const char *panicfn = compiler.build.panicfn ? compiler.build.panicfn : "std::core::builtin::panic";
	Path *path;
	const char *ident;
	if (sema_splitpathref(panicfn, strlen(panicfn), &path, &ident) != TOKEN_IDENT || path == NULL || !ident)
	{
		error_exit("'%s' is not a valid panic function.", panicfn);
	}
	Decl *decl = sema_find_decl_in_modules(compiler.context.module_list, path, ident);
	if (!decl)
	{
		error_exit("Panic function pointer '%s::%s' could not be found.", path->module, ident);
	}
	Type *panic_fn_type = decl->type->canonical;
	if (decl->decl_kind != DECL_VAR || !type_is_func_ptr(panic_fn_type))
	{
		error_exit("'%s::%s' is not a function pointer.", path->module, ident);
	}
	if (!type_func_match(panic_fn_type, type_void, 4, type_string, type_string, type_string, type_uint))
	{
		error_exit("Expected panic function to have the signature fn void(String, String, String, uint).");
	}
	compiler.context.panic_var = decl;
	decl->no_strip = true;

	if (no_stdlib()) return;

	const char *panicf = "std::core::builtin::panicf";
	if (sema_splitpathref(panicf, strlen(panicf), &path, &ident) != TOKEN_IDENT || path == NULL || !ident)
	{
		error_exit("'%s' is not a valid panicf function.", panicf);
	}
	Decl *panicf_decl = sema_find_decl_in_modules(compiler.context.module_list, path, ident);
	if (!panicf_decl)
	{
		compiler.context.panicf = NULL;
		return;
	}

	panicf_decl->no_strip = true;

	Type *panicf_fn_type = panicf_decl->type->canonical;
	if (panicf_decl->decl_kind != DECL_FUNC)
	{
		error_exit("'%s' is not a function function.", panicf);
	}
	if (!type_func_match(type_get_func_ptr(panicf_fn_type), type_void, 5, type_string, type_string, type_string, type_uint,
	                     type_get_slice(type_any)))
	{
		error_exit("Expected panic function to have the signature fn void(String, String, String, uint, ...).");
	}
	compiler.context.panicf = panicf_decl;
}

static void assign_testfn(void)
{
	if (!compiler.build.testing) return;
	if (!compiler.build.testfn && no_stdlib())
	{
		error_exit("No test function could be found.");
	}
	const char *testfn = compiler.build.testfn ? compiler.build.testfn : "std::core::runtime::default_test_runner";
	Path *path;
	const char *ident;
	if (sema_splitpathref(testfn, strlen(testfn), &path, &ident) != TOKEN_IDENT || path == NULL || !ident)
	{
		error_exit("'%s' is not a valid test function.", testfn);
	}
	Decl *decl = sema_find_decl_in_modules(compiler.context.module_list, path, ident);
	if (!decl)
	{
		error_exit("Test function '%s::%s' could not be found.", path->module, ident);
	}
	if (decl->decl_kind != DECL_FUNC)
	{
		error_exit("'%s::%s' is not a function.", path->module, ident);
	}
	if (!type_func_match(type_get_func_ptr(decl->type->canonical), type_bool, 1, type_get_slice(type_string)))
	{
		error_exit("Expected test runner to have the signature fn bool(String[]).");
	}
	decl->no_strip = true;
	if (compiler.build.type != TARGET_TYPE_TEST) return;

	if (!setup_main_runner(decl))
	{
		error_exit("Failed to set up test runner.");
	}
}

static void assign_benchfn(void)
{
	if (!compiler.build.benchmarking) return;
	if (!compiler.build.benchfn && no_stdlib())
	{
		return;
	}
	const char *testfn = compiler.build.benchfn ? compiler.build.benchfn : "std::core::runtime::default_benchmark_runner";
	Path *path;
	const char *ident;
	if (sema_splitpathref(testfn, strlen(testfn), &path, &ident) != TOKEN_IDENT || path == NULL || !ident)
	{
		error_exit("'%s' is not a valid benchmark function.", testfn);
	}
	Decl *decl = sema_find_decl_in_modules(compiler.context.module_list, path, ident);
	if (!decl)
	{
		error_exit("Benchmark function '%s::%s' could not be found.", path->module, ident);
	}
	if (decl->decl_kind != DECL_FUNC)
	{
		error_exit("'%s::%s' is not a function.", path->module, ident);
	}
	if (!type_func_match(type_get_func_ptr(decl->type->canonical), type_bool, 1, type_get_slice(type_string)))
	{
		error_exit("Expected benchmark function to have the signature fn bool(String[] args).");
	}
	decl->no_strip = true;
	if (!setup_main_runner(decl))
	{
		error_exit("Failed to set up benchmark runner.");
	}
}

/**
 * Perform the entire semantic analysis.
 */
void sema_analysis_run(void)
{
	compiler_parse();

	// All global defines are added to the std module
	compiler.context.std_module_path = (Path) { .module = kw_std, .span = INVALID_SPAN, .len = (uint32_t) strlen(kw_std) };
	compiler.context.std_module = (Module){ .name = &compiler.context.std_module_path, .short_path = compiler.context.std_module_path.module };
	compiler.context.std_module.stage = ANALYSIS_LAST;
	compiler.context.locals_list = NULL;

	// Set a maximum of symbols in the std_module and test module
	htable_init(&compiler.context.std_module.symbols, 0x1000);

	// Set up the func prototype hash map
	type_func_prototype_init(0x10000);

	// Do we have zero modules?
	if (!compiler.context.module_list)
	{
		if (compiler.context.errors_found) exit_compiler(EXIT_FAILURE);
		error_exit("No modules to compile.");
	}


	// We parse the generic modules, just by storing the decls.
	FOREACH(Module *, module, compiler.context.generic_module_list)
	{
		analyze_generic_module(module);
	}

	for (AnalysisStage stage = ANALYSIS_NOT_BEGUN + 1; stage <= ANALYSIS_LAST; stage++)
	{
		sema_analyze_to_stage(stage);
	}

RESOLVE_LAMBDA:;
	bool found_lambda = false;
	FOREACH(Module *, module, compiler.context.module_list)
	{
		if (vec_size(module->lambdas_to_evaluate))
		{
			sema_analysis_pass_lambda(module);
			found_lambda = true;
		}
	}
	if (found_lambda) goto RESOLVE_LAMBDA;
	halt_on_error();

	assign_panicfn();
	assign_testfn();
	assign_benchfn();

	if (strip_unused())
	{
		sema_trace_liveness();
	}
}

void sema_context_init(SemaContext *context, CompilationUnit *unit)
{
	*context = (SemaContext) { .unit = unit, .compilation_unit = unit,
							   .ct_locals = global_context_acquire_locals_list(),
							   .locals = global_context_acquire_locals_list() };
}

void sema_context_pop_ct_stack(SemaContext *context, unsigned old_state)
{
	vec_resize(context->ct_locals, old_state);
}

unsigned sema_context_push_ct_stack(SemaContext *context)
{
	return vec_size(context->ct_locals);
}

void sema_context_destroy(SemaContext *context)
{
	if (!context->unit) return;
	generic_context_release_locals_list(context->locals);
	generic_context_release_locals_list(context->ct_locals);
}

Decl **global_context_acquire_locals_list(void)
{
	if (!vec_size(compiler.context.locals_list))
	{
		return VECNEW(Decl*, 64);
	}
	Decl **result = VECLAST(compiler.context.locals_list);
	vec_pop(compiler.context.locals_list);
	vec_resize(result, 0);
	return result;
}

void generic_context_release_locals_list(Decl **list)
{
	vec_add(compiler.context.locals_list, list);
}

SemaContext *context_transform_for_eval(SemaContext *context, SemaContext *temp_context, CompilationUnit *eval_unit)
{
	if (eval_unit == context->unit)
	{
		temp_context->unit = NULL;
		return context;
	}
	DEBUG_LOG("Changing compilation unit to %s", eval_unit->file->name);
	sema_context_init(temp_context, eval_unit);
	temp_context->compilation_unit = context->compilation_unit;
	temp_context->call_env = context->call_env;
	temp_context->current_macro = context->current_macro;
	temp_context->is_temp = true;
	return temp_context;
}

void sema_print_inline(SemaContext *context, SourceSpan original)
{
	if (!context) return;
	InliningSpan *inlined_at = context->inlined_at;
	while (inlined_at)
	{
		if (inlined_at->span.a != original.a)
		{
			sema_note_prev_at(inlined_at->span, "Inlined from here.");
		}
		inlined_at = inlined_at->prev;
	}
}

void sema_error_at(SemaContext *context, SourceSpan span, const char *message, ...)
{
	va_list list;
	va_start(list, message);
	sema_verror_range(span, message, list);
	va_end(list);
	sema_print_inline(context, span);
}

bool sema_warn_at(SemaContext *context, SourceSpan span, const char *message, ...)
{
	bool is_warn = compiler.build.validation_level < VALIDATION_STRICT;
	va_list list;
	va_start(list, message);
	if (is_warn)
	{
		sema_vwarn_range(span, message, list);
	}
	else
	{
		sema_verror_range(span, message, list);
	}
	va_end(list);
	sema_print_inline(context, span);
	return is_warn;
}
