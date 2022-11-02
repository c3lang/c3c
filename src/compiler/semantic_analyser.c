// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include <compiler_tests/benchmark.h>
#include "sema_internal.h"

void context_change_scope_with_flags(SemaContext *context, ScopeFlags flags)
{
	unsigned depth = context->active_scope.depth + 1;
	if (depth > MAX_SCOPE_DEPTH)
	{
		FATAL_ERROR("Too deeply nested scopes.");
	}

	Ast *previous_defer = context->active_scope.in_defer;
	AstId parent_defer = context->active_scope.defer_last;
	unsigned last_local = context->active_scope.current_local;
	assert(parent_defer < 1000000);
	// Defer and expression blocks introduce their own return/break/continue
	// otherwise just merge with the old flags.
	if (flags & (SCOPE_EXPR_BLOCK | SCOPE_MACRO))
	{
		previous_defer = 0;
		parent_defer = 0;
	}
	bool new_label_scope = (flags & (SCOPE_EXPR_BLOCK | SCOPE_MACRO)) != 0;
	if (!(flags & SCOPE_EXPR_BLOCK))
	{
		flags = context->active_scope.flags | flags;
	}
	unsigned label_start = new_label_scope ? last_local : context->active_scope.label_start;
	context->active_scope = (DynamicScope) {
			.scope_id = ++context->scope_id,
			.allow_dead_code = false,
			.jump_end = false,
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

void context_change_scope_for_label(SemaContext *context, Decl *label)
{
	context_change_scope_with_flags(context, SCOPE_NONE);

	if (label)
	{
		label->label.defer = context->active_scope.defer_last;
		sema_add_local(context, label);
		label->label.scope_defer = astid(context->active_scope.in_defer);
	}
}

AstId context_get_defers(SemaContext *context, AstId defer_top, AstId defer_bottom)
{
	AstId first = 0;
	AstId *next = &first;
	while (defer_bottom != defer_top)
	{
		Ast *defer = astptr(defer_top);
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
	if (next && !context->active_scope.jump_end)
	{
		AstId defer_current = context->active_scope.defer_last;
		while (defer_current != defer_start)
		{
			Ast *defer = astptr(defer_current);
			Ast *defer_body = copy_ast_defer(astptr(defer->defer_stmt.body));
			*next = astid(defer_body);
			next = &defer_body->next;
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
	assert(ast->ast_kind != AST_COMPOUND_STMT);
	Ast *replacement = ast_copy(ast);
	ast->ast_kind = AST_COMPOUND_STMT;
	ast->compound_stmt.first_stmt = astid(replacement);
	replacement->next = defer_first;
}

static inline void halt_on_error(void)
{
	if (global_context.errors_found > 0) exit_compiler(EXIT_FAILURE);
}



void sema_analyze_stage(Module *module, AnalysisStage stage)
{
	while (module->stage < stage)
	{
		global_context.decl_stack_bottom = global_context.decl_stack_top = global_context.decl_stack;
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
			case ANALYSIS_REGISTER_GLOBALS:
				sema_analysis_pass_register_globals(module);
				break;
			case ANALYSIS_CONDITIONAL_COMPILATION:
				sema_analysis_pass_conditional_compilation(module);
				break;
			case ANALYSIS_DECLS:
				sema_analysis_pass_decls(module);
				break;
			case ANALYSIS_CT_ASSERT:
				sema_analysis_pass_ct_assert(module);
				break;
			case ANALYSIS_FUNCTIONS:
				sema_analysis_pass_functions(module);
				break;
		}
		if (global_context.errors_found) return;
	}
}

static void register_generic_decls(CompilationUnit *unit, Decl **decls)
{
	VECEACH(decls, i)
	{
		Decl *decl = decls[i];
		decl->unit = unit;
		switch (decl->decl_kind)
		{
			case DECL_POISONED:
			case DECL_ENUM_CONSTANT:
			case DECL_FAULTVALUE:
			case DECL_IMPORT:
			case DECL_LABEL:
			case DECL_CT_ASSERT:
			case DECL_DECLARRAY:
			case DECL_INITIALIZE:
			case DECL_FINALIZE:
			case DECL_CT_IF:
			case DECL_CT_SWITCH:
				continue;
			case DECL_ATTRIBUTE:
				break;
			case DECL_CT_CASE:
			case DECL_CT_ELIF:
			case DECL_CT_ELSE:
			case DECL_BODYPARAM:
				UNREACHABLE
			case DECL_MACRO:
			case DECL_DEFINE:
			case DECL_DISTINCT:
			case DECL_ENUM:
			case DECL_GENERIC:
			case DECL_FAULT:
			case DECL_FUNC:
			case DECL_STRUCT:
			case DECL_TYPEDEF:
			case DECL_UNION:
			case DECL_VAR:
			case DECL_BITSTRUCT:
				break;
		}
		htable_set(&unit->module->symbols, decl->name, decl);
		if (decl->visibility == VISIBLE_PUBLIC) global_context_add_generic_decl(decl);
	}
}


static void analyze_generic_module(Module *module)
{
	assert(module->parameters && module->is_generic);
	VECEACH(module->units, index)
	{
		CompilationUnit *unit = module->units[index];
		register_generic_decls(unit, unit->global_decls);
	}
	sema_analyze_stage(module, ANALYSIS_MODULE_HIERARCHY);
}

static void sema_analyze_to_stage(AnalysisStage stage)
{
	VECEACH(global_context.module_list, i)
	{
		sema_analyze_stage(global_context.module_list[i], stage);
	}
	halt_on_error();
}

/**
 * Perform the entire semantic analysis.
 */
void sema_analysis_run(void)
{
	// Cleanup any errors (could there really be one here?!)
	global_context_clear_errors();

	// Add the standard library
	if (global_context.lib_dir && !active_target.no_stdlib)
	{
		file_add_wildcard_files(&global_context.sources, global_context.lib_dir, true, c3_suffix_list, 3);
	}

	// Load and parse all files.
	bool has_error = false;
	VECEACH(global_context.sources, i)
	{
		bool loaded = false;
		File *file = source_file_load(global_context.sources[i], &loaded);
		if (loaded) continue;
		if (!parse_file(file)) has_error = true;
	}
	if (active_target.read_stdin)
	{
		if (!parse_stdin()) has_error = true;
	}

	if (has_error) exit_compiler(EXIT_FAILURE);
	compiler_parsing_time = bench_mark();

	// All global defines are added to the std module
	global_context.std_module_path = (Path) { .module = kw_std, .span = INVALID_SPAN, .len = (uint32_t) strlen(kw_std) };
	global_context.std_module = (Module){ .name = &global_context.std_module_path };
	global_context.std_module.stage = ANALYSIS_LAST;
	global_context.locals_list = NULL;

	// Set a maximum of symbols in the std_module and test module
	htable_init(&global_context.std_module.symbols, 0x10000);


	// Setup the func prototype hash map
	type_func_prototype_init(0x10000);

	// Do we have zero modules?
	if (!global_context.module_list)
	{
		if (global_context.errors_found) exit_compiler(EXIT_FAILURE);
		error_exit("No modules to compile.");
	}

	// Create the core module if needed.
	Path core_path = { .module = kw_std__core, .span = INVALID_SPAN, .len = strlen(kw_std__core) };
	global_context.core_module = compiler_find_or_create_module(&core_path, NULL, false);

	// We parse the generic modules, just by storing the decls.
	VECEACH(global_context.generic_module_list, i)
	{
		analyze_generic_module(global_context.generic_module_list[i]);
	}

	for (AnalysisStage stage = ANALYSIS_NOT_BEGUN + 1; stage <= ANALYSIS_LAST; stage++)
	{
		sema_analyze_to_stage(stage);
	}

	if (active_target.panicfn || !active_target.no_stdlib)
	{
		const char *panicfn = active_target.panicfn ? active_target.panicfn : "std::core::builtin::panic";
		Path *path;
		const char *ident;
		TokenType type;
		if (sema_splitpathref(panicfn, strlen(panicfn), &path, &ident) != TOKEN_IDENT || path == NULL || !ident)
		{
			error_exit("'%s' is not a valid panic function.", panicfn);
		}
		Decl *decl = sema_find_decl_in_modules(global_context.module_list, path, ident);
		if (!decl)
		{
			error_exit("Panic function pointer '%s::%s' could not be found.", path->module, ident);
		}
		Type *panic_fn_type = decl->type->canonical;
		if (decl->decl_kind != DECL_VAR || !type_is_func_ptr(panic_fn_type))
		{
			error_exit("'%s::%s' is not a function pointer.", path->module, ident);
		}
		if (!type_func_match(panic_fn_type, type_void, 4, type_chars, type_chars, type_chars, type_uint))
		{
			error_exit("Expected panic function to have the signature fn void(char[], char[], char[], uint).");
		}
		global_context.panic_var = decl;
	}
	else
	{
		global_context.panic_var = NULL;
	}
	compiler_sema_time = bench_mark();

}

void sema_context_init(SemaContext *context, CompilationUnit *unit)
{
	*context = (SemaContext) { .unit = unit, .compilation_unit = unit, .locals = global_context_acquire_locals_list() };
}

void sema_context_destroy(SemaContext *context)
{
	if (!context->unit) return;
	generic_context_release_locals_list(context->locals);
}

Decl **global_context_acquire_locals_list(void)
{
	if (!vec_size(global_context.locals_list))
	{
		return VECNEW(Decl*, 64);
	}
	Decl **result = VECLAST(global_context.locals_list);
	vec_pop(global_context.locals_list);
	return result;
}

void generic_context_release_locals_list(Decl **list)
{
	vec_add(global_context.locals_list, list);
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
	return temp_context;
}
