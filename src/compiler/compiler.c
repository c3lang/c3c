// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"
#ifndef _MSC_VER
#include <unistd.h>
#endif

#define MAX_OUTPUT_FILES 1000000
#define MAX_MODULES 100000
GlobalContext global_context;
BuildTarget active_target;

Vmem ast_arena;
Vmem expr_arena;
Vmem sourceloc_arena;
Vmem toktype_arena;
Vmem tokdata_arena;
Vmem decl_arena;
Vmem type_info_arena;

static void global_context_clear_errors(void)
{
	global_context.in_panic_mode = false;
	global_context.errors_found = 0;
	global_context.warnings_found = 0;
}

void compiler_init(const char *std_lib_dir)
{
	DEBUG_LOG("Version: %s", COMPILER_VERSION);

	global_context = (GlobalContext ){ .in_panic_mode = false };
	// Skip library detection.
	//compiler.lib_dir = find_lib_dir();
	//DEBUG_LOG("Found std library: %s", compiler.lib_dir);
	stable_init(&global_context.modules, 64);
	stable_init(&global_context.scratch_table, 32);
	stable_init(&global_context.compiler_defines, 512);
	global_context.module_list = NULL;
	global_context.generic_module_list = NULL;
	stable_init(&global_context.global_symbols, 0x1000);
	vmem_init(&ast_arena, 4 * 1024);
	vmem_init(&expr_arena, 4 * 1024);
	vmem_init(&decl_arena, 1024);
	vmem_init(&sourceloc_arena, 4 * 1024);
	vmem_init(&toktype_arena, 4 * 1024);
	vmem_init(&tokdata_arena, 4 * 1024);
	vmem_init(&type_info_arena, 1024);
	// Create zero index value.
	(void) sourceloc_calloc();
	(void) toktype_calloc();
	(void) tokdata_calloc();
	if (std_lib_dir)
	{
		global_context.lib_dir = std_lib_dir;
	}
	else
	{
		global_context.lib_dir = find_lib_dir();
	}
}

static void compiler_lex(void)
{
	VECEACH(global_context.sources, i)
	{
		bool loaded = false;
		File *file = source_file_load(global_context.sources[i], &loaded);
		if (loaded) continue;
		Lexer lexer;
		lexer_init_with_file(&lexer, file);
		printf("# %s\n", file->full_path);
		while (1)
		{
			Token token = lexer_advance(&lexer);
			printf("%s ", token_type_to_string(token.type));
			if (token.type == TOKEN_EOF) break;
		}
		printf("\n");
	}
	exit_compiler(COMPILER_SUCCESS_EXIT);
}

void compiler_parse(void)
{
	VECEACH(global_context.sources, i)
	{
		bool loaded = false;
		File *file = source_file_load(global_context.sources[i], &loaded);
		if (loaded) continue;

		global_context_clear_errors();
		parse_file(file);
	}
	exit_compiler(COMPILER_SUCCESS_EXIT);
}

static inline void halt_on_error(void)
{
	if (global_context.errors_found > 0) exit_compiler(EXIT_FAILURE);
}

typedef struct CompileData_
{
	void *context;
	const char *object_name;
	Task task;
} CompileData;

void thread_compile_task_llvm(void *compiledata)
{
	CompileData *data = compiledata;
	data->object_name = llvm_codegen(data->context);
}

void thread_compile_task_tb(void *compiledata)
{
	CompileData *data = compiledata;
	data->object_name = tinybackend_codegen(data->context);
}

void sema_analyze_stage(Module *module, AnalysisStage stage)
{
	while (module->stage < stage)
	{
		module->stage++;
		switch (module->stage)
		{
			case ANALYSIS_NOT_BEGUN:
				UNREACHABLE
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

static void register_generic_decls(Module *module, Decl **decls)
{
	VECEACH(decls, i)
	{
		Decl *decl = decls[i];
		decl->module = module;
		switch (decl->decl_kind)
		{
			case DECL_POISONED:
			case DECL_ENUM_CONSTANT:
			case DECL_ERRVALUE:
			case DECL_IMPORT:
			case DECL_LABEL:
			case DECL_CT_ASSERT:
				continue;
			case DECL_ATTRIBUTE:
				break;
			case DECL_CT_CASE:
				register_generic_decls(module, decl->ct_case_decl.body);
				continue;
			case DECL_CT_ELIF:
				register_generic_decls(module, decl->ct_elif_decl.then);
				continue;
			case DECL_CT_ELSE:
				register_generic_decls(module, decl->ct_else_decl);
				continue;
			case DECL_CT_IF:
				register_generic_decls(module, decl->ct_if_decl.then);
				continue;
			case DECL_CT_SWITCH:
				register_generic_decls(module, decl->ct_switch_decl.cases);
				continue;
			case DECL_MACRO:
			case DECL_DEFINE:
			case DECL_DISTINCT:
			case DECL_ENUM:
			case DECL_GENERIC:
			case DECL_ERRTYPE:
			case DECL_FUNC:
			case DECL_STRUCT:
			case DECL_TYPEDEF:
			case DECL_UNION:
			case DECL_VAR:
			case DECL_BITSTRUCT:
				break;
		}
		if (decl->visibility > VISIBLE_MODULE)
		{
			stable_set(&module->public_symbols, decl->name, decl);
		}
		stable_set(&module->symbols, decl->name, decl);
	}

}
static void analyze_generic_module(Module *module)
{
	assert(module->parameters && module->is_generic);
	// TODO maybe do this analysis: sema_analysis_pass_process_imports(module);
	VECEACH(module->contexts, index)
	{
		Context *context = module->contexts[index];
		register_generic_decls(module, context->global_decls);
	}
}

static void analyze_to_stage(AnalysisStage stage)
{
	VECEACH(global_context.module_list, i)
	{
		sema_analyze_stage(global_context.module_list[i], stage);
	}
	halt_on_error();
}

static void add_global_define(const char *name, Expr *value)
{
	Decl *dec = decl_calloc();
	TokenType type = TOKEN_CONST_IDENT;
	const char *unique_name = symtab_add(name, (uint32_t)strlen(name), fnv1a(name, (uint32_t)strlen(name)), &type);
	dec->name = unique_name;
	dec->module = &global_context.std_module;
	dec->visibility = VISIBLE_PUBLIC;
	dec->decl_kind = DECL_VAR;
	dec->var.kind = VARDECL_CONST;
	dec->var.constant = true;
	dec->var.type_info = NULL;
	dec->var.init_expr = value;
	dec->type = value->type;
	dec->resolve_status = RESOLVE_DONE;
	decl_set_external_name(dec);
	compiler_register_public_symbol(dec);
	stable_set(&dec->module->public_symbols, dec->name, dec);
	stable_set(&dec->module->symbols, dec->name, dec);
}

static void setup_int_define(const char *id, uint64_t i, Type *type)
{
	TokenType token_type = TOKEN_CONST_IDENT;
	id = symtab_add(id, (uint32_t) strlen(id), fnv1a(id, (uint32_t) strlen(id)), &token_type);
	Expr *expr = expr_new(EXPR_CONST, INVALID_RANGE);
	assert(type_is_integer(type));
	expr_const_set_int(&expr->const_expr, i, type->type_kind);
	if (expr_const_will_overflow(&expr->const_expr, type->type_kind))
	{
		error_exit("Integer define %s overflow.", id);
	}
	expr->type = type;
	expr->const_expr.narrowable = true;
	expr->span = INVALID_RANGE;
	expr->resolve_status = RESOLVE_NOT_DONE;
	void *previous = stable_set(&global_context.compiler_defines, id, expr);
	if (previous)
	{
		error_exit("Redefined ident %s", id);
	}
}

static void setup_bool_define(const char *id, bool value)
{
	TokenType token_type = TOKEN_CONST_IDENT;
	id = symtab_add(id, (uint32_t) strlen(id), fnv1a(id, (uint32_t) strlen(id)), &token_type);
	Expr *expr = expr_new(EXPR_CONST, INVALID_RANGE);
	expr_const_set_bool(&expr->const_expr, value);
	expr->type = type_bool;
	expr->span = INVALID_RANGE;
	expr->resolve_status = RESOLVE_NOT_DONE;
	void *previous = stable_set(&global_context.compiler_defines, id, expr);
	if (previous)
	{
		error_exit("Redefined ident %s", id);
	}
}

void compiler_compile(void)
{
	setup_int_define("C_SHORT_SIZE", platform_target.width_c_short, type_long);
	setup_int_define("C_INT_SIZE", platform_target.width_c_int, type_long);
	setup_int_define("C_LONG_SIZE", platform_target.width_c_long, type_long);
	setup_int_define("C_LONG_LONG_SIZE", platform_target.width_c_long_long, type_long);
	setup_bool_define("C_CHAR_IS_SIGNED", platform_target.signed_c_char);
	setup_bool_define("PLATFORM_BIG_ENDIAN", platform_target.big_endian);
	setup_bool_define("PLATFORM_I128_SUPPORTED", platform_target.int128);
	setup_int_define("COMPILER_OPT_LEVEL", (uint64_t)active_target.optimization_level, type_int);
	setup_int_define("COMPILER_SIZE_OPT_LEVEL", (uint64_t)active_target.size_optimization_level, type_int);
	setup_bool_define("COMPILER_SAFE_MODE", active_target.feature.safe_mode);

	global_context_clear_errors();

	if (global_context.lib_dir)
	{
		file_add_wildcard_files(&global_context.sources, global_context.lib_dir, true);
	}
	bool has_error = false;
	VECEACH(global_context.sources, i)
	{
		bool loaded = false;
		File *file = source_file_load(global_context.sources[i], &loaded);
		if (loaded) continue;
		if (!parse_file(file)) has_error = true;
	}

	if (has_error) exit_compiler(EXIT_FAILURE);

	global_context.std_module_path = (Path) { .module = kw_std, .span = INVALID_RANGE, .len = (uint32_t) strlen(kw_std) };
	global_context.std_module = (Module){ .name = &global_context.std_module_path };
	global_context.std_module.stage = ANALYSIS_LAST;
	stable_init(&global_context.std_module.symbols, 0x10000);

	if (!global_context.module_list)
	{
		if (global_context.errors_found) exit_compiler(EXIT_FAILURE);
		error_exit("No modules to compile.");
	}
	VECEACH(global_context.generic_module_list, i)
	{
		analyze_generic_module(global_context.generic_module_list[i]);
	}
	for (AnalysisStage stage = ANALYSIS_NOT_BEGUN + 1; stage <= ANALYSIS_LAST; stage++)
	{
		analyze_to_stage(stage);
	}

	Module **modules = global_context.module_list;
	unsigned module_count = vec_size(modules);

	if (module_count > MAX_MODULES)
	{
		error_exit("Too many modules.");
	}
	if (module_count < 1)
	{
		error_exit("No module to compile.");
	}

	if (active_target.output_headers)
	{
		for (unsigned i = 0; i < module_count; i++)
		{
			header_gen(modules[i]);
		}
		return;
	}

	void **gen_contexts = NULL;
	void (*task)(void *);

	switch (active_target.backend)
	{
		case BACKEND_LLVM:
			llvm_codegen_setup();
			for (unsigned i = 0; i < module_count; i++)
			{
				void *result = llvm_gen(modules[i]);
				if (result) vec_add(gen_contexts, result);
			}
			task = &thread_compile_task_llvm;
			break;
		case BACKEND_TB:
			tinybackend_codegen_setup();
			for (unsigned i = 0; i < module_count; i++)
			{
				void *result = tinybackend_gen(modules[i]);
				if (result) vec_add(gen_contexts, result);
			}
			task = &thread_compile_task_tb;
			break;
	}



	if (debug_stats)
	{
		printf("-- AST/EXPR INFO -- \n");
		printf(" * Ast memory use: %llukb\n", (unsigned long long)ast_arena.allocated / 1024);
		printf(" * Decl memory use: %llukb\n", (unsigned long long)decl_arena.allocated / 1024);
		printf(" * Expr memory use: %llukb\n", (unsigned long long)expr_arena.allocated / 1024);
		printf(" * TypeInfo memory use: %llukb\n", (unsigned long long)type_info_arena.allocated / 1024);
		printf(" * Token memory use: %llukb\n", (unsigned long long)(toktype_arena.allocated) / 1024);
		printf(" * Sourceloc memory use: %llukb\n", (unsigned long long)(sourceloc_arena.allocated) / 1024);
		printf(" * Token data memory use: %llukb\n", (unsigned long long)(tokdata_arena.allocated) / 1024);
	}

	ast_arena_free();
	decl_arena_free();
	expr_arena_free();
	type_info_arena_free();
	sourceloc_arena_free();
	tokdata_arena_free();

	if (debug_stats) print_arena_status();


	bool create_exe = !active_target.no_link && !active_target.test_output && (active_target.type == TARGET_TYPE_EXECUTABLE || active_target.type == TARGET_TYPE_TEST);

	uint32_t output_file_count = vec_size(gen_contexts);
	if (output_file_count > MAX_OUTPUT_FILES)
	{
		error_exit("Too many output files.");
	}
	if (!output_file_count)
	{
		error_exit("No output files found.");
	}

	CompileData *compile_data = malloc(sizeof(CompileData) * output_file_count);
	const char **obj_files = malloc(sizeof(char*) * output_file_count);

	TaskQueueRef queue = taskqueue_create(16);


	for (unsigned i = 0; i < output_file_count; i++)
	{
		compile_data[i] = (CompileData) { .context = gen_contexts[i] };
		compile_data[i].task = (Task) { task, &compile_data[i] };
		taskqueue_add(queue, &compile_data[i].task);
	}

	taskqueue_wait_for_completion(queue);
	taskqueue_destroy(queue);

	for (unsigned i = 0; i < output_file_count; i++)
	{
		obj_files[i] = compile_data[i].object_name;
		assert(obj_files[i] || !create_exe);
	}

	free(compile_data);

	if (create_exe)
	{
		if (active_target.arch_os_target == ARCH_OS_TARGET_DEFAULT)
		{
			platform_linker(active_target.name, obj_files, output_file_count);
		}
		else
		{
			if (!obj_format_linking_supported(platform_target.object_format) || !linker(active_target.name, obj_files,
			                                                                            output_file_count))
			{
				printf("No linking is performed due to missing linker support.\n");
				active_target.run_after_compile = false;
			}
		}
		if (active_target.run_after_compile)
		{
			system(strformat("./%s", active_target.name));
		}
	}

	memory_release();
	exit_compiler(COMPILER_SUCCESS_EXIT);
}

static void target_expand_source_names(BuildTarget *target)
{
	const char **files = NULL;
	VECEACH(target->source_dirs, i)
	{
		const char *name = target->source_dirs[i];
		size_t name_len = strlen(name);
		if (name_len < 1) goto INVALID_NAME;
		if (name[name_len - 1] == '*')
		{
			if (name_len == 1 || name[name_len - 2] == '/')
			{
				char *path = strdup(name);
				path[name_len - 1] = '\0';
				file_add_wildcard_files(&files, path, false);
				free(path);
				continue;
			}
			if (name[name_len - 2] != '*') goto INVALID_NAME;
			if (name_len == 2 || name[name_len - 3] == '/')
			{
				char *path = strdup(name);
				path[name_len - 2] = '\0';
				file_add_wildcard_files(&files, path, true);
				free(path);
				continue;
			}
			goto INVALID_NAME;
		}
		if (name_len < 4) goto INVALID_NAME;
		if (strcmp(&name[name_len - 3], ".c3") != 0 &&
		    (name_len < 5 || strcmp(&name[name_len - 4], ".c3t") != 0)) goto INVALID_NAME;
		vec_add(files, name);
		continue;
		INVALID_NAME:
		error_exit("File names must end with .c3 or they cannot be compiled: '%s' is invalid.", name);
	}
	target->sources = files;
}

void compile_target(BuildOptions *options)
{
	init_default_build_target(&active_target, options, DEFAULT_EXE);
	compile();
}

void compile_file_list(BuildOptions *options)
{
	init_build_target(&active_target, options);
	compile();
}

void compile()
{
	target_expand_source_names(&active_target);
	global_context.sources = active_target.sources;
	symtab_init(active_target.symtab_size ? active_target.symtab_size : 64 * 1024);
	target_setup(&active_target);

	if (!vec_size(active_target.sources)) error_exit("No files to compile.");
	if (active_target.lex_only)
	{
		compiler_lex();
		return;
	}
	if (active_target.parse_only)
	{
		compiler_parse();
		return;
	}
	compiler_compile();
}


Decl *compiler_find_symbol(const char *string)
{
	return stable_get(&global_context.global_symbols, string);
}

void global_context_add_type(Type *type)
{
	DEBUG_LOG("Created type %s.", type->name);
	assert(type_ok(type));
	vec_add(global_context.type, type);
}

Module *global_context_find_module(const char *name)
{
	return stable_get(&global_context.modules, name);
}

Module *compiler_find_or_create_module(Path *module_name, TokenId *parameters, bool is_private)
{
	Module *module = global_context_find_module(module_name->module);
	if (module) return module;

	DEBUG_LOG("Creating module %s.", module_name->module);
	// Set up the module.
	module = CALLOCS(Module);
	module->name = module_name;
	module->stage = ANALYSIS_NOT_BEGUN;
	module->parameters = parameters;
	module->is_generic = vec_size(parameters) > 0;
	module->is_private = is_private;
	stable_init(&module->symbols, 0x10000);
	stable_set(&global_context.modules, module_name->module, module);
	if (parameters)
	{
		vec_add(global_context.generic_module_list, module);
	}
	else
	{
		vec_add(global_context.module_list, module);
	}

	return module;
}

void compiler_register_public_symbol(Decl *decl)
{
	assert(decl->name);
	Decl *prev = stable_get(&global_context.global_symbols, decl->name);
	// If the previous symbol was already declared globally, remove it.
	stable_set(&global_context.global_symbols, decl->name, prev ? poisoned_decl : decl);
	STable *sub_module_space = stable_get(&global_context.qualified_symbols, decl->module->name->module);
	if (!sub_module_space)
	{
		sub_module_space = malloc_arena(sizeof(*sub_module_space));
		stable_init(sub_module_space, 0x100);
		stable_set(&global_context.qualified_symbols, decl->module->name->module, sub_module_space);
	}
	prev = stable_get(sub_module_space, decl->name);
	stable_set(sub_module_space, decl->name, prev ? poisoned_decl : decl);
}

void scratch_buffer_clear(void)
{
	global_context.scratch_buffer_len = 0;
}

void scratch_buffer_append_len(const char *string, size_t len)
{
	if (len + global_context.scratch_buffer_len > MAX_STRING_BUFFER - 1)
	{
		error_exit("Scratch buffer size (%d chars) exceeded", MAX_STRING_BUFFER - 1);
	}
	memcpy(global_context.scratch_buffer + global_context.scratch_buffer_len, string, len);
	global_context.scratch_buffer_len += len;
}

void scratch_buffer_append(const char *string)
{
	scratch_buffer_append_len(string, strlen(string));
}

void scratch_buffer_append_signed_int(int64_t i)
{
	uint32_t len_needed = (uint32_t)sprintf(&global_context.scratch_buffer[global_context.scratch_buffer_len], "%lld", (long long)i);
	if (global_context.scratch_buffer_len + len_needed > MAX_STRING_BUFFER - 1)
	{
		error_exit("Scratch buffer size (%d chars) exceeded", MAX_STRING_BUFFER - 1);
	}
	global_context.scratch_buffer_len += len_needed;
}

void scratch_buffer_append_unsigned_int(uint64_t i)
{
	uint32_t len_needed = (uint32_t)sprintf(&global_context.scratch_buffer[global_context.scratch_buffer_len], "%llu", (unsigned long long)i);
	if (global_context.scratch_buffer_len + len_needed > MAX_STRING_BUFFER - 1)
	{
		error_exit("Scratch buffer size (%d chars) exceeded", MAX_STRING_BUFFER - 1);
	}
	global_context.scratch_buffer_len += len_needed;
}

void scratch_buffer_append_char(char c)
{
	if (global_context.scratch_buffer_len + 1 > MAX_STRING_BUFFER - 1)
	{
		error_exit("Scratch buffer size (%d chars) exceeded", MAX_STRING_BUFFER - 1);
	}
	global_context.scratch_buffer[global_context.scratch_buffer_len++] = c;
}

char *scratch_buffer_to_string(void)
{
	global_context.scratch_buffer[global_context.scratch_buffer_len] = '\0';
	return global_context.scratch_buffer;
}

const char *scratch_buffer_interned(void)
{
	TokenType type = TOKEN_INVALID_TOKEN;
	return symtab_add(global_context.scratch_buffer, global_context.scratch_buffer_len,
	                  fnv1a(global_context.scratch_buffer, global_context.scratch_buffer_len), &type);
}
