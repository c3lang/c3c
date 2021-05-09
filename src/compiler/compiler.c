// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"
#include <unistd.h>

#if PLATFORM_POSIX
#include <pthread.h>
#define USE_PTHREAD 1
#else
#define USE_PTHREAD 0
#endif

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
	// Skip library detection.
	//compiler.lib_dir = find_lib_dir();
	//DEBUG_LOG("Found std library: %s", compiler.lib_dir);
	stable_init(&global_context.modules, 64);
	stable_init(&global_context.scratch_table, 32);
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
	exit(EXIT_SUCCESS);
}

void compiler_parse(void)
{
	VECEACH(global_context.sources, i)
	{
		bool loaded = false;
		File *file = source_file_load(global_context.sources[i], &loaded);
		if (loaded) continue;

		global_context_clear_errors();
		Context *context = context_create(file);
		parse_file(context);
		context_print_ast(context, stdout);
	}
	exit(EXIT_SUCCESS);
}

static inline void halt_on_error(void)
{
	if (global_context.errors_found > 0) exit(EXIT_FAILURE);
}

#if USE_PTHREAD
void* compile_on_pthread(void *gencontext)
{
	return (void *)llvm_codegen(gencontext);
}
#endif
void compiler_compile(void)
{
	Context **contexts = NULL;

	global_context_clear_errors();

	if (global_context.lib_dir)
	{
		vec_add(global_context.sources, strformat("%s/std/runtime.c3", global_context.lib_dir));
		vec_add(global_context.sources, strformat("%s/std/builtin.c3", global_context.lib_dir));
		vec_add(global_context.sources, strformat("%s/std/io.c3", global_context.lib_dir));
		vec_add(global_context.sources, strformat("%s/std/mem.c3", global_context.lib_dir));
		vec_add(global_context.sources, strformat("%s/std/array.c3", global_context.lib_dir));
		vec_add(global_context.sources, strformat("%s/std/math.c3", global_context.lib_dir));
	}
	VECEACH(global_context.sources, i)
	{
		bool loaded = false;
		File *file = source_file_load(global_context.sources[i], &loaded);
		if (loaded) continue;
		Context *context = context_create(file);
		vec_add(contexts, context);
		if (!parse_file(context)) continue;
	}
	unsigned source_count = vec_size(contexts);
	if (!source_count)
	{
		error_exit("No source files to compile.");
	}
	assert(contexts);
	Module **modules = global_context.module_list;

	unsigned module_count = vec_size(modules);
	for (unsigned i = 0; i < module_count; i++)
	{
		sema_analysis_pass_process_imports(modules[i]);
	}

	halt_on_error();

	for (unsigned i = 0; i < module_count; i++)
	{
		sema_analysis_pass_register_globals(modules[i]);
	}

	halt_on_error();

	for (unsigned i = 0; i < source_count; i++)
	{
		sema_analysis_pass_conditional_compilation(contexts[i]);
	}

	halt_on_error();

	for (unsigned i = 0; i < source_count; i++)
	{
		sema_analysis_pass_decls(contexts[i]);
	}

	halt_on_error();

	for (unsigned i = 0; i < source_count; i++)
	{
		sema_analysis_pass_ct_assert(contexts[i]);
	}

	halt_on_error();

	for (unsigned i = 0; i < source_count; i++)
	{
		sema_analysis_pass_functions(contexts[i]);
	}

	halt_on_error();

	if (active_target.output_headers)
	{
		for (unsigned i = 0; i < source_count; i++)
		{
			Context *context = contexts[i];
			if (context->module->parameters) break;
			header_gen(context);
		}
		return;
	}


	llvm_codegen_setup();

	void **gen_contexts = malloc(source_count * sizeof(void *));

	for (unsigned i = 0; i < source_count; i++)
	{
		Context *context = contexts[i];
		if (context->module->parameters)
		{
			gen_contexts[i] = NULL;
			continue;
		}
		gen_contexts[i] = llvm_gen(context);
	}


	printf("-- AST/EXPR INFO -- \n");
	printf(" * Ast memory use: %llukb\n", (unsigned long long)ast_arena.allocated / 1024);
	printf(" * Decl memory use: %llukb\n", (unsigned long long)decl_arena.allocated / 1024);
	printf(" * Expr memory use: %llukb\n", (unsigned long long)expr_arena.allocated / 1024);
	printf(" * TypeInfo memory use: %llukb\n", (unsigned long long)type_info_arena.allocated / 1024);
	printf(" * Token memory use: %llukb\n", (unsigned long long)(toktype_arena.allocated) / 1024);
	printf(" * Sourceloc memory use: %llukb\n", (unsigned long long)(sourceloc_arena.allocated) / 1024);
	printf(" * Token data memory use: %llukb\n", (unsigned long long)(tokdata_arena.allocated) / 1024);

	ast_arena_free();
	decl_arena_free();
	expr_arena_free();
	type_info_arena_free();
	sourceloc_arena_free();
	tokdata_arena_free();

	print_arena_status();


	bool create_exe = !active_target.test_output && (active_target.type == TARGET_TYPE_EXECUTABLE || active_target.type == TARGET_TYPE_TEST);

	const char **obj_files = NULL;

#if USE_PTHREAD
	pthread_t *threads = malloc(source_count * sizeof(threads));
	for (unsigned i = 0; i < source_count; i++)
	{
		if (!gen_contexts[i]) continue;
		pthread_create(&threads[i], NULL, &compile_on_pthread, gen_contexts[i]);
	}
	for (unsigned i = 0; i < source_count; i++)
	{
		void *file_name;
		pthread_join(threads[i], &file_name);
		assert(file_name || !create_exe);
		printf("Received result: %s\n", (char*)file_name);
		vec_add(obj_files, file_name);
	}
#else
	for (unsigned i = 0; i < source_count; i++)
	{
		if (!gen_contexts[i]) continue;
		const char *file_name = llvm_codegen(gen_contexts[i]);
		assert(file_name || !create_exe);
		vec_add(obj_files, file_name);
	}
#endif

	if (create_exe)
	{
		if (active_target.arch_os_target == ARCH_OS_TARGET_DEFAULT)
		{
			platform_linker(active_target.name, obj_files, vec_size(obj_files));
		}
		else
		{
			if (!obj_format_linking_supported(platform_target.object_format) || !linker(active_target.name, obj_files,
			                                                                            vec_size(obj_files)))
			{
				printf("No linking is performed due to missing linker support.");
				active_target.run_after_compile = false;
			}
		}
		if (active_target.run_after_compile)
		{
			system(strformat("./%s", active_target.name));
		}
	}

	free_arena();
	exit(EXIT_SUCCESS);
}

static void target_expand_source_names(BuildTarget *target)
{
	const char **files = NULL;
	VECEACH(target->sources, i)
	{
		const char *name = target->sources[i];
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
		if (strcmp(&name[name_len - 3], ".c3") != 0) goto INVALID_NAME;
		vec_add(files, name);
		continue;
		INVALID_NAME:
		error_exit("File names must end with .c3 or they cannot be compiled: '%s' is invalid.", name);
	}
	target->sources = files;
}

void compile_target(BuildOptions *options)
{
	init_default_build_target(&active_target, options, "foo.out");
	compile();
}

void compile_file_list(BuildOptions *options)
{
	init_build_target(&active_target, options);
	compile();
}

void compile()
{
	global_context.sources = active_target.sources;
	symtab_init(active_target.symtab_size ? active_target.symtab_size : 64 * 1024);
	target_expand_source_names(&active_target);
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
	VECADD(global_context.type, type);
}

Module *global_context_find_module(const char *name)
{
	return stable_get(&global_context.modules, name);
}

Module *compiler_find_or_create_module(Path *module_name, TokenId *parameters)
{
	Module *module = global_context_find_module(module_name->module);
	if (module)
	{
		// We might have gotten an auto-generated module, if so
		// update the path here.
		if (module->name->span.loc.index == INVALID_TOKEN_ID.index && module_name->span.loc.index != INVALID_TOKEN_ID.index)
		{
			module->name = module_name;
			module->parameters = parameters;
		}
		return module;
	}

	DEBUG_LOG("Creating module %s.", module_name->module);
	// Set up the module.
	module = CALLOCS(Module);
	module->name = module_name;
	module->parameters = parameters;
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

	// Now find the possible parent array:
	Path *parent_path = path_find_parent_path(NULL, module_name);
	if (parent_path)
	{
		// Get the parent
		compiler_find_or_create_module(parent_path, NULL);
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
