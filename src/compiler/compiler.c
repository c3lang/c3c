// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"
#include "../build/build_options.h"

Compiler compiler;

Vmem ast_arena;
Vmem expr_arena;
Vmem sourceloc_arena;
Vmem toktype_arena;
Vmem tokdata_arena;
Vmem decl_arena;
Vmem type_info_arena;

void compiler_init(void)
{
	// Skip library detection.
	//compiler.lib_dir = find_lib_dir();
	//DEBUG_LOG("Found std library: %s", compiler.lib_dir);
	stable_init(&compiler.modules, 64);
	stable_init(&compiler.global_symbols, 0x1000);
	vmem_init(&ast_arena, 4 * 1024);
	vmem_init(&expr_arena, 4 * 1024);
	vmem_init(&decl_arena, 1024);
	vmem_init(&sourceloc_arena, 4 * 1024);
	vmem_init(&toktype_arena, 4 * 1024);
	vmem_init(&tokdata_arena, 4 * 1024);
	vmem_init(&type_info_arena, 1024);
	// Create zero index value.
	SourceLocation *loc = sourceloc_calloc();
	char *token_type = toktype_calloc();
	TokenData *data = tokdata_calloc();
	compiler.lib_dir = find_lib_dir();
}

static void compiler_lex(BuildTarget *target)
{
	VECEACH(target->sources, i)
	{
		bool loaded = false;
		File *file = source_file_load(target->sources[i], &loaded);
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

void compiler_parse(BuildTarget *target)
{
	VECEACH(target->sources, i)
	{
		bool loaded = false;
		File *file = source_file_load(target->sources[i], &loaded);
		if (loaded) continue;
		diag_reset();
		Context *context = context_create(file, target);
		parse_file(context);
		context_print_ast(context, stdout);
	}
	exit(EXIT_SUCCESS);
}

void compiler_compile(BuildTarget *target)
{
	Context **contexts = NULL;
	diag_reset();
	if (compiler.lib_dir)
	{
		vec_add(target->sources, strformat("%s/std/builtin.c3", compiler.lib_dir));
		vec_add(target->sources, strformat("%s/std/io.c3", compiler.lib_dir));
		vec_add(target->sources, strformat("%s/std/mem.c3", compiler.lib_dir));
		vec_add(target->sources, strformat("%s/std/array.c3", compiler.lib_dir));
	}
	VECEACH(target->sources, i)
	{
		bool loaded = false;
		File *file = source_file_load(target->sources[i], &loaded);
		if (loaded) continue;
		Context *context = context_create(file, target);
		vec_add(contexts, context);
		parse_file(context);
	}
	unsigned source_count = vec_size(contexts);
	assert(contexts);
	for (unsigned i = 0; i < source_count; i++)
	{
		sema_analysis_pass_process_imports(contexts[i]);
	}
	if (diagnostics.errors > 0) exit(EXIT_FAILURE);

	for (unsigned i = 0; i < source_count; i++)
	{
		sema_analysis_pass_register_globals(contexts[i]);
	}
	if (diagnostics.errors > 0) exit(EXIT_FAILURE);

	for (unsigned i = 0; i < source_count; i++)
	{
		sema_analysis_pass_conditional_compilation(contexts[i]);
	}
	if (diagnostics.errors > 0) exit(EXIT_FAILURE);

	for (unsigned i = 0; i < source_count; i++)
	{
		sema_analysis_pass_decls(contexts[i]);
	}
	if (diagnostics.errors > 0) exit(EXIT_FAILURE);

	for (unsigned i = 0; i < source_count; i++)
	{
		sema_analysis_pass_ct_assert(contexts[i]);
	}
	if (diagnostics.errors > 0) exit(EXIT_FAILURE);

	for (unsigned i = 0; i < source_count; i++)
	{
		sema_analysis_pass_functions(contexts[i]);
	}
	if (diagnostics.errors > 0) exit(EXIT_FAILURE);

	if (build_options.command == COMMAND_GENERATE_HEADERS)
	{
		for (unsigned i = 0; i < source_count; i++)
		{
			Context *context = contexts[i];
			header_gen(context);
		}
		return;
	}


	llvm_codegen_setup();

	void **gen_contexts = malloc(source_count * sizeof(void*));
	for (unsigned i = 0; i < source_count; i++)
	{
		Context *context = contexts[i];
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

	free_arena();

	for (unsigned i = 0; i < source_count; i++)
	{
		llvm_codegen(gen_contexts[i]);
	}

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

void compile_files(BuildTarget *target)
{
	if (!target)
	{
		target = CALLOCS(BuildTarget);
		target->type = TARGET_TYPE_EXECUTABLE;
		target->sources = build_options.files;
		target->name = "a.out";
	}
	target_expand_source_names(target);
	target_setup();

	if (!vec_size(target->sources)) error_exit("No files to compile.");
	switch (build_options.compile_option)
	{
		case COMPILE_LEX_ONLY:
			compiler_lex(target);
			break;
		case COMPILE_LEX_PARSE_ONLY:
			compiler_parse(target);
			break;
		case COMPILE_OUTPUT_HEADERS:
		default:
			compiler_compile(target);
			break;
	}
}


Decl *compiler_find_symbol(const char *string)
{
	return stable_get(&compiler.global_symbols, string);
}

void compiler_add_type(Type *type)
{
	DEBUG_LOG("Created type %s.", type->name);
	assert(type_ok(type));
	VECADD(compiler.type, type);
}
Module *compiler_find_or_create_module(Path *module_name)
{
	Module *module = stable_get(&compiler.modules, module_name->module);

	if (module)
	{
		// We might have gotten an auto-generated module, if so
		// update the path here.
		if (module->name->span.loc.index == INVALID_TOKEN_ID.index && module_name->span.loc.index != INVALID_TOKEN_ID.index)
		{
			module->name = module_name;
		}
		return module;
	}

	DEBUG_LOG("Creating module %s.", module_name->module);
	// Set up the module.
	module = CALLOCS(Module);
	module->name = module_name;
	stable_init(&module->symbols, 0x10000);
	stable_set(&compiler.modules, module_name->module, module);
	// Now find the possible parent array:
	Path *parent_path = path_find_parent_path(NULL, module_name);
	if (parent_path)
	{
		// Get the parent
		Module *parent_module = compiler_find_or_create_module(parent_path);
		vec_add(parent_module->sub_modules, module);
	}
	return module;
}

void compiler_register_public_symbol(Decl *decl)
{
	assert(decl->name);
	Decl *prev = stable_get(&compiler.global_symbols, decl->name);
	// If the previous symbol was already declared globally, remove it.
	stable_set(&compiler.global_symbols, decl->name, prev ? poisoned_decl : decl);
	STable *sub_module_space = stable_get(&compiler.qualified_symbols, decl->module->name->module);
	if (!sub_module_space)
	{
		sub_module_space = malloc_arena(sizeof(*sub_module_space));
		stable_init(sub_module_space, 0x100);
		stable_set(&compiler.qualified_symbols, decl->module->name->module, sub_module_space);
	}
	prev = stable_get(sub_module_space, decl->name);
	stable_set(sub_module_space, decl->name, prev ? poisoned_decl : decl);
}
