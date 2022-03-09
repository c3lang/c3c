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
Vmem decl_arena;
Vmem type_info_arena;



void compiler_init(const char *std_lib_dir)
{
	DEBUG_LOG("Version: %s", COMPILER_VERSION);

	global_context = (GlobalContext ){ .in_panic_mode = false };
	// Skip library detection.
	//compiler.lib_dir = find_lib_dir();
	//DEBUG_LOG("Found std library: %s", compiler.lib_dir);
	htable_init(&global_context.modules, 16 * 1024);
	decltable_init(&global_context.symbols, INITIAL_SYMBOL_MAP);
	decltable_init(&global_context.generic_symbols, INITIAL_GENERIC_SYMBOL_MAP);

	htable_init(&global_context.compiler_defines, 16 * 1024);
	global_context.module_list = NULL;
	global_context.generic_module_list = NULL;
	vmem_init(&ast_arena, 4 * 1024);
	ast_calloc();
	vmem_init(&expr_arena, 4 * 1024);
	expr_calloc();
	vmem_init(&decl_arena, 1024);
	decl_calloc();
	vmem_init(&type_info_arena, 1024);
	type_info_calloc();
	// Create zero index value.
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
		Lexer lexer = { .file = file };
		lexer_init(&lexer);
		printf("# %s\n", file->full_path);
		while (lexer_next_token(&lexer))
		{
			TokenType token_type = lexer.token_type;
			printf("%s ", token_type_to_string(token_type));
			if (token_type == TOKEN_EOF) break;
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


typedef struct CompileData_
{
	void *context;
	const char *object_name;
	Task task;
} CompileData;

void thread_compile_task_llvm(void *compile_data)
{
	CompileData *data = compile_data;
	data->object_name = llvm_codegen(data->context);
}

void thread_compile_task_tb(void *compile_data)
{
	CompileData *data = compile_data;
	data->object_name = tinybackend_codegen(data->context);
}


static const char *active_target_name(void)
{
	if (active_target.name) return active_target.name;
	switch (active_target.arch_os_target)
	{
		case X86_WINDOWS:
		case X64_WINDOWS:
		case X64_WINDOWS_GNU:
			return "a.exe";
		default:
			return "a.out";
	}
}

static void free_arenas(void)
{
	if (debug_stats)
	{
		printf("-- AST/EXPR/TYPE INFO -- \n");
		printf(" * Ast size: %u bytes\n", (unsigned)sizeof(Ast));
		printf(" * Decl size: %u bytes\n", (unsigned)sizeof(Decl));
		printf(" * Expr size: %u bytes\n", (unsigned)sizeof(Expr));
		printf(" * TypeInfo size: %u bytes\n", (unsigned)sizeof(TypeInfo));
		printf(" * Ast memory use: %llukb (%u elements)\n",
			   (unsigned long long)ast_arena.allocated / 1024,
		       (unsigned)(ast_arena.allocated / sizeof(Ast)));
		printf(" * Decl memory use: %llukb (%u elements)\n",
			   (unsigned long long)decl_arena.allocated / 1024,
			   (unsigned)(decl_arena.allocated / sizeof(Decl)));
		printf(" * Expr memory use: %llukb (%u elements)\n",
			   (unsigned long long)expr_arena.allocated / 1024,
			   (unsigned)(expr_arena.allocated / sizeof(Expr)));
		printf(" * TypeInfo memory use: %llukb (%u elements)\n",
			   (unsigned long long)type_info_arena.allocated / 1024,
			   (unsigned)(type_info_arena.allocated / sizeof(TypeInfo)));

	}

	ast_arena_free();
	decl_arena_free();
	expr_arena_free();
	type_info_arena_free();

	if (debug_stats) print_arena_status();
}


void compiler_compile(void)
{
	sema_analysis_run();

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

	if (active_target.check_only)
	{
		free_arenas();
		return;
	}

	void **gen_contexts = VECNEW(void*, module_count);
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
		default:
			UNREACHABLE
	}


	free_arenas();


	bool create_exe = !active_target.no_link && !active_target.test_output && (active_target.type == TARGET_TYPE_EXECUTABLE || active_target.type == TARGET_TYPE_TEST);

	uint32_t output_file_count = vec_size(gen_contexts);
	unsigned cfiles = vec_size(active_target.csources);

	if (output_file_count + cfiles > MAX_OUTPUT_FILES)
	{
		error_exit("Too many output files.");
	}
	if (!output_file_count)
	{
		error_exit("No output files found.");
	}

	CompileData *compile_data = ccalloc(sizeof(CompileData), output_file_count);
	const char **obj_files = cmalloc(sizeof(char*) * (output_file_count + cfiles));

	if (cfiles)
	{
		platform_compiler(active_target.csources, cfiles, active_target.cflags);
		for (int i = 0; i < cfiles; i++)
		{
			char *filename = NULL;
			bool split_worked = filenamesplit(active_target.csources[i], &filename, NULL);
			assert(split_worked);
			size_t len = strlen(filename);
			// .c -> .o (quick hack to fix the name on linux)
			filename[len - 1] = 'o';
			obj_files[output_file_count + i] = filename;
		}

	}


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

	output_file_count += cfiles;
	free(compile_data);

	if (create_exe)
	{
		const char *output_name = active_target_name();
		if (active_target.arch_os_target == default_target)
		{
			platform_linker(output_name, obj_files, output_file_count);
		}
		else
		{
			if (!obj_format_linking_supported(platform_target.object_format) || !linker(output_name, obj_files,
			                                                                            output_file_count))
			{
				printf("No linking is performed due to missing linker support.\n");
				active_target.run_after_compile = false;
			}
		}
		if (active_target.run_after_compile)
		{
			system(strformat("./%s", output_name));
		}
	}

	free(obj_files);
}

static const char **target_expand_source_names(const char** dirs, const char *suffix1, const char *suffix2, bool error_on_mismatch)
{
	const char **files = NULL;
	size_t len1 = strlen(suffix1);
	size_t len2 = strlen(suffix2);
	VECEACH(dirs, i)
	{
		const char *name = dirs[i];
		size_t name_len = strlen(name);
		if (name_len < 1) goto INVALID_NAME;
		if (name[name_len - 1] == '*')
		{
			if (name_len == 1 || name[name_len - 2] == '/')
			{
				char *path = copy_string(name, name_len - 1);
				file_add_wildcard_files(&files, path, false, suffix1, suffix2);
				continue;
			}
			if (name[name_len - 2] != '*') goto INVALID_NAME;
			if (name_len == 2 || name[name_len - 3] == '/')
			{
				char *path = copy_string(name, name_len - 2);
				file_add_wildcard_files(&files, path, true, suffix1, suffix2);
				continue;
			}
			goto INVALID_NAME;
		}
		if (name_len < 4) goto INVALID_NAME;
		if (strcmp(&name[name_len - len1], suffix1) != 0 &&
		    (name_len < 5 || strcmp(&name[name_len - len2], suffix2) != 0)) goto INVALID_NAME;
		vec_add(files, name);
		continue;
		INVALID_NAME:
		if (!error_on_mismatch) continue;
		error_exit("File names must end with %s or they cannot be compiled: '%s' is invalid.", name, suffix1);
	}
	return files;
}

void compile_target(BuildOptions *options)
{
	init_default_build_target(&active_target, options);
	compile();
}

void compile_file_list(BuildOptions *options)
{
	init_build_target(&active_target, options);
	compile();
}

static void setup_int_define(const char *id, uint64_t i, Type *type)
{
	TokenType token_type = TOKEN_CONST_IDENT;
	id = symtab_add(id, (uint32_t) strlen(id), fnv1a(id, (uint32_t) strlen(id)), &token_type);
	Expr *expr = expr_new(EXPR_CONST, INVALID_SPAN);
	assert(type_is_integer(type));
	expr_const_set_int(&expr->const_expr, i, type->type_kind);
	if (expr_const_will_overflow(&expr->const_expr, type->type_kind))
	{
		error_exit("Integer define %s overflow.", id);
	}
	expr->type = type;
	expr->const_expr.narrowable = true;
	expr->span = INVALID_SPAN;
	expr->resolve_status = RESOLVE_NOT_DONE;
	void *previous = htable_set(&global_context.compiler_defines, id, expr);
	if (previous)
	{
		error_exit("Redefined ident %s", id);
	}
}

static void setup_bool_define(const char *id, bool value)
{
	TokenType token_type = TOKEN_CONST_IDENT;
	id = symtab_add(id, (uint32_t) strlen(id), fnv1a(id, (uint32_t) strlen(id)), &token_type);
	Expr *expr = expr_new(EXPR_CONST, INVALID_SPAN);
	expr_const_set_bool(&expr->const_expr, value);
	expr->type = type_bool;
	expr->span = INVALID_SPAN;
	expr->resolve_status = RESOLVE_NOT_DONE;
	void *previous = htable_set(&global_context.compiler_defines, id, expr);
	if (previous)
	{
		error_exit("Redefined ident %s", id);
	}
}

void print_syntax(BuildOptions *options)
{
	symtab_init(64 * 1024);

	if (options->print_keywords)
	{
		int index = 1;
		for (int i = 1; i < TOKEN_LAST; i++)
		{
			const char *name = token_type_to_string((TokenType)i);
			if (name[0] == '$' || (name[0] >= 'a' && name[0] <= 'z'))
			{
				if (name[1] == '$' || name[1] == '\0') continue;
				printf("%2d %s\n", index++, name);
			}
		}
	}
	if (options->print_operators)
	{
		int index = 1;
		for (int i = 1; i < TOKEN_LAST; i++)
		{
			if (i == TOKEN_DOCS_START || i == TOKEN_DOCS_END) continue;
			const char *name = token_type_to_string((TokenType)i);
			char first_char = name[0];
			if (first_char == '$' || first_char == '@'
				|| (first_char >= 'a' && first_char <= 'z')
				|| (first_char >= 'A' && first_char <= 'Z'))
			{
				continue;
			}
			printf("%2d %s\n", index++, name);
		}
	}
	if (options->print_attributes)
	{
		for (int i = 0; i < NUMBER_OF_ATTRIBUTES; i++)
		{
			printf("%2d @%s\n", i + 1, attribute_list[i]);
		}
	}
	if (options->print_builtins)
	{
		for (int i = 0; i < NUMBER_OF_BUILTINS; i++)
		{
			printf("%2d $$%s\n", i + 1, builtin_list[i]);
		}
	}
	if (options->print_precedence)
	{
		puts("precedence     | operators");
		puts("---------------+----------");
		puts(" 1. Macro      | @        ");
		puts(" 2. Call       | . () [] postfix ++/-- postfix !");
		puts(" 3. Unary      | ! - + ~ * & prefix ++/-- try catch (cast)");
		puts(" 4. Mult       | * / %");
		puts(" 5. Shift      | << >>");
		puts(" 6. Bitwise    | ^ | &");
		puts(" 7. Additive   | + -");
		puts(" 8. Relational | < > <= >= == !=");
		puts(" 9. And        | &&");
		puts("10. Or         | ||");
		puts("11. Ternary    | ?: ??");
		puts("12. Assign     | = *= /= %= -= += |= &= ^= <<= >>=");
	}

}
void compile()
{
	active_target.sources = target_expand_source_names(active_target.source_dirs, ".c3", ".c3t", true);
	if (active_target.csource_dirs)
	{
		active_target.csources = target_expand_source_names(active_target.csource_dirs, ".c", ".c", false);
	}
	global_context.sources = active_target.sources;
	symtab_init(active_target.symtab_size);
	target_setup(&active_target);

	setup_int_define("C_SHORT_SIZE", platform_target.width_c_short, type_long);
	setup_int_define("C_INT_SIZE", platform_target.width_c_int, type_long);
	setup_int_define("C_LONG_SIZE", platform_target.width_c_long, type_long);
	setup_int_define("C_LONG_LONG_SIZE", platform_target.width_c_long_long, type_long);
	setup_bool_define("C_CHAR_IS_SIGNED", platform_target.signed_c_char);
	setup_bool_define("PLATFORM_BIG_ENDIAN", platform_target.big_endian);
	setup_bool_define("PLATFORM_I128_SUPPORTED", platform_target.int128);
	setup_int_define("COMPILER_OPT_LEVEL", (uint64_t)active_target.optimization_level, type_int);
	setup_int_define("OS_TYPE", (uint64_t)platform_target.os, type_int);
	setup_int_define("COMPILER_SIZE_OPT_LEVEL", (uint64_t)active_target.size_optimization_level, type_int);
	setup_bool_define("COMPILER_SAFE_MODE", active_target.feature.safe_mode);

	type_init_cint();

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




void global_context_add_decl(Decl *decl)
{
	decltable_set(&global_context.symbols, decl);
}

void global_context_add_generic_decl(Decl *decl)
{
	decltable_set(&global_context.generic_symbols, decl);
}

void global_context_add_type(Type *type)
{
	DEBUG_LOG("Created type %s.", type->name);
	assert(type_ok(type));
	vec_add(global_context.type, type);
}

const char *get_object_extension(void)
{
	switch (active_target.arch_os_target)
	{
		case X64_WINDOWS:
		case X86_WINDOWS:
		case X64_WINDOWS_GNU:
			return ".obj";
		default:
			return ".o";
	}
}

Module *global_context_find_module(const char *name)
{
	return htable_get(&global_context.modules, name);
}

Module *compiler_find_or_create_module(Path *module_name, const char **parameters, bool is_private)
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
	htable_init(&module->symbols, 0x10000);
	htable_set(&global_context.modules, module_name->module, module);
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

char *scratch_buffer_copy(void)
{
	return copy_string(global_context.scratch_buffer, global_context.scratch_buffer_len);
}
