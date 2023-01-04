// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"
#include <compiler_tests/benchmark.h>
#ifndef _MSC_VER
#include <unistd.h>
#endif

#define MAX_OUTPUT_FILES 1000000
#define MAX_MODULES 100000
GlobalContext global_context;
BuildTarget active_target;
AsmTarget asm_target;

Vmem ast_arena;
Vmem expr_arena;
Vmem decl_arena;
Vmem type_info_arena;
double compiler_init_time;
double compiler_parsing_time;
double compiler_sema_time;
double compiler_ir_gen_time;
double compiler_codegen_time;
double compiler_link_time;

const char* c3_suffix_list[3] = { ".c3", ".c3t", ".c3i" };

extern int llvm_version_major;

void compiler_init(const char *std_lib_dir)
{
	compiler_init_time = -1;
	compiler_parsing_time = -1;
	compiler_sema_time = -1;
	compiler_ir_gen_time = -1;
	compiler_codegen_time = -1;
	compiler_link_time = -1;

	INFO_LOG("Version: %s", COMPILER_VERSION);

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
	global_context.method_extensions = NULL;
	vmem_init(&ast_arena, 512);
	ast_calloc();
	vmem_init(&expr_arena, 512);
	expr_calloc();
	vmem_init(&decl_arena, 256);
	decl_calloc();
	vmem_init(&type_info_arena, 256);
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
		const char *error;
		File *file = source_file_load(global_context.sources[i], &loaded, &error);
		if (!file) error_exit(error);
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
		const char *error;
		File *file = source_file_load(global_context.sources[i], &loaded, &error);
		if (!file) error_exit(error);
		if (loaded) continue;

		global_context_clear_errors();
		parse_file(file);
	}
	if (active_target.read_stdin)
	{
		global_context_clear_errors();
		parse_stdin();
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


static const char *exe_name(void)
{
	assert(global_context.main);
	const char *name;
	if (active_target.name)
	{
		name = active_target.name;
	}
	else
	{
		Path *path = global_context.main->unit->module->name;
		size_t first = 0;
		for (size_t i = path->len; i > 0; i--)
		{
			if (path->module[i - 1] == ':')
			{
				first = i;
				break;
			}
		}
		name = &path->module[first];
	}
	switch (active_target.arch_os_target)
	{
		case WINDOWS_X86:
		case WINDOWS_X64:
		case MINGW_X64:
			return str_cat(name, ".exe");
		default:
			return name;
	}
}

static const char *dynamic_lib_name(void) { return NULL; }

static const char *static_lib_name(void)
{
	const char *name;
	if (active_target.name)
	{
		name = active_target.name;
	}
	else
	{
		assert(vec_size(global_context.module_list));
		Path *path = global_context.module_list[0]->name;
		size_t first = 0;
		for (size_t i = path->len; i > 0; i--)
		{
			if (path->module[i - 1] == ':')
			{
				first = i;
				break;
			}
		}
		name = &path->module[first];
	}
	switch (active_target.arch_os_target)
	{
		case WINDOWS_X86:
		case WINDOWS_X64:
		case MINGW_X64:
			return str_cat(name, ".lib");
		default:
			return str_cat(name, ".a");
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

static void compiler_print_bench(void)
{
	if (debug_stats)
	{
		printf("Timings\n");
		printf("-------\n");
		if (compiler_init_time >= 0) printf("Initialization took: %.4f ms\n", compiler_init_time * 1000);
		if (compiler_parsing_time >= 0) printf("Parsing took:        %.4f ms\n", (compiler_parsing_time - compiler_init_time) * 1000);
		if (compiler_sema_time >= 0) printf("Analysis took:       %.4f ms\n", (compiler_sema_time - compiler_parsing_time) * 1000);
		if (compiler_ir_gen_time >= 0) printf("Ir gen took:         %.4f ms\n", (compiler_ir_gen_time - compiler_sema_time) * 1000);
		if (compiler_codegen_time >= 0) printf("Codegen took:        %.4f ms\n", (compiler_codegen_time - compiler_ir_gen_time) * 1000);
		if (compiler_link_time >= 0) printf("Linking took:        %f ms\n", (compiler_link_time - compiler_codegen_time) * 1000);
	}
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
			REMINDER("Header gen is needed");
			// header_gen(modules[i]);
		}
	}

	if (active_target.check_only)
	{
		free_arenas();
		return;
	}

	void **gen_contexts = VECNEW(void*, module_count);
	void (*task)(void *);

	if (active_target.asm_file_dir || active_target.llvm_file_dir || active_target.emit_object_files)
	{
		if (active_target.build_dir && !file_exists(active_target.build_dir) && !dir_make(active_target.build_dir))
		{
			error_exit("Failed to create build directory '%s'.", active_target.build_dir);
		}
	}
	if (active_target.llvm_file_dir && active_target.emit_llvm)
	{
		if (!file_exists(active_target.llvm_file_dir) && !dir_make(active_target.llvm_file_dir))
		{
			error_exit("Failed to create output directory '%s'.", active_target.llvm_file_dir);
		}
	}
	if (active_target.asm_file_dir && active_target.emit_asm)
	{
		if (!file_exists(active_target.asm_file_dir) && !dir_make(active_target.asm_file_dir))
		{
			error_exit("Failed to create output directory '%s'.", active_target.asm_file_dir);
		}
	}
	if (active_target.object_file_dir && active_target.emit_object_files)
	{
		if (!file_exists(active_target.object_file_dir) && !dir_make(active_target.object_file_dir))
		{
			error_exit("Failed to create output directory '%s'.", active_target.object_file_dir);
		}
	}
	switch (active_target.backend)
	{
		case BACKEND_LLVM:
			gen_contexts = llvm_gen(modules, module_count);
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
	compiler_ir_gen_time = bench_mark();

	const char *output_exe = NULL;
	const char *output_static = NULL;
	const char *output_dynamic = NULL;
	if (!active_target.test_output)
	{
		switch (active_target.type)
		{
			case TARGET_TYPE_TEST:
				active_target.name = "testrun";
				output_exe = exe_name();
				break;
			case TARGET_TYPE_EXECUTABLE:
				if (!global_context.main)
				{
					puts("No main function was found, compilation only object files are generated.");
				}
				else
				{
					output_exe = exe_name();
				}
				break;
			case TARGET_TYPE_STATIC_LIB:
				output_static = static_lib_name();
				break;
			case TARGET_TYPE_DYNAMIC_LIB:
				output_dynamic = dynamic_lib_name();
				break;
			case TARGET_TYPE_OBJECT_FILES:
				break;
			default:
				UNREACHABLE
		}
	}

	free_arenas();

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
			bool split_worked = file_namesplit(active_target.csources[i], &filename, NULL);
			assert(split_worked);
			size_t len = strlen(filename);
			// .c -> .o (quick hack to fix the name on linux)
			filename[len - 1] = 'o';
			obj_files[output_file_count + i] = filename;
		}
	}

	Task **tasks = NULL;
	for (unsigned i = 0; i < output_file_count; i++)
	{
		compile_data[i] = (CompileData) { .context = gen_contexts[i] };
		compile_data[i].task = (Task) { task, &compile_data[i] };
		vec_add(tasks, &compile_data[i].task);
	}

#if USE_PTHREAD
	INFO_LOG("Will use %d thread(s).", active_target.build_threads);
#endif

	TaskQueueRef queue = taskqueue_create(active_target.build_threads, tasks);
	taskqueue_wait_for_completion(queue);

	if (active_target.print_output)
	{
		puts("# output-files-begin");
	}
	for (unsigned i = 0; i < output_file_count; i++)
	{
		obj_files[i] = compile_data[i].object_name;
		if (active_target.print_output)
		{
			puts(obj_files[i]);
		}
		assert(obj_files[i] || !output_exe);
	}
	if (active_target.print_output)
	{
		puts("# output-files-end");
	}

	output_file_count += cfiles;
	free(compile_data);
	compiler_codegen_time = bench_mark();

	if (output_exe)
	{
		if (!active_target.no_libc && platform_target.os != OS_TYPE_WIN32 && active_target.arch_os_target == default_target && !active_target.force_linker)
		{
			platform_linker(output_exe, obj_files, output_file_count);
			compiler_link_time = bench_mark();
			compiler_print_bench();
		}
		else
		{
			compiler_print_bench();
			if (!obj_format_linking_supported(platform_target.object_format) || !linker(output_exe, obj_files,
			                                                                            output_file_count))
			{
				printf("No linking is performed due to missing linker support.\n");
				active_target.run_after_compile = false;
			}
		}

		if (active_target.run_after_compile)
		{
			DEBUG_LOG("Will run");
			printf("Launching %s...\n", output_exe);
			int ret = system(platform_target.os == OS_TYPE_WIN32 ? output_exe : str_printf("./%s", output_exe));
			printf("Program finished with exit code %d.\n", ret);
		}
	}
	if (output_static)
	{
		if (!static_lib_linker(output_static, obj_files, output_file_count))
		{
			error_exit("Failed to produce static library '%s'.", output_static);
		}
		printf("Static library '%s' created.", output_static);
	}
	if (output_dynamic)
	{
		if (!dynamic_lib_linker(output_dynamic, obj_files, output_file_count))
		{
			error_exit("Failed to produce static library '%s'.", output_dynamic);
		}
		printf("Dynamic library '%s' created.", output_dynamic);
	}
	free(obj_files);
}

static const char **target_expand_source_names(const char** dirs, const char **suffix_list, int suffix_count, bool error_on_mismatch)
{
	const char **files = NULL;
	VECEACH(dirs, i)
	{
		const char *name = dirs[i];
		DEBUG_LOG("Searching for sources in %s", name);
		size_t name_len = strlen(name);
		if (name_len < 1) goto INVALID_NAME;
		if (name[name_len - 1] == '*')
		{
			if (name_len == 1 || name[name_len - 2] == '/')
			{
				char *path = str_copy(name, name_len - 1);
				file_add_wildcard_files(&files, path, false, suffix_list, suffix_count);
				continue;
			}
			if (name[name_len - 2] != '*') goto INVALID_NAME;
			DEBUG_LOG("Searching for wildcard sources in %s", name);
			if (name_len == 2 || name[name_len - 3] == '/')
			{
				char *path = str_copy(name, name_len - 2);
				DEBUG_LOG("Reduced path %s", path);
				file_add_wildcard_files(&files, path, true, suffix_list, suffix_count);
				continue;
			}
			goto INVALID_NAME;
		}
		if (name_len < 4) goto INVALID_NAME;
		if (name_len < 5 || !file_has_suffix_in_list(name, name_len, suffix_list, suffix_count)) goto INVALID_NAME;
		vec_add(files, name);
		continue;
		INVALID_NAME:
		if (file_is_dir(name))
		{
			file_add_wildcard_files(&files, name, true, suffix_list, suffix_count);
			continue;
		}
		if (!error_on_mismatch) continue;
		error_exit("File names must end with %s or they cannot be compiled: '%s' is invalid.", suffix_list[0], name);
	}
	return files;
}

void compile_target(BuildOptions *options)
{
	init_default_build_target(&active_target, options);
	compile();
}

void compile_clean(BuildOptions *options)
{
	init_build_target(&active_target, options);
	file_delete_all_files_in_dir_with_suffix(active_target.build_dir, get_object_extension());
}
void compile_file_list(BuildOptions *options)
{
	init_build_target(&active_target, options);
	if (options->command == COMMAND_CLEAN_RUN)
	{
		file_delete_all_files_in_dir_with_suffix(active_target.build_dir, get_object_extension());
	}
	compile();
}

static void setup_int_define(const char *id, uint64_t i, Type *type)
{
	TokenType token_type = TOKEN_CONST_IDENT;
	id = symtab_add(id, (uint32_t) strlen(id), fnv1a(id, (uint32_t) strlen(id)), &token_type);
	assert(type_is_integer(type));
	Expr *expr = expr_new_const_int(INVALID_SPAN, type, i, true);
	if (expr_const_will_overflow(&expr->const_expr, type->type_kind))
	{
		error_exit("Integer define %s overflow.", id);
	}
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
	Expr *expr = expr_new_const_bool(INVALID_SPAN, type_bool, value);
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
			printf("%2d %s\n", i + 1, attribute_list[i]);
		}
	}
	if (options->print_builtins)
	{
		for (int i = 0; i < NUMBER_OF_BUILTINS; i++)
		{
			printf("%2d $$%s\n", i + 1, builtin_list[i]);
		}
		puts("---");
		for (int i = 0; i < NUMBER_OF_BUILTIN_DEFINES; i++)
		{
			printf("%2d $$%s\n", i + 1, builtin_defines[i]);
		}
	}
	if (options->print_type_properties)
	{
		for (int i = 0; i < NUMBER_OF_TYPE_PROPERTIES; i++)
		{
			printf("%2d .%s\n", i + 1, type_property_list[i]);
		}
	}
	if (options->print_project_properties)
	{
		puts("Project properties");
		puts("------------------");
		for (int i = 0; i < project_default_keys_count; i++)
		{
			printf("%2d %s\n", i + 1, project_default_keys[i]);
		}
		puts("");
		puts("Target properties");
		puts("-----------------");
		for (int i = 0; i < project_target_keys_count; i++)
		{
			printf("%2d %s\n", i + 1, project_target_keys[i]);
		}
		puts("");
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

void resolve_libraries(void);

static int jump_buffer_size()
{
	switch (active_target.arch_os_target)
	{
		case ARCH_OS_TARGET_DEFAULT:
			return 512;
		case ELF_RISCV32:
		case ELF_RISCV64:
		case LINUX_RISCV32:
		case LINUX_RISCV64:
			REMINDER("RISCV jmpbuf size is unknown");
			return 512;
		case ELF_X64:
		case FREEBSD_X64:
		case LINUX_X64:
		case MACOS_X64:
		case WINDOWS_X64:
		case MINGW_X64:
		case NETBSD_X64:
		case OPENBSD_X64:
			// Based on MacOS headers
			return ((9 * 2) + 3 + 16);
		case LINUX_AARCH64:
		case ELF_AARCH64:
		case MACOS_AARCH64:
			// Based on MacOS headers
			return ((14 + 8 + 2) * 2);
		case LINUX_X86:
		case MCU_X86:
		case NETBSD_X86:
		case OPENBSD_X86:
		case WINDOWS_X86:
		case ELF_X86:
		case FREEBSD_X86:
			return 18;
		case WASM32:
		case WASM64:
			REMINDER("WASM setjmp size is unknown");
			return 512;
	}
	UNREACHABLE
}
void compile()
{
	symtab_init(active_target.symtab_size);
	active_target.sources = target_expand_source_names(active_target.source_dirs, c3_suffix_list, 3, true);
	if (active_target.csource_dirs)
	{
		static const char* c_suffix_list[3] = { ".c" };
		active_target.csources = target_expand_source_names(active_target.csource_dirs, c_suffix_list, 1, false);
	}
	global_context.sources = active_target.sources;
	global_context.main = NULL;
	asm_target.initialized = false;
	target_setup(&active_target);
	resolve_libraries();

	setup_int_define("C_SHORT_SIZE", platform_target.width_c_short, type_long);
	setup_int_define("C_INT_SIZE", platform_target.width_c_int, type_long);
	setup_int_define("C_LONG_SIZE", platform_target.width_c_long, type_long);
	setup_int_define("C_LONG_LONG_SIZE", platform_target.width_c_long_long, type_long);
	setup_bool_define("C_CHAR_IS_SIGNED", platform_target.signed_c_char);
	setup_bool_define("PLATFORM_BIG_ENDIAN", platform_target.big_endian);
	setup_bool_define("PLATFORM_I128_SUPPORTED", platform_target.int128);
	setup_bool_define("PLATFORM_F128_SUPPORTED", platform_target.float128);
	setup_bool_define("PLATFORM_F16_SUPPORTED", platform_target.float16);
	setup_bool_define("COMPILER_LIBC_AVAILABLE", !active_target.no_libc);
	setup_int_define("COMPILER_OPT_LEVEL", (uint64_t)active_target.optimization_level, type_int);
	setup_int_define("OS_TYPE", (uint64_t)platform_target.os, type_int);
	setup_int_define("COMPILER_SIZE_OPT_LEVEL", (uint64_t)active_target.size_optimization_level, type_int);
	setup_bool_define("COMPILER_SAFE_MODE", active_target.feature.safe_mode);
	setup_int_define("LLVM_VERSION", llvm_version_major, type_int);
	setup_bool_define("BENCHMARKING", active_target.benchmarking);
	setup_int_define("JMP_BUF_SIZE", jump_buffer_size(), type_int);
	setup_bool_define("TESTING", active_target.testing);

	type_init_cint();

	compiler_init_time = bench_mark();

	if (!vec_size(active_target.sources) && !active_target.read_stdin) error_exit("No files to compile.");
	if (active_target.lex_only)
	{
		compiler_lex();
		compiler_parsing_time = bench_mark();
		return;
	}
	if (active_target.parse_only)
	{
		compiler_parse();
		compiler_parsing_time = bench_mark();
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

void global_context_clear_errors(void)
{
	global_context.in_panic_mode = false;
	global_context.errors_found = 0;
	global_context.warnings_found = 0;
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
		case WINDOWS_X64:
		case WINDOWS_X86:
		case MINGW_X64:
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


const char *scratch_buffer_interned(void)
{
	TokenType type = TOKEN_INVALID_TOKEN;
	return symtab_add(scratch_buffer.str, scratch_buffer.len,
	                  fnv1a(scratch_buffer.str, scratch_buffer.len), &type);
}

