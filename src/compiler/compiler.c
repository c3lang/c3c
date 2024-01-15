// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"
#include <compiler_tests/benchmark.h>
#include "../utils/whereami.h"

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

	htable_init(&global_context.features, 1024);
	htable_init(&global_context.compiler_defines, 16 * 1024);
	global_context.module_list = NULL;
	global_context.generic_module_list = NULL;
	global_context.method_extensions = NULL;
	global_context.section_list = NULL;
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
	data->object_name = tilde_codegen(data->context);
}

#if !TB_AVAILABLE
const char *tilde_codegen(void *context)
{
	error_exit("TB backend not available.");
}
void **tilde_gen(Module** modules, unsigned module_count)
{
	error_exit("TB backend not available.");
}

#endif

const char *build_base_name(void)
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
	return name;
}

static const char *exe_name(void)
{
	assert(global_context.main || active_target.no_entry);
	const char *name;
	if (active_target.name || active_target.no_entry)
	{
		name = active_target.name ? active_target.name : "out";
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
		case WINDOWS_AARCH64:
		case WINDOWS_X64:
		case MINGW_X64:
			return str_cat(name, ".exe");
		default:
			if (arch_is_wasm(platform_target.arch)) return str_cat(name, ".wasm");
			return name;
	}
}

static const char *dynamic_lib_name(void)
{
	const char *name = build_base_name();
	switch (active_target.arch_os_target)
	{
		case WINDOWS_AARCH64:
		case WINDOWS_X64:
		case MINGW_X64:
			return str_cat(name, ".dll");
		case MACOS_X64:
		case MACOS_AARCH64:
			return str_cat(name, ".dylib");
		default:
			return str_cat(name, ".a");
	}
}

static const char *static_lib_name(void)
{
	const char *name = build_base_name();
	switch (active_target.arch_os_target)
	{
		case WINDOWS_AARCH64:
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

void delete_object_files(const char **files, size_t count)
{
	for (size_t i = 0; i < count; i++)
	{
		file_delete_file(files[i]);
	}
}

void compiler_parse(void)
{
	// Cleanup any errors (could there really be one here?!)
	global_context_clear_errors();

	// Add the standard library
	if (global_context.lib_dir && !no_stdlib())
	{
		file_add_wildcard_files(&global_context.sources, global_context.lib_dir, true, c3_suffix_list, 3);
	}

	// Load and parse all files.
	bool has_error = false;
	VECEACH(global_context.sources, i)
	{
		bool loaded = false;
		const char *error;
		File *file = source_file_load(global_context.sources[i], &loaded, &error);
		if (!file) error_exit(error);
		if (loaded) continue;
		if (!parse_file(file)) has_error = true;
	}
	if (active_target.read_stdin)
	{
		if (!parse_stdin()) has_error = true;
	}

	if (has_error) exit_compiler(EXIT_FAILURE);
	compiler_parsing_time = bench_mark();
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
		header_gen(modules, module_count);
	}

	if (active_target.check_only)
	{
		free_arenas();
		return;
	}

	void **gen_contexts;
	void (*task)(void *);

	if (active_target.asm_file_dir || active_target.ir_file_dir || active_target.emit_object_files)
	{
		if (active_target.build_dir && !file_exists(active_target.build_dir) && !dir_make(active_target.build_dir))
		{
			error_exit("Failed to create build directory '%s'.", active_target.build_dir);
		}
	}
	if (active_target.ir_file_dir && active_target.emit_llvm)
	{
		if (!file_exists(active_target.ir_file_dir) && !dir_make(active_target.ir_file_dir))
		{
			error_exit("Failed to create output directory '%s'.", active_target.ir_file_dir);
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
			gen_contexts = tilde_gen(modules, module_count);
			task = &thread_compile_task_tb;
			break;
		default:
			UNREACHABLE
	}
	compiler_ir_gen_time = bench_mark();

	const char *output_exe = NULL;
	const char *output_static = NULL;
	const char *output_dynamic = NULL;
	if (!active_target.test_output && !active_target.benchmark_output)
	{
		switch (active_target.type)
		{
			case TARGET_TYPE_BENCHMARK:
				active_target.name = "benchmarkrun";
				output_exe = exe_name();
				break;
			case TARGET_TYPE_TEST:
				active_target.name = "testrun";
				output_exe = exe_name();
				break;
			case TARGET_TYPE_EXECUTABLE:
				if (!global_context.main && !active_target.no_entry)
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
		for (int i = 0; i < cfiles; i++)
		{
			const char *file = active_target.csources[i];
			const char *obj = platform_compiler(file, active_target.cflags);
			obj_files[output_file_count + i] = obj;
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
	unsigned task_count = vec_size(tasks);
	if (task_count == 1)
	{
		tasks[0]->task(tasks[0]->arg);
	}
	else if (task_count > 1)
	{
		taskqueue_run(active_target.build_threads > task_count ? task_count : active_target.build_threads, tasks);
	}

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
		if (active_target.output_dir) output_exe = file_append_path(active_target.output_dir, output_exe);
		if (file_is_dir(output_exe))
		{
			error_exit("Cannot create exe with the name '%s' - there is already a directory with that name.", output_exe);
		}
		bool system_linker_available = link_libc() && platform_target.os != OS_TYPE_WIN32;
		bool use_system_linker = system_linker_available && active_target.arch_os_target == default_target;
		switch (active_target.system_linker)
		{
			case SYSTEM_LINKER_ON:
				if (!system_linker_available)
				{
					eprintf("System linker is not supported, defaulting to built-in linker\n");
					break;
				}
				use_system_linker = true;
				break;
			case SYSTEM_LINKER_OFF:
				use_system_linker = false;
				break;
			default:
				break;
		}
		if (use_system_linker)
		{
			platform_linker(output_exe, obj_files, output_file_count);
			compiler_link_time = bench_mark();
			compiler_print_bench();
			delete_object_files(obj_files, output_file_count);
		}
		else
		{
			compiler_print_bench();
			if (!obj_format_linking_supported(platform_target.object_format) || !linker(output_exe, obj_files,
																						output_file_count))
			{
				eprintf("No linking is performed due to missing linker support.\n");
				active_target.run_after_compile = false;
			}
			else
			{
				delete_object_files(obj_files, output_file_count);
			}
		}

		if (active_target.run_after_compile)
		{
			DEBUG_LOG("Will run");
			printf("Launching %s...\n", output_exe);
			int ret = system(platform_target.os == OS_TYPE_WIN32 ? output_exe : str_printf("./%s", output_exe));
			printf("Program finished with exit code %d.\n", ret);
			if (ret != 0) exit(EXIT_FAILURE);
		}
	}
	else if (output_static)
	{
		if (active_target.output_dir) output_static = file_append_path(active_target.output_dir, output_static);
		if (file_is_dir(output_static))
		{
			error_exit("Cannot create a static library with the name '%s' - there is already a directory with that name.", output_exe);
		}
		if (!static_lib_linker(output_static, obj_files, output_file_count))
		{
			error_exit("Failed to produce static library '%s'.", output_static);
		}
		delete_object_files(obj_files, output_file_count);
		compiler_link_time = bench_mark();
		compiler_print_bench();
		printf("Static library '%s' created.\n", output_static);
	}
	else if (output_dynamic)
	{
		if (active_target.output_dir) output_dynamic = file_append_path(active_target.output_dir, output_dynamic);
		if (file_is_dir(output_dynamic))
		{
			error_exit("Cannot create a dynamic library with the name '%s' - there is already a directory with that name.", output_exe);
		}
		if (!dynamic_lib_linker(output_dynamic, obj_files, output_file_count))
		{
			error_exit("Failed to produce dynamic library '%s'.", output_dynamic);
		}
		delete_object_files(obj_files, output_file_count);
		printf("Dynamic library '%s' created.\n", output_dynamic);
		compiler_link_time = bench_mark();
		compiler_print_bench();
	}
	else
	{
		compiler_print_bench();
	}
	free(obj_files);
}

static const char **target_expand_source_names(const char** dirs, const char **suffix_list, int suffix_count, bool error_on_mismatch)
{
	const char **files = NULL;
	VECEACH(dirs, i)
	{
		const char *name = dirs[i];
		INFO_LOG("Searching for sources in %s", name);
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
			INFO_LOG("Searching for wildcard sources in %s", name);
			if (name_len == 2 || name[name_len - 3] == '/')
			{
				char *path = str_copy(name, name_len - 2);
				DEBUG_LOG("Reduced path %s", path);
				file_add_wildcard_files(&files, path, true, suffix_list, suffix_count);
				continue;
			}
			goto INVALID_NAME;
		}
		if (!file_has_suffix_in_list(name, name_len, suffix_list, suffix_count)) goto INVALID_NAME;
		vec_add(files, name);
		continue;
		INVALID_NAME:
		if (file_is_dir(name))
		{
			file_add_wildcard_files(&files, name, true, suffix_list, suffix_count);
			continue;
		}
		if (!error_on_mismatch) continue;
		error_exit("File names must be a non-empty name followed by %s or they cannot be compiled: '%s' is invalid.", suffix_list[0], name);
	}
	return files;
}

void compile_target(BuildOptions *options)
{
	init_default_build_target(&active_target, options);
	compile();
}

void clean_obj_files(void)
{
	file_delete_all_files_in_dir_with_suffix(active_target.ir_file_dir, ".ll");
	file_delete_all_files_in_dir_with_suffix(active_target.asm_file_dir, ".s");
	file_delete_all_files_in_dir_with_suffix(active_target.object_file_dir, ".obj");
	file_delete_all_files_in_dir_with_suffix(active_target.object_file_dir, ".o");
}
void compile_clean(BuildOptions *options)
{
	init_build_target(&active_target, options);
	clean_obj_files();
}
void compile_file_list(BuildOptions *options)
{
	init_build_target(&active_target, options);
	if (options->command == COMMAND_CLEAN_RUN)
	{
		clean_obj_files();
	}
	compile();
}

static void setup_int_define(const char *id, uint64_t i, Type *type)
{
	TokenType token_type = TOKEN_CONST_IDENT;
	id = symtab_add(id, (uint32_t) strlen(id), fnv1a(id, (uint32_t) strlen(id)), &token_type);
	Type *flat = type_flatten(type);
	assert(type_is_integer(flat));
	Expr *expr = expr_new_const_int(INVALID_SPAN, flat, i);
	expr->type = type;
	if (expr_const_will_overflow(&expr->const_expr, flat->type_kind))
	{
		error_exit("Integer define %s overflow.", id);
	}
	void *previous = htable_set(&global_context.compiler_defines, (void*)id, expr);
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
	void *previous = htable_set(&global_context.compiler_defines, (void *)id, expr);
	if (previous)
	{
		error_exit("Redefined ident %s", id);
	}
}
#if FETCH_AVAILABLE
void vendor_fetch(BuildOptions *options)
{
	unsigned count = 0;
	FOREACH_BEGIN(const char *lib, options->libraries_to_fetch)
		const char *resource = str_printf("/c3lang/vendor/releases/download/latest/%s.c3l", lib);
		printf("Fetching library '%s'...", lib);
		fflush(stdout);
		const char *error = download_file("https://github.com", resource, str_printf("%s.c3l", lib));
		if (!error)
		{
			puts("ok.");
			count++;
		}
		else
		{
			printf("FAILED: '%s'\n", error);
		}
	FOREACH_END();
	if (count == 0)	error_exit("Error: Failed to download any libraries.");
	if (count < vec_size(options->libraries_to_fetch)) error_exit("Error: Only some libraries were downloaded.");
}
#else
void vendor_fetch(BuildOptions *options)
{
	error_exit("Error: vendor-fetch only available when compiled with cURL.");
}
#endif

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
				printf("%3d %s\n", index++, name);
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
			printf("%3d $$%s\n", i + 1, builtin_list[i]);
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
		int width;
		puts("Project properties");
		puts("------------------");
		for (int i = 0; i < project_default_keys_count; i++)
		{
			printf("%2d %-*s%s\n", i + 1, 35, project_default_keys[i][0], project_default_keys[i][1]);
		}
		puts("");
		puts("Target properties");
		puts("-----------------");
		for (int i = 0; i < project_target_keys_count; i++)
		{
			printf("%2d %-*s%s\n", i + 1, 35, project_target_keys[i][0], project_target_keys[i][1]);
		}
		puts("");
	}
	if (options->print_precedence)
	{
		puts("precedence     | operators");
		puts("---------------+----------");
		puts(" 1. Macro      | @        ");
		puts(" 2. Call       | . () [] !! postfix ++/-- postfix !");
		puts(" 3. Unary      | ! - + ~ * & prefix ++/-- (cast)");
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
		case LINUX_RISCV32:
			// Godbolt test
			return 76;
		case ELF_RISCV64:
		case LINUX_RISCV64:
			// Godbolt test
			return 43;
		case MACOS_X64:
			return 19; // Actually 18.5
		case WINDOWS_X64: // 16 on x32
		case MINGW_X64:
			// Godbolt test
			return 32;
		case ELF_X64:
		case LINUX_X64:
			// Godbolt test
			return 25;
		case FREEBSD_X64:
		case NETBSD_X64:
		case OPENBSD_X64:
			REMINDER("Guessing setjmp for platform.");
			return 32;
		case LINUX_AARCH64:
		case ELF_AARCH64:
			return 39;
		case WINDOWS_AARCH64:
			// Based on Godbolt
			return 24;
		case MACOS_AARCH64:
			// Based on macOS headers
			return 25;
		case LINUX_X86:
		case MCU_X86:
		case NETBSD_X86:
		case OPENBSD_X86:
		case ELF_X86:
		case FREEBSD_X86:
			// Early GCC
			return 39;
		case WASM32:
		case WASM64:
			REMINDER("WASM setjmp size is unknown");
			return 512;
	}
	UNREACHABLE
}

static void execute_scripts(void)
{
	if (!vec_size(active_target.exec)) return;
	if (active_target.trust_level < TRUST_FULL)
	{
		error_exit("This target has 'exec' directives, to run it trust level must be set to '--trust=full'.");
	}
	char *old_path = NULL;
	if (active_target.script_dir)
	{
		old_path = getcwd(NULL, 0);
		if (!dir_change(active_target.script_dir))
		{
			free(old_path);
			error_exit("Failed to open script dir '%s'", active_target.script_dir);
		}
	}
	FOREACH_BEGIN(const char *exec, active_target.exec)
		StringSlice execs = slice_from_string(exec);
		StringSlice call = slice_next_token(&execs, ' ');
		if (call.len < 3 || call.ptr[call.len - 3] != '.' || call.ptr[call.len - 2] != 'c' || call.ptr[call.len - 2] != '3')
		{
			(void) execute_cmd(exec, false);
			continue;
		}
		scratch_buffer_clear();
		scratch_buffer_append_len(call.ptr, call.len);
		(void)compile_and_invoke(scratch_buffer_to_string(), execs.len ? execs.ptr : "");
	FOREACH_END();
	dir_change(old_path);
	free(old_path);
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
	execute_scripts();
	global_context.main = NULL;
	global_context.string_type = NULL;
	asm_target.initialized = false;
	target_setup(&active_target);
	resolve_libraries();
	global_context.sources = active_target.sources;

	TokenType type = TOKEN_CONST_IDENT;
	FOREACH_BEGIN(const char *feature_flag, active_target.feature_list)
		feature_flag = symtab_preset(feature_flag, TOKEN_CONST_IDENT);
		htable_set(&global_context.features, (void *)feature_flag, (void *)feature_flag);
	FOREACH_END();

	setup_int_define("C_SHORT_SIZE", platform_target.width_c_short, type_long);
	setup_int_define("C_INT_SIZE", platform_target.width_c_int, type_long);
	setup_int_define("C_LONG_SIZE", platform_target.width_c_long, type_long);
	setup_int_define("C_LONG_LONG_SIZE", platform_target.width_c_long_long, type_long);
	setup_bool_define("C_CHAR_IS_SIGNED", platform_target.signed_c_char);
	setup_bool_define("PLATFORM_BIG_ENDIAN", platform_target.big_endian);
	setup_bool_define("PLATFORM_I128_SUPPORTED", platform_target.int128);
	setup_bool_define("PLATFORM_F128_SUPPORTED", platform_target.float128);
	setup_bool_define("PLATFORM_F16_SUPPORTED", platform_target.float16);
	setup_int_define("ARCH_TYPE", (uint64_t)platform_target.arch, type_int);
	setup_int_define("MEMORY_ENVIRONMENT", (uint64_t)active_target.memory_environment, type_int);
	setup_bool_define("COMPILER_LIBC_AVAILABLE", link_libc());
	setup_int_define("COMPILER_OPT_LEVEL", (uint64_t)active_target.optlevel, type_int);
	setup_int_define("OS_TYPE", (uint64_t)platform_target.os, type_int);
	setup_int_define("COMPILER_SIZE_OPT_LEVEL", (uint64_t)active_target.optsize, type_int);
	setup_bool_define("COMPILER_SAFE_MODE", safe_mode_enabled());
	setup_bool_define("DEBUG_SYMBOLS", active_target.debug_info == DEBUG_INFO_FULL);
	setup_int_define("LLVM_VERSION", llvm_version_major, type_int);
	setup_bool_define("BENCHMARKING", active_target.benchmarking);
	setup_int_define("JMP_BUF_SIZE", jump_buffer_size(), type_int);
	setup_bool_define("TESTING", active_target.testing);

	type_init_cint();

	compiler_init_time = bench_mark();

	if (!vec_size(active_target.sources) && !active_target.read_stdin) error_exit("No files to compile.");

	if (active_target.exec)
	{

	}
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
		emit_json();
		exit_compiler(COMPILER_SUCCESS_EXIT);

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

SectionId global_context_register_section(const char *section)
{
	scratch_buffer_clear();
	scratch_buffer_append("SECTION#");
	scratch_buffer_append(section);
	TokenType type = TOKEN_INVALID_TOKEN;
	const char *result = scratch_buffer_interned();
	FOREACH_BEGIN_IDX(i, const char *candidate, global_context.section_list)
		if (result == candidate) return i + 1;
	FOREACH_END();
	unsigned len = vec_size(global_context.section_list);
	if (len >= MAX_SECTIONS)
	{
		error_exit("Too many sections in source, max %d allowed.", MAX_SECTIONS);
	}
	vec_add(global_context.section_list, result);
	return len + 1;
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
		case ANY_WINDOWS_ARCH_OS:
			return ".obj";
		default:
			return ".o";
	}
}

Module *global_context_find_module(const char *name)
{
	assert(name);
	return htable_get(&global_context.modules, (void *)name);
}

Module *compiler_find_or_create_module(Path *module_name, const char **parameters)
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
	htable_init(&module->symbols, 0x1000);
	htable_set(&global_context.modules, (void *)module_name->module, module);
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

File *compile_and_invoke(const char *file, const char *args)
{
	char *name;
	if (!file_namesplit(compiler_exe_name, &name, NULL))
	{
		error_exit("Failed to extract file name from '%s'", compiler_exe_name);
	}
	const char *compiler_path = file_append_path(find_executable_path(), name);
	scratch_buffer_clear();
	scratch_buffer_append(compiler_path);
#if (_MSC_VER)
	const char *output = "__c3exec__.exe";
#else
	const char *output = "__c3exec__";
#endif
	scratch_buffer_append(" compile -g0 --single-module=yes");
	StringSlice slice = slice_from_string(file);
	while (slice.len > 0)
	{
		StringSlice file_name = slice_next_token(&slice, ';');
		if (!file_name.len) continue;
		scratch_buffer_append_char(' ');
		scratch_buffer_append_len(file_name.ptr, file_name.len);
	}
	scratch_buffer_printf(" -o %s", output);
	const char *out;
	if (!execute_cmd_failable(scratch_buffer_to_string(), &out))
	{
		error_exit("Failed to compile script '%s'.", file);
	}
	DEBUG_LOG("EXEC OUT: %s", out);
	scratch_buffer_clear();
#if (!_MSC_VER)
	scratch_buffer_append("./");
#endif
	scratch_buffer_append(output);
	scratch_buffer_append(" ");
	scratch_buffer_append(args);
	if (!execute_cmd_failable(scratch_buffer_to_string(), &out))
	{
		error_exit("Error invoking script '%s' with arguments %s.", file, args);
	}
	file_delete_file(output);
	return source_file_text_load(file, out);
}

int find_padding_length(const char** arr, const int count)
{
	int width = 0;
	for (int i = 0; i < count; i++)
	{
		const char* str = arr[i];
		int len = strlen(str);
		if (width < len) width = len;
	}
	width += 2;
	return width;
}