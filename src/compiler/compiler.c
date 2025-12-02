// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"
#include "../build/project.h"
#include <compiler_tests/benchmark.h>
#include "../utils/whereami.h"
#if LLVM_AVAILABLE
#include "c3_llvm.h"
#endif
#include "git_hash.h"
#include <errno.h>

#define MAX_OUTPUT_FILES 1000000
#define MAX_MODULES 100000


CompilerState compiler;

Vmem ast_arena;
Vmem expr_arena;
Vmem decl_arena;
Vmem type_info_arena;

static double compiler_init_time;
static double compiler_parsing_time;
static double compiler_sema_time;
static double compiler_exec_time;
static double compiler_ir_gen_time;
static double compiler_codegen_time;
static double compiler_link_time;

const char* c3_suffix_list[3] = { ".c3", ".c3t", ".c3i" };


static const char *out_name(void)
{
	if (compiler.build.output_name) return compiler.build.output_name;
	if (compiler.build.name) return compiler.build.name;
	return NULL;
}

#define START_VMEM_SIZE (sizeof(size_t) == 4 ? 1024 : 4096)

void compiler_init(BuildOptions *build_options)
{
	// Process --path
	if (build_options->path && !dir_change(build_options->path))
	{
		error_exit("Failed to change path to '%s'.", build_options->path);
	}

	FOREACH(const char *, dir, build_options->unchecked_directories)
	{
		(void)check_dir(dir);
	}

	compiler_init_time = -1;
	compiler_parsing_time = -1;
	compiler_sema_time = -1;
	compiler_ir_gen_time = -1;
	compiler_codegen_time = -1;
	compiler_link_time = -1;

	INFO_LOG("Version: %s", COMPILER_VERSION);

	compiler.context = (GlobalContext) { .in_panic_mode = false };
	// Skip library detection.
	//compiler.lib_dir = find_lib_dir();
	//DEBUG_LOG("Found std library: %s", compiler.lib_dir);
	htable_init(&compiler.context.modules, 16 * 1024);
	pathtable_init(&compiler.context.path_symbols, INITIAL_SYMBOL_MAP);
	decltable_init(&compiler.context.symbols, INITIAL_SYMBOL_MAP);

	htable_init(&compiler.context.features, 1024);
	htable_init(&compiler.context.compiler_defines, 16 * 1024);
	methodtable_init(&compiler.context.method_extensions, 16 * 1024);
	compiler.context.module_list = NULL;
	compiler.context.generic_module_list = NULL;
	compiler.context.method_extension_list = NULL;

	vmem_init(&ast_arena, START_VMEM_SIZE);
	ast_calloc();
	vmem_init(&expr_arena, START_VMEM_SIZE);
	expr_calloc();
	vmem_init(&decl_arena, START_VMEM_SIZE);
	decl_calloc();
	vmem_init(&type_info_arena, START_VMEM_SIZE);
	type_info_calloc();
	// Create zero index value.
	if (build_options->std_lib_dir)
	{
		compiler.context.lib_dir = build_options->std_lib_dir;
	}
	else
	{
		compiler.context.lib_dir = find_lib_dir();
	}

	compiler.context.ansi = build_options->ansi;
	if (build_options->print_env)
	{
		compiler.context.should_print_environment = true;
	}
	if (build_options->print_asm)
	{
		compiler.context.should_print_asm = true;
	}
}

static void compiler_lex(void)
{
	FOREACH(const char *, source, compiler.context.sources)
	{
		bool loaded = false;
		const char *error;
		File *file = source_file_load(source, &loaded, &error);
		if (!file) error_exit("%s", error);
		if (loaded) continue;
		Lexer lexer = { .file = file };
		lexer_init(&lexer);
		OUTF("# %s\n", file->full_path);
		while (lexer_next_token(&lexer))
		{
			TokenType token_type = lexer.token_type;
			OUTF("%s ", token_type_to_string(token_type));
			if (token_type == TOKEN_EOF) break;
		}
		OUTN("");
	}
	exit_compiler(COMPILER_SUCCESS_EXIT);
}



typedef struct CompileData_
{
	void *context;
	const char *object_name;
	Task task;
} CompileData;

#if LLVM_AVAILABLE
void thread_compile_task_llvm(void *compile_data)
{
	CompileData *data = compile_data;
	data->object_name = llvm_codegen(data->context);
}
#else 
void thread_compile_task_llvm(void *compile_data)
{
    error_exit("LLVM backend not available.");
}
#endif 

void thread_compile_task_tb(void *compile_data)
{
	CompileData *data = compile_data;
	data->object_name = tilde_codegen(data->context);
}

#if !TB_AVAILABLE
const char *tilde_codegen(void *context)
{
	error_exit("TB backend not available.");
	UNREACHABLE
}
void **tilde_gen(Module** modules, unsigned module_count)
{
	error_exit("TB backend not available.");
	UNREACHABLE
}

#endif

const char *build_base_name(void)
{
	const char *name = out_name();
	if (name) return name;
	Module **modules = compiler.context.module_list;
	Module *main_module = (modules[0] == compiler.context.core_module && vec_size(modules) > 1) ? modules[1] : modules[0];
	return main_module->short_path;
}

static const char *exe_name(void)
{
	ASSERT(compiler.build.output_name || compiler.build.name || compiler.context.main || compiler.build.no_entry);
	const char *name = out_name();
	if (!name && compiler.build.no_entry)
	{
		name = "out";
	}
	if (!name)
	{
		name = compiler.context.main->unit->module->short_path;
	}
	
	// Use custom extension if specified
	if (compiler.build.extension)
	{
		return str_cat(name, compiler.build.extension);
	}
	
	switch (compiler.build.arch_os_target)
	{
		case WINDOWS_AARCH64:
		case WINDOWS_X64:
		case MINGW_X64:
			return str_cat(name, ".exe");
		default:
			if (arch_is_wasm(compiler.platform.arch)) return str_cat(name, ".wasm");
			return name;
	}
}

static const char *dynamic_lib_name(void)
{
	const char *name = build_base_name();
	
	// Use custom extension if specified
	if (compiler.build.extension)
	{
		return str_cat(name, compiler.build.extension);
	}
	
	switch (compiler.build.arch_os_target)
	{
		case WINDOWS_AARCH64:
		case WINDOWS_X64:
		case MINGW_X64:
			return str_cat(name, ".dll");
		case MACOS_X64:
		case MACOS_AARCH64:
			return str_cat(name, ".dylib");
		default:
			return str_cat(name, ".so");
	}
}

static const char *static_lib_name(void)
{
	const char *name = build_base_name();
	
	// Use custom extension if specified
	if (compiler.build.extension)
	{
		return str_cat(name, compiler.build.extension);
	}
	
	switch (compiler.build.arch_os_target)
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
	if (compiler.build.print_stats)
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

	if (compiler.build.print_stats) print_arena_status();
}

static int compile_cfiles(const char *cc, const char **files, const char *flags, const char **include_dirs,
                          const char **out_files, const char *output_subdir)
{
	if (!cc) cc = default_c_compiler();
	int total = 0;
	FOREACH(const char *, file, files)
	{
		out_files[total++] = cc_compiler(cc, file, flags, include_dirs, output_subdir);
	}
	return total;
}

static void compiler_print_bench(void)
{
	if (compiler.build.print_stats)
	{
		puts("--------- Compilation time statistics --------\n");
		double last = compiler_init_time;
		double parse_time = compiler_parsing_time - compiler_init_time;
		if (compiler_parsing_time >= 0) last = compiler_parsing_time;
		double sema_time = compiler_sema_time - compiler_parsing_time;
		if (compiler_sema_time >= 0) last = compiler_sema_time;
		double ir_time = compiler_ir_gen_time - compiler_sema_time;
		if (compiler_ir_gen_time >= 0) last = compiler_ir_gen_time;
		double codegen_time = compiler_codegen_time - compiler_ir_gen_time;
		if (compiler_codegen_time >= 0) last = compiler_codegen_time;
		double link_time = compiler_link_time - compiler_codegen_time;
		if (compiler_link_time >= 0) last = compiler_link_time;
		printf("Frontend -------------------- Time --- %% total\n");
		if (compiler_init_time >= 0)
		{
			compiler_init_time -= compiler.script_time;
			printf("Initialization took: %10.3f ms  %8.1f %%\n", compiler_init_time * 1000, compiler_init_time * 100 / last);
			if (compiler.script_time > 0)
			{
				printf("Scripts took:        %10.3f ms  %8.1f %%\n", compiler.script_time * 1000, compiler.script_time * 100 / last);
			}
		}
		if (compiler_parsing_time >= 0) printf("Parsing took:        %10.3f ms  %8.1f %%\n", parse_time * 1000, parse_time * 100 / last);
		if (compiler_sema_time >= 0)
		{
			printf("Analysis took:       %10.3f ms  %8.1f %%\n", sema_time * 1000, sema_time * 100 / last);
			if (compiler.exec_time > 0)
			{
				printf(" - Scripts took:     %10.3f ms  %8.1f %%\n", compiler.exec_time * 1000, compiler.exec_time * 100 / last);
			}
			printf("TOTAL:               %10.3f ms  %8.1f %%\n", compiler_sema_time * 1000, compiler_sema_time * 100 / last);
			puts("");
		}
		if (compiler_ir_gen_time >= 0)
		{
			printf("Backend --------------------- Time --- %% total\n");
			printf("Ir gen took:         %10.3f ms  %8.1f %%\n", ir_time * 1000, ir_time * 100 / last);
			if (compiler_codegen_time >= 0)
			{
				if (compiler.build.build_threads > 1)
				{
					printf("Codegen took:        %10.3f ms  %8.1f %%  (%d threads)\n", codegen_time * 1000, codegen_time * 100 / last, compiler.build.build_threads);
				}
				else
				{
					printf("Codegen took:        %10.3f ms  %8.1f %%\n", codegen_time * 1000, codegen_time * 100 / last);
				}
			}
			if (compiler_link_time >= 0)
			{
				printf("Linking took:        %10.3f ms  %8.1f %%\n", link_time * 1000, link_time * 100 / last);
			}
			printf("TOTAL:               %10.3f ms  %8.1f %%\n", (last - compiler_sema_time) * 1000, 100 - compiler_sema_time * 100 / last);
		}
		if (last)
		{
			puts("----------------------------------------------");
			printf("TOTAL compile time: %.3f ms.\n", last * 1000);
			puts("----------------------------------------------");
		}
	}
}

void delete_object_files(const char **files, size_t count)
{
	for (size_t i = 0; i < count; i++)
	{
		assert(files);
		file_delete_file(files[i]);
	}
}

void compiler_parse(void)
{
	// Cleanup any errors (could there really be one here?!)
	global_context_clear_errors();

	// Add the standard library
	if (compiler.context.lib_dir && !no_stdlib())
	{
		file_add_wildcard_files(&compiler.context.sources, compiler.context.lib_dir, true, c3_suffix_list, 3);
	}

	// Load and parse all files.
	bool has_error = false;
	if (compiler.build.print_input)
	{
		puts("# input-files-begin");
	}
	FOREACH(const char *, source, compiler.context.sources)
	{
		bool loaded = false;
		const char *error;
		File *file = source_file_load(source, &loaded, &error);
		if (!file) error_exit("%s", error);
		if (loaded) continue;
		if (!parse_file(file)) has_error = true;
		if (compiler.build.print_input) puts(file->full_path);
	}
	if (compiler.build.print_input)
	{
		puts("# input-files-end");
	}
	if (compiler.build.read_stdin)
	{
		if (!parse_stdin()) has_error = true;
	}

	if (has_error)
	{
		if (compiler.build.lsp_output)
		{
			eprintf("> ENDLSP-ERROR\n");
			exit_compiler(COMPILER_SUCCESS_EXIT);
		}
		exit_compiler(EXIT_FAILURE);
	}
	compiler_parsing_time = bench_mark();
}

bool compiler_should_output_file(const char *file)
{
	if (!vec_size(compiler.build.emit_only)) return true;
	FOREACH(const char *, f, compiler.build.emit_only)
	{
		if (str_eq(file, f)) return true;
	}
	return false;
}

static void create_output_dir(const char *dir)
{
	if (!dir) return;
	if (strlen(dir) == 0) return;
	if (file_exists(dir))
	{
		if (!file_is_dir(dir)) error_exit("Output directory is not a directory %s.", dir);
		return;
	}
	scratch_buffer_clear();
	scratch_buffer_append(dir);
	dir_make_recursive(scratch_buffer_to_string());
	if (!file_exists(dir))
	{
		error_exit("Failed to create directory '%s'.", dir);
	}
}


void compiler_compile(void)
{
	if (compiler.build.lsp_output)
	{
		eprintf("> BEGINLSP\n");
	}
	sema_analysis_run();
	if (compiler.build.lsp_output)
	{
		eprintf("> ENDLSP-OK\n");
		exit_compiler(COMPILER_SUCCESS_EXIT);
	}
	compiler_sema_time = bench_mark();
	compiler_exec_time = compiler.exec_time;
	Module **modules = compiler.context.module_list;
	unsigned module_count = vec_size(modules);
	if (module_count > MAX_MODULES)
	{
		error_exit("Too many modules.");
	}
	if (module_count < 1 && !compiler.build.object_files)
	{
		error_exit("No module to compile.");
	}
	if (compiler.build.output_headers)
	{
		if (compiler.build.header_file_dir)
		{
			create_output_dir(compiler.build.header_file_dir);
		}
		header_gen(modules, module_count);
	}

	if (compiler.build.check_only)
	{
		free_arenas();
		return;
	}

	void **gen_contexts;
	void (*task)(void *);

	if ((compiler.build.emit_llvm || compiler.build.test_output || compiler.build.lsp_output))
	{
		create_output_dir(compiler.build.ir_file_dir);
	}
	if (compiler.build.emit_asm)
	{
		create_output_dir(compiler.build.asm_file_dir);
	}
	if (compiler.build.emit_object_files)
	{
		create_output_dir(compiler.build.object_file_dir);
	}
	if (compiler.build.type == TARGET_TYPE_EXECUTABLE && !compiler.context.main && !compiler.build.no_entry)
	{
		error_exit("The 'main' function for the executable could not found, did you forget to add it?\n\n"
				   "- If you're using an alternative entry point you can suppress this message using '--no-entry'.\n"
				   "- If you want to build a library, use 'static-lib' or 'dynamic-lib'.\n"
				   "- If you just want to output object files for later linking, use 'compile-only'.");
	}

	if (compiler.build.type == TARGET_TYPE_STATIC_LIB)
	{
		compiler.build.single_module = SINGLE_MODULE_ON;
	}
	if (compiler.build.emit_asm)
	{
		scratch_buffer_clear();
		scratch_buffer_append(compiler.build.asm_file_dir);
		dir_make_recursive(scratch_buffer_to_string());
	}
	if (compiler.build.emit_object_files)
	{
		scratch_buffer_clear();
		scratch_buffer_append(compiler.build.object_file_dir);
		dir_make_recursive(scratch_buffer_to_string());
	}
	switch (compiler.build.backend)
	{
		case BACKEND_C:
			gen_contexts = c_gen(modules, module_count);
			(void)gen_contexts;
			error_exit("Unfinished C backend!");
		case BACKEND_LLVM:
#if LLVM_AVAILABLE
			gen_contexts = llvm_gen(modules, module_count);
			task = &thread_compile_task_llvm;
#else 
            error_exit("C3C compiled without LLVM!");
#endif
            break;
		case BACKEND_TB:
			gen_contexts = tilde_gen(modules, module_count);
			task = &thread_compile_task_tb;
			break;
		default:
			UNREACHABLE_VOID
	}
	compiler_ir_gen_time = bench_mark();
	const char *output_exe = NULL;
	const char *output_static = NULL;
	const char *output_dynamic = NULL;
	if (!compiler.build.test_output && !compiler.build.benchmark_output)
	{
		switch (compiler.build.type)
		{
			case TARGET_TYPE_BENCHMARK:
				compiler.build.name = "benchmarkrun";
				compiler.build.output_name = compiler.build.runner_output_name ? compiler.build.runner_output_name : compiler.build.name;
				output_exe = exe_name();
				break;
			case TARGET_TYPE_TEST:
				compiler.build.name = "testrun";
				compiler.build.output_name = compiler.build.runner_output_name ? compiler.build.runner_output_name : compiler.build.name;
				output_exe = exe_name();
				break;
			case TARGET_TYPE_EXECUTABLE:
				ASSERT(compiler.context.main || compiler.build.no_entry);
				output_exe = exe_name();
				break;
			case TARGET_TYPE_STATIC_LIB:
				output_static = static_lib_name();
				break;
			case TARGET_TYPE_DYNAMIC_LIB:
				output_dynamic = dynamic_lib_name();
				break;
			case TARGET_TYPE_OBJECT_FILES:
				if (compiler.obj_output)
				{
					OUTF("Object file %s created.\n", compiler.obj_output);
					break;
				}
				OUTF("Object files written to %s.\n", compiler.build.object_file_dir);
				break;
			case TARGET_TYPE_PREPARE:
				break;
			default:
				UNREACHABLE_VOID
		}
	}
	if (compiler.build.emit_llvm)
	{
		OUTF("LLVM files written to %s.\n", compiler.build.ir_file_dir);
	}
	if (compiler.build.emit_asm)
	{
		OUTF("Asm files written to %s.\n", compiler.build.asm_file_dir);
	}
	free_arenas();

	uint32_t output_file_count = vec_size(gen_contexts);
	unsigned external_objfile_count = vec_size(compiler.build.object_files);
	unsigned cfiles = vec_size(compiler.build.csources);
	unsigned cfiles_library = 0;
	FOREACH(LibraryTarget *, lib, compiler.build.ccompiling_libraries)
	{
		cfiles_library += vec_size(lib->csources);
	}
	unsigned total_output = output_file_count + cfiles + cfiles_library + external_objfile_count;

	if (total_output > MAX_OUTPUT_FILES)
	{
		error_exit("Too many output files.");
	}
	if (!total_output)
	{
		if (output_exe)
		{
			error_exit("No output files were generated. This may happen if the program is not "
					   "linked with anything and all the code is optimized away.");
		}
		else
		{
			error_exit("No output files were generated. This may happen if there were no exported functions "
					   "and all the other code was optimized away.");
		}
	}

	CompileData *compile_data = ccalloc(sizeof(CompileData), output_file_count);
	const char **obj_files = cmalloc(sizeof(char*) * total_output);

	if (cfiles)
	{
		int compiled = compile_cfiles(compiler.build.cc, compiler.build.csources,
		                              compiler.build.cflags, compiler.build.cinclude_dirs, &obj_files[output_file_count], "tmp_c_compile");
		ASSERT(cfiles == compiled);
		(void)compiled;
	}
	const char **obj_file_next = &obj_files[output_file_count + cfiles];
	FOREACH(LibraryTarget *, lib, compiler.build.ccompiling_libraries)
	{
		obj_file_next += compile_cfiles(lib->cc ? lib->cc : compiler.build.cc, lib->csources,
		                                lib->cflags, lib->cinclude_dirs, obj_file_next, lib->parent->provides);
	}
	for (unsigned i = 0; i < external_objfile_count; i++)
	{
		obj_file_next[0] = compiler.build.object_files[i];
		obj_file_next++;
	}

	Task **tasks = NULL;
	for (unsigned i = 0; i < output_file_count; i++)
	{
		compile_data[i] = (CompileData) { .context = gen_contexts[i] };
		compile_data[i].task = (Task) { task, &compile_data[i] };
		vec_add(tasks, &compile_data[i].task);
	}

#if USE_PTHREAD
	INFO_LOG("Will use %d thread(s).\n", compiler.build.build_threads);
#endif
	unsigned task_count = vec_size(tasks);
	if (task_count > 0)
	{
		Task *task_last = VECLAST(tasks);
		vec_pop(tasks);
		task_last->task(task_last->arg);
		task_count--;
		if (task_count)
		{
			taskqueue_run((int)(compiler.build.build_threads > task_count ? task_count : compiler.build.build_threads), tasks);
		}
	}
	if (compiler.build.print_output)
	{
		puts("# output-files-begin");
	}
	int index = 0;
	for (unsigned i = output_file_count; i > 0; i--)
	{
		const char *name = compile_data[i - 1].object_name;
		if (!name) output_file_count--;
		obj_files[index++] = name;
		if (compiler.build.print_output)
		{
			puts(name);
		}
	}
	if (compiler.build.print_output)
	{
		puts("# output-files-end");
	}

	output_file_count += cfiles + cfiles_library + external_objfile_count;
	unsigned objfile_delete_count = output_file_count - external_objfile_count;
	free(compile_data);
	compiler_codegen_time = bench_mark();

	if ((output_static || output_dynamic || output_exe) && !output_file_count)
	{
		if (!compiler.build.object_files)
		{
			error_exit("Compilation could not complete due to --no-obj, please try removing it.");
		}
		error_exit("Compilation produced no object files, maybe there was no code?");
	}
	if (vec_size(compiler.build.emit_only)) goto SKIP;
	if (output_exe)
	{
		if (file_path_is_relative(output_exe))
		{
			output_exe = file_append_path(compiler.build.output_dir, output_exe);
		}
		;
		file_create_folders(output_exe);
		bool system_linker_available = link_libc() && compiler.platform.os != OS_TYPE_WIN32;
		bool use_system_linker = system_linker_available && compiler.build.arch_os_target == default_target;
		switch (compiler.build.linker_type)
		{
			case LINKER_TYPE_CC:
				if (!system_linker_available)
				{
					eprintf("System linker is not supported, defaulting to built-in linker\n");
					break;
				}
				use_system_linker = true;
				break;
			case LINKER_TYPE_BUILTIN:
				use_system_linker = false;
				break;
			default:
				break;
		}
		if (use_system_linker || compiler.build.linker_type == LINKER_TYPE_CC)
		{
			platform_linker(output_exe, obj_files, output_file_count);
			compiler_link_time = bench_mark();
			compiler_print_bench();
			delete_object_files(obj_files, objfile_delete_count);
		}
		else
		{
			compiler_print_bench();
			if (!obj_format_linking_supported(compiler.platform.object_format) || !linker(output_exe, obj_files,
																						output_file_count))
			{
				eprintf("No linking is performed due to missing linker support.\n");
				compiler.build.run_after_compile = false;
			}
			else
			{
				delete_object_files(obj_files, objfile_delete_count);
			}
		}

		if (compiler.build.run_after_compile)
		{
			DEBUG_LOG("Will run");
			const char *name = output_exe;
			while (name[0] == '.' && name[1] == '/') name += 2;
			scratch_buffer_clear();
			if (compiler.platform.os == OS_TYPE_WIN32)
			{
				size_t len = strlen(name);
				for (unsigned i = 0; i < len; i++)
				{
					if (name[i] == '/')
					{
						if (name[i + 1] == '.' && name[i + 2] == '/')
						{
							i++;
							continue;
						}
						scratch_buffer_append_char('\\');
						continue;
					}
					scratch_buffer_append_char(name[i]);
				}
			}
			else
			{
				if (name[0] != '/') scratch_buffer_append("./");
				scratch_buffer_append(name);
			}
			name = scratch_buffer_to_string();
			const char *full_path = realpath(scratch_buffer_to_string(), NULL);
			if (!full_path)
			{
				error_exit("The binary '%s' was unexpectedly not found.", scratch_buffer_to_string());
			}
			OUTF("Launching %s", name);
			FOREACH(const char *, arg, compiler.build.args)
			{
				OUTF(" %s", arg);
			}
			if (compiler.build.run_dir)
			{
				OUTF(" from directory %s", compiler.build.run_dir);
				dir_change(compiler.build.run_dir);
			}
			OUTN("");
			int ret = run_subprocess(full_path, compiler.build.args);
			if (compiler.build.delete_after_run)
			{
				file_delete_file(full_path);
			}
			if (ret < 0) exit_compiler(EXIT_FAILURE);
			OUTF("Program completed with exit code %d.\n", ret);
			if (ret != 0) exit_compiler(ret);
		}
	}
	else if (output_static)
	{
		if (file_path_is_relative(output_static))
		{
			output_static = file_append_path(compiler.build.output_dir, output_static);
		}
		file_create_folders(output_static);
		if (!static_lib_linker(output_static, obj_files, output_file_count))
		{
			error_exit("Failed to produce static library '%s'.", output_static);
		}
		delete_object_files(obj_files, objfile_delete_count);
		compiler_link_time = bench_mark();
		compiler_print_bench();
		OUTF("Static library '%s' created.\n", output_static);
	}
	else if (output_dynamic)
	{
		if (file_path_is_relative(output_dynamic))
		{
			output_dynamic = file_append_path(compiler.build.output_dir, output_dynamic);
		}
		file_create_folders(output_dynamic);
		if (!dynamic_lib_linker(output_dynamic, obj_files, output_file_count))
		{
			error_exit("Failed to produce dynamic library '%s'.", output_dynamic);
		}
		delete_object_files(obj_files, objfile_delete_count);
		OUTF("Dynamic library '%s' created.\n", output_dynamic);
		compiler_link_time = bench_mark();
		compiler_print_bench();
	}
	else
	{
		SKIP:
		compiler_print_bench();
	}
	free(obj_files);
}
INLINE void expand_csources(const char *base_dir, const char **source_dirs, const char ***sources_ref)
{
	if (source_dirs)
	{
		static const char* c_suffix_list[3] = { ".c", ".m" };
		*sources_ref = target_expand_source_names(base_dir, source_dirs, c_suffix_list, NULL, 2, false);
	}
}

INLINE void expand_cinclude_dirs(const char *base_dir, const char **include_dirs, const char ***include_dirs_ref)
{
	const char **expanded_include_dirs = NULL;
	FOREACH(const char *, include_dir, include_dirs)
	{
		vec_add(expanded_include_dirs, file_append_path(base_dir, include_dir));
	}
	*include_dirs_ref = expanded_include_dirs;
}

void compile_target(BuildOptions *options)
{
	init_default_build_target(&compiler.build, options);
	compile();
}

void clean_obj_files(void)
{
	file_delete_all_files_in_dir_with_suffix(compiler.build.ir_file_dir, ".ll");
	file_delete_all_files_in_dir_with_suffix(compiler.build.asm_file_dir, ".s");
	file_delete_all_files_in_dir_with_suffix(compiler.build.object_file_dir, ".obj");
	file_delete_all_files_in_dir_with_suffix(compiler.build.object_file_dir, ".o");
}
void compile_clean(BuildOptions *options)
{
	init_build_target(&compiler.build, options);
	clean_obj_files();
}
void compile_file_list(BuildOptions *options)
{
	init_build_target(&compiler.build, options);
	if (compiler.build.type == TARGET_TYPE_PREPARE)
	{
		if (options->command != COMMAND_BUILD)
		{
			error_exit("The target is a 'prepare' target, and only 'build' can be used with it.");
		}
		OUTF("Running prepare target '%s'.\n", options->target_select);
		execute_scripts();
		OUTN("Completed.\n.");
		return;
	}
	if (options->command == COMMAND_CLEAN_RUN)
	{
		clean_obj_files();
	}
	compile();
}

static inline void setup_define(const char *id, Expr *expr)
{
	TokenType token_type = TOKEN_CONST_IDENT;
	id = symtab_add(id, (uint32_t) strlen(id), fnv1a(id, (uint32_t) strlen(id)), &token_type);
	void *previous = htable_set(&compiler.context.compiler_defines, (void*)id, expr);
	if (previous)
	{
		error_exit("Redefined ident %s", id);
	}
}
static void setup_int_define(const char *id, uint64_t i, Type *type)
{
	Type *flat = type_flatten(type);
	ASSERT(type_is_integer(flat));
	Expr *expr = expr_new_const_int(INVALID_SPAN, flat, i);
	expr->type = type;
	if (expr_const_will_overflow(&expr->const_expr, flat->type_kind))
	{
		error_exit("Integer define %s overflow.", id);
	}
	setup_define(id, expr);
}

static void setup_string_define(const char *id, const char *value)
{
	setup_define(id, expr_new_const_string(INVALID_SPAN, value));
}

static void setup_bool_define(const char *id, bool value)
{
	setup_define(id, expr_new_const_bool(INVALID_SPAN, type_bool, value));
}

bool use_ansi(void)
{
	switch (compiler.context.ansi)
	{
		case ANSI_DETECT:
			break;
		case ANSI_OFF:
			return false;
		case ANSI_ON:
			return true;
	}
#if PLATFORM_WINDOWS
	return false;
#else
	return isatty(fileno(stdout));
#endif
}

#if FETCH_AVAILABLE
const char * vendor_fetch_single(const char* lib, const char* path) 
{
	const char *resource = str_printf("/c3lang/vendor/releases/download/latest/%s.c3l", lib);
	const char *destination = file_append_path(path, str_printf("%s.c3l", lib));
	const char *error = download_file("https://github.com", resource, destination);
	return error;	
}

#define PROGRESS_BAR_LENGTH 35

void update_progress_bar(const char* lib, int current_step, int total_steps)
{
    float progress = (float)current_step / (float)total_steps;
    int filled_length = (int)(progress * PROGRESS_BAR_LENGTH);
	printf("\033[2K%-10s ", lib);
    printf("[");
    for (int i = 0; i < PROGRESS_BAR_LENGTH; i++)
    {
	    printf(i < filled_length ? "=" : " ");
    }
	printf("] %d%%\r", (int)(progress * 100));
	(void)fflush(stdout);
}

void vendor_fetch(BuildOptions *options)
{
	bool ansi = use_ansi();

	if (str_eq(options->path, DEFAULT_PATH))
	{
		// check if there is a project JSON file
		if (file_exists(PROJECT_JSON5) || file_exists(PROJECT_JSON))
		{
			const char** deps_dirs =  get_project_dependency_directories();
			int num_lib = (int)vec_size(deps_dirs);
			if (num_lib > 0) options->vendor_download_path = deps_dirs[0];
		}
	}

	unsigned count = 0;
	const char** fetched_libraries = NULL;
	int total_libraries = (int)vec_size(options->libraries_to_fetch);
	
	for(int i = 0; i < total_libraries; i++)
	{
		const char *lib = options->libraries_to_fetch[i];
		if (!ansi || total_libraries == 1)
		{
			printf("Fetching library '%s'...", lib);
			(void)fflush(stdout);
		}
		else
		{
			update_progress_bar(lib, i, total_libraries);
		}
		const char *error = vendor_fetch_single(lib, options->vendor_download_path);

		if (!error)
		{
			if (!ansi || total_libraries == 1)
			{
				puts("finished.");
			}
			else
			{
				update_progress_bar(lib, i + 1, total_libraries);
			}
			vec_add(fetched_libraries, lib);
			count++;
		}
		else
		{
			if (ansi)
			{
				printf("\033[2K\033[31mFailed to fetch library '%s': %s\033[0m\n", lib, error);
			}
			else
			{
				printf("Failed: '%s'\n", error);
			}
			(void)fflush(stdout);
		}
	}

	if (ansi && total_libraries > 1) printf("\033[2K");

	// add fetched library to the dependency list
	add_libraries_to_project_file(fetched_libraries, options->project_options.target_name);

	if (count == 0)	error_exit("Error: Failed to download any libraries.");
	if (count < vec_size(options->libraries_to_fetch)) error_exit("Error: Only some libraries were downloaded.");

	if (ansi) printf("\033[32mFetching complete.\033[0m\t\t\n");
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
		for (int i = 1; i < TOKEN_LAST; i++)
		{
			const char *name = token_type_to_string((TokenType)i);
			if (name[0] == '$' || (name[0] >= 'a' && name[0] <= 'z'))
			{
				if (name[1] == '$' || name[1] == '\0') continue;
				printf("%s\n", name);
			}
		}
	}
	if (options->print_operators)
	{
		for (int i = 1; i < TOKEN_LAST; i++)
		{
			if (i == TOKEN_DOCS_START || i == TOKEN_DOCS_END) continue;
			const char *name = token_type_to_string((TokenType)i);
			char first_char = name[0];
			if (first_char == '$' || first_char == '@' || first_char == '_' || first_char == '#'
				|| (first_char >= 'a' && first_char <= 'z')
				|| (first_char >= 'A' && first_char <= 'Z'))
			{
				continue;
			}
			printf("%s\n", name);
		}
	}
	if (options->print_attributes)
	{
		for (int i = 0; i < NUMBER_OF_ATTRIBUTES; i++)
		{
			printf("%s\n", attribute_list[i]);
		}
	}
	if (options->print_builtins)
	{
		for (int i = 0; i < NUMBER_OF_BUILTINS; i++)
		{
			printf("$$%s\n", builtin_list[i]);
		}
		for (int i = 0; i < NUMBER_OF_BUILTIN_DEFINES; i++)
		{
			printf("$$%s\n", builtin_defines[i]);
		}
	}
	if (options->print_type_properties)
	{
		for (int i = 0; i < NUMBER_OF_TYPE_PROPERTIES; i++)
		{
			printf("%s\n", type_property_list[i]);
		}
	}
	if (options->print_project_properties)
	{
		puts("Project properties");
		puts("------------------");
		for (int i = 0; i < project_default_keys_count; i++)
		{
			printf("%-*s%s\n", 35, project_default_keys[i][0], project_default_keys[i][1]);
		}
		puts("");
		puts("Target properties");
		puts("-----------------");
		for (int i = 0; i < project_target_keys_count; i++)
		{
			printf("%-*s%s\n", 35, project_target_keys[i][0], project_target_keys[i][1]);
		}
		puts("");
	}
	if (options->print_manifest_properties)
	{
		puts("Manifest properties");
		puts("------------------");
		for (int i = 0; i < manifest_default_keys_count; i++)
		{
			printf("%-*s%s\n", 35, manifest_default_keys[i][0], manifest_default_keys[i][1]);
		}
		puts("");
		puts("Target properties");
		puts("-----------------");
		for (int i = 0; i < manifest_target_keys_count; i++)
		{
			printf("%-*s%s\n", 35, manifest_target_keys[i][0], manifest_target_keys[i][1]);
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
		puts(" 7. Additive   | + - +++");
		puts(" 8. Relational | < > <= >= == !=");
		puts(" 9. And        | && &&&");
		puts("10. Or         | || |||");
		puts("11. Ternary    | ?: ?? ???");
		puts("12. Assign     | = *= /= %= -= += |= &= ^= <<= >>= +++=");
	}

}

static int jump_buffer_size()
{
	switch (compiler.build.arch_os_target)
	{
		case ARCH_OS_TARGET_DEFAULT:
			return 512;
		case ELF_RISCV32:
		case LINUX_RISCV32:
			// Godbolt test
			return 76;
		case ELF_XTENSA:
			// Godbolt 68 => 17 with 32 bit pointers
			return 17;
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
		case ANDROID_X86_64:
			// Godbolt test
			return 25;
		case FREEBSD_X64:
		case NETBSD_X64:
		case OPENBSD_X64:
			REMINDER("Guessing setjmp for platform.");
			return 32;
		case ANDROID_AARCH64:
		case LINUX_AARCH64:
		case ELF_AARCH64:
			return 39;
		case WINDOWS_AARCH64:
			// Based on Godbolt
			return 24;
		case IOS_AARCH64:
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

void execute_scripts(void)
{
	if (!vec_size(compiler.build.exec)) return;
	if (compiler.build.trust_level < TRUST_FULL)
	{
		error_exit("This target has 'exec' directives, to run it trust level must be set to '--trust=full'.");
	}
	char old_path[PATH_MAX + 1];
	if (compiler.build.script_dir)
	{
		if (getcwd(old_path, PATH_MAX) && !dir_change(compiler.build.script_dir))
		{
			error_exit("Failed to open script dir '%s'", compiler.build.script_dir);
		}
	}
	double start = bench_mark();
	FOREACH(const char *, exec, compiler.build.exec)
	{
		StringSlice execs = slice_from_string(exec);
		StringSlice call = slice_next_token(&execs, ' ');
		File *script;
		if (call.len < 3 || call.ptr[call.len - 3] != '.' || call.ptr[call.len - 2] != 'c' ||
		    call.ptr[call.len - 1] != '3')
		{
			char *res = execute_cmd(exec, false, NULL, 0);
			if (compiler.build.silent) continue;
			script = source_file_text_load(exec, res);
			goto PRINT_SCRIPT;
		}
		scratch_buffer_clear();
		scratch_buffer_append_len(call.ptr, call.len);
		script = compile_and_invoke(scratch_buffer_copy(), execs.len ? execs.ptr : "", NULL, 2048);
PRINT_SCRIPT:;
		size_t out_len = script->content_len;
		const char *out = script->contents;
		if (!compiler.build.silent && script->content_len > 0)
		{
			printf("%.*s\n", (int)out_len, out);
		}
	}
	dir_change(old_path);
	compiler.script_time += bench_mark() - start;
}

static void check_address_sanitizer_options(BuildTarget *target)
{
	if (target->feature.sanitize_memory || target->feature.sanitize_thread)
	{
		error_exit("Address sanitizer cannot be used together with memory or thread sanitizer.");
	}
	switch (target->arch_os_target)
	{
		case WINDOWS_X64:
		{
			WinCrtLinking crt_linking = target->win.crt_linking;
			if (crt_linking == WIN_CRT_DEFAULT)
			{
				error_exit("Please specify `static` or `dynamic` for `wincrt` when using address sanitizer.");
			}

			if (crt_linking == WIN_CRT_STATIC_DEBUG || crt_linking == WIN_CRT_DYNAMIC_DEBUG)
			{
				// We currently don't have ASan runtime libraries linked against debug CRT.
				error_exit("Address sanitizer cannot be used when using `static-debug` or `dynamic-debug` for `wincrt`. Please use `static` or `dynamic` instead.");
			}

			WARNING("Using address sanitizer on Windows requires the sanitizer option `detect_odr_violation=0`, either set by returning it from `__asan_default_options`, or via an environment variable `ASAN_OPTIONS=detect_odr_violation=0`");
			break;
		}
		case LINUX_X86:
		case LINUX_X64:
		case MACOS_AARCH64:
		case MACOS_X64:
		case FREEBSD_X86:
		case FREEBSD_X64:
		case NETBSD_X86:
		case NETBSD_X64:
			break;
		default:
			error_exit("Address sanitizer is only supported on Linux, FreeBSD, NetBSD, Darwin and Windows.");
	}
}

static void check_memory_sanitizer_options(BuildTarget *target)
{
	if (target->feature.sanitize_thread)
	{
		error_exit("Memory sanitizer cannot be used together thread sanitizer.");
	}
	switch (target->arch_os_target)
	{
		case LINUX_AARCH64:
		case LINUX_X86:
		case LINUX_X64:
		case FREEBSD_X86:
		case FREEBSD_X64:
		case NETBSD_X86:
		case NETBSD_X64:
			break;
		default:
			error_exit("Memory sanitizer is only supported on Linux, FreeBSD and NetBSD.");
	}
	if (target->reloc_model != RELOC_BIG_PIE)
	{
		error_exit("Memory sanitizer requires `PIE` relocation model.");
	}
}

static void check_thread_sanitizer_options(BuildTarget *target)
{
	switch (target->arch_os_target)
	{
		case LINUX_AARCH64:
		case LINUX_X64:
		case MACOS_AARCH64:
		case MACOS_X64:
		case FREEBSD_X64:
		case NETBSD_X64:
			break;
		default:
			error_exit("Thread sanitizer is only supported on 64-bit Linux, NetBSD, FreeBSD and Darwin.");
	}
}
static void check_sanitizer_options(BuildTarget *target)
{
	if (target->feature.sanitize_address) check_address_sanitizer_options(target);
	if (target->feature.sanitize_memory) check_memory_sanitizer_options(target);
	if (target->feature.sanitize_thread) check_thread_sanitizer_options(target);

	if (target->type == TARGET_TYPE_BENCHMARK)
	{
		if (target->feature.sanitize_address)
		{
			WARNING("Running benchmarks with address sanitizer enabled!");
		}
		else if (target->feature.sanitize_thread)
		{
			WARNING("Running benchmarks with thread sanitizer enabled!");
		}
		else if (target->feature.sanitize_memory)
		{
			WARNING("Running benchmarks with memory sanitizer enabled!");
		}
	}

}

const char *compiler_date_to_iso(void)
{
	const char *comp_date = __DATE__;
	static char iso[11] = "2000-01-01";
	iso[2] = comp_date[9];
	iso[3] = comp_date[10];
	int month;
	switch (comp_date[0])
	{
		case 'O':
			month = 10;
			break;
		case 'D':
			month = 12;
			break;
		case 'N':
			month = 11;
			break;
		case 'J':
			if (comp_date[1] == 'a')
			{
				month = 1;
				break;
			}
			if (comp_date[2] == 'n')
			{
				month = 6;
				break;
			}
			month = 7;
			break;
		case 'F':
			month = 2;
			break;
		case 'A':
			month = comp_date[2] == 'p' ? 4 : 8;
			break;
		case 'M':
			month = comp_date[2] == 'r' ? 3 : 5;
			break;
		case 'S':
			month = 9;
			break;
		default:
			UNREACHABLE
	}
	iso[5] = (char)(month / 10 + '0');
	iso[6] = (char)(month % 10 + '0');
	iso[8] = (char)(comp_date[4] == ' ' ? '0' : comp_date[4]);
	iso[9] = comp_date[5];
	return iso;
}

void compile()
{
	symtab_init(compiler.build.symtab_size);
	compiler.build.sources = target_expand_source_names(NULL, compiler.build.source_dirs, c3_suffix_list, &compiler.build.object_files, 3, true);
	if (compiler.build.testing && compiler.build.test_source_dirs)
	{
		const char **test_sources = target_expand_source_names(NULL, compiler.build.test_source_dirs, c3_suffix_list, &compiler.build.object_files, 3, true);
		FOREACH(const char *, file, test_sources) vec_add(compiler.build.sources, file);
	}
	expand_csources(NULL, compiler.build.csource_dirs, &compiler.build.csources);
	execute_scripts();
	compiler.context.main = NULL;
	compiler.context.string_type = NULL;
	compiler.platform.asm_initialized = false;
	// Create the core module if needed.
	Path *core_path = MALLOCS(Path);
	core_path->module = kw_std__core;
	core_path->span = INVALID_SPAN;
	core_path->len = strlen(kw_std__core);
	compiler.context.core_module = compiler_find_or_create_module(core_path, NULL);
	CompilationUnit *unit = CALLOCS(CompilationUnit);
	unit->file = source_file_generate("core_internal.c3");
	unit->module = compiler.context.core_module;
	compiler.context.core_unit = unit;
	target_setup(&compiler.build);
	if (compiler.context.should_print_environment)
	{
		print_build_env();
		exit_compiler(COMPILER_SUCCESS_EXIT);
	}
	if (compiler.context.should_print_asm)
	{
		print_asm(&compiler.platform);
		exit_compiler(COMPILER_SUCCESS_EXIT);
	}
	check_sanitizer_options(&compiler.build);
	resolve_libraries(&compiler.build);
	compiler.context.sources = compiler.build.sources;
	FOREACH(LibraryTarget *, lib, compiler.build.ccompiling_libraries)
	{
		expand_csources(lib->parent->dir, lib->csource_dirs, &lib->csources);
		expand_cinclude_dirs(lib->parent->dir, lib->cinclude_dirs, &lib->cinclude_dirs);
	}
	FOREACH(const char *, feature_flag, compiler.build.feature_list)
	{
		feature_flag = symtab_preset(feature_flag, TOKEN_CONST_IDENT);
		htable_set(&compiler.context.features, (void *) feature_flag, (void *) feature_flag);
	}

	setup_int_define("C_SHORT_SIZE", compiler.platform.width_c_short, type_int);
	setup_int_define("C_INT_SIZE", compiler.platform.width_c_int, type_int);
	setup_int_define("C_LONG_SIZE", compiler.platform.width_c_long, type_int);
	setup_int_define("C_LONG_LONG_SIZE", compiler.platform.width_c_long_long, type_int);
	setup_int_define("REGISTER_SIZE", compiler.platform.width_register, type_int);
	setup_int_define("MAX_VECTOR_SIZE", compiler.build.max_vector_size, type_int);
	setup_bool_define("C_CHAR_IS_SIGNED", compiler.platform.signed_c_char);
	setup_bool_define("PLATFORM_BIG_ENDIAN", compiler.platform.big_endian);
	setup_bool_define("PLATFORM_I128_SUPPORTED", compiler.platform.int128);
	setup_bool_define("PLATFORM_F128_SUPPORTED", compiler.platform.float128);
	setup_bool_define("PLATFORM_F16_SUPPORTED", compiler.platform.float16);
	setup_int_define("ARCH_TYPE", (uint64_t)compiler.platform.arch, type_int);
	setup_int_define("MEMORY_ENVIRONMENT", (uint64_t)compiler.build.memory_environment, type_int);
	setup_bool_define("COMPILER_LIBC_AVAILABLE", link_libc());
	setup_int_define("COMPILER_OPT_LEVEL", (uint64_t)compiler.build.optlevel, type_int);
	setup_int_define("OS_TYPE", (uint64_t)compiler.platform.os, type_int);
	setup_int_define("COMPILER_SIZE_OPT_LEVEL", (uint64_t)compiler.build.optsize, type_int);
	setup_bool_define("COMPILER_SAFE_MODE", safe_mode_enabled());
	setup_bool_define("DEBUG_SYMBOLS", compiler.build.debug_info == DEBUG_INFO_FULL);
	setup_bool_define("PANIC_MSG", compiler.build.feature.panic_level != PANIC_OFF);
	setup_bool_define("BACKTRACE", compiler.build.show_backtrace != SHOW_BACKTRACE_OFF);
#if LLVM_AVAILABLE
    setup_int_define("LLVM_VERSION", llvm_version_major, type_int);
#else 
    setup_int_define("LLVM_VERSION", 0, type_int);
#endif

    setup_bool_define("BENCHMARKING", compiler.build.benchmarking);
	setup_int_define("JMP_BUF_SIZE", jump_buffer_size(), type_int);
	setup_bool_define("TESTING", compiler.build.testing);
	setup_int_define("LANGUAGE_DEV_VERSION", 7, type_int);
	setup_bool_define("ADDRESS_SANITIZER", compiler.build.feature.sanitize_address);
	setup_bool_define("MEMORY_SANITIZER", compiler.build.feature.sanitize_memory);
	setup_bool_define("THREAD_SANITIZER", compiler.build.feature.sanitize_thread);
	setup_string_define("BUILD_HASH", GIT_HASH);
	setup_string_define("BUILD_DATE", compiler_date_to_iso());
	Expr *expr_names = expr_new(EXPR_CONST, INVALID_SPAN);
	Expr *expr_emails = expr_new(EXPR_CONST, INVALID_SPAN);
	expr_names->const_expr.const_kind = CONST_UNTYPED_LIST;
	expr_emails->const_expr.const_kind = CONST_UNTYPED_LIST;
	expr_names->type = type_untypedlist;
	expr_emails->type = type_untypedlist;
	expr_names->resolve_status = expr_emails->resolve_status = RESOLVE_DONE;
	FOREACH(AuthorEntry, entry, compiler.build.authors)
	{
		Expr *const_name = expr_new_const_string(INVALID_SPAN, entry.author); // NOLINT
		Expr *const_email = expr_new_const_string(INVALID_SPAN, entry.email ? entry.email : ""); // NOLINT
		vec_add(expr_names->const_expr.untyped_list, const_name);
		vec_add(expr_emails->const_expr.untyped_list, const_email);
	}
	setup_define("AUTHORS", expr_names);
	setup_define("AUTHOR_EMAILS", expr_emails);
	setup_string_define("PROJECT_VERSION", compiler.build.version);
	type_init_cint();
	compiler_init_time = bench_mark();

	if (!vec_size((compiler.build.object_files)) && !vec_size(compiler.build.sources) && !compiler.build.read_stdin) error_exit("No files to compile.");

	if (compiler.build.lex_only)
	{
		compiler_lex();
		compiler_parsing_time = bench_mark();
		return;
	}
	if (compiler.build.parse_only)
	{
		compiler_parse();
		compiler_parsing_time = bench_mark();
		emit_json();
		exit_compiler(COMPILER_SUCCESS_EXIT);
	}
	compiler_compile();
}

void global_context_add_decl(Decl *decl)
{
	decltable_set(&compiler.context.symbols, decl);
	pathtable_set(&compiler.context.path_symbols, decl);
}

void linking_add_link(Linking *linking, const char *link)
{
	FOREACH(const char *, existing_link, linking->links)
	{
		if (str_eq(link, existing_link)) return;
	}
	vec_add(linking->links, link);
}

void global_context_clear_errors(void)
{
	compiler.context.in_panic_mode = false;
	compiler.context.errors_found = 0;
	compiler.context.warnings_found = 0;
}

void global_context_add_type(Type *type)
{
	DEBUG_LOG("Created type %s.", type->name);
	ASSERT(type_ok(type));
	vec_add(compiler.context.type, type);
}

const char *get_object_extension(void)
{
	switch (compiler.build.arch_os_target)
	{
		case ANY_WINDOWS_ARCH_OS:
			return ".obj";
		case WASM32:
		case WASM64:
			return ".wasm";
		default:
			return ".o";
	}
}

Module *global_context_find_module(const char *name)
{
	ASSERT(name);
	return htable_get(&compiler.context.modules, (void *)name);
}

Module *compiler_find_or_create_module(Path *module_name, const char **parameters)
{
	Module *module = global_context_find_module(module_name->module);
	if (module) return module;

	DEBUG_LOG("Creating module %s.", module_name->module);
	// Set up the module.
	module = CALLOCS(Module);
	module->name = module_name;
	module->inlined_at = (InliningSpan) { INVALID_SPAN, NULL };
	size_t first = 0;
	for (size_t i = module_name->len; i > 0; i--)
	{
		if (module_name->module[i - 1] == ':')
		{
			first = i;
			break;
		}
	}
	if (!first)
	{
		module->short_path = module_name->module;
	}
	else
	{
		const char *name = &module_name->module[first];
		size_t len = module_name->len - first;
		TokenType type = TOKEN_IDENT;
		module->short_path = symtab_add(name, len, fnv1a(name, len), &type);
	}
	module->stage = ANALYSIS_NOT_BEGUN;
	module->parameters = parameters;
	module->is_generic = vec_size(parameters) > 0;
	htable_init(&module->symbols, 0x1000);
	htable_set(&compiler.context.modules, (void *)module_name->module, module);
	if (parameters)
	{
		vec_add(compiler.context.generic_module_list, module);
	}
	else
	{
		vec_add(compiler.context.module_list, module);
	}

	return module;
}


const char *scratch_buffer_interned(void)
{
	TokenType type = TOKEN_INVALID_TOKEN;
	return scratch_buffer_interned_as(&type);
}

const char *scratch_buffer_interned_as(TokenType* type)
{
	return symtab_add(scratch_buffer.str, scratch_buffer.len,
	                  fnv1a(scratch_buffer.str, scratch_buffer.len), type);
}

void scratch_buffer_append_native_safe_path(const char *data, int len)
{
#if PLATFORM_WINDOWS
	scratch_buffer_append("\"");
	for (int i = 0; i < len; i++)
	{
		char c = data[i];
		switch (c)
		{
			case '/':
			case '\\':
				scratch_buffer_append("\\");
				break;
			default:
				scratch_buffer_append_char(c);
				break;
		}
	}
	scratch_buffer_append("\"");
#else
	scratch_buffer_append_len(data, len);
#endif
}

File *compile_and_invoke(const char *file, const char *args, const char *stdin_data, size_t limit)
{
	char *name;
	if (!file_namesplit(compiler_exe_name, &name, NULL))
	{
		error_exit("Failed to extract file name from '%s'", compiler_exe_name);
	}
	const char *compiler_path = file_append_path(find_executable_path(), name);

	scratch_buffer_clear();
#if PLATFORM_WINDOWS
	scratch_buffer_append_char('"');
#endif
	scratch_buffer_append_native_safe_path(compiler_path, (int)strlen(compiler_path));
	const char *output = "__c3exec__";
	scratch_buffer_append(" compile -g0 --single-module=yes");
	StringSlice slice = slice_from_string(file);
	while (slice.len > 0)
	{
		StringSlice file_name = slice_next_token(&slice, ';');
		if (!file_name.len) continue;
		scratch_buffer_append(" ");
		scratch_buffer_append_native_safe_path(file_name.ptr, (int)file_name.len);
	}
	scratch_buffer_printf(" -o %s", output);
	char *out;
#if PLATFORM_WINDOWS
	scratch_buffer_append_char('"');
#endif
	if (!execute_cmd_failable(scratch_buffer_to_string(), &out, NULL, limit))
	{
		if (strlen(out))
		{
			eprintf("+-- Script compilation output ---------+\n");
			eprintf("%s\n", out);
			eprintf("+--------------------------------------+\n");
		}
		error_exit("Failed to compile script '%s'.", file);
	}
	DEBUG_LOG("EXEC OUT: %s", out);
	scratch_buffer_clear();
#if (!PLATFORM_WINDOWS)
	scratch_buffer_append("./");
#endif
	scratch_buffer_append(output);
	scratch_buffer_append(" ");
	scratch_buffer_append(args);
	if (!execute_cmd_failable(scratch_buffer_to_string(), &out, stdin_data, limit))
	{
		if (strlen(out))
		{
			eprintf("+-- Script output ---------------------+\n");
			eprintf("%s\n", out);
			eprintf("+--------------------------------------+\n");
		}
		error_exit("Error invoking script '%s' with arguments %s.", file, args);
	}
	file_delete_file(output);
	return source_file_text_load(file, out);
}

const char *default_c_compiler(void)
{
	static const char *cc = NULL;
	if (cc) return cc;
	const char *cc_env = getenv("C3C_CC");
	if (cc_env && strlen(cc_env) > 0)
	{
		INFO_LOG("Setting CC to %s from environment variable 'C3C_CC'.", cc_env);
		cc = strdup(cc_env);
		return cc;
	}
#if PLATFORM_WINDOWS
	WindowsSDK *sdk = windows_get_sdk();

	if (sdk && sdk->cl_path)
	{
		return cc = sdk->cl_path;
	}
	return cc = "cl.exe";
#else
	return cc = "cc";
#endif
}

static bool is_posix(OsType os)
{
	switch (os)
	{
		case OS_TYPE_IOS:
		case OS_TYPE_MACOSX:
		case OS_TYPE_WATCHOS:
		case OS_TYPE_TVOS:
		case OS_TYPE_NETBSD:
		case OS_TYPE_LINUX:
		case OS_TYPE_KFREEBSD:
		case OS_TYPE_FREEBSD:
		case OS_TYPE_OPENBSD:
		case OS_TYPE_SOLARIS:
			return true;
		case OS_TYPE_WIN32:
		case OS_TYPE_WASI:
		case OS_TYPE_EMSCRIPTEN:
			return false;
		default:
			return false;
	}
}
void print_build_env(void)
{
	char path[PATH_MAX + 1];
	printf("Version           : %s\n", COMPILER_VERSION);
	printf("Stdlib            : %s\n", compiler.context.lib_dir);
	printf("Exe name          : %s\n", compiler_exe_name);
	printf("Base path         : %s\n", getcwd(path, PATH_MAX));
	if (compiler.build.name && !compiler.build.is_non_project)
	{
		printf("Target name       : %s\n", compiler.build.name);
	}
	printf("Output name       : %s\n", compiler.build.output_name);
	printf("System path       : %s\n", getenv("PATH"));
	printf("Arch/OS target    : %s\n", arch_os_target[compiler.build.arch_os_target]);
	printf("env::POSIX        : %s\n", link_libc() && is_posix(compiler.platform.os) ? "true" : "false");
	printf("env::WIN32        : %s\n", compiler.platform.os == OS_TYPE_WIN32 ? "true" : "false");
	printf("env::LIBC         : %s\n", link_libc() ? "true" : "false");
}
