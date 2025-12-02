// Copyright (c) 2019-2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.
#include "build.h"
#include "build_internal.h"

void load_library_files(void) {}

#if defined(_M_X64) || defined(_M_AMD64)
ArchOsTarget default_target = WINDOWS_X64;
#elif defined(__amd64__) || defined(__amd64) || defined(__x86_64__) || defined(__x86_64)
	#if defined(__MACH__)
ArchOsTarget default_target = MACOS_X64;
	#elif defined(__ANDROID__)
ArchOsTarget default_target = ANDROID_X86_64;
	#elif defined(__linux__) && __linux__
ArchOsTarget default_target = LINUX_X64;
		#if (defined(__GLIBC__) && __GLIBC__) || (defined(__GLIBC_MINOR__) && __GLIBC_MINOR__)
#define LINUX_LIBC
LinuxLibc default_libc = LINUX_LIBC_GNU;
		#elif defined(__DEFINED_va_list)
#define LINUX_LIBC
LinuxLibc default_libc = LINUX_LIBC_MUSL;
		#endif
	#elif defined(__NetBSD__)
ArchOsTarget default_target = NETBSD_X64;
	#elif defined(__FreeBSD__)
ArchOsTarget default_target = FREEBSD_X64;
	#elif defined(__OpenBSD__)
ArchOsTarget default_target = OPENBSD_X64;
	#else
ArchOsTarget default_target = ELF_X64;
	#endif
#elif defined(__aarch64__) || defined(_M_ARM64)
	#if defined(__MACH__)
ArchOsTarget default_target = MACOS_AARCH64;
	#elif defined(__ANDROID__)
ArchOsTarget default_target = ANDROID_AARCH64;
	#elif defined(__linux__) && __linux__
ArchOsTarget default_target = LINUX_AARCH64;
		#if (defined(__GLIBC__) && __GLIBC__) || (defined(__GLIBC_MINOR__) && __GLIBC_MINOR__)
#define LINUX_LIBC
LinuxLibc default_libc = LINUX_LIBC_GNU;
		#elif defined(__DEFINED_va_list)
#define LINUX_LIBC
LinuxLibc default_libc = LINUX_LIBC_MUSL;
		#endif
	#else
ArchOsTarget default_target = ELF_AARCH64;
	#endif
#elif defined(i386) || defined(__i386__) || defined(__i386) || defined(_M_IX86)
	#if defined(__linux__) && __linux__
ArchOsTarget default_target = LINUX_X86;
		#if (defined(__GLIBC__) && __GLIBC__) || (defined(__GLIBC_MINOR__) && __GLIBC_MINOR__)
#define LINUX_LIBC
LinuxLibc default_libc = LINUX_LIBC_GNU;
		#elif defined(__DEFINED_va_list)
#define LINUX_LIBC
LinuxLibc default_libc = LINUX_LIBC_MUSL;
		#endif
	#elif defined(__FreeBSD__)
ArchOsTarget default_target = FREEBSD_X86;
	#elif defined(__OpenBSD__)
ArchOsTarget default_target = OPENBSD_X86;
	#elif defined(__NetBSD__)
ArchOsTarget default_target = NETBSD_X86;
	#elif defined(_MSC_VER) && _MSC_VER
ArchOsTarget default_target = WINDOWS_X86;
	#else
ArchOsTarget default_target = ELF_X86;
	#endif
#elif defined(__riscv32)
	#if defined(__linux__) && __linux__
ArchOsTarget default_target = LINUX_RISCV32;
		#if (defined(__GLIBC__) && __GLIBC__) || (defined(__GLIBC_MINOR__) && __GLIBC_MINOR__)
#define LINUX_LIBC
LinuxLibc default_libc = LINUX_LIBC_GNU;
		#elif defined(__DEFINED_va_list)
#define LINUX_LIBC
LinuxLibc default_libc = LINUX_LIBC_MUSL;
		#endif
	#else
ArchOsTarget default_target = ELF_RISCV32;
	#endif
#elif defined(__riscv64)
	#if defined(__linux__) && __linux__
ArchOsTarget default_target = LINUX_RISCV64;
		#if (defined(__GLIBC__) && __GLIBC__) || (defined(__GLIBC_MINOR__) && __GLIBC_MINOR__)
#define LINUX_LIBC
LinuxLibc default_libc = LINUX_LIBC_GNU;
		#elif defined(__DEFINED_va_list)
#define LINUX_LIBC
LinuxLibc default_libc = LINUX_LIBC_MUSL;
		#endif
	#else
ArchOsTarget default_target = ELF_RISCV64;
	#endif
#else
ArchOsTarget default_target = ARCH_OS_TARGET_DEFAULT;
#endif

#ifndef LINUX_LIBC
LinuxLibc default_libc = LINUX_LIBC_GNU;
#endif

bool command_accepts_files(CompilerCommand command)
{
	switch (command)
	{
		case COMMAND_COMPILE:
		case COMMAND_COMPILE_ONLY:
		case COMMAND_COMPILE_RUN:
		case COMMAND_DYNAMIC_LIB:
		case COMMAND_STATIC_LIB:
		case COMMAND_COMPILE_BENCHMARK:
		case COMMAND_COMPILE_TEST:
		case COMMAND_UNIT_TEST:
			return true;
		case COMMAND_MISSING:
		case COMMAND_INIT:
		case COMMAND_INIT_LIB:
		case COMMAND_BUILD:
		case COMMAND_RUN:
		case COMMAND_CLEAN_RUN:
		case COMMAND_CLEAN:
		case COMMAND_DIST:
		case COMMAND_BENCH:
		case COMMAND_PRINT_SYNTAX:
		case COMMAND_BENCHMARK:
		case COMMAND_TEST:
		case COMMAND_VENDOR_FETCH:
		case COMMAND_PROJECT:
			return false;
	}
	UNREACHABLE
}

bool command_passes_args(CompilerCommand command)
{
	switch (command)
	{
		case COMMAND_CLEAN_RUN:
		case COMMAND_COMPILE_RUN:
		case COMMAND_RUN:
		case COMMAND_BENCHMARK:
		case COMMAND_TEST:
			return true;
		case COMMAND_COMPILE:
		case COMMAND_COMPILE_ONLY:
		case COMMAND_DYNAMIC_LIB:
		case COMMAND_STATIC_LIB:
		case COMMAND_COMPILE_BENCHMARK:
		case COMMAND_COMPILE_TEST:
		case COMMAND_UNIT_TEST:
		case COMMAND_MISSING:
		case COMMAND_INIT:
		case COMMAND_INIT_LIB:
		case COMMAND_BUILD:
		case COMMAND_CLEAN:
		case COMMAND_DIST:
		case COMMAND_BENCH:
		case COMMAND_PRINT_SYNTAX:
		case COMMAND_VENDOR_FETCH:
		case COMMAND_PROJECT:
			return false;
	}
	UNREACHABLE
}

void update_build_target_with_opt_level(BuildTarget *target, OptimizationSetting level)
{
	OptimizationLevel optlevel = OPTIMIZATION_NONE;
	SizeOptimizationLevel optsize = SIZE_OPTIMIZATION_NONE;
	DebugInfo debug = DEBUG_INFO_FULL;
	SafetyLevel safety_level = SAFETY_ON;
	PanicLevel panic_level = PANIC_ON;
	UnrollLoops unroll_loops = UNROLL_LOOPS_OFF;
	AutoVectorization vectorization = VECTORIZATION_OFF;
	AutoVectorization slp_vectorization = VECTORIZATION_OFF;
	MergeFunctions merge_functions = MERGE_FUNCTIONS_OFF;
	ShowBacktrace show_backtrace = SHOW_BACKTRACE_ON;
	SingleModule single_module = SINGLE_MODULE_OFF;
	FpOpt fp_opt = FP_STRICT;
	switch (level)
	{
		case OPT_SETTING_O0:
			break;
		case OPT_SETTING_O1:
			optlevel = OPTIMIZATION_MORE;
			slp_vectorization = VECTORIZATION_ON;
			unroll_loops = UNROLL_LOOPS_ON;
			vectorization = VECTORIZATION_ON;
			break;
		case OPT_SETTING_O2:
			merge_functions = MERGE_FUNCTIONS_ON;
			optlevel = OPTIMIZATION_MORE;
			safety_level = SAFETY_OFF;
			slp_vectorization = VECTORIZATION_ON;
			unroll_loops = UNROLL_LOOPS_ON;
			vectorization = VECTORIZATION_ON;
			break;
		case OPT_SETTING_O3:
			merge_functions = MERGE_FUNCTIONS_ON;
			optlevel = OPTIMIZATION_MORE;
			safety_level = SAFETY_OFF;
			single_module = SINGLE_MODULE_ON;
			slp_vectorization = VECTORIZATION_ON;
			unroll_loops = UNROLL_LOOPS_ON;
			vectorization = VECTORIZATION_ON;
			break;
		case OPT_SETTING_O4:
			fp_opt = FP_RELAXED;
			merge_functions = MERGE_FUNCTIONS_ON;
			optlevel = OPTIMIZATION_AGGRESSIVE;
			panic_level = PANIC_OFF;
			safety_level = SAFETY_OFF;
			single_module = SINGLE_MODULE_ON;
			slp_vectorization = VECTORIZATION_ON;
			unroll_loops = UNROLL_LOOPS_ON;
			vectorization = VECTORIZATION_ON;
			break;
		case OPT_SETTING_O5:
			fp_opt = FP_FAST;
			merge_functions = MERGE_FUNCTIONS_ON;
			optlevel = OPTIMIZATION_AGGRESSIVE;
			panic_level = PANIC_OFF;
			safety_level = SAFETY_OFF;
			single_module = SINGLE_MODULE_ON;
			slp_vectorization = VECTORIZATION_ON;
			unroll_loops = UNROLL_LOOPS_ON;
			vectorization = VECTORIZATION_ON;
			break;
		case OPT_SETTING_OSMALL:
			merge_functions = MERGE_FUNCTIONS_ON;
			optlevel = OPTIMIZATION_MORE;
			optsize = SIZE_OPTIMIZATION_SMALL;
			panic_level = PANIC_OFF;
			safety_level = SAFETY_OFF;
			slp_vectorization = VECTORIZATION_ON;
			vectorization = VECTORIZATION_ON;
			break;
		case OPT_SETTING_OTINY:
			debug = DEBUG_INFO_NONE;
			merge_functions = MERGE_FUNCTIONS_ON;
			optlevel = OPTIMIZATION_MORE;
			optsize = SIZE_OPTIMIZATION_TINY;
			panic_level = PANIC_OFF;
			safety_level = SAFETY_OFF;
			show_backtrace = SHOW_BACKTRACE_OFF;
			single_module = SINGLE_MODULE_ON;
			slp_vectorization = VECTORIZATION_ON;
			vectorization = VECTORIZATION_OFF;
			break;
		case OPT_SETTING_NOT_SET:
		default:
			UNREACHABLE_VOID
	}
	COPY_IF_DEFAULT(target->optsize, optsize);
	COPY_IF_DEFAULT(target->optlevel, optlevel);
	COPY_IF_DEFAULT(target->show_backtrace, show_backtrace);
	COPY_IF_DEFAULT(target->feature.safe_mode, safety_level);
	COPY_IF_DEFAULT(target->feature.panic_level, panic_level);
	COPY_IF_DEFAULT(target->debug_info, debug);
	COPY_IF_DEFAULT(target->feature.fp_math, fp_opt);
	COPY_IF_DEFAULT(target->unroll_loops, unroll_loops);
	COPY_IF_DEFAULT(target->merge_functions, merge_functions);
	COPY_IF_DEFAULT(target->slp_vectorization, slp_vectorization);
	COPY_IF_DEFAULT(target->loop_vectorization, vectorization);
	COPY_IF_DEFAULT(target->single_module, single_module);
}

static LinkLibc libc_from_arch_os(ArchOsTarget target)
{
	switch (target)
	{
		case ANDROID_AARCH64:
		case ANDROID_X86_64:
		case FREEBSD_X86:
		case FREEBSD_X64:
		case IOS_AARCH64:
		case LINUX_AARCH64:
		case LINUX_RISCV32:
		case LINUX_RISCV64:
		case LINUX_X86:
		case LINUX_X64:
		case MACOS_AARCH64:
		case MACOS_X64:
		case MINGW_X64:
		case NETBSD_X86:
		case NETBSD_X64:
		case OPENBSD_X86:
		case OPENBSD_X64:
		case WINDOWS_AARCH64:
		case WINDOWS_X64:
		case ARCH_OS_TARGET_DEFAULT:
			return LINK_LIBC_ON;
		case WASM32:
		case WASM64:
		case MCU_X86:
		case ELF_AARCH64:
		case ELF_RISCV32:
		case ELF_RISCV64:
		case ELF_X86:
		case ELF_X64:
		case ELF_XTENSA:
			return LINK_LIBC_OFF;
	}
	UNREACHABLE
}

#define OVERRIDE_IF_SET(prop_) do { if (options->prop_) target->prop_ = options->prop_; } while (0)
#define set_if_updated(target_, original_) do { if ((int)original_ != -1) target_ = original_; } while (0)

static void set_dir_with_default(const char **setting, const char *option, const char *default_value)
{
	if (option)
	{
		*setting = option;
		return;
	}
	if (!*setting) *setting = default_value;
}

static void set_output_dir_from_options(const char **setting, const char *option, const char *default_value, const char *target_name, const char *out_dir)
{
	if (option)
	{
		*setting = option;
	}
	if (!*setting)
	{
		*setting = file_append_path(file_append_path(out_dir, default_value), target_name);
	}
}

static void update_build_target_from_options(BuildTarget *target, BuildOptions *options)
{
	switch (options->command)
	{
		case COMMAND_COMPILE_BENCHMARK:
		case COMMAND_BENCHMARK:
			target->run_after_compile = !options->suppress_run;
			target->type = TARGET_TYPE_BENCHMARK;
			break;
		case COMMAND_COMPILE_TEST:
		case COMMAND_TEST:
			target->run_after_compile = !options->suppress_run;
			target->type = TARGET_TYPE_TEST;
			switch (options->ansi)
			{
				case ANSI_ON:
					vec_add(target->args, "--useansi");
					break;
				case ANSI_OFF:
					vec_add(target->args, "--noansi");
					break;
				default:
					break;
			}
			if (options->test_filter)
			{
				vec_add(target->args, "--test-filter");
				vec_add(target->args, options->test_filter);
			}
			if (options->test_log_level > -1)
			{
				vec_add(target->args, "--test-log-level");
				vec_add(target->args, test_log_levels[options->test_log_level]);
			}
			if (options->test_breakpoint) vec_add(target->args, "--test-breakpoint");
			if (options->test_nosort) vec_add(target->args, "--test-nosort");
			if (options->test_quiet) vec_add(target->args, "--test-quiet");
			if (options->test_noleak) vec_add(target->args, "--test-noleak");
			if (options->test_show_output) vec_add(target->args, "--test-show-output");
			break;
		case COMMAND_RUN:
		case COMMAND_COMPILE_RUN:
		case COMMAND_CLEAN_RUN:
			target->run_after_compile = true;
			target->delete_after_run = options->run_once;
			target->args = options->args;
			break;
		case COMMAND_COMPILE_ONLY:
			target->type = TARGET_TYPE_OBJECT_FILES;
			target->emit_object_files = true;
			break;
		case COMMAND_DYNAMIC_LIB:
			target->type = TARGET_TYPE_DYNAMIC_LIB;
			break;
		case COMMAND_STATIC_LIB:
			target->type = TARGET_TYPE_STATIC_LIB;
			break;
		default:
			target->run_after_compile = false;
			break;
	}

	switch (options->command)
	{
		case COMMAND_BUILD:
			target->output_headers = (target->type == TARGET_TYPE_DYNAMIC_LIB || target->type == TARGET_TYPE_STATIC_LIB) && !options->no_headers;
			break;
		case COMMAND_STATIC_LIB:
		case COMMAND_DYNAMIC_LIB:
			target->output_headers = true;
			break;
		default:
			target->output_headers = false;
			break;
	}

	target->backend = options->backend;
	target->old_slice_copy = options->old_slice_copy;
	target->old_enums = options->old_enums;
	target->old_compact_eq = options->old_compact_eq;
	// Remove feature flags
	FOREACH(const char *, remove_feature, options->removed_feature_names)
	{
		FOREACH_IDX(i, const char *, feature, target->feature_list)
		{
			if (str_eq(feature, remove_feature))
			{
				vec_erase_at(target->feature_list, i);
				break;
			}
		}
	}

	// Add feature flags
	FOREACH(const char *, add_feature, options->feature_names)
	{
		FOREACH_IDX(i, const char *, feature, target->feature_list)
		{
			if (str_eq(feature, add_feature)) goto NEXT;
		}
		vec_add(target->feature_list, add_feature);
		NEXT:;
	}


	target->read_stdin = options->read_stdin;

	if (options->cc) target->cc = options->cc;
	set_if_updated(target->optlevel, options->optlevel);
	set_if_updated(target->optsize, options->optsize);
	set_if_updated(target->optsetting, options->optsetting);
	set_if_updated(target->single_module, options->single_module);
	set_if_updated(target->unroll_loops, options->unroll_loops);
	set_if_updated(target->merge_functions, options->merge_functions);
	set_if_updated(target->loop_vectorization, options->loop_vectorization);
	set_if_updated(target->slp_vectorization, options->slp_vectorization);
	set_if_updated(target->validation_level, options->validation_level);
	set_if_updated(target->feature.safe_mode, options->safety_level);
	set_if_updated(target->feature.panic_level, options->panic_level);
	set_if_updated(target->strip_unused, options->strip_unused);
	set_if_updated(target->memory_environment, options->memory_environment);
	set_if_updated(target->debug_info, options->debug_info_override);
	set_if_updated(target->show_backtrace, options->show_backtrace);
	set_if_updated(target->arch_os_target, options->arch_os_target_override);
	set_if_updated(target->reloc_model, options->reloc_model);
	set_if_updated(target->use_stdlib, options->use_stdlib);
	set_if_updated(target->link_libc, options->link_libc);
	set_if_updated(target->emit_stdlib, options->emit_stdlib);
	set_if_updated(target->win.crt_linking, options->win.crt_linking);
	set_if_updated(target->feature.fp_math, options->fp_math);
	set_if_updated(target->feature.x86_vector_capability, options->x86_vector_capability);
	set_if_updated(target->feature.x86_cpu_set, options->x86_cpu_set);
	set_if_updated(target->feature.riscv_cpu_set, options->riscv_cpu_set);
	set_if_updated(target->feature.riscv_abi, options->riscv_abi);
	set_if_updated(target->feature.win_debug, options->win_debug);
	set_if_updated(target->linuxpaths.libc, options->linux_libc);
	set_if_updated(target->feature.pass_win64_simd_as_arrays, options->win_64_simd);

	OVERRIDE_IF_SET(output_dir);
	OVERRIDE_IF_SET(panicfn);
	OVERRIDE_IF_SET(testfn);
	OVERRIDE_IF_SET(benchfn);
	OVERRIDE_IF_SET(symtab_size);
	OVERRIDE_IF_SET(max_vector_size);
	OVERRIDE_IF_SET(max_stack_object_size);
	OVERRIDE_IF_SET(max_macro_iterations);
	OVERRIDE_IF_SET(win.def);
	OVERRIDE_IF_SET(no_entry);
	OVERRIDE_IF_SET(echo_prefix);

	OVERRIDE_IF_SET(macos.sysroot);
	OVERRIDE_IF_SET(win.sdk);
	OVERRIDE_IF_SET(win.vs_dirs);
	OVERRIDE_IF_SET(macos.min_version);
	OVERRIDE_IF_SET(macos.sdk_version);
	OVERRIDE_IF_SET(linuxpaths.crt);
	OVERRIDE_IF_SET(linuxpaths.crtbegin);
	OVERRIDE_IF_SET(android.ndk_path);
	OVERRIDE_IF_SET(android.api_version);

	if (options->cpu_flags)
	{
		if (target->cpu_flags)
		{
			scratch_buffer_clear();
			scratch_buffer_printf("%s,%s", target->cpu_flags, options->cpu_flags);
			target->cpu_flags = scratch_buffer_copy();
		}
		else
		{
			target->cpu_flags = options->cpu_flags;
		}
	}
	if (!target->max_vector_size) target->max_vector_size = DEFAULT_VECTOR_WIDTH;
	if (!target->max_stack_object_size) target->max_stack_object_size = DEFAULT_STACK_OBJECT_SIZE;
	if (!target->max_macro_iterations) target->max_macro_iterations = DEFAULT_MAX_MACRO_ITERATIONS;
	if (target->quiet && !options->verbosity_level) options->verbosity_level = -1;

	if (options->silence_deprecation || options->verbosity_level < 0) target->silence_deprecation = true;
	target->print_linking = options->print_linking || options->verbosity_level > 1;

	for (size_t i = 0; i < options->linker_arg_count; i++)
	{
		vec_add(target->link_args, options->linker_args[i]);
	}
	for (size_t i = 0; i < options->linker_lib_dir_count; i++)
	{
		vec_add(target->linker_libdirs, options->linker_lib_dir[i]);
	}
	for (size_t i = 0; i < options->linker_lib_count; i++)
	{
		vec_add(target->linker_libs, options->linker_libs[i]);
	}
	target->trust_level = options->trust_level;
	if (options->linker_type != LINKER_TYPE_NOT_SET)
	{
		target->custom_linker_path = options->custom_linker_path;
		target->linker_type = options->linker_type;
	}
	target->print_output = options->print_output;
	target->print_input = options->print_input;
	target->emit_llvm = options->emit_llvm;
	target->build_threads = options->build_threads;
	target->emit_asm = options->emit_asm;
	target->print_stats = options->verbosity_level >= 2;

	target->benchmarking = options->benchmarking;
	target->testing = options->testing;
	target->silent = options->verbosity_level < 0;
	switch (options->sanitize_mode)
	{
		case SANITIZE_NOT_SET: break;
		case SANITIZE_NONE:
			target->feature.sanitize_address = false;
			target->feature.sanitize_memory = false;
			target->feature.sanitize_thread = false;
			break;
		case SANITIZE_ADDRESS: target->feature.sanitize_address = true; break;
		case SANITIZE_MEMORY: target->feature.sanitize_memory = true; break;
		case SANITIZE_THREAD: target->feature.sanitize_thread = true; break;
		default: UNREACHABLE_VOID;
	}

	if (target->arch_os_target == ARCH_OS_TARGET_DEFAULT) target->arch_os_target = default_target;

	if (target->arch_os_target == ARCH_OS_TARGET_DEFAULT)
	{
		error_exit("Unable to detect the default target, please set an explicit --target value.");
	}

	target->emit_only = options->emit_only;
	const char *target_name = arch_os_target[target->arch_os_target];
	OVERRIDE_IF_SET(run_dir);

	if (command_accepts_files(options->command))
	{
		set_dir_with_default(&target->output_dir, options->output_dir, ".");
		if (!target->build_dir)
		{
			if (!options->build_dir)
			{
				options->build_dir = dir_make_temp_dir();
				if (!options->build_dir)
				{
					error_exit("Unable to create temporary directory for build.");
				}
			}
			target->build_dir = options->build_dir;
		}
		set_dir_with_default(&target->script_dir, options->script_dir, ".");
	}
	else
	{
		set_dir_with_default(&target->output_dir, options->output_dir, "out");
		set_dir_with_default(&target->build_dir, options->build_dir, "build");
		set_dir_with_default(&target->script_dir, options->script_dir, "scripts");
	}

	set_output_dir_from_options(&target->ir_file_dir, options->llvm_out, "llvm", target_name, target->output_dir);
	set_output_dir_from_options(&target->asm_file_dir, options->asm_out, "asm", target_name, target->output_dir);
	set_output_dir_from_options(&target->header_file_dir, options->header_out, "headers", target_name, target->output_dir);
	if (target->type == TARGET_TYPE_OBJECT_FILES)
	{
		set_output_dir_from_options(&target->object_file_dir, options->obj_out, "obj", target_name, target->output_dir);
	}
	else
	{
		set_output_dir_from_options(&target->object_file_dir, options->obj_out, "obj", target_name, target->build_dir);
	}

	if (options->files)
	{
		append_strings_to_strings(&target->source_dirs, options->files);
	}

	switch (options->compile_option)
	{
		case COMPILE_NORMAL:
			target->emit_object_files = true;
			break;
		case COMPILE_LEX_ONLY:
			target->lex_only = true;
			break;
		case COMPILE_LEX_PARSE_ONLY:
			target->parse_only = true;
			break;
		case COMPILE_LEX_PARSE_CHECK_ONLY:
			target->check_only = true;
			break;
		case COMPILE_OUTPUT_HEADERS:
			target->output_headers = true;
			target->run_after_compile = false;
			target->emit_object_files = false;
			break;
		case COMPILE_OUTPUT_AST:
			target->parse_only = true;
			target->output_ast = true;
			break;
	}
	if (options->benchmark_mode)
	{
		target->benchmark_output = true;
		target->emit_llvm = false;
		target->emit_asm = false;
		target->emit_object_files = false;
	}
	if (options->lsp_mode)
	{
		target->lsp_output = true;
		target->emit_llvm = false;
		target->emit_asm = false;
		target->emit_object_files = false;
	}
	if (options->test_mode)
	{
		target->test_output = true;
		target->emit_llvm = false;
		target->emit_asm = false;
		target->emit_object_files = false;
	}
	if (options->no_obj)
	{
		target->emit_object_files = false;
	}
	if (options->output_name && target->emit_object_files
		&& target->single_module != SINGLE_MODULE_ON && target->type == TARGET_TYPE_OBJECT_FILES)
	{
		error_exit("'-o' cannot be used when generating multiple output files, try using '--single-module=yes' to compile into a single object file.");
	}
	for (int i = 0; i < options->lib_dir_count; i++)
	{
		vec_add(target->libdirs, options->lib_dir[i]);
	}
	for (int i = 0; i < options->lib_count; i++)
	{
		vec_add(target->libs, options->libs[i]);
	}
	if (target->optsetting == OPT_SETTING_NOT_SET) target->optsetting = OPT_SETTING_O0;
	update_build_target_with_opt_level(target, target->optsetting);
	if (target->link_libc == LINK_LIBC_NOT_SET)
	{
		target->link_libc = libc_from_arch_os(target->arch_os_target);
	}
}

void init_default_build_target(BuildTarget *target, BuildOptions *options)
{
	*target = default_build_target;
	target->source_dirs = NULL;
	target->name = options->output_name;
	target->output_name = options->output_name;
	target->runner_output_name = options->runner_output_name;
	update_build_target_from_options(target, options);
}

void init_build_target(BuildTarget *target, BuildOptions *options)
{
	*target = (BuildTarget) { 0 };
	// Parse it
	const char *filename;
	Project *project = project_load(&filename);
	*target = *project_select_target(filename, project, options->target_select);

	update_build_target_from_options(target, options);
	if (target->build_dir && !file_exists(target->build_dir))
	{
		if (!dir_make(target->build_dir)) error_exit("Failed to create build directory '%s'.", target->build_dir);
		if (!file_is_dir(target->build_dir)) error_exit("Expected '%s' to be a directory.", target->build_dir);
	}
	load_library_files();
}
