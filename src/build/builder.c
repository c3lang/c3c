// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.
#include "build_internal.h"
#include "build_options.h"

void load_library_files(void) {}

#if defined(_M_X64) || defined(_M_AMD64)
ArchOsTarget default_target = WINDOWS_X64;
#elif defined(__amd64__) || defined(__amd64) || defined(__x86_64__) || defined(__x86_64)
	#if defined(__MACH__)
ArchOsTarget default_target = MACOS_X64;
	#elif defined(__linux__) && __linux__
ArchOsTarget default_target = LINUX_X64;
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
	#elif defined(__linux__) && __linux__
ArchOsTarget default_target = LINUX_AARCH64;
	#else
ArchOsTarget default_target = ELF_AARCH64;
	#endif
#elif defined(i386) || defined(__i386__) || defined(__i386) || defined(_M_IX86)
	#if defined(__linux__) && __linux__
ArchOsTarget default_target = LINUX_X86;
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
	#else
ArchOsTarget default_target = ELF_RISCV32;
	#endif
#elif defined(__riscv64)
	#if defined(__linux__) && __linux__
ArchOsTarget default_target = LINUX_RISCV64;
	#else
ArchOsTarget default_target = ELF_RISCV64;
	#endif
#else
ArchOsTarget default_target = ARCH_OS_TARGET_DEFAULT;
#endif

bool command_is_projectless(CompilerCommand command)
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
		case COMMAND_GENERATE_HEADERS:
		case COMMAND_INIT:
		case COMMAND_BUILD:
		case COMMAND_RUN:
		case COMMAND_CLEAN_RUN:
		case COMMAND_CLEAN:
		case COMMAND_DIST:
		case COMMAND_DOCS:
		case COMMAND_BENCH:
		case COMMAND_PRINT_SYNTAX:
		case COMMAND_BENCHMARK:
		case COMMAND_TEST:
		case COMMAND_VENDOR_FETCH:
			return false;
	}
	UNREACHABLE
}

void update_build_target_with_opt_level(BuildTarget *target, OptimizationSetting level)
{
	if (level == OPT_SETTING_NOT_SET) level = OPT_SETTING_O0;
	OptimizationLevel optlevel = OPTIMIZATION_NONE;
	SizeOptimizationLevel optsize = SIZE_OPTIMIZATION_NONE;
	DebugInfo debug = DEBUG_INFO_FULL;
	SafetyLevel safety_level = SAFETY_ON;
	bool single_module = false;
	FpOpt fp_opt = FP_STRICT;
	switch (level)
	{
		case OPT_SETTING_O0_PLUS:
			single_module = true;
			break;
		case OPT_SETTING_O0:
			break;
		case OPT_SETTING_O1_PLUS:
			single_module = true;
			FALLTHROUGH;
		case OPT_SETTING_O1:
			optlevel = OPTIMIZATION_MORE;
			break;
		case OPT_SETTING_O2_PLUS:
			single_module = true;
			FALLTHROUGH;
		case OPT_SETTING_O2:
			optlevel = OPTIMIZATION_MORE;
			safety_level = false;
			break;
		case OPT_SETTING_O3_PLUS:
			single_module = true;
			FALLTHROUGH;
		case OPT_SETTING_O3:
			optlevel = OPTIMIZATION_AGGRESSIVE;
			safety_level = SAFETY_OFF;
			fp_opt = FP_RELAXED;
			break;
		case OPT_SETTING_O4_PLUS:
			single_module = true;
			FALLTHROUGH;
		case OPT_SETTING_O4:
			optlevel = OPTIMIZATION_AGGRESSIVE;
			safety_level = SAFETY_OFF;
			fp_opt = FP_FAST;
			break;
		case OPT_SETTING_OSMALL_PLUS:
			single_module = true;
			FALLTHROUGH;
		case OPT_SETTING_OSMALL:
			optlevel = OPTIMIZATION_MORE;
			optsize = SIZE_OPTIMIZATION_SMALL;
			safety_level = SAFETY_OFF;
			break;
		case OPT_SETTING_OTINY_PLUS:
			target->single_module = true;
			FALLTHROUGH;
		case OPT_SETTING_OTINY:
			optlevel = OPTIMIZATION_MORE;
			optsize = SIZE_OPTIMIZATION_TINY;
			safety_level = SAFETY_OFF;
			debug = DEBUG_INFO_NONE;
			break;
		case OPT_SETTING_NOT_SET:
		default:
			UNREACHABLE
	}
	if (target->optsize == SIZE_OPTIMIZATION_NOT_SET) target->optsize = optsize;
	if (target->optlevel == OPTIMIZATION_NOT_SET) target->optlevel = optlevel;
	if (target->feature.safe_mode == SAFETY_NOT_SET) target->feature.safe_mode = safety_level;
	if (target->debug_info == DEBUG_INFO_NOT_SET) target->debug_info = debug;
	if (target->feature.fp_math == FP_DEFAULT) target->feature.fp_math = fp_opt;
	if (single_module) target->single_module = true;
}
static void update_build_target_from_options(BuildTarget *target, BuildOptions *options)
{
	switch (options->command)
	{
		case COMMAND_COMPILE_BENCHMARK:
		case COMMAND_BENCHMARK:
			target->run_after_compile = true;
			target->type = TARGET_TYPE_BENCHMARK;
			break;
		case COMMAND_COMPILE_TEST:
		case COMMAND_TEST:
			target->run_after_compile = true;
			target->type = TARGET_TYPE_TEST;
			break;
		case COMMAND_RUN:
		case COMMAND_COMPILE_RUN:
		case COMMAND_CLEAN_RUN:
			target->run_after_compile = true;
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
			target->output_headers = target->type == TARGET_TYPE_DYNAMIC_LIB || target->type == TARGET_TYPE_STATIC_LIB;
			break;
		case COMMAND_GENERATE_HEADERS:
			target->output_headers = true;
			break;
		default:
			target->output_headers = false;
			break;
	}

	target->backend = options->backend;

	// Remove feature flags
	FOREACH_BEGIN(const char *remove_feature, options->removed_feature_names)
		FOREACH_BEGIN_IDX(i, const char *feature, target->feature_list)
			if (str_eq(feature, remove_feature))
			{
				vec_erase_ptr_at(target->feature_list, i);
				break;
			}
		FOREACH_END();
	FOREACH_END();

	// Add feature flags
	FOREACH_BEGIN(const char *add_feature, options->feature_names)
		FOREACH_BEGIN_IDX(i, const char *feature, target->feature_list)
			if (str_eq(feature, add_feature)) goto NEXT;
		FOREACH_END();
		vec_add(target->feature_list, add_feature);
	NEXT:;
	FOREACH_END();


	if (options->cc) target->cc = options->cc;
	if (options->optlevel != OPTIMIZATION_NOT_SET)
	{
		target->optlevel = options->optlevel;
	}
	if (options->optsize != SIZE_OPTIMIZATION_NOT_SET)
	{
		target->optsize = options->optsize;
	}
	if (options->safety_level != SAFETY_NOT_SET)
	{
		target->feature.safe_mode = options->safety_level;
	}
	if (options->no_strip_unused || options->test_mode) target->no_strip_unused = true;

	if (options->memory_environment != MEMORY_ENV_NOT_SET)
	{
		target->memory_environment = options->memory_environment;
	}
	if (options->debug_info_override != DEBUG_INFO_NOT_SET)
	{
		target->debug_info = options->debug_info_override;
	}
	if (options->arch_os_target_override != ARCH_OS_TARGET_DEFAULT)
	{
		target->arch_os_target = options->arch_os_target_override;
	}
	if (options->reloc_model != RELOC_DEFAULT) target->reloc_model = options->reloc_model;

	for (int i = 0; i < options->linker_arg_count; i++)
	{
		vec_add(target->link_args, options->linker_args[i]);
	}
	for (int i = 0; i < options->linker_lib_dir_count; i++)
	{
		vec_add(target->linker_libdirs, options->linker_lib_dir[i]);
	}
	for (int i = 0; i < options->linker_lib_count; i++)
	{
		vec_add(target->linker_libs, options->linker_libs[i]);
	}
	target->trust_level = options->trust_level;
	if (options->no_stdlib) target->no_stdlib = true;
	if (options->no_libc) target->no_libc = true;
	if (options->no_entry) target->no_entry = true;
	target->print_output = options->print_output;
	target->emit_llvm = options->emit_llvm;
	target->build_threads = options->build_threads;
	target->emit_asm = options->emit_asm;
	target->force_linker = options->force_linker;
	target->panicfn = options->panicfn;
	target->benchmarking = options->benchmarking;
	target->testing = options->testing;
	if (options->macos.sdk) target->macos.sdk = options->macos.sdk;
	if (options->win.sdk) target->win.sdk = options->win.sdk;
	if (options->macos.min_version) target->macos.min_version = options->macos.min_version;
	if (options->macos.sdk_version) target->macos.sdk_version = options->macos.sdk_version;
	if (options->win.crt_linking != WIN_CRT_DEFAULT) target->win.crt_linking = options->win.crt_linking;
	if (options->linuxpaths.crt) target->linuxpaths.crt = options->linuxpaths.crt;
	if (options->linuxpaths.crtbegin) target->linuxpaths.crtbegin = options->linuxpaths.crtbegin;
	if (options->fp_math != FP_DEFAULT)
	{
		target->feature.fp_math = options->fp_math;
	}
	if (options->x86_vector_capability != X86VECTOR_DEFAULT)
	{
		target->feature.x86_vector_capability = options->x86_vector_capability;
	}
	if (options->x86_cpu_set != X86CPU_DEFAULT)
	{
		target->feature.x86_cpu_set = options->x86_cpu_set;
	}
	if (options->riscv_float_capability != RISCVFLOAT_DEFAULT)
	{
		target->feature.riscv_float_capability = options->riscv_float_capability;
	}
	if (command_is_projectless(options->command))
	{
		target->build_dir = options->build_dir ? options->build_dir : NULL;
		target->object_file_dir = options->obj_out ? options->obj_out : target->build_dir;
		target->ir_file_dir = options->llvm_out ? options->llvm_out : target->build_dir;
		target->asm_file_dir = options->asm_out ? options->asm_out : target->build_dir;
	}
	else
	{
		target->build_dir = options->build_dir ? options->build_dir : "build";
		target->object_file_dir = options->obj_out ? options->obj_out : file_append_path(target->build_dir, "tmp");
		target->ir_file_dir = options->llvm_out ? options->llvm_out : file_append_path(target->build_dir, "llvm_ir");
		target->asm_file_dir = options->asm_out ? options->asm_out : file_append_path(target->build_dir, "asm");
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
	target->no_emit_stdlib = options->no_emit_stdlib;
	for (int i = 0; i < options->lib_dir_count; i++)
	{
		vec_add(target->libdirs, options->lib_dir[i]);
	}
	for (int i = 0; i < options->lib_count; i++)
	{
		vec_add(target->libs, options->libs[i]);
	}
	if (options->optsetting != OPT_SETTING_NOT_SET)
	{
		target->optsetting = options->optsetting;
	}
	update_build_target_with_opt_level(target, target->optsetting);


}

void init_default_build_target(BuildTarget *target, BuildOptions *options)
{
	*target = (BuildTarget) {
		.read_stdin = options->read_stdin,
		.type = TARGET_TYPE_EXECUTABLE,
		.source_dirs = options->files,
		.name = options->output_name,
		.optsetting = OPT_SETTING_NOT_SET,
		.optlevel = OPTIMIZATION_NOT_SET,
		.memory_environment = MEMORY_ENV_NORMAL,
		.optsize = SIZE_OPTIMIZATION_NOT_SET,
		.symtab_size = options->symtab_size ? options->symtab_size : DEFAULT_SYMTAB_SIZE,
		.switchrange_max_size = DEFAULT_SWITCHRANGE_MAX_SIZE,
		.debug_info = DEBUG_INFO_NOT_SET,
		.arch_os_target = ARCH_OS_TARGET_DEFAULT,
		.reloc_model = RELOC_DEFAULT,
		.feature.x86_vector_capability = X86VECTOR_DEFAULT,
		.feature.x86_cpu_set = X86CPU_DEFAULT,
		.feature.fp_math = FP_DEFAULT,
		.feature.riscv_float_capability = RISCVFLOAT_DEFAULT,
		.feature.safe_mode = SAFETY_NOT_SET,
		.win.crt_linking = WIN_CRT_DEFAULT,
	};
	update_build_target_from_options(target, options);
}

void init_build_target(BuildTarget *target, BuildOptions *options)
{
	*target = (BuildTarget) { 0 };
	// Locate the project.json
	file_find_top_dir();
	// Parse it
	Project *project = project_load();
	*target = *project_select_target(project, options->target_select);

	update_build_target_from_options(target, options);
	if (target->build_dir && !file_exists(target->build_dir))
	{
		if (!dir_make(target->build_dir)) error_exit("Failed to create build directory '%s'.", target->build_dir);
		if (!file_is_dir(target->build_dir)) error_exit("Expected '%s' to be a directory.", target->build_dir);
	}
	load_library_files();
}