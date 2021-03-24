// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include <dirent.h>
#include "build_internal.h"
#include "build_options.h"


void load_library_files(void) {}
void load_files(void) {}
void compile_files(BuildTarget *target);

void build(const char *optional_target)
{
	// Locate the project.toml
	file_find_top_dir();
	// Parse it
	Project *project = project_load();
	BuildTarget *target = project_select_target(project, optional_target);

	load_library_files();
	compile_files(target);
}

static void update_build_target_from_options(BuildTarget *target, BuildOptions *options)
{
	switch (options->command)
	{
		case COMMAND_RUN:
		case COMMAND_COMPILE_RUN:
		case COMMAND_CLEAN_RUN:
			target->run_after_compile = true;
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

	// Copy optimization levels.
	switch (options->optimization_setting_override)
	{
		case OPT_SETTING_O0:
			target->optimization_level = OPTIMIZATION_NONE;
			target->size_optimization_level = SIZE_OPTIMIZATION_NONE;
			break;
		case OPT_SETTING_O1:
			target->optimization_level = OPTIMIZATION_LESS;
			target->size_optimization_level = SIZE_OPTIMIZATION_NONE;
			break;
		case OPT_SETTING_O2:
			target->optimization_level = OPTIMIZATION_DEFAULT;
			target->size_optimization_level = SIZE_OPTIMIZATION_NONE;
			break;
		case OPT_SETTING_O3:
			target->optimization_level = OPTIMIZATION_AGGRESSIVE;
			target->size_optimization_level = SIZE_OPTIMIZATION_NONE;
			break;
		case OPT_SETTING_OSMALL:
			target->optimization_level = OPTIMIZATION_DEFAULT;
			target->size_optimization_level = SIZE_OPTIMIZATION_SMALL;
			break;
		case OPT_SETTING_OTINY:
			target->optimization_level = OPTIMIZATION_DEFAULT;
			target->size_optimization_level = SIZE_OPTIMIZATION_TINY;
			break;
		case OPT_SETTING_NOT_SET:
			break;
		default:
			UNREACHABLE
	}
	if (options->debug_info_override != DEBUG_INFO_NOT_SET)
	{
		target->debug_info = options->debug_info_override;
	}
	if (options->arch_os_target_override != ARCH_OS_TARGET_DEFAULT)
	{
		target->arch_os_target = options->arch_os_target_override;
	}
	target->emit_llvm = options->emit_llvm;
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
	if (options->test_mode)
	{
		target->test_output = true;
		target->emit_llvm = false;
		target->emit_object_files = false;
	}
}

void init_default_build_target(BuildTarget *target, BuildOptions *options, const char *name)
{
	*target = (BuildTarget) {
		.type = TARGET_TYPE_EXECUTABLE,
		.sources = options->files,
		.name = name,
		.optimization_level = OPTIMIZATION_DEFAULT,
		.size_optimization_level = SIZE_OPTIMIZATION_NONE,
		.symtab_size = DEFAULT_SYMTAB_SIZE,
		.debug_info = DEBUG_INFO_NONE,
		.arch_os_target = ARCH_OS_TARGET_DEFAULT
	};
	update_build_target_from_options(target, options);
}

void init_build_target(BuildTarget *target, BuildOptions *options)
{
	// Locate the project.toml
	file_find_top_dir();
	// Parse it
	Project *project = project_load();
	*target = *project_select_target(project, options->target_select);

	update_build_target_from_options(target, options);

	load_library_files();
}