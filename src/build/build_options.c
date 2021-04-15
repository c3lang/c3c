// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "build_options.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>
#include <stdbool.h>
#include <string.h>
#include <utils/lib.h>






static int arg_index;
static int arg_count;
static const char** args;
static const char* current_arg;

char *arch_os_target[ARCH_OS_TARGET_LAST + 1] = {
		[X86_FREEBSD] = "x86_freebsd",
		[X86_OPENBSD] = "x86_openbsd",
		[X86_MCU] = "x86_mcu",
		[X86_WINDOWS] = "x86-windows",
		[X86_LINUX] = "x86_linux",
		[X64_DARWIN] = "x64_darwin",
		[X64_LINUX] = "x64_linux",
		[X64_WINDOWS] = "x64_windows",
		[X64_WINDOWS_GNU] = "x64_mingw",
		[X64_NETBSD] = "x64_netbsd",
		[AARCH64_LINUX] = "aarch64_linux",
		[AARCH64_DARWIN] = "aarch64_darwin",
		[RISCV32_LINUX] = "riscv32-linux",
		[RISCV64_LINUX] = "riscv64-linux",
		[WASM32] = "wasm32",
		[WASM64] = "wasm64",
};

#define EOUTPUT(string, ...) fprintf(stderr, string "\n", ##__VA_ARGS__)
#define OUTPUT(string, ...) fprintf(stdout, string "\n", ##__VA_ARGS__)
#define FAIL_WITH_ERR(string, ...) do { fprintf(stderr, "Error: " string "\n\n", ##__VA_ARGS__); usage(); exit(EXIT_FAILURE); } while (0)

static void usage(void)
{
	OUTPUT("Usage: %s [<options>] <command> [<args>]", args[0]);
	OUTPUT("");
	OUTPUT("Commands:");
	OUTPUT("");
	OUTPUT("  compile <file1> [<file2> ...]      Compile files without a project into an executable.");
	OUTPUT("  init <project name>                Initialize a new project structure.");
	OUTPUT("  build [<target>]                   Build the target in the current project.");
	OUTPUT("  clean                              Clean all build files.");
	OUTPUT("  run [<target>]                     Run (and build if needed) the target in the current project.");
	OUTPUT("  dist [<target>]                    Clean and build a target for distribution.");
	OUTPUT("  directives [<target>]                    Generate documentation for the target.");
	OUTPUT("  bench [<target>]                   Benchmark a target.");
	OUTPUT("  clean-run [<target>]               Clean, then run the target.");
	OUTPUT("  compile-run <file1> [<file2> ...]  Compile files then immediately run the result.");
	OUTPUT("  headers <file1> [<file2> ...]      Analyse files and generate C headers for public methods.");
	OUTPUT("");
	OUTPUT("Options:");
	OUTPUT("  --lib <dir>           - Use this directory as the c3 library path.");
	OUTPUT("  --path <dir>          - Use this as the base directory for the current command.");
	OUTPUT("  --template <template> - Use a different template: \"lib\", \"staticlib\" or a path.");
	OUTPUT("  --about               - Prints a short description of C3.");
	OUTPUT("  --symtab <value>      - Sets the preferred symtab size.");
	OUTPUT("  -E                    - Lex only.");
	OUTPUT("  -P                    - Only parse and output the AST as S-expressions.");
	OUTPUT("  -O0                   - Optimizations off.");
	OUTPUT("  -O1                   - Simple optimizations only.");
	OUTPUT("  -O2                   - Default optimization level.");
	OUTPUT("  -Os                   - Optimize for size.");
	OUTPUT("  -O3                   - Aggressive optimization.");
	OUTPUT("  --emit-llvm           - Emit LLVM IR as a .ll file per module.");
	OUTPUT("  --target <target>     - Compile for a particular architecture + OS target.");
	OUTPUT("  --target-list         - List all architectures the compiler supports.");
	OUTPUT("");
	OUTPUT("  -g                    - Emit full debug info.");
	OUTPUT("  -g0                   - Emit no debug info.");
	OUTPUT("  -gline-tables-only    - Only emit line tables for debugging.");
	OUTPUT("");
	OUTPUT("  -freg-struct-return   - Override default ABI to return small structs in registers.");
	OUTPUT("  -fpcc-struct-return   - Override default ABI to return small structs on the stack.");
	OUTPUT("  -fno-memcpy-pass      - Prevents compiler from doing a mem copy pass (for debug).");
	OUTPUT("  -fpic                 - Generate position independent (PIC) code if suitable.");
	OUTPUT("  -fno-pic              - Do not generate position independent code.");
	OUTPUT("  -fPIC                 - Always generate position independent (PIC) code.");
	OUTPUT("  -fno-PIC              - generate position independent (PIC) code.");
	OUTPUT("");
	OUTPUT("  -msoft-float          - Use software floating point.");
	OUTPUT("  -mno-soft-float       - Prevent use of software floating point.");
}



static const char* check_dir(const char *path)
{
	static char *original_path = NULL;
	if (!original_path)
	{
		original_path = getcwd(NULL, 0);
	}
	if (chdir(path) == -1) error_exit("The path \"%s\" does not point to a valid directory.", path);
	int err = chdir(original_path);
	assert(!err);
	return path;
}

static inline bool at_end()
{
	return arg_index == arg_count - 1;
}

static inline const char* next_arg()
{
	assert(!at_end());
	current_arg = args[++arg_index];
	return current_arg;
}


static inline bool next_is_opt()
{
	return args[arg_index + 1][0] == '-';
}

static inline bool match_longopt(const char* name)
{
	return strcmp(&current_arg[2], name) == 0;
}

static inline bool match_shortopt(const char* name)
{
	return strcmp(&current_arg[1], name) == 0;
}


void append_file(BuildOptions *build_options)
{
	if (vec_size(build_options->files) == MAX_FILES)
	{
		fprintf(stderr, "Max %d files may be specified\n", MAX_FILES);
		exit(EXIT_FAILURE);
	}
	build_options->files = VECADD(build_options->files, current_arg);
}

static bool arg_match(const char *candidate)
{
	return strcmp(current_arg, candidate) == 0;
}

static void parse_optional_target(BuildOptions *options)
{
	if (at_end() || next_is_opt())
	{
		options->target_select = NULL;
	}
	else
	{
		options->target_select = next_arg();
	}
}

static void parse_command(BuildOptions *options)
{
	if (arg_match("init"))
	{
		options->command = COMMAND_INIT;
		if (at_end() || next_is_opt()) error_exit("Expected a project name after init");
		options->project_name = next_arg();
		return;
	}
	if (arg_match("utest"))
	{
		options->command = COMMAND_UNIT_TEST;
		return;
	}
	if (arg_match("compile"))
	{
		options->command = COMMAND_COMPILE;
		return;
	}
	if (arg_match("headers"))
	{
		options->command = COMMAND_GENERATE_HEADERS;
		return;
	}
	if (arg_match("build"))
	{
		options->command = COMMAND_BUILD;
		parse_optional_target(options);
		return;
	}
	if (arg_match("run"))
	{
		options->command = COMMAND_RUN;
		parse_optional_target(options);
		return;
	}
	if (arg_match("compile-run"))
	{
		options->command = COMMAND_COMPILE_RUN;
		return;
	}
	if (arg_match("clean-run"))
	{
		options->command = COMMAND_CLEAN_RUN;
		parse_optional_target(options);
		return;
	}
	if (arg_match("clean"))
	{
		options->command = COMMAND_CLEAN;
		return;
	}
	if (arg_match("dist"))
	{
		options->command = COMMAND_CLEAN_RUN;
		parse_optional_target(options);
		return;
	}
	if (arg_match("directives"))
	{
		options->command = COMMAND_DOCS;
		parse_optional_target(options);
		return;
	}
	if (arg_match("bench"))
	{
		options->command = COMMAND_BENCH;
		parse_optional_target(options);
		return;
	}
	FAIL_WITH_ERR("Cannot process the unknown command \"%s\".", current_arg);
}
static void print_all_targets(void)
{
	OUTPUT("Available targets:");
	for (unsigned i = 1; i <= ARCH_OS_TARGET_LAST; i++)
	{
		OUTPUT("   %s", arch_os_target[i]);
	}
}

static void parse_option(BuildOptions *options)
{
	switch (current_arg[1])
	{
		case 'g':
			if (match_shortopt("gline-tables-only"))
			{
				options->debug_info_override = DEBUG_INFO_LINE_TABLES;
				FATAL_ERROR("Line tables only are currently not available");
			}
			if (match_shortopt("g") || match_shortopt("g1"))
			{
				options->debug_info_override = DEBUG_INFO_FULL;
				return;
			}
			if (match_shortopt("g0"))
			{
				options->debug_info_override = DEBUG_INFO_NONE;
				return;
			}
			FAIL_WITH_ERR("Unknown debug argument -%s.", &current_arg[1]);
		case 'h':
			break;
		case 'O':
			if (options->optimization_setting_override != OPT_SETTING_NOT_SET)
			{
				FAIL_WITH_ERR("Multiple optimization levels were set.");
			}
			if (match_shortopt("O0"))
			{
				options->optimization_setting_override = OPT_SETTING_O0;
			}
			else if (match_shortopt("O1"))
			{
				options->optimization_setting_override = OPT_SETTING_O1;
			}
			else if (match_shortopt("O2"))
			{
				options->optimization_setting_override = OPT_SETTING_O2;
			}
			else if (match_shortopt("O3"))
			{
				options->optimization_setting_override = OPT_SETTING_O3;
			}
			else if (match_shortopt("Os"))
			{
				options->optimization_setting_override = OPT_SETTING_OSMALL;
			}
			else if (match_shortopt("Oz"))
			{
				options->optimization_setting_override = OPT_SETTING_OTINY;
			}
			else
			{
				FAIL_WITH_ERR("Invalid optimization level.");
			}
			return;
		case 'E':
			if (options->compile_option != COMPILE_NORMAL)
			{
				FAIL_WITH_ERR("Illegal combination of compile options.");
			}
			options->compile_option = COMPILE_LEX_ONLY;
			return;
		case 'P':
			if (options->compile_option != COMPILE_NORMAL)
			{
				FAIL_WITH_ERR("Illegal combination of compile options.");
			}
			options->compile_option = COMPILE_LEX_PARSE_ONLY;
			return;
		case '-':
			if (match_longopt("about"))
			{
				OUTPUT("The C3 Compiler");
				OUTPUT("C3 is low level programming language based on C.");
				exit(EXIT_SUCCESS);
			}
			if (match_longopt("target"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --target needs a arch+os definition.");
				const char *target = next_arg();
				ArchOsTarget target_arch_os = arch_os_target_from_string(target);
				if (target_arch_os != ARCH_OS_TARGET_DEFAULT)
				{
					options->arch_os_target_override = target_arch_os;
					return;
				}
				OUTPUT("Available targets:");
				EOUTPUT("Invalid target %s.", target);
				EOUTPUT("These targets are supported:");
				for (unsigned i = 1; i <= ARCH_OS_TARGET_LAST; i++)
				{
					EOUTPUT("   %s", arch_os_target[i]);
				}
				exit(EXIT_FAILURE);
			}
			if (match_longopt("target-list"))
			{
				print_all_targets();
				exit(EXIT_SUCCESS);
			}
			if (match_longopt("emit-llvm"))
			{
				options->emit_llvm = true;
				return;
			}
			if (match_longopt("lib"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --lib needs a directory.");
				if (options->lib_count == MAX_LIB_DIRS) error_exit("Max %d libraries may be specified.", MAX_LIB_DIRS);
				options->lib_dir[options->lib_count++] = check_dir(next_arg());
				return;
			}
			if (match_longopt("test"))
			{
				options->test_mode = true;
				return;
			}
			if (match_longopt("path"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --path needs a directory.");
				options->path = check_dir(next_arg());
				return;
			}
			if (match_longopt("safe"))
			{
				options->safe_mode = 1;
				return;
			}
			if (match_longopt("unsafe"))
			{
				options->safe_mode = 0;
				return;
			}
			if (match_longopt("help"))
			{
				break;
			}
			break;
		default:
			break;

	}
	FAIL_WITH_ERR("Cannot process the unknown option \"%s\".", current_arg);
}


BuildOptions parse_arguments(int argc, const char *argv[])
{
	arg_count = argc;
	args = argv;

	if (argc < 2)
	{
		usage();
		exit(EXIT_SUCCESS);
	}

	BuildOptions build_options = {
		.path = ".",
		.emit_llvm = false,
		.emit_bitcode = true,
		.optimization_setting_override = OPT_SETTING_NOT_SET,
		.debug_info_override = DEBUG_INFO_NOT_SET,
		.safe_mode = -1,
		.command = COMMAND_MISSING,
		.files = NULL
	};
	for (int i = DIAG_NONE; i < DIAG_WARNING_TYPE; i++)
	{
		build_options.severity[i] = DIAG_IGNORE;
	}
	for (int i = DIAG_WARNING_TYPE; i < DIAG_ERROR_TYPE; i++)
	{
		build_options.severity[i] = DIAG_WARN;
	}
	for (int i = DIAG_ERROR_TYPE; i < DIAG_END_SENTINEL; i++)
	{
		build_options.severity[i] = DIAG_ERROR;
	}

	for (arg_index = 1; arg_index < arg_count; arg_index++)
	{
		current_arg = args[arg_index];
		if (current_arg[0] == '-')
		{
			parse_option(&build_options);
			continue;
		}
		if (build_options.command == COMMAND_MISSING)
		{
			parse_command(&build_options);
			continue;
		}
		if (build_options.command == COMMAND_COMPILE_RUN
			|| build_options.command == COMMAND_COMPILE
			|| build_options.command == COMMAND_GENERATE_HEADERS)
		{
			append_file(&build_options);
			continue;
		}
		FAIL_WITH_ERR("Found the unexpected argument \"%s\".", current_arg);
	}
	if (build_options.command == COMMAND_MISSING)
	{
		FAIL_WITH_ERR("No command found.");
	}
	return build_options;
}


ArchOsTarget arch_os_target_from_string(const char *target)
{
	for (unsigned i = 1; i <= ARCH_OS_TARGET_LAST; i++)
	{
		if (strcasecmp(arch_os_target[i], target) == 0)
		{
			return i;
		}
	}
	return ARCH_OS_TARGET_DEFAULT;
}