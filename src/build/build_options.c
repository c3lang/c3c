// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "build_options.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#ifndef _MSC_VER
#include <unistd.h>
#endif
#include <stdbool.h>
#include <string.h>
#include <utils/lib.h>
#include "../utils/whereami.h"

extern int llvm_version_major;

static int arg_index;
static int arg_count;
static const char** args;
static const char* current_arg;
extern const char* llvm_version;
extern const char* llvm_target;

char *arch_os_target[ARCH_OS_TARGET_LAST + 1] = {
		[ELF_AARCH64] = "elf-aarch64",
		[ELF_RISCV32] = "elf-riscv32",
		[ELF_RISCV64] = "elf-riscv64",
		[ELF_X86] = "elf-x86",
		[ELF_X64] = "elf-x64",
		[FREEBSD_X86] = "freebsd-x86",
		[FREEBSD_X64] = "freebsd-x64",
		[LINUX_AARCH64] = "linux-aarch64",
		[LINUX_RISCV32] = "linux-riscv32",
		[LINUX_RISCV64] = "linux-riscv64",
		[LINUX_X86] = "linux-x86",
		[LINUX_X64] = "linux-x64",
		[MACOS_AARCH64] = "macos-aarch64",
		[MACOS_X64] = "macos-x64",
		[MCU_X86] = "mcu-x86",
		[MINGW_X64] = "mingw-x64",
		[NETBSD_X86] = "netbsd-x86",
		[NETBSD_X64] = "netbsd-x64",
		[OPENBSD_X86] = "openbsd-x86",
		[OPENBSD_X64] = "openbsd-x64",
		[WASM32] = "wasm32",
		[WASM64] = "wasm64",
		[WINDOWS_X86] = "windows-x86",
		[WINDOWS_X64] = "windows-x64",
};

#define EOUTPUT(string, ...) fprintf(stderr, string "\n", ##__VA_ARGS__)
#define OUTPUT(string, ...) fprintf(stdout, string "\n", ##__VA_ARGS__)
#define FAIL_WITH_ERR(string, ...) do { fprintf(stderr, "Error: " string "\n\n", ##__VA_ARGS__); usage(); exit_compiler(EXIT_FAILURE); } while (0)

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
	OUTPUT("  static-lib <file1> [<file2> ...]   Compile files without a project into a static library.");
	OUTPUT("  dynamic-lib <file1> [<file2> ...]  Compile files without a project into a dynamic library.");
	OUTPUT("  headers <file1> [<file2> ...]      Analyse files and generate C headers for public methods.");
	OUTPUT("");
	OUTPUT("Options:");
	OUTPUT("  --tinybackend         - Use the TinyBackend for compilation.");
	OUTPUT("  --stdlib <dir>        - Use this directory as the C3 standard library path.");
	OUTPUT("  --nostdlib            - Do not include the standard library.");
	OUTPUT("  --libdir <dir>        - Add this directory to the C3 library search paths.");
	OUTPUT("  --lib <name>          - Add this library to the compilation.");
	OUTPUT("  --path <dir>          - Use this as the base directory for the current command.");
	OUTPUT("  --template <template> - Use a different template: \"lib\", \"static-lib\" or a path.");
	OUTPUT("  --about               - Prints a short description of C3.");
	OUTPUT("  --symtab <value>      - Sets the preferred symtab size.");
	OUTPUT("  -V --version          - Print version information.");
	OUTPUT("  -E                    - Lex only.");
	OUTPUT("  -P                    - Only parse and output the AST as S-expressions.");
	OUTPUT("  -C                    - Only lex, parse and check.");
	OUTPUT("  -o <file>             - Write output to <file>.");
	OUTPUT("  -O0                   - Optimizations off.");
	OUTPUT("  -O1                   - Simple optimizations only.");
	OUTPUT("  -O2                   - Default optimization level.");
	OUTPUT("  -Os                   - Optimize for size.");
	OUTPUT("  -O3                   - Aggressive optimization.");
	OUTPUT("  --build-dir <dir>     - Override build output directory.");
	OUTPUT("  --obj-out <dir>       - Override object file output directory.");
	OUTPUT("  --llvm-out <dir>      - Override llvm output directory for '--emit-llvm'.");
	OUTPUT("  --emit-llvm           - Emit LLVM IR as a .ll file per module.");
	OUTPUT("  --asm-out <dir>       - Override asm output directory for '--emit-asm'.");
	OUTPUT("  --emit-asm            - Emit asm as a .s file per module.");
	OUTPUT("  --target <target>     - Compile for a particular architecture + OS target.");
	OUTPUT("  --threads <number>    - Set the number of threads to use for compilation.");
	OUTPUT("  --safe                - Set mode to 'safe', generating runtime traps on overflows and contract violations.");
	OUTPUT("  --fast                - Set mode to 'fast', removes runtime traps.");
	OUTPUT("");
	OUTPUT("  -g                    - Emit full debug info.");
	OUTPUT("  -g0                   - Emit no debug info.");
	OUTPUT("  -gline-tables-only    - Only emit line tables for debugging.");
	OUTPUT("");
	OUTPUT("");
	OUTPUT("  -l <library>          - Link with the library provided.");
	OUTPUT("  -L <library dir>      - Append the directory to the linker search paths.");
	OUTPUT("  -z <argument>         - Send the <argument> as a parameter to the linker.");
	OUTPUT("  --forcelinker         - Force built in linker usage when doing non-cross linking.");
	OUTPUT("");
	OUTPUT("  --reloc=<option>      - Relocation model: none, pic, PIC, pie, PIE");
	OUTPUT("  --x86vec=<option>     - Set max level of vector instructions: none, mmx, sse, avx, avx512.");
	OUTPUT("");
	OUTPUT("  --debug-stats         - Print debug statistics.");
#ifndef NDEBUG
	OUTPUT("  --debug-log           - Print debug logging to stdout.");
#endif
	OUTPUT("");
	OUTPUT("  --list-attributes     - List all attributes.");
	OUTPUT("  --list-builtins       - List all builtins.");
	OUTPUT("  --list-keywords       - List all keywords.");
	OUTPUT("  --list-operators      - List all operators.");
	OUTPUT("  --list-precedence     - List operator precedence order.");
	OUTPUT("  --list-targets        - List all architectures the compiler supports.");
	OUTPUT("");
	OUTPUT("  --winsdk <dir>        - Set the directory for Windows system library files for cross compilation.");
	OUTPUT("  --wincrt=<option>     - Windows CRT linking: none, static, dynamic (default).");
	OUTPUT("");
	OUTPUT("  --macossdk <dir>      - Set the directory for the MacOS SDK for cross compilation.");

}



static const char* check_dir(const char *path)
{
	static char *original_path = NULL;
	if (!original_path)
	{
		original_path = getcwd(NULL, 0);
	}
	if (!dir_change(path)) error_exit("The path \"%s\" does not point to a valid directory.", path);
	if (!dir_change(original_path)) FAIL_WITH_ERR("Failed to change path to %s.", original_path);
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

static inline const char *match_argopt(const char* name)
{
	size_t len = strlen(name);
	if (memcmp(&current_arg[2], name, len) != 0) return false;
	if (current_arg[2 + len] != '=') return false;
	return &current_arg[2 + len + 1];
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
		exit_compiler(EXIT_FAILURE);
	}
	vec_add(build_options->files, current_arg);
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
	if (arg_match("compile-only"))
	{
		options->command = COMMAND_COMPILE_ONLY;
		return;
	}
	if (arg_match("headers"))
	{
		options->command = COMMAND_GENERATE_HEADERS;
		return;
	}
	if (arg_match("static-lib"))
	{
		options->command = COMMAND_STATIC_LIB;
		return;
	}
	if (arg_match("dynamic-lib"))
	{
		options->command = COMMAND_DYNAMIC_LIB;
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

static void print_version(void)
{
	OUTPUT("C3 Compiler Version (pre-alpha):   %s", COMPILER_VERSION);
	OUTPUT("Installed directory:               %s", find_executable_path());
	OUTPUT("LLVM version:                      %s", llvm_version);
	OUTPUT("LLVM default target:               %s", llvm_target);
}

static void add_linker_arg(BuildOptions *options, const char *arg)
{
	if (options->linker_arg_count == MAX_LIB_DIRS)
	{
		error_exit("Too many linker arguments are given, more than %d\n", MAX_LIB_DIRS);
	}
	options->linker_args[options->linker_arg_count++] = arg;
}

static int parse_multi_option(const char *start, unsigned count, const char** elements)
{
	const char *arg = current_arg;
	int select = str_findlist(start, count, elements);
	if (select < 0) error_exit("error: %.*s invalid option '%s' given.", (int)(start - arg), start, arg);
	return select;
}

static void parse_option(BuildOptions *options)
{
	const char *argopt;
	switch (current_arg[1])
	{
		case '?':
			if (match_shortopt("?"))
			{
				usage();
				exit_compiler(COMPILER_SUCCESS_EXIT);
			}
			break;
		case 'V':
			if (match_shortopt("V"))
			{
				print_version();
				exit_compiler(-1000);
			}
			break;
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
		case 'z':
			if (at_end()) error_exit("error: -z needs a value");
			add_linker_arg(options, next_arg());
			return;
		case 'o':
			if (at_end()) error_exit("error: -o needs a name");
			options->output_name = next_arg();
			return;
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
		case 'L':
			if (at_end() || next_is_opt()) error_exit("error: -L needs a directory.");
			options->linker_lib_dir[options->linker_lib_dir_count++] = check_dir(next_arg());
			return;
		case 'l':
			if (at_end() || next_is_opt()) error_exit("error: -l needs a library name.");
			options->linker_libs[options->linker_lib_count++] = next_arg();
			return;
		case 'P':
			if (options->compile_option != COMPILE_NORMAL)
			{
				FAIL_WITH_ERR("Illegal combination of compile options.");
			}
			options->compile_option = COMPILE_LEX_PARSE_ONLY;
			return;
		case 'C':
			if (options->compile_option != COMPILE_NORMAL)
			{
				FAIL_WITH_ERR("Illegal combination of compile options.");
			}
			options->compile_option = COMPILE_LEX_PARSE_CHECK_ONLY;
			return;
		case '-':
			if (match_longopt("tinybackend"))
			{
#if TB_BACKEND
				options->backend = BACKEND_TB;
				return;
#else
				error_exit("error: The TinyBackend is not supported on this platform.");
#endif
			}
			if (match_longopt("symtab"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --symtab needs a valid integer.");
				const char *symtab_string = next_arg();
				int symtab = atoi(symtab_string);
				if (symtab < 1024) OUTPUT("Expected a valid positive integer >= 1024.");
				options->symtab_size = next_highest_power_of_2(symtab);
				return;
			}
			if (match_longopt("forcelinker"))
			{
				options->force_linker = true;
				return;
			}
			if (match_longopt("version"))
			{
				print_version();
				exit_compiler(COMPILER_SUCCESS_EXIT);
			}
			if ((argopt = match_argopt("x86vec")))
			{
				options->x86_vector_capability = (X86VectorCapability)parse_multi_option(argopt, 5, vector_capability);
				return;
			}
			if ((argopt = match_argopt("reloc")))
			{
				options->reloc_model = (RelocModel)parse_multi_option(argopt, 5, reloc_models);
				return;
			}
			if (match_longopt("about"))
			{
				OUTPUT("The C3 Compiler");
				OUTPUT("C3 is low level programming language based on C.");
				exit_compiler(COMPILER_SUCCESS_EXIT);
			}
			if (match_longopt("debug-log"))
			{
				debug_log = true;
				debug_stats = true;
				return;
			}
			if (match_longopt("debug-stats"))
			{
				debug_stats = true;
				return;
			}
			if (match_longopt("list-keywords"))
			{
				options->print_keywords = true;
				options->command = COMMAND_PRINT_SYNTAX;
				return;
			}
			if (match_longopt("list-attributes"))
			{
				options->print_attributes = true;
				options->command = COMMAND_PRINT_SYNTAX;
				return;
			}
			if (match_longopt("list-builtins"))
			{
				options->print_builtins = true;
				options->command = COMMAND_PRINT_SYNTAX;
				return;
			}
			if (match_longopt("list-operators"))
			{
				options->print_operators = true;
				options->command = COMMAND_PRINT_SYNTAX;
				return;
			}
			if (match_longopt("list-precedence"))
			{
				options->print_precedence = true;
				options->command = COMMAND_PRINT_SYNTAX;
				return;
			}
			if (match_longopt("threads"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --threads needs a valid integer 1 or higher.");
				const char *thread_string = next_arg();
				int threads = atoi(thread_string);
				if (threads < 1) OUTPUT("Expected a valid integer 1 or higher.");
				if (threads > MAX_THREADS) OUTPUT("Cannot exceed %d threads.", MAX_THREADS);
				options->build_threads = threads;
				return;
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
				exit_compiler(EXIT_FAILURE);
			}
			if (match_longopt("list-targets"))
			{
				print_all_targets();
				exit_compiler(COMPILER_SUCCESS_EXIT);
			}
			if (match_longopt("emit-llvm"))
			{
				options->emit_llvm = true;
				return;
			}
			if (match_longopt("emit-asm"))
			{
				options->emit_asm = true;
				return;
			}
			if (match_longopt("cc"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --cc needs a compiler name.");
				options->cc = next_arg();
				return;
			}
			if (match_longopt("stdlib"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --stdlib needs a directory.");
				options->std_lib_dir = check_dir(next_arg());
				return;
			}
			if (match_longopt("nostdlib"))
			{
				options->no_stdlib = true;
				return;
			}
			if (match_longopt("panicfn"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --panicfn needs a function name.");
				options->panicfn = next_arg();
				return;
			}
			if (match_longopt("macossdk"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --macossdk needs a directory.");
				options->macos.sdk = check_dir(next_arg());
				return;
			}
			if (match_longopt("winsdk"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --winsdk needs a directory.");
				options->win.sdk = check_dir(next_arg());
				return;
			}
			if ((argopt = match_argopt("wincrt")))
			{
				options->win.crt_linking = (WinCrtLinking)parse_multi_option(argopt, 3, wincrt_linking);
				return;
			}
			if (match_longopt("build-dir"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --build-dir needs a directory.");
				options->build_dir = next_arg();
				return;
			}
			if (match_longopt("obj-out"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --obj-out needs a directory.");
				options->obj_out = next_arg();
				return;
			}
			if (match_longopt("llvm-out"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --llvm-out needs a directory.");
				options->llvm_out = next_arg();
				return;
			}
			if (match_longopt("asm-out"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --asm-out needs a directory.");
				options->asm_out = next_arg();
				return;
			}
			if (match_longopt("lib"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --lib needs a name.");
				const char *name = next_arg();
				if (!str_is_valid_lowercase_name(name))
				{
					char *name_copy = strdup(name);
					str_ellide_in_place(name_copy, 32);
					error_exit("Invalid library name '%s', it should be something like 'foo_lib'.", name_copy);
				}
				options->libs[options->lib_count++] = name;
				return;
			}
			if (match_longopt("libdir"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --libdir needs a directory.");
				if (options->lib_dir_count == MAX_LIB_DIRS) error_exit("Max %d library directories may be specified.", MAX_LIB_DIRS);
				options->lib_dir[options->lib_dir_count++] = check_dir(next_arg());
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
			if (match_longopt("fast"))
			{
				options->safe_mode = 0;
				return;
			}
			if (match_longopt("help"))
			{
				usage();
				exit_compiler(COMPILER_SUCCESS_EXIT);
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
		exit_compiler(COMPILER_SUCCESS_EXIT);
	}

	BuildOptions build_options = {
		.path = ".",
		.emit_llvm = false,
		.emit_bitcode = true,
		.optimization_setting_override = OPT_SETTING_NOT_SET,
		.debug_info_override = DEBUG_INFO_NOT_SET,
		.safe_mode = -1,
		.build_threads = 16,
		.command = COMMAND_MISSING,
		.reloc_model = RELOC_DEFAULT,
		.backend = BACKEND_LLVM,
		.x86_vector_capability = X86VECTOR_DEFAULT,
		.win.crt_linking = WIN_CRT_DEFAULT,
		.files = NULL,
		.build_dir = NULL,

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
		if (command_is_projectless(build_options.command) || build_options.command == COMMAND_GENERATE_HEADERS)
		{
			append_file(&build_options);
			continue;
		}
		FAIL_WITH_ERR("Found the unexpected argument \"%s\".", current_arg);
	}
	return build_options;
}


ArchOsTarget arch_os_target_from_string(const char *target)
{
	for (unsigned i = 1; i <= ARCH_OS_TARGET_LAST; i++)
	{
		if (strcmp(arch_os_target[i], target) == 0)
		{
			return (ArchOsTarget)i;
		}
	}
	return ARCH_OS_TARGET_DEFAULT;
}
