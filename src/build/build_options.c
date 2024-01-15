// Copyright (c) 2019-2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "build_internal.h"
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
	[WINDOWS_AARCH64] = "windows-aarch64",
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
	OUTPUT("  compile <file1> [<file2> ...]           Compile files without a project into an executable.");
	OUTPUT("  init <project name>                     Initialize a new project structure.");
	OUTPUT("  init-lib <library name>                 Initialize a new library structure.");
	OUTPUT("  build [<target>]                        Build the target in the current project.");
	OUTPUT("  benchmark                               Run the benchmarks in the current project.");
	OUTPUT("  test                                    Run the unit tests in the current project.");
	OUTPUT("  clean                                   Clean all build files.");
	OUTPUT("  run [<target>]                          Run (and build if needed) the target in the current project.");
	OUTPUT("  dist [<target>]                         Clean and build a target for distribution.");
	OUTPUT("  directives [<target>]                   Generate documentation for the target.");
	OUTPUT("  bench [<target>]                        Benchmark a target.");
	OUTPUT("  clean-run [<target>]                    Clean, then run the target.");
	OUTPUT("  compile-run <file1> [<file2> ...]       Compile files then immediately run the result.");
	OUTPUT("  compile-only <file1> [<file2> ...]      Compile files but do not perform linking.");
	OUTPUT("  compile-benchmark <file1> [<file2> ...] Compile files into an executable and run benchmarks.");
	OUTPUT("  compile-test <file1> [<file2> ...]      Compile files into an executable and run unit tests.");
	OUTPUT("  static-lib <file1> [<file2> ...]        Compile files without a project into a static library.");
	OUTPUT("  dynamic-lib <file1> [<file2> ...]       Compile files without a project into a dynamic library.");
	OUTPUT("  headers <file1> [<file2> ...]           Analyse files and generate C headers for public methods.");
	OUTPUT("  vendor-fetch <library> ...              Fetches one or more libraries from the vendor collection.");
	OUTPUT("");
	OUTPUT("Options:");
	OUTPUT("  --tb                      - Use Tilde Backend for compilation.");
	OUTPUT("  --stdlib <dir>            - Use this directory as the C3 standard library path.");
	OUTPUT("  --no-entry                - Do not generate (or require) a main function.");
	OUTPUT("  --libdir <dir>            - Add this directory to the C3 library search paths.");
	OUTPUT("  --lib <name>              - Add this library to the compilation.");
	OUTPUT("  --path <dir>              - Use this as the base directory for the current command.");
	OUTPUT("  --template <template>     - Select template for 'init': \"exe\", \"static-lib\", \"dynamic-lib\" or a path.");
	OUTPUT("  --about                   - Prints a short description of C3.");
	OUTPUT("  --symtab <value>          - Sets the preferred symtab size.");
	OUTPUT("  -V --version              - Print version information.");
	OUTPUT("  -E                        - Lex only.");
	OUTPUT("  -P                        - Only parse and output the AST as JSON.");
	OUTPUT("  -C                        - Only lex, parse and check.");
	OUTPUT("  -                         - Read code from standard in.");
	OUTPUT("  -o <file>                 - Write output to <file>.");
	OUTPUT("  -O0                       - Safe, no optimizations, emit debug info.");
	OUTPUT("  -O1                       - Safe, high optimization, emit debug info.");
	OUTPUT("  -O2                       - Unsafe, high optimization, emit debug info.");
	OUTPUT("  -O3                       - Unsafe, high optimization, single module, emit debug info.");
	OUTPUT("  -O4                       - Unsafe, highest optimization, relaxed maths, single module, emit debug info.");
	OUTPUT("  -O5                       - Unsafe, highest optimization, fast maths, single module, emit debug info.");
	OUTPUT("  -Os                       - Unsafe, high optimization, small code, single module, no debug info.");
	OUTPUT("  -Oz                       - Unsafe, high optimization, tiny code, single module, no debug info.");
	OUTPUT("  -D <name>                 - Add feature flag <name>.");
	OUTPUT("  -U <name>                 - Remove feature flag <name>.");
	OUTPUT("  --trust=<option>          - Trust level: none (default), include ($include allowed), full ($exec / exec allowed).");
	OUTPUT("  --build-dir <dir>         - Override build output directory.");
	OUTPUT("  --obj-out <dir>           - Override object file output directory.");
	OUTPUT("  --script-dir <dir>        - Override the base directory for $exec.");
	OUTPUT("  --llvm-out <dir>          - Override llvm output directory for '--emit-llvm'.");
	OUTPUT("  --emit-llvm               - Emit LLVM IR as a .ll file per module.");
	OUTPUT("  --asm-out <dir>           - Override asm output directory for '--emit-asm'.");
	OUTPUT("  --emit-asm                - Emit asm as a .s file per module.");
	OUTPUT("  --obj                     - Emit object files. (Enabled by default)");
	OUTPUT("  --no-obj                  - Do not output object files, this is only valid for `compile-only`.");
	OUTPUT("  --target <target>         - Compile for a particular architecture + OS target.");
	OUTPUT("  --threads <number>        - Set the number of threads to use for compilation.");
	OUTPUT("  --safe=<yes|no>           - Turn safety (contracts, runtime bounds checking, null pointer checks etc) on or off.");
	OUTPUT("  --optlevel=<option>       - Code optimization level: none, less, more, max.");
	OUTPUT("  --optsize=<option>        - Code size optimization: none, small, tiny.");
	OUTPUT("  --single-module=<yes|no>  - Compile all modules together, enables more inlining.");
	OUTPUT("");
	OUTPUT("  -g                        - Emit debug info.");
	OUTPUT("  -g0                       - Emit no debug info.");
	OUTPUT("");
	OUTPUT("");
	OUTPUT("  -l <library>              - Link with the library provided.");
	OUTPUT("  -L <library dir>          - Append the directory to the linker search paths.");
	OUTPUT("  -z <argument>             - Send the <argument> as a parameter to the linker.");
	OUTPUT("  --system-linker=<yes|no>  - Use the system linker (default: no for cross compilation, yes otherwise).");
	OUTPUT("  --cc <path>               - Set C compiler (for C files in projects and use as system linker).");
	OUTPUT("");
	OUTPUT("  --use-stdlib=<yes|no>     - Include the standard library (default: yes).");
	OUTPUT("  --link-libc=<yes|no>      - Link libc other default libraries (default: yes).");
	OUTPUT("  --emit-stdlib=<yes|no>    - Output files for the standard library. (default: yes)");
	OUTPUT("  --panicfn <name>          - Override the panic function name.");
	OUTPUT("  --testfn <name>           - Override the test runner function name.");
	OUTPUT("  --benchfn <name>          - Override the benchmark runner function name.");
	OUTPUT("");
	OUTPUT("  --reloc=<option>          - Relocation model: none, pic, PIC, pie, PIE.");
	OUTPUT("  --x86cpu=<option>         - Set general level of x64 cpu: baseline, ssse3, sse4, avx1, avx2-v1, avx2-v2 (Skylake/Zen1+), avx512 (Icelake/Zen4+), native.");
	OUTPUT("  --x86vec=<option>         - Set max type of vector use: none, mmx, sse, avx, avx512, native.");
	OUTPUT("  --riscvfloat=<option>     - Set type of RISC-V float support: none, float, double");
	OUTPUT("  --memory-env=<option>     - Set the memory environment: normal, small, tiny, none.");
	OUTPUT("  --strip-unused=<yes|no>   - Strip unused code and globals from the output. (default: yes)");
	OUTPUT("  --fp-math=<option>        - FP math behaviour: strict, relaxed, fast.");
	OUTPUT("");
	OUTPUT("  --debug-stats             - Print debug statistics.");
	OUTPUT("  --print-linking           - Print linker arguments.");
#ifndef NDEBUG
	OUTPUT("  --debug-log               - Print debug logging to stdout.");
#endif
	OUTPUT("");
	OUTPUT("  --benchmarking            - Run built-in benchmarks.");
	OUTPUT("  --testing                 - Run built-in tests.");
	OUTPUT("");
	OUTPUT("  --list-attributes         - List all attributes.");
	OUTPUT("  --list-builtins           - List all builtins.");
	OUTPUT("  --list-keywords           - List all keywords.");
	OUTPUT("  --list-operators          - List all operators.");
	OUTPUT("  --list-precedence         - List operator precedence order.");
	OUTPUT("  --list-project-properties - List all available keys used in project.json files.");
	OUTPUT("  --list-targets            - List all architectures the compiler supports.");
	OUTPUT("  --list-type-properties    - List all type properties.");
	OUTPUT("");
	OUTPUT("  --print-output            - Print the object files created to stdout.");
	OUTPUT("");
	OUTPUT("  --winsdk <dir>            - Set the directory for Windows system library files for cross compilation.");
	OUTPUT("  --wincrt=<option>         - Windows CRT linking: none, static, dynamic (default).");
	OUTPUT("  --windef <file>           - Use Windows 'def' file for function exports instead of 'dllexport'.");
	OUTPUT("");
	OUTPUT("  --macossdk <dir>          - Set the directory for the MacOS SDK for cross compilation.");
	OUTPUT("  --macos-min-version <ver> - Set the minimum MacOS version to compile for.");
	OUTPUT("  --macos-sdk-version <ver> - Set the MacOS SDK compiled for.");
	OUTPUT("");
	OUTPUT("  --linux-crt <dir>         - Set the directory to use for finding crt1.o and related files.");
	OUTPUT("  --linux-crtbegin <dir>    - Set the directory to use for finding crtbegin.o and related files.");
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

INLINE bool match_longopt(const char* name)
{
	return str_eq(&current_arg[2], name);
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
	if (arg_match("init-lib"))
	{
		options->command = COMMAND_INIT_LIB;
		if (at_end() || next_is_opt()) error_exit("Expected a library name after init");
		options->project_name = next_arg();
		return;
	}
	if (arg_match("utest"))
	{
		options->command = COMMAND_UNIT_TEST;
		return;
	}
	if (arg_match("compile-benchmark"))
	{
		options->command = COMMAND_COMPILE_BENCHMARK;
		options->benchmarking = true;
		return;
	}
	if (arg_match("compile-test"))
	{
		options->command = COMMAND_COMPILE_TEST;
		options->testing = true;
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
	if (arg_match("vendor-fetch"))
	{
		options->command = COMMAND_VENDOR_FETCH;
		if (at_end() || next_is_opt()) error_exit("error: vendor-fetch needs at least one library.");
		while (!at_end() && !next_is_opt())
		{
			const char *lib = next_arg();
			vec_add(options->libraries_to_fetch, lib);
		}
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
	if (arg_match("benchmark"))
	{
		options->command = COMMAND_BENCHMARK;
		options->benchmarking = true;
		return;
	}
	if (arg_match("test"))
	{
		options->command = COMMAND_TEST;
		options->testing = true;
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
	OUTPUT("C3 Compiler Version (alpha):       %s", COMPILER_VERSION);
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

/**
 * Update feature flags, adding to one list and removing it from the other.
 * @param flags the "add" flags
 * @param removed_flags the "undef" flags
 * @param arg the argument to add or undef
 * @param add true if we add, false to undef
 */
void update_feature_flags(const char ***flags, const char ***removed_flags, const char *arg, bool add)
{
	// We keep two lists "remove" and "add" lists:
	const char ***to_remove_from = add ? removed_flags : flags;

	// Remove from opposite list using string equality
	// More elegant would be using a Set or Map, but that's overkill
	// for something that's likely just 1-2 values.
	FOREACH_BEGIN_IDX(i, const char *value, *to_remove_from)
		if (str_eq(value, arg))
		{
			vec_erase_ptr_at(*to_remove_from, i);
			break;
		}
	FOREACH_END();

	// First we check that it's not in the list
	const char ***to_add_to_ref = add ? flags : removed_flags;
	FOREACH_BEGIN(const char *value, *to_add_to_ref)
		// If we have a match, we don't add it.
		if (str_eq(value, arg)) return;
	FOREACH_END();

	// No match, so add it.
	vec_add(*to_add_to_ref, arg);
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
		case '\0':
			options->read_stdin = true;
			return;
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
			if (match_shortopt("z"))
			{
				if (at_end()) error_exit("error: -z needs a value.");
				add_linker_arg(options, next_arg());
				return;
			}
			break;
		case 'o':
			if (match_shortopt("o"))
			{
				if (at_end()) error_exit("error: -o needs a name.");
				options->output_name = next_arg();
				return;
			}
			break;
		case 't':
			if (match_shortopt("t1"))
			{
				options->trust_level = TRUST_NONE;
				return;
			}
			if (match_shortopt("t2"))
			{
				options->trust_level = TRUST_INCLUDE;
				return;
			}
			if (match_shortopt("t3"))
			{
				options->trust_level = TRUST_FULL;
				return;
			}
			break;
		case 'D':
			if (match_shortopt("D"))
			{
				if (at_end()) error_exit("error: -D needs a feature name.");
				const char *arg = next_arg();
				if (!str_is_valid_constant(arg)) error_exit("Invalid feature name '%s', expected an all-uppercase constant name.", arg);
				update_feature_flags(&options->feature_names, &options->removed_feature_names, arg, true);
				return;
			}
			break;
		case 'U':
			if (match_shortopt("U"))
			{
				if (at_end()) error_exit("error: -U needs a feature name.");
				const char *arg = next_arg();
				if (!str_is_valid_constant(arg)) error_exit("Invalid feature name '%s', expected an all-uppercase constant name.", arg);
				update_feature_flags(&options->feature_names, &options->removed_feature_names, arg, false);
				return;
			}
			break;
		case 'O':
			if (match_shortopt("O0"))
			{
				options->optsetting = OPT_SETTING_O0;
			}
			else if (match_shortopt("O1"))
			{
				options->optsetting = OPT_SETTING_O1;
			}
			else if (match_shortopt("O2"))
			{
				options->optsetting = OPT_SETTING_O2;
			}
			else if (match_shortopt("O3"))
			{
				options->optsetting = OPT_SETTING_O3;
			}
			else if (match_shortopt("O4"))
			{
				options->optsetting = OPT_SETTING_O4;
			}
			else if (match_shortopt("O5"))
			{
				options->optsetting = OPT_SETTING_O5;
			}
			else if (match_shortopt("Os"))
			{
				options->optsetting = OPT_SETTING_OSMALL;
			}
			else if (match_shortopt("Oz"))
			{
				options->optsetting = OPT_SETTING_OTINY;
			}
			else
			{
				FAIL_WITH_ERR("Invalid optimization level, expected O0 - O5, Os or Oz.");
			}
			return;
		case 'E':
			if (match_shortopt("E"))
			{
				if (options->compile_option != COMPILE_NORMAL)
				{
					FAIL_WITH_ERR("Illegal combination of compile options.");
				}
				options->compile_option = COMPILE_LEX_ONLY;
				return;
			}
			break;
		case 'L':
			if (match_shortopt("L"))
			{
				if (at_end() || next_is_opt()) error_exit("error: -L needs a directory.");
				options->linker_lib_dir[options->linker_lib_dir_count++] = check_dir(next_arg());
				return;
			}
			break;
		case 'l':
			if (match_shortopt("l"))
			{
				if (at_end() || next_is_opt()) error_exit("error: -l needs a library name.");
				options->linker_libs[options->linker_lib_count++] = next_arg();
				return;
			}
			break;
		case 'P':
			if (match_shortopt("P"))
			{
				if (options->compile_option != COMPILE_NORMAL)
				{
					FAIL_WITH_ERR("Illegal combination of compile options.");
				}
				options->compile_option = COMPILE_LEX_PARSE_ONLY;
				return;
			}
			break;
		case 'C':
			if (match_shortopt("C"))
			{
				if (options->compile_option != COMPILE_NORMAL)
				{
					FAIL_WITH_ERR("Illegal combination of compile options.");
				}
				options->compile_option = COMPILE_LEX_PARSE_CHECK_ONLY;
				return;
			}
			break;
		case '-':
			if (match_longopt("tb"))
			{
				options->backend = BACKEND_TB;
				return;
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
			if (match_longopt("version"))
			{
				print_version();
				exit_compiler(COMPILER_SUCCESS_EXIT);
			}
			if ((argopt = match_argopt("fp-math")))
			{
				options->fp_math = (FpOpt)parse_multi_option(argopt, 3, fp_math);
				return;
			}
			if ((argopt = match_argopt("optsize")))
			{
				options->optsize = (SizeOptimizationLevel)parse_multi_option(argopt, 3, optsizes);
				return;
			}
			if ((argopt = match_argopt("optlevel")))
			{
				options->optlevel = (OptimizationLevel)parse_multi_option(argopt, 4, optlevels);
				return;
			}
			if ((argopt = match_argopt("safe")))
			{
				options->safety_level = (SafetyLevel)parse_multi_option(argopt, 2, on_off);
				return;
			}
			if ((argopt = match_argopt("single-module")))
			{
				options->single_module = (SingleModule)parse_multi_option(argopt, 2, on_off);
				return;
			}
			if ((argopt = match_argopt("system-linker")))
			{
				options->system_linker = (SystemLinker)parse_multi_option(argopt, 2, on_off);
				return;
			}
			if ((argopt = match_argopt("link-libc")))
			{
				options->link_libc = (LinkLibc)parse_multi_option(argopt, 2, on_off);
				return;
			}
			if ((argopt = match_argopt("strip-unused")))
			{
				options->strip_unused = (StripUnused)parse_multi_option(argopt, 2, on_off);
				return;
			}
			if ((argopt = match_argopt("emit-stdlib")))
			{
				options->emit_stdlib = (EmitStdlib)parse_multi_option(argopt, 2, on_off);
				return;
			}
			if ((argopt = match_argopt("use-stdlib")))
			{
				options->use_stdlib = (UseStdlib)parse_multi_option(argopt, 2, on_off);
				return;
			}
			if ((argopt = match_argopt("x86vec")))
			{
				options->x86_vector_capability = (X86VectorCapability)parse_multi_option(argopt, 6, x86_vector_capability);
				return;
			}
			if ((argopt = match_argopt("x86cpu")))
			{
				options->x86_cpu_set = (X86CpuSet)parse_multi_option(argopt, 8, x86_cpu_set);
				return;
			}
			if ((argopt = match_argopt("riscvfloat")))
			{
				options->riscv_float_capability = (RiscvFloatCapability)parse_multi_option(argopt, 3, riscv_capability);
				return;
			}
			if ((argopt = match_argopt("memory-env")))
			{
				options->memory_environment = (MemoryEnvironment )parse_multi_option(argopt, 4, memory_environment);
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
			if (match_longopt("no-obj"))
			{
				options->no_obj = true;
				return;
			}
			if (match_longopt("obj"))
			{
				options->no_obj = false;
				return;
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
			if (match_longopt("print-linking"))
			{
				options->print_linking = true;
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
			if (match_longopt("list-type-properties"))
			{
				options->print_type_properties = true;
				options->command = COMMAND_PRINT_SYNTAX;
				return;
			}
			if (match_longopt("list-project-properties"))
			{
				options->print_project_properties = true;
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
			if (match_longopt("print-output"))
			{
				options->print_output = true;
				return;
			}
			if (match_longopt("no-entry"))
			{
				options->no_entry = true;
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
				options->emit_stdlib = EMIT_STDLIB_ON;
				return;
			}
			if (match_longopt("panicfn"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --panicfn needs a function name.");
				options->panicfn = next_arg();
				return;
			}
			if (match_longopt("testfn"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --testfn needs a function name.");
				options->testfn = next_arg();
				return;
			}
			if (match_longopt("benchfn"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --benchfn needs a function name.");
				options->benchfn = next_arg();
				return;
			}
			if (match_longopt("macossdk"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --macossdk needs a directory.");
				options->macos.sysroot = check_dir(next_arg());
				return;
			}
			if (match_longopt("winsdk"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --winsdk needs a directory.");
				options->win.sdk = check_dir(next_arg());
				return;
			}
			if ((argopt = match_argopt("trust")))
			{
				options->trust_level = (TrustLevel) parse_multi_option(argopt, 3, trust_level);
				return;
			}
			if (match_longopt("windef"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --windef needs a file.");
				options->win.def = next_arg();
				return;
			}
			if ((argopt = match_argopt("wincrt")))
			{
				options->win.crt_linking = (WinCrtLinking)parse_multi_option(argopt, 3, wincrt_linking);
				return;
			}
			if (match_longopt("macos-sdk-version"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --macos-sdk-version needs a version.");
				options->macos.sdk_version = next_arg();
				return;
			}
			if (match_longopt("macos-min-version"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --macos-min-version needs a version.");
				options->macos.min_version = next_arg();
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
			if (match_longopt("script-dir"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --script-dir needs a directory.");
				options->script_dir = next_arg();
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
			if (match_longopt("benchmark"))
			{
				options->benchmark_mode = true;
				return;
			}
			if (match_longopt("test"))
			{
				options->test_mode = true;
				options->strip_unused = STRIP_UNUSED_OFF;
				return;
			}
			if (match_longopt("template"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --template needs an argument.");
				options->template = next_arg();
				return;
			}
			if (match_longopt("path"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --path needs a directory.");
				options->path = check_dir(next_arg());
				return;
			}
			if (match_longopt("linux-crt"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --linux-crt needs a directory.");
				options->linuxpaths.crt = check_dir(next_arg());
				return;
			}
			if (match_longopt("linux-crtbegin"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --linux-crtbegin needs a directory.");
				options->linuxpaths.crtbegin = check_dir(next_arg());
				return;
			}
			if (match_longopt("benchmarking"))
			{
				options->benchmarking = true;
				return;
			}
			if (match_longopt("testing"))
			{
				options->testing = true;
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
		.optsetting = OPT_SETTING_NOT_SET,
		.debug_info_override = DEBUG_INFO_NOT_SET,
		.safety_level = SAFETY_NOT_SET,
		.optlevel = OPTIMIZATION_NOT_SET,
		.optsize = SIZE_OPTIMIZATION_NOT_SET,
		.build_threads = cpus(),
		.command = COMMAND_MISSING,
		.reloc_model = RELOC_DEFAULT,
		.backend = BACKEND_LLVM,
		.x86_vector_capability = X86VECTOR_DEFAULT,
		.fp_math = FP_DEFAULT,
		.x86_cpu_set = X86CPU_DEFAULT,
		.riscv_float_capability = RISCVFLOAT_DEFAULT,
		.memory_environment = MEMORY_ENV_NOT_SET,
		.win.crt_linking = WIN_CRT_DEFAULT,
		.emit_stdlib = EMIT_STDLIB_NOT_SET,
		.link_libc = LINK_LIBC_NOT_SET,
		.use_stdlib = USE_STDLIB_NOT_SET,
		.system_linker = SYSTEM_LINKER_NOT_SET,
		.strip_unused = STRIP_UNUSED_NOT_SET,
		.single_module = SINGLE_MODULE_NOT_SET,
		.files = NULL,
		.build_dir = NULL,
		.script_dir = NULL,

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
		if (command_accepts_files(build_options.command) || build_options.command == COMMAND_GENERATE_HEADERS)
		{
			append_file(&build_options);
			continue;
		}
		FAIL_WITH_ERR("Found the unexpected argument \"%s\".", current_arg);
	}
	if (build_options.command == COMMAND_MISSING)
	{
		FAIL_WITH_ERR("Missing a compiler command such as 'compile' or 'build'.");
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
