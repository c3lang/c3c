// Copyright (c) 2019-2025 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "../utils/whereami.h"
#include "build.h"
#include "build_internal.h"
#include "git_hash.h"

extern int llvm_version_major;
bool silence_deprecation;

static int arg_index;
static int arg_count;
static const char **args;
static const char *current_arg;
extern const char *llvm_version;
extern const char *llvm_target;

static const char *unchecked_dir(BuildOptions *options, const char *path);
static inline bool at_end();
static inline const char *next_arg();
static inline bool next_is_opt();
INLINE bool match_longopt(const char *name);
static inline const char *match_argopt(const char *name);
static inline bool match_shortopt(const char *name);
void append_file(BuildOptions *build_options);
static inline const char *match_argopt(const char *name);
void append_arg(BuildOptions *build_options);
static bool arg_match(const char *candidate);
static void parse_optional_target(BuildOptions *options);
static void add_linker_arg(BuildOptions *options, const char *arg);
static void update_feature_flags(const char ***flags, const char ***removed_flags, const char *arg, bool add);
static void print_all_targets(void);
static int parse_option_select(const char *start, unsigned count, const char **elements);
static void print_cmd(const char *command, const char *desc);
static void print_opt(const char *option, const char *desc);

#define parse_opt_select(type_, start_, elements_) (type_)parse_option_select(start_, ELEMENTLEN(elements_), elements_)

const char *arch_os_target[ARCH_OS_TARGET_LAST + 1];

#define EOUTPUT(string, ...) fprintf(stderr, string "\n", ##__VA_ARGS__) // NOLINT
#define PRINTF(string, ...) fprintf(stdout, string "\n", ##__VA_ARGS__) // NOLINT
#define FAIL_WITH_ERR(string, ...) do { fprintf(stderr, "Error: " string "\n\n", ##__VA_ARGS__); usage(false); exit_compiler(EXIT_FAILURE); } while (0) /* NOLINT */
#define FAIL_WITH_ERR_LONG(string, ...) do { fprintf(stderr, "Error: " string "\n\n", ##__VA_ARGS__); usage(true); exit_compiler(EXIT_FAILURE); } while (0) /* NOLINT */
#define PROJECT_FAIL_WITH_ERR(string, ...) do { fprintf(stderr, "Error: " string "\n\n", ##__VA_ARGS__); project_usage(); exit_compiler(EXIT_FAILURE); } while (0) /* NOLINT */

static void usage(bool full)
{
	PRINTF("Usage: %s [<options>] <command> [<args>]", args[0]);
	PRINTF("");
	PRINTF("Commands:");
	PRINTF("");
	print_cmd("compile <file1> [<file2> ...]", "Compile files without a project into an executable.");
	print_cmd("init <project name>", "Initialize a new project structure.");
	print_cmd("init-lib <library name>", "Initialize a new library structure.");
	print_cmd("build [<target>]", "Build the target in the current project.");
	print_cmd("benchmark [<target>]", "Run the benchmarks for the target in the current project.");
	print_cmd("test [<target>]", "Run the unit tests for the target in the current project.");
	print_cmd("clean", "Clean all build files.");
	print_cmd("run [<target>] [-- [<arg1> ...]] ", "Run (and build if needed) the target in the current project.");
	print_cmd("dist [<target>]", "Clean and build a target for distribution.");
	print_cmd("clean-run [<target>] [-- [<arg1> ...]]", "Clean, then run the target.");
	print_cmd("compile-run <file1> [<file2> ...] [-- [<arg1> ...]]", "Compile files then immediately run the result.");
	print_cmd("compile-only <file1> [<file2> ...]", "Compile files but do not perform linking.");
	print_cmd("compile-benchmark <file1> [<file2> ...]", "Compile files into a test-executable and run benchmarks.");
	print_cmd("compile-test <file1> [<file2> ...]", "Compile files into a benchmark-executable and run unit tests.");
	print_cmd("static-lib <file1> [<file2> ...]", "Compile files without a project into a static library.");
	print_cmd("dynamic-lib <file1> [<file2> ...]", "Compile files without a project into a dynamic library.");
	print_cmd("vendor-fetch <library> ...", "Fetches one or more libraries from the vendor collection.");
	print_cmd("project <subcommand> ...", "Manipulate or view project files.");
	PRINTF("");
	full ? PRINTF("Options:") : PRINTF("Common options:");
	print_opt("-h -hh --help", "Print the help, -h for the normal options, -hh for the full help.");
	print_opt("-V --version", "Print version information.");
	print_opt("-q --quiet", "Silence unnecessary output.");
	print_opt("-v -vv -vvv", "Verbose output, -v for default, -vv and -vvv gives more information.");
	print_opt("-E", "Lex only.");
	print_opt("-P", "Only parse and output the AST as JSON.");
	print_opt("-C", "Only lex, parse and check.");
	print_opt("-", "Read code from standard in.");
	print_opt("-o <file>", "Write output to <file>.");
	print_opt("-O0", "Safe, no optimizations, emit debug info.");
	print_opt("-O1", "Safe, high optimization, emit debug info.");
	print_opt("-O2", "Unsafe, high optimization, emit debug info.");
	print_opt("-O3", "Unsafe, high optimization, single module, emit debug info.");
	print_opt("-O4", "Unsafe, highest optimization, relaxed maths, single module, emit debug info, no panic messages.");
	print_opt("-O5", "Unsafe, highest optimization, fast maths, single module, emit debug info, no panic messages, no backtrace.");
	print_opt("-Os", "Unsafe, high optimization, small code, single module, no debug info, no panic messages.");
	print_opt("-Oz", "Unsafe, high optimization, tiny code, single module, no debug info, no panic messages, no backtrace.");
	print_opt("-D <name>", "Add feature flag <name>.");
	print_opt("-U <name>", "Remove feature flag <name>.");
	PRINTF("");
	print_opt("--about", "Prints a short description of C3.");
	print_opt("--build-env", "Prints build environment information (only valid with in combination with a command such as 'compile').");
	print_opt("--run-dir <dir>", "Set the directory from where to run the binary (only for run and compile-run).");
	print_opt("--libdir <dir>", "Add this directory to the c3l library search paths.");
	print_opt("--lib <name>", "Add this c3l library to the compilation.");
	print_opt("--sources <file1> [<file2> ...]", "Add these additional sources to the compilation.");
	if (full)
	{
		print_opt("--validation=<option>", "Strictness of code validation: lenient (default), strict, obnoxious (very strict)");
		print_opt("--stdlib <dir>", "Use this directory as the C3 standard library path.");
		print_opt("--no-entry", "Do not generate (or require) a main function.");
		print_opt("--path <dir>", "Use this as the base directory for the current command.");
		print_opt("--template <template>", "Select template for 'init': \"exe\", \"static-lib\", \"dynamic-lib\" or a path.");
		print_opt("--symtab <value>", "Sets the preferred symtab size.");
		print_opt("--max-mem <value>", "Set the maximum memory size (in MB).");
		print_opt("--run-once", "After running the output file, delete it immediately.");
		print_opt("--suppress-run", "Build but do not run on test/benchmark options.");
		print_opt("--trust=<option>", "Trust level: none (default), include ($include allowed), full ($exec / exec allowed).");
		print_opt("--output-dir <dir>", "Override general output directory.");
		print_opt("--build-dir <dir>", "Override build output directory.");
		print_opt("--obj-out <dir>", "Override object file output directory.");
		print_opt("--script-dir <dir>", "Override the base directory for $exec.");
		print_opt("--llvm-out <dir>", "Override llvm output directory for '--emit-llvm'.");
		print_opt("--asm-out <dir> ", "Override asm output directory for '--emit-asm'.");
		print_opt("--header-output <dir>", "Override header file output directory when building libraries.");
		print_opt("--emit-llvm", "Emit LLVM IR as a .ll file per module.");
		print_opt("--emit-asm", "Emit asm as a .s file per module.");
		print_opt("--obj", "Emit object files. (Enabled by default)");
		print_opt("--no-obj", "Do not output object files, this is only valid for `compile-only`.");
		print_opt("--no-headers", "Do not generate C headers when building a library.");
		print_opt("--target <target>", "Compile for a particular architecture + OS target.");
		print_opt("--threads <number>", "Set the number of threads to use for compilation.");
		print_opt("--safe=<yes|no>", "Turn safety (contracts, runtime bounds checking, null pointer checks etc) on or off.");
		print_opt("--panic-msg=<yes|no>", "Turn panic message output on or off.");
		print_opt("--optlevel=<option>", "Code optimization level: none, less, more, max.");
		print_opt("--optsize=<option>", "Code size optimization: none, small, tiny.");
		print_opt("--unroll-loops=<yes|no>", "Enable loop unrolling.");
		print_opt("--loop-vectorize=<yes|no>", "Enable loop auto-vectorization.");
		print_opt("--slp-vectorize=<yes|no>", "Enable SLP (superword-level parallelism) auto-vectorization.");
		print_opt("--merge-functions=<yes|no>", "Enable function merging.");
		print_opt("--single-module=<yes|no>", "Compile all modules together, enables more inlining.");
		print_opt("--show-backtrace=<yes|no>", "Show detailed backtrace on segfaults.");
		print_opt("--lsp", "Emit data about errors suitable for a LSP.");
		print_opt("--use-old-slice-copy", "Use the old slice copy semantics.");
		print_opt("--use-old-enums", "Use the old enum syntax and semantics.");
		print_opt("--use-old-compact-eq", "Enable the old ability to use '@compact' to make a struct comparable.");
	}
	PRINTF("");
	print_opt("-g", "Emit debug info.");
	print_opt("-g0", "Emit no debug info.");
	if (full)
	{
		PRINTF("");
		print_opt("--ansi=<yes|no>", "Set colour output using ansi on/off, default is to try to detect it.");
		print_opt("--echo-prefix <arg>", "Sets the prefix for any output using '$echo', defaults to 'c3c:'.");
		print_opt("--test-filter <arg>", "Set a filter when running tests, running only matching tests.");
		print_opt("--test-breakpoint", "When running tests, trigger a breakpoint on failure.");
		print_opt("--test-nosort", "Do not sort tests.");
		print_opt("--test-noleak", "Disable tracking allocator and memory leak detection for tests.");
		print_opt("--test-show-output", "Disable test stdout capturing, all tests can print as they run.");
		print_opt("--test-nocapture", "Disable test stdout capturing, all tests can print as they run, same as --test-show-output.");
		print_opt("--test-quiet", "Run tests without printing full names, printing output only on failure");
		print_opt("--test-log-level=<verbose|debug|info|warn|error|critical>", "Set log priority when running tests.");
	}
	PRINTF("");
	print_opt("-l <library>", "Link with the static or dynamic library provided.");
	print_opt("-L <library dir>", "Append the directory to the linker search paths.");
	print_opt("-z <argument>", "Send the <argument> as a parameter to the linker.");
	if (full)
	{
		print_opt("--cc <path>", "Set C compiler (for C files in projects and use as system linker).");
		print_opt("--linker=<option> [<path>]", "Linker: builtin, cc, custom (default is 'cc'), 'custom' requires a path.");
		PRINTF("");
		print_opt("--use-stdlib=<yes|no>", "Include the standard library (default: yes).");
		print_opt("--link-libc=<yes|no>", "Link libc other default libraries (default: yes).");
		print_opt("--emit-stdlib=<yes|no>", "Output files for the standard library. (default: yes)");
		print_opt("--emit-only <file>", "Output only the file matching <file>.");
		print_opt("--panicfn <name>", "Override the panic function name.");
		print_opt("--testfn <name>", "Override the test runner function name.");
		print_opt("--benchfn <name>", "Override the benchmark runner function name.");
		PRINTF("");
		print_opt("--reloc=<option>", "Relocation model: none, pic, PIC, pie, PIE.");
		print_opt("--cpu-flags <string>", "Add/remove cpu flags fromt the default, e.g. '+a,-b'.");
		print_opt("--x86cpu=<option>", "Set general level of x64 cpu: baseline, ssse3, sse4, avx1, avx2-v1, avx2-v2 (Skylake/Zen1+), avx512 (Icelake/Zen4+), native.");
		print_opt("--x86vec=<option>", "Set max type of vector use: none, mmx, sse, avx, avx512, default.");
		print_opt("--riscv-abi=<option>", "Set type of RISC-V ABI: int-only, float, double.");
		print_opt("--riscv-cpu=<option>", "Set the general level of RISC-V cpu: rvi (default 32-bit) , rvimac, rvimafc, rvgc (default 64-bit), rvgcv.");
		print_opt("--memory-env=<option>", "Set the memory environment: normal, small, tiny, none.");
		print_opt("--strip-unused=<yes|no>", "Strip unused code and globals from the output. (default: yes)");
		print_opt("--fp-math=<option>", "FP math behaviour: strict, relaxed, fast.");
		print_opt("--win64-simd=<option>", "Win64 SIMD ABI: array, full.");
		print_opt("--win-debug=<option>", "Select debug output on Windows: codeview or dwarf (default: codeview).");
		print_opt("--max-vector-size <number>", "Set the maximum vector bit size to allow (default: 4096).");
		print_opt("--max-stack-object-size <number>", "Set the maximum size of a stack object in KB (default: 128).");
		print_opt("--max-macro-iterations <number>", "Set the maximum number of iterations in a macro loop (default: 1048575).");
		PRINTF("");
		print_opt("--print-linking", "Print linker arguments.");
		PRINTF("");
		print_opt("--benchmarking", "Run built-in benchmarks.");
		print_opt("--testing", "Run built-in tests.");
		PRINTF("");
		print_opt("--list-attributes", "List all attributes.");
		print_opt("--list-builtins", "List all builtins.");
		print_opt("--list-keywords", "List all keywords.");
		print_opt("--list-operators", "List all operators.");
		print_opt("--list-precedence", "List operator precedence order.");
		print_opt("--list-project-properties", "List all available keys used in project.json files.");
		print_opt("--list-manifest-properties", "List all available keys used in manifest.json files.");
		print_opt("--list-targets", "List all architectures the compiler supports.");
		print_opt("--list-type-properties", "List all type properties.");
		print_opt("--list-asm", "List all asm instructions for the current target.");
		PRINTF("");
		print_opt("--print-output", "Print the object files created to stdout.");
		print_opt("--print-input", "Print inputted C3 files to stdout.");
		PRINTF("");
		print_opt("--winsdk <dir>", "Set the directory for Windows system library files for cross compilation.");
		print_opt("--wincrt=<option>", "Windows CRT linking: none, static-debug, static, dynamic-debug (default if debug info enabled), dynamic (default).");
		print_opt("--windef <file>", "Use Windows 'def' file for function exports instead of 'dllexport'.");
		print_opt("--win-vs-dirs <dir>;<dir>", "Override Windows VS detection.");
		PRINTF("");
		print_opt("--macossdk <dir>", "Set the directory for the MacOS SDK for cross compilation.");
		print_opt("--macos-min-version <ver>", "Set the minimum MacOS version to compile for.");
		print_opt("--macos-sdk-version <ver>", "Set the MacOS SDK compiled for.");
		PRINTF("");
		print_opt("--linux-libc=<host|gnu|musl>", "Set the libc to use on Linux, defaults to host.");
		print_opt("--linux-crt <dir>", "Set the directory to use for finding crt1.o and related files.");
		print_opt("--linux-crtbegin <dir>", "Set the directory to use for finding crtbegin.o and related files.");
		PRINTF("");
		print_opt("--android-ndk <dir>", "Set the NDK directory location.");
		print_opt("--android-api <ver>", "Set Android API version.");
		PRINTF("");
		print_opt("--sanitize=<option>", "Enable sanitizer: address, memory, thread.");
	}
	if (!full)
	{
		PRINTF("");
		PRINTF("Use --help or -hh to view the full list of options.");
	}
}

static void project_usage()
{
	PRINTF("Usage: %s [<options>] project <subcommand> [<args>]", args[0]);
	PRINTF("");
	PRINTF("Project Subcommands:");
	print_cmd("view", "view the current projects structure.");
	print_cmd("add-target <name>  <target_type>  [sources...]", "add a new target to the project.");
	#if FETCH_AVAILABLE
		print_cmd("fetch", "fetch missing project libraries.");
	#endif
}

static void project_view_usage()
{
	PRINTF("Usage: %s [<options>] project view [<options>]", args[0]);
	PRINTF("");
	PRINTF("View the content of project.json in a more readable format.");
	PRINTF("Without flags, this prints out all the project properties.");
	PRINTF("");
	PRINTF("Flags can be used to filter for specific properties.");
	PRINTF("When one or more flags are used, the other properties do not get shown");
	PRINTF("");
	PRINTF("The verbosity of the output can be controlled with -v and -vv");
	PRINTF("Verbosity has no effect without flags. When -vv is used with flags,");
	PRINTF("the results will be printed out like they are in the full view.");
	PRINTF("Otherwise the \"<header>: \" is left out.");
	PRINTF("");
	PRINTF("With flags on, each selected property will be separated by an empty");
	PRINTF("line, and properties with multiple values (like --authors) will have");
	PRINTF("their values printed each on a new line.");
	PRINTF("");
	PRINTF("View options:");
	print_opt("-h --help", "Show this help.");
	print_opt("-v -vv", "Normal or very verbose output.");
	print_opt("--authors", "List of authors.");
	print_opt("--version", "Project version.");
	print_opt("--language-revision", "Project language revision.");
	print_opt("--warnings-used", "List of enabled compiler-warnings.");
	print_opt("--c3l-lib-search-paths", "List of C3 library (.c3l) search paths.");
	print_opt("--c3l-lib-dependencies", "List of C3 library (.c3l) dependencies.");
	print_opt("--source-paths", "List of C3 source file paths.");
	print_opt("--output-location", "Output directory.");
	print_opt("--default-optimization", "Default optimization level.");
	print_opt("--targets", "Project targets (!= compilation-targets)-");
}

static void parse_project_view_subcommand(BuildOptions *options)
{
	if (at_end() || !next_is_opt()) return;

	while (!at_end())
	{
		next_arg();
		current_arg = args[arg_index];

		if (current_arg[0] != '-')
		{
			FAIL_WITH_ERR("'project view' does not accept arguments, only flags. Failed on: %s.", current_arg);
		}

		if (match_shortopt("v"))
		{
			options->project_options.view_modifier.verbose = false;
			continue;
		}
		if (match_shortopt("vv"))
		{
			options->project_options.view_modifier.verbose = true;
			continue;
		}
		if (match_longopt("help") || match_shortopt("h"))
		{
			project_view_usage();
			exit_compiler(COMPILER_SUCCESS_EXIT);
		}
		int flag = parse_option_select(current_arg + 2, 10, project_view_flags);
		options->project_options.view_modifier.flags_bitvector |= 1 << flag;
	}
}

static void parse_project_subcommand(BuildOptions *options)
{
	if (match_longopt("help") || match_shortopt("h"))
	{
		project_usage();
		exit_compiler(COMPILER_SUCCESS_EXIT);
	}
	if (arg_match("view"))
	{
		options->project_options.command = SUBCOMMAND_VIEW;
		options->project_options.view_modifier.flags_bitvector = 0;
		options->project_options.view_modifier.verbose = false;
		parse_project_view_subcommand(options);
		return;
	}
	if (arg_match("add-target"))
	{
		options->project_options.command = SUBCOMMAND_ADD;

		if (at_end() || next_is_opt()) error_exit("Expected a target name");
		options->project_options.target_name = next_arg();

		if (at_end() || next_is_opt()) error_exit("Expected a target type like 'executable' or 'static-lib'");
		options->project_options.target_type = (TargetType)get_valid_enum_from_string(next_arg(), "type", targets, ELEMENTLEN(targets), "a target type like 'executable' or 'static-lib'");

		while (!at_end() && !next_is_opt())
		{
			vec_add(options->project_options.sources, next_arg());
		}

		return;
	}
	if (arg_match("fetch"))
	{
		options->project_options.command = SUBCOMMAND_FETCH;
		return;
	}

	PROJECT_FAIL_WITH_ERR("Cannot process the unknown subcommand \"%s\".", current_arg);
}

static void parse_project_options(BuildOptions *options)
{
	options->project_options.command = SUBCOMMAND_MISSING;
	if (at_end())
	{
		project_usage();
		return;
	}
	next_arg();
	parse_project_subcommand(options);
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
	if (arg_match("bench"))
	{
		options->command = COMMAND_BENCH;
		parse_optional_target(options);
		return;
	}
	if (arg_match("project"))
	{
		options->command = COMMAND_PROJECT;
		parse_project_options(options);
		return;
	}
	FAIL_WITH_ERR("Cannot process the unknown command \"%s\".", current_arg);
}


static void print_version(void)
{
	static const char *BUILD_DATE = __DATE__;
	static const char *BUILD_TIME = __TIME__;
#if	PRERELEASE
	PRINTF("C3 Compiler Version:       %s (Pre-release, %s %s)", COMPILER_VERSION, BUILD_DATE, BUILD_TIME);
#else
	PRINTF("C3 Compiler Version:       %s", COMPILER_VERSION);
#endif
	PRINTF("Installed directory:       %s", find_executable_path());
	PRINTF("Git Hash:                  %s", GIT_HASH);

#if LLVM_AVAILABLE && TB_AVAILABLE
    PRINTF("Backends:                  LLVM; TB");
#elif LLVM_AVAILABLE
    PRINTF("Backends:                  LLVM");
#elif TB_AVAILABLE
    PRINTF("Backends:                  TB");
#else

    PRINTF("No backends available");
#endif

#if LLVM_AVAILABLE
	PRINTF("LLVM version:              %s", llvm_version);
	PRINTF("LLVM default target:       %s", llvm_target);
#endif
}

static void parse_option(BuildOptions *options)
{
	const char *argopt;
	switch (current_arg[1])
	{
		case '\0':
			options->read_stdin = true;
			return;
		case 'h':
			if (match_shortopt("hh"))
			{
				usage(true);
				exit_compiler(COMPILER_SUCCESS_EXIT);
			}
			if (match_shortopt("h"))
			{
				usage(false);
				exit_compiler(COMPILER_SUCCESS_EXIT);
			}
			break;
		case 'q':
			if (match_shortopt("q"))
			{
				options->verbosity_level = -1;
				return;
			}
			break;
		case 'v':
			if (match_shortopt("vvv"))
			{
				options->verbosity_level = 3;
				return;
			}
			if (match_shortopt("vv"))
			{
				options->verbosity_level = 2;
				return;
			}
			if (match_shortopt("v"))
			{
				options->verbosity_level = 1;
				return;
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
				options->runner_output_name = options->output_name = next_arg();
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
				options->linker_lib_dir[options->linker_lib_dir_count++] = unchecked_dir(options, next_arg());
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
			if ((argopt = match_argopt("validation")))
			{
				options->validation_level = parse_opt_select(ValidationLevel, argopt, validation_levels);
				return;
			}
			if ((argopt = match_argopt("ansi")))
			{
				options->ansi = parse_opt_select(Ansi, argopt, on_off);
				return;
			}
			if (match_longopt("max-mem"))
			{
				if (at_end() || next_is_opt())
				{
					FAIL_WITH_ERR_LONG("'--max-mem' expected a max memory.");
				}
				if (atoll(next_arg()) < 1)
				{
					FAIL_WITH_ERR_LONG("'--max-mem' expected a positive integer value.");
				}
				return;
			}
			if (match_longopt("sources"))
			{
				if (at_end() || next_is_opt())
				{
					FAIL_WITH_ERR_LONG("'--sources' expected at least one source file.");
				}
				do
				{
					next_arg();
					append_file(options);

				} while(!(at_end() || next_is_opt()));
				return;
			}
			if (match_longopt("use-old-slice-copy"))
			{
				options->old_slice_copy = true;
				return;
			}
			if (match_longopt("use-old-enums"))
			{
				options->old_enums = true;
				return;
			}
			if (match_longopt("use-old-compact-eq"))
			{
				options->old_compact_eq = true;
				return;
			}
			if (match_longopt("test-filter"))
			{
				if (at_end() || next_is_opt()) FAIL_WITH_ERR_LONG("error: --test-filter needs an argument.");
				options->test_filter = next_arg();
				return;
			}
			if (match_longopt("test-breakpoint"))
			{
				options->test_breakpoint = true;
				return;
			}
			if (match_longopt("test-noleak"))
			{
				options->test_noleak = true;
				return;
			}
			if (match_longopt("test-nocapture") || match_longopt("test-show-output"))
			{
				options->test_show_output = true;
				return;
			}
			if (match_longopt("test-quiet"))
			{
				options->test_quiet = true;
				return;
			}
			if (match_longopt("test-nosort"))
			{
				options->test_nosort = true;
				return;
			}
			if (match_longopt("silence-deprecation"))
			{
				options->silence_deprecation = true;
				silence_deprecation = true;
				return;
			}
			if (match_longopt("build-env"))
			{
				options->print_env = true;
				return;
			}
			if (match_longopt("symtab"))
			{
				if (at_end() || next_is_opt()) FAIL_WITH_ERR_LONG("error: --symtab needs a valid integer.");
				const char *symtab_string = next_arg();
				int symtab = atoi(symtab_string);
				if (symtab < 1024) FAIL_WITH_ERR_LONG("Expected the --symtab size to be valid positive integer >= 1024.");
				if (symtab > MAX_SYMTAB_SIZE)
				{
					FAIL_WITH_ERR_LONG("The symtab size is too large. The maximum size is %d.", (int)MAX_SYMTAB_SIZE);
				}
				options->symtab_size = next_highest_power_of_2(symtab);
				return;
			}
			if (match_longopt("quiet"))
			{
				options->verbosity_level = -1;
				return;
			}
			if (match_longopt("version"))
			{
				print_version();
				exit_compiler(COMPILER_SUCCESS_EXIT);
			}
			if ((argopt = match_argopt("backend")))
			{
				options->backend = parse_opt_select(CompilerBackend, argopt, backends);
				return;
			}
			if (match_longopt("run-once"))
			{
				options->run_once = true;
				if (!options->verbosity_level) options->verbosity_level = -1;
				return;
			}
			if ((argopt = match_argopt("fp-math")))
			{
				options->fp_math = parse_opt_select(FpOpt, argopt, fp_math);
				return;
			}
			if ((argopt = match_argopt("linux-libc")))
			{
				if (str_eq(argopt, "host"))
				{
					options->linux_libc = default_libc;
				}
				else
					options->linux_libc = parse_opt_select(LinuxLibc, argopt, linuxlibc);
				return;
			}
			if ((argopt = match_argopt("optsize")))
			{
				options->optsize = parse_opt_select(SizeOptimizationLevel, argopt, optsizes);
				return;
			}
			if ((argopt = match_argopt("optlevel")))
			{
				options->optlevel = parse_opt_select(OptimizationLevel, argopt, optlevels);
				return;
			}
			if ((argopt = match_argopt("test-log-level")))
			{
				options->test_log_level = parse_opt_select(TestLogLevel, argopt, test_log_levels);
				return;
			}
			if ((argopt = match_argopt("merge-functions")))
			{
				options->merge_functions = parse_opt_select(MergeFunctions, argopt, on_off);
				return;
			}
			if ((argopt = match_argopt("loop-vectorize")))
			{
				options->loop_vectorization = parse_opt_select(AutoVectorization, argopt, on_off);
				return;
			}
			if ((argopt = match_argopt("unroll-loops")))
			{
				options->unroll_loops = parse_opt_select(UnrollLoops, argopt, on_off);
				return;
			}
			if ((argopt = match_argopt("slp-vectorize")))
			{
				options->slp_vectorization = parse_opt_select(AutoVectorization, argopt, on_off);
				return;
			}
			if ((argopt = match_argopt("safe")))
			{
				options->safety_level = parse_opt_select(SafetyLevel, argopt, on_off);
				return;
			}
			if ((argopt = match_argopt("show-backtrace")))
			{
				options->show_backtrace = parse_opt_select(ShowBacktrace, argopt, on_off);
				return;
			}
			if ((argopt = match_argopt("panic-msg")))
			{
				options->panic_level = parse_opt_select(PanicLevel, argopt, on_off);
				return;
			}
			if ((argopt = match_argopt("single-module")))
			{
				options->single_module = parse_opt_select(SingleModule, argopt, on_off);
				return;
			}
			if ((argopt = match_argopt("linker")))
			{
				options->custom_linker_path = NULL;
				options->linker_type = parse_opt_select(LinkerType, argopt, linker_kind);
				if (options->linker_type == LINKER_TYPE_CUSTOM)
				{
					if (at_end() || next_is_opt()) error_exit("error: --linker=custom expects a valid linker name.");
					options->custom_linker_path = next_arg();
				}
				return;
			}
			if ((argopt = match_argopt("link-libc")))
			{
				options->link_libc = parse_opt_select(LinkLibc, argopt, on_off);
				return;
			}
			if ((argopt = match_argopt("strip-unused")))
			{
				options->strip_unused = parse_opt_select(StripUnused, argopt, on_off);
				return;
			}
			if ((argopt = match_argopt("emit-stdlib")))
			{
				options->emit_stdlib = parse_opt_select(EmitStdlib, argopt, on_off);
				return;
			}
			if (match_longopt("emit-only"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --emit-only expects an output name, e.g. 'foo', to only output 'foo.o'.");
				vec_add(options->emit_only, next_arg());
				return;
			}
			if ((argopt = match_argopt("use-stdlib")))
			{
				options->use_stdlib = parse_opt_select(UseStdlib, argopt, on_off);
				return;
			}
			if ((argopt = match_argopt("x86vec")))
			{
				options->x86_vector_capability = parse_opt_select(X86VectorCapability, argopt, x86_vector_capability);
				return;
			}
			if ((argopt = match_argopt("win64-simd")))
			{
				options->win_64_simd = parse_opt_select(Win64Simd, argopt, win64_simd_type);
				return;
			}
			if ((argopt = match_argopt("win-debug")))
			{
				options->win_debug = parse_opt_select(WinDebug, argopt, win_debug_type);
				return;
			}
			if ((argopt = match_argopt("x86cpu")))
			{
				options->x86_cpu_set = parse_opt_select(X86CpuSet, argopt, x86_cpu_set);
				return;
			}
			if ((argopt = match_argopt("riscv-cpu")))
			{
				options->riscv_cpu_set = parse_opt_select(RiscvCpuSet, argopt, riscv_cpu_set);
				return;
			}
			if ((argopt = match_argopt("riscvfloat")))
			{
				options->riscv_abi = parse_opt_select(RiscvAbi, argopt, riscv_capability);
				return;
			}
			if ((argopt = match_argopt("riscv-abi")))
			{
				options->riscv_abi = parse_opt_select(RiscvAbi, argopt, riscv_abi);
				return;
			}
			if (match_longopt("cpu-flags"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --cpu-flags expected a comma-separated list, like '+a,-b,+x'.");
				scratch_buffer_clear();
				if (options->cpu_flags)
				{
					scratch_buffer_append(options->cpu_flags);
					scratch_buffer_append_char(',');
				}
				scratch_buffer_append(next_arg());
				options->cpu_flags = scratch_buffer_copy();
				return;
			}
			if (match_longopt("max-stack-object-size"))
			{
				int size = (at_end() || next_is_opt()) ? 0 : atoi(next_arg());
				if (size < 1) error_exit("Expected a valid positive integer >= 1 for --max-stack-object-size.");
				if (size > MAX_STACK_OBJECT_SIZE) error_exit("Expected a valid positive integer <= %u for --max-stack-object-size.", (unsigned)MAX_STACK_OBJECT_SIZE);
				options->max_stack_object_size = size;
				return;
			}
			if (match_longopt("max-macro-iterations"))
			{
				int size = (at_end() || next_is_opt()) ? 0 : atoi(next_arg());
				if (size < 1) error_exit("Expected a valid positive integer >= 128 for --max-macro-iterations");
				if (size > MAX_MACRO_ITERATIONS) error_exit("Expected a valid positive integer <= %u for --max-macro-iterations.", (unsigned)MAX_MACRO_ITERATIONS);
				options->max_macro_iterations = size;
				return;
			}
			if (match_longopt("max-vector-size"))
			{
				int size = (at_end() || next_is_opt()) ? 0 : atoi(next_arg());
				if (size < 128) error_exit("Expected a valid positive integer >= 128 for --max-vector-size.");
				if (size > MAX_VECTOR_WIDTH) error_exit("Expected a valid positive integer <= %u for --max-vector-size.", (unsigned)MAX_VECTOR_WIDTH);
				if (size != next_highest_power_of_2(size))
				{
					error_exit("The --max-vector-size value must be a power of 2, try using %u instead.", next_highest_power_of_2(size));
				}
				options->max_vector_size = size;
				return;
			}
			if ((argopt = match_argopt("memory-env")))
			{
				options->memory_environment = parse_opt_select(MemoryEnvironment, argopt, memory_environment);
				return;
			}
			if ((argopt = match_argopt("reloc")))
			{
				options->reloc_model = parse_opt_select(RelocModel, argopt, reloc_models);
				return;
			}
			if (match_longopt("about"))
			{
				PRINTF("The C3 Compiler");
				PRINTF("C3 is low level programming language based on C.");
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
			if (match_longopt("no-headers"))
			{
				options->no_headers = true;
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
			if (match_longopt("list-asm"))
			{
				options->print_asm = true;
				return;
			}
			if (match_longopt("list-project-properties"))
			{
				options->print_project_properties = true;
				options->command = COMMAND_PRINT_SYNTAX;
				return;
			}
			if (match_longopt("list-manifest-properties"))
			{
				options->print_manifest_properties = true;
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
				if (threads < 1) PRINTF("Expected a valid integer 1 or higher.");
				if (threads > MAX_THREADS) PRINTF("Cannot exceed %d threads.", MAX_THREADS);
				options->build_threads = threads;
				return;
			}
			if (match_longopt("echo-prefix"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --echo-prefix needs a prefix.");
				options->echo_prefix = next_arg();
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
				PRINTF("Available targets:");
				EOUTPUT("Invalid target %s.", target);
				EOUTPUT("These targets are supported:");
				for (unsigned i = 0; i <= ARCH_OS_TARGET_LAST; i++)
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
			if (match_longopt("print-input"))
			{
				options->print_input = true;
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
				options->std_lib_dir = unchecked_dir(options, next_arg());
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
				options->macos.sysroot = unchecked_dir(options, next_arg());
				return;
			}
			if (match_longopt("winsdk"))
			{
				if (options->win.vs_dirs)
				{
					error_exit("error: --winsdk cannot be combined with --win-vs-dirs.");
				}
				if (at_end() || next_is_opt()) error_exit("error: --winsdk needs a directory.");
				options->win.sdk = unchecked_dir(options, next_arg());
				return;
			}
			if ((argopt = match_argopt("trust")))
			{
				options->trust_level = parse_opt_select(TrustLevel, argopt, trust_level);
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
				options->win.crt_linking = parse_opt_select(WinCrtLinking, argopt, wincrt_linking);
				return;
			}
			if (match_longopt("win-vs-dirs"))
			{
				if (options->win.sdk)
				{
					error_exit("error: --win-vs-dirs cannot be combined with --winsdk.");
				}
				if (at_end() || next_is_opt()) error_exit("error: --win-vs-dirs needs to followed by the directories.");
				options->win.vs_dirs = next_arg();
				return;
			}
			if ((argopt = match_argopt("sanitize")))
			{
				options->sanitize_mode = parse_opt_select(SanitizeMode, argopt, sanitize_modes);
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
			if (match_longopt("output-dir"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --output-dir needs a directory.");
				options->output_dir = next_arg();
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
			if (match_longopt("header-output"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --header-output needs a directory.");
				options->header_out = next_arg();
				return;
			}
			if (match_longopt("lib"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --lib needs a name.");
				const char *name = next_arg();
				if (!str_is_valid_lowercase_name(name))
				{
					if (str_has_suffix(name, ".c3l"))
					{
						error_exit("When specifying libraries, the .c3l suffix should"
						           " not be included, so rather than '--lib %s', try using '--lib %s' instead.",
						           name, str_remove_suffix(name, ".c3l"));
					}
					if (str_has_suffix(name, ".lib") || str_has_suffix(name, ".a")
					    || str_has_suffix(name, ".dll") || str_has_suffix(name, ".so"))
					{
						error_exit("You tried to add '%s' as a C3 library, but from the name it appears to be a"
						           " static/dynamic library. To link with such a library, use '-l <name>' instead.",
						           name);
					}
					char *name_copy = strdup(name);
					str_elide_in_place(name_copy, 32);
					if (strchr(name, '/') != NULL || (PLATFORM_WINDOWS && strchr(name, '\\') != NULL))
					{
						error_exit(
								"There is a problem including the library '%s': a library name should never contain the path. Use '--libdir' to add the "
								"directory to the library search paths, then use the plain name for '--lib', "
								"e.g '--libdir my_project/libs --lib some_lib'.", name_copy);
					}
					error_exit("Invalid library name '%s', it should be something like 'foo_lib'.", name_copy);
				}
				options->libs[options->lib_count++] = name;
				return;
			}
			if (match_longopt("libdir"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --libdir needs a directory.");
				if (options->lib_dir_count == MAX_BUILD_LIB_DIRS) error_exit("Max %d library directories may be specified.", MAX_BUILD_LIB_DIRS);
				options->lib_dir[options->lib_dir_count++] = unchecked_dir(options, next_arg());
				return;
			}
			if (match_longopt("run-dir"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --run-dir needs a directory.");
				options->run_dir = unchecked_dir(options, next_arg());
				return;
			}
			if (match_longopt("benchmark"))
			{
				options->benchmark_mode = true;
				return;
			}
			if (match_longopt("lsp"))
			{
				options->lsp_mode = true;
				options->strip_unused = STRIP_UNUSED_OFF;
				options->test_mode = false;
				options->benchmarking = true;
				options->testing = true;
				return;
			}
			if (match_longopt("test"))
			{
				options->lsp_mode = false;
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
				options->linuxpaths.crt = unchecked_dir(options, next_arg());
				return;
			}
			if (match_longopt("linux-crtbegin"))
			{
				if (at_end() || next_is_opt()) error_exit("error: --linux-crtbegin needs a directory.");
				options->linuxpaths.crtbegin = unchecked_dir(options, next_arg());
				return;
			}
			if (match_longopt("android-ndk"))
			{
				if (at_end() || next_is_opt()) error_exit("error: android-ndk needs a directory.");
				options->android.ndk_path = unchecked_dir(options, next_arg());
				return;
			}
			if (match_longopt("android-api"))
			{
				if (at_end() || next_is_opt()) error_exit("error: android-api needs a version.");
				options->android.api_version = atoi(next_arg());
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
			if (match_longopt("suppress-run"))
			{
				options->suppress_run = true;
				return;
			}
			if (match_longopt("help"))
			{
				usage(true);
				exit_compiler(COMPILER_SUCCESS_EXIT);
			}
			break;
		default:
			break;

	}
	FAIL_WITH_ERR_LONG("Cannot process the unknown option \"%s\".", current_arg);
}

BuildOptions parse_arguments(int argc, const char *argv[])
{
	arg_count = argc;
	args = argv;

	if (argc < 2)
	{
		usage(false);
		exit_compiler(COMPILER_SUCCESS_EXIT);
	}

	BuildOptions build_options = {
		.path = DEFAULT_PATH,
		.vendor_download_path = DEFAULT_PATH,
		.emit_llvm = false,
		.optsetting = OPT_SETTING_NOT_SET,
		.debug_info_override = DEBUG_INFO_NOT_SET,
		.test_log_level = TESTLOGLEVEL_NOT_SET,
		.safety_level = SAFETY_NOT_SET,
		.panic_level = PANIC_NOT_SET,
		.show_backtrace = SHOW_BACKTRACE_NOT_SET,
		.optlevel = OPTIMIZATION_NOT_SET,
		.optsize = SIZE_OPTIMIZATION_NOT_SET,
		.build_threads = cpus(),
		.command = COMMAND_MISSING,
		.reloc_model = RELOC_DEFAULT,
		.backend = BACKEND_LLVM,
		.x86_vector_capability = X86VECTOR_DEFAULT,
		.win_64_simd = WIN64_SIMD_DEFAULT,
		.win_debug = WIN_DEBUG_DEFAULT,
		.fp_math = FP_DEFAULT,
		.x86_cpu_set = X86CPU_DEFAULT,
		.riscv_abi = RISCV_ABI_DEFAULT,
		.memory_environment = MEMORY_ENV_NOT_SET,
		.win.crt_linking = WIN_CRT_DEFAULT,
		.emit_stdlib = EMIT_STDLIB_NOT_SET,
		.link_libc = LINK_LIBC_NOT_SET,
		.use_stdlib = USE_STDLIB_NOT_SET,
		.arch_os_target_override = ARCH_OS_TARGET_DEFAULT,
		.linker_type = LINKER_TYPE_NOT_SET,
		.validation_level = VALIDATION_NOT_SET,
		.ansi = ANSI_DETECT,
		.strip_unused = STRIP_UNUSED_NOT_SET,
		.single_module = SINGLE_MODULE_NOT_SET,
		.sanitize_mode = SANITIZE_NOT_SET,
		.unroll_loops = UNROLL_LOOPS_NOT_SET,
		.merge_functions = MERGE_FUNCTIONS_NOT_SET,
		.slp_vectorization = VECTORIZATION_NOT_SET,
		.loop_vectorization = VECTORIZATION_NOT_SET,
		.linux_libc = LINUX_LIBC_NOT_SET,
		.files = NULL,
		.build_dir = NULL,
		.output_dir = NULL,
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

	bool collecting_args = false;
	for (arg_index = 1; arg_index < arg_count; arg_index++)
	{
		current_arg = args[arg_index];
		if (collecting_args)
		{
			append_arg(&build_options);
			continue;
		}
		if (command_passes_args(build_options.command) && arg_match("--"))
		{
			collecting_args = true;
			continue;
		}
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
		if (command_accepts_files(build_options.command))
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
	if (build_options.arch_os_target_override == ANDROID_AARCH64)
	{
		if (!build_options.android.ndk_path)
		{
			const char *ndk_path = getenv("ANDROID_NDK");
			if (!ndk_path)
			{
				FAIL_WITH_ERR("Can't find Android NDK, please set --android-ndk.");
			}
			build_options.android.ndk_path = strdup(ndk_path);
		}
		if (build_options.android.api_version <= 0)
		{
			build_options.android.api_version = 30; // 30 = Android 11
		}
	}
	debug_log = build_options.verbosity_level > 2;
	return build_options;
}

ArchOsTarget arch_os_target_from_string(const char *target)
{
	for (unsigned i = 0; i <= ARCH_OS_TARGET_LAST; i++)
	{
		if (strcmp(arch_os_target[i], target) == 0)
		{
			return (ArchOsTarget)i;
		}
	}
	return ARCH_OS_TARGET_DEFAULT;
}

// -- helpers

static const char *unchecked_dir(BuildOptions *options, const char *path)
{
	vec_add(options->unchecked_directories, path);
	return path;
}

const char *check_dir(const char *path)
{
	char original_path[PATH_MAX + 1];
	if (!getcwd(original_path, PATH_MAX)) error_exit("Failed to store path.");
	if (!dir_change(path)) error_exit("The path \"%s\" does not point to a valid directory from '%s'.", path, original_path);
	if (!dir_change(original_path)) FAIL_WITH_ERR("Failed to change path to %s.", original_path);
	return path;
}

static inline bool at_end()
{
	return arg_index == arg_count - 1;
}

static inline const char *next_arg()
{
	ASSERT(!at_end());
	current_arg = args[++arg_index];
	return current_arg;
}

static inline bool next_is_opt()
{
	return args[arg_index + 1][0] == '-';
}

INLINE bool match_longopt(const char *name)
{
	return str_eq(&current_arg[2], name);
}

static inline bool match_shortopt(const char *name)
{
	return str_eq(&current_arg[1], name);
}

void append_file(BuildOptions *build_options)
{
	if (vec_size(build_options->files) == MAX_COMMAND_LINE_FILES)
	{
		EOUTPUT("Max %d files may be specified.", MAX_COMMAND_LINE_FILES);
		exit_compiler(EXIT_FAILURE);
	}
	vec_add(build_options->files, current_arg);
}

static inline const char *match_argopt(const char *name)
{
	size_t len = strlen(name);
	if (!str_start_with(&current_arg[2], name)) return NULL;
	if (current_arg[2 + len] != '=') return NULL;
	return &current_arg[2 + len + 1];
}

void append_arg(BuildOptions *build_options)
{
	if (vec_size(build_options->args) == MAX_COMMAND_LINE_RUN_ARGS)
	{
		EOUTPUT("Max %d args may be specified.", MAX_COMMAND_LINE_RUN_ARGS);
		exit_compiler(EXIT_FAILURE);
	}
	vec_add(build_options->args, current_arg);
}

static bool arg_match(const char *candidate)
{
	return str_eq(current_arg, candidate);
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

static void add_linker_arg(BuildOptions *options, const char *arg)
{
	if (options->linker_arg_count == MAX_BUILD_LIB_DIRS)
	{
		error_exit("Too many linker arguments are given, more than %d\n", MAX_BUILD_LIB_DIRS);
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
static void update_feature_flags(const char ***flags, const char ***removed_flags, const char *arg, bool add)
{
	// We keep two lists "remove" and "add" lists:
	const char ***to_remove_from = add ? removed_flags : flags;

	// Remove from opposite list using string equality
	// More elegant would be using a Set or Map, but that's overkill
	// for something that's likely just 1-2 values.
	FOREACH_IDX(i, const char *, value, *to_remove_from)
	{
		if (str_eq(value, arg))
		{
			vec_erase_at(*to_remove_from, i);
			break;
		}
	}

	// First we check that it's not in the list
	const char ***to_add_to_ref = add ? flags : removed_flags;
	FOREACH(const char *, value, *to_add_to_ref)
	{
		// If we have a match, we don't add it.
		if (str_eq(value, arg)) return;
	}

	// No match, so add it.
	vec_add(*to_add_to_ref, arg);
}

static void print_all_targets(void)
{
	PRINTF("Available targets:");
	for (unsigned i = 0; i <= ARCH_OS_TARGET_LAST; i++)
	{
		PRINTF("   %s", arch_os_target[i]);
	}
}

static int parse_option_select(const char *start, unsigned count, const char **elements)
{
	assert(count >= 2);
	const char *arg = current_arg;
	int select = str_findlist(start, count, elements);
	if (select < 0)
	{
		switch (count)
		{
			case 2:
				error_exit("Error: '%.*s' does not support the option '%s', expected '%s' or '%s'.", (int)(start - arg - 1), arg, start, elements[0], elements[1]);
			case 3:
				error_exit("Error: '%.*s' does not support the option '%s', expected '%s', '%s' or '%s'.", (int)(start - arg - 1), arg, start, elements[0], elements[1], elements[2]);
			default:
				error_exit("Error: '%.*s' does not support the option '%s', expected an option like '%s' or '%s'.", (int)(start - arg - 1), arg, start, elements[0], elements[1]);
		}
		UNREACHABLE
	}
	return select;
}

const char *arch_os_target[ARCH_OS_TARGET_LAST + 1] = {
		[ANDROID_AARCH64] = "android-aarch64",
		[ANDROID_X86_64] = "android-x86_64",
		[ELF_AARCH64] = "elf-aarch64",
		[ELF_RISCV32] = "elf-riscv32",
		[ELF_RISCV64] = "elf-riscv64",
		[ELF_X86] = "elf-x86",
		[ELF_X64] = "elf-x64",
		[ELF_XTENSA] = "elf-xtensa",
		[FREEBSD_X86] = "freebsd-x86",
		[FREEBSD_X64] = "freebsd-x64",
		[IOS_AARCH64] = "ios-aarch64",
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

static void print_cmd(const char *command, const char *desc)
{
	printf("  %-51s %s\n", command, desc);
}

static void print_opt(const char *option, const char *desc)
{
	printf("  %-26s - %s\n", option, desc);
}
