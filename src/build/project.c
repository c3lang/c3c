// Copyright (c) 2020-2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "build_internal.h"
#include "utils/json.h"


const char *project_default_keys[][2] = {
		{"$schema", "Json schema url"},
		{"authors", "Authors, optionally with email."},
		{"benchfn", "Override the benchmark function."},
		{"build-dir", "Build location, where intermediate files are placed by default, relative to project file."},
		{"c-include-dirs", "Set the include directories for C sources."},
		{"c-sources", "Set the C sources to be compiled."},
		{"cc", "Set C compiler (defaults to 'cc')."},
		{"cflags", "C compiler flags."},
		{"cpu", "CPU name, used for optimizations in the compiler backend."},
		{"cpu-flags", "Set the cpu flags to add or remove with the format '+avx,-sse'."},
		{"debug-info", "Debug level: none, line-tables, full."},
		{"dependencies", "C3 library dependencies for all targets."},
		{"dependency-search-paths", "The C3 library search paths."},
		{"exec", "Scripts run for all targets."},
		{"features", "Features enabled for all targets."},
		{"fp-math", "Set math behaviour: `strict`, `relaxed` or `fast`."},
		{"langrev", "Version of the C3 language used."},
		{"link-args", "Linker arguments for all targets."},
		{"link-libc", "Link libc (default: true)."},
		{"custom-libc", "Implement your own libc (default: false)."},
		{"linked-libraries", "Libraries linked by the linker for all targets."},
		{"linker", "'builtin' for the builtin linker, 'cc' for the system linker or <path> to a custom compiler."},
		{"linker-search-paths", "Linker search paths."},
		{"linux-crt", "Set the directory to use for finding crt1.o and related files."},
		{"linux-crtbegin", "Set the directory to use for finding crtbegin.o and related files."},
		{"linux-libc", "Set the libc to use for Linux. Valid options are 'host', 'gnu' and 'musl', default is 'host'"},
		{"loop-vectorize", "Force enable/disable loop auto-vectorization."},
		{"macos-min-version", "Set the minimum MacOS version to compile for."},
		{"macos-sdk-version", "Set the MacOS SDK compiled for." },
		{"macossdk", "Set the directory for the MacOS SDK for cross compilation."},
		{"memory-env", "Set the memory environment: normal, small, tiny, none."},
		{"merge-functions", "Force enable/disable function merging."},
		{"no-entry", "Do not generate (or require) a main function."},
		{"opt", "Optimization setting: O0, O1, O2, O3, O4, O5, Os, Oz."},
		{"optlevel", "Code optimization level: none, less, more, max."},
		{"optsize", "Code size optimization: none, small, tiny."},
		{"output", "Output location, relative to project file."},
		{"panic-msg", "Turn panic message output on or off."},
		{"panicfn", "Override the panic function."},
		{"quiet", "Silence unnecessary output."},
		{"reloc", "Relocation model: none, pic, PIC, pie, PIE."},
		{"riscv-abi", "RiscV ABI: int-only, float, double."},
		{"riscv-cpu", "Set general level of RISC-V cpu: `rvi`, `rvimac`, `rvimafc`, `rvgc` or `rvgcv`."},
		{"run-dir", "Override run directory for 'run'."},
		{"safe", "Set safety (contracts, runtime bounds checking, null pointer checks etc) on or off."},
		{"sanitize", "Enable sanitizer: none, address, memory, thread."},
		{"script-dir", "The directory where 'exec' is run."},
		{"show-backtrace", "Print backtrace on signals."},
		{"single-module", "Compile all modules together, enables more inlining."},
		{"slp-vectorize", "Force enable/disable SLP auto-vectorization."},
		{"soft-float", "Output soft-float functions."},
		{"sources", "Paths to project sources for all targets."},
		{"strip-unused", "Strip unused code and globals from the output. (default: true)"},
		{"symtab", "Sets the preferred symtab size."},
		{"target", "Compile for a particular architecture + OS target."},
		{"targets", "Set of targets for the project."},
		{"test-sources", "Paths to project test sources for all targets."},
		{"testfn", "Override the test function."},
		{"trap-on-wrap", "Make signed and unsigned integer overflow generate a panic rather than wrapping."},
		{"unroll-loops", "Force enable/disable loop unrolling optimization."},
		{"use-stdlib", "Include the standard library (default: true)."},
		{"vendor", "Vendor specific extensions, ignored by c3c."},
		{"version", "Version using semantic versioning."},
		{"warnings", "Warnings used for all targets."},
		{"wincrt", "Windows CRT linking: none, static-debug, static, dynamic-debug (default if debug info enabled), dynamic (default)."},
		{"windef", "Windows def file, used as an alternative to dllexport when exporting a DLL."},
		{"winsdk", "Set the path to Windows system library files for cross compilation."},
		{"x86-stack-struct-return", "Return structs on the stack for x86."},
		{"x86cpu", "Set general level of x64 cpu: baseline, ssse3, sse4, avx1, avx2-v1, avx2-v2 (Skylake/Zen1+), avx512 (Icelake/Zen4+), native."},
		{"x86vec", "Set max type of vector use: none, mmx, sse, avx, avx512, native."},

};

const int project_default_keys_count = ELEMENTLEN(project_default_keys);

const char* project_deprecated_target_keys[] = { "xxxxxxxxxx" };
const char* project_target_keys[][2] = {
		{"benchfn", "Override the benchmark function."},
		{"build-dir", "Build location, where intermediate files are placed by default, relative to project file."},
		{"c-include-dirs", "C sources include directories for the target."},
		{"c-include-dirs-override", "Additional C sources include directories for the target, overriding global settings."},
		{"c-sources", "Additional C sources to be compiled for the target."},
		{"c-sources-override", "C sources to be compiled, overriding global settings."},
		{"cc", "Set C compiler (defaults to 'cc')."},
		{"cflags", "Additional C compiler flags for the target."},
		{"cflags-override", "C compiler flags for the target, overriding global settings."},
		{"cpu", "CPU name, used for optimizations in the compiler backend."},
		{"cpu-flags", "Additional cpu flags to add or remove with the format '+avx,-sse'."},
		{"cpu-flags-override", "Additional cpu flags to add or remove with the format '+avx,-sse', overriding global settings."},
		{"debug-info", "Debug level: none, line-tables, full."},
		{"dependencies", "Additional C3 library dependencies for the target."},
		{"dependencies-override", "C3 library dependencies for this target, overriding global settings."},
		{"dependency-search-paths", "Additional C3 library search paths for the target."},
		{"dependency-search-paths-override", "C3 library search paths for this target, overriding global settings."},
		{"exec", "Additional scripts to run for the target."},
		{"exec-override", "Scripts to run for this target, overriding global settings."},
		{"extension", "Override the default file extension for the build output."},
		{"features", "Features enabled for all targets."},
		{"fp-math", "Set math behaviour: `strict`, `relaxed` or `fast`."},
		{"langrev", "Version of the C3 language used."},
		{"link-args", "Additional linker arguments for the target."},
		{"link-args-override", "Linker arguments for this target, overriding global settings."},
		{"link-libc", "Link libc (default: true)."},
		{"custom-libc", "Implement your own libc (default: false)."},
		{"linked-libraries", "Additional libraries linked by the linker for the target."},
		{"linked-libraries-override", "Libraries linked by the linker for this target, overriding global settings."},
		{"linker", "'builtin' for the builtin linker, 'cc' for the system linker or <path> to a custom compiler."},
		{"linker-search-paths", "Additional linker search paths for the target."},
		{"linker-search-paths-override", "Linker search paths for this target, overriding global settings."},
		{"linux-crt", "Set the directory to use for finding crt1.o and related files."},
		{"linux-crtbegin", "Set the directory to use for finding crtbegin.o and related files."},
		{"linux-libc", "Set the libc to use for Linux. Valid options are 'host', 'gnu' and 'musl', default is 'host'"},
		{"loop-vectorize", "Force enable/disable loop auto-vectorization."},
		{"macos-min-version", "Set the minimum MacOS version to compile for."},
		{"macos-sdk-version", "Set the MacOS SDK compiled for." },
		{"macossdk", "Set the directory for the MacOS SDK for cross compilation."},
		{"memory-env", "Set the memory environment: normal, small, tiny, none."},
		{"merge-functions", "Force enable/disable function merging."},
		{"name", "Set the name to be different from the target name."},
		{"no-entry", "Do not generate (or require) a main function."},
		{"opt", "Optimization setting: O0, O1, O2, O3, O4, O5, Os, Oz."},
		{"optlevel", "Code optimization level: none, less, more, max."},
		{"optsize", "Code size optimization: none, small, tiny."},
		{"output", "Output location, relative to project file."},
		{"panic-msg", "Turn panic message output on or off."},
		{"panicfn", "Override the panic function."},
		{"quiet", "Silence unnecessary output."},
		{"reloc", "Relocation model: none, pic, PIC, pie, PIE."},
		{"riscv-abi", "RiscV ABI: int-only, float, double."},
		{"run-dir", "Override run directory for 'run'."},
		{"safe", "Set safety (contracts, runtime bounds checking, null pointer checks etc) on or off."},
		{"sanitize", "Enable sanitizer: none, address, memory, thread."},
		{"script-dir", "The directory where 'exec' is run."},
		{"show-backtrace", "Print backtrace on signals."},
		{"single-module", "Compile all modules together, enables more inlining."},
		{"slp-vectorize", "Force enable/disable SLP auto-vectorization."},
		{"soft-float", "Output soft-float functions."},
		{"sources", "Additional paths to project sources for the target."},
		{"sources-override", "Paths to project sources for this target, overriding global settings."},
		{"strip-unused", "Strip unused code and globals from the output. (default: true)"},
		{"symtab", "Sets the preferred symtab size."},
		{"target", "Compile for a particular architecture + OS target."},
		{"test-sources", "Additional paths to project test sources for the target."},
		{"test-sources-override", "Paths to project test sources for this target, overriding global settings."},
		{"testfn", "Override the test function."},
		{"trap-on-wrap", "Make signed and unsigned integer overflow generate a panic rather than wrapping."},
		{"type", "Type of output, one of 'executable', 'static-lib', 'dynamic-lib', 'benchmark', 'test', 'object-files' and 'prepare'." },
		{"unroll-loops", "Force enable/disable loop unrolling optimization."},
		{"use-stdlib", "Include the standard library (default: true)."},
		{"vendor", "Vendor specific extensions, ignored by c3c."},
		{"version", "Version using semantic versioning."},
		{"warnings", "Warnings used for all targets."},
		{"wincrt", "Windows CRT linking: none, static-debug, static, dynamic-debug (default if debug info enabled), dynamic (default)."},
		{"windef", "Windows def file, used as an alternative to dllexport when exporting a DLL."},
		{"winsdk", "Set the path to Windows system library files for cross compilation."},
		{"x86-stack-struct-return", "Return structs on the stack for x86."},
		{"x86cpu", "Set general level of x64 cpu: baseline, ssse3, sse4, avx1, avx2-v1, avx2-v2 (Skylake/Zen1+), avx512 (Icelake/Zen4+), native."},
		{"x86vec", "Set max type of vector use: none, mmx, sse, avx, avx512, native."},

};

const int project_target_keys_count = ELEMENTLEN(project_target_keys);
const int project_deprecated_target_keys_count = ELEMENTLEN(project_deprecated_target_keys);

#define GET_SETTING(type__, key__, strings__, comment__) \
  (type__)get_valid_string_setting(context, json, key__, strings__, 0, ELEMENTLEN(strings__), comment__)

// Json -> target / default target
static void load_into_build_target(BuildParseContext context, JSONObject *json, BuildTarget *target)
{
	if (context.target)
	{
		check_json_keys(project_target_keys, project_target_keys_count, project_deprecated_target_keys, project_deprecated_target_keys_count, json, context.target, "--list-project-properties");
	}
	else
	{
		check_json_keys(project_default_keys, project_default_keys_count, NULL, 0, json, "default target", "--list-project-properties");
	}

	// The default c compiler name
	target->cc = get_string(context, json, "cc", target->cc);

	// Where to find and execute the scripts
	target->script_dir = get_string(context, json, "script-dir", target->script_dir);

	// Where to `run` from
	target->run_dir = get_string(context, json, "run-dir", target->run_dir);
	// The output directory
	target->output_dir = get_string(context, json, "output", target->output_dir);
	target->build_dir = get_string(context, json, "build-dir", target->build_dir);

	const char *cpu_flags = get_optional_string(context, json, "cpu-flags");
	if (cpu_flags)
	{
		if (target->cpu_flags)
		{
			scratch_buffer_clear();
			scratch_buffer_printf("%s,%s", target->cpu_flags, cpu_flags);
			target->cpu_flags = scratch_buffer_copy();
		}
		else
		{
			target->cpu_flags = cpu_flags;
		}
	}
	if (context.target)
	{
		const char *cpu_flags_override = get_optional_string(context, json, "cpu-flags-override");
		if (cpu_flags_override)
		{
			if (cpu_flags)
			{
				error_exit("Error reading %s: 'cpu-flags' and 'cpu-flags-override' cannot be combined.", context.file);
			}
			target->cpu_flags = cpu_flags_override;
		}
	}

	if (context.target)
	{
		// The output extension
		target->extension = get_string(context, json, "extension", target->extension);

		if (target->extension && target->extension[0] != '.')
		{
			error_exit("Error reading %s: output extension '%s' must start with a '.'", context.file, target->extension);
		}
	}
	else
	{
		const char **authors = get_optional_string_array(context, json, "authors");
		AuthorEntry *author_list = NULL;
		FOREACH(const char *, author, authors)
		{
			const char *email_start = strstr(author, "<");
			if (email_start)
			{
				const char *end = strstr(email_start + 1, ">");
				if (!end || end[1] != 0 || email_start + 1 == end) error_exit("Error reading %s: invalid author format '%s', expected an e-mail address between '< >'.", context.file, author);
				const char *email = str_trim(str_copy(email_start + 1, end - email_start - 1));
				AuthorEntry entry = { str_trim(str_copy(author, email_start - author)), email };
				vec_add(author_list, entry);
			}
			else
			{
				AuthorEntry entry = { str_trim(str_dup(author)), NULL };
				vec_add(author_list, entry);
			}
		}
		target->authors = author_list;
	}
	// "Before compilation" execution
	APPEND_STRING_LIST(&target->exec, "exec");

	// CFlags
	target->cflags = get_cflags(context, json, target->cflags);

	// C source dirs.
	APPEND_STRING_LIST(&target->csource_dirs, "c-sources");

	// C include dirs.
	APPEND_STRING_LIST(&target->cinclude_dirs, "c-include-dirs");

	// Sources
	APPEND_STRING_LIST(&target->source_dirs, "sources");

	// Test sources
	APPEND_STRING_LIST(&target->test_source_dirs, "test-sources");

	// Linked-libraries - libraries to add at link time
	APPEND_STRING_LIST(&target->linker_libs, "linked-libraries");

	// linker-search-paths libs dir - libraries to add at link time

	APPEND_STRING_LIST(&target->linker_libdirs, "linker-search-paths");

	// link-args - link args to add at link time
	APPEND_STRING_LIST(&target->link_args, "link-args");

	// dependency-search-paths - path to search for libraries
	APPEND_STRING_LIST(&target->libdirs, "dependency-search-paths");

	// Dependencies
	APPEND_STRING_LIST(&target->libs, "dependencies");

	FOREACH(const char *, name, target->libs)
	{
		if (!str_is_valid_lowercase_name(name))
		{
			char *name_copy = strdup(name);
			str_elide_in_place(name_copy, 32);
			error_exit("Error reading %s: invalid library target name '%s' – it should only contain alphanumerical letters and '_'.", context.file, name_copy);
		}
	}

	// debug-info
	static const char *debug_infos[3] = {
			[DEBUG_INFO_FULL] = "full",
			[DEBUG_INFO_NONE] = "none",
			[DEBUG_INFO_LINE_TABLES] = "line-tables"
	};

	DebugInfo info = GET_SETTING(DebugInfo, "debug-info", debug_infos, "one of 'full' 'line-table' or 'none'.");
	if (info > -1) target->debug_info = info;

	// Optimization Level
	target->optlevel = GET_SETTING(OptimizationLevel, "optlevel", optlevels, "`none`, `less`, `more`, `max`.");

	// Size optimization
	target->optsize = GET_SETTING(SizeOptimizationLevel, "optsize", optsizes, "`none`, `small`, `tiny`.");

	target->loop_vectorization = (AutoVectorization)get_valid_bool(context, json, "loop-vectorize", target->loop_vectorization);
	target->slp_vectorization = (AutoVectorization)get_valid_bool(context, json, "slp-vectorize", target->slp_vectorization);
	target->unroll_loops = (UnrollLoops)get_valid_bool(context, json, "unroll-loops", target->unroll_loops);
	target->merge_functions = (MergeFunctions)get_valid_bool(context, json, "merge-functions", target->merge_functions);

	static const char *opt_settings[8] = {
			[OPT_SETTING_O0] = "O0",
			[OPT_SETTING_O1] = "O1",
			[OPT_SETTING_O2] = "O2",
			[OPT_SETTING_O3] = "O3",
			[OPT_SETTING_O4] = "O4",
			[OPT_SETTING_O5] = "O5",
			[OPT_SETTING_OSMALL] = "Os",
			[OPT_SETTING_OTINY] = "Oz"
	};
	OptimizationSetting opt = GET_SETTING(OptimizationSetting, "opt", opt_settings, "'O0', 'O1' etc.");
	if (opt != OPT_SETTING_NOT_SET) target->optsetting = opt;

	// --quiet
	target->quiet = get_valid_bool(context, json, "quiet", target->quiet);

	// Safety level
	target->feature.safe_mode = (SafetyLevel)get_valid_bool(context, json, "safe", target->feature.safe_mode);

	// Backtrace
	target->show_backtrace = (ShowBacktrace) get_valid_bool(context, json, "show-backtrace", target->show_backtrace);

	// Panic level
	target->feature.panic_level = (PanicLevel)get_valid_bool(context, json, "panic-msg",
	                                                         target->feature.panic_level);

	// Overridden name
	target->output_name = get_optional_string(context, json, "name");

	// Single module
	target->single_module = (SingleModule) get_valid_bool(context, json, "single-module", target->single_module);

	// Memory environment for memory constrained environments.
	MemoryEnvironment env = GET_SETTING(MemoryEnvironment, "memory-env", memory_environment, "one of 'normal', 'small', 'tiny' or 'none'.");
	if (env != MEMORY_ENV_NOT_SET) target->memory_environment = env;

	// Symtab
	long symtab_size = get_valid_integer(context, json, "symtab", false);
	if (symtab_size > 0)
	{
		if (symtab_size < 1024)
		{
			error_exit("Error reading %s: symtab was less than 1024.", context.file);
		}
		if (symtab_size > MAX_SYMTAB_SIZE)
		{
			error_exit("Error reading %s: symtab may not exceed %d.", context.file, MAX_SYMTAB_SIZE);
		}
		target->symtab_size = (uint32_t)symtab_size;
	}

	// Vector size
	long vector_size = get_valid_integer(context, json, "max-vector-size", false);
	if (vector_size > 0)
	{
		if (vector_size < 128)
		{
			error_exit("Error reading %s: max vector size was less than 128.", context.file);
		}
		if (vector_size > MAX_VECTOR_WIDTH)
		{
			error_exit("Error reading %s: max vector width may not exceed %d.", context.file, MAX_VECTOR_WIDTH);
		}
		target->max_vector_size = (uint32_t)vector_size;
	}

	// Target
	const char *arch_os_string = get_optional_string(context, json, "target");
	if (arch_os_string)
	{
		ArchOsTarget arch_os = arch_os_target_from_string(arch_os_string);
		if (arch_os == ARCH_OS_TARGET_DEFAULT) error_exit("Error reading %s: %s target was not valid.", context.file, context.target);
		target->arch_os_target = arch_os;
	}

	// Reloc
	RelocModel reloc = GET_SETTING(RelocModel, "reloc", reloc_models, "'none', 'pic', 'PIC', 'pie' or 'PIE'.");
	if (reloc != RELOC_DEFAULT) target->reloc_model = reloc;

	// Sanitize
	SanitizeMode sanitize_mode = GET_SETTING(SanitizeMode, "sanitize", sanitize_modes, "'none', 'address', 'memory' or 'thread'.");
	switch (sanitize_mode)
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

	// Cpu
	target->cpu = get_string(context, json, "cpu", target->cpu);

	// WinCRT
	WinCrtLinking wincrt = GET_SETTING(WinCrtLinking, "wincrt", wincrt_linking, "'none', 'static', 'static-debug, 'dynamic-debug' or 'dynamic'.");
	if (wincrt != WIN_CRT_DEFAULT) target->win.crt_linking = wincrt;

	// fp-math
	FpOpt fpmath = GET_SETTING(FpOpt, "fp-math", fp_math, "`strict`, `relaxed` or `fast`.");
	if (fpmath > -1) target->feature.fp_math = fpmath;

	const char **features = get_optional_string_array(context, json, "features");
	if (features)
	{
		FOREACH(const char *, feature, features)
		{
			if (!str_is_valid_constant(feature))
			{
				error_exit("Error reading 'features': '%s' is not a valid feature name.", feature);
			}
			vec_add(target->feature_list, feature);
		}
	}

	// x86vec
	X86VectorCapability x86vec = GET_SETTING(X86VectorCapability, "x86vec", x86_vector_capability, "`none`, `native`, `mmx`, `sse`, `avx` or `avx512`.");
	if (x86vec > -1) target->feature.x86_vector_capability = x86vec;

	// x86cpu
	X86CpuSet x86cpu = GET_SETTING(X86CpuSet, "x86cpu", x86_cpu_set, "`baseline`, `ssse3`, `sse4`, `avx1`, `avx2-v1`, `avx2-v2`, `avx512` or `native`.");
	if (x86cpu > X86CPU_DEFAULT) target->feature.x86_cpu_set = x86cpu;

	// riscv-cpu
	RiscvCpuSet riscv_cpu = GET_SETTING(RiscvCpuSet, "riscv-cpu", riscv_cpu_set, "`rvi`, `rvimac`, `rvimafc`, `rvgc` or `rvgcv`.");
	if (riscv_cpu > RISCV_CPU_DEFAULT) target->feature.riscv_cpu_set = riscv_cpu;

	// riscv-abi
	RiscvAbi riscv_abi_val = GET_SETTING(RiscvAbi, "riscv-abi", riscv_abi, "`int-only`, `float` or `double`.");
	if (riscv_abi_val != RISCV_ABI_DEFAULT) target->feature.riscv_abi = riscv_abi_val;

	// win-debug
	WinDebug win_debug = GET_SETTING(WinDebug , "win-debug", win_debug_type, "`codeview` or `dwarf`.");
	if (win_debug != WIN_DEBUG_DEFAULT) target->feature.win_debug = win_debug;

	// winsdk
	target->win.vs_dirs = get_string(context, json, "win-vs-dirs", target->win.vs_dirs);


	// winsdk
	target->win.sdk = get_string(context, json, "winsdk", target->win.sdk);

	// windef
	target->win.def = get_string(context, json, "windef", target->win.def);

	// macossdk
	target->macos.sysroot = get_string(context, json, "macossdk", target->macos.sysroot);

	// macos-min-version
	target->macos.min_version = get_string(context, json, "macos-min-version", target->macos.min_version);

	// macos-sdk-version
	target->macos.sdk_version = get_string(context, json, "macos-sdk-version", target->macos.sdk_version);

	// Linux crt
	target->linuxpaths.crt = get_string(context, json, "linux-crt", target->linuxpaths.crt);

	// Linux crtbegin
	target->linuxpaths.crtbegin = get_string(context, json, "linux-crtbegin", target->linuxpaths.crtbegin);

	// linux-libc
	LinuxLibc linux_libc = GET_SETTING(LinuxLibc, "linux-libc", linuxlibc, "`gnu`, `musl` or `host`.");
	if (linux_libc > -1) target->linuxpaths.libc = linux_libc;

	// version
	target->version = get_string(context, json, "version", target->version);

	// langrev
	target->langrev = get_string(context, json, "langrev", target->langrev);

	// panicfn
	target->panicfn = get_string(context, json, "panicfn", target->panicfn);

	// testfn
	target->testfn = get_string(context, json, "testfn", target->testfn);

	// benchfn
	target->benchfn = get_string(context, json, "benchfn", target->benchfn);

	// link-libc
	target->link_libc = (LinkLibc) get_valid_bool(context, json, "link-libc", target->link_libc);

	// strip-unused
	target->strip_unused = (StripUnused) get_valid_bool(context, json, "strip-unused", target->strip_unused);

	// linker
	const char *linker_selection = get_optional_string(context, json, "linker");
	if (linker_selection)
	{
		if (str_eq("cc", linker_selection))
		{
			target->linker_type = LINKER_TYPE_CC;
			target->custom_linker_path = NULL;
		}
		else if (str_eq("builtin", linker_selection))
		{
			target->linker_type = LINKER_TYPE_BUILTIN;
			target->custom_linker_path = NULL;
		}
		else
		{
			target->linker_type = LINKER_TYPE_CUSTOM;
			target->custom_linker_path = linker_selection;
		}
	}

	// no-entry
	target->no_entry = get_valid_bool(context, json, "no-entry", target->no_entry);

	// use-stdlib
	target->use_stdlib = (UseStdlib) get_valid_bool(context, json, "use-stdlib", target->use_stdlib);

	// emit-stdlib
	target->emit_stdlib = (EmitStdlib) get_valid_bool(context, json, "emit-stdlib", target->emit_stdlib);

	// implement-stdlib
	target->custom_libc = (CustomLibc) get_valid_bool(context, json, "custom-libc", target->custom_libc);

	// single-module
	target->single_module = (SingleModule) get_valid_bool(context, json, "single-module", target->single_module);

	// Trap on wrap
	target->feature.trap_on_wrap = get_valid_bool(context, json, "trap-on-wrap", target->feature.trap_on_wrap);

	// Use the fact that they correspond to 0, 1, -1
	target->feature.x86_struct_return = get_valid_bool(context, json, "x86-stack-struct-return",
	                                                   target->feature.x86_struct_return);

	// Soft float
	target->feature.soft_float = get_valid_bool(context, json, "soft-float", target->feature.soft_float);

	// Win64 simd feature switch
	target->feature.pass_win64_simd_as_arrays = get_valid_bool(context, json, "win64-simd-array",
	                                                           target->feature.pass_win64_simd_as_arrays);
}

static void duplicate_prop(const char ***prop_ref)
{
	if (!*prop_ref) return;
	const char **copy = NULL;
	FOREACH(const char *, str, *prop_ref)
	{
		vec_add(copy, str);
	}
	*prop_ref = copy;
}
static void project_add_target(BuildParseContext context, Project *project, BuildTarget *default_target, JSONObject *json,
                               const char *type, TargetType target_type)
{
	ASSERT(json->type == J_OBJECT);
	BuildTarget *target = CALLOCS(BuildTarget);
	*target = *default_target;
	duplicate_prop(&target->args);
	duplicate_prop(&target->csource_dirs);
	duplicate_prop(&target->csources);
	duplicate_prop(&target->cinclude_dirs);
	duplicate_prop(&target->exec);
	duplicate_prop(&target->feature_list);
	duplicate_prop(&target->sources);
	duplicate_prop(&target->source_dirs);
	duplicate_prop(&target->test_source_dirs);
	duplicate_prop(&target->libdirs);
	duplicate_prop(&target->libs);
	duplicate_prop(&target->linker_libdirs);
	duplicate_prop(&target->linker_libs);
	duplicate_prop(&target->link_args);

	vec_add(project->targets, target);
	target->name = context.target;
	target->type = target_type;
	FOREACH(BuildTarget *, other_target, project->targets)
	{
		if (other_target == target) continue;
		if (strcmp(other_target->name, target->name) == 0)
		{
			error_exit("More %s contained more than one target with the name %s. Please make all target names unique.", context.file, target->name);
		}
	}
	context.target = str_printf("%s %s", type, context.target);
	load_into_build_target(context, json, target);
}

static void project_add_targets(const char *filename, Project *project, JSONObject *project_data)
{
	ASSERT(project_data->type == J_OBJECT);

	BuildTarget default_target = default_build_target;
	load_into_build_target((BuildParseContext) { filename, NULL }, project_data, &default_target);
	JSONObject *targets_json = json_map_get(project_data, "targets");
	if (!targets_json)
	{
		error_exit("No targets found in project.");
	}
	if (targets_json->type != J_OBJECT)
	{
		error_exit("'targets' did not contain map of targets.");
	}
	FOREACH_IDX(i, JSONObject *, object, targets_json->members)
	{
		const char *key = targets_json->keys[i];
		if (object->type != J_OBJECT)
		{
			error_exit("Invalid data in target '%s'", key);
		}
		BuildParseContext context = { filename, key };
		int type = get_valid_string_setting(context, object, "type", targets, 0, ELEMENTLEN(targets), "a target type like 'executable' or 'static-lib'");
		if (type < 0) error_exit("Target %s did not contain 'type' key.", key);
		project_add_target(context, project, &default_target, object, target_desc[type], type);
	}
}


/**
 * Select the project target. This may be given a target, if so it is looked for.
 * If the project has no targets, or the optional target can't be found, issue an error.
 *
 * @param project the project to look in.
 * @param optional_target the selected target, may be NULL.
 * @return the target if one is provided, otherwise the default target.
 */
BuildTarget *project_select_target(const char *filename, Project *project, const char *optional_target)
{
	if (!vec_size(project->targets))
	{
		error_exit("No targets could be found in %s. Please define at least one target, for example an 'executable' and try again.", filename);
	}
	if (!optional_target)
	{
		return project->targets[0];
	}
	FOREACH(BuildTarget *, target, project->targets)
	{
		if (str_eq(target->name, optional_target)) return target;
	}
	error_exit("No build target named '%s' was found in %s. Was it misspelled?", optional_target, filename);
	UNREACHABLE
}

JSONObject *project_json_load(const char **filename_ref)
{
	// Locate the project.json
	file_find_top_dir();
	const char *filename = *filename_ref = file_exists(PROJECT_JSON5) ? PROJECT_JSON5 : PROJECT_JSON;

	size_t size;
	char *read = file_read_all(filename, &size);

	JsonParser parser;
	json_init_string(&parser, read);
	JSONObject *json = json_parse(&parser);

	if (parser.error_message)
	{
		error_exit("Error on line %d reading '%s':'%s'", parser.line, filename, parser.error_message);
	}
	if (!json || json->type != J_OBJECT)
	{
		error_exit("Expected a map of targets in '%s'.", filename);
	}

	return json;
}

Project *project_load(const char **filename_ref)
{
	Project *project = CALLOCS(Project);
	JSONObject *json = project_json_load(filename_ref);
	project_add_targets(*filename_ref, project, json);
	return project;
}
