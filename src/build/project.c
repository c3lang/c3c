// Copyright (c) 2020-2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "build_internal.h"
#include "utils/json.h"

const char *project_default_keys[][2] = {
		{"authors", "Authors, optionally with email."},
		{"benchfn", "Override the benchmark function."},
		{"c-sources", "Set the C sources to be compiled."},
		{"c-include-dirs", "Set the include directories for C sources."},
		{"cc", "Set C compiler (defaults to 'cc')."},
		{"cflags", "C compiler flags."},
		{"cpu", "CPU name, used for optimizations in the compiler backend."},
		{"debug-info", "Debug level: none, line-tables, full."},
		{"dependencies", "C3 library dependencies for all targets."},
		{"dependency-search-paths", "The C3 library search paths."},
		{"exec", "Scripts run for all targets.."},
		{"features", "Features enabled for all targets."},
		{"fp-math", "Set math behaviour: `strict`, `relaxed` or `fast`."},
		{"langrev", "Version of the C3 language used."},
		{"linked-libraries", "Libraries linked by the linker for all targets."},
		{"linker", "'builtin' for the builtin linker, 'cc' for the system linker or <path> to a custom compiler."},
		{"linker-search-paths", "Linker search paths."},
		{"link-args", "Linker arguments for all targets."},
		{"link-libc", "Link libc (default: true)."},
		{"macos-min-version", "Set the minimum MacOS version to compile for."},
		{"macos-sdk-version", "Set the MacOS SDK compiled for." },
		{"macossdk", "Set the directory for the MacOS SDK for cross compilation."},
		{"memory-env", "Set the memory environment: normal, small, tiny, none."},
		{"no-entry", "Do not generate (or require) a main function."},
		{"opt", "Optimization setting: O0, O1, O2, O3, O4, O5, Os, Oz."},
		{"optlevel", "Code optimization level: none, less, more, max."},
		{"optsize", "Code size optimization: none, small, tiny."},
		{"output", "Output location, relative to project file."},
		{"panicfn", "Override the panic function."},
		{"panic-msg", "Turn panic message output on or off."},
		{"reloc", "Relocation model: none, pic, PIC, pie, PIE."},
		{"safe", "Set safety (contracts, runtime bounds checking, null pointer checks etc) on or off."},
		{"sanitize", "Enable sanitizer: none, address, memory, thread."},
		{"show-backtrace", "Print backtrace on signals."},
		{"script-dir", "The directory where 'exec' is run."},
		{"single-module", "Compile all modules together, enables more inlining."},
		{"soft-float", "Output soft-float functions."},
		{"sources", "Paths to project sources for all targets."},
		{"test-sources", "Paths to project test sources for all targets."},
		{"strip-unused", "Strip unused code and globals from the output. (default: true)"},
		{"symtab", "Sets the preferred symtab size."},
		{"target", "Compile for a particular architecture + OS target."},
		{"targets", "Set of targets for the project."},
		{"testfn", "Override the test function."},
		{"trap-on-wrap", "Make signed and unsigned integer overflow generate a panic rather than wrapping."},
		{"use-stdlib", "Include the standard library (default: true)."},
		{"version", "Version using semantic versioning."},
		{"vendor", "Vendor specific extensions, ignored by c3c."},
		{"warnings", "Warnings used for all targets."},
		{"wincrt", "Windows CRT linking: none, static-debug, static, dynamic-debug (default if debug info enabled), dynamic (default)."},
		{"windef", "Windows def file, used as an alternative to dllexport when exporting a DLL."},
		{"winsdk", "Set the path to Windows system library files for cross compilation."},
		{"x86cpu", "Set general level of x64 cpu: baseline, ssse3, sse4, avx1, avx2-v1, avx2-v2 (Skylake/Zen1+), avx512 (Icelake/Zen4+), native."},
		{"x86vec", "Set max type of vector use: none, mmx, sse, avx, avx512, native."},
		{"x86-stack-struct-return", "Return structs on the stack for x86."},
};

const int project_default_keys_count = ELEMENTLEN(project_default_keys);

const char* project_deprecated_target_keys[] = {
		"c-source-add", "cflags-add", "dependencies-add", "dependency-search-paths-add", "exec-add",
		"linked-libraries", "linker-search-paths", "link-args-add", "sources-add"
};
const char* project_target_keys[][2] = {
		{"benchfn", "Override the benchmark function."},
		{"c-sources", "Additional C sources to be compiled for the target."},
		{"c-sources-override", "C sources to be compiled, overriding global settings."},
		{"c-include-dirs", "C sources include directories for the target."},
		{"c-include-dirs-override", "Additional C sources include directories for the target, overriding global settings."},
		{"cc", "Set C compiler (defaults to 'cc')."},
		{"cflags", "Additional C compiler flags for the target."},
		{"cflags-override", "C compiler flags for the target, overriding global settings."},
		{"cpu", "CPU name, used for optimizations in the compiler backend."},
		{"debug-info", "Debug level: none, line-tables, full."},
		{"dependencies", "Additional C3 library dependencies for the target."},
		{"dependencies-override", "C3 library dependencies for this target, overriding global settings."},
		{"dependency-search-paths", "Additional C3 library search paths for the target."},
		{"dependency-search-paths-override", "C3 library search paths for this target, overriding global settings."},
		{"exec", "Additional scripts to run for the target."},
		{"exec-override", "Scripts to run for this target, overriding global settings."},
		{"features", "Features enabled for all targets."},
		{"fp-math", "Set math behaviour: `strict`, `relaxed` or `fast`."},
		{"langrev", "Version of the C3 language used."},
		{"linked-libraries", "Additional libraries linked by the linker for the target."},
		{"linked-libraries-override", "Libraries linked by the linker for this target, overriding global settings."},
		{"linker", "'builtin' for the builtin linker, 'cc' for the system linker or <path> to a custom compiler."},
		{"linker-search-paths", "Additional linker search paths for the target."},
		{"linker-search-paths-override", "Linker search paths for this target, overriding global settings."},
		{"link-args", "Additional linker arguments for the target."},
		{"link-args-override", "Linker arguments for this target, overriding global settings."},
		{"link-libc", "Link libc (default: true)."},
		{"macos-min-version", "Set the minimum MacOS version to compile for."},
		{"macos-sdk-version", "Set the MacOS SDK compiled for." },
		{"macossdk", "Set the directory for the MacOS SDK for cross compilation."},
		{"memory-env", "Set the memory environment: normal, small, tiny, none."},
		{"name", "Set the name to be different from the target name."},
		{"no-entry", "Do not generate (or require) a main function."},
		{"opt", "Optimization setting: O0, O1, O2, O3, O4, O5, Os, Oz."},
		{"optlevel", "Code optimization level: none, less, more, max."},
		{"optsize", "Code size optimization: none, small, tiny."},
		{"output", "Output location, relative to project file."},
		{"panicfn", "Override the panic function."},
		{"panic-msg", "Turn panic message output on or off."},
		{"reloc", "Relocation model: none, pic, PIC, pie, PIE."},
		{"safe", "Set safety (contracts, runtime bounds checking, null pointer checks etc) on or off."},
		{"sanitize", "Enable sanitizer: none, address, memory, thread."},
		{"script-dir", "The directory where 'exec' is run."},
		{"single-module", "Compile all modules together, enables more inlining."},
		{"show-backtrace", "Print backtrace on signals."},
		{"soft-float", "Output soft-float functions."},
		{"sources", "Additional paths to project sources for the target."},
		{"sources-override", "Paths to project sources for this target, overriding global settings."},
		{"test-sources", "Additional paths to project test sources for the target."},
		{"test-sources-override", "Paths to project test sources for this target, overriding global settings."},
		{"strip-unused", "Strip unused code and globals from the output. (default: true)"},
		{"symtab", "Sets the preferred symtab size."},
		{"target", "Compile for a particular architecture + OS target."},
		{"testfn", "Override the test function."},
		{"trap-on-wrap", "Make signed and unsigned integer overflow generate a panic rather than wrapping."},
		{"type", "Type of output, one of 'executable', 'static-lib', 'dynamic-lib', 'benchmark', 'test', 'object-files' and 'prepare'." },
		{"use-stdlib", "Include the standard library (default: true)."},
		{"vendor", "Vendor specific extensions, ignored by c3c."},
		{"version", "Version using semantic versioning."},
		{"warnings", "Warnings used for all targets."},
		{"wincrt", "Windows CRT linking: none, static-debug, static, dynamic-debug (default if debug info enabled), dynamic (default)."},
		{"windef", "Windows def file, used as an alternative to dllexport when exporting a DLL."},
		{"winsdk", "Set the path to Windows system library files for cross compilation."},
		{"x86cpu", "Set general level of x64 cpu: baseline, ssse3, sse4, avx1, avx2-v1, avx2-v2 (Skylake/Zen1+), avx512 (Icelake/Zen4+), native."},
		{"x86vec", "Set max type of vector use: none, mmx, sse, avx, avx512, native."},
		{"x86-stack-struct-return", "Return structs on the stack for x86."},
};

const int project_target_keys_count = ELEMENTLEN(project_target_keys);
const int project_deprecated_target_keys_count = ELEMENTLEN(project_deprecated_target_keys);

#define MAX_SYMTAB_SIZE (1024 * 1024)
#define GET_SETTING(type__, key__, strings__, comment__) \
  (type__)get_valid_string_setting(filename, target_name, json, key__, strings__, 0, ELEMENTLEN(strings__), comment__)

// Json -> target / default target
static void load_into_build_target(const char *filename, JSONObject *json, const char *target_name,
                                   BuildTarget *target)
{
	if (target_name)
	{
		check_json_keys(project_target_keys, project_target_keys_count, project_deprecated_target_keys, project_deprecated_target_keys_count, json, target_name, "--list-project-properties");
	}
	else
	{
		check_json_keys(project_default_keys, project_default_keys_count, NULL, 0, json, "default target", "--list-project-properties");
	}

	// The default c compiler name
	target->cc = get_string(filename, target_name, json, "cc", target->cc);

	// Where to find and execute the scripts
	target->script_dir = get_string(filename, target_name, json, "script-dir", target->script_dir);

	// The output directory
	target->output_dir = get_string(filename, target_name, json, "output", target->output_dir);

	// "Before compilation" execution
	get_list_append_strings(filename, target_name, json, &target->exec, "exec", "exec-override", "exec-add");

	// CFlags
	target->cflags = get_cflags(filename, target_name, json, target->cflags);
	
	// C source dirs.
	get_list_append_strings(filename, target_name, json, &target->csource_dirs, "c-sources", "c-sources-override", "c-sources-add");

	// C include dirs.
	get_list_append_strings(filename, target_name, json, &target->cinclude_dirs, "c-include-dirs", "c-include-dirs-override", "c-include-dirs-add");

	// Sources
	get_list_append_strings(filename, target_name, json, &target->source_dirs, "sources", "sources-override", "sources-add");

	// Test sources
	get_list_append_strings(filename, target_name, json, &target->test_source_dirs, "test-sources", "test-sources-override", "test-sources-add");

	// Linked-libraries - libraries to add at link time
	get_list_append_strings(filename, target_name, json, &target->linker_libs, "linked-libraries", "linked-libraries-override", "linked-libraries-add");

	// linker-search-paths libs dir - libraries to add at link time
	get_list_append_strings(filename, target_name, json, &target->linker_libdirs, "linker-search-paths", "linker-search-paths-override", "linker-search-paths-add");

	// link-args - link args to add at link time
	get_list_append_strings(filename, target_name, json, &target->link_args, "link-args", "link-args-override", "link-args-add");

	// dependency-search-paths - path to search for libraries
	get_list_append_strings(filename, target_name, json, &target->libdirs, "dependency-search-paths", "dependency-search-paths-override", "dependency-search-paths-add");

	// Dependencies
	get_list_append_strings(filename, target_name, json, &target->libs, "dependencies", "dependencies-override", "dependencies-add");
	FOREACH(const char *, name, target->libs)
	{
		if (!str_is_valid_lowercase_name(name))
		{
			char *name_copy = strdup(name);
			str_ellide_in_place(name_copy, 32);
			error_exit("Error reading %s: invalid library target '%s'.", filename, name_copy);
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

	// Safety level
	target->feature.safe_mode = (SafetyLevel)get_valid_bool(filename, target_name, json, "safe", target->feature.safe_mode);

	// Backtrace
	target->show_backtrace = (ShowBacktrace) get_valid_bool(filename, target_name, json, "show-backtrace", target->show_backtrace);

	// Panic level
	target->feature.panic_level = (PanicLevel)get_valid_bool(filename, target_name, json, "panic-msg",
	                                                         target->feature.panic_level);

	// Overridden name
	target->output_name = get_optional_string(filename, target_name, json, "name");

	// Single module
	target->single_module = (SingleModule) get_valid_bool(filename, target_name, json, "single-module", target->single_module);

	// Memory environment for memory constrained environments.
	MemoryEnvironment env = GET_SETTING(MemoryEnvironment, "memory-env", memory_environment, "one of 'normal', 'small', 'tiny' or 'none'.");
	if (env != MEMORY_ENV_NOT_SET) target->memory_environment = env;

	// Symtab
	long symtab_size = get_valid_integer(json, "symtab", target_name, false);
	if (symtab_size > 0)
	{
		if (symtab_size < 1024)
		{
			error_exit("Error reading %s: %s symtab was less than 1024.", filename, target_name);
		}
		if (symtab_size > MAX_SYMTAB_SIZE)
		{
			error_exit("Error reading %s: %s symtab may not exceed %d.", filename, target_name, MAX_SYMTAB_SIZE);
		}
		target->symtab_size = (uint32_t)symtab_size;
	}

	// Target
	const char *arch_os_string = get_optional_string(filename, target_name, json, "target");
	if (arch_os_string)
	{
		ArchOsTarget arch_os = arch_os_target_from_string(arch_os_string);
		if (arch_os == ARCH_OS_TARGET_DEFAULT) error_exit("Error reading %s: %s target was not valid.", filename, target_name);
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
		default: UNREACHABLE;
	}

	// Cpu
	target->cpu = get_string(filename, target_name, json, "cpu", target->cpu);

	// WinCRT
	WinCrtLinking wincrt = GET_SETTING(WinCrtLinking, "wincrt", wincrt_linking, "'none', 'static-debug', 'staticdebug, 'dynamic-debug' or 'dynamic'.");
	if (wincrt != WIN_CRT_DEFAULT) target->win.crt_linking = wincrt;

	// fp-math
	FpOpt fpmath = GET_SETTING(FpOpt, "fp-math", fp_math, "`strict`, `relaxed` or `fast`.");
	if (fpmath > -1) target->feature.fp_math = fpmath;

	const char **features = get_optional_string_array(filename, target_name, json, "features");
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

	// x86vec
	X86CpuSet x86cpu = GET_SETTING(X86CpuSet, "x86cpu", x86_cpu_set, "`baseline`, `ssse3`, `sse4`, `avx1`, `avx2-v1`, `avx2-v2`, `avx512` or `native`.");
	if (x86cpu > X86CPU_DEFAULT) target->feature.x86_cpu_set = x86cpu;

	// riscvfloat
	RiscvFloatCapability riscv_float = GET_SETTING(RiscvFloatCapability, "riscvfloat", riscv_capability, "`none`, `float` or `double`.");
	if (riscv_float != RISCVFLOAT_DEFAULT) target->feature.riscv_float_capability = riscv_float;

	// winsdk
	target->win.vs_dirs = get_string(filename, target_name, json, "win-vs-dirs", target->win.vs_dirs);

	// winsdk
	target->win.sdk = get_string(filename, target_name, json, "winsdk", target->win.sdk);

	// windef
	target->win.def = get_string(filename, target_name, json, "windef", target->win.def);

	// macossdk
	target->macos.sysroot = get_string(filename, target_name, json, "macossdk", target->macos.sysroot);

	// macos-min-version
	target->macos.min_version = get_string(filename, target_name, json, "macos-min-version", target->macos.min_version);

	// macos-sdk-version
	target->macos.sdk_version = get_string(filename, target_name, json, "macos-sdk-version", target->macos.sdk_version);

	// Linux crt
	target->linuxpaths.crt = get_string(filename, target_name, json, "linux-crt", target->linuxpaths.crt);

	// Linux crtbegin
	target->linuxpaths.crtbegin = get_string(filename, target_name, json, "linux-crtbegin", target->linuxpaths.crtbegin);

	// version
	target->version = get_string(filename, target_name, json, "version", target->version);

	// langrev
	target->langrev = get_string(filename, target_name, json, "langrev", target->langrev);

	// panicfn
	target->panicfn = get_string(filename, target_name, json, "panicfn", target->panicfn);

	// testfn
	target->testfn = get_string(filename, target_name, json, "testfn", target->testfn);

	// testfn
	target->benchfn = get_string(filename, target_name, json, "benchfn", target->benchfn);

	// link-libc
	target->link_libc = (LinkLibc) get_valid_bool(filename, target_name, json, "link-libc", target->link_libc);

	// strip-unused
	target->strip_unused = (StripUnused) get_valid_bool(filename, target_name, json, "strip-unused", target->strip_unused);

	// linker
	const char *linker_selection = get_optional_string(filename, target_name, json, "linker");
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
	target->no_entry = get_valid_bool(filename, target_name, json, "no-entry", target->no_entry);

	// use-stdlib
	target->use_stdlib = (UseStdlib) get_valid_bool(filename, target_name, json, "use-stdlib", target->use_stdlib);

	// emit-stdlib
	target->emit_stdlib = (EmitStdlib) get_valid_bool(filename, target_name, json, "emit-stdlib", target->emit_stdlib);

	// single-module
	target->single_module = (SingleModule) get_valid_bool(filename, target_name, json, "single-module", target->single_module);

	// Trap on wrap
	target->feature.trap_on_wrap = get_valid_bool(filename, target_name, json, "trap-on-wrap", target->feature.trap_on_wrap);

	// Use the fact that they correspond to 0, 1, -1
	target->feature.x86_struct_return = get_valid_bool(filename, target_name, json, "x86-stack-struct-return",
	                                                   target->feature.x86_struct_return);

	// Soft float
	target->feature.soft_float = get_valid_bool(filename, target_name, json, "soft-float", target->feature.soft_float);

	// Win64 simd feature switch
	target->feature.pass_win64_simd_as_arrays = get_valid_bool(filename, target_name, json, "win64-simd-array",
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
static void project_add_target(const char *filename, Project *project, BuildTarget *default_target, JSONObject *json,
                               const char *name, const char *type, TargetType target_type)
{
	ASSERT0(json->type == J_OBJECT);
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
	target->name = name;
	target->type = target_type;
	FOREACH(BuildTarget *, other_target, project->targets)
	{
		if (other_target == target) continue;
		if (strcmp(other_target->name, target->name) == 0)
		{
			error_exit("More %s contained more than one target with the name %s. Please make all target names unique.", filename, target->name);
		}
	}
	type = str_printf("%s %s", type, target->name);
	load_into_build_target(filename, json, type, target);
}

static void project_add_targets(const char *filename, Project *project, JSONObject *project_data)
{
	ASSERT0(project_data->type == J_OBJECT);

	BuildTarget default_target = default_build_target;
	load_into_build_target(filename, project_data, NULL, &default_target);
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
		int type = get_valid_string_setting(filename, NULL, object, "type", targets, 0, ELEMENTLEN(targets), "a target type like 'executable' or 'static-lib'");
		if (type < 0) error_exit("Target %s did not contain 'type' key.", key);
		project_add_target(filename, project, &default_target, object, key, target_desc[type], type);
	}
}

/**
 * Grab the default target, this will be the executable if one exists, otherwise
 * it's the first target in the list.
 *
 * @param project
 * @return the selected build target.
 */
static BuildTarget *project_select_default_target(Project *project)
{
	FOREACH(BuildTarget *, target, project->targets)
	{
		if (target->type == TARGET_TYPE_EXECUTABLE) return target;
	}
	return project->targets[0];
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
		return project_select_default_target(project);
	}
	FOREACH(BuildTarget *, target, project->targets)
	{
		if (str_eq(target->name, optional_target)) return target;
	}
	error_exit("No build target named '%s' was found in %s. Was it misspelled?", optional_target, filename);
}

Project *project_load(const char **filename_ref)
{
	Project *project = CALLOCS(Project);
	size_t size;
	const char *filename = *filename_ref = file_exists(PROJECT_JSON5) ? PROJECT_JSON5 : PROJECT_JSON;
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
	project_add_targets(filename, project, json);
	return project;
}
