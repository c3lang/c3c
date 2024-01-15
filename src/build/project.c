// Copyright (c) 2020-2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include <math.h>
#include "build_internal.h"
#define MAX_SYMTAB_SIZE (1024 * 1024)

const char *project_default_keys[][2] = {
		{"authors", "Authors, optionally with email."},
		{"benchfn", "Override the benchmark function."},
		{"c-sources", "Set the C sources to be compiled."},
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
		{"linker-search-paths", "Linker search paths."},
		{"link-args", "Linker arguments for all targets."},
		{"link-libc", "Link libc (default: true)."},
		{"macossdk", "Set the directory for the MacOS SDK for cross compilation."},
		{"memory-env", "Set the memory environment: normal, small, tiny, none."},
		{"no-entry", "Do not generate (or require) a main function."},
		{"opt", "Link libc (default: true)."},
		{"optlevel", "Code optimization level: none, less, more, max."},
		{"optsize", "Code size optimization: none, small, tiny."},
		{"output", "Output location, relative to project file."},
		{"panicfn", "Override the panic function."},
		{"reloc", "Relocation model: none, pic, PIC, pie, PIE."},
		{"safe", "Set safety (contracts, runtime bounds checking, null pointer checks etc) on or off."},
		{"script-dir", "The directory where 'exec' is run."},
		{"single-module", "Compile all modules together, enables more inlining."},
		{"soft-float", "Output soft-float functions."},
		{"sources", "Paths to project sources for all targets."},
		{"strip-unused", "Strip unused code and globals from the output. (default: true)"},
		{"symtab", "Sets the preferred symtab size."},
		{"system-linker", "Use the system linker (default: no for cross compilation, yes otherwise)."},
		{"target", "Compile for a particular architecture + OS target."},
		{"targets", "Set of targets for the project."},
		{"testfn", "Override the test function."},
		{"trap-on-wrap", "Make signed and unsigned integer overflow generate a panic rather than wrapping."},
		{"use-stdlib", "Include the standard library (default: true)."},
		{"version", "Version using semantic versioning."},
		{"warnings", "Warnings used for all targets."},
		{"wincrt", "Windows CRT linking: none, static, dynamic (default)."},
		{"windef", "Windows def file, used as an alternative to dllexport when exporting a DLL."},
		{"winsdk", "Set the path to Windows system library files for cross compilation."},
		{"x86cpu", "Set general level of x64 cpu: baseline, ssse3, sse4, avx1, avx2-v1, avx2-v2 (Skylake/Zen1+), avx512 (Icelake/Zen4+), native."},
		{"x86vec", "Set max type of vector use: none, mmx, sse, avx, avx512, native."},
		{"x86-stack-struct-return", "Return structs on the stack for x86."},
};

const int project_default_keys_count = ELEMENTLEN(project_default_keys);

const char* project_target_keys[][2] = {
		{"benchfn", "Override the benchmark function."},
		{"c-sources-add", "Additional C sources to be compiled for the target."},
		{"c-sources-override", "C sources to be compiled, overriding global settings."},
		{"cc", "Set C compiler (defaults to 'cc')."},
		{"cflags-add", "Additional C compiler flags for the target."},
		{"cflags-override", "C compiler flags for the target, overriding global settings."},
		{"cpu", "CPU name, used for optimizations in the compiler backend."},
		{"debug-info", "Debug level: none, line-tables, full."},
		{"dependencies-add", "Additional C3 library dependencies for the target."},
		{"dependencies-override", "C3 library dependencies for this target, overriding global settings."},
		{"dependency-search-paths-add", "Additional C3 library search paths for the target."},
		{"dependency-search-paths-override", "C3 library search paths for this target, overriding global settings."},
		{"exec-add", "Additional scripts to run for the target."},
		{"exec-override", "Scripts to run for this target, overriding global settings."},
		{"features", "Features enabled for all targets."},
		{"fp-math", "Set math behaviour: `strict`, `relaxed` or `fast`."},
		{"langrev", "Version of the C3 language used."},
		{"linked-libraries-add", "Additional libraries linked by the linker for the target."},
		{"linked-libraries-override", "Libraries linked by the linker for this target, overriding global settings."},
		{"linker-search-paths-add", "Additional linker search paths for the target."},
		{"linker-search-paths-override", "Linker search paths for this target, overriding global settings."},
		{"link-args-add", "Additional linker arguments for the target."},
		{"link-args-override", "Linker arguments for this target, overriding global settings."},
		{"link-libc", "Link libc (default: true)."},
		{"macossdk", "Set the directory for the MacOS SDK for cross compilation."},
		{"memory-env", "Set the memory environment: normal, small, tiny, none."},
		{"no-entry", "Do not generate (or require) a main function."},
		{"opt", "Link libc (default: true)."},
		{"optlevel", "Code optimization level: none, less, more, max."},
		{"optsize", "Code size optimization: none, small, tiny."},
		{"output", "Output location, relative to project file."},
		{"panicfn", "Override the panic function."},
		{"reloc", "Relocation model: none, pic, PIC, pie, PIE."},
		{"safe", "Set safety (contracts, runtime bounds checking, null pointer checks etc) on or off."},
		{"script-dir", "The directory where 'exec' is run."},
		{"single-module", "Compile all modules together, enables more inlining."},
		{"soft-float", "Output soft-float functions."},
		{"sources-add", "Additional paths to project sources for the target."},
		{"sources-override", "Paths to project sources for this target, overriding global settings."},
		{"strip-unused", "Strip unused code and globals from the output. (default: true)"},
		{"symtab", "Sets the preferred symtab size."},
		{"system-linker", "Use the system linker (default: no for cross compilation, yes otherwise)."},
		{"target", "Compile for a particular architecture + OS target."},
		{"testfn", "Override the test function."},
		{"trap-on-wrap", "Make signed and unsigned integer overflow generate a panic rather than wrapping."},
		{"type", "Type of output, one of 'executable', 'static-lib', 'dynamic-lib', 'benchmark', 'test', 'object-files'." },
		{"use-stdlib", "Include the standard library (default: true)."},
		{"version", "Version using semantic versioning."},
		{"warnings", "Warnings used for all targets."},
		{"wincrt", "Windows CRT linking: none, static, dynamic (default)."},
		{"windef", "Windows def file, used as an alternative to dllexport when exporting a DLL."},
		{"winsdk", "Set the path to Windows system library files for cross compilation."},
		{"x86cpu", "Set general level of x64 cpu: baseline, ssse3, sse4, avx1, avx2-v1, avx2-v2 (Skylake/Zen1+), avx512 (Icelake/Zen4+), native."},
		{"x86vec", "Set max type of vector use: none, mmx, sse, avx, avx512, native."},
		{"x86-stack-struct-return", "Return structs on the stack for x86."},
};

const int project_target_keys_count = ELEMENTLEN(project_target_keys);

const char *get_valid_string(JSONObject *table, const char *key, const char *category, bool mandatory)
{
	JSONObject *value = json_obj_get(table, key);
	if (!value)
	{
		if (mandatory)
		{
			error_exit("%s was missing a mandatory '%s' field, please add it.", category, key);
		}
		return NULL;
	}
	if (value->type != J_STRING)
	{
		error_exit("%s had an invalid mandatory '%s' field that was not a string, please correct it.", category, key);
	}
	return value->str;
}

int get_valid_bool(JSONObject *json, const char *key, const char *category, int default_val)
{
	JSONObject *value = json_obj_get(json, key);
	if (!value) return default_val;
	if (value->type != J_BOOL)
	{
		error_exit("%s had an invalid mandatory '%s' field that was not a boolean, please correct it.", category, key);
	}
	return value->b;
}


static int get_valid_string_setting(JSONObject *json, const char *key, const char *category, const char** values, int first_result, int count, const char *expected)
{
	JSONObject *value = json_obj_get(json, key);
	if (!value)
	{
		return -1;
	}
	if (value->type == J_STRING)
	{
		int res = str_findlist(value->str, count, values);
		if (res >= 0) return res + first_result;
	}
	error_exit("%s had an invalid value for '%s', expected %s", category, key, expected);
}

long get_valid_integer(JSONObject *table, const char *key, const char *category, bool mandatory)
{
	JSONObject *value = json_obj_get(table, key);
	if (!value)
	{
		if (mandatory)
		{
			error_exit("%s was missing a mandatory '%s' field, please add it.", category, key);
		}
		return -1;
	}
	if (value->type != J_NUMBER || trunc(value->f) != value->f)
	{
		error_exit("%s had an invalid mandatory '%s' field that was not an integer, please correct it.", category, key);
	}
	return (long)trunc(value->f);
}


static const char **get_valid_array(JSONObject *table, const char *key, const char *category, bool mandatory)
{
	JSONObject *value = json_obj_get(table, key);
	if (!value)
	{
		if (mandatory)
		{
			error_exit("Error reading %s: %s was missing a mandatory '%s' field, please add it.", PROJECT_JSON, category, key);
		}
		return NULL;
	}
	if (value->type != J_ARRAY)
	{
		error_exit("Error reading %s: %s had an invalid mandatory '%s' field that was not an array, please correct it.", PROJECT_JSON, category, key);
	}
	const char **values = NULL;
	for (unsigned i = 0; i < value->array_len; i++)
	{
		JSONObject *val = value->elements[i];
		if (val->type != J_STRING)
		{
			error_exit("Error reading %s: %s had an invalid mandatory '%s' array that did not only hold strings, please correct it.", PROJECT_JSON, category, key);
		}
		vec_add(values, val->str);
	}
	return values;
}

static void check_json_keys(const char* valid_keys[][2], size_t key_count, JSONObject *json, const char *type)
{
	static bool failed_shown = false;
	bool failed = false;
	for (size_t i = 0; i < json->member_len; i++)
	{
		const char *key = json->keys[i];
		for (size_t j = 0; j < key_count; j++)
		{
			if (strcmp(key, valid_keys[j][0]) == 0) goto OK;
		}
		eprintf("WARNING: Unknown parameter '%s' in '%s'.\n", key, type);
		failed = true;
	OK:;
	}
	if (failed && !failed_shown)
	{
		eprintf("You can use '--list-project-properties' to list all valid properties.\n");
		failed_shown = true;
	}
}

INLINE void append_strings_to_strings(const char*** list_of_strings_ptr, const char **strings_to_append)
{
	FOREACH_BEGIN(const char *string, strings_to_append)
		vec_add(*list_of_strings_ptr, string);
	FOREACH_END();
}

static void target_append_strings(JSONObject *json, const char *type, const char ***list_ptr, const char *base, const char *override, const char *add, bool is_default)
{
	const char **value = get_valid_array(json, is_default ? base : override, type, false);
	const char **add_value = is_default ? NULL : get_valid_array(json, add, type, false);
	if (value && add_value)
	{
		error_exit("'%s' is combining both '%s' and '%s', only one may be used.", type, override, add);
	}
	if (value) *list_ptr = value;
	if (add_value)
	{
		append_strings_to_strings(&add_value, *list_ptr);
		*list_ptr = add_value;
	}
}
static void load_into_build_target(JSONObject *json, const char *type, BuildTarget *target, bool is_default)
{
	if (is_default)
	{
		check_json_keys(project_default_keys, project_default_keys_count, json, type);
	}
	else
	{
		check_json_keys(project_target_keys, project_target_keys_count, json, type);
	}
	const char *cc = get_valid_string(json, "cc", type, false);
	if (cc) target->cc = cc;

	const char *script_dir = get_valid_string(json, "script-dir", type, false);
	if (script_dir) target->script_dir = script_dir;

	// Exec
	const char **exec = get_valid_array(json, is_default ? "exec" : "exec-override" , type, false);
	const char **exec_add = is_default ? NULL : get_valid_array(json, "exec-add" , type, false);
	if (exec && exec_add)
	{
		error_exit("'%s' is combining both 'exec-add' and 'exec-override', only one may be used.", type);
	}
	if (exec_add && !target->exec)
	{
		target->exec = exec_add;
		exec_add = NULL;
	}
	if (exec) target->exec = exec;
	if (exec_add)
	{
		assert(target->exec);
		FOREACH_BEGIN(const char *exec_command, exec_add)
			vec_add(target->exec, exec_command);
		FOREACH_END();
	}

	const char *output_dir = get_valid_string(json, "output", type, false);
	if (output_dir) target->output_dir = output_dir;

	// CFlags
	const char *cflags = get_valid_string(json, is_default ? "cflags" : "cflags-override" , type, false);
	const char *cflags_add = is_default ? NULL : get_valid_string(json, "cflags-add" , type, false);
	if (cflags && cflags_add)
	{
		error_exit("'%s' is combining both 'cflags-add' and 'cflags-override', only one may be used.", type);
	}
	if (cflags) target->cflags = cflags;
	if (cflags_add)
	{
		if (target->cflags)
		{
			target->cflags = str_printf("%s %s", target->cflags, cflags_add);
		}
		else
		{
			target->cflags = cflags_add;
		}
	}
	// C source dirs.
	target_append_strings(json, type, &target->csource_dirs, "c-sources", "c-sources-override", "c-sources-add", is_default);

	// Sources
	target_append_strings(json, type, &target->source_dirs, "sources", "sources-override", "sources-add", is_default);

	// Linked-libraries - libraries to add at link time
	target_append_strings(json, type, &target->linker_libs, "linked-libraries", "linked-libraries-override", "linked-libraries-add", is_default);

	// linker-search-paths libs dir - libraries to add at link time
	target_append_strings(json, type, &target->linker_libdirs, "linker-search-paths", "linker-search-paths-override", "linker-search-paths-add", is_default);

	// link-args - link args to add at link time
	target_append_strings(json, type, &target->link_args, "link-args", "link-args-override", "link-args-add", is_default);

	// dependency-search-paths - path to search for libraries
	target_append_strings(json, type, &target->libdirs, "dependency-search-paths", "dependency-search-paths-override", "dependency-search-paths-add", is_default);

	// Dependencies
	target_append_strings(json, type, &target->libs, "dependencies", "dependencies-override", "dependencies-add", is_default);
	FOREACH_BEGIN(const char *name, target->libs)
		if (!str_is_valid_lowercase_name(name))
		{
			char *name_copy = strdup(name);
			str_ellide_in_place(name_copy, 32);
			error_exit("Error reading %s: invalid library target '%s'.", PROJECT_JSON, name_copy);
		}
	FOREACH_END();

	// debug-info
	static const char *debug_infos[3] = {
			[DEBUG_INFO_FULL] = "full",
			[DEBUG_INFO_NONE] = "none",
			[DEBUG_INFO_LINE_TABLES] = "line-tables"
	};
	DebugInfo info = get_valid_string_setting(json, "debug-info", type, debug_infos, 0, ELEMENTLEN(debug_infos), "one of 'full' 'line-table' or 'none'.");
	if (info > -1) target->debug_info = info;

	// Optimization Level
	OptimizationLevel optlevel = (OptimizationLevel)get_valid_string_setting(json, "optlevel", type, optlevels, 0, ELEMENTLEN(optlevels), "`none`, `less`, `more`, `max`.");
	target->optlevel = optlevel;

	// Size optimization
	SizeOptimizationLevel optsize = (SizeOptimizationLevel)get_valid_string_setting(json, "optsize", type, optsizes, 0, ELEMENTLEN(optlevels), "`none`, `small`, `tiny`.");
	target->optsize = optsize;

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
	OptimizationSetting opt = (OptimizationSetting)get_valid_string_setting(json, "opt", type, opt_settings, 0, ELEMENTLEN(opt_settings), "'O0', 'O1' etc.");
	if (opt != OPT_SETTING_NOT_SET) target->optsetting = opt;

	// Safety level
	target->feature.safe_mode = (SafetyLevel)get_valid_bool(json, "safe", type, target->feature.safe_mode);

	// Single module
	target->single_module = (SingleModule)get_valid_bool(json, "single-module", type, target->single_module);

	// Memory environment for memory constrained environments.
	MemoryEnvironment env = get_valid_string_setting(json, "memory-env", type, memory_environment, 0, ELEMENTLEN(memory_environment), "one of 'normal', 'small', 'tiny' or 'none'.");
	if (env > -1) target->memory_environment = env;

	// Symtab
	long symtab_size = get_valid_integer(json, "symtab", type, false);
	if (symtab_size > 0)
	{
		if (symtab_size < 1024)
		{
			error_exit("Error reading %s: %s symtab was less than 1024.", PROJECT_JSON, type);
		}
		if (symtab_size > MAX_SYMTAB_SIZE)
		{
			error_exit("Error reading %s: %s symtab may not exceed %d.", PROJECT_JSON, type, MAX_SYMTAB_SIZE);
		}
		target->symtab_size = (uint32_t)symtab_size;
	}

	// Target
	const char *arch_os_string = get_valid_string(json, "target", type, false);
	if (arch_os_string)
	{
		ArchOsTarget arch_os = arch_os_target_from_string(arch_os_string);
		if (arch_os == ARCH_OS_TARGET_DEFAULT) error_exit("Error reading %s: %s target was not valid.", PROJECT_JSON, type);
		target->arch_os_target = arch_os;
	}

	// Reloc
	int reloc = get_valid_string_setting(json, "reloc", type, reloc_models, 0, ELEMENTLEN(reloc_models), "'none', 'pic', 'PIC', 'pie' or 'PIE'.");
	if (reloc > -1) target->reloc_model = (RelocModel)reloc;

	// Cpu
	const char *cpu = get_valid_string(json, "cpu", type, false);
	if (cpu) target->cpu = cpu;

	// WinCRT
	int wincrt = get_valid_string_setting(json, "wincrt", type, wincrt_linking, 0, ELEMENTLEN(wincrt_linking), "'none', 'static' or 'dynamic'.");
	if (wincrt > -1) target->win.crt_linking = (WinCrtLinking)wincrt;

	// fp-math
	int fpmath = get_valid_string_setting(json, "fp-math", type, fp_math, 0, ELEMENTLEN(fp_math), "`strict`, `relaxed` or `fast`.");
	if (fpmath > -1) target->feature.fp_math = fpmath;

	const char **features = get_valid_array(json, "features", type, false);
	if (features)
	{
		FOREACH_BEGIN(const char *feature, features)
			if (!str_is_valid_constant(feature))
			{
				error_exit("Error reading 'features': '%s' is not a valid feature name.", feature);
			}
			vec_add(target->feature_list, feature);
		FOREACH_END();
	}

	// x86vec
	int x86vec = get_valid_string_setting(json, "x86vec", type, x86_vector_capability, 0, ELEMENTLEN(x86_vector_capability), "`none`, `native`, `mmx`, `sse`, `avx` or `avx512`.");
	if (x86vec > -1) target->feature.x86_vector_capability = x86vec;

	// x86vec
	int x86cpu = get_valid_string_setting(json, "x86cpu", type, x86_cpu_set, 0, ELEMENTLEN(x86_cpu_set), "`baseline`, `ssse3`, `sse4`, `avx1`, `avx2-v1`, `avx2-v2`, `avx512` or `native`.");
	if (x86cpu > -1) target->feature.x86_cpu_set = x86cpu;

	// riscvfloat
	int riscv_float = get_valid_string_setting(json, "riscvfloat", type, riscv_capability, 0, ELEMENTLEN(riscv_capability), "`none`, `float` or `double`.");
	if (riscv_float > -1) target->feature.riscv_float_capability = riscv_float;

	// winsdk
	target->win.sdk = get_valid_string(json, "winsdk", type, false);

	// windef
	target->win.def = get_valid_string(json, "windef", type, false);

	// macossdk
	target->macos.sysroot = get_valid_string(json, "macossdk", type, false);

	// macos-min-version
	target->macos.min_version = get_valid_string(json, "macos-min-version", type, false);

	// macos-sdk-version
	target->macos.sdk_version = get_valid_string(json, "macos-sdk-version", type, false);

	// Linux crt
	target->linuxpaths.crt = get_valid_string(json, "linux-crt", type, false);

	// Linux crtbegin
	target->linuxpaths.crtbegin = get_valid_string(json, "linux-crtbegin", type, false);

	// version
	const char *version = get_valid_string(json, "version", type, false);
	if (version) target->version = version;

	// langrev
	const char *langrev = get_valid_string(json, "langrev", type, false);
	if (langrev) target->langrev = langrev;

	// panicfn
	const char *panicfn = get_valid_string(json, "panicfn", type, false);
	target->panicfn = panicfn;

	// testfn
	const char *testfn = get_valid_string(json, "testfn", type, false);
	target->testfn = testfn;

	// testfn
	const char *benchfn = get_valid_string(json, "benchfn", type, false);
	target->benchfn = benchfn;

	// link-libc
	target->link_libc = (LinkLibc)get_valid_bool(json, "link-libc", type, target->link_libc);

	// strip-unused
	target->strip_unused = (StripUnused)get_valid_bool(json, "strip-unused", type, target->strip_unused);

	// system-linker
	target->system_linker = (SystemLinker)get_valid_bool(json, "system-linker", type, target->system_linker);

	// no-entry
	target->no_entry = get_valid_bool(json, "no-entry", type, target->no_entry);

	// use-stdlib
	target->use_stdlib = (UseStdlib)get_valid_bool(json, "use-stdlib", type, target->use_stdlib);

	// emit-stdlib
	target->emit_stdlib = (EmitStdlib)get_valid_bool(json, "emit-stdlib", type, target->emit_stdlib);

	// single-module
	target->single_module = (SingleModule)get_valid_bool(json, "single-module", type, target->single_module);

	// Trap on wrap
	target->feature.trap_on_wrap = get_valid_bool(json, "trap-on-wrap", type, target->feature.trap_on_wrap);

	// Use the fact that they correspond to 0, 1, -1
	target->feature.x86_struct_return = get_valid_bool(json, "x86-stack-struct-return", type, target->feature.x86_struct_return);
	target->feature.soft_float = get_valid_bool(json, "soft-float", type, target->feature.soft_float);

}
static void project_add_target(Project *project, BuildTarget *default_target,  JSONObject *json, const char *name, const char *type, TargetType target_type)
{
	assert(json->type == J_OBJECT);
	BuildTarget *target = CALLOCS(BuildTarget);
	*target = *default_target;
	vec_add(project->targets, target);
	target->name = name;
	target->type = target_type;
	VECEACH(project->targets, i)
	{
		BuildTarget *other_target = project->targets[i];
		if (other_target == target) continue;
		if (strcmp(other_target->name, target->name) == 0)
		{
			error_exit("More %s contained more than one target with the name %s. Please make all target names unique.", PROJECT_JSON, target->name);
		}
	}
	type = str_printf("%s %s", type, target->name);
	load_into_build_target(json, type, target, false);
	update_build_target_with_opt_level(target, target->optsetting);
}

static void project_add_targets(Project *project, JSONObject *project_data)
{
	assert(project_data->type == J_OBJECT);
	static const char* targets[6] = {
			[TARGET_TYPE_EXECUTABLE] = "executable",
			[TARGET_TYPE_STATIC_LIB] = "static-lib",
			[TARGET_TYPE_DYNAMIC_LIB] = "dynamic-lib",
			[TARGET_TYPE_BENCHMARK] = "benchmark",
			[TARGET_TYPE_TEST] = "test",
			[TARGET_TYPE_OBJECT_FILES] = "object-files"
	};
	static const char *target_desc[6] = {
			[TARGET_TYPE_EXECUTABLE] = "Executable",
			[TARGET_TYPE_STATIC_LIB] = "Static library",
			[TARGET_TYPE_DYNAMIC_LIB] = "Dynamic library",
			[TARGET_TYPE_BENCHMARK] = "benchmark suite",
			[TARGET_TYPE_TEST] = "test suite",
			[TARGET_TYPE_OBJECT_FILES] = "object files"
	};

	BuildTarget default_target = default_build_target;
	load_into_build_target(project_data, "default target", &default_target, true);
	JSONObject *targets_json = json_obj_get(project_data, "targets");
	if (!targets_json)
	{
		error_exit("No targets found in project.");
	}
	if (targets_json->type != J_OBJECT)
	{
		error_exit("'targets' did not contain map of targets.");
	}
	for (unsigned i = 0; i < targets_json->member_len; i++)
	{
		JSONObject *object = targets_json->members[i];
		const char *key = targets_json->keys[i];
		if (object->type != J_OBJECT)
		{
			error_exit("Invalid data in target '%s'", key);
		}
		int type = get_valid_string_setting(object, "type", "Target type", targets, 0, ELEMENTLEN(targets), "a target type like 'executable' or 'static-lib'");
		if (type < 0) error_exit("Target %s did not contain 'type' key.", key);
		project_add_target(project, &default_target, object, key, target_desc[type], type);
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
	VECEACH(project->targets, i)
	{
		BuildTarget *target = project->targets[i];
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
BuildTarget *project_select_target(Project *project, const char *optional_target)
{
	if (!vec_size(project->targets))
	{
		error_exit("No targets could be found in %s. Please define at least one target, for example an 'executable' and try again.", PROJECT_JSON);
	}
	if (!optional_target)
	{
		return project_select_default_target(project);
	}
	VECEACH(project->targets, i)
	{
		BuildTarget *target = project->targets[i];
		if (str_eq(target->name, optional_target)) return target;
	}
	error_exit("No build target named '%s' was found in %s. Was it misspelled?", optional_target, PROJECT_JSON);
}

Project *project_load(void)
{
	Project *project = CALLOCS(Project);
	size_t size;
	char *read = file_read_all(PROJECT_JSON, &size);
	JsonParser parser;
	json_init_string(&parser, read, &malloc_arena);
	JSONObject *json = json_parse(&parser);
	if (parser.error_message)
	{
		error_exit("Error on line %d reading '%s':'%s'", parser.line, PROJECT_JSON, parser.error_message);
	}
	if (!json || json->type != J_OBJECT)
	{
		error_exit("Expected a map of targets in '%s'.", PROJECT_JSON);
	}
	project_add_targets(project, json);
	return project;
}
