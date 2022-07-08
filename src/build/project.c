// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include <math.h>
#include "build_internal.h"
#define MAX_SYMTAB_SIZE (1024 * 1024)



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

static void load_into_build_target(JSONObject *json, const char *type, BuildTarget *target)
{
	const char *cc = get_valid_string(json, "cc", type, false);
	const char *cflags = get_valid_string(json, "cflags", type, false);
	const char **csource_dirs = get_valid_array(json, "csources", type, false);
	const char *version = get_valid_string(json, "version", type, false);
	const char *langrev = get_valid_string(json, "langrev", type, false);
	const char **source_dirs = get_valid_array(json, "sources", type, target->source_dirs == NULL);
	const char **libraries = get_valid_array(json, "libs", type, false);
	const char **linker_libs = get_valid_array(json, "linker-libs", type, false);
	VECEACH(libraries, i)
	{
		if (!str_is_valid_lowercase_name(libraries[i]))
		{
			char *name = strdup(libraries[i]);
			str_ellide_in_place(name, 32);
			error_exit("Error reading %s: invalid library target '%s'.", PROJECT_JSON, name);
		}
	}
	const char **libdirs = get_valid_array(json, "libdir", type, false);
	const char **linker_libdirs = get_valid_array(json, "linker-libdir", type, false);

	static const char *debug_infos[3] = {
			[DEBUG_INFO_FULL] = "full",
			[DEBUG_INFO_NONE] = "none",
			[DEBUG_INFO_LINE_TABLES] = "line-tables"
	};
	DebugInfo info = get_valid_string_setting(json, "debug-info", type, debug_infos, 0, 3, "one of 'full' 'line-table' or 'none'.");
	const char *arch_os_string = get_valid_string(json, "target", type, false);
	long symtab_size = get_valid_integer(json, "symtab", type, false);
	const char *cpu = get_valid_string(json, "cpu", type, false);
	int reloc = get_valid_string_setting(json, "reloc", type, reloc_models, 0, 5, "'none', 'pic', 'PIC', 'pie' or 'PIE'.");
	int wincrt = get_valid_string_setting(json, "wincrt", type, wincrt_linking, 0, 5, "'none', 'static' or 'dynamic'.");
	int x86vec = get_valid_string_setting(json, "x86vec", type, vector_capability, 0, 5, "none, mmx, sse, avx or avx512");
	const char *panicfn = get_valid_string(json, "panicfn", type, false);
	target->win.sdk = get_valid_string(json, "winsdk", type, false);
	target->macos.sdk = get_valid_string(json, "macossdk", type, false);
	target->panicfn = panicfn;
	if (cc) target->cc = cc;
	if (cflags) target->cflags = cflags;
	if (csource_dirs) target->csource_dirs = csource_dirs;
	if (version) target->version = version;
	if (langrev) target->langrev = langrev;
	if (source_dirs) target->source_dirs = source_dirs;
	if (libdirs) target->libdirs = libdirs;
	if (linker_libdirs) target->linker_libdirs = linker_libdirs;
	if (linker_libs) target->linker_libs = linker_libs;
	if (libraries) target->libs = libraries;
	if (info > -1) target->debug_info = info;
	if (cpu) target->cpu = cpu;
	if (wincrt > -1) target->win.crt_linking = (WinCrtLinking)wincrt;
	if (reloc > -1) target->reloc_model = (RelocModel)reloc;
	if (x86vec > -1) target->feature.x86_vector_capability = x86vec;

	if (arch_os_string)
	{
		ArchOsTarget arch_os = arch_os_target_from_string(arch_os_string);
		if (arch_os == ARCH_OS_TARGET_DEFAULT) error_exit("Error reading %s: %s target was not valid.", PROJECT_JSON, type);
		target->arch_os_target = arch_os;
	}
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

	target->feature.trap_on_wrap = get_valid_bool(json, "trap-on-wrap", type, target->feature.trap_on_wrap);
	// Use the fact that they correspond to 0, 1, -1
	target->feature.x86_struct_return = get_valid_bool(json, "x86-stack-struct-return", type, target->feature.x86_struct_return);
	target->feature.soft_float = get_valid_bool(json, "soft-float", type, target->feature.soft_float);
	target->no_stdlib = get_valid_bool(json, "nostdlib", type, false);

}
static void project_add_target(Project *project, BuildTarget *default_target,  JSONObject *json, const char *name, const char *type)
{
	assert(json->type == J_OBJECT);
	BuildTarget *target = CALLOCS(BuildTarget);
	*target = *default_target;
	vec_add(project->targets, target);
	target->name = name;
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
	load_into_build_target(json, type, target);
}

static void project_add_targets(Project *project, JSONObject *project_data)
{
	assert(project_data->type == J_OBJECT);
	static const char* targets[4] = { [TARGET_TYPE_EXECUTABLE] = "executable",
									  [TARGET_TYPE_STATIC_LIB] = "static-lib",
									  [TARGET_TYPE_DYNAMIC_LIB] = "dynamic-lib",
									  [TARGET_TYPE_TEST] = "test" };
	static const char *target_desc[4] = {
			[TARGET_TYPE_EXECUTABLE] = "Executable",
			[TARGET_TYPE_STATIC_LIB] = "Static library",
			[TARGET_TYPE_DYNAMIC_LIB] = "Dynamic library",
			[TARGET_TYPE_TEST] = "test suite" };

	BuildTarget default_target = {
			.optimization_level = OPTIMIZATION_DEFAULT,
			.size_optimization_level = SIZE_OPTIMIZATION_NONE,
			.arch_os_target = ARCH_OS_TARGET_DEFAULT,
			.debug_info = DEBUG_INFO_NONE,
			.symtab_size = DEFAULT_SYMTAB_SIZE,
			.cc = "cc",
			.version = "1.0.0",
			.langrev = "1",
			.cpu = "generic",
			.feature.x86_struct_return = STRUCT_RETURN_DEFAULT,
			.feature.soft_float = SOFT_FLOAT_DEFAULT,
			.feature.trap_on_wrap = false,
			.feature.x86_vector_capability = X86VECTOR_DEFAULT,
			.feature.safe_mode = true,
			.win.crt_linking = WIN_CRT_DEFAULT,
	};
	load_into_build_target(project_data, "default target", &default_target);
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
		int type = get_valid_string_setting(object, "type", "Target type", targets, 0, 4, "a target type like 'executable' or 'static-lib'");
		if (type < 0) error_exit("Target %s did not contain 'type' key.", key);
		project_add_target(project, &default_target, object, key, target_desc[type]);
	}
}

static BuildTarget *project_select_default_target(Project *project)
{
	VECEACH(project->targets, i)
	{
		BuildTarget *target = project->targets[i];
		if (target->type == TARGET_TYPE_EXECUTABLE) return target;
	}
	return project->targets[0];
}

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
		if (strcmp(target->name, optional_target) == 0) return target;
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
