// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include <utils/toml.h>
#include "build_internal.h"
#define MAX_SYMTAB_SIZE (1024 * 1024)

TomlArray *get_array(TomlTable *table, const char *key)
{
	TomlValue *value = toml_table_get(table, key);
	if (!value) return NULL;
	if (value->type != TOML_ARRAY)
	{
		error_exit("The key '%s' was not an array field. Did you type '[%s]' instead of '[[%s]]'?");
	}
	return value->value.array;
}

static inline const char *copy_toml_string(TomlString *string)
{
	size_t len = string->len;
	char *new_str = malloc_arena(len + 1);
	memcpy(new_str, string->str, len);
	new_str[len] = '\0';
	return new_str;
}

const char *get_valid_string(TomlTable *table, const char *key, const char *category, bool mandatory)
{
	TomlValue *value = toml_table_get(table, key);
	if (!value)
	{
		if (mandatory)
		{
			error_exit("%s was missing a mandatory '%s' field, please add it.", category, key);
		}
		return NULL;
	}
	if (value->type != TOML_STRING)
	{
		error_exit("%s had an invalid mandatory '%s' field that was not a string, please correct it.", category, key);
	}
	return copy_toml_string(value->value.string);
}

int get_valid_bool(TomlTable *table, const char *key, const char *category, int default_val)
{
	TomlValue *value = toml_table_get(table, key);
	if (!value) return default_val;
	if (value->type != TOML_BOOLEAN)
	{
		error_exit("%s had an invalid mandatory '%s' field that was not a boolean, please correct it.", category, key);
	}
	return value->value.boolean;
}


static int get_valid_string_setting(TomlTable *table, const char *key, const char *category, const char** values, int first_result, int count, const char *expected)
{
	TomlValue *value = toml_table_get(table, key);
	if (!value)
	{
		return -1;
	}
	if (value->type == TOML_STRING)
	{
		for (int i = 0; i < count; i++)
		{
			unsigned str_len = (unsigned) strlen(values[i]);
			if (str_len != value->value.string->len) continue;
			if (memcmp(values[i], value->value.string->str, str_len) == 0) return i + first_result;
		}
	}
	error_exit("%s had an invalid value for '%s', expected %s", category, key, expected);
}

long get_valid_integer(TomlTable *table, const char *key, const char *category, bool mandatory)
{
	TomlValue *value = toml_table_get(table, key);
	if (!value)
	{
		if (mandatory)
		{
			error_exit("%s was missing a mandatory '%s' field, please add it.", category, key);
		}
		return -1;
	}
	if (value->type != TOML_INTEGER)
	{
		error_exit("%s had an invalid mandatory '%s' field that was not an integer, please correct it.", category, key);
	}
	return value->value.integer;
}


static const char **get_valid_array(TomlTable *table, const char *key, const char *category, bool mandatory)
{
	TomlValue *value = toml_table_get(table, key);
	if (!value)
	{
		if (mandatory)
		{
			error_exit("Error reading %s: %s was missing a mandatory '%s' field, please add it.", PROJECT_TOML, category, key);
		}
		return NULL;
	}
	if (value->type != TOML_ARRAY)
	{
		error_exit("Error reading %s: %s had an invalid mandatory '%s' field that was not an array, please correct it.", PROJECT_TOML, category, key);
	}
	const char **values = NULL;
	for (unsigned i = 0; i < value->value.array->len; i++)
	{
		TomlValue *val = value->value.array->elements[i];
		if (val->type != TOML_STRING)
		{
			error_exit("Error reading %s: %s had an invalid mandatory '%s' array that did not only hold strings, please correct it.", PROJECT_TOML, category, key);
		}
		vec_add(values, copy_toml_string(val->value.string));
	}
	return values;
}

void project_add_target(Project *project, TomlValue *wrapped_table, const char *type, const char *type_key, TargetType target_type)
{
	if (wrapped_table->type != TOML_TABLE)
	{
		error_exit("The %s had an invalid %s. Please check your [[%s]] configurations.", PROJECT_TOML, type, type_key);
	}
	BuildTarget *target = CALLOCS(BuildTarget);
	target->optimization_level = OPTIMIZATION_DEFAULT;
	target->size_optimization_level = SIZE_OPTIMIZATION_NONE;
	target->arch_os_target = ARCH_OS_TARGET_DEFAULT;
	target->debug_info = DEBUG_INFO_NONE;
	target->symtab_size = DEFAULT_SYMTAB_SIZE;
	vec_add(project->targets, target);
	TomlTable *table = wrapped_table->value.table;
	target->name = get_valid_string(table, "name", type, true);
	VECEACH(project->targets, i)
	{
		BuildTarget *other_target = project->targets[i];
		if (other_target == target) continue;
		if (strcmp(other_target->name, target->name) == 0)
		{
			error_exit("More %s contained more than one target with the name %s. Please make all target names unique.", PROJECT_TOML, target->name);
		}
	}
	target->cc = get_valid_string(table, "cc", type, false);
	if (!target->cc) target->cc = "cc";
	target->cflags = get_valid_string(table, "cflags", type, false);
	target->csource_dirs = get_valid_array(table, "csources", type, false);

	type = strformat("%s %s", type, target->name);
	target->version = get_valid_string(table, "version", type, false);
	if (!target->version) target->version = "1.0.0";
	target->langrev = get_valid_string(table, "langrev", type, false);
	target->source_dirs = get_valid_array(table, "sources", type, true);
	target->libraries = get_valid_array(table, "libs", type, false);
	static const char *debug_infos[3] = {
			[DEBUG_INFO_FULL] = "full",
			[DEBUG_INFO_NONE] = "none",
			[DEBUG_INFO_LINE_TABLES] = "line-tables"
	};
	DebugInfo info = get_valid_string_setting(table, "debug-info", type, debug_infos, 0, 3, "one of 'full' 'line-table' or 'none'.");
	if (info > -1) target->debug_info = info;
	const char *arch_os_string = get_valid_string(table, "target", type, false);
	if (arch_os_string)
	{
		ArchOsTarget arch_os = arch_os_target_from_string(arch_os_string);
		if (arch_os == ARCH_OS_TARGET_DEFAULT) error_exit("Error reading %s: %s target was not valid.", PROJECT_TOML, type);
		target->arch_os_target = arch_os;
	}
	long symtab_size = get_valid_integer(table, "symtab", type, false);
	if (symtab_size > 0)
	{
		if (symtab_size < 1024)
		{
			error_exit("Error reading %s: %s symtab was less than 1024.", PROJECT_TOML, type);
		}
		if (symtab_size > MAX_SYMTAB_SIZE)
		{
			error_exit("Error reading %s: %s symtab may not exceed %d.", PROJECT_TOML, type, MAX_SYMTAB_SIZE);
		}
		target->symtab_size = (uint32_t)symtab_size;
	}
	const char *cpu = get_valid_string(table, "cpu", type, false);
	target->cpu = cpu ? cpu : "generic";

	static const char *pies[3] = {
			[PIE_SMALL] = "yes-limited",
			[PIE_NONE] = "no",
			[PIE_BIG] = "yes-unlimited"
	};
	target->pie = get_valid_string_setting(table, "pie", type, pies, 0, 3, "'yes-limited', 'yes-unlimited' or 'no'.");
	target->pic = get_valid_string_setting(table, "pic", type, pies, 0, 3, "'yes-limited', 'yes-unlimited' or 'no'.");

	target->feature.no_memcpy_pass = get_valid_bool(table, "no-memcpy-pass", type, false);
	target->feature.trap_on_wrap = get_valid_bool(table, "trap-on-wrap", type, false);
	// Use the fact that they correspond to 0, 1, -1
	target->feature.struct_return = get_valid_bool(table, "stack-struct-return", type, STRUCT_RETURN_DEFAULT);
	target->feature.soft_float = get_valid_bool(table, "soft-float", type, SOFT_FLOAT_DEFAULT);
	target->feature.no_avx = get_valid_bool(table, "no-avx", type, false);
	target->feature.no_sse = get_valid_bool(table, "no-sse", type, false);
	target->feature.no_mmx = get_valid_bool(table, "no-mmx", type, false);
}

static void project_add_targets(Project *project, TomlTable *table, const char *type, const char *type_key, TargetType target_type)
{
	TomlArray *targets = get_array(table, type_key);
	if (!targets) return;
	for (unsigned i = 0; i < targets->len; i++)
	{
		project_add_target(project, targets->elements[i], type, type_key, target_type);
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
		error_exit("No targets could be found in %s. Please define at least one target, for example a [[executable]] and try again.", PROJECT_TOML);
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
	error_exit("No build target named '%s' was found in %s. Was it misspelled?", optional_target, PROJECT_TOML);
}

Project *project_load(void)
{
	Project *project = CALLOCS(Project);
	TomlErr err = { .code = TOML_OK };
	TomlTable *toml = toml_load_filename(PROJECT_TOML, &err);
	if (err.code != TOML_OK)
	{
		error_exit("%s could not be read. Can you please check the read permissions on the file?", PROJECT_TOML);
	}
	project_add_targets(project, toml, "executable", "executable", TARGET_TYPE_EXECUTABLE);
	project_add_targets(project, toml, "dynamic library", "dynamic-lib", TARGET_TYPE_DYNAMIC_LIB);
	project_add_targets(project, toml, "static library", "static-lib", TARGET_TYPE_STATIC_LIB);
	return project;
}
