// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include <utils/toml.h>
#include "build_internal.h"

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
	type = strformat("%s %s", type, target->name);
	target->version = get_valid_string(table, "version", type, false);
	if (!target->version) target->version = "1.0.0";
	target->langrev = get_valid_string(table, "langrev", type, false);
	target->sources = get_valid_array(table, "sources", type, true);
	target->libraries = get_valid_array(table, "libs", type, false);
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
