#include "build_internal.h"
#include "utils/common.h"
#include <math.h>


void check_json_keys(const char* valid_keys[][2], size_t key_count, const char* deprecated_keys[], size_t deprecated_key_count, JSONObject *json, const char *target_name, const char *option)
{
	static bool failed_shown = false;
	bool failed = false;
	FOREACH(const char *, key, json->keys)
	{
		for (size_t j = 0; j < key_count; j++)
		{
			if (str_eq(key, valid_keys[j][0])) goto OK;
		}
		for (size_t j = 0; j < deprecated_key_count; j++)
		{
			if (str_eq(key, deprecated_keys[j]))
			{
				if (silence_deprecation) goto OK;
				WARNING("Target '%s' is using the deprecated parameter '%s'.", target_name, key);
				goto OK;
			}
		}
		WARNING("Unknown parameter '%s' in '%s'", key, target_name);
		failed = true;
		OK:;
	}
	if (failed && !failed_shown)
	{
		eprintf("You can use '%s' to list all valid properties.\n", option);
		failed_shown = true;
	}
}

const char *get_optional_string(const char *file, const char *category, JSONObject *table, const char *key)
{
	JSONObject *value = json_map_get(table, key);
	if (!value) return NULL;
	if (value->type != J_STRING)
	{
		if (category) error_exit("In file '%s': '%s' had an invalid '%s' field that was not a string, please correct it.", file, category, key);
		error_exit("File '%s' had an invalid '%s' field that was not a string, please correct it.", file, category, key);
	}
	return value->str;
}

const char *get_mandatory_string(const char *file, const char *category, JSONObject *object, const char *key)
{
	const char *value = get_optional_string(file, category, object, key);
	if (!value)
	{
		if (category) error_exit("In file '%s': The mandatory field '%s' was missing in '%s'.", file, key, category);
		error_exit("In file '%s': The mandatory field '%s' was missing.", file, key);
	}
	return value;
}

const char *get_string(const char *file, const char *category, JSONObject *table, const char *key,
                       const char *default_value)
{
	const char *value = get_optional_string(file, category, table, key);
	return value ? value : default_value;
}


int get_valid_bool(const char *file, const char *target, JSONObject *json, const char *key, int default_val)
{
	JSONObject *value = json_map_get(json, key);
	if (!value) return default_val;
	if (value->type != J_BOOL)
	{
		if (target) error_exit("In file '%s': '%s' had an invalid '%s' field that was not a boolean, please correct it.", file, target, key);
		error_exit("In file '%s': An invalid '%s' field that was not a boolean, please correct it.", file, key);
	}
	return value->b;
}

const char **get_string_array(const char *file, const char *category, JSONObject *table, const char *key, bool mandatory)
{
	JSONObject *value = json_map_get(table, key);
	if (!value)
	{
		if (mandatory)
		{
			if (category) error_exit("In file '%s': '%s' was missing a mandatory '%s' field, please add it.", file, category, key);
			error_exit("In file '%s': mandatory '%s' field is missing, please add it.", file, key);
		}
		return NULL;
	}
	if (value->type != J_ARRAY) goto NOT_ARRAY;
	const char **values = NULL;
	FOREACH(JSONObject *, val, value->elements)
	{
		if (val->type != J_STRING) goto NOT_ARRAY;
		vec_add(values, val->str);
	}
	return values;
NOT_ARRAY:
	if (category) error_exit("In file '%s': '%s' had an invalid mandatory '%s' field that was not a string array, please correct it", file, category, key);
	error_exit("In file '%s': mandatory '%s' field that was not a string array, please correct it.", file, key);
}

const char **get_optional_string_array(const char *file, const char *target, JSONObject *table, const char *key)
{
	return get_string_array(file, target, table, key, false);
}

const char *get_cflags(const char *file, const char *target, JSONObject *json, const char *original_flags)
{
	// CFlags
	const char *cflags = get_optional_string(file, target, json, target ? "cflags-override" : "cflags");
	const char *cflags_add = target ? get_optional_string(file, target, json, "cflags") : NULL;
	if (cflags && cflags_add)
	{
		error_exit("In file '%s': '%s' is combining both 'cflags' and 'cflags-override', only one may be used.", file, target);
	}
	if (target && !cflags_add) cflags_add = get_optional_string(file, target, json, "cflags-add");
	if (cflags && cflags_add)
	{
		// TODO remove in 0.7
		error_exit("In file '%s': '%s' is combining both 'cflags-add' and 'cflags-override', only one may be used.", file, target);
	}

	if (cflags) original_flags = cflags;
	if (!cflags_add) return original_flags;
	if (original_flags)
	{
		return str_printf("%s %s", original_flags, cflags_add);
	}
	return cflags_add;
}

INLINE void append_strings_to_strings(const char*** list_of_strings_ptr, const char **strings_to_append)
{
	FOREACH(const char *, string, strings_to_append) vec_add(*list_of_strings_ptr, string);
}

void get_list_append_strings(const char *file, const char *target, JSONObject *json, const char ***list_ptr,
                             const char *base, const char *override, const char *add)
{
	const char **value = get_optional_string_array(file, target, json, target ? override : base);
	const char **add_value = target ? get_optional_string_array(file, target, json, base) : NULL;
	if (value && add_value)
	{
		error_exit("In file '%s': '%s' is combining both '%s' and '%s', only one may be used.", file, target, override, base);
	}
	if (!add_value && target) add_value = get_optional_string_array(file, target, json, add);
	if (value && add_value)
	{
		// TODO remove in 0.7
		error_exit("In file '%s': '%s' is combining both '%s' and '%s', only one may be used.", file, target, override, add);
	}
	if (value) *list_ptr = value;
	if (add_value)
	{
		append_strings_to_strings(&add_value, *list_ptr);
		*list_ptr = add_value;
	}
}

int get_valid_string_setting(const char *file, const char *target, JSONObject *json, const char *key, const char** values, int first_result, int count, const char *expected)
{
	JSONObject *value = json_map_get(json, key);
	if (!value)
	{
		return -1;
	}
	if (value->type == J_STRING)
	{
		int res = str_findlist(value->str, count, values);
		if (res >= 0) return res + first_result;
	}
	if (target)
	{
		error_exit("In file '%s': '%s' had an invalid value for '%s', expected %s", file, target, key, expected);
	}
	error_exit("In file '%s': Invalid value for '%s', expected %s", file, key, expected);
}

int get_valid_enum_from_string(const char *str, const char *target, const char **values, int count, const char *expected)
{
	int res = str_findlist(str, count, values);
	if (res >= 0) return res;
	if (target)
	{
		error_exit("'%s' had an invalid value, expected %s", target, expected);
	}
	error_exit("Invalid value, expected %s", expected);
}

long get_valid_integer(JSONObject *table, const char *key, const char *category, bool mandatory)
{
	JSONObject *value = json_map_get(table, key);
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