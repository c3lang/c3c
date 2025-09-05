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
			if (str_eq(key, valid_keys[j][0])) goto OK; // NOLINT
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


static void error_missing_mandatory(BuildParseContext context, const char *key)
{
	if (context.target) error_exit("In file '%s': The mandatory field '%s' was missing in '%s'.", context.file, key, context.target);
	error_exit("In file '%s': The mandatory field '%s' was missing.", context.file, key);
}

static void error_wrong_type(BuildParseContext context, const char *key, const char *expected)
{
	if (context.target) error_exit("In file '%s': '%s' had an invalid '%s' field that was not %s, please correct it.", context.file, context.target, key, expected);
	error_exit("File '%s' had an invalid '%s' field that was not %s, please correct it.", context.file, key, expected);

}
const char *get_optional_string(BuildParseContext context, JSONObject *table, const char *key)
{
	JSONObject *value = json_map_get(table, key);
	if (!value) return NULL;
	if (value->type != J_STRING)
	{
		error_wrong_type(context, key, "a string");
	}
	return value->str;
}

const char *get_mandatory_string(BuildParseContext context, JSONObject *object, const char *key)
{
	const char *value = get_optional_string(context, object, key);
	if (!value) error_missing_mandatory(context, key);
	return value;
}

const char *get_string(BuildParseContext context, JSONObject *table, const char *key,
                       const char *default_value)
{
	const char *value = get_optional_string(context, table, key);
	return value ? value : default_value;
}


int get_valid_bool(BuildParseContext context, JSONObject *json, const char *key, int default_val)
{
	JSONObject *value = json_map_get(json, key);
	if (!value) return default_val;
	if (value->type != J_BOOL) error_wrong_type(context, key, "a boolean");
	return value->b;
}

const char **get_string_array(BuildParseContext context, JSONObject *table, const char *key, bool mandatory)
{
	JSONObject *value = json_map_get(table, key);
	if (!value)
	{
		if (mandatory) error_missing_mandatory(context, key);
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
	error_wrong_type(context, key, "a string array");
	UNREACHABLE
}

const char **get_optional_string_array(BuildParseContext context, JSONObject *table, const char *key)
{
	return get_string_array(context, table, key, false);
}

const char *get_cflags(BuildParseContext context, JSONObject *json, const char *original_flags)
{
	// CFlags
	if (context.target)
	{
		const char *cflags = get_optional_string(context, json, "cflags-override");
		if (cflags) return cflags;
	}
	const char *cflags = get_optional_string(context, json, "cflags");
	if (!cflags) return original_flags;
	if (!original_flags) return cflags;
	return str_printf("%s %s", original_flags, cflags);
}

void get_list_append_strings(BuildParseContext context, JSONObject *json, const char ***list_ptr, const char *base, const char *override)
{
	const char **value = get_optional_string_array(context, json, context.target ? override : base);
	const char **add_value = context.target ? get_optional_string_array(context, json, base) : NULL;
	if (value) *list_ptr = value;
	if (add_value)
	{
		append_strings_to_strings(&add_value, *list_ptr);
		*list_ptr = add_value;
	}
}

int get_valid_string_setting(BuildParseContext context, JSONObject *json, const char *key, const char** values, int first_result, int count, const char *expected)
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
	if (context.target)
	{
		error_exit("In file '%s': '%s' had an invalid value for '%s', expected %s", context.file, context.target, key, expected);
	}
	error_exit("In file '%s': Invalid value for '%s', expected %s", context.file, key, expected);
	UNREACHABLE
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
	UNREACHABLE
}

long get_valid_integer(BuildParseContext context, JSONObject *table, const char *key, bool mandatory)
{
	JSONObject *value = json_map_get(table, key);
	if (!value)
	{
		if (mandatory) error_missing_mandatory(context, key);
		return -1;
	}
	if (value->type != J_NUMBER || trunc(value->f) != value->f)
	{
		error_wrong_type(context, key, "an integer");
	}
	return (long)trunc(value->f);
}
