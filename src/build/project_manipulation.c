#include "build_internal.h"
#define PRINTFN(string, ...) fprintf(stdout, string "\n", ##__VA_ARGS__) // NOLINT
#define PRINTF(string, ...) fprintf(stdout, string, ##__VA_ARGS__) // NOLINT
static JSONObject *read_project()
{
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
		error_exit("Expected a map of project information in '%s'.", PROJECT_JSON);
	}
	return json;
}

static void print_vec(const char *header, const char **vec, bool opt)
{
	if (opt && !vec) return;
	PRINTF("%s: ", header);
	if (!vec_size(vec))
	{
		PRINTFN("*none*");
		return;
	}
	FOREACH_IDX(i, const char *, item, vec)
	{
		if (i > 0) PRINTF(", ");
		PRINTF("%s", item);
	}
	PRINTFN("");
}

static void print_opt_str(const char *header, const char *str)
{
	if (!str) return;
	PRINTFN("%s: %s", header, str);
}

static void print_opt_setting(const char *header, int setting, const char **values)
{
	if (setting < 0) return;
	PRINTFN("%s: %s", header, values[setting]);
}

static void print_opt_bool(const char *header, int b)
{
	if (b == -1) return;
	PRINTF("%s: ", header);
	PRINTFN(b ? "true" : "false");
}

static void print_opt_int(const char *header, long v)
{
	if (v < 0) return;
	PRINTFN("%s: %ld", header, v);
}

static const char *generate_expected(const char **options, size_t n)
{
	scratch_buffer_clear();
	for (size_t i = 0; i < n; i++)
	{
		if (i > 0) scratch_buffer_append(", ");
		if (i == n - 1) scratch_buffer_append(" or ");
		scratch_buffer_printf("\"%s\"", options[i]);
	}
	return scratch_buffer_to_string();
}


const char *optimization_levels[] = {
	[OPT_SETTING_O0] = "O0",
	[OPT_SETTING_O1] = "O1",
	[OPT_SETTING_O2] = "O2",
	[OPT_SETTING_O3] = "O3",
	[OPT_SETTING_O4] = "O4",
	[OPT_SETTING_O5] = "O5",
	[OPT_SETTING_OSMALL] = "Os",
	[OPT_SETTING_OTINY] = "Oz"
};

const char *debug_levels[] = {
	[DEBUG_INFO_NONE] = "none",
	[DEBUG_INFO_LINE_TABLES] = "line-tables",
	[DEBUG_INFO_FULL] = "full"
};


#define VIEW_MANDATORY_STRING_ARRAY(header, key) \
do { \
	const char** arr = get_string_array(PROJECT_JSON, NULL, project_json, key, true);\
	print_vec(header, arr, false);\
} while(0)

#define VIEW_STRING_ARRAY(header, key) \
do { \
	const char** arr = get_optional_string_array(PROJECT_JSON, NULL, project_json, key);\
	print_vec(header, arr, true);\
} while(0)

#define VIEW_MANDATORY_STRING(header, key) \
do { \
	const char* str = get_mandatory_string(PROJECT_JSON, NULL, project_json, key);\
	print_opt_str(header, str);\
} while(0)

#define VIEW_STRING(header, key) \
do { \
	const char* str = get_optional_string(PROJECT_JSON, NULL, project_json, key);\
	print_opt_str(header, str);\
} while(0)

#define VIEW_SETTING(header, key, expected_arr) \
do { \
	int setting = get_valid_string_setting(PROJECT_JSON, NULL, project_json, key, expected_arr, 0, ELEMENTLEN(expected_arr), generate_expected(expected_arr, ELEMENTLEN(expected_arr)));\
 	print_opt_setting(header, setting, expected_arr);\
} while(0)

#define VIEW_BOOL(header, key) \
do {\
    int val = get_valid_bool(PROJECT_JSON, NULL, project_json, key, -1);\
	print_opt_bool(header, val);\
} while(0)

#define VIEW_INTEGER(header, key) \
do {\
	long v = get_valid_integer(project_json, key, PROJECT_JSON, false);\
    print_opt_int(header, v);\
} while(0);

#define TARGET_VIEW_STRING_ARRAY(header, key) \
do {\
    const char** arr = get_optional_string_array(PROJECT_JSON, name, target, key);\
	print_vec("\t" header, arr, true);\
} while(0)

#define TARGET_VIEW_MANDATORY_STRING(header, key) \
do { \
	const char* str = get_mandatory_string(PROJECT_JSON, name, target, key);\
	print_opt_str("\t" header, str);\
} while(0)

#define TARGET_VIEW_STRING(header, key) \
do { \
	const char* str = get_optional_string(PROJECT_JSON, name, target, key);\
	print_opt_str("\t" header, str);\
} while(0)


#define TARGET_VIEW_SETTING(header, key, expected_arr) \
do { \
	int setting = get_valid_string_setting(PROJECT_JSON, name, target, key, expected_arr, 0, ELEMENTLEN(expected_arr), generate_expected(expected_arr, ELEMENTLEN(expected_arr)));\
 	print_opt_setting("\t" header, setting, expected_arr);\
} while(0)

#define TARGET_VIEW_BOOL(header, key) \
do {\
    int val = get_valid_bool(PROJECT_JSON, name, target, key, -1);\
	print_opt_bool("\t" header, val);\
} while(0)

#define TARGET_VIEW_INTEGER(header, key) \
do {\
	long v = get_valid_integer(target, key, name, false);\
    print_opt_int("\t" header, v);\
} while(0);

static void view_target(const char *name, JSONObject *target)
{
	/* General target information */
	PRINTFN("- %s", name);
	print_opt_str("\tName", name);
	TARGET_VIEW_MANDATORY_STRING("Type", "type");
	TARGET_VIEW_STRING("Target language target", "langrev");
	TARGET_VIEW_STRING_ARRAY("Warnings used", "warnings");
	TARGET_VIEW_STRING_ARRAY("Additional c3l library search paths", "dependency-search-paths");
	TARGET_VIEW_STRING_ARRAY("c3l library search paths (override)", "dependency-search-paths-override");
	TARGET_VIEW_STRING_ARRAY("Additional c3l library dependencies", "dependencies");
	TARGET_VIEW_STRING_ARRAY("c3l library dependencies (override)", "dependencies-override");
	TARGET_VIEW_STRING_ARRAY("Additional source paths", "sources");
	TARGET_VIEW_STRING_ARRAY("Source paths (override)", "sources-override");
	TARGET_VIEW_STRING_ARRAY("Additional C source paths", "c-sources");
	TARGET_VIEW_STRING_ARRAY("C source paths (override)", "c-sources-override");
	TARGET_VIEW_STRING_ARRAY("Additional C source include directories", "c-include-dirs");
	TARGET_VIEW_STRING_ARRAY("C source include directories (override)", "c-include-dirs-override");
	TARGET_VIEW_SETTING("Optimization level", "opt", optimization_levels);

	/* Extended target information */
	TARGET_VIEW_STRING("Benchmark function override", "benchfn");
	TARGET_VIEW_STRING("C compiler", "cc");
	TARGET_VIEW_STRING("Additional C compiler flags", "cflags");
	TARGET_VIEW_STRING("C compiler flags (override)", "cflags-override");
	TARGET_VIEW_STRING("CPU name", "cpu");
	TARGET_VIEW_SETTING("Debug level", "debug-info", debug_levels);
	TARGET_VIEW_STRING_ARRAY("Additional scripts to run", "exec");
	TARGET_VIEW_STRING_ARRAY("Scripts to run (override)", "exec");
	TARGET_VIEW_STRING_ARRAY("Enabled features", "features");
	TARGET_VIEW_SETTING("Floating point behaviour", "fp-math", fp_math);
	TARGET_VIEW_STRING_ARRAY("Additional linked libraries", "linked-libraries");
	TARGET_VIEW_STRING_ARRAY("Linked libraries (override)", "linked-libraries-override");
	TARGET_VIEW_STRING("Linker", "linker");
	TARGET_VIEW_STRING_ARRAY("Additional linker search paths", "linker-search-paths");
	TARGET_VIEW_STRING_ARRAY("Linker search paths (override)", "linker-search-paths-override");
	TARGET_VIEW_STRING_ARRAY("Additional linker arguments", "link-args");
	TARGET_VIEW_STRING_ARRAY("Linker arguments (override)", "link-args-override");
	TARGET_VIEW_BOOL("Link libc", "link-libc");
	TARGET_VIEW_STRING("MacOS SDK directory", "macossdk");
	TARGET_VIEW_SETTING("Memory environment", "memory-env", memory_environment);
	TARGET_VIEW_BOOL("Don't generate/require main function", "no-entry");
	TARGET_VIEW_SETTING("Code optimization level", "optlevel", optlevels);
	TARGET_VIEW_SETTING("Code size optimization", "optsize", optsizes);
	TARGET_VIEW_STRING("Panic function override", "panicfn");
	TARGET_VIEW_BOOL("Panic message output", "panic-msg");
	TARGET_VIEW_SETTING("Relocation model", "reloc", reloc_models);
	TARGET_VIEW_BOOL("Runtime safety checks enabled", "safe");
	TARGET_VIEW_BOOL("Print backtrace on signals", "show-backtrace");
	TARGET_VIEW_STRING("Script directory", "script-dir");
	TARGET_VIEW_BOOL("Compile into single module", "single-module");
	TARGET_VIEW_BOOL("Output soft-float functions", "soft-float");
	TARGET_VIEW_BOOL("Strip unused code/globals", "strip-unused");
	TARGET_VIEW_INTEGER("Preferred symtab size", "symtab");
	TARGET_VIEW_STRING("Target", "target");
	TARGET_VIEW_STRING("Test function override", "testfn");
	TARGET_VIEW_BOOL("Integers panic on wrapping", "trap-on-wrap");
	TARGET_VIEW_BOOL("Include standard library", "use-stdlib");
	TARGET_VIEW_SETTING("Windows CRT linking", "wincrt", wincrt_linking);
	TARGET_VIEW_STRING("Windows SDK path", "winsdk");
	TARGET_VIEW_SETTING("x64 CPU level", "x86cpu", x86_cpu_set);
	TARGET_VIEW_SETTING("Max vector use type", "x86vec", x86_vector_capability);
	TARGET_VIEW_BOOL("Return structs on the stack", "x86-stack-struct-return");
}

void add_target_project(BuildOptions *build_options)
{
	JSONObject *project_json = read_project();
	JSONObject *targets_json = json_obj_get(project_json, "targets");

	for (unsigned i = 0; i < targets_json->member_len; i++)
	{
		JSONObject *object = targets_json->members[i];
		const char *key = targets_json->keys[i];

		if (key == NULL)
		{
			continue;
		}

		if (strcmp(key, build_options->project_options.target_name) == 0)
		{
			error_exit("Target with name '%s' already exists", key);
		}
	}

	JSONObject *target_type_obj = MALLOCS(JSONObject);
	target_type_obj->type = J_STRING;
	target_type_obj->str = targets[build_options->project_options.target_type];


	JSONObject *new_target = MALLOCS(JSONObject);
	new_target->type = J_OBJECT;

	new_target->members = malloc_arena(sizeof(JSONObject) * 16);
	new_target->keys = malloc_arena(sizeof(JSONObject) * 16);

	new_target->keys[0] = "type";
	new_target->members[0] = target_type_obj;
	new_target->member_len = 1;


	size_t index = targets_json->member_len;
	targets_json->members[index] = new_target;
	targets_json->keys[index] = build_options->project_options.target_name;
	targets_json->member_len++;

	FILE *file = fopen(PROJECT_JSON, "w");
	print_json_to_file(project_json, file);
	fclose(file);
}

void view_project(BuildOptions *build_options)
{
	JSONObject *project_json = read_project();

	/* General information */
	VIEW_MANDATORY_STRING_ARRAY("Authors", "authors");
	VIEW_MANDATORY_STRING("Version", "version");
	VIEW_MANDATORY_STRING("Project language target", "langrev");
	VIEW_MANDATORY_STRING_ARRAY("Warnings used", "warnings");
	VIEW_MANDATORY_STRING_ARRAY("c3l library search paths", "dependency-search-paths");
	VIEW_MANDATORY_STRING_ARRAY("c3l library dependencies", "dependencies");
	VIEW_MANDATORY_STRING_ARRAY("Source paths", "sources");
	VIEW_STRING_ARRAY("C source paths", "c-sources");
	VIEW_STRING("Output location", "output");
	VIEW_SETTING("Default optimization level", "opt", optimization_levels);

	/* Extended information */
	VIEW_STRING("Benchmark function override", "benchfn");
	VIEW_STRING("C compiler", "cc");
	VIEW_STRING("C compiler flags", "cflags");
	VIEW_STRING("CPU name", "cpu");
	VIEW_SETTING("Debug level", "debug-info", debug_levels);
	VIEW_STRING_ARRAY("Scripts to run", "exec");
	VIEW_STRING_ARRAY("Enabled features", "features");
	VIEW_SETTING("Floating point behaviour", "fp-math", fp_math);
	VIEW_STRING_ARRAY("Linked libraries", "linked-libraries");
	VIEW_STRING("Linker", "linker");
	VIEW_STRING_ARRAY("Linker search paths", "linker-search-paths");
	VIEW_STRING_ARRAY("Linker arguments", "link-args");
	VIEW_BOOL("Link libc", "link-libc");
	VIEW_STRING("MacOS SDK directory", "macossdk");
	VIEW_SETTING("Memory environment", "memory-env", memory_environment);
	VIEW_BOOL("Don't generate/require main function", "no-entry");
	VIEW_SETTING("Code optimization level", "optlevel", optlevels);
	VIEW_SETTING("Code size optimization", "optsize", optsizes);
	VIEW_STRING("Panic function override", "panicfn");
	VIEW_BOOL("Panic message output", "panic-msg");
	VIEW_SETTING("Relocation model", "reloc", reloc_models);
	VIEW_BOOL("Runtime safety checks enabled", "safe");
	VIEW_BOOL("Print backtrace on signals", "show-backtrace");
	VIEW_STRING("Script directory", "script-dir");
	VIEW_BOOL("Compile into single module", "single-module");
	VIEW_BOOL("Output soft-float functions", "soft-float");
	VIEW_BOOL("Strip unused code/globals", "strip-unused");
	VIEW_INTEGER("Preferred symtab size", "symtab");
	VIEW_STRING("Target", "target");
	VIEW_STRING("Test function override", "testfn");
	VIEW_BOOL("Integers panic on wrapping", "trap-on-wrap");
	VIEW_BOOL("Include standard library", "use-stdlib");
	VIEW_SETTING("Windows CRT linking", "wincrt", wincrt_linking);
	VIEW_STRING("Windows SDK path", "winsdk");
	VIEW_SETTING("x64 CPU level", "x86cpu", x86_cpu_set);
	VIEW_SETTING("Max vector use type", "x86vec", x86_vector_capability);
	VIEW_BOOL("Return structs on the stack", "x86-stack-struct-return");

	/* Target information */
	PRINTFN("Targets: ");
	JSONObject *targets_json = json_obj_get(project_json, "targets");
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

		if (object->type == J_COMMENT_LINE)
		{
			continue;
		}

		if (object->type != J_OBJECT)
		{
			error_exit("Invalid data in target '%s'", key);
		}
		view_target(key, object);
	}
}
