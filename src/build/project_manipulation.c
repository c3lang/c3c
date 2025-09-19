#include "build/build.h"
#include "build_internal.h"
#include "project.h"
#include "utils/json.h"
#include "utils/lib.h"
#define PRINTFN(string, ...) fprintf(stdout, string "\n", ##__VA_ARGS__) // NOLINT
#define PRINTF(string, ...) fprintf(stdout, string, ##__VA_ARGS__) // NOLINT

const char** get_project_dependency_directories()
{
	const char *filename;
	JSONObject *json = project_json_load(&filename);

	const char *target = NULL;
	const char **deps_dirs = NULL;
	BuildParseContext context = { filename, target };
	APPEND_STRING_LIST(&deps_dirs, "dependency-search-paths");

	return deps_dirs;
}

static void print_vec(const char *header, const char **vec, bool opt, const char *delim)
{
	if (opt && !vec) return;
	if (header[0] != '\0') PRINTF("%s: ", header);
	if (!vec_size(vec))
	{
		PRINTFN("*none*");
		return;
	}
	FOREACH_IDX(i, const char *, item, vec)
	{
		if (i > 0) PRINTF("%s", delim);
		PRINTF("%s", item);
	}
	PRINTFN("");
}

const char** get_project_dependencies()
{
	const char *filename;
	const char** dependencies = NULL;

	JSONObject *project_json = project_json_load(&filename);
	JSONObject *dependencies_json = json_map_get(project_json, "dependencies");

	FOREACH(JSONObject *, element, dependencies_json->elements)
	{
		vec_add(dependencies, element->str);
	}
	return dependencies;
}

static void print_opt_str(const char *header, const char *str)
{
	if (!str) return;
	if (header[0] != '\0') PRINTF("%s: ", header);
	PRINTFN("%s", str);
}

static void print_opt_setting(const char *header, int setting, const char **values)
{
	if (setting < 0) return;
	if (header[0] != '\0') PRINTF("%s: ", header);
	PRINTFN("%s", values[setting]);
}

static void print_opt_bool(const char *header, int b)
{
	if (b == -1) return;
	if (header[0] != '\0') PRINTF("%s: ", header);
	PRINTFN(b ? "true" : "false");
}

static void print_opt_int(const char *header, long v)
{
	if (v < 0) return;
	if (header[0] != '\0') PRINTF("%s: ", header);
	PRINTFN("%ld", v);
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


#define VIEW_MANDATORY_STRING_ARRAY(header, key, delim) \
do { \
	const char** arr = get_string_array(context, project_json, key, true);\
	print_vec(header, arr, false, delim);\
} while(0)

#define VIEW_STRING_ARRAY(header, key, delim) \
do { \
	const char** arr = get_optional_string_array(context, project_json, key);\
	print_vec(header, arr, true, delim);\
} while(0)

#define VIEW_MANDATORY_STRING(header, key) \
do { \
	const char* str = get_mandatory_string(context, project_json, key);\
	print_opt_str(header, str);\
} while(0)

#define VIEW_STRING(header, key) \
do { \
	const char* str = get_optional_string(context, project_json, key);\
	print_opt_str(header, str);\
} while(0)

#define VIEW_SETTING(header, key, expected_arr) \
do { \
	int setting = get_valid_string_setting(context, project_json, key, expected_arr, 0, ELEMENTLEN(expected_arr), generate_expected(expected_arr, ELEMENTLEN(expected_arr)));\
 	print_opt_setting(header, setting, expected_arr);\
} while(0)

#define VIEW_BOOL(header, key) \
do {\
    int val = get_valid_bool(context, project_json, key, -1);\
	print_opt_bool(header, val);\
} while(0)

#define VIEW_INTEGER(header, key) \
do {\
	long v = get_valid_integer(context, project_json, context.file, false);\
    print_opt_int(header, v);\
} while(0);

#define TARGET_VIEW_STRING_ARRAY(header, key, delim) \
do {\
    const char** arr = get_optional_string_array(context, target, key);\
	print_vec("\t" header, arr, true, delim);\
} while(0)

#define TARGET_VIEW_MANDATORY_STRING(header, key) \
do { \
	const char* str = get_mandatory_string(context, target, key);\
	print_opt_str("\t" header, str);\
} while(0)

#define TARGET_VIEW_STRING(header, key) \
do { \
	const char* str = get_optional_string(context, target, key);\
	print_opt_str("\t" header, str);\
} while(0)


#define TARGET_VIEW_SETTING(header, key, expected_arr) \
do { \
	int setting = get_valid_string_setting(context, target, key, expected_arr, 0, ELEMENTLEN(expected_arr), generate_expected(expected_arr, ELEMENTLEN(expected_arr)));\
 	print_opt_setting("\t" header, setting, expected_arr);\
} while(0)

#define TARGET_VIEW_BOOL(header, key) \
do {\
    int val = get_valid_bool(context, target, key, -1);\
	print_opt_bool("\t" header, val);\
} while(0)

#define TARGET_VIEW_INTEGER(header, key) \
do {\
	long v = get_valid_integer(context, target, key, false);\
    print_opt_int("\t" header, v);\
} while(0);

static void view_target(BuildParseContext context, JSONObject *target, bool verbose)
{
	if (!verbose)
	{
		PRINTFN("%s", context.target);
		return;
	}
	/* General target information */
	PRINTFN("- %s", context.target);
	print_opt_str("\tName", context.target);
	TARGET_VIEW_MANDATORY_STRING("Type", "type");
	TARGET_VIEW_STRING("Target language target", "langrev");
	TARGET_VIEW_STRING("Target output name", "name");
	TARGET_VIEW_STRING_ARRAY("Warnings used", "warnings", ", ");
	TARGET_VIEW_STRING_ARRAY("Additional c3l library search paths", "dependency-search-paths", ", ");
	TARGET_VIEW_STRING_ARRAY("c3l library search paths (override)", "dependency-search-paths-override", ", ");
	TARGET_VIEW_STRING_ARRAY("Additional c3l library dependencies", "dependencies", ", ");
	TARGET_VIEW_STRING_ARRAY("c3l library dependencies (override)", "dependencies-override", ", ");
	TARGET_VIEW_STRING_ARRAY("Additional source paths", "sources", ", ");
	TARGET_VIEW_STRING_ARRAY("Source paths (override)", "sources-override", ", ");
	TARGET_VIEW_STRING_ARRAY("Additional C source paths", "c-sources", ", ");
	TARGET_VIEW_STRING_ARRAY("C source paths (override)", "c-sources-override", ", ");
	TARGET_VIEW_STRING_ARRAY("Additional C source include directories", "c-include-dirs", ", ");
	TARGET_VIEW_STRING_ARRAY("C source include directories (override)", "c-include-dirs-override", ", ");
	TARGET_VIEW_SETTING("Optimization level", "opt", optimization_levels);

	/* Extended target information */
	TARGET_VIEW_STRING("Benchmark function override", "benchfn");
	TARGET_VIEW_STRING("C compiler", "cc");
	TARGET_VIEW_STRING("Additional C compiler flags", "cflags");
	TARGET_VIEW_STRING("C compiler flags (override)", "cflags-override");
	TARGET_VIEW_STRING("CPU name", "cpu");
	TARGET_VIEW_SETTING("Debug level", "debug-info", debug_levels);
	TARGET_VIEW_STRING_ARRAY("Additional scripts to run", "exec", ", ");
	TARGET_VIEW_STRING_ARRAY("Scripts to run (override)", "exec", ", ");
	TARGET_VIEW_STRING_ARRAY("Enabled features", "features", ", ");
	TARGET_VIEW_SETTING("Floating point behaviour", "fp-math", fp_math);
	TARGET_VIEW_STRING_ARRAY("Additional linked libraries", "linked-libraries", ", ");
	TARGET_VIEW_STRING_ARRAY("Linked libraries (override)", "linked-libraries-override", ", ");
	TARGET_VIEW_STRING("Linker", "linker");
	TARGET_VIEW_STRING_ARRAY("Additional linker search paths", "linker-search-paths", ", ");
	TARGET_VIEW_STRING_ARRAY("Linker search paths (override)", "linker-search-paths-override", ", ");
	TARGET_VIEW_STRING_ARRAY("Additional linker arguments", "link-args", ", ");
	TARGET_VIEW_STRING_ARRAY("Linker arguments (override)", "link-args-override", ", ");
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
	TARGET_VIEW_STRING("Run directory", "run-dir");
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
	TARGET_VIEW_BOOL("Unroll loops", "unroll-loops");
	TARGET_VIEW_BOOL("SLP auto-vectorization", "slp-vectorize");
	TARGET_VIEW_BOOL("Loop auto-vectorization", "loop-vectorize");
	TARGET_VIEW_BOOL("Merge functions", "merge-functions");
}



#if FETCH_AVAILABLE
void fetch_project(BuildOptions* options)
{
	if (!file_exists(PROJECT_JSON5) && !file_exists(PROJECT_JSON))
	{
		error_exit("Failed: no project file found.");
	}

	if (str_eq(options->path, DEFAULT_PATH))
	{
		{
			const char** deps_dirs =  get_project_dependency_directories();
			int num_lib = vec_size(deps_dirs);
			if (num_lib > 0) options->vendor_download_path = deps_dirs[0];
		}
	}

	const char **libdirs = get_project_dependency_directories();
	const char **deps = get_project_dependencies();
	const char *filename;
	JSONObject *project_json = project_json_load(&filename);

	JSONObject *targets_json = json_map_get(project_json, "targets");

	if (targets_json && targets_json->type == J_OBJECT)
	{

		FOREACH_IDX(i, JSONObject *, target, targets_json->members)
		{
			const char *key = targets_json->keys[i];
			if (target->type != J_OBJECT)
			{
				error_exit("Invalid data in target '%s'", key);
			}

			const char **target_deps = get_optional_string_array((BuildParseContext) { filename, key }, target, "dependencies");

			FOREACH(const char*, dep, target_deps)
			{
				vec_add(deps, dep);
			}
		}
	}

	// dependency check tree
	while (vec_size(deps) > 0)
	{
		FOREACH(const char*, dir, libdirs)
		{
			const char *dep = VECLAST(deps);
			if (file_exists(file_append_path(dir, str_printf("%s.c3l", dep))))
			{
				vec_pop(deps);
				break;
			}

			printf("Fetching missing library '%s'...", dep);
			fflush(stdout);

			const char *error = vendor_fetch_single(dep, options->vendor_download_path);

			if (!error)
			{
				puts(" finished.");
			}
			else
			{
				printf("Failed: '%s'\n", error);
			}

			vec_pop(deps);
			break;
		}
	}
}
#else
void fetch_project(BuildOptions* options)
{
	error_exit("Error: project fetch only available when compiled with cURL.");
}
#endif


void add_libraries_to_project_file(const char** libs, const char* target_name) {

	if (!file_exists(PROJECT_JSON5) && !file_exists(PROJECT_JSON)) return;
	//TODO! Target name option not implemented

	const char *filename;
	JSONObject *project_json = project_json_load(&filename);

	// TODO! check if target is specified and exists (NULL at the moment)
	JSONObject *libraries_json = json_map_get(project_json, "dependencies");

	const char** dependencies = NULL;
	FOREACH(JSONObject *, element, libraries_json->elements)
	{
		vec_add(dependencies, element->str);
	}

	// check if libraries are already present
	FOREACH(const char*, lib, libs)
	{
		if (str_findlist(lib, vec_size(dependencies), dependencies)!=-1) continue;
		vec_add(dependencies, lib);
	}

	JSONObject** elements = NULL;
	FOREACH(const char*, dep, dependencies)
	{
		vec_add(elements, json_new_string(dep));
	}

	// Apply changes to JSON object
	libraries_json->elements = elements;

	// write to project json file
	FILE *file = fopen(filename, "w");
	if (!file) error_exit("Failed to open file '%s'", filename);
	print_json_to_file(project_json, file);
	fclose(file);
}

void add_target_project(BuildOptions *build_options)
{
	const char *filename;
	/* NOTE: this previously allowed project.json to not exist, and create it */
	JSONObject *project_json = project_json_load(&filename);
	JSONObject *targets_json = json_map_get(project_json, "targets");

	if (targets_json == NULL)
	{
		targets_json = json_new_object(J_OBJECT);
		json_map_set(project_json, "targets", targets_json);
	}

	if (json_map_get(targets_json, build_options->project_options.target_name) != NULL)
	{
		error_exit("Target with name '%s' already exists", build_options->project_options.target_name);
	}

	JSONObject *target_type_obj = json_new_string(targets[build_options->project_options.target_type]);

	JSONObject *new_target = json_new_map();
	json_map_set(new_target, "type", target_type_obj);
	JSONObject *target_sources = json_new_object(J_ARRAY);
	FOREACH(const char *, source, build_options->project_options.sources)
	{
		vec_add(target_sources->elements, json_new_string(source));
	}
	json_map_set(new_target, "sources", target_sources);

	json_map_set(targets_json, build_options->project_options.target_name, new_target);

	FILE *file = fopen(filename, "w");
	if (!file) error_exit("Failed to open file '%s'", filename);
	print_json_to_file(project_json, file);
	fclose(file);
}

static void view_filtered_project_properties(BuildOptions *build_options, const char* filename, JSONObject *project_json)
{
	uint16_t bitvector = build_options->project_options.view_modifier.flags_bitvector;
	bool verbose = build_options->project_options.view_modifier.verbose;
	BuildParseContext context = { filename, NULL };
	const char* delim = verbose ? ", " : "\n";
	char* prop_header;

	if (bitvector & (1 << 0))
	{
		prop_header = verbose ? "Authors" : "";
		VIEW_STRING_ARRAY(prop_header, "authors", delim);
		PRINTFN("");
	}
	if (bitvector & (1 << 1))
	{
		prop_header = verbose ? "Version" : "";
		VIEW_STRING(prop_header, "version");
		PRINTFN("");
	}
	if (bitvector & (1 << 2))
	{
		prop_header = verbose ? "Project language target" : "";
		VIEW_STRING(prop_header, "langrev");
		PRINTFN("");
	}
	if (bitvector & (1 << 3))
	{
		prop_header = verbose ? "Warnings used" : "";
		VIEW_STRING_ARRAY(prop_header, "warnings", delim);
		PRINTFN("");
	}
	if (bitvector & (1 << 4))
	{
		prop_header = verbose ? "c3l library search paths" : "";
		VIEW_STRING_ARRAY(prop_header, "dependency-search-paths", delim);
		PRINTFN("");
	}
	if (bitvector & (1 << 5))
	{
		prop_header = verbose ? "c3l library dependencies" : "";
		VIEW_STRING_ARRAY(prop_header, "dependencies", delim);
		PRINTFN("");
	}
	if (bitvector & (1 << 6))
	{
		prop_header = verbose ? "Source paths" : "";
		VIEW_STRING_ARRAY(prop_header, "sources", delim);
		PRINTFN("");
	}
	if (bitvector & (1 << 7))
	{
		prop_header = verbose ? "Output location" : "";
		VIEW_STRING(prop_header, "output");
		PRINTFN("");
	}
	if (bitvector & (1 << 8))
	{
		prop_header = verbose ? "Default optimization level" : "";
		VIEW_SETTING(prop_header, "opt", optimization_levels);
		PRINTFN("");
	}

	/* Target information */
	if (bitvector & (1 << 9))
	{
		if (verbose) PRINTFN("Targets: ");
		JSONObject *targets_json = json_map_get(project_json, "targets");
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
			view_target((BuildParseContext) { filename, key }, object, verbose);
		}
	}
}

void view_project(BuildOptions *build_options)
{
	const char *filename;
	JSONObject *project_json = project_json_load(&filename);

	bool filter_properties = build_options->project_options.view_modifier.flags_bitvector != 0;

	if (filter_properties)
	{
		view_filtered_project_properties(build_options, filename, project_json);
		return;
	}

	BuildParseContext context = { filename, NULL };
	/* General information */
	VIEW_STRING_ARRAY("Authors", "authors", ", ");
	VIEW_STRING("Version", "version");
	VIEW_STRING("Project language target", "langrev");
	VIEW_STRING_ARRAY("Warnings used", "warnings", ", ");
	VIEW_STRING_ARRAY("c3l library search paths", "dependency-search-paths", ", ");
	VIEW_STRING_ARRAY("c3l library dependencies", "dependencies", ", ");
	VIEW_STRING_ARRAY("Source paths", "sources", ", ");
	VIEW_STRING_ARRAY("C source paths", "c-sources", ", ");
	VIEW_STRING("Output location", "output");
	VIEW_STRING("Output extension", "extension");
	VIEW_SETTING("Default optimization level", "opt", optimization_levels);

	/* Extended information */
	VIEW_STRING("Benchmark function override", "benchfn");
	VIEW_STRING("C compiler", "cc");
	VIEW_STRING("C compiler flags", "cflags");
	VIEW_STRING("CPU name", "cpu");
	VIEW_SETTING("Debug level", "debug-info", debug_levels);
	VIEW_STRING_ARRAY("Scripts to run", "exec", ", ");
	VIEW_STRING_ARRAY("Enabled features", "features", ", ");
	VIEW_SETTING("Floating point behaviour", "fp-math", fp_math);
	VIEW_STRING_ARRAY("Linked libraries", "linked-libraries", ", ");
	VIEW_STRING("Linker", "linker");
	VIEW_STRING_ARRAY("Linker search paths", "linker-search-paths", ", ");
	VIEW_STRING_ARRAY("Linker arguments", "link-args", ", ");
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
	VIEW_STRING("Run directory", "run-dir");
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
	JSONObject *targets_json = json_map_get(project_json, "targets");
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
		view_target((BuildParseContext) {filename, key }, object, true);
	}
}
