#include "build/build.h"
#include "build_internal.h"
#include "project.h"
#include "utils/json.h"
#include "utils/lib.h"
#define PRINTFN(string, ...) fprintf(stdout, string "\n", ##__VA_ARGS__) // NOLINT
#define PRINTF(string, ...) fprintf(stdout, string, ##__VA_ARGS__) // NOLINT

bool use_ansi(void);

const char** get_project_dependency_directories(void)
{
	const char *filename;
	JSONObject *json = project_json_load(&filename);

	const char *target = NULL;
	const char **deps_dirs = NULL;
	BuildParseContext context = { filename, target, false };
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
	TARGET_VIEW_STRING("CPU flags", "cpu-flags");
	TARGET_VIEW_STRING("CPU flags (override)", "cpu-flags-override");
	TARGET_VIEW_SETTING("Debug level", "debug-info", debug_levels);
	TARGET_VIEW_STRING_ARRAY("Targets built first", "depends-on", ", ");
	TARGET_VIEW_STRING_ARRAY("Additional environment variables for scripts", "env", ", ");
	TARGET_VIEW_STRING_ARRAY("Environment variables for scripts (override)", "env-override", ", ");
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
	TARGET_VIEW_BOOL("Custom libc", "custom-libc");
	TARGET_VIEW_STRING("MacOS SDK directory", "macos-sdk");
	TARGET_VIEW_SETTING("Memory environment", "memory-env", memory_environment);
	TARGET_VIEW_BOOL("Don't generate/require main function", "no-entry");
	TARGET_VIEW_SETTING("Code optimization level", "optlevel", optlevels);
	TARGET_VIEW_SETTING("Code size optimization", "optsize", optsizes);
	TARGET_VIEW_STRING("Panic function override", "panicfn");
	TARGET_VIEW_BOOL("Panic message output", "panic-msg");
	TARGET_VIEW_SETTING("Relocation model", "reloc", reloc_models);
	TARGET_VIEW_BOOL("Runtime safety checks enabled", "safe");
	TARGET_VIEW_BOOL("Print backtrace on signals", "show-backtrace");
	TARGET_VIEW_STRING("Exec directory", "exec-dir");
	TARGET_VIEW_STRING("Script directory", "script-dir");
	TARGET_VIEW_STRING("Run directory", "run-dir");
	TARGET_VIEW_BOOL("Compile into single module", "single-module");
	TARGET_VIEW_BOOL("Output soft-float functions", "soft-float");
	TARGET_VIEW_BOOL("Strip unused code/globals", "strip-unused");
	TARGET_VIEW_INTEGER("Preferred symtab size", "symtab");
	TARGET_VIEW_STRING("Target", "target");
	TARGET_VIEW_SETTING("RISC-V CPU level", "riscv-cpu", riscv_cpu_set);
	TARGET_VIEW_SETTING("RISC-V ABI", "riscv-abi", riscv_abi);
	TARGET_VIEW_STRING("Test function override", "testfn");
	TARGET_VIEW_BOOL("Integers panic on wrapping", "trap-on-wrap");
	TARGET_VIEW_BOOL("Include standard library", "use-stdlib");
	TARGET_VIEW_SETTING("Windows CRT linking", "wincrt", wincrt_linking);
	TARGET_VIEW_STRING("Windows SDK path", "win-sdk");
	TARGET_VIEW_SETTING("x64 CPU level", "x86cpu", x86_cpu_set);
	TARGET_VIEW_SETTING("Max vector use type", "x86vec", x86_vector_capability);
	TARGET_VIEW_BOOL("Return structs on the stack", "x86-stack-struct-return");
	TARGET_VIEW_BOOL("Unroll loops", "unroll-loops");
	TARGET_VIEW_BOOL("SLP auto-vectorization", "slp-vectorize");
	TARGET_VIEW_BOOL("Loop auto-vectorization", "loop-vectorize");
	TARGET_VIEW_BOOL("Merge functions", "merge-functions");
	TARGET_VIEW_BOOL("Function sections", "function-sections");
	TARGET_VIEW_BOOL("Data sections", "data-sections");
}

// A list of the project, or of the target if there is one: as when building, the target's own
// entries go first and "<key>-override" replaces the project's.
static const char **get_list(const char *file, JSONObject *project, const char *target, const char *key, const char *override_key)
{
	const char **list = NULL;
	BuildParseContext context = { file, NULL, false };
	get_list_append_strings(context, project, &list, key, override_key);
	if (!target) return list;
	context.target = target;
	get_list_append_strings(context, json_map_get(json_map_get(project, "targets"), target), &list, key, override_key);
	return list;
}

// The registry named by "fetch-url" in the project file, or NULL.
const char *project_fetch_url(void)
{
	if (!file_exists(PROJECT_JSON5) && !file_exists(PROJECT_JSON)) return NULL;
	const char *filename;
	JSONObject *url = json_map_get(project_json_load(&filename), "fetch-url");
	if (!url) return NULL;
	if (url->type != J_STRING) error_exit("'fetch-url' in '%s' must be a string.", filename);
	return url->str;
}

// Libraries are installed as <name>.c3l, so two dependencies of the target (or of the project if target is NULL)
// with the same name can't be used together, unless it is the same dependency written twice.
static void check_dependency_names(const char *file, JSONObject *project, const char *target)
{
	const char **deps = get_list(file, project, target, "dependencies", "dependencies-override");
	if (!vec_size(deps)) return;
	FOREACH_IDX(dep_index, const char *, dep, deps)
	{
		const char *name = dependency_parse(dep).name;
		for (int i = 0; i < dep_index; i++)
		{
			if (!str_eq(dependency_parse(deps[i]).name, name) || str_eq(deps[i], dep)) continue;
			error_exit("Failed: the dependencies '%s' and '%s' would both be installed as '%s.c3l'.", deps[i], dep, name);
		}
	}
}

// Fetch the dependencies of the target (or of the project if target is NULL) missing from its search paths.
// A dependency "[<url>:]<namespace>/<name>[@<version>]" comes from the registry (the one at <url> if it has one),
// any other from the vendor collection. The dependencies of a target include those of the project, so
// `handled` lists the ones already dealt with in this run.
static void fetch_missing_dependencies(BuildOptions *options, const char *file, JSONObject *project, const char *target, const char ***handled)
{
	const char **deps = get_list(file, project, target, "dependencies", "dependencies-override");
	if (!vec_size(deps)) return;
	const char **dirs = get_list(file, project, target, "dependency-search-paths", "dependency-search-paths-override");
	if (!vec_size(dirs))
	{
		error_exit("Failed: no 'dependency-search-paths' to fetch '%s' into.\n"
				   "Add e.g. \"dependency-search-paths\": [ \"lib\" ] to the project file.", deps[0]);
	}
	FOREACH(const char *, dep, deps)
	{
		if (str_findlist(dep, vec_size(*handled), *handled) != -1) continue;
		vec_add(*handled, dep);
		Dependency library = dependency_parse(dep);
		const char *lib_file = str_printf("%s.c3l", library.name);
		// New libraries go into the first search path, an installed one is replaced where it is.
		const char *install_dir = dirs[0];
		bool found = false;
		FOREACH(const char *, dir, dirs)
		{
			if (found || !file_exists(file_append_path(dir, lib_file))) continue;
			found = true;
			install_dir = dir;
		}
		if (found)
		{
			// An installed library is kept, unless the dependency asks for a version and the
			// manifest of the installed library says it is another one.
			const char *installed_version = NULL;
			if (library.registry && library.version) installed_version = library_version(file_append_path(install_dir, lib_file));
			if (!installed_version || str_eq(installed_version, library.version)) continue;
			printf("Updating library '%s' (installed version is %s)...\n", dep, installed_version);
		}
		else
		{
			printf("Fetching missing library '%s'...\n", dep);
		}
		fflush(stdout);
		file_create_folders(file_append_path(install_dir, lib_file));
		const char *error;
		if (library.registry)
		{
			error = registry_fetch_single(options, &library, install_dir, NULL, use_ansi());
		}
		else
		{
			error = vendor_fetch_single(dep, install_dir, use_ansi(), !options->fetch_compressed);
		}
		if (error) printf("Failed: '%s'\n", error);
	}
}

void fetch_project(BuildOptions* options)
{
	if (!download_available())
	{
		error_exit("The 'project fetch' command requires libcurl to download dependencies.\n"
				   "Please ensure libcurl is installed on your system.");
	}
	if (!file_exists(PROJECT_JSON5) && !file_exists(PROJECT_JSON))
	{
		error_exit("Failed: no project file found.");
	}

	// First the dependencies of the project itself, then those of each target.
	const char *filename;
	JSONObject *project = project_json_load(&filename);
	JSONObject *project_targets = json_map_get(project, "targets");
	if (project_targets && project_targets->type != J_OBJECT) project_targets = NULL;
	// Check all the names before fetching anything.
	check_dependency_names(filename, project, NULL);
	if (project_targets)
	{
		FOREACH_IDX(i, JSONObject *, target, project_targets->members)
		{
			if (target->type != J_OBJECT) error_exit("Invalid data in target '%s'", project_targets->keys[i]);
			check_dependency_names(filename, project, project_targets->keys[i]);
		}
	}
	const char **handled = NULL;
	fetch_missing_dependencies(options, filename, project, NULL, &handled);
	if (!project_targets) return;
	FOREACH(const char *, target_name, project_targets->keys)
	{
		fetch_missing_dependencies(options, filename, project, target_name, &handled);
	}
}



// Add the libraries to "dependencies". A library is listed once, by name: a registry library
// ("<namespace>/<name>@<version>") replaces an older entry for it, a plain name never does.
// The registry is saved as "fetch-url" if the project doesn't have one yet. If the project has another
// one, the libraries are listed with their own registry: "<url>:<namespace>/<name>@<version>".
void add_libraries_to_project_file(const char** libs, const char *fetch_url) {

	if (!vec_size(libs)) return;
	if (!file_exists(PROJECT_JSON5) && !file_exists(PROJECT_JSON)) return;

	const char *filename;
	JSONObject *project_json = project_json_load(&filename);

	JSONObject *libraries_json = json_map_get(project_json, "dependencies");
	if (!libraries_json)
	{
		libraries_json = json_new_object(J_ARRAY);
		json_map_set(project_json, "dependencies", libraries_json);
	}

	const char** dependencies = NULL;
	FOREACH(JSONObject *, element, libraries_json->elements)
	{
		vec_add(dependencies, element->str);
	}

	const char *project_url = project_fetch_url();
	bool own_url = fetch_url && project_url && !str_eq(project_url, fetch_url);

	FOREACH(const char*, lib, libs)
	{
		Dependency library = dependency_parse(lib);
		if (own_url && library.registry && !library.url)
		{
			printf("'%s' is fetched from '%s', and not from the 'fetch-url' of the project, so it is listed with that URL.\n", lib, fetch_url);
			library.url = fetch_url;
			lib = dependency_string(&library);
		}
		int present = -1;
		FOREACH_IDX(i, const char *, dependency, dependencies)
		{
			if (str_eq(dependency_parse(dependency).name, library.name)) present = i;
		}
		if (present == -1) {
			vec_add(dependencies, lib);
		}
		else if (library.registry) {
			dependencies[present] = lib;
		}
	}
	if (fetch_url && !project_url)
	{
		json_map_set(project_json, "fetch-url", json_new_string(fetch_url));
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
	BuildParseContext context = { filename, NULL, false };
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
			view_target((BuildParseContext) { filename, key, false }, object, verbose);
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

	BuildParseContext context = { filename, NULL, false };
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
	VIEW_STRING("Build location", "build-dir");
	VIEW_STRING("Output extension", "extension");
	VIEW_SETTING("Default optimization level", "opt", optimization_levels);

	/* Extended information */
	VIEW_STRING("Benchmark function override", "benchfn");
	VIEW_STRING("C compiler", "cc");
	VIEW_STRING("C compiler flags", "cflags");
	VIEW_STRING("Android API version", "android-api");
	VIEW_STRING("Android NDK directory", "android-ndk");
	VIEW_STRING("CPU name", "cpu");
	VIEW_STRING("CPU flags", "cpu-flags");
	VIEW_SETTING("Debug level", "debug-info", debug_levels);
	VIEW_STRING_ARRAY("Environment variables for scripts", "env", ", ");
	VIEW_STRING_ARRAY("Scripts to run", "exec", ", ");
	VIEW_STRING_ARRAY("Enabled features", "features", ", ");
	VIEW_SETTING("Floating point behaviour", "fp-math", fp_math);
	VIEW_STRING_ARRAY("Linked libraries", "linked-libraries", ", ");
	VIEW_STRING("Linker", "linker");
	VIEW_STRING_ARRAY("Linker search paths", "linker-search-paths", ", ");
	VIEW_STRING_ARRAY("Linker arguments", "link-args", ", ");
	VIEW_BOOL("Link libc", "link-libc");
	VIEW_BOOL("Custom libc", "custom-libc");
	VIEW_STRING("MacOS SDK directory", "macos-sdk");
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
	VIEW_STRING("Exec directory", "exec-dir");
	VIEW_STRING("Run directory", "run-dir");
	VIEW_BOOL("Compile into single module", "single-module");
	VIEW_BOOL("Output soft-float functions", "soft-float");
	VIEW_BOOL("Strip unused code/globals", "strip-unused");
	VIEW_INTEGER("Preferred symtab size", "symtab");
	VIEW_STRING("Target", "target");
	VIEW_SETTING("RISC-V CPU level", "riscv-cpu", riscv_cpu_set);
	VIEW_SETTING("RISC-V ABI", "riscv-abi", riscv_abi);
	VIEW_STRING("Test function override", "testfn");
	VIEW_BOOL("Integers panic on wrapping", "trap-on-wrap");
	VIEW_BOOL("Include standard library", "use-stdlib");
	VIEW_SETTING("Windows CRT linking", "wincrt", wincrt_linking);
	VIEW_STRING("Windows SDK path", "win-sdk");
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
		view_target((BuildParseContext) {filename, key, false }, object, true);
	}
}
