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


static const char *optimization_levels[] = {
	[OPT_SETTING_O0] = "O0",
	[OPT_SETTING_O1] = "O1",
	[OPT_SETTING_O2] = "O2",
	[OPT_SETTING_O3] = "O3",
	[OPT_SETTING_O4] = "O4",
	[OPT_SETTING_O5] = "O5",
	[OPT_SETTING_OSMALL] = "Os",
	[OPT_SETTING_OTINY] = "Oz"
};

static const char *debug_levels[] = {
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

// The remote named by "fetch-url" in the project file, or NULL.
const char *project_fetch_url(void)
{
	if (!file_exists(PROJECT_JSON5) && !file_exists(PROJECT_JSON)) return NULL;
	const char *filename;
	JSONObject *url = json_map_get(project_json_load(&filename), "fetch-url");
	if (!url) return NULL;
	if (url->type != J_STRING) error_exit("'fetch-url' in '%s' must be a string.", filename);
	return url->str;
}

// The search path where <name>.c3l is installed, or NULL.
static const char *find_library_dir(const char **dirs, const char *name)
{
	const char *lib_file = str_printf("%s.c3l", name);
	FOREACH(const char *, dir, dirs)
	{
		if (file_exists(file_append_path(dir, lib_file))) return dir;
	}
	return NULL;
}

// The strings in the "dependencies" of a manifest or one of its targets, skipping anything malformed.
static void add_manifest_dependencies(const char ***deps, JSONObject *json)
{
	JSONObject *list = json_map_get(json, "dependencies");
	if (!list || list->type != J_ARRAY) return;
	FOREACH(JSONObject *, dep, list->elements)
	{
		if (dep->type == J_STRING && !parse_library_path(dep->str).error) vec_add(*deps, dep->str);
	}
}

// The dependencies in the manifests of the installed libraries that `direct` depends on, directly or not.
static const char **derived_dependencies(const char **dirs, const char **direct)
{
	const char **derived = NULL;
	const char **pending = NULL;
	const char **visited = NULL;
	FOREACH(const char *, dep, direct) vec_add(pending, dep);
	for (int i = 0; i < vec_size(pending); i++)
	{
		// Let's get the actual name.
		const char *name = parse_library_path(pending[i]).name;
		if (str_findlist(name, vec_size(visited), visited) != -1) continue;
		vec_add(visited, name);
		const char *dir = find_library_dir(dirs, name);
		if (!dir) continue;
		const char *manifest_path;
		JSONObject *manifest;
		// Mangled or missing manifest, ignore it, so that the library can be fetched again.
		const char *error;
		manifest = load_library_manifest(file_append_path(dir, str_printf("%s.c3l", name)), &error, &manifest_path);
		if (!manifest || manifest->type != J_OBJECT) continue;
		// Pick the main dependencies
		const char **deps = NULL;
		add_manifest_dependencies(&deps, manifest);
		JSONObject *targets = json_map_get(manifest, "targets");
		if (targets && targets->type == J_OBJECT)
		{
			// Add the target dependencies
			FOREACH(JSONObject *, target, targets->members)
			{
				if (target->type == J_OBJECT) add_manifest_dependencies(&deps, target);
			}
		}
		// Add everything to pending and derived sets.
		FOREACH(const char *, dep, deps)
		{
			vec_add(derived, dep);
			vec_add(pending, dep);
		}
	}
	return derived;
}

// A field the dependency gives must be the same in the argument, which takes it if it has none.
static bool field_matches(const char **arg_field, const char *dep_field)
{
	if (!dep_field) return true;
	if (!*arg_field) *arg_field = dep_field;
	return str_eq(*arg_field, dep_field);
}

// What to fetch for `arg`, which must name a dependency of the project, or of a library it depends on.
// A bare name uses the path of the dependency, or else is looked up in the registry.
// The registry is the project's for its own dependencies, and the default one for those of libraries.
static LibraryPathInfo resolve_fetch(BuildOptions *options, const char *arg, const char **direct, const char **derived)
{
	LibraryPathInfo wanted = parse_library_path(arg);
	if (wanted.error) error_exit("Failed: '%s' is not a valid library, %s.", arg, wanted.error);
	bool found = false;
	bool found_direct = false;
	const char *path = NULL;
	bool path_direct = false;
	const char **dep_set[2] = { direct, derived };
	for (int i = 0; i < 2; i++)
	{
		const char **deps = dep_set[i];
		FOREACH(const char *, dep, deps)
		{
			LibraryPathInfo info = parse_library_path(dep);
			if (!str_eq(info.name, wanted.name)) continue;
			found = true;
			if (deps == direct) found_direct = true;
			if (!info.use_remote_fetch) continue;
			if (path && !str_eq(path, dep))
			{
				error_exit("Failed: '%s' is ambiguous, the dependencies have both '%s' and '%s'.", arg, path, dep);
			}
			if (!path) path_direct = deps == direct;
			path = dep;
		}
	}
	if (!found) error_exit("Failed: '%s' is not a dependency of the project, or of the libraries it depends on.", arg);

	const char *base = DEFAULT_FETCH_URL;
	if ((path && path_direct) || (!path && found_direct)) base = registry_url(options);

	LibraryPathInfo library;
	if (wanted.use_remote_fetch)
	{
		library = wanted;
		if (path)
		{
			// Given something like foo/bar as the fetch parameter, we require that the path also contains foo/bar
			// so the user-provided info needs to match the project provided data.
			LibraryPathInfo dep = parse_library_path(path);
			if (!str_eq(dep.id, library.id) || !field_matches(&library.url, dep.url)
				|| !field_matches(&library.version, dep.version) || !field_matches(&library.checksum, dep.checksum))
			{
				error_exit("Failed: '%s' doesn't match the dependency '%s'.", arg, path);
			}
		}
		if (!library.url) library.url = base;
		return library;
	}

	// We just have the path.
	if (path)
	{
		library = parse_library_path(path);
		if (!library.url) library.url = base;
		return library;
	}

	// Try to find the library in the remote.
	const char *id;
	const char *error = registry_find_library(base, wanted.name, &id);
	if (error) error_exit("Failed: %s.", error);
	library = parse_library_path(id);
	if (library.error) error_exit("Failed: the registry has an invalid library '%s', %s.", id, library.error);
	if (!library.url) library.url = base;
	return library;
}


// Fetch the libraries named on the command line, in order. Each must be a dependency of the project,
// or of a library that is installed, possibly by an earlier one of them.
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

	const char *filename;
	JSONObject *project = project_json_load(&filename);
	JSONObject *project_targets = json_map_get(project, "targets");
	if (project_targets && project_targets->type != J_OBJECT) project_targets = NULL;
	const char **direct = NULL;
	str_list_add_all_unique(&direct, get_list(filename, project, NULL, "dependencies", "dependencies-override"));
	if (project_targets)
	{
		FOREACH_IDX(i, JSONObject *, target, project_targets->members)
		{
			if (target->type != J_OBJECT) error_exit("Invalid data in target '%s'", project_targets->keys[i]);
			str_list_add_all_unique(&direct, get_list(filename, project, project_targets->keys[i], "dependencies", "dependencies-override"));
		}
	}
	FOREACH(const char *, dep, direct)
	{
		const char *error = parse_library_path(dep).error;
		if (error) error_exit("Failed: invalid dependency '%s' in the project file, %s.", dep, error);
	}
	const char **dirs = get_list(filename, project, NULL, "dependency-search-paths", "dependency-search-paths-override");
	if (!vec_size(dirs))
	{
		error_exit("Failed: no 'dependency-search-paths' to fetch into.\n"
				   "Add e.g. \"dependency-search-paths\": [ \"lib\" ] to the project file.");
	}

	if (!vec_size(options->libraries_to_fetch))
	{
		const char **all = NULL;
		str_list_add_all_unique(&all, direct);
		str_list_add_all_unique(&all, derived_dependencies(dirs, direct));
		const char **missing = NULL;
		FOREACH(const char *, dep, all)
		{
			const char *name = parse_library_path(dep).name;
			if (find_library_dir(dirs, name) || str_findlist(name, vec_size(missing), missing) != -1) continue;
			vec_add(missing, name);
		}
		if (!vec_size(missing))
		{
			puts("All dependencies are installed.");
			return;
		}
		puts("Missing dependencies, fetch them with 'c3c project fetch <library>...':");
		FOREACH(const char *, name, missing) printf("  %s\n", name);
		return;
	}

	FOREACH(const char *, arg, options->libraries_to_fetch)
	{
		LibraryPathInfo library = resolve_fetch(options, arg, direct, derived_dependencies(dirs, direct));
		// A new library goes into the first search path, an installed one is replaced where it is.
		const char *dir = find_library_dir(dirs, library.name);
		printf("Fetching '%s'...\n", library_path_info_to_string(&library));
		fflush(stdout);
		const char *error = registry_fetch_single(options, &library, dir ? dir : dirs[0], use_ansi());
		if (error) error_exit("Failed to fetch '%s': %s", arg, error);
	}
}

// Add the library names to "dependencies", unless there already is a dependency with that name.
void add_libraries_to_project_file(const char** libs)
{
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

	FOREACH(const char*, lib, libs)
	{
		FOREACH(const char *, dependency, dependencies)
		{
			if (str_eq(parse_library_path(dependency).name, lib)) goto NEXT;
		}
		vec_add(dependencies, lib);
		NEXT:;
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

	JSONObject *target_type_obj = json_new_string(target_types[build_options->project_options.target_type]);

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
