#include "build_internal.h"
#define PRINTFN(string, ...) fprintf(stdout, string "\n", ##__VA_ARGS__) // NOLINT
#define PRINTF(string, ...) fprintf(stdout, string, ##__VA_ARGS__) // NOLINT
static JSONObject* read_project() {
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

static void print_vec(const char* header, const char** vec, bool opt) {
	if (opt && !vec) return;
	PRINTF("%s: ", header);
	if (vec_size(vec) > 0) {
		for (int i = 0; i < vec_size(vec); ++i) {
			if (i > 0) {
				PRINTF(", ");
			}
			PRINTF("%s",vec[i]);
		}
		PRINTFN("");
	} else {
		PRINTFN("*none*");
	}
}

static void print_opt_str(const char* header, const char* str) {
	if (!str) return;
	PRINTFN("%s: %s", header, str);
}

static void print_opt_setting(const char* header, int setting, const char** values) {
	if (setting < 0) return;
	PRINTFN("%s: %s", header, values[setting]);
}

const char* optimization_levels[] = {
	[OPT_SETTING_O0] = "O0",
	[OPT_SETTING_O1] = "O1",
	[OPT_SETTING_O2] = "O2",
	[OPT_SETTING_O3] = "O3",
	[OPT_SETTING_O4] = "O4",
	[OPT_SETTING_O5] = "O5",
	[OPT_SETTING_OSMALL] = "Small",
	[OPT_SETTING_OTINY] = "Tiny" 
};

const char* optimization_level_expected = "one of \"O0\", \"O1\", \"O2\", \"O3\", \"O4\", \"O5\", \"Os\", or \"Oz\"";

static void view_target(const char* name, JSONObject* target) {
	PRINTFN("- %s",name);
	print_opt_str("\tName", name);
	const char* type = get_mandatory_string(PROJECT_JSON,name,target,"type");
	print_opt_str("\tType",type);
}

void view_project(BuildOptions *build_options) {
	JSONObject* project_json = read_project();

	/* General information */
	const char** authors = get_string_array(PROJECT_JSON, NULL, project_json, "authors", true);
	print_vec("Authors", authors, false);
	const char* version = get_mandatory_string(PROJECT_JSON, NULL, project_json, "version");
	print_opt_str("version", version);
	const char* langrev = get_mandatory_string(PROJECT_JSON, NULL, project_json, "langrev");
	PRINTFN("Project language target: %s", langrev);
	const char** warnings = get_string_array(PROJECT_JSON, NULL, project_json, "warnings", true);
	print_vec("Warnings used", warnings, false);
	const char** search_paths = get_string_array(PROJECT_JSON, NULL, project_json, "dependency-search-paths", true);
	print_vec("Search paths, .c3l files", search_paths, false);
	const char** dependencies = get_string_array(PROJECT_JSON, NULL, project_json, "dependencies", true);
	print_vec("Dependencies, .c3l", dependencies, false);
	const char** sources = get_string_array(PROJECT_JSON, NULL, project_json, "sources", true);
	print_vec("Source folders", sources, false);
	const char** c_sources = get_string_array(PROJECT_JSON, NULL, project_json, "c-sources", false);
	print_vec("Source folders, c files", c_sources, true);
	const char* output = get_mandatory_string(PROJECT_JSON, NULL, project_json, "output");
	print_opt_str("Output location", output);
	int optimization_level = get_valid_string_setting(PROJECT_JSON, NULL, project_json, "opt", optimization_levels, 0, ELEMENTLEN(optimization_levels), optimization_level_expected);
	print_opt_setting("Default optimization level", optimization_level, optimization_levels);

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

	for (unsigned i = 0; i < targets_json->member_len; i++) {
		JSONObject *object = targets_json->members[i];
		const char *key = targets_json->keys[i];
		if (object->type != J_OBJECT)
		{
			error_exit("Invalid data in target '%s'", key);
		}
		view_target(key,object);
	}
}