#pragma once
#include "../utils/json.h"
#include "../build/build.h"

JSONObject *project_json_load(const char **filename_ref);

const char** get_project_dependency_directories(JSONObject *json, const char *filename);
const char** get_project_dependencies(JSONObject *json);

static void print_vec(const char *header, const char **vec, bool opt, const char *delim);
void add_libraries_to_project_file(const char** libs, const char* target_name, JSONObject *project_json, const char* filename);
const char* vendor_fetch_single(const char* lib, const char* path, bool progress);
void vendor_fetch(BuildOptions *options);