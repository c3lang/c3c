#pragma once
#include "build.h"
#include "../utils/json.h"

const char** get_project_dependency_directories(void);
const char *project_fetch_url(void);

static void print_vec(const char *header, const char **vec, bool opt, const char *delim);
void add_libraries_to_project_file(const char** libs, const char *fetch_url);
const char* vendor_fetch_single(const char* lib, const char* path, bool progress, bool decompress);
const char *registry_fetch_single(BuildOptions *options, const Dependency *dependency, const char *path,
								 const char **fetched_version, bool progress);
