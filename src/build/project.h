#pragma once
#include "../utils/json.h"

const char** get_project_dependency_directories(void);
const char** get_project_dependencies(void);

static void print_vec(const char *header, const char **vec, bool opt, const char *delim);
void add_libraries_to_project_file(const char** libs, const char* target_name);
const char* vendor_fetch_single(const char* lib, const char* path, bool progress);
