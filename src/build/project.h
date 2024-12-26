#pragma once 
#include "../utils/json.h"

const char** get_project_dependency_directories();
const char** get_project_dependencies();

static void print_vec(const char *header, const char **vec, bool opt);

void add_libraries_to_project_file(const char** libs, const char* target_name);

const char* vendor_fetch_single(const char* lib, const char* path);
