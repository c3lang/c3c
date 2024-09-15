#pragma once 
#include "../utils/json.h"

const char** get_project_dependency_directories();

void add_libraries_to_project_file(const char** libs, const char* target_name);