#pragma once 
#include "../utils/json.h"

const char** proj_lib_dirs_get();

void add_libraries_project(const char** libs, const char* target);