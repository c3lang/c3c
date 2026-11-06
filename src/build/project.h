#pragma once
#include "../utils/json.h"

const char** get_project_dependency_directories(void);
const char *project_fetch_url(void);

static void print_vec(const char *header, const char **vec, bool opt, const char *delim);
void add_libraries_to_project_file(const char** libs);
const char* vendor_fetch_single(const char* lib, const char* path, bool progress, bool decompress);
const char *registry_url(BuildOptions *options);
const char *registry_find_library(const char *base, const char *name, const char **id_ref);
const char *registry_fetch_single(BuildOptions *options, const LibraryPathInfo *dependency, const char *path, bool progress);
