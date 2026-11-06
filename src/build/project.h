#pragma once
#include "../utils/json.h"

const char *project_fetch_url(void);

static void print_vec(const char *header, const char **vec, bool opt, const char *delim);
const char *registry_find_library(const char *base, const char *name, const char **id_ref);
