#pragma once

// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "utils/lib.h"
#include "utils/toml.h"
#include "build_options.h"
#define DEFAULT_SYMTAB_SIZE (256 * 1024)
#define DEFAULT_SWITCHRANGE_MAX_SIZE (256)

typedef struct
{
	BuildTarget **targets;
} Project;

Project *project_load(void);
BuildTarget *project_select_target(Project *project, const char *optional_target);

