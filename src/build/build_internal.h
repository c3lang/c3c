#pragma once

// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "utils/lib.h"
#include "utils/json.h"
#include "build_options.h"


typedef struct
{
	BuildTarget **targets;
} Project;

Project *project_load(void);
BuildTarget *project_select_target(Project *project, const char *optional_target);

