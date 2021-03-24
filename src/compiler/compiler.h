#pragma once

#include "build/build_options.h"

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

void compiler_init();
void compile_files(BuildTarget *target);
void init_build_target(BuildTarget *build_target, BuildOptions *build_options);
void init_default_build_target(BuildTarget *target, BuildOptions *options, const char *name);
void symtab_init(uint32_t max_size);

