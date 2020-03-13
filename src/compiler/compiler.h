#pragma once

#include "build/build_options.h"

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

void compiler_init();
void compile_files(BuildTarget *target);
void build();
void symtab_init(uint32_t max_size);

