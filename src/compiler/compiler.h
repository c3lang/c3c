#pragma once
// Copyright (c) 2019-2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "../build/build.h"

void compiler_init(BuildOptions *options);
void compile();
void compile_target(BuildOptions *options);
void compile_file_list(BuildOptions *options);
void compile_clean(BuildOptions *options);
void execute_scripts(void);
void init_build_target(BuildTarget *build_target, BuildOptions *build_options);
void init_default_build_target(BuildTarget *target, BuildOptions *options);
void symtab_init(uint32_t max_size);
void symtab_destroy();
void print_syntax(BuildOptions *options);
void vendor_fetch(BuildOptions *options);

extern const char* c3_suffix_list[3];
