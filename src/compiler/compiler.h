#pragma once
// Copyright (c) 2019-2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "../build/build.h"

void compiler_init(const char *std_lib_dir);
void compile();
void compile_target(BuildOptions *options);
void compile_file_list(BuildOptions *options);
void compile_clean(BuildOptions *options);
void init_build_target(BuildTarget *build_target, BuildOptions *build_options);
void init_default_build_target(BuildTarget *target, BuildOptions *options);
void symtab_init(uint32_t max_size);
void symtab_destroy();
void print_syntax(BuildOptions *options);
void vendor_fetch(BuildOptions *options);
int find_padding_length(const char** str, const int count);

extern double compiler_init_time;
extern double compiler_parsing_time;
extern double compiler_sema_time;
extern double compiler_ir_gen_time;
extern double compiler_codegen_time;
extern double compiler_link_time;
extern const char* c3_suffix_list[3];
extern char *arch_os_target[ARCH_OS_TARGET_LAST + 1];