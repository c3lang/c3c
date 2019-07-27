#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <stdint.h>

#define MAX_LIB_DIRS 1024
#define MAX_FILES 2048

typedef enum
{
	COMMAND_MISSING = 0,
	COMMAND_COMPILE,
	COMMAND_INIT,
	COMMAND_BUILD,
	COMMAND_COMPILE_RUN,
	COMMAND_RUN,
	COMMAND_CLEAN_RUN,
	COMMAND_CLEAN,
	COMMAND_DIST,
	COMMAND_DOCS,
	COMMAND_BENCH,
	COMMAND_UNIT_TEST,
} CompilerCommand;

typedef struct
{
	const char* lib_dir[MAX_LIB_DIRS];
	int lib_count;
	const char* files[MAX_FILES];
	int file_count;
	const char* project_name;
	const char* target;
	const char* path;
	CompilerCommand command;
	uint32_t symtab_size;
} BuildOptions;

extern BuildOptions build_options;

void parse_arguments(int argc, const char *argv[]);
