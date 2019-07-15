#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#define MAX_LIB_DIRS 1024
#define MAX_FILES 2048

typedef struct
{
	const char* lib_dir[MAX_LIB_DIRS];
	int lib_count;
	const char* files[MAX_FILES];
	int file_count;
	const char* project_name;
	const char* path;
	const char* original_path;
} BuildOptions;

extern BuildOptions build_options;