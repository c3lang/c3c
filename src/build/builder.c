// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include <dirent.h>
#include "build_internal.h"
#include "build_options.h"

void load_library_files(void) {}
void load_files(void) {}
void compile_files(BuildTarget *target);

void build(void)
{
	// Locate the project.toml
	file_find_top_dir();
	// Parse it
	Project *project = project_load();
	BuildTarget *target = project_select_target(project, build_options.target);

	if (!target->target_triple && build_options.target_triple)
	{
		target->target_triple = build_options.target_triple;
	}

	load_library_files();
	compile_files(target);
}