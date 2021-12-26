// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include <sys/stat.h>
#ifdef _MSC_VER

#define PATH_MAX 260

#else
#include <limits.h>
#endif
#include "compiler_internal.h"
#include "../build/build_options.h"

static const size_t LEXER_FILES_START_CAPACITY = 128;


File *source_file_by_id(FileId file)
{
	assert(file < vec_size(global_context.loaded_sources));
	return global_context.loaded_sources[file];
}

File *source_file_load(const char *filename, bool *already_loaded)
{
	if (already_loaded) *already_loaded = false;
	if (!global_context.loaded_sources) global_context.loaded_sources = VECNEW(File*, LEXER_FILES_START_CAPACITY);

	char* full_path = malloc_arena(PATH_MAX + 1);

	if (!realpath(filename, full_path))
	{
		error_exit("Failed to resolve %s", filename);
	}

	VECEACH(global_context.loaded_sources, index)
	{
		if (strcmp(global_context.loaded_sources[index]->full_path, full_path) == 0)
		{
			*already_loaded = true;
			return global_context.loaded_sources[index];
		}
	}
	if (vec_size(global_context.loaded_sources) == MAX_FILES)
	{
		error_exit("Exceeded max number of files %d", MAX_FILES);
	}

	size_t size;
	const char* source_text = read_file(filename, &size);
	File *file = CALLOCS(File);
	file->file_id = vec_size(global_context.loaded_sources);
	file->full_path = full_path;
	file->contents = source_text;
	path_get_dir_and_filename_from_full(file->full_path, &file->name, &file->dir_path);
	vec_add(global_context.loaded_sources, file);
	return file;
}


