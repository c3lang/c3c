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

File pseudo_file;

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

	file->full_path = full_path;
	file->start_id = vec_size(global_context.loaded_sources) ? VECLAST(global_context.loaded_sources)->end_id : 0;
	file->current_line_start = file->start_id;
	file->contents = source_text;
	ASSERT(file->start_id + size < UINT32_MAX, "Total files loaded exceeded %d bytes", UINT32_MAX);
	file->end_id = (SourceLoc) (file->start_id + size);
	size_t pre_allocated_lines = size / 40;
	file->lines = VECNEW(SourceLoc, pre_allocated_lines < 16 ? 16 : pre_allocated_lines);
	vec_add(file->lines, file->start_id);
	path_get_dir_and_filename_from_full(file->full_path, &file->name, &file->dir_path);
	vec_add(global_context.loaded_sources, file);
	return file;
}

void source_file_append_line_end(File *file, SourceLoc loc)
{
	if (file->current_line_start > loc) return;
	file->current_line_start = loc + 1;
	vec_add(file->lines, file->current_line_start);
}

SourcePosition source_file_find_position_in_file(File *file, SourceLoc loc)
{
	assert(file->start_id <= loc);

	unsigned lines = vec_size(file->lines);
	unsigned low = 0;
	unsigned high = lines;
	while (1)
	{
		// Line found iff line_start[mid] <= loc && line_start[mid + 1] < loc
		// Binary search
		uint32_t mid = (high + low) / 2;

		// Mid is before the location.
		SourceLoc line_start = file->lines[mid];
		if (line_start > loc)
		{
			high = mid;
			continue;
		}
		if (mid + 1 != lines && file->lines[mid + 1] <= loc)
		{
			low = mid;
			continue;
		}
		return (SourcePosition)
				{
					.file = file,
					.line = mid + 1,
					.col = loc - line_start + 1,
					.loc = loc,
					.start = file->contents + loc - file->start_id,
				};
	}
}


