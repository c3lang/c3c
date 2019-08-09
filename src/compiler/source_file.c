// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <sys/stat.h>
#include <limits.h>
#include "../build/build_options.h"
#include "source_file.h"
#include "../utils/lib.h"
#include "../utils/file_utils.h"
#include "lexer.h"

static const size_t LEXER_FILES_START_CAPACITY = 128;

File pseudo_file;

typedef struct
{
	File **files;
} SourceFiles;

SourceFiles source_files;

File *source_file_load(const char *filename, bool *already_loaded)
{
	if (already_loaded) *already_loaded = false;
	if (!source_files.files) source_files.files = VECNEW(File *, LEXER_FILES_START_CAPACITY);

	char *full_path = malloc_arena(PATH_MAX + 1);
	if (!realpath(filename, full_path))
	{
		error_exit("Failed to resolve %s", filename);
	}

	VECEACH(source_files.files, index)
	{
		if (strcmp(source_files.files[index]->full_path, full_path) == 0)
		{
			*already_loaded = true;
			return source_files.files[index];
		}
	}
	if (vec_size(source_files.files) == MAX_FILES)
	{
		error_exit("Exceeded max number of files %d", MAX_FILES);
	}

	size_t size;
	const char* source_text = read_file(filename, &size);
	File *file = malloc(sizeof(File));
	file->full_path = full_path;
	file->start_id = vec_size(source_files.files) ? VECLAST(source_files.files)->end_id : 0;
	file->contents = source_text;
	ASSERT(file->start_id + size < UINT32_MAX, "Total files loaded exceeded %d bytes", UINT32_MAX);
	file->end_id = (SourceLoc) (file->start_id + size);
	file->name = filename;
	source_files.files = VECADD(source_files.files, file);
	return file;
}

File *source_file_from_position(SourceLoc loc)
{
	if (loc == INVALID_LOC)
	{
		pseudo_file.contents = "---";
		return &pseudo_file;
	}
	if (lexer_current_file()->start_id <= loc) return lexer_current_file();
	unsigned low = 0;
	unsigned high = vec_size(source_files.files) - 2;
	assert(vec_size(source_files.files) > 1);
	while (1)
	{
		// Binary search
		unsigned mid = (high + low) / 2;
		File *file = source_files.files[mid];
		if (file->start_id > loc)
		{
			high = mid - 1;
			continue;
		}
		if (file->end_id < loc)
		{
			low = mid + 1;
			continue;
		}
		return file;
	}
}


