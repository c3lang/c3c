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

static const size_t LEXER_FILES_START_CAPACITY = 128;


File *source_file_by_id(FileId file)
{
	if (file == STDIN_FILE_ID) return &stdin_file;
	ASSERT0(file < vec_size(compiler.context.loaded_sources));
	return compiler.context.loaded_sources[file];
}

File *source_file_text_load(const char *filename, const char *content)
{
	File *file = CALLOCS(File);
	file->file_id = vec_size(compiler.context.loaded_sources);
	file->full_path = str_copy(filename, strlen(filename));
	file->contents = content;
	file->content_len = strlen(content);
	file->name = (char *)file->full_path;
	file->dir_path = str_copy("", 0);
	vec_add(compiler.context.loaded_sources, file);
	return file;

}

File *source_file_generate(const char *filename)
{
	File *file = CALLOCS(File);
	file->file_id = vec_size(compiler.context.loaded_sources);
	file->full_path = "<generated>";
	file->contents = "";
	file->content_len = 0;
	vec_add(compiler.context.loaded_sources, file);
	return file;
}

File *source_file_load(const char *filename, bool *already_loaded, const char **error)
{
	if (already_loaded) *already_loaded = false;
	if (!compiler.context.loaded_sources) compiler.context.loaded_sources = VECNEW(File*, LEXER_FILES_START_CAPACITY);

	char* full_path = malloc_arena(PATH_MAX + 1);

	if (!realpath(filename, full_path))
	{
		*error = str_printf("Failed to resolve %s", filename);
		return NULL;
	}

	FOREACH(File *, file, compiler.context.loaded_sources)
	{
		if (strcmp(file->full_path, full_path) == 0)
		{
			if (already_loaded) *already_loaded = true;
			return file;
		}
	}
	if (vec_size(compiler.context.loaded_sources) == MAX_COMMAND_LINE_FILES)
	{
		*error = str_printf("Exceeded max number of files %d", MAX_COMMAND_LINE_FILES);
		return NULL;
	}

	size_t size;
	const char* source_text = file_read_all(filename, &size);
	File *file = CALLOCS(File);
	file->file_id = vec_size(compiler.context.loaded_sources);
	file->full_path = full_path;
	file->contents = source_text;
	file->content_len = size;
	file_get_dir_and_filename_from_full(file->full_path, &file->name, &file->dir_path);
	vec_add(compiler.context.loaded_sources, file);
	return file;
}


