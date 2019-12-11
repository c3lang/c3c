// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "errors.h"
#include "malloc.h"
#include "lib.h"
#include <stdio.h>
#include <stdlib.h>
#include <libgen.h>

const char* expand_path(const char* path)
{
	if (path[0] == '~' && path[1] == '/')
	{
		// Ignore leak.
		char *ret = NULL;
		char *home = getenv("HOME");
		if (!home || asprintf(&ret, "%s%s", home, &path[1]) == -1) return &path[2];
		return ret;
	}
	return path;
}

int filename_to_module(const char *path, char buffer[MAX_IDENTIFIER_LENGTH + 1])
{
	int len = (int)strlen(path);
	int last_slash = 0;
	int last_dot = -1;
	for (int i = 0; i < len; i++)
	{
		if (path[i] == '/') last_slash = i;
		if (path[i] == '.') last_dot = i;
	}
	int namelen = last_dot - last_slash - 1;
	if (namelen < 2) return 0;
	if (namelen > MAX_IDENTIFIER_LENGTH) namelen = MAX_IDENTIFIER_LENGTH;
	for (int i = last_slash + 1; i < last_dot; i++)
	{
		char c = path[i];
		if (is_letter(c))
		{
			c = (char)(is_upper(c) ? c + 'a' - 'A' : c);
		}
		else
		{
			c = '_';
		}
		buffer[i - last_slash - 1] = c;
	}
	buffer[namelen] = '\0';
	return namelen;
}

char *read_file(const char *path, size_t *return_size)
{
	FILE *file = fopen(path, "rb");

	if (file == NULL)
	{
		error_exit("Could not open file \"%s\".\n", path);
	}

	fseek(file, 0L, SEEK_END);
	size_t file_size = (size_t)ftell(file);
	*return_size = file_size;
	rewind(file);

	char *buffer = (char *)malloc((size_t)file_size + 1);
	if (buffer == NULL)
	{
		error_exit("Not enough memory to read \"%s\".\n", path);
	}

	size_t bytesRead = fread(buffer, sizeof(char), (size_t)file_size, file);
	if (bytesRead < file_size)
	{
		error_exit("Failed to read file \"%s\".\n", path);
	}

	buffer[bytesRead] = '\0';

	fclose(file);
	return buffer;
}

void path_get_dir_and_filename_from_full(const char *full_path, char **filename, char **dir_path)
{
	char path[256];
	size_t path_len = strlen(full_path);
	if (path_len > 255) error_exit("Path %s too long.", full_path);

	strncpy(path, full_path, path_len + 1);
	const char *base_name = basename(path);
	size_t filename_len = strlen(base_name);
	*filename = malloc(filename_len + 1);
	strncpy(*filename, base_name, filename_len + 1);

	strncpy(path, full_path, path_len + 1);
	const char *dir = dirname(path);
	size_t dir_len = strlen(dir);
	*dir_path = malloc(dir_len + 1);
	strncpy(*dir_path, dir, dir_len + 1);
}
