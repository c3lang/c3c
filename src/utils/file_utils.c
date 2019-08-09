// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "file_utils.h"
#include "errors.h"
#include "malloc.h"
#include "lib.h"
#include <stdio.h>
#include <stdlib.h>

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
		exit(74);
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
