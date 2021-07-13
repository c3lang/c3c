// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include <sys/stat.h>
#include "common.h"
#include "errors.h"
#include "lib.h"
#include <libgen.h>
#include <dirent.h>
#include <limits.h>
#include <unistd.h>
#include <errno.h>
#include "whereami.h"

#if PLATFORM_WINDOWS
#include <windows.h>
#endif


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
	if (namelen < 1) return 0;
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

	char *buffer = (char *)malloc(file_size + 1);
	if (buffer == NULL)
	{
		error_exit("Not enough memory to read \"%s\".\n", path);
	}

	size_t bytes_read = fread(buffer, sizeof(char), file_size, file);
	if (bytes_read < file_size)
	{
		error_exit("Failed to read file \"%s\".\n", path);
	}

	buffer[bytes_read] = '\0';

	fclose(file);
	return buffer;
}

const char* find_lib_dir(void)
{

	const char *path = find_executable_path();

	DEBUG_LOG("Detected executable path at %s", path);

	struct stat info;
	char *lib_path = NULL;

	asprintf(&lib_path, "%s../lib/std/", path);
	DEBUG_LOG("Checking %s", lib_path);
	int err = stat(lib_path, &info);

	// Found it at ../lib/std
	if (!err && S_ISDIR(info.st_mode))
	{
		asprintf(&lib_path, "%s../lib/", path);
		return lib_path;
	}

	asprintf(&lib_path, "%slib/std/", path);
	err = stat(lib_path, &info);

	// Found it at ./lib/std
	if (!err && S_ISDIR(info.st_mode))
	{
		asprintf(&lib_path, "%slib/", path);
		return lib_path;
	}

	DEBUG_LOG("Could not find the standard library /lib/std/");
	return NULL;
}

void path_get_dir_and_filename_from_full(const char *full_path, char **filename, char **dir_path)
{
	char path[1024];
	size_t path_len = strlen(full_path);
	if (path_len >= sizeof(path)) error_exit("Path %s too long.", full_path);

	strcpy(path, full_path);
	*filename = strdup(basename(path));

	strcpy(path, full_path);
	*dir_path = strdup(dirname(path));
}


void file_find_top_dir()
{
	while (1)
	{
		struct stat info;
		int err = stat(PROJECT_TOML, &info);

		// Error and the it's not a "missing file"?
		if (err && errno != ENOENT)
		{
			error_exit("Can't open %s: %s.", PROJECT_TOML, strerror(errno));
		}

		// Everything worked and it's a regular file? We're done!
		if (!err && S_ISREG(info.st_mode)) return; // NOLINT(hicpp-signed-bitwise)

		// Otherwise just continue upwards.
		// Note that symbolically linked files are ignored.
		char start_path[PATH_MAX + 1];
		getcwd(start_path, PATH_MAX);
		if (chdir(".."))
		{
			error_exit("Can't change directory to search for %s: %s.", PROJECT_TOML, strerror(errno));
		}
		char new_path[PATH_MAX + 1];
		getcwd(new_path, PATH_MAX);
		if (strcmp(new_path, start_path) != 0) continue;
		error_exit("The root build directory containing %s could not be found. Did you use the correct directory?", PROJECT_TOML);
	}
}

void file_add_wildcard_files(const char ***files, const char *path, bool recursive)
{
	DIR *dir = opendir(path);
	if (!dir)
	{
		error_exit("Can't open the directory '%s'. Please check the paths. %s", path, strerror(errno));
	}
	struct dirent *ent;
	while ((ent = readdir(dir)))
	{
		size_t namelen = strlen(ent->d_name);
		if (namelen < 4) continue;

		// Doesn't end with .c3
		if (strncmp(&ent->d_name[namelen - 3], ".c3", 3) != 0)
		{
			bool is_directory;
#if PLATFORM_WINDOWS
			struct stat st;
			is_directory = stat(ent->d_name, &st) == 0 && S_ISDIR(st.st_mode);
#else
			is_directory = ent->d_type == DT_DIR;  // is it POSIX-compliant? As
#endif
			if (is_directory && ent->d_name[0] != '.' && recursive)
			{
				char *new_path = strndup(ent->d_name, namelen);
				file_add_wildcard_files(files, new_path, recursive);
				free(new_path);
			}
			continue;
		}

		char *name = malloc_arena(namelen + 1);
		memcpy(name, ent->d_name, namelen);
		name[namelen] = '\0';
		vec_add(*files, name);
	}
	closedir(dir);
}

#if PLATFORM_WINDOWS

char *realpath(const char *path, char *const resolved_path)
{
	char *result = NULL == resolved_path ? calloc(PATH_MAX + 1, 1) : resolved_path;
	if (NULL == result) return NULL;
	if (!GetFullPathNameA(path, MAX_PATH, result, NULL))
	{
		if (NULL == resolved_path) free(result);
		return NULL;
	}
	return result;
}

#endif
