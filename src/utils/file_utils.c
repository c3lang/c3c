// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include <sys/stat.h>
#include "common.h"
#include "errors.h"
#include "lib.h"

#if PLATFORM_WINDOWS

#include <windows.h>

#endif

#ifndef _MSC_VER

#include <libgen.h>
#include <unistd.h>
#include <dirent.h>
#include <limits.h>

#else
#include "utils/dirent.h"
#define PATH_MAX 260


// copied from https://github.com/kindkaktus/libconfig/commit/d6222551c5c01c326abc99627e151d549e0f0958
#ifndef S_ISDIR
#define S_ISDIR(mode)  (((mode) & S_IFMT) == S_IFDIR)
#endif
// copied from https://stackoverflow.com/questions/11238918/s-isreg-macro-undefined
#if !defined(S_ISREG) && defined(S_IFMT) && defined(S_IFREG)
#define S_ISREG(m) (((m) & S_IFMT) == S_IFREG)
#endif

#endif

#include <errno.h>
#include "whereami.h"

#if PLATFORM_WINDOWS


/**
 * remove C: drive prefix (C:, D:, etc.) from a path. THIS WON'T WORK WITH D:, ETC.
 * If that is an issue, I think dirent will have to be replaced or the dirent
 * port in use will have to be replaced.
 */
char *strip_drive_prefix(char *path)
{
	if ((*path == 'c' || *path == 'C') && path[1] == ':')
	{
		return path + 2; // remove first two characters
	}

	if (path[1] == ':' && (path[2] == '/' || path[2] == '\\'))
	{ // I don't *think* a relative path can start with '[char]:/' ? right?
		// nothing can be done about this currently
		error_exit("Illegal path %s - absolute path must start with /, \\, "
				   "c:, or C: (file a github issue if this is a problem)");
	}

	// path is ok
	return path;
}

#endif

static inline bool is_path_separator(char c)
{
#if PLATFORM_WINDOWS
	return c == '/' || c == '\\';
#else
	return c == '/';
#endif
}

/**
 * Split a file into path + filename, allocating memory for them and returning them in
 * the out params
 * 'foo' => '.' / 'foo'
 * '/' => false
 * '.' => false
 * '..' => false
 * 'bar/' => false
 *
 * @param path the path to extract the filename from.
 * @param filename_ptr the pointer to return the filename in.
 * @param directory_ptr the pointer to return the directory in.
 * @return false if only a directory could be found, true otherwise
 */
bool filenamesplit(const char *path, char** filename_ptr, char** directory_ptr)
{
	size_t len = strlen(path);
	if (len == 0) return false;
	size_t found_at = (size_t)-1;
	for (size_t i = len - 1; i > 0; i--)
	{
		if (is_path_separator(path[i]))
		{
			found_at = i;
			break;
		}
	}
	size_t file_len = (found_at != ((size_t)-1)) ? len - found_at - 1 : len;
	if (file_len == 1 && path[0] == '.') return false;
	if (file_len == 2 && path[0] == '.' && path[1] == '.') return false;
	if (!file_len) return false;
	*filename_ptr = strdup(&path[len - file_len]);
	if (file_len < len)
	{
		size_t dir_len = len - file_len;
		char *dir = malloc(dir_len + 1);
		memcpy(dir, path, dir_len - 1);
		dir[dir_len] = 0;
		*directory_ptr = dir;
	}
	else
	{
		char *dir = malloc(2);
		dir[0] = '.';
		dir[1] = 0;
		*directory_ptr = dir;
	}
	return true;
}



const char *expand_path(const char *path)
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
	assert(bytes_read == file_size);

	buffer[bytes_read] = '\0';
	// Filter '\r' early.
	size_t offset = 0;
	for (size_t i = 0; i < file_size - offset; i++)
	{
		char c = buffer[i + offset];
		if (c == '\r')
		{
			offset++;
			i--;
			continue;
		}
		if (offset)
		{
			buffer[i] = c;
		}
	}
	buffer[bytes_read - offset] = '\0';
	fclose(file);
	return buffer;
}

static inline const char *lib_find(const char *exe_path, const char *rel_path)
{
	struct stat info;
	char *lib_path = NULL;
	asprintf(&lib_path, "%s%sstd", exe_path, rel_path);
	DEBUG_LOG("Checking %s", lib_path);
	int err = stat(lib_path, &info);

	// Not a dir or had error?
	if (err || !S_ISDIR(info.st_mode)) return NULL;

	char *check_path = NULL;
	asprintf(&check_path, "%s/libc.c3", lib_path);
	DEBUG_LOG("Potential lib found, sanity check for libc...");
	err = stat(check_path, &info);
	if (err || !S_ISREG(info.st_mode)) return NULL;

	asprintf(&lib_path, "%s%s", exe_path, rel_path);
	DEBUG_LOG("Library path found at %s", lib_path);
	return lib_path;
}

const char *find_lib_dir(void)
{

	char *path = find_executable_path();

	DEBUG_LOG("Detected executable path at %s", path);

	size_t strlen_path = strlen(path);
	// Remove any last path slash
	if (strlen_path > 1 && (path[strlen_path - 1] == '/' || path[strlen_path - 1] == '\\'))
	{
		path[strlen_path - 1] = '\0';
	}
	const char *lib_path;
	if ((lib_path = lib_find(path, "/../lib/"))) return lib_path;
	if ((lib_path = lib_find(path, "/lib/"))) return lib_path;
	if ((lib_path = lib_find(path, "/"))) return lib_path;
	if ((lib_path = lib_find(path, "/../"))) return lib_path;
	if ((lib_path = lib_find(path, "/../../lib/"))) return lib_path;

	DEBUG_LOG("Could not find the standard library /lib/std/");
	return NULL;
}

void path_get_dir_and_filename_from_full(const char *full_path, char **filename, char **dir_path)
{
	if (!filenamesplit(full_path, filename, dir_path))
	{
		error_exit("The filename could not be extracted from '%s'.", full_path);
	}
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
		error_exit("The root build directory containing %s could not be found. Did you use the correct directory?",
		           PROJECT_TOML);
	}
}

void file_add_wildcard_files(const char ***files, const char *path, bool recursive)
{
#ifdef _MSC_VER
	DIR *dir = opendir(strip_drive_prefix(path));
#else
	DIR *dir = opendir(path);
#endif
	bool path_ends_with_slash = path[strlen(path) - 1] == '/';
	if (!dir)
	{
		error_exit("Can't open the directory '%s'. Please check the paths. %s", path, strerror(errno));
	}
	struct dirent *ent;
	while ((ent = readdir(dir)))
	{
		size_t namelen = strlen(ent->d_name);
		if (namelen < 3) continue;

		// Doesn't end with .c3
		if (strncmp(&ent->d_name[namelen - 3], ".c3", 3) != 0)
		{
			char *new_path = NULL;
			char *format = path_ends_with_slash ? "%s%s" : "%s/%s";
			if (!asprintf(&new_path, format, path, ent->d_name))
			{
				error_exit("Failed to allocate path.");
			}
			bool is_directory;
			struct stat st;
			if (stat(new_path, &st))
			{
				DEBUG_LOG("Failed to stat %s", new_path);
				continue;
			}
			is_directory = S_ISDIR(st.st_mode);
			if (is_directory && ent->d_name[0] != '.' && recursive)
			{
				file_add_wildcard_files(files, new_path, recursive);
			}
			free(new_path);
			continue;
		}
		char *format = path_ends_with_slash ? "%s%s" : "%s/%s";
		vec_add(*files, strformat(format, path, ent->d_name));
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
	for (char *c = result; *c != '\0'; ++c)
	{
		if ('\\' == *c) *c = '/';
	}
	return result;
}

#endif
