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
bool file_namesplit(const char *path, char** filename_ptr, char** directory_ptr)
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
	*filename_ptr = str_copy(&path[len - file_len], file_len);
	if (!directory_ptr) return true;
	if (file_len < len)
	{
		size_t dir_len = len - file_len;
		*directory_ptr = str_copy(path, dir_len - 1);
	}
	else
	{
		*directory_ptr = calloc_string(2);
		(*directory_ptr)[0] = '.';
	}
	return true;
}



const char *file_expand_path(const char *path)
{
	if (path[0] == '~' && path[1] == '/')
	{
		char *home = getenv("HOME");
		if (!home) return &path[2];
		return str_printf("%s%s", home, &path[1]);
	}
	return path;
}


char *file_read_all(const char *path, size_t *return_size)
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

	char *buffer = (char *)MALLOC(file_size + 1);
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
	scratch_buffer_clear();
	scratch_buffer_printf("%s%sstd", exe_path, rel_path);
	DEBUG_LOG("Checking %s", scratch_buffer_to_string());
	int err = stat(scratch_buffer_to_string(), &info);

	// Not a dir or had error?
	if (err || !S_ISDIR(info.st_mode)) return NULL;

	DEBUG_LOG("Potential lib found, sanity check for libc...");
	scratch_buffer_append("/libc.c3");
	err = stat(scratch_buffer_to_string(), &info);
	if (err || !S_ISREG(info.st_mode)) return NULL;

	const char *lib_path = str_printf("%s%s", exe_path, rel_path);
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

void file_get_dir_and_filename_from_full(const char *full_path, char **filename, char **dir_path)
{
	if (!file_namesplit(full_path, filename, dir_path))
	{
		error_exit("The filename could not be extracted from '%s'.", full_path);
	}
}


void file_find_top_dir()
{
	while (1)
	{
		struct stat info;
		int err = stat(PROJECT_JSON, &info);

		// Error and the it's not a "missing file"?
		if (err && errno != ENOENT)
		{
			error_exit("Can't open %s: %s.", PROJECT_JSON, strerror(errno));
		}

		// Everything worked and it's a regular file? We're done!
		if (!err && S_ISREG(info.st_mode)) return; // NOLINT(hicpp-signed-bitwise)

		// Otherwise just continue upwards.
		// Note that symbolically linked files are ignored.
		char start_path[PATH_MAX + 1];
		getcwd(start_path, PATH_MAX);
		if (chdir(".."))
		{
			error_exit("Can't change directory to search for %s: %s.", PROJECT_JSON, strerror(errno));
		}
		char new_path[PATH_MAX + 1];
		getcwd(new_path, PATH_MAX);
		if (strcmp(new_path, start_path) != 0) continue;
		error_exit("The root build directory containing %s could not be found. Did you use the correct directory?",
				   PROJECT_JSON);
	}
}

bool file_has_suffix_in_list(const char *file_name, int name_len, const char **suffix_list, int suffix_count)
{
	for (int i = 0; i < suffix_count; i++)
	{
		const char *suffix = suffix_list[i];
		int len = (int)strlen(suffix);
		if (strncmp(&file_name[name_len - len], suffix, len) == 0) return true;
	}
	return false;
}

bool file_is_dir(const char *file)
{
	struct stat st;
	if (stat(file, &st)) return false;
	return S_ISDIR(st.st_mode);
}

bool file_exists(const char *path)
{
	struct stat st;
	if (stat(path, &st)) return false;
	return S_ISDIR(st.st_mode) || S_ISREG(st.st_mode) || S_ISREG(st.st_mode);
}

const char *file_append_path(const char *path, const char *name)
{
	size_t path_len = strlen(path);
	if (!path_len) return name;
	if (path[path_len - 1] == '/') return str_cat(path, name);
	return str_printf("%s/%s", path, name);
}

const char *file_first(const char *path)
{
#ifdef _MSC_VER
	DIR *dir = opendir(strip_drive_prefix(path));
#else
	DIR *dir = opendir(path);
#endif
	if (!dir) return NULL;
	struct dirent *ent;
	while ((ent = readdir(dir)))
	{
		size_t name_len = strlen(ent->d_name);
		if (name_len > 2)
		{
			return str_printf("%.*s", (int)name_len, ent->d_name);
		}
	}
	return NULL;
}

void file_add_wildcard_files(const char ***files, const char *path, bool recursive, const char **suffix_list, int suffix_count)
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
		if (namelen == 0 || ent->d_name[0] == '.') continue;

		if (namelen < 3 || !file_has_suffix_in_list(ent->d_name, namelen, suffix_list, suffix_count))
		{
			char *format = path_ends_with_slash ? "%s%s" : "%s/%s";
			char *new_path = str_printf(format, path, ent->d_name);
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
				file_add_wildcard_files(files, new_path, recursive, suffix_list, suffix_count);
			}
			continue;
		}
		char *format = path_ends_with_slash ? "%s%s" : "%s/%s";
		vec_add(*files, str_printf(format, path, ent->d_name));
	}
	closedir(dir);
}

#if PLATFORM_WINDOWS
const char *execute_cmd(const char *cmd)
{
	FATAL_ERROR("Not implemented");
}
#else
#define BUFSIZE 1024
const char *execute_cmd(const char *cmd)
{
	char buffer[BUFSIZE];
	char *output = "";
	FILE *process = NULL;
	if (!(process = popen(cmd, "r")))
	{
		error_exit("Failed to open a pipe for command '%s'.", cmd);
	}
	while (fgets(buffer, BUFSIZE - 1, process))
	{
		output = str_cat(output, buffer);
	}
	if (pclose(process))
	{
		error_exit("Failed to execute '%s'.", cmd);
	}
	while (output[0] != 0)
	{
		switch (output[0])
		{
			case ' ':
			case '\t':
			case '\n':
			case '\r':
				output++;
				continue;
			default:
				break;
		}
		break;
	}
	return str_trim(output);
}
#endif

#if PLATFORM_WINDOWS

char *realpath(const char *path, char *const resolved_path)
{
	char *result = NULL == resolved_path ? ccalloc(PATH_MAX + 1, 1) : resolved_path;
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
