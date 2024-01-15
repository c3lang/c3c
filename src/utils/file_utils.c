// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include <sys/stat.h>
#include <limits.h>
#include "common.h"
#include "lib.h"

#if PLATFORM_WINDOWS
#include <windows.h>
#endif

#ifndef _MSC_VER
#include <dirent.h>
#include <unistd.h>
#else
#include <fileapi.h>
#include <stringapiset.h>

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

#ifndef PATH_MAX
#define PATH_MAX 1024
#endif


uint16_t *win_utf8to16(const char *value UNUSED)
{
#if (_MSC_VER)
	size_t len = strlen(value);
	int needed = MultiByteToWideChar(CP_UTF8, 0, value, len + 1, NULL, 0);
	if (needed <= 0)
	{
		error_exit("Failed to convert name '%s'.", value);
	}
	uint16_t *wide = malloc(needed * sizeof(uint16_t));
	if (MultiByteToWideChar(CP_UTF8, 0, value, len + 1, wide, needed) <= 0)
	{
		error_exit("Failed to convert name '%s'.", value);
	}
	return wide;
#else
	UNREACHABLE
#endif
}

#include <wchar.h>
char *win_utf16to8(const uint16_t *wname UNUSED)
{
#if (_MSC_VER)
	size_t len = wcslen(wname);
	int needed = WideCharToMultiByte(CP_UTF8, 0, wname, len + 1, NULL, 0, NULL, NULL);
	if (needed <= 0)
	{
		error_exit("Failed to convert wide name.");
	}
	char *chars = malloc(needed);
	if (WideCharToMultiByte(CP_UTF8, 0, wname, len + 1, chars, needed, NULL, NULL) <= 0)
	{
		error_exit("Failed to convert wide name.");
	}
	return chars;
#else
	UNREACHABLE
#endif
}

bool dir_make(const char *path)
{
#if (_MSC_VER)
	return CreateDirectoryW(win_utf8to16(path), NULL);
#else
	return mkdir(path, 0755) == 0;
#endif
}

bool dir_make_recursive(char *path)
{
	size_t len = strlen(path);
	for (size_t i = len; i > 1; i--)
	{
		char c = path[i];
		if (c == '\\' || c == '/')
		{
			path[i] = '\0';
			dir_make_recursive(path);
			path[i] = c;
			break;
		}
	}
	return dir_make(path);
}

bool dir_change(const char *path)
{
#if (_MSC_VER)
	return SetCurrentDirectoryW(win_utf8to16(path));
#else
	return chdir(path) == 0;
#endif
}

static inline bool is_path_separator(char c)
{
#if PLATFORM_WINDOWS
	return c == '/' || c == '\\';
#else
	return c == '/';
#endif
}

const char *filename(const char *path)
{
	// Find the filename.
	for (size_t j = strlen(path); j > 0; j--)
	{
		switch (path[j - 1])
		{
			case '/':
			case '\\':
				return &path[j];
			default:
				break;
		}
	}
	return path;
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

FILE *file_open_read(const char *path)
{
#if (_MSC_VER)
	return _wfopen(win_utf8to16(path), L"rb");
#else
	return fopen(path, "rb");
#endif
}

bool file_touch(const char *path)
{
#if (_MSC_VER)
	FILE *file = _wfopen(win_utf8to16(path), L"a");
#else
	FILE *file = fopen(path, "a");
#endif
	if (!file) return false;
	return fclose(file) == 0;
}

char *file_read_all(const char *path, size_t *return_size)
{
	FILE *file = file_open_read(path);

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

static bool file_read(FILE *file, char *buffer, size_t *read)
{
	size_t to_read = *read;
	size_t total_read = 0;
	while (to_read > 0)
	{
		size_t bytes_read = fread(buffer, sizeof(char), to_read, file);
		total_read += bytes_read;
		to_read -= bytes_read;
		buffer += bytes_read;
		if (bytes_read < to_read)
		{
			if (feof(file)) goto DONE;
			if (ferror(file)) return false;
		}
	}
DONE:
	*read = total_read;
	return true;
}

static char zero[1];

char *file_read_binary(const char *path, size_t *size)
{
	size_t max_read = *size;
	FILE *file = file_open_read(path);

	if (file == NULL) return NULL;

	fseek(file, 0L, SEEK_END);
	size_t file_size = (size_t)ftell(file);

	if (!file_size)
	{
		*size = 0;
		return zero;
	}
	if (file_size > max_read) file_size = max_read;
	rewind(file);
	char *buffer = (char *)MALLOC(file_size);
	if (buffer == NULL)
	{
		error_exit("Not enough memory to read \"%s\".\n", path);
	}
	*size = file_size;
	bool success = file_read(file, buffer, size);
	fclose(file);
	return success ? buffer : NULL;
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
	scratch_buffer_append("/libc/libc.c3");
	err = stat(scratch_buffer_to_string(), &info);
	if (err || !S_ISREG(info.st_mode)) return NULL;

	const char *lib_path = str_printf("%s%s", exe_path, rel_path);
	INFO_LOG("Library path found at %s", lib_path);
	return lib_path;
}

const char *find_rel_exe_dir(const char *dir)
{
	char *path = find_executable_path();
	INFO_LOG("Detected executable path at %s", path);
	size_t strlen_path = strlen(path);
	// Remove any last path slash
	if (strlen_path > 1 && (path[strlen_path - 1] == '/' || path[strlen_path - 1] == '\\'))
	{
		path[strlen_path - 1] = '\0';
	}
	struct stat info;
	const char *attempts[5] = { "/../", "/lib/", "/../lib/", "/", "/../../lib/" };

	for (size_t i = 0; i < 5; i++)
	{
		scratch_buffer_clear();
		scratch_buffer_printf("%s%s%s", path, attempts[i], dir);
		DEBUG_LOG("Checking %s", scratch_buffer_to_string());
		int err = stat(scratch_buffer_to_string(), &info);

		// Not a dir or had error?
		if (err || !S_ISDIR(info.st_mode)) continue;
		return scratch_buffer_to_string();
	}
	return NULL;


}

const char *find_lib_dir(void)
{

	char *path = find_executable_path();

	INFO_LOG("Detected executable path at %s", path);

	size_t strlen_path = strlen(path);
	// Remove any last path slash
	if (strlen_path > 1 && (path[strlen_path - 1] == '/' || path[strlen_path - 1] == '\\'))
	{
		path[strlen_path - 1] = '\0';
	}
	const char *lib_path = NULL;
	if ((lib_path = lib_find(path, "/../lib/c3/"))) goto DONE;
	if ((lib_path = lib_find(path, "/../lib/"))) goto DONE;
	if ((lib_path = lib_find(path, "/lib/c3/"))) goto DONE;
	if ((lib_path = lib_find(path, "/lib/"))) goto DONE;
	if ((lib_path = lib_find(path, "/c3/"))) goto DONE;
	if ((lib_path = lib_find(path, "/"))) goto DONE;
	if ((lib_path = lib_find(path, "/../c3/"))) goto DONE;
	if ((lib_path = lib_find(path, "/../"))) goto DONE;
	if ((lib_path = lib_find(path, "/../../lib/c3/"))) goto DONE;
	if ((lib_path = lib_find(path, "/../../lib/"))) goto DONE;

	INFO_LOG("Could not find the standard library /lib/std/");
DONE:;
	free(path);
	return lib_path;
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
		if (!dir_change(".."))
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
		if (name_len < len + 1) continue;
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

#ifdef _MSC_VER
extern int _getdrive(void);
extern int _chdrive(int drive);
#endif

bool file_delete_file(const char *path)
{
	assert(path);
#if (_MSC_VER)
	return DeleteFileW(win_utf8to16(path));
#else
	return !unlink(path);
#endif
}

void file_delete_all_files_in_dir_with_suffix(const char *path, const char *suffix)
{
	assert(path);
#if (_MSC_VER)
	const char *cmd = "del /q \"%s\\*%s\" >nul 2>&1";
#else
	const char *cmd = "rm -f %s/*%s";
#endif
	execute_cmd(str_printf(cmd, path, suffix), true);
}

#if (_MSC_VER)

#include <io.h>

void file_add_wildcard_files(const char ***files, const char *path, bool recursive, const char **suffix_list, int suffix_count)
{
	bool path_ends_with_slash = is_path_separator(path[strlen(path) - 1]);
	struct _wfinddata_t file_data;
	intptr_t file_handle;
	const char *search = str_printf(path_ends_with_slash ? "%s*.*" : "%s/*.*", path);
	if ((file_handle = _wfindfirst(win_utf8to16(search), &file_data)) == -1L) return;
	do
	{
		if ((file_data.attrib & _A_SUBDIR))
		{
			if (recursive)
			{
				if (file_data.name[0] == L'.')
				{
					continue;
				}
				char *format = path_ends_with_slash ? "%s%ls" : "%s/%ls";
				char *new_path = str_printf(format, path, file_data.name);
				file_add_wildcard_files(files, new_path, true, suffix_list, suffix_count);
			}
			continue;
		}
		char *name = win_utf16to8(file_data.name);
		if (!file_has_suffix_in_list(name, strlen(name), suffix_list, suffix_count)) continue;
		char *format = path_ends_with_slash ? "%s%s" : "%s/%s";
		vec_add(*files, str_printf(format, path, name));
	} while (_wfindnext(file_handle, &file_data) == 0);
	_findclose(file_handle);
}
#else

void file_add_wildcard_files(const char ***files, const char *path, bool recursive, const char **suffix_list, int suffix_count)
{
	size_t len = strlen(path);
	if (len == 0)
	{
		path = "./";
		len = 2;
	}
	DIR *dir = opendir(path);
	bool path_ends_with_slash = is_path_separator(path[len - 1]);
	if (!dir)
	{
		error_exit("Can't open the directory '%s'. Please check the paths. %s", path, strerror(errno));
	}
	struct dirent *ent;
	while ((ent = readdir(dir)))
	{
		size_t namelen = strlen(ent->d_name);
		if (namelen == 0 || ent->d_name[0] == '.') continue;
		DEBUG_LOG("Searching file %s", ent->d_name);
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
				DEBUG_LOG("Enter sub dir %s", ent->d_name);
				file_add_wildcard_files(files, new_path, recursive, suffix_list, suffix_count);
			}
			continue;
		}
		char *format = path_ends_with_slash ? "%s%s" : "%s/%s";
		INFO_LOG("Added file %s", ent->d_name);
		vec_add(*files, str_printf(format, path, ent->d_name));
	}
	closedir(dir);
}

#endif

#define BUFSIZE 1024
const char *execute_cmd(const char *cmd, bool ignore_failure)
{
	char buffer[BUFSIZE];
	char *output = "";
	FILE *process = NULL;
#if (_MSC_VER)
	if (!(process = _wpopen(win_utf8to16(cmd), L"r")))
	{
		if (ignore_failure) return "";
		error_exit("Failed to open a pipe for command '%s'.", cmd);
	}
#else
	if (!(process = popen(cmd, "r")))
	{
		if (ignore_failure) return "";
		error_exit("Failed to open a pipe for command '%s'.", cmd);
	}
#endif
	while (fgets(buffer, BUFSIZE - 1, process))
	{
		output = str_cat(output, buffer);
	}
#if PLATFORM_WINDOWS
	int err = _pclose(process);
#else
	int err = pclose(process);
#endif
	if (err)
	{
		if (ignore_failure) return "";
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

bool execute_cmd_failable(const char *cmd, const char **result)
{
	char buffer[BUFSIZE];
	char *output = "";
	FILE *process = NULL;
#if (_MSC_VER)
	if (!(process = _wpopen(win_utf8to16(cmd), L"r"))) return false;
#else
	if (!(process = popen(cmd, "r"))) return false;
#endif
	while (fgets(buffer, BUFSIZE - 1, process))
	{
		output = str_cat(output, buffer);
	}
#if PLATFORM_WINDOWS
	int err = _pclose(process);
#else
	int err = pclose(process);
#endif
	if (err) return false;

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
	*result = str_trim(output);
	return true;
}

#if PLATFORM_WINDOWS

char *realpath(const char *path, char *const resolved_path)
{
	char *result = NULL == resolved_path ? ccalloc(PATH_MAX + 1, 1) : resolved_path;
	if (NULL == result) return NULL;
	if (!GetFullPathNameA(path, PATH_MAX, result, NULL))
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
