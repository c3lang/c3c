#pragma once
#include "../lib.h"

#if PLATFORM_WINDOWS
	#define WIN32_LEAN_AND_MEAN
	#define TokenType WindowsTokenType
	#define MAX_PRIORITY WindowsMAX_PRIORITY
	#include <windows.h>
	#undef TokenType
	#undef MAX_PRIORITY
#else
	#include <dirent.h>
	#include <strings.h>
#endif

#ifndef MAX_PATH
	#if defined(PATH_MAX)
		#define MAX_PATH PATH_MAX
	#elif defined(_MAX_PATH)
		#define MAX_PATH _MAX_PATH
	#else
		#define MAX_PATH 260
	#endif
#endif

// Minimal dirent-like structure for Windows (POSIX dirent.h not available)
#if PLATFORM_WINDOWS
struct dirent
{
	char d_name[MAX_PATH];
};
typedef struct
{
	HANDLE handle;
	WIN32_FIND_DATAW data;
	struct dirent entry;
	bool first;
} DIR;

static DIR *opendir(const char *name)
{
	DIR *dir = calloc(1, sizeof(DIR));
	char *search_path = str_printf("%s\\*", name);
	uint16_t *wpath = win_utf8to16(search_path);
	dir->handle = FindFirstFileW(wpath, &dir->data);
	free(wpath);
	if (dir->handle == INVALID_HANDLE_VALUE)
	{
		free(dir);
		return NULL;
	}
	dir->first = true;
	return dir;
}

static struct dirent *readdir(DIR *dir)
{
	if (!dir->first && !FindNextFileW(dir->handle, &dir->data)) return NULL;
	dir->first = false;
	char *name = win_utf16to8(dir->data.cFileName);
	strncpy(dir->entry.d_name, name, MAX_PATH);
	free(name);
	return &dir->entry;
}

static void closedir(DIR *dir)
{
	if (dir) FindClose(dir->handle);
	free(dir);
}
#endif
