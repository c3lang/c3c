// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

// Code based off Gregory Pakosz's whereami.

#include "whereami.h"
#include "lib.h"
#include "common.h"

#define MAX_EXE_PATH 4096

#if PLATFORM_WINDOWS
#include <wchar.h>
#include <windows.h>

static int get_executable_path_raw(char *buffer)
{
	wchar_t buffer1[MAX_EXE_PATH];
	wchar_t buffer2[MAX_EXE_PATH];
	DWORD size = GetModuleFileNameW(NULL, buffer1, MAX_EXE_PATH);
	if (!size) error_exit("Failed to get module path.");
	if (size == MAX_EXE_PATH) error_exit("Module path too long");
	if (!_wfullpath(buffer2, buffer1, MAX_EXE_PATH)) error_exit("Failed to get the full module path.");
	int length_ = (int)wcslen(buffer2);

	int length = WideCharToMultiByte(CP_UTF8, 0, buffer2, length_, buffer, MAX_EXE_PATH, NULL, NULL);
	if (!length || length == MAX_EXE_PATH) error_exit("Failed to convert module path.");
	buffer[length] = 0;
	return length;
}

#elif defined(__linux__)

#if !defined(PROC_SELF_EXE)
#define PROC_SELF_EXE "/proc/self/exe"
#endif


static int get_executable_path_raw(char *out)
{
	char buffer[MAX_EXE_PATH];
	char *resolved = realpath(PROC_SELF_EXE, buffer);
	if (!resolved) error_exit("Failed to retrieve executable path");
	int length = (int)strlen(resolved);
	if (length >= MAX_EXE_PATH) error_exit("Executable path too long");
	memcpy(out, resolved, length);
	out[length] = 0;
	return length;
}

#elif defined(__APPLE__)

#define _DARWIN_BETTER_REALPATH

#include <mach-o/dyld.h>
#include <string.h>

static int get_executable_path_raw(char *out)
{
	assert(out);
	char buffer1[MAX_EXE_PATH];
	char buffer2[MAX_EXE_PATH];

	uint32_t size = MAX_EXE_PATH;
	if (_NSGetExecutablePath(buffer1, &size) == -1) error_exit("Executable path too long");
	char *resolved = realpath(buffer1, buffer2);
	if (!resolved) error_exit("Failed to retrieve executable path");

	int length = (int)strlen(resolved);
	if (length >= MAX_EXE_PATH) error_exit("Executable path too long");
	memcpy(out, resolved, length);
	out[length] = 0;
	return length;
}

#elif defined(__QNXNTO__)

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>

#if !defined(PROC_SELF_EXE)
#define PROC_SELF_EXE "/proc/self/exefile"
#endif

static int get_executable_path_raw(char *out)
{
	assert(out);
	char buffer1[MAX_EXE_PATH];
	char buffer2[MAX_EXE_PATH];
	FILE *self_exe = fopen(PROC_SELF_EXE, "r");
	if (!self_exe) error_exit("Failed to find the executable path");
	if (!fgets(buffer1, PATH_MAX, self_exe)) error_exit("Failed to find the executable path");
	char *resolved = realpath(buffer1, buffer2);
	if (!resolved) error_exit("Failed to resolve the executable path");
	int length = (int) strlen(resolved);
	if (length >= MAX_EXE_PATH) error_exit("Executable path too long");
	memcpy(out, resolved, length);
	out[length] = 0;
	fclose(self_exe);
	return length;
}

#elif defined(__DragonFly__) || defined(__FreeBSD__) || \
	  defined(__FreeBSD_kernel__) || defined(__NetBSD__)

#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/sysctl.h>
#include <dlfcn.h>

static int get_executable_path_raw(char *out)
{
	char buffer1[MAX_EXE_PATH];
	char buffer2[MAX_EXE_PATH];
#if defined(__NetBSD__)
	int mib[4] = { CTL_KERN, KERN_PROC_ARGS, -1, KERN_PROC_PATHNAME };
#else
	int mib[4] = { CTL_KERN, KERN_PROC, KERN_PROC_PATHNAME, -1 };
#endif
	size_t size = MAX_EXE_PATH;

	if (sysctl(mib, (u_int) (sizeof(mib) / sizeof(mib[0])), buffer1, &size, NULL, 0) != 0)
	{
		error_exit("Failed to get the executable path");
	}

	char *resolved = realpath(buffer1, buffer2);
	if (!resolved) error_exit("Failed to resolve the executable path");
	int length = (int)strlen(resolved);

	if (length >= MAX_EXE_PATH) error_exit("Executable path too long");
	memcpy(out, resolved, length);
	out[length] = 0;
	return length;
}

#elif defined(__OpenBSD__)
// this target doesn't have a reliable way to get full path to the executable
// a partially functional fix is implemented

extern const char *compiler_exe_name;

static int get_executable_path_raw(char *out)
{
	char tmp[PATH_MAX] = { 0 };
	if (compiler_exe_name[0] == '.')
		realpath(compiler_exe_name, tmp);
	else if (compiler_exe_name[0] == '/')
		strcpy(tmp, compiler_exe_name);
	else if (strcmp(compiler_exe_name, "c3c") == 0) {
		char *path = getenv("PATH");
		int len = 0;
		do {
			len = strcspn(path, ":");
			strncat(tmp, path, len);
			tmp[len] = '/';
			strcat(tmp, "c3c");
			if (realpath(tmp, tmp) != NULL) break;
			memset(tmp, 0, len + 4); // to account for /c3c
			path += len + 1;
		} while (path[-1]);
		if (path[-1] == 0) error_exit("Unable to find full path of the executable");
	} else error_exit("Unable to find full path of the executable");
	
	int length = strlen(tmp);
	if (length >= MAX_EXE_PATH) error_exit("Executable path too long");
	memcpy(out, tmp, length);
	out[length] = 0;
	
	return length;
}

#else

#error unsupported platform

#endif

static char buffer[MAX_EXE_PATH];
static bool path_found = false;
const char *find_executable_path(void)
{
	if (path_found) return buffer;
	int len = get_executable_path_raw(buffer);
	path_found = true;
	// Convert backslashes to forward slashes,
	// but consider the case of d:\foo?
	for (int i = 0; i < len; ++i)
	{
		if ('\\' == buffer[i]) buffer[i] = '/';
	}
	for (int i = len - 1; i >= 0; i--)
	{
		switch (buffer[i])
		{
			case '/':
			case '\\':
				buffer[i] = '\0';
				return buffer;
			default:
				break;
		}
	}
	buffer[0] = 0;
	return buffer;
}
