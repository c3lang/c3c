// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

// Code based off Gregory Pakosz's whereami.

#if defined(WIN32)
#include <windows.h>
#endif

#include "whereami.h"

#include <stdlib.h>

#ifndef NOINLINE
#if defined(_MSC_VER)
#define NOINLINE __declspec(noinline)
#elif defined(__GNUC__)
#define NOINLINE __attribute__((noinline))
#else
#error unsupported compiler
#endif
#endif

#if defined(_MSC_VER)
#define RETURN_ADDRESS() _ReturnAddress()
#elif defined(__GNUC__)
#define RETURN_ADDRESS() __builtin_extract_return_addr(__builtin_return_address(0))
#else
#error unsupported compiler
#endif

#if defined(_WIN32)

#if defined(_MSC_VER)
#pragma warning(push, 3)
#endif

#include <intrin.h>

#if defined(_MSC_VER)
#pragma warning(pop)
#endif

static int get_module_path_(HMODULE module, char *out, int capacity, int *dirname_length)
{
	wchar_t buffer1[MAX_PATH];
	wchar_t buffer2[MAX_PATH];
	wchar_t *path = NULL;
	int length = -1;

	for (;;)
	{
		DWORD size;
		int length_, length__;

		size = GetModuleFileNameW(module, buffer1, sizeof(buffer1) / sizeof(buffer1[0]));

		if (size == 0)
		{
			break;
		}
		else if (size == (DWORD)(sizeof(buffer1) / sizeof(buffer1[0])))
		{
			DWORD size_ = size;
			do
			{
				wchar_t *path_;

				path_ = (wchar_t *) realloc(path, sizeof(wchar_t) * size_ * 2);
				if (!path_)
				{
					break;
				}
				size_ *= 2;
				path = path_;
				size = GetModuleFileNameW(module, path, size_);
			} while (size == size_);

			if (size == size_)
			{
				break;
			}
		}
		else
		{
			path = buffer1;
		}

		if (!_wfullpath(buffer2, path, MAX_PATH))
		{
			break;
		}
		length_ = (int) wcslen(buffer2);
		length__ = WideCharToMultiByte(CP_UTF8, 0, buffer2, length_, out, capacity, NULL, NULL);

		if (length__ == 0)
		{
			length__ = WideCharToMultiByte(CP_UTF8, 0, buffer2, length_, NULL, 0, NULL, NULL);
		}
		if (length__ == 0)
		{
			break;
		}

		if (length__ <= capacity && dirname_length)
		{
			int i;

			for (i = length__ - 1; i >= 0; --i)
			{
				if (out[i] == '\\')
				{
					*dirname_length = i;
					break;
				}
			}
		}

		length = length__;

		break;
	}

	if (path != buffer1)
	{
		free(path);
	}

	return length;
}

NOINLINE
int get_executable_path_raw(char *out, int capacity, int *dirname_length)
{
	return get_module_path_(NULL, out, capacity, dirname_length);
}


#elif defined(__linux__) || defined(__CYGWIN__) || defined(__sun) || defined(USE_PROC_SELF_EXE)

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(__linux__)

#include <linux/limits.h>

#else
#include <limits.h>
#endif
#ifndef __STDC_FORMAT_MACROS
#define __STDC_FORMAT_MACROS
#endif

#include <inttypes.h>

#if !defined(PROC_SELF_EXE)
#if defined(__sun)
#define PROC_SELF_EXE "/proc/self/path/a.out"
#else
#define PROC_SELF_EXE "/proc/self/exe"
#endif
#endif

static int get_executable_path_raw(char *out, int capacity, int *dirname_length)
{
	char buffer[PATH_MAX];
	char *resolved = NULL;
	int length = -1;

	for (;;)
	{
		resolved = realpath(PROC_SELF_EXE, buffer);
		if (!resolved)
		{
			break;
		}

		length = (int) strlen(resolved);
		if (length <= capacity)
		{
			memcpy(out, resolved, length);

			if (dirname_length)
			{
				int i;

				for (i = length - 1; i >= 0; --i)
				{
					if (out[i] == '/')
					{
						*dirname_length = i;
						break;
					}
				}
			}
		}

		break;
	}

	return length;
}

#elif defined(__APPLE__)

#define _DARWIN_BETTER_REALPATH

#include <mach-o/dyld.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>

#include "lib.h"

static int get_executable_path_raw(char *out, int capacity, int *dirname_length)
{
	char buffer1[PATH_MAX];
	char buffer2[PATH_MAX];
	char *path = buffer1;
	char *resolved = NULL;
	int length = -1;

	for (;;)
	{
		uint32_t size = (uint32_t) sizeof(buffer1);
		if (_NSGetExecutablePath(path, &size) == -1)
		{
			path = (char *) cmalloc(size);
			if (!_NSGetExecutablePath(path, &size))
			{
				break;
			}
		}

		resolved = realpath(path, buffer2);
		if (!resolved)
		{
			break;
		}

		length = (int) strlen(resolved);
		if (length <= capacity)
		{
			memcpy(out, resolved, length);

			if (dirname_length)
			{
				int i;

				for (i = length - 1; i >= 0; --i)
				{
					if (out[i] == '/')
					{
						*dirname_length = i;
						break;
					}
				}
			}
		}

		break;
	}

	if (path != buffer1)
	{
		free(path);
	}
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

static int get_executable_path_raw(char *out, int capacity, int *dirname_length)
{
	char buffer1[PATH_MAX];
	char buffer2[PATH_MAX];
	char *resolved = NULL;
	FILE *self_exe = NULL;
	int length = -1;

	for (;;)
	{
		self_exe = fopen(PROC_SELF_EXE, "r");
		if (!self_exe)
		{
			break;
		}

		if (!fgets(buffer1, sizeof(buffer1), self_exe))
		{
			break;
		}

		resolved = realpath(buffer1, buffer2);
		if (!resolved)
		{
			break;
		}

		length = (int) strlen(resolved);
		if (length <= capacity)
		{
			memcpy(out, resolved, length);

			if (dirname_length)
			{
				int i;

				for (i = length - 1; i >= 0; --i)
				{
					if (out[i] == '/')
					{
						*dirname_length = i;
						break;
					}
				}
			}
		}

		break;
	}

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

static int get_executable_path_raw(char *out, int capacity, int *dirname_length)
{
	char buffer1[PATH_MAX];
	char buffer2[PATH_MAX];
	char *path = buffer1;
	char *resolved = NULL;
	int length = -1;

	for (;;)
	{
#if defined(__NetBSD__)
		int mib[4] = { CTL_KERN, KERN_PROC_ARGS, -1, KERN_PROC_PATHNAME };
#else
		int mib[4] = { CTL_KERN, KERN_PROC, KERN_PROC_PATHNAME, -1 };
#endif
		size_t size = sizeof(buffer1);

		if (sysctl(mib, (u_int) (sizeof(mib) / sizeof(mib[0])), path, &size, NULL, 0) != 0)
		{
			break;
		}

		resolved = realpath(path, buffer2);
		if (!resolved)
		{
			break;
		}

		length = (int) strlen(resolved);
		if (length <= capacity)
		{
			memcpy(out, resolved, length);

			if (dirname_length)
			{
				int i;

				for (i = length - 1; i >= 0; --i)
				{
					if (out[i] == '/')
					{
						*dirname_length = i;
						break;
					}
				}
			}
		}

		break;
	}

	if (path != buffer1)
	{
		free(path);
	}

	return length;
}

#else

#error unsupported platform

#endif

char *find_executable_path(void)
{
	int len = get_executable_path_raw(NULL, 0, NULL);
	if (len < 0) return "";
	char *path = malloc((unsigned)len + 1);
	get_executable_path_raw(path, len, NULL);
	path[len] = '\0';
	for (int i = 0; i < len; ++i)
	{
		if ('\\' == path[i]) path[i] = '/';
	}
	for (int i = len - 1; i >= 0; i--)
	{
		switch (path[i])
		{
			case '/':
			case '\\':
				path[i + 1] = '\0';
				return path;
			default:
				break;
		}
	}
	path[len] = '\0';
	return path;
}