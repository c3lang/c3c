// Copyright (c) 2022 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"


static WindowsSDK *sdk = NULL;

// find paths to library directories.
// ex:
// C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\BuildTools\\VC\\Tools\\MSVC\\14.28.29910\\lib\\x64
// C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\BuildTools\\VC\\Tools\\MSVC\\14.28.29910\\atlmfc\\lib\\x64
// C:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.19041.0\\ucrt\\x64
// C:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.19041.0\\um\\x64

#if PLATFORM_WINDOWS

WindowsSDK get_windows_paths(void);

static WindowsSDK loaded;
WindowsSDK *windows_get_sdk(void)
{
	if (!sdk)
	{
		loaded = get_windows_paths();
		if (loaded.windows_sdk_path && loaded.vs_library_path)
		{
			sdk = &loaded;
		}
	}
	return sdk;
}

#else

WindowsSDK *windows_get_sdk(void)
{
	return NULL;
}

#endif

const char *windows_cross_compile_library(void)
{
	const char *local = find_rel_exe_dir("msvc_sdk");
	if (local && file_is_dir((char *)local)) return local;

#if !PLATFORM_WINDOWS
	char *cache_home = getenv("XDG_CACHE_HOME");
	if (cache_home)
	{
		scratch_buffer_clear();
		scratch_buffer_printf("%s/c3/msvc_sdk", cache_home);
		const char *path = scratch_buffer_to_string();
		if (file_is_dir((char *)path)) return path;
	}

	char *home = getenv("HOME");
	if (home)
	{
		scratch_buffer_clear();
		scratch_buffer_printf("%s/.cache/c3/msvc_sdk", home);
		const char *path = scratch_buffer_to_string();
		if (file_is_dir((char *)path)) return path;
	}
#endif
	return NULL;
}