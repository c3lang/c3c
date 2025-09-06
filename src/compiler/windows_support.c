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
		sdk = &loaded;
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
	return find_rel_exe_dir("msvc_sdk");
}