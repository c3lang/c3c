#include "lib.h"

#if PLATFORM_WINDOWS

#include <windows.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

static char *find_visual_studio(void);
static char *find_windows_kit_root(void);

WindowsSDK get_windows_link_paths()
{
	WindowsSDK out = {0};

	char *path = find_windows_kit_root();

	if (!path)
	{
		error_exit("Failed to find windows kit root.");
	}

	out.windows_sdk_path = path;
	out.vs_library_path = find_visual_studio();

	return out;
}


static char *find_visual_studio(void)
{
	// Let's locate vswhere.exe
	char *path = win_utf16to8(_wgetenv(L"ProgramFiles(x86)"));
	scratch_buffer_clear();
	scratch_buffer_printf("\"%s\\Microsoft Visual Studio\\Installer\\vswhere.exe\" -latest -prerelease -property installationPath -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -products *", path);
	const char *install_path = NULL;

	// Call vswhere.exe
	if (!execute_cmd_failable(scratch_buffer_to_string(), &install_path, NULL))
	{
		error_exit("Failed to find vswhere.exe to detect MSVC.");
	}

	// Find and read the version file.
	scratch_buffer_clear();
	scratch_buffer_printf("%s\\VC\\Auxiliary\\Build\\Microsoft.VCToolsVersion.default.txt", install_path);
	const char *version_file = scratch_buffer_to_string();
	DEBUG_LOG("MSVC version file: %s", version_file);
	size_t size;
	char *version = file_read_all(scratch_buffer_to_string(), &size);
	if (version) version = str_trim(version);
	if (!version || strlen(version) == 0)
	{
		error_exit("Failed to detect MSVC, could not read %s.", scratch_buffer_to_string());
	}

	// We have the version, so we're done with the path:
	scratch_buffer_clear();
	scratch_buffer_printf("%s\\VC\\Tools\\MSVC\\%s\\lib\\x64", install_path, version);
	return scratch_buffer_copy();
}


static char *find_windows_kit_root(void)
{
	HKEY main_key;
	LSTATUS rc = RegOpenKeyExW(HKEY_LOCAL_MACHINE, L"SOFTWARE\\Microsoft\\Windows Kits\\Installed Roots",
	                           0, KEY_QUERY_VALUE | KEY_WOW64_32KEY | KEY_ENUMERATE_SUB_KEYS, &main_key);
	if (rc != S_OK) return NULL;

	// Look for a Windows 10 entry.
	DWORD required_length;
	rc = RegQueryValueExW(main_key, L"KitsRoot10", NULL, NULL, NULL, &required_length);
	if (rc != S_OK) return NULL;

	DWORD length = required_length + 1;  // The +2 is for the maybe optional zero later on. Probably we are over-allocating.
	uint16_t *value = (uint16_t *)cmalloc(length * 2);
	if (!value) return NULL;

	rc = RegQueryValueExW(main_key, L"KitsRoot10", NULL, NULL, (LPBYTE)value, &required_length);  // We know that version is zero-terminated...
	if (rc != S_OK)
	{
		free(value);
		return NULL;
	}
	value[required_length / 2] = 0;

	// The key is found, so let's search the directory.
	char *root = win_utf16to8(value);
	free(value);

	scratch_buffer_clear();
	scratch_buffer_append(root);
	scratch_buffer_append("Lib\\*");

	WIN32_FIND_DATAW find_data;
	uint16_t *wildcard_name = win_utf8to16(scratch_buffer_to_string());
	HANDLE handle = FindFirstFileW(wildcard_name, &find_data);
	free(wildcard_name);

	if (handle == INVALID_HANDLE_VALUE) goto SEARCH_FAILED;

	char *best_file = NULL;
	long long best = 0;
	do
	{

		if (!(find_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)) continue;
		int i0, i1, i2, i3;
		int success = swscanf_s(find_data.cFileName, L"%d.%d.%d.%d", &i0, &i1, &i2, &i3);
		if (success < 4) continue;

		long long ver = i0 * 0x1000000000000LL
		                + i1 * 0x100000000LL
		                + i2 * 0x10000LL
		                + i3;
		if (ver > best)
		{
			best = ver;
			if (best_file) free(best_file);
			best_file = win_utf16to8(find_data.cFileName);
		}
	}
	while (FindNextFileW(handle, &find_data));
	FindClose(handle);

	if (!best_file) goto SEARCH_FAILED;;

	scratch_buffer_clear();
	scratch_buffer_append(root);
	scratch_buffer_append("Lib\\");
	scratch_buffer_append(best_file);

	free(root);
	free(best_file);
	return scratch_buffer_copy();

SEARCH_FAILED:
	free(root);
	return NULL;
}

#endif