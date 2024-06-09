#include "lib.h"

#if PLATFORM_WINDOWS || 1

#include <windows.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <io.h>         // For _get_osfhandle

void free_windows_link_paths(WindowsSDK *obj)
{
	free(obj->vs_library_path);
	free(obj->windows_sdk_ucrt_library_path);
	free(obj->windows_sdk_um_library_path);
}

typedef struct
{
	int windows_sdk_version;   // Zero if no Windows SDK found.
	wchar_t *windows_sdk_root;
	wchar_t *windows_sdk_um_library_path;
	wchar_t *windows_sdk_ucrt_library_path;
	char *vs_library_path;
} Find_Result;


Find_Result find_visual_studio_and_windows_sdk();
void free_resources(Find_Result* result);

WindowsSDK get_windows_link_paths()
{
	Find_Result paths = find_visual_studio_and_windows_sdk();
	WindowsSDK out = {0};

	// note: WideCharToMultiByte doesn't seem to do null termination.
	// I'm wary of manually adding a null terminator, so hopefully this is reliable.
	// This wouldn't be a problem if windows used UTF-8 like the rest of the world >:(
	out.windows_sdk_um_library_path = win_utf16to8(paths.windows_sdk_um_library_path);
	out.windows_sdk_ucrt_library_path = win_utf16to8(paths.windows_sdk_ucrt_library_path);
	out.vs_library_path = paths.vs_library_path;
	free_resources(&paths);
	return out;
}


// Everything past here is taken from microsoft_craziness.h:
// https://gist.github.com/ActuallyaDeviloper/cd25b190743234d58079d6b08a8631e3
// Retrieved 2021-11-25
//
// Credit to Jonathan Blow, Kalinovcic, and ActuallyaDeviloper.

void free_resources(Find_Result *result)
{
	free(result->windows_sdk_root);
	free(result->windows_sdk_um_library_path);
	free(result->windows_sdk_ucrt_library_path);
	free(result->vs_library_path);
}

typedef struct
{
	int32_t best_version[4];  // For Windows 8 versions, only two of these numbers are used.
	wchar_t *best_name;
} Version_Data;

bool os_file_exists(wchar_t* name)
{
	// @Robustness: What flags do we really want to check here?

	DWORD attrib = GetFileAttributesW(name);
	if (attrib == INVALID_FILE_ATTRIBUTES) return false;
	if (attrib & FILE_ATTRIBUTE_DIRECTORY) return false;

	return true;
}

#define concat2(a, b) concat(a, b, NULL, NULL)
#define concat3(a, b, c) concat(a, b, c, NULL)
#define concat4(a, b, c, d) concat(a, b, c, d)
wchar_t* concat(wchar_t* a, wchar_t* b, wchar_t* c, wchar_t* d) {
  // Concatenate up to 4 wide strings together. Allocated with malloc.
  // If you don't like that, use a programming language that actually
  // helps you with using custom allocators. Or just edit the code.

  size_t len_a = wcslen(a);
  size_t len_b = wcslen(b);

  size_t len_c = 0;
  if (c) len_c = wcslen(c);

  size_t len_d = 0;
  if (d) len_d = wcslen(d);

  wchar_t* result = (wchar_t*)malloc((len_a + len_b + len_c + len_d + 1) * 2);
  memcpy(result, a, len_a * 2);
  memcpy(result + len_a, b, len_b * 2);

  if (c) memcpy(result + len_a + len_b, c, len_c * 2);
  if (d) memcpy(result + len_a + len_b + len_c, d, len_d * 2);

  result[len_a + len_b + len_c + len_d] = 0;

  return result;
}

typedef void (*Visit_Proc_W)(wchar_t* short_name, wchar_t* full_name, Version_Data* data);

bool visit_files_w(wchar_t *dir_name, Version_Data *data, Visit_Proc_W proc)
{
	// Visit everything in one folder (non-recursively). If it's a directory
	// that doesn't start with ".", call the visit proc on it. The visit proc
	// will see if the filename conforms to the expected versioning pattern.

	WIN32_FIND_DATAW find_data;

	wchar_t *wildcard_name = concat2(dir_name, L"\\*");
	HANDLE handle = FindFirstFileW(wildcard_name, &find_data);
	free(wildcard_name);

	if (handle == INVALID_HANDLE_VALUE) return false;

	while (true)
	{
		if ((find_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) && (find_data.cFileName[0] != '.'))
		{
			wchar_t *full_name = concat3(dir_name, L"\\", find_data.cFileName);
			proc(find_data.cFileName, full_name, data);
			free(full_name);
		}

		BOOL success = FindNextFileW(handle, &find_data);
		if (!success) break;
	}

	FindClose(handle);

	return true;
}


void win10_best(wchar_t* short_name, wchar_t* full_name, Version_Data* data)
{
	// Find the Windows 10 subdirectory with the highest version number.

	int i0, i1, i2, i3;
	int success = swscanf_s(short_name, L"%d.%d.%d.%d", &i0, &i1, &i2, &i3);
	if (success < 4) return;

	if (i0 < data->best_version[0]) return;
	else if (i0 == data->best_version[0]) {
		if (i1 < data->best_version[1]) return;
		else if (i1 == data->best_version[1]) {
		  if (i2 < data->best_version[2]) return;
		  else if (i2 == data->best_version[2]) {
			if (i3 < data->best_version[3]) return;
		  }
		}
	}

	// we have to copy_string and free here because visit_files free's the full_name string
	// after we execute this function, so Win*_Data would contain an invalid pointer.
	if (data->best_name) free(data->best_name);
	data->best_name = _wcsdup(full_name);

	if (data->best_name)
	{
		data->best_version[0] = i0;
		data->best_version[1] = i1;
		data->best_version[2] = i2;
		data->best_version[3] = i3;
	}
}


bool find_windows_kit_root(Find_Result* result)
{
	// Information about the Windows 10 and Windows 8 development kits
	// is stored in the same place in the registry. We open a key
	// to that place, first checking preferntially for a Windows 10 kit,
	// then, if that's not found, a Windows 8 kit.

	HKEY main_key;
	LSTATUS rc = RegOpenKeyExW(HKEY_LOCAL_MACHINE, L"SOFTWARE\\Microsoft\\Windows Kits\\Installed Roots",
		0, KEY_QUERY_VALUE | KEY_WOW64_32KEY | KEY_ENUMERATE_SUB_KEYS, &main_key);
	if (rc != S_OK) return false;

	// Look for a Windows 10 entry.

	DWORD required_length;
	rc = RegQueryValueExW(main_key, L"KitsRoot10", NULL, NULL, NULL, &required_length);
	if (rc != S_OK) return false;

	DWORD length = required_length + 1;  // The +2 is for the maybe optional zero later on. Probably we are over-allocating.
	uint16_t *value = (uint16_t *)cmalloc(length * 2);
	if (!value) return false;

	rc = RegQueryValueExW(main_key, L"KitsRoot10", NULL, NULL, (LPBYTE)value, &required_length);  // We know that version is zero-terminated...
	if (rc != S_OK) return false;
	value[required_length / 2] = 0;

	puts("Convert");
	char *root = win_utf16to8(value);
	printf("Now: %s\n", root);
	scratch_buffer_clear();
	scratch_buffer_append(root);
	scratch_buffer_append("Lib\\*");

	printf("Grabbing %s\n", scratch_buffer_to_string());
	puts("Find first");
	WIN32_FIND_DATAW find_data;
	uint16_t *wildcard_name = win_utf8to16(scratch_buffer_to_string());
	HANDLE handle = FindFirstFileW(wildcard_name, &find_data);
	free(wildcard_name);

	if (handle == INVALID_HANDLE_VALUE) return false;
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
		printf("%lld\n", ver);
		if (ver > best)
		{
			best = ver;
			if (best_file) free(best_file);
			best_file = win_utf16to8(find_data.cFileName);
			printf("Best now %s\n", best_file);
		}
	}
	while (FindNextFileW(handle, &find_data));

	FindClose(handle);
	if (!best_file) return false;

	printf("Best was %s\n", best_file);
	error_exit("Ok bye");


	if (root) {
		wchar_t* windows10_lib = concat2(value, L"Lib");
		free(root);

		Version_Data data = { 0 };
		visit_files_w(windows10_lib, &data, win10_best);
		free(windows10_lib);

		if (data.best_name) {
		  result->windows_sdk_version = 10;
		  result->windows_sdk_root = data.best_name;
		  RegCloseKey(main_key);
		  return true;
		}
	}
	return false;
}

bool find_visual_studio(Find_Result* result)
{
	// Let's locate vswhere.exe
	char *path = win_utf16to8(_wgetenv(L"ProgramFiles(x86)"));
	scratch_buffer_clear();
	DEBUG_LOG("Program files path: %s", path);
	scratch_buffer_printf("\"%s\\Microsoft Visual Studio\\Installer\\vswhere.exe\" -latest -prerelease -property installationPath", path);
	const char *install_path = NULL;

	// Call vswhere.exe
	if (!execute_cmd_failable(scratch_buffer_to_string(), &install_path))
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
	result->vs_library_path = scratch_buffer_copy();
	return true;
}


Find_Result find_visual_studio_and_windows_sdk()
{
	Find_Result result = {0};

	if (!find_windows_kit_root(&result))
	{
		error_exit("Failed to find windows kit root.");
	}

	if (result.windows_sdk_root)
	{
		result.windows_sdk_um_library_path = concat2(result.windows_sdk_root, L"\\um\\x64");
		result.windows_sdk_ucrt_library_path = concat2(result.windows_sdk_root, L"\\ucrt\\x64");
	}

	find_visual_studio(&result);

	return result;
}

#endif