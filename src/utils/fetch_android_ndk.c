#if defined(_WIN32) || defined(_WIN64)
	#define WIN32_LEAN_AND_MEAN
	#define TokenType WindowsTokenType
	#define MAX_PRIORITY WindowsMAX_PRIORITY
	#include <windows.h>
	#undef TokenType
	#undef MAX_PRIORITY
	#define STRCASECMP _stricmp
	#define STRNCASECMP _strnicmp
#else
	#include <dirent.h>
	#include <strings.h>
	#define STRCASECMP strcasecmp
	#define STRNCASECMP strncasecmp
#endif

#include "../compiler/compiler_internal.h"
#include "json.h"
#include "whereami.h"

// Minimal dirent-like structure for Windows
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

static char *get_android_ndk_output_path(void)
{
	char *env_path = NULL;
#if PLATFORM_WINDOWS
	env_path = getenv("LOCALAPPDATA");
#else
	env_path = getenv("XDG_CACHE_HOME");
#endif

	if (env_path)
	{
		return file_append_path(env_path, "c3/android_ndk");
	}

#if !PLATFORM_WINDOWS
	char *home = getenv("HOME");
	if (home) return file_append_path(home, ".cache/c3/android_ndk");
#endif

	const char *path = find_executable_path();
	return file_append_path(path, "android_ndk");
}

static bool check_license(void)
{
	printf("Do you agree with the license: https://developer.android.com/studio/terms  (Y/n): ");
	char c = (char)getchar();
	return (c == 'y' || c == 'Y' || c == '\n'); // assume yes on empty enter, or just Y
}

static void extract_ndk_zip(const char *zip_path, const char *out_root)
{
	FILE *f = file_open_read(zip_path);
	if (!f) error_exit("Failed to open %s", zip_path);
	ZipDirIterator iter;
	const char *err = zip_dir_iterator(f, &iter);
	if (err) error_exit("Zip error: %s", err);

	ZipFile zfile;
	while (iter.current_file < iter.files)
	{
		err = zip_dir_iterator_next(&iter, &zfile);
		if (err) error_exit("Zip iteration error: %s", err);
		zip_file_write(f, &zfile, out_root, true);
	}
	fclose(f);
}

char *fetch_android_ndk(BuildOptions *options)
{
	char *ndk_output = get_android_ndk_output_path();

	// If the folder already exists and has an NDK, just return it
	DIR *d_check = opendir(ndk_output);
	if (d_check)
	{
		struct dirent *dir;
		while ((dir = readdir(d_check)) != NULL)
		{
			if (strstr(dir->d_name, "android-ndk"))
			{
				char *final_path = file_append_path(ndk_output, dir->d_name);
				closedir(d_check);
				return final_path;
			}
		}
		closedir(d_check);
	}

	if (!check_license())
	{
		error_exit("License not accepted.");
	}

#if PLATFORM_WINDOWS
	const char *url = "https://dl.google.com/android/repository/android-ndk-r29-windows.zip";
#elif defined(__APPLE__)
	const char *url = "https://dl.google.com/android/repository/android-ndk-r29-darwin.zip";
#else
	const char *url = "https://dl.google.com/android/repository/android-ndk-r29-linux.zip";
#endif

	if (!download_available())
	{
		error_exit("Failed to find Android NDK.\n"
				   "To download the NDK automatically, please ensure libcurl is installed.\n"
				   "Alternatively, provide the NDK path manually using --android-ndk.");
	}

	const char *tmp_dir_base = dir_make_temp_dir();
	dir_make_recursive((char*)tmp_dir_base);
	if (!tmp_dir_base) error_exit("Failed to create temp directory");

	char *zpath = (char *)file_append_path(tmp_dir_base, "android_ndk.zip");

	printf("Downloading Android NDK from %s...\n", url);
	const char *err = download_file(url, "", zpath);
	if (err) error_exit("Download failed: %s", err);

	printf("Extracting NDK...\n");
	extract_ndk_zip(zpath, ndk_output);

	file_delete_dir(tmp_dir_base);

	// find the android-ndk-* directory inside ndk_output
	DIR *d = opendir(ndk_output);
	if (d)
	{
		struct dirent *dir;
		while ((dir = readdir(d)) != NULL)
		{
			if (strstr(dir->d_name, "android-ndk"))
			{
				char *final_path = file_append_path(ndk_output, dir->d_name);
				closedir(d);
				return final_path;
			}
		}
		closedir(d);
	}

	return str_dup(ndk_output);
}
