#include "platform.h"
#include "fetch_utils.h"
#include "../../compiler/compiler_internal.h"
#include "../json.h"

static bool check_license(bool accept_all)
{
	if (accept_all) return true;
	printf("Do you accept the license? https://developer.android.com/studio/terms (Y/n): ");
	char c = (char)getchar();
	return (c == 'y' || c == 'Y' || c == '\n');
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
	char *ndk_output = get_cache_output_path("android_ndk");

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

	if (!options->fetch_accept_license)
	{
		printf("To cross-compile to android-*, you need the Android NDK.\n");
		printf("Downloading to %s.\n", ndk_output);
	}

	if (!check_license(options->fetch_accept_license))
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
	dir_make_recursive((char *)tmp_dir_base);
	if (!tmp_dir_base) error_exit("Failed to create temp directory");

	char *zpath = (char *)file_append_path(tmp_dir_base, "android_ndk.zip");

	printf("Downloading Android NDK r29...\n");
	const char *err = download_file_with_progress(url, "", zpath, print_progress);
	if (err) error_exit("Download failed: %s", err);

	printf("Extracting NDK...\n");
	extract_ndk_zip(zpath, ndk_output);

	file_delete_dir(tmp_dir_base);

	// Find the android-ndk-* directory inside ndk_output
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
