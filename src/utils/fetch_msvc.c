#include <ctype.h>
#include <sys/stat.h>
#include <limits.h>

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
#include "msi.h"
#include "whereami.h"

#ifndef MAX_PATH
	#if defined(PATH_MAX)
		#define MAX_PATH PATH_MAX
	#elif defined(_MAX_PATH)
		#define MAX_PATH _MAX_PATH
	#else
		#define MAX_PATH 260
	#endif
#endif
#define MAX_PATH_ZIP_FILENAME 512

#define MANIFEST_URL "https://aka.ms/vs/17/release/channel"
#define VS_MANIFEST_ID "Microsoft.VisualStudio.Manifests.VisualStudio"
#define BUILD_TOOLS_ID "Microsoft.VisualStudio.Product.BuildTools"

static char *get_sdk_output_path(void)
{
#if PLATFORM_WINDOWS
	const char *path = find_executable_path();
	return file_append_path(path, "msvc_sdk");
#else
	char *cache_home = getenv("XDG_CACHE_HOME");
	if (cache_home) return file_append_path(cache_home, "c3/msvc_sdk");

	char *home = getenv("HOME");
	if (home) return file_append_path(home, ".cache/c3/msvc_sdk");

	const char *path = find_executable_path();
	return file_append_path(path, "msvc_sdk");
#endif
}

static int verbose_level = 0;

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

static int version_compare(const char *v1, const char *v2)
{
	while (*v1 && *v2)
	{
		int n1 = atoi(v1);
		int n2 = atoi(v2);
		if (n1 > n2) return 1;
		if (n1 < n2) return -1;
		while (char_is_digit(*v1))
			v1++;
		while (char_is_digit(*v2))
			v2++;
		if (*v1 == '.') v1++;
		if (*v2 == '.') v2++;
	}
	if (*v1) return 1;
	if (*v2) return -1;
	return 0;
}

static char *pick_max_version(JSONObject *map)
{
	char *max_v = NULL;
	FOREACH(const char *, key, map->keys)
	{
		if (!max_v || version_compare(key, max_v) > 0) max_v = (char *)key;
	}
	return max_v;
}

static bool my_strcasestr(const char *h, const char *n)
{
	size_t nl = strlen(n);
	for (; *h; h++)
		if (STRNCASECMP(h, n, nl) == 0) return true;
	return false;
}

static char *find_folder_inf(const char *root, const char *pattern, bool exact)
{
	DIR *d = opendir(root);
	if (!d) return NULL;
	struct dirent *de;
	char *found = NULL;
	while ((de = readdir(d)))
	{
		if (de->d_name[0] == '.') continue;
		char *path = (char *)file_append_path(root, de->d_name);
		if (file_is_dir(path))
		{
			bool match = exact ? (STRCASECMP(de->d_name, pattern) == 0)
			                   : (my_strcasestr(de->d_name, pattern));
			if (match)
			{
				found = path;
				break;
			}
			found = find_folder_inf(path, pattern, exact);
			if (found) break;
		}
	}
	closedir(d);
	return found;
}

static bool download_with_verification(const char *url, const char *name,
                                       const char *dst)
{
    if (verbose_level >= 1)
    {
        printf("%s ... downloading", name);
        fflush(stdout);
    }
    else if (verbose_level == 0)
    {
        printf(".");
        fflush(stdout);
    }
    const char *err = download_file(url, "", dst);
    if (err)
    {
        if (verbose_level >= 1) printf(" ... failed.\n");
        if (verbose_level >= 0)
            eprintf("\nWarning: Download failed for %s: %s\n", name, err);
        return false;
    }
    if (verbose_level >= 1) printf(" ... done.\n");
    return true;
}

static void copy_to_msvc_sdk(const char *src, const char *dst)
{
	DIR *d = opendir(src);
	if (!d) return;
	dir_make_recursive((char *)dst);
	struct dirent *de;
	while ((de = readdir(d)))
	{
		if (de->d_name[0] == '.') continue;
		char *s_path = file_append_path(src, de->d_name);
		char *low_name = str_dup(de->d_name);
		for (char *p = low_name; *p; p++)
			*p = (char)tolower((unsigned char)*p);
		char *d_path = file_append_path(dst, low_name);

		if (file_is_dir(s_path))
		{
			copy_to_msvc_sdk(s_path, d_path);
		}
		else
		{
			if (str_eq(low_name, "msvcrt.lib"))
				file_copy_file(s_path, (char *)file_append_path(dst, "MSVCRT.lib"), true);
			else if (str_eq(low_name, "oldnames.lib"))
				file_copy_file(s_path, (char *)file_append_path(dst, "OLDNAMES.lib"), true);

			file_copy_file(s_path, d_path, true);
		}
	}
	closedir(d);
}

static void extract_msvc_zip(const char *zip_path, const char *out_root)
{
	FILE *f = file_open_read(zip_path);
	if (!f) error_exit("Failed to open %s", zip_path);
	ZipDirIterator iter;
	const char *err = zip_dir_iterator(f, &iter);
	if (err) error_exit("Zip error: %s", err);

	ZipFile zfile;
	const char *zip_content_prefix = "Contents/";
	size_t prefix_len = strlen(zip_content_prefix);
	while (iter.current_file < iter.files)
	{
		err = zip_dir_iterator_next(&iter, &zfile);
		if (err) error_exit("Zip iteration error: %s", err);
		if (str_start_with(zfile.name, zip_content_prefix))
		{
			char original_name[MAX_PATH_ZIP_FILENAME];
			memcpy(original_name, zfile.name, MAX_PATH_ZIP_FILENAME);
			char *name = zfile.name;
			memmove(name, name + prefix_len, strlen(name) - prefix_len + 1);
			for (char *p = name; *p; p++)
			{
				if (*p == '\\') *p = '/';
				*p = (char)tolower((unsigned char)*p);
			}
			zip_file_write(f, &zfile, out_root, true);
			memcpy(zfile.name, original_name, MAX_PATH_ZIP_FILENAME);
		}
	}
	fclose(f);
}

static void get_msi_cab_list(const char *msi_path, const char ***cabs)
{
	const size_t ext_len = 4;
	const size_t guid_len = 32;
	const size_t filename_len = guid_len + ext_len;

	size_t size = (size_t)-1;
	unsigned char *buf = (unsigned char *)file_read_binary(msi_path, &size);
	if (!buf) return;
	for (size_t i = 0; i < size - ext_len; i++)
	{
		if (STRNCASECMP((char *)buf + i, ".cab", ext_len) == 0 && i >= guid_len)
		{
			char cab[128];
			memcpy(cab, buf + i - guid_len, filename_len);
			cab[filename_len] = 0;
			bool valid = true;
			for (int j = 0; j < (int)guid_len; j++)
				if (!char_is_hex(cab[j]))
				{
					valid = false;
					break;
				}
			if (valid)
			{
				bool exists = false;
				FOREACH(const char *, existing, *cabs)
				{
					if (STRCASECMP(existing, cab) == 0)
					{
						exists = true;
						break;
					}
				}
				if (!exists) vec_add(*cabs, str_dup(cab));
			}
		}
	}
}

static void print_msvc_version(JSONObject *pkg, char out[128])
{
	const char *id = json_map_get(pkg, "id")->str;
	StringSlice slice = slice_from_string(id);
	const int id_prefix_segments = 4;
	for (int i = 0; i < id_prefix_segments; i++)
		slice_next_token(&slice, '.');
	out[0] = 0;
	const int max_version_segments = 20;
	for (int i = 0; i < max_version_segments; i++)
	{
		StringSlice v = slice_next_token(&slice, '.');
		if (slice_strcmp(v, "x86")) break;
		if (i > 0) strcat(out, ".");
		strncat(out, v.ptr, v.len);
	}
}

static void extract_msi(const char *mpath, const char *out_root,
                        const char *dl_root)
{
	if (verbose_level >= 1)
	{
		printf("Extracting MSI: %s\n", mpath);
		fflush(stdout);
	}
	if (!msi_extract(mpath, out_root, dl_root, verbose_level >= 1))
	{
		fprintf(stderr, "Failed to extract MSI: %s\n", mpath);
	}
}

static bool is_english_package(JSONObject *pkg)
{
	JSONObject *lang = json_map_get(pkg, "language");
	return !lang || STRCASECMP(lang->str, "en-US") == 0;
}

static JSONObject *find_package_by_id(JSONObject *pkgs, const char *id)
{
	FOREACH(JSONObject *, pkg, pkgs->elements)
	{
		if (STRCASECMP(json_map_get(pkg, "id")->str, id) == 0)
		{
			if (is_english_package(pkg)) return pkg;
		}
	}
	return NULL;
}

static void collect_versions(JSONObject *pkgs, JSONObject **msvc_vers_out,
                             JSONObject **sdk_paths_out)
{
	JSONObject *msvc_vers = json_new_object(J_OBJECT);
	JSONObject *sdk_paths = json_new_object(J_OBJECT);

	FOREACH(JSONObject *, pkg, pkgs->elements)
	{
		JSONObject *id_obj = json_map_get(pkg, "id");
		if (!id_obj) continue;
		const char *id = id_obj->str;
		if (str_start_with(id, "Microsoft.VisualStudio.Component.VC.") &&
		    strstr(id, ".x86.x64"))
		{
			StringSlice slice = slice_from_string(id);
			const int id_prefix_segments = 4;
			for (int i = 0; i < id_prefix_segments; i++)
				slice_next_token(&slice, '.');
			StringSlice v4 = slice_next_token(&slice, '.');
			if (!v4.len || !char_is_digit(v4.ptr[0])) continue;
			StringSlice v5 = slice_next_token(&slice, '.');
			char *vkey = str_printf("%.*s.%.*s", (int)v4.len, v4.ptr,
			                        (int)v5.len, v5.ptr);
			json_map_set(msvc_vers, vkey, pkg);
		}
		else if (str_start_with(id, "Microsoft.VisualStudio.Component.Windows10SDK.") ||
		         str_start_with(id, "Microsoft.VisualStudio.Component.Windows11SDK."))
		{
			const char *last_dot = strrchr(id, '.');
			if (last_dot && char_is_digit(last_dot[1]))
				json_map_set(sdk_paths, last_dot + 1, pkg);
		}
	}
	*msvc_vers_out = msvc_vers;
	*sdk_paths_out = sdk_paths;
}

static JSONObject *load_manifest(const char *url, const char *path, const char *description)
{
	if (verbose_level >= 1)
	{
		printf("Downloading %s manifest...\n", description);
	}

	const char *err = download_file(url, "", path);
	if (err) error_exit("Failed to download %s manifest: %s", description, err);
	if (verbose_level >= 1)
	{
		printf(" Done.\n");
	}

	size_t size;
	char *json_str = file_read_all(path, &size);
	JsonParser parser;
	json_init_string(&parser, json_str);
	JSONObject *obj = json_parse(&parser);
	if (!obj || obj->type == J_ERROR) error_exit("Failed to parse %s manifest", description);
	return obj;
}

static void select_versions(BuildOptions *options, JSONObject *msvc_vers, JSONObject *sdk_paths,
                            char **msvc_key_out, char **sdk_key_out)
{
	char *msvc_key = (char *)options->msvc_version_override;
	if (!msvc_key)
	{
		msvc_key = pick_max_version(msvc_vers);
	}
	else if (!json_map_get(msvc_vers, msvc_key))
	{
		bool found = false;
		FOREACH(const char *, key, msvc_vers->keys)
		{
			char full_v[128];
			print_msvc_version(json_map_get(msvc_vers, key), full_v);
			if (str_eq(full_v, msvc_key))
			{
				msvc_key = (char *)key;
				found = true;
				break;
			}
		}
		if (!found) error_exit("Could not find MSVC version '%s'", options->msvc_version_override);
	}

	char *sdk_key = (char *)options->msvc_sdk_version_override;
	if (!sdk_key) sdk_key = pick_max_version(sdk_paths);
	if (!json_map_get(sdk_paths, sdk_key)) error_exit("Could not find SDK version '%s'", sdk_key);

	*msvc_key_out = msvc_key;
	*sdk_key_out = sdk_key;
}

static bool check_license(JSONObject *rj1_channel_items, bool accept_all)
{
	if (accept_all) return true;

	JSONObject *tools = NULL;
	FOREACH(JSONObject *, item, rj1_channel_items->elements)
	{
		JSONObject *id = json_map_get(item, "id");
		if (id && str_eq(id->str, BUILD_TOOLS_ID))
		{
			tools = item;
			break;
		}
	}

	const char *lic = "";
	if (tools)
	{
		JSONObject *res = json_map_get(tools, "localizedResources");
		FOREACH(JSONObject *, r, res->elements)
		{
			JSONObject *lang = json_map_get(r, "language");
			if (lang && (STRCASECMP(lang->str, "en-us") == 0 || STRCASECMP(lang->str, "en-US") == 0))
			{
				lic = json_map_get(r, "license")->str;
				break;
			}
		}
	}

	printf("Do you accept the license %s? [y/N]: ", lic);

	char c = (char)getchar();
	return (c == 'y' || c == 'Y');
}

void fetch_msvc(BuildOptions *options)
{
	verbose_level = options->verbosity_level;
	const char *tmp_dir_base = dir_make_temp_dir();
	if (!tmp_dir_base) error_exit("Failed to create temp directory");
	if (verbose_level >= 1) printf("Temp dir: %s\n", tmp_dir_base);

	const char *m1_path = file_append_path(tmp_dir_base, "vschannel.json");
	JSONObject *rj1 = load_manifest(MANIFEST_URL, m1_path, "channel");

	JSONObject *vsm = NULL;
	JSONObject *rj1_channel_items = json_map_get(rj1, "channelItems");
	FOREACH(JSONObject *, item, rj1_channel_items->elements)
	{
		JSONObject *id = json_map_get(item, "id");
		if (id && str_eq(id->str, VS_MANIFEST_ID))
		{
			vsm = item;
			break;
		}
	}
	if (!vsm) error_exit("Could not find VS manifest entry in channel file");

	JSONObject *payloads = json_map_get(vsm, "payloads");
	const char *vsu = json_map_get(payloads->elements[0], "url")->str;
	const char *vs_path_manifest = file_append_path(tmp_dir_base, "vs_manifest.json");
	JSONObject *vsroot = load_manifest(vsu, vs_path_manifest, "VS packages");



	JSONObject *pkgs = json_map_get(vsroot, "packages");
	JSONObject *msvc_vers, *sdk_paths;
	collect_versions(pkgs, &msvc_vers, &sdk_paths);

	if (options->msvc_show_versions)
	{
		printf("Available MSVC versions:\n");
		FOREACH(const char *, key, msvc_vers->keys)
		{
			char full_v[128];
			print_msvc_version(json_map_get(msvc_vers, key), full_v);
			printf("  %s (%s)\n", full_v, key);
		}
		printf("\nAvailable Windows SDK versions:\n");
		FOREACH(const char *, key, sdk_paths->keys) { printf("  %s\n", key); }
		return;
	}

	char *msvc_key, *sdk_key;
	select_versions(options, msvc_vers, sdk_paths, &msvc_key, &sdk_key);

	JSONObject *msvc_pkg_obj = json_map_get(msvc_vers, msvc_key);
	char full_msvc_v[128];
	print_msvc_version(msvc_pkg_obj, full_msvc_v);

	char *sdk_output = get_sdk_output_path();

	if (verbose_level >= 1) printf("Selected: MSVC %s, SDK %s\n", full_msvc_v, sdk_key);

	if (!options->msvc_accept_license)
	{
#if PLATFORM_WINDOWS
		printf("To target windows-x64 you need the MSVC SDK.\n");
#else
		printf("To cross-compile to windows-x64 you need the MSVC SDK.\n");
#endif
		printf("Downloading version %s to %s.\n", full_msvc_v, sdk_output);
	}

	if (!check_license(rj1_channel_items, options->msvc_accept_license))
	{
		exit_compiler(EXIT_FAILURE);
	}

	char *out_root = (char *)file_append_path(tmp_dir_base, "OUTPUT");
	char *dl_root = (char *)file_append_path(tmp_dir_base, "DL");
	dir_make_recursive(out_root);
	dir_make_recursive(dl_root);

	if (verbose_level == 0)
	{
		printf("Downloading and extracting packages");
		fflush(stdout);
	}

	const char *suffixes[] = {"asan.headers.base", "crt.x64.desktop.base", "crt.x64.store.base", "asan.x64.base"};
	for (int i = 0; i < ELEMENTLEN(suffixes); i++)
	{
		char *pid_part = str_printf("microsoft.vc.%s.%s", full_msvc_v, suffixes[i]);
		JSONObject *best_pkg = find_package_by_id(pkgs, pid_part);
		if (best_pkg)
		{
			JSONObject *payloads_arr = json_map_get(best_pkg, "payloads");
			FOREACH_IDX(j, JSONObject *, payload, payloads_arr->elements)
			{
				char *zpath = (char *)file_append_path(dl_root, str_printf("p%d_%lu.zip", i, (unsigned long)j));
				if (download_with_verification(json_map_get(payload, "url")->str, pid_part, zpath))
				{
					extract_msvc_zip(zpath, out_root);
				}
			}
			if (verbose_level == 0)
			{
				printf(".");
				fflush(stdout);
			}
		}
	}

	JSONObject *sdk_comp = json_map_get(sdk_paths, sdk_key);
	const char **sdk_pkg_ids = NULL;
	JSONObject *deps_obj = json_map_get(sdk_comp, "dependencies");
	if (deps_obj && deps_obj->type == J_OBJECT)
	{
		FOREACH(const char *, dep, deps_obj->keys) vec_add(sdk_pkg_ids, dep);
	}

	const char *msi_names[] = {
	    "Windows SDK for Windows Store Apps Libs-x86_en-us.msi",
	    "Windows SDK Desktop Libs x64-x86_en-us.msi",
	    "Universal CRT Headers Libraries and Sources-x86_en-us.msi"};
	const char **cab_list = NULL;
	JSONObject **checked_pkgs = NULL;

	FOREACH(const char *, sid, sdk_pkg_ids)
	{
		JSONObject *pkg = find_package_by_id(pkgs, sid);
		if (pkg)
		{
			vec_add(checked_pkgs, pkg);
			JSONObject *p_deps = json_map_get(pkg, "dependencies");
			if (p_deps && p_deps->type == J_OBJECT)
			{
				FOREACH(const char *, pd_id, p_deps->keys)
				{
					JSONObject *ppkg = find_package_by_id(pkgs, pd_id);
					if (ppkg) vec_add(checked_pkgs, ppkg);
				}
			}
		}
	}

	for (int i = 0; i < ELEMENTLEN(msi_names); i++)
	{
		FOREACH(JSONObject *, pkg, checked_pkgs)
		{
			JSONObject *pls = json_map_get(pkg, "payloads");
			if (!pls) continue;
			FOREACH(JSONObject *, pl, pls->elements)
			{
				const char *f_name = json_map_get(pl, "fileName")->str;
				if (STRCASECMP(filename(f_name), msi_names[i]) == 0)
				{
					char *mpath = (char *)file_append_path(dl_root, msi_names[i]);
					if (download_with_verification(json_map_get(pl, "url")->str, msi_names[i], mpath))
					{
						get_msi_cab_list(mpath, &cab_list);
					}
					goto NEXT_MSI;
				}
			}
		}
	NEXT_MSI:;
	}

	FOREACH(const char *, cab, cab_list)
	{
		FOREACH(JSONObject *, pkg, checked_pkgs)
		{
			JSONObject *pls = json_map_get(pkg, "payloads");
			if (!pls) continue;
			FOREACH(JSONObject *, pl, pls->elements)
			{
				const char *p_fname = json_map_get(pl, "fileName")->str;
				if (STRCASECMP(filename(p_fname), cab) == 0)
				{
					download_with_verification(json_map_get(pl, "url")->str, cab, (char *)file_append_path(dl_root, cab));
					goto NEXT_CAB;
				}
			}
		}
	NEXT_CAB:;
	}

	for (int i = 0; i < ELEMENTLEN(msi_names); i++)
	{
		char *mpath = (char *)file_append_path(dl_root, msi_names[i]);
		if (file_exists(mpath))
		{
			extract_msi(mpath, out_root, dl_root);
			if (verbose_level == 0)
			{
				printf(".");
				fflush(stdout);
			}
		}
	}

	if (verbose_level == 0)
	{
		printf(" Done.\n");
		fflush(stdout);
	}

	if (verbose_level >= 1) printf("Finalizing SDK\n");
	char *s_vc_root = find_folder_inf(out_root, "vc", false);
	char *s_msvc_base = s_vc_root ? find_folder_inf(s_vc_root, "msvc", false) : NULL;
	char *s_msvc = s_msvc_base ? find_folder_inf(s_msvc_base, "lib", true) : NULL;

	char *s_kits = find_folder_inf(out_root, "windows kits", false);
	char *s_lib = s_kits ? find_folder_inf(s_kits, "lib", true) : NULL;
	char *s_sdk_v = s_lib ? find_folder_inf(s_lib, sdk_key, false) : NULL;
	char *s_ucrt = s_sdk_v ? find_folder_inf(s_sdk_v, "ucrt", true) : NULL;
	char *s_um = s_sdk_v ? find_folder_inf(s_sdk_v, "um", true) : NULL;

	if (!s_ucrt || !s_um || !s_msvc)
	{
		if (verbose_level >= 0)
			eprintf("UCRT: %s, UM: %s, MSVC: %s\n", s_ucrt ? "OK" : "MISSING", s_um ? "OK" : "MISSING", s_msvc ? "OK" : "MISSING");
		error_exit("Missing library components");
	}

	char *sdk_x64 = (char *)file_append_path(sdk_output, "x64");
	dir_make_recursive(sdk_x64);
	copy_to_msvc_sdk(file_append_path(s_ucrt, "x64"), sdk_x64);
	copy_to_msvc_sdk(file_append_path(s_um, "x64"), sdk_x64);
	copy_to_msvc_sdk(file_append_path(s_msvc, "x64"), sdk_x64);

	if (verbose_level >= 0) printf("The 'msvc_sdk' directory was successfully generated at %s.\n", sdk_output);

	if (verbose_level == 0) file_delete_dir(tmp_dir_base);
}
