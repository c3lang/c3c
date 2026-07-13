#include <ctype.h>
#include <sys/stat.h>

#include "platform.h"
#include "fetch_utils.h"
#include "../../compiler/compiler_internal.h"
#include "../json.h"
#include "../msi.h"
#include "../whereami.h"

#define MAX_PATH_ZIP_FILENAME 512


static int verbose_level = 0;

#define MANIFEST_URL "https://aka.ms/vs/17/release/channel"
#define VS_MANIFEST_ID "Microsoft.VisualStudio.Manifests.VisualStudio"
#define BUILD_TOOLS_ID "Microsoft.VisualStudio.Product.BuildTools"


static void sdk_progress(int percent)
{
	print_progress("Downloading and extracting packages", percent, verbose_level);
}

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
	const char *err = download_file(url, "", dst, false);
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
			static const char *remaps[][2] = {
				{ "msvcrt.lib", "MSVCRT.lib" },
				{ "msvcrtd.lib", "MSVCRTD.lib" },
				{ "oldnames.lib", "OLDNAMES.lib" },
				{ "kernel32.lib", "Kernel32.lib" },
				{ "user32.lib", "User32.lib" },
				{ "gdi32.lib", "Gdi32.lib" },
				{ "shell32.lib", "Shell32.lib" },
				{ "winmm.lib", "Winmm.lib" },
				{ "advapi32.lib", "Advapi32.lib" },
				{ "ws2_32.lib", "Ws2_32.lib" },
				{ "opengl32.lib", "OpenGL32.lib" },
				{ "ntdll.lib", "Ntdll.lib" },
				{ "shlwapi.lib", "Shlwapi.lib" },
				{ "dbghelp.lib", "Dbghelp.lib" },
				{ "libcmt.lib", "LIBCMT.lib" },
				{ "libcmtd.lib", "LIBCMTD.lib" },
				{ "libcpmt.lib", "LIBCPMT.lib" },
				{ "libcpmtd.lib", "LIBCPMTD.lib" },
				{ "ole32.lib", "Ole32.lib" },
				{ "oleaut32.lib", "OleAut32.lib" },
				{ "uuid.lib", "Uuid.lib" },
				{ "comdlg32.lib", "ComDlg32.lib" },
			};
			for (size_t i = 0; i < ELEMENTLEN(remaps); i++)
			{
				if (str_eq(low_name, remaps[i][0]))
				{
					file_copy_file(s_path, file_append_path(dst, remaps[i][1]), true);
					break;
				}
			}

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

	const char *err = download_file(url, "", path, false);
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

static bool check_license(bool accept_all)
{
	if (accept_all) return true;
	printf("Do you accept the license? https://visualstudio.microsoft.com/license-terms/vs2022-ga-diagnosticbuildtools/"
	       " (Y/n): ");
	fflush(stdout);
	char c = (char)getchar();
	return (c == 'y' || c == 'Y' || c == '\n');
}

void fetch_winsdk(BuildOptions *options)
{
	if (!download_available())
	{
		error_exit("Failed to find Windows SDK.\n"
				   "Alternatively, provide the SDK path manually using --win-sdk.");
	}
	verbose_level = options->verbosity_level;

	// Determine which architectures to fetch.
	// When called from the linker, options->fetch_sdk_archs has one entry for the
	// target arch.  When called manually via 'fetch-sdk windows' the user can pass
	// one or more `--arch` flags; if none are given we default to the host architecture.
	const char **archs = options->fetch_sdk_archs;
	if (!archs || vec_size(archs) == 0)
	{
		archs = NULL;
#if defined(__aarch64__) || defined(_M_ARM64)
		vec_add(archs, "arm64");
#elif defined(__x86_64__) || defined(_M_X64) || defined(_M_AMD64)
		vec_add(archs, "x64");
#elif defined(__arm__) || defined(_M_ARM) // ...for completeness although not supported
		vec_add(archs, "arm");
#elif defined(__i386__) || defined(_M_IX86)
		vec_add(archs, "x86");
#else
		vec_add(archs, "x64"); // unknown host, fall back to x64 since it's the most widely used
#endif
	}

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

	char *sdk_output = get_cache_output_path("msvc_sdk");

	if (verbose_level >= 1) printf("Selected: MSVC %s, SDK %s\n", full_msvc_v, sdk_key);

	if (!options->fetch_accept_license)
	{
		char arch_list[256] = "";
		for (int i = 0; i < (int)vec_size(archs); i++)
		{
			if (i > 0) strcat(arch_list, ", ");
			strcat(arch_list, archs[i]);
		}
#if PLATFORM_WINDOWS
		printf("To target %s you need the MSVC SDK.\n", arch_list);
#else
		printf("To cross-compile to %s you need the MSVC SDK.\n", arch_list);
#endif
		printf("Downloading version %s to %s.\n", full_msvc_v, sdk_output);
	}

	if (!check_license(options->fetch_accept_license))
	{
		exit_compiler(EXIT_FAILURE);
	}

	char *out_root = file_append_path(tmp_dir_base, "OUTPUT");
	char *dl_root = file_append_path(tmp_dir_base, "DL");
	dir_make_recursive(out_root);
	dir_make_recursive(dl_root);

	JSONObject **checked_pkgs = NULL;
	JSONObject *sdk_comp = json_map_get(sdk_paths, sdk_key);
	JSONObject *deps_obj = json_map_get(sdk_comp, "dependencies");
	if (deps_obj && deps_obj->type == J_OBJECT)
	{
		FOREACH(const char *, dep, deps_obj->keys)
		{
			JSONObject *pkg = find_package_by_id(pkgs, dep);
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
	}

	// Dynamically prepare MSIs list based on requested archs
	const char **msi_names = NULL;
	vec_add(msi_names, "Windows SDK for Windows Store Apps Libs-x86_en-us.msi");
	vec_add(msi_names, "Universal CRT Headers Libraries and Sources-x86_en-us.msi");
	for (int i = 0; i < (int)vec_size(archs); i++)
	{
		char *msi_name = str_printf("Windows SDK Desktop Libs %s-x86_en-us.msi", archs[i]);
		vec_add(msi_names, msi_name);
	}

	int msi_count = (int)vec_size(msi_names);
	JSONObject **msi_packages = NULL;
	for (int i = 0; i < msi_count; i++) vec_add(msi_packages, (JSONObject *)NULL);

	for (int i = 0; i < msi_count; i++)
	{
		FOREACH(JSONObject *, pkg, checked_pkgs)
		{
			JSONObject *pls = json_map_get(pkg, "payloads");
			if (!pls) continue;
			FOREACH(JSONObject *, pl, pls->elements)
			{
				if (STRCASECMP(filename(json_map_get(pl, "fileName")->str), msi_names[i]) == 0)
				{
					msi_packages[i] = pkg;
					break;
				}
			}
			if (msi_packages[i]) break;
		}
		if (!msi_packages[i] && verbose_level >= 0)
		{
			eprintf("Warning: Could not find MSI package '%s' in the manifest.\n", msi_names[i]);
		}
		if (msi_packages[i] && verbose_level >= 1)
		{
			printf("Found MSI package: %s\n", msi_names[i]);
		}
	}

	int progress = 0;
	if (verbose_level == 0) sdk_progress(progress);

	// Download MSVC ASan headers base first
	{
		char *pid_headers = str_printf("microsoft.vc.%s.asan.headers.base", full_msvc_v);
		JSONObject *headers_pkg = find_package_by_id(pkgs, pid_headers);
		if (headers_pkg)
		{
			JSONObject *payloads_arr = json_map_get(headers_pkg, "payloads");
			FOREACH_IDX(j, JSONObject *, payload, payloads_arr->elements)
			{
				char *zpath = file_append_path(dl_root, str_printf("p_headers_%lu.zip", (unsigned long)j));
				if (download_with_verification(json_map_get(payload, "url")->str, pid_headers, zpath))
				{
					extract_msvc_zip(zpath, out_root);
				}
			}
		}
		progress += 8;
		sdk_progress(progress);
	}

	// Dynamic download of CRT and optionally ASan per requested arch
	for (int i = 0; i < (int)vec_size(archs); i++)
	{
		const char *arch = archs[i];
		const char *arch_suffixes[] = {"crt.%s.desktop.base", "crt.%s.store.base", "asan.%s.base"};
		for (int j = 0; j < ELEMENTLEN(arch_suffixes); j++)
		{
			char *suffix = str_printf(arch_suffixes[j], arch);
			char *pid_part = str_printf("microsoft.vc.%s.%s", full_msvc_v, suffix);
			JSONObject *best_pkg = find_package_by_id(pkgs, pid_part);
			if (best_pkg)
			{
				JSONObject *payloads_arr = json_map_get(best_pkg, "payloads");
				FOREACH_IDX(k, JSONObject *, payload, payloads_arr->elements)
				{
					char *zpath = file_append_path(dl_root, str_printf("p_%s_%d_%lu.zip", arch, j, (unsigned long)k));
					if (download_with_verification(json_map_get(payload, "url")->str, pid_part, zpath))
					{
						extract_msvc_zip(zpath, out_root);
					}
				}
			}
			progress += (32 / ((int)vec_size(archs) * (int)ELEMENTLEN(arch_suffixes)));
			sdk_progress(progress);
		}
	}
	progress = 40;
	sdk_progress(progress);

	const char **cab_list = NULL;
	for (int i = 0; i < msi_count; i++)
	{
		if (!msi_packages[i]) continue;
		JSONObject *pls = json_map_get(msi_packages[i], "payloads");
		FOREACH(JSONObject *, pl, pls->elements)
		{
			const char *f_name = json_map_get(pl, "fileName")->str;
			if (STRCASECMP(filename(f_name), msi_names[i]) == 0)
			{
				char *mpath = file_append_path(dl_root, msi_names[i]);
				if (download_with_verification(json_map_get(pl, "url")->str, msi_names[i], mpath))
				{
					get_msi_cab_list(mpath, &cab_list);
				}
				goto NEXT_MSI;
			}
		}
	NEXT_MSI:
		progress += (30 / msi_count);
		sdk_progress(progress);
	}

	int cabs_done = 0;
	int cab_count = (int)vec_size(cab_list);
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
					download_with_verification(json_map_get(pl, "url")->str, cab, file_append_path(dl_root, cab));
					goto NEXT_CAB;
				}
			}
		}
	NEXT_CAB:
		cabs_done++;
		if (cab_count > 0) sdk_progress(70 + (20 * cabs_done) / cab_count);
	}

	for (int i = 0; i < msi_count; i++)
	{
		char *mpath = (char *)file_append_path(dl_root, msi_names[i]);
		if (file_exists(mpath))
		{
			extract_msi(mpath, out_root, dl_root);
			sdk_progress(90 + (10 * (i + 1)) / msi_count);
		}
	}

	if (verbose_level == 0)
	{
		sdk_progress(100);
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

	// Finalizing and copying files for all requested target architectures
	for (int i = 0; i < (int)vec_size(archs); i++)
	{
		const char *arch = archs[i];
		char *sdk_arch = file_append_path(sdk_output, arch);
		dir_make_recursive(sdk_arch);

		char *ucrt_path = find_folder_inf(s_ucrt, arch, true);
		char *um_path = find_folder_inf(s_um, arch, true);
		char *msvc_path = find_folder_inf(s_msvc, arch, true);

		if (ucrt_path && um_path && msvc_path)
		{
			copy_to_msvc_sdk(ucrt_path, sdk_arch);
			copy_to_msvc_sdk(um_path, sdk_arch);
			copy_to_msvc_sdk(msvc_path, sdk_arch);
		}
		else
		{
			if (verbose_level >= 0)
			{
				eprintf("Warning: Missing library components for architecture %s (UCRT: %s, UM: %s, MSVC: %s)\n",
				        arch, ucrt_path ? "OK" : "MISSING", um_path ? "OK" : "MISSING", msvc_path ? "OK" : "MISSING");
			}
		}
	}

	if (verbose_level >= 0) printf("The 'msvc_sdk' directory was successfully generated at %s.\n", sdk_output);

	if (verbose_level == 0) file_delete_dir(tmp_dir_base);
}