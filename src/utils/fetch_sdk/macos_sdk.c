#include <stdio.h>
#include <string.h>

#include "fetch_utils.h"
#include "xz.h"
#include "../lib.h"
#include "build/build.h"
#include "utils/cpio.h"
#include "utils/pbzx.h"
#include "utils/sucatalog.h"
#include "utils/xar.h"

#include "platform.h"

/* Simply append macOS version after index- */
#define LATEST_SUCATALOG "https://swscan.apple.com/content/catalogs/others/index-27-26-15-14-13-12-10.16-10.15-10.14-10.13-10.12-10.11-10.10-10.9-mountainlion-lion-snowleopard-leopard.merged-1.sucatalog"
#define BASE_URL "https://swcdn.apple.com/content/downloads"
#define BASE_PKG "CLTools_macOS_SDK.pkg"
#define BASE_PKM "CLTools_macOSNMOS_SDK.pkm"
#define SDK_PKG "CLTools_macOSNMOS_SDK.pkg"

#define FETCH_DYNAMIC -1

#define PROGRESS_UPDATE 19

typedef struct
{
	Version version;
	char *sub_url;
} Sdk;

typedef struct
{
	int major;
	const char *name;
} ReleaseInfo;

typedef enum
{
	DOWNLOAD,
	EXTRACT,
	FIXUP,
	CLEANUP,
	DONE
} Progress;

static int verbose_level = 0;
static char *sucatalog_out = "";
static char *metadata_out = "";

static Sdk hardcoded[] = {
	{ { 12, 4 }, "/46/21/001-89745-A_56FM390IW5/v1um2qppgfdnam2e9cdqcqu2r6k8aa3lis/" },
	{ { 12, 5 }, "/02/62/071-54303-A_EU2CL1YVT7/943i95dpeyi2ghlnj2mgyq3t202t5gf18b/" },
	{ { 13, 2 }, "/52/17/002-41708-A_E8MFK7B2PK/6p55tbmh0qttgbt4cy94uuvnacy6tkw435/" },
	{ { 13, 4 }, "/24/42/002-83793-A_74JRE8GVAT/rlnkct919wgc5c0pjq986z5bb9h62uvni2/" },
	{ { 14, 2 }, "/03/28/012-92431-A_FKICGWU4EK/eflw1v4c64sgmvux4ljc083cfjj663wu9g/" },
	{ { 14, 3 }, "/15/62/032-84673-A_7A1TG1RF8Z/xpc8q44ggn2pkn82iwr0fi1zeb9cxi8ath/" },
	{ { 15, 0 }, "/11/17/042-32697-A_1GOTVNQE4A/hy88qtn1sygbgxswlmbfdepd4pcg52xyrx/" },
	{ { 15, 3 }, "/14/48/052-59890-A_I0F5YGAY0Y/p9n40hio7892gou31o1v031ng6fnm9sb3c/" },
	{ { 16, 2 }, "/61/45/072-44426-A_16242I3TPF/74lkhd4yt26hcrwgpibrqtltf7yth9kc5w/" },
	{ { 16, 4 }, "/52/01/082-41241-A_0747ZN8FHV/dectd075r63pppkkzsb75qk61s0lfee22j/" },
	{ { 26, 5 }, "/09/08/047-91568-A_Y1CFZWQCD4/4xekpyz43i26dbp4enxfro8eb1q7wiujh5/" },
	{ { 26, 6 }, "/33/19/140-17812-A_21ZLMMLY4E/zu3xwktttpoe71qiawhzgzvqss6rovawsa/" }
};

static ReleaseInfo releases[] = {
	{ 11, "macOS Big Sur" 		},
	{ 12, "macOS Monterey" 		},
	{ 13, "macOS Ventura" 		},
	{ 14, "macOS Sonoma" 		},
	{ 15, "macOS Sequoia" 		},
	{ 26, "macOS Tahoe" 			},
	{ 27, "macOS Golden Gate"	},
	{ 0,  "macOS (Unknown)"		}
};

static const char *progresses[] = {
	[DOWNLOAD]	= "Downloading SDK",
	[EXTRACT]	= "Extracting SDK",
	[FIXUP]		= "Fixing up SDK",
	[CLEANUP]	= "Cleaning up SDK",
	[DONE]		= "SDK Installed"
};

static void sdk_progress(Progress progress, int percent)
{
	print_progress(progresses[progress], percent, verbose_level);
}

static bool check_license(bool accept_all)
{
	if (accept_all) return true;
	printf("Do you accept the license? https://www.apple.com/legal/sla/docs/xcode.pdf"
	       " (Y/n): ");
	fflush(stdout);
	switch (getchar())
	{
		case 'y': case 'Y': case '\n': return true;
		default: return false;
	}
}

static const char *get_release_name(const Sdk *sdk)
{
	const ReleaseInfo *iter = releases;
	while (iter->major)
	{
		if (iter->major == sdk->version.major)
		{
			return iter->name;
		}
		iter++;
	}
	return iter->name;
}

static void list_sdks(Sdk *sdks, size_t count)
{
	size_t longest = 0;

	for (size_t i = 0; i < count; i++)
	{
		const char *name = get_release_name(sdks + i);
		const size_t len = strlen(name);
		if (len > longest) longest = len;
	}

	for (size_t i = 0; i < count; i++)
	{
		Sdk *sdk = sdks + i;
		const char *name = get_release_name(sdk);

		printf("[%2zu] %*s - Version %d.%d\n", i + 1, (int)longest, name,
		       sdk->version.major, sdk->version.minor);
	}
	puts("");
}

static Version get_version(const char *pkm)
{
	/* Let's not flood Apple servers with requests */
#ifdef _WIN32
	Sleep(5000);
#else
	sleep(5);
#endif

	download_file(pkm, "", metadata_out, false);
	size_t size;
	const char *content = file_read_all(metadata_out, &size);
	if (!content) goto FAILED;

	content = strstr(content, "<pkg-info");
	if (!content) goto FAILED;
	char *version = strstr(content, " version=\"");
	if (!version) goto FAILED;

	version += 10;
	char *end = strchr(version, '"');
	if (!end) goto FAILED;
	*end = 0;

	const int major = (int) strtol(version, &version, 10);
	version++;
	const int minor = (int) strtol(version, &version, 10);

	return (Version) { major, minor };
FAILED:
	return (Version) { 0, 0 };
}

static Sdk get_sdk(const char *array_tag)
{
	Sdk sdk = {};

	char *base_pkm = strstr(array_tag, BASE_PKM);
	char *sdk_pkg = strstr(array_tag, SDK_PKG);
	if (!base_pkm || !sdk_pkg) return sdk;

	char *tags[] = {base_pkm, sdk_pkg};
	for (int i = 0; i < 2; i++)
	{
		char *end = strstr(tags[i], "</string>");
		if (!end) continue;
		*end = 0;
	}

	char **starts[] = {&base_pkm, &sdk_pkg};
	for (int i = 0; i < 2; i++)
	{
		char **start = starts[i];
		while ((*start)[-1] != '>') (*start)--;
	}

	sdk.version = get_version(base_pkm);
	if (!sdk.version.major && !sdk.version.minor)
	{
		error_exit("Unable to parse macOS SDK version");
	}

	char *base_start = sdk_pkg + sizeof(BASE_URL) - 1;
	char *end = strstr(base_start, SDK_PKG);
	if (end) *end = 0;

	sdk.sub_url = base_start;

	return sdk;
}

static Sdk *get_sdk_list(size_t *count)
{
	printf("Fetching dynamic SDK list takes time, go grab a drink.\n");
	download_file(LATEST_SUCATALOG, "", sucatalog_out, true);
	printf("Downloading version information...\n");

	Sdk *sdks = VECNEW(Sdk, 16);

	SuCatalog catalog;
	sucatalog_init(&catalog, sucatalog_out);

	const char *file_arr;
	while ((file_arr = sucatalog_next(&catalog)) != NULL)
	{
		if (!strstr(file_arr, "CLTools_macOS")) continue;

		const Sdk sdk = get_sdk(file_arr);
		if (!sdk.sub_url) error_exit("Failed to parse macOS SDK information");
		vec_add(sdks, sdk);
	}

	*count = vec_size(sdks);
	return sdks;
}

void fetch_macsdk(BuildOptions *options)
{
	if (!download_available())
	{
		error_exit("Failed to find MacOS SDK.\n"
				   "Alternatively, provide the SDK path manually using --macos-sdk.");
	}

	xz_crc32_init();
	xz_crc64_init();

	if (options->macos_list_sdks)
	{
		list_sdks(hardcoded, ELEMENTLEN(hardcoded));
		return;
	}

	verbose_level = options->verbosity_level;
	const char *tmp_dir_base = dir_make_temp_dir();

	if (!tmp_dir_base) error_exit("Failed to create temp directory");
	if (verbose_level >= 1) printf("Temp dir: %s\n", tmp_dir_base);

	sucatalog_out = file_append_path(tmp_dir_base, "sucatalog");
	metadata_out = file_append_path(tmp_dir_base, BASE_PKM);

	if (!check_license(options->fetch_accept_license))
	{
		exit_compiler(EXIT_FAILURE);
	}

	int64_t select = ELEMENTLEN(hardcoded) - 1;
	size_t sdk_count = ELEMENTLEN(hardcoded);
	Sdk *list = hardcoded;

	if (options->macos_fetch_sdk_list)
	{
		printf("Warning: using dynamic SDK list may end up installing beta version of the SDK\n");
		list = get_sdk_list(&sdk_count);
		list_sdks(list, sdk_count);
		select = (int64_t) sdk_count - 1;
	}

	if (options->macos_sdk_version_override != NULL)
	{
		select = -1;
		char *picked = str_dup(options->macos_sdk_version_override);
		const int major = (int) strtol(picked, &picked, 10);
		picked++;
		const int minor = (int) strtol(picked, &picked, 10);

		for (int i = 0; i < sdk_count; i++)
		{
			const Sdk *sdk_i = list + i;
			if (sdk_i->version.major == major && sdk_i->version.minor == minor)
			{
				select = i;
				break;
			}
		}

		if (select == -1)
		{
			error_exit("Selected MacOS version not found");
		}
	}

	const Sdk *sel = list + select;

	printf("Selected %s - %d.%d\n", get_release_name(sel),
		sel->version.major, sel->version.minor);

	/* Prepare dest and work dir */
	char *sdk_output = get_cache_output_path("MacOSX.sdk");
	dir_make_recursive(sdk_output);

	dir_change(sdk_output);
	dir_change("..");

	int progress = 0;
	sdk_progress(DOWNLOAD, progress);

	char *sdk_dest_path = NULL;

	for (int i = 0; i < 2; i++)
	{
		const char *files[] = {SDK_PKG, BASE_PKG};
		const char *dest = file_append_path(tmp_dir_base, files[i]);

		if (file_exists(dest)) goto done;

		const char *source = str_cat(BASE_URL, sel->sub_url);
		source = str_cat(source, files[i]);

		download_file(source, "", dest, false);
done:
		progress += PROGRESS_UPDATE;
		sdk_progress(EXTRACT, progress);

		FILE *pkg = file_open_read(dest);
		Cpio cpio;
		cpio_init(&cpio, "./Library/Developer/CommandLineTools");
		cpio.stage = (CpioStage) i;

		XarHeader header = xar_header(pkg);
		if (strncmp(header.signature, "xar!", 4) != 0)
		{
			error_exit("Expected xar! package signature.");
		}
		if (header.version > 1)
		{
			error_exit("Xar archive is newer version than expected 1.");
		}

		XarFile payload = xar_open(&header, "Payload");
		if (payload.file == NULL)
		{
			error_exit("Unable to find Payload file in Xar archive.");
		}

		if (!pbzx_extract(&payload, &cpio))
		{
			error_exit("Failed to extract pbzx.");
		}

		if (cpio.stage == SDK_INFO)
		{
			if (cpio.sdk == NULL) error_exit("Extraction layout changed");
			sdk_dest_path = str_dup(cpio.sdk);
		}

		cpio_free(&cpio);
		fclose(pkg);

		progress += PROGRESS_UPDATE;
	}
	sdk_progress(FIXUP, progress);

	/* target MacOSX.sdk will be moved there */
	file_delete_dir(sdk_output);

	char *sdk = str_printf("./SDKs/%s", sdk_dest_path);
	rename(sdk, "MacOSX.sdk");

	progress += PROGRESS_UPDATE;
	sdk_progress(CLEANUP, progress);
	file_delete_dir("SDKs");

	sdk_progress(DONE, 100);
	printf("\n");
}
