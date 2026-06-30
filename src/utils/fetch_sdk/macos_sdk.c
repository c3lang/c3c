#include <stdio.h>
#include <string.h>

#include "fetch_utils.h"
#include "../lib.h"
#include "../../compiler/compiler_internal.h"
#include "utils/xar.h"

#define BASE_URL "https://swcdn.apple.com/content/downloads"
#define BASE_PKG "CLTools_macOS_SDK.pkg"
#define SDK_PKG "CLTools_macOSNMOS_SDK.pkg"

typedef struct {
	Version version;
	char *suburl;
} Sdk;

typedef struct {
	int major;
	const char *name;
} ReleaseInfo;

typedef enum {
	DOWNLOAD,
	EXTRACT,
	COPY,
	CLEANUP
} Progress;

static int verbose_level = 0;

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
	{ 26, "macOS Tahoe" 		},
	{ 27, "macOS Golden Gate"	},
	{ 0,  "macOS (Unknown)"		}
};

static const char *progresses[] = {
	[DOWNLOAD]	= "Downloading SDK",
	[EXTRACT]	= "Extracting SDK",
	[COPY]		= "Copying SDK",
	[CLEANUP]	= "Cleaning up SDK"
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
	char c = (char) getchar();
	return (c == 'y' || c == 'Y' || c == '\n');
}

static size_t select_sdk(size_t count)
{
	char buffer[128];

	for (;;)
	{
		printf("Select sdk (%ld): ", count);
		fflush(stdout);

		fgets(buffer, 128, stdin);
		const char *trimmed = str_trim(buffer);
		if (strlen(trimmed) == 0)
		{
			return count;
		}

		const long num = strtol(buffer, NULL, 10);
		if (num >= 1 && num <= count)
		{
			return num;
		}

		printf("Selection is out of range 1-%ld!\n", count);
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

	for (size_t i = 0; i < count; i++) {
		const char *name = get_release_name(sdks + i);
		const size_t len = strlen(name);

		if (len > longest) longest = len;
	}

	for (size_t i = 0; i < count; i++) {
		Sdk *sdk = sdks + i;
		const char *name = get_release_name(sdk);

		printf("[%2zu] %*s - Version %d.%d\n", i + 1, (int) longest, name,
			sdk->version.major, sdk->version.minor);
	}
	puts("");
}

void fetch_macsdk(BuildOptions *options)
{
	if (!download_available())
	{
		error_exit("Failed to find MacOS SDK.\n"
				   "Alternatively, provide the SDK path manually using --macos-sdk.");
	}
	verbose_level = options->verbosity_level;
	const char *tmp_dir_base = dir_make_temp_dir();

	if (!tmp_dir_base) error_exit("Failed to create temp directory");
	if (verbose_level >= 1) printf("Temp dir: %s\n", tmp_dir_base);

	if (!check_license(options->fetch_accept_license))
	{
		exit_compiler(EXIT_FAILURE);
	}

	list_sdks(hardcoded, ELEMENTLEN(hardcoded));
	const size_t select = select_sdk(ELEMENTLEN(hardcoded));
	const Sdk *sel = hardcoded + (select - 1);

	printf("Selected %s - %d.%d\n", get_release_name(sel),
		sel->version.major, sel->version.minor);

	int progress = 0;
	sdk_progress(DOWNLOAD, progress);

	for (int i = 0; i < 2; i++)
	{
		const char *files[] = {SDK_PKG, BASE_PKG};
		const char *dest = file_append_path(tmp_dir_base, files[i]);

		const char *source = str_cat(BASE_URL, sel->suburl);
		source = str_cat(source, files[i]);

		download_file(source, "", dest, false);

		progress += 10;

		sdk_progress(DOWNLOAD, progress);

		FILE *pkg = file_open_read(dest);
		XarHeader header = xar_header(pkg);
		if (strncmp(header.signature, "xar!", 4) != 0)
		{
			error_exit("Expected xar! package signature");
		}
		if (header.version > 1)
		{
			error_exit("Xar archive is newer version than expected 1");
		}

		char *toc = xar_toc(&header, pkg);
		printf("\n%s\n", toc);

		fclose(pkg);
	}
}
