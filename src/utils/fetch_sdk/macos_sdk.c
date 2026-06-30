#include <stdio.h>

#include "fetch_utils.h"
#include "../../compiler/compiler_internal.h"

static int verbose_level = 0;

static bool check_license(bool accept_all)
{
	if (accept_all) return true;
	printf("Do you accept the license? https://www.apple.com/legal/sla/docs/xcode.pdf"
	       " (Y/n): ");
	fflush(stdout);
	char c = (char) getchar();
	return (c == 'y' || c == 'Y' || c == '\n');
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
}
