#include "fetch_utils.h"
#include "../../compiler/compiler_internal.h"
#include "../whereami.h"

char *get_cache_output_path(const char *subdir)
{
	char *env_path = NULL;
#if PLATFORM_WINDOWS
	env_path = getenv("LOCALAPPDATA");
#else
	env_path = getenv("XDG_CACHE_HOME");
#endif

	if (env_path)
	{
		return file_append_path(env_path, str_printf("c3/%s", subdir));
	}

#if !PLATFORM_WINDOWS
	char *home = getenv("HOME");
	if (home) return file_append_path(home, str_printf(".cache/c3/%s", subdir));
#endif

	return file_append_path(find_executable_path(), subdir);
}

void print_progress(const char *label, int percent, int verbose_output)
{
	if (verbose_output > 0) return;
	if (percent > 100) percent = 100;

	int width = 40;
	printf("\r%s [", label);

	const char *parts[] = { " ", "▏", "▎", "▍", "▌", "▋", "▊", "▉" };
	int total_blocks = width * 8;
	int filled_blocks = (percent * total_blocks) / 100;
	int full_blocks = filled_blocks / 8;
	int partial_block = filled_blocks % 8;

	for (int i = 0; i < full_blocks; i++) printf("█");
	if (full_blocks < width)
	{
		printf("%s", parts[partial_block]);
		for (int i = full_blocks + 1; i < width; i++) printf(" ");
	}

	printf("] %3d%%", percent);
	fflush(stdout);
}
