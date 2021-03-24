#include "compiler_internal.h"

#if PLATFORM_WINDOWS
static void link_exe(const char *output_file, const char **files_to_link, unsigned file_count)
{
	TODO
}
#else
static void link_exe(const char *output_file, const char **files_to_link, unsigned file_count)
{
	char *result = NULL;
	asprintf(&result, "cc -o %s ", output_file);

	for (unsigned i = 0; i < file_count; i++)
	{
		char *new_res = NULL;
		asprintf(&new_res, "%s %s", result, files_to_link[i]);
		free(result);
		result = new_res;
	}
	if (system(result) != EXIT_SUCCESS)
	{
		error_exit("Failed to create an executable.");
	}
}
#endif

void linker(const char *output_file, const char **files, unsigned file_count)
{
	link_exe(output_file, files, file_count);
}
