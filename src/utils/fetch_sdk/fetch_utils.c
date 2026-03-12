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
