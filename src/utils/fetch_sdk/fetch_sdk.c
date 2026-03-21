#include "../../compiler/compiler_internal.h"

void fetch_sdk(BuildOptions *options)
{
	const char *target = options->fetch_sdk_target;

	if (!target)
	{
		error_exit("fetch-sdk requires a target!");
	}

	if (str_eq(target, "windows") || str_eq(target, "win") || str_eq(target, "msvc"))
	{
		fetch_winsdk(options);
	}
	else if (str_eq(target, "android") || str_eq(target, "ndk"))
	{
		fetch_android_ndk(options);
	}
	else if (str_eq(target, "macos") || str_eq(target, "mac") || str_eq(target, "apple"))
	{
		error_exit("macos SDK fetching is not yet implemented.");
	}
	else
	{
		error_exit("Unknown fetch-sdk target '%s'. Available targets: windows, macos, android.", target);
	}
}
