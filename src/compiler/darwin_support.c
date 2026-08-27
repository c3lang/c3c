#include "compiler_internal.h"
#include "utils/json.h"

const char *darwin_sysroot(void)
{
#if __APPLE__
		static const char *xcode_sysroot;
		if (compiler.platform.os == OS_TYPE_IOS)
		{
			if (compiler.build.ios.simulator)
			{
				xcode_sysroot = "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk";
			}
			else
			{
				xcode_sysroot =  "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk";
			}
		}
		else
		{
			xcode_sysroot = "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk";
		}
		static const char *commandline_tool_sysroot = "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk";
		if(file_is_dir(xcode_sysroot)) return xcode_sysroot;
		if(file_is_dir(commandline_tool_sysroot) && !(compiler.platform.os == OS_TYPE_IOS)) return commandline_tool_sysroot;
#endif
	return NULL;
}

void parse_version(const char *version_string, Version *version)
{
	StringSlice slice = slice_from_string(version_string);
	StringSlice first = slice_next_token(&slice, '.');
	version->major = atoi(first.ptr);
	version->minor = atoi(slice.ptr);
}

DarwinSDK *darwin_sysroot_sdk_information(const char *sdk_path)
{
	JsonParser parser;
	size_t len;
	
	scratch_buffer_clear();
	scratch_buffer_printf("%s/SDKSettings.json", sdk_path);
	const char *settings_json_path = scratch_buffer_to_string();
	if(!file_exists(settings_json_path)) error_exit("Invalid %s SDK path: '%s'.", compiler.platform.os == OS_TYPE_IOS ? "iOS" : "MacOS", sdk_path);
	const char *file = file_read_all(settings_json_path, &len);
	json_init_string(&parser, file);
	DarwinSDK *sdk = CALLOCS(DarwinSDK);
	JSONObject *top_object = json_parse(&parser);
	JSONObject *supported_targets = json_map_get(top_object, "SupportedTargets");
	JSONObject *darwin_target = json_map_get(supported_targets, compiler.build.ios.simulator ? "iphonesimulator" : compiler.platform.os == OS_TYPE_IOS ? "iphoneos" : "macosx");

	const char *default_deploy_target = json_map_get(darwin_target, "DefaultDeploymentTarget")->str;
	parse_version(default_deploy_target, compiler.platform.os == OS_TYPE_IOS ? &sdk->iossdk.ios_deploy_target : &sdk->macossdk.macos_deploy_target);

	const char *min_deploy_target = json_map_get(darwin_target, "MinimumDeploymentTarget")->str;
	parse_version(min_deploy_target, compiler.platform.os == OS_TYPE_IOS ? &sdk->iossdk.ios_deploy_target : &sdk->macossdk.macos_deploy_target);

	return sdk;
}

const char *darwin_cross_compile_library(void)
{
	const char *sdk = compiler.build.ios.simulator ? "iPhoneSimulator.sdk" : compiler.platform.os == OS_TYPE_IOS ? "iPhoneOS.sdk" : "MacOSX.sdk";
	const char *local = find_rel_exe_dir(sdk);
	if (local && file_is_dir((char *)local)) return local;

#if PLATFORM_WINDOWS
	char *app_data = getenv("LOCALAPPDATA");
	if (app_data)
	{
		scratch_buffer_clear();
		scratch_buffer_printf("%s/c3/%s", app_data, sdk);
		const char *path = scratch_buffer_to_string();
		if (file_is_dir(path)) return path;
	}
#else
	char *cache_home = getenv("XDG_CACHE_HOME");
	if (cache_home)
	{
		scratch_buffer_clear();
		scratch_buffer_printf("%s/c3/%s", cache_home, sdk);
		const char *path = scratch_buffer_to_string();
		if (file_is_dir(path)) return path;
	}

	char *home = getenv("HOME");
	if (home)
	{
		scratch_buffer_clear();
		scratch_buffer_printf("%s/.cache/c3/%s", home, sdk);
		const char *path = scratch_buffer_to_string();
		if (file_is_dir(path)) return path;
	}
#endif
	return NULL;
}