#include "compiler_internal.h"
#include "utils/json.h"

const char *macos_sysroot(void)
{
#if __APPLE__
		static const char *xcode_sysroot = "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk";
		static const char *commandline_tool_sysroot = "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk";
		if (file_is_dir(xcode_sysroot)) return xcode_sysroot;
		if (file_is_dir(commandline_tool_sysroot)) return commandline_tool_sysroot;
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

MacSDK *macos_sysroot_sdk_information(const char *sdk_path)
{
	JsonParser parser;
	size_t len;

	scratch_buffer_clear();
	scratch_buffer_printf("%s/SDKSettings.json", sdk_path);
	const char *settings_json_path = scratch_buffer_to_string();
	if (!file_exists(settings_json_path)) error_exit("Invalid MacOS SDK path: '%s'.", sdk_path);
	const char *file = file_read_all(settings_json_path, &len);
	json_init_string(&parser, file);
	MacSDK *sdk = CALLOCS(MacSDK);
	JSONObject *top_object = json_parse(&parser);
	JSONObject *supported_targets = json_map_get(top_object, "SupportedTargets");
	JSONObject *macosx_target = json_map_get(supported_targets, "macosx");

	const char *default_deploy_target = json_map_get(macosx_target, "DefaultDeploymentTarget")->str;
	parse_version(default_deploy_target, &sdk->macos_deploy_target);

	const char *min_deploy_target = json_map_get(macosx_target, "MinimumDeploymentTarget")->str;
	parse_version(min_deploy_target, &sdk->macos_min_deploy_target);

	return sdk;
}