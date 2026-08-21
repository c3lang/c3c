#include "compiler_internal.h"
#include "utils/json.h"

const char *ios_sysroot(bool simulator)
{
#if __APPLE__
    static const char *xcode_device = "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk";
    static const char *xcode_simulator = "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk";
    if(simulator)
    {
      if(file_is_dir(xcode_simulator)) return xcode_simulator;
    }
    else
    {
      if(file_is_dir(xcode_device)) return xcode_device;
    }
#endif
    return NULL;
}

static void parse_version(const char *version_string, Version *version)
{
  StringSlice slice = slice_from_string(version_string);
  StringSlice first = slice_next_token(&slice, '.');
  version->major = atoi(first.ptr);
  version->minor = atoi(slice.ptr);
}

IosSDK *ios_sysroot_sdk_information(const char *sdk_path)
{
  JsonParser parser;
  size_t len;
  scratch_buffer_clear();
  scratch_buffer_printf("%s/SDKSettings.json", sdk_path);
  const char * settings_json_path = scratch_buffer_to_string();
  if(!file_exists(settings_json_path)) error_exit("Invalid iOS SDK path: '%s'.", sdk_path);
  const char *file = file_read_all(settings_json_path, &len);
  json_init_string(&parser, file);
  IosSDK *sdk = CALLOCS(IosSDK);
  JSONObject *top_object = json_parse(&parser);
  JSONObject *supported_targets = json_map_get(top_object, "SupportedTargets");
  JSONObject *ios_target = json_map_get(supported_targets, "iphoneos");
  if(!ios_target)
  {
    ios_target = json_map_get(supported_targets, "iphonesimulator");
  }
  if(!ios_target)
  {
    error_exit("Failed to locate target platform definition inside iOS SDKSettings.json");
  }
  const char *default_deploy_target = json_map_get(ios_target, "DefaultDeploymentTarget")->str;
  parse_version(default_deploy_target, &sdk->ios_deploy_target);
  const char *min_deploy_target = json_map_get(ios_target, "MinimumDeploymentTarget")->str;
  parse_version(min_deploy_target, &sdk->ios_min_deploy_target);
  return sdk;
}

const char *ios_cross_compile_library(bool simulator)
{
  const char *sdk_dirname = simulator ? "iPhoneSimulator.sdk" : "iPhoneOS.sdk";
  const char *local = find_rel_exe_dir((char*)sdk_dirname);
  if(local && file_is_dir((char*)local)) return local;
#if PLATFORM_WINDOWS
  char *app_data = getenv("LOCALAPPDATA");
  if(app_data)
  {
    scratch_buffer_clear();
    scratch_buffer_printf("%s/c3/%s", app_data, sdk_dirname);
    const char *path = scratch_buffer_to_string();
    if(file_is_dir(path)) return path;
  }
#else
  char *cache_home = getenv("XDG_CACHE_HOME");
  if(cache_home)
  {
    scratch_buffer_clear();
    scratch_buffer_printf("%s/c3/%s", cache_home, sdk_dirname);
    const char *path = scratch_buffer_to_string();
    if(file_is_dir(path)) return path;
  }
  char *home = getenv("HOME");
  if(home)
  {
    scratch_buffer_clear();
    scratch_buffer_printf("%s/.cache/c3/%s", home, sdk_dirname);
    const char *path = scratch_buffer_to_string();
    if(file_is_dir(path)) return path;
  }
#endif
  return NULL;
}