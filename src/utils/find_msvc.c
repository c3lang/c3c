#include "utils/common.h"

#ifdef _MSC_VER

#include "find_msvc.h"

// in microsoft_craziness.h
// #include <windows.h>

#define MICROSOFT_CRAZINESS_IMPLEMENTATION
#include "microsoft_craziness.h"

WindowsLinkPathsUTF8 get_windows_link_paths() {
  Find_Result paths = find_visual_studio_and_windows_sdk();

  WindowsLinkPathsUTF8 out = { 0 };

  // note: WideCharToMultiByte doesn't seem to do null termination.
  // I'm wary of manually adding a null terminator, so hopefully this is reliable.
  // This wouldn't be a problem if windows used UTF-8 like the rest of the world >:(
  out.windows_sdk_um_library_path = (char*)calloc(MAX_PATH, 1);
  out.windows_sdk_ucrt_library_path = (char*)calloc(MAX_PATH, 1);
  out.vs_library_path = (char*)calloc(MAX_PATH, 1);

  int um_len = wcslen(paths.windows_sdk_um_library_path);
  WideCharToMultiByte(
    CP_UTF8,                          // UTF-8
    0,                                // config flags- I don't think we need these
    paths.windows_sdk_um_library_path,// in
    um_len,                           // in len
    out.windows_sdk_um_library_path,  // out
    MAX_PATH,                         // out len
    NULL,
    NULL
  );
  int ucrt_len = wcslen(paths.windows_sdk_ucrt_library_path);
  WideCharToMultiByte(
    CP_UTF8,
    0,
    paths.windows_sdk_ucrt_library_path,
    ucrt_len,
    out.windows_sdk_ucrt_library_path,
    MAX_PATH,
    NULL,
    NULL
  );
  int vsl_len = wcslen(paths.vs_library_path);
  WideCharToMultiByte(
    CP_UTF8,
    0,
    paths.vs_library_path,
    vsl_len,
    out.vs_library_path,
    MAX_PATH,
    NULL,
    NULL
  );

  free_resources(&paths);
  return out;
}

void free_windows_link_paths(WindowsLinkPathsUTF8* obj) {
  free(obj->vs_library_path);
  free(obj->windows_sdk_ucrt_library_path);
  free(obj->windows_sdk_um_library_path);
}

#endif //defined(_MSC_VER)