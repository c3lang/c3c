#pragma once

typedef struct
{
    char *windows_sdk_um_library_path;
    char *windows_sdk_ucrt_library_path;
    char *vs_library_path;
} WindowsLinkPathsUTF8;

WindowsLinkPathsUTF8 get_windows_link_paths();
void free_windows_link_paths(WindowsLinkPathsUTF8 *obj);