module std::os::win32::files;

$if (env::OS_TYPE == OsType.WIN32):
/*
private extern ulong _win32_GetCurrentDirectoryW(ulong, Char16* buffer) @extern("GetCurrentDirectoryW");
private extern bool _win32_CreateSymbolicLinkW(Char16* symlink_file, Char16* target_file, ulong flags) @extern("CreateSymbolicLinkW");
private extern bool _win32_CreateDirectoryW(Char16* path_name, void* security_attributes) @extern("CreateDirectoryW");
private extern bool _win32_DeleteFileW(Char16* file) @extern("DeleteFileW");
private extern bool _win32_CopyFileW(Char16* from_file, Char16* to_file, bool no_overwrite) @extern("CopyFileW");
private extern ulong _win32_GetFullPathNameW(Char16* file_name, ulong buffer_len, Char16* buffer, Char16** file_part) @extern("GetFullPathNameW");
*/
$endif;