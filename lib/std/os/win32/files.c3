module std::os::win32::files;

$if (env::os_is_win32()):

enum Win32_GET_FILEEX_INFO_LEVELS
{
	STANDARD,
	MAX,
}

struct Win32_FILETIME
{
	Win32_DWORD dwLowDateTime;
	Win32_DWORD dwHighDateTime;
}

struct Win32_FILE_ATTRIBUTE_DATA
{
	Win32_DWORD    dwFileAttributes;
	Win32_FILETIME ftCreationTime;
	Win32_FILETIME ftLastAccessTime;
	Win32_FILETIME ftLastWriteTime;
	Win32_DWORD    nFileSizeHigh;
	Win32_DWORD    nFileSizeLow;
}

extern fn bool win32_GetFileAttributesExW(Win32_LPCWSTR, Win32_GET_FILEEX_INFO_LEVELS, Win32_LPVOID) @extern("GetFileAttributesExW");
extern fn bool win32_PathFileExistsW(Win32_LPCWSTR) @extern("PathFileExistsW");
extern fn Win32_DWORD win32_GetTempPathW(Win32_DWORD nBufferLength, Win32_LPWSTR lpBuffer) @extern("GetTempPathW");
extern fn bool win32_SetCurrentDirectoryW(Win32_LPCTSTR buffer) @extern("SetCurrentDirectoryW");

/*
extern ulong _win32_GetCurrentDirectoryW(ulong, Char16* buffer) @extern("GetCurrentDirectoryW");
extern bool _win32_CreateSymbolicLinkW(Char16* symlink_file, Char16* target_file, ulong flags) @extern("CreateSymbolicLinkW");
extern bool _win32_CreateDirectoryW(Char16* path_name, void* security_attributes) @extern("CreateDirectoryW");
extern bool _win32_DeleteFileW(Char16* file) @extern("DeleteFileW");
extern bool _win32_CopyFileW(Char16* from_file, Char16* to_file, bool no_overwrite) @extern("CopyFileW");
extern ulong _win32_GetFullPathNameW(Char16* file_name, ulong buffer_len, Char16* buffer, Char16** file_part) @extern("GetFullPathNameW");
*/
$endif;