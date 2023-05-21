module std::os::win32;

$if env::os_is_win32():

enum Win32_GET_FILEEX_INFO_LEVELS
{
	STANDARD,
	MAX,
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


const MAX_PATH = 260;

struct Win32_WIN32_FIND_DATAW
{
	Win32_DWORD         dwFileAttributes;
	Win32_FILETIME      ftCreationTime;
	Win32_FILETIME      ftLastAccessTime;
	Win32_FILETIME      ftLastWriteTime;
	Win32_DWORD         nFileSizeHigh;
	Win32_DWORD         nFileSizeLow;
	Win32_DWORD         dwReserved0;
	Win32_DWORD         dwReserved1;
	Win32_WCHAR[260]    cFileName;
	Win32_WCHAR[14]     cAlternateFileName;
	Win32_DWORD         dwFileType; // Obsolete. Do not use.
	Win32_DWORD         dwCreatorType; // Obsolete. Do not use
	Win32_WORD          wFinderFlags; // Obsolete. Do not use
}

def Win32_LPWIN32_FIND_DATAW = Win32_WIN32_FIND_DATAW*;

extern fn Win32_BOOL win32_CloseHandle(Win32_HANDLE) @extern("CloseHandle");
extern fn Win32_BOOL win32_CreatePipe(Win32_PHANDLE hReadPipe, Win32_PHANDLE hWritePipe, Win32_LPSECURITY_ATTRIBUTES lpPipeAttributes, Win32_DWORD nSize) @extern("CreatePipe");
extern fn Win32_BOOL win32_GetFileAttributesExW(Win32_LPCWSTR, Win32_GET_FILEEX_INFO_LEVELS, Win32_LPVOID) @extern("GetFileAttributesExW");
extern fn Win32_BOOL win32_PathFileExistsW(Win32_LPCWSTR) @extern("PathFileExistsW");
extern fn Win32_DWORD win32_GetTempPathW(Win32_DWORD nBufferLength, Win32_LPWSTR lpBuffer) @extern("GetTempPathW");
extern fn Win32_BOOL win32_SetCurrentDirectoryW(Win32_LPCTSTR buffer) @extern("SetCurrentDirectoryW");
extern fn Win32_BOOL win32_RemoveDirectoryW(Win32_LPCWSTR lpPathName) @extern("RemoveDirectoryW");
extern fn Win32_BOOL win32_CreateDirectoryW(Win32_LPCWSTR lpPathName, Win32_LPSECURITY_ATTRIBUTES lpPipeAttributes) @extern("CreateDirectoryW");
extern fn Win32_BOOL win32_DeleteFileW(Win32_LPCWSTR lpFileName) @extern("DeleteFileW");
extern fn Win32_HANDLE win32_FindFirstFileW(Win32_LPCWSTR lpFileName, Win32_LPWIN32_FIND_DATAW lpFindFileData) @extern("FindFirstFileW");
extern fn Win32_BOOL win32_FindNextFileW(Win32_HANDLE hFindFile, Win32_LPWIN32_FIND_DATAW lpFindFileData) @extern("FindNextFileW");
extern fn Win32_BOOL win32_FindClose(Win32_HANDLE hFindFile) @extern("FindClose");

extern fn Char16* _wgetcwd(Char16* buffer, int maxlen);
extern fn usz wcslen(Char16* str);

/*
extern ulong _win32_GetCurrentDirectoryW(ulong, Char16* buffer) @extern("GetCurrentDirectoryW");
extern bool _win32_CreateSymbolicLinkW(Char16* symlink_file, Char16* target_file, ulong flags) @extern("CreateSymbolicLinkW");
extern bool _win32_CopyFileW(Char16* from_file, Char16* to_file, bool no_overwrite) @extern("CopyFileW");
extern ulong _win32_GetFullPathNameW(Char16* file_name, ulong buffer_len, Char16* buffer, Char16** file_part) @extern("GetFullPathNameW");
*/
$endif