module std::os::win32 @feat(WIN32);
import libc;

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
	Win32_WCHAR[MAX_PATH] cFileName;
	Win32_WCHAR[14]     cAlternateFileName;
}

alias Win32_LPWIN32_FIND_DATAW = Win32_WIN32_FIND_DATAW*;

constset Win32_FILE_INFO_BY_HANDLE_CLASS : CInt
{
	FILE_BASIC_INFO = 0,
}

struct Win32_FILE_BASIC_INFO
{
	Win32_LARGE_INTEGER creationTime;
	Win32_LARGE_INTEGER lastAccessTime;
	Win32_LARGE_INTEGER lastWriteTime;
	Win32_LARGE_INTEGER changeTime;
	Win32_DWORD         fileAttributes;
}

extern fn Win32_BOOL closeHandle(Win32_HANDLE) @cname("CloseHandle");
extern fn Win32_BOOL createPipe(Win32_PHANDLE hReadPipe, Win32_PHANDLE hWritePipe, Win32_LPSECURITY_ATTRIBUTES lpPipeAttributes, Win32_DWORD nSize) @cname("CreatePipe");
extern fn Win32_BOOL getFileAttributesExW(Win32_LPCWSTR, Win32_GET_FILEEX_INFO_LEVELS, Win32_LPVOID) @cname("GetFileAttributesExW");
extern fn Win32_DWORD getFileAttributesW(Win32_LPCWSTR lpFileName) @cname("GetFileAttributesW");
extern fn Win32_BOOL setFileAttributesW(Win32_LPCWSTR lpFileName, Win32_DWORD dwFileAttributes) @cname("SetFileAttributesW");
extern fn Win32_BOOL getFileInformationByHandleEx(Win32_HANDLE hFile, Win32_FILE_INFO_BY_HANDLE_CLASS fileInformationClass, Win32_LPVOID lpFileInformation, Win32_DWORD dwBufferSize) @cname("GetFileInformationByHandleEx");
extern fn Win32_BOOL setFileInformationByHandle(Win32_HANDLE hFile, Win32_FILE_INFO_BY_HANDLE_CLASS fileInformationClass, Win32_LPVOID lpFileInformation, Win32_DWORD dwBufferSize) @cname("SetFileInformationByHandle");
extern fn Win32_BOOL pathFileExistsW(Win32_LPCWSTR) @cname("PathFileExistsW");
extern fn Win32_DWORD getTempPathW(Win32_DWORD nBufferLength, Win32_LPWSTR lpBuffer) @cname("GetTempPathW");
extern fn Win32_BOOL setCurrentDirectoryW(Win32_LPCTSTR buffer) @cname("SetCurrentDirectoryW");
extern fn Win32_BOOL removeDirectoryW(Win32_LPCWSTR lpPathName) @cname("RemoveDirectoryW");
extern fn Win32_BOOL createDirectoryW(Win32_LPCWSTR lpPathName, Win32_LPSECURITY_ATTRIBUTES lpPipeAttributes) @cname("CreateDirectoryW");
extern fn Win32_BOOL deleteFileW(Win32_LPCWSTR lpFileName) @cname("DeleteFileW");
extern fn Win32_HANDLE findFirstFileW(Win32_LPCWSTR lpFileName, Win32_LPWIN32_FIND_DATAW lpFindFileData) @cname("FindFirstFileW");
extern fn Win32_BOOL findNextFileW(Win32_HANDLE hFindFile, Win32_LPWIN32_FIND_DATAW lpFindFileData) @cname("FindNextFileW");
extern fn Win32_BOOL findClose(Win32_HANDLE hFindFile) @cname("FindClose");

const Win32_DWORD GENERIC_WRITE = 0x40000000;
const Win32_DWORD OPEN_EXISTING = 3;
const Win32_DWORD INVALID_FILE_ATTRIBUTES = 0xFFFFFFFF;
const Win32_DWORD FILE_ATTRIBUTE_READONLY = 0x01;
const Win32_DWORD FILE_ATTRIBUTE_HIDDEN = 0x02;
const Win32_DWORD FILE_ATTRIBUTE_SYSTEM = 0x04;
const Win32_DWORD FILE_ATTRIBUTE_DIRECTORY = 0x10;
const Win32_DWORD FILE_ATTRIBUTE_ARCHIVE = 0x20;
const Win32_DWORD FILE_ATTRIBUTE_DEVICE = 0x40;
const Win32_DWORD FILE_ATTRIBUTE_NORMAL = 0x80;
const Win32_DWORD FILE_ATTRIBUTE_TEMPORARY = 0x100;
const Win32_DWORD FILE_ATTRIBUTE_SPARSE_FILE = 0x200;
const Win32_DWORD FILE_ATTRIBUTE_REPARSE_POINT = 0x400;
const Win32_DWORD FILE_ATTRIBUTE_COMPRESSED = 0x800;
const Win32_DWORD FILE_ATTRIBUTE_OFFLINE = 0x1000;
const Win32_DWORD FILE_ATTRIBUTE_NOT_CONTENT_INDEXED = 0x2000;
const Win32_DWORD FILE_ATTRIBUTE_ENCRYPTED = 0x4000;
const Win32_DWORD FILE_ATTRIBUTE_INTEGRITY_STREAM = 0x8000;
const Win32_DWORD FILE_ATTRIBUTE_VIRTUAL = 0x10000;
const Win32_DWORD FILE_ATTRIBUTE_NO_SCRUB_DATA = 0x20000;
const Win32_DWORD FILE_ATTRIBUTE_EA = 0x40000;
const Win32_DWORD FILE_ATTRIBUTE_PINNED = 0x80000;
const Win32_DWORD FILE_ATTRIBUTE_UNPINNED = 0x100000;
const Win32_DWORD FILE_ATTRIBUTE_RECALL_ON_OPEN = 0x40000;
const Win32_DWORD FILE_ATTRIBUTE_RECALL_ON_DATA_ACCESS = 0x400000;

extern fn Win32_HANDLE createFileA(
	Win32_LPCSTR lpFileName,
	Win32_DWORD dwDesiredAccess,
	Win32_DWORD dwShareMode,
	Win32_LPSECURITY_ATTRIBUTES lpSecurityAttributes,
	Win32_DWORD dwCreationDisposition,
	Win32_DWORD dwFlagsAndAttributes,
	Win32_HANDLE hTemplateFile
) @cname("CreateFileA");
extern fn Win32_HANDLE createFileW(
	Win32_LPCWSTR lpFileName,
	Win32_DWORD dwDesiredAccess,
	Win32_DWORD dwShareMode,
	Win32_LPSECURITY_ATTRIBUTES lpSecurityAttributes,
	Win32_DWORD dwCreationDisposition,
	Win32_DWORD dwFlagsAndAttributes,
	Win32_HANDLE hTemplateFile
) @cname("CreateFileW");
extern fn Win32_BOOL deviceIoControl(
	Win32_HANDLE hDevice,
	Win32_DWORD dwIoControlCode,
	Win32_LPVOID lpInBuffer,
	Win32_DWORD nInBufferSize,
	Win32_LPVOID lpOutBuffer,
	Win32_DWORD nOutBufferSize,
	Win32_LPDWORD lpBytesReturned,
	Win32_LPOVERLAPPED lpOverlapped
) @cname("DeviceIoControl");
extern fn Win32_BOOLEAN createSymbolicLinkW(Win32_LPCWSTR lpSymlinkFileName, Win32_LPCWSTR lpTargetFileName, Win32_DWORD dwFlags) @cname("CreateSymbolicLinkW");

const Win32_DWORD FILE_SHARE_READ = 0x1;
const Win32_DWORD FILE_SHARE_WRITE = 0x2;
const Win32_DWORD FILE_SHARE_DELETE = 0x4;
const Win32_DWORD FILE_FLAG_OPEN_REPARSE_POINT = 0x00200000;
const Win32_DWORD FILE_FLAG_BACKUP_SEMANTICS = 0x02000000;
const Win32_DWORD FSCTL_GET_REPARSE_POINT = 0x000900A8;
const Win32_DWORD IO_REPARSE_TAG_SYMLINK = 0xA000000C;
const Win32_DWORD SYMBOLIC_LINK_FLAG_DIRECTORY = 0x1;
const Win32_DWORD SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE = 0x2;
const MAXIMUM_REPARSE_DATA_BUFFER_SIZE = 16 * 1024;

struct Win32_REPARSE_SYMLINK_BUFFER
{
	Win32_DWORD reparseTag;
	Win32_WORD  reparseDataLength;
	Win32_WORD  reserved;
	Win32_WORD  substituteNameOffset;
	Win32_WORD  substituteNameLength;
	Win32_WORD  printNameOffset;
	Win32_WORD  printNameLength;
	Win32_DWORD flags;
	Win32_WCHAR[*] pathBuffer;
}

extern fn Win32_BOOL readFile(Win32_HANDLE hFile, Win32_LPVOID lpBuffer, Win32_DWORD nNumberOfBytesToRead,
  Win32_LPDWORD lpNumberOfBytesRead, Win32_LPOVERLAPPED lpOverlapped
) @cname("ReadFile");

extern fn WString _wgetcwd(Char16* buffer, int maxlen);
extern fn usz wcslen(WString str);

extern fn int _open_osfhandle(iptr osfhandle, int flags);
extern fn iptr _get_osfhandle(int fd);
extern fn CFile _fdopen(int fd, ZString mode);
extern fn CInt _access(ZString path, CInt mode);
extern fn CInt _waccess(WString path, CInt mode);

extern fn WString _wfullpath(WString absPath, WString relPath, usz maxLength);

extern fn int _setmode(int fd, int mode);

<*
 Copies data from a named or anonymous pipe into a buffer without removing it from the pipe.
 It also returns information about data in the pipe.
*>
extern fn Win32_BOOL peekNamedPipe(
	Win32_HANDLE  hNamedPipe,
	Win32_LPVOID  lpBuffer,
	Win32_DWORD   nBufferSize,
	Win32_LPDWORD lpBytesRead = null,
	Win32_LPDWORD lpTotalBytesAvail = null,
	Win32_LPDWORD lpBytesLeftThisMessage = null
) @cname("PeekNamedPipe");

/*
extern ulong _win32_GetCurrentDirectoryW(ulong, Char16* buffer) @cname("GetCurrentDirectoryW");
extern bool _win32_CopyFileW(WString from_file, WString to_file, bool no_overwrite) @cname("CopyFileW");
extern ulong _win32_GetFullPathNameW(WString file_name, ulong buffer_len, Char16* buffer, WString* file_part) @cname("GetFullPathNameW");
*/
