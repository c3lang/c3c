module std::os::win32 @if(env::WIN32);

constdef Win32_AllocationType
{
	MEM_COMMIT      = 0x00001000,
	MEM_RESERVE     = 0x00002000,
    MEM_RESET       = 0x00080000,
    MEM_RESET_UNDO  = 0x01000000,
    MEM_LARGE_PAGES = 0x20000000,
    MEM_PHYSICAL    = 0x00400000,
    MEM_TOP_DOWN    = 0x00100000,
    MEM_WRITE_WATCH = 0x00200000
}

constdef Win32_Protect : Win32_DWORD
{
	PAGE_EXECUTE           = 0x10,
	PAGE_EXECUTE_READ      = 0x20,
	PAGE_EXECUTE_READWRITE = 0x40,
	PAGE_EXECUTE_WRITECOPY = 0x80,
	PAGE_NOACCESS          = 0x01,
	PAGE_READONLY          = 0x02,
	PAGE_READWRITE         = 0x04,
	PAGE_WRITECOPY         = 0x08,
	PAGE_TARGETS_INVALID   = 0x40000000,
	PAGE_TARGETS_NO_UPDATE = 0x40000000,
	PAGE_GUARD             = 0x100,
	PAGE_NOCACHE           = 0x200,
	PAGE_WRITECOMBINE      = 0x400,
}

constdef Win32_FreeType : Win32_DWORD
{
	MEM_DECOMMIT              = 0x00004000,
	MEM_RELEASE               = 0x00008000,
	MEM_COALESCE_PLACEHOLDERS = 0x00000001,
	MEM_PRESERVE_PLACEHOLDER  = 0x00000002,
}

constdef Win32_FileMapAccess : Win32_DWORD
{
	FILE_MAP_COPY    = 0x0001,
	FILE_MAP_WRITE   = 0x0002,
	FILE_MAP_READ    = 0x0004,
	FILE_MAP_EXECUTE = 0x0008,
}

extern fn Win32_LPVOID virtualAlloc(Win32_LPVOID lpAddress, Win32_SIZE_T dwSize, Win32_AllocationType flAllocationType, Win32_Protect flProtect) @cname("VirtualAlloc");
extern fn Win32_PVOID virtualAlloc2(Win32_HANDLE process, Win32_PVOID baseAddress, Win32_SIZE_T size, Win32_AllocationType allocationType, Win32_ULONG pageProtection, Win32_MEM_EXTENDED_PARAMETER* extendedParameters, Win32_ULONG parameterCount) @cname("VirtualAlloc2");
extern fn Win32_BOOL virtualFree(Win32_LPVOID lpAddress, Win32_SIZE_T dwSize, Win32_FreeType dwFreeType) @cname("VirtualFree");
extern fn Win32_BOOL virtualProtect(Win32_LPVOID lpAddress, Win32_SIZE_T dwSize, Win32_Protect flNewProtect, Win32_Protect* lpflOldProtect) @cname("VirtualProtect");
extern fn Win32_HANDLE createFileMappingW(Win32_HANDLE hFile, void* lpFileMappingAttributes, Win32_Protect flProtect, Win32_DWORD dwMaximumSizeHigh, Win32_DWORD dwMaximumSizeLow, Win32_LPCWSTR lpName) @cname("CreateFileMappingW");
extern fn Win32_LPVOID mapViewOfFile(Win32_HANDLE hFileMappingObject, Win32_FileMapAccess dwDesiredAccess, Win32_DWORD dwFileOffsetHigh, Win32_DWORD dwFileOffsetLow, Win32_SIZE_T dwNumberOfBytesToMap) @cname("MapViewOfFile");
extern fn Win32_BOOL unmapViewOfFile(void* lpBaseAddress) @cname("UnmapViewOfFile");

fn usz allocation_granularity()
{
    static usz granularity;
	if (granularity) return granularity;

	Win32_SYSTEM_INFO info;
	win32::getSystemInfo(&info);
	return granularity = (usz)info.dwAllocationGranularity;
}
