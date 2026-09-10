module std::os::win32 @feat(WIN32) @link("kernel32");

extern fn Win32_HGLOBAL globalAlloc(Win32_UINT uFlags, Win32_SIZE_T dwBytes) @cname("GlobalAlloc");
extern fn Win32_LPVOID globalLock(Win32_HGLOBAL hMem) @cname("GlobalLock");
extern fn Win32_BOOL globalUnlock(Win32_HGLOBAL hMem) @cname("GlobalUnlock");
extern fn Win32_HGLOBAL globalFree(Win32_HGLOBAL hMem) @cname("GlobalFree");