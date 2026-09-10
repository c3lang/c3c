module std::os::win32 @feat(WIN32) @link("advapi32");

extern fn Win32_LSTATUS regGetValueW(Win32_HKEY hkey, Win32_LPCWSTR lpSubKey, Win32_LPCWSTR lpValue, Win32_DWORD dwFlags, Win32_LPDWORD pdwType, Win32_LPVOID pvData, Win32_LPDWORD pcbData) @cname("RegGetValueW");