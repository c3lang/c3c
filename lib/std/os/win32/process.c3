module std::os::win32::process;

$if (env::os_is_win32())

extern fn bool win32_CreateProcessW(
	Win32_LPCWSTR lpApplicationName,
	Win32_LPWSTR lpCommandLine,
	Win32_LPSECURITY_ATTRIBUTES lpProcessAttributes,
	Win32_LPSECURITY_ATTRIBUTES lpThreadAttributes,
	Win32_BOOL bInheritHandles,
	Win32_DWORD dwCreationFlags,
	Win32_LPVOID lpEnvironment,
	Win32_LPCWSTR lpCurrentDirectory,
	Win32_LPSTARTUPINFOW lpStartupInfo,
	Win32_LPPROCESS_INFORMATION lpProcessInformation
);

$endif