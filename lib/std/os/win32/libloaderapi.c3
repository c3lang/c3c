module std::os::win32 @if(env::WIN32);

typedef Win32_DLL_DIRECTORY_COOKIE = void*;
alias Win32_PDLL_DIRECTORY_COOKIE = Win32_DLL_DIRECTORY_COOKIE*;

extern fn Win32_HMODULE loadLibraryA(Win32_LPCSTR lpLibFileName) @extern("LoadLibraryA");
extern fn Win32_HMODULE loadLibraryW(Win32_LPCWSTR lpLibFileName) @extern("LoadLibraryW");
extern fn Win32_HMODULE loadLibraryExA(Win32_LPCSTR lpLibFileName, Win32_HANDLE hFile, Win32_DWORD dwFlags) @extern("LoadLibraryExA");
extern fn Win32_HMODULE loadLibraryExW(Win32_LPCWSTR lpLibFileName, Win32_HANDLE hFile, Win32_DWORD dwFlags) @extern("LoadLibraryExW");

extern fn Win32_BOOL freeLibrary(Win32_HMODULE hLibModule) @extern("FreeLibrary");
extern fn void freeLibraryAndExitThread(Win32_HMODULE hLibModule, Win32_DWORD dwExitCode) @extern("FreeLibraryAndExitThread");

extern fn Win32_DWORD getModuleFileNameA(Win32_HMODULE hModule, Win32_LPSTR lpFilename, Win32_DWORD nSize) @extern("GetModuleFileNameA");
extern fn Win32_DWORD getModuleFileNameW(Win32_HMODULE hModule, Win32_LPWSTR lpFilename, Win32_DWORD nSize) @extern("GetModuleFileNameW");

extern fn Win32_HMODULE getModuleHandleA(Win32_LPCSTR lpModuleName) @extern("GetModuleHandleA");
extern fn Win32_HMODULE getModuleHandleW(Win32_LPCWSTR lpModuleName) @extern("GetModuleHandleW");
extern fn Win32_BOOL getModuleHandleExA(Win32_DWORD dwFlags, Win32_LPCSTR lpModuleName, Win32_HMODULE* phModule) @extern("GetModuleHandleExA");
extern fn Win32_BOOL getModuleHandleExW(Win32_DWORD dwFlags, Win32_LPCWSTR lpModuleName, Win32_HMODULE* phModule) @extern("GetModuleHandleExW");

extern fn Win32_BOOL disableThreadLibraryCalls(Win32_HMODULE hLibModule) @extern("DisableThreadLibraryCalls");

extern fn Win32_FARPROC getProcAddress(Win32_HMODULE hModule, Win32_LPCSTR lpProcName) @extern("GetProcAddress");
extern fn Win32_DLL_DIRECTORY_COOKIE addDllDirectory(Win32_PCWSTR newDirectory) @extern("AddDllDirectory");
extern fn Win32_BOOL removeDllDirectory(Win32_DLL_DIRECTORY_COOKIE cookie) @extern("RemoveDllDirectory");
extern fn Win32_BOOL setDefaultDllDirectories(Win32_DWORD directoryFlags) @extern("SetDefaultDllDirectories");