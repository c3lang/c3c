module std::os::win32 @feat(WIN32) @link("shell32");

extern fn Win32_BOOL dragQueryPoint(Win32_HDROP hDrop, Win32_LPOINT lppt) @cname("DragQueryPoint");
extern fn Win32_UINT dragQueryFileW(Win32_HDROP hDrop, Win32_UINT iFile, Win32_LPWSTR lpszFile, Win32_UINT cch) @cname("DragQueryFileW");
extern fn void dragFinish(Win32_HDROP hDrop) @cname("DragFinish");
