module std::os::win32 @if(env::WIN32);

extern fn Win32_HBRUSH createSolidBrush(Win32_COLORREF) @cname("CreateSolidBrush");
extern fn Win32_COLORREF setTextColor(Win32_HDC, Win32_COLORREF) @cname("SetTextColor");
extern fn CInt setBkMode(Win32_HDC, CInt) @cname("SetBkMode");
extern fn Win32_BOOL textOut(Win32_HDC, CInt, CInt, Win32_LPCWSTR, CInt) @cname("TextOutW");