module std::os::win32 @if(env::WIN32);

def Win32_WNDPROC = fn Win32_LRESULT(Win32_HWND, Win32_UINT, Win32_WPARAM, Win32_LPARAM);

struct Win32_WNDCLASSEXW
{
	Win32_UINT      cbSize;
	Win32_UINT      style;
	Win32_WNDPROC   lpfnWndProc;
	CInt            cbClsExtra;
	CInt            cbWndExtra;
	Win32_HINSTANCE hInstance;
	Win32_HICON     hIcon;
	Win32_HCURSOR   hCursor;
	Win32_HBRUSH    hbrBackground;
	Win32_LPCWSTR   lpszMenuName;
	Win32_LPCWSTR   lpszClassName;
	Win32_HICON     hIconSm;
}

struct Win32_MSG
{
	Win32_HWND hwnd;
	Win32_UINT message;
	Win32_WPARAM wParam;
	Win32_LPARAM lParam;
	Win32_DWORD time;
	Win32_POINT pt;
	Win32_DWORD lPrivate;
}

struct Win32_PAINTSTRUCT
{
	Win32_HDC  hdc;
	Win32_BOOL fErase;
	Win32_RECT rcPaint;
	Win32_BOOL fRestore;
	Win32_BOOL fIncUpdate;
	Win32_BYTE[32] rgbReserved;
}
def Win32_PWNDCLASSEXW = Win32_WNDCLASSEXW*;
def Win32_LPWNDCLASSEXW = Win32_WNDCLASSEXW*;
def Win32_NPWNDCLASSEXW = Win32_WNDCLASSEXW*;

def Win32_PPAINTSTRUCT = Win32_PAINTSTRUCT*;
def Win32_LPPAINTSTRUCT = Win32_PAINTSTRUCT*;
def Win32_NPPAINTSTRUCT = Win32_PAINTSTRUCT*;

def Win32_PMSG = Win32_MSG*;
def Win32_LPMSG = Win32_MSG*;
def Win32_NPMSG = Win32_MSG*;

def Win32_ATOM = ushort;

const Win32_DWORD WS_BORDER           = 0x00800000L;
const Win32_DWORD WS_CAPTION          = 0x00C00000L;
const Win32_DWORD WS_CHILD            = 0x40000000L;
const Win32_DWORD WS_CHILDWINDOW      = 0x40000000L;
const Win32_DWORD WS_CLIPCHILDREN     = 0x02000000L;
const Win32_DWORD WS_CLIPSIBLINGS     = 0x04000000L;
const Win32_DWORD WS_DISABLED         = 0x08000000L;
const Win32_DWORD WS_DLGFRAME         = 0x00400000L;
const Win32_DWORD WS_GROUP            = 0x00020000L;
const Win32_DWORD WS_HSCROLL          = 0x00100000L;
const Win32_DWORD WS_ICONIC           = 0x20000000L;
const Win32_DWORD WS_MAXIMIZE         = 0x01000000L;
const Win32_DWORD WS_MAXIMIZEBOX      = 0x00010000L;
const Win32_DWORD WS_MINIMIZE         = 0x20000000L;
const Win32_DWORD WS_MINIMIZEBOX      = 0x00020000L;
const Win32_DWORD WS_OVERLAPPED       = 0x00000000L;
const Win32_DWORD WS_OVERLAPPEDWINDOW = WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_THICKFRAME | WS_MINIMIZEBOX | WS_MAXIMIZEBOX;
const Win32_DWORD WS_POPUP            = 0x80000000L;
const Win32_DWORD WS_POPUPWINDOW      = WS_POPUP | WS_BORDER | WS_SYSMENU;
const Win32_DWORD WS_SIZEBOX          = 0x00040000L;
const Win32_DWORD WS_SYSMENU          = 0x00080000L;
const Win32_DWORD WS_TABSTOP          = 0x00010000L;
const Win32_DWORD WS_THICKFRAME       = 0x00040000L;
const Win32_DWORD WS_TILED            = 0x00000000L;
const Win32_DWORD WS_TILEDWINDOW      = WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_THICKFRAME | WS_MINIMIZEBOX | WS_MAXIMIZEBOX;
const Win32_DWORD WS_VISIBLE          = 0x10000000L;
const Win32_DWORD WS_VSCROLL          = 0x00200000L;

extern fn Win32_HDC beginPaint(Win32_HWND, Win32_LPPAINTSTRUCT) @extern("BeginPaint");
extern fn Win32_HWND createWindowExW(Win32_DWORD, Win32_LPCWSTR, Win32_LPCWSTR, Win32_DWORD, CInt, CInt, CInt, CInt, Win32_HWND, Win32_HMENU, Win32_HINSTANCE, Win32_LPVOID) @extern("CreateWindowExW");
extern fn Win32_LRESULT defWindowProcW(Win32_HWND, Win32_UINT, Win32_WPARAM, Win32_LPARAM) @extern("DefWindowProcW");
extern fn Win32_BOOL dispatchMessage(Win32_MSG* lpMsg) @extern("DispatchMessageW");
extern fn Win32_BOOL endPaint(Win32_HWND, Win32_LPPAINTSTRUCT) @extern("EndPaint");
extern fn Win32_BOOL getMessageW(Win32_LPMSG, Win32_HWND, Win32_UINT, Win32_UINT) @extern("GetMessageW");
extern fn Win32_HCURSOR loadCursorW(Win32_HINSTANCE instance, Win32_LPCWSTR cursorName) @extern("LoadCursorW");
extern fn Win32_HICON loadIconW(Win32_HINSTANCE instance, Win32_LPCWSTR iconName) @extern("LoadIconW");
extern fn int messageBoxW(Win32_HWND hWnd, Win32_LPCWSTR lpText, Win32_LPCWSTR lpCaption, Win32_UINT uType) @extern("MessageBoxW");
extern fn void postQuitMessage(CInt) @extern("PostQuitMessage");
extern fn Win32_ATOM registerClassExW(Win32_WNDCLASSEXW*) @extern("RegisterClassExW");
extern fn Win32_BOOL showWindow(Win32_HWND, CInt) @extern("ShowWindow");
extern fn Win32_BOOL translateMessage(Win32_MSG* lpMsg) @extern("TranslateMessage");
extern fn Win32_BOOL updateWindow(Win32_HWND) @extern("UpdateWindow");

