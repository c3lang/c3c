module std::os::win32 @if(env::WIN32);

struct Win32_RECT
{
	Win32_LONG left;
	Win32_LONG top;
	Win32_LONG right;
	Win32_LONG bottom;
}

struct Win32_POINT
{
	Win32_LONG x;
	Win32_LONG y;
}

struct Win32_SIZE
{
	Win32_LONG cx;
	Win32_LONG cy;
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

def Win32_PSIZE = Win32_SIZE*;
def Win32_NPSIZE = Win32_SIZE*;
def Win32_LPSIZE = Win32_SIZE*;

def Win32_PPOINT = Win32_POINT*;
def Win32_NPOINT = Win32_POINT*;
def Win32_LPOINT = Win32_POINT*;

def Win32_PRECT = Win32_RECT*;
def Win32_NPRECT = Win32_RECT*;
def Win32_LPRECT = Win32_RECT*;

def Win32_PMSG = Win32_MSG*;
def Win32_LPMSG = Win32_MSG*;
def Win32_NPMSG = Win32_MSG*;

def Win32_PPAINTSTRUCT = Win32_PAINTSTRUCT*;
def Win32_LPPAINTSTRUCT = Win32_PAINTSTRUCT*;
def Win32_NPPAINTSTRUCT = Win32_PAINTSTRUCT*;

def Win32_PWNDCLASSEXW = Win32_WNDCLASSEXW*;
def Win32_LPWNDCLASSEXW = Win32_WNDCLASSEXW*;
def Win32_NPWNDCLASSEXW = Win32_WNDCLASSEXW*;

def Win32_ATOM = ushort;

def Win32_WNDPROC = fn Win32_LRESULT(Win32_HWND, Win32_UINT, Win32_WPARAM, Win32_LPARAM);

extern fn int messageBoxW(Win32_HWND hWnd, Win32_LPCWSTR lpText, Win32_LPCWSTR lpCaption, Win32_UINT uType) @extern("MessageBoxW");
extern fn Win32_HCURSOR loadCursorW(Win32_HINSTANCE instance, Win32_LPCWSTR cursorName) @extern("LoadCursorW");
extern fn Win32_HICON loadIconW(Win32_HINSTANCE instance, Win32_LPCWSTR iconName) @extern("LoadIconW");
extern fn Win32_ATOM registerClassExW(Win32_WNDCLASSEXW*) @extern("RegisterClassExW");
extern fn Win32_HWND createWindowExW(Win32_DWORD, Win32_LPCWSTR, Win32_LPCWSTR, Win32_DWORD, CInt, CInt, CInt, CInt, Win32_HWND, Win32_HMENU, Win32_HINSTANCE, Win32_LPVOID) @extern("CreateWindowExW");
extern fn Win32_BOOL showWindow(Win32_HWND, CInt) @extern("ShowWindow");
extern fn Win32_BOOL updateWindow(Win32_HWND) @extern("UpdateWindow");
extern fn Win32_BOOL getMessageW(Win32_LPMSG, Win32_HWND, Win32_UINT, Win32_UINT) @extern("GetMessageW");
extern fn Win32_BOOL translateMessage(Win32_MSG* lpMsg) @extern("TranslateMessage");
extern fn Win32_BOOL dispatchMessage(Win32_MSG* lpMsg) @extern("DispatchMessageW");
extern fn Win32_LRESULT defWindowProcW(Win32_HWND, Win32_UINT, Win32_WPARAM, Win32_LPARAM) @extern("DefWindowProcW");
extern fn void postQuitMessage(CInt) @extern("PostQuitMessage");
extern fn Win32_HDC beginPaint(Win32_HWND, Win32_LPPAINTSTRUCT) @extern("BeginPaint");
extern fn Win32_BOOL endPaint(Win32_HWND, Win32_LPPAINTSTRUCT) @extern("EndPaint");

// Function declarations from gdi32.dll
extern fn Win32_HBRUSH createSolidBrush(Win32_COLORREF) @extern("CreateSolidBrush");
extern fn Win32_COLORREF setTextColor(Win32_HDC, Win32_COLORREF) @extern("SetTextColor");
extern fn CInt setBkMode(Win32_HDC, CInt) @extern("SetBkMode");
extern fn Win32_BOOL textOut(Win32_HDC, CInt, CInt, Win32_LPCWSTR, CInt) @extern("TextOutW");