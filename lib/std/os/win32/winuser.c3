module std::os::win32 @link("user32") @feat(WIN32);

const CCHDEVICENAME = 32;
alias Win32_WNDPROC = fn Win32_LRESULT(Win32_HWND, Win32_UINT, Win32_WPARAM, Win32_LPARAM);
alias Win32_MONITORENUMPROC = fn Win32_BOOL(Win32_HMONITOR, Win32_HDC, Win32_LPRECT, Win32_LPARAM);
alias Win32_TIMERPROC = fn void(Win32_HWND, Win32_UINT, Win32_UINT_PTR, Win32_DWORD);
alias Win32_HRAWINPUT = Win32_HANDLE;

struct Win32_RAWHID
{
	Win32_DWORD    dwSizeHid;
	Win32_DWORD    dwCount;
	Win32_BYTE[1]  bRawData;
}

struct Win32_RAWINPUT
{
	Win32_RAWINPUTHEADER header;
	union
	{
		Win32_RAWMOUSE    mouse;
		Win32_RAWKEYBOARD keyboard;
		Win32_RAWHID      hid;
	}
}

struct Win32_ICONINFO
{
	Win32_BOOL    fIcon;
	Win32_DWORD   xHotspot;
	Win32_DWORD   yHotspot;
	Win32_HBITMAP hbmMask;
	Win32_HBITMAP hbmColor;
}

struct Win32_RAWINPUTHEADER
{
	Win32_DWORD  dwType;
	Win32_DWORD  dwSize;
	Win32_HANDLE hDevice;
	Win32_WPARAM wParam;
}

struct Win32_RAWKEYBOARD
{
	Win32_USHORT makeCode;
	Win32_USHORT flags;
	Win32_USHORT reserved;
	Win32_USHORT vKey;
	Win32_UINT   message;
	Win32_ULONG  extraInformation;
}

struct Win32_RAWINPUTDEVICE
{
	Win32_USHORT usUsagePage;
	Win32_USHORT usUsage;
	Win32_DWORD  dwFlags;
	Win32_HWND   hwndTarget;
}

struct Win32_RAWMOUSE
{
	Win32_USHORT usFlags;
	union
	{
		Win32_ULONG ulButtons;
		struct
		{
			Win32_USHORT usButtonFlags;
			Win32_USHORT usButtonData;
		}
	}
	Win32_ULONG  ulRawButtons;
	Win32_LONG   lLastX;
	Win32_LONG   lLastY;
	Win32_ULONG  ulExtraInformation;
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

struct Win32_FLASHWINFO
{
	Win32_UINT  cbSize;
	Win32_HWND  hwnd;
	Win32_DWORD dwFlags;
	Win32_UINT  uCount;
	Win32_DWORD dwTimeout;
}

struct Win32_WNDCLASSW
{
	Win32_UINT      style;
	Win32_WNDPROC   lpfnWndProc;
	int             cbClsExtra;
	int             cbWndExtra;
	Win32_HINSTANCE hInstance;
	Win32_HICON     hIcon;
	Win32_HCURSOR   hCursor;
	Win32_HBRUSH    hbrBackground;
	Win32_LPCWSTR   lpszMenuName;
	Win32_LPCWSTR   lpszClassName;
}

struct Win32_DEVMODEW
{
	Win32_WCHAR[CCHDEVICENAME] dmDeviceName;
	Win32_WORD  dmSpecVersion;
	Win32_WORD  dmDriverVersion;
	Win32_WORD  dmSize;
	Win32_WORD  dmDriverExtra;
	Win32_DWORD dmFields;
	union {
		struct {
			short dmOrientation;
			short dmPaperSize;
			short dmPaperLength;
			short dmPaperWidth;
			short dmScale;
			short dmCopies;
			short dmDefaultSource;
			short dmPrintQuality;
		}
		Win32_POINTL dmPosition;
		struct dummy2
		{
			Win32_POINTL dmPosition;
			Win32_DWORD  dmDisplayOrientation;
			Win32_DWORD  dmDisplayFixedOutput;
		}
	}
	short dmColor;
	short dmDuplex;
	short dmYResolution;
	short dmTTOption;
	short dmCollate;
	Win32_WCHAR[CCHDEVICENAME] dmFormName;
	Win32_WORD  dmLogPixels;
	Win32_DWORD dmBitsPerPel;
	Win32_DWORD dmPelsWidth;
	Win32_DWORD dmPelsHeight;
	union
	{
		Win32_DWORD dmDisplayFlags;
		Win32_DWORD dmNup;
	}
	Win32_DWORD dmDisplayFrequency;
	Win32_DWORD dmICMMethod;
	Win32_DWORD dmICMIntent;
	Win32_DWORD dmMediaType;
	Win32_DWORD dmDitherType;
	Win32_DWORD dmReserved1;
	Win32_DWORD dmReserved2;
	Win32_DWORD dmPanningWidth;
	Win32_DWORD dmPanningHeight;
}

struct Win32_DISPLAY_DEVICEW
{
	Win32_DWORD      cb;
	Win32_WCHAR[32]  deviceName;
	Win32_WCHAR[128] deviceString;
	Win32_DWORD      stateFlags;
	Win32_WCHAR[128] deviceID;
	Win32_WCHAR[128] deviceKey;
}

struct Win32_WINDOWPLACEMENT
{
	Win32_UINT  length;
	Win32_UINT  flags;
	Win32_UINT  showCmd;
	Win32_POINT ptMinPosition;
	Win32_POINT ptMaxPosition;
	Win32_RECT  rcNormalPosition;
	Win32_RECT  rcDevice;
}

struct Win32_MONITORINFO
{
	Win32_DWORD cbSize;
	Win32_RECT  rcMonitor;
	Win32_RECT  rcWork;
	Win32_DWORD dwFlags;
}

struct Win32_MONITORINFOEXW
{
	inline Win32_MONITORINFO parent;
	Win32_WCHAR[CCHDEVICENAME] szDevice;
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
alias Win32_PWNDCLASSEXW = Win32_WNDCLASSEXW*;
alias Win32_LPWNDCLASSEXW = Win32_WNDCLASSEXW*;
alias Win32_NPWNDCLASSEXW = Win32_WNDCLASSEXW*;

alias Win32_PPAINTSTRUCT = Win32_PAINTSTRUCT*;
alias Win32_LPPAINTSTRUCT = Win32_PAINTSTRUCT*;
alias Win32_NPPAINTSTRUCT = Win32_PAINTSTRUCT*;

alias Win32_PMSG = Win32_MSG*;
alias Win32_LPMSG = Win32_MSG*;
alias Win32_NPMSG = Win32_MSG*;

alias Win32_PDEVMODEW = Win32_DEVMODEW*;
alias Win32_NPDEVMODEW = Win32_DEVMODEW*;
alias Win32_LPDEVMODEW = Win32_DEVMODEW*;

alias Win32_PRAWMOUSE = Win32_RAWMOUSE*;
alias Win32_LPRAWMOUSE = Win32_RAWMOUSE*;

alias Win32_PRAWKEYBOARD = Win32_RAWKEYBOARD*;
alias Win32_LPRAWKEYBOARD = Win32_RAWKEYBOARD*;

alias Win32_PRAWINPUT = Win32_RAWINPUT*;
alias Win32_LPRAWINPUT = Win32_RAWINPUT*;

alias Win32_PFLASHWINFO = Win32_FLASHWINFO*;

alias Win32_PRAWINPUTDEVICE = Win32_RAWINPUTDEVICE*;
alias Win32_LPRAWINPUTDEVICE = Win32_RAWINPUTDEVICE*;

alias Win32_PDISPLAY_DEVICEW = Win32_DISPLAY_DEVICEW*;
alias Win32_LPDISPLAY_DEVICEW = Win32_DISPLAY_DEVICEW*;

alias Win32_LPMONITORINFO = Win32_MONITORINFO*;
alias Win32_LPMONITORINFOEXW = Win32_MONITORINFOEXW*;

alias Win32_PRAWINPUTHEADER = Win32_RAWINPUTHEADER*;
alias Win32_LPRAWINPUTHEADER = Win32_RAWINPUTHEADER*;

alias Win32_PRAWHID = Win32_RAWHID*;
alias Win32_LPRAWHID = Win32_RAWHID*;

alias Win32_PWNDCLASSW = Win32_WNDCLASSW*;
alias Win32_NPWNDCLASSW = Win32_WNDCLASSW*;
alias Win32_LPWNDCLASSW = Win32_WNDCLASSW*;

alias Win32_ATOM = ushort;

const Win32_DWORD WS_BORDER               = 0x00800000L;
const Win32_DWORD WS_CAPTION              = 0x00C00000L;
const Win32_DWORD WS_CHILD                = 0x40000000L;
const Win32_DWORD WS_CHILDWINDOW          = 0x40000000L;
const Win32_DWORD WS_CLIPCHILDREN         = 0x02000000L;
const Win32_DWORD WS_CLIPSIBLINGS         = 0x04000000L;
const Win32_DWORD WS_DISABLED             = 0x08000000L;
const Win32_DWORD WS_DLGFRAME             = 0x00400000L;
const Win32_DWORD WS_GROUP                = 0x00020000L;
const Win32_DWORD WS_HSCROLL              = 0x00100000L;
const Win32_DWORD WS_ICONIC               = 0x20000000L;
const Win32_DWORD WS_MAXIMIZE             = 0x01000000L;
const Win32_DWORD WS_MAXIMIZEBOX          = 0x00010000L;
const Win32_DWORD WS_MINIMIZE             = 0x20000000L;
const Win32_DWORD WS_MINIMIZEBOX          = 0x00020000L;
const Win32_DWORD WS_OVERLAPPED           = 0x00000000L;
const Win32_DWORD WS_OVERLAPPEDWINDOW     = WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_THICKFRAME | WS_MINIMIZEBOX | WS_MAXIMIZEBOX;
const Win32_DWORD WS_POPUP                = 0x80000000L;
const Win32_DWORD WS_POPUPWINDOW          = WS_POPUP | WS_BORDER | WS_SYSMENU;
const Win32_DWORD WS_SIZEBOX              = 0x00040000L;
const Win32_DWORD WS_SYSMENU              = 0x00080000L;
const Win32_DWORD WS_TABSTOP              = 0x00010000L;
const Win32_DWORD WS_THICKFRAME           = 0x00040000L;
const Win32_DWORD WS_TILED                = 0x00000000L;
const Win32_DWORD WS_TILEDWINDOW          = WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_THICKFRAME | WS_MINIMIZEBOX | WS_MAXIMIZEBOX;
const Win32_DWORD WS_VISIBLE              = 0x10000000L;
const Win32_DWORD WS_VSCROLL              = 0x00200000L;

const Win32_DWORD WS_EX_ACCEPTFILES       = 0x00000010L;
const Win32_DWORD WS_EX_APPWINDOW         = 0x00040000L;
const Win32_DWORD WS_EX_CLIENTEDGE        = 0x00000200L;
const Win32_DWORD WS_EX_COMPOSITED        = 0x02000000L;
const Win32_DWORD WS_EX_CONTEXTHELP       = 0x00000400L;
const Win32_DWORD WS_EX_CONTROLPARENT     = 0x00010000L;
const Win32_DWORD WS_EX_DLGMODALFRAME     = 0x00000001L;
const Win32_DWORD WS_EX_LAYERED           = 0x00080000L;
const Win32_DWORD WS_EX_LAYOUTRTL         = 0x00400000L;
const Win32_DWORD WS_EX_LEFT              = 0x00000000L;
const Win32_DWORD WS_EX_LEFTSCROLLBAR     = 0x00004000L;
const Win32_DWORD WS_EX_LTRREADING        = 0x00000000L;
const Win32_DWORD WS_EX_MDICHILD          = 0x00000040L;
const Win32_DWORD WS_EX_NOACTIVATE        = 0x08000000L;
const Win32_DWORD WS_EX_NOINHERITLAYOUT   = 0x00100000L;
const Win32_DWORD WS_EX_NOPARENTNOTIFY    = 0x00000004L;
const Win32_DWORD WS_EX_NOREDIRECTIONBITMAP = 0x00200000L;
const Win32_DWORD WS_EX_OVERLAPPEDWINDOW  = (WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE);
const Win32_DWORD WS_EX_PALETTEWINDOW     = (WS_EX_WINDOWEDGE | WS_EX_TOOLWINDOW | WS_EX_TOPMOST);
const Win32_DWORD WS_EX_RIGHT             = 0x00001000L;
const Win32_DWORD WS_EX_RIGHTSCROLLBAR    = 0x00000000L;
const Win32_DWORD WS_EX_RTLREADING        = 0x00002000L;
const Win32_DWORD WS_EX_STATICEDGE        = 0x00020000L;
const Win32_DWORD WS_EX_TOOLWINDOW        = 0x00000080L;
const Win32_DWORD WS_EX_TOPMOST           = 0x00000008L;
const Win32_DWORD WS_EX_TRANSPARENT       = 0x00000020L;
const Win32_DWORD WS_EX_WINDOWEDGE        = 0x00000100L;

const Win32_HWND HWND_TOPMOST = (Win32_HWND)(uptr)(-1);
const Win32_HWND HWND_TOP = (Win32_HWND)(uptr)0;

const Win32_UINT MB_OK = 0x00000000;
const Win32_UINT MB_OKCANCEL = 0x00000001;
const Win32_UINT MB_ABORTRETRYIGNORE = 0x00000002;
const Win32_UINT MB_YESNOCANCEL = 0x00000003;
const Win32_UINT MB_YESNO = 0x00000004;
const Win32_UINT MB_RETRYCANCEL = 0x00000005;
const Win32_UINT MB_CANCELTRYCONTINUE = 0x00000006;
const Win32_UINT MB_ICONHAND = 0x00000010;
const Win32_UINT MB_ICONQUESTION = 0x00000020;
const Win32_UINT MB_ICONEXCLAMATION = 0x00000030;
const Win32_UINT MB_ICONASTERISK = 0x00000040;
const Win32_UINT MB_USERICON = 0x00000080;
const Win32_UINT MB_ICONWARNING = MB_ICONEXCLAMATION;
const Win32_UINT MB_ICONERROR = MB_ICONHAND;
const Win32_UINT MB_ICONINFORMATION = MB_ICONASTERISK;
const Win32_UINT MB_ICONSTOP = MB_ICONHAND;

const FLASHW_ALL       = 0x00000003;
const FLASHW_CAPTION   = 0x00000001;
const FLASHW_STOP      = 0;
const FLASHW_TIMER     = 0x00000004;
const FLASHW_TIMERNOFG = 0x0000000C;
const FLASHW_TRAY      = 0x00000002;

const WM_ACTIVATEAPP                 = 0x001C;
const WM_CAPTURECHANGED              = 0x0215;
const WM_CHAR                        = 0x0102;
const WM_CLOSE                       = 0x0010;
const WM_CREATE                      = 0x0001;
const WM_DISPLAYCHANGE               = 0x007E;
const WM_DPICHANGED                  = 0x02E0;
const WM_DROPFILES                   = 0x0233;
const WM_DWMCOLORIZATIONCOLORCHANGED = 0x0320;
const WM_DWMCOMPOSITIONCHANGED       = 0x031E;
const WM_ENTERSIZEMOVE               = 0x0231;
const WM_EXITSIZEMOVE                = 0x0232;
const WM_GETMINMAXINFO               = 0x0024;
const WM_INPUT                       = 0x00FF;
const WM_KEYDOWN                     = 0x0100;
const WM_KEYUP                       = 0x0101;
const WM_LBUTTONDOWN                 = 0x0201;
const WM_LBUTTONUP                   = 0x0202;
const WM_MBUTTONDOWN                 = 0x0207;
const WM_MBUTTONUP                   = 0x0208;
const WM_MOUSEACTIVATE               = 0x0021;
const WM_MOUSEHWHEEL                 = 0x020E;
const WM_MOUSELEAVE                  = 0x02A3;
const WM_MOUSEMOVE                   = 0x0200;
const WM_MOUSEWHEEL                  = 0x020A;
const WM_MOVE                        = 0x0003;
const WM_MOVING                      = 0x0216;
const WM_NCHITTEST                   = 0x0084;
const WM_NCLBUTTONDOWN               = 0x00A1;
const WM_PAINT                       = 0x000F;
const WM_QUIT                        = 0x0012;
const WM_RBUTTONDOWN                 = 0x0204;
const WM_RBUTTONUP                   = 0x0205;
const WM_SETICON                     = 0x0080;
const WM_SHOWWINDOW                  = 0x0018;
const WM_SIZE                        = 0x0005;
const WM_SIZING                      = 0x0214;
const WM_SYSCHAR                     = 0x0106;
const WM_SYSKEYDOWN                  = 0x0104;
const WM_SYSKEYUP                    = 0x0105;
const WM_TIMER                       = 0x0113;
const WM_UNICHAR                     = 0x0109;
const WM_XBUTTONDOWN                 = 0x020B;
const WM_XBUTTONUP                   = 0x020C;

const HTCAPTION = 2;
const HTCLIENT = 1;

const RIDEV_REMOVE       = 0x00000001;
const RIDEV_EXCLUDE      = 0x00000010;
const RIDEV_PAGEONLY     = 0x00000020;
const RIDEV_NOLEGACY     = 0x00000030;
const RIDEV_INPUTSINK    = 0x00000100;
const RIDEV_CAPTUREMOUSE = 0x00000200;
const RIDEV_NOHOTKEYS    = 0x00000200;
const RIDEV_APPKEYS      = 0x00000400;
const RIDEV_EXINPUTSINK  = 0x00001000;
const RIDEV_DEVNOTIFY    = 0x00002000;


const WMSZ_BOTTOM = 6;
const WMSZ_BOTTOMLEFT = 7;
const WMSZ_BOTTOMRIGHT = 8;
const WMSZ_LEFT = 1;
const WMSZ_RIGHT = 2;
const WMSZ_TOP = 3;
const WMSZ_TOPLEFT = 4;
const WMSZ_TOPRIGHT = 5;

const WPF_ASYNCWINDOWPLACEMENT = 0x0004;
const WPF_RESTORETOMAXIMIZED   = 0x0002;
const WPF_SETMINPOSITION       = 0x0001;

const SW_HIDE            = 0;
const SW_SHOWNORMAL      = 1;
const SW_NORMAL          = 1;
const SW_SHOWMINIMIZED   = 2;
const SW_SHOWMAXIMIZED   = 3;
const SW_MAXIMIZE        = 3;
const SW_SHOWNOACTIVATE  = 4;
const SW_SHOW            = 5;
const SW_MINIMIZE        = 6;
const SW_SHOWMINNOACTIVE = 7;
const SW_SHOWNA          = 8;
const SW_RESTORE         = 9;
const SW_SHOWDEFAULT     = 10;
const SW_FORCEMINIMIZE   = 11;

const GWL_WNDPROC @feat(ARCH_32_BIT) = -4;
const GWL_HINSTANCE @feat(ARCH_32_BIT) = -6;
const GWL_HWNDPARENT @feat(ARCH_32_BIT) = -8;
const GWL_STYLE = -16;
const GWL_EXSTYLE = -20;
const GWL_USERDATA @feat(ARCH_32_BIT) = -21;
const GWL_ID = -12;
const GWLP_WNDPROC = -4;
const GWLP_HINSTANCE = -6;
const GWLP_HWNDPARENT = -8;
const GWLP_USERDATA = -21;
const GWLP_ID = -12;

const GCW_ATOM = -32;
const GCL_CBCLSEXTRA = -20;
const GCL_CBWNDEXTRA = -18;
const GCLP_HBRBACKGROUND = -10;
const GCLP_HCURSOR = -12;
const GCLP_HICON = -14;
const GCLP_HICONSM = -34;
const GCLP_HMODULE = -16;
const GCLP_MENUNAME = -8;
const GCL_STYLE = -26;
const GCLP_WNDPROC = -24;

const PM_NOREMOVE = 0x0000;
const PM_REMOVE   = 0x0001;
const PM_NOYIELD  = 0x0002;

extern fn Win32_LRESULT callWindowProcW(Win32_WNDPROC lpPrevWndFunc, Win32_HWND hWnd, Win32_UINT msg, Win32_WPARAM wParam, Win32_LPARAM lParam) @cname("CallWindowProcW");
extern fn Win32_HWND createWindowExW(Win32_DWORD, Win32_LPCWSTR, Win32_LPCWSTR, Win32_DWORD, CInt, CInt, CInt, CInt, Win32_HWND, Win32_HMENU, Win32_HINSTANCE, Win32_LPVOID) @cname("CreateWindowExW");
extern fn Win32_LRESULT defWindowProcW(Win32_HWND, Win32_UINT, Win32_WPARAM, Win32_LPARAM) @cname("DefWindowProcW");
extern fn Win32_BOOL dispatchMessage(Win32_MSG* lpMsg) @cname("DispatchMessageW");
extern fn Win32_BOOL endPaint(Win32_HWND, Win32_LPPAINTSTRUCT) @cname("EndPaint");
extern fn Win32_BOOL getMessageW(Win32_LPMSG, Win32_HWND, Win32_UINT, Win32_UINT) @cname("GetMessageW");
extern fn Win32_BOOL getUpdateRect(Win32_HWND hWnd, Win32_LPRECT lpRect, Win32_BOOL bErase) @cname("GetUpdateRect");
extern fn Win32_HCURSOR loadCursorW(Win32_HINSTANCE instance, Win32_LPCWSTR cursorName) @cname("LoadCursorW");
extern fn Win32_HICON loadIconW(Win32_HINSTANCE instance, Win32_LPCWSTR iconName) @cname("LoadIconW");
extern fn Win32_BOOL destroyCursor(Win32_HCURSOR hCursor) @cname("DestroyCursor");
extern fn Win32_BOOL destroyWindow(Win32_HWND wnd) @cname("DestroyWindow");

extern fn int messageBoxW(Win32_HWND hWnd, Win32_LPCWSTR lpText, Win32_LPCWSTR lpCaption, Win32_UINT uType) @cname("MessageBoxW");
extern fn Win32_BOOL peekMessageA(Win32_LPMSG lpMsg, Win32_HWND hWnd, Win32_UINT wMsgFilterMin, Win32_UINT wMsgFilterMax, Win32_UINT wRemoveMsg) @cname("PeekMessageA");
extern fn Win32_BOOL peekMessageW(Win32_LPMSG lpMsg, Win32_HWND hWnd, Win32_UINT wMsgFilterMin, Win32_UINT wMsgFilterMax, Win32_UINT wRemoveMsg) @cname("PeekMessageW");
extern fn Win32_BOOL translateMessage(Win32_MSG* lpMsg) @cname("TranslateMessage");
extern fn Win32_LRESULT dispatchMessageA(Win32_MSG* lpMsg) @cname("DispatchMessageA");
extern fn Win32_BOOL postMessageW(Win32_HWND hWnd, Win32_UINT msg, Win32_WPARAM wParam, Win32_LPARAM lParam) @cname("PostMessageW");
extern fn Win32_BOOL postMessage(Win32_HWND hWnd, Win32_UINT msg, Win32_WPARAM wParam, Win32_LPARAM lParam) @cname("PostMessageA");
extern fn Win32_LRESULT sendMessageA(Win32_HWND hWnd, Win32_UINT msg, Win32_WPARAM wParam, Win32_LPARAM lParam) @cname("SendMessageA");
extern fn void postQuitMessage(CInt) @cname("PostQuitMessage");
extern fn Win32_BOOL deleteDC(Win32_HDC hDC) @cname("DeleteDC");
extern fn Win32_HDC getDC(Win32_HWND hWnd) @cname("GetDC");
extern fn int releaseDC(Win32_HWND hWnd, Win32_HDC hDC) @cname("ReleaseDC");
extern fn Win32_ATOM registerClassExW(Win32_WNDCLASSEXW*) @cname("RegisterClassExW");

extern fn Win32_BOOL getWindowRect(Win32_HWND hWnd, Win32_LPRECT lpRect) @cname("GetWindowRect");
extern fn Win32_BOOL getClientRect(Win32_HWND hWnd, Win32_LPRECT lpRect) @cname("GetClientRect");
extern fn Win32_BOOL adjustWindowRectEx(Win32_LPRECT lpRect, Win32_DWORD dwStyle, Win32_DWORD bMenu, Win32_DWORD dwExStyle) @cname("AdjustWindowRectEx");
extern fn int setWindowPos(Win32_HWND hWnd, Win32_HWND hWndInsertAfter, int x, int y, int cx, int cy, Win32_UINT uFlags) @cname("SetWindowPos");
extern fn Win32_LONG setWindowLongW(Win32_HWND hWnd, int nIndex, Win32_LONG dwNewLong) @cname("SetWindowLongW");
extern fn Win32_LONG setWindowLongA(Win32_HWND hWnd, int nIndex, Win32_LONG dwNewLong) @cname("SetWindowLongA");
extern fn Win32_LONG getWindowLongA(Win32_HWND hWnd, int nIndex) @cname("GetWindowLongA");
extern fn Win32_LONG getWindowLongW(Win32_HWND hWnd, int nIndex) @cname("GetWindowLongW");
extern fn Win32_LONG_PTR getWindowLongPtrA(Win32_HWND hWnd, int nIndex) @cname("GetWindowLongPtrA");
extern fn Win32_LONG_PTR getWindowLongPtrW(Win32_HWND hWnd, int nIndex) @cname("GetWindowLongPtrW");
extern fn Win32_BOOL showWindow(Win32_HWND wnd, int nCmdShow) @cname("ShowWindow");
extern fn Win32_BOOL isWindowVisible(Win32_HWND hWnd) @cname("IsWindowVisible");
extern fn Win32_BOOL isZoomed(Win32_HWND hWnd) @cname("IsZoomed");
extern fn Win32_BOOL getWindowPlacement(Win32_HWND hWnd, Win32_WINDOWPLACEMENT* lpwndpl) @cname("GetWindowPlacement");
extern fn Win32_BOOL bringWindowToTop(Win32_HWND hWnd) @cname("BringWindowToTop");

extern fn Win32_LONG_PTR setWindowLongPtrW(Win32_HWND hWnd, CInt nIndex, Win32_LONG_PTR dwNewLong) @cname("SetWindowLongPtrW");
extern fn Win32_BOOL updateWindow(Win32_HWND hWnd) @cname("UpdateWindow");
extern fn Win32_BOOL enumDisplayMonitors(Win32_HDC hdc, Win32_LPCRECT lprcClip, Win32_MONITORENUMPROC* lpfnEnum, Win32_LPARAM dwData) @cname("EnumDisplayMonitors");
extern fn Win32_BOOL setProcessDPIAware() @cname("SetProcessDPIAware");
extern fn Win32_BOOL setForegroundWindow(Win32_HWND hWnd) @cname("SetForegroundWindow");
extern fn Win32_HWND setFocus(Win32_HWND hWnd) @cname("SetFocus");
extern fn Win32_BOOL setWindowTextW(Win32_HWND hWnd, Win32_LPCWSTR lpString) @cname("SetWindowTextW");
extern fn Win32_BOOL setLayeredWindowAttributes(Win32_HWND hwnd, Win32_COLORREF crKey, Win32_BYTE bAlpha, Win32_DWORD dwFlags) @cname("SetLayeredWindowAttributes");
extern fn Win32_BOOL getLayeredWindowAttributes(Win32_HWND hwnd, Win32_COLORREF* pcrKey, Win32_BYTE* pbAlpha, Win32_DWORD* pdwFlags) @cname("GetLayeredWindowAttributes");
extern fn Win32_HMONITOR monitorFromWindow(Win32_HWND hwnd, Win32_DWORD dwFlags) @cname("MonitorFromWindow");
extern fn Win32_BOOL flashWindowEx(Win32_FLASHWINFO* pfwi) @cname("FlashWindowEx");
extern fn Win32_HICON createIconIndirect(Win32_ICONINFO* piconinfo) @cname("CreateIconIndirect");
extern fn Win32_BOOL destroyIcon(Win32_HICON hIcon) @cname("DestroyIcon");
extern fn Win32_HCURSOR loadCursorA(Win32_HINSTANCE hInstance, Win32_LPCSTR lpCursorName) @cname("LoadCursorA");
extern fn Win32_HICON loadIconA(Win32_HINSTANCE hInstance, Win32_LPCSTR lpIconName) @cname("LoadIconA");
extern fn Win32_HANDLE loadImageA(Win32_HINSTANCE hInst, Win32_LPCSTR name, Win32_UINT type, CInt cx, CInt cy, Win32_UINT fuLoad) @cname("LoadImageA");
extern fn Win32_HCURSOR setCursor(Win32_HCURSOR hCursor) @cname("SetCursor");
extern fn Win32_ULONG_PTR setClassLongPtrA(Win32_HWND hWnd, CInt nIndex, Win32_LONG_PTR dwNewLong) @cname("SetClassLongPtrA");
extern fn Win32_BOOL getCursorPos(Win32_LPOINT lpPoint) @cname("GetCursorPos");
extern fn Win32_BOOL setCursorPos(CInt x, CInt y) @cname("SetCursorPos");
extern fn Win32_BOOL screenToClient(Win32_HWND hWnd, Win32_LPOINT lpPoint) @cname("ScreenToClient");
extern fn Win32_BOOL clientToScreen(Win32_HWND hWnd, Win32_LPOINT lpPoint) @cname("ClientToScreen");
extern fn Win32_BOOL clipCursor(Win32_LPRECT lpRect) @cname("ClipCursor");
extern fn Win32_BOOL registerRawInputDevices(Win32_RAWINPUTDEVICE* pRawInputDevices, Win32_UINT uiNumDevices, Win32_UINT cbSize) @cname("RegisterRawInputDevices");
extern fn Win32_UINT getRawInputData(Win32_HRAWINPUT hRawInput, Win32_UINT uiCommand, Win32_LPVOID pData, Win32_UINT* pcbSize, Win32_UINT cbSizeHeader) @cname("GetRawInputData");
extern fn CInt getSystemMetrics(CInt nIndex) @cname("GetSystemMetrics");

extern fn Win32_BOOL getMonitorInfoA(Win32_HMONITOR hMonitor, Win32_LPMONITORINFO lpmi) @cname("GetMonitorInfoA");
extern fn Win32_BOOL getMonitorInfoW(Win32_HMONITOR hMonitor, Win32_LPMONITORINFO lpmi) @cname("GetMonitorInfoW");
extern fn Win32_BOOL enumDisplayDevicesW(Win32_LPCWSTR lpDevice, Win32_DWORD devNum, Win32_PDISPLAY_DEVICEW lpDisplayDevice, Win32_DWORD flags) @cname("EnumDisplayDevicesW");
extern fn Win32_BOOL enumDisplaySettingsW(Win32_LPCWSTR lpszDeviceName, Win32_DWORD modeNum, Win32_DEVMODEW* lpDevMode) @cname("EnumDisplaySettingsW");
extern fn Win32_BOOL changeDisplaySettingsExW(Win32_LPCWSTR lpszDeviceName, Win32_DEVMODEW* lpDevMode, Win32_HWND hwnd, Win32_DWORD dwflags, Win32_LPVOID lParam) @cname("ChangeDisplaySettingsExW");
extern fn Win32_ATOM registerClassW(Win32_WNDCLASSW* lpWndClass) @cname("RegisterClassW");
extern fn Win32_BOOL unregisterClassW(Win32_LPCWSTR lpClassName, Win32_HINSTANCE hInstance) @cname("UnregisterClassW");

extern fn void* setPropW(Win32_HWND hWnd, Win32_LPCWSTR lpString, Win32_HANDLE hData) @cname("SetPropW");
extern fn void* getPropW(Win32_HWND hWnd, Win32_LPCWSTR lpString) @cname("GetPropW");
extern fn void* removePropW(Win32_HWND hWnd, Win32_LPCWSTR lpString) @cname("RemovePropW");

extern fn uint msgWaitForMultipleObjects(Win32_DWORD nCount, Win32_HANDLE* pHandles, Win32_BOOL fWaitAll, Win32_DWORD dwMilliseconds, Win32_DWORD dwWakeMask) @cname("MsgWaitForMultipleObjects");
extern fn Win32_BOOL openClipboard(Win32_HWND hWndNewOwner) @cname("OpenClipboard");
extern fn Win32_BOOL closeClipboard() @cname("CloseClipboard");
extern fn Win32_BOOL emptyClipboard() @cname("EmptyClipboard");
extern fn Win32_HANDLE getClipboardData(Win32_UINT uFormat) @cname("GetClipboardData");
extern fn Win32_HANDLE setClipboardData(Win32_UINT uFormat, Win32_HANDLE hMem) @cname("SetClipboardData");
extern fn Win32_BOOL getKeyboardState(Win32_PBYTE lpKeyState) @cname("GetKeyboardState");
extern fn short getKeyState(CInt nVirtKey) @cname("GetKeyState");
extern fn Win32_UINT mapVirtualKeyW(Win32_UINT uCode, Win32_UINT uMapType) @cname("MapVirtualKeyW");
extern fn Win32_HKL getKeyboardLayout(Win32_DWORD idThread) @cname("GetKeyboardLayout");
extern fn CInt toUnicodeEx(Win32_UINT wVirtKey, Win32_UINT wScanCode, Win32_PBYTE lpKeyState, Win32_LPWSTR pwszBuff, CInt cchBuff, Win32_UINT wFlags, Win32_HKL dwhkl) @cname("ToUnicodeEx");
extern fn Win32_HDC beginPaint(Win32_HWND hWnd, Win32_LPPAINTSTRUCT lpPaint) @cname("BeginPaint");
extern fn Win32_UINT_PTR setTimer(Win32_HWND hWnd, Win32_UINT_PTR nIDEvent, Win32_UINT uElapse, Win32_TIMERPROC lpTimerFunc) @cname("SetTimer");
extern fn Win32_BOOL killTimer(Win32_HWND hWnd, Win32_UINT_PTR uIDEvent) @cname("KillTimer");

macro getWindowLongPtr(Win32_HWND hWnd, CInt nIndex)
{
	$if env::ARCH_64_BIT:
		return getWindowLongPtrW(hWnd, nIndex);
	$else
		return getWindowLongW(hWnd, nIndex);
	$endif
}

macro setWindowLongPtr(Win32_HWND hWnd, CInt nIndex, dwNewLong)
{
	$if env::ARCH_64_BIT:
		return setWindowLongPtrW(hWnd, nIndex, dwNewLong);
	$else
		return setWindowLongW(hWnd, nIndex, dwNewLong);
	$endif
}


