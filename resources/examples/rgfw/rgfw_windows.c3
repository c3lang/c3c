module gfx @link("user32", "gdi32", "shell32", "advapi32", "dwmapi", "opengl32", "winmm");

// C3 port of the Win32/WGL RGFW backend from /Users/lerno/Downloads/AGFW.h (RGFW).
// Mechanical, line-by-line style translation of the RGFW_WINDOWS implementation.
// Standalone alternative to rgfw.c3 (macOS) / rgfw_x11.c3 / rgfw_wayland.c3 - only
// ONE rgfw*.c3 file should be included for a given target, matching how the
// original C header only ever compiles one #ifdef backend at a time.

const bool RGFW_BUILD_MACOS = false;
const bool RGFW_BUILD_X11 = false;
const bool RGFW_BUILD_WAYLAND = false;
const bool RGFW_BUILD_WINDOWS = true;
const bool RGFW_BUILD_EGL = false;

alias VkInstance = void*;
alias VkPhysicalDevice = void*;
alias VkSurfaceKHR = ulong;
alias VkResult = int;
alias RgfwVkGetInstanceProcAddressProc = fn RgfwProc(VkInstance instance, char* procname);

extern fn void* malloc(usz size);
extern fn void free(void* ptr);
extern fn void* memcpy(void* dest, void* src, usz count);
extern fn void* memset(void* dest, int value, usz count);
extern fn usz strlen(char* str);
extern fn char* strstr(char* haystack, char* needle);
extern fn float powf(float x, float y);
extern fn void* dlopen(char* path, int mode);
extern fn void* dlsym(void* handle, char* symbol);
extern fn int dlclose(void* handle);
extern fn int chdir(char* path);
extern fn int clock_gettime(int clock_id, Timespec* ts);

struct Timespec { long tv_sec; long tv_nsec; }

alias EglDisplay = void*;
alias EglContext = void*;
alias EglSurface = void*;
alias EglConfig = void*;
alias EglNativeDisplayType = void*;
alias EglNativeWindowType = void*;
alias EglBoolean = uint;
alias EglInt = int;
alias EglGetProcAddressProc = fn RgfwProc(char* procname);
alias EglGetDisplayProc = fn EglDisplay(EglNativeDisplayType display);
alias EglInitializeProc = fn EglBoolean(EglDisplay display, EglInt* major, EglInt* minor);
alias EglTerminateProc = fn EglBoolean(EglDisplay display);
alias EglBindApiProc = fn EglBoolean(uint api);
alias EglChooseConfigProc = fn EglBoolean(EglDisplay display, EglInt* attribs, EglConfig* configs, EglInt configSize, EglInt* numConfig);
alias EglCreateContextProc = fn EglContext(EglDisplay display, EglConfig config, EglContext shareContext, EglInt* attribs);
alias EglDestroyContextProc = fn EglBoolean(EglDisplay display, EglContext context);
alias EglCreateWindowSurfaceProc = fn EglSurface(EglDisplay display, EglConfig config, EglNativeWindowType window, EglInt* attribs);
alias EglDestroySurfaceProc = fn EglBoolean(EglDisplay display, EglSurface surface);
alias EglMakeCurrentProc = fn EglBoolean(EglDisplay display, EglSurface draw, EglSurface read, EglContext context);
alias EglSwapBuffersProc = fn EglBoolean(EglDisplay display, EglSurface surface);
alias EglSwapIntervalProc = fn EglBoolean(EglDisplay display, EglInt interval);
alias EglGetCurrentContextProc = fn EglContext();
alias EglQueryStringProc = fn char*(EglDisplay display, EglInt name);
alias EglGetConfigAttribProc = fn EglBoolean(EglDisplay display, EglConfig config, EglInt attribute, EglInt* value);


// ---- Win32 base types ----
typedef HWnd = void;
typedef HModule = void;
alias HDc = void*;
alias HIcon = void*;
alias HCursor = void*;
alias HBitmap = void*;
alias HMonitor = void*;
alias HGlrc = void*;
alias HRgn = void*;
alias HBrush = void*;
alias HGdiObj = void*;
alias HRawInput = void*;
alias WndProc = fn iptr(HWnd*, uint, usz, iptr);

struct Point { int x, y; }
struct Rect { int left, top, right, bottom; }
struct Msg { HWnd* hwnd; uint message; usz wParam; iptr lParam; uint time; Point pt; }
struct WndClassW { uint style; WndProc wndProc; int clsExtra; int wndExtra; void* hInstance; HIcon hIcon; HCursor hCursor; HBrush hbrBackground; ushort* menuName; ushort* className; }

struct PixelFormatDescriptor
{
	ushort size, version;
	uint flags;
	char pixelType, colorBits, redBits, redShift, greenBits, greenShift, blueBits, blueShift;
	char alphaBits, alphaShift, accumBits, accumRedBits, accumGreenBits, accumBlueBits, accumAlphaBits;
	char depthBits, stencilBits, auxBuffers, layerType, reserved;
	uint layerMask, visibleMask, damageMask;
}
const char PFD_TYPE_RGBA = 0;
const char PFD_MAIN_PLANE = 0;
const uint PFD_DOUBLEBUFFER = 0x00000001;
const uint PFD_DRAW_TO_WINDOW = 0x00000004;
const uint PFD_SUPPORT_OPENGL = 0x00000020;
const uint PFD_GENERIC_FORMAT = 0x00000040;
const uint PFD_STEREO = 0x00000002;
const uint PFD_GENERIC_ACCELERATED = 0x00001000;

struct BitmapV5Header
{
	uint size;
	int width, height;
	ushort planes, bitCount;
	uint compression, sizeImage;
	int xPelsPerMeter, yPelsPerMeter;
	uint clrUsed, clrImportant;
	uint redMask, greenMask, blueMask, alphaMask;
	uint csType;
	int[9] endpoints;
	uint gammaRed, gammaGreen, gammaBlue;
	uint intent, profileData, profileSize, reserved;
}
const uint BI_RGB = 0;

struct IconInfo { int fIcon; uint xHotspot, yHotspot; HBitmap hbmMask; HBitmap hbmColor; }

struct DwmBlurBehind { uint dwFlags; int fEnable; HRgn hRgnBlur; int fTransitionOnMaximized; }

struct MonitorInfoExW { uint cbSize; Rect rcMonitor; Rect rcWork; uint dwFlags; ushort[32] szDevice; }
const uint MONITORINFOF_PRIMARY = 1;

struct DevModeW
{
	ushort[32] deviceName;
	ushort specVersion, driverVersion, size, driverExtra;
	uint fields;
	int positionX, positionY;
	uint displayOrientation, displayFixedOutput;
	short color, duplex, yResolution, ttOption, collate;
	ushort[32] formName;
	ushort logPixels;
	uint bitsPerPel;
	uint pelsWidth, pelsHeight;
	uint displayFlags;
	uint displayFrequency;
	uint icmMethod, icmIntent, mediaType, ditherType, reserved1, reserved2, panningWidth, panningHeight;
}
const uint DM_PELSWIDTH = 0x80000;
const uint DM_PELSHEIGHT = 0x100000;
const uint DM_DISPLAYFREQUENCY = 0x400000;
const uint DM_BITSPERPEL = 0x40000;
const int CDS_TEST = 0x2;
const int CDS_UPDATEREGISTRY = 0x1;
const int DISP_CHANGE_SUCCESSFUL = 0;
const int ENUM_CURRENT_SETTINGS = -1;

struct DisplayDeviceW { uint cb; ushort[32] deviceName; ushort[128] deviceString; uint stateFlags; ushort[128] deviceID; ushort[128] deviceKey; }
const uint DISPLAY_DEVICE_ACTIVE = 0x1;

struct WindowPlacement { uint length, flags, showCmd; Point ptMinPosition, ptMaxPosition; Rect rcNormalPosition; }
const uint SW_SHOWMINIMIZED = 2;
const uint SW_SHOWMAXIMIZED = 3;
const uint SW_MAXIMIZE = 3;
const uint SW_MINIMIZE = 6;
const uint SW_RESTORE = 9;
const uint SW_HIDE = 0;

struct RawInputDevice { ushort usagePage, usage; uint flags; HWnd* target; }
const uint RIDEV_REMOVE = 0x00000001;

struct RawInputHeader { uint dwType; uint dwSize; void* hDevice; usz wParam; }
struct RawMouse { ushort usFlags; uint usButtons; uint ulRawButtons; int lLastX; int lLastY; uint ulExtraInformation; }
struct RawInputMouse { RawInputHeader header; RawMouse mouse; }
const uint RID_INPUT = 0x10000003;
const uint RIM_TYPEMOUSE = 0;
const ushort MOUSE_MOVE_ABSOLUTE = 0x01;
const ushort MOUSE_VIRTUAL_DESKTOP = 0x02;
const int SM_CXSCREEN = 0;
const int SM_CYSCREEN = 1;
const int SM_XVIRTUALSCREEN = 76;
const int SM_YVIRTUALSCREEN = 77;
const int SM_CXVIRTUALSCREEN = 78;
const int SM_CYVIRTUALSCREEN = 79;

struct FlashWInfo { uint cbSize; HWnd* hwnd; uint dwFlags; uint uCount; uint dwTimeout; }
const uint FLASHW_STOP = 0;
const uint FLASHW_TRAY = 2;
const uint FLASHW_TIMERNOFG = 12;

struct MinMaxInfo { Point ptReserved, ptMaxSize, ptMaxPosition, ptMinTrackSize, ptMaxTrackSize; }

// ---- extern Win32 functions (kernel32/user32/gdi32/shell32/dwmapi) ----
extern fn HModule* loadLibraryA(char* name) @cname("LoadLibraryA");
extern fn int freeLibrary(HModule* hmod) @cname("FreeLibrary");
extern fn void* getProcAddress(HModule* hmod, char* name) @cname("GetProcAddress");
extern fn HModule* getModuleHandleW(ushort* name) @cname("GetModuleHandleW");
extern fn int multiByteToWideChar(uint codePage, uint flags, char* str, int cbMultiByte, ushort* out, int cchWideChar) @cname("MultiByteToWideChar");
extern fn int wideCharToMultiByte(uint codePage, uint flags, ushort* wstr, int cchWideChar, char* out, int cbMultiByte, char* defChar, int* usedDefChar) @cname("WideCharToMultiByte");

extern fn ushort registerClassW(WndClassW* wc) @cname("RegisterClassW");
extern fn int unregisterClassW(ushort* className, void* instance) @cname("UnregisterClassW");
extern fn HWnd* createWindowExW(uint exStyle, ushort* className, ushort* windowName, uint style, int x, int y, int w, int h, HWnd* parent, void* menu, void* instance, void* param) @cname("CreateWindowExW");
extern fn int destroyWindow(HWnd* wnd) @cname("DestroyWindow");
extern fn iptr defWindowProcW(HWnd* wnd, uint msg, usz wParam, iptr lParam) @cname("DefWindowProcW");
extern fn int peekMessageA(Msg* msg, HWnd* wnd, uint msgFilterMin, uint msgFilterMax, uint removeMsg) @cname("PeekMessageA");
extern fn int translateMessage(Msg* msg) @cname("TranslateMessage");
extern fn iptr dispatchMessageA(Msg* msg) @cname("DispatchMessageA");
extern fn int postMessageW(HWnd* wnd, uint msg, usz wParam, iptr lParam) @cname("PostMessageW");
extern fn int postMessage(HWnd* wnd, uint msg, usz wParam, iptr lParam) @cname("PostMessageA");
extern fn int sendMessage(HWnd* wnd, uint msg, usz wParam, iptr lParam) @cname("SendMessageA");
extern fn uint msgWaitForMultipleObjects(uint count, void* handles, int waitAll, uint milliseconds, uint wakeMask) @cname("MsgWaitForMultipleObjects");

extern fn void* setPropW(HWnd* wnd, ushort* name, void* data) @cname("SetPropW");
extern fn void* getPropW(HWnd* wnd, ushort* name) @cname("GetPropW");
extern fn void* removePropW(HWnd* wnd, ushort* name) @cname("RemovePropW");

extern fn int getWindowRect(HWnd* wnd, Rect* rect) @cname("GetWindowRect");
extern fn int getClientRect(HWnd* wnd, Rect* rect) @cname("GetClientRect");
extern fn int adjustWindowRectEx(Rect* rect, uint style, int menu, uint exStyle) @cname("AdjustWindowRectEx");
extern fn int setWindowPos(HWnd* wnd, HWnd* insertAfter, int x, int y, int cx, int cy, uint flags) @cname("SetWindowPos");
extern fn iptr setWindowLongW(HWnd* wnd, int index, iptr newLong) @cname("SetWindowLongW");
extern fn iptr setWindowLong(HWnd* wnd, int index, iptr newLong) @cname("SetWindowLongA");
extern fn iptr getWindowLongW(HWnd* wnd, int index) @cname("GetWindowLongW");
extern fn iptr getWindowLongPtr(HWnd* wnd, int index) @cname("GetWindowLongPtrA");
extern fn int showWindow(HWnd* wnd, int cmdShow) @cname("ShowWindow");
extern fn int isWindowVisible(HWnd* wnd) @cname("IsWindowVisible");
extern fn int isZoomed(HWnd* wnd) @cname("IsZoomed");
extern fn int getWindowPlacement(HWnd* wnd, WindowPlacement* placement) @cname("GetWindowPlacement");
extern fn int bringWindowToTop(HWnd* wnd) @cname("BringWindowToTop");
extern fn HWnd* setForegroundWindow(HWnd* wnd) @cname("SetForegroundWindow");
extern fn HWnd* setFocus(HWnd* wnd) @cname("SetFocus");
extern fn int setWindowTextW(HWnd* wnd, ushort* text) @cname("SetWindowTextW");
extern fn int setLayeredWindowAttributes(HWnd* wnd, uint crKey, char alpha, uint flags) @cname("SetLayeredWindowAttributes");
extern fn int getLayeredWindowAttributes(HWnd* wnd, uint* crKey, char* alpha, uint* flags) @cname("GetLayeredWindowAttributes");
extern fn HWnd* monitorFromWindow(HWnd* wnd, uint flags) @cname("MonitorFromWindow");
extern fn int flashWindowEx(FlashWInfo* info) @cname("FlashWindowEx");
extern fn int dragAcceptFiles(HWnd* wnd, int accept) @cname("DragAcceptFiles");

extern fn HDc* getDC(HWnd* wnd) @cname("GetDC");
extern fn int releaseDC(HWnd* wnd, HDc* dc) @cname("ReleaseDC");
extern fn int choosePixelFormat(HDc* dc, PixelFormatDescriptor* pfd) @cname("ChoosePixelFormat");
extern fn int setPixelFormat(HDc* dc, int format, PixelFormatDescriptor* pfd) @cname("SetPixelFormat");
extern fn int describePixelFormat(HDc* dc, int format, uint bytes, PixelFormatDescriptor* pfd) @cname("DescribePixelFormat");
extern fn int swapBuffers(HDc* dc) @cname("SwapBuffers");
extern fn HBitmap createDIBSection(HDc* dc, void* bmi, uint usage, void** bits, void* section, uint offset) @cname("CreateDIBSection");
extern fn HDc* createCompatibleDC(HDc* dc) @cname("CreateCompatibleDC");
extern fn HDc* createDCW(ushort* driver, ushort* device, ushort* output, void* initData) @cname("CreateDCW");
extern fn int deleteDC(HDc* dc) @cname("DeleteDC");
extern fn void* selectObject(HDc* dc, void* obj) @cname("SelectObject");
extern fn int deleteObject(void* obj) @cname("DeleteObject");
extern fn int bitBlt(HDc* dst, int x, int y, int w, int h, HDc* src, int x1, int y1, uint rop) @cname("BitBlt");
extern fn int getDeviceCaps(HDc* dc, int index) @cname("GetDeviceCaps");
extern fn int getDeviceGammaRamp(HDc* dc, void* ramp) @cname("GetDeviceGammaRamp");
extern fn int setDeviceGammaRamp(HDc* dc, void* ramp) @cname("SetDeviceGammaRamp");
extern fn HBitmap createBitmap(int w, int h, uint planes, uint bitCount, void* bits) @cname("CreateBitmap");
extern fn HIcon createIconIndirect(IconInfo* info) @cname("CreateIconIndirect");
extern fn int destroyIcon(HIcon icon) @cname("DestroyIcon");
extern fn HCursor loadCursorA(void* instance, char* name) @cname("LoadCursorA");
extern fn HIcon loadIconA(void* instance, char* name) @cname("LoadIconA");
extern fn void* loadImageA(void* instance, char* name, uint c_type, int cx, int cy, uint fuLoad) @cname("LoadImageA");
extern fn int destroyCursor(HCursor cursor) @cname("DestroyCursor");
extern fn int setCursor(HCursor cursor) @cname("SetCursor");
extern fn iptr setClassLongPtrA(HWnd* wnd, int index, iptr newLong) @cname("SetClassLongPtrA");
extern fn int getCursorPos(Point* p) @cname("GetCursorPos");
extern fn int setCursorPos(int x, int y) @cname("SetCursorPos");
extern fn int screenToClient(HWnd* wnd, Point* p) @cname("ScreenToClient");
extern fn int clientToScreen(HWnd* wnd, Point* p) @cname("ClientToScreen");
extern fn int clipCursor(Rect* rect) @cname("ClipCursor");
extern fn int registerRawInputDevices(RawInputDevice* devices, uint numDevices, uint size) @cname("RegisterRawInputDevices");
extern fn uint getRawInputData(HRawInput hRawInput, uint command, void* data, uint* size, uint headerSize) @cname("GetRawInputData");
extern fn int getSystemMetrics(int index) @cname("GetSystemMetrics");

extern fn int enumDisplayMonitors(HDc* dc, Rect* clip, void* proc, iptr data) @cname("EnumDisplayMonitors");
extern fn int getMonitorInfoA(HMonitor mon, void* info) @cname("GetMonitorInfoA");
extern fn int getMonitorInfoW(HMonitor mon, void* info) @cname("GetMonitorInfoW");
extern fn int enumDisplayDevicesW(ushort* device, uint devNum, DisplayDeviceW* dd, uint flags) @cname("EnumDisplayDevicesW");
extern fn int enumDisplaySettingsW(ushort* device, uint modeNum, DevModeW* dm) @cname("EnumDisplaySettingsW");
extern fn int changeDisplaySettingsExW(ushort* device, DevModeW* dm, void* wnd, uint flags, void* param) @cname("ChangeDisplaySettingsExW");
extern fn void setProcessDPIAware() @cname("SetProcessDPIAware");
extern fn uint regGetValueW(void* key, ushort* subKey, ushort* value, uint flags, uint* type, void* data, uint* dataSize) @cname("RegGetValueW");

extern fn int openClipboard(HWnd* wnd) @cname("OpenClipboard");
extern fn int closeClipboard() @cname("CloseClipboard");
extern fn int emptyClipboard() @cname("EmptyClipboard");
extern fn void* getClipboardData(uint format) @cname("GetClipboardData");
extern fn void* setClipboardData(uint format, void* mem) @cname("SetClipboardData");
extern fn void* globalAlloc(uint flags, usz bytes) @cname("GlobalAlloc");
extern fn void* globalLock(void* mem) @cname("GlobalLock");
extern fn int globalUnlock(void* mem) @cname("GlobalUnlock");
extern fn void* globalFree(void* mem) @cname("GlobalFree");

extern fn int getKeyboardState(char* state) @cname("GetKeyboardState");
extern fn short getKeyState(int vKey) @cname("GetKeyState");
extern fn uint mapVirtualKeyW(uint code, uint mapType) @cname("MapVirtualKeyW");
extern fn void* getKeyboardLayout(uint threadId) @cname("GetKeyboardLayout");
extern fn int toUnicodeEx(uint vk, uint scanCode, char* keyState, ushort* buf, int bufSize, uint flags, void* layout) @cname("ToUnicodeEx");

extern fn int dragQueryPoint(void* drop, Point* p) @cname("DragQueryPoint");
extern fn uint dragQueryFileW(void* drop, uint fileIndex, ushort* file, uint cch) @cname("DragQueryFileW");
extern fn void dragFinish(void* drop) @cname("DragFinish");

extern fn int getUpdateRect(HWnd* wnd, Rect* rect, int erase) @cname("GetUpdateRect");
extern fn int beginPaint(HWnd* wnd, void* ps) @cname("BeginPaint");
extern fn int endPaint(HWnd* wnd, void* ps) @cname("EndPaint");
extern fn uint setTimer(HWnd* wnd, uptr idEvent, uint elapse, void* proc) @cname("SetTimer");
extern fn int killTimer(HWnd* wnd, uptr idEvent) @cname("KillTimer");

extern fn int dwmEnableBlurBehindWindow(HWnd* wnd, void* bb) @cname("DwmEnableBlurBehindWindow");
extern fn int dwmSetWindowAttribute(HWnd* wnd, uint attr, void* value, uint size) @cname("DwmSetWindowAttribute");

extern fn uint timeBeginPeriod(uint period) @cname("timeBeginPeriod");
extern fn uint timeEndPeriod(uint period) @cname("timeEndPeriod");

alias RgfwWglCreateContextProc = fn HGlrc(HDc*);
alias RgfwWglDeleteContextProc = fn int(HGlrc);
alias RgfwWglMakeCurrentProc = fn int(HDc*, HGlrc);
alias RgfwWglGetCurrentDcProc = fn HDc*();
alias RgfwWglGetCurrentContextProc = fn HGlrc();
alias RgfwWglShareListsProc = fn int(HGlrc, HGlrc);
alias RgfwWglCreateContextAttribsArbProc = fn HGlrc(HDc*, HGlrc, int*);
alias RgfwWglChoosePixelFormatArbProc = fn int(HDc*, int*, float*, uint, int*, uint*);
alias RgfwWglSwapIntervalExtProc = fn int(int);

RgfwWglCreateContextProc rgfw_wglCreateContext;
RgfwWglDeleteContextProc rgfw_wglDeleteContext;
RgfwWglMakeCurrentProc rgfw_wglMakeCurrent;
RgfwWglGetCurrentDcProc rgfw_wglGetCurrentDC;
RgfwWglGetCurrentContextProc rgfw_wglGetCurrentContext;
RgfwWglShareListsProc rgfw_wglShareLists;
RgfwWglCreateContextAttribsArbProc rgfw_wglCreateContextAttribsARB;
RgfwWglChoosePixelFormatArbProc rgfw_wglChoosePixelFormatARB;
RgfwWglSwapIntervalExtProc rgfw_wglSwapIntervalEXT;

macro bool rgfw_bool(x) => x ? (bool)1 : (bool)0;
macro uint rgfw_bit(x) => (uint)1 << (uint)(x);
macro void* rgfw_memcpy(void* dest, void* src, usz count) => memcpy(dest, src, count);
macro void* rgfw_memzero(void* ptr, usz count) => memset(ptr, 0, count);
macro float rgfw_min(float a, float b) => a < b ? a : b;

fn void* rgfw_alloc_impl(usz size) => malloc(size);
fn void rgfw_free_impl(void* ptr) { if (ptr != null) free(ptr); }


alias RgfwInitFlags = char;
alias RgfwFormat = char;
alias RgfwModeRequest = char;
alias RgfwKey = char;
alias RgfwMouseButton = char;
alias RgfwKeymod = char;
alias RgfwDndActionType = char;
alias RgfwDataTransferType = char;
alias RgfwEventType = char;
alias RgfwEventFlag = uint;
alias RgfwEventWait = int;
alias RgfwWindowFlags = uint;
alias RgfwIcon = char;
alias RgfwMouseIcon = char;
alias RgfwFlashRequest = char;
alias RgfwDebugType = char;
alias RgfwErrorCode = char;
alias RgfwGlReleaseBehavior = int;
alias RgfwGlProfile = int;
alias RgfwGlRenderer = int;
alias RgfwGfxContextType = uint;

const RGFW_TRUE = (bool)1;
const RGFW_FALSE = (bool)0;

const RgfwInitFlags RGFW_INIT_OPEN_GL = 1;
const RgfwInitFlags RGFW_INIT_EGL = 2;
const RgfwInitFlags RGFW_INIT_VULKAN = 4;
const RgfwInitFlags RGFW_INIT_X11 = 8;

const RgfwFormat RGFW_FORMAT_RGB8 = 0;
const RgfwFormat RGFW_FORMAT_BGR8 = 1;
const RgfwFormat RGFW_FORMAT_RGBA8 = 2;
const RgfwFormat RGFW_FORMAT_ARGB8 = 3;
const RgfwFormat RGFW_FORMAT_BGRA8 = 4;
const RgfwFormat RGFW_FORMAT_ABGR8 = 5;
const RgfwFormat RGFW_FORMAT_COUNT = 6;

const RgfwModeRequest RGFW_MONITOR_SCALE = 1;
const RgfwModeRequest RGFW_MONITOR_REFRESH = 2;
const RgfwModeRequest RGFW_MONITOR_RGB = 4;
const RgfwModeRequest RGFW_MONITOR_ALL = 7;

const RgfwKey RGFW_KEY_NULL = 0;
const RgfwKey RGFW_KEY_ESCAPE = 27;
const RgfwKey RGFW_KEY_BACKTICK = '`';
const RgfwKey RGFW_KEY0 = '0';
const RgfwKey RGFW_KEY1 = '1';
const RgfwKey RGFW_KEY2 = '2';
const RgfwKey RGFW_KEY3 = '3';
const RgfwKey RGFW_KEY4 = '4';
const RgfwKey RGFW_KEY5 = '5';
const RgfwKey RGFW_KEY6 = '6';
const RgfwKey RGFW_KEY7 = '7';
const RgfwKey RGFW_KEY8 = '8';
const RgfwKey RGFW_KEY9 = '9';
const RgfwKey RGFW_KEY_MINUS = '-';
const RgfwKey RGFW_KEY_EQUAL = '=';
const RgfwKey RGFW_KEY_EQUALS = '=';
const RgfwKey RGFW_KEY_BACK_SPACE = 8;
const RgfwKey RGFW_KEY_TAB = 9;
const RgfwKey RGFW_KEY_SPACE = ' ';
const RgfwKey RGFW_KEY_A = 'a';
const RgfwKey RGFW_KEY_B = 'b';
const RgfwKey RGFW_KEY_C = 'c';
const RgfwKey RGFW_KEY_D = 'd';
const RgfwKey RGFW_KEY_E = 'e';
const RgfwKey RGFW_KEY_F = 'f';
const RgfwKey RGFW_KEY_G = 'g';
const RgfwKey RGFW_KEY_H = 'h';
const RgfwKey RGFW_KEY_I = 'i';
const RgfwKey RGFW_KEY_J = 'j';
const RgfwKey RGFW_KEY_K = 'k';
const RgfwKey RGFW_KEY_L = 'l';
const RgfwKey RGFW_KEY_M = 'm';
const RgfwKey RGFW_KEY_N = 'n';
const RgfwKey RGFW_KEY_O = 'o';
const RgfwKey RGFW_KEY_P = 'p';
const RgfwKey RGFW_KEY_Q = 'q';
const RgfwKey RGFW_KEY_R = 'r';
const RgfwKey RGFW_KEY_S = 's';
const RgfwKey RGFW_KEY_T = 't';
const RgfwKey RGFW_KEY_U = 'u';
const RgfwKey RGFW_KEY_V = 'v';
const RgfwKey RGFW_KEY_W = 'w';
const RgfwKey RGFW_KEY_X = 'x';
const RgfwKey RGFW_KEY_Y = 'y';
const RgfwKey RGFW_KEY_Z = 'z';
const RgfwKey RGFW_KEY_PERIOD = '.';
const RgfwKey RGFW_KEY_COMMA = ',';
const RgfwKey RGFW_KEY_SLASH = '/';
const RgfwKey RGFW_KEY_BRACKET = '[';
const RgfwKey RGFW_KEY_CLOSE_BRACKET = ']';
const RgfwKey RGFW_KEY_SEMICOLON = ';';
const RgfwKey RGFW_KEY_APOSTROPHE = '\'';
const RgfwKey RGFW_KEY_BACK_SLASH = '\\';
const RgfwKey RGFW_KEY_RETURN = 10;
const RgfwKey RGFW_KEY_ENTER = 10;
const RgfwKey RGFW_KEY_DELETE = 127;
const RgfwKey RGFW_KEY_F1 = 128;
const RgfwKey RGFW_KEY_F2 = 129;
const RgfwKey RGFW_KEY_F3 = 130;
const RgfwKey RGFW_KEY_F4 = 131;
const RgfwKey RGFW_KEY_F5 = 132;
const RgfwKey RGFW_KEY_F6 = 133;
const RgfwKey RGFW_KEY_F7 = 134;
const RgfwKey RGFW_KEY_F8 = 135;
const RgfwKey RGFW_KEY_F9 = 136;
const RgfwKey RGFW_KEY_F10 = 137;
const RgfwKey RGFW_KEY_F11 = 138;
const RgfwKey RGFW_KEY_F12 = 139;
const RgfwKey RGFW_KEY_F13 = 140;
const RgfwKey RGFW_KEY_F14 = 141;
const RgfwKey RGFW_KEY_F15 = 142;
const RgfwKey RGFW_KEY_F16 = 143;
const RgfwKey RGFW_KEY_F17 = 144;
const RgfwKey RGFW_KEY_F18 = 145;
const RgfwKey RGFW_KEY_F19 = 146;
const RgfwKey RGFW_KEY_F20 = 147;
const RgfwKey RGFW_KEY_F21 = 148;
const RgfwKey RGFW_KEY_F22 = 149;
const RgfwKey RGFW_KEY_F23 = 150;
const RgfwKey RGFW_KEY_F24 = 151;
const RgfwKey RGFW_KEY_F25 = 152;
const RgfwKey RGFW_KEY_CAPS_LOCK = 153;
const RgfwKey RGFW_KEY_SHIFT_L = 154;
const RgfwKey RGFW_KEY_CONTROL_L = 155;
const RgfwKey RGFW_KEY_ALT_L = 156;
const RgfwKey RGFW_KEY_SUPER_L = 157;
const RgfwKey RGFW_KEY_SHIFT_R = 158;
const RgfwKey RGFW_KEY_CONTROL_R = 159;
const RgfwKey RGFW_KEY_ALT_R = 160;
const RgfwKey RGFW_KEY_SUPER_R = 161;
const RgfwKey RGFW_KEY_UP = 162;
const RgfwKey RGFW_KEY_DOWN = 163;
const RgfwKey RGFW_KEY_LEFT = 164;
const RgfwKey RGFW_KEY_RIGHT = 165;
const RgfwKey RGFW_KEY_INSERT = 166;
const RgfwKey RGFW_KEY_MENU = 167;
const RgfwKey RGFW_KEY_END = 168;
const RgfwKey RGFW_KEY_HOME = 169;
const RgfwKey RGFW_KEY_PAGE_UP = 170;
const RgfwKey RGFW_KEY_PAGE_DOWN = 171;
const RgfwKey RGFW_KEY_NUM_LOCK = 172;
const RgfwKey RGFW_KEY_PAD_SLASH = 173;
const RgfwKey RGFW_KEY_PAD_MULTIPLY = 174;
const RgfwKey RGFW_KEY_PAD_PLUS = 175;
const RgfwKey RGFW_KEY_PAD_MINUS = 176;
const RgfwKey RGFW_KEY_PAD_EQUAL = 177;
const RgfwKey RGFW_KEY_PAD_EQUALS = 177;
const RgfwKey RGFW_KEY_PAD1 = 178;
const RgfwKey RGFW_KEY_PAD2 = 179;
const RgfwKey RGFW_KEY_PAD3 = 180;
const RgfwKey RGFW_KEY_PAD4 = 181;
const RgfwKey RGFW_KEY_PAD5 = 182;
const RgfwKey RGFW_KEY_PAD6 = 183;
const RgfwKey RGFW_KEY_PAD7 = 184;
const RgfwKey RGFW_KEY_PAD8 = 185;
const RgfwKey RGFW_KEY_PAD9 = 186;
const RgfwKey RGFW_KEY_PAD0 = 187;
const RgfwKey RGFW_KEY_PAD_PERIOD = 188;
const RgfwKey RGFW_KEY_PAD_RETURN = 189;
const RgfwKey RGFW_KEY_SCROLL_LOCK = 190;
const RgfwKey RGFW_KEY_PRINT_SCREEN = 191;
const RgfwKey RGFW_KEY_PAUSE = 192;
const RgfwKey RGFW_KEY_WORLD1 = 193;
const RgfwKey RGFW_KEY_WORLD2 = 194;

const RGFW_KEY_LAST = 256;

const RgfwMouseButton RGFW_MOUSE_LEFT = 0;
const RgfwMouseButton RGFW_MOUSE_MIDDLE = 1;
const RgfwMouseButton RGFW_MOUSE_RIGHT = 2;
const RgfwMouseButton RGFW_MOUSE_MISC1 = 3;
const RgfwMouseButton RGFW_MOUSE_MISC2 = 4;
const RgfwMouseButton RGFW_MOUSE_MISC3 = 5;
const RgfwMouseButton RGFW_MOUSE_MISC4 = 6;
const RgfwMouseButton RGFW_MOUSE_MISC5 = 7;
const RgfwMouseButton RGFW_MOUSE_FINAL = 8;

const RgfwKeymod RGFW_MOD_CAPS_LOCK = 1;
const RgfwKeymod RGFW_MOD_NUM_LOCK = 2;
const RgfwKeymod RGFW_MOD_CONTROL = 4;
const RgfwKeymod RGFW_MOD_ALT = 8;
const RgfwKeymod RGFW_MOD_SHIFT = 16;
const RgfwKeymod RGFW_MOD_SUPER = 32;
const RgfwKeymod RGFW_MOD_SCROLL_LOCK = 64;

const RgfwDndActionType RGFW_DND_ACTION_NONE = 0;
const RgfwDndActionType RGFW_DND_ACTION_ENTER = 1;
const RgfwDndActionType RGFW_DND_ACTION_MOVE = 2;
const RgfwDndActionType RGFW_DND_ACTION_EXIT = 3;

const RgfwDataTransferType RGFW_DATA_NONE = 0;
const RgfwDataTransferType RGFW_DATA_TEXT = 1;
const RgfwDataTransferType RGFW_DATA_FILE = 2;
const RgfwDataTransferType RGFW_DATA_URL = 3;
const RgfwDataTransferType RGFW_DATA_IMAGE = 4;
const RgfwDataTransferType RGFW_DATA_UNKNOWN = 5;

const RgfwEventType RGFW_EVENT_NONE = 0;
const RgfwEventType RGFW_KEY_PRESSED = 1;
const RgfwEventType RGFW_KEY_RELEASED = 2;
const RgfwEventType RGFW_KEY_CHAR = 3;
const RgfwEventType RGFW_MOUSE_BUTTON_PRESSED = 4;
const RgfwEventType RGFW_MOUSE_BUTTON_RELEASED = 5;
const RgfwEventType RGFW_MOUSE_SCROLL = 6;
const RgfwEventType RGFW_MOUSE_MOTION = 7;
const RgfwEventType RGFW_MOUSE_RAW_MOTION = 8;
const RgfwEventType RGFW_MOUSE_ENTER = 9;
const RgfwEventType RGFW_MOUSE_LEAVE = 10;
const RgfwEventType RGFW_WINDOW_MOVED = 11;
const RgfwEventType RGFW_WINDOW_RESIZED = 12;
const RgfwEventType RGFW_WINDOW_FOCUS_IN = 13;
const RgfwEventType RGFW_WINDOW_FOCUS_OUT = 14;
const RgfwEventType RGFW_WINDOW_REFRESH = 15;
const RgfwEventType RGFW_WINDOW_CLOSE = 16;
const RgfwEventType RGFW_WINDOW_MAXIMIZED = 17;
const RgfwEventType RGFW_WINDOW_MINIMIZED = 18;
const RgfwEventType RGFW_WINDOW_RESTORED = 19;
const RgfwEventType RGFW_DATA_DROP = 20;
const RgfwEventType RGFW_DATA_DRAG = 21;
const RgfwEventType RGFW_SCALE_UPDATED = 22;
const RgfwEventType RGFW_MONITOR_CONNECTED = 23;
const RgfwEventType RGFW_MONITOR_DISCONNECTED = 24;
const RgfwEventType RGFW_EVENT_COUNT = 25;
const RgfwEventType RGFW_MOUSE_POS_CHANGED = 7;

const RgfwEventFlag RGFW_KEY_PRESSED_FLAG = 2;
const RgfwEventFlag RGFW_KEY_RELEASED_FLAG = 4;
const RgfwEventFlag RGFW_KEY_CHAR_FLAG = 8;
const RgfwEventFlag RGFW_MOUSE_SCROLL_FLAG = 64;
const RgfwEventFlag RGFW_MOUSE_BUTTON_PRESSED_FLAG = 16;
const RgfwEventFlag RGFW_MOUSE_BUTTON_RELEASED_FLAG = 32;
const RgfwEventFlag RGFW_MOUSE_MOTION_FLAG = 128;
const RgfwEventFlag RGFW_MOUSE_RAW_MOTION_FLAG = 256;
const RgfwEventFlag RGFW_MOUSE_ENTER_FLAG = 512;
const RgfwEventFlag RGFW_MOUSE_LEAVE_FLAG = 1024;
const RgfwEventFlag RGFW_WINDOW_MOVED_FLAG = 2048;
const RgfwEventFlag RGFW_WINDOW_RESIZED_FLAG = 4096;
const RgfwEventFlag RGFW_WINDOW_FOCUS_IN_FLAG = 8192;
const RgfwEventFlag RGFW_WINDOW_FOCUS_OUT_FLAG = 16384;
const RgfwEventFlag RGFW_WINDOW_REFRESH_FLAG = 32768;
const RgfwEventFlag RGFW_WINDOW_MAXIMIZED_FLAG = 131072;
const RgfwEventFlag RGFW_WINDOW_MINIMIZED_FLAG = 262144;
const RgfwEventFlag RGFW_WINDOW_RESTORED_FLAG = 524288;
const RgfwEventFlag RGFW_SCALE_UPDATED_FLAG = 4194304;
const RgfwEventFlag RGFW_WINDOW_CLOSE_FLAG = 65536;
const RgfwEventFlag RGFW_DATA_DROP_FLAG = 1048576;
const RgfwEventFlag RGFW_DATA_DRAG_FLAG = 2097152;
const RgfwEventFlag RGFW_MONITOR_CONNECTED_FLAG = 8388608;
const RgfwEventFlag RGFW_MONITOR_DISCONNECTED_FLAG = 16777216;
const RgfwEventFlag RGFW_MOUSE_POS_CHANGED_FLAG = 128;
const RgfwEventFlag RGFW_KEY_EVENTS_FLAG = 14;
const RgfwEventFlag RGFW_MOUSE_EVENTS_FLAG = 2016;
const RgfwEventFlag RGFW_WINDOW_EVENTS_FLAG = 5029888;
const RgfwEventFlag RGFW_WINDOW_FOCUS_EVENTS_FLAG = 24576;
const RgfwEventFlag RGFW_DATA_DRAG_DROP_EVENTS_FLAG = 3145728;
const RgfwEventFlag RGFW_MONITOR_EVENTS_FLAG = 25165824;
const RgfwEventFlag RGFW_ALL_EVENT_FLAGS = 33488894;

const RgfwEventWait RGFW_EVENT_NO_WAIT = 0;
const RgfwEventWait RGFW_EVENT_WAIT_NEXT = -1;

const RgfwWindowFlags RGFW_WINDOW_NO_BORDER = 1;
const RgfwWindowFlags RGFW_WINDOW_NO_RESIZE = 2;
const RgfwWindowFlags RGFW_WINDOW_ALLOW_DND = 4;
const RgfwWindowFlags RGFW_WINDOW_HIDE_MOUSE = 8;
const RgfwWindowFlags RGFW_WINDOW_FULLSCREEN = 16;
const RgfwWindowFlags RGFW_WINDOW_TRANSLUCENT = 32;
const RgfwWindowFlags RGFW_WINDOW_TRANSPARENT = 32;
const RgfwWindowFlags RGFW_WINDOW_CENTER = 64;
const RgfwWindowFlags RGFW_WINDOW_RAW_MOUSE = 128;
const RgfwWindowFlags RGFW_WINDOW_SCALE_TO_MONITOR = 256;
const RgfwWindowFlags RGFW_WINDOW_HIDE = 512;
const RgfwWindowFlags RGFW_WINDOW_MAXIMIZE = 1024;
const RgfwWindowFlags RGFW_WINDOW_CENTER_CURSOR = 2048;
const RgfwWindowFlags RGFW_WINDOW_FLOATING = 4096;
const RgfwWindowFlags RGFW_WINDOW_FOCUS_ON_SHOW = 8192;
const RgfwWindowFlags RGFW_WINDOW_MINIMIZE = 16384;
const RgfwWindowFlags RGFW_WINDOW_FOCUS = 32768;
const RgfwWindowFlags RGFW_WINDOW_CAPTURE_MOUSE = 65536;
const RgfwWindowFlags RGFW_WINDOW_OPEN_GL = 131072;
const RgfwWindowFlags RGFW_WINDOW_EGL = 262144;
const RgfwWindowFlags RGFW_WINDOWED_FULLSCREEN = 1025;
const RgfwWindowFlags RGFW_WINDOW_CAPTURE_RAW_MOUSE = 65664;

const RgfwIcon RGFW_ICON_TASKBAR = 1;
const RgfwIcon RGFW_ICON_WINDOW = 2;
const RgfwIcon RGFW_ICON_BOTH = 3;

const RgfwMouseIcon RGFW_MOUSE_NORMAL = 0;
const RgfwMouseIcon RGFW_MOUSE_ARROW = 1;
const RgfwMouseIcon RGFW_MOUSE_IBEAM = 2;
const RgfwMouseIcon RGFW_MOUSE_TEXT = 2;
const RgfwMouseIcon RGFW_MOUSE_CROSSHAIR = 3;
const RgfwMouseIcon RGFW_MOUSE_POINTING_HAND = 4;
const RgfwMouseIcon RGFW_MOUSE_RESIZE_EW = 5;
const RgfwMouseIcon RGFW_MOUSE_RESIZE_NS = 6;
const RgfwMouseIcon RGFW_MOUSE_RESIZE_NWSE = 7;
const RgfwMouseIcon RGFW_MOUSE_RESIZE_NESW = 8;
const RgfwMouseIcon RGFW_MOUSE_RESIZE_NW = 9;
const RgfwMouseIcon RGFW_MOUSE_RESIZE_N = 10;
const RgfwMouseIcon RGFW_MOUSE_RESIZE_NE = 11;
const RgfwMouseIcon RGFW_MOUSE_RESIZE_E = 12;
const RgfwMouseIcon RGFW_MOUSE_RESIZE_SE = 13;
const RgfwMouseIcon RGFW_MOUSE_RESIZE_S = 14;
const RgfwMouseIcon RGFW_MOUSE_RESIZE_SW = 15;
const RgfwMouseIcon RGFW_MOUSE_RESIZE_W = 16;
const RgfwMouseIcon RGFW_MOUSE_RESIZE_ALL = 17;
const RgfwMouseIcon RGFW_MOUSE_NOT_ALLOWED = 18;
const RgfwMouseIcon RGFW_MOUSE_WAIT = 19;
const RgfwMouseIcon RGFW_MOUSE_PROGRESS = 20;
const RgfwMouseIcon RGFW_MOUSE_ICON_COUNT = 21;
const RgfwMouseIcon RGFW_MOUSE_ICON_FINAL = 16;

const RgfwFlashRequest RGFW_FLASH_CANCEL = 0;
const RgfwFlashRequest RGFW_FLASH_BRIEFLY = 1;
const RgfwFlashRequest RGFW_FLASH_UNTIL_FOCUSED = 2;

const RgfwDebugType RGFW_TYPE_ERROR = 0;
const RgfwDebugType RGFW_TYPE_WARNING = 1;
const RgfwDebugType RGFW_TYPE_INFO = 2;

const RgfwErrorCode RGFW_NO_ERROR = 0;
const RgfwErrorCode RGFW_ERR_OUT_OF_MEMORY = 1;
const RgfwErrorCode RGFW_ERR_OPEN_GL_CONTEXT = 2;
const RgfwErrorCode RGFW_ERR_EGL_CONTEXT = 3;
const RgfwErrorCode RGFW_ERR_WAYLAND = 4;
const RgfwErrorCode RGFW_ERR_X11 = 5;
const RgfwErrorCode RGFW_ERR_DIRECT_X_CONTEXT = 6;
const RgfwErrorCode RGFW_ERR_IO_KIT = 7;
const RgfwErrorCode RGFW_ERR_CLIPBOARD = 8;
const RgfwErrorCode RGFW_ERR_FAILED_FUNC_LOAD = 9;
const RgfwErrorCode RGFW_ERR_BUFFER = 10;
const RgfwErrorCode RGFW_ERR_METAL = 11;
const RgfwErrorCode RGFW_ERR_PLATFORM = 12;
const RgfwErrorCode RGFW_ERR_EVENT_QUEUE = 13;
const RgfwErrorCode RGFW_ERR_NO_INIT = 14;
const RgfwErrorCode RGFW_INFO_WINDOW = 15;
const RgfwErrorCode RGFW_INFO_BUFFER = 16;
const RgfwErrorCode RGFW_INFO_GLOBAL = 17;
const RgfwErrorCode RGFW_INFO_OPEN_GL = 18;
const RgfwErrorCode RGFW_WARNING_WAYLAND = 19;
const RgfwErrorCode RGFW_WARNING_OPEN_GL = 20;

typedef RgfwMouse = void;
alias RgfwProc = fn void();
alias RgfwConvertImageDataFunc = fn void(char* dest_data, char* src_data, RgfwColorLayout* srcLayout, RgfwColorLayout* destLayout, usz count);
alias RgfwGenericFunc = fn void(RgfwEvent* e);
alias RgfwDebugFunc = fn void(RgfwDebugInfo* info);

struct RgfwColorLayout { int r, g, b, a; uint channels; }

struct RgfwGammaRamp
{
	ushort* red;
	ushort* green;
	ushort* blue;
	usz count;
}

struct RgfwMonitorMode
{
	int w, h;
	float refreshRate;
	char red, blue, green;
	void* src;
}

struct RgfwMonitor
{
	int x, y;
	char[128] name;
	float scaleX, scaleY;
	float pixelRatio;
	float physW, physH;
	RgfwMonitorMode mode;
	void* userPtr;
	RgfwMonitorNode* node;
}

struct RgfwDataTransfer
{
	char* data;
	usz length;
	RgfwDataTransferType type;
}

struct RgfwDataDropNode
{
	char* data;
	usz length;
	RgfwDataTransferType type;
	RgfwDataDropNode* next;
}

struct RgfwCommonEvent { RgfwEventType type; RgfwWindow* win; }
struct RgfwWindowFocusEvent { RgfwEventType type; RgfwWindow* win; bool state; }
struct RgfwMouseButtonEvent { RgfwEventType type; RgfwWindow* win; RgfwMouseButton value; bool state; }
struct RgfwMouseDeltaEvent { RgfwEventType type; RgfwWindow* win; float x, y; }
struct RgfwMouseMotionEvent { RgfwEventType type; RgfwWindow* win; int x, y; bool inWindow; }
struct RgfwKeyEvent { RgfwEventType type; RgfwWindow* win; RgfwKey value; bool repeat; RgfwKeymod mod; bool state; }
struct RgfwKeyCharEvent { RgfwEventType type; RgfwWindow* win; uint value; }
struct RgfwDataDropEvent { RgfwEventType type; RgfwWindow* win; RgfwDataDropNode* value; }
struct RgfwDataDragEvent { RgfwEventType type; RgfwWindow* win; int x, y; RgfwDndActionType action; RgfwDataTransferType dataType; }
struct RgfwScaleUpdatedEvent { RgfwEventType type; RgfwWindow* win; float x, y; }
struct RgfwMonitorEvent { RgfwEventType type; RgfwWindow* win; RgfwMonitor* monitor; bool state; }
struct RgfwWindowUpdateEvent { RgfwEventType type; RgfwWindow* win; int x, y, w, h; }

union RgfwEvent
{
	RgfwEventType type;
	RgfwCommonEvent common;
	RgfwWindowFocusEvent focus;
	RgfwWindowUpdateEvent update;
	RgfwMouseButtonEvent button;
	RgfwMouseDeltaEvent delta;
	RgfwMouseMotionEvent mouse;
	RgfwKeyEvent key;
	RgfwKeyCharEvent keyChar;
	RgfwDataDropEvent drop;
	RgfwDataDragEvent drag;
	RgfwScaleUpdatedEvent scale;
	RgfwMonitorEvent monitor;
}

struct RgfwCallbacks
{
	RgfwGenericFunc[25] arr;
}

struct RgfwDebugInfo
{
	RgfwDebugType type;
	RgfwErrorCode code;
	char* msg;
}

struct RgfwGlContext { void* ctx; } // HGLRC
struct RgfwEglContext { void* ctx; void* surface; void* eglWindow; }

const RgfwGlReleaseBehavior RGFW_GL_RELEASE_FLUSH = 0;
const RgfwGlReleaseBehavior RGFW_GL_RELEASE_NONE = 1;

const RgfwGlProfile RGFW_GL_CORE = 0;
const RgfwGlProfile RGFW_GL_FORWARD_COMPATIBILITY = 1;
const RgfwGlProfile RGFW_GL_COMPATIBILITY = 2;
const RgfwGlProfile RGFW_GL_ES = 3;
const RgfwGlProfile RGFW_GL_WEB = 4;

const RgfwGlRenderer RGFW_GL_ACCELERATED = 0;
const RgfwGlRenderer RGFW_GL_SOFTWARE = 1;

struct RgfwGlHints
{
	int stencil;
	int samples;
	int stereo;
	int auxBuffers;
	int doubleBuffer;
	int red, green, blue, alpha;
	int depth;
	int accumRed, accumGreen, accumBlue, accumAlpha;
	bool sRGB;
	bool robustness;
	bool debug;
	bool noError;
	RgfwGlReleaseBehavior releaseBehavior;
	RgfwGlProfile profile;
	int major, minor;
	RgfwGlContext* share;
	RgfwEglContext* shareEGL;
	RgfwGlRenderer renderer;
}

union RgfwGfxContext
{
	RgfwGlContext* native;
	RgfwEglContext* egl;
}

const RgfwGfxContextType RGFW_GFX_NATIVE_OPEN_GL = 1;
const RgfwGfxContextType RGFW_GFX_EGL = 2;
const RgfwGfxContextType RGFW_GFX_OWNED_BY_RGFW = 4;

struct RgfwNativeImage
{
	RgfwFormat format;
	char* buffer; // bitmapBits
	void* bitmap; // HBITMAP
	void* hdcMem; // HDC
}

struct RgfwWindowSrc
{
	void* window; // HWND
	void* hdc; // HDC
	void* hIconSmall; // HICON
	void* hIconBig; // HICON
	int maxSizeW, maxSizeH, minSizeW, minSizeH, aspectRatioW, aspectRatioH;
	bool actionFrame;
	ushort highSurrogate;
	RgfwGfxContext ctx;
	RgfwGfxContextType gfxType;
	int x, y, w, h;
}

struct RgfwSurface
{
	char* data;
	int w, h;
	RgfwFormat format;
	RgfwConvertImageDataFunc convertFunc;
	RgfwNativeImage native;
}

struct RgfwWindowInternal
{
	RgfwKey exitKey;
	int lastMouseX, lastMouseY;
	bool shouldClose;
	bool rawMouse;
	bool captureMouse;
	bool inFocus;
	bool mouseInside;
	RgfwKeymod mod;
	RgfwEventFlag enabledEvents;
	uint flags;
	int oldX, oldY, oldW, oldH;
	RgfwMonitorMode oldMode;
	RgfwMouse* mouse;
}

struct RgfwWindow
{
	RgfwWindowSrc src;
	RgfwWindowInternal internal;
	void* userPtr;
	int x, y, w, h;
}

struct RgfwWindowState
{
	bool mouseEnter;
	bool dataDragging;
	bool dataDrop;
	usz dataLength;
	int dropX, dropY;
	RgfwWindow* win;
	bool mouseLeave;
	RgfwWindow* winLeave;
}

struct RgfwKeyState
{
	bool current;
	bool prev;
}

struct RgfwMonitorNode
{
	RgfwMonitor mon;
	bool disconnected;
	RgfwMonitorNode* next;
	void* native0;
	void* native1;
	ulong nativeId0;
	ulong nativeId1;
	void* hMonitor; // HMONITOR
	ushort[32] adapterName;
	ushort[32] deviceName;
}

struct RgfwMonitorList
{
	RgfwMonitorNode* head;
	RgfwMonitorNode* cur;
}

struct RgfwMonitors
{
	RgfwMonitorList list;
	usz count;
	RgfwMonitorNode* primary;
}

struct RgfwInfo
{
	RgfwWindow* root;
	int windowCount;
	RgfwMouse* hiddenMouse;
	RgfwMouse*[21] standardMice;
	RgfwDebugFunc debugCallbackSrc;
	RgfwGenericFunc[25] callbacks;
	RgfwEvent[MAX_EVENTS] events;
	int eventBottom;
	int eventLen;
	bool queueEvents;
	bool polledEvents;
	uint[RGFW_KEY_LAST] apiKeycodes;
	RgfwKey[512] keycodes;
	bool stopCheckEvents_bool;
	ulong timerOffset;
	RgfwDataTransfer* clipboard;
	bool dndBuild;
	RgfwDataDropNode* dndRoot;
	RgfwDataDropNode* dndCur;
	RgfwMonitors monitors;
	RgfwWindow* current;
	void* nativeGL_handle;
	void* glGetProcAddress;
	void* vulkan_handle;
	RgfwVkGetInstanceProcAddressProc vkGetInstanceProcAddress;
	void* egl_display;
	void* egl_handle;
	void* eglGetProcAddress;
	void* instance; // HINSTANCE
	WndClassW wndClass;
	ushort[256] wideClassName;
	void* helperWindow; // HWND
	bool rawMouse;
	RgfwWindowState windowState;
	RgfwKeyState[8] mouseButtons;
	RgfwKeyState[RGFW_KEY_LAST] keyboard;
	float scrollX, scrollY;
	float vectorX, vectorY;
}

RgfwInfo* _rgfw;
RgfwInfo _rgfw_global;

fn void rgfw_setInfo(RgfwInfo* info) { _rgfw = info; }
fn RgfwInfo* rgfw_getInfo() { return _rgfw; }
fn void* rgfw_alloc(usz size) { return rgfw_alloc_impl(size); }
fn void rgfw_free(void* ptr) { rgfw_free_impl(ptr); }
fn usz rgfw_sizeofInfo() { return RgfwInfo::size; }
fn usz rgfw_sizeofNativeImage() { return RgfwNativeImage::size; }
fn usz rgfw_sizeofSurface() { return RgfwSurface::size; }
fn usz rgfw_sizeofWindow() { return RgfwWindow::size; }
fn usz rgfw_sizeofWindowSrc() { return RgfwWindowSrc::size; }

fn bool rgfw_loadGL()
{
	if (_rgfw.nativeGL_handle != null && _rgfw.glGetProcAddress != null) return RGFW_TRUE;
	_rgfw.nativeGL_handle = loadLibraryA("opengl32.dll");
	_rgfw.glGetProcAddress = getProcAddress((HModule*)_rgfw.nativeGL_handle, "wglGetProcAddress");

	rgfw_wglCreateContext = (RgfwWglCreateContextProc)getProcAddress((HModule*)_rgfw.nativeGL_handle, "wglCreateContext");
	rgfw_wglDeleteContext = (RgfwWglDeleteContextProc)getProcAddress((HModule*)_rgfw.nativeGL_handle, "wglDeleteContext");
	rgfw_wglMakeCurrent = (RgfwWglMakeCurrentProc)getProcAddress((HModule*)_rgfw.nativeGL_handle, "wglMakeCurrent");
	rgfw_wglGetCurrentDC = (RgfwWglGetCurrentDcProc)getProcAddress((HModule*)_rgfw.nativeGL_handle, "wglGetCurrentDC");
	rgfw_wglGetCurrentContext = (RgfwWglGetCurrentContextProc)getProcAddress((HModule*)_rgfw.nativeGL_handle, "wglGetCurrentContext");
	rgfw_wglShareLists = (RgfwWglShareListsProc)getProcAddress((HModule*)_rgfw.nativeGL_handle, "wglShareLists");

	HWnd* dummyWin = createWindowExW(0, _rgfw.wndClass.className, null, 0, 0, 0, 0, 0, null, null, _rgfw.instance, null);
	HDc dummyDc = getDC(dummyWin);
	uint pfdFlags = PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER;

	PixelFormatDescriptor pfd;
	rgfw_memzero(&pfd, PixelFormatDescriptor::size);
	pfd.size = (ushort)PixelFormatDescriptor::size;
	pfd.version = 1;
	pfd.flags = pfdFlags;
	pfd.pixelType = PFD_TYPE_RGBA;
	pfd.colorBits = 32;
	pfd.alphaBits = 8;
	pfd.layerType = PFD_MAIN_PLANE;
	pfd.depthBits = 32;
	pfd.stencilBits = 8;

	int dummyPixelFormat = choosePixelFormat(dummyDc, &pfd);
	setPixelFormat(dummyDc, dummyPixelFormat, &pfd);

	void* dummyContext = rgfw_wglCreateContext(dummyDc);
	void* cur = rgfw_wglGetCurrentContext();
	rgfw_wglMakeCurrent(dummyDc, dummyContext);

	rgfw_wglCreateContextAttribsARB = (RgfwWglCreateContextAttribsArbProc)rgfw_getProcAddress_OpenGL("wglCreateContextAttribsARB");
	rgfw_wglChoosePixelFormatARB = (RgfwWglChoosePixelFormatArbProc)rgfw_getProcAddress_OpenGL("wglChoosePixelFormatARB");
	rgfw_wglSwapIntervalEXT = (RgfwWglSwapIntervalExtProc)rgfw_getProcAddress_OpenGL("wglSwapIntervalEXT");

	if (rgfw_wglSwapIntervalEXT == null) rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_OPEN_GL_CONTEXT, "Failed to load swap interval function");

	rgfw_wglMakeCurrent(dummyDc, cur);
	rgfw_wglDeleteContext(dummyContext);
	releaseDC(dummyWin, dummyDc);
	destroyWindow(dummyWin);
	return RGFW_TRUE;
}

fn void rgfw_unloadGL()
{
	_rgfw.nativeGL_handle = null;
	_rgfw.glGetProcAddress = null;
}

const VkResult VK_ERROR_EXTENSION_NOT_PRESENT = -7;
const int VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR = 1000009000;
struct VkWin32SurfaceCreateInfoKHR { int sType; void* pNext; uint flags; void* hinstance; void* hwnd; }
alias RgfwVkCreateWin32SurfaceKHRProc = fn VkResult(VkInstance instance, VkWin32SurfaceCreateInfoKHR* createInfo, void* allocator, VkSurfaceKHR* surface);
char*[2] rgfw_vulkanExtensions = {"VK_KHR_surface", "VK_KHR_win32_surface"};

fn bool rgfw_loadVulkan()
{
	if (_rgfw.vulkan_handle != null) return RGFW_TRUE;
	_rgfw.vulkan_handle = loadLibraryA("vulkan-1.dll");
	if (_rgfw.vulkan_handle == null) return RGFW_FALSE;
	void* proc = getProcAddress((HModule*)_rgfw.vulkan_handle, "vkGetInstanceProcAddr");
	if (proc == null) return RGFW_FALSE;
	_rgfw.vkGetInstanceProcAddress = (RgfwVkGetInstanceProcAddressProc)proc;
	return RGFW_TRUE;
}

fn void rgfw_unloadVulkan()
{
	if (_rgfw.vulkan_handle == null) return;
	freeLibrary((HModule*)_rgfw.vulkan_handle);
	_rgfw.vulkan_handle = null;
	_rgfw.vkGetInstanceProcAddress = null;
}

fn RgfwProc rgfw_getInstanceProcAddress_Vulkan(VkInstance instance, char* procname)
{
	if (_rgfw == null || _rgfw.vkGetInstanceProcAddress == null) return null;
	return _rgfw.vkGetInstanceProcAddress(instance, procname);
}

fn char** rgfw_getRequiredInstanceExtensions_Vulkan(usz* count)
{
	if (count != null) *count = 2;
	return &rgfw_vulkanExtensions;
}

fn VkResult rgfw_window_createSurface_Vulkan(RgfwWindow* win, VkInstance instance, VkSurfaceKHR* surface)
{
	if (surface == null || win == null) return VK_ERROR_EXTENSION_NOT_PRESENT;
	*surface = 0;
	RgfwProc raw = rgfw_getInstanceProcAddress_Vulkan(instance, "vkCreateWin32SurfaceKHR");
	if (raw == null) return VK_ERROR_EXTENSION_NOT_PRESENT;
	VkWin32SurfaceCreateInfoKHR createInfo;
	createInfo.sType = VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR;
	createInfo.pNext = null;
	createInfo.flags = 0;
	createInfo.hinstance = _rgfw.instance;
	createInfo.hwnd = win.src.window;
	return ((RgfwVkCreateWin32SurfaceKHRProc)raw)(instance, &createInfo, null, surface);
}

alias RgfwVkGetPhysicalDeviceWin32PresentationSupportKHRProc = fn uint(VkPhysicalDevice, uint);

fn bool rgfw_getPhysicalDevicePresentationSupport_Vulkan(VkInstance instance, VkPhysicalDevice physicalDevice, uint queueFamilyIndex)
{
	RgfwProc raw = rgfw_getInstanceProcAddress_Vulkan(instance, "vkGetPhysicalDeviceWin32PresentationSupportKHR");
	if (raw == null) return RGFW_FALSE;
	uint out = ((RgfwVkGetPhysicalDeviceWin32PresentationSupportKHRProc)raw)(physicalDevice, queueFamilyIndex);
	return rgfw_bool(out != 0);
}

alias WGPUInstance = void*;
alias WGPUSurface = void*;
alias WGPUSType = int;
const WGPUSType WGPU_STYPE_SURFACE_SOURCE_WINDOWS_HWND = 0x00000005;
struct WgpuChainedStruct { WgpuChainedStruct* next; WGPUSType sType; }
struct WgpuSurfaceDescriptor { WgpuChainedStruct* nextInChain; }
struct WgpuSurfaceSourceWindowsHWND { WgpuChainedStruct chain; void* hinstance; void* hwnd; }
extern fn WGPUSurface wgpuInstanceCreateSurface(WGPUInstance instance, WgpuSurfaceDescriptor* descriptor) @cname("wgpuInstanceCreateSurface");

fn WGPUSurface rgfw_window_createSurface_WebGPU(RgfwWindow* window, WGPUInstance instance)
{
	WgpuSurfaceSourceWindowsHWND fromHwnd;
	fromHwnd.chain.next = null;
	fromHwnd.chain.sType = WGPU_STYPE_SURFACE_SOURCE_WINDOWS_HWND;
	fromHwnd.hwnd = window.src.window;
	fromHwnd.hinstance = _rgfw.instance;
	WgpuSurfaceDescriptor surfaceDesc;
	surfaceDesc.nextInChain = (WgpuChainedStruct*)&fromHwnd;
	return wgpuInstanceCreateSurface(instance, &surfaceDesc);
}

alias HResult = int;
const int DXGI_FORMAT_R8G8B8A8_UNORM = 28;
const uint DXGI_USAGE_RENDER_TARGET_OUTPUT = 0x20;
const uint DXGI_SWAP_CHAIN_FLAG_ALLOW_MODE_SWITCH = 2;
const int DXGI_SWAP_EFFECT_DISCARD = 0;

struct DxgiRational { uint numerator; uint denominator; }
struct DxgiSampleDesc { uint count; uint quality; }
struct DxgiModeDesc
{
	uint width;
	uint height;
	DxgiRational refreshRate;
	int format;
	int scanlineOrdering;
	int scaling;
}
struct DxgiSwapChainDesc
{
	DxgiModeDesc bufferDesc;
	DxgiSampleDesc sampleDesc;
	uint bufferUsage;
	uint bufferCount;
	void* outputWindow;
	int windowed;
	int swapEffect;
	uint flags;
}

alias DxgiCreateSwapChainFn = fn HResult(void* self, void* pDevice, DxgiSwapChainDesc* pDesc, void** ppSwapChain);
struct DxgiFactoryVtbl
{
	void* queryInterface;
	void* addRef;
	void* release;
	void* setPrivateData;
	void* setPrivateDataInterface;
	void* getPrivateData;
	void* getParent;
	void* enumAdapters;
	void* makeWindowAssociation;
	void* getWindowAssociation;
	DxgiCreateSwapChainFn createSwapChain;
}
struct DxgiFactory { DxgiFactoryVtbl* lpVtbl; }

fn int rgfw_window_createSwapChain_DirectX(RgfwWindow* win, DxgiFactory* pFactory, void* pDevice, void** swapchain)
{
	if (win == null || pFactory == null || pDevice == null || swapchain == null) return -1;

	DxgiSwapChainDesc swapChainDesc;
	rgfw_memzero(&swapChainDesc, DxgiSwapChainDesc::size);
	swapChainDesc.bufferCount = 2;
	swapChainDesc.bufferDesc.width = (uint)win.w;
	swapChainDesc.bufferDesc.height = (uint)win.h;
	swapChainDesc.bufferDesc.format = DXGI_FORMAT_R8G8B8A8_UNORM;
	swapChainDesc.bufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
	swapChainDesc.outputWindow = win.src.window;
	swapChainDesc.sampleDesc.count = 1;
	swapChainDesc.sampleDesc.quality = 0;
	swapChainDesc.windowed = 1;
	swapChainDesc.flags = DXGI_SWAP_CHAIN_FLAG_ALLOW_MODE_SWITCH;

	HResult hr = pFactory.lpVtbl.createSwapChain(pFactory, pDevice, &swapChainDesc, swapchain);
	if (hr < 0)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_DIRECT_X_CONTEXT, "Failed to create DirectX swap chain!");
		return -2;
	}
	return 0;
}

fn void rgfw_debugCallback(RgfwDebugType type, RgfwErrorCode code, char* msg)
{
	RgfwDebugInfo info;
	info.type = type;
	info.code = code;
	info.msg = msg;
	if (_rgfw != null && _rgfw.debugCallbackSrc != null) _rgfw.debugCallbackSrc(&info);
}

fn RgfwDebugFunc rgfw_setDebugCallback(RgfwDebugFunc func)
{
	RgfwDebugFunc prev = _rgfw.debugCallbackSrc;
	_rgfw.debugCallbackSrc = func;
	return prev;
}

fn void rgfw_initKeycodes()
{
	rgfw_memzero((void*)&_rgfw.keycodes, RgfwKey[512]::size);
	rgfw_initKeycodesPlatform();
	for (usz i = 0; i < (usz)RGFW_KEY_LAST; i++)
	{
		for (usz y = 0; y < 512; y++)
		{
			if (_rgfw.keycodes[y] == (RgfwKey)i)
			{
				_rgfw.apiKeycodes[i] = (uint)y;
				break;
			}
		}
	}
	rgfw_resetKey();
}

fn RgfwKey rgfw_apiKeyToRGFW(uint keycode)
{
	if (keycode > 512) return RGFW_KEY_NULL;
	return _rgfw.keycodes[(usz)keycode];
}

fn uint rgfw_rgfwToApiKey(RgfwKey keycode)
{
	return _rgfw.apiKeycodes[(usz)keycode];
}

fn void rgfw_resetKey()
{
	rgfw_memzero((void*)&_rgfw.keyboard, RgfwKeyState[256]::size);
}

fn RgfwGenericFunc rgfw_setEventCallback(RgfwEventType type, RgfwGenericFunc func)
{
	RgfwGenericFunc old = _rgfw.callbacks[(usz)type];
	_rgfw.callbacks[(usz)type] = func;
	return old;
}

fn void rgfw_setDualEventCallback(RgfwEventType type, RgfwGenericFunc func, RgfwGenericFunc* first, RgfwGenericFunc* second)
{
	RgfwGenericFunc func1 = rgfw_setEventCallback(type, func);
	RgfwGenericFunc func2 = rgfw_setEventCallback((RgfwEventType)(type + 1), func);
	if (first != null) *first = func1;
	if (second != null) *second = func2;
}

fn void rgfw_setAllEventCallbacks(RgfwGenericFunc func, RgfwCallbacks* callbacks)
{
	for (RgfwEventType i = RGFW_EVENT_NONE + 1; i < RGFW_EVENT_COUNT; i++)
	{
		if (callbacks != null) callbacks.arr[(usz)i] = _rgfw.callbacks[(usz)i];
		rgfw_setEventCallback(i, func);
	}
}

fn void rgfw_eventQueueFlush() { _rgfw.eventLen = 0; }

fn void rgfw_eventQueuePush(RgfwEvent* event)
{
	if (_rgfw.queueEvents == RGFW_FALSE) return;
	if (_rgfw.eventLen >= MAX_EVENTS)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_EVENT_QUEUE, "Event queue limit 'MAX_EVENTS' has been reached automatically flushing queue.");
		rgfw_eventQueueFlush();
		return;
	}
	int eventTop = (_rgfw.eventBottom + _rgfw.eventLen) % MAX_EVENTS;
	_rgfw.eventLen += 1;
	_rgfw.events[(usz)eventTop] = *event;
}

fn RgfwEvent* rgfw_eventQueuePop()
{
	if (_rgfw.eventLen == 0) return null;
	RgfwEvent* ev = &_rgfw.events[(usz)_rgfw.eventBottom];
	_rgfw.eventLen -= 1;
	_rgfw.eventBottom = (_rgfw.eventBottom + 1) % MAX_EVENTS;
	return ev;
}

fn void rgfw_eventQueuePushAndCall(RgfwEvent* event)
{
	if (_rgfw.callbacks[(usz)event.type] != null) _rgfw.callbacks[(usz)event.type](event);
	rgfw_eventQueuePush(event);
}

fn bool rgfw_checkEvent(RgfwEvent* event)
{
	if (_rgfw.eventLen == 0 && _rgfw.polledEvents == RGFW_FALSE)
	{
		_rgfw.queueEvents = RGFW_TRUE;
		rgfw_pollEvents();
		_rgfw.polledEvents = RGFW_TRUE;
	}
	if (rgfw_checkQueuedEvent(event) == RGFW_FALSE)
	{
		_rgfw.polledEvents = RGFW_FALSE;
		return RGFW_FALSE;
	}
	return RGFW_TRUE;
}

fn bool rgfw_checkQueuedEvent(RgfwEvent* event)
{
	_rgfw.queueEvents = RGFW_TRUE;
	RgfwEvent* ev = rgfw_eventQueuePop();
	if (ev != null)
	{
		*event = *ev;
		return RGFW_TRUE;
	}
	return RGFW_FALSE;
}

fn RgfwEvent* rgfw_window_eventQueuePop(RgfwWindow* win)
{
	RgfwEvent* ev = rgfw_eventQueuePop();
	if (ev == null) return ev;
	for (int i = 1; i < _rgfw.eventLen && ev.common.win != win && ev.common.win != null; i++)
	{
		rgfw_eventQueuePush(ev);
		ev = rgfw_eventQueuePop();
	}
	if (ev.common.win != win && ev.common.win != null) return null;
	return ev;
}

fn bool rgfw_window_checkEvent(RgfwWindow* win, RgfwEvent* event)
{
	if (_rgfw.eventLen == 0 && _rgfw.polledEvents == RGFW_FALSE)
	{
		_rgfw.queueEvents = RGFW_TRUE;
		rgfw_pollEvents();
		_rgfw.polledEvents = RGFW_TRUE;
	}
	if (rgfw_window_checkQueuedEvent(win, event) == RGFW_FALSE)
	{
		_rgfw.polledEvents = RGFW_FALSE;
		return RGFW_FALSE;
	}
	return RGFW_TRUE;
}

fn bool rgfw_window_checkQueuedEvent(RgfwWindow* win, RgfwEvent* event)
{
	RgfwEvent* ev = rgfw_window_eventQueuePop(win);
	if (ev == null) return RGFW_FALSE;
	*event = *ev;
	return RGFW_TRUE;
}

fn void rgfw_windowMaximizedCallback(RgfwWindow* win, int x, int y, int w, int h)
{
	win.internal.flags |= RGFW_WINDOW_MAXIMIZE;
	win.x = x; win.y = y; win.w = w; win.h = h;
	if (!(win.internal.enabledEvents & RGFW_WINDOW_MAXIMIZED_FLAG)) return;
	RgfwEvent event;
	event.type = RGFW_WINDOW_MAXIMIZED;
	event.update.x = x; event.update.y = y; event.update.w = w; event.update.h = h;
	event.common.win = win;
	rgfw_eventQueuePushAndCall(&event);
}

fn void rgfw_windowMinimizedCallback(RgfwWindow* win)
{
	win.internal.flags |= RGFW_WINDOW_MINIMIZE;
	if (!(win.internal.enabledEvents & RGFW_WINDOW_MINIMIZED_FLAG)) return;
	RgfwEvent event;
	event.type = RGFW_WINDOW_MINIMIZED;
	event.update.x = win.x; event.update.y = win.y; event.update.w = win.w; event.update.h = win.h;
	event.common.win = win;
	rgfw_eventQueuePushAndCall(&event);
}

fn void rgfw_windowRestoredCallback(RgfwWindow* win, int x, int y, int w, int h)
{
	win.internal.flags &= ~((uint)RGFW_WINDOW_MINIMIZE);
	win.x = x; win.y = y; win.w = w; win.h = h;
	if (rgfw_window_isMaximized(win) == RGFW_FALSE) win.internal.flags &= ~((uint)RGFW_WINDOW_MAXIMIZE);
	if (!(win.internal.enabledEvents & RGFW_WINDOW_RESTORED_FLAG)) return;
	RgfwEvent event;
	event.type = RGFW_WINDOW_RESTORED;
	event.update.x = x; event.update.y = y; event.update.w = w; event.update.h = h;
	event.common.win = win;
	rgfw_eventQueuePushAndCall(&event);
}

fn void rgfw_windowMovedCallback(RgfwWindow* win, int x, int y)
{
	win.x = x; win.y = y;
	if (!(win.internal.enabledEvents & RGFW_WINDOW_MOVED_FLAG)) return;
	RgfwEvent event;
	event.type = RGFW_WINDOW_MOVED;
	event.update.x = x; event.update.y = y; event.update.w = win.w; event.update.h = win.h;
	event.common.win = win;
	rgfw_eventQueuePushAndCall(&event);
}

fn void rgfw_windowResizedCallback(RgfwWindow* win, int w, int h)
{
	win.w = w; win.h = h;
	if (!(win.internal.enabledEvents & RGFW_WINDOW_RESIZED_FLAG)) return;
	RgfwEvent event;
	event.type = RGFW_WINDOW_RESIZED;
	event.update.x = win.x; event.update.y = win.y; event.update.w = w; event.update.h = h;
	event.common.win = win;
	rgfw_eventQueuePushAndCall(&event);
}

fn void rgfw_windowCloseCallback(RgfwWindow* win)
{
	win.internal.shouldClose = RGFW_TRUE;
	RgfwEvent event;
	event.type = RGFW_WINDOW_CLOSE;
	event.common.win = win;
	rgfw_eventQueuePushAndCall(&event);
}

fn void rgfw_mouseMotionCallback(RgfwWindow* win, int x, int y)
{
	win.internal.lastMouseX = x;
	win.internal.lastMouseY = y;
	if (!(win.internal.enabledEvents & RGFW_MOUSE_MOTION_FLAG)) return;
	RgfwEvent event;
	event.type = RGFW_MOUSE_MOTION;
	event.mouse.x = x; event.mouse.y = y; event.mouse.inWindow = win.internal.mouseInside;
	event.common.win = win;
	rgfw_eventQueuePushAndCall(&event);
}

fn void rgfw_rawMotionCallback(RgfwWindow* win, float x, float y)
{
	_rgfw.vectorX = x; _rgfw.vectorY = y;
	if (!(win.internal.enabledEvents & RGFW_MOUSE_RAW_MOTION_FLAG)) return;
	RgfwEvent event;
	event.type = RGFW_MOUSE_RAW_MOTION;
	event.delta.x = x; event.delta.y = y;
	event.common.win = win;
	rgfw_eventQueuePushAndCall(&event);
}

fn void rgfw_windowRefreshCallback(RgfwWindow* win, int x, int y, int w, int h)
{
	if (!(win.internal.enabledEvents & RGFW_WINDOW_REFRESH_FLAG)) return;
	RgfwEvent event;
	event.type = RGFW_WINDOW_REFRESH;
	event.update.x = x; event.update.y = y; event.update.w = w; event.update.h = h;
	event.common.win = win;
	rgfw_eventQueuePushAndCall(&event);
}

fn void rgfw_windowFocusCallback(RgfwWindow* win, bool inFocus)
{
	win.internal.inFocus = inFocus;
	if (win.internal.captureMouse) rgfw_window_captureMousePlatform(win, inFocus);
	RgfwEvent event;
	event.common.win = win;
	event.focus.state = inFocus;
	if (inFocus == RGFW_TRUE)
	{
		if (win.internal.flags & RGFW_WINDOW_FULLSCREEN) rgfw_window_raise(win);
		event.type = RGFW_WINDOW_FOCUS_IN;
	}
	else
	{
		if (win.internal.flags & RGFW_WINDOW_FULLSCREEN) rgfw_window_minimize(win);
		for (usz key = 0; key < (usz)RGFW_KEY_LAST; key++)
		{
			if (rgfw_isKeyDown((RgfwKey)key) == RGFW_FALSE) continue;
			_rgfw.keyboard[key].current = RGFW_FALSE;
			if (win.internal.enabledEvents & RGFW_KEY_RELEASED_FLAG) rgfw_keyCallback(win, (RgfwKey)key, win.internal.mod, RGFW_FALSE, RGFW_FALSE);
		}
		rgfw_resetKey();
		event.type = RGFW_WINDOW_FOCUS_OUT;
	}
	rgfw_eventQueuePushAndCall(&event);
}

fn void rgfw_mouseNotifyCallback(RgfwWindow* win, int x, int y, bool status)
{
	win.internal.mouseInside = status;
	_rgfw.windowState.win = win;
	win.internal.lastMouseX = x;
	win.internal.lastMouseY = y;
	RgfwEvent event;
	event.common.win = win;
	event.mouse.x = x; event.mouse.y = y; event.mouse.inWindow = win.internal.mouseInside;
	if (status)
	{
		if (!(win.internal.enabledEvents & RGFW_MOUSE_ENTER_FLAG)) return;
		_rgfw.windowState.mouseEnter = RGFW_TRUE;
		event.type = RGFW_MOUSE_ENTER;
	}
	else
	{
		if (!(win.internal.enabledEvents & RGFW_MOUSE_LEAVE_FLAG)) return;
		_rgfw.windowState.winLeave = win;
		_rgfw.windowState.mouseLeave = RGFW_TRUE;
		event.type = RGFW_MOUSE_LEAVE;
	}
	rgfw_eventQueuePushAndCall(&event);
}

fn void rgfw_dataDragCallback(RgfwWindow* win, RgfwDataTransferType dataType, RgfwDndActionType action, int x, int y)
{
	if (!(win.internal.enabledEvents & RGFW_DATA_DRAG_FLAG) || !(win.internal.flags & RGFW_WINDOW_ALLOW_DND)) return;
	_rgfw.windowState.win = win;
	_rgfw.windowState.dataDragging = RGFW_TRUE;
	_rgfw.windowState.dropX = x;
	_rgfw.windowState.dropY = y;
	RgfwEvent event;
	event.type = RGFW_DATA_DRAG;
	event.drag.x = x; event.drag.y = y; event.drag.action = action; event.drag.dataType = dataType;
	event.common.win = win;
	rgfw_eventQueuePushAndCall(&event);
}

fn void rgfw_dataDropCallback(RgfwWindow* win, char* data, usz length, RgfwDataTransferType dataType)
{
	if (!(win.internal.enabledEvents & RGFW_DATA_DROP_FLAG) || !(win.internal.flags & RGFW_WINDOW_ALLOW_DND)) return;
	_rgfw.windowState.win = win;
	_rgfw.windowState.dataDrop = RGFW_TRUE;
	_rgfw.windowState.dataLength = length;
	RgfwDataDropNode node;
	rgfw_memzero(&node, RgfwDataDropNode::size);
	node.data = data;
	node.length = length;
	node.type = dataType;
	RgfwEvent event;
	event.type = RGFW_DATA_DROP;
	event.drop.value = &node;
	event.common.win = win;
	if (_rgfw.callbacks[(usz)event.type] != null) _rgfw.callbacks[(usz)event.type](&event);
	if (_rgfw.queueEvents == RGFW_FALSE && _rgfw.dndBuild == RGFW_FALSE) return;
	RgfwDataDropNode* heapNode = (RgfwDataDropNode*)rgfw_alloc_impl(RgfwDataDropNode::size);
	if (heapNode == null) return;
	*heapNode = node;
	if (_rgfw.dndRoot == null) { _rgfw.dndRoot = heapNode; _rgfw.dndCur = heapNode; }
	else { _rgfw.dndCur.next = heapNode; _rgfw.dndCur = heapNode; }
	event.drop.value = heapNode;
	rgfw_eventQueuePush(&event);
}

fn void rgfw_keyCharCallback(RgfwWindow* win, uint codepoint)
{
	if (!(win.internal.enabledEvents & RGFW_KEY_CHAR_FLAG)) return;
	RgfwEvent event;
	event.type = RGFW_KEY_CHAR;
	event.keyChar.value = codepoint;
	event.common.win = win;
	rgfw_eventQueuePushAndCall(&event);
}

fn void rgfw_keyCallback(RgfwWindow* win, RgfwKey key, RgfwKeymod mod, bool repeat, bool state)
{
	RgfwEvent event;
	if (state)
	{
		if (!(win.internal.enabledEvents & RGFW_KEY_PRESSED_FLAG)) return;
		event.type = RGFW_KEY_PRESSED;
	}
	else
	{
		if (!(win.internal.enabledEvents & RGFW_KEY_RELEASED_FLAG)) return;
		event.type = RGFW_KEY_RELEASED;
	}
	_rgfw.keyboard[(usz)key].prev = _rgfw.keyboard[(usz)key].current;
	_rgfw.keyboard[(usz)key].current = state;
	event.key.value = key; event.key.repeat = repeat; event.key.mod = mod; event.key.state = state;
	event.common.win = win;
	rgfw_eventQueuePushAndCall(&event);
}

fn void rgfw_mouseButtonCallback(RgfwWindow* win, RgfwMouseButton button, bool press)
{
	RgfwEvent event;
	if (press)
	{
		if (!(win.internal.enabledEvents & RGFW_MOUSE_BUTTON_PRESSED_FLAG)) return;
		event.type = RGFW_MOUSE_BUTTON_PRESSED;
	}
	else
	{
		if (!(win.internal.enabledEvents & RGFW_MOUSE_BUTTON_RELEASED_FLAG)) return;
		event.type = RGFW_MOUSE_BUTTON_RELEASED;
	}
	_rgfw.mouseButtons[(usz)button].prev = _rgfw.mouseButtons[(usz)button].current;
	_rgfw.mouseButtons[(usz)button].current = press;
	event.button.value = button;
	event.button.state = press;
	event.common.win = win;
	rgfw_eventQueuePushAndCall(&event);
}

fn void rgfw_mouseScrollCallback(RgfwWindow* win, float x, float y)
{
	if (!(win.internal.enabledEvents & RGFW_MOUSE_SCROLL_FLAG)) return;
	_rgfw.scrollX = x; _rgfw.scrollY = y;
	RgfwEvent event;
	event.type = RGFW_MOUSE_SCROLL;
	event.delta.x = x; event.delta.y = y;
	event.common.win = win;
	rgfw_eventQueuePushAndCall(&event);
}

fn void rgfw_scaleUpdatedCallback(RgfwWindow* win, float scaleX, float scaleY)
{
	if (!(win.internal.enabledEvents & RGFW_SCALE_UPDATED_FLAG)) return;
	RgfwEvent event;
	event.type = RGFW_SCALE_UPDATED;
	event.scale.x = scaleX; event.scale.y = scaleY;
	event.common.win = win;
	rgfw_eventQueuePushAndCall(&event);
}

fn void rgfw_monitorCallback(RgfwWindow* win, RgfwMonitor* monitor, bool connected)
{
	if (win != null)
	{
		if (connected && !(win.internal.enabledEvents & RGFW_MONITOR_CONNECTED_FLAG)) return;
		if (!connected && !(win.internal.enabledEvents & RGFW_MONITOR_DISCONNECTED_FLAG)) return;
	}
	RgfwEvent event;
	event.type = connected ? RGFW_MONITOR_CONNECTED : RGFW_MONITOR_DISCONNECTED;
	event.monitor.monitor = monitor;
	event.monitor.state = connected;
	event.common.win = win;
	rgfw_eventQueuePushAndCall(&event);
}

fn int rgfw_init(char* className, RgfwInitFlags flags)
{
	return rgfw_init_ptr(className, flags, &_rgfw_global);
}

fn void rgfw_deinit() { rgfw_deinit_ptr(_rgfw); }

fn int rgfw_init_ptr(char* className, RgfwInitFlags flags, RgfwInfo* info)
{
	if (info == _rgfw || info == null) return 1;
	if (className == null) className = "RGFW";
	rgfw_setInfo(info);
	rgfw_memzero(_rgfw, RgfwInfo::size);
	if (flags & RGFW_INIT_OPEN_GL && rgfw_loadGL() == RGFW_FALSE) return -1;
	if (flags & RGFW_INIT_EGL && rgfw_loadEGL() == RGFW_FALSE) return -1;
	if (flags & RGFW_INIT_VULKAN && rgfw_loadVulkan() == RGFW_FALSE) return -1;
	rgfw_initKeycodes();
	int out = rgfw_initPlatform(className, flags);
	if (out != 0)
	{
		rgfw_deinitPlatform();
		rgfw_memzero(_rgfw, RgfwInfo::size);
		rgfw_setInfo(null);
		return out;
	}
	for (usz i = 0; i < (usz)RGFW_MOUSE_ICON_COUNT; i++)
	{
		_rgfw.standardMice[i] = rgfw_createMouseStandard((RgfwMouseIcon)i);
	}
	rgfw_pollMonitors();
	return out;
}

fn void rgfw_deinit_ptr(RgfwInfo* info)
{
	if (info == null) return;
	rgfw_setInfo(info);
	for (RgfwMouseIcon i = 0; i < RGFW_MOUSE_ICON_COUNT; i++)
	{
		if (_rgfw.standardMice[(usz)i] != null) rgfw_freeMouse(_rgfw.standardMice[(usz)i]);
	}
	rgfw_freeMonitors();
	rgfw_deinitPlatform();
	rgfw_unloadEGL();
	rgfw_unloadGL();
	_rgfw.root = null;
	_rgfw.windowCount = 0;
	rgfw_setInfo(null);
}

fn RgfwWindow* rgfw_createWindow(char* name, int x, int y, int w, int h, RgfwWindowFlags flags)
{
	RgfwWindow* win = (RgfwWindow*)rgfw_alloc_impl(RgfwWindow::size);
	if (win == null) return null;
	return rgfw_createWindowPtr(name, x, y, w, h, flags, win);
}

fn RgfwWindow* rgfw_createWindowPtr(char* name, int x, int y, int w, int h, RgfwWindowFlags flags, RgfwWindow* win)
{
	if (win == null || _rgfw == null) return null;
	if (name == null) name = "";
	rgfw_memzero(win, RgfwWindow::size);
	_rgfw.windowCount++;
	if (_rgfw.root == null) rgfw_setRootWindow(win);
	win.x = x; win.y = y; win.w = w; win.h = h;
	win.internal.mouse = _rgfw.standardMice[(usz)RGFW_MOUSE_NORMAL];
	win.internal.flags = flags;
	win.internal.enabledEvents = RGFW_ALL_EVENT_FLAGS;
	RgfwWindowFlags reservedFlags = flags & RGFW_WINDOW_SCALE_TO_MONITOR;
	RgfwWindow* ret = rgfw_createWindowPlatform(name, flags, win);
	if (ret == null) return null;
	flags |= reservedFlags;
	rgfw_window_setFlagsInternal(win, flags, 0);

	win.src.gfxType = 0;
	if (flags & RGFW_WINDOW_OPEN_GL) rgfw_window_createContext_OpenGL(win, rgfw_getGlobalHints_OpenGL());

	if (!(flags & RGFW_WINDOW_HIDE_MOUSE)) rgfw_window_setMouseDefault(win);
	rgfw_window_setName(win, name);
	if (!(flags & RGFW_WINDOW_HIDE))
	{
		flags |= RGFW_WINDOW_HIDE;
		rgfw_window_show(win);
	}
	else if ((flags & RGFW_WINDOW_MAXIMIZE) || (flags & RGFW_WINDOW_FULLSCREEN))
	{
		rgfw_window_hide(win);
	}
	return ret;
}

fn void rgfw_window_close(RgfwWindow* win)
{
	if (win == null) return;
	rgfw_window_closePtr(win);
	rgfw_free_impl(win);
}

fn void rgfw_window_closePtr(RgfwWindow* win)
{
	if (win == null) return;
	if (win.internal.captureMouse) rgfw_window_captureMouse(win, RGFW_FALSE);

	if ((win.src.gfxType & RGFW_GFX_NATIVE_OPEN_GL) && win.src.ctx.native != null)
	{
		rgfw_window_deleteContext_OpenGL(win, win.src.ctx.native);
		win.src.ctx.native = null;
	}

	rgfw_window_closePlatform(win);
	if (_rgfw.clipboard != null) rgfw_free_impl(_rgfw.clipboard);
	_rgfw.clipboard = null;
	_rgfw.windowCount--;
}

fn void rgfw_setQueueEvents(bool queue) { _rgfw.queueEvents = rgfw_bool(queue); }

fn void rgfw_resetPrevState()
{
	for (usz i = 0; i < (usz)RGFW_KEY_LAST; i++) _rgfw.keyboard[i].prev = _rgfw.keyboard[i].current;
	for (usz i = 0; i < (usz)RGFW_MOUSE_FINAL; i++) _rgfw.mouseButtons[i].prev = _rgfw.mouseButtons[i].current;
	_rgfw.scrollX = 0.0f; _rgfw.scrollY = 0.0f;
	_rgfw.vectorX = 0.0f; _rgfw.vectorY = 0.0f;
	rgfw_memzero(&_rgfw.windowState, RgfwWindowState::size);
	RgfwDataDropNode* node = _rgfw.dndRoot;
	while (node != null)
	{
		RgfwDataDropNode* next = node.next;
		rgfw_free_impl(node);
		node = next;
	}
	_rgfw.dndRoot = null;
	_rgfw.dndCur = null;
}

fn bool rgfw_isKeyPressed(RgfwKey key) => rgfw_bool(_rgfw.keyboard[(usz)key].current && !_rgfw.keyboard[(usz)key].prev);
fn bool rgfw_isKeyDown(RgfwKey key) => _rgfw.keyboard[(usz)key].current;
fn bool rgfw_isKeyReleased(RgfwKey key) => rgfw_bool(!_rgfw.keyboard[(usz)key].current && _rgfw.keyboard[(usz)key].prev);
fn bool rgfw_isMousePressed(RgfwMouseButton button) => rgfw_bool(_rgfw.mouseButtons[(usz)button].current && !_rgfw.mouseButtons[(usz)button].prev);
fn bool rgfw_isMouseDown(RgfwMouseButton button) => _rgfw.mouseButtons[(usz)button].current;
fn bool rgfw_isMouseReleased(RgfwMouseButton button) => rgfw_bool(!_rgfw.mouseButtons[(usz)button].current && _rgfw.mouseButtons[(usz)button].prev);

fn void rgfw_getMouseScroll(float* x, float* y) { if (x != null) *x = _rgfw.scrollX; if (y != null) *y = _rgfw.scrollY; }
fn void rgfw_getMouseVector(float* x, float* y) { if (x != null) *x = _rgfw.vectorX; if (y != null) *y = _rgfw.vectorY; }

fn RgfwWindowSrc* rgfw_window_getSrc(RgfwWindow* win) => &win.src;
fn bool rgfw_window_getPosition(RgfwWindow* win, int* x, int* y) { if (x != null) *x = win.x; if (y != null) *y = win.y; return RGFW_TRUE; }
fn bool rgfw_window_getSize(RgfwWindow* win, int* w, int* h) { if (w != null) *w = win.w; if (h != null) *h = win.h; return RGFW_TRUE; }
fn uint rgfw_window_getFlags(RgfwWindow* win) => win.internal.flags;
fn RgfwKey rgfw_window_getExitKey(RgfwWindow* win) => win.internal.exitKey;
fn void rgfw_window_setExitKey(RgfwWindow* win, RgfwKey key) { win.internal.exitKey = key; }
fn void rgfw_window_setEnabledEvents(RgfwWindow* win, RgfwEventFlag events) { win.internal.enabledEvents = events; }
fn RgfwEventFlag rgfw_window_getEnabledEvents(RgfwWindow* win) => win.internal.enabledEvents;
fn void rgfw_window_setDisabledEvents(RgfwWindow* win, RgfwEventFlag events) { rgfw_window_setEnabledEvents(win, RGFW_ALL_EVENT_FLAGS & ~events); }
fn void rgfw_window_setEventState(RgfwWindow* win, RgfwEventFlag event, bool state) { rgfw_setBit(&win.internal.enabledEvents, event, state); }
fn void* rgfw_window_getUserPtr(RgfwWindow* win) => win.userPtr;
fn void rgfw_window_setUserPtr(RgfwWindow* win, void* ptr) { win.userPtr = ptr; }

fn bool rgfw_window_getSizeInPixels(RgfwWindow* win, int* w, int* h)
{
	RgfwMonitor* mon = rgfw_window_getMonitor(win);
	if (mon == null) return RGFW_FALSE;
	if (w != null) *w = (int)((float)win.w * mon.pixelRatio);
	if (h != null) *h = (int)((float)win.h * mon.pixelRatio);
	return RGFW_TRUE;
}

fn bool rgfw_window_didMouseLeave(RgfwWindow* win) => rgfw_bool(_rgfw.windowState.winLeave == win && _rgfw.windowState.mouseLeave);
fn bool rgfw_window_didMouseEnter(RgfwWindow* win) => rgfw_bool(_rgfw.windowState.win == win && _rgfw.windowState.mouseEnter);
fn bool rgfw_window_isMouseInside(RgfwWindow* win) => win.internal.mouseInside;
fn bool rgfw_window_isDataDragging(RgfwWindow* win) => rgfw_window_getDataDrag(win, null, null);
fn bool rgfw_window_didDataDrop(RgfwWindow* win) => rgfw_bool(rgfw_window_getDataDrop(win) != null);

fn bool rgfw_window_getDataDrag(RgfwWindow* win, int* x, int* y)
{
	if (_rgfw.windowState.win != win || _rgfw.windowState.dataDragging == RGFW_FALSE) return RGFW_FALSE;
	if (x != null) *x = _rgfw.windowState.dropX;
	if (y != null) *y = _rgfw.windowState.dropY;
	return RGFW_TRUE;
}

fn RgfwDataDropNode* rgfw_window_getDataDrop(RgfwWindow* win)
{
	if (_rgfw.windowState.win != win || _rgfw.windowState.dataDrop == RGFW_FALSE) return null;
	return _rgfw.dndRoot;
}

fn void rgfw_setRootWindow(RgfwWindow* win) { _rgfw.root = win; }
fn RgfwWindow* rgfw_getRootWindow() { return _rgfw.root; }

fn void rgfw_setBit(uint* value, uint mask, bool set)
{
	if (set) { *value |= mask; }
	else { *value &= ~mask; }
}

fn void rgfw_window_setFlagsInternal(RgfwWindow* win, RgfwWindowFlags flags, RgfwWindowFlags cmpFlags)
{
	if (flags & RGFW_WINDOW_NO_BORDER) { rgfw_window_setBorder(win, 0); }
	else if (cmpFlags & RGFW_WINDOW_NO_BORDER) { rgfw_window_setBorder(win, 1); }
	if (flags & RGFW_WINDOW_MAXIMIZE) { rgfw_window_maximize(win); }
	else if (cmpFlags & RGFW_WINDOW_MAXIMIZE) { rgfw_window_restore(win); }
	if (flags & RGFW_WINDOW_MINIMIZE) { rgfw_window_minimize(win); }
	else if (cmpFlags & RGFW_WINDOW_MINIMIZE) { rgfw_window_restore(win); }
	if (flags & RGFW_WINDOW_CENTER) rgfw_window_center(win);
	if (flags & RGFW_WINDOW_CENTER_CURSOR) rgfw_window_moveMouse(win, win.x + (win.w / 2), win.y + (win.h / 2));
	if (flags & RGFW_WINDOW_FULLSCREEN) { rgfw_window_setFullscreen(win, RGFW_TRUE); }
	else if (cmpFlags & RGFW_WINDOW_FULLSCREEN) { rgfw_window_setFullscreen(win, 0); }
	if (flags & RGFW_WINDOW_HIDE_MOUSE) { rgfw_window_showMouse(win, 0); }
	else if (cmpFlags & RGFW_WINDOW_HIDE_MOUSE) { rgfw_window_showMouse(win, 1); }
	if (flags & RGFW_WINDOW_HIDE) { rgfw_window_hide(win); }
	else if (cmpFlags & RGFW_WINDOW_HIDE) { rgfw_window_show(win); }
	if (flags & RGFW_WINDOW_FLOATING) { rgfw_window_setFloating(win, 1); }
	else if (cmpFlags & RGFW_WINDOW_FLOATING) { rgfw_window_setFloating(win, 0); }
	if (flags & RGFW_WINDOW_RAW_MOUSE) { rgfw_window_setRawMouseMode(win, RGFW_TRUE); }
	else if (cmpFlags & RGFW_WINDOW_RAW_MOUSE) { rgfw_window_setRawMouseMode(win, RGFW_FALSE); }
	if (flags & RGFW_WINDOW_CAPTURE_MOUSE) { rgfw_window_captureRawMouse(win, RGFW_TRUE); }
	else if (cmpFlags & RGFW_WINDOW_CAPTURE_MOUSE) { rgfw_window_captureMouse(win, RGFW_FALSE); }
	if (flags & RGFW_WINDOW_FOCUS) rgfw_window_focus(win);
	if (flags & RGFW_WINDOW_SCALE_TO_MONITOR) rgfw_window_scaleToMonitor(win);
	if (flags & RGFW_WINDOW_NO_RESIZE)
	{
		rgfw_window_setMaxSize(win, win.w, win.h);
		rgfw_window_setMinSize(win, win.w, win.h);
	}
	else if (cmpFlags & RGFW_WINDOW_NO_RESIZE)
	{
		rgfw_window_setMaxSize(win, 0, 0);
		rgfw_window_setMinSize(win, 0, 0);
	}
	win.internal.flags = flags;
}

fn void rgfw_window_setFlags(RgfwWindow* win, RgfwWindowFlags flags) { rgfw_window_setFlagsInternal(win, flags, win.internal.flags); }
fn bool rgfw_window_isInFocus(RgfwWindow* win) => rgfw_bool(win.internal.inFocus);
fn void rgfw_setBuildDND(bool state) { _rgfw.dndBuild = state; }
fn bool rgfw_window_getMouse(RgfwWindow* win, int* x, int* y) { if (x != null) *x = win.internal.lastMouseX; if (y != null) *y = win.internal.lastMouseY; return RGFW_TRUE; }
fn bool rgfw_window_isKeyPressed(RgfwWindow* win, RgfwKey key) => rgfw_bool(rgfw_isKeyPressed(key) && rgfw_window_isInFocus(win));
fn bool rgfw_window_isKeyDown(RgfwWindow* win, RgfwKey key) => rgfw_bool(rgfw_isKeyDown(key) && rgfw_window_isInFocus(win));
fn bool rgfw_window_isKeyReleased(RgfwWindow* win, RgfwKey key) => rgfw_bool(rgfw_isKeyReleased(key) && rgfw_window_isInFocus(win));
fn bool rgfw_window_isMousePressed(RgfwWindow* win, RgfwMouseButton button) => rgfw_bool(rgfw_isMousePressed(button) && rgfw_window_isInFocus(win));
fn bool rgfw_window_isMouseDown(RgfwWindow* win, RgfwMouseButton button) => rgfw_bool(rgfw_isMouseDown(button) && rgfw_window_isInFocus(win));
fn bool rgfw_window_isMouseReleased(RgfwWindow* win, RgfwMouseButton button) => rgfw_bool(rgfw_isMouseReleased(button) && rgfw_window_isInFocus(win));

fn void rgfw_window_center(RgfwWindow* win)
{
	RgfwMonitor* mon = rgfw_window_getMonitor(win);
	if (mon == null) return;
	rgfw_window_move(win, mon.x + ((mon.mode.w - win.w) / 2), mon.y + ((mon.mode.h - win.h) / 2));
}

fn bool rgfw_monitor_scaleToWindow(RgfwMonitor* mon, RgfwWindow* win)
{
	RgfwMonitorMode mode;
	mode.w = win.w;
	mode.h = win.h;
	bool ret = rgfw_monitor_requestMode(mon, &mode, RGFW_MONITOR_SCALE);
	rgfw_window_move(win, mon.x, mon.y);
	return ret;
}

fn void rgfw_splitBPP(uint bpp, RgfwMonitorMode* mode)
{
	if (bpp == 32) bpp = 24;
	mode.red = mode.green = mode.blue = (char)(bpp / 3);
	uint delta = bpp - (mode.red * 3);
	if (delta >= 1) mode.green = mode.green + 1;
	if (delta == 2) mode.red = mode.red + 1;
}

fn bool rgfw_monitorModeCompare(RgfwMonitorMode* mon, RgfwMonitorMode* mon2, RgfwModeRequest request)
{
	return rgfw_bool(((mon.w == mon2.w && mon.h == mon2.h) || !(request & RGFW_MONITOR_SCALE)) &&
		((mon.refreshRate == mon2.refreshRate) || !(request & RGFW_MONITOR_REFRESH)) &&
		((mon.red == mon2.red && mon.green == mon2.green && mon.blue == mon2.blue) || !(request & RGFW_MONITOR_RGB)));
}

fn bool rgfw_window_shouldClose(RgfwWindow* win) => rgfw_bool(win == null || win.internal.shouldClose || (win.internal.exitKey && rgfw_isKeyDown(win.internal.exitKey)));
fn void rgfw_window_setShouldClose(RgfwWindow* win, bool shouldClose) { if (shouldClose) { rgfw_windowCloseCallback(win); } else { win.internal.shouldClose = RGFW_FALSE; } }
fn void rgfw_window_scaleToMonitor(RgfwWindow* win) { RgfwMonitor* monitor = rgfw_window_getMonitor(win); if (monitor == null) return; rgfw_window_resize(win, (int)(monitor.scaleX * (float)win.w), (int)(monitor.scaleY * (float)win.h)); }
fn void rgfw_window_moveToMonitor(RgfwWindow* win, RgfwMonitor* m) { rgfw_window_move(win, m.x + win.x, m.y + win.y); }

RgfwColorLayout[6] rgfw_layouts = {
	{0, 1, 2, 3, 3},
	{2, 1, 0, 3, 3},
	{0, 1, 2, 3, 4},
	{1, 2, 3, 0, 4},
	{2, 1, 0, 3, 4},
	{3, 2, 1, 0, 4},
};

fn void rgfw_copyImageData(char* dest_data, int w, int h, RgfwFormat dest_format, char* src_data, RgfwFormat src_format, RgfwConvertImageDataFunc func)
{
	rgfw_copyImageData64(dest_data, w, h, dest_format, src_data, src_format, RGFW_FALSE, func);
}

fn void rgfw_convertImageData64(char* dest_data, char* src_data, RgfwColorLayout* srcLayout, RgfwColorLayout* destLayout, usz count, bool is64bit)
{
	char[4] rgba = {0, 0, 0, 0};
	uint i2 = 0;
	for (uint i = 0; i < count; i++)
	{
		char* src_px = &src_data[(usz)(i * srcLayout.channels)];
		char* dst_px = &dest_data[(usz)(i2 * destLayout.channels)];
		rgba[0] = src_px[(usz)srcLayout.r];
		rgba[1] = src_px[(usz)srcLayout.g];
		rgba[2] = src_px[(usz)srcLayout.b];
		rgba[3] = srcLayout.channels == 4 ? src_px[(usz)srcLayout.a] : 255;
		dst_px[(usz)destLayout.r] = rgba[0];
		dst_px[(usz)destLayout.g] = rgba[1];
		dst_px[(usz)destLayout.b] = rgba[2];
		if (destLayout.channels == 4) dst_px[(usz)destLayout.a] = rgba[3];
		i2 += 1 + is64bit;
	}
}

fn void rgfw_copyImageData64(char* dest_data, int dest_w, int dest_h, RgfwFormat dest_format, char* src_data, RgfwFormat src_format, bool is64bit, RgfwConvertImageDataFunc func)
{
	uint count = (uint)(dest_w * dest_h);
	if (src_format == dest_format)
	{
		uint channels = dest_format >= RGFW_FORMAT_RGBA8 ? 4 : 3;
		rgfw_memcpy(dest_data, src_data, (usz)(count * channels));
		return;
	}
	RgfwColorLayout* srcLayout = &rgfw_layouts[(usz)src_format];
	RgfwColorLayout* destLayout = &rgfw_layouts[(usz)dest_format];
	if (is64bit || func == null) { rgfw_convertImageData64(dest_data, src_data, srcLayout, destLayout, count, is64bit); }
	else { func(dest_data, src_data, srcLayout, destLayout, count); }
}

fn RgfwSurface* rgfw_createSurface(char* data, int w, int h, RgfwFormat format)
{
	RgfwSurface* surface = (RgfwSurface*)rgfw_alloc_impl(RgfwSurface::size);
	if (surface == null) return null;
	rgfw_memzero(surface, RgfwSurface::size);
	rgfw_createSurfacePtr(data, w, h, format, surface);
	return surface;
}

fn void rgfw_surface_setConvertFunc(RgfwSurface* surface, RgfwConvertImageDataFunc func) { surface.convertFunc = func; }
fn RgfwNativeImage* rgfw_surface_getNativeImage(RgfwSurface* surface) => &surface.native;
fn void rgfw_surface_free(RgfwSurface* surface) { rgfw_surface_freePtr(surface); rgfw_free_impl(surface); }

fn RgfwSurface* rgfw_window_createSurface(RgfwWindow* win, char* data, int w, int h, RgfwFormat format)
{
	RgfwSurface* surface = (RgfwSurface*)rgfw_alloc_impl(RgfwSurface::size);
	if (surface == null) return null;
	rgfw_memzero(surface, RgfwSurface::size);
	rgfw_window_createSurfacePtr(win, data, w, h, format, surface);
	return surface;
}

fn bool rgfw_window_createSurfacePtr(RgfwWindow* win, char* data, int w, int h, RgfwFormat format, RgfwSurface* surface)
{
	return rgfw_createSurfacePtr(data, w, h, format, surface);
}

fn RgfwMonitorNode* rgfw_monitors_add(RgfwMonitor* mon)
{
	RgfwMonitorNode* node = (RgfwMonitorNode*)rgfw_alloc_impl(RgfwMonitorNode::size);
	if (node == null) return null;
	rgfw_memzero(node, RgfwMonitorNode::size);
	if (_rgfw.monitors.list.head == null) { _rgfw.monitors.list.head = node; }
	else { _rgfw.monitors.list.cur.next = node; }
	_rgfw.monitors.list.cur = node;
	if (mon != null) node.mon = *mon;
	node.mon.node = node;
	node.disconnected = RGFW_FALSE;
	_rgfw.monitors.count += 1;
	return node;
}

fn void rgfw_monitors_remove(RgfwMonitorNode* node, RgfwMonitorNode* prev)
{
	_rgfw.monitors.count -= 1;
	rgfw_monitorNode_free(node);
	if (prev != node) { prev.next = node.next; }
	else { _rgfw.monitors.list.head = node.next; }
	node.next = null;
	rgfw_free_impl(node);
}

fn void rgfw_monitors_refresh()
{
	RgfwMonitorNode* prev = _rgfw.monitors.list.head;
	RgfwMonitorNode* node = _rgfw.monitors.list.head;
	while (node != null)
	{
		RgfwMonitorNode* next = node.next;
		if (node.disconnected != RGFW_FALSE)
		{
			rgfw_monitorCallback(_rgfw.root, &node.mon, RGFW_FALSE);
			rgfw_monitors_remove(node, prev);
		}
		prev = node;
		node = next;
	}
}

fn void rgfw_freeMonitors()
{
	RgfwMonitorNode* node = _rgfw.monitors.list.head;
	while (node != null)
	{
		RgfwMonitorNode* next = node.next;
		rgfw_monitors_remove(node, node);
		node = next;
	}
}

fn RgfwMonitor** rgfw_getMonitors(usz* len)
{
	if (len != null) *len = 0;
	usz count = 0;
	if (rgfw_getMonitorsPtr(0, null, &count) == RGFW_FALSE || count == 0) return null;
	RgfwMonitor** monitors = (RgfwMonitor**)rgfw_alloc_impl(RgfwMonitor*::size * count);
	if (rgfw_getMonitorsPtr(count, monitors, &count) == RGFW_FALSE)
	{
		rgfw_free_impl(monitors);
		return null;
	}
	if (len != null) *len = count;
	return monitors;
}

fn bool rgfw_getMonitorsPtr(usz max, RgfwMonitor** monitors, usz* len)
{
	if (len != null) *len = _rgfw.monitors.count;
	if (monitors == null || max == 0) return RGFW_TRUE;
	if (len != null) *len = max;
	usz i = 0;
	RgfwMonitorNode* cur_node = _rgfw.monitors.list.head;
	while (cur_node != null && i < max)
	{
		monitors[i] = &cur_node.mon;
		i++;
		cur_node = cur_node.next;
	}
	return RGFW_TRUE;
}

fn RgfwMonitor* rgfw_getPrimaryMonitor()
{
	if (_rgfw.monitors.primary == null) _rgfw.monitors.primary = _rgfw.monitors.list.head;
	if (_rgfw.monitors.primary == null) return null;
	return &_rgfw.monitors.primary.mon;
}

fn RgfwMonitorMode* rgfw_monitor_getModes(RgfwMonitor* monitor, usz* count)
{
	usz num = rgfw_monitor_getModesPtr(monitor, null);
	RgfwMonitorMode* modes = (RgfwMonitorMode*)rgfw_alloc_impl(num * RgfwMonitorMode::size);
	num = rgfw_monitor_getModesPtr(monitor, &modes);
	if (count != null) *count = num;
	return modes;
}

fn void rgfw_freeModes(RgfwMonitorMode* modes) { rgfw_free_impl(modes); }

fn bool rgfw_monitor_findClosestMode(RgfwMonitor* monitor, RgfwMonitorMode* mode, RgfwMonitorMode* closest)
{
	usz count = rgfw_monitor_getModesPtr(monitor, null);
	RgfwMonitorMode* modes = (RgfwMonitorMode*)rgfw_alloc_impl(count * RgfwMonitorMode::size);
	count = rgfw_monitor_getModesPtr(monitor, &modes);
	RgfwMonitorMode* chosen = null;
	uint topScore = 1;
	for (usz i = 0; i < count; i++)
	{
		RgfwMonitorMode* mode2 = &modes[i];
		uint score = 0;
		if (mode.w == mode2.w && mode.h == mode2.h) score += 1000;
		if (mode.red == mode2.red && mode.green == mode2.green && mode.blue == mode2.blue) score += 100;
		if (mode.refreshRate == mode.refreshRate) score += 10;
		if (score > topScore) { topScore = score; chosen = mode2; }
	}
	if (chosen != null && closest != null) *closest = *chosen;
	rgfw_free_impl(modes);
	return chosen == null ? RGFW_FALSE : RGFW_TRUE;
}

fn bool rgfw_monitor_getPosition(RgfwMonitor* monitor, int* x, int* y) { if (x != null) *x = monitor.x; if (y != null) *y = monitor.y; return RGFW_TRUE; }
fn char* rgfw_monitor_getName(RgfwMonitor* monitor) => &monitor.name[0];
fn bool rgfw_monitor_getScale(RgfwMonitor* monitor, float* x, float* y) { if (x != null) *x = monitor.scaleX; if (y != null) *y = monitor.scaleY; return RGFW_TRUE; }
fn bool rgfw_monitor_getPhysicalSize(RgfwMonitor* monitor, float* w, float* h) { if (w != null) *w = monitor.physW; if (h != null) *h = monitor.physH; return RGFW_TRUE; }
fn void rgfw_monitor_setUserPtr(RgfwMonitor* monitor, void* userPtr) { monitor.userPtr = userPtr; }
fn void* rgfw_monitor_getUserPtr(RgfwMonitor* monitor) => monitor.userPtr;
fn bool rgfw_monitor_getMode(RgfwMonitor* monitor, RgfwMonitorMode* mode) { if (mode != null) *mode = monitor.mode; return RGFW_TRUE; }

fn RgfwGammaRamp* rgfw_monitor_getGammaRamp(RgfwMonitor* monitor)
{
	RgfwGammaRamp* ramp = (RgfwGammaRamp*)rgfw_alloc_impl(RgfwGammaRamp::size);
	ramp.count = rgfw_monitor_getGammaRampPtr(monitor, null);
	ramp.red = (ushort*)rgfw_alloc_impl(ushort::size * ramp.count);
	ramp.green = (ushort*)rgfw_alloc_impl(ushort::size * ramp.count);
	ramp.blue = (ushort*)rgfw_alloc_impl(ushort::size * ramp.count);
	ramp.count = rgfw_monitor_getGammaRampPtr(monitor, ramp);
	return ramp;
}

fn void rgfw_freeGammaRamp(RgfwGammaRamp* ramp)
{
	rgfw_free_impl(ramp.red);
	rgfw_free_impl(ramp.green);
	rgfw_free_impl(ramp.blue);
	rgfw_free_impl(ramp);
}

fn bool rgfw_monitor_setGammaPtr(RgfwMonitor* monitor, float gamma, ushort* ptr, usz count)
{
	for (usz i = 0; i < count; i++)
	{
		float value = (float)i / (float)(count - 1);
		value = powf(value, 1.0f / gamma) * 65535.0f + 0.5f;
		value = rgfw_min(value, 65535.0f);
		ptr[i] = (ushort)value;
	}
	RgfwGammaRamp ramp;
	ramp.red = ptr; ramp.green = ptr; ramp.blue = ptr; ramp.count = count;
	return rgfw_monitor_setGammaRamp(monitor, &ramp);
}

fn bool rgfw_monitor_setGamma(RgfwMonitor* monitor, float gamma)
{
	usz count = rgfw_monitor_getGammaRampPtr(monitor, null);
	ushort* ptr = (ushort*)rgfw_alloc_impl(count * ushort::size);
	bool ret = rgfw_monitor_setGammaPtr(monitor, gamma, ptr, count);
	rgfw_free_impl(ptr);
	return ret;
}

fn bool rgfw_window_setIcon(RgfwWindow* win, char* data, int w, int h, RgfwFormat format)
{
	return rgfw_window_setIconEx(win, data, w, h, format, RGFW_ICON_BOTH);
}

fn void rgfw_window_captureMouse(RgfwWindow* win, bool state) { win.internal.captureMouse = state; rgfw_window_captureMousePlatform(win, state); }
fn void rgfw_window_setRawMouseMode(RgfwWindow* win, bool state) { win.internal.rawMouse = state; rgfw_window_setRawMouseModePlatform(win, state); }
fn void rgfw_setRawMouseMode(bool state) { _rgfw.rawMouse = state; if (_rgfw.root != null) rgfw_window_setRawMouseModePlatform(_rgfw.root, state); }
fn void rgfw_window_captureRawMouse(RgfwWindow* win, bool state) { rgfw_window_captureMouse(win, state); rgfw_window_setRawMouseMode(win, state); }
fn bool rgfw_window_isRawMouseMode(RgfwWindow* win) => rgfw_bool(win.internal.rawMouse);
fn bool rgfw_window_isCaptured(RgfwWindow* win) => rgfw_bool(win.internal.captureMouse);

fn void rgfw_keyUpdateKeyMod(RgfwWindow* win, RgfwKeymod mod, bool value) { if (value) { win.internal.mod |= mod; } else { win.internal.mod &= ~mod; } }
fn void rgfw_keyUpdateKeyModsEx(RgfwWindow* win, bool capital, bool numlock, bool control, bool alt, bool shift, bool super, bool scroll)
{
	rgfw_keyUpdateKeyMod(win, RGFW_MOD_CAPS_LOCK, capital);
	rgfw_keyUpdateKeyMod(win, RGFW_MOD_NUM_LOCK, numlock);
	rgfw_keyUpdateKeyMod(win, RGFW_MOD_CONTROL, control);
	rgfw_keyUpdateKeyMod(win, RGFW_MOD_ALT, alt);
	rgfw_keyUpdateKeyMod(win, RGFW_MOD_SHIFT, shift);
	rgfw_keyUpdateKeyMod(win, RGFW_MOD_SUPER, super);
	rgfw_keyUpdateKeyMod(win, RGFW_MOD_SCROLL_LOCK, scroll);
}

fn void rgfw_keyUpdateKeyMods(RgfwWindow* win, bool capital, bool numlock, bool scroll)
{
	rgfw_keyUpdateKeyModsEx(win, capital, numlock,
		rgfw_bool(rgfw_isKeyDown(RGFW_KEY_CONTROL_L) || rgfw_isKeyDown(RGFW_KEY_CONTROL_R)),
		rgfw_bool(rgfw_isKeyDown(RGFW_KEY_ALT_L) || rgfw_isKeyDown(RGFW_KEY_ALT_R)),
		rgfw_bool(rgfw_isKeyDown(RGFW_KEY_SHIFT_L) || rgfw_isKeyDown(RGFW_KEY_SHIFT_R)),
		rgfw_bool(rgfw_isKeyDown(RGFW_KEY_SUPER_L) || rgfw_isKeyDown(RGFW_KEY_SUPER_R)),
		scroll);
}

fn void rgfw_window_showMouseFlags(RgfwWindow* win, bool show)
{
	if (show && (win.internal.flags & RGFW_WINDOW_HIDE_MOUSE)) { win.internal.flags ^= RGFW_WINDOW_HIDE_MOUSE; }
	else if (!show && !(win.internal.flags & RGFW_WINDOW_HIDE_MOUSE)) { win.internal.flags |= RGFW_WINDOW_HIDE_MOUSE; }
}

fn bool rgfw_window_isMouseHidden(RgfwWindow* win) => rgfw_bool(win.internal.flags & RGFW_WINDOW_HIDE_MOUSE);
fn bool rgfw_window_borderless(RgfwWindow* win) => rgfw_bool(win.internal.flags & RGFW_WINDOW_NO_BORDER);
fn bool rgfw_window_isFullscreen(RgfwWindow* win) => rgfw_bool(win.internal.flags & RGFW_WINDOW_FULLSCREEN);
fn bool rgfw_window_allowsDND(RgfwWindow* win) => rgfw_bool(win.internal.flags & RGFW_WINDOW_ALLOW_DND);
fn bool rgfw_window_setMouseDefault(RgfwWindow* win) => rgfw_window_setMouseStandard(win, RGFW_MOUSE_NORMAL);
fn bool rgfw_window_setMouseStandard(RgfwWindow* win, RgfwMouseIcon icon) => rgfw_window_setMouse(win, _rgfw.standardMice[(usz)icon]);
fn bool rgfw_window_setMouse(RgfwWindow* win, RgfwMouse* mouse) { if (mouse != _rgfw.hiddenMouse) win.internal.mouse = mouse; return rgfw_window_setMousePlatform(win, mouse); }
fn void rgfw_window_showMouse(RgfwWindow* win, bool show) { rgfw_window_showMouseFlags(win, show); if (show == RGFW_FALSE) { rgfw_window_setMouse(win, _rgfw.hiddenMouse); } else { rgfw_window_setMouse(win, win.internal.mouse); } }

fn bool rgfw_isLatin(char* string, usz length)
{
	for (usz i = 0; i < length; i++) if ((char)string[i] >= 0x80) return RGFW_TRUE;
	return RGFW_FALSE;
}

fn uint rgfw_decodeUTF8(char* string, usz* starting_index)
{
	uint[6] offsets = {0x00000000u, 0x00003080u, 0x000e2080u, 0x03c82080u, 0xfa082080u, 0x82082080u};
	uint codepoint = (char)string[*starting_index];
	usz count = 1;
	while ((string[count + *starting_index] & 0xc0) == 0x80)
	{
		codepoint = (codepoint << 6) + (char)string[count + *starting_index];
		count++;
	}
	*starting_index += count;
	return codepoint - offsets[count - 1];
}

fn void rgfw_window_show(RgfwWindow* win) { if (win.internal.flags & RGFW_WINDOW_FOCUS_ON_SHOW) rgfw_window_focus(win); win.internal.flags &= ~((uint)RGFW_WINDOW_HIDE); rgfw_window_showPlatform(win); if (win.internal.flags & RGFW_WINDOW_MAXIMIZE) rgfw_window_maximize(win); }
fn void rgfw_window_restore(RgfwWindow* win) { rgfw_window_restorePlatform(win); win.internal.flags &= ~((uint)RGFW_WINDOW_MAXIMIZE); rgfw_window_show(win); rgfw_window_move(win, win.internal.oldX, win.internal.oldY); rgfw_window_resize(win, win.internal.oldW, win.internal.oldH); }
fn void rgfw_window_minimize(RgfwWindow* win) { rgfw_window_minimizePlatform(win); win.internal.flags &= ~((uint)RGFW_WINDOW_MAXIMIZE); }
fn void rgfw_window_maximize(RgfwWindow* win) { win.internal.oldX = win.x; win.internal.oldY = win.y; win.internal.oldW = win.w; win.internal.oldH = win.h; win.internal.flags |= RGFW_WINDOW_MAXIMIZE; rgfw_window_fetchSize(win, null, null); rgfw_window_maximizePlatform(win); }
fn void rgfw_window_move(RgfwWindow* win, int x, int y) { win.x = x; win.y = y; rgfw_window_movePlatform(win, x, y); win.internal.flags &= ~((uint)RGFW_WINDOW_MAXIMIZE); }
fn void rgfw_window_resize(RgfwWindow* win, int w, int h) { win.w = w; win.h = h; rgfw_window_resizePlatform(win, w, h); win.internal.flags &= ~((uint)RGFW_WINDOW_MAXIMIZE); }

fn uint rgfw_winapiWindowGetStyle(RgfwWindow* win, RgfwWindowFlags flags)
{
	uint style = 0x04000000 | 0x02000000; // WS_CLIPSIBLINGS | WS_CLIPCHILDREN
	if (flags & RGFW_WINDOW_FULLSCREEN) { style |= 0x80000000; } // WS_POPUP
	else
	{
		style |= 0x00080000 | 0x00020000; // WS_SYSMENU | WS_MINIMIZEBOX
		if (!(flags & RGFW_WINDOW_NO_BORDER))
		{
			style |= 0x00C00000; // WS_CAPTION
			if (!(flags & RGFW_WINDOW_NO_RESIZE)) style |= 0x00010000 | 0x00040000; // WS_MAXIMIZEBOX | WS_THICKFRAME
		}
		else { style |= 0x80000000; }
	}
	return style;
}

fn uint rgfw_winapiWindowGetExStyle(RgfwWindow* win, RgfwWindowFlags flags)
{
	uint style = 0x00040000; // WS_EX_APPWINDOW
	if ((flags & RGFW_WINDOW_FULLSCREEN) || (flags & RGFW_WINDOW_FLOATING) || rgfw_window_isFloating(win)) style |= 0x00000008; // WS_EX_TOPMOST
	return style;
}

fn bool rgfw_createUTF8FromWideStringWin32(ushort* source, char* output, usz max)
{
	if (source == null) return RGFW_FALSE;
	int size = wideCharToMultiByte(65001 /* CP_UTF8 */, 0, source, -1, null, 0, null, null);
	if (size == 0) return RGFW_FALSE;
	if (size > (int)max) size = (int)max;
	if (wideCharToMultiByte(65001, 0, source, -1, output, size, null, null) == 0) return RGFW_FALSE;
	output[size] = 0;
	return RGFW_TRUE;
}

fn void rgfw_win32MakeWindowTransparent(RgfwWindow* win)
{
	if (!(win.internal.flags & RGFW_WINDOW_TRANSPARENT)) return;

	DwmBlurBehind bb;
	rgfw_memzero(&bb, DwmBlurBehind::size);
	bb.dwFlags = 0x1; // DWM_BB_ENABLE
	bb.fEnable = 1;
	bb.hRgnBlur = null;
	if (dwmEnableBlurBehindWindow(win.src.window, &bb) != 0)
	{
		setWindowLong(win.src.window, -20 /* GWL_EXSTYLE */, 0x00080000); // WS_EX_LAYERED
		setLayeredWindowAttributes(win.src.window, 0, (char)128, 2 /* LWA_ALPHA */);
	}
}

fn bool rgfw_win32GetDarkModeState()
{
	uint lightMode = 1;
	uint len = uint::size;
	static ushort[61] subKey = {(ushort)'S', (ushort)'o', (ushort)'f', (ushort)'t', (ushort)'w', (ushort)'a', (ushort)'r', (ushort)'e', (ushort)'\\', (ushort)'M', (ushort)'i', (ushort)'c', (ushort)'r', (ushort)'o', (ushort)'s', (ushort)'o', (ushort)'f', (ushort)'t', (ushort)'\\', (ushort)'W', (ushort)'i', (ushort)'n', (ushort)'d', (ushort)'o', (ushort)'w', (ushort)'s', (ushort)'\\', (ushort)'C', (ushort)'u', (ushort)'r', (ushort)'r', (ushort)'e', (ushort)'n', (ushort)'t', (ushort)'V', (ushort)'e', (ushort)'r', (ushort)'s', (ushort)'i', (ushort)'o', (ushort)'n', (ushort)'\\', (ushort)'T', (ushort)'h', (ushort)'e', (ushort)'m', (ushort)'e', (ushort)'s', (ushort)'\\', (ushort)'P', (ushort)'e', (ushort)'r', (ushort)'s', (ushort)'o', (ushort)'n', (ushort)'a', (ushort)'l', (ushort)'i', (ushort)'z', (ushort)'e', 0}; // "Software\Microsoft\Windows\CurrentVersion\Themes\Personalize"
	static ushort[18] valueName = {(ushort)'A', (ushort)'p', (ushort)'p', (ushort)'s', (ushort)'U', (ushort)'s', (ushort)'e', (ushort)'L', (ushort)'i', (ushort)'g', (ushort)'h', (ushort)'t', (ushort)'T', (ushort)'h', (ushort)'e', (ushort)'m', (ushort)'e', 0}; // "AppsUseLightTheme"
	regGetValueW((void*)(uptr)0x80000001 /* HKEY_CURRENT_USER */, (ushort*)&subKey, (ushort*)&valueName, 0x00000010 /* RRF_RT_REG_DWORD */, null, &lightMode, &len);
	return rgfw_bool(lightMode == 0);
}

fn void rgfw_win32MakeWindowDarkMode(RgfwWindow* win, bool state)
{
	int value = (state == RGFW_TRUE) ? 1 : 0;
	dwmSetWindowAttribute(win.src.window, 20 /* DWMWA_USE_IMMERSIVE_DARK_MODE */, &value, (uint)int::size);
}

fn void rgfw_window_checkMode(RgfwWindow* win)
{
	if (rgfw_window_isMinimized(win))
	{
		rgfw_windowMinimizedCallback(win);
	}
	else if (rgfw_window_isMaximized(win))
	{
		rgfw_windowMaximizedCallback(win, win.x, win.y, win.w, win.h);
	}
	else if (((win.internal.flags & RGFW_WINDOW_MINIMIZE) && !rgfw_window_isMaximized(win)) ||
	         ((win.internal.flags & RGFW_WINDOW_MAXIMIZE) && !rgfw_window_isMaximized(win)))
	{
		rgfw_windowRestoredCallback(win, win.x, win.y, win.w, win.h);
	}
}

fn iptr rgfw_wndProcW(HWnd* hWnd, uint message, usz wParam, iptr lParam)
{
	if (message == 0x007E /* WM_DISPLAYCHANGE */) rgfw_pollMonitors();

	if (hWnd == _rgfw.helperWindow) return defWindowProcW(hWnd, message, wParam, lParam);

	RgfwWindow* win = (RgfwWindow*)getPropW(hWnd, rgfw_wideRgfwPropName());
	if (win == null) return defWindowProcW(hWnd, message, wParam, lParam);

	Rect frame;
	rgfw_memzero(&frame, Rect::size);
	uint style = rgfw_winapiWindowGetStyle(win, win.internal.flags);
	uint exStyle = rgfw_winapiWindowGetExStyle(win, win.internal.flags);
	adjustWindowRectEx(&frame, style, 0, exStyle);

	switch (message)
	{
		case 0x0010: case 0x0012: rgfw_windowCloseCallback(win); return 0; // WM_CLOSE, WM_QUIT
		case 0x0006: // WM_ACTIVATE
		{
			bool inFocus = rgfw_bool((wParam & 0xffff) != 0);
			rgfw_windowFocusCallback(win, inFocus);
			return defWindowProcW(hWnd, message, wParam, lParam);
		}
		case 0x0003: // WM_MOVE
		{
			if (win.internal.captureMouse) rgfw_window_captureMousePlatform(win, RGFW_TRUE);
			rgfw_windowMovedCallback(win, (int)(short)(lParam & 0xffff), (int)(short)((lParam >> 16) & 0xffff));
			return defWindowProcW(hWnd, message, wParam, lParam);
		}
		case 0x0005: // WM_SIZE
		{
			if (win.internal.captureMouse) rgfw_window_captureMousePlatform(win, RGFW_TRUE);
			rgfw_windowResizedCallback(win, (int)(lParam & 0xffff), (int)((lParam >> 16) & 0xffff));
			rgfw_window_checkMode(win);
			return defWindowProcW(hWnd, message, wParam, lParam);
		}
		case 0x0021: // WM_MOUSEACTIVATE
		{
			if (((lParam >> 16) & 0xffff) == 0x0201 /* WM_LBUTTONDOWN */)
			{
				if ((lParam & 0xffff) != 1 /* HTCLIENT */) win.src.actionFrame = RGFW_TRUE;
			}
			break;
		}
		case 0x0215: // WM_CAPTURECHANGED
		{
			if (lParam == 0 && win.src.actionFrame)
			{
				rgfw_window_captureMousePlatform(win, win.internal.captureMouse);
				win.src.actionFrame = RGFW_FALSE;
			}
			break;
		}
		case 0x02E0: // WM_DPICHANGED
		{
			float scaleX = (float)((wParam >> 16) & 0xffff) / 96.0f;
			float scaleY = (float)(wParam & 0xffff) / 96.0f;
			rgfw_scaleUpdatedCallback(win, scaleX, scaleY);
			return defWindowProcW(hWnd, message, wParam, lParam);
		}
		case 0x0214: // WM_SIZING
		{
			if (win.src.aspectRatioW == 0 && win.src.aspectRatioH == 0) break;

			Rect* area = (Rect*)(uptr)lParam;
			int edge = (int)wParam;
			double ratio = (double)win.src.aspectRatioW / (double)win.src.aspectRatioH;

			if (edge == 1 || edge == 7 || edge == 2 || edge == 8) // WMSZ_LEFT, WMSZ_BOTTOMLEFT, WMSZ_RIGHT, WMSZ_BOTTOMRIGHT
			{
				area.bottom = area.top + (frame.bottom - frame.top) + (int)(((double)((area.right - area.left) - (frame.right - frame.left))) / ratio);
			}
			else if (edge == 4 || edge == 5) // WMSZ_TOPLEFT, WMSZ_TOPRIGHT
			{
				area.top = area.bottom - (frame.bottom - frame.top) - (int)(((double)((area.right - area.left) - (frame.right - frame.left))) / ratio);
			}
			else if (edge == 3 || edge == 6) // WMSZ_TOP, WMSZ_BOTTOM
			{
				area.right = area.left + (frame.right - frame.left) + (int)(((double)((area.bottom - area.top) - (frame.bottom - frame.top))) * ratio);
			}
			return 1; // TRUE
		}
		case 0x0024: // WM_GETMINMAXINFO
		{
			MinMaxInfo* mmi = (MinMaxInfo*)(uptr)lParam;
			bool resize = rgfw_bool((win.src.minSizeW == win.src.maxSizeW) && (win.src.minSizeH == win.src.maxSizeH));
			rgfw_setBit(&win.internal.flags, RGFW_WINDOW_NO_RESIZE, resize);

			mmi.ptMinTrackSize.x = win.src.minSizeW + (frame.right - frame.left);
			mmi.ptMinTrackSize.y = win.src.minSizeH + (frame.bottom - frame.top);
			if (win.src.maxSizeW == 0 && win.src.maxSizeH == 0) return defWindowProcW(hWnd, message, wParam, lParam);

			mmi.ptMaxTrackSize.x = win.src.maxSizeW + (frame.right - frame.left);
			mmi.ptMaxTrackSize.y = win.src.maxSizeH + (frame.bottom - frame.top);
			return defWindowProcW(hWnd, message, wParam, lParam);
		}
		case 0x0231: case 0x0232: // WM_ENTERSIZEMOVE, WM_EXITSIZEMOVE
		{
			if (win.src.actionFrame) rgfw_window_captureMousePlatform(win, win.internal.captureMouse);
			break;
		}
		case 0x0113: rgfw_windowRefreshCallback(win, 0, 0, win.w, win.h); break; // WM_TIMER
		case 0x00A1: // WM_NCLBUTTONDOWN
		{
			// workaround for half-second pause when starting to move window
			Point point = {0, 0};
			if (sendMessage(win.src.window, 0x0084 /* WM_NCHITTEST */, wParam, lParam) != 2 /* HTCAPTION */ || getCursorPos(&point) == 0) break;

			screenToClient(win.src.window, &point);
			postMessage(win.src.window, 0x0200 /* WM_MOUSEMOVE */, 0, (iptr)((uint)point.x | ((uint)point.y << 16)));
			break;
		}
		case 0x000F: // WM_PAINT
		{
			Rect rect;
			if (getUpdateRect(hWnd, &rect, 0) != 0)
			{
				rgfw_windowRefreshCallback(win, rect.left, rect.top, rect.right - rect.left, rect.bottom - rect.top);
			}
			else
			{
				char[128] ps;
				beginPaint(hWnd, &ps);
				rgfw_windowRefreshCallback(win, 0, 0, win.w, win.h);
				endPaint(hWnd, &ps);
			}
			return defWindowProcW(hWnd, message, wParam, lParam);
		}
		case 0x031E: case 0x0320: rgfw_win32MakeWindowTransparent(win); break; // WM_DWMCOMPOSITIONCHANGED, WM_DWMCOLORIZATIONCOLORCHANGED
		case 0x0102: case 0x0106: // WM_CHAR, WM_SYSCHAR
		{
			if (wParam >= 0xd800 && wParam <= 0xdbff) { win.src.highSurrogate = (ushort)wParam; }
			else
			{
				uint codepoint = 0;
				if (wParam >= 0xdc00 && wParam <= 0xdfff)
				{
					if (win.src.highSurrogate != 0)
					{
						codepoint += (uint)((win.src.highSurrogate - 0xd800) << 10);
						codepoint += (uint)((ushort)wParam - 0xdc00);
						codepoint += 0x10000;
					}
				}
				else { codepoint = (ushort)wParam; }
				win.src.highSurrogate = 0;
				rgfw_keyCharCallback(win, codepoint);
			}
			return 0;
		}
		case 0x0109: // WM_UNICHAR
		{
			if (wParam == 0xFFFF /* UNICODE_NOCHAR */) return 1;
			rgfw_keyCharCallback(win, (uint)wParam);
			return 0;
		}
		case 0x0101: case 0x0105: // WM_KEYUP, WM_SYSKEYUP
		{
			if (!(win.internal.enabledEvents & RGFW_KEY_RELEASED_FLAG)) return defWindowProcW(hWnd, message, wParam, lParam);
			int scancode = (int)((lParam >> 16) & (0x0100 | 0xff));
			if (scancode == 0) scancode = (int)mapVirtualKeyW((uint)wParam, 0 /* MAPVK_VK_TO_VSC */);
			switch (scancode)
			{
				case 0x54: scancode = 0x137;
				case 0x146: scancode = 0x45;
				case 0x136: scancode = 0x36;
				default: break;
			}
			RgfwKey value = (RgfwKey)rgfw_apiKeyToRGFW((uint)scancode);
			if (wParam == 0x11 /* VK_CONTROL */) { value = ((lParam >> 16) & 0x0100) != 0 ? RGFW_KEY_CONTROL_R : RGFW_KEY_CONTROL_L; }
			rgfw_keyUpdateKeyMods(win, rgfw_bool(getKeyState(0x14) & 1), rgfw_bool(getKeyState(0x90) & 1), rgfw_bool(getKeyState(0x91) & 1));
			rgfw_keyCallback(win, value, win.internal.mod, RGFW_FALSE, RGFW_FALSE);
			break;
		}
		case 0x0100: case 0x0104: // WM_KEYDOWN, WM_SYSKEYDOWN
		{
			if (!(win.internal.enabledEvents & RGFW_KEY_PRESSED_FLAG)) return defWindowProcW(hWnd, message, wParam, lParam);
			int scancode = (int)((lParam >> 16) & (0x0100 | 0xff));
			if (scancode == 0) scancode = (int)mapVirtualKeyW((uint)wParam, 0);
			switch (scancode)
			{
				case 0x54: scancode = 0x137;
				case 0x146: scancode = 0x45;
				case 0x136: scancode = 0x36;
				default: break;
			}
			RgfwKey value = (RgfwKey)rgfw_apiKeyToRGFW((uint)scancode);
			if (wParam == 0x11) { value = ((lParam >> 16) & 0x0100) != 0 ? RGFW_KEY_CONTROL_R : RGFW_KEY_CONTROL_L; }
			bool repeat = rgfw_isKeyDown(value);
			rgfw_keyUpdateKeyMods(win, rgfw_bool(getKeyState(0x14) & 1), rgfw_bool(getKeyState(0x90) & 1), rgfw_bool(getKeyState(0x91) & 1));
			rgfw_keyCallback(win, value, win.internal.mod, repeat, RGFW_TRUE);
			break;
		}
		case 0x0200: // WM_MOUSEMOVE
		{
			int mx = (int)(short)(lParam & 0xffff);
			int my = (int)(short)((lParam >> 16) & 0xffff);
			if (win.internal.mouseInside == RGFW_FALSE) rgfw_mouseNotifyCallback(win, mx, my, RGFW_TRUE);
			rgfw_mouseMotionCallback(win, mx, my);
			break;
		}
		case 0x00FF: // WM_INPUT
		{
			if (!(win.internal.rawMouse || _rgfw.rawMouse)) return defWindowProcW(hWnd, message, wParam, lParam);

			RawInputMouse raw;
			rgfw_memzero(&raw, RawInputMouse::size);
			uint rawSize = (uint)RawInputMouse::size;
			getRawInputData((HRawInput)(uptr)lParam, RID_INPUT, &raw, &rawSize, (uint)RawInputHeader::size);

			if (raw.header.dwType != RIM_TYPEMOUSE || (raw.mouse.lLastX == 0 && raw.mouse.lLastY == 0)) break;

			float vecX = 0.0f;
			float vecY = 0.0f;

			if (raw.mouse.usFlags & MOUSE_MOVE_ABSOLUTE)
			{
				Point pos = {0, 0};
				int width, height;
				if (raw.mouse.usFlags & MOUSE_VIRTUAL_DESKTOP)
				{
					pos.x += getSystemMetrics(SM_XVIRTUALSCREEN);
					pos.y += getSystemMetrics(SM_YVIRTUALSCREEN);
					width = getSystemMetrics(SM_CXVIRTUALSCREEN);
					height = getSystemMetrics(SM_CYVIRTUALSCREEN);
				}
				else
				{
					width = getSystemMetrics(SM_CXSCREEN);
					height = getSystemMetrics(SM_CYSCREEN);
				}

				pos.x += (int)(((float)raw.mouse.lLastX / 65535.0f) * (float)width);
				pos.y += (int)(((float)raw.mouse.lLastY / 65535.0f) * (float)height);
				screenToClient(win.src.window, &pos);

				vecX = (float)(pos.x - win.internal.lastMouseX);
				vecY = (float)(pos.y - win.internal.lastMouseY);
			}
			else
			{
				vecX = (float)raw.mouse.lLastX;
				vecY = (float)raw.mouse.lLastY;
			}

			rgfw_rawMotionCallback(win, vecX, vecY);
			break;
		}
		case 0x02A3: rgfw_mouseNotifyCallback(win, win.internal.lastMouseX, win.internal.lastMouseY, RGFW_FALSE); break; // WM_MOUSELEAVE
		case 0x0201: case 0x0204: case 0x0207: case 0x020B: // WM_LBUTTONDOWN, WM_RBUTTONDOWN, WM_MBUTTONDOWN, WM_XBUTTONDOWN
		{
			RgfwMouseButton value = 0;
			if (message == 0x020B) { value = (RgfwMouseButton)(RGFW_MOUSE_MISC1 + (((wParam >> 16) & 0xffff) == 2 ? 1 : 0)); }
			else if (message == 0x0201) { value = RGFW_MOUSE_LEFT; }
			else if (message == 0x0204) { value = RGFW_MOUSE_RIGHT; }
			else { value = RGFW_MOUSE_MIDDLE; }
			rgfw_mouseButtonCallback(win, value, RGFW_TRUE);
			break;
		}
		case 0x0202: case 0x0205: case 0x0208: case 0x020C: // ...UP variants
		{
			RgfwMouseButton value = 0;
			if (message == 0x020C) { value = (RgfwMouseButton)(RGFW_MOUSE_MISC1 + (((wParam >> 16) & 0xffff) == 2 ? 1 : 0)); }
			else if (message == 0x0202) { value = RGFW_MOUSE_LEFT; }
			else if (message == 0x0205) { value = RGFW_MOUSE_RIGHT; }
			else { value = RGFW_MOUSE_MIDDLE; }
			rgfw_mouseButtonCallback(win, value, RGFW_FALSE);
			break;
		}
		case 0x020A: // WM_MOUSEWHEEL
		{
			float scrollY = (float)((short)((wParam >> 16) & 0xffff)) / 120.0f; // WHEEL_DELTA
			rgfw_mouseScrollCallback(win, 0.0f, scrollY);
			break;
		}
		case 0x020E: // WM_MOUSEHWHEEL
		{
			float scrollX = -(float)((short)((wParam >> 16) & 0xffff)) / 120.0f;
			rgfw_mouseScrollCallback(win, scrollX, 0.0f);
			break;
		}
		case 0x0233: // WM_DROPFILES
		{
			void* drop = (void*)wParam;
			Point pt;
			dragQueryPoint(drop, &pt);
			rgfw_dataDragCallback(win, RGFW_DATA_FILE, RGFW_DND_ACTION_MOVE, pt.x, pt.y);
			if (!(win.internal.enabledEvents & RGFW_DATA_DROP_FLAG)) return defWindowProcW(hWnd, message, wParam, lParam);
			uint count = dragQueryFileW(drop, 0xffffffff, null, 0);
			for (uint i = 0; i < count; i++)
			{
				uint length = dragQueryFileW(drop, i, null, 0);
				if (length == 0) continue;
				ushort* buffer = (ushort*)rgfw_alloc_impl((usz)(length + 1) * 2);
				char* cbuffer = (char*)rgfw_alloc_impl((usz)length + 1);
				dragQueryFileW(drop, i, buffer, length + 1);
				rgfw_createUTF8FromWideStringWin32(buffer, cbuffer, length);
				rgfw_dataDropCallback(win, cbuffer, (usz)length + 1, RGFW_DATA_FILE);
				rgfw_free_impl(buffer);
				rgfw_free_impl(cbuffer);
			}
			dragFinish(drop);
			break;
		}
		default: break;
	}

	return defWindowProcW(hWnd, message, wParam, lParam);
}

fn ushort* rgfw_wideRgfwPropName()
{
	static ushort[5] name = {(ushort)'R', (ushort)'G', (ushort)'F', (ushort)'W', 0};
	return &name;
}

fn void rgfw_initKeycodesPlatform()
{
	_rgfw.keycodes[0x00B] = RGFW_KEY0; _rgfw.keycodes[0x002] = RGFW_KEY1; _rgfw.keycodes[0x003] = RGFW_KEY2;
	_rgfw.keycodes[0x004] = RGFW_KEY3; _rgfw.keycodes[0x005] = RGFW_KEY4; _rgfw.keycodes[0x006] = RGFW_KEY5;
	_rgfw.keycodes[0x007] = RGFW_KEY6; _rgfw.keycodes[0x008] = RGFW_KEY7; _rgfw.keycodes[0x009] = RGFW_KEY8;
	_rgfw.keycodes[0x00A] = RGFW_KEY9;
	_rgfw.keycodes[0x01E] = RGFW_KEY_A; _rgfw.keycodes[0x030] = RGFW_KEY_B; _rgfw.keycodes[0x02E] = RGFW_KEY_C;
	_rgfw.keycodes[0x020] = RGFW_KEY_D; _rgfw.keycodes[0x012] = RGFW_KEY_E; _rgfw.keycodes[0x021] = RGFW_KEY_F;
	_rgfw.keycodes[0x022] = RGFW_KEY_G; _rgfw.keycodes[0x023] = RGFW_KEY_H; _rgfw.keycodes[0x017] = RGFW_KEY_I;
	_rgfw.keycodes[0x024] = RGFW_KEY_J; _rgfw.keycodes[0x025] = RGFW_KEY_K; _rgfw.keycodes[0x026] = RGFW_KEY_L;
	_rgfw.keycodes[0x032] = RGFW_KEY_M; _rgfw.keycodes[0x031] = RGFW_KEY_N; _rgfw.keycodes[0x018] = RGFW_KEY_O;
	_rgfw.keycodes[0x019] = RGFW_KEY_P; _rgfw.keycodes[0x010] = RGFW_KEY_Q; _rgfw.keycodes[0x013] = RGFW_KEY_R;
	_rgfw.keycodes[0x01F] = RGFW_KEY_S; _rgfw.keycodes[0x014] = RGFW_KEY_T; _rgfw.keycodes[0x016] = RGFW_KEY_U;
	_rgfw.keycodes[0x02F] = RGFW_KEY_V; _rgfw.keycodes[0x011] = RGFW_KEY_W; _rgfw.keycodes[0x02D] = RGFW_KEY_X;
	_rgfw.keycodes[0x015] = RGFW_KEY_Y; _rgfw.keycodes[0x02C] = RGFW_KEY_Z;
	_rgfw.keycodes[0x028] = RGFW_KEY_APOSTROPHE; _rgfw.keycodes[0x02B] = RGFW_KEY_BACK_SLASH;
	_rgfw.keycodes[0x033] = RGFW_KEY_COMMA; _rgfw.keycodes[0x00D] = RGFW_KEY_EQUALS;
	_rgfw.keycodes[0x029] = RGFW_KEY_BACKTICK; _rgfw.keycodes[0x01A] = RGFW_KEY_BRACKET;
	_rgfw.keycodes[0x00C] = RGFW_KEY_MINUS; _rgfw.keycodes[0x034] = RGFW_KEY_PERIOD;
	_rgfw.keycodes[0x01B] = RGFW_KEY_CLOSE_BRACKET; _rgfw.keycodes[0x027] = RGFW_KEY_SEMICOLON;
	_rgfw.keycodes[0x035] = RGFW_KEY_SLASH; _rgfw.keycodes[0x056] = RGFW_KEY_WORLD2;
	_rgfw.keycodes[0x00E] = RGFW_KEY_BACK_SPACE; _rgfw.keycodes[0x153] = RGFW_KEY_DELETE;
	_rgfw.keycodes[0x14F] = RGFW_KEY_END; _rgfw.keycodes[0x01C] = RGFW_KEY_ENTER;
	_rgfw.keycodes[0x001] = RGFW_KEY_ESCAPE; _rgfw.keycodes[0x147] = RGFW_KEY_HOME;
	_rgfw.keycodes[0x152] = RGFW_KEY_INSERT; _rgfw.keycodes[0x15D] = RGFW_KEY_MENU;
	_rgfw.keycodes[0x151] = RGFW_KEY_PAGE_DOWN; _rgfw.keycodes[0x149] = RGFW_KEY_PAGE_UP;
	_rgfw.keycodes[0x045] = RGFW_KEY_PAUSE; _rgfw.keycodes[0x039] = RGFW_KEY_SPACE;
	_rgfw.keycodes[0x00F] = RGFW_KEY_TAB; _rgfw.keycodes[0x03A] = RGFW_KEY_CAPS_LOCK;
	_rgfw.keycodes[0x145] = RGFW_KEY_NUM_LOCK; _rgfw.keycodes[0x046] = RGFW_KEY_SCROLL_LOCK;
	_rgfw.keycodes[0x03B] = RGFW_KEY_F1; _rgfw.keycodes[0x03C] = RGFW_KEY_F2; _rgfw.keycodes[0x03D] = RGFW_KEY_F3;
	_rgfw.keycodes[0x03E] = RGFW_KEY_F4; _rgfw.keycodes[0x03F] = RGFW_KEY_F5; _rgfw.keycodes[0x040] = RGFW_KEY_F6;
	_rgfw.keycodes[0x041] = RGFW_KEY_F7; _rgfw.keycodes[0x042] = RGFW_KEY_F8; _rgfw.keycodes[0x043] = RGFW_KEY_F9;
	_rgfw.keycodes[0x044] = RGFW_KEY_F10; _rgfw.keycodes[0x057] = RGFW_KEY_F11; _rgfw.keycodes[0x058] = RGFW_KEY_F12;
	_rgfw.keycodes[0x064] = RGFW_KEY_F13; _rgfw.keycodes[0x065] = RGFW_KEY_F14; _rgfw.keycodes[0x066] = RGFW_KEY_F15;
	_rgfw.keycodes[0x067] = RGFW_KEY_F16; _rgfw.keycodes[0x068] = RGFW_KEY_F17; _rgfw.keycodes[0x069] = RGFW_KEY_F18;
	_rgfw.keycodes[0x06A] = RGFW_KEY_F19; _rgfw.keycodes[0x06B] = RGFW_KEY_F20; _rgfw.keycodes[0x06C] = RGFW_KEY_F21;
	_rgfw.keycodes[0x06D] = RGFW_KEY_F22; _rgfw.keycodes[0x06E] = RGFW_KEY_F23; _rgfw.keycodes[0x076] = RGFW_KEY_F24;
	_rgfw.keycodes[0x038] = RGFW_KEY_ALT_L; _rgfw.keycodes[0x01D] = RGFW_KEY_CONTROL_L; _rgfw.keycodes[0x02A] = RGFW_KEY_SHIFT_L;
	_rgfw.keycodes[0x15B] = RGFW_KEY_SUPER_L; _rgfw.keycodes[0x137] = RGFW_KEY_PRINT_SCREEN;
	_rgfw.keycodes[0x138] = RGFW_KEY_ALT_R; _rgfw.keycodes[0x11D] = RGFW_KEY_CONTROL_R;
	_rgfw.keycodes[0x036] = RGFW_KEY_SHIFT_R; _rgfw.keycodes[0x15C] = RGFW_KEY_SUPER_R;
	_rgfw.keycodes[0x150] = RGFW_KEY_DOWN; _rgfw.keycodes[0x14B] = RGFW_KEY_LEFT;
	_rgfw.keycodes[0x14D] = RGFW_KEY_RIGHT; _rgfw.keycodes[0x148] = RGFW_KEY_UP;
	_rgfw.keycodes[0x052] = RGFW_KEY_PAD0; _rgfw.keycodes[0x04F] = RGFW_KEY_PAD1; _rgfw.keycodes[0x050] = RGFW_KEY_PAD2;
	_rgfw.keycodes[0x051] = RGFW_KEY_PAD3; _rgfw.keycodes[0x04B] = RGFW_KEY_PAD4; _rgfw.keycodes[0x04C] = RGFW_KEY_PAD5;
	_rgfw.keycodes[0x04D] = RGFW_KEY_PAD6; _rgfw.keycodes[0x047] = RGFW_KEY_PAD7; _rgfw.keycodes[0x048] = RGFW_KEY_PAD8;
	_rgfw.keycodes[0x049] = RGFW_KEY_PAD9; _rgfw.keycodes[0x04E] = RGFW_KEY_PAD_PLUS; _rgfw.keycodes[0x053] = RGFW_KEY_PAD_PERIOD;
	_rgfw.keycodes[0x135] = RGFW_KEY_PAD_SLASH; _rgfw.keycodes[0x11C] = RGFW_KEY_PAD_RETURN;
	_rgfw.keycodes[0x059] = RGFW_KEY_PAD_EQUAL; _rgfw.keycodes[0x037] = RGFW_KEY_PAD_MULTIPLY; _rgfw.keycodes[0x04A] = RGFW_KEY_PAD_MINUS;
}

fn int rgfw_initPlatform(char* className, RgfwInitFlags flags)
{
	setProcessDPIAware();
	timeBeginPeriod(1);

	_rgfw.instance = getModuleHandleW(null);
	multiByteToWideChar(65001, 0, className, -1, (ushort*)&_rgfw.wideClassName, 255);

	rgfw_memzero(&_rgfw.wndClass, WndClassW::size);
	_rgfw.wndClass.className = (ushort*)&_rgfw.wideClassName;
	_rgfw.wndClass.hInstance = _rgfw.instance;
	_rgfw.wndClass.hCursor = loadCursorA(null, (char*)(uptr)32512 /* IDC_ARROW */);
	_rgfw.wndClass.wndProc = &rgfw_wndProcW;
	_rgfw.wndClass.clsExtra = (int)usz::size;

	_rgfw.wndClass.hIcon = (HIcon)loadImageA(_rgfw.instance, "RGFW_ICON", 1 /* IMAGE_ICON */, 0, 0, 0x00000040 | 0x00008000 /* LR_DEFAULTSIZE|LR_SHARED */);
	if (_rgfw.wndClass.hIcon == null)
	{
		_rgfw.wndClass.hIcon = (HIcon)loadImageA(null, (char*)(uptr)32512 /* IDI_APPLICATION */, 1, 0, 0, 0x00000040 | 0x00008000);
	}

	registerClassW(&_rgfw.wndClass);
	_rgfw.helperWindow = createWindowExW(0, _rgfw.wndClass.className, null, 0, 0, 0, 0, 0, null, null, _rgfw.instance, null);

	char[4] blk = {0, 0, 0, 0};
	_rgfw.hiddenMouse = rgfw_createMouse((char*)&blk, 1, 1, RGFW_FORMAT_RGBA8);
	return 0;
}

fn RgfwWindow* rgfw_createWindowPlatform(char* name, RgfwWindowFlags flags, RgfwWindow* win)
{
	if (name[0] == 0) name = " ";
	win.src.hIconSmall = null; win.src.hIconBig = null;
	win.src.maxSizeW = 0; win.src.maxSizeH = 0;
	win.src.minSizeW = 0; win.src.minSizeH = 0;
	win.src.aspectRatioW = 0; win.src.aspectRatioH = 0;

	uint windowStyle = 0x04000000 | 0x02000000; // WS_CLIPSIBLINGS | WS_CLIPCHILDREN
	if (!(flags & RGFW_WINDOW_NO_BORDER))
	{
		windowStyle |= 0x00C00000 | 0x00080000 | 0x00800000 | 0x00020000; // WS_CAPTION|WS_SYSMENU|WS_BORDER|WS_MINIMIZEBOX
		if (!(flags & RGFW_WINDOW_NO_RESIZE)) windowStyle |= 0x00040000 | 0x00010000; // WS_SIZEBOX | WS_MAXIMIZEBOX
	}
	else { windowStyle |= 0x80000000 | 0x10000000 | 0x00080000; } // WS_POPUP|WS_VISIBLE|WS_SYSMENU

	ushort[256] wideName;
	multiByteToWideChar(65001, 0, name, -1, &wideName, 255);

	Rect rect = {0, 0, win.w, win.h};
	uint style = rgfw_winapiWindowGetStyle(win, flags);
	uint exStyle = rgfw_winapiWindowGetExStyle(win, flags);
	adjustWindowRectEx(&rect, style, 0, exStyle);

	win.src.window = createWindowExW(0, _rgfw.wndClass.className, &wideName, windowStyle, win.x + rect.left, win.y + rect.top, rect.right - rect.left, rect.bottom - rect.top, null, null, _rgfw.instance, null);
	setPropW(win.src.window, rgfw_wideRgfwPropName(), win);
	rgfw_window_resize(win, win.w, win.h);

	if (flags & RGFW_WINDOW_ALLOW_DND)
	{
		win.internal.flags |= RGFW_WINDOW_ALLOW_DND;
		rgfw_window_setDND(win, 1);
	}
	win.src.hdc = getDC(win.src.window);

	rgfw_win32MakeWindowDarkMode(win, rgfw_win32GetDarkModeState());
	rgfw_win32MakeWindowTransparent(win);
	return win;
}

fn void rgfw_window_setBorder(RgfwWindow* win, bool border)
{
	rgfw_setBit(&win.internal.flags, RGFW_WINDOW_NO_BORDER, rgfw_bool(!border));

	Rect rect;
	getClientRect(win.src.window, &rect);
	iptr style = getWindowLongW(win.src.window, -16 /* GWL_STYLE */);
	style |= (iptr)rgfw_winapiWindowGetStyle(win, win.internal.flags);

	if (border == 0) { style &= ~(iptr)0x00CF0000; } // ~WS_OVERLAPPEDWINDOW
	else
	{
		if (win.internal.flags & RGFW_WINDOW_NO_RESIZE) style &= ~(iptr)0x00010000; // ~WS_MAXIMIZEBOX
		style |= 0x00CF0000; // WS_OVERLAPPEDWINDOW
	}

	uint exStyle = rgfw_winapiWindowGetExStyle(win, win.internal.flags);
	clientToScreen(win.src.window, (Point*)&rect.left);
	clientToScreen(win.src.window, (Point*)&rect.right);
	adjustWindowRectEx(&rect, (uint)style, 0, exStyle);
	setWindowLongW(win.src.window, -16, style);
	setWindowPos(win.src.window, (HWnd*)(uptr)0 /* HWND_TOP */, rect.left, rect.top, rect.right - rect.left, rect.bottom - rect.top, 0x0020 | 0x0010 | 0x0004); // SWP_FRAMECHANGED|NOACTIVATE|NOZORDER
}

fn void rgfw_window_setDND(RgfwWindow* win, bool allow)
{
	rgfw_setBit(&win.internal.flags, RGFW_WINDOW_ALLOW_DND, allow);
	dragAcceptFiles(win.src.window, allow);
}

fn bool rgfw_getGlobalMouse(int* x, int* y)
{
	Point p;
	getCursorPos(&p);
	if (x != null) *x = p.x;
	if (y != null) *y = p.y;
	return RGFW_TRUE;
}

fn void rgfw_window_setAspectRatio(RgfwWindow* win, int w, int h) { win.src.aspectRatioW = w; win.src.aspectRatioH = h; }
fn void rgfw_window_setMinSize(RgfwWindow* win, int w, int h) { win.src.minSizeW = w; win.src.minSizeH = h; }
fn void rgfw_window_setMaxSize(RgfwWindow* win, int w, int h) { win.src.maxSizeW = w; win.src.maxSizeH = h; }

fn void rgfw_window_focus(RgfwWindow* win) { setForegroundWindow(win.src.window); setFocus(win.src.window); }
fn void rgfw_window_raise(RgfwWindow* win)
{
	bringWindowToTop(win.src.window);
	setWindowPos(win.src.window, (HWnd*)(uptr)0, win.x, win.y, win.w, win.h, 0x0001 | 0x0002 | 0x0200 | 0x0020); // NOSIZE|NOMOVE|NOOWNERZORDER|FRAMECHANGED
}

fn void rgfw_window_setFullscreen(RgfwWindow* win, bool fullscreen)
{
	RgfwMonitor* mon = rgfw_window_getMonitor(win);

	if (fullscreen == RGFW_FALSE)
	{
		rgfw_monitor_setMode(mon, &win.internal.oldMode);
		rgfw_window_setBorder(win, 1);

		Rect rect = {0, 0, win.internal.oldW, win.internal.oldH};
		uint style = rgfw_winapiWindowGetStyle(win, win.internal.flags);
		uint exStyle = rgfw_winapiWindowGetExStyle(win, win.internal.flags);
		adjustWindowRectEx(&rect, style, 0, exStyle);
		setWindowPos(win.src.window, (HWnd*)(uptr)0, win.internal.oldX, win.internal.oldY, rect.right - rect.left, rect.bottom - rect.top, 0x0010 | 0x0200 | 0x0004);

		win.internal.flags &= ~(uint)RGFW_WINDOW_FULLSCREEN;
		win.x = win.internal.oldX; win.y = win.internal.oldY; win.w = win.internal.oldW; win.h = win.internal.oldH;
		return;
	}

	win.internal.oldX = win.x; win.internal.oldY = win.y; win.internal.oldW = win.w; win.internal.oldH = win.h;
	win.internal.oldMode = mon.mode;
	win.internal.flags |= RGFW_WINDOW_FULLSCREEN;
	rgfw_window_setBorder(win, 0);

	setWindowPos(win.src.window, (HWnd*)(uptr)(-1) /* HWND_TOPMOST */, mon.x, mon.y, 0, 0, 0x0200 | 0x0020 | 0x0040 | 0x0001);
	win.x = mon.x; win.y = mon.y;

	rgfw_monitor_scaleToWindow(mon, win);
	setWindowPos(win.src.window, (HWnd*)(uptr)(-1), 0, 0, mon.mode.w, mon.mode.h, 0x0200 | 0x0020 | 0x0040 | 0x0002);
	win.w = mon.mode.w; win.h = mon.mode.h;
}

fn void rgfw_window_maximizePlatform(RgfwWindow* win) { rgfw_window_hide(win); showWindow(win.src.window, (int)SW_MAXIMIZE); }
fn void rgfw_window_minimizePlatform(RgfwWindow* win) { showWindow(win.src.window, (int)SW_MINIMIZE); }
fn void rgfw_window_setFloating(RgfwWindow* win, bool floating)
{
	if (floating) { setWindowPos(win.src.window, (HWnd*)(uptr)(-1), 0, 0, 0, 0, 0x0002 | 0x0001 | 0x0040); }
	else { setWindowPos(win.src.window, (HWnd*)(uptr)(-2) /* HWND_NOTOPMOST */, 0, 0, 0, 0, 0x0002 | 0x0001 | 0x0040); }
}
fn void rgfw_window_setOpacity(RgfwWindow* win, char opacity)
{
	setWindowLong(win.src.window, -20, 0x00080000);
	setLayeredWindowAttributes(win.src.window, 0, opacity, 2);
}
fn void rgfw_window_restorePlatform(RgfwWindow* win) {}
fn bool rgfw_window_isFloating(RgfwWindow* win) => rgfw_bool((getWindowLongPtr(win.src.window, -20) & 0x00000008) != 0);
fn void rgfw_stopCheckEvents() { postMessageW(_rgfw.helperWindow, 0, 0, 0); }
fn void rgfw_waitForEvent(int waitMS) { msgWaitForMultipleObjects(0, null, 0, (uint)waitMS, 0x04FF /* QS_ALLINPUT */); }

ushort[8] rgfw_wideDisplayName = {(ushort)'D', (ushort)'I', (ushort)'S', (ushort)'P', (ushort)'L', (ushort)'A', (ushort)'Y', 0};

struct RgfwAttribStack
{
	int* attribs;
	usz count;
	usz max;
}

fn void rgfw_attribStack_init(RgfwAttribStack* stack, int* attribs, usz max) { stack.attribs = attribs; stack.count = 0; stack.max = max; }
fn void rgfw_attribStack_pushAttrib(RgfwAttribStack* stack, int attrib) { stack.attribs[stack.count] = attrib; stack.count += 1; }
fn void rgfw_attribStack_pushAttribs(RgfwAttribStack* stack, int attrib1, int attrib2) { rgfw_attribStack_pushAttrib(stack, attrib1); rgfw_attribStack_pushAttrib(stack, attrib2); }

fn bool rgfw_extensionSupportedStr(char* extensions, char* ext, usz len)
{
	if (extensions == null || ext == null) return RGFW_FALSE;
	while (ext[len - 1] == 0 && len > 3) len--;
	char* start = extensions;
	char* where = strstr(extensions, ext);
	while (where != null)
	{
		char* terminator = where + len;
		if ((where == start || *(where - 1) == ' ') && (*terminator == ' ' || *terminator == 0)) return RGFW_TRUE;
		where = strstr(terminator, ext);
	}
	return RGFW_FALSE;
}

fn RgfwKey rgfw_physicalToMappedKey(RgfwKey key)
{
	uint vsc = rgfw_rgfwToApiKey(key);
	char[256] keyboardState;
	rgfw_memzero(&keyboardState, 256);
	if (getKeyboardState((char*)&keyboardState) == 0) return key;

	uint vk = mapVirtualKeyW(vsc, 1 /* MAPVK_VSC_TO_VK */);
	void* layout = getKeyboardLayout(0);

	ushort[4] charBuffer;
	rgfw_memzero(&charBuffer, 8);
	int result = toUnicodeEx(vk, vsc, (char*)&keyboardState, &charBuffer, 1, 0, layout);
	if (result == 1 && charBuffer[0] < 256) return (RgfwKey)charBuffer[0];

	switch (vk)
	{
		case 0x70: return RGFW_KEY_F1; case 0x71: return RGFW_KEY_F2; case 0x72: return RGFW_KEY_F3;
		case 0x73: return RGFW_KEY_F4; case 0x74: return RGFW_KEY_F5; case 0x75: return RGFW_KEY_F6;
		case 0x76: return RGFW_KEY_F7; case 0x77: return RGFW_KEY_F8; case 0x78: return RGFW_KEY_F9;
		case 0x79: return RGFW_KEY_F10; case 0x7A: return RGFW_KEY_F11; case 0x7B: return RGFW_KEY_F12;
		case 0x7C: return RGFW_KEY_F13; case 0x7D: return RGFW_KEY_F14; case 0x7E: return RGFW_KEY_F15;
		case 0x7F: return RGFW_KEY_F16; case 0x80: return RGFW_KEY_F17; case 0x81: return RGFW_KEY_F18;
		case 0x82: return RGFW_KEY_F19; case 0x83: return RGFW_KEY_F20; case 0x84: return RGFW_KEY_F21;
		case 0x85: return RGFW_KEY_F22; case 0x86: return RGFW_KEY_F23; case 0x87: return RGFW_KEY_F24;
		case 0xA0: return RGFW_KEY_SHIFT_L; case 0xA1: return RGFW_KEY_SHIFT_R;
		case 0xA2: return RGFW_KEY_CONTROL_L; case 0xA3: return RGFW_KEY_CONTROL_R;
		case 0xA4: return RGFW_KEY_ALT_L; case 0xA5: return RGFW_KEY_ALT_R;
		case 0x5B: return RGFW_KEY_SUPER_L; case 0x5C: return RGFW_KEY_SUPER_R;
		case 0x14: return RGFW_KEY_CAPS_LOCK; case 0x90: return RGFW_KEY_NUM_LOCK; case 0x91: return RGFW_KEY_SCROLL_LOCK;
		case 0x26: return RGFW_KEY_UP; case 0x28: return RGFW_KEY_DOWN; case 0x25: return RGFW_KEY_LEFT; case 0x27: return RGFW_KEY_RIGHT;
		case 0x24: return RGFW_KEY_HOME; case 0x23: return RGFW_KEY_END; case 0x21: return RGFW_KEY_PAGE_UP; case 0x22: return RGFW_KEY_PAGE_DOWN;
		case 0x2D: return RGFW_KEY_INSERT; case 0x5D: return RGFW_KEY_MENU;
		case 0x6B: return RGFW_KEY_PAD_PLUS; case 0x6D: return RGFW_KEY_PAD_MINUS; case 0x6A: return RGFW_KEY_PAD_MULTIPLY;
		case 0x6F: return RGFW_KEY_PAD_SLASH; case 0x0D: return RGFW_KEY_PAD_RETURN; case 0x6E: return RGFW_KEY_PAD_PERIOD;
		case 0x60: return RGFW_KEY_PAD0; case 0x61: return RGFW_KEY_PAD1; case 0x62: return RGFW_KEY_PAD2;
		case 0x63: return RGFW_KEY_PAD3; case 0x64: return RGFW_KEY_PAD4; case 0x65: return RGFW_KEY_PAD5;
		case 0x66: return RGFW_KEY_PAD6; case 0x67: return RGFW_KEY_PAD7; case 0x68: return RGFW_KEY_PAD8; case 0x69: return RGFW_KEY_PAD9;
		case 0x2C: return RGFW_KEY_PRINT_SCREEN; case 0x13: return RGFW_KEY_PAUSE;
		default: return RGFW_KEY_NULL;
	}
}

fn bool rgfw_window_fetchSize(RgfwWindow* win, int* w, int* h)
{
	Rect area;
	getClientRect(win.src.window, &area);
	win.w = area.right;
	win.h = area.bottom;
	return rgfw_window_getSize(win, w, h);
}

fn void rgfw_pollEvents()
{
	rgfw_resetPrevState();
	Msg msg;
	while (peekMessageA(&msg, null, 0, 0, 1 /* PM_REMOVE */) != 0)
	{
		translateMessage(&msg);
		dispatchMessageA(&msg);
	}
}

fn bool rgfw_window_isHidden(RgfwWindow* win) => rgfw_bool(isWindowVisible(win.src.window) == 0 && !rgfw_window_isMinimized(win));

fn bool rgfw_window_isMinimized(RgfwWindow* win)
{
	WindowPlacement placement;
	rgfw_memzero(&placement, WindowPlacement::size);
	getWindowPlacement(win.src.window, &placement);
	return rgfw_bool(placement.showCmd == SW_SHOWMINIMIZED);
}

fn bool rgfw_window_isMaximized(RgfwWindow* win)
{
	WindowPlacement placement;
	rgfw_memzero(&placement, WindowPlacement::size);
	getWindowPlacement(win.src.window, &placement);
	return rgfw_bool(placement.showCmd == SW_SHOWMAXIMIZED || isZoomed(win.src.window) != 0);
}

fn bool rgfw_monitor_getWorkarea(RgfwMonitor* monitor, int* x, int* y, int* width, int* height)
{
	MonitorInfoExW mi;
	mi.cbSize = MonitorInfoExW::size;
	getMonitorInfoA(monitor.node.hMonitor, &mi);
	if (x != null) *x = mi.rcWork.left;
	if (y != null) *y = mi.rcWork.top;
	if (width != null) *width = mi.rcWork.right - mi.rcWork.left;
	if (height != null) *height = mi.rcWork.bottom - mi.rcWork.top;
	return RGFW_TRUE;
}

fn usz rgfw_monitor_getGammaRampPtr(RgfwMonitor* monitor, RgfwGammaRamp* ramp)
{
	ushort[768] values; // 3 x 256
	HDc dc = createDCW(&rgfw_wideDisplayName, (ushort*)&monitor.node.adapterName, null, null);
	getDeviceGammaRamp(dc, &values);
	deleteDC(dc);
	if (ramp != null)
	{
		rgfw_memcpy(ramp.red, &values[0], 512);
		rgfw_memcpy(ramp.green, &values[256], 512);
		rgfw_memcpy(ramp.blue, &values[512], 512);
	}
	return 256;
}

fn bool rgfw_monitor_setGammaRamp(RgfwMonitor* monitor, RgfwGammaRamp* ramp)
{
	ushort[768] values;
	if (ramp.count != 256)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_PLATFORM, "Win32: Gamma ramp size must be 256");
		return RGFW_FALSE;
	}
	rgfw_memcpy(&values[0], ramp.red, 512);
	rgfw_memcpy(&values[256], ramp.green, 512);
	rgfw_memcpy(&values[512], ramp.blue, 512);
	HDc dc = createDCW(&rgfw_wideDisplayName, (ushort*)&monitor.node.adapterName, null, null);
	setDeviceGammaRamp(dc, &values);
	deleteDC(dc);
	return RGFW_TRUE;
}

fn void rgfw_win32GetMode(DevModeW* dm, RgfwMonitorMode* mode)
{
	mode.w = (int)dm.pelsWidth;
	mode.h = (int)dm.pelsHeight;
	rgfw_splitBPP(dm.bitsPerPel, mode);
	switch (dm.displayFrequency)
	{
		case 119: case 59: case 29: mode.refreshRate = ((float)(dm.displayFrequency + 1) * 1000.0f) / 1001.0f;
		default: mode.refreshRate = (float)dm.displayFrequency;
	}
}

fn usz rgfw_monitor_getModesPtr(RgfwMonitor* monitor, RgfwMonitorMode** modes)
{
	usz count = 0;
	uint modeIndex = 0;
	for (;;)
	{
		DevModeW dm;
		rgfw_memzero(&dm, DevModeW::size);
		dm.size = (ushort)DevModeW::size;

		if (enumDisplaySettingsW((ushort*)&monitor.node.adapterName, modeIndex, &dm) == 0) break;
		if (changeDisplaySettingsExW((ushort*)&monitor.node.adapterName, &dm, null, CDS_TEST, null) != DISP_CHANGE_SUCCESSFUL) continue;
		modeIndex++;
		if (dm.bitsPerPel < 15) continue;

		if (modes != null)
		{
			RgfwMonitorMode mode;
			rgfw_win32GetMode(&dm, &mode);
			usz i;
			for (i = 0; i < count; i++)
			{
				if (rgfw_monitorModeCompare(&(*modes)[i], &mode, RGFW_MONITOR_ALL) == RGFW_TRUE) break;
			}
			if (i < count) continue;
			(*modes)[count] = mode;
		}
		count += 1;
	}
	return count;
}

fn int rgfw_win32GetMonitorHandleCallback(void* hMonitor, void* hdcMonitor, Rect* lprcMonitor, iptr dwData)
{
	MonitorInfoExW mi;
	rgfw_memzero(&mi, MonitorInfoExW::size);
	mi.cbSize = MonitorInfoExW::size;
	if (getMonitorInfoW(hMonitor, &mi) != 0)
	{
		RgfwMonitorNode* node = (RgfwMonitorNode*)(void*)dwData;
		usz i;
		for (i = 0; mi.szDevice[i] != 0 && node.adapterName[i] != 0 && mi.szDevice[i] == node.adapterName[i]; i++) {}
		if (mi.szDevice[i] == 0 && node.adapterName[i] == 0) node.hMonitor = hMonitor;
	}
	return 1;
}

fn void rgfw_win32CreateMonitor(DisplayDeviceW* adapter, DisplayDeviceW* dd)
{
	DevModeW dm;
	rgfw_memzero(&dm, DevModeW::size);
	dm.size = (ushort)DevModeW::size;
	if (enumDisplaySettingsW((ushort*)&adapter.deviceName, (uint)ENUM_CURRENT_SETTINGS, &dm) == 0) return;

	RgfwMonitorNode* node = rgfw_monitors_add(null);
	rgfw_memcpy(&node.adapterName, &adapter.deviceName, 64);
	if (dd != null) rgfw_memcpy(&node.deviceName, &dd.deviceName, 64);

	if (dd != null) rgfw_createUTF8FromWideStringWin32((ushort*)&dd.deviceString, (char*)&node.mon.name, 127);
	node.mon.name[127] = 0;

	Rect rect;
	rect.left = dm.positionX;
	rect.top = dm.positionY;
	rect.right = dm.positionX + (int)dm.pelsWidth;
	rect.bottom = dm.positionY + (int)dm.pelsHeight;
	enumDisplayMonitors(null, &rect, (void*)&rgfw_win32GetMonitorHandleCallback, (iptr)(void*)node);

	rgfw_win32GetMode(&dm, &node.mon.mode);

	MonitorInfoExW monitorInfo;
	rgfw_memzero(&monitorInfo, MonitorInfoExW::size);
	monitorInfo.cbSize = MonitorInfoExW::size;
	getMonitorInfoW(node.hMonitor, &monitorInfo);

	node.mon.x = monitorInfo.rcMonitor.left;
	node.mon.y = monitorInfo.rcMonitor.top;

	HDc hdc = createDCW((ushort*)&monitorInfo.szDevice, null, null, null);
	float dpiX = (float)getDeviceCaps(hdc, 88 /* LOGPIXELSX */);
	float dpiY = (float)getDeviceCaps(hdc, 90 /* LOGPIXELSY */);
	node.mon.scaleX = dpiX / 96.0f;
	node.mon.scaleY = dpiY / 96.0f;
	node.mon.pixelRatio = dpiX >= 192.0f ? 2.0f : 1.0f;
	node.mon.physW = (float)getDeviceCaps(hdc, 4 /* HORZSIZE */) / 25.4f;
	node.mon.physH = (float)getDeviceCaps(hdc, 6 /* VERTSIZE */) / 25.4f;
	deleteDC(hdc);

	if (monitorInfo.dwFlags & MONITORINFOF_PRIMARY) _rgfw.monitors.primary = node;
	rgfw_monitorCallback(_rgfw.root, &node.mon, RGFW_TRUE);
}

fn void rgfw_pollMonitors()
{
	for (RgfwMonitorNode* node = _rgfw.monitors.list.head; node != null; node = node.next) node.disconnected = RGFW_TRUE;

	DisplayDeviceW adapter;
	for (uint adapterNum = 0;; adapterNum++)
	{
		rgfw_memzero(&adapter, DisplayDeviceW::size);
		adapter.cb = DisplayDeviceW::size;
		if (enumDisplayDevicesW(null, adapterNum, &adapter, 0) == 0) break;
		if (!(adapter.stateFlags & DISPLAY_DEVICE_ACTIVE)) continue;

		DisplayDeviceW dd;
		uint deviceNum;
		for (deviceNum = 0;; deviceNum++)
		{
			rgfw_memzero(&dd, DisplayDeviceW::size);
			dd.cb = DisplayDeviceW::size;
			if (enumDisplayDevicesW((ushort*)&adapter.deviceName, deviceNum, &dd, 0) == 0) break;
			if (!(dd.stateFlags & DISPLAY_DEVICE_ACTIVE)) continue;

			RgfwMonitorNode* node;
			for (node = _rgfw.monitors.list.head; node != null; node = node.next)
			{
				usz i;
				for (i = 0; dd.deviceName[i] != 0 && node.deviceName[i] != 0 && dd.deviceName[i] == node.deviceName[i]; i++) {}
				if (node.disconnected == RGFW_TRUE && dd.deviceName[i] == 0 && node.deviceName[i] == 0)
				{
					node.disconnected = RGFW_FALSE;
					enumDisplayMonitors(null, null, (void*)&rgfw_win32GetMonitorHandleCallback, (iptr)(void*)&node.mon);
					break;
				}
			}
			if (node != null) continue;
			rgfw_win32CreateMonitor(&adapter, &dd);
		}

		if (deviceNum == 0)
		{
			RgfwMonitorNode* node;
			for (node = _rgfw.monitors.list.head; node != null; node = node.next)
			{
				usz i;
				for (i = 0; adapter.deviceName[i] != 0 && node.adapterName[i] != 0 && adapter.deviceName[i] == node.adapterName[i]; i++) {}
				if (node.disconnected == RGFW_TRUE && adapter.deviceName[i] == 0 && node.adapterName[i] == 0) { node.disconnected = RGFW_FALSE; break; }
			}
			if (node != null) continue;
			rgfw_win32CreateMonitor(&adapter, null);
		}
	}
	rgfw_monitors_refresh();
}

fn void rgfw_monitorNode_free(RgfwMonitorNode* node) {}

fn RgfwMonitor* rgfw_window_getMonitor(RgfwWindow* win)
{
	HMonitor src = monitorFromWindow(win.src.window, 1 /* MONITOR_DEFAULTTOPRIMARY */);
	for (RgfwMonitorNode* node = _rgfw.monitors.list.head; node != null; node = node.next)
	{
		if (node.hMonitor == src) return &node.mon;
	}
	return rgfw_getPrimaryMonitor();
}

fn bool rgfw_monitor_setMode(RgfwMonitor* mon, RgfwMonitorMode* mode)
{
	DevModeW dm;
	rgfw_memzero(&dm, DevModeW::size);
	dm.size = (ushort)DevModeW::size;
	dm.fields |= DM_PELSWIDTH | DM_PELSHEIGHT;
	dm.pelsWidth = (uint)mode.w;
	dm.pelsHeight = (uint)mode.h;
	dm.fields |= DM_DISPLAYFREQUENCY;
	dm.displayFrequency = (uint)mode.refreshRate;
	dm.fields |= DM_BITSPERPEL;
	dm.bitsPerPel = (uint)(mode.red + mode.green + mode.blue);

	if (changeDisplaySettingsExW((ushort*)&mon.node.adapterName, &dm, null, CDS_TEST, null) == DISP_CHANGE_SUCCESSFUL)
	{
		if (changeDisplaySettingsExW((ushort*)&mon.node.adapterName, &dm, null, CDS_UPDATEREGISTRY, null) == DISP_CHANGE_SUCCESSFUL)
		{
			rgfw_win32GetMode(&dm, &mon.mode);
			return RGFW_TRUE;
		}
		return RGFW_FALSE;
	}
	return RGFW_FALSE;
}

fn bool rgfw_monitor_requestMode(RgfwMonitor* mon, RgfwMonitorMode* mode, RgfwModeRequest request)
{
	DevModeW dm;
	rgfw_memzero(&dm, DevModeW::size);
	dm.size = (ushort)DevModeW::size;
	uint index = 0;

	for (;;)
	{
		if (enumDisplaySettingsW((ushort*)&mon.node.adapterName, index, &dm) == 0) break;
		index += 1;

		if (request & RGFW_MONITOR_SCALE) { dm.fields |= DM_PELSWIDTH | DM_PELSHEIGHT; dm.pelsWidth = (uint)mode.w; dm.pelsHeight = (uint)mode.h; }
		if (request & RGFW_MONITOR_REFRESH) { dm.fields |= DM_DISPLAYFREQUENCY; dm.displayFrequency = (uint)mode.refreshRate; }
		if (request & RGFW_MONITOR_RGB) { dm.fields |= DM_BITSPERPEL; dm.bitsPerPel = (uint)(mode.red + mode.green + mode.blue); }

		if (changeDisplaySettingsExW((ushort*)&mon.node.adapterName, &dm, null, CDS_TEST, null) == DISP_CHANGE_SUCCESSFUL)
		{
			if (changeDisplaySettingsExW((ushort*)&mon.node.adapterName, &dm, null, CDS_UPDATEREGISTRY, null) == DISP_CHANGE_SUCCESSFUL)
			{
				rgfw_win32GetMode(&dm, &mon.mode);
				return RGFW_TRUE;
			}
			return RGFW_FALSE;
		}
	}
	return RGFW_FALSE;
}

fn HIcon rgfw_loadHandleImage(char* data, int w, int h, RgfwFormat format, int icon)
{
	BitmapV5Header bi;
	rgfw_memzero(&bi, BitmapV5Header::size);
	bi.size = BitmapV5Header::size;
	bi.width = w;
	bi.height = -h;
	bi.planes = 1;
	bi.bitCount = 32;
	bi.compression = BI_RGB;

	HDc dc = getDC(null);
	char* target;
	HBitmap color = createDIBSection(dc, &bi, 0, (void**)&target, null, 0);
	rgfw_copyImageData(target, w, h, RGFW_FORMAT_BGRA8, data, format, null);
	releaseDC(null, dc);

	HBitmap mask = createBitmap(w, h, 1, 1, null);

	IconInfo ii;
	rgfw_memzero(&ii, IconInfo::size);
	ii.fIcon = icon;
	ii.xHotspot = (uint)w / 2;
	ii.yHotspot = (uint)h / 2;
	ii.hbmMask = mask;
	ii.hbmColor = color;

	HIcon handle = createIconIndirect(&ii);
	deleteObject(color);
	deleteObject(mask);
	return handle;
}

fn RgfwMouse* rgfw_createMouseStandard(RgfwMouseIcon mouse)
{
	uint mouseIcon;
	switch (mouse)
	{
		case RGFW_MOUSE_NORMAL: mouseIcon = 32512; case RGFW_MOUSE_ARROW: mouseIcon = 32512; // OCR_NORMAL
		case RGFW_MOUSE_IBEAM: mouseIcon = 32513;
		case RGFW_MOUSE_WAIT: mouseIcon = 32514;
		case RGFW_MOUSE_CROSSHAIR: mouseIcon = 32515;
		case RGFW_MOUSE_PROGRESS: mouseIcon = 32650;
		case RGFW_MOUSE_RESIZE_NWSE: mouseIcon = 32642; case RGFW_MOUSE_RESIZE_NW: mouseIcon = 32642; case RGFW_MOUSE_RESIZE_SE: mouseIcon = 32642;
		case RGFW_MOUSE_RESIZE_NESW: mouseIcon = 32643; case RGFW_MOUSE_RESIZE_NE: mouseIcon = 32643; case RGFW_MOUSE_RESIZE_SW: mouseIcon = 32643;
		case RGFW_MOUSE_RESIZE_EW: mouseIcon = 32644; case RGFW_MOUSE_RESIZE_E: mouseIcon = 32644; case RGFW_MOUSE_RESIZE_W: mouseIcon = 32644;
		case RGFW_MOUSE_RESIZE_NS: mouseIcon = 32645; case RGFW_MOUSE_RESIZE_N: mouseIcon = 32645; case RGFW_MOUSE_RESIZE_S: mouseIcon = 32645;
		case RGFW_MOUSE_RESIZE_ALL: mouseIcon = 32646;
		case RGFW_MOUSE_NOT_ALLOWED: mouseIcon = 32648;
		case RGFW_MOUSE_POINTING_HAND: mouseIcon = 32649;
		default: return null;
	}
	return (RgfwMouse*)loadCursorA(null, (char*)(uptr)mouseIcon);
}

fn RgfwMouse* rgfw_createMouse(char* data, int w, int h, RgfwFormat format) => (RgfwMouse*)rgfw_loadHandleImage(data, w, h, format, 0);

fn bool rgfw_window_setMousePlatform(RgfwWindow* win, RgfwMouse* mouse)
{
	setClassLongPtrA(win.src.window, -12 /* GCLP_HCURSOR */, (iptr)(uptr)mouse);
	setCursor((HCursor)mouse);
	return RGFW_FALSE;
}

fn void rgfw_freeMouse(RgfwMouse* mouse) { destroyCursor((HCursor)mouse); }
fn void rgfw_window_hide(RgfwWindow* win) { showWindow(win.src.window, (int)SW_HIDE); }
fn void rgfw_window_showPlatform(RgfwWindow* win) { if (!(win.internal.flags & RGFW_WINDOW_MAXIMIZE)) showWindow(win.src.window, (int)SW_RESTORE); }

fn void rgfw_window_flash(RgfwWindow* win, RgfwFlashRequest request)
{
	if (rgfw_window_isInFocus(win) && request) return;
	FlashWInfo desc;
	rgfw_memzero(&desc, FlashWInfo::size);
	desc.cbSize = FlashWInfo::size;
	desc.hwnd = win.src.window;
	switch (request)
	{
		case RGFW_FLASH_CANCEL: desc.dwFlags = FLASHW_STOP;
		case RGFW_FLASH_BRIEFLY: desc.dwFlags = FLASHW_TRAY; desc.uCount = 1;
		case RGFW_FLASH_UNTIL_FOCUSED: desc.dwFlags = FLASHW_TRAY | FLASHW_TIMERNOFG;
		default: break;
	}
	flashWindowEx(&desc);
}

fn void rgfw_deinitPlatform()
{
	timeEndPeriod(1);
	if (_rgfw.helperWindow != null) destroyWindow(_rgfw.helperWindow);
	if (_rgfw.instance != null) unregisterClassW(_rgfw.wndClass.className, _rgfw.instance);
	if (_rgfw.hiddenMouse != null) rgfw_freeMouse(_rgfw.hiddenMouse);
}

fn void rgfw_window_closePlatform(RgfwWindow* win)
{
	removePropW(win.src.window, rgfw_wideRgfwPropName());
	releaseDC(win.src.window, win.src.hdc);
	destroyWindow(win.src.window);
	if (win.src.hIconSmall != null) destroyIcon(win.src.hIconSmall);
	if (win.src.hIconBig != null) destroyIcon(win.src.hIconBig);
}

fn void rgfw_window_movePlatform(RgfwWindow* win, int x, int y)
{
	win.x = x; win.y = y;
	setWindowPos(win.src.window, (HWnd*)(uptr)0, win.x, win.y, 0, 0, 0x0001 /* SWP_NOSIZE */);
}

fn void rgfw_window_resizePlatform(RgfwWindow* win, int w, int h)
{
	win.w = w; win.h = h;
	Rect rect = {0, 0, w, h};
	uint style = rgfw_winapiWindowGetStyle(win, win.internal.flags);
	uint exStyle = rgfw_winapiWindowGetExStyle(win, win.internal.flags);
	adjustWindowRectEx(&rect, style, 0, exStyle);
	setWindowPos(win.src.window, (HWnd*)(uptr)0, 0, 0, rect.right - rect.left, rect.bottom - rect.top, 0x0010 | 0x0200 | 0x0002 | 0x0004);
}

fn void rgfw_window_setName(RgfwWindow* win, char* name)
{
	if (name == null) name = "";
	ushort[256] wideName;
	multiByteToWideChar(65001, 0, name, -1, &wideName, 256);
	setWindowTextW(win.src.window, &wideName);
}

fn void rgfw_window_setMousePassthrough(RgfwWindow* win, bool passthrough)
{
	uint key = 0;
	char alpha = 0;
	uint flags = 0;
	iptr exStyle = getWindowLongW(win.src.window, -20);
	if (exStyle & 0x00080000) getLayeredWindowAttributes(win.src.window, &key, &alpha, &flags);

	if (passthrough) { exStyle |= (0x00000020 | 0x00080000); } // WS_EX_TRANSPARENT|WS_EX_LAYERED
	else
	{
		exStyle &= ~(iptr)0x00000020;
		if ((exStyle & 0x00080000) && !(flags & 2)) exStyle &= ~(iptr)0x00080000;
	}

	setWindowLongW(win.src.window, -20, exStyle);
	if (passthrough) setLayeredWindowAttributes(win.src.window, key, alpha, flags);
}

fn bool rgfw_window_setIconEx(RgfwWindow* win, char* data, int w, int h, RgfwFormat format, RgfwIcon type)
{
	if (win.src.hIconSmall != null && (type & RGFW_ICON_WINDOW)) destroyIcon(win.src.hIconSmall);
	if (win.src.hIconBig != null && (type & RGFW_ICON_TASKBAR)) destroyIcon(win.src.hIconBig);

	if (data == null)
	{
		HIcon defaultIcon = loadIconA(null, (char*)(uptr)32512 /* IDI_APPLICATION */);
		if (type & RGFW_ICON_WINDOW) sendMessage(win.src.window, 0x0080 /* WM_SETICON */, 0, (iptr)(uptr)defaultIcon);
		if (type & RGFW_ICON_TASKBAR) sendMessage(win.src.window, 0x0080, 1, (iptr)(uptr)defaultIcon);
		return RGFW_TRUE;
	}

	if (type & RGFW_ICON_WINDOW)
	{
		win.src.hIconSmall = rgfw_loadHandleImage(data, w, h, format, 1);
		sendMessage(win.src.window, 0x0080, 0, (iptr)(uptr)win.src.hIconSmall);
	}
	if (type & RGFW_ICON_TASKBAR)
	{
		win.src.hIconBig = rgfw_loadHandleImage(data, w, h, format, 1);
		sendMessage(win.src.window, 0x0080, 1, (iptr)(uptr)win.src.hIconBig);
	}
	return RGFW_TRUE;
}

fn bool rgfw_readClipboardPtr(RgfwDataTransferType requestedType, char* buffer, usz capacity, RgfwDataTransfer* data)
{
	if (requestedType != RGFW_DATA_TEXT) return RGFW_FALSE;

	usz retry = 0;
	int isOpen = 0;
	while (isOpen == 0 && retry < 3) { isOpen = openClipboard(null); retry += 1; }
	if (isOpen == 0) return RGFW_FALSE;

	void* hData = getClipboardData(13 /* CF_UNICODETEXT */);
	if (hData == null) { closeClipboard(); return RGFW_FALSE; }

	ushort* wstr = (ushort*)globalLock(hData);
	bool ret = RGFW_TRUE;
	int length = wideCharToMultiByte(65001, 0, wstr, -1, null, 0, null, null);
	if (length <= 0) return RGFW_FALSE;

	data.length = (usz)length;
	data.type = RGFW_DATA_TEXT;

	if (buffer != null && capacity < data.length) { ret = RGFW_FALSE; }
	else if (buffer != null && data.length != 0)
	{
		if (wideCharToMultiByte(65001, 0, wstr, -1, buffer, length, null, null) <= 0) return RGFW_FALSE;
		data.data = buffer;
	}

	globalUnlock(hData);
	closeClipboard();
	return ret;
}

fn bool rgfw_writeClipboard(RgfwDataTransfer* data)
{
	void* object = globalAlloc(0x0002 /* GMEM_MOVEABLE */, data.length * 2);
	if (object == null) return RGFW_FALSE;

	ushort* buffer = (ushort*)globalLock(object);
	if (buffer == null) { globalFree(object); return RGFW_FALSE; }

	multiByteToWideChar(65001, 0, data.data, -1, buffer, (int)data.length);
	globalUnlock(object);

	usz retry = 0;
	int isOpen = 0;
	while (isOpen == 0 && retry < 3) { isOpen = openClipboard(null); retry += 1; }
	if (isOpen == 0) { globalFree(object); return RGFW_FALSE; }

	emptyClipboard();
	setClipboardData(13, object);
	closeClipboard();
	return RGFW_TRUE;
}

fn void rgfw_window_moveMouse(RgfwWindow* win, int x, int y)
{
	win.internal.lastMouseX = x - win.x;
	win.internal.lastMouseY = y - win.y;
	setCursorPos(x, y);
}

const int WGL_ACCELERATION_ARB = 0x2003;
const int WGL_FULL_ACCELERATION_ARB = 0x2027;
const int WGL_DRAW_TO_WINDOW_ARB = 0x2001;
const int WGL_PIXEL_TYPE_ARB = 0x2013;
const int WGL_TYPE_RGBA_ARB = 0x202b;
const int WGL_SUPPORT_OPENGL_ARB = 0x2010;
const int WGL_COLOR_BITS_ARB = 0x2014;
const int WGL_DOUBLE_BUFFER_ARB = 0x2011;
const int WGL_ALPHA_BITS_ARB = 0x201b;
const int WGL_DEPTH_BITS_ARB = 0x2022;
const int WGL_STENCIL_BITS_ARB = 0x2023;
const int WGL_STEREO_ARB = 0x2012;
const int WGL_AUX_BUFFERS_ARB = 0x2024;
const int WGL_RED_BITS_ARB = 0x2015;
const int WGL_GREEN_BITS_ARB = 0x2017;
const int WGL_BLUE_BITS_ARB = 0x2019;
const int WGL_ACCUM_RED_BITS_ARB = 0x201e;
const int WGL_ACCUM_GREEN_BITS_ARB = 0x201f;
const int WGL_ACCUM_BLUE_BITS_ARB = 0x2020;
const int WGL_ACCUM_ALPHA_BITS_ARB = 0x2021;
const int WGL_COLORSPACE_SRGB_EXT = 0x3089;
const int WGL_CONTEXT_OPENGL_NO_ERROR_ARB = 0x31b3;
const int WGL_CONTEXT_RELEASE_BEHAVIOR_ARB = 0x2097;
const int WGL_CONTEXT_RELEASE_BEHAVIOR_NONE_ARB = 0x0000;
const int WGL_CONTEXT_RELEASE_BEHAVIOR_FLUSH_ARB = 0x2098;
const int WGL_CONTEXT_FLAGS_ARB = 0x2094;
const int WGL_COVERAGE_SAMPLES_NV = 0x2042;
const int WGL_CONTEXT_ES_PROFILE_BIT_EXT = 0x00000004;
const int WGL_CONTEXT_PROFILE_MASK_ARB = 0x9126;
const int WGL_CONTEXT_CORE_PROFILE_BIT_ARB = 0x00000001;
const int WGL_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB = 0x00000002;
const int WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB = 0x00000002;
const int WGL_CONTEXT_MAJOR_VERSION_ARB = 0x2091;
const int WGL_CONTEXT_MINOR_VERSION_ARB = 0x2092;
const int WGL_FRAMEBUFFER_SRGB_CAPABLE_ARB = 0x20A9;
const int WGL_CONTEXT_DEBUG_BIT_ARB = 0x00000001;
const int WGL_CONTEXT_ROBUST_ACCESS_BIT_ARB = 0x00000004;

alias GetExtStringArbFn = fn char*(HDc*);
alias GetExtStringExtFn = fn char*();
alias GlxGetProcAddressProc = fn RgfwProc(char*);

fn bool rgfw_extensionSupportedPlatform_OpenGL(char* extension, usz len)
{
	char* extensions = null;
	RgfwProc proc = rgfw_getProcAddress_OpenGL("wglGetExtensionsStringARB");
	RgfwProc proc2 = rgfw_getProcAddress_OpenGL("wglGetExtensionsStringEXT");
	if (proc != null) { extensions = ((GetExtStringArbFn)proc)(rgfw_wglGetCurrentDC()); }
	else if (proc2 != null) { extensions = ((GetExtStringExtFn)proc2)(); }
	return rgfw_bool(extensions != null && rgfw_extensionSupportedStr(extensions, extension, len));
}

fn RgfwProc rgfw_getProcAddress_OpenGL(char* procname)
{
	RgfwProc proc = _rgfw.glGetProcAddress != null ? ((GlxGetProcAddressProc)_rgfw.glGetProcAddress)(procname) : null;
	if (proc != null) return proc;
	return getProcAddress((HModule*)_rgfw.nativeGL_handle, procname);
}

RgfwGlHints rgfw_globalHints_OpenGL_src = {
	0, 0, RGFW_FALSE, 0, RGFW_TRUE,
	8, 8, 8, 8,
	24,
	0, 0, 0, 0,
	RGFW_FALSE, RGFW_FALSE, RGFW_FALSE, RGFW_FALSE,
	RGFW_GL_RELEASE_NONE, RGFW_GL_CORE,
	1, 0,
	null, null,
	RGFW_GL_ACCELERATED
};
RgfwGlHints* rgfw_globalHints_OpenGL = &rgfw_globalHints_OpenGL_src;

fn void rgfw_setGlobalHints_OpenGL(RgfwGlHints* hints) { rgfw_globalHints_OpenGL = hints; }
fn RgfwGlHints* rgfw_getGlobalHints_OpenGL() => rgfw_globalHints_OpenGL;

fn RgfwGlContext* rgfw_window_createContext_OpenGL(RgfwWindow* win, RgfwGlHints* hints)
{
	RgfwGlContext* ctx = (RgfwGlContext*)rgfw_alloc_impl(RgfwGlContext::size);
	if (rgfw_window_createContextPtr_OpenGL(win, ctx, hints) == RGFW_FALSE)
	{
		rgfw_free_impl(ctx);
		win.src.ctx.native = null;
		return null;
	}
	win.src.gfxType |= RGFW_GFX_OWNED_BY_RGFW;
	return ctx;
}

fn void rgfw_window_deleteContext_OpenGL(RgfwWindow* win, RgfwGlContext* ctx)
{
	rgfw_window_deleteContextPtr_OpenGL(win, ctx);
	if (win.src.gfxType & RGFW_GFX_OWNED_BY_RGFW) rgfw_free_impl(ctx);
}

fn bool rgfw_window_createContextPtr_OpenGL(RgfwWindow* win, RgfwGlContext* ctx, RgfwGlHints* hints)
{
	win.src.ctx.native = ctx;
	win.src.gfxType = RGFW_GFX_NATIVE_OPEN_GL;

	PixelFormatDescriptor pfd;
	rgfw_memzero(&pfd, PixelFormatDescriptor::size);
	pfd.size = (ushort)PixelFormatDescriptor::size;
	pfd.version = 1;
	pfd.flags = PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER;
	pfd.pixelType = PFD_TYPE_RGBA;
	pfd.layerType = PFD_MAIN_PLANE;
	pfd.colorBits = 32;
	pfd.alphaBits = 8;
	pfd.depthBits = 24;
	pfd.stencilBits = (char)hints.stencil;
	pfd.auxBuffers = (char)hints.auxBuffers;
	if (hints.stereo) pfd.flags |= PFD_STEREO;
	if (hints.renderer == RGFW_GL_SOFTWARE) pfd.flags |= PFD_GENERIC_FORMAT | PFD_GENERIC_ACCELERATED;

	int pixelFormat = choosePixelFormat(win.src.hdc, &pfd);
	if (rgfw_wglChoosePixelFormatARB != null)
	{
		int[50] pixelFormatAttribs;
		RgfwAttribStack stack;
		rgfw_attribStack_init(&stack, (int*)&pixelFormatAttribs, 50);

		rgfw_attribStack_pushAttribs(&stack, WGL_ACCELERATION_ARB, WGL_FULL_ACCELERATION_ARB);
		rgfw_attribStack_pushAttribs(&stack, WGL_DRAW_TO_WINDOW_ARB, 1);
		rgfw_attribStack_pushAttribs(&stack, WGL_PIXEL_TYPE_ARB, WGL_TYPE_RGBA_ARB);
		rgfw_attribStack_pushAttribs(&stack, WGL_SUPPORT_OPENGL_ARB, 1);
		rgfw_attribStack_pushAttribs(&stack, WGL_COLOR_BITS_ARB, 32);
		rgfw_attribStack_pushAttribs(&stack, WGL_DOUBLE_BUFFER_ARB, 1);
		rgfw_attribStack_pushAttribs(&stack, WGL_ALPHA_BITS_ARB, hints.alpha);
		rgfw_attribStack_pushAttribs(&stack, WGL_DEPTH_BITS_ARB, hints.depth);
		rgfw_attribStack_pushAttribs(&stack, WGL_STENCIL_BITS_ARB, hints.stencil);
		rgfw_attribStack_pushAttribs(&stack, WGL_STEREO_ARB, hints.stereo);
		rgfw_attribStack_pushAttribs(&stack, WGL_AUX_BUFFERS_ARB, hints.auxBuffers);
		rgfw_attribStack_pushAttribs(&stack, WGL_RED_BITS_ARB, hints.red);
		rgfw_attribStack_pushAttribs(&stack, WGL_GREEN_BITS_ARB, hints.blue);
		rgfw_attribStack_pushAttribs(&stack, WGL_BLUE_BITS_ARB, hints.green);
		rgfw_attribStack_pushAttribs(&stack, WGL_ACCUM_RED_BITS_ARB, hints.accumRed);
		rgfw_attribStack_pushAttribs(&stack, WGL_ACCUM_GREEN_BITS_ARB, hints.accumGreen);
		rgfw_attribStack_pushAttribs(&stack, WGL_ACCUM_BLUE_BITS_ARB, hints.accumBlue);
		rgfw_attribStack_pushAttribs(&stack, WGL_ACCUM_ALPHA_BITS_ARB, hints.accumAlpha);

		if (hints.sRGB)
		{
			if (hints.profile != RGFW_GL_ES) { rgfw_attribStack_pushAttribs(&stack, WGL_FRAMEBUFFER_SRGB_CAPABLE_ARB, 1); }
			else { rgfw_attribStack_pushAttribs(&stack, WGL_COLORSPACE_SRGB_EXT, hints.sRGB); }
		}
		rgfw_attribStack_pushAttribs(&stack, WGL_COVERAGE_SAMPLES_NV, hints.samples);
		rgfw_attribStack_pushAttribs(&stack, 0, 0);

		int newPixelFormat;
		uint numFormats;
		rgfw_wglChoosePixelFormatARB(win.src.hdc, (int*)&pixelFormatAttribs, null, 1, &newPixelFormat, &numFormats);
		if (numFormats == 0) { rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_OPEN_GL_CONTEXT, "Failed to create a pixel format for WGL"); }
		else { pixelFormat = newPixelFormat; }
	}

	PixelFormatDescriptor suggested;
	if (describePixelFormat(win.src.hdc, pixelFormat, (uint)PixelFormatDescriptor::size, &suggested) == 0 ||
		setPixelFormat(win.src.hdc, pixelFormat, &pfd) == 0)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_OPEN_GL_CONTEXT, "Failed to set the WGL pixel format");
	}

	if (rgfw_wglCreateContextAttribsARB != null)
	{
		int[40] attribs;
		RgfwAttribStack stack;
		rgfw_attribStack_init(&stack, (int*)&attribs, 40);

		int mask = 0;
		switch (hints.profile)
		{
			case RGFW_GL_ES: mask |= WGL_CONTEXT_ES_PROFILE_BIT_EXT;
			case RGFW_GL_COMPATIBILITY: mask |= WGL_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB;
			case RGFW_GL_FORWARD_COMPATIBILITY: mask |= WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB;
			default: mask |= WGL_CONTEXT_CORE_PROFILE_BIT_ARB;
		}
		rgfw_attribStack_pushAttribs(&stack, WGL_CONTEXT_PROFILE_MASK_ARB, mask);

		if (hints.minor != 0 || hints.major != 0)
		{
			rgfw_attribStack_pushAttribs(&stack, WGL_CONTEXT_MAJOR_VERSION_ARB, hints.major);
			rgfw_attribStack_pushAttribs(&stack, WGL_CONTEXT_MINOR_VERSION_ARB, hints.minor);
		}

		char[32] noError = "WGL_ARB_create_context_no_error";
		if (rgfw_extensionSupportedPlatform_OpenGL((char*)&noError, 31)) rgfw_attribStack_pushAttribs(&stack, WGL_CONTEXT_OPENGL_NO_ERROR_ARB, hints.noError);

		char[30] flushControl = "WGL_ARB_context_flush_control";
		if (rgfw_extensionSupportedPlatform_OpenGL((char*)&flushControl, 30))
		{
			if (hints.releaseBehavior == RGFW_GL_RELEASE_FLUSH) { rgfw_attribStack_pushAttribs(&stack, WGL_CONTEXT_RELEASE_BEHAVIOR_ARB, WGL_CONTEXT_RELEASE_BEHAVIOR_FLUSH_ARB); }
			else if (hints.releaseBehavior == RGFW_GL_RELEASE_NONE) { rgfw_attribStack_pushAttribs(&stack, WGL_CONTEXT_RELEASE_BEHAVIOR_ARB, WGL_CONTEXT_RELEASE_BEHAVIOR_NONE_ARB); }
		}

		int flags = 0;
		if (hints.debug) flags |= WGL_CONTEXT_DEBUG_BIT_ARB;
		char[34] robustness = "WGL_ARB_create_context_robustness";
		if (hints.robustness && rgfw_extensionSupportedPlatform_OpenGL((char*)&robustness, 33)) flags |= WGL_CONTEXT_ROBUST_ACCESS_BIT_ARB;
		if (flags != 0) rgfw_attribStack_pushAttribs(&stack, WGL_CONTEXT_FLAGS_ARB, flags);

		rgfw_attribStack_pushAttribs(&stack, 0, 0);
		win.src.ctx.native.ctx = rgfw_wglCreateContextAttribsARB(win.src.hdc, null, (int*)&attribs);
	}

	if (rgfw_wglCreateContextAttribsARB == null || win.src.ctx.native.ctx == null)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_OPEN_GL_CONTEXT, "Failed to create an accelerated OpenGL Context.");
		win.src.ctx.native.ctx = rgfw_wglCreateContext(win.src.hdc);
	}

	releaseDC(win.src.window, win.src.hdc);
	win.src.hdc = getDC(win.src.window);

	if (hints.share != null) rgfw_wglShareLists(rgfw_getCurrentContext_OpenGL(), hints.share.ctx);

	rgfw_wglMakeCurrent(win.src.hdc, win.src.ctx.native.ctx);
	rgfw_debugCallback(RGFW_TYPE_INFO, RGFW_INFO_OPEN_GL, "OpenGL context initalized.");
	return RGFW_TRUE;
}

fn void rgfw_window_deleteContextPtr_OpenGL(RgfwWindow* win, RgfwGlContext* ctx)
{
	rgfw_wglDeleteContext(ctx.ctx);
	win.src.ctx.native.ctx = null;
	rgfw_debugCallback(RGFW_TYPE_INFO, RGFW_INFO_OPEN_GL, "OpenGL context freed.");
}

fn void rgfw_window_makeCurrentContext_OpenGL(RgfwWindow* win)
{
	if (win == null) { rgfw_wglMakeCurrent(null, null); }
	else { rgfw_wglMakeCurrent(win.src.hdc, win.src.ctx.native.ctx); }
}

fn void* rgfw_getCurrentContext_OpenGL() => rgfw_wglGetCurrentContext();
fn void rgfw_window_swapBuffers_OpenGL(RgfwWindow* win) { swapBuffers(win.src.hdc); }
fn void rgfw_window_swapInterval_OpenGL(RgfwWindow* win, int swapInterval)
{
	if (rgfw_wglSwapIntervalEXT == null || rgfw_wglSwapIntervalEXT(swapInterval) == 0)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_OPEN_GL_CONTEXT, "Failed to set swap interval");
	}
}

fn bool rgfw_loadEGL() => RGFW_FALSE;
fn void rgfw_unloadEGL() {}

fn bool rgfw_createSurfacePtr(char* data, int w, int h, RgfwFormat format, RgfwSurface* surface)
{
	surface.data = data;
	surface.w = w;
	surface.h = h;
	surface.format = format;

	BitmapV5Header bi;
	rgfw_memzero(&bi, BitmapV5Header::size);
	bi.size = BitmapV5Header::size;
	bi.width = w;
	bi.height = -h;
	bi.planes = 1;
	bi.bitCount = (ushort)(format >= RGFW_FORMAT_RGBA8 ? 32 : 24);
	bi.compression = BI_RGB;

	surface.native.bitmap = createDIBSection(_rgfw.root.src.hdc, &bi, 0, (void**)&surface.native.buffer, null, 0);
	surface.native.format = format >= RGFW_FORMAT_RGBA8 ? RGFW_FORMAT_BGRA8 : RGFW_FORMAT_BGR8;

	if (surface.native.bitmap == null)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_BUFFER, "Failed to create DIB section.");
		return RGFW_FALSE;
	}

	surface.native.hdcMem = createCompatibleDC(_rgfw.root.src.hdc);
	selectObject(surface.native.hdcMem, surface.native.bitmap);
	return RGFW_TRUE;
}

fn void rgfw_surface_freePtr(RgfwSurface* surface)
{
	deleteDC(surface.native.hdcMem);
	deleteObject(surface.native.bitmap);
}

fn void rgfw_window_blitSurface(RgfwWindow* win, RgfwSurface* surface)
{
	int blitW = win.w < surface.w ? win.w : surface.w;
	int blitH = win.h < surface.h ? win.h : surface.h;
	rgfw_copyImageData(surface.native.buffer, surface.w, blitH, surface.native.format, surface.data, surface.format, surface.convertFunc);
	bitBlt(win.src.hdc, 0, 0, blitW, blitH, surface.native.hdcMem, 0, 0, 0x00CC0020 /* SRCCOPY */);
}

fn void rgfw_window_captureMousePlatform(RgfwWindow* win, bool state)
{
	if (state == RGFW_FALSE) { clipCursor(null); return; }

	Rect clipRect;
	getClientRect(win.src.window, &clipRect);
	clientToScreen(win.src.window, (Point*)&clipRect.left);
	clientToScreen(win.src.window, (Point*)&clipRect.right);
	clipCursor(&clipRect);
}

fn void rgfw_window_setRawMouseModePlatform(RgfwWindow* win, bool state)
{
	RawInputDevice id = {0x01, 0x02, 0, win.src.window};
	id.flags = state == RGFW_TRUE ? 0 : RIDEV_REMOVE;
	registerRawInputDevices(&id, 1, RawInputDevice::size);
}
