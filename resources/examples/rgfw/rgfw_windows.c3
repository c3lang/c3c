module std::gfx @feat(WIN32) @link("user32", "gdi32", "shell32", "advapi32", "dwmapi", "opengl32", "winmm");
import std::collections::list, std::math;

// C3 port of the Win32/WGL RGFW backend from /Users/lerno/Downloads/AGFW.h (RGFW).
// Mechanical, line-by-line style translation of the RGFW_WINDOWS implementation.
// Standalone alternative to rgfw.c3 (macOS) / rgfw_x11.c3 / rgfw_wayland.c3 - only
// ONE rgfw*.c3 file should be included for a given target, matching how the
// original C header only ever compiles one #ifdef backend at a time.


extern fn int clock_gettime(int clock_id, Timespec* ts);

struct Timespec { long tv_sec; CLong tv_nsec; }


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
alias WndProc = fn iptr(HWnd*, uint, sz, iptr);

struct Point { int x, y; }
struct Rect { int left, top, right, bottom; }
struct Msg { HWnd* hwnd; uint message; sz wParam; iptr lParam; uint time; Point pt; }
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

struct RawInputHeader { uint dwType; uint dwSize; void* hDevice; sz wParam; }
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
extern fn HModule* loadLibraryA(ZString name) @cname("LoadLibraryA");
extern fn int freeLibrary(HModule* hmod) @cname("FreeLibrary");
extern fn void* getProcAddress(HModule* hmod, ZString name) @cname("GetProcAddress");
extern fn HModule* getModuleHandleW(ushort* name) @cname("GetModuleHandleW");
extern fn int multiByteToWideChar(uint codePage, uint flags, ZString str, int cbMultiByte, ushort* out, int cchWideChar) @cname("MultiByteToWideChar");
extern fn int wideCharToMultiByte(uint codePage, uint flags, ushort* wstr, int cchWideChar, char* out, int cbMultiByte, char* defChar, int* usedDefChar) @cname("WideCharToMultiByte");

extern fn ushort registerClassW(WndClassW* wc) @cname("RegisterClassW");
extern fn int unregisterClassW(ushort* className, void* instance) @cname("UnregisterClassW");
extern fn HWnd* createWindowExW(uint exStyle, ushort* className, ushort* windowName, uint style, int x, int y, int w, int h, HWnd* parent, void* menu, void* instance, void* param) @cname("CreateWindowExW");
extern fn int destroyWindow(HWnd* wnd) @cname("DestroyWindow");
extern fn iptr defWindowProcW(HWnd* wnd, uint msg, sz wParam, iptr lParam) @cname("DefWindowProcW");
extern fn int peekMessageA(Msg* msg, HWnd* wnd, uint msgFilterMin, uint msgFilterMax, uint removeMsg) @cname("PeekMessageA");
extern fn int translateMessage(Msg* msg) @cname("TranslateMessage");
extern fn iptr dispatchMessageA(Msg* msg) @cname("DispatchMessageA");
extern fn int postMessageW(HWnd* wnd, uint msg, sz wParam, iptr lParam) @cname("PostMessageW");
extern fn int postMessage(HWnd* wnd, uint msg, sz wParam, iptr lParam) @cname("PostMessageA");
extern fn int sendMessage(HWnd* wnd, uint msg, sz wParam, iptr lParam) @cname("SendMessageA");
extern fn uint msgWaitForMultipleObjects(uint count, void* handles, int waitAll, uint milliseconds, uint wakeMask) @cname("MsgWaitForMultipleObjects");

extern fn void* setPropW(HWnd* wnd, ushort* name, void* data) @cname("SetPropW");
extern fn void* getPropW(HWnd* wnd, ushort* name) @cname("GetPropW");
extern fn void* removePropW(HWnd* wnd, ushort* name) @cname("RemovePropW");

extern fn int getWindowRect(HWnd* wnd, Rect* rect) @cname("GetWindowRect");
extern fn int getClientRect(HWnd* wnd, Rect* rect) @cname("GetClientRect");
extern fn int adjustWindowRectEx(Rect* rect, uint style, int menu, uint exStyle) @cname("AdjustWindowRectEx");
extern fn int setWindowPos(HWnd* wnd, HWnd* insertAfter, int x, int y, int cx, int cy, uint flags) @cname("SetWindowPos");
extern fn CLong setWindowLongW(HWnd* wnd, int index, CLong newLong) @cname("SetWindowLongW");
extern fn CLong setWindowLong(HWnd* wnd, int index, CLong newLong) @cname("SetWindowLongA");
extern fn CLong getWindowLongW(HWnd* wnd, int index) @cname("GetWindowLongW");
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
extern fn void* globalAlloc(uint flags, sz bytes) @cname("GlobalAlloc");
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

struct GfxNativeImage
{
	GfxFormat format;
	char* buffer; // bitmapBits
	void* bitmap; // HBITMAP
	void* hdcMem; // HDC
}

struct RgfwWindowSrc
{
	inline IntRect bounds;
	void* window; // HWND
	void* hdc; // HDC
	void* hIconSmall; // HICON
	void* hIconBig; // HICON
	int maxSizeW, maxSizeH, minSizeW, minSizeH, aspectRatioW, aspectRatioH;
	bool actionFrame;
	ushort highSurrogate;
	RgfwGfxContext ctx;
	RgfwGfxContextType gfxType;
}


// public
fn bool rgfw_loadGL()
{
	if (_gfx_ctx.nativeGL_handle != null && _gfx_ctx.glGetProcAddress != null) return true;
	_gfx_ctx.nativeGL_handle = loadLibraryA("opengl32.dll");
	_gfx_ctx.glGetProcAddress = getProcAddress((HModule*)_gfx_ctx.nativeGL_handle, "wglGetProcAddress");

	rgfw_wglCreateContext = (RgfwWglCreateContextProc)getProcAddress((HModule*)_gfx_ctx.nativeGL_handle, "wglCreateContext");
	rgfw_wglDeleteContext = (RgfwWglDeleteContextProc)getProcAddress((HModule*)_gfx_ctx.nativeGL_handle, "wglDeleteContext");
	rgfw_wglMakeCurrent = (RgfwWglMakeCurrentProc)getProcAddress((HModule*)_gfx_ctx.nativeGL_handle, "wglMakeCurrent");
	rgfw_wglGetCurrentDC = (RgfwWglGetCurrentDcProc)getProcAddress((HModule*)_gfx_ctx.nativeGL_handle, "wglGetCurrentDC");
	rgfw_wglGetCurrentContext = (RgfwWglGetCurrentContextProc)getProcAddress((HModule*)_gfx_ctx.nativeGL_handle, "wglGetCurrentContext");
	rgfw_wglShareLists = (RgfwWglShareListsProc)getProcAddress((HModule*)_gfx_ctx.nativeGL_handle, "wglShareLists");

	HWnd* dummyWin = createWindowExW(0, _gfx_ctx.wndClass.className, null, 0, 0, 0, 0, 0, null, null, _gfx_ctx.instance, null);
	HDc dummyDc = getDC(dummyWin);
	uint pfdFlags = PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER;

	PixelFormatDescriptor pfd;
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
	return true;
}

const VkResult VK_ERROR_EXTENSION_NOT_PRESENT = -7;
const int VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR = 1000009000;
struct VkWin32SurfaceCreateInfoKHR { int sType; void* pNext; uint flags; void* hinstance; void* hwnd; }
alias RgfwVkCreateWin32SurfaceKHRProc = fn VkResult(VkInstance instance, VkWin32SurfaceCreateInfoKHR* createInfo, void* allocator, VkSurfaceKHR* surface);
ZString[2] rgfw_vulkanExtensions = {"VK_KHR_surface", "VK_KHR_win32_surface"};

// public
fn bool rgfw_loadVulkan()
{
	if (_gfx_ctx.vulkan_handle != null) return true;
	_gfx_ctx.vulkan_handle = loadLibraryA("vulkan-1.dll");
	if (_gfx_ctx.vulkan_handle == null) return false;
	void* proc = getProcAddress((HModule*)_gfx_ctx.vulkan_handle, "vkGetInstanceProcAddr");
	if (proc == null) return false;
	_gfx_ctx.vkGetInstanceProcAddress = (GfxVkGetInstanceProcAddressProc)proc;
	return true;
}

// public
fn void rgfw_unloadVulkan()
{
	if (_gfx_ctx.vulkan_handle == null) return;
	freeLibrary((HModule*)_gfx_ctx.vulkan_handle);
	_gfx_ctx.vulkan_handle = null;
	_gfx_ctx.vkGetInstanceProcAddress = null;
}



// public
fn VkResult rgfw_window_createSurface_Vulkan(GfxWindow* win, VkInstance instance, VkSurfaceKHR* surface)
{
	if (surface == null || win == null) return VK_ERROR_EXTENSION_NOT_PRESENT;
	*surface = 0;
	GfxFn raw = rgfw_getInstanceProcAddress_Vulkan(instance, "vkCreateWin32SurfaceKHR");
	if (raw == null) return VK_ERROR_EXTENSION_NOT_PRESENT;
	VkWin32SurfaceCreateInfoKHR createInfo;
	createInfo.sType = VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR;
	createInfo.pNext = null;
	createInfo.flags = 0;
	createInfo.hinstance = _gfx_ctx.instance;
	createInfo.hwnd = win.src.window;
	return ((RgfwVkCreateWin32SurfaceKHRProc)raw)(instance, &createInfo, null, surface);
}

alias RgfwVkGetPhysicalDeviceWin32PresentationSupportKHRProc = fn uint(VkPhysicalDevice, uint);

// public
fn bool rgfw_getPhysicalDevicePresentationSupport_Vulkan(VkInstance instance, VkPhysicalDevice physicalDevice, uint queueFamilyIndex)
{
	GfxFn raw = rgfw_getInstanceProcAddress_Vulkan(instance, "vkGetPhysicalDeviceWin32PresentationSupportKHR");
	if (raw == null) return false;
	uint out = ((RgfwVkGetPhysicalDeviceWin32PresentationSupportKHRProc)raw)(physicalDevice, queueFamilyIndex);
	return out != 0;
}

alias WGPUInstance = void*;
alias WGPUSurface = void*;
alias WGPUSType = int;
const WGPUSType WGPU_STYPE_SURFACE_SOURCE_WINDOWS_HWND = 0x00000005;
struct WgpuChainedStruct { WgpuChainedStruct* next; WGPUSType sType; }
struct WgpuSurfaceDescriptor { WgpuChainedStruct* nextInChain; }
struct WgpuSurfaceSourceWindowsHWND { WgpuChainedStruct chain; void* hinstance; void* hwnd; }
extern fn WGPUSurface wgpuInstanceCreateSurface(WGPUInstance instance, WgpuSurfaceDescriptor* descriptor) @cname("wgpuInstanceCreateSurface");

// public
fn WGPUSurface rgfw_window_createSurface_WebGPU(GfxWindow* window, WGPUInstance instance)
{
	WgpuSurfaceSourceWindowsHWND fromHwnd;
	fromHwnd.chain.next = null;
	fromHwnd.chain.sType = WGPU_STYPE_SURFACE_SOURCE_WINDOWS_HWND;
	fromHwnd.hwnd = window.src.window;
	fromHwnd.hinstance = _gfx_ctx.instance;
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
	DxgiRational refresh_rate;
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

// public
fn int rgfw_window_createSwapChain_DirectX(GfxWindow* win, DxgiFactory* pFactory, void* pDevice, void** swapchain)
{
	if (win == null || pFactory == null || pDevice == null || swapchain == null) return -1;

	DxgiSwapChainDesc swapChainDesc;
	swapChainDesc.bufferCount = 2;
	swapChainDesc.bufferDesc.width = (uint)win.width;
	swapChainDesc.bufferDesc.height = (uint)win.height;
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




// public
fn uint rgfw_winapiWindowGetStyle(GfxWindow* win, GfxWindowFlags flags)
{
	uint style = 0x04000000 | 0x02000000; // WS_CLIPSIBLINGS | WS_CLIPCHILDREN
	if (flags.fullscreen)
	{
		style |= 0x80000000;
		return style;
	} // WS_POPUP
	style |= 0x00080000 | 0x00020000; // WS_SYSMENU | WS_MINIMIZEBOX
	if (!flags.no_border)
	{
		style |= 0x00C00000; // WS_CAPTION
		if (!flags.no_resize) style |= 0x00010000 | 0x00040000; // WS_MAXIMIZEBOX | WS_THICKFRAME
	}
	else
	{
		style |= 0x80000000;
	}
	return style;
}

// public
fn uint rgfw_winapiWindowGetExStyle(GfxWindow* win, GfxWindowFlags flags)
{
	uint style = 0x00040000; // WS_EX_APPWINDOW
	if (flags.fullscreen || flags.floating || win.is_floating()) style |= 0x00000008; // WS_EX_TOPMOST
	return style;
}

// private
fn bool rgfw_createUTF8FromWideStringWin32(ushort* source, char* output, sz max)
{
	if (source == null) return false;
	int size = wideCharToMultiByte(65001 /* CP_UTF8 */, 0, source, -1, null, 0, null, null);
	if (size == 0) return false;
	if (size > (int)max) size = (int)max;
	if (wideCharToMultiByte(65001, 0, source, -1, output, size, null, null) == 0) return false;
	output[size] = 0;
	return true;
}

// private
fn void rgfw_win32MakeWindowTransparent(GfxWindow* win)
{
	if (!(win.internal.flags.transparent)) return;

	DwmBlurBehind bb;
	bb.dwFlags = 0x1; // DWM_BB_ENABLE
	bb.fEnable = 1;
	bb.hRgnBlur = null;
	if (dwmEnableBlurBehindWindow(win.src.window, &bb) != 0)
	{
		setWindowLong(win.src.window, -20 /* GWL_EXSTYLE */, 0x00080000); // WS_EX_LAYERED
		setLayeredWindowAttributes(win.src.window, 0, (char)128, 2 /* LWA_ALPHA */);
	}
}

// public
fn bool rgfw_win32GetDarkModeState()
{
	uint lightMode = 1;
	uint len = uint::size;
	static ushort[61] subKey = {(ushort)'S', (ushort)'o', (ushort)'f', (ushort)'t', (ushort)'w', (ushort)'a', (ushort)'r', (ushort)'e', (ushort)'\\', (ushort)'M', (ushort)'i', (ushort)'c', (ushort)'r', (ushort)'o', (ushort)'s', (ushort)'o', (ushort)'f', (ushort)'t', (ushort)'\\', (ushort)'W', (ushort)'i', (ushort)'n', (ushort)'d', (ushort)'o', (ushort)'w', (ushort)'s', (ushort)'\\', (ushort)'C', (ushort)'u', (ushort)'r', (ushort)'r', (ushort)'e', (ushort)'n', (ushort)'t', (ushort)'V', (ushort)'e', (ushort)'r', (ushort)'s', (ushort)'i', (ushort)'o', (ushort)'n', (ushort)'\\', (ushort)'T', (ushort)'h', (ushort)'e', (ushort)'m', (ushort)'e', (ushort)'s', (ushort)'\\', (ushort)'P', (ushort)'e', (ushort)'r', (ushort)'s', (ushort)'o', (ushort)'n', (ushort)'a', (ushort)'l', (ushort)'i', (ushort)'z', (ushort)'e', 0}; // "Software\Microsoft\Windows\CurrentVersion\Themes\Personalize"
	static ushort[18] valueName = {(ushort)'A', (ushort)'p', (ushort)'p', (ushort)'s', (ushort)'U', (ushort)'s', (ushort)'e', (ushort)'L', (ushort)'i', (ushort)'g', (ushort)'h', (ushort)'t', (ushort)'T', (ushort)'h', (ushort)'e', (ushort)'m', (ushort)'e', 0}; // "AppsUseLightTheme"
	regGetValueW((void*)(uptr)0x80000001 /* HKEY_CURRENT_USER */, (ushort*)&subKey, (ushort*)&valueName, 0x00000010 /* RRF_RT_REG_DWORD */, null, &lightMode, &len);
	return lightMode == 0;
}

// public
fn void rgfw_win32MakeWindowDarkMode(GfxWindow* win, bool state)
{
	int value = (state == true) ? 1 : 0;
	dwmSetWindowAttribute(win.src.window, 20 /* DWMWA_USE_IMMERSIVE_DARK_MODE */, &value, (uint)int::size);
}


// private
fn iptr rgfw_wndProcW(HWnd* hWnd, uint message, sz wParam, iptr lParam)
{
	if (message == 0x007E /* WM_DISPLAYCHANGE */) rgfw_pollMonitors();

	if (hWnd == _gfx_ctx.helperWindow) return defWindowProcW(hWnd, message, wParam, lParam);

	GfxWindow* win = (GfxWindow*)getPropW(hWnd, rgfw_wideRgfwPropName());
	if (win == null) return defWindowProcW(hWnd, message, wParam, lParam);

	Rect frame;
	uint style = rgfw_winapiWindowGetStyle(win, win.internal.flags);
	uint exStyle = rgfw_winapiWindowGetExStyle(win, win.internal.flags);
	adjustWindowRectEx(&frame, style, 0, exStyle);

	switch (message)
	{
		case 0x0010: case 0x0012: _window_close_callback(win); return 0; // WM_CLOSE, WM_QUIT
		case 0x0006: // WM_ACTIVATE
		{
			bool inFocus = (wParam & 0xffff) != 0;
			rgfw_windowFocusCallback(win, inFocus);
			return defWindowProcW(hWnd, message, wParam, lParam);
		}
		case 0x0003: // WM_MOVE
		{
			if (win.internal.captureMouse) rgfw_window_captureMousePlatform(win, true);
			_window_moved_callback(win, (int)(short)(lParam & 0xffff), (int)(short)((lParam >> 16) & 0xffff));
			return defWindowProcW(hWnd, message, wParam, lParam);
		}
		case 0x0005: // WM_SIZE
		{
			if (win.internal.captureMouse) rgfw_window_captureMousePlatform(win, true);
			_window_resized_callback(win, (int)(lParam & 0xffff), (int)((lParam >> 16) & 0xffff));
			rgfw_window_checkMode(win);
			return defWindowProcW(hWnd, message, wParam, lParam);
		}
		case 0x0021: // WM_MOUSEACTIVATE
		{
			if (((lParam >> 16) & 0xffff) == 0x0201 /* WM_LBUTTONDOWN */)
			{
				if ((lParam & 0xffff) != 1 /* HTCLIENT */) win.src.actionFrame = true;
			}
			break;
		}
		case 0x0215: // WM_CAPTURECHANGED
		{
			if (lParam == 0 && win.src.actionFrame)
			{
				rgfw_window_captureMousePlatform(win, win.internal.captureMouse);
				win.src.actionFrame = false;
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
			bool resize = (win.src.minSizeW == win.src.maxSizeW) && (win.src.minSizeH == win.src.maxSizeH);
			win.internal.flags.no_resize = resize;

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
		case 0x0113: rgfw_windowRefreshCallback(win, 0, 0, win.width, win.height); break; // WM_TIMER
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
				rgfw_windowRefreshCallback(win, 0, 0, win.width, win.height);
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
			rgfw_keyUpdateKeyMods(win, getKeyState(0x14) & 1 != 0, getKeyState(0x90) & 1 != 0, getKeyState(0x91) & 1 != 0);
			rgfw_keyCallback(win, value, win.internal.mod, false, false);
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
			bool repeat = is_key_down(value);
			rgfw_keyUpdateKeyMods(win, getKeyState(0x14) & 1 != 0, getKeyState(0x90) & 1 != 0, getKeyState(0x91) & 1 != 0);
			rgfw_keyCallback(win, value, win.internal.mod, repeat, true);
			break;
		}
		case 0x0200: // WM_MOUSEMOVE
		{
			int mx = (int)(short)(lParam & 0xffff);
			int my = (int)(short)((lParam >> 16) & 0xffff);
			if (win.internal.mouseInside == false) _mouse_notify_callback(win, mx, my, true);
			_mouse_motion_callback(win, mx, my);
			break;
		}
		case 0x00FF: // WM_INPUT
		{
			if (!(win.internal.rawMouse || _gfx_ctx.rawMouse)) return defWindowProcW(hWnd, message, wParam, lParam);

			RawInputMouse raw;
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

				vecX = (float)(pos.x - win.internal.last_mouse.x);
				vecY = (float)(pos.y - win.internal.last_mouse.y);
			}
			else
			{
				vecX = (float)raw.mouse.lLastX;
				vecY = (float)raw.mouse.lLastY;
			}

			_raw_motion_callback(win, vecX, vecY);
			break;
		}
		case 0x02A3: _mouse_notify_callback(win, ...win.internal.last_mouse, false); break; // WM_MOUSELEAVE
		case 0x0201: rgfw_mouseButtonCallback(win, LEFT, true);
		case 0x0204: rgfw_mouseButtonCallback(win, RIGHT, true);
		case 0x0207: rgfw_mouseButtonCallback(win, MIDDLE, true);
		case 0x020B: rgfw_mouseButtonCallback(win, (GfxMouseButton)(GfxMouseButton.MISC1.ordinal + (((wParam >> 16) & 0xffff) == 2 ? 1 : 0)), true);
		case 0x0202: rgfw_mouseButtonCallback(win, LEFT, false);
		case 0x0205: rgfw_mouseButtonCallback(win, RIGHT, false);
		case 0x0208: rgfw_mouseButtonCallback(win, MIDDLE, false);
		case 0x020C: rgfw_mouseButtonCallback(win, (GfxMouseButton)(GfxMouseButton.MISC1.ordinal + (((wParam >> 16) & 0xffff) == 2 ? 1 : 0)), false);
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
				ushort* buffer = (ushort*)malloc((sz)(length + 1) * 2);
				char* cbuffer = (char*)malloc((sz)length + 1);
				dragQueryFileW(drop, i, buffer, length + 1);
				rgfw_createUTF8FromWideStringWin32(buffer, cbuffer, length);
				rgfw_dataDropCallback(win, cbuffer, (sz)length + 1, RGFW_DATA_FILE);
				free(buffer);
				free(cbuffer);
			}
			dragFinish(drop);
			break;
		}
		default: break;
	}

	return defWindowProcW(hWnd, message, wParam, lParam);
}

// private
fn ushort* rgfw_wideRgfwPropName()
{
	static ushort[5] name = {(ushort)'R', (ushort)'G', (ushort)'F', (ushort)'W', 0};
	return &name;
}

// public
fn void rgfw_initKeycodesPlatform()
{
	_gfx_ctx.keycodes[0x00B] = RGFW_KEY0; _gfx_ctx.keycodes[0x002] = RGFW_KEY1; _gfx_ctx.keycodes[0x003] = RGFW_KEY2;
	_gfx_ctx.keycodes[0x004] = RGFW_KEY3; _gfx_ctx.keycodes[0x005] = RGFW_KEY4; _gfx_ctx.keycodes[0x006] = RGFW_KEY5;
	_gfx_ctx.keycodes[0x007] = RGFW_KEY6; _gfx_ctx.keycodes[0x008] = RGFW_KEY7; _gfx_ctx.keycodes[0x009] = RGFW_KEY8;
	_gfx_ctx.keycodes[0x00A] = RGFW_KEY9;
	_gfx_ctx.keycodes[0x01E] = RGFW_KEY_A; _gfx_ctx.keycodes[0x030] = RGFW_KEY_B; _gfx_ctx.keycodes[0x02E] = RGFW_KEY_C;
	_gfx_ctx.keycodes[0x020] = RGFW_KEY_D; _gfx_ctx.keycodes[0x012] = RGFW_KEY_E; _gfx_ctx.keycodes[0x021] = RGFW_KEY_F;
	_gfx_ctx.keycodes[0x022] = RGFW_KEY_G; _gfx_ctx.keycodes[0x023] = RGFW_KEY_H; _gfx_ctx.keycodes[0x017] = RGFW_KEY_I;
	_gfx_ctx.keycodes[0x024] = RGFW_KEY_J; _gfx_ctx.keycodes[0x025] = RGFW_KEY_K; _gfx_ctx.keycodes[0x026] = RGFW_KEY_L;
	_gfx_ctx.keycodes[0x032] = RGFW_KEY_M; _gfx_ctx.keycodes[0x031] = RGFW_KEY_N; _gfx_ctx.keycodes[0x018] = RGFW_KEY_O;
	_gfx_ctx.keycodes[0x019] = RGFW_KEY_P; _gfx_ctx.keycodes[0x010] = RGFW_KEY_Q; _gfx_ctx.keycodes[0x013] = RGFW_KEY_R;
	_gfx_ctx.keycodes[0x01F] = RGFW_KEY_S; _gfx_ctx.keycodes[0x014] = RGFW_KEY_T; _gfx_ctx.keycodes[0x016] = RGFW_KEY_U;
	_gfx_ctx.keycodes[0x02F] = RGFW_KEY_V; _gfx_ctx.keycodes[0x011] = RGFW_KEY_W; _gfx_ctx.keycodes[0x02D] = RGFW_KEY_X;
	_gfx_ctx.keycodes[0x015] = RGFW_KEY_Y; _gfx_ctx.keycodes[0x02C] = RGFW_KEY_Z;
	_gfx_ctx.keycodes[0x028] = RGFW_KEY_APOSTROPHE; _gfx_ctx.keycodes[0x02B] = RGFW_KEY_BACK_SLASH;
	_gfx_ctx.keycodes[0x033] = RGFW_KEY_COMMA; _gfx_ctx.keycodes[0x00D] = RGFW_KEY_EQUALS;
	_gfx_ctx.keycodes[0x029] = RGFW_KEY_BACKTICK; _gfx_ctx.keycodes[0x01A] = RGFW_KEY_BRACKET;
	_gfx_ctx.keycodes[0x00C] = RGFW_KEY_MINUS; _gfx_ctx.keycodes[0x034] = RGFW_KEY_PERIOD;
	_gfx_ctx.keycodes[0x01B] = RGFW_KEY_CLOSE_BRACKET; _gfx_ctx.keycodes[0x027] = RGFW_KEY_SEMICOLON;
	_gfx_ctx.keycodes[0x035] = RGFW_KEY_SLASH; _gfx_ctx.keycodes[0x056] = RGFW_KEY_WORLD2;
	_gfx_ctx.keycodes[0x00E] = RGFW_KEY_BACK_SPACE; _gfx_ctx.keycodes[0x153] = RGFW_KEY_DELETE;
	_gfx_ctx.keycodes[0x14F] = RGFW_KEY_END; _gfx_ctx.keycodes[0x01C] = RGFW_KEY_ENTER;
	_gfx_ctx.keycodes[0x001] = RGFW_KEY_ESCAPE; _gfx_ctx.keycodes[0x147] = RGFW_KEY_HOME;
	_gfx_ctx.keycodes[0x152] = RGFW_KEY_INSERT; _gfx_ctx.keycodes[0x15D] = RGFW_KEY_MENU;
	_gfx_ctx.keycodes[0x151] = RGFW_KEY_PAGE_DOWN; _gfx_ctx.keycodes[0x149] = RGFW_KEY_PAGE_UP;
	_gfx_ctx.keycodes[0x045] = RGFW_KEY_PAUSE; _gfx_ctx.keycodes[0x039] = RGFW_KEY_SPACE;
	_gfx_ctx.keycodes[0x00F] = RGFW_KEY_TAB; _gfx_ctx.keycodes[0x03A] = RGFW_KEY_CAPS_LOCK;
	_gfx_ctx.keycodes[0x145] = RGFW_KEY_NUM_LOCK; _gfx_ctx.keycodes[0x046] = RGFW_KEY_SCROLL_LOCK;
	_gfx_ctx.keycodes[0x03B] = RGFW_KEY_F1; _gfx_ctx.keycodes[0x03C] = RGFW_KEY_F2; _gfx_ctx.keycodes[0x03D] = RGFW_KEY_F3;
	_gfx_ctx.keycodes[0x03E] = RGFW_KEY_F4; _gfx_ctx.keycodes[0x03F] = RGFW_KEY_F5; _gfx_ctx.keycodes[0x040] = RGFW_KEY_F6;
	_gfx_ctx.keycodes[0x041] = RGFW_KEY_F7; _gfx_ctx.keycodes[0x042] = RGFW_KEY_F8; _gfx_ctx.keycodes[0x043] = RGFW_KEY_F9;
	_gfx_ctx.keycodes[0x044] = RGFW_KEY_F10; _gfx_ctx.keycodes[0x057] = RGFW_KEY_F11; _gfx_ctx.keycodes[0x058] = RGFW_KEY_F12;
	_gfx_ctx.keycodes[0x064] = RGFW_KEY_F13; _gfx_ctx.keycodes[0x065] = RGFW_KEY_F14; _gfx_ctx.keycodes[0x066] = RGFW_KEY_F15;
	_gfx_ctx.keycodes[0x067] = RGFW_KEY_F16; _gfx_ctx.keycodes[0x068] = RGFW_KEY_F17; _gfx_ctx.keycodes[0x069] = RGFW_KEY_F18;
	_gfx_ctx.keycodes[0x06A] = RGFW_KEY_F19; _gfx_ctx.keycodes[0x06B] = RGFW_KEY_F20; _gfx_ctx.keycodes[0x06C] = RGFW_KEY_F21;
	_gfx_ctx.keycodes[0x06D] = RGFW_KEY_F22; _gfx_ctx.keycodes[0x06E] = RGFW_KEY_F23; _gfx_ctx.keycodes[0x076] = RGFW_KEY_F24;
	_gfx_ctx.keycodes[0x038] = RGFW_KEY_ALT_L; _gfx_ctx.keycodes[0x01D] = RGFW_KEY_CONTROL_L; _gfx_ctx.keycodes[0x02A] = RGFW_KEY_SHIFT_L;
	_gfx_ctx.keycodes[0x15B] = RGFW_KEY_SUPER_L; _gfx_ctx.keycodes[0x137] = RGFW_KEY_PRINT_SCREEN;
	_gfx_ctx.keycodes[0x138] = RGFW_KEY_ALT_R; _gfx_ctx.keycodes[0x11D] = RGFW_KEY_CONTROL_R;
	_gfx_ctx.keycodes[0x036] = RGFW_KEY_SHIFT_R; _gfx_ctx.keycodes[0x15C] = RGFW_KEY_SUPER_R;
	_gfx_ctx.keycodes[0x150] = RGFW_KEY_DOWN; _gfx_ctx.keycodes[0x14B] = RGFW_KEY_LEFT;
	_gfx_ctx.keycodes[0x14D] = RGFW_KEY_RIGHT; _gfx_ctx.keycodes[0x148] = RGFW_KEY_UP;
	_gfx_ctx.keycodes[0x052] = RGFW_KEY_PAD0; _gfx_ctx.keycodes[0x04F] = RGFW_KEY_PAD1; _gfx_ctx.keycodes[0x050] = RGFW_KEY_PAD2;
	_gfx_ctx.keycodes[0x051] = RGFW_KEY_PAD3; _gfx_ctx.keycodes[0x04B] = RGFW_KEY_PAD4; _gfx_ctx.keycodes[0x04C] = RGFW_KEY_PAD5;
	_gfx_ctx.keycodes[0x04D] = RGFW_KEY_PAD6; _gfx_ctx.keycodes[0x047] = RGFW_KEY_PAD7; _gfx_ctx.keycodes[0x048] = RGFW_KEY_PAD8;
	_gfx_ctx.keycodes[0x049] = RGFW_KEY_PAD9; _gfx_ctx.keycodes[0x04E] = RGFW_KEY_PAD_PLUS; _gfx_ctx.keycodes[0x053] = RGFW_KEY_PAD_PERIOD;
	_gfx_ctx.keycodes[0x135] = RGFW_KEY_PAD_SLASH; _gfx_ctx.keycodes[0x11C] = RGFW_KEY_PAD_RETURN;
	_gfx_ctx.keycodes[0x059] = RGFW_KEY_PAD_EQUAL; _gfx_ctx.keycodes[0x037] = RGFW_KEY_PAD_MULTIPLY; _gfx_ctx.keycodes[0x04A] = RGFW_KEY_PAD_MINUS;
}

// public
fn int rgfw_initPlatform(String class_name, GfxInitOption flags)
{
	setProcessDPIAware();
	timeBeginPeriod(1);

	_gfx_ctx.instance = getModuleHandleW(null);
	@pool()
	{
		multiByteToWideChar(65001, 0, class_name.zstr_copy(tmem), -1, (ushort*)&_gfx_ctx.wideClassName, 255);
	};

	_gfx_ctx.wndClass = {};
	_gfx_ctx.wndClass.className = (ushort*)&_gfx_ctx.wideClassName;
	_gfx_ctx.wndClass.hInstance = _gfx_ctx.instance;
	_gfx_ctx.wndClass.hCursor = loadCursorA(null, (char*)(uptr)32512 /* IDC_ARROW */);
	_gfx_ctx.wndClass.wndProc = &rgfw_wndProcW;
	_gfx_ctx.wndClass.clsExtra = (int)sz::size;

	_gfx_ctx.wndClass.hIcon = (HIcon)loadImageA(_gfx_ctx.instance, "RGFW_ICON", 1 /* IMAGE_ICON */, 0, 0, 0x00000040 | 0x00008000 /* LR_DEFAULTSIZE|LR_SHARED */);
	if (_gfx_ctx.wndClass.hIcon == null)
	{
		_gfx_ctx.wndClass.hIcon = (HIcon)loadImageA(null, (char*)(uptr)32512 /* IDI_APPLICATION */, 1, 0, 0, 0x00000040 | 0x00008000);
	}

	registerClassW(&_gfx_ctx.wndClass);
	_gfx_ctx.helperWindow = createWindowExW(0, _gfx_ctx.wndClass.className, null, 0, 0, 0, 0, 0, null, null, _gfx_ctx.instance, null);

	char[4] blk = {0, 0, 0, 0};
	_gfx_ctx.hiddenMouse = rgfw_createMouse((char*)&blk, 1, 1, RGBA8);
	return 0;
}

// public
fn void? rgfw_createWindowPlatform(String name, GfxWindowFlags flags, GfxWindow* win) => @pool()
{
	if (!name.len) name = " ";
	win.src.hIconSmall = null; win.src.hIconBig = null;
	win.src.maxSizeW = 0; win.src.maxSizeH = 0;
	win.src.minSizeW = 0; win.src.minSizeH = 0;
	win.src.aspectRatioW = 0; win.src.aspectRatioH = 0;

	uint windowStyle = 0x04000000 | 0x02000000; // WS_CLIPSIBLINGS | WS_CLIPCHILDREN
	if (!(flags.no_border))
	{
		windowStyle |= 0x00C00000 | 0x00080000 | 0x00800000 | 0x00020000; // WS_CAPTION|WS_SYSMENU|WS_BORDER|WS_MINIMIZEBOX
		if (!(flags.no_resize)) windowStyle |= 0x00040000 | 0x00010000; // WS_SIZEBOX | WS_MAXIMIZEBOX
	}
	else { windowStyle |= 0x80000000 | 0x10000000 | 0x00080000; } // WS_POPUP|WS_VISIBLE|WS_SYSMENU


	Rect rect = {0, 0, win.width, win.height};
	uint style = rgfw_winapiWindowGetStyle(win, flags);
	uint exStyle = rgfw_winapiWindowGetExStyle(win, flags);
	adjustWindowRectEx(&rect, style, 0, exStyle);

	win.src.window = createWindowExW(0, _gfx_ctx.wndClass.className, name.to_wstring(tmem)!!, windowStyle, win.x + rect.left, win.y + rect.top, rect.right - rect.left, rect.bottom - rect.top, null, null, _gfx_ctx.instance, null);
	setPropW(win.src.window, rgfw_wideRgfwPropName(), win);
	rgfw_window_resize(win, win.width, win.height);

	if (flags.allow_dnd)
	{
		win.internal.flags.allow_dnd = true;
		rgfw_window_setDND(win, true);
	}
	win.src.hdc = getDC(win.src.window);

	rgfw_win32MakeWindowDarkMode(win, rgfw_win32GetDarkModeState());
	rgfw_win32MakeWindowTransparent(win);
}

// public
fn void rgfw_window_setBorder(GfxWindow* win, bool border)
{

	win.internal.flags.no_border = !border;

	Rect rect;
	getClientRect(win.src.window, &rect);
	CLong style = getWindowLongW(win.src.window, -16 /* GWL_STYLE */);
	style |= (CLong)rgfw_winapiWindowGetStyle(win, win.internal.flags);

	if (!border) { style &= ~(CLong)0x00CF0000; } // ~WS_OVERLAPPEDWINDOW
	else
	{
		if (win.internal.flags.no_resize) style &= ~(CLong)0x00010000; // ~WS_MAXIMIZEBOX
		style |= 0x00CF0000; // WS_OVERLAPPEDWINDOW
	}

	uint exStyle = rgfw_winapiWindowGetExStyle(win, win.internal.flags);
	clientToScreen(win.src.window, (Point*)&rect.left);
	clientToScreen(win.src.window, (Point*)&rect.right);
	adjustWindowRectEx(&rect, (uint)style, 0, exStyle);
	setWindowLongW(win.src.window, -16, style);
	setWindowPos(win.src.window, (HWnd*)(uptr)0 /* HWND_TOP */, rect.left, rect.top, rect.right - rect.left, rect.bottom - rect.top, 0x0020 | 0x0010 | 0x0004); // SWP_FRAMECHANGED|NOACTIVATE|NOZORDER
}


// public
fn bool rgfw_getGlobalMouse(int* x, int* y)
{
	Point p;
	getCursorPos(&p);
	if (x != null) *x = p.x;
	if (y != null) *y = p.y;
	return true;
}

// public
fn void _window_set_aspect_ratio(GfxWindow* win, int w, int h) @private
{
	win.src.aspectRatioW = w;
	win.src.aspectRatioH = h;
}
// public
fn void rgfw_window_setMinSize(GfxWindow* win, int w, int h) { win.src.minSizeW = w; win.src.minSizeH = h; }
// public
fn void rgfw_window_setMaxSize(GfxWindow* win, int w, int h) { win.src.maxSizeW = w; win.src.maxSizeH = h; }

// public
fn void rgfw_window_focus(GfxWindow* win) { setForegroundWindow(win.src.window); setFocus(win.src.window); }
// public
fn void rgfw_window_raise(GfxWindow* win)
{
	bringWindowToTop(win.src.window);
	setWindowPos(win.src.window, (HWnd*)(uptr)0, win.x, win.y, win.width, win.height, 0x0001 | 0x0002 | 0x0200 | 0x0020); // NOSIZE|NOMOVE|NOOWNERZORDER|FRAMECHANGED
}

// public
fn void rgfw_window_setFullscreen(GfxWindow* win, bool fullscreen)
{
	GfxMonitor* mon = find_monitor(win);

	if (fullscreen == false)
	{
		rgfw_monitor_setMode(mon, &win.internal.oldMode);
		rgfw_window_setBorder(win, true);

		Rect rect = {0, 0, ...win.internal.old_bounds.size};
		uint style = rgfw_winapiWindowGetStyle(win, win.internal.flags);
		uint exStyle = rgfw_winapiWindowGetExStyle(win, win.internal.flags);
		adjustWindowRectEx(&rect, style, 0, exStyle);
		setWindowPos(win.src.window, (HWnd*)(uptr)0, ...win.internal.old_bounds.origin, rect.right - rect.left, rect.bottom - rect.top, 0x0010 | 0x0200 | 0x0004);

		win.internal.flags.fullscreen = false;
		win.bounds = win.internal.old_bounds;
		return;
	}

	win.internal.old_bounds = win.bounds;
	win.internal.oldMode = mon.mode;
	win.internal.flags.fullscreen = true;
	rgfw_window_setBorder(win, false);

	setWindowPos(win.src.window, (HWnd*)(uptr)(-1) /* HWND_TOPMOST */, ...mon.origin, 0, 0, 0x0200 | 0x0020 | 0x0040 | 0x0001);
	win.x = mon.origin.x; win.y = mon.origin.y;

	rgfw_monitor_scaleToWindow(mon, win);
	setWindowPos(win.src.window, (HWnd*)(uptr)(-1), 0, 0, mon.mode.w, mon.mode.h, 0x0200 | 0x0020 | 0x0040 | 0x0002);
	win.width = mon.mode.w; win.height = mon.mode.h;
}

// public
fn void rgfw_window_maximizePlatform(GfxWindow* win) { rgfw_window_hide(win); showWindow(win.src.window, (int)SW_MAXIMIZE); }
// public
fn void rgfw_window_minimizePlatform(GfxWindow* win) { showWindow(win.src.window, (int)SW_MINIMIZE); }
// public
fn void rgfw_window_setFloating(GfxWindow* win, bool floating)
{
	if (floating) { setWindowPos(win.src.window, (HWnd*)(uptr)(-1), 0, 0, 0, 0, 0x0002 | 0x0001 | 0x0040); }
	else { setWindowPos(win.src.window, (HWnd*)(uptr)(-2) /* HWND_NOTOPMOST */, 0, 0, 0, 0, 0x0002 | 0x0001 | 0x0040); }
}
// public
fn void rgfw_window_setOpacity(GfxWindow* win, char opacity)
{
	setWindowLong(win.src.window, -20, 0x00080000);
	setLayeredWindowAttributes(win.src.window, 0, opacity, 2);
}
// public
fn void rgfw_window_restorePlatform(GfxWindow* win) {}
// public
fn bool GfxWindow.is_floating(&win) => getWindowLongPtr(win.src.window, -20) & 0x00000008 != 0;
// public
fn void rgfw_stopCheckEvents() { postMessageW(_gfx_ctx.helperWindow, 0, 0, 0); }
// public
fn void rgfw_waitForEvent(int waitMS) { msgWaitForMultipleObjects(0, null, 0, (uint)waitMS, 0x04FF /* QS_ALLINPUT */); }

ushort[8] rgfw_wideDisplayName = {(ushort)'D', (ushort)'I', (ushort)'S', (ushort)'P', (ushort)'L', (ushort)'A', (ushort)'Y', 0};

struct RgfwAttribStack
{
	int* attribs;
	sz count;
	sz max;
}

// public
fn RgfwKey rgfw_physicalToMappedKey(RgfwKey key)
{
	uint vsc = rgfw_rgfwToApiKey(key);
	char[256] keyboardState;
	if (getKeyboardState((char*)&keyboardState) == 0) return key;

	uint vk = mapVirtualKeyW(vsc, 1 /* MAPVK_VSC_TO_VK */);
	void* layout = getKeyboardLayout(0);

	ushort[4] charBuffer;
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

// public
fn IntVec2 rgfw_window_fetchSize(GfxWindow* win)
{
	Rect area;
	getClientRect(win.src.window, &area);
	win.width = area.right;
	win.height = area.bottom;
	return win.get_size();
}

// public
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

// public
fn bool rgfw_window_isHidden(GfxWindow* win) => isWindowVisible(win.src.window) == 0 && !rgfw_window_isMinimized(win);

// public
fn bool rgfw_window_isMinimized(GfxWindow* win)
{
	WindowPlacement placement;
	getWindowPlacement(win.src.window, &placement);
	return placement.showCmd == SW_SHOWMINIMIZED;
}

// public
fn bool rgfw_window_isMaximized(GfxWindow* win)
{
	WindowPlacement placement;
	getWindowPlacement(win.src.window, &placement);
	return placement.showCmd == SW_SHOWMAXIMIZED || isZoomed(win.src.window) != 0;
}

// public
fn bool rgfw_monitor_getWorkarea(GfxMonitor* monitor, int* x, int* y, int* width, int* height)
{
	MonitorInfoExW mi;
	mi.cbSize = MonitorInfoExW::size;
	getMonitorInfoA(monitor.node.hMonitor, &mi);
	if (x != null) *x = mi.rcWork.left;
	if (y != null) *y = mi.rcWork.top;
	if (width != null) *width = mi.rcWork.right - mi.rcWork.left;
	if (height != null) *height = mi.rcWork.bottom - mi.rcWork.top;
	return true;
}

// public
fn sz rgfw_monitor_getGammaRampPtr(GfxMonitor* monitor, RgfwGammaRamp* ramp)
{
	ushort[768] values; // 3 x 256
	HDc dc = createDCW(&rgfw_wideDisplayName, (ushort*)&monitor.node.adapterName, null, null);
	getDeviceGammaRamp(dc, &values);
	deleteDC(dc);
	if (ramp != null)
	{
		mem::copy(ramp.red, &values[0], 512);
		mem::copy(ramp.green, &values[256], 512);
		mem::copy(ramp.blue, &values[512], 512);
	}
	return 256;
}

// public
fn bool rgfw_monitor_setGammaRamp(GfxMonitor* monitor, RgfwGammaRamp* ramp)
{
	ushort[768] values;
	if (ramp.count != 256)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_PLATFORM, "Win32: Gamma ramp size must be 256");
		return false;
	}
	mem::copy(&values[0], ramp.red, 512);
	mem::copy(&values[256], ramp.green, 512);
	mem::copy(&values[512], ramp.blue, 512);
	HDc dc = createDCW(&rgfw_wideDisplayName, (ushort*)&monitor.node.adapterName, null, null);
	setDeviceGammaRamp(dc, &values);
	deleteDC(dc);
	return true;
}

// public
fn void rgfw_win32GetMode(DevModeW* dm, GfxMonitorMode* mode)
{
	mode.w = (int)dm.pelsWidth;
	mode.h = (int)dm.pelsHeight;
	rgfw_splitBPP(dm.bitsPerPel, mode);
	switch (dm.displayFrequency)
	{
		case 119: case 59: case 29: mode.refresh_rate = ((float)(dm.displayFrequency + 1) * 1000.0f) / 1001.0f;
		default: mode.refresh_rate = (float)dm.displayFrequency;
	}
}

// public
fn GfxMonitorMode[] rgfw_monitor_getModesPtr(GfxMonitor* monitor)
{
	uint modeIndex = 0;
	List{GfxMonitorMode} modes;
	for OUTER: (;;)
	{
		DevModeW dm = { .size = (ushort)DevModeW::size };

		if (enumDisplaySettingsW((WString)&monitor.node.adapterName, modeIndex, &dm) == 0) break;
		if (changeDisplaySettingsExW((WString)&monitor.node.adapterName, &dm, null, CDS_TEST, null) != DISP_CHANGE_SUCCESSFUL) continue;
		modeIndex++;
		if (dm.bitsPerPel < 15) continue;

		GfxMonitorMode mode;
		rgfw_win32GetMode(&dm, &mode);
		sz i;
		foreach (&mode2 : modes)
		{
			if (rgfw_monitorModeCompare(mode2, &mode, ALL)) continue OUTER;
		}
		modes.push(mode);
	}
	return modes.array_view();
}

// private
fn int rgfw_win32GetMonitorHandleCallback(void* hMonitor, void* hdcMonitor, Rect* lprcMonitor, iptr dwData)
{
	MonitorInfoExW mi;
	mi.cbSize = MonitorInfoExW::size;
	if (getMonitorInfoW(hMonitor, &mi) != 0)
	{
		GfxMonitorNode* node = (GfxMonitorNode*)(void*)dwData;
		sz i;
		for (i = 0; mi.szDevice[i] != 0 && node.adapterName[i] != 0 && mi.szDevice[i] == node.adapterName[i]; i++) {}
		if (mi.szDevice[i] == 0 && node.adapterName[i] == 0) node.hMonitor = hMonitor;
	}
	return 1;
}

// public
fn void rgfw_win32CreateMonitor(DisplayDeviceW* adapter, DisplayDeviceW* dd)
{
	DevModeW dm;
	dm.size = (ushort)DevModeW::size;
	if (enumDisplaySettingsW((ushort*)&adapter.deviceName, (uint)ENUM_CURRENT_SETTINGS, &dm) == 0) return;

	GfxMonitorNode* node = rgfw_monitors_add(null);
	mem::copy(&node.adapterName, &adapter.deviceName, 64);
	if (dd != null) mem::copy(&node.deviceName, &dd.deviceName, 64);

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
	monitorInfo.cbSize = MonitorInfoExW::size;
	getMonitorInfoW(node.hMonitor, &monitorInfo);

	node.mon.origin = { monitorInfo.rcMonitor.left, monitorInfo.rcMonitor.top};

	HDc hdc = createDCW((ushort*)&monitorInfo.szDevice, null, null, null);
	float dpiX = (float)getDeviceCaps(hdc, 88 /* LOGPIXELSX */);
	float dpiY = (float)getDeviceCaps(hdc, 90 /* LOGPIXELSY */);
	node.mon.scale = { dpiX / 96.0f, dpiY / 96.0f };
	node.mon.pixelRatio = dpiX >= 192.0f ? 2.0f : 1.0f;
	node.mon.phys_size = {
		(float)getDeviceCaps(hdc, 4 /* HORZSIZE */) / 25.4f,
		(float)getDeviceCaps(hdc, 6 /* VERTSIZE */) / 25.4f
	};
	deleteDC(hdc);

	if (monitorInfo.dwFlags & MONITORINFOF_PRIMARY) _gfx_ctx.monitors.primary = node;
	rgfw_monitorCallback(_gfx_ctx.root, &node.mon, true);
}

// public
fn void rgfw_pollMonitors()
{
	for (GfxMonitorNode* node = _gfx_ctx.monitors.list.head; node != null; node = node.next) node.disconnected = true;

	DisplayDeviceW adapter;
	for (uint adapterNum = 0;; adapterNum++)
	{
		adapter = {};
		adapter.cb = DisplayDeviceW::size;
		if (enumDisplayDevicesW(null, adapterNum, &adapter, 0) == 0) break;
		if (!(adapter.stateFlags & DISPLAY_DEVICE_ACTIVE)) continue;

		DisplayDeviceW dd;
		uint deviceNum;
		for (deviceNum = 0;; deviceNum++)
		{
			dd = {};
			dd.cb = DisplayDeviceW::size;
			if (enumDisplayDevicesW((ushort*)&adapter.deviceName, deviceNum, &dd, 0) == 0) break;
			if (!(dd.stateFlags & DISPLAY_DEVICE_ACTIVE)) continue;

			GfxMonitorNode* node;
			for (node = _gfx_ctx.monitors.list.head; node != null; node = node.next)
			{
				sz i;
				for (i = 0; dd.deviceName[i] != 0 && node.deviceName[i] != 0 && dd.deviceName[i] == node.deviceName[i]; i++) {}
				if (node.disconnected == true && dd.deviceName[i] == 0 && node.deviceName[i] == 0)
				{
					node.disconnected = false;
					enumDisplayMonitors(null, null, (void*)&rgfw_win32GetMonitorHandleCallback, (iptr)(void*)&node.mon);
					break;
				}
			}
			if (node != null) continue;
			rgfw_win32CreateMonitor(&adapter, &dd);
		}

		if (deviceNum == 0)
		{
			GfxMonitorNode* node;
			for (node = _gfx_ctx.monitors.list.head; node != null; node = node.next)
			{
				sz i;
				for (i = 0; adapter.deviceName[i] != 0 && node.adapterName[i] != 0 && adapter.deviceName[i] == node.adapterName[i]; i++) {}
				if (node.disconnected == true && adapter.deviceName[i] == 0 && node.adapterName[i] == 0) { node.disconnected = false; break; }
			}
			if (node != null) continue;
			rgfw_win32CreateMonitor(&adapter, null);
		}
	}
	rgfw_monitors_refresh();
}

// public
fn void rgfw_monitorNode_free(GfxMonitorNode* node) {}

// public
fn GfxMonitor* find_monitor(GfxWindow* win)
{
	HMonitor src = monitorFromWindow(win.src.window, 1 /* MONITOR_DEFAULTTOPRIMARY */);
	for (GfxMonitorNode* node = _gfx_ctx.monitors.list.head; node != null; node = node.next)
	{
		if (node.hMonitor == src) return &node.mon;
	}
	return rgfw_getPrimaryMonitor();
}

// public
fn bool rgfw_monitor_setMode(GfxMonitor* mon, GfxMonitorMode* mode)
{
	DevModeW dm;
	dm.size = (ushort)DevModeW::size;
	dm.fields |= DM_PELSWIDTH | DM_PELSHEIGHT;
	dm.pelsWidth = (uint)mode.w;
	dm.pelsHeight = (uint)mode.h;
	dm.fields |= DM_DISPLAYFREQUENCY;
	dm.displayFrequency = (uint)mode.refresh_rate;
	dm.fields |= DM_BITSPERPEL;
	dm.bitsPerPel = (uint)(mode.red + mode.green + mode.blue);

	if (changeDisplaySettingsExW((ushort*)&mon.node.adapterName, &dm, null, CDS_TEST, null) == DISP_CHANGE_SUCCESSFUL)
	{
		if (changeDisplaySettingsExW((ushort*)&mon.node.adapterName, &dm, null, CDS_UPDATEREGISTRY, null) == DISP_CHANGE_SUCCESSFUL)
		{
			rgfw_win32GetMode(&dm, &mon.mode);
			return true;
		}
		return false;
	}
	return false;
}

// public
fn bool rgfw_monitor_requestMode(GfxMonitor* mon, GfxMonitorMode* mode, GfxMonitorModeRequest request)
{
	DevModeW dm;
	dm.size = (ushort)DevModeW::size;
	uint index = 0;

	for (;;)
	{
		if (enumDisplaySettingsW((ushort*)&mon.node.adapterName, index, &dm) == 0) break;
		index += 1;

		if (request & SCALE) { dm.fields |= DM_PELSWIDTH | DM_PELSHEIGHT; dm.pelsWidth = (uint)mode.w; dm.pelsHeight = (uint)mode.h; }
		if (request & REFRESH) { dm.fields |= DM_DISPLAYFREQUENCY; dm.displayFrequency = (uint)mode.refresh_rate; }
		if (request & RGB) { dm.fields |= DM_BITSPERPEL; dm.bitsPerPel = (uint)(mode.red + mode.green + mode.blue); }

		if (changeDisplaySettingsExW((ushort*)&mon.node.adapterName, &dm, null, CDS_TEST, null) == DISP_CHANGE_SUCCESSFUL)
		{
			if (changeDisplaySettingsExW((ushort*)&mon.node.adapterName, &dm, null, CDS_UPDATEREGISTRY, null) == DISP_CHANGE_SUCCESSFUL)
			{
				rgfw_win32GetMode(&dm, &mon.mode);
				return true;
			}
			return false;
		}
	}
	return false;
}

// private
fn HIcon rgfw_loadHandleImage(char* data, int w, int h, GfxFormat format, int icon)
{
	BitmapV5Header bi;
	bi.size = BitmapV5Header::size;
	bi.width = w;
	bi.height = -h;
	bi.planes = 1;
	bi.bitCount = 32;
	bi.compression = BI_RGB;

	HDc dc = getDC(null);
	char* target;
	HBitmap color = createDIBSection(dc, &bi, 0, (void**)&target, null, 0);
	copy_image_data(target, w, h, BGRA8, data, format, null);
	releaseDC(null, dc);

	HBitmap mask = createBitmap(w, h, 1, 1, null);

	IconInfo ii;
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

// public
fn RgfwMouse* rgfw_createMouseStandard(GfxMouseIcon mouse)
{
	uint mouseIcon;
	switch (mouse)
	{
		case NORMAL:
		case ARROW: mouseIcon = 32512; // OCR_NORMAL
		case IBEAM: mouseIcon = 32513;
		case WAIT: mouseIcon = 32514;
		case CROSSHAIR: mouseIcon = 32515;
		case PROGRESS: mouseIcon = 32650;
		case RESIZE_NW:
		case RESIZE_SE:
		case RESIZE_NWSE: mouseIcon = 32642;
		case RESIZE_NE:
		case RESIZE_SW:
		case RESIZE_NESW: mouseIcon = 32643;
		case RESIZE_E:
		case RESIZE_W:
		case RESIZE_EW: mouseIcon = 32644;
		case RESIZE_N:
		case RESIZE_S:
		case RESIZE_NS: mouseIcon = 32645;
		case RESIZE_ALL: mouseIcon = 32646;
		case NOT_ALLOWED: mouseIcon = 32648;
		case POINTING_HAND: mouseIcon = 32649;
		default: return null;
	}
	return (RgfwMouse*)loadCursorA(null, (char*)(uptr)mouseIcon);
}

// public
fn RgfwMouse* rgfw_createMouse(char* data, int w, int h, GfxFormat format) => (RgfwMouse*)rgfw_loadHandleImage(data, w, h, format, 0);

// public
fn bool rgfw_window_setMousePlatform(GfxWindow* win, RgfwMouse* mouse)
{
	setClassLongPtrA(win.src.window, -12 /* GCLP_HCURSOR */, (iptr)(uptr)mouse);
	setCursor((HCursor)mouse);
	return false;
}

// public
fn void rgfw_freeMouse(RgfwMouse* mouse) { destroyCursor((HCursor)mouse); }
// public
fn void rgfw_window_hide(GfxWindow* win) { showWindow(win.src.window, (int)SW_HIDE); }

// public
fn void rgfw_window_flash(GfxWindow* win, GfxFlashRequest request)
{
	if (win.has_focus() && request != CANCEL) return;
	FlashWInfo desc;
	desc.cbSize = FlashWInfo::size;
	desc.hwnd = win.src.window;
	switch (request)
	{
		case CANCEL: desc.dwFlags = FLASHW_STOP;
		case BRIEFLY: desc.dwFlags = FLASHW_TRAY; desc.uCount = 1;
		case UNTIL_FOCUSED: desc.dwFlags = FLASHW_TRAY | FLASHW_TIMERNOFG;
		default: break;
	}
	flashWindowEx(&desc);
}

// public
fn void rgfw_deinitPlatform()
{
	timeEndPeriod(1);
	if (_gfx_ctx.helperWindow != null) destroyWindow(_gfx_ctx.helperWindow);
	if (_gfx_ctx.instance != null) unregisterClassW(_gfx_ctx.wndClass.className, _gfx_ctx.instance);
	if (_gfx_ctx.hiddenMouse != null) rgfw_freeMouse(_gfx_ctx.hiddenMouse);
}

// public
fn void rgfw_window_closePlatform(GfxWindow* win)
{
	removePropW(win.src.window, rgfw_wideRgfwPropName());
	releaseDC(win.src.window, win.src.hdc);
	destroyWindow(win.src.window);
	if (win.src.hIconSmall != null) destroyIcon(win.src.hIconSmall);
	if (win.src.hIconBig != null) destroyIcon(win.src.hIconBig);
}

// public
fn void rgfw_window_movePlatform(GfxWindow* win, int x, int y)
{
	win.x = x; win.y = y;
	setWindowPos(win.src.window, (HWnd*)(uptr)0, win.x, win.y, 0, 0, 0x0001 /* SWP_NOSIZE */);
}

// public
fn void rgfw_window_resizePlatform(GfxWindow* win, int w, int h)
{
	win.width = w; win.height = h;
	Rect rect = {0, 0, w, h};
	uint style = rgfw_winapiWindowGetStyle(win, win.internal.flags);
	uint exStyle = rgfw_winapiWindowGetExStyle(win, win.internal.flags);
	adjustWindowRectEx(&rect, style, 0, exStyle);
	setWindowPos(win.src.window, (HWnd*)(uptr)0, 0, 0, rect.right - rect.left, rect.bottom - rect.top, 0x0010 | 0x0200 | 0x0002 | 0x0004);
}

// public
fn void GfxWindow.set_name(&self, String name)
{
	setWindowTextW(self.src.window, name.to_wstring(tmem)!!);
}

// public
fn void rgfw_window_setMousePassthrough(GfxWindow* win, bool passthrough)
{
	uint key = 0;
	char alpha = 0;
	uint flags = 0;
	CLong exStyle = getWindowLongW(win.src.window, -20);
	if (exStyle & 0x00080000) getLayeredWindowAttributes(win.src.window, &key, &alpha, &flags);

	if (passthrough) { exStyle |= (0x00000020 | 0x00080000); } // WS_EX_TRANSPARENT|WS_EX_LAYERED
	else
	{
		exStyle &= ~(CLong)0x00000020;
		if ((exStyle & 0x00080000) && !(flags & 2)) exStyle &= ~(CLong)0x00080000;
	}

	setWindowLongW(win.src.window, -20, exStyle);
	if (passthrough) setLayeredWindowAttributes(win.src.window, key, alpha, flags);
}

// public
fn bool GfxWindow.set_icon(&win, char* data, int w, int h, GfxFormat format, GfxWindowIconType type = BOTH)
{
	if (win.src.hIconSmall != null && (type & WINDOW)) destroyIcon(win.src.hIconSmall);
	if (win.src.hIconBig != null && (type & TASKBAR)) destroyIcon(win.src.hIconBig);

	if (!data)
	{
		HIcon defaultIcon = loadIconA(null, (char*)(uptr)32512 /* IDI_APPLICATION */);
		if (type & WINDOW) sendMessage(win.src.window, 0x0080 /* WM_SETICON */, 0, (iptr)(uptr)defaultIcon);
		if (type & TASKBAR) sendMessage(win.src.window, 0x0080, 1, (iptr)(uptr)defaultIcon);
		return true;
	}

	if (type & WINDOW)
	{
		win.src.hIconSmall = rgfw_loadHandleImage(data, w, h, format, 1);
		sendMessage(win.src.window, 0x0080, 0, (iptr)(uptr)win.src.hIconSmall);
	}
	if (type & TASKBAR)
	{
		win.src.hIconBig = rgfw_loadHandleImage(data, w, h, format, 1);
		sendMessage(win.src.window, 0x0080, 1, (iptr)(uptr)win.src.hIconBig);
	}
	return true;
}

// public
fn bool rgfw_readClipboardPtr(RgfwDataTransferType requestedType, char* buffer, sz capacity, GfxDataTransfer* data)
{
	if (requestedType != RGFW_DATA_TEXT) return false;

	sz retry = 0;
	int isOpen = 0;
	while (isOpen == 0 && retry < 3) { isOpen = openClipboard(null); retry += 1; }
	if (isOpen == 0) return false;

	void* hData = getClipboardData(13 /* CF_UNICODETEXT */);
	if (hData == null) { closeClipboard(); return false; }

	ushort* wstr = (ushort*)globalLock(hData);
	bool ret = true;
	int length = wideCharToMultiByte(65001, 0, wstr, -1, null, 0, null, null);
	if (length <= 0) return false;

	data.length = (sz)length;
	data.type = RGFW_DATA_TEXT;

	if (buffer != null && capacity < data.length) { ret = false; }
	else if (buffer != null && data.length != 0)
	{
		if (wideCharToMultiByte(65001, 0, wstr, -1, buffer, length, null, null) <= 0) return false;
		data.data = buffer;
	}

	globalUnlock(hData);
	closeClipboard();
	return ret;
}

// public
fn bool rgfw_writeClipboard(GfxDataTransfer* data)
{
	void* object = globalAlloc(0x0002 /* GMEM_MOVEABLE */, data.length * 2);
	if (object == null) return false;

	ushort* buffer = (ushort*)globalLock(object);
	if (buffer == null) { globalFree(object); return false; }

	multiByteToWideChar(65001, 0, (ZString)data.data, -1, buffer, (int)data.length);
	globalUnlock(object);

	sz retry = 0;
	int isOpen = 0;
	while (isOpen == 0 && retry < 3) { isOpen = openClipboard(null); retry += 1; }
	if (isOpen == 0) { globalFree(object); return false; }

	emptyClipboard();
	setClipboardData(13, object);
	closeClipboard();
	return true;
}

// public
fn void rgfw_window_moveMouse(GfxWindow* win, int x, int y)
{
	win.internal.last_mouse = { x - win.x, y - win.y };
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

alias GetExtStringArbFn = fn ZString(HDc*);
alias GetExtStringExtFn = fn ZString();
alias GlxGetProcAddressProc = fn GfxFn(ZString);

// public
fn bool rgfw_extensionSupportedPlatform_OpenGL(ZString extension, sz len)
{
	ZString extensions = null;
	GfxFn proc = rgfw_getProcAddress_OpenGL("wglGetExtensionsStringARB");
	GfxFn proc2 = rgfw_getProcAddress_OpenGL("wglGetExtensionsStringEXT");
	if (proc != null) { extensions = ((GetExtStringArbFn)proc)(rgfw_wglGetCurrentDC()); }
	else if (proc2 != null) { extensions = ((GetExtStringExtFn)proc2)(); }
	return extensions && rgfw_extensionSupportedStr(extensions, extension, len);
}

// public
fn GfxFn rgfw_getProcAddress_OpenGL(ZString procname)
{
	GfxFn proc = _gfx_ctx.glGetProcAddress != null ? ((GlxGetProcAddressProc)_gfx_ctx.glGetProcAddress)(procname) : null;
	if (proc != null) return proc;
	return getProcAddress((HModule*)_gfx_ctx.nativeGL_handle, procname);
}




// public
fn bool rgfw_window_createContextPtr_OpenGL(GfxWindow* win, RgfwGlContext* ctx, RgfwGlHints* hints)
{
	win.src.ctx.native = ctx;
	win.src.gfxType = RGFW_GFX_NATIVE_OPEN_GL;

	PixelFormatDescriptor pfd;
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
			else { rgfw_attribStack_pushAttribs(&stack, WGL_COLORSPACE_SRGB_EXT, (int)hints.sRGB); }
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
		if (rgfw_extensionSupportedPlatform_OpenGL((ZString)&noError, 31)) rgfw_attribStack_pushAttribs(&stack, WGL_CONTEXT_OPENGL_NO_ERROR_ARB, (int)hints.noError);

		char[30] flushControl = "WGL_ARB_context_flush_control";
		if (rgfw_extensionSupportedPlatform_OpenGL((ZString)&flushControl, 30))
		{
			if (hints.releaseBehavior == RGFW_GL_RELEASE_FLUSH) { rgfw_attribStack_pushAttribs(&stack, WGL_CONTEXT_RELEASE_BEHAVIOR_ARB, WGL_CONTEXT_RELEASE_BEHAVIOR_FLUSH_ARB); }
			else if (hints.releaseBehavior == RGFW_GL_RELEASE_NONE) { rgfw_attribStack_pushAttribs(&stack, WGL_CONTEXT_RELEASE_BEHAVIOR_ARB, WGL_CONTEXT_RELEASE_BEHAVIOR_NONE_ARB); }
		}

		int flags = 0;
		if (hints.debug) flags |= WGL_CONTEXT_DEBUG_BIT_ARB;
		char[34] robustness = "WGL_ARB_create_context_robustness";
		if (hints.robustness && rgfw_extensionSupportedPlatform_OpenGL((ZString)&robustness, 33)) flags |= WGL_CONTEXT_ROBUST_ACCESS_BIT_ARB;
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
	return true;
}

// public
fn void rgfw_window_deleteContextPtr_OpenGL(GfxWindow* win, RgfwGlContext* ctx)
{
	rgfw_wglDeleteContext(ctx.ctx);
	win.src.ctx.native.ctx = null;
	rgfw_debugCallback(RGFW_TYPE_INFO, RGFW_INFO_OPEN_GL, "OpenGL context freed.");
}

// public
fn void rgfw_window_makeCurrentContext_OpenGL(GfxWindow* win)
{
	if (win == null) { rgfw_wglMakeCurrent(null, null); }
	else { rgfw_wglMakeCurrent(win.src.hdc, win.src.ctx.native.ctx); }
}

// public
fn void* rgfw_getCurrentContext_OpenGL() => rgfw_wglGetCurrentContext();
// public
fn void rgfw_window_swapBuffers_OpenGL(GfxWindow* win) { swapBuffers(win.src.hdc); }
// public
fn void rgfw_window_swapInterval_OpenGL(GfxWindow* win, int swapInterval)
{
	if (rgfw_wglSwapIntervalEXT == null || rgfw_wglSwapIntervalEXT(swapInterval) == 0)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_OPEN_GL_CONTEXT, "Failed to set swap interval");
	}
}






// public
fn void? GfxSurface.init(&self, void* data, int w, int h, GfxFormat format, GfxWindow* window = null)
{
	self.data = data;
	self.size = { w, h };
	self.format = format;

	BitmapV5Header bi;
	bi.size = BitmapV5Header::size;
	bi.width = w;
	bi.height = -h;
	bi.planes = 1;
	bi.bitCount = (ushort)format.channels * 8;
	bi.compression = BI_RGB;
	self.native.format = format.channels > 3 ? BGRA8 : BGR8;
	self.native.bitmap = createDIBSection(_gfx_ctx.root.src.hdc, &bi, 0, (void**)&self.native.buffer, null, 0);

	if (self.native.bitmap == null)
	{
		log::error("Failed to create DIB section.", category: log::CATEGORY_VIDEO);
		return SURFACE_CREATION_FAILED~;
	}
	defer catch self.release();
	self.native.hdcMem = createCompatibleDC(_gfx_ctx.root.src.hdc);
	selectObject(self.native.hdcMem, self.native.bitmap);
}

// public
fn void GfxSurface.release(&self)
{
	if (!self.data) return;
	deleteDC(self.native.hdcMem);
	deleteObject(self.native.bitmap);
	*self = {};
}

// public
fn void rgfw_window_blitSurface(GfxWindow* win, GfxSurface* surface)
{
	IntVec2 blit = math::min(win.size, surface.size);
	copy_image_data(surface.native.buffer, surface.size.x, blit.y, surface.native.format, surface.data, surface.format, surface.convertFunc);
	bitBlt(win.src.hdc, 0, 0, ...blit, surface.native.hdcMem, 0, 0, 0x00CC0020 /* SRCCOPY */);
}

// public
fn void rgfw_window_captureMousePlatform(GfxWindow* win, bool state)
{
	if (state == false) { clipCursor(null); return; }

	Rect clipRect;
	getClientRect(win.src.window, &clipRect);
	clientToScreen(win.src.window, (Point*)&clipRect.left);
	clientToScreen(win.src.window, (Point*)&clipRect.right);
	clipCursor(&clipRect);
}

// public
fn void rgfw_window_setRawMouseModePlatform(GfxWindow* win, bool state)
{
	RawInputDevice id = {0x01, 0x02, 0, win.src.window};
	id.flags = state == true ? 0 : RIDEV_REMOVE;
	registerRawInputDevices(&id, 1, RawInputDevice::size);
}
