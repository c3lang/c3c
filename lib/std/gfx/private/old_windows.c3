module std::gfx @feat(WIN32) @link("user32", "gdi32", "shell32", "advapi32", "dwmapi", "opengl32", "winmm");
import std::collections::list, std::math, std::os::win32;


extern fn int clock_gettime(int clock_id, Timespec* ts);

struct Timespec { long tv_sec; CLong tv_nsec; }


// ---- Win32 base types ----
alias HGlrc = void*;
alias HRgn = void*;
alias HGdiObj = void*;
alias HRawInput = void*;
alias WndProc = fn iptr(Win32_HWND*, uint, sz, iptr);

struct Win32_POINT { int x, y; }
struct Rect { int left, top, right, bottom; }
struct Msg { Win32_HWND* hwnd; uint message; sz wParam; iptr lParam; uint time; Win32_POINT pt; }
struct WndClassW { uint style; WndProc wndProc; int clsExtra; int wndExtra; void* hInstance; Win32_HICON hIcon; Win32_HCURSOR hCursor; Win32_HBRUSH hbrBackground; ushort* menuName; ushort* className; }

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

struct IconInfo { int fIcon; uint xHotspot, yHotspot; Win32_HBITMAP hbmMask; Win32_HBITMAP hbmColor; }

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

struct WindowPlacement { uint length, flags, showCmd; Win32_POINT ptMinPosition, ptMaxPosition; Rect rcNormalPosition; }
const uint SW_SHOWMINIMIZED = 2;
const uint SW_SHOWMAXIMIZED = 3;
const uint SW_MAXIMIZE = 3;
const uint SW_MINIMIZE = 6;
const uint SW_RESTORE = 9;
const uint SW_HIDE = 0;

struct RawInputDevice { ushort usagePage, usage; uint flags; Win32_HWND* target; }
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

struct FlashWInfo { uint cbSize; Win32_HWND* hwnd; uint dwFlags; uint uCount; uint dwTimeout; }
const uint FLASHW_STOP = 0;
const uint FLASHW_TRAY = 2;
const uint FLASHW_TIMERNOFG = 12;

struct MinMaxInfo { Win32_POINT ptReserved, ptMaxSize, ptMaxPosition, ptMinTrackSize, ptMaxTrackSize; }

// ---- extern Win32 functions (kernel32/user32/gdi32/shell32/dwmapi) ----
extern fn Win32_HMODULE* getModuleHandleW(ushort* name) @cname("GetModuleHandleW");
extern fn int multiByteToWideChar(uint codePage, uint flags, ZString str, int cbMultiByte, ushort* out, int cchWideChar) @cname("MultiByteToWideChar");
extern fn int wideCharToMultiByte(uint codePage, uint flags, ushort* wstr, int cchWideChar, char* out, int cbMultiByte, char* defChar, int* usedDefChar) @cname("WideCharToMultiByte");

extern fn ushort registerClassW(WndClassW* wc) @cname("RegisterClassW");
extern fn int unregisterClassW(ushort* className, void* instance) @cname("UnregisterClassW");
extern fn int destroyWindow(Win32_HWND* wnd) @cname("DestroyWindow");
extern fn iptr defWindowProcW(Win32_HWND* wnd, uint msg, sz wParam, iptr lParam) @cname("DefWindowProcW");
extern fn int peekMessageA(Msg* msg, Win32_HWND* wnd, uint msgFilterMin, uint msgFilterMax, uint removeMsg) @cname("PeekMessageA");
extern fn int translateMessage(Msg* msg) @cname("TranslateMessage");
extern fn iptr dispatchMessageA(Msg* msg) @cname("DispatchMessageA");
extern fn int postMessageW(Win32_HWND* wnd, uint msg, sz wParam, iptr lParam) @cname("PostMessageW");
extern fn int postMessage(Win32_HWND* wnd, uint msg, sz wParam, iptr lParam) @cname("PostMessageA");
extern fn int sendMessage(Win32_HWND* wnd, uint msg, sz wParam, iptr lParam) @cname("SendMessageA");
extern fn uint msgWaitForMultipleObjects(uint count, void* handles, int waitAll, uint milliseconds, uint wakeMask) @cname("MsgWaitForMultipleObjects");

extern fn void* setPropW(Win32_HWND* wnd, ushort* name, void* data) @cname("SetPropW");
extern fn void* getPropW(Win32_HWND* wnd, ushort* name) @cname("GetPropW");
extern fn void* removePropW(Win32_HWND* wnd, ushort* name) @cname("RemovePropW");

extern fn int getWindowRect(Win32_HWND* wnd, Rect* rect) @cname("GetWindowRect");
extern fn int getClientRect(Win32_HWND* wnd, Rect* rect) @cname("GetClientRect");
extern fn int adjustWindowRectEx(Rect* rect, uint style, int menu, uint exStyle) @cname("AdjustWindowRectEx");
extern fn int setWindowPos(Win32_HWND* wnd, Win32_HWND* insertAfter, int x, int y, int cx, int cy, uint flags) @cname("SetWindowPos");
extern fn CLong setWindowLongW(Win32_HWND* wnd, int index, CLong newLong) @cname("SetWindowLongW");
extern fn CLong setWindowLong(Win32_HWND* wnd, int index, CLong newLong) @cname("SetWindowLongA");
extern fn CLong getWindowLongW(Win32_HWND* wnd, int index) @cname("GetWindowLongW");
extern fn iptr getWindowLongPtr(Win32_HWND* wnd, int index) @cname("GetWindowLongPtrA");
extern fn int showWindow(Win32_HWND* wnd, int cmdShow) @cname("ShowWindow");
extern fn int isWindowVisible(Win32_HWND* wnd) @cname("IsWindowVisible");
extern fn int isZoomed(Win32_HWND* wnd) @cname("IsZoomed");
extern fn int getWindowPlacement(Win32_HWND* wnd, WindowPlacement* placement) @cname("GetWindowPlacement");
extern fn int bringWindowToTop(Win32_HWND* wnd) @cname("BringWindowToTop");
extern fn Win32_HWND* setForegroundWindow(Win32_HWND* wnd) @cname("SetForegroundWindow");
extern fn Win32_HWND* setFocus(Win32_HWND* wnd) @cname("SetFocus");
extern fn int setWindowTextW(Win32_HWND* wnd, ushort* text) @cname("SetWindowTextW");
extern fn int setLayeredWindowAttributes(Win32_HWND* wnd, uint crKey, char alpha, uint flags) @cname("SetLayeredWindowAttributes");
extern fn int getLayeredWindowAttributes(Win32_HWND* wnd, uint* crKey, char* alpha, uint* flags) @cname("GetLayeredWindowAttributes");
extern fn Win32_HWND* monitorFromWindow(Win32_HWND* wnd, uint flags) @cname("MonitorFromWindow");
extern fn int flashWindowEx(FlashWInfo* info) @cname("FlashWindowEx");
extern fn int dragAcceptFiles(Win32_HWND* wnd, int accept) @cname("DragAcceptFiles");

extern fn Win32_HDC* getDC(Win32_HWND* wnd) @cname("GetDC");
extern fn int releaseDC(Win32_HWND* wnd, Win32_HDC* dc) @cname("ReleaseDC");
extern fn int choosePixelFormat(Win32_HDC* dc, PixelFormatDescriptor* pfd) @cname("ChoosePixelFormat");
extern fn int setPixelFormat(Win32_HDC* dc, int format, PixelFormatDescriptor* pfd) @cname("SetPixelFormat");
extern fn int describePixelFormat(Win32_HDC* dc, int format, uint bytes, PixelFormatDescriptor* pfd) @cname("DescribePixelFormat");
extern fn int swapBuffers(Win32_HDC* dc) @cname("SwapBuffers");
extern fn Win32_HBITMAP createDIBSection(Win32_HDC* dc, void* bmi, uint usage, void** bits, void* section, uint offset) @cname("CreateDIBSection");
extern fn Win32_HDC* createCompatibleDC(Win32_HDC* dc) @cname("CreateCompatibleDC");
extern fn Win32_HDC* createDCW(ushort* driver, ushort* device, ushort* output, void* initData) @cname("CreateDCW");
extern fn int deleteDC(Win32_HDC* dc) @cname("DeleteDC");
extern fn void* selectObject(Win32_HDC* dc, void* obj) @cname("SelectObject");
extern fn int deleteObject(void* obj) @cname("DeleteObject");
extern fn int bitBlt(Win32_HDC* dst, int x, int y, int w, int h, Win32_HDC* src, int x1, int y1, uint rop) @cname("BitBlt");
extern fn int getDeviceCaps(Win32_HDC* dc, int index) @cname("GetDeviceCaps");
extern fn int getDeviceGammaRamp(Win32_HDC* dc, void* ramp) @cname("GetDeviceGammaRamp");
extern fn int setDeviceGammaRamp(Win32_HDC* dc, void* ramp) @cname("SetDeviceGammaRamp");
extern fn Win32_HBITMAP createBitmap(int w, int h, uint planes, uint bitCount, void* bits) @cname("CreateBitmap");
extern fn Win32_HICON createIconIndirect(IconInfo* info) @cname("CreateIconIndirect");
extern fn int destroyIcon(Win32_HICON icon) @cname("DestroyIcon");
extern fn Win32_HCURSOR loadCursorA(void* instance, char* name) @cname("LoadCursorA");
extern fn Win32_HICON loadIconA(void* instance, char* name) @cname("LoadIconA");
extern fn void* loadImageA(void* instance, char* name, uint c_type, int cx, int cy, uint fuLoad) @cname("LoadImageA");
extern fn int destroyCursor(Win32_HCURSOR cursor) @cname("DestroyCursor");
extern fn int setCursor(Win32_HCURSOR cursor) @cname("SetCursor");
extern fn iptr setClassLongPtrA(Win32_HWND* wnd, int index, iptr newLong) @cname("SetClassLongPtrA");
extern fn int getCursorPos(Win32_POINT* p) @cname("GetCursorPos");
extern fn int setCursorPos(int x, int y) @cname("SetCursorPos");
extern fn int screenToClient(Win32_HWND* wnd, Win32_POINT* p) @cname("ScreenToClient");
extern fn int clientToScreen(Win32_HWND* wnd, Win32_POINT* p) @cname("ClientToScreen");
extern fn int clipCursor(Rect* rect) @cname("ClipCursor");
extern fn int registerRawInputDevices(RawInputDevice* devices, uint numDevices, uint size) @cname("RegisterRawInputDevices");
extern fn uint getRawInputData(HRawInput hRawInput, uint command, void* data, uint* size, uint headerSize) @cname("GetRawInputData");
extern fn int getSystemMetrics(int index) @cname("GetSystemMetrics");

extern fn int enumDisplayMonitors(Win32_HDC* dc, Rect* clip, void* proc, iptr data) @cname("EnumDisplayMonitors");
extern fn int getMonitorInfoA(Win32_HMONITOR mon, void* info) @cname("GetMonitorInfoA");
extern fn int getMonitorInfoW(Win32_HMONITOR mon, void* info) @cname("GetMonitorInfoW");
extern fn int enumDisplayDevicesW(ushort* device, uint devNum, DisplayDeviceW* dd, uint flags) @cname("EnumDisplayDevicesW");
extern fn int enumDisplaySettingsW(ushort* device, uint modeNum, DevModeW* dm) @cname("EnumDisplaySettingsW");
extern fn int changeDisplaySettingsExW(ushort* device, DevModeW* dm, void* wnd, uint flags, void* param) @cname("ChangeDisplaySettingsExW");
extern fn void setProcessDPIAware() @cname("SetProcessDPIAware");
extern fn uint regGetValueW(void* key, ushort* subKey, ushort* value, uint flags, uint* type, void* data, uint* dataSize) @cname("RegGetValueW");

extern fn int openClipboard(Win32_HWND* wnd) @cname("OpenClipboard");
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

extern fn int dragQueryPoint(void* drop, Win32_POINT* p) @cname("DragQueryPoint");
extern fn uint dragQueryFileW(void* drop, uint fileIndex, ushort* file, uint cch) @cname("DragQueryFileW");
extern fn void dragFinish(void* drop) @cname("DragFinish");

extern fn int getUpdateRect(Win32_HWND* wnd, Rect* rect, int erase) @cname("GetUpdateRect");
extern fn int beginPaint(Win32_HWND* wnd, void* ps) @cname("BeginPaint");
extern fn int endPaint(Win32_HWND* wnd, void* ps) @cname("EndPaint");
extern fn uint setTimer(Win32_HWND* wnd, uptr idEvent, uint elapse, void* proc) @cname("SetTimer");
extern fn int killTimer(Win32_HWND* wnd, uptr idEvent) @cname("KillTimer");

extern fn int dwmEnableBlurBehindWindow(Win32_HWND* wnd, void* bb) @cname("DwmEnableBlurBehindWindow");
extern fn int dwmSetWindowAttribute(Win32_HWND* wnd, uint attr, void* value, uint size) @cname("DwmSetWindowAttribute");

extern fn uint timeBeginPeriod(uint period) @cname("timeBeginPeriod");
extern fn uint timeEndPeriod(uint period) @cname("timeEndPeriod");

alias TodoRenameWglCreateContextProc = fn HGlrc(Win32_HDC*);
alias TodoRenameWglDeleteContextProc = fn int(HGlrc);
alias TodoRenameWglMakeCurrentProc = fn int(Win32_HDC*, HGlrc);
alias TodoRenameWglGetCurrentDcProc = fn Win32_HDC*();
alias TodoRenameWglGetCurrentContextProc = fn HGlrc();
alias TodoRenameWglShareListsProc = fn int(HGlrc, HGlrc);
alias TodoRenameWglCreateContextAttribsArbProc = fn HGlrc(Win32_HDC*, HGlrc, int*);
alias TodoRenameWglChoosePixelFormatArbProc = fn int(Win32_HDC*, int*, float*, uint, int*, uint*);
alias TodoRenameWglSwapIntervalExtProc = fn int(int);

TodoRenameWglCreateContextProc todo_fix_wglCreateContext;
TodoRenameWglDeleteContextProc todo_fix_wglDeleteContext;
TodoRenameWglMakeCurrentProc todo_fix_wglMakeCurrent;
TodoRenameWglGetCurrentDcProc todo_fix_wglGetCurrentDC;
TodoRenameWglGetCurrentContextProc todo_fix_wglGetCurrentContext;
TodoRenameWglShareListsProc todo_fix_wglShareLists;
TodoRenameWglCreateContextAttribsArbProc todo_fix_wglCreateContextAttribsARB;
TodoRenameWglChoosePixelFormatArbProc todo_fix_wglChoosePixelFormatARB;
TodoRenameWglSwapIntervalExtProc todo_fix_wglSwapIntervalEXT;


struct TodoRenameGlContext { void* ctx; } // HGLRC
struct TodoRenameEglContext { void* ctx; void* surface; void* eglWindow; }

const TodoRenameGlReleaseBehavior TODO_RENAME_GL_RELEASE_FLUSH = 0;
const TodoRenameGlReleaseBehavior TODO_RENAME_GL_RELEASE_NONE = 1;

const TodoRenameGlProfile TODO_RENAME_GL_CORE = 0;
const TodoRenameGlProfile TODO_RENAME_GL_FORWARD_COMPATIBILITY = 1;
const TodoRenameGlProfile TODO_RENAME_GL_COMPATIBILITY = 2;
const TodoRenameGlProfile TODO_RENAME_GL_ES = 3;
const TodoRenameGlProfile TODO_RENAME_GL_WEB = 4;

const TodoRenameGlRenderer TODO_RENAME_GL_ACCELERATED = 0;
const TodoRenameGlRenderer TODO_RENAME_GL_SOFTWARE = 1;

struct TodoRenameGlHints
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
	TodoRenameGlReleaseBehavior releaseBehavior;
	TodoRenameGlProfile profile;
	int major, minor;
	TodoRenameGlContext* share;
	TodoRenameEglContext* shareEGL;
	TodoRenameGlRenderer renderer;
}

union TodoRenameGfxContext
{
	TodoRenameGlContext* native;
	TodoRenameEglContext* egl;
}


struct GfxNativeImage
{
	GfxFormat format;
	char* buffer; // bitmapBits
	void* bitmap; // HBITMAP
	void* hdcMem; // HDC
}

struct TodoRenameWindowSrc
{
	inline IntRect bounds;
	void* window; // HWND
	void* hdc; // HDC
	void* hIconSmall; // HICON
	void* hIconBig; // HICON
	GfxPoint min_size;
	GfxPoint max_size;
	int aspectRatioW, aspectRatioH;
	bool actionFrame;
	ushort highSurrogate;
	TodoRenameGfxContext ctx;
	TodoRenameGfxContextType gfxType;
}





// public


const WGPUSType WGPU_STYPE_SURFACE_SOURCE_WINDOWS_HWND = 0x00000005;
struct WgpuSurfaceSourceWindowsHWND { WgpuChainedStruct chain; void* hinstance; void* hwnd; }
extern fn WGPUSurface wgpuInstanceCreateSurface(WGPUInstance instance, WgpuSurfaceDescriptor* descriptor) @cname("wgpuInstanceCreateSurface");

// public
fn WGPUSurface todo_fix_window_createSurface_WebGPU(GfxWindow* window, WGPUInstance instance)
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
fn int todo_fix_window_createSwapChain_DirectX(GfxWindow* win, DxgiFactory* pFactory, void* pDevice, void** swapchain)
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

		todo_fix_debugCallback(TODO_RENAME_TYPE_ERROR, TODO_RENAME_ERR_DIRECT_X_CONTEXT, "Failed to create DirectX swap chain!");
		return -2;
	}
	return 0;
}




// public
fn uint todo_fix_winapiWindowGetStyle(GfxWindow* win, GfxWindowFlags flags)
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
fn uint todo_fix_winapiWindowGetExStyle(GfxWindow* win, GfxWindowFlags flags)
{
	uint style = 0x00040000; // WS_EX_APPWINDOW
	if (flags.fullscreen || flags.floating || win.is_floating()) style |= 0x00000008; // WS_EX_TOPMOST
	return style;
}

// private
fn bool todo_fix_createUTF8FromWideStringWin32(ushort* source, char* output, sz max)
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
fn void todo_fix_win32MakeWindowTransparent(GfxWindow* win)
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
fn bool todo_fix_win32GetDarkModeState()
{
	uint lightMode = 1;
	uint len = uint::size;
	static ushort[61] subKey = {(ushort)'S', (ushort)'o', (ushort)'f', (ushort)'t', (ushort)'w', (ushort)'a', (ushort)'r', (ushort)'e', (ushort)'\\', (ushort)'M', (ushort)'i', (ushort)'c', (ushort)'r', (ushort)'o', (ushort)'s', (ushort)'o', (ushort)'f', (ushort)'t', (ushort)'\\', (ushort)'W', (ushort)'i', (ushort)'n', (ushort)'d', (ushort)'o', (ushort)'w', (ushort)'s', (ushort)'\\', (ushort)'C', (ushort)'u', (ushort)'r', (ushort)'r', (ushort)'e', (ushort)'n', (ushort)'t', (ushort)'V', (ushort)'e', (ushort)'r', (ushort)'s', (ushort)'i', (ushort)'o', (ushort)'n', (ushort)'\\', (ushort)'T', (ushort)'h', (ushort)'e', (ushort)'m', (ushort)'e', (ushort)'s', (ushort)'\\', (ushort)'P', (ushort)'e', (ushort)'r', (ushort)'s', (ushort)'o', (ushort)'n', (ushort)'a', (ushort)'l', (ushort)'i', (ushort)'z', (ushort)'e', 0}; // "Software\Microsoft\Windows\CurrentVersion\Themes\Personalize"
	static ushort[18] valueName = {(ushort)'A', (ushort)'p', (ushort)'p', (ushort)'s', (ushort)'U', (ushort)'s', (ushort)'e', (ushort)'L', (ushort)'i', (ushort)'g', (ushort)'h', (ushort)'t', (ushort)'T', (ushort)'h', (ushort)'e', (ushort)'m', (ushort)'e', 0}; // "AppsUseLightTheme"
	regGetValueW((void*)(uptr)0x80000001 /* HKEY_CURRENT_USER */, (ushort*)&subKey, (ushort*)&valueName, 0x00000010 /* RRF_RT_REG_DWORD */, null, &lightMode, &len);
	return lightMode == 0;
}


// public
fn void todo_fix_win32MakeWindowDarkMode(GfxWindow* win, bool state)
{
	int value = (state == true) ? 1 : 0;
	dwmSetWindowAttribute(win.src.window, 20 /* DWMWA_USE_IMMERSIVE_DARK_MODE */, &value, (uint)int::size);
}

macro GfxPoint _param_to_point(iptr lParam)
{
	return { (int)(lParam & 0xffff), (int)((lParam >> 16) & 0xffff) };
}

// private
fn iptr todo_fix_wndProcW(Win32_HWND* hWnd, uint message, sz wParam, iptr lParam)
{
	if (message == 0x007E /* WM_DISPLAYCHANGE */) todo_fix_pollMonitors();

	if (hWnd == _gfx_ctx.helperWindow) return defWindowProcW(hWnd, message, wParam, lParam);

	GfxWindow* win = (GfxWindow*)getPropW(hWnd, todo_fix_wideTodoRenamePropName());
	if (win == null) return defWindowProcW(hWnd, message, wParam, lParam);

	Rect frame;
	uint style = todo_fix_winapiWindowGetStyle(win, win.internal.flags);
	uint exStyle = todo_fix_winapiWindowGetExStyle(win, win.internal.flags);
	adjustWindowRectEx(&frame, style, 0, exStyle);

	switch (message)
	{
		case 0x0010: case 0x0012: _window_close_callback(win); return 0; // WM_CLOSE, WM_QUIT
		case 0x0006: // WM_ACTIVATE
		{
			bool inFocus = (wParam & 0xffff) != 0;
			todo_fix_windowFocusCallback(win, inFocus);
			return defWindowProcW(hWnd, message, wParam, lParam);
		}
		case 0x0003: // WM_MOVE
		{
			if (win.internal.capture_mouse) _window_set_capture_mouse_native(win, true);
			_window_moved_callback(win, _param_to_point(lParam));
			return defWindowProcW(hWnd, message, wParam, lParam);
		}
		case 0x0005: // WM_SIZE
		{
			if (win.internal.capture_mouse) _window_set_capture_mouse_native(win, true);
			_window_resized_callback(win, _param_to_point(lParam));
			todo_fix_window_checkMode(win);
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
				_window_set_capture_mouse_native(win, win.internal.capture_mouse);
				win.src.actionFrame = false;
			}
			break;
		}
		case 0x02E0: // WM_DPICHANGED
			float scaleX = (float)((wParam >> 16) & 0xffff) / 96.0f;
			float scaleY = (float)(wParam & 0xffff) / 96.0f;
			todo_fix_scaleUpdatedCallback(win, { scaleX, scaleY});
			return defWindowProcW(hWnd, message, wParam, lParam);
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
			bool resize = win.src.min_size == win.src.max_size;
			win.internal.flags.no_resize = resize;

			mmi.ptMinTrackSize.x = win.src.min_size.x + (frame.right - frame.left);
			mmi.ptMinTrackSize.y = win.src.min_size.y + (frame.bottom - frame.top);
			if (win.src.max_size == { 0, 0 }) return defWindowProcW(hWnd, message, wParam, lParam);

			mmi.ptMaxTrackSize.x = win.src.max_size.x + (frame.right - frame.left);
			mmi.ptMaxTrackSize.y = win.src.max_size.y + (frame.bottom - frame.top);
			return defWindowProcW(hWnd, message, wParam, lParam);
		}
		case 0x0231: // WM_ENTERSIZEMOVE
		case 0x0232: // WM_EXITSIZEMOVE
		{
			if (win.src.actionFrame) _window_set_capture_mouse_native(win, win.internal.capture_mouse);
			break;
		}
		case 0x0113: todo_fix_windowRefreshCallback(win, { 0, 0, ...win.size }); break; // WM_TIMER
		case 0x00A1: // WM_NCLBUTTONDOWN
		{
			// workaround for half-second pause when starting to move window
			Win32_POINT point = {0, 0};
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
				todo_fix_windowRefreshCallback(win, { rect.left, rect.top, rect.right - rect.left, rect.bottom - rect.top });
			}
			else
			{
				char[128] ps;
				beginPaint(hWnd, &ps);
				todo_fix_windowRefreshCallback(win, { 0, 0, ...win.size });
				endPaint(hWnd, &ps);
			}
			return defWindowProcW(hWnd, message, wParam, lParam);
		}
		case 0x031E: case 0x0320: todo_fix_win32MakeWindowTransparent(win); break; // WM_DWMCOMPOSITIONCHANGED, WM_DWMCOLORIZATIONCOLORCHANGED
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
				todo_fix_keyCharCallback(win, codepoint);
			}
			return 0;
		}
		case 0x0109: // WM_UNICHAR
		{
			if (wParam == 0xFFFF /* UNICODE_NOCHAR */) return 1;
			todo_fix_keyCharCallback(win, (uint)wParam);
			return 0;
		}
		case 0x0101: case 0x0105: // WM_KEYUP, WM_SYSKEYUP
		{
			if (!(win.internal.enabled_events & TODO_RENAME_KEY_RELEASED_FLAG)) return defWindowProcW(hWnd, message, wParam, lParam);
			int scancode = (int)((lParam >> 16) & (0x0100 | 0xff));
			if (scancode == 0) scancode = (int)mapVirtualKeyW((uint)wParam, 0 /* MAPVK_VK_TO_VSC */);
			switch (scancode)
			{
				case 0x54: scancode = 0x137;
				case 0x146: scancode = 0x45;
				case 0x136: scancode = 0x36;
				default: break;
			}
			GfxKey value = (GfxKey)todo_fix_apiKeyToRGFW((uint)scancode);
			if (wParam == 0x11 /* VK_CONTROL */) { value = ((lParam >> 16) & 0x0100) != 0 ? CONTROL_R : CONTROL_L; }
			_key_update_key_mods(win, getKeyState(0x14) & 1 != 0, getKeyState(0x90) & 1 != 0, getKeyState(0x91) & 1 != 0);
			todo_fix_keyCallback(win, value, win.internal.mod, false, false);
			break;
		}
		case 0x0100: case 0x0104: // WM_KEYDOWN, WM_SYSKEYDOWN
		{
			if (!(win.internal.enabled_events & TODO_RENAME_KEY_PRESSED_FLAG)) return defWindowProcW(hWnd, message, wParam, lParam);
			int scancode = (int)((lParam >> 16) & (0x0100 | 0xff));
			if (scancode == 0) scancode = (int)mapVirtualKeyW((uint)wParam, 0);
			switch (scancode)
			{
				case 0x54: scancode = 0x137;
				case 0x146: scancode = 0x45;
				case 0x136: scancode = 0x36;
				default: break;
			}
			GfxKey value = (GfxKey)todo_fix_apiKeyToRGFW((uint)scancode);
			if (wParam == 0x11) { value = ((lParam >> 16) & 0x0100) != 0 ? CONTROL_R : CONTROL_L; }
			bool repeat = is_key_down(value);
			_key_update_key_mods(win, getKeyState(0x14) & 1 != 0, getKeyState(0x90) & 1 != 0, getKeyState(0x91) & 1 != 0);
			todo_fix_keyCallback(win, value, win.internal.mod, repeat, true);
			break;
		}
		case 0x0200: // WM_MOUSEMOVE
			GfxPoint where = _param_to_point(lParam);
			if (win.internal.mouse_inside == false) _mouse_notify_callback(win, where, true);
			_mouse_motion_callback(win, where);
		case 0x00FF: // WM_INPUT
			if (!(win.internal.raw_mouse || _gfx_ctx.raw_mouse)) return defWindowProcW(hWnd, message, wParam, lParam);

			RawInputMouse raw;
			uint rawSize = (uint)RawInputMouse::size;
			getRawInputData((HRawInput)(uptr)lParam, RID_INPUT, &raw, &rawSize, (uint)RawInputHeader::size);

			if (raw.header.dwType != RIM_TYPEMOUSE || (raw.mouse.lLastX == 0 && raw.mouse.lLastY == 0)) break;

			float vecX = 0.0f;
			float vecY = 0.0f;

			if (raw.mouse.usFlags & MOUSE_MOVE_ABSOLUTE)
			{
				Win32_POINT pos = {0, 0};
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

			_raw_motion_callback(win, { vecX, vecY });
		case 0x02A3: _mouse_notify_callback(win, win.internal.last_mouse, false); // WM_MOUSELEAVE
		case 0x0201: todo_fix_mouseButtonCallback(win, LEFT, true);
		case 0x0204: todo_fix_mouseButtonCallback(win, RIGHT, true);
		case 0x0207: todo_fix_mouseButtonCallback(win, MIDDLE, true);
		case 0x020B: todo_fix_mouseButtonCallback(win, (GfxMouseButton)(GfxMouseButton.MISC1.ordinal + (((wParam >> 16) & 0xffff) == 2 ? 1 : 0)), true);
		case 0x0202: todo_fix_mouseButtonCallback(win, LEFT, false);
		case 0x0205: todo_fix_mouseButtonCallback(win, RIGHT, false);
		case 0x0208: todo_fix_mouseButtonCallback(win, MIDDLE, false);
		case 0x020C: todo_fix_mouseButtonCallback(win, (GfxMouseButton)(GfxMouseButton.MISC1.ordinal + (((wParam >> 16) & 0xffff) == 2 ? 1 : 0)), false);
		case 0x020A: // WM_MOUSEWHEEL
			float scrollY = (float)((short)((wParam >> 16) & 0xffff)) / 120.0f; // WHEEL_DELTA
			todo_fix_mouseScrollCallback(win, { 0.0f, scrollY });
		case 0x020E: // WM_MOUSEHWHEEL
			float scrollX = -(float)((short)((wParam >> 16) & 0xffff)) / 120.0f;
			todo_fix_mouseScrollCallback(win, { scrollX, 0.0f });
		case 0x0233: // WM_DROPFILES
		{
			void* drop = (void*)wParam;
			Win32_POINT pt;
			dragQueryPoint(drop, &pt);
			todo_fix_dataDragCallback(win, FILE, MOVE, { pt.x, pt.y });
			if (!(win.internal.enabled_events & TODO_RENAME_DATA_DROP_FLAG)) return defWindowProcW(hWnd, message, wParam, lParam);
			uint count = dragQueryFileW(drop, 0xffffffff, null, 0);
			for (uint i = 0; i < count; i++)
			{
				uint length = dragQueryFileW(drop, i, null, 0);
				if (length == 0) continue;
				ushort* buffer = (ushort*)malloc((sz)(length + 1) * 2);
				char* cbuffer = (char*)malloc((sz)length + 1);
				dragQueryFileW(drop, i, buffer, length + 1);
				todo_fix_createUTF8FromWideStringWin32(buffer, cbuffer, length);
				todo_fix_dataDropCallback(win, cbuffer, (sz)length + 1, FILE);
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
fn ushort* todo_fix_wideTodoRenamePropName()
{
	static ushort[5] name = {(ushort)'R', (ushort)'G', (ushort)'F', (ushort)'W', 0};
	return &name;
}

// public
fn void todo_fix_initKeycodesPlatform()
{
	_gfx_ctx.keycodes[0x00B] = NUM0; _gfx_ctx.keycodes[0x002] = NUM1; _gfx_ctx.keycodes[0x003] = NUM2;
	_gfx_ctx.keycodes[0x004] = NUM3; _gfx_ctx.keycodes[0x005] = NUM4; _gfx_ctx.keycodes[0x006] = NUM5;
	_gfx_ctx.keycodes[0x007] = NUM6; _gfx_ctx.keycodes[0x008] = NUM7; _gfx_ctx.keycodes[0x009] = NUM8;
	_gfx_ctx.keycodes[0x00A] = NUM9;
	_gfx_ctx.keycodes[0x01E] = A; _gfx_ctx.keycodes[0x030] = B; _gfx_ctx.keycodes[0x02E] = C;
	_gfx_ctx.keycodes[0x020] = D; _gfx_ctx.keycodes[0x012] = E; _gfx_ctx.keycodes[0x021] = F;
	_gfx_ctx.keycodes[0x022] = G; _gfx_ctx.keycodes[0x023] = H; _gfx_ctx.keycodes[0x017] = I;
	_gfx_ctx.keycodes[0x024] = J; _gfx_ctx.keycodes[0x025] = K; _gfx_ctx.keycodes[0x026] = L;
	_gfx_ctx.keycodes[0x032] = M; _gfx_ctx.keycodes[0x031] = N; _gfx_ctx.keycodes[0x018] = O;
	_gfx_ctx.keycodes[0x019] = P; _gfx_ctx.keycodes[0x010] = Q; _gfx_ctx.keycodes[0x013] = R;
	_gfx_ctx.keycodes[0x01F] = S; _gfx_ctx.keycodes[0x014] = T; _gfx_ctx.keycodes[0x016] = U;
	_gfx_ctx.keycodes[0x02F] = V; _gfx_ctx.keycodes[0x011] = W; _gfx_ctx.keycodes[0x02D] = X;
	_gfx_ctx.keycodes[0x015] = Y; _gfx_ctx.keycodes[0x02C] = Z;
	_gfx_ctx.keycodes[0x028] = APOSTROPHE; _gfx_ctx.keycodes[0x02B] = BACK_SLASH;
	_gfx_ctx.keycodes[0x033] = COMMA; _gfx_ctx.keycodes[0x00D] = EQUALS;
	_gfx_ctx.keycodes[0x029] = BACKTICK; _gfx_ctx.keycodes[0x01A] = BRACKET;
	_gfx_ctx.keycodes[0x00C] = MINUS; _gfx_ctx.keycodes[0x034] = PERIOD;
	_gfx_ctx.keycodes[0x01B] = CLOSE_BRACKET; _gfx_ctx.keycodes[0x027] = SEMICOLON;
	_gfx_ctx.keycodes[0x035] = SLASH; _gfx_ctx.keycodes[0x056] = WORLD2;
	_gfx_ctx.keycodes[0x00E] = BACK_SPACE; _gfx_ctx.keycodes[0x153] = DELETE;
	_gfx_ctx.keycodes[0x14F] = END; _gfx_ctx.keycodes[0x01C] = ENTER;
	_gfx_ctx.keycodes[0x001] = ESCAPE; _gfx_ctx.keycodes[0x147] = HOME;
	_gfx_ctx.keycodes[0x152] = INSERT; _gfx_ctx.keycodes[0x15D] = MENU;
	_gfx_ctx.keycodes[0x151] = PAGE_DOWN; _gfx_ctx.keycodes[0x149] = PAGE_UP;
	_gfx_ctx.keycodes[0x045] = PAUSE; _gfx_ctx.keycodes[0x039] = SPACE;
	_gfx_ctx.keycodes[0x00F] = TAB; _gfx_ctx.keycodes[0x03A] = CAPS_LOCK;
	_gfx_ctx.keycodes[0x145] = NUM_LOCK; _gfx_ctx.keycodes[0x046] = SCROLL_LOCK;
	_gfx_ctx.keycodes[0x03B] = F1; _gfx_ctx.keycodes[0x03C] = F2; _gfx_ctx.keycodes[0x03D] = F3;
	_gfx_ctx.keycodes[0x03E] = F4; _gfx_ctx.keycodes[0x03F] = F5; _gfx_ctx.keycodes[0x040] = F6;
	_gfx_ctx.keycodes[0x041] = F7; _gfx_ctx.keycodes[0x042] = F8; _gfx_ctx.keycodes[0x043] = F9;
	_gfx_ctx.keycodes[0x044] = F10; _gfx_ctx.keycodes[0x057] = F11; _gfx_ctx.keycodes[0x058] = F12;
	_gfx_ctx.keycodes[0x064] = F13; _gfx_ctx.keycodes[0x065] = F14; _gfx_ctx.keycodes[0x066] = F15;
	_gfx_ctx.keycodes[0x067] = F16; _gfx_ctx.keycodes[0x068] = F17; _gfx_ctx.keycodes[0x069] = F18;
	_gfx_ctx.keycodes[0x06A] = F19; _gfx_ctx.keycodes[0x06B] = F20; _gfx_ctx.keycodes[0x06C] = F21;
	_gfx_ctx.keycodes[0x06D] = F22; _gfx_ctx.keycodes[0x06E] = F23; _gfx_ctx.keycodes[0x076] = F24;
	_gfx_ctx.keycodes[0x038] = ALT_L; _gfx_ctx.keycodes[0x01D] = CONTROL_L; _gfx_ctx.keycodes[0x02A] = SHIFT_L;
	_gfx_ctx.keycodes[0x15B] = SUPER_L; _gfx_ctx.keycodes[0x137] = PRINT_SCREEN;
	_gfx_ctx.keycodes[0x138] = ALT_R; _gfx_ctx.keycodes[0x11D] = CONTROL_R;
	_gfx_ctx.keycodes[0x036] = SHIFT_R; _gfx_ctx.keycodes[0x15C] = SUPER_R;
	_gfx_ctx.keycodes[0x150] = DOWN; _gfx_ctx.keycodes[0x14B] = LEFT;
	_gfx_ctx.keycodes[0x14D] = RIGHT; _gfx_ctx.keycodes[0x148] = UP;
	_gfx_ctx.keycodes[0x052] = PAD0; _gfx_ctx.keycodes[0x04F] = PAD1; _gfx_ctx.keycodes[0x050] = PAD2;
	_gfx_ctx.keycodes[0x051] = PAD3; _gfx_ctx.keycodes[0x04B] = PAD4; _gfx_ctx.keycodes[0x04C] = PAD5;
	_gfx_ctx.keycodes[0x04D] = PAD6; _gfx_ctx.keycodes[0x047] = PAD7; _gfx_ctx.keycodes[0x048] = PAD8;
	_gfx_ctx.keycodes[0x049] = PAD9; _gfx_ctx.keycodes[0x04E] = PAD_PLUS; _gfx_ctx.keycodes[0x053] = PAD_PERIOD;
	_gfx_ctx.keycodes[0x135] = PAD_SLASH; _gfx_ctx.keycodes[0x11C] = PAD_RETURN;
	_gfx_ctx.keycodes[0x059] = PAD_EQUAL; _gfx_ctx.keycodes[0x037] = PAD_MULTIPLY; _gfx_ctx.keycodes[0x04A] = PAD_MINUS;
}

// public
fn int todo_fix_initPlatform(String class_name, GfxInitOption flags)
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
	_gfx_ctx.wndClass.wndProc = &todo_fix_wndProcW;
	_gfx_ctx.wndClass.clsExtra = (int)sz::size;

	_gfx_ctx.wndClass.hIcon = (Win32_HICON)loadImageA(_gfx_ctx.instance, "TODO_RENAME_ICON", 1 /* IMAGE_ICON */, 0, 0, 0x00000040 | 0x00008000 /* LR_DEFAULTSIZE|LR_SHARED */);
	if (_gfx_ctx.wndClass.hIcon == null)
	{
		_gfx_ctx.wndClass.hIcon = (Win32_HICON)loadImageA(null, (char*)(uptr)32512 /* IDI_APPLICATION */, 1, 0, 0, 0x00000040 | 0x00008000);
	}

	registerClassW(&_gfx_ctx.wndClass);
	_gfx_ctx.helperWindow = win32::createWindowExW(0, _gfx_ctx.wndClass.className, null, 0, 0, 0, 0, 0, null, null, _gfx_ctx.instance, null);

	char[4] blk = {0, 0, 0, 0};
	_gfx_ctx.hiddenMouse = todo_fix_createMouse((char*)&blk, 1, 1, RGBA8);
	return 0;
}

// public
fn void? todo_fix_createWindowPlatform(String name, GfxWindowFlags flags, GfxWindow* win) => @pool()
{
	if (!name.len) name = " ";
	win.src.hIconSmall = null; win.src.hIconBig = null;
	win.src.max_size = {};
	win.src.min_size = {};
	win.src.aspectRatioW = 0; win.src.aspectRatioH = 0;

	uint windowStyle = 0x04000000 | 0x02000000; // WS_CLIPSIBLINGS | WS_CLIPCHILDREN
	if (!(flags.no_border))
	{
		windowStyle |= 0x00C00000 | 0x00080000 | 0x00800000 | 0x00020000; // WS_CAPTION|WS_SYSMENU|WS_BORDER|WS_MINIMIZEBOX
		if (!(flags.no_resize)) windowStyle |= 0x00040000 | 0x00010000; // WS_SIZEBOX | WS_MAXIMIZEBOX
	}
	else { windowStyle |= 0x80000000 | 0x10000000 | 0x00080000; } // WS_POPUP|WS_VISIBLE|WS_SYSMENU


	Rect rect = {0, 0, win.width, win.height};
	uint style = todo_fix_winapiWindowGetStyle(win, flags);
	uint exStyle = todo_fix_winapiWindowGetExStyle(win, flags);
	adjustWindowRectEx(&rect, style, 0, exStyle);

	win.src.window = win32::createWindowExW(0, _gfx_ctx.wndClass.className, name.to_wstring(tmem)!!, windowStyle, win.x + rect.left, win.y + rect.top, rect.right - rect.left, rect.bottom - rect.top, null, null, _gfx_ctx.instance, null);
	setPropW(win.src.window, todo_fix_wideTodoRenamePropName(), win);
	win.resize(win.size);

	if (flags.allow_dnd)
	{
		win.internal.flags.allow_dnd = true;
		win.set_dnd(true);
	}
	win.src.hdc = getDC(win.src.window);

	todo_fix_win32MakeWindowDarkMode(win, todo_fix_win32GetDarkModeState());
	todo_fix_win32MakeWindowTransparent(win);
}

// public
fn void _window_set_border_native(GfxWindow* win, bool border)
{

	win.internal.flags.no_border = !border;

	Rect rect;
	getClientRect(win.src.window, &rect);
	CLong style = getWindowLongW(win.src.window, -16 /* GWL_STYLE */);
	style |= (CLong)todo_fix_winapiWindowGetStyle(win, win.internal.flags);

	if (!border) { style &= ~(CLong)0x00CF0000; } // ~WS_OVERLAPPEDWINDOW
	else
	{
		if (win.internal.flags.no_resize) style &= ~(CLong)0x00010000; // ~WS_MAXIMIZEBOX
		style |= 0x00CF0000; // WS_OVERLAPPEDWINDOW
	}

	uint exStyle = todo_fix_winapiWindowGetExStyle(win, win.internal.flags);
	clientToScreen(win.src.window, (Win32_POINT*)&rect.left);
	clientToScreen(win.src.window, (Win32_POINT*)&rect.right);
	adjustWindowRectEx(&rect, (uint)style, 0, exStyle);
	setWindowLongW(win.src.window, -16, style);
	setWindowPos(win.src.window, (Win32_HWND*)(uptr)0 /* HWND_TOP */, rect.left, rect.top, rect.right - rect.left, rect.bottom - rect.top, 0x0020 | 0x0010 | 0x0004); // SWP_FRAMECHANGED|NOACTIVATE|NOZORDER
}


// public

// public
fn void _window_set_aspect_ratio_native(GfxWindow* win, int w, int h) @private
{
	win.src.aspectRatioW = w;
	win.src.aspectRatioH = h;
}
// public
// public
fn void _window_set_max_size_native(GfxWindow* win, GfxSize size)
{
	win.src.max_size = size;
}

// public
fn void _window_focus_native(GfxWindow* win) { setForegroundWindow(win.src.window); setFocus(win.src.window); }
// public
fn void todo_fix_window_raise(GfxWindow* win)
{
	bringWindowToTop(win.src.window);
	setWindowPos(win.src.window, (Win32_HWND*)(uptr)0, win.x, win.y, win.width, win.height, 0x0001 | 0x0002 | 0x0200 | 0x0020); // NOSIZE|NOMOVE|NOOWNERZORDER|FRAMECHANGED
}

// public

// public
fn void _window_maximize_native(GfxWindow* win) { win.hide(); showWindow(win.src.window, (int)SW_MAXIMIZE); }
// public
fn void todo_fix_window_minimizePlatform(GfxWindow* win) { showWindow(win.src.window, (int)SW_MINIMIZE); }
// public
fn void todo_fix_window_setFloating(GfxWindow* win, bool floating)
{
	if (floating) { setWindowPos(win.src.window, (Win32_HWND*)(uptr)(-1), 0, 0, 0, 0, 0x0002 | 0x0001 | 0x0040); }
	else { setWindowPos(win.src.window, (Win32_HWND*)(uptr)(-2) /* HWND_NOTOPMOST */, 0, 0, 0, 0, 0x0002 | 0x0001 | 0x0040); }
}
// public
fn void todo_fix_window_setOpacity(GfxWindow* win, char opacity)
{
	setWindowLong(win.src.window, -20, 0x00080000);
	setLayeredWindowAttributes(win.src.window, 0, opacity, 2);
}
// public
fn void _window_restore_native(GfxWindow* win) {}
// public
// public
fn void todo_fix_stopCheckEvents() { postMessageW(_gfx_ctx.helperWindow, 0, 0, 0); }
// public
fn void todo_fix_waitForEvent(int waitMS) { msgWaitForMultipleObjects(0, null, 0, (uint)waitMS, 0x04FF /* QS_ALLINPUT */); }

ushort[8] todo_fix_wideDisplayName = {(ushort)'D', (ushort)'I', (ushort)'S', (ushort)'P', (ushort)'L', (ushort)'A', (ushort)'Y', 0};

struct TodoRenameAttribStack
{
	int* attribs;
	sz count;
	sz max;
}

// public
fn GfxKey todo_fix_physicalToMappedKey(GfxKey key)
{
	uint vsc = todo_fix_rgfwToApiKey(key);
	char[256] keyboardState;
	if (getKeyboardState((char*)&keyboardState) == 0) return key;

	uint vk = mapVirtualKeyW(vsc, 1 /* MAPVK_VSC_TO_VK */);
	void* layout = getKeyboardLayout(0);

	ushort[4] charBuffer;
	int result = toUnicodeEx(vk, vsc, (char*)&keyboardState, &charBuffer, 1, 0, layout);
	if (result == 1 && charBuffer[0] < 256) return (GfxKey)charBuffer[0];

	switch (vk)
	{
		case 0x70: return F1; case 0x71: return F2; case 0x72: return F3;
		case 0x73: return F4; case 0x74: return F5; case 0x75: return F6;
		case 0x76: return F7; case 0x77: return F8; case 0x78: return F9;
		case 0x79: return F10; case 0x7A: return F11; case 0x7B: return F12;
		case 0x7C: return F13; case 0x7D: return F14; case 0x7E: return F15;
		case 0x7F: return F16; case 0x80: return F17; case 0x81: return F18;
		case 0x82: return F19; case 0x83: return F20; case 0x84: return F21;
		case 0x85: return F22; case 0x86: return F23; case 0x87: return F24;
		case 0xA0: return SHIFT_L; case 0xA1: return SHIFT_R;
		case 0xA2: return CONTROL_L; case 0xA3: return CONTROL_R;
		case 0xA4: return ALT_L; case 0xA5: return ALT_R;
		case 0x5B: return SUPER_L; case 0x5C: return SUPER_R;
		case 0x14: return CAPS_LOCK; case 0x90: return NUM_LOCK; case 0x91: return SCROLL_LOCK;
		case 0x26: return UP; case 0x28: return DOWN; case 0x25: return LEFT; case 0x27: return RIGHT;
		case 0x24: return HOME; case 0x23: return END; case 0x21: return PAGE_UP; case 0x22: return PAGE_DOWN;
		case 0x2D: return INSERT; case 0x5D: return MENU;
		case 0x6B: return PAD_PLUS; case 0x6D: return PAD_MINUS; case 0x6A: return PAD_MULTIPLY;
		case 0x6F: return PAD_SLASH; case 0x0D: return PAD_RETURN; case 0x6E: return PAD_PERIOD;
		case 0x60: return PAD0; case 0x61: return PAD1; case 0x62: return PAD2;
		case 0x63: return PAD3; case 0x64: return PAD4; case 0x65: return PAD5;
		case 0x66: return PAD6; case 0x67: return PAD7; case 0x68: return PAD8; case 0x69: return PAD9;
		case 0x2C: return PRINT_SCREEN; case 0x13: return PAUSE;
		default: return NULL;
	}
}

// public
fn GfxPoint _window_fetch_size_native(GfxWindow* win)
{
	Rect area;
	getClientRect(win.src.window, &area);
	win.width = area.right;
	win.height = area.bottom;
	return win.get_size();
}

// public
fn void todo_fix_pollEvents()
{
	todo_fix_resetPrevState();
	Msg msg;
	while (peekMessageA(&msg, null, 0, 0, 1 /* PM_REMOVE */) != 0)
	{
		translateMessage(&msg);
		dispatchMessageA(&msg);
	}
}

// public
fn bool _window_is_hidden_native(GfxWindow* win) => isWindowVisible(win.src.window) == 0 && !_window_is_minimized_native(win);

// public
fn bool _window_is_minimized_native(GfxWindow* win)
{
	WindowPlacement placement;
	getWindowPlacement(win.src.window, &placement);
	return placement.showCmd == SW_SHOWMINIMIZED;
}

// public
fn bool _window_is_maximized_native(GfxWindow* win)
{
	WindowPlacement placement;
	getWindowPlacement(win.src.window, &placement);
	return placement.showCmd == SW_SHOWMAXIMIZED || isZoomed(win.src.window) != 0;
}

// public
fn bool todo_fix_monitor_getWorkarea(GfxMonitor* monitor, int* x, int* y, int* width, int* height)
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
fn sz todo_fix_monitor_getGammaRampPtr(GfxMonitor* monitor, TodoRenameGammaRamp* ramp)
{
	ushort[768] values; // 3 x 256
	Win32_HDC dc = createDCW(&todo_fix_wideDisplayName, (ushort*)&monitor.node.adapterName, null, null);
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
fn bool todo_fix_monitor_setGammaRamp(GfxMonitor* monitor, TodoRenameGammaRamp* ramp)
{
	ushort[768] values;
	if (ramp.count != 256)
	{
		todo_fix_debugCallback(TODO_RENAME_TYPE_ERROR, TODO_RENAME_ERR_PLATFORM, "Win32: Gamma ramp size must be 256");
		return false;
	}
	mem::copy(&values[0], ramp.red, 512);
	mem::copy(&values[256], ramp.green, 512);
	mem::copy(&values[512], ramp.blue, 512);
	Win32_HDC dc = createDCW(&todo_fix_wideDisplayName, (ushort*)&monitor.node.adapterName, null, null);
	setDeviceGammaRamp(dc, &values);
	deleteDC(dc);
	return true;
}

// public
fn void todo_fix_win32GetMode(DevModeW* dm, GfxMonitorMode* mode)
{
	mode.size = { (int)dm.pelsWidth, (int)dm.pelsHeight };
	todo_fix_splitBPP(dm.bitsPerPel, mode);
	switch (dm.displayFrequency)
	{
		case 119: case 59: case 29: mode.refresh_rate = ((float)(dm.displayFrequency + 1) * 1000.0f) / 1001.0f;
		default: mode.refresh_rate = (float)dm.displayFrequency;
	}
}

// public

// private
fn int todo_fix_win32GetMonitorHandleCallback(void* hMonitor, void* hdcMonitor, Rect* lprcMonitor, iptr dwData)
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
fn void todo_fix_win32CreateMonitor(DisplayDeviceW* adapter, DisplayDeviceW* dd)
{
	DevModeW dm;
	dm.size = (ushort)DevModeW::size;
	if (enumDisplaySettingsW((ushort*)&adapter.deviceName, (uint)ENUM_CURRENT_SETTINGS, &dm) == 0) return;

	GfxMonitorNode* node = _monitor_add(null);
	mem::copy(&node.adapterName, &adapter.deviceName, 64);
	if (dd != null) mem::copy(&node.deviceName, &dd.deviceName, 64);

	if (dd != null) todo_fix_createUTF8FromWideStringWin32((ushort*)&dd.deviceString, (char*)&node.mon.name, 127);
	node.mon.name[127] = 0;

	Rect rect;
	rect.left = dm.positionX;
	rect.top = dm.positionY;
	rect.right = dm.positionX + (int)dm.pelsWidth;
	rect.bottom = dm.positionY + (int)dm.pelsHeight;
	enumDisplayMonitors(null, &rect, (void*)&todo_fix_win32GetMonitorHandleCallback, (iptr)(void*)node);

	todo_fix_win32GetMode(&dm, &node.mon.mode);

	MonitorInfoExW monitorInfo;
	monitorInfo.cbSize = MonitorInfoExW::size;
	getMonitorInfoW(node.hMonitor, &monitorInfo);

	node.mon.origin = { monitorInfo.rcMonitor.left, monitorInfo.rcMonitor.top};

	Win32_HDC hdc = createDCW((ushort*)&monitorInfo.szDevice, null, null, null);
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
	todo_fix_monitorCallback(_gfx_ctx.root, &node.mon, true);
}

// public
fn void todo_fix_pollMonitors()
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
					enumDisplayMonitors(null, null, (void*)&todo_fix_win32GetMonitorHandleCallback, (iptr)(void*)&node.mon);
					break;
				}
			}
			if (node != null) continue;
			todo_fix_win32CreateMonitor(&adapter, &dd);
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
			todo_fix_win32CreateMonitor(&adapter, null);
		}
	}
	refresh_monitors();
}

// public
fn void todo_fix_monitorNode_free(GfxMonitorNode* node) {}

// public
fn GfxMonitor* find_monitor(GfxWindow* win)
{
	Win32_HMONITOR src = monitorFromWindow(win.src.window, 1 /* MONITOR_DEFAULTTOPRIMARY */);
	for (GfxMonitorNode* node = _gfx_ctx.monitors.list.head; node != null; node = node.next)
	{
		if (node.hMonitor == src) return &node.mon;
	}
	return get_primary_monitor() ?? null;
}

// public
fn bool todo_fix_monitor_setMode(GfxMonitor* mon, GfxMonitorMode* mode)
{
	DevModeW dm;
	dm.size = (ushort)DevModeW::size;
	dm.fields |= DM_PELSWIDTH | DM_PELSHEIGHT;
	dm.pelsWidth = (uint)mode.size.x;
	dm.pelsHeight = (uint)mode.size.y;
	dm.fields |= DM_DISPLAYFREQUENCY;
	dm.displayFrequency = (uint)mode.refresh_rate;
	dm.fields |= DM_BITSPERPEL;
	dm.bitsPerPel = (uint)(mode.rgb.sum());

	if (changeDisplaySettingsExW((ushort*)&mon.node.adapterName, &dm, null, CDS_TEST, null) == DISP_CHANGE_SUCCESSFUL)
	{
		if (changeDisplaySettingsExW((ushort*)&mon.node.adapterName, &dm, null, CDS_UPDATEREGISTRY, null) == DISP_CHANGE_SUCCESSFUL)
		{
			todo_fix_win32GetMode(&dm, &mon.mode);
			return true;
		}
		return false;
	}
	return false;
}

// public
fn bool todo_fix_monitor_requestMode(GfxMonitor* mon, GfxMonitorMode* mode, GfxMonitorModeRequest request)
{
	DevModeW dm;
	dm.size = (ushort)DevModeW::size;
	uint index = 0;

	for (;;)
	{
		if (enumDisplaySettingsW((ushort*)&mon.node.adapterName, index, &dm) == 0) break;
		index += 1;

		if (request & SCALE) { dm.fields |= DM_PELSWIDTH | DM_PELSHEIGHT; dm.pelsWidth = (uint)mode.size.x; dm.pelsHeight = (uint)mode.size.y; }
		if (request & REFRESH) { dm.fields |= DM_DISPLAYFREQUENCY; dm.displayFrequency = (uint)mode.refresh_rate; }
		if (request & RGB) { dm.fields |= DM_BITSPERPEL; dm.bitsPerPel = (uint)(mode.rgb.sum()); }

		if (changeDisplaySettingsExW((ushort*)&mon.node.adapterName, &dm, null, CDS_TEST, null) == DISP_CHANGE_SUCCESSFUL)
		{
			if (changeDisplaySettingsExW((ushort*)&mon.node.adapterName, &dm, null, CDS_UPDATEREGISTRY, null) == DISP_CHANGE_SUCCESSFUL)
			{
				todo_fix_win32GetMode(&dm, &mon.mode);
				return true;
			}
			return false;
		}
	}
	return false;
}

// private
fn Win32_HICON todo_fix_loadHandleImage(char* data, int w, int h, GfxFormat format, int icon)
{
	BitmapV5Header bi;
	bi.size = BitmapV5Header::size;
	bi.width = w;
	bi.height = -h;
	bi.planes = 1;
	bi.bitCount = 32;
	bi.compression = BI_RGB;

	Win32_HDC dc = getDC(null);
	char* target;
	Win32_HBITMAP color = createDIBSection(dc, &bi, 0, (void**)&target, null, 0);
	copy_image_data(target, w, h, BGRA8, data, format, null);
	releaseDC(null, dc);

	Win32_HBITMAP mask = createBitmap(w, h, 1, 1, null);

	IconInfo ii;
	ii.fIcon = icon;
	ii.xHotspot = (uint)w / 2;
	ii.yHotspot = (uint)h / 2;
	ii.hbmMask = mask;
	ii.hbmColor = color;

	Win32_HICON handle = createIconIndirect(&ii);
	deleteObject(color);
	deleteObject(mask);
	return handle;
}

// public
fn TodoRenameMouse* todo_fix_createMouseStandard(GfxMouseIcon mouse)
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
	return (TodoRenameMouse*)loadCursorA(null, (char*)(uptr)mouseIcon);
}

// public
fn TodoRenameMouse* todo_fix_createMouse(char* data, int w, int h, GfxFormat format) => (TodoRenameMouse*)todo_fix_loadHandleImage(data, w, h, format, 0);

// public
fn bool todo_fix_window_setMousePlatform(GfxWindow* win, TodoRenameMouse* mouse)
{
	setClassLongPtrA(win.src.window, -12 /* GCLP_HCURSOR */, (iptr)(uptr)mouse);
	setCursor((Win32_HCURSOR)mouse);
	return false;
}

// public
fn void todo_fix_freeMouse(TodoRenameMouse* mouse) { destroyCursor((Win32_HCURSOR)mouse); }

fn void _window_hide_native(GfxWindow* win) @private
{
	showWindow(win.src.window, (int)SW_HIDE);
}

fn void _window_flash_native(GfxWindow* win, GfxFlashRequest request) @private
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
fn void todo_fix_deinitPlatform()
{
	timeEndPeriod(1);
	if (_gfx_ctx.helperWindow != null) destroyWindow(_gfx_ctx.helperWindow);
	if (_gfx_ctx.instance != null) unregisterClassW(_gfx_ctx.wndClass.className, _gfx_ctx.instance);
	if (_gfx_ctx.hiddenMouse != null) todo_fix_freeMouse(_gfx_ctx.hiddenMouse);
}

// public
fn void todo_fix_window_closePlatform(GfxWindow* win)
{
	removePropW(win.src.window, todo_fix_wideTodoRenamePropName());
	releaseDC(win.src.window, win.src.hdc);
	destroyWindow(win.src.window);
	if (win.src.hIconSmall != null) destroyIcon(win.src.hIconSmall);
	if (win.src.hIconBig != null) destroyIcon(win.src.hIconBig);
}

// public
fn void _window_move_native(GfxWindow* win, GfxPoint where)
{
	win.origin = where;
	setWindowPos(win.src.window, (Win32_HWND*)(uptr)0, ...where, 0, 0, 0x0001 /* SWP_NOSIZE */);
}

// public

// public
fn void GfxWindow.set_name(&self, String name)
{
	setWindowTextW(self.src.window, name.to_wstring(tmem)!!);
}

// public
fn void todo_fix_window_setMousePassthrough(GfxWindow* win, bool passthrough)
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
fn bool todo_fix_readClipboardPtr(GfxDataTransferType requestedType, char* buffer, sz capacity, GfxDataTransfer* data)
{
	if (requestedType != TEXT) return false;

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
	data.type = TEXT;

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
fn bool todo_fix_writeClipboard(GfxDataTransfer* data)
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
fn void todo_fix_window_moveMouse(GfxWindow* win, int x, int y)
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

alias GetExtStringArbFn = fn ZString(Win32_HDC*);
alias GetExtStringExtFn = fn ZString();
alias GlxGetProcAddressProc = fn GfxFn(ZString);

// public
fn bool todo_fix_extensionSupportedPlatform_OpenGL(ZString extension, sz len)
{
	ZString extensions = null;
	GfxFn proc = _get_opengl_proc_native("wglGetExtensionsStringARB");
	GfxFn proc2 = _get_opengl_proc_native("wglGetExtensionsStringEXT");
	if (proc != null) { extensions = ((GetExtStringArbFn)proc)(todo_fix_wglGetCurrentDC()); }
	else if (proc2 != null) { extensions = ((GetExtStringExtFn)proc2)(); }
	return extensions && todo_fix_extensionSupportedStr(extensions, extension, len);
}

// public
fn bool todo_fix_window_createContextPtr_OpenGL(GfxWindow* win, TodoRenameGlContext* ctx, TodoRenameGlHints* hints)
{
	win.src.ctx.native = ctx;
	win.src.gfxType = TODO_RENAME_GFX_NATIVE_OPEN_GL;

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
	if (hints.renderer == TODO_RENAME_GL_SOFTWARE) pfd.flags |= PFD_GENERIC_FORMAT | PFD_GENERIC_ACCELERATED;

	int pixelFormat = choosePixelFormat(win.src.hdc, &pfd);
	if (todo_fix_wglChoosePixelFormatARB != null)
	{
		int[50] pixelFormatAttribs;
		TodoRenameAttribStack stack;
		todo_fix_attribStack_init(&stack, (int*)&pixelFormatAttribs, 50);

		todo_fix_attribStack_pushAttribs(&stack, WGL_ACCELERATION_ARB, WGL_FULL_ACCELERATION_ARB);
		todo_fix_attribStack_pushAttribs(&stack, WGL_DRAW_TO_WINDOW_ARB, 1);
		todo_fix_attribStack_pushAttribs(&stack, WGL_PIXEL_TYPE_ARB, WGL_TYPE_RGBA_ARB);
		todo_fix_attribStack_pushAttribs(&stack, WGL_SUPPORT_OPENGL_ARB, 1);
		todo_fix_attribStack_pushAttribs(&stack, WGL_COLOR_BITS_ARB, 32);
		todo_fix_attribStack_pushAttribs(&stack, WGL_DOUBLE_BUFFER_ARB, 1);
		todo_fix_attribStack_pushAttribs(&stack, WGL_ALPHA_BITS_ARB, hints.alpha);
		todo_fix_attribStack_pushAttribs(&stack, WGL_DEPTH_BITS_ARB, hints.depth);
		todo_fix_attribStack_pushAttribs(&stack, WGL_STENCIL_BITS_ARB, hints.stencil);
		todo_fix_attribStack_pushAttribs(&stack, WGL_STEREO_ARB, hints.stereo);
		todo_fix_attribStack_pushAttribs(&stack, WGL_AUX_BUFFERS_ARB, hints.auxBuffers);
		todo_fix_attribStack_pushAttribs(&stack, WGL_RED_BITS_ARB, hints.red);
		todo_fix_attribStack_pushAttribs(&stack, WGL_GREEN_BITS_ARB, hints.blue);
		todo_fix_attribStack_pushAttribs(&stack, WGL_BLUE_BITS_ARB, hints.green);
		todo_fix_attribStack_pushAttribs(&stack, WGL_ACCUM_RED_BITS_ARB, hints.accumRed);
		todo_fix_attribStack_pushAttribs(&stack, WGL_ACCUM_GREEN_BITS_ARB, hints.accumGreen);
		todo_fix_attribStack_pushAttribs(&stack, WGL_ACCUM_BLUE_BITS_ARB, hints.accumBlue);
		todo_fix_attribStack_pushAttribs(&stack, WGL_ACCUM_ALPHA_BITS_ARB, hints.accumAlpha);

		if (hints.sRGB)
		{
			if (hints.profile != TODO_RENAME_GL_ES) { todo_fix_attribStack_pushAttribs(&stack, WGL_FRAMEBUFFER_SRGB_CAPABLE_ARB, 1); }
			else { todo_fix_attribStack_pushAttribs(&stack, WGL_COLORSPACE_SRGB_EXT, (int)hints.sRGB); }
		}
		todo_fix_attribStack_pushAttribs(&stack, WGL_COVERAGE_SAMPLES_NV, hints.samples);
		todo_fix_attribStack_pushAttribs(&stack, 0, 0);

		int newPixelFormat;
		uint numFormats;
		todo_fix_wglChoosePixelFormatARB(win.src.hdc, (int*)&pixelFormatAttribs, null, 1, &newPixelFormat, &numFormats);
		if (numFormats == 0) { todo_fix_debugCallback(TODO_RENAME_TYPE_ERROR, TODO_RENAME_ERR_OPEN_GL_CONTEXT, "Failed to create a pixel format for WGL"); }
		else { pixelFormat = newPixelFormat; }
	}

	PixelFormatDescriptor suggested;
	if (describePixelFormat(win.src.hdc, pixelFormat, (uint)PixelFormatDescriptor::size, &suggested) == 0 ||
		setPixelFormat(win.src.hdc, pixelFormat, &pfd) == 0)
	{
		todo_fix_debugCallback(TODO_RENAME_TYPE_ERROR, TODO_RENAME_ERR_OPEN_GL_CONTEXT, "Failed to set the WGL pixel format");
	}

	if (todo_fix_wglCreateContextAttribsARB != null)
	{
		int[40] attribs;
		TodoRenameAttribStack stack;
		todo_fix_attribStack_init(&stack, (int*)&attribs, 40);

		int mask = 0;
		switch (hints.profile)
		{
			case TODO_RENAME_GL_ES: mask |= WGL_CONTEXT_ES_PROFILE_BIT_EXT;
			case TODO_RENAME_GL_COMPATIBILITY: mask |= WGL_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB;
			case TODO_RENAME_GL_FORWARD_COMPATIBILITY: mask |= WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB;
			default: mask |= WGL_CONTEXT_CORE_PROFILE_BIT_ARB;
		}
		todo_fix_attribStack_pushAttribs(&stack, WGL_CONTEXT_PROFILE_MASK_ARB, mask);

		if (hints.minor != 0 || hints.major != 0)
		{
			todo_fix_attribStack_pushAttribs(&stack, WGL_CONTEXT_MAJOR_VERSION_ARB, hints.major);
			todo_fix_attribStack_pushAttribs(&stack, WGL_CONTEXT_MINOR_VERSION_ARB, hints.minor);
		}

		char[32] noError = "WGL_ARB_create_context_no_error";
		if (todo_fix_extensionSupportedPlatform_OpenGL((ZString)&noError, 31)) todo_fix_attribStack_pushAttribs(&stack, WGL_CONTEXT_OPENGL_NO_ERROR_ARB, (int)hints.noError);

		char[30] flushControl = "WGL_ARB_context_flush_control";
		if (todo_fix_extensionSupportedPlatform_OpenGL((ZString)&flushControl, 30))
		{
			if (hints.releaseBehavior == TODO_RENAME_GL_RELEASE_FLUSH) { todo_fix_attribStack_pushAttribs(&stack, WGL_CONTEXT_RELEASE_BEHAVIOR_ARB, WGL_CONTEXT_RELEASE_BEHAVIOR_FLUSH_ARB); }
			else if (hints.releaseBehavior == TODO_RENAME_GL_RELEASE_NONE) { todo_fix_attribStack_pushAttribs(&stack, WGL_CONTEXT_RELEASE_BEHAVIOR_ARB, WGL_CONTEXT_RELEASE_BEHAVIOR_NONE_ARB); }
		}

		int flags = 0;
		if (hints.debug) flags |= WGL_CONTEXT_DEBUG_BIT_ARB;
		char[34] robustness = "WGL_ARB_create_context_robustness";
		if (hints.robustness && todo_fix_extensionSupportedPlatform_OpenGL((ZString)&robustness, 33)) flags |= WGL_CONTEXT_ROBUST_ACCESS_BIT_ARB;
		if (flags != 0) todo_fix_attribStack_pushAttribs(&stack, WGL_CONTEXT_FLAGS_ARB, flags);

		todo_fix_attribStack_pushAttribs(&stack, 0, 0);
		win.src.ctx.native.ctx = todo_fix_wglCreateContextAttribsARB(win.src.hdc, null, (int*)&attribs);
	}

	if (todo_fix_wglCreateContextAttribsARB == null || win.src.ctx.native.ctx == null)
	{
		todo_fix_debugCallback(TODO_RENAME_TYPE_ERROR, TODO_RENAME_ERR_OPEN_GL_CONTEXT, "Failed to create an accelerated OpenGL Context.");
		win.src.ctx.native.ctx = todo_fix_wglCreateContext(win.src.hdc);
	}

	releaseDC(win.src.window, win.src.hdc);
	win.src.hdc = getDC(win.src.window);

	if (hints.share != null) todo_fix_wglShareLists(todo_fix_getCurrentContext_OpenGL(), hints.share.ctx);

	todo_fix_wglMakeCurrent(win.src.hdc, win.src.ctx.native.ctx);
	todo_fix_debugCallback(TODO_RENAME_TYPE_INFO, TODO_RENAME_INFO_OPEN_GL, "OpenGL context initalized.");
	return true;
}

// public
fn void todo_fix_window_deleteContextPtr_OpenGL(GfxWindow* win, TodoRenameGlContext* ctx)
{
	todo_fix_wglDeleteContext(ctx.ctx);
	win.src.ctx.native.ctx = null;
	todo_fix_debugCallback(TODO_RENAME_TYPE_INFO, TODO_RENAME_INFO_OPEN_GL, "OpenGL context freed.");
}

// public
fn void todo_fix_window_makeCurrentContext_OpenGL(GfxWindow* win)
{
	if (win == null) { todo_fix_wglMakeCurrent(null, null); }
	else { todo_fix_wglMakeCurrent(win.src.hdc, win.src.ctx.native.ctx); }
}

// public
fn void* todo_fix_getCurrentContext_OpenGL() => todo_fix_wglGetCurrentContext();
// public
fn void todo_fix_window_swapBuffers_OpenGL(GfxWindow* win) { swapBuffers(win.src.hdc); }






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
fn void todo_fix_window_blitSurface(GfxWindow* win, GfxSurface* surface)
{
	GfxPoint blit = math::min(win.size, surface.size);
	copy_image_data(surface.native.buffer, surface.size.x, blit.y, surface.native.format, surface.data, surface.format, surface.convert_fn);
	bitBlt(win.src.hdc, 0, 0, ...blit, surface.native.hdcMem, 0, 0, 0x00CC0020 /* SRCCOPY */);
}

// public
fn void _window_set_capture_mouse_native(GfxWindow* win, bool state)
{
	if (state == false) { clipCursor(null); return; }

	Rect clipRect;
	getClientRect(win.src.window, &clipRect);
	clientToScreen(win.src.window, (Win32_POINT*)&clipRect.left);
	clientToScreen(win.src.window, (Win32_POINT*)&clipRect.right);
	clipCursor(&clipRect);
}

// public
fn void _window_set_raw_mouse_mode_native(GfxWindow* win, bool state)
{
	RawInputDevice id = {0x01, 0x02, 0, win.src.window};
	id.flags = state == true ? 0 : RIDEV_REMOVE;
	registerRawInputDevices(&id, 1, RawInputDevice::size);
}
