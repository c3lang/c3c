module std::gfx @feat(X11) @link("X11", "Xext", "Xrandr", "GL");

import std::collections::list, std::math, libc;

// C3 port of the X11/GLX RGFW backend from /Users/lerno/Downloads/AGFW.h (RGFW).
// Mechanical, line-by-line style translation of the TODO_RENAMEX11 implementation.
// This is a standalone alternative to rgfw.c3 (the macOS build) - only ONE of
// the rgfw*.c3 files should be included for a given target, matching how the
// original C header only ever compiles one #ifdef backend at a time.
// Optional extensions (Xcursor, XInput2, Xext sync/shape) are loaded via dlopen,
// exactly as the original does when TODO_RENAMENO_X11_CURSOR_PRELOAD etc. are not set.


extern fn char* strtok(char* str, char* delim);
extern fn char* strtokR(char* str, char* delim, char** saveptr) @cname("strtok_r");
extern fn CLong strtol(char* str, char** endptr, int base);
extern fn int clock_gettime(int clock_id, Timespec* ts);

struct Timespec { CLong tv_sec; CLong tv_nsec; }

// ---- Xlib core types (ABI-compatible aliases; XDisplay/XVisual/Screen stay opaque) ----
typedef Screen = void;
alias Colormap = CULong;
alias XCursor = CULong;
alias Pixmap = CULong;
alias GContext = void*;
alias Atom = CULong;
alias VisualID = CULong;
alias KeySym = CULong;
alias KeyCode = char;
alias XlibId = CULong;
alias Region = void*;
alias XlibIC = void*;
alias XlibIM = void*;
alias XIMStyle = CULong;
alias RRMode = CULong;
alias RRCrtc = CULong;
alias GLXContext = void*;
alias GLXFBConfig = void*;
alias GLXDrawable = CULong;
alias GLXWindow = CULong;

const int X_TRUE = 1;
const int X_FALSE = 0;
const int NONE = 0;
const int CURRENT_TIME = 0;

alias GlxGetProcAddressProc = fn GfxFn(ZString);

// GLX
alias TODO_RENAMEglXCreateContextAttribsARBProc = fn GLXContext(XDisplay*, GLXFBConfig, GLXContext, int, int*);
alias TODO_RENAMEglXQueryExtensionsStringProc = fn ZString(XDisplay*, int);
alias TODO_RENAMEglXChooseVisualProc = fn XVisualInfo*(XDisplay*, int, int*);
alias TODO_RENAMEglXCreateContextProc = fn GLXContext(XDisplay*, XVisualInfo*, GLXContext, int);
alias TODO_RENAMEglXMakeCurrentProc = fn int(XDisplay*, GLXDrawable, GLXContext);
alias TODO_RENAMEglXGetCurrentContextProc = fn GLXContext();
alias TODO_RENAMEglXSwapBuffersProc = fn void(XDisplay*, GLXDrawable);
alias TODO_RENAMEglXSwapIntervalEXTProc = fn void(XDisplay*, GLXDrawable, int);
alias TODO_RENAMEglXGetProcAddressProc = fn void*(ZString);
alias TODO_RENAMEglXGetVisualFromFBConfigProc = fn XVisualInfo*(XDisplay*, GLXFBConfig);
alias TODO_RENAMEglXGetFBConfigAttribProc = fn int(XDisplay*, GLXFBConfig, int, int*);
alias TODO_RENAMEglXGetProcAddressARBProc = fn void*(ZString);
alias TODO_RENAMEglXChooseFBConfigProc = fn GLXFBConfig*(XDisplay*, int, int*, int*);
alias TODO_RENAMEglXDestroyContextProc = fn void(XDisplay*, GLXContext);
alias TODO_RENAMEglXCreateWindowProc = fn GLXWindow(XDisplay*, GLXFBConfig, XWindow, int*);
alias TODO_RENAMEglXDestroyWindowProc = fn void(XDisplay*, GLXWindow);
alias TODO_RENAMEglXSwapIntervalNativeProc = fn int(int);

TODO_RENAMEglXCreateContextAttribsARBProc todo_fix_glXCreateContextAttribsARB;
TODO_RENAMEglXQueryExtensionsStringProc todo_fix_glXQueryExtensionsString;
TODO_RENAMEglXChooseVisualProc todo_fix_glXChooseVisual;
TODO_RENAMEglXCreateContextProc todo_fix_glXCreateContext;
TODO_RENAMEglXMakeCurrentProc todo_fix_glXMakeCurrent;
TODO_RENAMEglXGetCurrentContextProc todo_fix_glXGetCurrentContext;
TODO_RENAMEglXSwapBuffersProc todo_fix_glXSwapBuffers;
TODO_RENAMEglXSwapIntervalEXTProc todo_fix_glXSwapIntervalEXT;
TODO_RENAMEglXGetVisualFromFBConfigProc todo_fix_glXGetVisualFromFBConfig;
TODO_RENAMEglXGetFBConfigAttribProc todo_fix_glXGetFBConfigAttrib;
TODO_RENAMEglXGetProcAddressARBProc todo_fix_glXGetProcAddressARB;
TODO_RENAMEglXChooseFBConfigProc todo_fix_glXChooseFBConfig;
TODO_RENAMEglXDestroyContextProc todo_fix_glXDestroyContext;
TODO_RENAMEglXCreateWindowProc todo_fix_glXCreateWindow;
TODO_RENAMEglXDestroyWindowProc todo_fix_glXDestroyWindow;
TODO_RENAMEglXSwapIntervalNativeProc todo_fix_glXSwapIntervalNative;


// ---- Xlib core functions (directly linked, like the original non-dynamic X11 build) ----


// XIMCallback: { XPointer client_data; XIMProc callback; } from Xlib.h. XlibIC/XlibIM
// are both plain void* in this port, so one function-pointer alias covers the callback.
alias XImCallbackProc = fn void(void* im, char* clientData, char* callData);
struct XImCallback
{
	XPointer client_data;
	XImCallbackProc callback;
}

// XIMStyles: { unsigned short count_styles; XIMStyle *supported_styles; } from Xlib.h.
struct XImStyles
{
	ushort count_styles;
	CULong* supported_styles;
}

// Signature required by XRegisterIMInstantiateCallback / XUnregisterIMInstantiateCallback.
alias XImInstantiateCallbackProc = fn void(XDisplay* d, XPointer clientData, XPointer callData);


const uint XKB_USE_CORE_KBD = 0x0100;
const char SHIFT_MASK = 1 << 0;
const char LOCK_MASK = 1 << 1;
const char MOD2_MASK = 1 << 4;
const char MOD3_MASK = 1 << 5;


macro sz ximaskLen(event) => (sz)((event >> 3) + 1);
macro void xiSetMask(char* mask, event) { mask[(sz)(event / 8)] |= (char)(1 << (event % 8)); }
macro bool ximaskIsSet(char* mask, event) => (mask[(sz)(event / 8)] & (char)(1 << (event % 8))) != 0;


struct TodoRenameGlContext { GLXContext ctx; GLXWindow window; }
struct TodoRenameEglContext { void* ctx; void* surface; void* eglWindow; }

const TodoRenameGlReleaseBehavior TODO_RENAMEGL_RELEASE_FLUSH = 0;
const TodoRenameGlReleaseBehavior TODO_RENAMEGL_RELEASE_NONE = 1;

const TodoRenameGlProfile TODO_RENAMEGL_CORE = 0;
const TodoRenameGlProfile TODO_RENAMEGL_FORWARD_COMPATIBILITY = 1;
const TodoRenameGlProfile TODO_RENAMEGL_COMPATIBILITY = 2;
const TodoRenameGlProfile TODO_RENAMEGL_ES = 3;
const TodoRenameGlProfile TODO_RENAMEGL_WEB = 4;

const TodoRenameGlRenderer TODO_RENAMEGL_ACCELERATED = 0;
const TodoRenameGlRenderer TODO_RENAMEGL_SOFTWARE = 1;

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



struct TodoRenameWindowSrc
{
	inline IntRect bounds;
	XWindow window;
	XWindow parent;
	GContext gc;
	XlibIC ic;
	ulong flashEnd;
	long counter_value;
	XSyncCounter counter;
	TodoRenameGfxContext ctx;
	TodoRenameGfxContextType gfxType;
}


const int VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR = 1000004000;
const VkResult VK_ERROR_EXTENSION_NOT_PRESENT = -7;
struct VkXlibSurfaceCreateInfoKHR { int sType; void* pNext; uint flags; XDisplay* dpy; XWindow window; }
alias TodoRenameVkCreateXlibSurfaceKHRProc = fn VkResult(VkInstance instance, VkXlibSurfaceCreateInfoKHR* createInfo, void* allocator, VkSurfaceKHR* surface);
ZString[2] todo_fix_vulkanExtensions = {"VK_KHR_surface", "VK_KHR_xlib_surface"};


extern fn VisualID xVisualIDFromVisual(XVisual* visual) @cname("XVisualIDFromVisual");
alias TodoRenameVkGetPhysicalDeviceXlibPresentationSupportKHRProc = fn uint(VkPhysicalDevice, uint, XDisplay*, VisualID);

// public
fn bool todo_fix_getPhysicalDevicePresentationSupport_Vulkan(VkInstance instance, VkPhysicalDevice physicalDevice, uint queueFamilyIndex)
{
	GfxFn raw = todo_fix_getInstanceProcAddress_Vulkan(instance, "vkGetPhysicalDeviceXlibPresentationSupportKHR");
	if (!raw) return false;
	XVisual* visual = xDefaultVisual(_gfx_ctx.display, xDefaultScreen(_gfx_ctx.display));
	uint out = ((TodoRenameVkGetPhysicalDeviceXlibPresentationSupportKHRProc)raw)(physicalDevice, queueFamilyIndex, _gfx_ctx.display, xVisualIDFromVisual(visual));
	return out != 0;
}


const WGPUSType WGPU_STYPE_SURFACE_SOURCE_XLIB_WINDOW = 0x00000006;
struct WgpuSurfaceSourceXlibWindow { WgpuChainedStruct chain; XDisplay* display; XWindow window; }
extern fn WGPUSurface wgpuInstanceCreateSurface(WGPUInstance instance, WgpuSurfaceDescriptor* descriptor) @cname("wgpuInstanceCreateSurface");


// IM-level destroy callback: fires if the input-method daemon disappears later,
// so we don't keep a dangling _gfx_ctx.im handle around.


// Instantiate callback: fires once an input method actually becomes available.
// Opens the IM, then validates it supports the exact style this library wants
// (XIMPreeditNothing | XIMStatusNothing) before keeping it, and registers an
// IM-level destroy callback so a later-vanishing IM daemon nulls _gfx_ctx.im.


// public
fn void? todo_fix_xCreateWindow(XVisualInfo visual, String name, GfxWindowFlags flags, GfxWindow* win)
{
	if (!name) name = "C3GFX";
	// KeyPress | KeyRelease | ButtonPress | ButtonRelease | PointerMotion | StructureNotify | FocusChange |
	// LeaveWindow | EnterWindow | Exposure | VisibilityChange | PropertyChange
	CLong eventMask = (1 << 0) | (1 << 1) | (1 << 2) | (1 << 3) | (1 << 6) | (1 << 17) | (1 << 21) |
		(1 << 5) | (1 << 4) | (1 << 15) | (1 << 16) | (1 << 22);

	XSetWindowAttributes swa;
	win.src.parent = xDefaultRootWindow(_gfx_ctx.display);

	Colormap cmap = xCreateColormap(_gfx_ctx.display, win.src.parent, visual.visual, 0);
	swa.colormap = cmap;
	swa.event_mask = eventMask;
	swa.background_pixmap = 0;

	win.src.window = xCreateWindow(_gfx_ctx.display, win.src.parent, win.x, win.y, (uint)win.width, (uint)win.height,
		0, visual.depth, 1 /* InputOutput */, visual.visual, (1 << 3) | (1 << 13) | (1 << 11), &swa);
		// CWBorderPixel | CWColormap | CWEventMask
	if (!win.src.window) return X11_WINDOW_CREATION_FAILED~;

	win.src.flashEnd = 0;
	xFreeColors(_gfx_ctx.display, cmap, null, 0, 0);
	xSaveContext(_gfx_ctx.display, win.src.window, _gfx_ctx.context, (XPointer)win);
	win.src.gc = xCreateGC(_gfx_ctx.display, win.src.window, 0, null);

	if (_gfx_ctx.im != null)
	{
		XImCallback callback;
		callback.callback = &todo_fix_x11IcCallback;
		callback.client_data = (XPointer)win;

		win.src.ic = xCreateIC(_gfx_ctx.im, "inputStyle", (CULong)(0x0008 | 0x0400) /* XIMPreeditNothing | XIMStatusNothing */,
			"clientWindow", win.src.window, "focusWindow", win.src.window,
			"destroyCallback", &callback, (char*)null);
	}

	@pool()
	{
		XClassHint hint;

		hint.res_name = name.zstr_copy(tmem);
		hint.res_class = _gfx_ctx.class_name.zstr_copy(tmem);
		xSetClassHint(_gfx_ctx.display, win.src.window, &hint);
	};

	XWMHints hints;
	hints.flags = 1 << 1; // StateHint
	hints.initial_state = 1; // NormalState
	xSetWMHints(_gfx_ctx.display, win.src.window, &hints);

	xSelectInput(_gfx_ctx.display, win.src.window, eventMask);
	xSetWMProtocols(_gfx_ctx.display, win.src.window, &_gfx_ctx.wmDeleteWindow, 1);
	win.set_name(name);
	xMoveWindow(_gfx_ctx.display, win.src.window, win.x, win.y);

	if (flags.allow_dnd)
	{
		win.internal.flags.allow_dnd = true;
		char version = 5;
		xChangeProperty(_gfx_ctx.display, win.src.window, _gfx_ctx.xdndAware, 4, 32, 0, &version, 1);
	}

	win.src.bounds = win.bounds;
	xSetWindowBackground(_gfx_ctx.display, win.src.window, 0);
	xClearWindow(_gfx_ctx.display, win.src.window);
	xSetWindowBackgroundPixmap(_gfx_ctx.display, win.src.window, 0);
}

// public
fn void? todo_fix_createWindowPlatform(String name, GfxWindowFlags flags, GfxWindow* win) => todo_fix_createWindowPlatform_X11(name, flags, win);


const KeySym XK_F1 = 0xffbe; const KeySym XK_F2 = 0xffbf; const KeySym XK_F3 = 0xffc0; const KeySym XK_F4 = 0xffc1;
const KeySym XK_F5 = 0xffc2; const KeySym XK_F6 = 0xffc3; const KeySym XK_F7 = 0xffc4; const KeySym XK_F8 = 0xffc5;
const KeySym XK_F9 = 0xffc6; const KeySym XK_F10 = 0xffc7; const KeySym XK_F11 = 0xffc8; const KeySym XK_F12 = 0xffc9;
const KeySym XK_F13 = 0xffca; const KeySym XK_F14 = 0xffcb; const KeySym XK_F15 = 0xffcc; const KeySym XK_F16 = 0xffcd;
const KeySym XK_F17 = 0xffce; const KeySym XK_F18 = 0xffcf; const KeySym XK_F19 = 0xffd0; const KeySym XK_F20 = 0xffd1;
const KeySym XK_F21 = 0xffd2; const KeySym XK_F22 = 0xffd3; const KeySym XK_F23 = 0xffd4; const KeySym XK_F24 = 0xffd5;
const KeySym XK_F25 = 0xffd6;
const KeySym XK_SHIFT_L = 0xffe1; const KeySym XK_SHIFT_R = 0xffe2;
const KeySym XK_CONTROL_L = 0xffe3; const KeySym XK_CONTROL_R = 0xffe4;
const KeySym XK_ALT_L = 0xffe9; const KeySym XK_ALT_R = 0xffea;
const KeySym XK_SUPER_L = 0xffeb; const KeySym XK_SUPER_R = 0xffec;
const KeySym XK_CAPS_LOCK = 0xffe5; const KeySym XK_NUM_LOCK = 0xff7f; const KeySym XK_SCROLL_LOCK = 0xff14;
const KeySym XK_UP = 0xff52; const KeySym XK_DOWN = 0xff54; const KeySym XK_LEFT = 0xff51; const KeySym XK_RIGHT = 0xff53;
const KeySym XK_HOME = 0xff50; const KeySym XK_END = 0xff57;
const KeySym XK_PAGE_UP = 0xff55; const KeySym XK_PAGE_DOWN = 0xff56;
const KeySym XK_INSERT = 0xff63; const KeySym XK_MENU = 0xff67;
const KeySym XK_KP_ADD = 0xffab; const KeySym XK_KP_SUBTRACT = 0xffad;
const KeySym XK_KP_MULTIPLY = 0xffaa; const KeySym XK_KP_DIVIDE = 0xffaf; const KeySym XK_KP_EQUAL = 0xffbd;
const KeySym XK_KP_ENTER = 0xff8d; const KeySym XK_KP_DECIMAL = 0xffae;
const KeySym XK_KP_0 = 0xffb0; const KeySym XK_KP_1 = 0xffb1; const KeySym XK_KP_2 = 0xffb2; const KeySym XK_KP_3 = 0xffb3;
const KeySym XK_KP_4 = 0xffb4; const KeySym XK_KP_5 = 0xffb5; const KeySym XK_KP_6 = 0xffb6; const KeySym XK_KP_7 = 0xffb7;
const KeySym XK_KP_8 = 0xffb8; const KeySym XK_KP_9 = 0xffb9;
const KeySym XK_PRINT = 0xff61; const KeySym XK_PAUSE = 0xff13;


// public





struct TodoRenameAttribStack
{
	int* attribs;
	sz count;
	sz max;
}

