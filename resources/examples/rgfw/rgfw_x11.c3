module std::gfx @feat(X11) @link("X11", "Xext", "Xrandr", "GL");

import std::collections::list, std::math, libc;

// C3 port of the X11/GLX RGFW backend from /Users/lerno/Downloads/AGFW.h (RGFW).
// Mechanical, line-by-line style translation of the RGFW_X11 implementation.
// This is a standalone alternative to rgfw.c3 (the macOS build) - only ONE of
// the rgfw*.c3 files should be included for a given target, matching how the
// original C header only ever compiles one #ifdef backend at a time.
// Optional extensions (Xcursor, XInput2, Xext sync/shape) are loaded via dlopen,
// exactly as the original does when RGFW_NO_X11_CURSOR_PRELOAD etc. are not set.


extern fn char* strtok(char* str, char* delim);
extern fn char* strtokR(char* str, char* delim, char** saveptr) @cname("strtok_r");
extern fn CLong strtol(char* str, char** endptr, int base);
extern fn int clock_gettime(int clock_id, Timespec* ts);

struct Timespec { CLong tv_sec; CLong tv_nsec; }

// ---- Xlib core types (ABI-compatible aliases; GfxXDisplay/GfxXVisual/Screen stay opaque) ----
typedef Screen = void;
alias Colormap = CULong;
alias XCursor = CULong;
alias Pixmap = CULong;
alias GContext = void*;
alias Atom = CULong;
alias VisualID = CULong;
alias KeySym = CULong;
alias KeyCode = char;
alias Time = CULong;
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

struct XAspect { int x, y; }
struct X11WindowHints { CULong flags, functions, decorations, status; CLong input_mode; }
alias GlxGetProcAddressProc = fn GfxFn(ZString);

struct XVisualInfo
{
	GfxXVisual* visual;
	VisualID visualid;
	int screen;
	int depth;
	int c_class;
	CULong red_mask, green_mask, blue_mask;
	int colormap_size;
	int bits_per_rgb;
}

struct XSetWindowAttributes
{
	Pixmap background_pixmap;
	CULong background_pixel;
	Pixmap border_pixmap;
	CULong border_pixel;
	int bit_gravity;
	int win_gravity;
	int backing_store;
	CULong backing_planes;
	CULong backing_pixel;
	int save_under;
	CLong event_mask;
	CLong do_not_propagate_mask;
	int override_redirect;
	Colormap colormap;
	XCursor cursor;
}

struct XWindowAttributes
{
	int x, y;
	int width, height;
	int border_width;
	int depth;
	GfxXVisual* visual;
	GfxXWindow root;
	int c_class;
	int bit_gravity;
	int win_gravity;
	int backing_store;
	CULong backing_planes;
	CULong backing_pixel;
	int save_under;
	Colormap colormap;
	int map_installed;
	int map_state;
	CLong all_event_masks;
	CLong your_event_mask;
	CLong do_not_propagate_mask;
	int override_redirect;
	Screen* screen;
}

struct XClassHint { ZString res_name; ZString res_class; }

struct XWMHints
{
	CLong flags;
	int input;
	int initial_state;
	Pixmap icon_pixmap;
	GfxXWindow icon_window;
	int icon_x, icon_y;
	Pixmap icon_mask;
	XlibId window_group;
}

struct XSizeHints
{
	CLong flags;
	int x, y, width, height;
	int min_width, min_height;
	int max_width, max_height;
	int width_inc, height_inc;
	XAspect min_aspect;
	XAspect max_aspect;
	int base_width, base_height;
	int win_gravity;
}

struct XAnyEvent { int type; CULong serial; int send_event; GfxXDisplay* display; GfxXWindow window; }
struct XKeyEvent { int type; CULong serial; int send_event; GfxXDisplay* display; GfxXWindow window; GfxXWindow root; GfxXWindow subwindow; Time time; int x, y; int x_root, y_root; uint state; uint keycode; int same_screen; }
struct XButtonEvent { int type; CULong serial; int send_event; GfxXDisplay* display; GfxXWindow window; GfxXWindow root; GfxXWindow subwindow; Time time; int x, y; int x_root, y_root; uint state; uint button; int same_screen; }
struct XMotionEvent { int type; CULong serial; int send_event; GfxXDisplay* display; GfxXWindow window; GfxXWindow root; GfxXWindow subwindow; Time time; int x, y; int x_root, y_root; uint state; char is_hint; int same_screen; }
struct XCrossingEvent { int type; CULong serial; int send_event; GfxXDisplay* display; GfxXWindow window; GfxXWindow root; GfxXWindow subwindow; Time time; int x, y; int x_root, y_root; int mode; int detail; int same_screen; int focus; uint state; }
struct XFocusChangeEvent { int type; CULong serial; int send_event; GfxXDisplay* display; GfxXWindow window; int mode; int detail; }
struct XExposeEvent { int type; CULong serial; int send_event; GfxXDisplay* display; GfxXWindow window; int x, y, width, height; int count; }
struct XConfigureEvent { int type; CULong serial; int send_event; GfxXDisplay* display; GfxXWindow event; GfxXWindow window; int x, y, width, height; int border_width; GfxXWindow above; int override_redirect; }
struct XPropertyEvent { int type; CULong serial; int send_event; GfxXDisplay* display; GfxXWindow window; Atom atom; Time time; int state; }
struct XClientMessageDataL { CLong[5] l; }
struct XClientMessageEvent { int type; CULong serial; int send_event; GfxXDisplay* display; GfxXWindow window; Atom message_type; int format; XClientMessageDataL data; }
struct XSelectionRequestEvent { int type; CULong serial; int send_event; GfxXDisplay* display; GfxXWindow owner; GfxXWindow requestor; Atom selection; Atom target; Atom property; Time time; }
struct XSelectionEvent { int type; CULong serial; int send_event; GfxXDisplay* display; GfxXWindow requestor; Atom selection; Atom target; Atom property; Time time; }
struct XReparentEvent { int type; CULong serial; int send_event; GfxXDisplay* display; GfxXWindow event; GfxXWindow window; GfxXWindow parent; int x, y; int override_redirect; }
struct XMapEvent { int type; CULong serial; int send_event; GfxXDisplay* display; GfxXWindow event; GfxXWindow window; int override_redirect; }
struct XUnmapEvent { int type; CULong serial; int send_event; GfxXDisplay* display; GfxXWindow event; GfxXWindow window; int from_configure; }
struct XErrorEvent { int type; GfxXDisplay* display; CULong resourceid; CULong serial; char error_code; char request_code; char minor_code; }
struct XGenericEventCookie { int type; CULong serial; int send_event; GfxXDisplay* display; int extension; int evtype; uint cookie_data_len; void* data; }

union XEvent
{
	int type;
	XAnyEvent xany;
	XKeyEvent xkey;
	XButtonEvent xbutton;
	XMotionEvent xmotion;
	XCrossingEvent xcrossing;
	XFocusChangeEvent xfocus;
	XExposeEvent xexpose;
	XConfigureEvent xconfigure;
	XPropertyEvent xproperty;
	XClientMessageEvent xclient;
	XSelectionRequestEvent xselectionrequest;
	XSelectionEvent xselection;
	XReparentEvent xreparent;
	XMapEvent xmap;
	XUnmapEvent xunmap;
	XErrorEvent xerror;
	XGenericEventCookie xcookie;
	CLong[24] pad;
}

// ---- XRandr ----
struct XRRScreenResources { Time timestamp; Time configTimestamp; int ncrtc; RRCrtc* crtcs; int noutput; GfxRROutput* outputs; int nmode; XRRModeInfoNative* modes; }
struct XRRModeInfoNative { RRMode id; uint width; uint height; CULong dotClock; uint hSyncStart; uint hSyncEnd; uint hTotal; uint hSkew; uint vSyncStart; uint vSyncEnd; uint vTotal; char* name; uint nameLength; CULong modeFlags; }
struct XRRCrtcInfo { Time timestamp; int x, y; uint width, height; RRMode mode; int rotation; int noutput; GfxRROutput* outputs; int rotations; int npossible; GfxRROutput* possible; }
struct GfxXRROutputInfo { Time timestamp; RRCrtc crtc; ZString name; int nameLen; CULong mm_width; CULong mm_height; int connection; int subpixel_order; int ncrtc; RRCrtc* crtcs; int nclone; GfxRROutput* clones; int nmode; int npreferred; RRMode* modes; }
struct XRRCrtcGamma { int size; ushort* red; ushort* green; ushort* blue; }

// ---- Xcursor / XInput2 / Xext sync+shape (dlopen'd like the original) ----
struct XcursorImage { uint version; uint size; uint width, height; uint xhot, yhot; uint delay; uint* pixels; }
struct XIEventMask { int deviceid; int mask_len; char* mask; }
struct XIValuatorState { int mask_len; char* mask; double* values; }
struct XIRawEvent { int type; CULong serial; int send_event; GfxXDisplay* display; int extension; int evtype; Time time; int deviceid; int sourceid; int detail; int flags; XIValuatorState valuators; double* raw_values; }
struct XSyncValue { int hi; uint lo; }
alias XSyncCounter = XlibId;

// GLX
alias RGFW_glXCreateContextAttribsARBProc = fn GLXContext(GfxXDisplay*, GLXFBConfig, GLXContext, int, int*);
alias RGFW_glXQueryExtensionsStringProc = fn ZString(GfxXDisplay*, int);
alias RGFW_glXChooseVisualProc = fn XVisualInfo*(GfxXDisplay*, int, int*);
alias RGFW_glXCreateContextProc = fn GLXContext(GfxXDisplay*, XVisualInfo*, GLXContext, int);
alias RGFW_glXMakeCurrentProc = fn int(GfxXDisplay*, GLXDrawable, GLXContext);
alias RGFW_glXGetCurrentContextProc = fn GLXContext();
alias RGFW_glXSwapBuffersProc = fn void(GfxXDisplay*, GLXDrawable);
alias RGFW_glXSwapIntervalEXTProc = fn void(GfxXDisplay*, GLXDrawable, int);
alias RGFW_glXGetProcAddressProc = fn void*(ZString);
alias RGFW_glXGetVisualFromFBConfigProc = fn XVisualInfo*(GfxXDisplay*, GLXFBConfig);
alias RGFW_glXGetFBConfigAttribProc = fn int(GfxXDisplay*, GLXFBConfig, int, int*);
alias RGFW_glXGetProcAddressARBProc = fn void*(ZString);
alias RGFW_glXChooseFBConfigProc = fn GLXFBConfig*(GfxXDisplay*, int, int*, int*);
alias RGFW_glXDestroyContextProc = fn void(GfxXDisplay*, GLXContext);
alias RGFW_glXCreateWindowProc = fn GLXWindow(GfxXDisplay*, GLXFBConfig, GfxXWindow, int*);
alias RGFW_glXDestroyWindowProc = fn void(GfxXDisplay*, GLXWindow);
alias RGFW_glXSwapIntervalNativeProc = fn int(int);

RGFW_glXCreateContextAttribsARBProc rgfw_glXCreateContextAttribsARB;
RGFW_glXQueryExtensionsStringProc rgfw_glXQueryExtensionsString;
RGFW_glXChooseVisualProc rgfw_glXChooseVisual;
RGFW_glXCreateContextProc rgfw_glXCreateContext;
RGFW_glXMakeCurrentProc rgfw_glXMakeCurrent;
RGFW_glXGetCurrentContextProc rgfw_glXGetCurrentContext;
RGFW_glXSwapBuffersProc rgfw_glXSwapBuffers;
RGFW_glXSwapIntervalEXTProc rgfw_glXSwapIntervalEXT;
RGFW_glXGetVisualFromFBConfigProc rgfw_glXGetVisualFromFBConfig;
RGFW_glXGetFBConfigAttribProc rgfw_glXGetFBConfigAttrib;
RGFW_glXGetProcAddressARBProc rgfw_glXGetProcAddressARB;
RGFW_glXChooseFBConfigProc rgfw_glXChooseFBConfig;
RGFW_glXDestroyContextProc rgfw_glXDestroyContext;
RGFW_glXCreateWindowProc rgfw_glXCreateWindow;
RGFW_glXDestroyWindowProc rgfw_glXDestroyWindow;
RGFW_glXSwapIntervalNativeProc rgfw_glXSwapIntervalNative;

alias PFN_XcursorImageCreate = fn XcursorImage*(int, int);
alias PFN_XcursorImageDestroy = fn void(XcursorImage*);
alias PFN_XcursorImageLoadCursor = fn XCursor(GfxXDisplay*, XcursorImage*);
alias PFN_XISelectEvents = fn int(GfxXDisplay*, GfxXWindow, XIEventMask*, int);
alias PFN_XSyncIntToValue = fn void(XSyncValue*, int);
alias PFN_XSyncSetCounter = fn int(GfxXDisplay*, XSyncCounter, XSyncValue);
alias PFN_XSyncCreateCounter = fn XSyncCounter(GfxXDisplay*, XSyncValue);
alias PFN_XShapeCombineMask = fn void(GfxXDisplay*, GfxXWindow, int, int, int, Pixmap, int);
alias PFN_XShapeCombineRegion = fn void(GfxXDisplay*, GfxXWindow, int, int, int, Region, int);

PFN_XcursorImageCreate rgfw_xcursorImageCreateSRC;
PFN_XcursorImageDestroy rgfw_xcursorImageDestroySRC;
PFN_XcursorImageLoadCursor rgfw_xcursorImageLoadCursorSRC;
PFN_XISelectEvents rgfw_xiSelectEventsSRC;
PFN_XSyncIntToValue rgfw_xSyncIntToValueSRC;
PFN_XSyncSetCounter rgfw_xSyncSetCounterSRC;
PFN_XSyncCreateCounter rgfw_xSyncCreateCounterSRC;
PFN_XShapeCombineMask rgfw_xShapeCombineMaskSRC;
PFN_XShapeCombineRegion rgfw_xShapeCombineRegionSRC;
void* rgfw_x11CursorHandle;
void* rgfw_x11XiHandle;
void* rgfw_x11ExtHandle;

// ---- Xlib core functions (directly linked, like the original non-dynamic X11 build) ----


// XIMCallback: { GfxXPointer client_data; XIMProc callback; } from Xlib.h. XlibIC/XlibIM
// are both plain void* in this port, so one function-pointer alias covers the callback.
alias XImCallbackProc = fn void(void* im, char* clientData, char* callData);
struct XImCallback
{
	GfxXPointer client_data;
	XImCallbackProc callback;
}

// XIMStyles: { unsigned short count_styles; XIMStyle *supported_styles; } from Xlib.h.
struct XImStyles
{
	ushort count_styles;
	CULong* supported_styles;
}

// Signature required by XRegisterIMInstantiateCallback / XUnregisterIMInstantiateCallback.
alias XImInstantiateCallbackProc = fn void(GfxXDisplay* d, GfxXPointer clientData, GfxXPointer callData);

struct XkbStateRec
{
	char group;
	char locked_group;
	ushort base_group;
	ushort latched_group;
	char mods;
	char base_mods;
	char latched_mods;
	char locked_mods;
	char compat_state;
	char grab_mods;
	char compat_grab_mods;
	char lookup_mods;
	char compat_lookup_mods;
	ushort ptr_buttons;
}

const uint XKB_USE_CORE_KBD = 0x0100;
const char SHIFT_MASK = 1 << 0;
const char LOCK_MASK = 1 << 1;
const char MOD2_MASK = 1 << 4;
const char MOD3_MASK = 1 << 5;


macro sz ximaskLen(event) => (sz)((event >> 3) + 1);
macro void xiSetMask(char* mask, event) { mask[(sz)(event / 8)] |= (char)(1 << (event % 8)); }
macro bool ximaskIsSet(char* mask, event) => (mask[(sz)(event / 8)] & (char)(1 << (event % 8))) != 0;


struct RgfwGlContext { GLXContext ctx; GLXWindow window; }
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



struct RgfwWindowSrc
{
	inline IntRect bounds;
	GfxXWindow window;
	GfxXWindow parent;
	GContext gc;
	XlibIC ic;
	ulong flashEnd;
	long counter_value;
	XSyncCounter counter;
	RgfwGfxContext ctx;
	RgfwGfxContextType gfxType;
}


const int VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR = 1000004000;
const VkResult VK_ERROR_EXTENSION_NOT_PRESENT = -7;
struct VkXlibSurfaceCreateInfoKHR { int sType; void* pNext; uint flags; GfxXDisplay* dpy; GfxXWindow window; }
alias RgfwVkCreateXlibSurfaceKHRProc = fn VkResult(VkInstance instance, VkXlibSurfaceCreateInfoKHR* createInfo, void* allocator, VkSurfaceKHR* surface);
ZString[2] rgfw_vulkanExtensions = {"VK_KHR_surface", "VK_KHR_xlib_surface"};


extern fn VisualID xVisualIDFromVisual(GfxXVisual* visual) @cname("XVisualIDFromVisual");
alias RgfwVkGetPhysicalDeviceXlibPresentationSupportKHRProc = fn uint(VkPhysicalDevice, uint, GfxXDisplay*, VisualID);

// public
fn bool rgfw_getPhysicalDevicePresentationSupport_Vulkan(VkInstance instance, VkPhysicalDevice physicalDevice, uint queueFamilyIndex)
{
	GfxFn raw = rgfw_getInstanceProcAddress_Vulkan(instance, "vkGetPhysicalDeviceXlibPresentationSupportKHR");
	if (!raw) return false;
	GfxXVisual* visual = xDefaultVisual(_gfx_ctx.display, xDefaultScreen(_gfx_ctx.display));
	uint out = ((RgfwVkGetPhysicalDeviceXlibPresentationSupportKHRProc)raw)(physicalDevice, queueFamilyIndex, _gfx_ctx.display, xVisualIDFromVisual(visual));
	return out != 0;
}

alias WGPUInstance = void*;
alias WGPUSurface = void*;
alias WGPUSType = int;
const WGPUSType WGPU_STYPE_SURFACE_SOURCE_XLIB_WINDOW = 0x00000006;
struct WgpuChainedStruct { WgpuChainedStruct* next; WGPUSType sType; }
struct WgpuSurfaceDescriptor { WgpuChainedStruct* nextInChain; }
struct WgpuSurfaceSourceXlibWindow { WgpuChainedStruct chain; GfxXDisplay* display; GfxXWindow window; }
extern fn WGPUSurface wgpuInstanceCreateSurface(WGPUInstance instance, WgpuSurfaceDescriptor* descriptor) @cname("wgpuInstanceCreateSurface");


// IM-level destroy callback: fires if the input-method daemon disappears later,
// so we don't keep a dangling _gfx_ctx.im handle around.


// Instantiate callback: fires once an input method actually becomes available.
// Opens the IM, then validates it supports the exact style this library wants
// (XIMPreeditNothing | XIMStatusNothing) before keeping it, and registers an
// IM-level destroy callback so a later-vanishing IM daemon nulls _gfx_ctx.im.


// public
fn void? rgfw_xCreateWindow(XVisualInfo visual, String name, GfxWindowFlags flags, GfxWindow* win)
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
	xSaveContext(_gfx_ctx.display, win.src.window, _gfx_ctx.context, (GfxXPointer)win);
	win.src.gc = xCreateGC(_gfx_ctx.display, win.src.window, 0, null);

	if (_gfx_ctx.im != null)
	{
		XImCallback callback;
		callback.callback = &rgfw_x11IcCallback;
		callback.client_data = (GfxXPointer)win;

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
fn void? rgfw_createWindowPlatform(String name, GfxWindowFlags flags, GfxWindow* win) => rgfw_createWindowPlatform_X11(name, flags, win);


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





struct RgfwAttribStack
{
	int* attribs;
	sz count;
	sz max;
}

