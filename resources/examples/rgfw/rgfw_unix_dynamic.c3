module std::gfx @feat(X11_WAYLAND) @link("X11", "Xext", "Xrandr", "GL", "wayland-client", "wayland-cursor", "wayland-egl", "xkbcommon", "EGL");
import std::collections::list, std::math, libc;
// C3 port of RGFW's combined "X11 AND Wayland" + RGFW_DYNAMIC mode: a single
// Linux binary that supports both display servers, choosing at runtime
// (Wayland if available, falling back to X11).
// Standalone alternative to rgfw.c3 (macOS) / rgfw_x11.c3 / rgfw_wayland.c3 -
// only ONE rgfw*.c3 file should be included for a given target. This one is
// the "everything Linux" build: both backends are compiled in as *_X11/
// *_Wayland suffixed implementations, and the public rgfw_* functions are
// thin dispatchers that branch on rgfw_dynamicUsingWayland (set once at init
// after runtime display-server detection).


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

const RgfwGfxContextType RGFW_GFX_NATIVE_OPEN_GL = 1;
const RgfwGfxContextType RGFW_GFX_EGL = 2;
const RgfwGfxContextType RGFW_GFX_OWNED_BY_RGFW = 4;


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
	WlProxy* surface;
	WlProxy* xdgSurface;
	WlProxy* xdgToplevel;
	WlProxy* decoration;
	WlProxy* lockedPointer;
	WlProxy* icon;
	WlProxy* dataSource;
	WlProxy* customCursorSurface;
	GfxMonitorNode* activeMonitor;
	uint decorationMode;
	bool pendingActivated;
	bool activated;
	bool resizing;
	bool pendingMaximized;
	bool maximized;
	bool minimized;
	bool configured;
	bool usingCustomCursor;
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
	if (raw == null) return false;
	GfxXVisual* visual = xDefaultVisual(_gfx_ctx.display, xDefaultScreen(_gfx_ctx.display));
	uint out = ((RgfwVkGetPhysicalDeviceXlibPresentationSupportKHRProc)raw)(physicalDevice, queueFamilyIndex, _gfx_ctx.display, xVisualIDFromVisual(visual));
	return out != 0;
}

alias WGPUInstance = void*;
alias WGPUSurface = void*;
alias WGPUSType = int;
const WGPUSType WGPU_STYPE_SURFACE_SOURCE_XLIB_WINDOW = 0x00000006;
const WGPUSType WGPU_STYPE_SURFACE_SOURCE_WAYLAND_SURFACE = 0x00000007;
struct WgpuChainedStruct { WgpuChainedStruct* next; WGPUSType sType; }
struct WgpuSurfaceDescriptor { WgpuChainedStruct* nextInChain; }
struct WgpuSurfaceSourceXlibWindow { WgpuChainedStruct chain; GfxXDisplay* display; GfxXWindow window; }
struct WgpuSurfaceSourceWaylandSurface { WgpuChainedStruct chain; WlDisplay* display; WlProxy* surface; }
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
		hint.res_name = (name ?: "C3GFX").zstr_copy(tmem);
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
fn void rgfw_xHandleEvent()
{
	static float deltaX = 0.0f;
	static float deltaY = 0.0f;

	XEvent e;
	xNextEvent(_gfx_ctx.display, &e);

	if (e.type != 35 /* GenericEvent */) { deltaX = 0.0f; deltaY = 0.0f; }

	if (e.type == _gfx_ctx.xrandrEventBase + 0 /* RRNotify */) { rgfw_pollMonitors(); return; }

	if (e.type == 30 /* SelectionRequest */) { rgfw_xHandleClipboardSelection(&e); return; }

	if (e.type == 35 /* GenericEvent */)
	{
		xGetEventData(_gfx_ctx.display, &e.xcookie);
		if (e.xcookie.evtype == 17 /* XI_RawMotion */)
		{
			XIRawEvent* raw = (XIRawEvent*)e.xcookie.data;
			if (raw.valuators.mask_len != 0)
			{
				int index = 0;
				if (ximaskIsSet(raw.valuators.mask, 0)) { deltaX += (float)raw.raw_values[index]; index++; }
				if (ximaskIsSet(raw.valuators.mask, 1)) deltaY += (float)raw.raw_values[index];
				_gfx_ctx.vectorX = deltaX; _gfx_ctx.vectorY = deltaY;
				_raw_motion_callback(_gfx_ctx.root, _gfx_ctx.vectorX, _gfx_ctx.vectorY);
			}
		}
		xFreeEventData(_gfx_ctx.display, &e.xcookie);
		return;
	}

	GfxWindow* win = null;
	if (xFindContext(_gfx_ctx.display, e.xany.window, _gfx_ctx.context, (GfxXPointer*)&win) != 0) return;

	if (win.src.flashEnd != 0)
	{
		if (win.src.flashEnd <= rgfw_unix_getTimeNS() || win.has_focus()) rgfw_window_flash(win, CANCEL);
	}

	bool keyRepeat = false;
	if (e.type == 3 /* KeyRelease */ && xEventsQueued(_gfx_ctx.display, 1) != 0)
	{
		XEvent ne;
		xPeekEvent(_gfx_ctx.display, &ne);
		if (ne.type == 2 /* KeyPress */ && e.xkey.time == ne.xkey.time && e.xkey.keycode == ne.xkey.keycode)
		{
			xNextEvent(_gfx_ctx.display, &e);
			keyRepeat = true;
		}
	}

	switch (e.type)
	{
		case 2: // KeyPress
		{
			if (!(win.internal.enabledEvents & RGFW_KEY_PRESSED_FLAG)) return;
			RgfwKey value = (RgfwKey)rgfw_apiKeyToRGFW(e.xkey.keycode);

			XkbStateRec state;
			xkbGetState(_gfx_ctx.display, XKB_USE_CORE_KBD, &state);
			rgfw_keyUpdateKeyMods(win, state.locked_mods & LOCK_MASK != 0, state.locked_mods & MOD2_MASK != 0, state.locked_mods & MOD3_MASK != 0);

			if (win.src.ic != null && xFilterEvent(&e, 0) == 0)
			{
				char[100] buffer;
				int status;
				sz count = xutf8LookupString(win.src.ic, &e.xkey, (char*)&buffer, 99, null, &status);
				if (status == 2 /* XLookupChars */ || status == 4 /* XLookupBoth */)
				{
					buffer[count] = 0;
					sz index = 0;
					while (index < count) { rgfw_keyCharCallback(win, rgfw_decodeUTF8((ZString)&buffer, &index)); }
				}
			}
			else
			{
				GfxXWindow rootRet, childRet;
				int rootX, rootY, winX, winY;
				uint mask;
				xQueryPointer(_gfx_ctx.display, xDefaultRootWindow(_gfx_ctx.display), &rootRet, &childRet, &rootX, &rootY, &winX, &winY, &mask);
				KeySym sym = xkbKeycodeToKeysym(_gfx_ctx.display, (KeyCode)e.xkey.keycode, 0, (mask & SHIFT_MASK) ? 1 : 0);

				if ((mask & LOCK_MASK) && sym >= 0x0061 /* XK_a */ && sym <= 0x007a /* XK_z */)
				{
					sym = (mask & SHIFT_MASK) ? sym + 32 : sym - 32;
				}
				if ((KeySym)(char)sym != sym) sym = 0;

				rgfw_keyCharCallback(win, (uint)(char)sym);
			}

			rgfw_keyCallback(win, value, win.internal.mod, keyRepeat, true);
			break;
		}
		case 3: // KeyRelease
		{
			if (!(win.internal.enabledEvents & RGFW_KEY_RELEASED_FLAG)) return;
			RgfwKey value = (RgfwKey)rgfw_apiKeyToRGFW(e.xkey.keycode);

			XkbStateRec state;
			xkbGetState(_gfx_ctx.display, XKB_USE_CORE_KBD, &state);
			rgfw_keyUpdateKeyMods(win, state.locked_mods & LOCK_MASK != 0, state.locked_mods & MOD2_MASK != 0, state.locked_mods & MOD3_MASK != 0);

			rgfw_keyCallback(win, value, win.internal.mod, false, false);
			break;
		}
		case 4: // ButtonPress
		{
			bool scroll = e.xbutton.button >= 4 && e.xbutton.button <= 7;
			float scrollX = 0.0f;
			float scrollY = 0.0f;
			GfxMouseButton value;
			switch (e.xbutton.button)
			{
				case 1: value = LEFT;
				case 2: value = MIDDLE;
				case 3: value = RIGHT;
				case 4: scrollY = 1.0f;
				case 5: scrollY = -1.0f;
				case 6: scrollX = 1.0f;
				case 7: scrollX = -1.0f;
				default: value = (GfxMouseButton)(e.xbutton.button - 1 - 4);
			}
			if (scroll) { rgfw_mouseScrollCallback(win, scrollX, scrollY); break; }
			rgfw_mouseButtonCallback(win, value, true);
			break;
		}
		case 5: // ButtonRelease
		{
			if (e.xbutton.button >= 4 && e.xbutton.button <= 7) break;
			GfxMouseButton value;
			switch (e.xbutton.button)
			{
				case 1: value = LEFT;
				case 2: value = MIDDLE;
				case 3: value = RIGHT;
				default: value = (GfxMouseButton)(e.xbutton.button - 1 - 4);
			}
			rgfw_mouseButtonCallback(win, value, false);
			break;
		}
		case 6: _mouse_motion_callback(win, e.xmotion.x, e.xmotion.y); // MotionNotify
		case 12: rgfw_windowRefreshCallback(win, e.xexpose.x, e.xexpose.y, e.xexpose.width, e.xexpose.height); // Expose
		case 28: // PropertyNotify
			if (e.xproperty.state == 0 /* PropertyNewValue */)
			{
				bool handled = false;
				if (e.xproperty.atom == _gfx_ctx.wmState)
				{
					if (rgfw_window_isMinimized(win) && !(win.internal.flags.minimize))
					{
						rgfw_windowMinimizedCallback(win);
						handled = true;
					}
				}
				else if (e.xproperty.atom == _gfx_ctx.netWmState)
				{
					if (rgfw_window_isMaximized(win) && !(win.internal.flags.maximize))
					{
						rgfw_windowMaximizedCallback(win, win.x, win.y, win.width, win.height);
						handled = true;
					}
				}
				if (!handled) rgfw_window_checkMode(win);
			}
		case 19: case 18: // MapNotify, UnmapNotify
			rgfw_window_checkMode(win);
			break;
		case 33: // ClientMessage
		{
			if ((CULong)e.xclient.data.l[0] == _gfx_ctx.wmDeleteWindow) { _window_close_callback(win); break; }

			if (!(win.internal.flags.allow_dnd) || _gfx_ctx.x11Version > 5 /* RGFW_XDND_VERSION */) return;

			int dragX = 0;
			int dragY = 0;

			if (e.xclient.message_type == _gfx_ctx.xdndEnter)
			{
				CULong count;
				Atom* formats;
				Atom[3] realFormats;
				int list = (int)(e.xclient.data.l[1] & 1);

				_gfx_ctx.x11Source = (GfxXWindow)e.xclient.data.l[0];
				_gfx_ctx.x11Version = (int)(e.xclient.data.l[1] >> 24);
				_gfx_ctx.x11Format = 0;
				if (list)
				{
					Atom actualType;
					int actualFormat;
					CULong bytesAfter;
					xGetWindowProperty(_gfx_ctx.display, _gfx_ctx.x11Source, _gfx_ctx.xdndTypeList, 0, ~0, 0, 4,
						&actualType, &actualFormat, &count, &bytesAfter, (char**)&formats);
				}
				else
				{
					count = 0;
					for (sz i = 2; i < 2 + 3; i++)
					{
						if (e.xclient.data.l[i] != 0)
						{
							realFormats[count] = (CULong)e.xclient.data.l[i];
							count++;
						}
					}
					formats = (Atom*)&realFormats;
				}

				for (sz i = 0; i < (sz)count; i++)
				{
					if (formats[i] == _gfx_ctx.xtextUriList) { _gfx_ctx.x11TransferType = RGFW_DATA_FILE; }
					else if (formats[i] == _gfx_ctx.xtextPlain) { _gfx_ctx.x11TransferType = RGFW_DATA_TEXT; }
					else { continue; }

					_gfx_ctx.x11Format = (int)formats[i];
					break;
				}

				if (list != 0 && formats != null) xFree(formats);

				rgfw_dataDragCallback(win, _gfx_ctx.x11TransferType, RGFW_DND_ACTION_ENTER, dragX, dragY);
			}
			else if (e.xclient.message_type == _gfx_ctx.xdndPosition)
			{
				int xabs = (int)((e.xclient.data.l[2] >> 16) & 0xffff);
				int yabs = (int)(e.xclient.data.l[2] & 0xffff);
				GfxXWindow dummy;
				int xpos, ypos;

				xTranslateCoordinates(_gfx_ctx.display, xDefaultRootWindow(_gfx_ctx.display), win.src.window,
					xabs, yabs, &xpos, &ypos, &dummy);

				dragX = xpos;
				dragY = ypos;

				_mouse_motion_callback(win, xpos, ypos);

				XEvent reply;
				reply.type = 33; // ClientMessage
				reply.xclient.window = _gfx_ctx.x11Source;
				reply.xclient.message_type = _gfx_ctx.xdndStatus;
				reply.xclient.format = 32;
				reply.xclient.data.l[0] = (CLong)win.src.window;
				reply.xclient.data.l[2] = 0;
				reply.xclient.data.l[3] = 0;

				if (_gfx_ctx.x11Format != 0)
				{
					reply.xclient.data.l[1] = 1;
					if (_gfx_ctx.x11Version >= 2) reply.xclient.data.l[4] = (CLong)_gfx_ctx.xdndActionCopy;
				}

				xSendEvent(_gfx_ctx.display, _gfx_ctx.x11Source, 0, 0, &reply);
				xFlush(_gfx_ctx.display);

				rgfw_dataDragCallback(win, _gfx_ctx.x11TransferType, RGFW_DND_ACTION_MOVE, dragX, dragY);
			}
			else if (e.xclient.message_type == _gfx_ctx.xdndLeave)
			{
				rgfw_dataDragCallback(win, _gfx_ctx.x11TransferType, RGFW_DND_ACTION_EXIT, dragX, dragY);
			}
			else if (e.xclient.message_type == _gfx_ctx.xdndDrop)
			{
				if (_gfx_ctx.x11Format != 0)
				{
					Time time = _gfx_ctx.x11Version >= 1 ? (Time)e.xclient.data.l[2] : 0; // CurrentTime
					xConvertSelection(_gfx_ctx.display, _gfx_ctx.xdndSelection, (Atom)_gfx_ctx.x11Format,
						_gfx_ctx.xdndSelection, win.src.window, time);
				}
				else if (_gfx_ctx.x11Version >= 2)
				{
					XEvent reply;
					reply.type = 33; // ClientMessage
					reply.xclient.window = _gfx_ctx.x11Source;
					reply.xclient.message_type = _gfx_ctx.xdndFinished;
					reply.xclient.format = 32;
					reply.xclient.data.l[0] = (CLong)win.src.window;
					reply.xclient.data.l[1] = 0;
					reply.xclient.data.l[2] = 0;

					xSendEvent(_gfx_ctx.display, _gfx_ctx.x11Source, 0, 0, &reply);
					xFlush(_gfx_ctx.display);
				}
			}
			break;
		}
		case 31: // SelectionNotify
		{
			if (!(win.internal.enabledEvents & RGFW_DATA_DROP_FLAG) || e.xselection.property != _gfx_ctx.xdndSelection || !(win.internal.flags.allow_dnd)) return;

			ZString data;
			CULong result;
			Atom actualType;
			int actualFormat;
			CULong bytesAfter;

			xGetWindowProperty(_gfx_ctx.display, e.xselection.requestor, e.xselection.property, 0, ~0, 0, e.xselection.target,
				&actualType, &actualFormat, &result, &bytesAfter, (char**)&data);

			if (result != 0)
			{
				rgfw_unix_parseURI(win, data);
				if (data != null) xFree(data);
			}

			if (_gfx_ctx.x11Version >= 2)
			{
				XEvent reply;
				reply.type = 33; // ClientMessage
				reply.xclient.window = _gfx_ctx.x11Source;
				reply.xclient.message_type = _gfx_ctx.xdndFinished;
				reply.xclient.format = 32;
				reply.xclient.data.l[0] = (CLong)win.src.window;
				reply.xclient.data.l[1] = (CLong)result;
				reply.xclient.data.l[2] = (CLong)_gfx_ctx.xdndActionCopy;
				xSendEvent(_gfx_ctx.display, _gfx_ctx.x11Source, 0, 0, &reply);
				xFlush(_gfx_ctx.display);
			}
			break;
		}
		case 22: // ConfigureNotify
		{
			if (e.xconfigure.width != win.src.width || e.xconfigure.height != win.src.height)
			{
				rgfw_window_checkMode(win);
				win.src.size = { e.xconfigure.width, e.xconfigure.height };
				_window_resized_callback(win, e.xconfigure.width, e.xconfigure.height);
			}
			int configureX = e.xconfigure.x;
			int configureY = e.xconfigure.y;

			// if the event came from the server and we're not a direct child of the root window then
			// we're using local coords which need to be translated into screen coords
			GfxXWindow root = xDefaultRootWindow(_gfx_ctx.display);
			if (e.xany.send_event == 0 && win.src.parent != root)
			{
				GfxXWindow dummy;
				xTranslateCoordinates(_gfx_ctx.display, win.src.parent, root, configureX, configureY, &configureX, &configureY, &dummy);
			}

			if (configureX != win.src.x || configureY != win.src.y)
			{
				win.src.x = configureX;
				win.src.y = configureY;
				_window_moved_callback(win, configureX, configureY);
			}
			return;
		}
		case 9: // FocusIn
			if (win.src.ic != null) xSetICFocus(win.src.ic);
			rgfw_windowFocusCallback(win, true);
		case 10: // FocusOut
			if (win.src.ic != null) xUnsetICFocus(win.src.ic);
			rgfw_windowFocusCallback(win, false);
		case 7: _mouse_notify_callback(win, e.xcrossing.x, e.xcrossing.y, true); // EnterNotify
		case 8: _mouse_notify_callback(win, ...win.internal.last_mouse, false); // LeaveNotify
		case 21: win.src.parent = e.xreparent.parent; // ReparentNotify
		default: break;
	}

	xFlush(_gfx_ctx.display);
}


// public
fn void rgfw_window_swapInterval_OpenGL_X11(GfxWindow* win, int swapInterval)
{
	if (rgfw_glXSwapIntervalEXT != null) { rgfw_glXSwapIntervalEXT(_gfx_ctx.display, win.src.ctx.native.window, swapInterval); }
	else if (rgfw_glXSwapIntervalNative != null) { rgfw_glXSwapIntervalNative(swapInterval); }
	else { rgfw_debugCallback(RGFW_TYPE_WARNING, RGFW_ERR_OPEN_GL_CONTEXT, "Failed to load swap interval function"); }
}


struct RgfwAttribStack
{
	int* attribs;
	sz count;
	sz max;
}


// ==================== Wayland-specific declarations & backend ====================
// ---- POSIX / libc bits used by the Wayland backend ----
extern fn void* mmap(void* addr, sz length, int prot, int flags, int fd, sz offset);

extern fn int munmap(void* addr, sz length);

extern fn int shmOpen(ZString name, int oflag, int mode) @cname("shm_open");

extern fn int shmUnlink(ZString name) @cname("shm_unlink");

extern fn int ftruncate(int fd, sz length);

extern fn int close(int fd);

extern fn int pipe(int[2]* fds);

extern fn sz read(int fd, void* buf, sz count);

extern fn sz write(int fd, void* buf, sz count);

extern fn int poll(Pollfd* fds, CUInt nfds, int timeout);

extern fn int fcntl(int fd, int cmd, ...) @cname("fcntl");
const int F_GETFD = 1;
const int F_GETFL = 3;

extern fn ZString getenv(ZString name);

struct Pollfd { int fd; short events; short revents; }

const int PROT_READ = 0x1;

const int PROT_WRITE = 0x2;

const int MAP_SHARED = 0x01;

const void* MAP_FAILED = (void*)(uptr)(-1);

const int O_RDWR = 0x0002;

const int O_CREAT = 0x0200;

const int O_EXCL = 0x0800;

const int POLLIN = 0x0001;

// ---- Wayland client core (real libwayland-client ABI) ----

extern fn int* rgfwErrnoLocation() @cname("__errno_location");
const int RGFW_EAGAIN = 11;


const uint WL_MARSHAL_FLAG_DESTROY = 1;


// ---- listener vtables (field order matches each protocol's `_listener` struct) ----


// private
fn void rgfwDoNothing() {}


const int VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR = 1000006000;

struct VkWaylandSurfaceCreateInfoKHR { int sType; void* pNext; uint flags; WlDisplay* display; WlProxy* surface; }

alias RgfwVkCreateWaylandSurfaceKHRProc = fn VkResult(VkInstance instance, VkWaylandSurfaceCreateInfoKHR* createInfo, void* allocator, VkSurfaceKHR* surface);


// private


WlPointerListener rgfw_wlPointerListener = {
	.enter = &rgfw_wlPointerEnter, .leave = &rgfw_wlPointerLeave, .motion = &rgfw_wlPointerMotion,
	.button = &rgfw_wlPointerButton, .axis = &rgfw_wlPointerAxis
};

WlKeyboardListener rgfw_wlKeyboardListener = {
	.keymap = &rgfw_wlKeyboardKeymap, .enter = &rgfw_wlKeyboardEnter, .leave = &rgfw_wlKeyboardLeave,
	.key = &rgfw_wlKeyboardKey, .modifiers = &rgfw_wlKeyboardModifiers, .repeatInfo = &rgfw_wlKeyboardRepeatInfo
};


WlSeatListener rgfw_wlSeatListener = { .capabilities = &rgfw_wlSeatCapabilities, .name = &rgfw_wlSeatName };

WlShmListener rgfw_wlShmListener = { .format = &rgfw_wlShmFormatHandler };


WlOutputListener rgfw_wlOutputListener = {
	.geometry = &rgfw_wlOutputSetGeometry, .mode = &rgfw_wlOutputHandleMode, .done = &rgfw_wlOutputHandleDone,
	.scale = &rgfw_wlOutputSetScale, .name = &rgfw_wlOutputSetName, .description = &rgfw_wlOutputDescription
};

ZxdgOutputV1Listener rgfw_xdgOutputListener = {
	.name = &rgfw_xdgOutputNothingStr, .done = &rgfw_xdgOutputNothing, .description = &rgfw_xdgOutputNothingStr,
	.logicalPosition = &rgfw_xdgOutputLogicalPos, .logicalSize = &rgfw_xdgOutputLogicalSize
};


// private
fn void rgfw_wlDataDeviceSelection(void* data, WlProxy* device, WlProxy* offer)
{
	if (offer == null) return;

	int[2] pfds;
	pipe(&pfds);
	wlDataOfferReceive(offer, "text/plain", pfds[1]);
	close(pfds[1]);
	wlDisplayRoundtrip(_gfx_ctx.wlDisplay);

	char[1024] buf;
	sz n = read(pfds[0], &buf, 1024);
	if (n <= 0) { close(pfds[0]); wlDataOfferDestroy(offer); return; }

	sz length = (sz)n;
	if (buf[n - 1] != 0) length += 1;

	if (_gfx_ctx.unixClipboard != null) free(_gfx_ctx.unixClipboard);
	_gfx_ctx.unixClipboard = mem::new_with_padding(GfxDataTransfer, n);
	char* dataPtr = (char*)_gfx_ctx.unixClipboard + GfxDataTransfer::size - 1;
	mem::copy(dataPtr, &buf, (sz)n);
	dataPtr[length - 1] = 0;
	_gfx_ctx.unixClipboard.data = dataPtr;
	_gfx_ctx.unixClipboard.type = RGFW_DATA_TEXT;
	_gfx_ctx.unixClipboard.length = length;

	close(pfds[0]);
	wlDataOfferDestroy(offer);
}


WlRegistryListener rgfw_registryListener = { .global = &rgfw_wlGlobalRegistryHandler, .globalRemove = &rgfw_wlGlobalRegistryRemove };


// public
fn void? rgfw_createWindowPlatform_Wayland(String name, GfxWindowFlags flags, GfxWindow* win) => @pool()
{
	rgfw_debugCallback(RGFW_TYPE_WARNING, RGFW_WARNING_WAYLAND, "RGFW Wayland support is experimental");

	win.src.surface = wlCompositorCreateSurface(_gfx_ctx.compositor);
	wlSurfaceAddListener(win.src.surface, &rgfw_windowSurfaceListener, win);
	wlSurfaceSetUserData(win.src.surface, win);

	win.src.customCursorSurface = wlCompositorCreateSurface(_gfx_ctx.compositor);

	win.src.xdgSurface = xdgWmBaseGetXdgSurface(_gfx_ctx.xdgWmBase, win.src.surface);
	xdgSurfaceAddListener(win.src.xdgSurface, &rgfw_xdgSurfaceListener, win);
	xdgWmBaseSetUserData(_gfx_ctx.xdgWmBase, win);

	win.src.xdgToplevel = xdgSurfaceGetToplevel(win.src.xdgSurface);
	xdgToplevelSetAppId(win.src.xdgToplevel, name.zstr_copy(tmem));
	xdgSurfaceSetWindowGeometry(win.src.xdgSurface, 0, 0, win.width, win.height);

	if (!(win.internal.flags.transparent)) rgfw_wlSetOpaque(win);

	xdgToplevelAddListener(win.src.xdgToplevel, &rgfw_xdgToplevelListener, win);

	if (_gfx_ctx.decorationManager)
	{
		uint decorationMode = ZXDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE;
		win.src.decoration = zxdgDecorationManagerV1GetToplevelDecoration(_gfx_ctx.decorationManager, win.src.xdgToplevel);
		zxdgToplevelDecorationV1AddListener(win.src.decoration, &rgfw_xdgDecorationListener, win);
		if (flags.no_border) decorationMode = ZXDG_TOPLEVEL_DECORATION_V1_MODE_CLIENT_SIDE;
		zxdgToplevelDecorationV1SetMode(win.src.decoration, decorationMode);
	}

	if (_gfx_ctx.iconManager) xdgToplevelIconManagerV1SetIcon(_gfx_ctx.iconManager, win.src.xdgToplevel, null);

	wlSurfaceCommit(win.src.surface);

	while (!win.src.configured) wlDisplayDispatch(_gfx_ctx.wlDisplay);

}


WlDataSourceListener rgfw_dataSourceListener = {
	.target = &rgfw_wlDataSourceNothingStr, .action = &rgfw_wlDataSourceNothingU32,
	.dndDropPerformed = &rgfw_wlDataSourceNothing, .dndFinished = &rgfw_wlDataSourceNothing,
	.send = &rgfw_wlDataSourceSend, .cancelled = &rgfw_wlDataSourceCancelled
};


const EglInt EGL_NONE = 0x3038;

const EglInt EGL_RED_SIZE = 0x3024;

const EglInt EGL_GREEN_SIZE = 0x3023;

const EglInt EGL_BLUE_SIZE = 0x3022;

const EglInt EGL_ALPHA_SIZE = 0x3021;

const EglInt EGL_DEPTH_SIZE = 0x3025;

const EglInt EGL_STENCIL_SIZE = 0x3026;

const EglInt EGL_RENDERABLE_TYPE = 0x3040;

const EglInt EGL_SURFACE_TYPE = 0x3033;

const EglInt EGL_WINDOW_BIT = 0x0004;

const EglInt EGL_OPENGL_BIT = 0x0008;

const EglInt EGL_OPENGL_ES2_BIT = 0x0004;

const EglInt EGL_OPENGL_API = 0x30a2;

const EglInt EGL_OPENGL_ES_API = 0x30a0;

const EglInt EGL_CONTEXT_CLIENT_VERSION = 0x3098;

// public
fn bool rgfw_extensionSupportedPlatform_EGL(ZString extension, sz len)
{
	if (rgfw_eglQueryString == null || _gfx_ctx.egl_display == null) return false;
	ZString extensions = rgfw_eglQueryString((EglDisplay)_gfx_ctx.egl_display, 0x3055 /* EGL_EXTENSIONS */);
	return extensions && rgfw_extensionSupportedStr(extensions, extension, len);
}


// public
fn void rgfw_window_swapInterval_OpenGL_Wayland(GfxWindow* win, int swapInterval) { rgfw_window_swapInterval_EGL(win, swapInterval); }


module std::gfx @feat(X11_WAYLAND) @link("wayland-client", "wayland-cursor", "wayland-egl", "xkbcommon", "EGL");
import std::math;
// ==================== RGFW_DYNAMIC runtime dispatch layer ====================
// Combined X11+Wayland runtime dispatch: every entry point below branches on
// rgfw_dynamicUsingWayland (set once in rgfw_initPlatform) instead of a vtable.

bool rgfw_dynamicUsingWayland = false;

// public
fn bool rgfw_usingWayland() => rgfw_dynamicUsingWayland;


// public
fn void? rgfw_createWindowPlatform(String name, GfxWindowFlags flags, GfxWindow* win)
{
	if (rgfw_dynamicUsingWayland)
	{
		return rgfw_createWindowPlatform_Wayland(name, flags, win);
	}
	return rgfw_createWindowPlatform_X11(name, flags, win);
}


// public
fn void rgfw_window_swapInterval_OpenGL(GfxWindow* win, int swapInterval)
{
	if (rgfw_dynamicUsingWayland)
	{
		return rgfw_window_swapInterval_OpenGL_Wayland(win, swapInterval);
	}
	rgfw_window_swapInterval_OpenGL_X11(win, swapInterval);
}


// public
fn void rgfw_stopCheckEvents()
{
	_gfx_ctx.eventWait_forceStop[2] = 1;
	while (true)
	{
		char byte = 0;
		sz result = write(_gfx_ctx.eventWait_forceStop[1], &byte, 1);
		if (result == 1 || result == -1) break;
	}
}

// public
fn void rgfw_waitForEvent(int waitMS)
{
	if (waitMS == 0) return;

	if (_gfx_ctx.eventWait_forceStop[0] == 0 || _gfx_ctx.eventWait_forceStop[1] == 0)
	{
		if (pipe((int[2]*)&_gfx_ctx.eventWait_forceStop) != -1)
		{
			fcntl(_gfx_ctx.eventWait_forceStop[0], F_GETFL, 0);
			fcntl(_gfx_ctx.eventWait_forceStop[0], F_GETFD, 0);
			fcntl(_gfx_ctx.eventWait_forceStop[1], F_GETFL, 0);
			fcntl(_gfx_ctx.eventWait_forceStop[1], F_GETFD, 0);
		}
	}

	Pollfd[2] fds;
	fds[0].fd = 0;
	fds[0].events = (short)POLLIN;
	fds[1].fd = _gfx_ctx.eventWait_forceStop[0];
	fds[1].events = (short)POLLIN;

	if (rgfw_usingWayland())
	{
		fds[0].fd = wlDisplayGetFd(_gfx_ctx.wlDisplay);

		// empty the queue
		while (wlDisplayPrepareRead(_gfx_ctx.wlDisplay) != 0)
		{
			// error occured when dispatching the queue
			if (wlDisplayDispatchPending(_gfx_ctx.wlDisplay) == -1) return;
		}

		// send any pending requests to the compositor
		while (wlDisplayFlush(_gfx_ctx.wlDisplay) == -1)
		{
			// queue is full dispatch them
			if (*rgfwErrnoLocation() == RGFW_EAGAIN)
			{
				if (wlDisplayDispatchPending(_gfx_ctx.wlDisplay) == -1) return;
			}
			else { return; }
		}
	}
	else
	{
		fds[0].fd = xConnectionNumber(_gfx_ctx.display);
	}

	ulong start = rgfw_unix_getTimeNS();
	if (rgfw_usingWayland())
	{
		while (wlDisplayDispatchPending(_gfx_ctx.wlDisplay) == 0)
		{
			if (poll(&fds[0], 1, waitMS) <= 0)
			{
				wlDisplayCancelRead(_gfx_ctx.wlDisplay);
				break;
			}
			else
			{
				if (wlDisplayReadEvents(_gfx_ctx.wlDisplay) == -1) return;
			}

			if (waitMS != RGFW_EVENT_WAIT_NEXT)
			{
				waitMS -= (int)((rgfw_unix_getTimeNS() - start) / 1000000);
			}
		}

		// queue contains events from read, dispatch them
		if (wlDisplayDispatchPending(_gfx_ctx.wlDisplay) == -1) return;
	}
	else
	{
		while (xPending(_gfx_ctx.display) == 0)
		{
			if (poll(&fds[0], 1, waitMS) <= 0) break;

			if (waitMS != RGFW_EVENT_WAIT_NEXT)
			{
				waitMS -= (int)((rgfw_unix_getTimeNS() - start) / 1000000);
			}
		}
	}

	// drain any data in the stop request
	if (_gfx_ctx.eventWait_forceStop[2])
	{
		char[64] data;
		read(_gfx_ctx.eventWait_forceStop[0], &data, 64);

		_gfx_ctx.eventWait_forceStop[2] = 0;
	}
}
