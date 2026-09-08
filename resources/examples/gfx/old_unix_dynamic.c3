module std::gfx @feat(X11_WAYLAND) @link("X11", "Xext", "Xrandr", "GL", "wayland-client", "wayland-cursor", "wayland-egl", "xkbcommon", "EGL");
import std::collections::list, std::math, libc;

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
alias TODO_RENAMEglXCreateContextAttribsARBProc = fn GLXContext(GfxXDisplay*, GLXFBConfig, GLXContext, int, int*);
alias TODO_RENAMEglXQueryExtensionsStringProc = fn ZString(GfxXDisplay*, int);
alias TODO_RENAMEglXChooseVisualProc = fn XVisualInfo*(GfxXDisplay*, int, int*);
alias TODO_RENAMEglXCreateContextProc = fn GLXContext(GfxXDisplay*, XVisualInfo*, GLXContext, int);
alias TODO_RENAMEglXMakeCurrentProc = fn int(GfxXDisplay*, GLXDrawable, GLXContext);
alias TODO_RENAMEglXGetCurrentContextProc = fn GLXContext();
alias TODO_RENAMEglXSwapBuffersProc = fn void(GfxXDisplay*, GLXDrawable);
alias TODO_RENAMEglXSwapIntervalEXTProc = fn void(GfxXDisplay*, GLXDrawable, int);
alias TODO_RENAMEglXGetProcAddressProc = fn void*(ZString);
alias TODO_RENAMEglXGetVisualFromFBConfigProc = fn XVisualInfo*(GfxXDisplay*, GLXFBConfig);
alias TODO_RENAMEglXGetFBConfigAttribProc = fn int(GfxXDisplay*, GLXFBConfig, int, int*);
alias TODO_RENAMEglXGetProcAddressARBProc = fn void*(ZString);
alias TODO_RENAMEglXChooseFBConfigProc = fn GLXFBConfig*(GfxXDisplay*, int, int*, int*);
alias TODO_RENAMEglXDestroyContextProc = fn void(GfxXDisplay*, GLXContext);
alias TODO_RENAMEglXCreateWindowProc = fn GLXWindow(GfxXDisplay*, GLXFBConfig, GfxXWindow, int*);
alias TODO_RENAMEglXDestroyWindowProc = fn void(GfxXDisplay*, GLXWindow);
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

alias PFN_XcursorImageCreate = fn XcursorImage*(int, int);
alias PFN_XcursorImageDestroy = fn void(XcursorImage*);
alias PFN_XcursorImageLoadCursor = fn XCursor(GfxXDisplay*, XcursorImage*);
alias PFN_XISelectEvents = fn int(GfxXDisplay*, GfxXWindow, XIEventMask*, int);
alias PFN_XSyncIntToValue = fn void(XSyncValue*, int);
alias PFN_XSyncSetCounter = fn int(GfxXDisplay*, XSyncCounter, XSyncValue);
alias PFN_XSyncCreateCounter = fn XSyncCounter(GfxXDisplay*, XSyncValue);
alias PFN_XShapeCombineMask = fn void(GfxXDisplay*, GfxXWindow, int, int, int, Pixmap, int);
alias PFN_XShapeCombineRegion = fn void(GfxXDisplay*, GfxXWindow, int, int, int, Region, int);

PFN_XcursorImageCreate todo_fix_xcursorImageCreateSRC;
PFN_XcursorImageDestroy todo_fix_xcursorImageDestroySRC;
PFN_XcursorImageLoadCursor todo_fix_xcursorImageLoadCursorSRC;
PFN_XISelectEvents todo_fix_xiSelectEventsSRC;
PFN_XSyncIntToValue todo_fix_xSyncIntToValueSRC;
PFN_XSyncSetCounter todo_fix_xSyncSetCounterSRC;
PFN_XSyncCreateCounter todo_fix_xSyncCreateCounterSRC;
PFN_XShapeCombineMask todo_fix_xShapeCombineMaskSRC;
PFN_XShapeCombineRegion todo_fix_xShapeCombineRegionSRC;
void* todo_fix_x11CursorHandle;
void* todo_fix_x11XiHandle;
void* todo_fix_x11ExtHandle;

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
	GfxXWindow window;
	GfxXWindow parent;
	GContext gc;
	XlibIC ic;
	ulong flashEnd;
	long counter_value;
	XSyncCounter counter;
	TodoRenameGfxContext ctx;
	TodoRenameGfxContextType gfxType;
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
alias TodoRenameVkCreateXlibSurfaceKHRProc = fn VkResult(VkInstance instance, VkXlibSurfaceCreateInfoKHR* createInfo, void* allocator, VkSurfaceKHR* surface);
ZString[2] todo_fix_vulkanExtensions = {"VK_KHR_surface", "VK_KHR_xlib_surface"};


extern fn VisualID xVisualIDFromVisual(GfxXVisual* visual) @cname("XVisualIDFromVisual");
alias TodoRenameVkGetPhysicalDeviceXlibPresentationSupportKHRProc = fn uint(VkPhysicalDevice, uint, GfxXDisplay*, VisualID);

// public
fn bool todo_fix_getPhysicalDevicePresentationSupport_Vulkan(VkInstance instance, VkPhysicalDevice physicalDevice, uint queueFamilyIndex)
{
	GfxFn raw = todo_fix_getInstanceProcAddress_Vulkan(instance, "vkGetPhysicalDeviceXlibPresentationSupportKHR");
	if (raw == null) return false;
	GfxXVisual* visual = xDefaultVisual(_gfx_ctx.display, xDefaultScreen(_gfx_ctx.display));
	uint out = ((TodoRenameVkGetPhysicalDeviceXlibPresentationSupportKHRProc)raw)(physicalDevice, queueFamilyIndex, _gfx_ctx.display, xVisualIDFromVisual(visual));
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
fn void? todo_fix_xCreateWindow(XVisualInfo visual, String name, GfxWindowFlags flags, GfxWindow* win)
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
		callback.callback = &todo_fix_x11IcCallback;
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

struct TodoRenameAttribStack
{
	int* attribs;
	sz count;
	sz max;
}


// ==================== Wayland-specific declarations & backend ====================
// ---- POSIX / libc bits used by the Wayland backend ----

extern fn int shmOpen(ZString name, int oflag, int mode) @cname("shm_open");

extern fn int shmUnlink(ZString name) @cname("shm_unlink");

extern fn int ftruncate(int fd, sz length);


extern fn int pipe(int[2]* fds);

extern fn int poll(Pollfd* fds, CUInt nfds, int timeout);

extern fn int fcntl(int fd, int cmd, ...) @cname("fcntl");
const int F_GETFD = 1;
const int F_GETFL = 3;


struct Pollfd { int fd; short events; short revents; }


const int O_RDWR = 0x0002;

const int O_CREAT = 0x0040;
const int O_EXCL = 0x0080;

const int POLLIN = 0x0001;

// ---- Wayland client core (real libwayland-client ABI) ----

extern fn int* rgfwErrnoLocation() @cname("__errno_location");
const int TODO_RENAMEEAGAIN = 11;


const uint WL_MARSHAL_FLAG_DESTROY = 1;


// ---- listener vtables (field order matches each protocol's `_listener` struct) ----


// private
fn void rgfwDoNothing() {}


const int VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR = 1000006000;

struct VkWaylandSurfaceCreateInfoKHR { int sType; void* pNext; uint flags; WlDisplay* display; WlProxy* surface; }

alias TodoRenameVkCreateWaylandSurfaceKHRProc = fn VkResult(VkInstance instance, VkWaylandSurfaceCreateInfoKHR* createInfo, void* allocator, VkSurfaceKHR* surface);


// private


WlPointerListener todo_fix_wlPointerListener = {
	.enter = &todo_fix_wlPointerEnter, .leave = &todo_fix_wlPointerLeave, .motion = &todo_fix_wlPointerMotion,
	.button = &todo_fix_wlPointerButton, .axis = &todo_fix_wlPointerAxis
};

WlKeyboardListener todo_fix_wlKeyboardListener = {
	.keymap = &todo_fix_wlKeyboardKeymap, .enter = &todo_fix_wlKeyboardEnter, .leave = &todo_fix_wlKeyboardLeave,
	.key = &todo_fix_wlKeyboardKey, .modifiers = &todo_fix_wlKeyboardModifiers, .repeatInfo = &todo_fix_wlKeyboardRepeatInfo
};


WlSeatListener todo_fix_wlSeatListener = { .capabilities = &todo_fix_wlSeatCapabilities, .name = &todo_fix_wlSeatName };

WlShmListener todo_fix_wlShmListener = { .format = &todo_fix_wlShmFormatHandler };


WlOutputListener todo_fix_wlOutputListener = {
	.geometry = &todo_fix_wlOutputSetGeometry, .mode = &todo_fix_wlOutputHandleMode, .done = &todo_fix_wlOutputHandleDone,
	.scale = &todo_fix_wlOutputSetScale, .name = &todo_fix_wlOutputSetName, .description = &todo_fix_wlOutputDescription
};

ZxdgOutputV1Listener todo_fix_xdgOutputListener = {
	.name = &todo_fix_xdgOutputNothingStr, .done = &todo_fix_xdgOutputNothing, .description = &todo_fix_xdgOutputNothingStr,
	.logicalPosition = &todo_fix_xdgOutputLogicalPos, .logicalSize = &todo_fix_xdgOutputLogicalSize
};


// private
fn void _wl_data_device_selection(void* data, WlProxy* device, WlProxy* offer)
{
	if (!offer) return;

	int[2] pfds;
	pipe(&pfds);
	wlDataOfferReceive(offer, "text/plain", pfds[1]);
	libc::close(pfds[1]);
	wlDisplayRoundtrip(_gfx_ctx.wlDisplay);

	char[1024] buf;
	sz n = libc::read(pfds[0], &buf, 1024);
	if (n <= 0)
	{
		libc::close(pfds[0]);
		wlDataOfferDestroy(offer);
		return;
	}

	sz length = (sz)n;
	if (buf[n - 1] != 0) length += 1;

	if (_gfx_ctx.unixClipboard) free(_gfx_ctx.unixClipboard);
	_gfx_ctx.unixClipboard = mem::new_with_padding(GfxDataTransfer, n);
	char* dataPtr = (char*)_gfx_ctx.unixClipboard + GfxDataTransfer::size - 1;
	mem::copy(dataPtr, &buf, (sz)n);
	dataPtr[length - 1] = 0;
	_gfx_ctx.unixClipboard.data = dataPtr;
	_gfx_ctx.unixClipboard.type = TEXT;
	_gfx_ctx.unixClipboard.length = length;

	libc::close(pfds[0]);
	wlDataOfferDestroy(offer);
}


WlRegistryListener todo_fix_registryListener = { .global = &todo_fix_wlGlobalRegistryHandler, .globalRemove = &todo_fix_wlGlobalRegistryRemove };


// public
fn void? todo_fix_createWindowPlatform_Wayland(String name, GfxWindowFlags flags, GfxWindow* win) => @pool()
{
	todo_fix_debugCallback(TODO_RENAMETYPE_WARNING, TODO_RENAMEWARNING_WAYLAND, "RGFW Wayland support is experimental");

	win.src.surface = wlCompositorCreateSurface(_gfx_ctx.compositor);
	wlSurfaceAddListener(win.src.surface, &todo_fix_windowSurfaceListener, win);
	wlSurfaceSetUserData(win.src.surface, win);

	win.src.customCursorSurface = wlCompositorCreateSurface(_gfx_ctx.compositor);

	win.src.xdgSurface = xdgWmBaseGetXdgSurface(_gfx_ctx.xdgWmBase, win.src.surface);
	xdgSurfaceAddListener(win.src.xdgSurface, &todo_fix_xdgSurfaceListener, win);
	xdgWmBaseSetUserData(_gfx_ctx.xdgWmBase, win);

	win.src.xdgToplevel = xdgSurfaceGetToplevel(win.src.xdgSurface);
	xdgToplevelSetAppId(win.src.xdgToplevel, name.zstr_copy(tmem));
	xdgSurfaceSetWindowGeometry(win.src.xdgSurface, 0, 0, win.width, win.height);

	if (!(win.internal.flags.transparent)) todo_fix_wlSetOpaque(win);

	xdgToplevelAddListener(win.src.xdgToplevel, &todo_fix_xdgToplevelListener, win);

	if (_gfx_ctx.decorationManager)
	{
		uint decorationMode = ZXDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE;
		win.src.decoration = zxdgDecorationManagerV1GetToplevelDecoration(_gfx_ctx.decorationManager, win.src.xdgToplevel);
		zxdgToplevelDecorationV1AddListener(win.src.decoration, &todo_fix_xdgDecorationListener, win);
		if (flags.no_border) decorationMode = ZXDG_TOPLEVEL_DECORATION_V1_MODE_CLIENT_SIDE;
		zxdgToplevelDecorationV1SetMode(win.src.decoration, decorationMode);
	}

	if (_gfx_ctx.iconManager) xdgToplevelIconManagerV1SetIcon(_gfx_ctx.iconManager, win.src.xdgToplevel, null);

	wlSurfaceCommit(win.src.surface);

	while (!win.src.configured) wlDisplayDispatch(_gfx_ctx.wlDisplay);

}


WlDataSourceListener todo_fix_dataSourceListener = {
	.target = &todo_fix_wlDataSourceNothingStr, .action = &todo_fix_wlDataSourceNothingU32,
	.dndDropPerformed = &todo_fix_wlDataSourceNothing, .dndFinished = &todo_fix_wlDataSourceNothing,
	.send = &todo_fix_wlDataSourceSend, .cancelled = &todo_fix_wlDataSourceCancelled
};




// public
fn bool todo_fix_extensionSupportedPlatform_EGL(ZString extension, sz len)
{
	if (todo_fix_eglQueryString == null || _gfx_ctx.egl_display == null) return false;
	ZString extensions = todo_fix_eglQueryString((EglDisplay)_gfx_ctx.egl_display, 0x3055 /* EGL_EXTENSIONS */);
	return extensions && todo_fix_extensionSupportedStr(extensions, extension, len);
}


module std::gfx @feat(X11_WAYLAND) @link("wayland-client", "wayland-cursor", "wayland-egl", "xkbcommon", "EGL");
import std::math, libc;
// ==================== TODO_RENAMEDYNAMIC runtime dispatch layer ====================
// Combined X11+Wayland runtime dispatch: every entry point below branches on
// gfx_use_wayland (set once in todo_fix_initPlatform) instead of a vtable.

bool gfx_use_wayland = false;

// public
fn bool todo_fix_usingWayland() => gfx_use_wayland;


// public
fn void? todo_fix_createWindowPlatform(String name, GfxWindowFlags flags, GfxWindow* win)
{
	if (gfx_use_wayland)
	{
		return todo_fix_createWindowPlatform_Wayland(name, flags, win);
	}
	return todo_fix_createWindowPlatform_X11(name, flags, win);
}

// public
fn void todo_fix_stopCheckEvents()
{
	_gfx_ctx.eventWait_forceStop[2] = 1;
	while (true)
	{
		char byte = 0;
		sz result = libc::write(_gfx_ctx.eventWait_forceStop[1], &byte, 1);
		if (result == 1 || result == -1) break;
	}
}

// public
fn void todo_fix_waitForEvent(int waitMS)
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

	if (todo_fix_usingWayland())
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
			if (*rgfwErrnoLocation() == TODO_RENAMEEAGAIN)
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

	ulong start = todo_fix_unix_getTimeNS();
	if (todo_fix_usingWayland())
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

			if (waitMS != TODO_RENAMEEVENT_WAIT_NEXT)
			{
				waitMS -= (int)((todo_fix_unix_getTimeNS() - start) / 1000000);
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

			if (waitMS != TODO_RENAMEEVENT_WAIT_NEXT)
			{
				waitMS -= (int)((todo_fix_unix_getTimeNS() - start) / 1000000);
			}
		}
	}

	// drain any data in the stop request
	if (_gfx_ctx.eventWait_forceStop[2])
	{
		char[64] data;
		libc::read(_gfx_ctx.eventWait_forceStop[0], &data, 64);

		_gfx_ctx.eventWait_forceStop[2] = 0;
	}
}
