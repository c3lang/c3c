module std::gfx @feat(X11) @link("X11", "Xext", "Xrandr", "GL");

import std::collections::list, std::math, libc;

// C3 port of the X11/GLX RGFW backend from /Users/lerno/Downloads/AGFW.h (RGFW).
// Mechanical, line-by-line style translation of the RGFW_X11 implementation.
// This is a standalone alternative to rgfw.c3 (the macOS build) - only ONE of
// the rgfw*.c3 files should be included for a given target, matching how the
// original C header only ever compiles one #ifdef backend at a time.
// Optional extensions (Xcursor, XInput2, Xext sync/shape) are loaded via dlopen,
// exactly as the original does when RGFW_NO_X11_CURSOR_PRELOAD etc. are not set.



extern fn void* memcpy(void* dest, void* src, sz count);
extern fn void* memset(void* dest, int value, sz count);
extern fn char* strstr(char* haystack, char* needle);
extern fn char* strtok(char* str, char* delim);
extern fn char* strtokR(char* str, char* delim, char** saveptr) @cname("strtok_r");
extern fn long strtol(char* str, char** endptr, int base);
extern fn float powf(float x, float y);
extern fn int chdir(char* path);
extern fn int clock_gettime(int clock_id, Timespec* ts);

struct Timespec { long tv_sec; long tv_nsec; }

// ---- Xlib core types (ABI-compatible aliases; GfxXDisplay/GfxXVisual/Screen stay opaque) ----
typedef Screen = void;
alias Colormap = ulong;
alias XCursor = ulong;
alias Pixmap = ulong;
alias GContext = void*;
alias Atom = ulong;
alias VisualID = ulong;
alias KeySym = ulong;
alias KeyCode = char;
alias Time = ulong;
alias XlibId = ulong;
alias Region = void*;
alias XlibIC = void*;
alias XlibIM = void*;
alias XIMStyle = ulong;
alias RRMode = ulong;
alias RRCrtc = ulong;
alias GLXContext = void*;
alias GLXFBConfig = void*;
alias GLXDrawable = ulong;
alias GLXWindow = ulong;

const int X_TRUE = 1;
const int X_FALSE = 0;
const int NONE = 0;
const int CURRENT_TIME = 0;

struct XAspect { int x, y; }
struct X11WindowHints { ulong flags, functions, decorations, status; long input_mode; }
alias GlxGetProcAddressProc = fn GfxFn(ZString);

struct XVisualInfo
{
	GfxXVisual* visual;
	VisualID visualid;
	int screen;
	int depth;
	int c_class;
	ulong red_mask, green_mask, blue_mask;
	int colormap_size;
	int bits_per_rgb;
}

struct XSetWindowAttributes
{
	Pixmap background_pixmap;
	ulong background_pixel;
	Pixmap border_pixmap;
	ulong border_pixel;
	int bit_gravity;
	int win_gravity;
	int backing_store;
	ulong backing_planes;
	ulong backing_pixel;
	int save_under;
	long event_mask;
	long do_not_propagate_mask;
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
	ulong backing_planes;
	ulong backing_pixel;
	int save_under;
	Colormap colormap;
	int map_installed;
	int map_state;
	long all_event_masks;
	long your_event_mask;
	long do_not_propagate_mask;
	int override_redirect;
	Screen* screen;
}

struct XClassHint { ZString res_name; ZString res_class; }

struct XWMHints
{
	long flags;
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
	long flags;
	int x, y, width, height;
	int min_width, min_height;
	int max_width, max_height;
	int width_inc, height_inc;
	XAspect min_aspect;
	XAspect max_aspect;
	int base_width, base_height;
	int win_gravity;
}

struct XAnyEvent { int type; ulong serial; int send_event; GfxXDisplay* display; GfxXWindow window; }
struct XKeyEvent { int type; ulong serial; int send_event; GfxXDisplay* display; GfxXWindow window; GfxXWindow root; GfxXWindow subwindow; Time time; int x, y; int x_root, y_root; uint state; uint keycode; int same_screen; }
struct XButtonEvent { int type; ulong serial; int send_event; GfxXDisplay* display; GfxXWindow window; GfxXWindow root; GfxXWindow subwindow; Time time; int x, y; int x_root, y_root; uint state; uint button; int same_screen; }
struct XMotionEvent { int type; ulong serial; int send_event; GfxXDisplay* display; GfxXWindow window; GfxXWindow root; GfxXWindow subwindow; Time time; int x, y; int x_root, y_root; uint state; char is_hint; int same_screen; }
struct XCrossingEvent { int type; ulong serial; int send_event; GfxXDisplay* display; GfxXWindow window; GfxXWindow root; GfxXWindow subwindow; Time time; int x, y; int x_root, y_root; int mode; int detail; int same_screen; int focus; uint state; }
struct XFocusChangeEvent { int type; ulong serial; int send_event; GfxXDisplay* display; GfxXWindow window; int mode; int detail; }
struct XExposeEvent { int type; ulong serial; int send_event; GfxXDisplay* display; GfxXWindow window; int x, y, width, height; int count; }
struct XConfigureEvent { int type; ulong serial; int send_event; GfxXDisplay* display; GfxXWindow event; GfxXWindow window; int x, y, width, height; int border_width; GfxXWindow above; int override_redirect; }
struct XPropertyEvent { int type; ulong serial; int send_event; GfxXDisplay* display; GfxXWindow window; Atom atom; Time time; int state; }
struct XClientMessageDataL { long[5] l; }
struct XClientMessageEvent { int type; ulong serial; int send_event; GfxXDisplay* display; GfxXWindow window; Atom message_type; int format; XClientMessageDataL data; }
struct XSelectionRequestEvent { int type; ulong serial; int send_event; GfxXDisplay* display; GfxXWindow owner; GfxXWindow requestor; Atom selection; Atom target; Atom property; Time time; }
struct XSelectionEvent { int type; ulong serial; int send_event; GfxXDisplay* display; GfxXWindow requestor; Atom selection; Atom target; Atom property; Time time; }
struct XReparentEvent { int type; ulong serial; int send_event; GfxXDisplay* display; GfxXWindow event; GfxXWindow window; GfxXWindow parent; int x, y; int override_redirect; }
struct XMapEvent { int type; ulong serial; int send_event; GfxXDisplay* display; GfxXWindow event; GfxXWindow window; int override_redirect; }
struct XUnmapEvent { int type; ulong serial; int send_event; GfxXDisplay* display; GfxXWindow event; GfxXWindow window; int from_configure; }
struct XErrorEvent { int type; GfxXDisplay* display; ulong resourceid; ulong serial; char error_code; char request_code; char minor_code; }
struct XGenericEventCookie { int type; ulong serial; int send_event; GfxXDisplay* display; int extension; int evtype; uint cookie_data_len; void* data; }

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
	long[24] pad;
}

// ---- XRandr ----
struct XRRScreenResources { Time timestamp; Time configTimestamp; int ncrtc; RRCrtc* crtcs; int noutput; GfxRROutput* outputs; int nmode; XRRModeInfoNative* modes; }
struct XRRModeInfoNative { RRMode id; uint width; uint height; ulong dotClock; uint hSyncStart; uint hSyncEnd; uint hTotal; uint hSkew; uint vSyncStart; uint vSyncEnd; uint vTotal; char* name; uint nameLength; ulong modeFlags; }
struct XRRCrtcInfo { Time timestamp; int x, y; uint width, height; RRMode mode; int rotation; int noutput; GfxRROutput* outputs; int rotations; int npossible; GfxRROutput* possible; }
struct GfxXRROutputInfo { Time timestamp; RRCrtc crtc; ZString name; int nameLen; ulong mm_width; ulong mm_height; int connection; int subpixel_order; int ncrtc; RRCrtc* crtcs; int nclone; GfxRROutput* clones; int nmode; int npreferred; RRMode* modes; }
struct XRRCrtcGamma { int size; ushort* red; ushort* green; ushort* blue; }

// ---- Xcursor / XInput2 / Xext sync+shape (dlopen'd like the original) ----
struct XcursorImage { uint version; uint size; uint width, height; uint xhot, yhot; uint delay; uint* pixels; }
struct XIEventMask { int deviceid; int mask_len; char* mask; }
struct XIValuatorState { int mask_len; char* mask; double* values; }
struct XIRawEvent { int type; ulong serial; int send_event; GfxXDisplay* display; int extension; int evtype; Time time; int deviceid; int sourceid; int detail; int flags; XIValuatorState valuators; double* raw_values; }
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
	ulong* supported_styles;
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

struct RgfwNativeImage
{
	GfxFormat format;
	char* buffer;
	void* bitmap; // XImage*
}

struct RgfwWindowSrc
{
	GfxXWindow window;
	GfxXWindow parent;
	GContext gc;
	XlibIC ic;
	ulong flashEnd;
	long counter_value;
	XSyncCounter counter;
	RgfwGfxContext ctx;
	RgfwGfxContextType gfxType;
	int x, y, w, h;
}





RgfwInfo* _rgfw;
RgfwInfo _rgfw_global;
EglGetProcAddressProc rgfw_eglGetProcAddress;
EglGetDisplayProc rgfw_eglGetDisplay;
EglInitializeProc rgfw_eglInitialize;
EglTerminateProc rgfw_eglTerminate;
EglBindApiProc rgfw_eglBindApi;
EglChooseConfigProc rgfw_eglChooseConfig;
EglCreateContextProc rgfw_eglCreateContext;
EglDestroyContextProc rgfw_eglDestroyContext;
EglCreateWindowSurfaceProc rgfw_eglCreateWindowSurface;
EglDestroySurfaceProc rgfw_eglDestroySurface;
EglMakeCurrentProc rgfw_eglMakeCurrent;
EglSwapBuffersProc rgfw_eglSwapBuffers;
EglSwapIntervalProc rgfw_eglSwapInterval;
EglGetCurrentContextProc rgfw_eglGetCurrentContext;
EglQueryStringProc rgfw_eglQueryString;
EglGetConfigAttribProc rgfw_eglGetConfigAttrib;
fn void rgfw_setInfo(RgfwInfo* info) { _rgfw = info; }
fn RgfwInfo* rgfw_getInfo() { return _rgfw; }
fn void* rgfw_alloc(sz size) { return rgfw_alloc_impl(size); }
fn void rgfw_free(void* ptr) { rgfw_free_impl(ptr); }
fn sz rgfw_sizeofInfo() { return RgfwInfo::size; }
fn sz rgfw_sizeofNativeImage() { return RgfwNativeImage::size; }
fn sz rgfw_sizeofSurface() { return GfxSurface::size; }
fn sz rgfw_sizeofWindow() { return GfxWindow::size; }
fn sz rgfw_sizeofWindowSrc() { return RgfwWindowSrc::size; }

fn bool rgfw_loadGL()
{
	if (_rgfw.nativeGL_handle != null && _rgfw.glGetProcAddress != null) return true;
	ZString[3] glxSonames = {"libGLX.so.0", "libGL.so.1", "libGL.so"};
	for (sz i = 0; i < 3; i++)
	{
		_rgfw.nativeGL_handle = libc::dlopen(glxSonames[i], RTLD_LAZY | RTLD_LOCAL);
		if (_rgfw.nativeGL_handle != null) break;
	}
	if (_rgfw.nativeGL_handle == null) return false;
	void* ptr = libc::dlsym(_rgfw.nativeGL_handle, "glXGetProcAddress");
	_rgfw.glGetProcAddress = ptr;

	rgfw_glXChooseVisual = (RGFW_glXChooseVisualProc)libc::dlsym(_rgfw.nativeGL_handle, "glXChooseVisual");
	rgfw_glXCreateContext = (RGFW_glXCreateContextProc)libc::dlsym(_rgfw.nativeGL_handle, "glXCreateContext");
	rgfw_glXMakeCurrent = (RGFW_glXMakeCurrentProc)libc::dlsym(_rgfw.nativeGL_handle, "glXMakeCurrent");
	rgfw_glXGetCurrentContext = (RGFW_glXGetCurrentContextProc)libc::dlsym(_rgfw.nativeGL_handle, "glXGetCurrentContext");
	rgfw_glXSwapBuffers = (RGFW_glXSwapBuffersProc)libc::dlsym(_rgfw.nativeGL_handle, "glXSwapBuffers");
	rgfw_glXGetVisualFromFBConfig = (RGFW_glXGetVisualFromFBConfigProc)libc::dlsym(_rgfw.nativeGL_handle, "glXGetVisualFromFBConfig");
	rgfw_glXGetFBConfigAttrib = (RGFW_glXGetFBConfigAttribProc)libc::dlsym(_rgfw.nativeGL_handle, "glXGetFBConfigAttrib");
	rgfw_glXGetProcAddressARB = (RGFW_glXGetProcAddressARBProc)libc::dlsym(_rgfw.nativeGL_handle, "glXGetProcAddressARB");
	rgfw_glXChooseFBConfig = (RGFW_glXChooseFBConfigProc)libc::dlsym(_rgfw.nativeGL_handle, "glXChooseFBConfig");
	rgfw_glXDestroyContext = (RGFW_glXDestroyContextProc)libc::dlsym(_rgfw.nativeGL_handle, "glXDestroyContext");
	rgfw_glXCreateWindow = (RGFW_glXCreateWindowProc)libc::dlsym(_rgfw.nativeGL_handle, "glXCreateWindow");
	rgfw_glXDestroyWindow = (RGFW_glXDestroyWindowProc)libc::dlsym(_rgfw.nativeGL_handle, "glXDestroyWindow");
	rgfw_glXQueryExtensionsString = (RGFW_glXQueryExtensionsStringProc)libc::dlsym(_rgfw.nativeGL_handle, "glXQueryExtensionsString");
	rgfw_glXCreateContextAttribsARB = (RGFW_glXCreateContextAttribsARBProc)libc::dlsym(_rgfw.nativeGL_handle, "glXCreateContextAttribsARB");
	rgfw_glXSwapIntervalEXT = (RGFW_glXSwapIntervalEXTProc)libc::dlsym(_rgfw.nativeGL_handle, "glXSwapIntervalEXT");
	if (rgfw_glXSwapIntervalEXT == null)
	{
		ZString[2] array = {"GLX_MESA_swap_control", "GLX_SGI_swap_control"};
		for (sz i = 0; i < 2 && rgfw_glXSwapIntervalNative == null; i++)
		{
			rgfw_glXSwapIntervalNative = (RGFW_glXSwapIntervalNativeProc)libc::dlsym(_rgfw.nativeGL_handle, array[i]);
		}
	}
	return true;
}

fn void rgfw_unloadGL()
{
	_rgfw.nativeGL_handle = null;
	_rgfw.glGetProcAddress = null;
}
fn void* rgfw_eglProc(ZString name)
{
	if (_rgfw.egl_handle == null) return null;
	void* proc = libc::dlsym(_rgfw.egl_handle, name);
	if (proc == null && rgfw_eglGetProcAddress != null) proc = (void*)rgfw_eglGetProcAddress(name);
	return proc;
}

fn bool rgfw_loadEGL()
{
	if (_rgfw.egl_handle != null) return true;
	ZString[5] names = {"libEGL.dylib", "libEGL.1.dylib", "libEGL.so.1", "libEGL.so", "EGL.dll"};
	for (sz i = 0; i < 5; i++)
	{
		_rgfw.egl_handle = libc::dlopen(names[i], RTLD_LAZY | RTLD_LOCAL);
		if (_rgfw.egl_handle != null) break;
	}
	if (_rgfw.egl_handle == null) return false;
	void* getProc = libc::dlsym(_rgfw.egl_handle, "eglGetProcAddress");
	if (getProc == null) return false;
	rgfw_eglGetProcAddress = (EglGetProcAddressProc)getProc;
	_rgfw.eglGetProcAddress = getProc;
	void* proc;
	proc = rgfw_eglProc("eglGetDisplay"); if (proc == null) return false; rgfw_eglGetDisplay = (EglGetDisplayProc)proc;
	proc = rgfw_eglProc("eglInitialize"); if (proc == null) return false; rgfw_eglInitialize = (EglInitializeProc)proc;
	proc = rgfw_eglProc("eglTerminate"); if (proc == null) return false; rgfw_eglTerminate = (EglTerminateProc)proc;
	proc = rgfw_eglProc("eglBindAPI"); if (proc == null) return false; rgfw_eglBindApi = (EglBindApiProc)proc;
	proc = rgfw_eglProc("eglChooseConfig"); if (proc == null) return false; rgfw_eglChooseConfig = (EglChooseConfigProc)proc;
	proc = rgfw_eglProc("eglCreateContext"); if (proc == null) return false; rgfw_eglCreateContext = (EglCreateContextProc)proc;
	proc = rgfw_eglProc("eglDestroyContext"); if (proc == null) return false; rgfw_eglDestroyContext = (EglDestroyContextProc)proc;
	proc = rgfw_eglProc("eglCreateWindowSurface"); if (proc == null) return false; rgfw_eglCreateWindowSurface = (EglCreateWindowSurfaceProc)proc;
	proc = rgfw_eglProc("eglDestroySurface"); if (proc == null) return false; rgfw_eglDestroySurface = (EglDestroySurfaceProc)proc;
	proc = rgfw_eglProc("eglMakeCurrent"); if (proc == null) return false; rgfw_eglMakeCurrent = (EglMakeCurrentProc)proc;
	proc = rgfw_eglProc("eglSwapBuffers"); if (proc == null) return false; rgfw_eglSwapBuffers = (EglSwapBuffersProc)proc;
	proc = rgfw_eglProc("eglSwapInterval"); if (proc == null) return false; rgfw_eglSwapInterval = (EglSwapIntervalProc)proc;
	proc = rgfw_eglProc("eglGetCurrentContext"); if (proc == null) return false; rgfw_eglGetCurrentContext = (EglGetCurrentContextProc)proc;
	proc = rgfw_eglProc("eglQueryString"); if (proc == null) return false; rgfw_eglQueryString = (EglQueryStringProc)proc;
	proc = rgfw_eglProc("eglGetConfigAttrib"); if (proc == null) return false; rgfw_eglGetConfigAttrib = (EglGetConfigAttribProc)proc;
	return true;
}

fn void rgfw_unloadEGL()
{
	if (_rgfw.egl_handle == null) return;
	if (_rgfw.egl_display != null && rgfw_eglTerminate != null) rgfw_eglTerminate((EglDisplay)_rgfw.egl_display);
	libc::dlclose(_rgfw.egl_handle);
	_rgfw.egl_handle = null;
	_rgfw.egl_display = null;
	_rgfw.eglGetProcAddress = null;
	rgfw_eglGetProcAddress = null;
}
const int RTLD_LAZY = 0x1;
const int RTLD_LOCAL = 0x4;
const int VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR = 1000004000;
const VkResult VK_ERROR_EXTENSION_NOT_PRESENT = -7;
struct VkXlibSurfaceCreateInfoKHR { int sType; void* pNext; uint flags; GfxXDisplay* dpy; GfxXWindow window; }
alias RgfwVkCreateXlibSurfaceKHRProc = fn VkResult(VkInstance instance, VkXlibSurfaceCreateInfoKHR* createInfo, void* allocator, VkSurfaceKHR* surface);
ZString[2] rgfw_vulkanExtensions = {"VK_KHR_surface", "VK_KHR_xlib_surface"};

fn bool rgfw_loadVulkan()
{
	if (_rgfw.vulkan_handle != null) return true;
	ZString[3] names = {"libvulkan.so.1", "libvulkan.so", "libMoltenVK.so"};
	for (sz i = 0; i < 3; i++)
	{
		_rgfw.vulkan_handle = libc::dlopen(names[i], RTLD_LAZY | RTLD_LOCAL);
		if (_rgfw.vulkan_handle != null) break;
	}
	if (_rgfw.vulkan_handle == null) return false;
	void* proc = libc::dlsym(_rgfw.vulkan_handle, "vkGetInstanceProcAddr");
	if (proc == null) return false;
	_rgfw.vkGetInstanceProcAddress = (GfxVkGetInstanceProcAddressProc)proc;
	return true;
}

fn void rgfw_unloadVulkan()
{
	if (_rgfw.vulkan_handle == null) return;
	libc::dlclose(_rgfw.vulkan_handle);
	_rgfw.vulkan_handle = null;
	_rgfw.vkGetInstanceProcAddress = null;
}

fn GfxFn rgfw_getInstanceProcAddress_Vulkan(VkInstance instance, ZString procname)
{
	if (_rgfw == null || _rgfw.vkGetInstanceProcAddress == null) return null;
	return _rgfw.vkGetInstanceProcAddress(instance, procname);
}

fn ZString* rgfw_getRequiredInstanceExtensions_Vulkan(sz* count)
{
	if (count != null) *count = 2;
	return &rgfw_vulkanExtensions;
}

fn VkResult rgfw_window_createSurface_Vulkan(GfxWindow* win, VkInstance instance, VkSurfaceKHR* surface)
{
	if (surface == null || win == null) return VK_ERROR_EXTENSION_NOT_PRESENT;
	*surface = 0;
	GfxFn raw = rgfw_getInstanceProcAddress_Vulkan(instance, "vkCreateXlibSurfaceKHR");
	if (raw == null) return VK_ERROR_EXTENSION_NOT_PRESENT;
	VkXlibSurfaceCreateInfoKHR createInfo;
	createInfo.sType = VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR;
	createInfo.pNext = null;
	createInfo.flags = 0;
	createInfo.dpy = _rgfw.display;
	createInfo.window = win.src.window;
	return ((RgfwVkCreateXlibSurfaceKHRProc)raw)(instance, &createInfo, null, surface);
}

extern fn VisualID xVisualIDFromVisual(GfxXVisual* visual) @cname("XVisualIDFromVisual");
alias RgfwVkGetPhysicalDeviceXlibPresentationSupportKHRProc = fn uint(VkPhysicalDevice, uint, GfxXDisplay*, VisualID);

fn bool rgfw_getPhysicalDevicePresentationSupport_Vulkan(VkInstance instance, VkPhysicalDevice physicalDevice, uint queueFamilyIndex)
{
	GfxFn raw = rgfw_getInstanceProcAddress_Vulkan(instance, "vkGetPhysicalDeviceXlibPresentationSupportKHR");
	if (!raw) return false;
	GfxXVisual* visual = xDefaultVisual(_rgfw.display, xDefaultScreen(_rgfw.display));
	uint out = ((RgfwVkGetPhysicalDeviceXlibPresentationSupportKHRProc)raw)(physicalDevice, queueFamilyIndex, _rgfw.display, xVisualIDFromVisual(visual));
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

fn WGPUSurface rgfw_window_createSurface_WebGPU(GfxWindow* window, WGPUInstance instance)
{
	WgpuSurfaceSourceXlibWindow fromXlib;
	fromXlib.chain.next = null;
	fromXlib.chain.sType = WGPU_STYPE_SURFACE_SOURCE_XLIB_WINDOW;
	fromXlib.display = _rgfw.display;
	fromXlib.window = window.src.window;
	WgpuSurfaceDescriptor surfaceDesc;
	surfaceDesc.nextInChain = (WgpuChainedStruct*)&fromXlib;
	return wgpuInstanceCreateSurface(instance, &surfaceDesc);
}

fn void rgfw_debugCallback(RgfwDebugType type, RgfwErrorCode code, ZString msg)
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

fn int rgfw_init_ptr(ZString className, RgfwInitFlags flags, RgfwInfo* info)
{
	if (info == _rgfw || info == null) return 1;
	if (className == null) className = "RGFW";
	rgfw_setInfo(info);
	*_rgfw = {};
	if (flags & RGFW_INIT_OPEN_GL && rgfw_loadGL() == false) return -1;
	if (flags & RGFW_INIT_EGL && rgfw_loadEGL() == false) return -1;
	if (flags & RGFW_INIT_VULKAN && rgfw_loadVulkan() == false) return -1;
	rgfw_initKeycodes();
	int out = rgfw_initPlatform(className, flags);
	if (out != 0)
	{
		rgfw_deinitPlatform();
		*_rgfw = {};
		rgfw_setInfo(null);
		return out;
	}
	for (sz i = 0; i < (sz)RGFW_MOUSE_ICON_COUNT; i++)
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
		if (_rgfw.standardMice[(sz)i] != null) rgfw_freeMouse(_rgfw.standardMice[(sz)i]);
	}
	rgfw_freeMonitors();
	rgfw_deinitPlatform();
	rgfw_unloadEGL();
	rgfw_unloadGL();
	_rgfw.root = null;
	_rgfw.windowCount = 0;
	rgfw_setInfo(null);
}

fn GfxWindow* rgfw_createWindowPtr(ZString name, int x, int y, int w, int h, RgfwWindowFlags flags, GfxWindow* win)
{
	if (win == null || _rgfw == null) return null;
	if (name == null) name = "";
	*win = {};
	_rgfw.windowCount++;
	if (_rgfw.root == null) rgfw_setRootWindow(win);
	win.x = x; win.y = y; win.width = w; win.height = h;
	win.internal.mouse = _rgfw.standardMice[(sz)RGFW_MOUSE_NORMAL];
	win.internal.flags = flags;
	win.internal.enabledEvents = RGFW_ALL_EVENT_FLAGS;
	RgfwWindowFlags reservedFlags = flags & RGFW_WINDOW_SCALE_TO_MONITOR;
	GfxWindow* ret = rgfw_createWindowPlatform(name, flags, win);
	if (ret == null) return null;
	flags |= reservedFlags;
	_window_set_flags(win, flags, 0);
	if (!(flags & RGFW_WINDOW_HIDE_MOUSE)) rgfw_window_setMouseDefault(win);
	rgfw_window_setName(win, name);
	if (!(flags & RGFW_WINDOW_HIDE))
	{
		flags |= RGFW_WINDOW_HIDE;
		show(win);
	}
	else if ((flags & RGFW_WINDOW_MAXIMIZE) || (flags & RGFW_WINDOW_FULLSCREEN))
	{
		rgfw_window_hide(win);
	}
	return ret;
}


fn void rgfw_window_closePtr(GfxWindow* win)
{
	if (win == null) return;
	if (win.internal.captureMouse) rgfw_window_captureMouse(win, false);
	rgfw_window_closePlatform(win);
	if (_rgfw.clipboard != null) rgfw_free_impl(_rgfw.clipboard);
	_rgfw.clipboard = null;
	_rgfw.windowCount--;
}

fn void rgfw_window_setDND(GfxWindow* win, bool allow) { rgfw_setBit(&win.internal.flags, RGFW_WINDOW_ALLOW_DND, allow); }
fn void rgfw_window_showMouse(GfxWindow* win, bool show) { rgfw_window_showMouseFlags(win, show); if (show == false) { rgfw_window_setMouse(win, _rgfw.hiddenMouse); } else { rgfw_window_setMouse(win, win.internal.mouse); } }

fn void rgfw_window_setAspectRatio(GfxWindow* win, int w, int h)
{
	if (win == null) return;
	if (w == 0 && h == 0) return;
	XSizeHints hints;
	long flags;
	xGetWMNormalHints(_rgfw.display, win.src.window, &hints, &flags);
	hints.flags |= (1 << 6); // PAspect
	hints.min_aspect.x = w; hints.max_aspect.x = w;
	hints.min_aspect.y = h; hints.max_aspect.y = h;
	xSetWMNormalHints(_rgfw.display, win.src.window, &hints);
}

fn GfxFormat rgfw_nativeFormat() => BGRA8;

fn GfxFormat rgfw_xImageGetFormat(void* imagePtr)
{
	// XImage*: bits_per_pixel/red_mask/green_mask/blue_mask live at a fixed
	// offset in the real struct; here we only ever set the format ourselves
	// right after xCreateImage, so this mirrors the original's fallback path.
	return BGRA8;
}

fn bool rgfw_window_createSurfacePtr(GfxWindow* win, char* data, int w, int h, GfxFormat format, GfxSurface* surface)
{
	if (surface == null) return false;
	surface.data = data;
	surface.size = { w, h };
	surface.format = format;

	XWindowAttributes attrs;
	if (xGetWindowAttributes(_rgfw.display, win.src.window, &attrs) == 0)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_BUFFER, "Failed to get window attributes.");
		return false;
	}

	surface.native.bitmap = xCreateImage(_rgfw.display, attrs.visual, (uint)attrs.depth, 2 /* ZPixmap */, 0, null, ...(uint[<2>])surface.size, 32, 0);
	surface.native.buffer = (char*)rgfw_alloc_impl((sz)(w * h * 4));
	surface.native.format = rgfw_xImageGetFormat(surface.native.bitmap);

	if (surface.native.bitmap == null)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_BUFFER, "Failed to create XImage.");
		return false;
	}

	surface.native.format = BGRA8;
	return true;
}

fn bool rgfw_createSurfacePtr(char* data, int w, int h, GfxFormat format, GfxSurface* surface) => rgfw_window_createSurfacePtr(_rgfw.root, data, w, h, format, surface);

fn void rgfw_window_blitSurface(GfxWindow* win, GfxSurface* surface)
{
	if (surface == null) return;
	// surface.native.bitmap->data = surface.native.buffer would go here in real Xlib
	// (XImage.data is a char* at a fixed offset); left to the caller's XImage helper.
	IntVec2 min = math::min(win.size, surface.size);
	xPutImage(_rgfw.display, win.src.window, win.src.gc, surface.native.bitmap, 0, 0, 0, 0, (uint)min.x, (uint)min.y);
}

fn void rgfw_surface_freePtr(GfxSurface* surface)
{
	if (surface == null) return;
	rgfw_free_impl(surface.native.buffer);
	xDestroyImage(surface.native.bitmap);
}

fn void rgfw_window_setBorder(GfxWindow* win, bool border)
{
	rgfw_setBit(&win.internal.flags, RGFW_WINDOW_NO_BORDER, !border);
	X11WindowHints hints;
	hints.flags = 2;
	hints.decorations = (ulong)border;
	xChangeProperty(_rgfw.display, win.src.window, _rgfw.motifWmHints, _rgfw.motifWmHints, 32, 0, (char*)&hints, 5);
	if (!rgfw_window_isHidden(win))
	{
		rgfw_window_hide(win);
		show(win);
	}
}

fn void rgfw_window_setRawMouseModePlatform(GfxWindow* win, bool state)
{
	char[4] mask;
	if (state) xiSetMask((char*)&mask, 6 /* XI_RawMotion */);
	XIEventMask em;
	em.deviceid = 1; // XIAllMasterDevices
	em.mask_len = 4;
	em.mask = (char*)&mask;
	if (rgfw_xiSelectEventsSRC != null) rgfw_xiSelectEventsSRC(_rgfw.display, xDefaultRootWindow(_rgfw.display), &em, 1);
}

fn void rgfw_window_captureMousePlatform(GfxWindow* win, bool state)
{
	if (state)
	{
		uint eventMask = (1 << 2) | (1 << 3) | (1 << 6); // Button press/release + pointer motion
		xGrabPointer(_rgfw.display, win.src.window, X_TRUE, eventMask, 1, 1, win.src.window, 0, 0);
	}
	else
	{
		xUngrabPointer(_rgfw.display, 0);
	}
}

fn void rgfw_window_getVisual(XVisualInfo* visual, bool transparent)
{
	visual.visual = xDefaultVisual(_rgfw.display, xDefaultScreen(_rgfw.display));
	visual.depth = xDefaultDepth(_rgfw.display, xDefaultScreen(_rgfw.display));
	if (transparent)
	{
		xMatchVisualInfo(_rgfw.display, xDefaultScreen(_rgfw.display), 32, 4 /* TrueColor */, visual);
		if (visual.depth != 32) rgfw_debugCallback(RGFW_TYPE_WARNING, RGFW_WARNING_OPEN_GL, "Failed to load a 32-bit depth.");
	}
}

fn int rgfw_xErrorHandler(GfxXDisplay* display, XErrorEvent* ev)
{
	char[512] errorText;
	xGetErrorText(display, ev.error_code, (char*)&errorText, 512);
	rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_X11, (ZString)&errorText);
	_rgfw.x11Error = ev;
	return 0;
}

fn void rgfw_x11IcCallback(void* ic, char* clientData, char* callData)
{
	GfxWindow* win = (GfxWindow*)(void*)clientData;
	win.src.ic = null;
}

// IM-level destroy callback: fires if the input-method daemon disappears later,
// so we don't keep a dangling _rgfw.im handle around.
fn void rgfw_x11ImCallback(void* im, char* clientData, char* callData)
{
	_rgfw.im = null;
}

// Instantiate callback: fires once an input method actually becomes available.
// Opens the IM, then validates it supports the exact style this library wants
// (XIMPreeditNothing | XIMStatusNothing) before keeping it, and registers an
// IM-level destroy callback so a later-vanishing IM daemon nulls _rgfw.im.
fn void rgfw_x11ImInitCallback(GfxXDisplay* d, GfxXPointer clientData, GfxXPointer callData)
{
	if (_rgfw.im != null) return;

	_rgfw.im = xOpenIM(_rgfw.display, null, null, null);
	if (_rgfw.im == null) return;

	bool found = false;
	XImStyles* styles = null;

	if (xGetIMValues(_rgfw.im, "queryInputStyle", &styles, (char*)null) != null)
	{
		found = false;
	}
	else
	{
		for (uint i = 0; i < styles.count_styles; i++)
		{
			if (styles.supported_styles[i] == (0x0008 | 0x0400) /* XIMPreeditNothing | XIMStatusNothing */)
			{
				found = true;
				break;
			}
		}
		xFree(styles);
	}

	if (found == false)
	{
		xCloseIM(_rgfw.im);
		_rgfw.im = null;
		return;
	}

	XImCallback callback;
	callback.callback = &rgfw_x11ImCallback;
	callback.client_data = null;
	xSetIMValues(_rgfw.im, "destroyCallback", &callback, (char*)null);
}

fn bool rgfw_xCreateWindow(XVisualInfo visual, ZString name, RgfwWindowFlags flags, GfxWindow* win)
{
	// KeyPress | KeyRelease | ButtonPress | ButtonRelease | PointerMotion | StructureNotify | FocusChange |
	// LeaveWindow | EnterWindow | Exposure | VisibilityChange | PropertyChange
	long eventMask = (1 << 0) | (1 << 1) | (1 << 2) | (1 << 3) | (1 << 6) | (1 << 17) | (1 << 21) |
		(1 << 5) | (1 << 4) | (1 << 15) | (1 << 16) | (1 << 22);

	XSetWindowAttributes swa;
	win.src.parent = xDefaultRootWindow(_rgfw.display);

	Colormap cmap = xCreateColormap(_rgfw.display, win.src.parent, visual.visual, 0);
	swa.colormap = cmap;
	swa.event_mask = eventMask;
	swa.background_pixmap = 0;

	win.src.window = xCreateWindow(_rgfw.display, win.src.parent, win.x, win.y, (uint)win.width, (uint)win.height,
		0, visual.depth, 1 /* InputOutput */, visual.visual, (1 << 3) | (1 << 13) | (1 << 11), &swa);
		// CWBorderPixel | CWColormap | CWEventMask
	if (win.src.window == 0) return false;

	win.src.flashEnd = 0;
	xFreeColors(_rgfw.display, cmap, null, 0, 0);
	xSaveContext(_rgfw.display, win.src.window, _rgfw.context, (GfxXPointer)win);
	win.src.gc = xCreateGC(_rgfw.display, win.src.window, 0, null);

	if (_rgfw.im != null)
	{
		XImCallback callback;
		callback.callback = &rgfw_x11IcCallback;
		callback.client_data = (GfxXPointer)win;

		win.src.ic = xCreateIC(_rgfw.im, "inputStyle", (long)(0x0008 | 0x0400) /* XIMPreeditNothing | XIMStatusNothing */,
			"clientWindow", win.src.window, "focusWindow", win.src.window,
			"destroyCallback", &callback, (char*)null);
	}

	XClassHint hint;
	hint.res_name = name != null ? name : "RGFW";
	hint.res_class = _rgfw.className;
	xSetClassHint(_rgfw.display, win.src.window, &hint);

	XWMHints hints;
	hints.flags = 1 << 1; // StateHint
	hints.initial_state = 1; // NormalState
	xSetWMHints(_rgfw.display, win.src.window, &hints);

	xSelectInput(_rgfw.display, win.src.window, eventMask);
	xSetWMProtocols(_rgfw.display, win.src.window, &_rgfw.wmDeleteWindow, 1);
	rgfw_window_setName(win, name);
	xMoveWindow(_rgfw.display, win.src.window, win.x, win.y);

	if (flags & RGFW_WINDOW_ALLOW_DND)
	{
		win.internal.flags |= RGFW_WINDOW_ALLOW_DND;
		char version = 5;
		xChangeProperty(_rgfw.display, win.src.window, _rgfw.xdndAware, 4, 32, 0, &version, 1);
	}

	win.src.x = win.x; win.src.y = win.y; win.src.w = win.width; win.src.h = win.height;
	xSetWindowBackground(_rgfw.display, win.src.window, 0);
	xClearWindow(_rgfw.display, win.src.window);
	xSetWindowBackgroundPixmap(_rgfw.display, win.src.window, 0);
	return true;
}

fn GfxWindow* rgfw_createWindowPlatform(ZString name, RgfwWindowFlags flags, GfxWindow* win)
{
	if ((flags & RGFW_WINDOW_OPEN_GL) || (flags & RGFW_WINDOW_EGL))
	{
		win.src.window = 0;
		return win;
	}
	XVisualInfo visual;
	rgfw_window_getVisual(&visual, win.internal.flags & RGFW_WINDOW_TRANSPARENT != 0);
	if (!rgfw_xCreateWindow(visual, name, flags, win)) return null;
	return win;
}

fn bool rgfw_getGlobalMouse(int* fX, int* fY)
{
	int x, y;
	uint z;
	GfxXWindow window1, window2;
	xQueryPointer(_rgfw.display, xDefaultRootWindow(_rgfw.display), &window1, &window2, fX, fY, &x, &y, &z);
	return true;
}

fn void rgfw_unix_parseURI(GfxWindow* win, ZString data)
{
	ZString prefix = "file://";
	ZString line;
	ZString savePtr;
	while ((line = (ZString)strtokR(data, "\r\n", (char**)&savePtr)) != null)
	{
		data = null;

		if (line[0] == '#') continue;

		ZString l;
		for (l = line; true; l++)
		{
			if ((char*)l - (char*)line > 7) { break; }
			else if (l[0] != prefix[(char*)l - (char*)line]) { break; }
			else if (l[0] == 0 && prefix[(char*)l - (char*)line] == 0)
			{
				line += 7;
				while (line[0] != '/') line++;
				break;
			}
			else if (l[0] == 0) { break; }
		}

		sz len = line.len();
		ZString path = (ZString)rgfw_alloc_impl(len + 1);

		sz index = 0;
		while (line[0] != 0)
		{
			if (line[0] == '%' && line[1] != 0 && line[2] != 0)
			{
				char[3] digits = {0, 0, 0};
				digits[0] = line[1];
				digits[1] = line[2];
				digits[2] = 0;
				path[index] = (char)strtol((char*)&digits, null, 16);
				line += 2;
			}
			else
			{
				if (index >= len) break;
				path[index] = line[0];
			}
			index++;
			line++;
		}

		path[len] = 0;
		rgfw_dataDropCallback(win, path, len + 1, RGFW_DATA_FILE);
		rgfw_free_impl(path);
	}
}

fn void rgfw_xHandleClipboardSelection(XEvent* event)
{
	XSelectionRequestEvent* request = &event.xselectionrequest;
	Atom[2] formats = {_rgfw.utf8String, 31 /* XA_STRING */};

	if (request.target == _rgfw.targets)
	{
		Atom[4] targets = {_rgfw.targets, _rgfw.multiple, _rgfw.utf8String, 31};
		xChangeProperty(_rgfw.display, request.requestor, request.property, 4 /* XA_ATOM */, 32, 0, (char*)&targets, 4);
	}
	else if (request.target == _rgfw.multiple)
	{
		Atom* targets = null;
		Atom actualType;
		int actualFormat;
		ulong count, bytesAfter;
		xGetWindowProperty(_rgfw.display, request.requestor, request.property, 0, ~0, 0, _rgfw.atomPair,
			&actualType, &actualFormat, &count, &bytesAfter, (char**)&targets);

		for (ulong i = 0; i < count; i += 2)
		{
			if (targets[i] == _rgfw.utf8String || targets[i] == 31 /* XA_STRING */)
			{
				xChangeProperty(_rgfw.display, request.requestor, targets[i + 1], targets[i],
					8, 0, _rgfw.unixClipboard.data, (int)_rgfw.unixClipboard.length);
			}
			else
			{
				targets[i + 1] = 0; // None
			}
		}

		xChangeProperty(_rgfw.display, request.requestor, request.property, _rgfw.atomPair, 32, 0, (char*)targets, (int)count);
		xFlush(_rgfw.display);
		xFree(targets);
	}
	else if (request.target == _rgfw.saveTargets)
	{
		xChangeProperty(_rgfw.display, request.requestor, request.property, 0, 32, 0, null, 0);
	}
	else
	{
		for (sz i = 0; i < 2; i++)
		{
			if (request.target != formats[i]) continue;
			xChangeProperty(_rgfw.display, request.requestor, request.property, request.target,
				8, 0, _rgfw.unixClipboard.data, (int)_rgfw.unixClipboard.length);
		}
	}

	XEvent reply;
	reply.type = 31; // SelectionNotify
	reply.xselection.property = request.property;
	reply.xselection.display = request.display;
	reply.xselection.requestor = request.requestor;
	reply.xselection.selection = request.selection;
	reply.xselection.target = request.target;
	reply.xselection.time = request.time;
	xSendEvent(_rgfw.display, request.requestor, 0, 0, &reply);
	xFlush(_rgfw.display);
}

fn int rgfw_xHandleClipboardSelectionHelper()
{
	XEvent event;
	xPending(_rgfw.display);
	if (qLength(_rgfw.display) != 0 || xEventsQueued(_rgfw.display, 0) + xEventsQueued(_rgfw.display, 1) != 0)
	{
		xNextEvent(_rgfw.display, &event);
	}
	else
	{
		return 0;
	}

	if (event.type == 30) { rgfw_xHandleClipboardSelection(&event); return 0; } // SelectionRequest
	return 0;
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

fn RgfwKey rgfw_physicalToMappedKey(RgfwKey key)
{
	KeyCode keycode = (KeyCode)rgfw_rgfwToApiKey(key);
	KeySym sym = xkbKeycodeToKeysym(_rgfw.display, keycode, 0, 0);
	if (sym < 256) return (RgfwKey)sym;

	switch (sym)
	{
		case XK_F1: return RGFW_KEY_F1; case XK_F2: return RGFW_KEY_F2; case XK_F3: return RGFW_KEY_F3;
		case XK_F4: return RGFW_KEY_F4; case XK_F5: return RGFW_KEY_F5; case XK_F6: return RGFW_KEY_F6;
		case XK_F7: return RGFW_KEY_F7; case XK_F8: return RGFW_KEY_F8; case XK_F9: return RGFW_KEY_F9;
		case XK_F10: return RGFW_KEY_F10; case XK_F11: return RGFW_KEY_F11; case XK_F12: return RGFW_KEY_F12;
		case XK_F13: return RGFW_KEY_F13; case XK_F14: return RGFW_KEY_F14; case XK_F15: return RGFW_KEY_F15;
		case XK_F16: return RGFW_KEY_F16; case XK_F17: return RGFW_KEY_F17; case XK_F18: return RGFW_KEY_F18;
		case XK_F19: return RGFW_KEY_F19; case XK_F20: return RGFW_KEY_F20; case XK_F21: return RGFW_KEY_F21;
		case XK_F22: return RGFW_KEY_F22; case XK_F23: return RGFW_KEY_F23; case XK_F24: return RGFW_KEY_F24;
		case XK_F25: return RGFW_KEY_F25;
		case XK_SHIFT_L: return RGFW_KEY_SHIFT_L; case XK_SHIFT_R: return RGFW_KEY_SHIFT_R;
		case XK_CONTROL_L: return RGFW_KEY_CONTROL_L; case XK_CONTROL_R: return RGFW_KEY_CONTROL_R;
		case XK_ALT_L: return RGFW_KEY_ALT_L; case XK_ALT_R: return RGFW_KEY_ALT_R;
		case XK_SUPER_L: return RGFW_KEY_SUPER_L; case XK_SUPER_R: return RGFW_KEY_SUPER_R;
		case XK_CAPS_LOCK: return RGFW_KEY_CAPS_LOCK;
		case XK_NUM_LOCK: return RGFW_KEY_NUM_LOCK;
		case XK_SCROLL_LOCK: return RGFW_KEY_SCROLL_LOCK;
		case XK_UP: return RGFW_KEY_UP; case XK_DOWN: return RGFW_KEY_DOWN;
		case XK_LEFT: return RGFW_KEY_LEFT; case XK_RIGHT: return RGFW_KEY_RIGHT;
		case XK_HOME: return RGFW_KEY_HOME; case XK_END: return RGFW_KEY_END;
		case XK_PAGE_UP: return RGFW_KEY_PAGE_UP; case XK_PAGE_DOWN: return RGFW_KEY_PAGE_DOWN;
		case XK_INSERT: return RGFW_KEY_INSERT;
		case XK_MENU: return RGFW_KEY_MENU;
		case XK_KP_ADD: return RGFW_KEY_PAD_PLUS;
		case XK_KP_SUBTRACT: return RGFW_KEY_PAD_MINUS;
		case XK_KP_MULTIPLY: return RGFW_KEY_PAD_MULTIPLY;
		case XK_KP_DIVIDE: return RGFW_KEY_PAD_SLASH;
		case XK_KP_EQUAL: return RGFW_KEY_PAD_EQUAL;
		case XK_KP_ENTER: return RGFW_KEY_PAD_RETURN;
		case XK_KP_DECIMAL: return RGFW_KEY_PAD_PERIOD;
		case XK_KP_0: return RGFW_KEY_PAD0; case XK_KP_1: return RGFW_KEY_PAD1;
		case XK_KP_2: return RGFW_KEY_PAD2; case XK_KP_3: return RGFW_KEY_PAD3;
		case XK_KP_4: return RGFW_KEY_PAD4; case XK_KP_5: return RGFW_KEY_PAD5;
		case XK_KP_6: return RGFW_KEY_PAD6; case XK_KP_7: return RGFW_KEY_PAD7;
		case XK_KP_8: return RGFW_KEY_PAD8; case XK_KP_9: return RGFW_KEY_PAD9;
		case XK_PRINT: return RGFW_KEY_PRINT_SCREEN;
		case XK_PAUSE: return RGFW_KEY_PAUSE;
		default: break;
	}

	return RGFW_KEY_NULL;
}

fn void rgfw_xHandleEvent()
{
	static float deltaX = 0.0f;
	static float deltaY = 0.0f;

	XEvent e;
	xNextEvent(_rgfw.display, &e);

	if (e.type != 35 /* GenericEvent */) { deltaX = 0.0f; deltaY = 0.0f; }

	if (e.type == _rgfw.xrandrEventBase + 0 /* RRNotify */) { rgfw_pollMonitors(); return; }

	if (e.type == 30 /* SelectionRequest */) { rgfw_xHandleClipboardSelection(&e); return; }

	if (e.type == 35 /* GenericEvent */)
	{
		xGetEventData(_rgfw.display, &e.xcookie);
		if (e.xcookie.evtype == 17 /* XI_RawMotion */)
		{
			XIRawEvent* raw = (XIRawEvent*)e.xcookie.data;
			if (raw.valuators.mask_len != 0)
			{
				int index = 0;
				if (ximaskIsSet(raw.valuators.mask, 0)) { deltaX += (float)raw.raw_values[index]; index++; }
				if (ximaskIsSet(raw.valuators.mask, 1)) deltaY += (float)raw.raw_values[index];
				_rgfw.vectorX = deltaX; _rgfw.vectorY = deltaY;
				_raw_motion_callback(_rgfw.root, _rgfw.vectorX, _rgfw.vectorY);
			}
		}
		xFreeEventData(_rgfw.display, &e.xcookie);
		return;
	}

	GfxWindow* win = null;
	if (xFindContext(_rgfw.display, e.xany.window, _rgfw.context, (GfxXPointer*)&win) != 0) return;

	if (win.src.flashEnd != 0)
	{
		if (win.src.flashEnd <= rgfw_unix_getTimeNS() || win.has_focus()) rgfw_window_flash(win, CANCEL);
	}

	bool keyRepeat = false;
	if (e.type == 3 /* KeyRelease */ && xEventsQueued(_rgfw.display, 1) != 0)
	{
		XEvent ne;
		xPeekEvent(_rgfw.display, &ne);
		if (ne.type == 2 /* KeyPress */ && e.xkey.time == ne.xkey.time && e.xkey.keycode == ne.xkey.keycode)
		{
			xNextEvent(_rgfw.display, &e);
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
			xkbGetState(_rgfw.display, XKB_USE_CORE_KBD, &state);
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
				xQueryPointer(_rgfw.display, xDefaultRootWindow(_rgfw.display), &rootRet, &childRet, &rootX, &rootY, &winX, &winY, &mask);
				KeySym sym = xkbKeycodeToKeysym(_rgfw.display, (KeyCode)e.xkey.keycode, 0, (mask & SHIFT_MASK) ? 1 : 0);

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
			xkbGetState(_rgfw.display, XKB_USE_CORE_KBD, &state);
			rgfw_keyUpdateKeyMods(win, state.locked_mods & LOCK_MASK != 0, state.locked_mods & MOD2_MASK != 0, state.locked_mods & MOD3_MASK != 0);

			rgfw_keyCallback(win, value, win.internal.mod, false, false);
			break;
		}
		case 4: // ButtonPress
		{
			bool scroll = e.xbutton.button >= 4 && e.xbutton.button <= 7;
			float scrollX = 0.0f;
			float scrollY = 0.0f;
			RgfwMouseButton value = 0;
			switch (e.xbutton.button)
			{
				case 1: value = RGFW_MOUSE_LEFT;
				case 2: value = RGFW_MOUSE_MIDDLE;
				case 3: value = RGFW_MOUSE_RIGHT;
				case 4: scrollY = 1.0f;
				case 5: scrollY = -1.0f;
				case 6: scrollX = 1.0f;
				case 7: scrollX = -1.0f;
				default: value = (RgfwMouseButton)(e.xbutton.button - 1 - 4);
			}
			if (scroll) { rgfw_mouseScrollCallback(win, scrollX, scrollY); break; }
			rgfw_mouseButtonCallback(win, value, true);
			break;
		}
		case 5: // ButtonRelease
		{
			if (e.xbutton.button >= 4 && e.xbutton.button <= 7) break;
			RgfwMouseButton value = 0;
			switch (e.xbutton.button)
			{
				case 1: value = RGFW_MOUSE_LEFT;
				case 2: value = RGFW_MOUSE_MIDDLE;
				case 3: value = RGFW_MOUSE_RIGHT;
				default: value = (RgfwMouseButton)(e.xbutton.button - 1 - 4);
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
				if (e.xproperty.atom == _rgfw.wmState)
				{
					if (rgfw_window_isMinimized(win) && !(win.internal.flags & RGFW_WINDOW_MINIMIZE))
					{
						rgfw_windowMinimizedCallback(win);
						handled = true;
					}
				}
				else if (e.xproperty.atom == _rgfw.netWmState)
				{
					if (rgfw_window_isMaximized(win) && !(win.internal.flags & RGFW_WINDOW_MAXIMIZE))
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
			if ((ulong)e.xclient.data.l[0] == _rgfw.wmDeleteWindow) { _window_close_callback(win); break; }

			if (!(win.internal.flags & RGFW_WINDOW_ALLOW_DND) || _rgfw.x11Version > 5 /* RGFW_XDND_VERSION */) return;

			int dragX = 0;
			int dragY = 0;

			if (e.xclient.message_type == _rgfw.xdndEnter)
			{
				CULong count;
				Atom* formats;
				Atom[3] realFormats;
				int list = (int)(e.xclient.data.l[1] & 1);

				_rgfw.x11Source = (GfxXWindow)e.xclient.data.l[0];
				_rgfw.x11Version = (int)(e.xclient.data.l[1] >> 24);
				_rgfw.x11Format = 0;
				if (list)
				{
					Atom actualType;
					int actualFormat;
					ulong bytesAfter;
					xGetWindowProperty(_rgfw.display, _rgfw.x11Source, _rgfw.xdndTypeList, 0, ~0, 0, 4,
						&actualType, &actualFormat, &count, &bytesAfter, (char**)&formats);
				}
				else
				{
					count = 0;
					for (sz i = 2; i < 2 + 3; i++)
					{
						if (e.xclient.data.l[i] != 0)
						{
							realFormats[count] = (ulong)e.xclient.data.l[i];
							count++;
						}
					}
					formats = (Atom*)&realFormats;
				}

				for (sz i = 0; i < (sz)count; i++)
				{
					if (formats[i] == _rgfw.xtextUriList) { _rgfw.x11TransferType = RGFW_DATA_FILE; }
					else if (formats[i] == _rgfw.xtextPlain) { _rgfw.x11TransferType = RGFW_DATA_TEXT; }
					else { continue; }

					_rgfw.x11Format = (int)formats[i];
					break;
				}

				if (list != 0 && formats != null) xFree(formats);

				rgfw_dataDragCallback(win, _rgfw.x11TransferType, RGFW_DND_ACTION_ENTER, dragX, dragY);
			}
			else if (e.xclient.message_type == _rgfw.xdndPosition)
			{
				int xabs = (int)((e.xclient.data.l[2] >> 16) & 0xffff);
				int yabs = (int)(e.xclient.data.l[2] & 0xffff);
				GfxXWindow dummy;
				int xpos, ypos;

				xTranslateCoordinates(_rgfw.display, xDefaultRootWindow(_rgfw.display), win.src.window,
					xabs, yabs, &xpos, &ypos, &dummy);

				dragX = xpos;
				dragY = ypos;

				_mouse_motion_callback(win, xpos, ypos);

				XEvent reply;
				reply.type = 33; // ClientMessage
				reply.xclient.window = _rgfw.x11Source;
				reply.xclient.message_type = _rgfw.xdndStatus;
				reply.xclient.format = 32;
				reply.xclient.data.l[0] = (long)win.src.window;
				reply.xclient.data.l[2] = 0;
				reply.xclient.data.l[3] = 0;

				if (_rgfw.x11Format != 0)
				{
					reply.xclient.data.l[1] = 1;
					if (_rgfw.x11Version >= 2) reply.xclient.data.l[4] = (long)_rgfw.xdndActionCopy;
				}

				xSendEvent(_rgfw.display, _rgfw.x11Source, 0, 0, &reply);
				xFlush(_rgfw.display);

				rgfw_dataDragCallback(win, _rgfw.x11TransferType, RGFW_DND_ACTION_MOVE, dragX, dragY);
			}
			else if (e.xclient.message_type == _rgfw.xdndLeave)
			{
				rgfw_dataDragCallback(win, _rgfw.x11TransferType, RGFW_DND_ACTION_EXIT, dragX, dragY);
			}
			else if (e.xclient.message_type == _rgfw.xdndDrop)
			{
				if (_rgfw.x11Format != 0)
				{
					Time time = _rgfw.x11Version >= 1 ? (Time)e.xclient.data.l[2] : 0; // CurrentTime
					xConvertSelection(_rgfw.display, _rgfw.xdndSelection, (Atom)_rgfw.x11Format,
						_rgfw.xdndSelection, win.src.window, time);
				}
				else if (_rgfw.x11Version >= 2)
				{
					XEvent reply;
					reply.type = 33; // ClientMessage
					reply.xclient.window = _rgfw.x11Source;
					reply.xclient.message_type = _rgfw.xdndFinished;
					reply.xclient.format = 32;
					reply.xclient.data.l[0] = (long)win.src.window;
					reply.xclient.data.l[1] = 0;
					reply.xclient.data.l[2] = 0;

					xSendEvent(_rgfw.display, _rgfw.x11Source, 0, 0, &reply);
					xFlush(_rgfw.display);
				}
			}
			break;
		}
		case 31: // SelectionNotify
		{
			if (!(win.internal.enabledEvents & RGFW_DATA_DROP_FLAG) || e.xselection.property != _rgfw.xdndSelection || !(win.internal.flags & RGFW_WINDOW_ALLOW_DND)) return;

			ZString data;
			ulong result;
			Atom actualType;
			int actualFormat;
			ulong bytesAfter;

			xGetWindowProperty(_rgfw.display, e.xselection.requestor, e.xselection.property, 0, ~0, 0, e.xselection.target,
				&actualType, &actualFormat, &result, &bytesAfter, (char**)&data);

			if (result != 0)
			{
				rgfw_unix_parseURI(win, data);
				if (data != null) xFree(data);
			}

			if (_rgfw.x11Version >= 2)
			{
				XEvent reply;
				reply.type = 33; // ClientMessage
				reply.xclient.window = _rgfw.x11Source;
				reply.xclient.message_type = _rgfw.xdndFinished;
				reply.xclient.format = 32;
				reply.xclient.data.l[0] = (long)win.src.window;
				reply.xclient.data.l[1] = (long)result;
				reply.xclient.data.l[2] = (long)_rgfw.xdndActionCopy;
				xSendEvent(_rgfw.display, _rgfw.x11Source, 0, 0, &reply);
				xFlush(_rgfw.display);
			}
			break;
		}
		case 22: // ConfigureNotify
		{
			if (e.xconfigure.width != win.src.w || e.xconfigure.height != win.src.h)
			{
				rgfw_window_checkMode(win);
				win.src.w = e.xconfigure.width;
				win.src.h = e.xconfigure.height;
				_window_resized_callback(win, e.xconfigure.width, e.xconfigure.height);
			}
			int configureX = e.xconfigure.x;
			int configureY = e.xconfigure.y;

			// if the event came from the server and we're not a direct child of the root window then
			// we're using local coords which need to be translated into screen coords
			GfxXWindow root = xDefaultRootWindow(_rgfw.display);
			if (e.xany.send_event == 0 && win.src.parent != root)
			{
				GfxXWindow dummy;
				xTranslateCoordinates(_rgfw.display, win.src.parent, root, configureX, configureY, &configureX, &configureY, &dummy);
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

	xFlush(_rgfw.display);
}

fn IntVec2 rgfw_window_fetchSize(GfxWindow* win)
{
	XWindowAttributes attribs;
	xGetWindowAttributes(_rgfw.display, win.src.window, &attribs);
	win.size = { attribs.width, attribs.height };
	return win.get_size();
}

fn void rgfw_pollEvents()
{
	rgfw_resetPrevState();
	xPending(_rgfw.display);
	while (qLength(_rgfw.display) != 0) rgfw_xHandleEvent();
}

fn void rgfw_window_movePlatform(GfxWindow* win, int x, int y)
{
	win.x = x; win.y = y;
	xMoveWindow(_rgfw.display, win.src.window, x, y);
}

fn void rgfw_window_resizePlatform(GfxWindow* win, int w, int h)
{
	win.width = w; win.height = h;
	xResizeWindow(_rgfw.display, win.src.window, (uint)w, (uint)h);
	if (win.internal.flags & RGFW_WINDOW_NO_RESIZE)
	{
		XSizeHints sh;
		sh.flags = (1 << 4) | (1 << 5);
		sh.min_width = w; sh.max_width = w;
		sh.min_height = h; sh.max_height = h;
		xSetWMSizeHints(_rgfw.display, win.src.window, &sh, 0);
	}
}

fn void rgfw_window_setMinSize(GfxWindow* win, int w, int h)
{
	long flags;
	XSizeHints hints;
	xGetWMNormalHints(_rgfw.display, win.src.window, &hints, &flags);
	hints.flags |= 1 << 4; // PMinSize
	hints.min_width = w; hints.min_height = h;
	xSetWMNormalHints(_rgfw.display, win.src.window, &hints);
}

fn void rgfw_window_setMaxSize(GfxWindow* win, int w, int h)
{
	long flags;
	XSizeHints hints;
	xGetWMNormalHints(_rgfw.display, win.src.window, &hints, &flags);
	hints.flags |= 1 << 5; // PMaxSize
	hints.max_width = w; hints.max_height = h;
	xSetWMNormalHints(_rgfw.display, win.src.window, &hints);
}

fn void rgfw_toggleXMaximized(GfxWindow* win, bool maximized)
{
	XEvent xev;
	xev.type = 33; // ClientMessage
	xev.xclient.window = win.src.window;
	xev.xclient.message_type = _rgfw.netWmState;
	xev.xclient.format = 32;
	xev.xclient.data.l[0] = (int)maximized;
	xev.xclient.data.l[1] = (long)_rgfw.netWmStateMaximizedHorz;
	xev.xclient.data.l[2] = (long)_rgfw.netWmStateMaximizedVert;
	xSendEvent(_rgfw.display, xDefaultRootWindow(_rgfw.display), 0, (1 << 20) | (1 << 19), &xev);
}

fn void rgfw_window_maximizePlatform(GfxWindow* win) => rgfw_toggleXMaximized(win, true);

fn void rgfw_window_focus(GfxWindow* win)
{
	XWindowAttributes attr;
	xGetWindowAttributes(_rgfw.display, win.src.window, &attr);
	if (attr.map_state != 2 /* IsViewable */) return;
	xSetInputFocus(_rgfw.display, win.src.window, 1 /* RevertToPointerRoot */, 0);
	xFlush(_rgfw.display);
}

fn void rgfw_window_raise(GfxWindow* win)
{
	xMapRaised(_rgfw.display, win.src.window);
	rgfw_window_setFullscreen(win, win.is_fullscreen());
}

fn void rgfw_window_setXAtom(GfxWindow* win, Atom netAtom, bool fullscreen)
{
	XEvent xev;
	xev.xclient.type = 33;
	xev.xclient.send_event = 1;
	xev.xclient.message_type = _rgfw.netWmState;
	xev.xclient.window = win.src.window;
	xev.xclient.format = 32;
	xev.xclient.data.l[0] = (int)fullscreen;
	xev.xclient.data.l[1] = (long)netAtom;
	xSendEvent(_rgfw.display, xDefaultRootWindow(_rgfw.display), 0, (1 << 19) | (1 << 20), &xev);
}

fn void rgfw_window_setFullscreen(GfxWindow* win, bool fullscreen)
{
	if (fullscreen)
	{
		win.internal.flags |= RGFW_WINDOW_FULLSCREEN;
		win.internal.old_bounds = win.bounds;
	}
	else { win.internal.flags &= ~(uint)RGFW_WINDOW_FULLSCREEN; }

	xRaiseWindow(_rgfw.display, win.src.window);
	rgfw_window_setXAtom(win, _rgfw.netWmStateFullscreen, fullscreen);

	if (!(win.internal.flags & RGFW_WINDOW_TRANSPARENT))
	{
		char value = (char)fullscreen;
		xChangeProperty(_rgfw.display, win.src.window, _rgfw.netWmBypassCompositor, 6 /* XA_CARDINAL */, 32, 0, &value, 1);
	}
}

fn void rgfw_window_setFloating(GfxWindow* win, bool floating) => rgfw_window_setXAtom(win, _rgfw.netWmStateAbove, floating);

fn void rgfw_window_setOpacity(GfxWindow* win, char opacity)
{
	uint value = (uint)(4294967295.0 * (double)opacity);
	xChangeProperty(_rgfw.display, win.src.window, _rgfw.netWmWindowOpacity, 6, 32, 0, (char*)&value, 1);
}

fn void rgfw_window_minimizePlatform(GfxWindow* win)
{
	if (rgfw_window_isMaximized(win)) return;
	win.internal.old_bounds = win.bounds;
	xIconifyWindow(_rgfw.display, win.src.window, xDefaultScreen(_rgfw.display));
	xFlush(_rgfw.display);
}

fn void rgfw_window_restorePlatform(GfxWindow* win)
{
	rgfw_toggleXMaximized(win, false);
	xFlush(_rgfw.display);
}

fn bool GfxWindow.is_floating(&win)
{
	Atom actualType;
	int actualFormat;
	ulong nitems, bytesAfter;
	Atom* propReturn = null;
	int status = xGetWindowProperty(_rgfw.display, win.src.window, _rgfw.netWmState, 0, ~0, 0, 4, &actualType, &actualFormat, &nitems, &bytesAfter, (char**)&propReturn);
	if (status != 0 || actualType != 4) return false;
	for (ulong i = 0; i < nitems; i++) if (propReturn[i] == _rgfw.netWmStateAbove) return true;
	if (propReturn) xFree(propReturn);
	return false;
}

fn void rgfw_window_setName(GfxWindow* win, ZString name)
{
	if (name == null) name = "";
	xutf8SetWMProperties(_rgfw.display, win.src.window, name, name, null, 0, null, null, null);
	xStoreName(_rgfw.display, win.src.window, name);
	xChangeProperty(_rgfw.display, win.src.window, _rgfw.netWmName, _rgfw.utf8String, 8, 0, name, (int)name.len());
}

fn void rgfw_window_setMousePassthrough(GfxWindow* win, bool passthrough)
{
	if (passthrough)
	{
		Region region = xCreateRegion();
		if (rgfw_xShapeCombineRegionSRC != null) rgfw_xShapeCombineRegionSRC(_rgfw.display, win.src.window, 2, 0, 0, region, 0);
		xDestroyRegion(region);
		return;
	}
	if (rgfw_xShapeCombineMaskSRC != null) rgfw_xShapeCombineMaskSRC(_rgfw.display, win.src.window, 2, 0, 0, 0, 0);
}

fn bool GfxWindow.set_icon(&win, char* dataSrc, int w, int h, GfxFormat format, GfxWindowIconType type = BOTH)
{
	if (!dataSrc)
	{
		return !!xChangeProperty(_rgfw.display, win.src.window, _rgfw.netWmIcon, 6, 32, 0, null, 0);
	}
	int count = 2 + (w * h);
	ulong* data = (ulong*)rgfw_alloc_impl((sz)count * ulong::size);
	mem::clear(data, (sz)count * ulong::size);
	data[0] = (ulong)w;
	data[1] = (ulong)h;

	copy_image_data_64((char*)&data[2], w, h, BGRA8, dataSrc, format, true, null);
	bool res = true;
	if (type & TASKBAR)
	{
		res = !!xChangeProperty(_rgfw.display, win.src.window, _rgfw.netWmIcon, 6, 32, 0, (char*)data, count);
	}

	copy_image_data_64((char*)&data[2], w, h, BGRA8, dataSrc, format, false, null);

	if (type & WINDOW)
	{
		XWMHints wmHints;
		wmHints.flags = 1 << 2; // IconPixmapHint

		int depth = xDefaultDepth(_rgfw.display, xDefaultScreen(_rgfw.display));
		void* image = xCreateImage(_rgfw.display, xDefaultVisual(_rgfw.display, xDefaultScreen(_rgfw.display)),
			(uint)depth, 2 /* ZPixmap */, 0, (char*)&data[2], (uint)w, (uint)h, 32, 0);

		wmHints.icon_pixmap = xCreatePixmap(_rgfw.display, win.src.window, (uint)w, (uint)h, (uint)depth);
		xPutImage(_rgfw.display, wmHints.icon_pixmap, xDefaultGC(_rgfw.display, xDefaultScreen(_rgfw.display)), image, 0, 0, 0, 0, (uint)w, (uint)h);
		((XImageHeader*)image).data = null;
		xDestroyImage(image);

		xSetWMHints(_rgfw.display, win.src.window, &wmHints);
	}

	rgfw_free_impl(data);
	xFlush(_rgfw.display);
	return res;
}

fn RgfwMouse* rgfw_createMouseStandard(RgfwMouseIcon mouse)
{
	uint mouseIcon = 0;
	switch (mouse)
	{
		case RGFW_MOUSE_NORMAL: mouseIcon = 68; // XC_left_ptr
		case RGFW_MOUSE_ARROW: mouseIcon = 68;
		case RGFW_MOUSE_IBEAM: mouseIcon = 152; // XC_xterm
		case RGFW_MOUSE_WAIT: mouseIcon = 150; // XC_watch
		case RGFW_MOUSE_CROSSHAIR: mouseIcon = 130; // XC_tcross
		case RGFW_MOUSE_PROGRESS: mouseIcon = 150;
		case RGFW_MOUSE_RESIZE_NWSE: mouseIcon = 134;
		case RGFW_MOUSE_RESIZE_NESW: mouseIcon = 136;
		case RGFW_MOUSE_RESIZE_EW: mouseIcon = 108;
		case RGFW_MOUSE_RESIZE_NS: mouseIcon = 116;
		case RGFW_MOUSE_RESIZE_NW: mouseIcon = 134;
		case RGFW_MOUSE_RESIZE_N: mouseIcon = 138;
		case RGFW_MOUSE_RESIZE_NE: mouseIcon = 136;
		case RGFW_MOUSE_RESIZE_E: mouseIcon = 96;
		case RGFW_MOUSE_RESIZE_SE: mouseIcon = 14;
		case RGFW_MOUSE_RESIZE_S: mouseIcon = 16;
		case RGFW_MOUSE_RESIZE_SW: mouseIcon = 12;
		case RGFW_MOUSE_RESIZE_W: mouseIcon = 70;
		case RGFW_MOUSE_RESIZE_ALL: mouseIcon = 52; // XC_fleur
		case RGFW_MOUSE_NOT_ALLOWED: mouseIcon = 88; // XC_pirate
		case RGFW_MOUSE_POINTING_HAND: mouseIcon = 60; // XC_hand2
		default: return null;
	}
	XCursor cursor = xCreateFontCursor(_rgfw.display, mouseIcon);
	return (RgfwMouse*)cursor;
}

fn RgfwMouse* rgfw_createMouse(char* data, int w, int h, GfxFormat format)
{
	if (rgfw_xcursorImageCreateSRC == null) return null;
	XcursorImage* native = rgfw_xcursorImageCreateSRC(w, h);
	native.xhot = 0; native.yhot = 0;
	mem::clear(native.pixels, (sz)(w * h * 4));
	rgfw_memcpy(native.pixels, data, (sz)(w * h * 4));
	XCursor cursor = rgfw_xcursorImageLoadCursorSRC(_rgfw.display, native);
	rgfw_xcursorImageDestroySRC(native);
	return (RgfwMouse*)cursor;
}

fn bool rgfw_window_setMousePlatform(GfxWindow* win, RgfwMouse* mouse)
{
	xDefineCursor(_rgfw.display, win.src.window, (XCursor)mouse);
	return true;
}

fn void rgfw_freeMouse(RgfwMouse* mouse) { xFreeCursor(_rgfw.display, (XCursor)mouse); }

fn void rgfw_window_moveMouse(GfxWindow* win, int x, int y)
{
	int rootX, rootY, winX, winY;
	uint mask;
	GfxXWindow rootRet, childRet;
	xQueryPointer(_rgfw.display, xDefaultRootWindow(_rgfw.display), &rootRet, &childRet, &rootX, &rootY, &winX, &winY, &mask);
	win.internal.last_mouse = { x - win.x, y - win.y };
	if (winX == x && winY == y) return;
	xWarpPointer(_rgfw.display, 0, win.src.window, 0, 0, 0, 0, x - win.x, y - win.y);
}

fn void rgfw_window_hide(GfxWindow* win)
{
	win.internal.flags |= (uint)RGFW_WINDOW_HIDE;
	xUnmapWindow(_rgfw.display, win.src.window);
	xFlush(_rgfw.display);
}

fn void rgfw_window_showPlatform(GfxWindow* win)
{
	win.internal.flags &= ~(uint)RGFW_WINDOW_HIDE;
	if (rgfw_window_isHidden(win) == false) return;
	xMapWindow(_rgfw.display, win.src.window);
	rgfw_window_move(win, win.x, win.y);

	XEvent dummy;
	while (true)
	{
		// XCheckTypedWindowEvent(display, window, VisibilityNotify, &dummy)
		if (xPending(_rgfw.display) == 0) break;
		xNextEvent(_rgfw.display, &dummy);
		if (dummy.type == 15 /* VisibilityNotify */ && dummy.xany.window == win.src.window) break;
	}
	rgfw_window_setFullscreen(win, win.is_fullscreen());
}

fn void rgfw_window_flash(GfxWindow* win, GfxFlashRequest request)
{
	if (win.has_focus() && request != CANCEL) return;
	XWMHints* wmhints = xGetWMHints(_rgfw.display, win.src.window);
	if (wmhints == null) return;
	if (request != CANCEL)
	{
		wmhints.flags |= 1 << 8; // XUrgencyHint
		if (request == BRIEFLY) win.src.flashEnd = rgfw_unix_getTimeNS() + (ulong)1e9;
		if (request == UNTIL_FOCUSED) win.src.flashEnd = (ulong)-1;
	}
	else
	{
		win.src.flashEnd = 0;
		wmhints.flags &= ~(1 << 8);
	}
	xSetWMHints(_rgfw.display, win.src.window, wmhints);
	xFree(wmhints);
}

fn bool rgfw_readClipboardPtr(RgfwDataTransferType requestedType, char* buffer, sz capacity, GfxDataTransfer* dataTransfer)
{
	if (requestedType != RGFW_DATA_TEXT) return false;
	dataTransfer.data = buffer;

	if (xGetSelectionOwner(_rgfw.display, _rgfw.clipboardAtom) == _rgfw.helperWindow)
	{
		dataTransfer.length = _rgfw.unixClipboard.length;
		if (buffer != null && _rgfw.unixClipboard.data != null)
		{
			if (_rgfw.unixClipboard.length > capacity) return false;
			rgfw_memcpy(buffer, (void*)_rgfw.unixClipboard.data, _rgfw.unixClipboard.length);
		}
		dataTransfer.type = RGFW_DATA_TEXT;
		return true;
	}

	xConvertSelection(_rgfw.display, _rgfw.clipboardAtom, _rgfw.utf8String, _rgfw.xselData, _rgfw.helperWindow, 0);
	xSync(_rgfw.display, 0);
	XEvent event;
	while (true)
	{
		xNextEvent(_rgfw.display, &event);
		if (event.type != 31 /* SelectionNotify */) continue;
		if (event.xselection.selection != _rgfw.clipboardAtom || event.xselection.property == 0) return false;
		break;
	}

	Atom target;
	int format;
	CULong n, size;
	char* data;
	xGetWindowProperty(_rgfw.display, event.xselection.requestor, event.xselection.property, 0, ~0, 0, 0, &target, &format, &size, &n, (char**)&data);

	bool ret = true;
	sz length = (sz)size;
	if (data[size - 1] != 0) length += 1;

	if ((sz)size > capacity && buffer != null) { ret = false; }
	else if ((target == _rgfw.utf8String || target == 31) && buffer != null)
	{
		rgfw_memcpy(buffer, data, (sz)size);
		buffer[length - 1] = 0;
		xFree(data);
	}
	else if (buffer != null) { ret = false; }

	xDeleteProperty(_rgfw.display, event.xselection.requestor, event.xselection.property);
	dataTransfer.length = length;
	dataTransfer.type = RGFW_DATA_TEXT;
	return ret;
}

fn bool rgfw_writeClipboard(GfxDataTransfer* data)
{
	xSetSelectionOwner(_rgfw.display, _rgfw.clipboardAtom, _rgfw.helperWindow, 0);
	if (xGetSelectionOwner(_rgfw.display, _rgfw.clipboardAtom) != _rgfw.helperWindow)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_CLIPBOARD, "X11 failed to become owner of clipboard selection");
		return false;
	}
	if (_rgfw.unixClipboard != null) { rgfw_free_impl(_rgfw.unixClipboard); _rgfw.unixClipboard = null; }

	sz length = data.length;
	if (data.data[data.length - 1] != 0) length += 1;

	_rgfw.unixClipboard = (GfxDataTransfer*)rgfw_alloc_impl(GfxDataTransfer::size + data.length);
	char* dataPtr = (char*)_rgfw.unixClipboard + GfxDataTransfer::size - 1;
	rgfw_memcpy(dataPtr, (void*)data.data, data.length);
	dataPtr[length - 1] = 0;
	_rgfw.unixClipboard.data = dataPtr;
	_rgfw.unixClipboard.type = RGFW_DATA_TEXT;
	_rgfw.unixClipboard.length = length;
	return true;
}

fn bool rgfw_window_isHidden(GfxWindow* win)
{
	XWindowAttributes windowAttributes;
	xGetWindowAttributes(_rgfw.display, win.src.window, &windowAttributes);
	return windowAttributes.map_state != 2;
}

fn bool rgfw_window_isMinimized(GfxWindow* win)
{
	Atom actualType;
	int actualFormat;
	ulong nitems, bytesAfter;
	char* propData;
	int status = xGetWindowProperty(_rgfw.display, win.src.window, _rgfw.wmState, 0, 2, 0, 0, &actualType, &actualFormat, &nitems, &bytesAfter, &propData);
	if (status == 0 && nitems >= 1 && propData == (char*)(uptr)3 /* IconicState */)
	{
		xFree(propData);
		return true;
	}
	if (propData != null) xFree(propData);
	XWindowAttributes windowAttributes;
	xGetWindowAttributes(_rgfw.display, win.src.window, &windowAttributes);
	return windowAttributes.map_state != 2;
}

fn bool rgfw_window_isMaximized(GfxWindow* win)
{
	Atom actualType;
	int actualFormat;
	ulong nitems, bytesAfter;
	char* propData;
	int status = xGetWindowProperty(_rgfw.display, win.src.window, _rgfw.netWmState, 0, 1024, 0, 4, &actualType, &actualFormat, &nitems, &bytesAfter, &propData);
	if (status != 0) { if (propData != null) xFree(propData); return false; }
	for (ulong i = 0; i < nitems; i++)
	{
		if ((Atom)propData[i] == _rgfw.netWmStateMaximizedVert || (Atom)propData[i] == _rgfw.netWmStateMaximizedHorz)
		{
			xFree(propData);
			return true;
		}
	}
	if (propData != null) xFree(propData);
	return false;
}

fn void rgfw_xGetSystemContentDPI(float* dpi)
{
	if (dpi == null) return;
	float dpiOutput = 96.0f;
	ZString rms = xResourceManagerString(_rgfw.display);
	if (rms == null) return;
	void* db = xrmGetStringDatabase(rms);
	if (db == null) return;
	ZString type = null;
	// xrmGetResource's value-out parameter is a small {len,addr} struct; omitted here.
	xrmDestroyDatabase(db);
	*dpi = dpiOutput;
}

fn void* rgfw_xGetMode(XRRCrtcInfo* ci, XRRScreenResources* res, RRMode mode, GfxMonitorMode* foundMode)
{
	XRRModeInfoNative* mi = null;
	for (int j = 0; j < res.nmode; j++) if (res.modes[j].id == mode) mi = &res.modes[j];
	if (mi == null) return null;

	foundMode.w = (int)mi.width;
	foundMode.h = (int)mi.height;
	if (ci.rotation == 4 || ci.rotation == 8) // RR_Rotate_90 / RR_Rotate_270
	{
		foundMode.w = (int)mi.height;
		foundMode.h = (int)mi.width;
	}

	rgfw_splitBPP((uint)xDefaultDepth(_rgfw.display, xDefaultScreen(_rgfw.display)), foundMode);
	foundMode.src = (void*)(uptr)mode;
	foundMode.refresh_rate = 0;
	if (mi.hTotal == 0 || mi.vTotal == 0) return mi;

	uint vTotal = mi.vTotal;
	int numerator = (int)mi.dotClock;
	int denominator = (int)(mi.hTotal * vTotal);
	if (denominator <= 0) denominator = 1;
	float refresh_rate = (float)numerator / (float)denominator;
	foundMode.refresh_rate = (float)((int)(refresh_rate * 100 + 0.5f)) / 100.0f;
	return mi;
}

fn void rgfw_pollMonitors()
{
	GfxXWindow root = xDefaultRootWindow(_rgfw.display);
	XRRScreenResources* res = xRRGetScreenResourcesCurrent(_rgfw.display, root);
	if (res == null) return;
	GfxRROutput primary = xRRGetOutputPrimary(_rgfw.display, root);

	for (GfxMonitorNode* node = _rgfw.monitors.list.head; node != null; node = node.next) node.disconnected = true;

	for (int i = 0; i < res.noutput; i++)
	{
		GfxMonitorNode* node = null;
		for (node = _rgfw.monitors.list.head; node != null; node = node.next)
		{
			if (node.nativeId0 == res.outputs[i]) break;
		}
		if (node != null)
		{
			node.disconnected = false;
			if (node.nativeId0 == primary) _rgfw.monitors.primary = node;
			continue;
		}

		GfxMonitor monitor;
		GfxXRROutputInfo* info = xRRGetOutputInfo(_rgfw.display, res, res.outputs[i]);
		if (info == null) continue;
		if (info.connection != 0 || info.crtc == 0) { xRRFreeOutputInfo(info); continue; }

		XRRCrtcInfo* ci = xRRGetCrtcInfo(_rgfw.display, res, info.crtc);
		if (ci == null) continue;

		float physW = (float)info.mm_width / 25.4f;
		float physH = (float)info.mm_height / 25.4f;

		sz n = info.name.len();
		if (n > 127) n = 127;
		rgfw_memcpy(&monitor.name, info.name, n);
		monitor.name[n] = 0;

		if (physW > 0.0f && physH > 0.0f)
		{
			monitor.phys_size = { physW, physH };
		}
		else
		{
			monitor.phys_size = { ci.width / 96.0f, ci.height / 96.0f };
		}

		monitor.origin = { ci.x, ci.y };

		float dpi = 96.0f;
		rgfw_xGetSystemContentDPI(&dpi);
		monitor.scale = (Vec2)(dpi / 96.0f);
		monitor.pixelRatio = dpi >= 192.0f ? 2.0f : 1.0f;

		void* mi = rgfw_xGetMode(ci, res, ci.mode, &monitor.mode);
		if (mi == null) break;

		xRRFreeCrtcInfo(ci);
		node = rgfw_monitors_add(&monitor);
		if (node == null) break;
		node.nativeId0 = res.outputs[i];
		node.nativeId1 = info.crtc;
		if (node.nativeId0 == primary) _rgfw.monitors.primary = node;
		xRRFreeOutputInfo(info);
		rgfw_monitorCallback(_rgfw.root, &node.mon, true);
	}

	xRRFreeScreenResources(res);
	rgfw_monitors_refresh();
}

fn void rgfw_monitorNode_free(GfxMonitorNode* node) {}

fn bool rgfw_monitor_getWorkarea(GfxMonitor* monitor, int* x, int* y, int* width, int* height)
{
	GfxXWindow root = xDefaultRootWindow(_rgfw.display);
	int areaX = monitor.origin.x;
	int areaY = monitor.origin.y;
	int areaW = monitor.mode.w;
	int areaH = monitor.mode.h;

	if (_rgfw.netWorkarea != 0 && _rgfw.netCurrentDesktop != 0)
	{
		Atom* extents = null;
		Atom* desktop = null;
		Atom actualType; int actualFormat; ulong extentCount, bytesAfter, count;
		xGetWindowProperty(_rgfw.display, root, _rgfw.netWorkarea, 0, ~0, 0, 6, &actualType, &actualFormat, &extentCount, &bytesAfter, (char**)&extents);
		xGetWindowProperty(_rgfw.display, root, _rgfw.netCurrentDesktop, 0, ~0, 0, 6, &actualType, &actualFormat, &count, &bytesAfter, (char**)&desktop);

		if (count != 0 && extentCount >= 4 && *desktop < extentCount / 4)
		{
			int globalX = (int)extents[*desktop * 4 + 0];
			int globalY = (int)extents[*desktop * 4 + 1];
			int globalW = (int)extents[*desktop * 4 + 2];
			int globalH = (int)extents[*desktop * 4 + 3];
			if (areaX < globalX) { areaW -= globalX - areaX; areaX = globalX; }
			if (areaY < globalY) { areaH -= globalY - areaY; areaY = globalY; }
			if (areaX + areaW > globalX + globalW) areaW = globalX - areaX + globalW;
			if (areaY + areaH > globalY + globalH) areaH = globalY - areaY + globalH;
		}
		if (extents != null) xFree(extents);
		if (desktop != null) xFree(desktop);
	}

	if (x != null) *x = areaX;
	if (y != null) *y = areaY;
	if (width != null) *width = areaW;
	if (height != null) *height = areaH;
	return true;
}

fn GfxMonitorMode[] rgfw_monitor_getModesPtr(GfxMonitor* monitor)
{
	XRRScreenResources* res = xRRGetScreenResourcesCurrent(_rgfw.display, xDefaultRootWindow(_rgfw.display));
	if (!res) return {};
	XRRCrtcInfo* ci = xRRGetCrtcInfo(_rgfw.display, res, monitor.node.nativeId1);
	GfxXRROutputInfo* oi = xRRGetOutputInfo(_rgfw.display, res, monitor.node.nativeId0);
	sz count = (sz)oi.nmode;
	List{GfxMonitorMode} modes;
	for (int i = 0; i < oi.nmode; i++)
	{
		GfxMonitorMode mode;
		rgfw_xGetMode(ci, res, oi.modes[i], &mode);
		modes.push(mode);
	}
	xRRFreeOutputInfo(oi);
	xRRFreeCrtcInfo(ci);
	xRRFreeScreenResources(res);
	return modes.array_view();
}

fn sz rgfw_monitor_getGammaRampPtr(GfxMonitor* monitor, RgfwGammaRamp* ramp)
{
	sz size = (sz)xRRGetCrtcGammaSize(_rgfw.display, monitor.node.nativeId1);
	XRRCrtcGamma* gamma = xRRGetCrtcGamma(_rgfw.display, monitor.node.nativeId1);
	if (ramp != null)
	{
		rgfw_memcpy(ramp.red, gamma.red, size * ushort::size);
		rgfw_memcpy(ramp.green, gamma.green, size * ushort::size);
		rgfw_memcpy(ramp.blue, gamma.blue, size * ushort::size);
	}
	xRRFreeGamma(gamma);
	return size;
}

fn bool rgfw_monitor_setGammaRamp(GfxMonitor* monitor, RgfwGammaRamp* ramp)
{
	sz size = (sz)xRRGetCrtcGammaSize(_rgfw.display, monitor.node.nativeId1);
	if (size != ramp.count)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_X11, "X11: Gamma ramp size must match current ramp size");
		return false;
	}
	XRRCrtcGamma* gamma = xRRAllocGamma((int)ramp.count);
	rgfw_memcpy(gamma.red, ramp.red, ramp.count * ushort::size);
	rgfw_memcpy(gamma.green, ramp.green, ramp.count * ushort::size);
	rgfw_memcpy(gamma.blue, ramp.blue, ramp.count * ushort::size);
	xRRSetCrtcGamma(_rgfw.display, monitor.node.nativeId1, gamma);
	xRRFreeGamma(gamma);
	return true;
}

fn bool rgfw_monitor_setMode(GfxMonitor* mon, GfxMonitorMode* mode)
{
	bool out = false;
	XRRScreenResources* res = xRRGetScreenResourcesCurrent(_rgfw.display, xDefaultRootWindow(_rgfw.display));
	XRRCrtcInfo* ci = xRRGetCrtcInfo(_rgfw.display, res, mon.node.nativeId1);
	if (xRRSetCrtcConfig(_rgfw.display, res, mon.node.nativeId1, 0, ci.x, ci.y, (RRMode)(uptr)mode.src, ci.rotation, ci.outputs, ci.noutput) == 0) out = true;
	xRRFreeCrtcInfo(ci);
	xRRFreeScreenResources(res);
	return out;
}

fn bool rgfw_monitor_requestMode(GfxMonitor* mon, GfxMonitorMode* mode, GfxMonitorModeRequest request)
{
	bool output = false;
	XRRScreenResources* res = xRRGetScreenResourcesCurrent(_rgfw.display, xDefaultRootWindow(_rgfw.display));
	if (res == null) return false;
	XRRCrtcInfo* ci = xRRGetCrtcInfo(_rgfw.display, res, mon.node.nativeId1);
	GfxXRROutputInfo* oi = xRRGetOutputInfo(_rgfw.display, res, mon.node.nativeId0);
	RRMode native = 0;

	for (int i = 0; i < oi.nmode; i++)
	{
		GfxMonitorMode foundMode;
		void* mi = rgfw_xGetMode(ci, res, oi.modes[i], &foundMode);
		if (mi == null) continue;
		if (rgfw_monitorModeCompare(mode, &foundMode, request))
		{
			native = ((XRRModeInfoNative*)mi).id;
			output = true;
			mon.mode = foundMode;
			break;
		}
	}
	if (native != 0) xRRSetCrtcConfig(_rgfw.display, res, mon.node.nativeId1, 0, ci.x, ci.y, native, ci.rotation, ci.outputs, ci.noutput);
	xRRFreeOutputInfo(oi);
	xRRFreeCrtcInfo(ci);
	xRRFreeScreenResources(res);
	return output;
}

fn GfxMonitor* find_monitor(GfxWindow* win)
{
	XWindowAttributes attrs;
	if (xGetWindowAttributes(_rgfw.display, win.src.window, &attrs) == 0) return null;
	for (GfxMonitorNode* node = _rgfw.monitors.list.head; node != null; node = node.next)
	{
		if ((attrs.x < node.mon.origin.x + node.mon.mode.w) && (attrs.x + attrs.width > node.mon.origin.x) &&
			(attrs.y < node.mon.origin.y + node.mon.mode.h) && (attrs.y + attrs.height > node.mon.origin.y))
		{
			return &node.mon;
		}
	}
	return _rgfw.monitors.list.head != null ? &_rgfw.monitors.list.head.mon : null;
}

fn ulong rgfw_unix_getTimeNS()
{
	Timespec ts;
	clock_gettime(1 /* CLOCK_MONOTONIC */, &ts);
	return (ulong)ts.tv_sec * 1000000000 + (ulong)ts.tv_nsec;
}

fn bool rgfw_extensionSupportedPlatform_OpenGL(ZString extension, sz len)
{
	ZString extensions = rgfw_glXQueryExtensionsString(_rgfw.display, xDefaultScreen(_rgfw.display));
	return extensions && rgfw_extensionSupportedStr(extensions, extension, len);
}

fn GfxFn rgfw_getProcAddress_OpenGL(ZString procname)
{
	if (_rgfw.glGetProcAddress == null) return null;
	return ((GlxGetProcAddressProc)_rgfw.glGetProcAddress)(procname);
}

fn bool rgfw_window_createContextPtr_OpenGL(GfxWindow* win, RgfwGlContext* context, RgfwGlHints* hints)
{
	win.src.ctx.native = context;
	win.src.gfxType = RGFW_GFX_NATIVE_OPEN_GL;

	bool showWindow = false;
	if (win.src.window != 0)
	{
		showWindow = !rgfw_window_isMinimized(win);
		rgfw_window_closePlatform(win);
	}

	bool transparent = win.internal.flags & RGFW_WINDOW_TRANSPARENT != 0;

	int[40] visualAttribs;
	RgfwAttribStack stack;
	rgfw_attribStack_init(&stack, (int*)&visualAttribs, 40);
	rgfw_attribStack_pushAttribs(&stack, 0x22, 0x8002); // GLX_X_VISUAL_TYPE, GLX_TRUE_COLOR
	rgfw_attribStack_pushAttribs(&stack, 0x8012, 1); // GLX_X_RENDERABLE
	rgfw_attribStack_pushAttribs(&stack, 0x8011, 0x1); // GLX_RENDER_TYPE, GLX_RGBA_BIT
	rgfw_attribStack_pushAttribs(&stack, 0x8010, 0x1); // GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT
	rgfw_attribStack_pushAttribs(&stack, 5, 1); // GLX_DOUBLEBUFFER
	rgfw_attribStack_pushAttribs(&stack, 11, hints.alpha); // GLX_ALPHA_SIZE
	rgfw_attribStack_pushAttribs(&stack, 12, hints.depth); // GLX_DEPTH_SIZE
	rgfw_attribStack_pushAttribs(&stack, 13, hints.stencil); // GLX_STENCIL_SIZE
	rgfw_attribStack_pushAttribs(&stack, 6, hints.stereo); // GLX_STEREO
	rgfw_attribStack_pushAttribs(&stack, 7, hints.auxBuffers); // GLX_AUX_BUFFERS
	rgfw_attribStack_pushAttribs(&stack, 8, hints.red); // GLX_RED_SIZE
	rgfw_attribStack_pushAttribs(&stack, 9, hints.green); // GLX_GREEN_SIZE
	rgfw_attribStack_pushAttribs(&stack, 10, hints.blue); // GLX_BLUE_SIZE
	rgfw_attribStack_pushAttribs(&stack, 14, hints.accumRed); // GLX_ACCUM_RED_SIZE
	rgfw_attribStack_pushAttribs(&stack, 15, hints.accumGreen);
	rgfw_attribStack_pushAttribs(&stack, 16, hints.accumBlue);
	rgfw_attribStack_pushAttribs(&stack, 17, hints.accumAlpha);

	if (hints.sRGB)
	{
		ZString srgbArbStr = "GLX_ARB_framebuffer_sRGB";
		ZString srgbExtStr = "GLX_EXT_framebuffer_sRGB";
		if (rgfw_extensionSupportedPlatform_OpenGL(srgbArbStr, srgbArbStr.len() + 1)) rgfw_attribStack_pushAttribs(&stack, 0x20B2, (int)hints.sRGB); // GLX_FRAMEBUFFER_SRGB_CAPABLE_ARB
		if (rgfw_extensionSupportedPlatform_OpenGL(srgbExtStr, srgbExtStr.len() + 1)) rgfw_attribStack_pushAttribs(&stack, 0x20B2, (int)hints.sRGB); // GLX_FRAMEBUFFER_SRGB_CAPABLE_EXT
	}

	rgfw_attribStack_pushAttribs(&stack, 0, 0);

	int fbcount;
	GLXFBConfig* fbc = rgfw_glXChooseFBConfig(_rgfw.display, xDefaultScreen(_rgfw.display), (int*)&visualAttribs, &fbcount);
	if (fbc == null || fbcount == 0)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_OPEN_GL_CONTEXT, "Failed to find any valid GLX visual configs.");
		return false;
	}

	int bestFbcIdx = -1;
	int bestDepth = 0;
	int bestSamples = 0;
	for (int i = 0; i < fbcount; i++)
	{
		XVisualInfo* vi = rgfw_glXGetVisualFromFBConfig(_rgfw.display, fbc[i]);
		if (vi == null) continue;
		int sampBuf, samples;
		rgfw_glXGetFBConfigAttrib(_rgfw.display, fbc[i], 100000, &sampBuf); // GLX_SAMPLE_BUFFERS
		rgfw_glXGetFBConfigAttrib(_rgfw.display, fbc[i], 100001, &samples); // GLX_SAMPLES
		if (bestFbcIdx == -1) bestFbcIdx = i;
		if ((!transparent || vi.depth == 32) && bestDepth == 0) { bestFbcIdx = i; bestDepth = vi.depth; }
		if ((!transparent || vi.depth == 32) && samples <= hints.samples && samples > bestSamples) { bestFbcIdx = i; bestDepth = vi.depth; bestSamples = samples; }
		xFree(vi);
	}
	if (bestFbcIdx == -1)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_OPEN_GL_CONTEXT, "Failed to get a valid GLX visual.");
		return false;
	}

	GLXFBConfig bestFbc = fbc[bestFbcIdx];
	XVisualInfo* vi = rgfw_glXGetVisualFromFBConfig(_rgfw.display, bestFbc);
	xFree(fbc);
	XVisualInfo visual = *vi;
	xFree(vi);

	if (rgfw_xCreateWindow(visual, "", win.internal.flags, win) == false) return false;
	if (showWindow) show(win);

	int[40] contextAttribs;
	rgfw_attribStack_init(&stack, (int*)&contextAttribs, 40);
	int mask = 0;
	switch (hints.profile)
	{
		case RGFW_GL_ES: mask |= 0x4;
		case RGFW_GL_FORWARD_COMPATIBILITY: mask |= 0x2;
		case RGFW_GL_COMPATIBILITY: mask |= 0x2;
		default: mask |= 0x1; // GLX_CONTEXT_CORE_PROFILE_BIT_ARB
	}
	rgfw_attribStack_pushAttribs(&stack, 0x9126, mask); // GLX_CONTEXT_PROFILE_MASK_ARB
	if (hints.minor != 0 || hints.major != 0)
	{
		rgfw_attribStack_pushAttribs(&stack, 0x2091, hints.major);
		rgfw_attribStack_pushAttribs(&stack, 0x2092, hints.minor);
	}

	ZString flushStr = "GLX_ARB_context_flush_control";
	if (rgfw_extensionSupportedPlatform_OpenGL(flushStr, flushStr.len() + 1))
	{
		if (hints.releaseBehavior == RGFW_GL_RELEASE_FLUSH) { rgfw_attribStack_pushAttribs(&stack, 0x2097, 0x2098); } // GLX_CONTEXT_RELEASE_BEHAVIOR_ARB, GLX_CONTEXT_RELEASE_BEHAVIOR_FLUSH_ARB
		else if (hints.releaseBehavior == RGFW_GL_RELEASE_NONE) { rgfw_attribStack_pushAttribs(&stack, 0x2097, 0); } // GLX_CONTEXT_RELEASE_BEHAVIOR_ARB, GLX_CONTEXT_RELEASE_BEHAVIOR_NONE_ARB
	}

	ZString robustStr = "GLX_ARB_create_context_robustness";
	int glFlags = 0;
	if (hints.debug) glFlags |= 0x00000001; // GLX_CONTEXT_DEBUG_BIT_ARB
	if (hints.robustness && rgfw_extensionSupportedPlatform_OpenGL(robustStr, robustStr.len() + 1)) glFlags |= 0x00000004; // GLX_CONTEXT_ROBUST_ACCESS_BIT_ARB
	if (glFlags != 0) rgfw_attribStack_pushAttribs(&stack, 0x2094, glFlags); // GLX_CONTEXT_FLAGS_ARB

	ZString noErrorStr = "GLX_ARB_create_context_no_error";
	if (rgfw_extensionSupportedPlatform_OpenGL(noErrorStr, noErrorStr.len() + 1)) rgfw_attribStack_pushAttribs(&stack, 0x31B3, (int)hints.noError); // GLX_CONTEXT_OPENGL_NO_ERROR_ARB

	rgfw_attribStack_pushAttribs(&stack, 0, 0);

	GLXContext ctx = null;
	if (hints.share != null) ctx = hints.share.ctx;

	if (rgfw_glXCreateContextAttribsARB == null)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_OPEN_GL_CONTEXT, "Failed to load proc address 'glXCreateContextAttribsARB', loading a generic OpenGL context.");
		win.src.ctx.native.ctx = rgfw_glXCreateContext(_rgfw.display, &visual, ctx, X_TRUE);
	}
	else
	{
		_rgfw.x11Error = null;
		win.src.ctx.native.ctx = rgfw_glXCreateContextAttribsARB(_rgfw.display, bestFbc, ctx, X_TRUE, (int*)&contextAttribs);
		if (_rgfw.x11Error != null || win.src.ctx.native.ctx == null)
		{
			rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_OPEN_GL_CONTEXT, "Failed to create an OpenGL context with AttribsARB, loading a generic OpenGL context.");
			win.src.ctx.native.ctx = rgfw_glXCreateContext(_rgfw.display, &visual, ctx, X_TRUE);
		}
	}

	win.src.ctx.native.window = rgfw_glXCreateWindow(_rgfw.display, bestFbc, win.src.window, null);
	rgfw_glXMakeCurrent(_rgfw.display, win.src.ctx.native.window, win.src.ctx.native.ctx);
	rgfw_debugCallback(RGFW_TYPE_INFO, RGFW_INFO_OPEN_GL, "OpenGL context initalized.");
	rgfw_window_swapInterval_OpenGL(win, 0);
	return true;
}

fn void rgfw_window_deleteContextPtr_OpenGL(GfxWindow* win, RgfwGlContext* ctx)
{
	if (win.src.ctx.native.window != win.src.window) rgfw_glXDestroyWindow(_rgfw.display, win.src.ctx.native.window);
	rgfw_glXDestroyContext(_rgfw.display, ctx.ctx);
	win.src.ctx.native = null;
	rgfw_debugCallback(RGFW_TYPE_INFO, RGFW_INFO_OPEN_GL, "OpenGL context freed.");
}

fn void rgfw_window_makeCurrentContext_OpenGL(GfxWindow* win)
{
	if (win == null) { rgfw_glXMakeCurrent(null, 0, null); }
	else { rgfw_glXMakeCurrent(_rgfw.display, win.src.ctx.native.window, win.src.ctx.native.ctx); }
}

fn void* rgfw_getCurrentContext_OpenGL() => rgfw_glXGetCurrentContext();
fn void rgfw_window_swapBuffers_OpenGL(GfxWindow* win) { rgfw_glXSwapBuffers(_rgfw.display, win.src.ctx.native.window); }

fn void rgfw_window_swapInterval_OpenGL(GfxWindow* win, int swapInterval)
{
	if (rgfw_glXSwapIntervalEXT != null) { rgfw_glXSwapIntervalEXT(_rgfw.display, win.src.ctx.native.window, swapInterval); }
	else if (rgfw_glXSwapIntervalNative != null) { rgfw_glXSwapIntervalNative(swapInterval); }
	else { rgfw_debugCallback(RGFW_TYPE_WARNING, RGFW_ERR_OPEN_GL_CONTEXT, "Failed to load swap interval function"); }
}

fn int rgfw_initPlatform(ZString className, RgfwInitFlags flags)
{
	_rgfw.className = className;

	xInitThreads();
	_rgfw.display = xOpenDisplay(null);
	if (_rgfw.display == null) return -1;

	_rgfw.netCurrentDesktop = xInternAtom(_rgfw.display, "_NET_CURRENT_DESKTOP", 0);
	_rgfw.netWmStateMaximizedVert = xInternAtom(_rgfw.display, "_NET_WM_STATE_MAXIMIZED_VERT", 0);
	_rgfw.netWmStateMaximizedHorz = xInternAtom(_rgfw.display, "_NET_WM_STATE_MAXIMIZED_HORZ", 0);
	_rgfw.netWmBypassCompositor = xInternAtom(_rgfw.display, "_NET_WM_BYPASS_COMPOSITOR", 0);
	_rgfw.netWmState = xInternAtom(_rgfw.display, "_NET_WM_STATE", 0);
	_rgfw.motifWmHints = xInternAtom(_rgfw.display, "_MOTIF_WM_HINTS", 0);
	_rgfw.wmDeleteWindow = xInternAtom(_rgfw.display, "WM_DELETE_WINDOW", 0);
	_rgfw.netWmSyncRequestCounter = xInternAtom(_rgfw.display, "_NET_WM_SYNC_REQUEST_COUNTER", 0);
	_rgfw.netWmSyncRequest = xInternAtom(_rgfw.display, "_NET_WM_SYNC_REQUEST", 0);
	_rgfw.atomPair = xInternAtom(_rgfw.display, "ATOM_PAIR", 0);
	_rgfw.multiple = xInternAtom(_rgfw.display, "MULTIPLE", 0);
	_rgfw.targets = xInternAtom(_rgfw.display, "TARGETS", 0);
	_rgfw.saveTargets = xInternAtom(_rgfw.display, "SAVE_TARGETS", 0);
	_rgfw.utf8String = xInternAtom(_rgfw.display, "UTF8_STRING", 0);
	_rgfw.xdndTypeList = xInternAtom(_rgfw.display, "XdndTypeList", 0);
	_rgfw.xdndSelection = xInternAtom(_rgfw.display, "XdndSelection", 0);
	_rgfw.xdndAware = xInternAtom(_rgfw.display, "XdndAware", 0);
	_rgfw.xdndEnter = xInternAtom(_rgfw.display, "XdndEnter", 0);
	_rgfw.xdndPosition = xInternAtom(_rgfw.display, "XdndPosition", 0);
	_rgfw.xdndStatus = xInternAtom(_rgfw.display, "XdndStatus", 0);
	_rgfw.xdndLeave = xInternAtom(_rgfw.display, "XdndLeave", 0);
	_rgfw.xdndDrop = xInternAtom(_rgfw.display, "XdndDrop", 0);
	_rgfw.xdndFinished = xInternAtom(_rgfw.display, "XdndFinished", 0);
	_rgfw.xdndActionCopy = xInternAtom(_rgfw.display, "XdndActionCopy", 0);
	_rgfw.wmProtocols = xInternAtom(_rgfw.display, "WM_PROTOCOLS", 0);
	_rgfw.wmState = xInternAtom(_rgfw.display, "WM_STATE", 0);
	_rgfw.netWmStateFullscreen = xInternAtom(_rgfw.display, "_NET_WM_STATE_FULLSCREEN", 0);
	_rgfw.netWmStateAbove = xInternAtom(_rgfw.display, "_NET_WM_STATE_ABOVE", 0);
	_rgfw.netWmWindowOpacity = xInternAtom(_rgfw.display, "_NET_WM_WINDOW_OPACITY", 0);
	_rgfw.netWmName = xInternAtom(_rgfw.display, "_NET_WM_NAME", 0);
	_rgfw.netWmIcon = xInternAtom(_rgfw.display, "_NET_WM_ICON", 0);
	_rgfw.xselData = xInternAtom(_rgfw.display, "XSEL_DATA", 0);
	_rgfw.clipboardAtom = xInternAtom(_rgfw.display, "CLIPBOARD", 0);
	_rgfw.clipboardManager = xInternAtom(_rgfw.display, "CLIPBOARD_MANAGER", 0);
	_rgfw.netWorkarea = xInternAtom(_rgfw.display, "_NET_WORKAREA", 0);
	_rgfw.xtextUriList = xInternAtom(_rgfw.display, "text/uri-list", 0);
	_rgfw.xtextPlain = xInternAtom(_rgfw.display, "text/plain", 0);

	_rgfw.context = xUniqueContext();
	if (_rgfw.context == 0) return -1;

	XSetWindowAttributes wa;
	wa.event_mask = 1 << 22; // PropertyChangeMask
	_rgfw.helperWindow = xCreateWindow(_rgfw.display, xDefaultRootWindow(_rgfw.display), 0, 0, 1, 1, 0, 0,
		2 /* InputOnly */, xDefaultVisual(_rgfw.display, xDefaultScreen(_rgfw.display)), 1 << 11, &wa);
	if (_rgfw.helperWindow == 0) return -1;

	rgfw_x11CursorHandle = libc::dlopen("libXcursor.so.1", RTLD_LAZY | RTLD_LOCAL);
	if (rgfw_x11CursorHandle != null)
	{
		rgfw_xcursorImageCreateSRC = (PFN_XcursorImageCreate)libc::dlsym(rgfw_x11CursorHandle, "XcursorImageCreate");
		rgfw_xcursorImageDestroySRC = (PFN_XcursorImageDestroy)libc::dlsym(rgfw_x11CursorHandle, "XcursorImageDestroy");
		rgfw_xcursorImageLoadCursorSRC = (PFN_XcursorImageLoadCursor)libc::dlsym(rgfw_x11CursorHandle, "XcursorImageLoadCursor");
	}
	rgfw_x11XiHandle = libc::dlopen("libXi.so.6", RTLD_LAZY | RTLD_LOCAL);
	if (rgfw_x11XiHandle != null) rgfw_xiSelectEventsSRC = (PFN_XISelectEvents)libc::dlsym(rgfw_x11XiHandle, "XISelectEvents");
	rgfw_x11ExtHandle = libc::dlopen("libXext.so.6", RTLD_LAZY | RTLD_LOCAL);
	if (rgfw_x11ExtHandle != null)
	{
		rgfw_xSyncCreateCounterSRC = (PFN_XSyncCreateCounter)libc::dlsym(rgfw_x11ExtHandle, "XSyncCreateCounter");
		rgfw_xSyncIntToValueSRC = (PFN_XSyncIntToValue)libc::dlsym(rgfw_x11ExtHandle, "XSyncIntToValue");
		rgfw_xSyncSetCounterSRC = (PFN_XSyncSetCounter)libc::dlsym(rgfw_x11ExtHandle, "XSyncSetCounter");
		rgfw_xShapeCombineRegionSRC = (PFN_XShapeCombineRegion)libc::dlsym(rgfw_x11ExtHandle, "XShapeCombineRegion");
		rgfw_xShapeCombineMaskSRC = (PFN_XShapeCombineMask)libc::dlsym(rgfw_x11ExtHandle, "XShapeCombineMask");
	}

	char[4] blk = {0, 0, 0, 0};
	_rgfw.hiddenMouse = rgfw_createMouse((char*)&blk, 1, 1, RGBA8);
	_rgfw.unixClipboard = null;

	xSetErrorHandler((void*)&rgfw_xErrorHandler);
	xSync(_rgfw.display, 0);

	xSetLocaleModifiers("");
	xRegisterIMInstantiateCallback(_rgfw.display, null, null, null, (void*)&rgfw_x11ImInitCallback, null);

	int errorBase;
	if (xRRQueryExtension(_rgfw.display, &_rgfw.xrandrEventBase, &errorBase) != 0)
	{
		xRRSelectInput(_rgfw.display, xRootWindow(_rgfw.display, xDefaultScreen(_rgfw.display)), 1 << 4); // RROutputChangeNotifyMask
	}
	return 0;
}

fn void rgfw_deinitPlatform()
{
	if (_rgfw.display != null)
	{
		if (xGetSelectionOwner(_rgfw.display, _rgfw.clipboardAtom) == _rgfw.helperWindow)
		{
			xConvertSelection(_rgfw.display, _rgfw.clipboardManager, _rgfw.saveTargets, 0, _rgfw.helperWindow, 0);
			while (rgfw_xHandleClipboardSelectionHelper() != 0) {}
		}
		xUnregisterIMInstantiateCallback(_rgfw.display, null, null, null, (void*)&rgfw_x11ImInitCallback, null);
	}
	if (_rgfw.im != null) { xCloseIM(_rgfw.im); _rgfw.im = null; }
	if (_rgfw.unixClipboard != null) { rgfw_free_impl(_rgfw.unixClipboard); _rgfw.unixClipboard = null; }
	if (_rgfw.hiddenMouse != null) { rgfw_freeMouse(_rgfw.hiddenMouse); _rgfw.hiddenMouse = null; }
	if (_rgfw.helperWindow != 0) xDestroyWindow(_rgfw.display, _rgfw.helperWindow);
	if (_rgfw.display != null) xCloseDisplay(_rgfw.display);
	_rgfw.display = null;

	if (rgfw_x11CursorHandle != null) { libc::dlclose(rgfw_x11CursorHandle); rgfw_x11CursorHandle = null; }
	if (rgfw_x11XiHandle != null) { libc::dlclose(rgfw_x11XiHandle); rgfw_x11XiHandle = null; }
	if (rgfw_x11ExtHandle != null) { libc::dlclose(rgfw_x11ExtHandle); rgfw_x11ExtHandle = null; }
}

fn void rgfw_initKeycodesPlatform()
{
	// A full evdev-keycode -> RGFW_KEY table walk (xkbGetKeyboardByName) is left
	// out of this mechanical pass; keycodes[] stays zeroed (RGFW_KEY_NULL).
}

fn void rgfw_window_closePlatform(GfxWindow* win)
{
	if (win.src.ic != null) { xDestroyIC(win.src.ic); win.src.ic = null; }
	xFreeGC(_rgfw.display, win.src.gc);
	xDeleteContext(_rgfw.display, win.src.window, _rgfw.context);
	xDestroyWindow(_rgfw.display, win.src.window);
}

struct RgfwAttribStack
{
	int* attribs;
	sz count;
	sz max;
}

fn void rgfw_attribStack_init(RgfwAttribStack* stack, int* attribs, sz max) { stack.attribs = attribs; stack.count = 0; stack.max = max; }
fn void rgfw_attribStack_pushAttrib(RgfwAttribStack* stack, int attrib) { stack.attribs[stack.count] = attrib; stack.count += 1; }
fn void rgfw_attribStack_pushAttribs(RgfwAttribStack* stack, int attrib1, int attrib2) { rgfw_attribStack_pushAttrib(stack, attrib1); rgfw_attribStack_pushAttrib(stack, attrib2); }

fn bool rgfw_extensionSupportedStr(ZString extensions, ZString ext, sz len)
{
	if (extensions == null || ext == null) return false;
	while (ext[len - 1] == 0 && len > 3) len--;
	ZString start = extensions;
	ZString where = (ZString)strstr(extensions, ext);
	while (where != null)
	{
		ZString terminator = where + len;
		if ((where == start || *((char*)where - 1) == ' ') && (terminator[0] == ' ' || terminator[0] == 0)) return true;
		where = (ZString)strstr(terminator, ext);
	}
	return false;
}
