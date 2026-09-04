module gfx @link("X11", "Xext", "Xrandr", "GL", "wayland-client", "wayland-cursor", "wayland-egl", "xkbcommon", "EGL");

// C3 port of RGFW's combined "X11 AND Wayland" + RGFW_DYNAMIC mode: a single
// Linux binary that supports both display servers, choosing at runtime
// (Wayland if available, falling back to X11), matching the original's
// `RGFW_load_X11`/`RGFW_load_Wayland` + `RGFW_functionPointers` vtable design.
// Standalone alternative to rgfw.c3 (macOS) / rgfw_x11.c3 / rgfw_wayland.c3 -
// only ONE rgfw*.c3 file should be included for a given target. This one is
// the "everything Linux" build: both backends are compiled in as *_X11/
// *_Wayland suffixed implementations, and the public rgfw_* functions are
// thin dispatchers through the `rgfw_api` function-pointer table, populated
// once at init after runtime display-server detection.

const bool RGFW_BUILD_MACOS = false;
const bool RGFW_BUILD_X11 = true;
const bool RGFW_BUILD_WAYLAND = false;
const bool RGFW_BUILD_WINDOWS = false;
const bool RGFW_BUILD_EGL = true;

extern fn void* malloc(usz size);
extern fn void free(void* ptr);
extern fn void* memcpy(void* dest, void* src, usz count);
extern fn void* memset(void* dest, int value, usz count);
extern fn usz strlen(char* str);
extern fn char* strstr(char* haystack, char* needle);
extern fn char* strtok(char* str, char* delim);
extern fn char* strtokR(char* str, char* delim, char** saveptr) @cname("strtok_r");
extern fn long strtol(char* str, char** endptr, int base);
extern fn float powf(float x, float y);
extern fn void* dlopen(char* path, int mode);
extern fn void* dlsym(void* handle, char* symbol);
extern fn int dlclose(void* handle);
extern fn int chdir(char* path);
extern fn int clock_gettime(int clock_id, Timespec* ts);

struct Timespec { long tv_sec; long tv_nsec; }

alias VkInstance = void*;
alias VkPhysicalDevice = void*;
alias VkSurfaceKHR = ulong;
alias VkResult = int;
alias RgfwVkGetInstanceProcAddressProc = fn RgfwProc(VkInstance instance, char* procname);
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
// ---- Xlib core types (ABI-compatible aliases; Display/Visual/Screen stay opaque) ----
typedef Display = void;
typedef Visual = void;
typedef Screen = void;
alias XWindow = ulong;
alias Colormap = ulong;
alias XCursor = ulong;
alias Pixmap = ulong;
alias Drawable = ulong;
alias GContext = void*;
alias Atom = ulong;
alias VisualID = ulong;
alias KeySym = ulong;
alias KeyCode = char;
alias Time = ulong;
alias XlibId = ulong;
alias XPointer = char*;
alias Region = void*;
alias XlibIC = void*;
alias XlibIM = void*;
alias XIMStyle = ulong;
alias RRMode = ulong;
alias RROutput = ulong;
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
alias GlxGetProcAddressProc = fn RgfwProc(char*);

struct XVisualInfo
{
	Visual* visual;
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
	Visual* visual;
	XWindow root;
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

struct XClassHint { char* res_name; char* res_class; }

struct XWMHints
{
	long flags;
	int input;
	int initial_state;
	Pixmap icon_pixmap;
	XWindow icon_window;
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

struct XAnyEvent { int type; ulong serial; int send_event; Display* display; XWindow window; }
struct XKeyEvent { int type; ulong serial; int send_event; Display* display; XWindow window; XWindow root; XWindow subwindow; Time time; int x, y; int x_root, y_root; uint state; uint keycode; int same_screen; }
struct XButtonEvent { int type; ulong serial; int send_event; Display* display; XWindow window; XWindow root; XWindow subwindow; Time time; int x, y; int x_root, y_root; uint state; uint button; int same_screen; }
struct XMotionEvent { int type; ulong serial; int send_event; Display* display; XWindow window; XWindow root; XWindow subwindow; Time time; int x, y; int x_root, y_root; uint state; char is_hint; int same_screen; }
struct XCrossingEvent { int type; ulong serial; int send_event; Display* display; XWindow window; XWindow root; XWindow subwindow; Time time; int x, y; int x_root, y_root; int mode; int detail; int same_screen; int focus; uint state; }
struct XFocusChangeEvent { int type; ulong serial; int send_event; Display* display; XWindow window; int mode; int detail; }
struct XExposeEvent { int type; ulong serial; int send_event; Display* display; XWindow window; int x, y, width, height; int count; }
struct XConfigureEvent { int type; ulong serial; int send_event; Display* display; XWindow event; XWindow window; int x, y, width, height; int border_width; XWindow above; int override_redirect; }
struct XPropertyEvent { int type; ulong serial; int send_event; Display* display; XWindow window; Atom atom; Time time; int state; }
struct XClientMessageDataL { long[5] l; }
struct XClientMessageEvent { int type; ulong serial; int send_event; Display* display; XWindow window; Atom message_type; int format; XClientMessageDataL data; }
struct XSelectionRequestEvent { int type; ulong serial; int send_event; Display* display; XWindow owner; XWindow requestor; Atom selection; Atom target; Atom property; Time time; }
struct XSelectionEvent { int type; ulong serial; int send_event; Display* display; XWindow requestor; Atom selection; Atom target; Atom property; Time time; }
struct XReparentEvent { int type; ulong serial; int send_event; Display* display; XWindow event; XWindow window; XWindow parent; int x, y; int override_redirect; }
struct XMapEvent { int type; ulong serial; int send_event; Display* display; XWindow event; XWindow window; int override_redirect; }
struct XUnmapEvent { int type; ulong serial; int send_event; Display* display; XWindow event; XWindow window; int from_configure; }
struct XErrorEvent { int type; Display* display; ulong resourceid; ulong serial; char error_code; char request_code; char minor_code; }
struct XGenericEventCookie { int type; ulong serial; int send_event; Display* display; int extension; int evtype; uint cookie_data_len; void* data; }

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
struct XRRScreenResources { Time timestamp; Time configTimestamp; int ncrtc; RRCrtc* crtcs; int noutput; RROutput* outputs; int nmode; XRRModeInfoNative* modes; }
struct XRRModeInfoNative { RRMode id; uint width; uint height; ulong dotClock; uint hSyncStart; uint hSyncEnd; uint hTotal; uint hSkew; uint vSyncStart; uint vSyncEnd; uint vTotal; char* name; uint nameLength; ulong modeFlags; }
struct XRRCrtcInfo { Time timestamp; int x, y; uint width, height; RRMode mode; int rotation; int noutput; RROutput* outputs; int rotations; int npossible; RROutput* possible; }
struct XRROutputInfo { Time timestamp; RRCrtc crtc; char* name; int nameLen; ulong mm_width; ulong mm_height; int connection; int subpixel_order; int ncrtc; RRCrtc* crtcs; int nclone; RROutput* clones; int nmode; int npreferred; RRMode* modes; }
struct XRRCrtcGamma { int size; ushort* red; ushort* green; ushort* blue; }

// ---- Xcursor / XInput2 / Xext sync+shape (dlopen'd like the original) ----
struct XcursorImage { uint version; uint size; uint width, height; uint xhot, yhot; uint delay; uint* pixels; }
struct XIEventMask { int deviceid; int mask_len; char* mask; }
struct XIValuatorState { int mask_len; char* mask; double* values; }
struct XIRawEvent { int type; ulong serial; int send_event; Display* display; int extension; int evtype; Time time; int deviceid; int sourceid; int detail; int flags; XIValuatorState valuators; double* raw_values; }
struct XSyncValue { int hi; uint lo; }
alias XSyncCounter = XlibId;

// GLX
alias RGFW_glXCreateContextAttribsARBProc = fn GLXContext(Display*, GLXFBConfig, GLXContext, int, int*);
alias RGFW_glXQueryExtensionsStringProc = fn char*(Display*, int);
alias RGFW_glXChooseVisualProc = fn XVisualInfo*(Display*, int, int*);
alias RGFW_glXCreateContextProc = fn GLXContext(Display*, XVisualInfo*, GLXContext, int);
alias RGFW_glXMakeCurrentProc = fn int(Display*, GLXDrawable, GLXContext);
alias RGFW_glXGetCurrentContextProc = fn GLXContext();
alias RGFW_glXSwapBuffersProc = fn void(Display*, GLXDrawable);
alias RGFW_glXSwapIntervalEXTProc = fn void(Display*, GLXDrawable, int);
alias RGFW_glXGetProcAddressProc = fn void*(char*);
alias RGFW_glXGetVisualFromFBConfigProc = fn XVisualInfo*(Display*, GLXFBConfig);
alias RGFW_glXGetFBConfigAttribProc = fn int(Display*, GLXFBConfig, int, int*);
alias RGFW_glXGetProcAddressARBProc = fn void*(char*);
alias RGFW_glXChooseFBConfigProc = fn GLXFBConfig*(Display*, int, int*, int*);
alias RGFW_glXDestroyContextProc = fn void(Display*, GLXContext);
alias RGFW_glXCreateWindowProc = fn GLXWindow(Display*, GLXFBConfig, XWindow, int*);
alias RGFW_glXDestroyWindowProc = fn void(Display*, GLXWindow);
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
alias PFN_XcursorImageLoadCursor = fn XCursor(Display*, XcursorImage*);
alias PFN_XISelectEvents = fn int(Display*, XWindow, XIEventMask*, int);
alias PFN_XSyncIntToValue = fn void(XSyncValue*, int);
alias PFN_XSyncSetCounter = fn int(Display*, XSyncCounter, XSyncValue);
alias PFN_XSyncCreateCounter = fn XSyncCounter(Display*, XSyncValue);
alias PFN_XShapeCombineMask = fn void(Display*, XWindow, int, int, int, Pixmap, int);
alias PFN_XShapeCombineRegion = fn void(Display*, XWindow, int, int, int, Region, int);

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
extern fn Display* xOpenDisplay(char* name) @cname("XOpenDisplay");
extern fn int xCloseDisplay(Display* d) @cname("XCloseDisplay");
extern fn int xInitThreads() @cname("XInitThreads");
extern fn XWindow xDefaultRootWindow(Display* d) @cname("XDefaultRootWindow");
extern fn XWindow xRootWindow(Display* d, int screen) @cname("XRootWindow");
extern fn int xDefaultScreen(Display* d) @cname("XDefaultScreen");
extern fn Visual* xDefaultVisual(Display* d, int screen) @cname("XDefaultVisual");
extern fn int xDefaultDepth(Display* d, int screen) @cname("XDefaultDepth");
extern fn GContext xDefaultGC(Display* d, int screen) @cname("XDefaultGC");
extern fn Colormap xCreateColormap(Display* d, XWindow win, Visual* visual, int alloc) @cname("XCreateColormap");
extern fn int xFreeColors(Display* d, Colormap cm, ulong* pixels, int n, ulong planes) @cname("XFreeColors");
extern fn XWindow xCreateWindow(Display* d, XWindow parent, int x, int y, uint w, uint h, uint borderWidth, int depth, uint c_class, Visual* visual, ulong valuemask, XSetWindowAttributes* attrs) @cname("XCreateWindow");
extern fn int xDestroyWindow(Display* d, XWindow w) @cname("XDestroyWindow");
extern fn int xMapWindow(Display* d, XWindow w) @cname("XMapWindow");
extern fn int xMapRaised(Display* d, XWindow w) @cname("XMapRaised");
extern fn int xUnmapWindow(Display* d, XWindow w) @cname("XUnmapWindow");
extern fn int xMoveWindow(Display* d, XWindow w, int x, int y) @cname("XMoveWindow");
extern fn int xResizeWindow(Display* d, XWindow w, uint width, uint height) @cname("XResizeWindow");
extern fn int xRaiseWindow(Display* d, XWindow w) @cname("XRaiseWindow");
extern fn GContext xCreateGC(Display* d, Drawable draw, ulong valuemask, void* values) @cname("XCreateGC");
extern fn int xFreeGC(Display* d, GContext gc) @cname("XFreeGC");
extern fn int xSaveContext(Display* d, XlibId xid, int context, XPointer data) @cname("XSaveContext");
extern fn int xFindContext(Display* d, XlibId xid, int context, XPointer* dataOut) @cname("XFindContext");
extern fn int xDeleteContext(Display* d, XlibId xid, int context) @cname("XDeleteContext");
extern fn int xUniqueContext() @cname("XUniqueContext");
extern fn XlibIC xCreateIC(XlibIM im, ...) @cname("XCreateIC");

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
	ulong* supported_styles;
}

// Signature required by XRegisterIMInstantiateCallback / XUnregisterIMInstantiateCallback.
alias XImInstantiateCallbackProc = fn void(Display* d, XPointer clientData, XPointer callData);
extern fn int xDestroyIC(XlibIC ic) @cname("XDestroyIC");
extern fn int xSetICFocus(XlibIC ic) @cname("XSetICFocus");
extern fn int xUnsetICFocus(XlibIC ic) @cname("XUnsetICFocus");
extern fn XlibIM xOpenIM(Display* d, void* db, char* res_name, char* res_class) @cname("XOpenIM");
extern fn int xCloseIM(XlibIM im) @cname("XCloseIM");
extern fn char* xGetIMValues(XlibIM im, ...) @cname("XGetIMValues");
extern fn char* xSetIMValues(XlibIM im, ...) @cname("XSetIMValues");
extern fn int xRegisterIMInstantiateCallback(Display* d, void* db, char* res_name, char* res_class, void* callback, XPointer clientData) @cname("XRegisterIMInstantiateCallback");
extern fn int xUnregisterIMInstantiateCallback(Display* d, void* db, char* res_name, char* res_class, void* callback, XPointer clientData) @cname("XUnregisterIMInstantiateCallback");
extern fn char* xSetLocaleModifiers(char* modifierList) @cname("XSetLocaleModifiers");
extern fn usz xutf8LookupString(XlibIC ic, XKeyEvent* event, char* buffer, int bytes, KeySym* keysym, int* status) @cname("Xutf8LookupString");
extern fn int xutf8SetWMProperties(Display* d, XWindow w, char* windowName, char* iconName, char** argv, int argc, XSizeHints* hints, XWMHints* wmHints, void* classHints) @cname("Xutf8SetWMProperties");
extern fn int xSetClassHint(Display* d, XWindow w, XClassHint* hint) @cname("XSetClassHint");
extern fn int xSetWMHints(Display* d, XWindow w, XWMHints* hints) @cname("XSetWMHints");
extern fn XWMHints* xGetWMHints(Display* d, XWindow w) @cname("XGetWMHints");
extern fn int xSetWMProtocols(Display* d, XWindow w, Atom* protocols, int count) @cname("XSetWMProtocols");
extern fn int xSetWMSizeHints(Display* d, Drawable w, XSizeHints* hints, Atom property) @cname("XSetWMSizeHints");
extern fn int xGetWMNormalHints(Display* d, XWindow w, XSizeHints* hints, long* flags) @cname("XGetWMNormalHints");
extern fn int xSetWMNormalHints(Display* d, XWindow w, XSizeHints* hints) @cname("XSetWMNormalHints");
extern fn int xSelectInput(Display* d, Drawable w, long eventMask) @cname("XSelectInput");
extern fn int xStoreName(Display* d, XWindow w, char* name) @cname("XStoreName");
extern fn int xSetWindowBackground(Display* d, XWindow w, ulong background) @cname("XSetWindowBackground");
extern fn int xSetWindowBackgroundPixmap(Display* d, XWindow w, Pixmap pixmap) @cname("XSetWindowBackgroundPixmap");
extern fn int xClearWindow(Display* d, XWindow w) @cname("XClearWindow");
extern fn int xFlush(Display* d) @cname("XFlush");
extern fn int xSync(Display* d, int discard) @cname("XSync");
extern fn int xPending(Display* d) @cname("XPending");
extern fn int xConnectionNumber(Display* d) @cname("XConnectionNumber");
extern fn int qLength(Display* d) @cname("QLength");
extern fn int xEventsQueued(Display* d, int mode) @cname("XEventsQueued");
extern fn int xNextEvent(Display* d, XEvent* ev) @cname("XNextEvent");
extern fn int xPeekEvent(Display* d, XEvent* ev) @cname("XPeekEvent");
extern fn int xCheckTypedWindowEvent(Display* d, XWindow w, int eventType, XEvent* ev) @cname("XCheckTypedWindowEvent");
extern fn int xFilterEvent(XEvent* ev, XWindow w) @cname("XFilterEvent");
extern fn int xSendEvent(Display* d, XWindow w, int propagate, long eventMask, XEvent* ev) @cname("XSendEvent");
extern fn int xGetEventData(Display* d, XGenericEventCookie* cookie) @cname("XGetEventData");
extern fn int xFreeEventData(Display* d, XGenericEventCookie* cookie) @cname("XFreeEventData");
extern fn int xGetWindowAttributes(Display* d, XWindow w, XWindowAttributes* attrs) @cname("XGetWindowAttributes");
extern fn int xChangeProperty(Display* d, XWindow w, Atom property, Atom type, int format, int mode, char* data, int nelements) @cname("XChangeProperty");
extern fn int xGetWindowProperty(Display* d, XWindow w, Atom property, long offset, long length, int delet, Atom reqType, Atom* actualType, int* actualFormat, ulong* nitems, ulong* bytesAfter, char** propReturn) @cname("XGetWindowProperty");
extern fn int xDeleteProperty(Display* d, XWindow w, Atom property) @cname("XDeleteProperty");
extern fn Atom xInternAtom(Display* d, char* name, int onlyIfExists) @cname("XInternAtom");
extern fn int xConvertSelection(Display* d, Atom selection, Atom target, Atom property, XWindow requestor, Time time) @cname("XConvertSelection");
extern fn XWindow xGetSelectionOwner(Display* d, Atom selection) @cname("XGetSelectionOwner");
extern fn int xSetSelectionOwner(Display* d, Atom selection, XWindow owner, Time time) @cname("XSetSelectionOwner");
extern fn int xFree(void* data) @cname("XFree");
extern fn int xSetErrorHandler(void* handler) @cname("XSetErrorHandler");
extern fn int xGetErrorText(Display* d, int code, char* buffer, int len) @cname("XGetErrorText");
extern fn int xQueryPointer(Display* d, XWindow w, XWindow* rootRet, XWindow* childRet, int* rootX, int* rootY, int* winX, int* winY, uint* mask) @cname("XQueryPointer");
extern fn int xWarpPointer(Display* d, XWindow src, XWindow dest, int srcX, int srcY, uint srcW, uint srcH, int destX, int destY) @cname("XWarpPointer");
extern fn int xTranslateCoordinates(Display* d, XWindow src, XWindow dest, int srcX, int srcY, int* destXRet, int* destYRet, XWindow* childRet) @cname("XTranslateCoordinates");
extern fn int xSetInputFocus(Display* d, XWindow w, int revert, Time time) @cname("XSetInputFocus");
extern fn int xGrabPointer(Display* d, XWindow w, int ownerEvents, uint eventMask, int pointerMode, int keyboardMode, XWindow confineTo, XCursor cursor, Time time) @cname("XGrabPointer");
extern fn int xUngrabPointer(Display* d, Time time) @cname("XUngrabPointer");
extern fn int xIconifyWindow(Display* d, XWindow w, int screen) @cname("XIconifyWindow");
extern fn int xDefineCursor(Display* d, XWindow w, XCursor cursor) @cname("XDefineCursor");
extern fn int xFreeCursor(Display* d, XCursor cursor) @cname("XFreeCursor");
extern fn XCursor xCreateFontCursor(Display* d, uint shape) @cname("XCreateFontCursor");
extern fn KeySym xkbKeycodeToKeysym(Display* d, KeyCode kc, int group, int level) @cname("XkbKeycodeToKeysym");
extern fn int xkbGetState(Display* d, uint deviceSpec, void* stateRet) @cname("XkbGetState");

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
extern fn void* xkbGetMap(Display* d, uint which, uint deviceSpec) @cname("XkbGetMap");
extern fn int xkbGetNames(Display* d, uint which, void* xkb) @cname("XkbGetNames");
extern fn void* xkbGetKeyboardByName(Display* d, uint deviceSpec, void* names, uint want, uint need, int loadKeycodes) @cname("XkbGetKeyboardByName");
extern fn int xkbFreeKeyboard(void* xkb, uint which, int freeAll) @cname("XkbFreeKeyboard");
extern fn int xMatchVisualInfo(Display* d, int screen, int depth, int c_class, XVisualInfo* visualOut) @cname("XMatchVisualInfo");
extern fn int xChangeWindowAttributes(Display* d, XWindow w, ulong valuemask, XSetWindowAttributes* attrs) @cname("XChangeWindowAttributes");
extern fn Region xCreateRegion() @cname("XCreateRegion");
extern fn int xDestroyRegion(Region r) @cname("XDestroyRegion");
extern fn XCursor xCreatePixmap(Display* d, Drawable draw, uint w, uint h, uint depth) @cname("XCreatePixmap");
extern fn void* xCreateImage(Display* d, Visual* visual, uint depth, int format, int offset, char* data, uint w, uint h, int bitmapPad, int bytesPerLine) @cname("XCreateImage");
extern fn int xPutImage(Display* d, Drawable draw, GContext gc, void* image, int srcX, int srcY, int destX, int destY, uint w, uint h) @cname("XPutImage");
extern fn int xDestroyImage(void* image) @cname("XDestroyImage");

// Leading fields of the real XImage struct (ABI-compatible prefix; only used to null
// out `data` before XDestroyImage, since that buffer is owned by our own allocation).
struct XImageHeader
{
	int width;
	int height;
	int xoffset;
	int format;
	char* data;
}
extern fn char* xResourceManagerString(Display* d) @cname("XResourceManagerString");
extern fn void* xrmGetStringDatabase(char* data) @cname("XrmGetStringDatabase");
extern fn int xrmGetResource(void* db, char* name, char* c_class, char** typeOut, void* valueOut) @cname("XrmGetResource");
extern fn int xrmDestroyDatabase(void* db) @cname("XrmDestroyDatabase");
extern fn double atof(char* s) @cname("atof");
extern fn XRRScreenResources* xRRGetScreenResourcesCurrent(Display* d, XWindow w) @cname("XRRGetScreenResourcesCurrent");
extern fn int xRRFreeScreenResources(XRRScreenResources* res) @cname("XRRFreeScreenResources");
extern fn RROutput xRRGetOutputPrimary(Display* d, XWindow w) @cname("XRRGetOutputPrimary");
extern fn XRROutputInfo* xRRGetOutputInfo(Display* d, XRRScreenResources* res, RROutput output) @cname("XRRGetOutputInfo");
extern fn int xRRFreeOutputInfo(XRROutputInfo* info) @cname("XRRFreeOutputInfo");
extern fn XRRCrtcInfo* xRRGetCrtcInfo(Display* d, XRRScreenResources* res, RRCrtc crtc) @cname("XRRGetCrtcInfo");
extern fn int xRRFreeCrtcInfo(XRRCrtcInfo* info) @cname("XRRFreeCrtcInfo");
extern fn int xRRSetCrtcConfig(Display* d, XRRScreenResources* res, RRCrtc crtc, Time time, int x, int y, RRMode mode, int rotation, RROutput* outputs, int noutputs) @cname("XRRSetCrtcConfig");
extern fn int xRRGetCrtcGammaSize(Display* d, RRCrtc crtc) @cname("XRRGetCrtcGammaSize");
extern fn XRRCrtcGamma* xRRGetCrtcGamma(Display* d, RRCrtc crtc) @cname("XRRGetCrtcGamma");
extern fn XRRCrtcGamma* xRRAllocGamma(int size) @cname("XRRAllocGamma");
extern fn int xRRSetCrtcGamma(Display* d, RRCrtc crtc, XRRCrtcGamma* gamma) @cname("XRRSetCrtcGamma");
extern fn int xRRFreeGamma(XRRCrtcGamma* gamma) @cname("XRRFreeGamma");
extern fn int xRRQueryExtension(Display* d, int* eventBase, int* errorBase) @cname("XRRQueryExtension");
extern fn int xRRSelectInput(Display* d, XWindow w, int mask) @cname("XRRSelectInput");

macro usz ximaskLen(event) => (usz)((event >> 3) + 1);
macro void xiSetMask(char* mask, event) { mask[(usz)(event / 8)] |= (char)(1 << (event % 8)); }
macro bool ximaskIsSet(char* mask, event) => (mask[(usz)(event / 8)] & (char)(1 << (event % 8))) != 0;

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
	RgfwFormat format;
	char* buffer;
	void* bitmap; // XImage* (X11)
	WlProxy* pool; // wl_shm_pool* (Wayland)
	int fd; // Wayland
	WlProxy* wlBuffer; // wl_buffer* (Wayland)
}

struct RgfwWindowSrc
{
	XWindow window;
	XWindow parent;
	GContext gc;
	XlibIC ic;
	ulong flashEnd;
	long counter_value;
	XSyncCounter counter;
	RgfwGfxContext ctx;
	RgfwGfxContextType gfxType;
	int x, y, w, h;
	WlProxy* surface;
	WlProxy* xdgSurface;
	WlProxy* xdgToplevel;
	WlProxy* decoration;
	WlProxy* lockedPointer;
	WlProxy* icon;
	WlProxy* dataSource;
	WlProxy* customCursorSurface;
	RgfwMonitorNode* activeMonitor;
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
	uint id;
	WlProxy* output;
	WlProxy* xdgOutput;
	RgfwMonitorMode* modes;
	usz modeCount;
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
	RgfwKey[256] keycodes;
	bool stopCheckEvents_bool;
	int[3] eventWait_forceStop;
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
	Display* display;
	int context;
	XWindow helperWindow;
	char* className;
	XlibIM im;
	RgfwDataTransfer* unixClipboard;
	int xrandrEventBase;
	XWindow x11Source;
	int x11Version;
	int x11Format;
	RgfwDataTransferType x11TransferType;
	XErrorEvent* x11Error;
	Atom netCurrentDesktop, netWmStateMaximizedVert, netWmStateMaximizedHorz, netWmBypassCompositor;
	Atom netWmState, motifWmHints, wmDeleteWindow, netWmSyncRequestCounter, netWmSyncRequest;
	Atom atomPair, multiple, targets, saveTargets, utf8String;
	Atom xdndTypeList, xdndSelection, xdndAware, xdndEnter, xdndPosition, xdndStatus, xdndLeave, xdndDrop, xdndFinished, xdndActionCopy;
	Atom wmProtocols, wmState, netWmStateFullscreen, netWmStateAbove, netWmWindowOpacity;
	Atom netWmName, netWmIcon, xselData, clipboardAtom, clipboardManager;
	Atom xtextUriList, xtextPlain, netWorkarea;
	WlDisplay* wlDisplay;
	WlProxy* registry;
	WlProxy* compositor;
	WlProxy* seat;
	WlProxy* shm;
	WlProxy* wlPointer;
	WlProxy* wlKeyboard;
	WlProxy* xdgWmBase;
	WlProxy* decorationManager;
	WlProxy* constraintManager;
	WlProxy* relativePointerManager;
	WlProxy* relativePointer;
	WlProxy* iconManager;
	WlProxy* xdgOutputManager;
	WlProxy* wpPointerWarp;
	WlProxy* dataDeviceManager;
	WlProxy* dataDevice;
	WlCursorTheme* wlCursorTheme;
	WlProxy* cursorSurface;
	RgfwWindow* mouseOwner;
	RgfwWindow* kbOwner;
	uint mouseEnterSerial;
	XkbContext* xkbContext;
	XkbKeymap* keymap;
	XkbState* xkbState;
	XkbComposeState* composeState;
	int wlRepeatInfoRate;
	int wlRepeatInfoDelay;
	uint lastKey;
	uint lastKeyTime;
	bool useWaylandBool;
	bool rawMouse;
	RgfwWindowState windowState;
	RgfwKeyState[8] mouseButtons;
	RgfwKeyState[RGFW_KEY_LAST] keyboard;
	float scrollX, scrollY;
	float vectorX, vectorY;
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
fn void* rgfw_alloc(usz size) { return rgfw_alloc_impl(size); }
fn void rgfw_free(void* ptr) { rgfw_free_impl(ptr); }
fn usz rgfw_sizeofInfo() { return RgfwInfo::size; }
fn usz rgfw_sizeofNativeImage() { return RgfwNativeImage::size; }
fn usz rgfw_sizeofSurface() { return RgfwSurface::size; }
fn usz rgfw_sizeofWindow() { return RgfwWindow::size; }
fn usz rgfw_sizeofWindowSrc() { return RgfwWindowSrc::size; }

fn bool rgfw_loadGL_X11()
{
	if (_rgfw.nativeGL_handle != null && _rgfw.glGetProcAddress != null) return RGFW_TRUE;
	char*[3] glxSonames = {"libGLX.so.0", "libGL.so.1", "libGL.so"};
	for (usz i = 0; i < 3; i++)
	{
		_rgfw.nativeGL_handle = dlopen(glxSonames[i], RTLD_LAZY | RTLD_LOCAL);
		if (_rgfw.nativeGL_handle != null) break;
	}
	if (_rgfw.nativeGL_handle == null) return RGFW_FALSE;
	void* ptr = dlsym(_rgfw.nativeGL_handle, "glXGetProcAddress");
	_rgfw.glGetProcAddress = ptr;

	rgfw_glXChooseVisual = (RGFW_glXChooseVisualProc)dlsym(_rgfw.nativeGL_handle, "glXChooseVisual");
	rgfw_glXCreateContext = (RGFW_glXCreateContextProc)dlsym(_rgfw.nativeGL_handle, "glXCreateContext");
	rgfw_glXMakeCurrent = (RGFW_glXMakeCurrentProc)dlsym(_rgfw.nativeGL_handle, "glXMakeCurrent");
	rgfw_glXGetCurrentContext = (RGFW_glXGetCurrentContextProc)dlsym(_rgfw.nativeGL_handle, "glXGetCurrentContext");
	rgfw_glXSwapBuffers = (RGFW_glXSwapBuffersProc)dlsym(_rgfw.nativeGL_handle, "glXSwapBuffers");
	rgfw_glXGetVisualFromFBConfig = (RGFW_glXGetVisualFromFBConfigProc)dlsym(_rgfw.nativeGL_handle, "glXGetVisualFromFBConfig");
	rgfw_glXGetFBConfigAttrib = (RGFW_glXGetFBConfigAttribProc)dlsym(_rgfw.nativeGL_handle, "glXGetFBConfigAttrib");
	rgfw_glXGetProcAddressARB = (RGFW_glXGetProcAddressARBProc)dlsym(_rgfw.nativeGL_handle, "glXGetProcAddressARB");
	rgfw_glXChooseFBConfig = (RGFW_glXChooseFBConfigProc)dlsym(_rgfw.nativeGL_handle, "glXChooseFBConfig");
	rgfw_glXDestroyContext = (RGFW_glXDestroyContextProc)dlsym(_rgfw.nativeGL_handle, "glXDestroyContext");
	rgfw_glXCreateWindow = (RGFW_glXCreateWindowProc)dlsym(_rgfw.nativeGL_handle, "glXCreateWindow");
	rgfw_glXDestroyWindow = (RGFW_glXDestroyWindowProc)dlsym(_rgfw.nativeGL_handle, "glXDestroyWindow");
	rgfw_glXQueryExtensionsString = (RGFW_glXQueryExtensionsStringProc)dlsym(_rgfw.nativeGL_handle, "glXQueryExtensionsString");
	rgfw_glXCreateContextAttribsARB = (RGFW_glXCreateContextAttribsARBProc)dlsym(_rgfw.nativeGL_handle, "glXCreateContextAttribsARB");
	rgfw_glXSwapIntervalEXT = (RGFW_glXSwapIntervalEXTProc)dlsym(_rgfw.nativeGL_handle, "glXSwapIntervalEXT");
	if (rgfw_glXSwapIntervalEXT == null)
	{
		char*[2] array = {"GLX_MESA_swap_control", "GLX_SGI_swap_control"};
		for (usz i = 0; i < 2 && rgfw_glXSwapIntervalNative == null; i++)
		{
			rgfw_glXSwapIntervalNative = (RGFW_glXSwapIntervalNativeProc)dlsym(_rgfw.nativeGL_handle, array[i]);
		}
	}
	return RGFW_TRUE;
}

fn void rgfw_unloadGL_X11()
{
	_rgfw.nativeGL_handle = null;
	_rgfw.glGetProcAddress = null;
}
fn void* rgfw_eglProc(char* name)
{
	if (_rgfw.egl_handle == null) return null;
	void* proc = dlsym(_rgfw.egl_handle, name);
	if (proc == null && rgfw_eglGetProcAddress != null) proc = (void*)rgfw_eglGetProcAddress(name);
	return proc;
}

fn bool rgfw_loadEGL()
{
	if (_rgfw.egl_handle != null) return RGFW_TRUE;
	char*[5] names = {"libEGL.dylib", "libEGL.1.dylib", "libEGL.so.1", "libEGL.so", "EGL.dll"};
	for (usz i = 0; i < 5; i++)
	{
		_rgfw.egl_handle = dlopen(names[i], RTLD_LAZY | RTLD_LOCAL);
		if (_rgfw.egl_handle != null) break;
	}
	if (_rgfw.egl_handle == null) return RGFW_FALSE;
	void* getProc = dlsym(_rgfw.egl_handle, "eglGetProcAddress");
	if (getProc == null) return RGFW_FALSE;
	rgfw_eglGetProcAddress = (EglGetProcAddressProc)getProc;
	_rgfw.eglGetProcAddress = getProc;
	void* proc;
	proc = rgfw_eglProc("eglGetDisplay"); if (proc == null) return RGFW_FALSE; rgfw_eglGetDisplay = (EglGetDisplayProc)proc;
	proc = rgfw_eglProc("eglInitialize"); if (proc == null) return RGFW_FALSE; rgfw_eglInitialize = (EglInitializeProc)proc;
	proc = rgfw_eglProc("eglTerminate"); if (proc == null) return RGFW_FALSE; rgfw_eglTerminate = (EglTerminateProc)proc;
	proc = rgfw_eglProc("eglBindAPI"); if (proc == null) return RGFW_FALSE; rgfw_eglBindApi = (EglBindApiProc)proc;
	proc = rgfw_eglProc("eglChooseConfig"); if (proc == null) return RGFW_FALSE; rgfw_eglChooseConfig = (EglChooseConfigProc)proc;
	proc = rgfw_eglProc("eglCreateContext"); if (proc == null) return RGFW_FALSE; rgfw_eglCreateContext = (EglCreateContextProc)proc;
	proc = rgfw_eglProc("eglDestroyContext"); if (proc == null) return RGFW_FALSE; rgfw_eglDestroyContext = (EglDestroyContextProc)proc;
	proc = rgfw_eglProc("eglCreateWindowSurface"); if (proc == null) return RGFW_FALSE; rgfw_eglCreateWindowSurface = (EglCreateWindowSurfaceProc)proc;
	proc = rgfw_eglProc("eglDestroySurface"); if (proc == null) return RGFW_FALSE; rgfw_eglDestroySurface = (EglDestroySurfaceProc)proc;
	proc = rgfw_eglProc("eglMakeCurrent"); if (proc == null) return RGFW_FALSE; rgfw_eglMakeCurrent = (EglMakeCurrentProc)proc;
	proc = rgfw_eglProc("eglSwapBuffers"); if (proc == null) return RGFW_FALSE; rgfw_eglSwapBuffers = (EglSwapBuffersProc)proc;
	proc = rgfw_eglProc("eglSwapInterval"); if (proc == null) return RGFW_FALSE; rgfw_eglSwapInterval = (EglSwapIntervalProc)proc;
	proc = rgfw_eglProc("eglGetCurrentContext"); if (proc == null) return RGFW_FALSE; rgfw_eglGetCurrentContext = (EglGetCurrentContextProc)proc;
	proc = rgfw_eglProc("eglQueryString"); if (proc == null) return RGFW_FALSE; rgfw_eglQueryString = (EglQueryStringProc)proc;
	proc = rgfw_eglProc("eglGetConfigAttrib"); if (proc == null) return RGFW_FALSE; rgfw_eglGetConfigAttrib = (EglGetConfigAttribProc)proc;
	return RGFW_TRUE;
}

fn void rgfw_unloadEGL()
{
	if (_rgfw.egl_handle == null) return;
	if (_rgfw.egl_display != null && rgfw_eglTerminate != null) rgfw_eglTerminate((EglDisplay)_rgfw.egl_display);
	dlclose(_rgfw.egl_handle);
	_rgfw.egl_handle = null;
	_rgfw.egl_display = null;
	_rgfw.eglGetProcAddress = null;
	rgfw_eglGetProcAddress = null;
}
const int RTLD_LAZY = 0x1;
const int RTLD_LOCAL = 0x4;
const int VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR = 1000004000;
const VkResult VK_ERROR_EXTENSION_NOT_PRESENT = -7;
struct VkXlibSurfaceCreateInfoKHR { int sType; void* pNext; uint flags; Display* dpy; XWindow window; }
alias RgfwVkCreateXlibSurfaceKHRProc = fn VkResult(VkInstance instance, VkXlibSurfaceCreateInfoKHR* createInfo, void* allocator, VkSurfaceKHR* surface);
char*[2] rgfw_vulkanExtensions = {"VK_KHR_surface", "VK_KHR_xlib_surface"};

fn bool rgfw_loadVulkan()
{
	if (_rgfw.vulkan_handle != null) return RGFW_TRUE;
	char*[3] names = {"libvulkan.so.1", "libvulkan.so", "libMoltenVK.so"};
	for (usz i = 0; i < 3; i++)
	{
		_rgfw.vulkan_handle = dlopen(names[i], RTLD_LAZY | RTLD_LOCAL);
		if (_rgfw.vulkan_handle != null) break;
	}
	if (_rgfw.vulkan_handle == null) return RGFW_FALSE;
	void* proc = dlsym(_rgfw.vulkan_handle, "vkGetInstanceProcAddr");
	if (proc == null) return RGFW_FALSE;
	_rgfw.vkGetInstanceProcAddress = (RgfwVkGetInstanceProcAddressProc)proc;
	return RGFW_TRUE;
}

fn void rgfw_unloadVulkan()
{
	if (_rgfw.vulkan_handle == null) return;
	dlclose(_rgfw.vulkan_handle);
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
	RgfwProc raw = rgfw_getInstanceProcAddress_Vulkan(instance, "vkCreateXlibSurfaceKHR");
	if (raw == null) return VK_ERROR_EXTENSION_NOT_PRESENT;
	VkXlibSurfaceCreateInfoKHR createInfo;
	createInfo.sType = VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR;
	createInfo.pNext = null;
	createInfo.flags = 0;
	createInfo.dpy = _rgfw.display;
	createInfo.window = win.src.window;
	return ((RgfwVkCreateXlibSurfaceKHRProc)raw)(instance, &createInfo, null, surface);
}

extern fn VisualID xVisualIDFromVisual(Visual* visual) @cname("XVisualIDFromVisual");
alias RgfwVkGetPhysicalDeviceXlibPresentationSupportKHRProc = fn uint(VkPhysicalDevice, uint, Display*, VisualID);

fn bool rgfw_getPhysicalDevicePresentationSupport_Vulkan(VkInstance instance, VkPhysicalDevice physicalDevice, uint queueFamilyIndex)
{
	RgfwProc raw = rgfw_getInstanceProcAddress_Vulkan(instance, "vkGetPhysicalDeviceXlibPresentationSupportKHR");
	if (raw == null) return RGFW_FALSE;
	Visual* visual = xDefaultVisual(_rgfw.display, xDefaultScreen(_rgfw.display));
	uint out = ((RgfwVkGetPhysicalDeviceXlibPresentationSupportKHRProc)raw)(physicalDevice, queueFamilyIndex, _rgfw.display, xVisualIDFromVisual(visual));
	return rgfw_bool(out != 0);
}

alias WGPUInstance = void*;
alias WGPUSurface = void*;
alias WGPUSType = int;
const WGPUSType WGPU_STYPE_SURFACE_SOURCE_XLIB_WINDOW = 0x00000006;
const WGPUSType WGPU_STYPE_SURFACE_SOURCE_WAYLAND_SURFACE = 0x00000007;
struct WgpuChainedStruct { WgpuChainedStruct* next; WGPUSType sType; }
struct WgpuSurfaceDescriptor { WgpuChainedStruct* nextInChain; }
struct WgpuSurfaceSourceXlibWindow { WgpuChainedStruct chain; Display* display; XWindow window; }
struct WgpuSurfaceSourceWaylandSurface { WgpuChainedStruct chain; WlDisplay* display; WlProxy* surface; }
extern fn WGPUSurface wgpuInstanceCreateSurface(WGPUInstance instance, WgpuSurfaceDescriptor* descriptor) @cname("wgpuInstanceCreateSurface");

fn WGPUSurface rgfw_window_createSurface_WebGPU_X11(RgfwWindow* window, WGPUInstance instance)
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

fn WGPUSurface rgfw_window_createSurface_WebGPU_Wayland(RgfwWindow* window, WGPUInstance instance)
{
	WgpuSurfaceSourceWaylandSurface fromWl;
	fromWl.chain.next = null;
	fromWl.chain.sType = WGPU_STYPE_SURFACE_SOURCE_WAYLAND_SURFACE;
	fromWl.display = _rgfw.wlDisplay;
	fromWl.surface = window.src.surface;
	WgpuSurfaceDescriptor surfaceDesc;
	surfaceDesc.nextInChain = (WgpuChainedStruct*)&fromWl;
	return wgpuInstanceCreateSurface(instance, &surfaceDesc);
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
	rgfw_memzero((void*)&_rgfw.keycodes, RgfwKey[256]::size);
	rgfw_initKeycodesPlatform();
	for (usz i = 0; i < (usz)RGFW_KEY_LAST; i++)
	{
		for (usz y = 0; y < 256; y++)
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
	if (keycode > 256) return RGFW_KEY_NULL;
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
fn void rgfw_window_setDND(RgfwWindow* win, bool allow) { rgfw_setBit(&win.internal.flags, RGFW_WINDOW_ALLOW_DND, allow); }
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
fn void rgfw_window_setAspectRatio_X11(RgfwWindow* win, int w, int h)
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

fn RgfwFormat rgfw_nativeFormat_X11() => RGFW_FORMAT_BGRA8;

fn RgfwFormat rgfw_xImageGetFormat(void* imagePtr)
{
	// XImage*: bits_per_pixel/red_mask/green_mask/blue_mask live at a fixed
	// offset in the real struct; here we only ever set the format ourselves
	// right after xCreateImage, so this mirrors the original's fallback path.
	return RGFW_FORMAT_BGRA8;
}

fn bool rgfw_window_createSurfacePtr(RgfwWindow* win, char* data, int w, int h, RgfwFormat format, RgfwSurface* surface)
{
	if (surface == null) return RGFW_FALSE;
	surface.data = data;
	surface.w = w;
	surface.h = h;
	surface.format = format;

	XWindowAttributes attrs;
	if (xGetWindowAttributes(_rgfw.display, win.src.window, &attrs) == 0)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_BUFFER, "Failed to get window attributes.");
		return RGFW_FALSE;
	}

	surface.native.bitmap = xCreateImage(_rgfw.display, attrs.visual, (uint)attrs.depth, 2 /* ZPixmap */, 0, null, (uint)surface.w, (uint)surface.h, 32, 0);
	surface.native.buffer = (char*)rgfw_alloc_impl((usz)(w * h * 4));
	surface.native.format = rgfw_xImageGetFormat(surface.native.bitmap);

	if (surface.native.bitmap == null)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_BUFFER, "Failed to create XImage.");
		return RGFW_FALSE;
	}

	surface.native.format = RGFW_FORMAT_BGRA8;
	return RGFW_TRUE;
}

fn bool rgfw_createSurfacePtr_X11(char* data, int w, int h, RgfwFormat format, RgfwSurface* surface) => rgfw_window_createSurfacePtr(_rgfw.root, data, w, h, format, surface);

fn void rgfw_window_blitSurface_X11(RgfwWindow* win, RgfwSurface* surface)
{
	if (surface == null) return;
	// surface.native.bitmap->data = surface.native.buffer would go here in real Xlib
	// (XImage.data is a char* at a fixed offset); left to the caller's XImage helper.
	int minW = win.w < surface.w ? win.w : surface.w;
	int minH = win.h < surface.h ? win.h : surface.h;
	xPutImage(_rgfw.display, win.src.window, win.src.gc, surface.native.bitmap, 0, 0, 0, 0, (uint)minW, (uint)minH);
}

fn void rgfw_surface_freePtr_X11(RgfwSurface* surface)
{
	if (surface == null) return;
	rgfw_free_impl(surface.native.buffer);
	xDestroyImage(surface.native.bitmap);
}

fn void rgfw_window_setBorder_X11(RgfwWindow* win, bool border)
{
	rgfw_setBit(&win.internal.flags, RGFW_WINDOW_NO_BORDER, rgfw_bool(!border));
	X11WindowHints hints;
	hints.flags = 2;
	hints.decorations = border;
	xChangeProperty(_rgfw.display, win.src.window, _rgfw.motifWmHints, _rgfw.motifWmHints, 32, 0, (char*)&hints, 5);
	if (rgfw_window_isHidden(win) == 0)
	{
		rgfw_window_hide(win);
		rgfw_window_show(win);
	}
}

fn void rgfw_window_setRawMouseModePlatform_X11(RgfwWindow* win, bool state)
{
	char[4] mask;
	rgfw_memzero(&mask, 4);
	if (state) xiSetMask((char*)&mask, 6 /* XI_RawMotion */);
	XIEventMask em;
	em.deviceid = 1; // XIAllMasterDevices
	em.mask_len = 4;
	em.mask = (char*)&mask;
	if (rgfw_xiSelectEventsSRC != null) rgfw_xiSelectEventsSRC(_rgfw.display, xDefaultRootWindow(_rgfw.display), &em, 1);
}

fn void rgfw_window_captureMousePlatform_X11(RgfwWindow* win, bool state)
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

fn int rgfw_xErrorHandler(Display* display, XErrorEvent* ev)
{
	char[512] errorText;
	xGetErrorText(display, ev.error_code, (char*)&errorText, 512);
	rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_X11, (char*)&errorText);
	_rgfw.x11Error = ev;
	return 0;
}

fn void rgfw_x11IcCallback(void* ic, char* clientData, char* callData)
{
	RgfwWindow* win = (RgfwWindow*)(void*)clientData;
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
fn void rgfw_x11ImInitCallback(Display* d, XPointer clientData, XPointer callData)
{
	if (_rgfw.im != null) return;

	_rgfw.im = xOpenIM(_rgfw.display, null, null, null);
	if (_rgfw.im == null) return;

	bool found = RGFW_FALSE;
	XImStyles* styles = null;

	if (xGetIMValues(_rgfw.im, "queryInputStyle", &styles, (char*)null) != null)
	{
		found = RGFW_FALSE;
	}
	else
	{
		for (uint i = 0; i < styles.count_styles; i++)
		{
			if (styles.supported_styles[i] == (0x0008 | 0x0400) /* XIMPreeditNothing | XIMStatusNothing */)
			{
				found = RGFW_TRUE;
				break;
			}
		}
		xFree(styles);
	}

	if (found == RGFW_FALSE)
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

fn bool rgfw_xCreateWindow(XVisualInfo visual, char* name, RgfwWindowFlags flags, RgfwWindow* win)
{
	long eventMask = (1 << 0) | (1 << 1) | (1 << 2) | (1 << 3) | (1 << 6) | (1 << 17) | (1 << 21) |
		(1 << 5) | (1 << 4) | (1 << 15) | (1 << 16) | (1 << 22);

	XSetWindowAttributes swa;
	rgfw_memzero(&swa, XSetWindowAttributes::size);
	win.src.parent = xDefaultRootWindow(_rgfw.display);

	Colormap cmap = xCreateColormap(_rgfw.display, win.src.parent, visual.visual, 0);
	swa.colormap = cmap;
	swa.event_mask = eventMask;
	swa.background_pixmap = 0;

	win.src.window = xCreateWindow(_rgfw.display, win.src.parent, win.x, win.y, (uint)win.w, (uint)win.h,
		0, visual.depth, 1 /* InputOutput */, visual.visual, (1 << 3) | (1 << 13) | (1 << 11), &swa);
		// CWBorderPixel | CWColormap | CWEventMask
	if (win.src.window == 0) return RGFW_FALSE;

	win.src.flashEnd = 0;
	xFreeColors(_rgfw.display, cmap, null, 0, 0);
	xSaveContext(_rgfw.display, win.src.window, _rgfw.context, (XPointer)win);
	win.src.gc = xCreateGC(_rgfw.display, win.src.window, 0, null);

	if (_rgfw.im != null)
	{
		XImCallback callback;
		callback.callback = &rgfw_x11IcCallback;
		callback.client_data = (XPointer)win;

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

	win.src.x = win.x; win.src.y = win.y; win.src.w = win.w; win.src.h = win.h;
	xSetWindowBackground(_rgfw.display, win.src.window, 0);
	xClearWindow(_rgfw.display, win.src.window);
	xSetWindowBackgroundPixmap(_rgfw.display, win.src.window, 0);
	return RGFW_TRUE;
}

fn RgfwWindow* rgfw_createWindowPlatform_X11(char* name, RgfwWindowFlags flags, RgfwWindow* win)
{
	if ((flags & RGFW_WINDOW_OPEN_GL) || (flags & RGFW_WINDOW_EGL))
	{
		win.src.window = 0;
		return win;
	}
	XVisualInfo visual;
	rgfw_window_getVisual(&visual, rgfw_bool(win.internal.flags & RGFW_WINDOW_TRANSPARENT));
	if (rgfw_xCreateWindow(visual, name, flags, win) == RGFW_FALSE) return null;
	return win;
}

fn bool rgfw_getGlobalMouse_X11(int* fX, int* fY)
{
	int x, y;
	uint z;
	XWindow window1, window2;
	xQueryPointer(_rgfw.display, xDefaultRootWindow(_rgfw.display), &window1, &window2, fX, fY, &x, &y, &z);
	return RGFW_TRUE;
}

fn void rgfw_unix_parseURI(RgfwWindow* win, char* data)
{
	char* prefix = "file://";
	char* line;
	char* savePtr;
	while ((line = strtokR(data, "\r\n", &savePtr)) != null)
	{
		data = null;

		if (line[0] == '#') continue;

		char* l;
		for (l = line; true; l++)
		{
			if (l - line > 7) { break; }
			else if (*l != prefix[l - line]) { break; }
			else if (*l == 0 && prefix[l - line] == 0)
			{
				line += 7;
				while (*line != '/') line++;
				break;
			}
			else if (*l == 0) { break; }
		}

		usz len = strlen(line);
		char* path = (char*)rgfw_alloc_impl(len + 1);

		usz index = 0;
		while (*line != 0)
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
				path[index] = *line;
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
		for (usz i = 0; i < 2; i++)
		{
			if (request.target != formats[i]) continue;
			xChangeProperty(_rgfw.display, request.requestor, request.property, request.target,
				8, 0, _rgfw.unixClipboard.data, (int)_rgfw.unixClipboard.length);
		}
	}

	XEvent reply;
	rgfw_memzero(&reply, XEvent::size);
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

fn RgfwKey rgfw_physicalToMappedKey_X11(RgfwKey key)
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
				rgfw_rawMotionCallback(_rgfw.root, _rgfw.vectorX, _rgfw.vectorY);
			}
		}
		xFreeEventData(_rgfw.display, &e.xcookie);
		return;
	}

	RgfwWindow* win = null;
	if (xFindContext(_rgfw.display, e.xany.window, _rgfw.context, (XPointer*)&win) != 0) return;

	if (win.src.flashEnd != 0)
	{
		if (win.src.flashEnd <= rgfw_unix_getTimeNS() || rgfw_window_isInFocus(win)) rgfw_window_flash(win, RGFW_FLASH_CANCEL);
	}

	bool keyRepeat = RGFW_FALSE;
	if (e.type == 3 /* KeyRelease */ && xEventsQueued(_rgfw.display, 1) != 0)
	{
		XEvent ne;
		xPeekEvent(_rgfw.display, &ne);
		if (ne.type == 2 /* KeyPress */ && e.xkey.time == ne.xkey.time && e.xkey.keycode == ne.xkey.keycode)
		{
			xNextEvent(_rgfw.display, &e);
			keyRepeat = RGFW_TRUE;
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
			rgfw_keyUpdateKeyMods(win, rgfw_bool(state.locked_mods & LOCK_MASK), rgfw_bool(state.locked_mods & MOD2_MASK), rgfw_bool(state.locked_mods & MOD3_MASK));

			if (win.src.ic != null && xFilterEvent(&e, 0) == 0)
			{
				char[100] buffer;
				int status;
				usz count = xutf8LookupString(win.src.ic, &e.xkey, (char*)&buffer, 99, null, &status);
				if (status == 2 /* XLookupChars */ || status == 4 /* XLookupBoth */)
				{
					buffer[count] = 0;
					usz index = 0;
					while (index < count) { rgfw_keyCharCallback(win, rgfw_decodeUTF8((char*)&buffer, &index)); }
				}
			}
			else
			{
				XWindow rootRet, childRet;
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

			rgfw_keyCallback(win, value, win.internal.mod, keyRepeat, RGFW_TRUE);
			break;
		}
		case 3: // KeyRelease
		{
			if (!(win.internal.enabledEvents & RGFW_KEY_RELEASED_FLAG)) return;
			RgfwKey value = (RgfwKey)rgfw_apiKeyToRGFW(e.xkey.keycode);

			XkbStateRec state;
			xkbGetState(_rgfw.display, XKB_USE_CORE_KBD, &state);
			rgfw_keyUpdateKeyMods(win, rgfw_bool(state.locked_mods & LOCK_MASK), rgfw_bool(state.locked_mods & MOD2_MASK), rgfw_bool(state.locked_mods & MOD3_MASK));

			rgfw_keyCallback(win, value, win.internal.mod, RGFW_FALSE, RGFW_FALSE);
			break;
		}
		case 4: // ButtonPress
		{
			bool scroll = rgfw_bool(e.xbutton.button >= 4 && e.xbutton.button <= 7);
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
			rgfw_mouseButtonCallback(win, value, RGFW_TRUE);
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
			rgfw_mouseButtonCallback(win, value, RGFW_FALSE);
			break;
		}
		case 6: rgfw_mouseMotionCallback(win, e.xmotion.x, e.xmotion.y); // MotionNotify
		case 12: rgfw_windowRefreshCallback(win, e.xexpose.x, e.xexpose.y, e.xexpose.width, e.xexpose.height); // Expose
		case 28: // PropertyNotify
			if (e.xproperty.state == 0 /* PropertyNewValue */)
			{
				bool handled = RGFW_FALSE;
				if (e.xproperty.atom == _rgfw.wmState)
				{
					if (rgfw_window_isMinimized(win) && !(win.internal.flags & RGFW_WINDOW_MINIMIZE))
					{
						rgfw_windowMinimizedCallback(win);
						handled = RGFW_TRUE;
					}
				}
				else if (e.xproperty.atom == _rgfw.netWmState)
				{
					if (rgfw_window_isMaximized(win) && !(win.internal.flags & RGFW_WINDOW_MAXIMIZE))
					{
						rgfw_windowMaximizedCallback(win, win.x, win.y, win.w, win.h);
						handled = RGFW_TRUE;
					}
				}
				if (!handled) rgfw_window_checkMode(win);
			}
		case 19: case 18: // MapNotify, UnmapNotify
			rgfw_window_checkMode(win);
			break;
		case 33: // ClientMessage
		{
			if ((ulong)e.xclient.data.l[0] == _rgfw.wmDeleteWindow) { rgfw_windowCloseCallback(win); break; }

			if (!(win.internal.flags & RGFW_WINDOW_ALLOW_DND) || _rgfw.x11Version > 5 /* RGFW_XDND_VERSION */) return;

			int dragX = 0;
			int dragY = 0;

			if (e.xclient.message_type == _rgfw.xdndEnter)
			{
				ulong count;
				Atom* formats;
				Atom[3] realFormats;
				int list = (int)(e.xclient.data.l[1] & 1);

				_rgfw.x11Source = (XWindow)e.xclient.data.l[0];
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
					for (usz i = 2; i < 2 + 3; i++)
					{
						if (e.xclient.data.l[i] != 0)
						{
							realFormats[count] = (ulong)e.xclient.data.l[i];
							count++;
						}
					}
					formats = (Atom*)&realFormats;
				}

				for (usz i = 0; i < count; i++)
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
				XWindow dummy;
				int xpos, ypos;

				xTranslateCoordinates(_rgfw.display, xDefaultRootWindow(_rgfw.display), win.src.window,
					xabs, yabs, &xpos, &ypos, &dummy);

				dragX = xpos;
				dragY = ypos;

				rgfw_mouseMotionCallback(win, xpos, ypos);

				XEvent reply;
				rgfw_memzero(&reply, XEvent::size);
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
					rgfw_memzero(&reply, XEvent::size);
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

			char* data;
			ulong result;
			Atom actualType;
			int actualFormat;
			ulong bytesAfter;

			xGetWindowProperty(_rgfw.display, e.xselection.requestor, e.xselection.property, 0, ~0, 0, e.xselection.target,
				&actualType, &actualFormat, &result, &bytesAfter, &data);

			if (result != 0)
			{
				rgfw_unix_parseURI(win, data);
				if (data != null) xFree(data);
			}

			if (_rgfw.x11Version >= 2)
			{
				XEvent reply;
				rgfw_memzero(&reply, XEvent::size);
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
				rgfw_windowResizedCallback(win, e.xconfigure.width, e.xconfigure.height);
			}
			int configureX = e.xconfigure.x;
			int configureY = e.xconfigure.y;

			// if the event came from the server and we're not a direct child of the root window then
			// we're using local coords which need to be translated into screen coords
			XWindow root = xDefaultRootWindow(_rgfw.display);
			if (e.xany.send_event == 0 && win.src.parent != root)
			{
				XWindow dummy;
				xTranslateCoordinates(_rgfw.display, win.src.parent, root, configureX, configureY, &configureX, &configureY, &dummy);
			}

			if (configureX != win.src.x || configureY != win.src.y)
			{
				win.src.x = configureX;
				win.src.y = configureY;
				rgfw_windowMovedCallback(win, configureX, configureY);
			}
			return;
		}
		case 9: // FocusIn
			if (win.src.ic != null) xSetICFocus(win.src.ic);
			rgfw_windowFocusCallback(win, RGFW_TRUE);
		case 10: // FocusOut
			if (win.src.ic != null) xUnsetICFocus(win.src.ic);
			rgfw_windowFocusCallback(win, RGFW_FALSE);
		case 7: rgfw_mouseNotifyCallback(win, e.xcrossing.x, e.xcrossing.y, RGFW_TRUE); // EnterNotify
		case 8: rgfw_mouseNotifyCallback(win, win.internal.lastMouseX, win.internal.lastMouseY, RGFW_FALSE); // LeaveNotify
		case 21: win.src.parent = e.xreparent.parent; // ReparentNotify
		default: break;
	}

	xFlush(_rgfw.display);
}

fn bool rgfw_window_fetchSize_X11(RgfwWindow* win, int* w, int* h)
{
	XWindowAttributes attribs;
	xGetWindowAttributes(_rgfw.display, win.src.window, &attribs);
	win.w = attribs.width;
	win.h = attribs.height;
	return rgfw_window_getSize(win, w, h);
}

fn void rgfw_pollEvents_X11()
{
	rgfw_resetPrevState();
	xPending(_rgfw.display);
	while (qLength(_rgfw.display) != 0) rgfw_xHandleEvent();
}

fn void rgfw_window_movePlatform_X11(RgfwWindow* win, int x, int y)
{
	win.x = x; win.y = y;
	xMoveWindow(_rgfw.display, win.src.window, x, y);
}

fn void rgfw_window_resizePlatform_X11(RgfwWindow* win, int w, int h)
{
	win.w = w; win.h = h;
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

fn void rgfw_window_setMinSize_X11(RgfwWindow* win, int w, int h)
{
	long flags;
	XSizeHints hints;
	rgfw_memzero(&hints, XSizeHints::size);
	xGetWMNormalHints(_rgfw.display, win.src.window, &hints, &flags);
	hints.flags |= 1 << 4; // PMinSize
	hints.min_width = w; hints.min_height = h;
	xSetWMNormalHints(_rgfw.display, win.src.window, &hints);
}

fn void rgfw_window_setMaxSize_X11(RgfwWindow* win, int w, int h)
{
	long flags;
	XSizeHints hints;
	rgfw_memzero(&hints, XSizeHints::size);
	xGetWMNormalHints(_rgfw.display, win.src.window, &hints, &flags);
	hints.flags |= 1 << 5; // PMaxSize
	hints.max_width = w; hints.max_height = h;
	xSetWMNormalHints(_rgfw.display, win.src.window, &hints);
}

fn void rgfw_toggleXMaximized(RgfwWindow* win, bool maximized)
{
	XEvent xev;
	rgfw_memzero(&xev, XEvent::size);
	xev.type = 33; // ClientMessage
	xev.xclient.window = win.src.window;
	xev.xclient.message_type = _rgfw.netWmState;
	xev.xclient.format = 32;
	xev.xclient.data.l[0] = maximized;
	xev.xclient.data.l[1] = (long)_rgfw.netWmStateMaximizedHorz;
	xev.xclient.data.l[2] = (long)_rgfw.netWmStateMaximizedVert;
	xSendEvent(_rgfw.display, xDefaultRootWindow(_rgfw.display), 0, (1 << 20) | (1 << 19), &xev);
}

fn void rgfw_window_maximizePlatform_X11(RgfwWindow* win) => rgfw_toggleXMaximized(win, 1);

fn void rgfw_window_focus_X11(RgfwWindow* win)
{
	XWindowAttributes attr;
	xGetWindowAttributes(_rgfw.display, win.src.window, &attr);
	if (attr.map_state != 2 /* IsViewable */) return;
	xSetInputFocus(_rgfw.display, win.src.window, 1 /* RevertToPointerRoot */, 0);
	xFlush(_rgfw.display);
}

fn void rgfw_window_raise_X11(RgfwWindow* win)
{
	xMapRaised(_rgfw.display, win.src.window);
	rgfw_window_setFullscreen(win, rgfw_window_isFullscreen(win));
}

fn void rgfw_window_setXAtom(RgfwWindow* win, Atom netAtom, bool fullscreen)
{
	XEvent xev;
	rgfw_memzero(&xev, XEvent::size);
	xev.xclient.type = 33;
	xev.xclient.send_event = 1;
	xev.xclient.message_type = _rgfw.netWmState;
	xev.xclient.window = win.src.window;
	xev.xclient.format = 32;
	xev.xclient.data.l[0] = fullscreen;
	xev.xclient.data.l[1] = (long)netAtom;
	xSendEvent(_rgfw.display, xDefaultRootWindow(_rgfw.display), 0, (1 << 19) | (1 << 20), &xev);
}

fn void rgfw_window_setFullscreen_X11(RgfwWindow* win, bool fullscreen)
{
	if (fullscreen)
	{
		win.internal.flags |= RGFW_WINDOW_FULLSCREEN;
		win.internal.oldX = win.x; win.internal.oldY = win.y; win.internal.oldW = win.w; win.internal.oldH = win.h;
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

fn void rgfw_window_setFloating_X11(RgfwWindow* win, bool floating) => rgfw_window_setXAtom(win, _rgfw.netWmStateAbove, floating);

fn void rgfw_window_setOpacity_X11(RgfwWindow* win, char opacity)
{
	uint value = (uint)(4294967295.0 * (double)opacity);
	xChangeProperty(_rgfw.display, win.src.window, _rgfw.netWmWindowOpacity, 6, 32, 0, (char*)&value, 1);
}

fn void rgfw_window_minimizePlatform_X11(RgfwWindow* win)
{
	if (rgfw_window_isMaximized(win)) return;
	win.internal.oldX = win.x; win.internal.oldY = win.y; win.internal.oldW = win.w; win.internal.oldH = win.h;
	xIconifyWindow(_rgfw.display, win.src.window, xDefaultScreen(_rgfw.display));
	xFlush(_rgfw.display);
}

fn void rgfw_window_restorePlatform_X11(RgfwWindow* win)
{
	rgfw_toggleXMaximized(win, RGFW_FALSE);
	xFlush(_rgfw.display);
}

fn bool rgfw_window_isFloating_X11(RgfwWindow* win)
{
	Atom actualType;
	int actualFormat;
	ulong nitems, bytesAfter;
	Atom* propReturn = null;
	int status = xGetWindowProperty(_rgfw.display, win.src.window, _rgfw.netWmState, 0, ~0, 0, 4, &actualType, &actualFormat, &nitems, &bytesAfter, (char**)&propReturn);
	if (status != 0 || actualType != 4) return RGFW_FALSE;
	for (ulong i = 0; i < nitems; i++) if (propReturn[i] == _rgfw.netWmStateAbove) return RGFW_TRUE;
	if (propReturn != null) xFree(propReturn);
	return RGFW_FALSE;
}

fn void rgfw_window_setName_X11(RgfwWindow* win, char* name)
{
	if (name == null) name = "";
	xutf8SetWMProperties(_rgfw.display, win.src.window, name, name, null, 0, null, null, null);
	xStoreName(_rgfw.display, win.src.window, name);
	xChangeProperty(_rgfw.display, win.src.window, _rgfw.netWmName, _rgfw.utf8String, 8, 0, name, (int)strlen(name));
}

fn void rgfw_window_setMousePassthrough_X11(RgfwWindow* win, bool passthrough)
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

fn bool rgfw_window_setIconEx_X11(RgfwWindow* win, char* dataSrc, int w, int h, RgfwFormat format, RgfwIcon type)
{
	if (dataSrc == null)
	{
		return rgfw_bool(xChangeProperty(_rgfw.display, win.src.window, _rgfw.netWmIcon, 6, 32, 0, null, 0));
	}
	int count = 2 + (w * h);
	ulong* data = (ulong*)rgfw_alloc_impl((usz)count * ulong::size);
	rgfw_memzero(data, (usz)count * ulong::size);
	data[0] = (ulong)w;
	data[1] = (ulong)h;

	rgfw_copyImageData64((char*)&data[2], w, h, RGFW_FORMAT_BGRA8, dataSrc, format, RGFW_TRUE, null);
	bool res = RGFW_TRUE;
	if (type & RGFW_ICON_TASKBAR)
	{
		res = rgfw_bool(xChangeProperty(_rgfw.display, win.src.window, _rgfw.netWmIcon, 6, 32, 0, (char*)data, count));
	}

	rgfw_copyImageData64((char*)&data[2], w, h, RGFW_FORMAT_BGRA8, dataSrc, format, RGFW_FALSE, null);

	if (type & RGFW_ICON_WINDOW)
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

fn RgfwMouse* rgfw_createMouseStandard_X11(RgfwMouseIcon mouse)
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

fn RgfwMouse* rgfw_createMouse_X11(char* data, int w, int h, RgfwFormat format)
{
	if (rgfw_xcursorImageCreateSRC == null) return null;
	XcursorImage* native = rgfw_xcursorImageCreateSRC(w, h);
	native.xhot = 0; native.yhot = 0;
	rgfw_memzero(native.pixels, (usz)(w * h * 4));
	rgfw_memcpy(native.pixels, data, (usz)(w * h * 4));
	XCursor cursor = rgfw_xcursorImageLoadCursorSRC(_rgfw.display, native);
	rgfw_xcursorImageDestroySRC(native);
	return (RgfwMouse*)cursor;
}

fn bool rgfw_window_setMousePlatform_X11(RgfwWindow* win, RgfwMouse* mouse)
{
	xDefineCursor(_rgfw.display, win.src.window, (XCursor)mouse);
	return RGFW_TRUE;
}

fn void rgfw_freeMouse_X11(RgfwMouse* mouse) { xFreeCursor(_rgfw.display, (XCursor)mouse); }

fn void rgfw_window_moveMouse_X11(RgfwWindow* win, int x, int y)
{
	int rootX, rootY, winX, winY;
	uint mask;
	XWindow rootRet, childRet;
	xQueryPointer(_rgfw.display, xDefaultRootWindow(_rgfw.display), &rootRet, &childRet, &rootX, &rootY, &winX, &winY, &mask);
	win.internal.lastMouseX = x - win.x;
	win.internal.lastMouseY = y - win.y;
	if (winX == x && winY == y) return;
	xWarpPointer(_rgfw.display, 0, win.src.window, 0, 0, 0, 0, x - win.x, y - win.y);
}

fn void rgfw_window_hide_X11(RgfwWindow* win)
{
	win.internal.flags |= (uint)RGFW_WINDOW_HIDE;
	xUnmapWindow(_rgfw.display, win.src.window);
	xFlush(_rgfw.display);
}

fn bool rgfw_waitForShowEvent_X11(RgfwWindow* win)
{
	XEvent dummy;
	while (xCheckTypedWindowEvent(_rgfw.display, win.src.window, 15 /* VisibilityNotify */, &dummy) == 0)
	{
		rgfw_waitForEvent(100);
	}

	return RGFW_TRUE;
}

fn void rgfw_window_showPlatform_X11(RgfwWindow* win)
{
	win.internal.flags &= ~(uint)RGFW_WINDOW_HIDE;
	if (rgfw_window_isHidden(win) == RGFW_FALSE) return;
	xMapWindow(_rgfw.display, win.src.window);
	rgfw_window_move(win, win.x, win.y);

	rgfw_waitForShowEvent_X11(win);
	rgfw_window_setFullscreen(win, rgfw_window_isFullscreen(win));
}

fn void rgfw_window_flash_X11(RgfwWindow* win, RgfwFlashRequest request)
{
	if (rgfw_window_isInFocus(win) && request) return;
	XWMHints* wmhints = xGetWMHints(_rgfw.display, win.src.window);
	if (wmhints == null) return;
	if (request)
	{
		wmhints.flags |= 1 << 8; // XUrgencyHint
		if (request == RGFW_FLASH_BRIEFLY) win.src.flashEnd = rgfw_unix_getTimeNS() + (ulong)1e9;
		if (request == RGFW_FLASH_UNTIL_FOCUSED) win.src.flashEnd = (ulong)-1;
	}
	else
	{
		win.src.flashEnd = 0;
		wmhints.flags &= ~(1 << 8);
	}
	xSetWMHints(_rgfw.display, win.src.window, wmhints);
	xFree(wmhints);
}

fn bool rgfw_readClipboardPtr_X11(RgfwDataTransferType requestedType, char* buffer, usz capacity, RgfwDataTransfer* dataTransfer)
{
	if (requestedType != RGFW_DATA_TEXT) return RGFW_FALSE;
	dataTransfer.data = buffer;

	if (xGetSelectionOwner(_rgfw.display, _rgfw.clipboardAtom) == _rgfw.helperWindow)
	{
		dataTransfer.length = _rgfw.unixClipboard.length;
		if (buffer != null && _rgfw.unixClipboard.data != null)
		{
			if (_rgfw.unixClipboard.length > capacity) return RGFW_FALSE;
			rgfw_memcpy(buffer, (void*)_rgfw.unixClipboard.data, _rgfw.unixClipboard.length);
		}
		dataTransfer.type = RGFW_DATA_TEXT;
		return RGFW_TRUE;
	}

	xConvertSelection(_rgfw.display, _rgfw.clipboardAtom, _rgfw.utf8String, _rgfw.xselData, _rgfw.helperWindow, 0);
	xSync(_rgfw.display, 0);
	XEvent event;
	while (true)
	{
		xNextEvent(_rgfw.display, &event);
		if (event.type != 31 /* SelectionNotify */) continue;
		if (event.xselection.selection != _rgfw.clipboardAtom || event.xselection.property == 0) return RGFW_FALSE;
		break;
	}

	Atom target;
	int format;
	ulong n, size;
	char* data;
	xGetWindowProperty(_rgfw.display, event.xselection.requestor, event.xselection.property, 0, ~0, 0, 0, &target, &format, &size, &n, (char**)&data);

	bool ret = RGFW_TRUE;
	usz length = size;
	if (data[size - 1] != 0) length += 1;

	if (size > capacity && buffer != null) { ret = RGFW_FALSE; }
	else if ((target == _rgfw.utf8String || target == 31) && buffer != null)
	{
		rgfw_memcpy(buffer, data, size);
		buffer[length - 1] = 0;
		xFree(data);
	}
	else if (buffer != null) { ret = RGFW_FALSE; }

	xDeleteProperty(_rgfw.display, event.xselection.requestor, event.xselection.property);
	dataTransfer.length = length;
	dataTransfer.type = RGFW_DATA_TEXT;
	return ret;
}

fn bool rgfw_writeClipboard_X11(RgfwDataTransfer* data)
{
	xSetSelectionOwner(_rgfw.display, _rgfw.clipboardAtom, _rgfw.helperWindow, 0);
	if (xGetSelectionOwner(_rgfw.display, _rgfw.clipboardAtom) != _rgfw.helperWindow)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_CLIPBOARD, "X11 failed to become owner of clipboard selection");
		return RGFW_FALSE;
	}
	if (_rgfw.unixClipboard != null) { rgfw_free_impl(_rgfw.unixClipboard); _rgfw.unixClipboard = null; }

	usz length = data.length;
	if (data.data[data.length - 1] != 0) length += 1;

	_rgfw.unixClipboard = (RgfwDataTransfer*)rgfw_alloc_impl(RgfwDataTransfer::size + data.length);
	char* dataPtr = (char*)_rgfw.unixClipboard + RgfwDataTransfer::size - 1;
	rgfw_memcpy(dataPtr, (void*)data.data, data.length);
	dataPtr[length - 1] = 0;
	_rgfw.unixClipboard.data = dataPtr;
	_rgfw.unixClipboard.type = RGFW_DATA_TEXT;
	_rgfw.unixClipboard.length = length;
	return RGFW_TRUE;
}

fn bool rgfw_window_isHidden_X11(RgfwWindow* win)
{
	XWindowAttributes windowAttributes;
	xGetWindowAttributes(_rgfw.display, win.src.window, &windowAttributes);
	return rgfw_bool(windowAttributes.map_state != 2);
}

fn bool rgfw_window_isMinimized_X11(RgfwWindow* win)
{
	Atom actualType;
	int actualFormat;
	ulong nitems, bytesAfter;
	char* propData;
	int status = xGetWindowProperty(_rgfw.display, win.src.window, _rgfw.wmState, 0, 2, 0, 0, &actualType, &actualFormat, &nitems, &bytesAfter, &propData);
	if (status == 0 && nitems >= 1 && propData == (char*)(uptr)3 /* IconicState */)
	{
		xFree(propData);
		return RGFW_TRUE;
	}
	if (propData != null) xFree(propData);
	XWindowAttributes windowAttributes;
	xGetWindowAttributes(_rgfw.display, win.src.window, &windowAttributes);
	return rgfw_bool(windowAttributes.map_state != 2);
}

fn bool rgfw_window_isMaximized_X11(RgfwWindow* win)
{
	Atom actualType;
	int actualFormat;
	ulong nitems, bytesAfter;
	char* propData;
	int status = xGetWindowProperty(_rgfw.display, win.src.window, _rgfw.netWmState, 0, 1024, 0, 4, &actualType, &actualFormat, &nitems, &bytesAfter, &propData);
	if (status != 0) { if (propData != null) xFree(propData); return RGFW_FALSE; }
	for (ulong i = 0; i < nitems; i++)
	{
		if ((Atom)propData[i] == _rgfw.netWmStateMaximizedVert || (Atom)propData[i] == _rgfw.netWmStateMaximizedHorz)
		{
			xFree(propData);
			return RGFW_TRUE;
		}
	}
	if (propData != null) xFree(propData);
	return RGFW_FALSE;
}

fn void rgfw_xGetSystemContentDPI(float* dpi)
{
	if (dpi == null) return;
	float dpiOutput = 96.0f;
	char* rms = xResourceManagerString(_rgfw.display);
	if (rms == null) return;
	void* db = xrmGetStringDatabase(rms);
	if (db == null) return;
	char* type = null;
	// xrmGetResource's value-out parameter is a small {len,addr} struct; omitted here.
	xrmDestroyDatabase(db);
	*dpi = dpiOutput;
}

fn void* rgfw_xGetMode(XRRCrtcInfo* ci, XRRScreenResources* res, RRMode mode, RgfwMonitorMode* foundMode)
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
	foundMode.refreshRate = 0;
	if (mi.hTotal == 0 || mi.vTotal == 0) return mi;

	uint vTotal = mi.vTotal;
	int numerator = (int)mi.dotClock;
	int denominator = (int)(mi.hTotal * vTotal);
	if (denominator <= 0) denominator = 1;
	float refreshRate = (float)numerator / (float)denominator;
	foundMode.refreshRate = (float)((int)(refreshRate * 100 + 0.5f)) / 100.0f;
	return mi;
}

fn void rgfw_pollMonitors_X11()
{
	XWindow root = xDefaultRootWindow(_rgfw.display);
	XRRScreenResources* res = xRRGetScreenResourcesCurrent(_rgfw.display, root);
	if (res == null) return;
	RROutput primary = xRRGetOutputPrimary(_rgfw.display, root);

	for (RgfwMonitorNode* node = _rgfw.monitors.list.head; node != null; node = node.next) node.disconnected = RGFW_TRUE;

	for (int i = 0; i < res.noutput; i++)
	{
		RgfwMonitorNode* node = null;
		for (node = _rgfw.monitors.list.head; node != null; node = node.next)
		{
			if (node.nativeId0 == res.outputs[i]) break;
		}
		if (node != null)
		{
			node.disconnected = RGFW_FALSE;
			if (node.nativeId0 == primary) _rgfw.monitors.primary = node;
			continue;
		}

		RgfwMonitor monitor;
		XRROutputInfo* info = xRRGetOutputInfo(_rgfw.display, res, res.outputs[i]);
		if (info == null) continue;
		if (info.connection != 0 || info.crtc == 0) { xRRFreeOutputInfo(info); continue; }

		XRRCrtcInfo* ci = xRRGetCrtcInfo(_rgfw.display, res, info.crtc);
		if (ci == null) continue;

		float physW = (float)info.mm_width / 25.4f;
		float physH = (float)info.mm_height / 25.4f;

		usz n = strlen(info.name);
		if (n > 127) n = 127;
		rgfw_memcpy(&monitor.name, info.name, n);
		monitor.name[n] = 0;

		if (physW > 0.0f && physH > 0.0f) { monitor.physW = physW; monitor.physH = physH; }
		else { monitor.physW = (float)ci.width / 96.0f; monitor.physH = (float)ci.height / 96.0f; }

		monitor.x = ci.x; monitor.y = ci.y;

		float dpi = 96.0f;
		rgfw_xGetSystemContentDPI(&dpi);
		monitor.scaleX = dpi / 96.0f; monitor.scaleY = dpi / 96.0f;
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
		rgfw_monitorCallback(_rgfw.root, &node.mon, RGFW_TRUE);
	}

	xRRFreeScreenResources(res);
	rgfw_monitors_refresh();
}

fn void rgfw_monitorNode_free_X11(RgfwMonitorNode* node) {}

fn bool rgfw_monitor_getWorkarea_X11(RgfwMonitor* monitor, int* x, int* y, int* width, int* height)
{
	XWindow root = xDefaultRootWindow(_rgfw.display);
	int areaX = monitor.x;
	int areaY = monitor.y;
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
	return RGFW_TRUE;
}

fn usz rgfw_monitor_getModesPtr_X11(RgfwMonitor* monitor, RgfwMonitorMode** modes)
{
	XRRScreenResources* res = xRRGetScreenResourcesCurrent(_rgfw.display, xDefaultRootWindow(_rgfw.display));
	if (res == null) return 0;
	XRRCrtcInfo* ci = xRRGetCrtcInfo(_rgfw.display, res, monitor.node.nativeId1);
	XRROutputInfo* oi = xRRGetOutputInfo(_rgfw.display, res, monitor.node.nativeId0);
	usz count = (usz)oi.nmode;
	for (int i = 0; modes != null && i < oi.nmode; i++) rgfw_xGetMode(ci, res, oi.modes[i], &(*modes)[i]);
	xRRFreeOutputInfo(oi);
	xRRFreeCrtcInfo(ci);
	xRRFreeScreenResources(res);
	return count;
}

fn usz rgfw_monitor_getGammaRampPtr_X11(RgfwMonitor* monitor, RgfwGammaRamp* ramp)
{
	usz size = (usz)xRRGetCrtcGammaSize(_rgfw.display, monitor.node.nativeId1);
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

fn bool rgfw_monitor_setGammaRamp_X11(RgfwMonitor* monitor, RgfwGammaRamp* ramp)
{
	usz size = (usz)xRRGetCrtcGammaSize(_rgfw.display, monitor.node.nativeId1);
	if (size != ramp.count)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_X11, "X11: Gamma ramp size must match current ramp size");
		return RGFW_FALSE;
	}
	XRRCrtcGamma* gamma = xRRAllocGamma((int)ramp.count);
	rgfw_memcpy(gamma.red, ramp.red, ramp.count * ushort::size);
	rgfw_memcpy(gamma.green, ramp.green, ramp.count * ushort::size);
	rgfw_memcpy(gamma.blue, ramp.blue, ramp.count * ushort::size);
	xRRSetCrtcGamma(_rgfw.display, monitor.node.nativeId1, gamma);
	xRRFreeGamma(gamma);
	return RGFW_TRUE;
}

fn bool rgfw_monitor_setMode_X11(RgfwMonitor* mon, RgfwMonitorMode* mode)
{
	bool out = RGFW_FALSE;
	XRRScreenResources* res = xRRGetScreenResourcesCurrent(_rgfw.display, xDefaultRootWindow(_rgfw.display));
	XRRCrtcInfo* ci = xRRGetCrtcInfo(_rgfw.display, res, mon.node.nativeId1);
	if (xRRSetCrtcConfig(_rgfw.display, res, mon.node.nativeId1, 0, ci.x, ci.y, (RRMode)(uptr)mode.src, ci.rotation, ci.outputs, ci.noutput) == 0) out = RGFW_TRUE;
	xRRFreeCrtcInfo(ci);
	xRRFreeScreenResources(res);
	return out;
}

fn bool rgfw_monitor_requestMode_X11(RgfwMonitor* mon, RgfwMonitorMode* mode, RgfwModeRequest request)
{
	bool output = RGFW_FALSE;
	XRRScreenResources* res = xRRGetScreenResourcesCurrent(_rgfw.display, xDefaultRootWindow(_rgfw.display));
	if (res == null) return RGFW_FALSE;
	XRRCrtcInfo* ci = xRRGetCrtcInfo(_rgfw.display, res, mon.node.nativeId1);
	XRROutputInfo* oi = xRRGetOutputInfo(_rgfw.display, res, mon.node.nativeId0);
	RRMode native = 0;

	for (int i = 0; i < oi.nmode; i++)
	{
		RgfwMonitorMode foundMode;
		void* mi = rgfw_xGetMode(ci, res, oi.modes[i], &foundMode);
		if (mi == null) continue;
		if (rgfw_monitorModeCompare(mode, &foundMode, request))
		{
			native = ((XRRModeInfoNative*)mi).id;
			output = RGFW_TRUE;
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

fn RgfwMonitor* rgfw_window_getMonitor_X11(RgfwWindow* win)
{
	XWindowAttributes attrs;
	if (xGetWindowAttributes(_rgfw.display, win.src.window, &attrs) == 0) return null;
	for (RgfwMonitorNode* node = _rgfw.monitors.list.head; node != null; node = node.next)
	{
		if ((attrs.x < node.mon.x + node.mon.mode.w) && (attrs.x + attrs.width > node.mon.x) &&
			(attrs.y < node.mon.y + node.mon.mode.h) && (attrs.y + attrs.height > node.mon.y))
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

fn bool rgfw_extensionSupportedPlatform_OpenGL_X11(char* extension, usz len)
{
	char* extensions = rgfw_glXQueryExtensionsString(_rgfw.display, xDefaultScreen(_rgfw.display));
	return rgfw_bool(extensions != null && rgfw_extensionSupportedStr(extensions, extension, len));
}

fn RgfwProc rgfw_getProcAddress_OpenGL_X11(char* procname)
{
	if (_rgfw.glGetProcAddress == null) return null;
	return ((GlxGetProcAddressProc)_rgfw.glGetProcAddress)(procname);
}

fn bool rgfw_window_createContextPtr_OpenGL_X11(RgfwWindow* win, RgfwGlContext* context, RgfwGlHints* hints)
{
	win.src.ctx.native = context;
	win.src.gfxType = RGFW_GFX_NATIVE_OPEN_GL;

	bool showWindow = RGFW_FALSE;
	if (win.src.window != 0)
	{
		showWindow = rgfw_bool(rgfw_window_isMinimized(win) == RGFW_FALSE);
		rgfw_window_closePlatform(win);
	}

	bool transparent = rgfw_bool(win.internal.flags & RGFW_WINDOW_TRANSPARENT);

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
		char* srgbArbStr = "GLX_ARB_framebuffer_sRGB";
		char* srgbExtStr = "GLX_EXT_framebuffer_sRGB";
		if (rgfw_extensionSupportedPlatform_OpenGL_X11(srgbArbStr, strlen(srgbArbStr) + 1)) rgfw_attribStack_pushAttribs(&stack, 0x20B2, hints.sRGB); // GLX_FRAMEBUFFER_SRGB_CAPABLE_ARB
		if (rgfw_extensionSupportedPlatform_OpenGL_X11(srgbExtStr, strlen(srgbExtStr) + 1)) rgfw_attribStack_pushAttribs(&stack, 0x20B2, hints.sRGB); // GLX_FRAMEBUFFER_SRGB_CAPABLE_EXT
	}

	rgfw_attribStack_pushAttribs(&stack, 0, 0);

	int fbcount;
	GLXFBConfig* fbc = rgfw_glXChooseFBConfig(_rgfw.display, xDefaultScreen(_rgfw.display), (int*)&visualAttribs, &fbcount);
	if (fbc == null || fbcount == 0)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_OPEN_GL_CONTEXT, "Failed to find any valid GLX visual configs.");
		return RGFW_FALSE;
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
		return RGFW_FALSE;
	}

	GLXFBConfig bestFbc = fbc[bestFbcIdx];
	XVisualInfo* vi = rgfw_glXGetVisualFromFBConfig(_rgfw.display, bestFbc);
	xFree(fbc);
	XVisualInfo visual = *vi;
	xFree(vi);

	if (rgfw_xCreateWindow(visual, "", win.internal.flags, win) == RGFW_FALSE) return RGFW_FALSE;
	if (showWindow) rgfw_window_show(win);

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

	char* flushStr = "GLX_ARB_context_flush_control";
	if (rgfw_extensionSupportedPlatform_OpenGL_X11(flushStr, strlen(flushStr) + 1))
	{
		if (hints.releaseBehavior == RGFW_GL_RELEASE_FLUSH) { rgfw_attribStack_pushAttribs(&stack, 0x2097, 0x2098); } // GLX_CONTEXT_RELEASE_BEHAVIOR_ARB, GLX_CONTEXT_RELEASE_BEHAVIOR_FLUSH_ARB
		else if (hints.releaseBehavior == RGFW_GL_RELEASE_NONE) { rgfw_attribStack_pushAttribs(&stack, 0x2097, 0); } // GLX_CONTEXT_RELEASE_BEHAVIOR_ARB, GLX_CONTEXT_RELEASE_BEHAVIOR_NONE_ARB
	}

	char* robustStr = "GLX_ARB_create_context_robustness";
	int glFlags = 0;
	if (hints.debug) glFlags |= 0x00000001; // GLX_CONTEXT_DEBUG_BIT_ARB
	if (hints.robustness && rgfw_extensionSupportedPlatform_OpenGL_X11(robustStr, strlen(robustStr) + 1)) glFlags |= 0x00000004; // GLX_CONTEXT_ROBUST_ACCESS_BIT_ARB
	if (glFlags != 0) rgfw_attribStack_pushAttribs(&stack, 0x2094, glFlags); // GLX_CONTEXT_FLAGS_ARB

	char* noErrorStr = "GLX_ARB_create_context_no_error";
	if (rgfw_extensionSupportedPlatform_OpenGL_X11(noErrorStr, strlen(noErrorStr) + 1)) rgfw_attribStack_pushAttribs(&stack, 0x31B3, hints.noError); // GLX_CONTEXT_OPENGL_NO_ERROR_ARB

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
	return RGFW_TRUE;
}

fn void rgfw_window_deleteContextPtr_OpenGL_X11(RgfwWindow* win, RgfwGlContext* ctx)
{
	if (win.src.ctx.native.window != win.src.window) rgfw_glXDestroyWindow(_rgfw.display, win.src.ctx.native.window);
	rgfw_glXDestroyContext(_rgfw.display, ctx.ctx);
	win.src.ctx.native = null;
	rgfw_debugCallback(RGFW_TYPE_INFO, RGFW_INFO_OPEN_GL, "OpenGL context freed.");
}

fn void rgfw_window_makeCurrentContext_OpenGL_X11(RgfwWindow* win)
{
	if (win == null) { rgfw_glXMakeCurrent(null, 0, null); }
	else { rgfw_glXMakeCurrent(_rgfw.display, win.src.ctx.native.window, win.src.ctx.native.ctx); }
}

fn void* rgfw_getCurrentContext_OpenGL_X11() => rgfw_glXGetCurrentContext();
fn void rgfw_window_swapBuffers_OpenGL_X11(RgfwWindow* win) { rgfw_glXSwapBuffers(_rgfw.display, win.src.ctx.native.window); }

fn void rgfw_window_swapInterval_OpenGL_X11(RgfwWindow* win, int swapInterval)
{
	if (rgfw_glXSwapIntervalEXT != null) { rgfw_glXSwapIntervalEXT(_rgfw.display, win.src.ctx.native.window, swapInterval); }
	else if (rgfw_glXSwapIntervalNative != null) { rgfw_glXSwapIntervalNative(swapInterval); }
	else { rgfw_debugCallback(RGFW_TYPE_WARNING, RGFW_ERR_OPEN_GL_CONTEXT, "Failed to load swap interval function"); }
}

fn int rgfw_initPlatform_X11(char* className, RgfwInitFlags flags)
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
	rgfw_memzero(&wa, XSetWindowAttributes::size);
	wa.event_mask = 1 << 22; // PropertyChangeMask
	_rgfw.helperWindow = xCreateWindow(_rgfw.display, xDefaultRootWindow(_rgfw.display), 0, 0, 1, 1, 0, 0,
		2 /* InputOnly */, xDefaultVisual(_rgfw.display, xDefaultScreen(_rgfw.display)), 1 << 11, &wa);
	if (_rgfw.helperWindow == 0) return -1;

	rgfw_x11CursorHandle = dlopen("libXcursor.so.1", RTLD_LAZY | RTLD_LOCAL);
	if (rgfw_x11CursorHandle != null)
	{
		rgfw_xcursorImageCreateSRC = (PFN_XcursorImageCreate)dlsym(rgfw_x11CursorHandle, "XcursorImageCreate");
		rgfw_xcursorImageDestroySRC = (PFN_XcursorImageDestroy)dlsym(rgfw_x11CursorHandle, "XcursorImageDestroy");
		rgfw_xcursorImageLoadCursorSRC = (PFN_XcursorImageLoadCursor)dlsym(rgfw_x11CursorHandle, "XcursorImageLoadCursor");
	}
	rgfw_x11XiHandle = dlopen("libXi.so.6", RTLD_LAZY | RTLD_LOCAL);
	if (rgfw_x11XiHandle != null) rgfw_xiSelectEventsSRC = (PFN_XISelectEvents)dlsym(rgfw_x11XiHandle, "XISelectEvents");
	rgfw_x11ExtHandle = dlopen("libXext.so.6", RTLD_LAZY | RTLD_LOCAL);
	if (rgfw_x11ExtHandle != null)
	{
		rgfw_xSyncCreateCounterSRC = (PFN_XSyncCreateCounter)dlsym(rgfw_x11ExtHandle, "XSyncCreateCounter");
		rgfw_xSyncIntToValueSRC = (PFN_XSyncIntToValue)dlsym(rgfw_x11ExtHandle, "XSyncIntToValue");
		rgfw_xSyncSetCounterSRC = (PFN_XSyncSetCounter)dlsym(rgfw_x11ExtHandle, "XSyncSetCounter");
		rgfw_xShapeCombineRegionSRC = (PFN_XShapeCombineRegion)dlsym(rgfw_x11ExtHandle, "XShapeCombineRegion");
		rgfw_xShapeCombineMaskSRC = (PFN_XShapeCombineMask)dlsym(rgfw_x11ExtHandle, "XShapeCombineMask");
	}

	char[4] blk = {0, 0, 0, 0};
	_rgfw.hiddenMouse = rgfw_createMouse((char*)&blk, 1, 1, RGFW_FORMAT_RGBA8);
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

fn void rgfw_deinitPlatform_X11()
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

	if (rgfw_x11CursorHandle != null) { dlclose(rgfw_x11CursorHandle); rgfw_x11CursorHandle = null; }
	if (rgfw_x11XiHandle != null) { dlclose(rgfw_x11XiHandle); rgfw_x11XiHandle = null; }
	if (rgfw_x11ExtHandle != null) { dlclose(rgfw_x11ExtHandle); rgfw_x11ExtHandle = null; }
}

fn void rgfw_initKeycodesPlatform_X11()
{
	// A full evdev-keycode -> RGFW_KEY table walk (xkbGetKeyboardByName) is left
	// out of this mechanical pass; keycodes[] stays zeroed (RGFW_KEY_NULL).
}

fn void rgfw_window_closePlatform_X11(RgfwWindow* win)
{
	if (win.src.ic != null) { xDestroyIC(win.src.ic); win.src.ic = null; }
	xFreeGC(_rgfw.display, win.src.gc);
	xDeleteContext(_rgfw.display, win.src.window, _rgfw.context);
	xDestroyWindow(_rgfw.display, win.src.window);
}

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


// ==================== Wayland-specific declarations & backend ====================
// ---- POSIX / libc bits used by the Wayland backend ----
extern fn void* mmap(void* addr, usz length, int prot, int flags, int fd, long offset);

extern fn int munmap(void* addr, usz length);

extern fn int shmOpen(char* name, int oflag, int mode) @cname("shm_open");

extern fn int shmUnlink(char* name) @cname("shm_unlink");

extern fn int ftruncate(int fd, long length);

extern fn int close(int fd);

extern fn int pipe(int[2]* fds);

extern fn long read(int fd, void* buf, usz count);

extern fn long write(int fd, void* buf, usz count);

extern fn int poll(Pollfd* fds, ulong nfds, int timeout);

extern fn int fcntl(int fd, int cmd, ...) @cname("fcntl");
const int F_GETFD = 1;
const int F_GETFL = 3;

extern fn char* getenv(char* name);

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
typedef WlDisplay = void;

typedef WlProxy = void;

alias WlFixed = int;

struct WlMessage { char* name; char* signature; void* types; }

struct WlInterface { char* name; int version; int methodCount; WlMessage* methods; int eventCount; WlMessage* events; }

struct WlArray { usz size; usz alloc; void* data; }

extern fn WlDisplay* wlDisplayConnect(char* name) @cname("wl_display_connect");

extern fn void wlDisplayDisconnect(WlDisplay* display) @cname("wl_display_disconnect");

extern fn int wlDisplayGetFd(WlDisplay* display) @cname("wl_display_get_fd");

extern fn int wlDisplayDispatch(WlDisplay* display) @cname("wl_display_dispatch");

extern fn int wlDisplayDispatchPending(WlDisplay* display) @cname("wl_display_dispatch_pending");

extern fn int wlDisplayFlush(WlDisplay* display) @cname("wl_display_flush");

extern fn int wlDisplayRoundtrip(WlDisplay* display) @cname("wl_display_roundtrip");

extern fn int wlDisplayPrepareRead(WlDisplay* display) @cname("wl_display_prepare_read");

extern fn int wlDisplayReadEvents(WlDisplay* display) @cname("wl_display_read_events");

extern fn void wlDisplayCancelRead(WlDisplay* display) @cname("wl_display_cancel_read");

extern fn int wlDisplayGetError(WlDisplay* display) @cname("wl_display_get_error");

extern fn int* rgfwErrnoLocation() @cname("__errno_location");
const int RGFW_EAGAIN = 11;

extern fn WlProxy* wlProxyMarshalFlags(WlProxy* proxy, uint opcode, WlInterface* iface, uint version, uint flags, ...) @cname("wl_proxy_marshal_flags");

extern fn int wlProxyAddListener(WlProxy* proxy, void* implementation, void* data) @cname("wl_proxy_add_listener");

extern fn void* wlProxyGetUserData(WlProxy* proxy) @cname("wl_proxy_get_user_data");

extern fn void wlProxySetUserData(WlProxy* proxy, void* data) @cname("wl_proxy_set_user_data");

extern fn uint wlProxyGetVersion(WlProxy* proxy) @cname("wl_proxy_get_version");

extern fn void wlProxyDestroy(WlProxy* proxy) @cname("wl_proxy_destroy");

extern fn void wlProxyMarshal(WlProxy* proxy, uint opcode, ...) @cname("wl_proxy_marshal");

const uint WL_MARSHAL_FLAG_DESTROY = 1;

macro WlFixed wlFixedFromInt(int i) => (WlFixed)(i * 256);

macro int wlFixedToInt(WlFixed f) => f / 256;

macro double wlFixedToDouble(WlFixed f) => (double)f / 256.0;

macro WlFixed wlFixedFromDouble(double d) => (WlFixed)(d * 256.0);

// Core wl_*_interface descriptor tables: exported globals from libwayland-client.
extern WlInterface wlRegistryInterface @cname("wl_registry_interface");

extern WlInterface wlCompositorInterface @cname("wl_compositor_interface");

extern WlInterface wlSurfaceInterface @cname("wl_surface_interface");

extern WlInterface wlSeatInterface @cname("wl_seat_interface");

extern WlInterface wlPointerInterface @cname("wl_pointer_interface");

extern WlInterface wlKeyboardInterface @cname("wl_keyboard_interface");

extern WlInterface wlOutputInterface @cname("wl_output_interface");

extern WlInterface wlShmInterface @cname("wl_shm_interface");

extern WlInterface wlShmPoolInterface @cname("wl_shm_pool_interface");

extern WlInterface wlBufferInterface @cname("wl_buffer_interface");

extern WlInterface wlRegionInterface @cname("wl_region_interface");

extern WlInterface wlDataDeviceManagerInterface @cname("wl_data_device_manager_interface");

extern WlInterface wlDataDeviceInterface @cname("wl_data_device_interface");

extern WlInterface wlDataSourceInterface @cname("wl_data_source_interface");

extern WlInterface wlDataOfferInterface @cname("wl_data_offer_interface");

// xdg-shell / extension protocol interface tables: NOT part of libwayland-client;
// these come from wayland-scanner-generated protocol code (wayland-protocols)
// that a real build must generate and link in, exactly like the original C
// header's `#include "xdg-shell.h"` implies.
extern WlInterface xdgWmBaseInterface @cname("xdg_wm_base_interface");

extern WlInterface xdgSurfaceInterface @cname("xdg_surface_interface");

extern WlInterface xdgToplevelInterface @cname("xdg_toplevel_interface");

extern WlInterface zxdgDecorationManagerV1Interface @cname("zxdg_decoration_manager_v1_interface");

extern WlInterface zxdgToplevelDecorationV1Interface @cname("zxdg_toplevel_decoration_v1_interface");

extern WlInterface zwpRelativePointerManagerV1Interface @cname("zwp_relative_pointer_manager_v1_interface");

extern WlInterface zwpRelativePointerV1Interface @cname("zwp_relative_pointer_v1_interface");

extern WlInterface zwpPointerConstraintsV1Interface @cname("zwp_pointer_constraints_v1_interface");

extern WlInterface zwpLockedPointerV1Interface @cname("zwp_locked_pointer_v1_interface");

extern WlInterface zxdgOutputManagerV1Interface @cname("zxdg_output_manager_v1_interface");

extern WlInterface zxdgOutputV1Interface @cname("zxdg_output_v1_interface");

extern WlInterface xdgToplevelIconManagerV1Interface @cname("xdg_toplevel_icon_manager_v1_interface");

extern WlInterface xdgToplevelIconV1Interface @cname("xdg_toplevel_icon_v1_interface");

extern WlInterface wpPointerWarpV1Interface @cname("wp_pointer_warp_v1_interface");

// ---- listener vtables (field order matches each protocol's `_listener` struct) ----
struct WlRegistryListener { void* global; void* globalRemove; }

struct WlSeatListener { void* capabilities; void* name; }

struct WlShmListener { void* format; }

struct WlPointerListener
{
	void* enter;
	void* leave;
	void* motion;
	void* button;
	void* axis;
	void* frame;
	void* axisSource;
	void* axisStop;
	void* axisDiscrete;
}

struct WlKeyboardListener
{
	void* keymap;
	void* enter;
	void* leave;
	void* key;
	void* modifiers;
	void* repeatInfo;
}

struct WlOutputListener
{
	void* geometry;
	void* mode;
	void* done;
	void* scale;
	void* name;
	void* description;
}

struct WlSurfaceListener { void* enter; void* leave; void* preferredBufferScale; void* preferredBufferTransform; }

struct WlDataSourceListener
{
	void* target;
	void* send;
	void* cancelled;
	void* dndDropPerformed;
	void* dndFinished;
	void* action;
}

struct WlDataOfferListener { void* offer; void* sourceActions; void* action; }

struct WlDataDeviceListener
{
	void* dataOffer;
	void* enter;
	void* leave;
	void* motion;
	void* drop;
	void* selection;
}

struct XdgWmBaseListener { void* ping; }

struct XdgSurfaceListener { void* configure; }

struct XdgToplevelListener
{
	void* configure;
	void* close;
	void* configureBounds;
	void* wmCapabilities;
}

struct ZxdgToplevelDecorationV1Listener { void* configure; }

struct ZwpRelativePointerV1Listener { void* relativeMotion; }

struct ZwpLockedPointerV1Listener { void* locked; void* unlocked; }

struct ZxdgOutputV1Listener
{
	void* logicalPosition;
	void* logicalSize;
	void* done;
	void* name;
	void* description;
}

fn void rgfwDoNothing() {}

// ---- wl_cursor (libwayland-cursor) ----
typedef WlCursorTheme = void;

struct WlCursorImage { uint width; uint height; uint hotspotX; uint hotspotY; uint delay; }

struct WlCursor { uint imageCount; WlCursorImage** images; char* name; }

extern fn WlCursorTheme* wlCursorThemeLoad(char* name, int size, void* shm) @cname("wl_cursor_theme_load");

extern fn void wlCursorThemeDestroy(WlCursorTheme* theme) @cname("wl_cursor_theme_destroy");

extern fn WlCursor* wlCursorThemeGetCursor(WlCursorTheme* theme, char* name) @cname("wl_cursor_theme_get_cursor");

extern fn WlProxy* wlCursorImageGetBuffer(WlCursorImage* image) @cname("wl_cursor_image_get_buffer");

// ---- wl_egl (libwayland-egl) ----
typedef WlEglWindow = void;

extern fn WlEglWindow* wlEglWindowCreate(WlProxy* surface, int width, int height) @cname("wl_egl_window_create");

extern fn void wlEglWindowDestroy(WlEglWindow* window) @cname("wl_egl_window_destroy");

extern fn void wlEglWindowResize(WlEglWindow* window, int width, int height, int dx, int dy) @cname("wl_egl_window_resize");

// ---- xkbcommon ----
typedef XkbContext = void;

typedef XkbKeymap = void;

typedef XkbState = void;

typedef XkbComposeTable = void;

typedef XkbComposeState = void;

alias XkbKeysym = uint;

alias XkbKeycode = uint;

extern fn XkbContext* xkbContextNew(int flags) @cname("xkb_context_new");

extern fn void xkbContextUnref(XkbContext* ctx) @cname("xkb_context_unref");

extern fn XkbKeymap* xkbKeymapNewFromString(XkbContext* ctx, char* string, int format, int flags) @cname("xkb_keymap_new_from_string");

extern fn void xkbKeymapUnref(XkbKeymap* keymap) @cname("xkb_keymap_unref");

extern fn int xkbKeymapModGetIndex(XkbKeymap* keymap, char* name) @cname("xkb_keymap_mod_get_index");

extern fn XkbState* xkbStateNew(XkbKeymap* keymap) @cname("xkb_state_new");

extern fn void xkbStateUnref(XkbState* state) @cname("xkb_state_unref");

extern fn uint xkbStateUpdateMask(XkbState* state, uint depressed, uint latched, uint locked, uint depressedLayout, uint latchedLayout, uint lockedLayout) @cname("xkb_state_update_mask");

extern fn int xkbStateKeyGetSyms(XkbState* state, XkbKeycode key, XkbKeysym** syms) @cname("xkb_state_key_get_syms");

extern fn XkbKeysym xkbStateKeyGetOneSym(XkbState* state, XkbKeycode key) @cname("xkb_state_key_get_one_sym");

extern fn uint xkbKeysymToUtf32(XkbKeysym sym) @cname("xkb_keysym_to_utf32");

extern fn XkbComposeTable* xkbComposeTableNewFromLocale(XkbContext* ctx, char* locale, int flags) @cname("xkb_compose_table_new_from_locale");

extern fn void xkbComposeTableUnref(XkbComposeTable* table) @cname("xkb_compose_table_unref");

extern fn XkbComposeState* xkbComposeStateNew(XkbComposeTable* table, int flags) @cname("xkb_compose_state_new");

extern fn void xkbComposeStateUnref(XkbComposeState* state) @cname("xkb_compose_state_unref");

extern fn int xkbComposeStateFeed(XkbComposeState* state, XkbKeysym sym) @cname("xkb_compose_state_feed");

extern fn int xkbComposeStateGetStatus(XkbComposeState* state) @cname("xkb_compose_state_get_status");

extern fn XkbKeysym xkbComposeStateGetOneSym(XkbComposeState* state) @cname("xkb_compose_state_get_one_sym");

const int XKB_CONTEXT_NO_FLAGS = 0;

const int XKB_KEYMAP_FORMAT_TEXT_V1 = 1;

const int XKB_KEYMAP_COMPILE_NO_FLAGS = 0;

const int XKB_COMPOSE_COMPILE_NO_FLAGS = 0;

const int XKB_COMPOSE_STATE_NO_FLAGS = 0;

const int XKB_COMPOSE_FEED_IGNORED = 0;

const int XKB_COMPOSE_FEED_ACCEPTED = 1;

const int XKB_COMPOSE_NOTHING = 0;

const int XKB_COMPOSE_COMPOSING = 1;

const int XKB_COMPOSE_COMPOSED = 2;

const int XKB_COMPOSE_CANCELLED = 3;

const XkbKeysym XKB_KEY_NO_SYMBOL = 0;

const XkbKeysym XKB_KEY_F1 = 0xffbe;

const XkbKeysym XKB_KEY_F2 = 0xffbf;

const XkbKeysym XKB_KEY_F3 = 0xffc0;

const XkbKeysym XKB_KEY_F4 = 0xffc1;

const XkbKeysym XKB_KEY_F5 = 0xffc2;

const XkbKeysym XKB_KEY_F6 = 0xffc3;

const XkbKeysym XKB_KEY_F7 = 0xffc4;

const XkbKeysym XKB_KEY_F8 = 0xffc5;

const XkbKeysym XKB_KEY_F9 = 0xffc6;

const XkbKeysym XKB_KEY_F10 = 0xffc7;

const XkbKeysym XKB_KEY_F11 = 0xffc8;

const XkbKeysym XKB_KEY_F12 = 0xffc9;

const XkbKeysym XKB_KEY_F13 = 0xffca;

const XkbKeysym XKB_KEY_F14 = 0xffcb;

const XkbKeysym XKB_KEY_F15 = 0xffcc;

const XkbKeysym XKB_KEY_F16 = 0xffcd;

const XkbKeysym XKB_KEY_F17 = 0xffce;

const XkbKeysym XKB_KEY_F18 = 0xffcf;

const XkbKeysym XKB_KEY_F19 = 0xffd0;

const XkbKeysym XKB_KEY_F20 = 0xffd1;

const XkbKeysym XKB_KEY_F21 = 0xffd2;

const XkbKeysym XKB_KEY_F22 = 0xffd3;

const XkbKeysym XKB_KEY_F23 = 0xffd4;

const XkbKeysym XKB_KEY_F24 = 0xffd5;

const XkbKeysym XKB_KEY_F25 = 0xffd6;

const XkbKeysym XKB_KEY_SHIFT_L = 0xffe1;

const XkbKeysym XKB_KEY_SHIFT_R = 0xffe2;

const XkbKeysym XKB_KEY_CONTROL_L = 0xffe3;

const XkbKeysym XKB_KEY_CONTROL_R = 0xffe4;

const XkbKeysym XKB_KEY_CAPS_LOCK = 0xffe5;

const XkbKeysym XKB_KEY_ALT_L = 0xffe9;

const XkbKeysym XKB_KEY_ALT_R = 0xffea;

const XkbKeysym XKB_KEY_SUPER_L = 0xffeb;

const XkbKeysym XKB_KEY_SUPER_R = 0xffec;

const XkbKeysym XKB_KEY_NUM_LOCK = 0xff7f;

const XkbKeysym XKB_KEY_SCROLL_LOCK = 0xff14;

const XkbKeysym XKB_KEY_UP = 0xff52;

const XkbKeysym XKB_KEY_DOWN = 0xff54;

const XkbKeysym XKB_KEY_LEFT = 0xff51;

const XkbKeysym XKB_KEY_RIGHT = 0xff53;

const XkbKeysym XKB_KEY_HOME = 0xff50;

const XkbKeysym XKB_KEY_END = 0xff57;

const XkbKeysym XKB_KEY_PAGE_UP = 0xff55;

const XkbKeysym XKB_KEY_PAGE_DOWN = 0xff56;

const XkbKeysym XKB_KEY_INSERT = 0xff63;

const XkbKeysym XKB_KEY_MENU = 0xff67;

const XkbKeysym XKB_KEY_KP_ADD = 0xffab;

const XkbKeysym XKB_KEY_KP_SUBTRACT = 0xffad;

const XkbKeysym XKB_KEY_KP_MULTIPLY = 0xffaa;

const XkbKeysym XKB_KEY_KP_DIVIDE = 0xffaf;

const XkbKeysym XKB_KEY_KP_EQUAL = 0xffbd;

const XkbKeysym XKB_KEY_KP_ENTER = 0xff8d;

const XkbKeysym XKB_KEY_KP_DECIMAL = 0xffae;

const XkbKeysym XKB_KEY_KP_0 = 0xffb0;

const XkbKeysym XKB_KEY_KP_1 = 0xffb1;

const XkbKeysym XKB_KEY_KP_2 = 0xffb2;

const XkbKeysym XKB_KEY_KP_3 = 0xffb3;

const XkbKeysym XKB_KEY_KP_4 = 0xffb4;

const XkbKeysym XKB_KEY_KP_5 = 0xffb5;

const XkbKeysym XKB_KEY_KP_6 = 0xffb6;

const XkbKeysym XKB_KEY_KP_7 = 0xffb7;

const XkbKeysym XKB_KEY_KP_8 = 0xffb8;

const XkbKeysym XKB_KEY_KP_9 = 0xffb9;

const XkbKeysym XKB_KEY_PRINT = 0xff61;

const XkbKeysym XKB_KEY_PAUSE = 0xff13;

fn bool rgfw_loadGL_Wayland()
{
	// Wayland only ever renders through EGL; "OpenGL" here just means EGL is ready.
	return rgfw_loadEGL();
}

fn void rgfw_unloadGL_Wayland()
{
	_rgfw.nativeGL_handle = null;
	_rgfw.glGetProcAddress = null;
}

const int VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR = 1000006000;

struct VkWaylandSurfaceCreateInfoKHR { int sType; void* pNext; uint flags; WlDisplay* display; WlProxy* surface; }

alias RgfwVkCreateWaylandSurfaceKHRProc = fn VkResult(VkInstance instance, VkWaylandSurfaceCreateInfoKHR* createInfo, void* allocator, VkSurfaceKHR* surface);

// ==== thin wl_proxy_marshal_flags wrappers (normally wayland-scanner "static inline") ====
fn WlProxy* wlDisplayGetRegistry(WlDisplay* display) => wlProxyMarshalFlags((WlProxy*)display, 1, &wlRegistryInterface, wlProxyGetVersion((WlProxy*)display), 0, null);

fn void* wlRegistryBind(WlProxy* registry, uint name, WlInterface* iface, uint version) => (void*)wlProxyMarshalFlags(registry, 0, iface, version, 0, name, iface.name, version, null);

fn int wlRegistryAddListener(WlProxy* registry, WlRegistryListener* listener, void* data) => wlProxyAddListener(registry, listener, data);

fn void wlRegistryDestroy(WlProxy* registry) { wlProxyDestroy(registry); }

fn WlProxy* wlCompositorCreateSurface(WlProxy* compositor) => wlProxyMarshalFlags(compositor, 0, &wlSurfaceInterface, wlProxyGetVersion(compositor), 0, null);

fn WlProxy* wlCompositorCreateRegion(WlProxy* compositor) => wlProxyMarshalFlags(compositor, 1, &wlRegionInterface, wlProxyGetVersion(compositor), 0, null);

fn void wlCompositorDestroy(WlProxy* compositor) { wlProxyDestroy(compositor); }

fn void wlRegionAdd(WlProxy* region, int x, int y, int w, int h) { wlProxyMarshalFlags(region, 1, null, wlProxyGetVersion(region), 0, x, y, w, h); }

fn void wlRegionDestroy(WlProxy* region) { wlProxyMarshalFlags(region, 0, null, wlProxyGetVersion(region), WL_MARSHAL_FLAG_DESTROY); }

fn int wlSurfaceAddListener(WlProxy* surface, WlSurfaceListener* listener, void* data) => wlProxyAddListener(surface, listener, data);

fn void wlSurfaceAttach(WlProxy* surface, WlProxy* buffer, int x, int y) { wlProxyMarshalFlags(surface, 1, null, wlProxyGetVersion(surface), 0, buffer, x, y); }

fn void wlSurfaceDamage(WlProxy* surface, int x, int y, int w, int h) { wlProxyMarshalFlags(surface, 2, null, wlProxyGetVersion(surface), 0, x, y, w, h); }

fn void wlSurfaceCommit(WlProxy* surface) { wlProxyMarshalFlags(surface, 6, null, wlProxyGetVersion(surface), 0); }

fn void wlSurfaceSetOpaqueRegion(WlProxy* surface, WlProxy* region) { wlProxyMarshalFlags(surface, 4, null, wlProxyGetVersion(surface), 0, region); }

fn void wlSurfaceDestroy(WlProxy* surface) { wlProxyMarshalFlags(surface, 0, null, wlProxyGetVersion(surface), WL_MARSHAL_FLAG_DESTROY); }

fn void* wlSurfaceGetUserData(WlProxy* surface) => wlProxyGetUserData(surface);

fn void wlSurfaceSetUserData(WlProxy* surface, void* data) { wlProxySetUserData(surface, data); }

fn WlProxy* wlSeatGetPointer(WlProxy* seat) => wlProxyMarshalFlags(seat, 0, &wlPointerInterface, wlProxyGetVersion(seat), 0, null);

fn WlProxy* wlSeatGetKeyboard(WlProxy* seat) => wlProxyMarshalFlags(seat, 1, &wlKeyboardInterface, wlProxyGetVersion(seat), 0, null);

fn int wlSeatAddListener(WlProxy* seat, WlSeatListener* listener, void* data) => wlProxyAddListener(seat, listener, data);

fn void wlSeatRelease(WlProxy* seat) { wlProxyDestroy(seat); }

fn int wlPointerAddListener(WlProxy* pointer, WlPointerListener* listener, void* data) => wlProxyAddListener(pointer, listener, data);

fn void wlPointerSetCursor(WlProxy* pointer, uint serial, WlProxy* surface, int hotspotX, int hotspotY) { wlProxyMarshalFlags(pointer, 0, null, wlProxyGetVersion(pointer), 0, serial, surface, hotspotX, hotspotY); }

fn void wlPointerDestroy(WlProxy* pointer) { wlProxyMarshalFlags(pointer, 1, null, wlProxyGetVersion(pointer), WL_MARSHAL_FLAG_DESTROY); }

fn int wlKeyboardAddListener(WlProxy* keyboard, WlKeyboardListener* listener, void* data) => wlProxyAddListener(keyboard, listener, data);

fn void wlKeyboardDestroy(WlProxy* keyboard) { wlProxyMarshalFlags(keyboard, 0, null, wlProxyGetVersion(keyboard), WL_MARSHAL_FLAG_DESTROY); }

fn int wlOutputAddListener(WlProxy* output, WlOutputListener* listener, void* data) => wlProxyAddListener(output, listener, data);

fn void* wlOutputGetUserData(WlProxy* output) => wlProxyGetUserData(output);

fn void wlOutputSetUserData(WlProxy* output, void* data) { wlProxySetUserData(output, data); }

fn void wlOutputDestroy(WlProxy* output) { wlProxyMarshalFlags(output, 0, null, wlProxyGetVersion(output), WL_MARSHAL_FLAG_DESTROY); }

fn int wlShmAddListener(WlProxy* shm, WlShmListener* listener, void* data) => wlProxyAddListener(shm, listener, data);

fn WlProxy* wlShmCreatePool(WlProxy* shm, int fd, int size) => wlProxyMarshalFlags(shm, 0, &wlShmPoolInterface, wlProxyGetVersion(shm), 0, null, fd, size);

fn void wlShmDestroy(WlProxy* shm) { wlProxyDestroy(shm); }

fn WlProxy* wlShmPoolCreateBuffer(WlProxy* pool, int offset, int w, int h, int stride, uint format) => wlProxyMarshalFlags(pool, 0, &wlBufferInterface, wlProxyGetVersion(pool), 0, null, offset, w, h, stride, format);

fn void wlShmPoolDestroy(WlProxy* pool) { wlProxyMarshalFlags(pool, 1, null, wlProxyGetVersion(pool), WL_MARSHAL_FLAG_DESTROY); }

fn void wlBufferDestroy(WlProxy* buffer) { wlProxyMarshalFlags(buffer, 0, null, wlProxyGetVersion(buffer), WL_MARSHAL_FLAG_DESTROY); }

fn WlProxy* wlDataDeviceManagerCreateDataSource(WlProxy* mgr) => wlProxyMarshalFlags(mgr, 0, &wlDataSourceInterface, wlProxyGetVersion(mgr), 0, null);

fn WlProxy* wlDataDeviceManagerGetDataDevice(WlProxy* mgr, WlProxy* seat) => wlProxyMarshalFlags(mgr, 1, &wlDataDeviceInterface, wlProxyGetVersion(mgr), 0, null, seat);

fn void wlDataDeviceManagerDestroy(WlProxy* mgr) { wlProxyDestroy(mgr); }

fn void wlDataSourceOffer(WlProxy* source, char* mimeType) { wlProxyMarshalFlags(source, 0, null, wlProxyGetVersion(source), 0, mimeType); }

fn int wlDataSourceAddListener(WlProxy* source, WlDataSourceListener* listener, void* data) => wlProxyAddListener(source, listener, data);

fn void wlDataSourceDestroy(WlProxy* source) { wlProxyMarshalFlags(source, 1, null, wlProxyGetVersion(source), WL_MARSHAL_FLAG_DESTROY); }

fn int wlDataOfferAddListener(WlProxy* offer, WlDataOfferListener* listener, void* data) => wlProxyAddListener(offer, listener, data);

fn void wlDataOfferReceive(WlProxy* offer, char* mimeType, int fd) { wlProxyMarshalFlags(offer, 1, null, wlProxyGetVersion(offer), 0, mimeType, fd); }

fn void wlDataOfferDestroy(WlProxy* offer) { wlProxyMarshalFlags(offer, 2, null, wlProxyGetVersion(offer), WL_MARSHAL_FLAG_DESTROY); }

fn int wlDataDeviceAddListener(WlProxy* dev, WlDataDeviceListener* listener, void* data) => wlProxyAddListener(dev, listener, data);

fn void wlDataDeviceSetSelection(WlProxy* dev, WlProxy* source, uint serial) { wlProxyMarshalFlags(dev, 1, null, wlProxyGetVersion(dev), 0, source, serial); }

fn void wlDataDeviceDestroy(WlProxy* dev) { wlProxyDestroy(dev); }

// ---- xdg-shell (stable) ----
fn void xdgWmBaseAddListener(WlProxy* base, XdgWmBaseListener* listener, void* data) { wlProxyAddListener(base, listener, data); }

fn void xdgWmBasePong(WlProxy* base, uint serial) { wlProxyMarshalFlags(base, 3, null, wlProxyGetVersion(base), 0, serial); }

fn WlProxy* xdgWmBaseGetXdgSurface(WlProxy* base, WlProxy* surface) => wlProxyMarshalFlags(base, 2, &xdgSurfaceInterface, wlProxyGetVersion(base), 0, null, surface);

fn void xdgWmBaseSetUserData(WlProxy* base, void* data) { wlProxySetUserData(base, data); }

fn void xdgWmBaseDestroy(WlProxy* base) { wlProxyMarshalFlags(base, 0, null, wlProxyGetVersion(base), WL_MARSHAL_FLAG_DESTROY); }

fn void xdgSurfaceAddListener(WlProxy* surface, XdgSurfaceListener* listener, void* data) { wlProxyAddListener(surface, listener, data); }

fn void xdgSurfaceAckConfigure(WlProxy* surface, uint serial) { wlProxyMarshalFlags(surface, 4, null, wlProxyGetVersion(surface), 0, serial); }

fn WlProxy* xdgSurfaceGetToplevel(WlProxy* surface) => wlProxyMarshalFlags(surface, 1, &xdgToplevelInterface, wlProxyGetVersion(surface), 0, null);

fn void xdgSurfaceSetWindowGeometry(WlProxy* surface, int x, int y, int w, int h) { wlProxyMarshalFlags(surface, 3, null, wlProxyGetVersion(surface), 0, x, y, w, h); }

fn void xdgSurfaceDestroy(WlProxy* surface) { wlProxyMarshalFlags(surface, 0, null, wlProxyGetVersion(surface), WL_MARSHAL_FLAG_DESTROY); }

fn void xdgToplevelAddListener(WlProxy* toplevel, XdgToplevelListener* listener, void* data) { wlProxyAddListener(toplevel, listener, data); }

fn void xdgToplevelSetTitle(WlProxy* toplevel, char* title) { wlProxyMarshalFlags(toplevel, 2, null, wlProxyGetVersion(toplevel), 0, title); }

fn void xdgToplevelSetAppId(WlProxy* toplevel, char* appId) { wlProxyMarshalFlags(toplevel, 3, null, wlProxyGetVersion(toplevel), 0, appId); }

fn void xdgToplevelSetMaxSize(WlProxy* toplevel, int w, int h) { wlProxyMarshalFlags(toplevel, 7, null, wlProxyGetVersion(toplevel), 0, w, h); }

fn void xdgToplevelSetMinSize(WlProxy* toplevel, int w, int h) { wlProxyMarshalFlags(toplevel, 8, null, wlProxyGetVersion(toplevel), 0, w, h); }

fn void xdgToplevelSetMaximized(WlProxy* toplevel) { wlProxyMarshalFlags(toplevel, 9, null, wlProxyGetVersion(toplevel), 0); }

fn void xdgToplevelUnsetMaximized(WlProxy* toplevel) { wlProxyMarshalFlags(toplevel, 10, null, wlProxyGetVersion(toplevel), 0); }

fn void xdgToplevelSetFullscreen(WlProxy* toplevel, WlProxy* output) { wlProxyMarshalFlags(toplevel, 11, null, wlProxyGetVersion(toplevel), 0, output); }

fn void xdgToplevelUnsetFullscreen(WlProxy* toplevel) { wlProxyMarshalFlags(toplevel, 12, null, wlProxyGetVersion(toplevel), 0); }

fn void xdgToplevelSetMinimized(WlProxy* toplevel) { wlProxyMarshalFlags(toplevel, 13, null, wlProxyGetVersion(toplevel), 0); }

fn void xdgToplevelDestroy(WlProxy* toplevel) { wlProxyMarshalFlags(toplevel, 0, null, wlProxyGetVersion(toplevel), WL_MARSHAL_FLAG_DESTROY); }

// ---- xdg-decoration-unstable-v1 ----
fn WlProxy* zxdgDecorationManagerV1GetToplevelDecoration(WlProxy* mgr, WlProxy* toplevel) => wlProxyMarshalFlags(mgr, 1, &zxdgToplevelDecorationV1Interface, wlProxyGetVersion(mgr), 0, null, toplevel);

fn void zxdgDecorationManagerV1Destroy(WlProxy* mgr) { wlProxyMarshalFlags(mgr, 0, null, wlProxyGetVersion(mgr), WL_MARSHAL_FLAG_DESTROY); }

fn void zxdgToplevelDecorationV1AddListener(WlProxy* dec, ZxdgToplevelDecorationV1Listener* listener, void* data) { wlProxyAddListener(dec, listener, data); }

fn void zxdgToplevelDecorationV1SetMode(WlProxy* dec, uint mode) { wlProxyMarshalFlags(dec, 1, null, wlProxyGetVersion(dec), 0, mode); }

fn void zxdgToplevelDecorationV1Destroy(WlProxy* dec) { wlProxyMarshalFlags(dec, 0, null, wlProxyGetVersion(dec), WL_MARSHAL_FLAG_DESTROY); }

const uint ZXDG_TOPLEVEL_DECORATION_V1_MODE_CLIENT_SIDE = 1;

const uint ZXDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE = 2;

// ---- relative-pointer-unstable-v1 ----
fn WlProxy* zwpRelativePointerManagerV1GetRelativePointer(WlProxy* mgr, WlProxy* pointer) => wlProxyMarshalFlags(mgr, 1, &zwpRelativePointerV1Interface, wlProxyGetVersion(mgr), 0, null, pointer);

fn void zwpRelativePointerManagerV1Destroy(WlProxy* mgr) { wlProxyMarshalFlags(mgr, 0, null, wlProxyGetVersion(mgr), WL_MARSHAL_FLAG_DESTROY); }

fn void zwpRelativePointerV1AddListener(WlProxy* rel, ZwpRelativePointerV1Listener* listener, void* data) { wlProxyAddListener(rel, listener, data); }

fn void zwpRelativePointerV1Destroy(WlProxy* rel) { wlProxyMarshalFlags(rel, 0, null, wlProxyGetVersion(rel), WL_MARSHAL_FLAG_DESTROY); }

// ---- pointer-constraints-unstable-v1 ----
fn WlProxy* zwpPointerConstraintsV1LockPointer(WlProxy* mgr, WlProxy* surface, WlProxy* pointer, WlProxy* region, uint lifetime) => wlProxyMarshalFlags(mgr, 1, &zwpLockedPointerV1Interface, wlProxyGetVersion(mgr), 0, null, surface, pointer, region, lifetime);

fn void zwpPointerConstraintsV1Destroy(WlProxy* mgr) { wlProxyMarshalFlags(mgr, 0, null, wlProxyGetVersion(mgr), WL_MARSHAL_FLAG_DESTROY); }

fn void zwpLockedPointerV1AddListener(WlProxy* lp, ZwpLockedPointerV1Listener* listener, void* data) { wlProxyAddListener(lp, listener, data); }

fn void zwpLockedPointerV1Destroy(WlProxy* lp) { wlProxyMarshalFlags(lp, 0, null, wlProxyGetVersion(lp), WL_MARSHAL_FLAG_DESTROY); }

const uint ZWP_POINTER_CONSTRAINTS_V1_LIFETIME_PERSISTENT = 2;

// ---- xdg-output-unstable-v1 ----
fn WlProxy* zxdgOutputManagerV1GetXdgOutput(WlProxy* mgr, WlProxy* output) => wlProxyMarshalFlags(mgr, 1, &zxdgOutputV1Interface, wlProxyGetVersion(mgr), 0, null, output);

fn void zxdgOutputManagerV1Destroy(WlProxy* mgr) { wlProxyMarshalFlags(mgr, 0, null, wlProxyGetVersion(mgr), WL_MARSHAL_FLAG_DESTROY); }

fn void zxdgOutputV1AddListener(WlProxy* out, ZxdgOutputV1Listener* listener, void* data) { wlProxyAddListener(out, listener, data); }

fn void zxdgOutputV1Destroy(WlProxy* out) { wlProxyMarshalFlags(out, 0, null, wlProxyGetVersion(out), WL_MARSHAL_FLAG_DESTROY); }

// ---- xdg-toplevel-icon-v1 ----
fn void xdgToplevelIconManagerV1SetIcon(WlProxy* mgr, WlProxy* toplevel, WlProxy* icon) { wlProxyMarshalFlags(mgr, 2, null, wlProxyGetVersion(mgr), 0, toplevel, icon); }

fn WlProxy* xdgToplevelIconManagerV1CreateIcon(WlProxy* mgr) => wlProxyMarshalFlags(mgr, 1, &xdgToplevelIconV1Interface, wlProxyGetVersion(mgr), 0, null);

fn void xdgToplevelIconManagerV1Destroy(WlProxy* mgr) { wlProxyMarshalFlags(mgr, 0, null, wlProxyGetVersion(mgr), WL_MARSHAL_FLAG_DESTROY); }

fn void xdgToplevelIconV1AddBuffer(WlProxy* icon, WlProxy* buffer, int scale) { wlProxyMarshalFlags(icon, 2, null, wlProxyGetVersion(icon), 0, buffer, scale); }

fn void xdgToplevelIconV1Destroy(WlProxy* icon) { wlProxyMarshalFlags(icon, 0, null, wlProxyGetVersion(icon), WL_MARSHAL_FLAG_DESTROY); }

// ---- pointer-warp-v1 ----
fn void wpPointerWarpV1WarpPointer(WlProxy* warp, WlProxy* surface, WlProxy* pointer, WlFixed x, WlFixed y, uint serial) { wlProxyMarshalFlags(warp, 1, null, wlProxyGetVersion(warp), 0, surface, pointer, x, y, serial); }

fn void wpPointerWarpV1Destroy(WlProxy* warp) { wlProxyMarshalFlags(warp, 0, null, wlProxyGetVersion(warp), WL_MARSHAL_FLAG_DESTROY); }

const uint WL_SEAT_CAPABILITY_POINTER = 1;

const uint WL_SEAT_CAPABILITY_KEYBOARD = 2;

const uint WL_POINTER_AXIS_VERTICAL_SCROLL = 0;

const uint WL_POINTER_AXIS_HORIZONTAL_SCROLL = 1;

const uint WL_OUTPUT_MODE_CURRENT = 1;

const uint WL_SHM_FORMAT_ARGB8888 = 0;

fn void rgfw_wlSetOpaque(RgfwWindow* win)
{
	WlProxy* region = wlCompositorCreateRegion(_rgfw.compositor);
	if (region == null) return;
	wlRegionAdd(region, 0, 0, win.w, win.h);
	wlSurfaceSetOpaqueRegion(win.src.surface, region);
	wlRegionDestroy(region);
}

fn void rgfw_toggleWaylandMaximized(RgfwWindow* win, bool maximized)
{
	win.src.maximized = maximized;
	if (maximized) { xdgToplevelSetMaximized(win.src.xdgToplevel); }
	else { xdgToplevelUnsetMaximized(win.src.xdgToplevel); }
}

fn void rgfw_wlXdgWmBasePingHandler(void* data, WlProxy* wmBase, uint serial) { xdgWmBasePong(wmBase, serial); }

fn void rgfw_wlXdgSurfaceConfigureHandler(void* data, WlProxy* xdgSurface, uint serial)
{
	xdgSurfaceAckConfigure(xdgSurface, serial);
	RgfwWindow* win = (RgfwWindow*)data;
	if (win == null)
	{
		win = _rgfw.kbOwner;
		if (win == null) return;
	}

	if (win.src.activated != win.src.pendingActivated) win.src.activated = win.src.pendingActivated;

	if (win.src.maximized != win.src.pendingMaximized)
	{
		rgfw_toggleWaylandMaximized(win, win.src.pendingMaximized);
		rgfw_window_checkMode(win);
	}

	if (win.src.resizing)
	{
		rgfw_windowResizedCallback(win, win.w, win.h);
		rgfw_window_resize(win, win.w, win.h);
		if (!(win.internal.flags & RGFW_WINDOW_TRANSPARENT)) rgfw_wlSetOpaque(win);
	}

	win.src.configured = RGFW_TRUE;
}

fn void rgfw_wlXdgToplevelConfigureHandler(void* data, WlProxy* toplevel, int width, int height, WlArray* states)
{
	RgfwWindow* win = (RgfwWindow*)data;
	win.src.pendingActivated = RGFW_FALSE;
	win.src.pendingMaximized = RGFW_FALSE;
	win.src.resizing = RGFW_FALSE;

	if (states != null)
	{
		uint* state = (uint*)states.data;
		usz count = states.size / uint::size;
		for (usz i = 0; i < count; i++)
		{
			switch (state[i])
			{
				case 4: // XDG_TOPLEVEL_STATE_ACTIVATED
					win.src.pendingActivated = RGFW_TRUE;
				case 1: // XDG_TOPLEVEL_STATE_MAXIMIZED
					win.src.pendingMaximized = RGFW_TRUE;
				default:
					break;
			}
		}
	}

	if (width != 0 && height != 0 && (win.w != width || win.h != height))
	{
		win.src.resizing = RGFW_TRUE;
		win.src.w = width; win.w = width;
		win.src.h = height; win.h = height;
	}
}

fn void rgfw_wlXdgToplevelCloseHandler(void* data, WlProxy* toplevel)
{
	RgfwWindow* win = (RgfwWindow*)data;
	if (!win.internal.shouldClose) rgfw_windowCloseCallback(win);
}

fn void rgfw_wlXdgDecorationConfigureHandler(void* data, WlProxy* decoration, uint mode)
{
	RgfwWindow* win = (RgfwWindow*)data;
	if (mode != win.src.decorationMode) win.src.decorationMode = mode;
}

fn void rgfw_wlShmFormatHandler(void* data, WlProxy* shm, uint format) {}

fn void rgfw_wlRelativePointerMotion(void* data, WlProxy* relPointer, uint timeHi, uint timeLo, WlFixed dx, WlFixed dy, WlFixed dxUnaccel, WlFixed dyUnaccel)
{
	RgfwInfo* info = (RgfwInfo*)data;
	RgfwWindow* win = info.mouseOwner;
	if (win == null) return;
	float vecX = (float)wlFixedToDouble(dx);
	float vecY = (float)wlFixedToDouble(dy);
	rgfw_rawMotionCallback(win, vecX, vecY);
}

fn void rgfw_wlPointerLocked(void* data, WlProxy* lockedPointer)
{
	RgfwInfo* info = (RgfwInfo*)data;
	wlPointerSetCursor(info.wlPointer, info.mouseEnterSerial, null, 0, 0);
}

fn void rgfw_wlPointerEnter(void* data, WlProxy* pointer, uint serial, WlProxy* surface, WlFixed surfaceX, WlFixed surfaceY)
{
	RgfwInfo* info = (RgfwInfo*)data;
	RgfwWindow* win = (RgfwWindow*)wlSurfaceGetUserData(surface);
	info.mouseEnterSerial = serial;
	win.internal.mouseInside = RGFW_TRUE;
	info.windowState.mouseEnter = RGFW_TRUE;
	info.mouseOwner = win;

	if (win.src.usingCustomCursor) { wlPointerSetCursor(pointer, serial, win.src.customCursorSurface, 0, 0); }
	else { rgfw_window_setMouseDefault(win); }

	int x = (int)wlFixedToDouble(surfaceX);
	int y = (int)wlFixedToDouble(surfaceY);
	rgfw_mouseNotifyCallback(win, x, y, RGFW_TRUE);
}

fn void rgfw_wlPointerLeave(void* data, WlProxy* pointer, uint serial, WlProxy* surface)
{
	RgfwWindow* win = (RgfwWindow*)wlSurfaceGetUserData(surface);
	RgfwInfo* info = (RgfwInfo*)data;
	if (info.mouseOwner == win) info.mouseOwner = null;
	rgfw_mouseNotifyCallback(win, win.internal.lastMouseX, win.internal.lastMouseY, RGFW_FALSE);
}

fn void rgfw_wlPointerMotion(void* data, WlProxy* pointer, uint time, WlFixed x, WlFixed y)
{
	RgfwInfo* info = (RgfwInfo*)data;
	RgfwWindow* win = info.mouseOwner;
	if (win == null) return;
	int cx = (int)wlFixedToDouble(x);
	int cy = (int)wlFixedToDouble(y);
	rgfw_mouseMotionCallback(win, cx, cy);
}

fn void rgfw_wlPointerButton(void* data, WlProxy* pointer, uint serial, uint time, uint button, uint state)
{
	RgfwInfo* info = (RgfwInfo*)data;
	RgfwWindow* win = info.mouseOwner;
	if (win == null) return;
	uint b = button - 0x110;
	if (b == 1) { b = 2; }
	else if (b == 2) { b = 1; }
	rgfw_mouseButtonCallback(win, (RgfwMouseButton)b, rgfw_bool(state));
}

fn void rgfw_wlPointerAxis(void* data, WlProxy* pointer, uint time, uint axis, WlFixed value)
{
	RgfwInfo* info = (RgfwInfo*)data;
	RgfwWindow* win = info.mouseOwner;
	if (win == null) return;
	float scrollX = 0.0f;
	float scrollY = 0.0f;
	if (!(win.internal.enabledEvents & RGFW_MOUSE_SCROLL_FLAG)) return;
	if (axis == WL_POINTER_AXIS_HORIZONTAL_SCROLL) { scrollX = (float)(-wlFixedToDouble(value) / 10.0); }
	else if (axis == WL_POINTER_AXIS_VERTICAL_SCROLL) { scrollY = (float)(-wlFixedToDouble(value) / 10.0); }
	rgfw_mouseScrollCallback(win, scrollX, scrollY);
}

fn void rgfw_wlKeyboardKeymap(void* data, WlProxy* keyboard, uint format, int fd, uint size)
{
	RgfwInfo* info = (RgfwInfo*)data;
	char* keymapString = mmap(null, size, PROT_READ, MAP_SHARED, fd, 0);
	xkbKeymapUnref(info.keymap);
	info.keymap = xkbKeymapNewFromString(info.xkbContext, keymapString, XKB_KEYMAP_FORMAT_TEXT_V1, XKB_KEYMAP_COMPILE_NO_FLAGS);
	munmap(keymapString, size);
	close(fd);
	xkbStateUnref(info.xkbState);
	info.xkbState = xkbStateNew(info.keymap);

	char* locale = getenv("LC_ALL");
	if (locale == null) locale = getenv("LC_CTYPE");
	if (locale == null) locale = getenv("LANG");
	if (locale == null) locale = "C";

	XkbComposeTable* composeTable = xkbComposeTableNewFromLocale(info.xkbContext, locale, XKB_COMPOSE_COMPILE_NO_FLAGS);
	if (composeTable != null)
	{
		info.composeState = xkbComposeStateNew(composeTable, XKB_COMPOSE_STATE_NO_FLAGS);
		xkbComposeTableUnref(composeTable);
	}
}

fn void rgfw_wlKeyboardEnter(void* data, WlProxy* keyboard, uint serial, WlProxy* surface, WlArray* keys)
{
	RgfwInfo* info = (RgfwInfo*)data;
	RgfwWindow* win = (RgfwWindow*)wlSurfaceGetUserData(surface);
	info.kbOwner = win;

	if (info.dataDevice != null && win.src.dataSource != null) wlDataDeviceSetSelection(info.dataDevice, win.src.dataSource, serial);
	if (win.src.minimized == RGFW_TRUE) win.src.minimized = RGFW_FALSE;
	rgfw_windowFocusCallback(win, RGFW_TRUE);
}

fn void rgfw_wlKeyboardLeave(void* data, WlProxy* keyboard, uint serial, WlProxy* surface)
{
	RgfwInfo* info = (RgfwInfo*)data;
	RgfwWindow* win = (RgfwWindow*)wlSurfaceGetUserData(surface);
	if (info.kbOwner == win) info.kbOwner = null;
	rgfw_windowFocusCallback(win, RGFW_FALSE);
}

fn XkbKeysym rgfw_wlComposeSymbol(RgfwInfo* info, XkbKeysym sym)
{
	if (sym == XKB_KEY_NO_SYMBOL || info.composeState == null) return sym;
	if (xkbComposeStateFeed(info.composeState, sym) != XKB_COMPOSE_FEED_ACCEPTED) return sym;
	int status = xkbComposeStateGetStatus(info.composeState);
	if (status == XKB_COMPOSE_COMPOSED) return xkbComposeStateGetOneSym(info.composeState);
	if (status == XKB_COMPOSE_COMPOSING || status == XKB_COMPOSE_CANCELLED) return XKB_KEY_NO_SYMBOL;
	return sym;
}

fn void rgfw_wlSendKeyEvent(uint key)
{
	XkbKeysym* keysyms;
	if (xkbStateKeyGetSyms(_rgfw.xkbState, key + 8, &keysyms) == 1)
	{
		XkbKeysym keysym = rgfw_wlComposeSymbol(_rgfw, keysyms[0]);
		uint codepoint = xkbKeysymToUtf32(keysym);
		if (codepoint != 0) rgfw_keyCharCallback(_rgfw.kbOwner, codepoint);
	}
}

fn void rgfw_wlKeyboardKey(void* data, WlProxy* keyboard, uint serial, uint time, uint key, uint state)
{
	RgfwInfo* info = (RgfwInfo*)data;
	if (info.kbOwner == null) return;

	RgfwWindow* win = info.kbOwner;
	RgfwKey rgfwKey = rgfw_apiKeyToRGFW(key + 8);

	rgfw_keyUpdateKeyMods(win, rgfw_bool(xkbKeymapModGetIndex(info.keymap, "Lock") != 0), rgfw_bool(xkbKeymapModGetIndex(info.keymap, "Mod2") != 0), rgfw_bool(xkbKeymapModGetIndex(info.keymap, "ScrollLock") != 0));
	rgfw_keyCallback(win, rgfwKey, win.internal.mod, rgfw_bool(rgfw_isKeyDown(rgfwKey) && state != 0), rgfw_bool(state));

	info.lastKey = state != 0 ? key : 0;
	info.lastKeyTime = time + (uint)_rgfw.wlRepeatInfoDelay;
	if (state != 0) rgfw_wlSendKeyEvent(_rgfw.lastKey);
}

fn void rgfw_wlKeyboardModifiers(void* data, WlProxy* keyboard, uint serial, uint modsDepressed, uint modsLatched, uint modsLocked, uint group)
{
	RgfwInfo* info = (RgfwInfo*)data;
	xkbStateUpdateMask(info.xkbState, modsDepressed, modsLatched, modsLocked, 0, 0, group);
}

fn void rgfw_wlKeyboardRepeatInfo(void* data, WlProxy* keyboard, int rate, int delay)
{
	_rgfw.wlRepeatInfoRate = rate;
	_rgfw.wlRepeatInfoDelay = delay;
}

WlPointerListener rgfw_wlPointerListener = {
	.enter = &rgfw_wlPointerEnter, .leave = &rgfw_wlPointerLeave, .motion = &rgfw_wlPointerMotion,
	.button = &rgfw_wlPointerButton, .axis = &rgfw_wlPointerAxis
};

WlKeyboardListener rgfw_wlKeyboardListener = {
	.keymap = &rgfw_wlKeyboardKeymap, .enter = &rgfw_wlKeyboardEnter, .leave = &rgfw_wlKeyboardLeave,
	.key = &rgfw_wlKeyboardKey, .modifiers = &rgfw_wlKeyboardModifiers, .repeatInfo = &rgfw_wlKeyboardRepeatInfo
};

fn void rgfw_wlSeatCapabilities(void* data, WlProxy* seat, uint capabilities)
{
	RgfwInfo* info = (RgfwInfo*)data;

	if ((capabilities & WL_SEAT_CAPABILITY_POINTER) != 0 && info.wlPointer == null)
	{
		info.wlPointer = wlSeatGetPointer(seat);
		wlPointerAddListener(info.wlPointer, &rgfw_wlPointerListener, info);
	}
	if ((capabilities & WL_SEAT_CAPABILITY_KEYBOARD) != 0 && info.wlKeyboard == null)
	{
		info.wlKeyboard = wlSeatGetKeyboard(seat);
		wlKeyboardAddListener(info.wlKeyboard, &rgfw_wlKeyboardListener, info);
	}
	if (!(capabilities & WL_SEAT_CAPABILITY_POINTER) && info.wlPointer != null) wlPointerDestroy(info.wlPointer);
	if (!(capabilities & WL_SEAT_CAPABILITY_KEYBOARD) && info.wlKeyboard != null) wlKeyboardDestroy(info.wlKeyboard);
}

fn void rgfw_wlSeatName(void* data, WlProxy* seat, char* name) {}

WlSeatListener rgfw_wlSeatListener = { .capabilities = &rgfw_wlSeatCapabilities, .name = &rgfw_wlSeatName };

WlShmListener rgfw_wlShmListener = { .format = &rgfw_wlShmFormatHandler };

fn void rgfw_wlOutputSetGeometry(void* data, WlProxy* output, int x, int y, int physW, int physH, int subpixel, char* make, char* model, int transform)
{
	RgfwMonitor* mon = &((RgfwMonitorNode*)data).mon;
	mon.x = x;
	mon.y = y;
	mon.physW = (float)physW / 25.4f;
	mon.physH = (float)physH / 25.4f;
}

fn void rgfw_wlOutputHandleMode(void* data, WlProxy* output, uint flags, int width, int height, int refresh)
{
	RgfwMonitorNode* node = (RgfwMonitorNode*)data;
	RgfwMonitor* mon = &node.mon;

	RgfwMonitorMode mode;
	mode.w = width;
	mode.h = height;
	mode.refreshRate = (float)refresh / 1000.0f;
	mode.src = output;

	node.modeCount += 1;
	RgfwMonitorMode* modes = (RgfwMonitorMode*)rgfw_alloc_impl(node.modeCount * RgfwMonitorMode::size);
	if (node.modeCount > 1)
	{
		rgfw_monitor_getModesPtr(mon, &modes);
		rgfw_free_impl(node.modes);
	}
	modes[node.modeCount - 1] = mode;
	node.modes = modes;

	if (flags & WL_OUTPUT_MODE_CURRENT) mon.mode = mode;
}

fn void rgfw_wlOutputSetScale(void* data, WlProxy* output, int factor)
{
	RgfwMonitor* mon = &((RgfwMonitorNode*)data).mon;
	mon.scaleX = (float)factor;
	mon.scaleY = (float)factor;
}

fn void rgfw_wlOutputSetName(void* data, WlProxy* output, char* name)
{
	RgfwMonitor* mon = &((RgfwMonitorNode*)data).mon;
	usz n = strlen(name);
	if (n > 127) n = 127;
	rgfw_memcpy(&mon.name, name, n);
	mon.name[n] = 0;
}

fn void rgfw_wlOutputDescription(void* data, WlProxy* output, char* description) {}

fn void rgfw_xdgOutputLogicalPos(void* data, WlProxy* xdgOutput, int x, int y)
{
	RgfwMonitor* mon = &((RgfwMonitorNode*)data).mon;
	mon.x = x;
	mon.y = y;
}

fn void rgfw_xdgOutputLogicalSize(void* data, WlProxy* xdgOutput, int width, int height)
{
	RgfwMonitor* mon = &((RgfwMonitorNode*)data).mon;
	float monFloatWidth = (float)mon.mode.w;
	float scaleX = monFloatWidth / (float)width;
	float dpi = scaleX * 96.0f;
	mon.pixelRatio = dpi >= 192.0f ? 2.0f : 1.0f;
	mon.mode.w = width;
	mon.mode.h = height;
}

fn void rgfw_xdgOutputNothing(void* data, WlProxy* xdgOutput) {}

fn void rgfw_xdgOutputNothingStr(void* data, WlProxy* xdgOutput, char* s) {}

fn void rgfw_wlOutputHandleDone(void* data, WlProxy* output)
{
	RgfwMonitorNode* node = (RgfwMonitorNode*)data;
	RgfwMonitor* mon = &node.mon;

	if (mon.physW <= 0 || mon.physH <= 0)
	{
		mon.physW = (float)mon.mode.w / 96.0f;
		mon.physH = (float)mon.mode.h / 96.0f;
	}

	if (node.disconnected == RGFW_FALSE) return;
	node.disconnected = RGFW_TRUE;
	rgfw_monitorCallback(_rgfw.root, mon, RGFW_TRUE);
}

WlOutputListener rgfw_wlOutputListener = {
	.geometry = &rgfw_wlOutputSetGeometry, .mode = &rgfw_wlOutputHandleMode, .done = &rgfw_wlOutputHandleDone,
	.scale = &rgfw_wlOutputSetScale, .name = &rgfw_wlOutputSetName, .description = &rgfw_wlOutputDescription
};

ZxdgOutputV1Listener rgfw_xdgOutputListener = {
	.name = &rgfw_xdgOutputNothingStr, .done = &rgfw_xdgOutputNothing, .description = &rgfw_xdgOutputNothingStr,
	.logicalPosition = &rgfw_xdgOutputLogicalPos, .logicalSize = &rgfw_xdgOutputLogicalSize
};

fn void rgfw_wlCreateOutputs(WlProxy* registry, uint id)
{
	// wl_output name/description events (used by rgfw_wlOutputSetName below) require version 4+.
	// If wl_seat hasn't been bound yet (compositor announced wl_output first), fall back to
	// version 4 instead of calling wlProxyGetVersion(null), which would crash.
	uint fallbackVersion = 4;
	WlProxy* output = wlRegistryBind(registry, id, &wlOutputInterface, _rgfw.seat == null ? fallbackVersion : wlProxyGetVersion(_rgfw.seat));
	if (output == null) return;

	RgfwMonitor mon;
	rgfw_memzero(&mon, RgfwMonitor::size);
	char[10] defaultName;
	int n = 0;
	char* prefix = "monitor-";
	while (prefix[n] != 0) { defaultName[n] = prefix[n]; n++; }
	defaultName[n] = (char)('0' + (_rgfw.monitors.count % 10));
	defaultName[n + 1] = 0;
	rgfw_memcpy(&mon.name, &defaultName, (usz)n + 2);

	mon.scaleY = 1.0f; mon.scaleX = 1.0f; mon.pixelRatio = 1.0f;

	RgfwMonitorNode* node = rgfw_monitors_add(&mon);
	if (node == null) return;

	node.modeCount = 0;
	node.disconnected = RGFW_TRUE;
	node.id = id;
	node.output = output;

	wlOutputSetUserData(output, node);
	wlOutputAddListener(output, &rgfw_wlOutputListener, node);

	if (_rgfw.xdgOutputManager == null) return;

	node.xdgOutput = zxdgOutputManagerV1GetXdgOutput(_rgfw.xdgOutputManager, node.output);
	zxdgOutputV1AddListener(node.xdgOutput, &rgfw_xdgOutputListener, node);
}

fn void rgfw_wlSurfaceEnter(void* data, WlProxy* surface, WlProxy* output)
{
	RgfwWindow* win = (RgfwWindow*)data;
	RgfwMonitorNode* node = (RgfwMonitorNode*)wlOutputGetUserData(output);
	if (node == null) return;
	win.src.activeMonitor = node;
}

fn void rgfw_wlSurfaceLeave(void* data, WlProxy* surface, WlProxy* output) {}

fn void rgfw_wlSurfacePrefScale(void* data, WlProxy* surface, int scale) {}

fn void rgfw_wlSurfacePrefTransform(void* data, WlProxy* surface, uint transform) {}

fn void rgfw_wlDataSourceSend(void* data, WlProxy* source, char* mimeType, int fd)
{
	if (rgfw_bool(mimeType != null) && strstr(mimeType, "text/plain") != null)
	{
		usz length = _rgfw.unixClipboard.length;
		if (_rgfw.unixClipboard.data[0] == 0) length -= 1;
		write(fd, (void*)_rgfw.unixClipboard.data, length);
	}
	close(fd);
}

fn void rgfw_wlDataSourceCancelled(void* data, WlProxy* source)
{
	RgfwInfo* info = (RgfwInfo*)data;
	if (info.kbOwner != null && info.kbOwner.src.dataSource == source) info.kbOwner.src.dataSource = null;
	wlDataSourceDestroy(source);
}

fn void rgfw_wlDataSourceNothingStr(void* data, WlProxy* source, char* s) {}

fn void rgfw_wlDataSourceNothing(void* data, WlProxy* source) {}

fn void rgfw_wlDataSourceNothingU32(void* data, WlProxy* source, uint u) {}

fn void rgfw_wlDataOfferNothingStr(void* data, WlProxy* offer, char* s) {}

fn void rgfw_wlDataOfferNothingU32(void* data, WlProxy* offer, uint u) {}

fn void rgfw_wlDataDeviceDataOffer(void* data, WlProxy* device, WlProxy* offer)
{
	static WlDataOfferListener listener = { .offer = &rgfw_wlDataOfferNothingStr, .sourceActions = &rgfw_wlDataOfferNothingU32, .action = &rgfw_wlDataOfferNothingU32 };
	wlDataOfferAddListener(offer, &listener, null);
}

fn void rgfw_wlDataDeviceSelection(void* data, WlProxy* device, WlProxy* offer)
{
	if (offer == null) return;

	int[2] pfds;
	pipe(&pfds);
	wlDataOfferReceive(offer, "text/plain", pfds[1]);
	close(pfds[1]);
	wlDisplayRoundtrip(_rgfw.wlDisplay);

	char[1024] buf;
	long n = read(pfds[0], &buf, 1024);
	if (n <= 0) { close(pfds[0]); wlDataOfferDestroy(offer); return; }

	usz length = (usz)n;
	if (buf[n - 1] != 0) length += 1;

	if (_rgfw.unixClipboard != null) rgfw_free_impl(_rgfw.unixClipboard);
	_rgfw.unixClipboard = (RgfwDataTransfer*)rgfw_alloc_impl(RgfwDataTransfer::size + (usz)n);
	char* dataPtr = (char*)_rgfw.unixClipboard + RgfwDataTransfer::size - 1;
	rgfw_memcpy(dataPtr, &buf, (usz)n);
	dataPtr[length - 1] = 0;
	_rgfw.unixClipboard.data = dataPtr;
	_rgfw.unixClipboard.type = RGFW_DATA_TEXT;
	_rgfw.unixClipboard.length = length;

	close(pfds[0]);
	wlDataOfferDestroy(offer);
}

fn void rgfw_wlDataDeviceNothing(void* data, WlProxy* device) {}

fn void rgfw_wlDataDeviceNothingEnter(void* data, WlProxy* device, uint serial, WlProxy* surface, WlFixed x, WlFixed y, WlProxy* offer) {}

fn void rgfw_wlDataDeviceNothingMotion(void* data, WlProxy* device, uint time, WlFixed x, WlFixed y) {}

fn void rgfw_wlGlobalRegistryHandler(void* data, WlProxy* registry, uint id, char* iface, uint version)
{
	RgfwInfo* info = (RgfwInfo*)data;

	if (strstr(iface, "wl_compositor") == iface) { info.compositor = wlRegistryBind(registry, id, &wlCompositorInterface, 4); }
	else if (strstr(iface, "xdg_wm_base") == iface) { info.xdgWmBase = wlRegistryBind(registry, id, &xdgWmBaseInterface, 1); }
	else if (strstr(iface, zxdgDecorationManagerV1Interface.name) == iface) { info.decorationManager = wlRegistryBind(registry, id, &zxdgDecorationManagerV1Interface, 1); }
	else if (strstr(iface, zwpPointerConstraintsV1Interface.name) == iface) { info.constraintManager = wlRegistryBind(registry, id, &zwpPointerConstraintsV1Interface, 1); }
	else if (strstr(iface, zwpRelativePointerManagerV1Interface.name) == iface) { info.relativePointerManager = wlRegistryBind(registry, id, &zwpRelativePointerManagerV1Interface, 1); }
	else if (strstr(iface, xdgToplevelIconManagerV1Interface.name) == iface) { info.iconManager = wlRegistryBind(registry, id, &xdgToplevelIconManagerV1Interface, 1); }
	else if (strstr(iface, "wl_shm") == iface)
	{
		info.shm = wlRegistryBind(registry, id, &wlShmInterface, 1);
		wlShmAddListener(info.shm, &rgfw_wlShmListener, info);
	}
	else if (strstr(iface, "wl_seat") == iface)
	{
		info.seat = wlRegistryBind(registry, id, &wlSeatInterface, version < 4 ? 3 : 4);
		wlSeatAddListener(info.seat, &rgfw_wlSeatListener, info);
	}
	else if (strstr(iface, zxdgOutputManagerV1Interface.name) == iface) { info.xdgOutputManager = wlRegistryBind(registry, id, &zxdgOutputManagerV1Interface, 1); }
	else if (strstr(iface, "wl_output") == iface) { rgfw_wlCreateOutputs(registry, id); }
	else if (strstr(iface, wpPointerWarpV1Interface.name) == iface) { info.wpPointerWarp = wlRegistryBind(registry, id, &wpPointerWarpV1Interface, 1); }
	else if (strstr(iface, "wl_data_device_manager") == iface) { info.dataDeviceManager = wlRegistryBind(registry, id, &wlDataDeviceManagerInterface, 1); }
}

fn void rgfw_wlGlobalRegistryRemove(void* data, WlProxy* registry, uint id)
{
	RgfwInfo* info = (RgfwInfo*)data;
	RgfwMonitorNode* prev = info.monitors.list.head;
	RgfwMonitorNode* node = null;
	if (prev == null) return;

	if (prev.id != id)
	{
		while (prev.next != null && prev.next.id != id) prev = prev.next;
		if (prev.next == null) return;
		node = prev.next;
	}
	else { node = prev; }

	rgfw_monitorCallback(_rgfw.root, &node.mon, RGFW_FALSE);
	rgfw_monitors_remove(node, prev);
}

WlRegistryListener rgfw_registryListener = { .global = &rgfw_wlGlobalRegistryHandler, .globalRemove = &rgfw_wlGlobalRegistryRemove };

fn void rgfw_wlRandname(char* buf)
{
	Timespec ts;
	clock_gettime(0, &ts);
	long r = ts.tv_nsec;
	for (int i = 0; i < 6; i++)
	{
		buf[i] = (char)('A' + (r & 15) + (r & 16) * 2);
		r >>= 5;
	}
}

fn int rgfw_wlAnonymousShmOpen()
{
	char[22] name = "/RGFW-wayland-XXXXXX\0\0";
	int retries = 100;
	do
	{
		rgfw_wlRandname((char*)&name + strlen((char*)&name) - 6);
		retries--;
		int fd = shmOpen((char*)&name, O_RDWR | O_CREAT | O_EXCL, 384);
		if (fd >= 0) { shmUnlink((char*)&name); return fd; }
	} while (retries > 0);
	return -1;
}

fn int rgfw_wlCreateShmFile(long size)
{
	int fd = rgfw_wlAnonymousShmOpen();
	if (fd < 0) return fd;
	if (ftruncate(fd, size) < 0) { close(fd); return -1; }
	return fd;
}

fn RgfwFormat rgfw_nativeFormat_Wayland() => RGFW_FORMAT_BGRA8;

fn bool rgfw_createSurfacePtr_Wayland(char* data, int w, int h, RgfwFormat format, RgfwSurface* surface)
{
	surface.data = data;
	surface.w = w;
	surface.h = h;
	surface.format = format;

	uint size = (uint)(surface.w * surface.h * 4);
	int fd = rgfw_wlCreateShmFile((long)size);
	if (fd < 0)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_BUFFER, "Failed to create a buffer.");
		return RGFW_FALSE;
	}

	surface.native.pool = wlShmCreatePool(_rgfw.shm, fd, (int)size);
	surface.native.buffer = mmap(null, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
	if (surface.native.buffer == MAP_FAILED)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_BUFFER, "mmap failed.");
		return RGFW_FALSE;
	}

	surface.native.fd = fd;
	surface.native.format = RGFW_FORMAT_BGRA8;
	return RGFW_TRUE;
}

fn void rgfw_window_blitSurface_Wayland(RgfwWindow* win, RgfwSurface* surface)
{
	int minW = win.w < surface.w ? win.w : surface.w;
	int minH = win.h < surface.h ? win.h : surface.h;
	surface.native.wlBuffer = wlShmPoolCreateBuffer(surface.native.pool, 0, minW, minH, surface.w * 4, WL_SHM_FORMAT_ARGB8888);

	wlSurfaceAttach(win.src.surface, surface.native.wlBuffer, 0, 0);
	wlSurfaceDamage(win.src.surface, 0, 0, minW, minH);
	wlSurfaceCommit(win.src.surface);
	wlBufferDestroy(surface.native.wlBuffer);
	surface.native.wlBuffer = null;
}

fn void rgfw_surface_freePtr_Wayland(RgfwSurface* surface)
{
	if (surface.native.pool != null) wlShmPoolDestroy(surface.native.pool);
	if (surface.native.fd != 0) close(surface.native.fd);
	if (surface.native.buffer != null) munmap(surface.native.buffer, (usz)(surface.w * surface.h * 4));
}

fn void rgfw_window_setBorder_Wayland(RgfwWindow* win, bool border)
{
	rgfw_setBit(&win.internal.flags, RGFW_WINDOW_NO_BORDER, rgfw_bool(!border));
	if (_rgfw.decorationManager != null)
	{
		zxdgToplevelDecorationV1SetMode(win.src.decoration, border ? ZXDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE : ZXDG_TOPLEVEL_DECORATION_V1_MODE_CLIENT_SIDE);
	}
}

fn void rgfw_window_setRawMouseModePlatform_Wayland(RgfwWindow* win, bool state)
{
	if (_rgfw.relativePointerManager == null) return;

	if (state == RGFW_FALSE)
	{
		if (_rgfw.relativePointer != null) zwpRelativePointerV1Destroy(_rgfw.relativePointer);
		_rgfw.relativePointer = null;
		return;
	}
	if (_rgfw.relativePointer != null) return;

	_rgfw.relativePointer = zwpRelativePointerManagerV1GetRelativePointer(_rgfw.relativePointerManager, _rgfw.wlPointer);
	static ZwpRelativePointerV1Listener listener = { .relativeMotion = &rgfw_wlRelativePointerMotion };
	zwpRelativePointerV1AddListener(_rgfw.relativePointer, &listener, _rgfw);
}

fn void rgfw_wlUnlockedNoop(void* data, WlProxy* p) {}

fn void rgfw_window_captureMousePlatform_Wayland(RgfwWindow* win, bool state)
{
	if (_rgfw.constraintManager == null) return;

	if (state == RGFW_FALSE)
	{
		if (win.src.lockedPointer != null) zwpLockedPointerV1Destroy(win.src.lockedPointer);
		win.src.lockedPointer = null;
		return;
	}
	if (win.src.lockedPointer != null) return;

	win.src.lockedPointer = zwpPointerConstraintsV1LockPointer(_rgfw.constraintManager, win.src.surface, _rgfw.wlPointer, null, ZWP_POINTER_CONSTRAINTS_V1_LIFETIME_PERSISTENT);
	static ZwpLockedPointerV1Listener listener = { .locked = &rgfw_wlPointerLocked, .unlocked = &rgfw_wlUnlockedNoop };
	zwpLockedPointerV1AddListener(win.src.lockedPointer, &listener, _rgfw);
}

XdgSurfaceListener rgfw_xdgSurfaceListener = { .configure = &rgfw_wlXdgSurfaceConfigureHandler };

WlSurfaceListener rgfw_windowSurfaceListener = {
	.enter = &rgfw_wlSurfaceEnter, .leave = &rgfw_wlSurfaceLeave,
	.preferredBufferScale = &rgfw_wlSurfacePrefScale, .preferredBufferTransform = &rgfw_wlSurfacePrefTransform
};

XdgToplevelListener rgfw_xdgToplevelListener = { .configure = &rgfw_wlXdgToplevelConfigureHandler, .close = &rgfw_wlXdgToplevelCloseHandler };

ZxdgToplevelDecorationV1Listener rgfw_xdgDecorationListener = { .configure = &rgfw_wlXdgDecorationConfigureHandler };

fn RgfwWindow* rgfw_createWindowPlatform_Wayland(char* name, RgfwWindowFlags flags, RgfwWindow* win)
{
	rgfw_debugCallback(RGFW_TYPE_WARNING, RGFW_WARNING_WAYLAND, "RGFW Wayland support is experimental");

	win.src.surface = wlCompositorCreateSurface(_rgfw.compositor);
	wlSurfaceAddListener(win.src.surface, &rgfw_windowSurfaceListener, win);
	wlSurfaceSetUserData(win.src.surface, win);

	win.src.customCursorSurface = wlCompositorCreateSurface(_rgfw.compositor);

	win.src.xdgSurface = xdgWmBaseGetXdgSurface(_rgfw.xdgWmBase, win.src.surface);
	xdgSurfaceAddListener(win.src.xdgSurface, &rgfw_xdgSurfaceListener, win);
	xdgWmBaseSetUserData(_rgfw.xdgWmBase, win);

	win.src.xdgToplevel = xdgSurfaceGetToplevel(win.src.xdgSurface);
	xdgToplevelSetAppId(win.src.xdgToplevel, name);
	xdgSurfaceSetWindowGeometry(win.src.xdgSurface, 0, 0, win.w, win.h);

	if (!(win.internal.flags & RGFW_WINDOW_TRANSPARENT)) rgfw_wlSetOpaque(win);

	xdgToplevelAddListener(win.src.xdgToplevel, &rgfw_xdgToplevelListener, win);

	if (_rgfw.decorationManager != null)
	{
		uint decorationMode = ZXDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE;
		win.src.decoration = zxdgDecorationManagerV1GetToplevelDecoration(_rgfw.decorationManager, win.src.xdgToplevel);
		zxdgToplevelDecorationV1AddListener(win.src.decoration, &rgfw_xdgDecorationListener, win);
		if (flags & RGFW_WINDOW_NO_BORDER) decorationMode = ZXDG_TOPLEVEL_DECORATION_V1_MODE_CLIENT_SIDE;
		zxdgToplevelDecorationV1SetMode(win.src.decoration, decorationMode);
	}

	if (_rgfw.iconManager != null) xdgToplevelIconManagerV1SetIcon(_rgfw.iconManager, win.src.xdgToplevel, null);

	wlSurfaceCommit(win.src.surface);

	while (win.src.configured == RGFW_FALSE) wlDisplayDispatch(_rgfw.wlDisplay);

	return win;
}

fn bool rgfw_getGlobalMouse_Wayland(int* x, int* y)
{
	if (x != null) *x = 0;
	if (y != null) *y = 0;
	return RGFW_FALSE;
}

fn RgfwKey rgfw_physicalToMappedKey_Wayland(RgfwKey key)
{
	uint keycode = rgfw_rgfwToApiKey(key);
	XkbKeycode kc = keycode + 8;
	XkbKeysym sym = xkbStateKeyGetOneSym(_rgfw.xkbState, kc);
	if (sym < 256) return (RgfwKey)sym;

	switch (sym)
	{
		case XKB_KEY_F1: return RGFW_KEY_F1;
		case XKB_KEY_F2: return RGFW_KEY_F2;
		case XKB_KEY_F3: return RGFW_KEY_F3;
		case XKB_KEY_F4: return RGFW_KEY_F4;
		case XKB_KEY_F5: return RGFW_KEY_F5;
		case XKB_KEY_F6: return RGFW_KEY_F6;
		case XKB_KEY_F7: return RGFW_KEY_F7;
		case XKB_KEY_F8: return RGFW_KEY_F8;
		case XKB_KEY_F9: return RGFW_KEY_F9;
		case XKB_KEY_F10: return RGFW_KEY_F10;
		case XKB_KEY_F11: return RGFW_KEY_F11;
		case XKB_KEY_F12: return RGFW_KEY_F12;
		case XKB_KEY_F13: return RGFW_KEY_F13;
		case XKB_KEY_F14: return RGFW_KEY_F14;
		case XKB_KEY_F15: return RGFW_KEY_F15;
		case XKB_KEY_F16: return RGFW_KEY_F16;
		case XKB_KEY_F17: return RGFW_KEY_F17;
		case XKB_KEY_F18: return RGFW_KEY_F18;
		case XKB_KEY_F19: return RGFW_KEY_F19;
		case XKB_KEY_F20: return RGFW_KEY_F20;
		case XKB_KEY_F21: return RGFW_KEY_F21;
		case XKB_KEY_F22: return RGFW_KEY_F22;
		case XKB_KEY_F23: return RGFW_KEY_F23;
		case XKB_KEY_F24: return RGFW_KEY_F24;
		case XKB_KEY_F25: return RGFW_KEY_F25;
		case XKB_KEY_SHIFT_L: return RGFW_KEY_SHIFT_L;
		case XKB_KEY_SHIFT_R: return RGFW_KEY_SHIFT_R;
		case XKB_KEY_CONTROL_L: return RGFW_KEY_CONTROL_L;
		case XKB_KEY_CONTROL_R: return RGFW_KEY_CONTROL_R;
		case XKB_KEY_ALT_L: return RGFW_KEY_ALT_L;
		case XKB_KEY_ALT_R: return RGFW_KEY_ALT_R;
		case XKB_KEY_SUPER_L: return RGFW_KEY_SUPER_L;
		case XKB_KEY_SUPER_R: return RGFW_KEY_SUPER_R;
		case XKB_KEY_CAPS_LOCK: return RGFW_KEY_CAPS_LOCK;
		case XKB_KEY_NUM_LOCK: return RGFW_KEY_NUM_LOCK;
		case XKB_KEY_SCROLL_LOCK: return RGFW_KEY_SCROLL_LOCK;
		case XKB_KEY_UP: return RGFW_KEY_UP;
		case XKB_KEY_DOWN: return RGFW_KEY_DOWN;
		case XKB_KEY_LEFT: return RGFW_KEY_LEFT;
		case XKB_KEY_RIGHT: return RGFW_KEY_RIGHT;
		case XKB_KEY_HOME: return RGFW_KEY_HOME;
		case XKB_KEY_END: return RGFW_KEY_END;
		case XKB_KEY_PAGE_UP: return RGFW_KEY_PAGE_UP;
		case XKB_KEY_PAGE_DOWN: return RGFW_KEY_PAGE_DOWN;
		case XKB_KEY_INSERT: return RGFW_KEY_INSERT;
		case XKB_KEY_MENU: return RGFW_KEY_MENU;
		case XKB_KEY_KP_ADD: return RGFW_KEY_PAD_PLUS;
		case XKB_KEY_KP_SUBTRACT: return RGFW_KEY_PAD_MINUS;
		case XKB_KEY_KP_MULTIPLY: return RGFW_KEY_PAD_MULTIPLY;
		case XKB_KEY_KP_DIVIDE: return RGFW_KEY_PAD_SLASH;
		case XKB_KEY_KP_EQUAL: return RGFW_KEY_PAD_EQUAL;
		case XKB_KEY_KP_ENTER: return RGFW_KEY_PAD_RETURN;
		case XKB_KEY_KP_DECIMAL: return RGFW_KEY_PAD_PERIOD;
		case XKB_KEY_KP_0: return RGFW_KEY_PAD0;
		case XKB_KEY_KP_1: return RGFW_KEY_PAD1;
		case XKB_KEY_KP_2: return RGFW_KEY_PAD2;
		case XKB_KEY_KP_3: return RGFW_KEY_PAD3;
		case XKB_KEY_KP_4: return RGFW_KEY_PAD4;
		case XKB_KEY_KP_5: return RGFW_KEY_PAD5;
		case XKB_KEY_KP_6: return RGFW_KEY_PAD6;
		case XKB_KEY_KP_7: return RGFW_KEY_PAD7;
		case XKB_KEY_KP_8: return RGFW_KEY_PAD8;
		case XKB_KEY_KP_9: return RGFW_KEY_PAD9;
		case XKB_KEY_PRINT: return RGFW_KEY_PRINT_SCREEN;
		case XKB_KEY_PAUSE: return RGFW_KEY_PAUSE;
		default: break;
	}
	return RGFW_KEY_NULL;
}

fn bool rgfw_window_fetchSize_Wayland(RgfwWindow* win, int* w, int* h) => rgfw_window_getSize(win, w, h);

fn void rgfw_pollEvents_Wayland()
{
	rgfw_resetPrevState();

	while (wlDisplayFlush(_rgfw.wlDisplay) == -1)
	{
		if (*rgfwErrnoLocation() == RGFW_EAGAIN)
		{
			if (wlDisplayDispatchPending(_rgfw.wlDisplay) == -1) return;
		}
		else { return; }
	}

	if (_rgfw.wlRepeatInfoRate != 0 && _rgfw.lastKey != 0)
	{
		uint now = (uint)(rgfw_unix_getTimeNS() / 1000000);
		if (now > _rgfw.lastKeyTime)
		{
			rgfw_wlSendKeyEvent(_rgfw.lastKey);
			_rgfw.lastKeyTime = now + 1000 / (uint)_rgfw.wlRepeatInfoRate;
		}
	}

	Pollfd fds;
	rgfw_memzero(&fds, Pollfd::size);
	fds.fd = wlDisplayGetFd(_rgfw.wlDisplay);
	fds.events = (short)POLLIN;

	while (true)
	{
		while (wlDisplayPrepareRead(_rgfw.wlDisplay) != 0)
		{
			if (wlDisplayDispatchPending(_rgfw.wlDisplay) > 0) return;
		}

		if (poll(&fds, 1, 0) == 0) { wlDisplayCancelRead(_rgfw.wlDisplay); return; }

		if (fds.revents & POLLIN)
		{
			wlDisplayReadEvents(_rgfw.wlDisplay);
			if (wlDisplayDispatchPending(_rgfw.wlDisplay) > 0) return;
		}
		else { wlDisplayCancelRead(_rgfw.wlDisplay); }
	}
}

fn void rgfw_window_movePlatform_Wayland(RgfwWindow* win, int x, int y) { win.x = x; win.y = y; }

fn void rgfw_window_resizePlatform_Wayland(RgfwWindow* win, int w, int h)
{
	win.w = w; win.h = h;
	if (_rgfw.compositor != null)
	{
		xdgSurfaceSetWindowGeometry(win.src.xdgSurface, 0, 0, win.w, win.h);
		if (win.src.ctx.egl != null && win.src.ctx.egl.eglWindow != null) wlEglWindowResize((WlEglWindow*)win.src.ctx.egl.eglWindow, w, h, 0, 0);
	}
}

fn void rgfw_window_setAspectRatio_Wayland(RgfwWindow* win, int w, int h)
{
	if (w == 0 && h == 0) return;
	xdgToplevelSetMaxSize(win.src.xdgToplevel, w, h);
}

fn void rgfw_window_setMinSize_Wayland(RgfwWindow* win, int w, int h) { xdgToplevelSetMinSize(win.src.xdgToplevel, w, h); }

fn void rgfw_window_setMaxSize_Wayland(RgfwWindow* win, int w, int h) { xdgToplevelSetMaxSize(win.src.xdgToplevel, w, h); }

fn void rgfw_window_maximizePlatform_Wayland(RgfwWindow* win) => rgfw_toggleWaylandMaximized(win, 1);

fn void rgfw_window_focus_Wayland(RgfwWindow* win) {}

fn void rgfw_window_raise_Wayland(RgfwWindow* win) {}

fn void rgfw_window_setFullscreen_Wayland(RgfwWindow* win, bool fullscreen)
{
	if (fullscreen)
	{
		win.internal.flags |= RGFW_WINDOW_FULLSCREEN;
		win.internal.oldX = win.x; win.internal.oldY = win.y; win.internal.oldW = win.w; win.internal.oldH = win.h;
		xdgToplevelSetFullscreen(win.src.xdgToplevel, null);
	}
	else
	{
		win.internal.flags &= ~(uint)RGFW_WINDOW_FULLSCREEN;
		xdgToplevelUnsetFullscreen(win.src.xdgToplevel);
	}
}

fn void rgfw_window_setFloating_Wayland(RgfwWindow* win, bool floating) {}

fn void rgfw_window_setOpacity_Wayland(RgfwWindow* win, char opacity) {}

fn void rgfw_window_minimizePlatform_Wayland(RgfwWindow* win)
{
	if (rgfw_window_isMaximized(win)) return;
	win.internal.oldX = win.x; win.internal.oldY = win.y; win.internal.oldW = win.w; win.internal.oldH = win.h;
	win.src.minimized = RGFW_TRUE;
	xdgToplevelSetMinimized(win.src.xdgToplevel);
}

fn void rgfw_window_restorePlatform_Wayland(RgfwWindow* win) => rgfw_toggleWaylandMaximized(win, RGFW_FALSE);

fn bool rgfw_window_isFloating_Wayland(RgfwWindow* win) => rgfw_bool(!rgfw_window_isFullscreen(win) && !rgfw_window_isMaximized(win));

fn void rgfw_window_setName_Wayland(RgfwWindow* win, char* name)
{
	if (name == null) name = "";
	if (_rgfw.compositor != null) xdgToplevelSetTitle(win.src.xdgToplevel, name);
}

fn void rgfw_window_setMousePassthrough_Wayland(RgfwWindow* win, bool passthrough) {}

fn bool rgfw_window_setIconEx_Wayland(RgfwWindow* win, char* data, int w, int h, RgfwFormat format, RgfwIcon type)
{
	if (_rgfw.iconManager == null || w != h) return RGFW_FALSE;

	if (win.src.icon != null) { xdgToplevelIconV1Destroy(win.src.icon); win.src.icon = null; }

	RgfwSurface* surface = rgfw_createSurface(data, w, h, format);
	if (surface == null) return RGFW_FALSE;

	int minW = w < surface.w ? w : surface.w;
	int minH = h < surface.h ? h : surface.h;
	rgfw_copyImageData(surface.native.buffer, minW, minH, surface.native.format, surface.data, surface.format, null);

	win.src.icon = xdgToplevelIconManagerV1CreateIcon(_rgfw.iconManager);
	surface.native.wlBuffer = wlShmPoolCreateBuffer(surface.native.pool, 0, w, h, w * 4, WL_SHM_FORMAT_ARGB8888);
	xdgToplevelIconV1AddBuffer(win.src.icon, surface.native.wlBuffer, 1);
	xdgToplevelIconManagerV1SetIcon(_rgfw.iconManager, win.src.xdgToplevel, win.src.icon);

	rgfw_surface_free(surface);
	return RGFW_TRUE;
}

fn RgfwMouse* rgfw_createMouseStandard_Wayland(RgfwMouseIcon mouse)
{
	char* cursorName;
	switch (mouse)
	{
		case RGFW_MOUSE_NORMAL: cursorName = "left_ptr";
		case RGFW_MOUSE_ARROW: cursorName = "left_ptr";
		case RGFW_MOUSE_IBEAM: cursorName = "xterm";
		case RGFW_MOUSE_CROSSHAIR: cursorName = "crosshair";
		case RGFW_MOUSE_POINTING_HAND: cursorName = "hand2";
		case RGFW_MOUSE_RESIZE_EW: cursorName = "sb_h_double_arrow";
		case RGFW_MOUSE_RESIZE_NS: cursorName = "sb_v_double_arrow";
		case RGFW_MOUSE_RESIZE_NWSE: cursorName = "top_left_corner";
		case RGFW_MOUSE_RESIZE_NESW: cursorName = "top_right_corner";
		case RGFW_MOUSE_RESIZE_NW: cursorName = "top_left_corner";
		case RGFW_MOUSE_RESIZE_N: cursorName = "top_side";
		case RGFW_MOUSE_RESIZE_NE: cursorName = "top_right_corner";
		case RGFW_MOUSE_RESIZE_E: cursorName = "right_side";
		case RGFW_MOUSE_RESIZE_SE: cursorName = "bottom_right_corner";
		case RGFW_MOUSE_RESIZE_S: cursorName = "bottom_side";
		case RGFW_MOUSE_RESIZE_SW: cursorName = "bottom_left_corner";
		case RGFW_MOUSE_RESIZE_W: cursorName = "left_side";
		case RGFW_MOUSE_RESIZE_ALL: cursorName = "fleur";
		case RGFW_MOUSE_NOT_ALLOWED: cursorName = "not-allowed";
		case RGFW_MOUSE_WAIT: cursorName = "watch";
		case RGFW_MOUSE_PROGRESS: cursorName = "watch";
		default: return null;
	}

	WlCursor* wlcursor = wlCursorThemeGetCursor(_rgfw.wlCursorTheme, cursorName);
	if (wlcursor == null) return null;
	WlCursorImage* cursorImage = wlcursor.images[0];
	WlProxy* cursorBuffer = wlCursorImageGetBuffer(cursorImage);

	RgfwSurface* surface = (RgfwSurface*)rgfw_alloc_impl(RgfwSurface::size);
	rgfw_memzero(surface, RgfwSurface::size);
	surface.w = (int)cursorImage.width;
	surface.h = (int)cursorImage.height;
	surface.native.wlBuffer = cursorBuffer;
	return (RgfwMouse*)surface;
}

fn RgfwMouse* rgfw_createMouse_Wayland(char* data, int w, int h, RgfwFormat format)
{
	RgfwSurface* surface = rgfw_createSurface(data, w, h, format);
	if (surface == null) return null;

	surface.native.wlBuffer = wlShmPoolCreateBuffer(surface.native.pool, 0, surface.w, surface.h, surface.w * 4, WL_SHM_FORMAT_ARGB8888);
	int minW = w < surface.w ? w : surface.w;
	int minH = h < surface.h ? h : surface.h;
	rgfw_copyImageData(surface.native.buffer, minW, minH, surface.native.format, surface.data, surface.format, null);
	return (RgfwMouse*)surface;
}

fn bool rgfw_window_setMousePlatform_Wayland(RgfwWindow* win, RgfwMouse* mouse)
{
	RgfwSurface* surface = (RgfwSurface*)mouse;
	win.src.usingCustomCursor = RGFW_TRUE;
	wlSurfaceAttach(win.src.customCursorSurface, surface.native.wlBuffer, 0, 0);
	wlSurfaceDamage(win.src.customCursorSurface, 0, 0, surface.w, surface.h);
	wlSurfaceCommit(win.src.customCursorSurface);
	return RGFW_TRUE;
}

fn void rgfw_freeMouse_Wayland(RgfwMouse* mouse)
{
	if (mouse == null) return;
	RgfwSurface* surface = (RgfwSurface*)mouse;
	if (surface.native.buffer != null && surface.native.wlBuffer != null) wlBufferDestroy(surface.native.wlBuffer);
	rgfw_surface_free(surface);
}

fn void rgfw_window_moveMouse_Wayland(RgfwWindow* win, int x, int y)
{
	if (_rgfw.wpPointerWarp != null)
	{
		wpPointerWarpV1WarpPointer(_rgfw.wpPointerWarp, win.src.surface, _rgfw.wlPointer, wlFixedFromInt(x), wlFixedFromInt(y), _rgfw.mouseEnterSerial);
	}
}

fn void rgfw_window_hide_Wayland(RgfwWindow* win)
{
	wlSurfaceAttach(win.src.surface, null, 0, 0);
	wlSurfaceCommit(win.src.surface);
	win.internal.flags |= RGFW_WINDOW_HIDE;
}

fn void rgfw_window_showPlatform_Wayland(RgfwWindow* win)
{
	win.internal.flags &= ~(uint)RGFW_WINDOW_HIDE;
	wlSurfaceCommit(win.src.surface);
}

fn void rgfw_window_flash_Wayland(RgfwWindow* win, RgfwFlashRequest request) {}

fn bool rgfw_readClipboardPtr_Wayland(RgfwDataTransferType requestedType, char* buffer, usz capacity, RgfwDataTransfer* data)
{
	if (requestedType != RGFW_DATA_TEXT) return RGFW_FALSE;
	if (_rgfw.unixClipboard == null || _rgfw.unixClipboard.length == 0)
	{
		data.length = 0;
		data.type = RGFW_DATA_NONE;
		return RGFW_FALSE;
	}
	data.length = _rgfw.unixClipboard.length;
	data.type = RGFW_DATA_TEXT;
	if (buffer == null) return RGFW_TRUE;
	if (_rgfw.unixClipboard.length > capacity) return RGFW_FALSE;
	rgfw_memcpy(buffer, (void*)_rgfw.unixClipboard.data, _rgfw.unixClipboard.length);
	data.data = buffer;
	return RGFW_TRUE;
}

WlDataSourceListener rgfw_dataSourceListener = {
	.target = &rgfw_wlDataSourceNothingStr, .action = &rgfw_wlDataSourceNothingU32,
	.dndDropPerformed = &rgfw_wlDataSourceNothing, .dndFinished = &rgfw_wlDataSourceNothing,
	.send = &rgfw_wlDataSourceSend, .cancelled = &rgfw_wlDataSourceCancelled
};

fn bool rgfw_writeClipboard_Wayland(RgfwDataTransfer* data)
{
	if (_rgfw.dataDeviceManager == null) return RGFW_FALSE;
	if (_rgfw.unixClipboard != null) { rgfw_free_impl(_rgfw.unixClipboard); _rgfw.unixClipboard = null; }

	_rgfw.unixClipboard = (RgfwDataTransfer*)rgfw_alloc_impl(RgfwDataTransfer::size + data.length);
	usz length = data.length;
	if (data.data[length - 1] != 0) length += 1;

	char* dataPtr = (char*)_rgfw.unixClipboard + RgfwDataTransfer::size - 1;
	rgfw_memcpy(dataPtr, (void*)data.data, data.length);
	dataPtr[length - 1] = 0;
	_rgfw.unixClipboard.data = dataPtr;
	_rgfw.unixClipboard.type = RGFW_DATA_TEXT;
	_rgfw.unixClipboard.length = length;

	RgfwWindow* win = _rgfw.kbOwner;
	if (win.src.dataSource != null) { wlDataSourceDestroy(win.src.dataSource); win.src.dataSource = null; }

	win.src.dataSource = wlDataDeviceManagerCreateDataSource(_rgfw.dataDeviceManager);
	if (win.src.dataSource == null)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_CLIPBOARD, "Could not create clipboard data source");
		return RGFW_FALSE;
	}

	wlDataSourceAddListener(win.src.dataSource, &rgfw_dataSourceListener, _rgfw);
	wlDataSourceOffer(win.src.dataSource, "text/plain;charset=utf-8");
	return RGFW_TRUE;
}

fn bool rgfw_window_isHidden_Wayland(RgfwWindow* win) => RGFW_FALSE;

fn bool rgfw_window_isMinimized_Wayland(RgfwWindow* win) => win.src.minimized;

fn bool rgfw_window_isMaximized_Wayland(RgfwWindow* win) => win.src.maximized;

fn void rgfw_pollMonitors_Wayland() { _rgfw.monitors.primary = _rgfw.monitors.list.head; }

fn void rgfw_monitorNode_free_Wayland(RgfwMonitorNode* node)
{
	if (node.output != null) wlOutputDestroy(node.output);
	if (node.xdgOutput != null) zxdgOutputV1Destroy(node.xdgOutput);
	if (node.modes != null) { rgfw_free_impl(node.modes); node.modeCount = 0; }
}

fn bool rgfw_monitor_getWorkarea_Wayland(RgfwMonitor* monitor, int* x, int* y, int* width, int* height)
{
	if (x != null) *x = monitor.x;
	if (y != null) *y = monitor.y;
	if (width != null) *width = monitor.mode.w;
	if (height != null) *height = monitor.mode.h;
	return RGFW_TRUE;
}

fn usz rgfw_monitor_getModesPtr_Wayland(RgfwMonitor* monitor, RgfwMonitorMode** modes)
{
	if (modes != null) rgfw_memcpy(*modes, monitor.node.modes, monitor.node.modeCount * RgfwMonitorMode::size);
	return monitor.node.modeCount;
}

fn usz rgfw_monitor_getGammaRampPtr_Wayland(RgfwMonitor* monitor, RgfwGammaRamp* ramp) => 0;

fn bool rgfw_monitor_setGammaRamp_Wayland(RgfwMonitor* monitor, RgfwGammaRamp* ramp) => RGFW_FALSE;

fn bool rgfw_monitor_requestMode_Wayland(RgfwMonitor* mon, RgfwMonitorMode* mode, RgfwModeRequest request)
{
	for (usz i = 0; i < mon.node.modeCount; i++)
	{
		if (rgfw_monitorModeCompare(mode, &mon.node.modes[i], request) == RGFW_FALSE) continue;
		rgfw_monitor_setMode(mon, &mon.node.modes[i]);
		return RGFW_TRUE;
	}
	return RGFW_FALSE;
}

fn bool rgfw_monitor_setMode_Wayland(RgfwMonitor* mon, RgfwMonitorMode* mode) => RGFW_FALSE;

fn RgfwMonitor* rgfw_window_getMonitor_Wayland(RgfwWindow* win)
{
	if (win.src.activeMonitor == null) return rgfw_getPrimaryMonitor();
	return &win.src.activeMonitor.mon;
}

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

fn bool rgfw_extensionSupportedPlatform_EGL(char* extension, usz len)
{
	if (rgfw_eglQueryString == null || _rgfw.egl_display == null) return RGFW_FALSE;
	char* extensions = rgfw_eglQueryString((EglDisplay)_rgfw.egl_display, 0x3055 /* EGL_EXTENSIONS */);
	return rgfw_bool(extensions != null && rgfw_extensionSupportedStr(extensions, extension, len));
}

fn bool rgfw_extensionSupportedPlatform_OpenGL_Wayland(char* extension, usz len) => rgfw_extensionSupportedPlatform_EGL(extension, len);

fn RgfwProc rgfw_getProcAddress_EGL(char* procname)
{
	if (rgfw_eglGetProcAddress == null) return null;
	return rgfw_eglGetProcAddress(procname);
}

fn RgfwProc rgfw_getProcAddress_OpenGL_Wayland(char* procname) => rgfw_getProcAddress_EGL(procname);

fn bool rgfw_window_createContextPtr_EGL(RgfwWindow* win, RgfwEglContext* ctx, RgfwGlHints* hints)
{
	if (win == null || ctx == null || hints == null) return RGFW_FALSE;
	if (rgfw_loadEGL() == RGFW_FALSE) return RGFW_FALSE;
	if (_rgfw.egl_display == null)
	{
		_rgfw.egl_display = rgfw_eglGetDisplay((EglNativeDisplayType)_rgfw.wlDisplay);
		if (_rgfw.egl_display == null) return RGFW_FALSE;
		EglInt major = 0;
		EglInt minor = 0;
		if (rgfw_eglInitialize((EglDisplay)_rgfw.egl_display, &major, &minor) == 0) return RGFW_FALSE;
	}

	EglInt[32] cfg;
	RgfwAttribStack cfgStack;
	rgfw_attribStack_init(&cfgStack, (int*)&cfg, 32);
	rgfw_attribStack_pushAttribs(&cfgStack, EGL_SURFACE_TYPE, EGL_WINDOW_BIT);
	rgfw_attribStack_pushAttribs(&cfgStack, EGL_RED_SIZE, hints.red);
	rgfw_attribStack_pushAttribs(&cfgStack, EGL_GREEN_SIZE, hints.green);
	rgfw_attribStack_pushAttribs(&cfgStack, EGL_BLUE_SIZE, hints.blue);
	rgfw_attribStack_pushAttribs(&cfgStack, EGL_ALPHA_SIZE, hints.alpha);
	rgfw_attribStack_pushAttribs(&cfgStack, EGL_DEPTH_SIZE, hints.depth);
	rgfw_attribStack_pushAttribs(&cfgStack, EGL_STENCIL_SIZE, hints.stencil);
	rgfw_attribStack_pushAttribs(&cfgStack, EGL_RENDERABLE_TYPE, hints.profile == RGFW_GL_ES ? EGL_OPENGL_ES2_BIT : EGL_OPENGL_BIT);
	rgfw_attribStack_pushAttribs(&cfgStack, EGL_NONE, EGL_NONE);

	EglConfig config = null;
	EglInt num = 0;
	if (rgfw_eglChooseConfig((EglDisplay)_rgfw.egl_display, (EglInt*)&cfg, &config, 1, &num) == 0 || num == 0) return RGFW_FALSE;
	rgfw_eglBindApi(hints.profile == RGFW_GL_ES ? (uint)EGL_OPENGL_ES_API : (uint)EGL_OPENGL_API);

	EglInt[8] ctxAttribs;
	RgfwAttribStack ctxStack;
	rgfw_attribStack_init(&ctxStack, (int*)&ctxAttribs, 8);
	if (hints.profile == RGFW_GL_ES) rgfw_attribStack_pushAttribs(&ctxStack, EGL_CONTEXT_CLIENT_VERSION, hints.major != 0 ? hints.major : 2);
	rgfw_attribStack_pushAttribs(&ctxStack, EGL_NONE, EGL_NONE);

	EglContext share = hints.shareEGL != null ? (EglContext)hints.shareEGL.ctx : null;
	ctx.ctx = rgfw_eglCreateContext((EglDisplay)_rgfw.egl_display, config, share, (EglInt*)&ctxAttribs);
	if (ctx.ctx == null) return RGFW_FALSE;

	WlEglWindow* eglWindow = wlEglWindowCreate(win.src.surface, win.w, win.h);
	ctx.eglWindow = eglWindow;
	ctx.surface = rgfw_eglCreateWindowSurface((EglDisplay)_rgfw.egl_display, config, (EglNativeWindowType)eglWindow, null);
	if (ctx.surface == null)
	{
		rgfw_eglDestroyContext((EglDisplay)_rgfw.egl_display, (EglContext)ctx.ctx);
		ctx.ctx = null;
		return RGFW_FALSE;
	}
	win.src.ctx.egl = ctx;
	win.src.gfxType = RGFW_GFX_EGL;
	rgfw_window_makeCurrentContext_EGL(win);
	return RGFW_TRUE;
}

fn void rgfw_window_deleteContextPtr_EGL(RgfwWindow* win, RgfwEglContext* ctx)
{
	if (ctx == null || _rgfw.egl_display == null) return;
	if (ctx.surface != null) rgfw_eglDestroySurface((EglDisplay)_rgfw.egl_display, (EglSurface)ctx.surface);
	if (ctx.ctx != null) rgfw_eglDestroyContext((EglDisplay)_rgfw.egl_display, (EglContext)ctx.ctx);
	if (ctx.eglWindow != null) wlEglWindowDestroy((WlEglWindow*)ctx.eglWindow);
	ctx.surface = null;
	ctx.ctx = null;
	ctx.eglWindow = null;
	if (win != null) win.src.ctx.egl = null;
}

fn void rgfw_window_makeCurrentContext_EGL(RgfwWindow* win)
{
	if (_rgfw.egl_display == null || rgfw_eglMakeCurrent == null) return;
	if (win == null || win.src.ctx.egl == null) { rgfw_eglMakeCurrent((EglDisplay)_rgfw.egl_display, null, null, null); return; }
	RgfwEglContext* ctx = win.src.ctx.egl;
	rgfw_eglMakeCurrent((EglDisplay)_rgfw.egl_display, (EglSurface)ctx.surface, (EglSurface)ctx.surface, (EglContext)ctx.ctx);
}

fn void* rgfw_getCurrentContext_EGL() => rgfw_eglGetCurrentContext != null ? rgfw_eglGetCurrentContext() : null;

fn void rgfw_window_swapBuffers_EGL(RgfwWindow* win) { if (win != null && win.src.ctx.egl != null && rgfw_eglSwapBuffers != null) rgfw_eglSwapBuffers((EglDisplay)_rgfw.egl_display, (EglSurface)win.src.ctx.egl.surface); }

fn void rgfw_window_swapInterval_EGL(RgfwWindow* win, int swapInterval) { if (rgfw_eglSwapInterval != null) rgfw_eglSwapInterval((EglDisplay)_rgfw.egl_display, swapInterval); }

fn bool rgfw_window_createContextPtr_OpenGL_Wayland(RgfwWindow* win, RgfwGlContext* ctx, RgfwGlHints* hints)
{
	bool out = rgfw_window_createContextPtr_EGL(win, (RgfwEglContext*)ctx, hints);
	win.src.gfxType = RGFW_GFX_NATIVE_OPEN_GL;
	rgfw_window_swapInterval_OpenGL(win, 0);
	return out;
}

fn void rgfw_window_deleteContextPtr_OpenGL_Wayland(RgfwWindow* win, RgfwGlContext* ctx) { rgfw_window_deleteContextPtr_EGL(win, (RgfwEglContext*)ctx); win.src.ctx.native = null; }

fn void rgfw_window_makeCurrentContext_OpenGL_Wayland(RgfwWindow* win) { rgfw_window_makeCurrentContext_EGL(win); }

fn void* rgfw_getCurrentContext_OpenGL_Wayland() => rgfw_getCurrentContext_EGL();

fn void rgfw_window_swapBuffers_OpenGL_Wayland(RgfwWindow* win) { rgfw_window_swapBuffers_EGL(win); }

fn void rgfw_window_swapInterval_OpenGL_Wayland(RgfwWindow* win, int swapInterval) { rgfw_window_swapInterval_EGL(win, swapInterval); }

fn int rgfw_initPlatform_Wayland(char* className, RgfwInitFlags flags)
{
	_rgfw.wlDisplay = wlDisplayConnect(null);
	if (_rgfw.wlDisplay == null)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_WAYLAND, "Failed to load Wayland display");
		return -1;
	}

	_rgfw.compositor = null;
	_rgfw.registry = wlDisplayGetRegistry(_rgfw.wlDisplay);
	wlRegistryAddListener(_rgfw.registry, &rgfw_registryListener, _rgfw);
	wlDisplayRoundtrip(_rgfw.wlDisplay);

	if (_rgfw.compositor == null)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_WAYLAND, "Can't find compositor.");
		return -1;
	}

	if (_rgfw.wlCursorTheme == null)
	{
		_rgfw.wlCursorTheme = wlCursorThemeLoad(null, 24, _rgfw.shm);
		_rgfw.cursorSurface = wlCompositorCreateSurface(_rgfw.compositor);
	}

	char[4] blk = {0, 0, 0, 0};
	_rgfw.hiddenMouse = rgfw_createMouse((char*)&blk, 1, 1, RGFW_FORMAT_RGBA8);

	static XdgWmBaseListener wmBaseListener = { .ping = &rgfw_wlXdgWmBasePingHandler };
	xdgWmBaseAddListener(_rgfw.xdgWmBase, &wmBaseListener, null);

	_rgfw.xkbContext = xkbContextNew(XKB_CONTEXT_NO_FLAGS);

	static WlDataDeviceListener dataDeviceListener = {
		.dataOffer = &rgfw_wlDataDeviceDataOffer, .enter = &rgfw_wlDataDeviceNothingEnter, .leave = &rgfw_wlDataDeviceNothing,
		.motion = &rgfw_wlDataDeviceNothingMotion, .drop = &rgfw_wlDataDeviceNothing, .selection = &rgfw_wlDataDeviceSelection
	};

	if (_rgfw.seat != null && _rgfw.dataDeviceManager != null)
	{
		_rgfw.dataDevice = wlDataDeviceManagerGetDataDevice(_rgfw.dataDeviceManager, _rgfw.seat);
		wlDataDeviceAddListener(_rgfw.dataDevice, &dataDeviceListener, null);
	}

	return 0;
}

fn void rgfw_deinitPlatform_Wayland()
{
	if (_rgfw.unixClipboard != null) { rgfw_free_impl(_rgfw.unixClipboard); _rgfw.unixClipboard = null; }
	if (_rgfw.wlPointer != null) wlPointerDestroy(_rgfw.wlPointer);
	if (_rgfw.wlKeyboard != null) wlKeyboardDestroy(_rgfw.wlKeyboard);
	if (_rgfw.registry != null) wlRegistryDestroy(_rgfw.registry);
	if (_rgfw.decorationManager != null) zxdgDecorationManagerV1Destroy(_rgfw.decorationManager);
	if (_rgfw.relativePointerManager != null) zwpRelativePointerManagerV1Destroy(_rgfw.relativePointerManager);
	if (_rgfw.relativePointer != null) zwpRelativePointerV1Destroy(_rgfw.relativePointer);
	if (_rgfw.constraintManager != null) zwpPointerConstraintsV1Destroy(_rgfw.constraintManager);
	if (_rgfw.iconManager != null) xdgToplevelIconManagerV1Destroy(_rgfw.iconManager);
	if (_rgfw.xdgOutputManager != null) zxdgOutputManagerV1Destroy(_rgfw.xdgOutputManager);
	if (_rgfw.dataDeviceManager != null) wlDataDeviceManagerDestroy(_rgfw.dataDeviceManager);
	if (_rgfw.dataDevice != null) wlDataDeviceDestroy(_rgfw.dataDevice);
	if (_rgfw.wlCursorTheme != null) wlCursorThemeDestroy(_rgfw.wlCursorTheme);
	if (_rgfw.wpPointerWarp != null) wpPointerWarpV1Destroy(_rgfw.wpPointerWarp);
	if (_rgfw.hiddenMouse != null) rgfw_freeMouse(_rgfw.hiddenMouse);
	if (_rgfw.cursorSurface != null) wlSurfaceDestroy(_rgfw.cursorSurface);
	if (_rgfw.shm != null) wlShmDestroy(_rgfw.shm);
	if (_rgfw.seat != null) wlSeatRelease(_rgfw.seat);
	if (_rgfw.xdgWmBase != null) xdgWmBaseDestroy(_rgfw.xdgWmBase);
	if (_rgfw.compositor != null) wlCompositorDestroy(_rgfw.compositor);
	if (_rgfw.wlDisplay != null) wlDisplayDisconnect(_rgfw.wlDisplay);
}

fn void rgfw_initKeycodesPlatform_Wayland()
{
	// Wayland keycodes come directly from the evdev-based xkb keymap (key+8);
	// no separate keycode table walk is needed the way X11's is.
}

fn void rgfw_window_closePlatform_Wayland(RgfwWindow* win)
{
	rgfw_debugCallback(RGFW_TYPE_INFO, RGFW_INFO_WINDOW, "a window was freed");
	if (win.src.decoration != null) zxdgToplevelDecorationV1Destroy(win.src.decoration);
	if (win.src.xdgToplevel != null) xdgToplevelDestroy(win.src.xdgToplevel);
	wlSurfaceDestroy(win.src.customCursorSurface);
	if (win.src.lockedPointer != null) zwpLockedPointerV1Destroy(win.src.lockedPointer);
	if (win.src.icon != null) xdgToplevelIconV1Destroy(win.src.icon);
	xdgSurfaceDestroy(win.src.xdgSurface);
	wlSurfaceDestroy(win.src.surface);
}

module rgfw @link("wayland-client", "wayland-cursor", "wayland-egl", "xkbcommon", "EGL");// ==================== RGFW_DYNAMIC vtable dispatch layer ====================
// Combined X11+Wayland runtime dispatch: mirrors the original's RGFW_FunctionPointers
// / RGFW_load_X11() / RGFW_load_Wayland() / RGFW_usingWayland() design.

alias RgfwWindowSetAspectRatioFn = fn void(RgfwWindow*, int, int);
alias RgfwNativeFormatFn = fn RgfwFormat();
alias RgfwCreateSurfacePtrFn = fn bool(char*, int, int, RgfwFormat, RgfwSurface*);
alias RgfwWindowBlitSurfaceFn = fn void(RgfwWindow*, RgfwSurface*);
alias RgfwSurfaceFreePtrFn = fn void(RgfwSurface*);
alias RgfwWindowSetBorderFn = fn void(RgfwWindow*, bool);
alias RgfwWindowSetRawMouseModePlatformFn = fn void(RgfwWindow*, bool);
alias RgfwWindowCaptureMousePlatformFn = fn void(RgfwWindow*, bool);
alias RgfwCreateWindowPlatformFn = fn RgfwWindow*(char*, RgfwWindowFlags, RgfwWindow*);
alias RgfwGetGlobalMouseFn = fn bool(int*, int*);
alias RgfwPhysicalToMappedKeyFn = fn RgfwKey(RgfwKey);
alias RgfwWindowFetchSizeFn = fn bool(RgfwWindow*, int*, int*);
alias RgfwPollEventsFn = fn void();
alias RgfwWindowMovePlatformFn = fn void(RgfwWindow*, int, int);
alias RgfwWindowResizePlatformFn = fn void(RgfwWindow*, int, int);
alias RgfwWindowSetMinSizeFn = fn void(RgfwWindow*, int, int);
alias RgfwWindowSetMaxSizeFn = fn void(RgfwWindow*, int, int);
alias RgfwWindowMaximizePlatformFn = fn void(RgfwWindow*);
alias RgfwWindowFocusFn = fn void(RgfwWindow*);
alias RgfwWindowRaiseFn = fn void(RgfwWindow*);
alias RgfwWindowSetFullscreenFn = fn void(RgfwWindow*, bool);
alias RgfwWindowSetFloatingFn = fn void(RgfwWindow*, bool);
alias RgfwWindowSetOpacityFn = fn void(RgfwWindow*, char);
alias RgfwWindowMinimizePlatformFn = fn void(RgfwWindow*);
alias RgfwWindowRestorePlatformFn = fn void(RgfwWindow*);
alias RgfwWindowIsFloatingFn = fn bool(RgfwWindow*);
alias RgfwWindowSetNameFn = fn void(RgfwWindow*, char*);
alias RgfwWindowSetMousePassthroughFn = fn void(RgfwWindow*, bool);
alias RgfwWindowSetIconExFn = fn bool(RgfwWindow*, char*, int, int, RgfwFormat, RgfwIcon);
alias RgfwCreateMouseStandardFn = fn RgfwMouse*(RgfwMouseIcon);
alias RgfwCreateMouseFn = fn RgfwMouse*(char*, int, int, RgfwFormat);
alias RgfwWindowSetMousePlatformFn = fn bool(RgfwWindow*, RgfwMouse*);
alias RgfwFreeMouseFn = fn void(RgfwMouse*);
alias RgfwWindowMoveMouseFn = fn void(RgfwWindow*, int, int);
alias RgfwWindowHideFn = fn void(RgfwWindow*);
alias RgfwWindowShowPlatformFn = fn void(RgfwWindow*);
alias RgfwWindowFlashFn = fn void(RgfwWindow*, RgfwFlashRequest);
alias RgfwReadClipboardPtrFn = fn bool(RgfwDataTransferType, char*, usz, RgfwDataTransfer*);
alias RgfwWriteClipboardFn = fn bool(RgfwDataTransfer*);
alias RgfwWindowIsHiddenFn = fn bool(RgfwWindow*);
alias RgfwWindowIsMinimizedFn = fn bool(RgfwWindow*);
alias RgfwWindowIsMaximizedFn = fn bool(RgfwWindow*);
alias RgfwPollMonitorsFn = fn void();
alias RgfwMonitorNodeFreeFn = fn void(RgfwMonitorNode*);
alias RgfwMonitorGetWorkareaFn = fn bool(RgfwMonitor*, int*, int*, int*, int*);
alias RgfwMonitorGetModesPtrFn = fn usz(RgfwMonitor*, RgfwMonitorMode**);
alias RgfwMonitorGetGammaRampPtrFn = fn usz(RgfwMonitor*, RgfwGammaRamp*);
alias RgfwMonitorSetGammaRampFn = fn bool(RgfwMonitor*, RgfwGammaRamp*);
alias RgfwMonitorSetModeFn = fn bool(RgfwMonitor*, RgfwMonitorMode*);
alias RgfwMonitorRequestModeFn = fn bool(RgfwMonitor*, RgfwMonitorMode*, RgfwModeRequest);
alias RgfwWindowGetMonitorFn = fn RgfwMonitor*(RgfwWindow*);
alias RgfwExtensionSupportedPlatformOpenGLFn = fn bool(char*, usz);
alias RgfwGetProcAddressOpenGLFn = fn RgfwProc(char*);
alias RgfwWindowCreateContextPtrOpenGLFn = fn bool(RgfwWindow*, RgfwGlContext*, RgfwGlHints*);
alias RgfwWindowDeleteContextPtrOpenGLFn = fn void(RgfwWindow*, RgfwGlContext*);
alias RgfwWindowMakeCurrentContextOpenGLFn = fn void(RgfwWindow*);
alias RgfwGetCurrentContextOpenGLFn = fn void*();
alias RgfwWindowSwapBuffersOpenGLFn = fn void(RgfwWindow*);
alias RgfwWindowSwapIntervalOpenGLFn = fn void(RgfwWindow*, int);
alias RgfwWindowClosePlatformFn = fn void(RgfwWindow*);
alias RgfwWindowCreateSurfaceWebGPUFn = fn WGPUSurface(RgfwWindow*, WGPUInstance);

struct RgfwFunctionPointers
{
	RgfwWindowSetAspectRatioFn window_setAspectRatio;
	RgfwNativeFormatFn nativeFormat;
	RgfwCreateSurfacePtrFn createSurfacePtr;
	RgfwWindowBlitSurfaceFn window_blitSurface;
	RgfwSurfaceFreePtrFn surface_freePtr;
	RgfwWindowSetBorderFn window_setBorder;
	RgfwWindowSetRawMouseModePlatformFn window_setRawMouseModePlatform;
	RgfwWindowCaptureMousePlatformFn window_captureMousePlatform;
	RgfwCreateWindowPlatformFn createWindowPlatform;
	RgfwGetGlobalMouseFn getGlobalMouse;
	RgfwPhysicalToMappedKeyFn physicalToMappedKey;
	RgfwWindowFetchSizeFn window_fetchSize;
	RgfwPollEventsFn pollEvents;
	RgfwWindowMovePlatformFn window_movePlatform;
	RgfwWindowResizePlatformFn window_resizePlatform;
	RgfwWindowSetMinSizeFn window_setMinSize;
	RgfwWindowSetMaxSizeFn window_setMaxSize;
	RgfwWindowMaximizePlatformFn window_maximizePlatform;
	RgfwWindowFocusFn window_focus;
	RgfwWindowRaiseFn window_raise;
	RgfwWindowSetFullscreenFn window_setFullscreen;
	RgfwWindowSetFloatingFn window_setFloating;
	RgfwWindowSetOpacityFn window_setOpacity;
	RgfwWindowMinimizePlatformFn window_minimizePlatform;
	RgfwWindowRestorePlatformFn window_restorePlatform;
	RgfwWindowIsFloatingFn window_isFloating;
	RgfwWindowSetNameFn window_setName;
	RgfwWindowSetMousePassthroughFn window_setMousePassthrough;
	RgfwWindowSetIconExFn window_setIconEx;
	RgfwCreateMouseStandardFn createMouseStandard;
	RgfwCreateMouseFn createMouse;
	RgfwWindowSetMousePlatformFn window_setMousePlatform;
	RgfwFreeMouseFn freeMouse;
	RgfwWindowMoveMouseFn window_moveMouse;
	RgfwWindowHideFn window_hide;
	RgfwWindowShowPlatformFn window_showPlatform;
	RgfwWindowFlashFn window_flash;
	RgfwReadClipboardPtrFn readClipboardPtr;
	RgfwWriteClipboardFn writeClipboard;
	RgfwWindowIsHiddenFn window_isHidden;
	RgfwWindowIsMinimizedFn window_isMinimized;
	RgfwWindowIsMaximizedFn window_isMaximized;
	RgfwPollMonitorsFn pollMonitors;
	RgfwMonitorNodeFreeFn monitorNode_free;
	RgfwMonitorGetWorkareaFn monitor_getWorkarea;
	RgfwMonitorGetModesPtrFn monitor_getModesPtr;
	RgfwMonitorGetGammaRampPtrFn monitor_getGammaRampPtr;
	RgfwMonitorSetGammaRampFn monitor_setGammaRamp;
	RgfwMonitorSetModeFn monitor_setMode;
	RgfwMonitorRequestModeFn monitor_requestMode;
	RgfwWindowGetMonitorFn window_getMonitor;
	RgfwExtensionSupportedPlatformOpenGLFn extensionSupportedPlatform_OpenGL;
	RgfwGetProcAddressOpenGLFn getProcAddress_OpenGL;
	RgfwWindowCreateContextPtrOpenGLFn window_createContextPtr_OpenGL;
	RgfwWindowDeleteContextPtrOpenGLFn window_deleteContextPtr_OpenGL;
	RgfwWindowMakeCurrentContextOpenGLFn window_makeCurrentContext_OpenGL;
	RgfwGetCurrentContextOpenGLFn getCurrentContext_OpenGL;
	RgfwWindowSwapBuffersOpenGLFn window_swapBuffers_OpenGL;
	RgfwWindowSwapIntervalOpenGLFn window_swapInterval_OpenGL;
	RgfwWindowClosePlatformFn window_closePlatform;
	RgfwWindowCreateSurfaceWebGPUFn window_createSurface_WebGPU;
}

RgfwFunctionPointers rgfw_api;
bool rgfw_dynamicUsingWayland = RGFW_FALSE;

fn bool rgfw_usingWayland() => rgfw_dynamicUsingWayland;

fn void rgfw_window_setAspectRatio(RgfwWindow* win, int w, int h) => rgfw_api.window_setAspectRatio(win, w, h);
fn RgfwFormat rgfw_nativeFormat() => rgfw_api.nativeFormat();
fn bool rgfw_createSurfacePtr(char* data, int w, int h, RgfwFormat format, RgfwSurface* surface) => rgfw_api.createSurfacePtr(data, w, h, format, surface);
fn void rgfw_window_blitSurface(RgfwWindow* win, RgfwSurface* surface) => rgfw_api.window_blitSurface(win, surface);
fn void rgfw_surface_freePtr(RgfwSurface* surface) => rgfw_api.surface_freePtr(surface);
fn void rgfw_window_setBorder(RgfwWindow* win, bool border) => rgfw_api.window_setBorder(win, border);
fn void rgfw_window_setRawMouseModePlatform(RgfwWindow* win, bool state) => rgfw_api.window_setRawMouseModePlatform(win, state);
fn void rgfw_window_captureMousePlatform(RgfwWindow* win, bool state) => rgfw_api.window_captureMousePlatform(win, state);
fn RgfwWindow* rgfw_createWindowPlatform(char* name, RgfwWindowFlags flags, RgfwWindow* win) => rgfw_api.createWindowPlatform(name, flags, win);
fn bool rgfw_getGlobalMouse(int* fX, int* fY) => rgfw_api.getGlobalMouse(fX, fY);
fn RgfwKey rgfw_physicalToMappedKey(RgfwKey key) => rgfw_api.physicalToMappedKey(key);
fn bool rgfw_window_fetchSize(RgfwWindow* win, int* w, int* h) => rgfw_api.window_fetchSize(win, w, h);
fn void rgfw_pollEvents() => rgfw_api.pollEvents();
fn void rgfw_window_movePlatform(RgfwWindow* win, int x, int y) => rgfw_api.window_movePlatform(win, x, y);
fn void rgfw_window_resizePlatform(RgfwWindow* win, int w, int h) => rgfw_api.window_resizePlatform(win, w, h);
fn void rgfw_window_setMinSize(RgfwWindow* win, int w, int h) => rgfw_api.window_setMinSize(win, w, h);
fn void rgfw_window_setMaxSize(RgfwWindow* win, int w, int h) => rgfw_api.window_setMaxSize(win, w, h);
fn void rgfw_window_maximizePlatform(RgfwWindow* win) => rgfw_api.window_maximizePlatform(win);
fn void rgfw_window_focus(RgfwWindow* win) => rgfw_api.window_focus(win);
fn void rgfw_window_raise(RgfwWindow* win) => rgfw_api.window_raise(win);
fn void rgfw_window_setFullscreen(RgfwWindow* win, bool fullscreen) => rgfw_api.window_setFullscreen(win, fullscreen);
fn void rgfw_window_setFloating(RgfwWindow* win, bool floating) => rgfw_api.window_setFloating(win, floating);
fn void rgfw_window_setOpacity(RgfwWindow* win, char opacity) => rgfw_api.window_setOpacity(win, opacity);
fn void rgfw_window_minimizePlatform(RgfwWindow* win) => rgfw_api.window_minimizePlatform(win);
fn void rgfw_window_restorePlatform(RgfwWindow* win) => rgfw_api.window_restorePlatform(win);
fn bool rgfw_window_isFloating(RgfwWindow* win) => rgfw_api.window_isFloating(win);
fn void rgfw_window_setName(RgfwWindow* win, char* name) => rgfw_api.window_setName(win, name);
fn void rgfw_window_setMousePassthrough(RgfwWindow* win, bool passthrough) => rgfw_api.window_setMousePassthrough(win, passthrough);
fn bool rgfw_window_setIconEx(RgfwWindow* win, char* dataSrc, int w, int h, RgfwFormat format, RgfwIcon type) => rgfw_api.window_setIconEx(win, dataSrc, w, h, format, type);
fn RgfwMouse* rgfw_createMouseStandard(RgfwMouseIcon mouse) => rgfw_api.createMouseStandard(mouse);
fn RgfwMouse* rgfw_createMouse(char* data, int w, int h, RgfwFormat format) => rgfw_api.createMouse(data, w, h, format);
fn bool rgfw_window_setMousePlatform(RgfwWindow* win, RgfwMouse* mouse) => rgfw_api.window_setMousePlatform(win, mouse);
fn void rgfw_freeMouse(RgfwMouse* mouse) => rgfw_api.freeMouse(mouse);
fn void rgfw_window_moveMouse(RgfwWindow* win, int x, int y) => rgfw_api.window_moveMouse(win, x, y);
fn void rgfw_window_hide(RgfwWindow* win) => rgfw_api.window_hide(win);
fn void rgfw_window_showPlatform(RgfwWindow* win) => rgfw_api.window_showPlatform(win);
fn void rgfw_window_flash(RgfwWindow* win, RgfwFlashRequest request) => rgfw_api.window_flash(win, request);
fn bool rgfw_readClipboardPtr(RgfwDataTransferType requestedType, char* buffer, usz capacity, RgfwDataTransfer* dataTransfer) => rgfw_api.readClipboardPtr(requestedType, buffer, capacity, dataTransfer);
fn bool rgfw_writeClipboard(RgfwDataTransfer* data) => rgfw_api.writeClipboard(data);
fn bool rgfw_window_isHidden(RgfwWindow* win) => rgfw_api.window_isHidden(win);
fn bool rgfw_window_isMinimized(RgfwWindow* win) => rgfw_api.window_isMinimized(win);
fn bool rgfw_window_isMaximized(RgfwWindow* win) => rgfw_api.window_isMaximized(win);
fn void rgfw_pollMonitors() => rgfw_api.pollMonitors();
fn void rgfw_monitorNode_free(RgfwMonitorNode* node) => rgfw_api.monitorNode_free(node);
fn bool rgfw_monitor_getWorkarea(RgfwMonitor* monitor, int* x, int* y, int* width, int* height) => rgfw_api.monitor_getWorkarea(monitor, x, y, width, height);
fn usz rgfw_monitor_getModesPtr(RgfwMonitor* monitor, RgfwMonitorMode** modes) => rgfw_api.monitor_getModesPtr(monitor, modes);
fn usz rgfw_monitor_getGammaRampPtr(RgfwMonitor* monitor, RgfwGammaRamp* ramp) => rgfw_api.monitor_getGammaRampPtr(monitor, ramp);
fn bool rgfw_monitor_setGammaRamp(RgfwMonitor* monitor, RgfwGammaRamp* ramp) => rgfw_api.monitor_setGammaRamp(monitor, ramp);
fn bool rgfw_monitor_setMode(RgfwMonitor* mon, RgfwMonitorMode* mode) => rgfw_api.monitor_setMode(mon, mode);
fn bool rgfw_monitor_requestMode(RgfwMonitor* mon, RgfwMonitorMode* mode, RgfwModeRequest request) => rgfw_api.monitor_requestMode(mon, mode, request);
fn RgfwMonitor* rgfw_window_getMonitor(RgfwWindow* win) => rgfw_api.window_getMonitor(win);
fn bool rgfw_extensionSupportedPlatform_OpenGL(char* extension, usz len) => rgfw_api.extensionSupportedPlatform_OpenGL(extension, len);
fn RgfwProc rgfw_getProcAddress_OpenGL(char* procname) => rgfw_api.getProcAddress_OpenGL(procname);
fn bool rgfw_window_createContextPtr_OpenGL(RgfwWindow* win, RgfwGlContext* context, RgfwGlHints* hints) => rgfw_api.window_createContextPtr_OpenGL(win, context, hints);
fn void rgfw_window_deleteContextPtr_OpenGL(RgfwWindow* win, RgfwGlContext* ctx) => rgfw_api.window_deleteContextPtr_OpenGL(win, ctx);
fn void rgfw_window_makeCurrentContext_OpenGL(RgfwWindow* win) => rgfw_api.window_makeCurrentContext_OpenGL(win);
fn void* rgfw_getCurrentContext_OpenGL() => rgfw_api.getCurrentContext_OpenGL();
fn void rgfw_window_swapBuffers_OpenGL(RgfwWindow* win) => rgfw_api.window_swapBuffers_OpenGL(win);
fn void rgfw_window_swapInterval_OpenGL(RgfwWindow* win, int swapInterval) => rgfw_api.window_swapInterval_OpenGL(win, swapInterval);
fn void rgfw_window_closePlatform(RgfwWindow* win) => rgfw_api.window_closePlatform(win);
fn WGPUSurface rgfw_window_createSurface_WebGPU(RgfwWindow* window, WGPUInstance instance) => rgfw_api.window_createSurface_WebGPU(window, instance);

fn void rgfw_loadX11()
{
	rgfw_api.window_setAspectRatio = &rgfw_window_setAspectRatio_X11;
	rgfw_api.nativeFormat = &rgfw_nativeFormat_X11;
	rgfw_api.createSurfacePtr = &rgfw_createSurfacePtr_X11;
	rgfw_api.window_blitSurface = &rgfw_window_blitSurface_X11;
	rgfw_api.surface_freePtr = &rgfw_surface_freePtr_X11;
	rgfw_api.window_setBorder = &rgfw_window_setBorder_X11;
	rgfw_api.window_setRawMouseModePlatform = &rgfw_window_setRawMouseModePlatform_X11;
	rgfw_api.window_captureMousePlatform = &rgfw_window_captureMousePlatform_X11;
	rgfw_api.createWindowPlatform = &rgfw_createWindowPlatform_X11;
	rgfw_api.getGlobalMouse = &rgfw_getGlobalMouse_X11;
	rgfw_api.physicalToMappedKey = &rgfw_physicalToMappedKey_X11;
	rgfw_api.window_fetchSize = &rgfw_window_fetchSize_X11;
	rgfw_api.pollEvents = &rgfw_pollEvents_X11;
	rgfw_api.window_movePlatform = &rgfw_window_movePlatform_X11;
	rgfw_api.window_resizePlatform = &rgfw_window_resizePlatform_X11;
	rgfw_api.window_setMinSize = &rgfw_window_setMinSize_X11;
	rgfw_api.window_setMaxSize = &rgfw_window_setMaxSize_X11;
	rgfw_api.window_maximizePlatform = &rgfw_window_maximizePlatform_X11;
	rgfw_api.window_focus = &rgfw_window_focus_X11;
	rgfw_api.window_raise = &rgfw_window_raise_X11;
	rgfw_api.window_setFullscreen = &rgfw_window_setFullscreen_X11;
	rgfw_api.window_setFloating = &rgfw_window_setFloating_X11;
	rgfw_api.window_setOpacity = &rgfw_window_setOpacity_X11;
	rgfw_api.window_minimizePlatform = &rgfw_window_minimizePlatform_X11;
	rgfw_api.window_restorePlatform = &rgfw_window_restorePlatform_X11;
	rgfw_api.window_isFloating = &rgfw_window_isFloating_X11;
	rgfw_api.window_setName = &rgfw_window_setName_X11;
	rgfw_api.window_setMousePassthrough = &rgfw_window_setMousePassthrough_X11;
	rgfw_api.window_setIconEx = &rgfw_window_setIconEx_X11;
	rgfw_api.createMouseStandard = &rgfw_createMouseStandard_X11;
	rgfw_api.createMouse = &rgfw_createMouse_X11;
	rgfw_api.window_setMousePlatform = &rgfw_window_setMousePlatform_X11;
	rgfw_api.freeMouse = &rgfw_freeMouse_X11;
	rgfw_api.window_moveMouse = &rgfw_window_moveMouse_X11;
	rgfw_api.window_hide = &rgfw_window_hide_X11;
	rgfw_api.window_showPlatform = &rgfw_window_showPlatform_X11;
	rgfw_api.window_flash = &rgfw_window_flash_X11;
	rgfw_api.readClipboardPtr = &rgfw_readClipboardPtr_X11;
	rgfw_api.writeClipboard = &rgfw_writeClipboard_X11;
	rgfw_api.window_isHidden = &rgfw_window_isHidden_X11;
	rgfw_api.window_isMinimized = &rgfw_window_isMinimized_X11;
	rgfw_api.window_isMaximized = &rgfw_window_isMaximized_X11;
	rgfw_api.pollMonitors = &rgfw_pollMonitors_X11;
	rgfw_api.monitorNode_free = &rgfw_monitorNode_free_X11;
	rgfw_api.monitor_getWorkarea = &rgfw_monitor_getWorkarea_X11;
	rgfw_api.monitor_getModesPtr = &rgfw_monitor_getModesPtr_X11;
	rgfw_api.monitor_getGammaRampPtr = &rgfw_monitor_getGammaRampPtr_X11;
	rgfw_api.monitor_setGammaRamp = &rgfw_monitor_setGammaRamp_X11;
	rgfw_api.monitor_setMode = &rgfw_monitor_setMode_X11;
	rgfw_api.monitor_requestMode = &rgfw_monitor_requestMode_X11;
	rgfw_api.window_getMonitor = &rgfw_window_getMonitor_X11;
	rgfw_api.extensionSupportedPlatform_OpenGL = &rgfw_extensionSupportedPlatform_OpenGL_X11;
	rgfw_api.getProcAddress_OpenGL = &rgfw_getProcAddress_OpenGL_X11;
	rgfw_api.window_createContextPtr_OpenGL = &rgfw_window_createContextPtr_OpenGL_X11;
	rgfw_api.window_deleteContextPtr_OpenGL = &rgfw_window_deleteContextPtr_OpenGL_X11;
	rgfw_api.window_makeCurrentContext_OpenGL = &rgfw_window_makeCurrentContext_OpenGL_X11;
	rgfw_api.getCurrentContext_OpenGL = &rgfw_getCurrentContext_OpenGL_X11;
	rgfw_api.window_swapBuffers_OpenGL = &rgfw_window_swapBuffers_OpenGL_X11;
	rgfw_api.window_swapInterval_OpenGL = &rgfw_window_swapInterval_OpenGL_X11;
	rgfw_api.window_closePlatform = &rgfw_window_closePlatform_X11;
	rgfw_api.window_createSurface_WebGPU = &rgfw_window_createSurface_WebGPU_X11;
}

fn void rgfw_loadWayland()
{
	rgfw_api.window_setAspectRatio = &rgfw_window_setAspectRatio_Wayland;
	rgfw_api.nativeFormat = &rgfw_nativeFormat_Wayland;
	rgfw_api.createSurfacePtr = &rgfw_createSurfacePtr_Wayland;
	rgfw_api.window_blitSurface = &rgfw_window_blitSurface_Wayland;
	rgfw_api.surface_freePtr = &rgfw_surface_freePtr_Wayland;
	rgfw_api.window_setBorder = &rgfw_window_setBorder_Wayland;
	rgfw_api.window_setRawMouseModePlatform = &rgfw_window_setRawMouseModePlatform_Wayland;
	rgfw_api.window_captureMousePlatform = &rgfw_window_captureMousePlatform_Wayland;
	rgfw_api.createWindowPlatform = &rgfw_createWindowPlatform_Wayland;
	rgfw_api.getGlobalMouse = &rgfw_getGlobalMouse_Wayland;
	rgfw_api.physicalToMappedKey = &rgfw_physicalToMappedKey_Wayland;
	rgfw_api.window_fetchSize = &rgfw_window_fetchSize_Wayland;
	rgfw_api.pollEvents = &rgfw_pollEvents_Wayland;
	rgfw_api.window_movePlatform = &rgfw_window_movePlatform_Wayland;
	rgfw_api.window_resizePlatform = &rgfw_window_resizePlatform_Wayland;
	rgfw_api.window_setMinSize = &rgfw_window_setMinSize_Wayland;
	rgfw_api.window_setMaxSize = &rgfw_window_setMaxSize_Wayland;
	rgfw_api.window_maximizePlatform = &rgfw_window_maximizePlatform_Wayland;
	rgfw_api.window_focus = &rgfw_window_focus_Wayland;
	rgfw_api.window_raise = &rgfw_window_raise_Wayland;
	rgfw_api.window_setFullscreen = &rgfw_window_setFullscreen_Wayland;
	rgfw_api.window_setFloating = &rgfw_window_setFloating_Wayland;
	rgfw_api.window_setOpacity = &rgfw_window_setOpacity_Wayland;
	rgfw_api.window_minimizePlatform = &rgfw_window_minimizePlatform_Wayland;
	rgfw_api.window_restorePlatform = &rgfw_window_restorePlatform_Wayland;
	rgfw_api.window_isFloating = &rgfw_window_isFloating_Wayland;
	rgfw_api.window_setName = &rgfw_window_setName_Wayland;
	rgfw_api.window_setMousePassthrough = &rgfw_window_setMousePassthrough_Wayland;
	rgfw_api.window_setIconEx = &rgfw_window_setIconEx_Wayland;
	rgfw_api.createMouseStandard = &rgfw_createMouseStandard_Wayland;
	rgfw_api.createMouse = &rgfw_createMouse_Wayland;
	rgfw_api.window_setMousePlatform = &rgfw_window_setMousePlatform_Wayland;
	rgfw_api.freeMouse = &rgfw_freeMouse_Wayland;
	rgfw_api.window_moveMouse = &rgfw_window_moveMouse_Wayland;
	rgfw_api.window_hide = &rgfw_window_hide_Wayland;
	rgfw_api.window_showPlatform = &rgfw_window_showPlatform_Wayland;
	rgfw_api.window_flash = &rgfw_window_flash_Wayland;
	rgfw_api.readClipboardPtr = &rgfw_readClipboardPtr_Wayland;
	rgfw_api.writeClipboard = &rgfw_writeClipboard_Wayland;
	rgfw_api.window_isHidden = &rgfw_window_isHidden_Wayland;
	rgfw_api.window_isMinimized = &rgfw_window_isMinimized_Wayland;
	rgfw_api.window_isMaximized = &rgfw_window_isMaximized_Wayland;
	rgfw_api.pollMonitors = &rgfw_pollMonitors_Wayland;
	rgfw_api.monitorNode_free = &rgfw_monitorNode_free_Wayland;
	rgfw_api.monitor_getWorkarea = &rgfw_monitor_getWorkarea_Wayland;
	rgfw_api.monitor_getModesPtr = &rgfw_monitor_getModesPtr_Wayland;
	rgfw_api.monitor_getGammaRampPtr = &rgfw_monitor_getGammaRampPtr_Wayland;
	rgfw_api.monitor_setGammaRamp = &rgfw_monitor_setGammaRamp_Wayland;
	rgfw_api.monitor_setMode = &rgfw_monitor_setMode_Wayland;
	rgfw_api.monitor_requestMode = &rgfw_monitor_requestMode_Wayland;
	rgfw_api.window_getMonitor = &rgfw_window_getMonitor_Wayland;
	rgfw_api.extensionSupportedPlatform_OpenGL = &rgfw_extensionSupportedPlatform_OpenGL_Wayland;
	rgfw_api.getProcAddress_OpenGL = &rgfw_getProcAddress_OpenGL_Wayland;
	rgfw_api.window_createContextPtr_OpenGL = &rgfw_window_createContextPtr_OpenGL_Wayland;
	rgfw_api.window_deleteContextPtr_OpenGL = &rgfw_window_deleteContextPtr_OpenGL_Wayland;
	rgfw_api.window_makeCurrentContext_OpenGL = &rgfw_window_makeCurrentContext_OpenGL_Wayland;
	rgfw_api.getCurrentContext_OpenGL = &rgfw_getCurrentContext_OpenGL_Wayland;
	rgfw_api.window_swapBuffers_OpenGL = &rgfw_window_swapBuffers_OpenGL_Wayland;
	rgfw_api.window_swapInterval_OpenGL = &rgfw_window_swapInterval_OpenGL_Wayland;
	rgfw_api.window_closePlatform = &rgfw_window_closePlatform_Wayland;
	rgfw_api.window_createSurface_WebGPU = &rgfw_window_createSurface_WebGPU_Wayland;
}

fn int rgfw_initPlatform(char* className, RgfwInitFlags flags)
{
	if (!(flags & RGFW_INIT_X11))
	{
		char* waylandDisplay = getenv("WAYLAND_DISPLAY");
		rgfw_dynamicUsingWayland = rgfw_bool(waylandDisplay != null && waylandDisplay[0] != 0);
		if (rgfw_dynamicUsingWayland)
		{
			rgfw_loadWayland();
			int ret = rgfw_initPlatform_Wayland(className, flags);
			if (ret == 0) return 0;

			rgfw_debugCallback(RGFW_TYPE_WARNING, RGFW_WARNING_WAYLAND, "Falling back to X11");
			rgfw_dynamicUsingWayland = RGFW_FALSE;
		}
	}
	else
	{
		rgfw_dynamicUsingWayland = RGFW_FALSE;
	}
	rgfw_loadX11();
	return rgfw_initPlatform_X11(className, flags);
}

fn void rgfw_deinitPlatform()
{
	if (_rgfw.eventWait_forceStop[0] || _rgfw.eventWait_forceStop[1])
	{
		close(_rgfw.eventWait_forceStop[0]);
		close(_rgfw.eventWait_forceStop[1]);
	}

	if (rgfw_usingWayland()) { rgfw_deinitPlatform_Wayland(); return; }
	rgfw_deinitPlatform_X11();
}

fn void rgfw_initKeycodesPlatform()
{
	if (rgfw_usingWayland()) { rgfw_initKeycodesPlatform_Wayland(); return; }
	rgfw_initKeycodesPlatform_X11();
}

fn bool rgfw_loadGL()
{
	if (rgfw_usingWayland()) return rgfw_loadGL_Wayland();
	return rgfw_loadGL_X11();
}

fn void rgfw_unloadGL()
{
	if (rgfw_usingWayland()) { rgfw_unloadGL_Wayland(); return; }
	rgfw_unloadGL_X11();
}

fn void rgfw_stopCheckEvents()
{
	_rgfw.eventWait_forceStop[2] = 1;
	while (true)
	{
		char byte = 0;
		long result = write(_rgfw.eventWait_forceStop[1], &byte, 1);
		if (result == 1 || result == -1) break;
	}
}

fn void rgfw_waitForEvent(int waitMS)
{
	if (waitMS == 0) return;

	if (_rgfw.eventWait_forceStop[0] == 0 || _rgfw.eventWait_forceStop[1] == 0)
	{
		if (pipe((int[2]*)&_rgfw.eventWait_forceStop) != -1)
		{
			fcntl(_rgfw.eventWait_forceStop[0], F_GETFL, 0);
			fcntl(_rgfw.eventWait_forceStop[0], F_GETFD, 0);
			fcntl(_rgfw.eventWait_forceStop[1], F_GETFL, 0);
			fcntl(_rgfw.eventWait_forceStop[1], F_GETFD, 0);
		}
	}

	Pollfd[2] fds;
	rgfw_memzero(&fds, Pollfd::size * 2);
	fds[0].fd = 0;
	fds[0].events = (short)POLLIN;
	fds[1].fd = _rgfw.eventWait_forceStop[0];
	fds[1].events = (short)POLLIN;

	if (rgfw_usingWayland())
	{
		fds[0].fd = wlDisplayGetFd(_rgfw.wlDisplay);

		// empty the queue
		while (wlDisplayPrepareRead(_rgfw.wlDisplay) != 0)
		{
			// error occured when dispatching the queue
			if (wlDisplayDispatchPending(_rgfw.wlDisplay) == -1) return;
		}

		// send any pending requests to the compositor
		while (wlDisplayFlush(_rgfw.wlDisplay) == -1)
		{
			// queue is full dispatch them
			if (*rgfwErrnoLocation() == RGFW_EAGAIN)
			{
				if (wlDisplayDispatchPending(_rgfw.wlDisplay) == -1) return;
			}
			else { return; }
		}
	}
	else
	{
		fds[0].fd = xConnectionNumber(_rgfw.display);
	}

	ulong start = rgfw_unix_getTimeNS();
	if (rgfw_usingWayland())
	{
		while (wlDisplayDispatchPending(_rgfw.wlDisplay) == 0)
		{
			if (poll(&fds[0], 1, waitMS) <= 0)
			{
				wlDisplayCancelRead(_rgfw.wlDisplay);
				break;
			}
			else
			{
				if (wlDisplayReadEvents(_rgfw.wlDisplay) == -1) return;
			}

			if (waitMS != RGFW_EVENT_WAIT_NEXT)
			{
				waitMS -= (int)((rgfw_unix_getTimeNS() - start) / 1000000);
			}
		}

		// queue contains events from read, dispatch them
		if (wlDisplayDispatchPending(_rgfw.wlDisplay) == -1) return;
	}
	else
	{
		while (xPending(_rgfw.display) == 0)
		{
			if (poll(&fds[0], 1, waitMS) <= 0) break;

			if (waitMS != RGFW_EVENT_WAIT_NEXT)
			{
				waitMS -= (int)((rgfw_unix_getTimeNS() - start) / 1000000);
			}
		}
	}

	// drain any data in the stop request
	if (_rgfw.eventWait_forceStop[2])
	{
		char[64] data;
		rgfw_memzero(&data, 64);
		read(_rgfw.eventWait_forceStop[0], &data, 64);

		_rgfw.eventWait_forceStop[2] = 0;
	}
}
