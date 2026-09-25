module std::gfx::x11 @private @feat(LINUX, BSD) @feat(!NO_X11) @link("X11", "Xext", "Xrandr", "GL", "xkbcommon");
import libc;

alias XcursorImageCreateFn = fn XcursorImage*(int, int);
alias XcursorImageDestroyFn = fn void(XcursorImage*);
alias XcursorImageLoadCursorFn = fn XCursor(XDisplay*, XcursorImage*);
alias XISelectEventsFn = fn int(XDisplay*, XWindow, XIEventMask*, int);
alias XSyncIntToValueFn = fn void(XSyncValue*, int);
alias XSyncSetCounterFn = fn int(XDisplay*, XSyncCounter, XSyncValue);
alias XSyncCreateCounterFn = fn XSyncCounter(XDisplay*, XSyncValue);
alias XSyncDestroyCounterFn = fn int(XDisplay*, XSyncCounter);
alias XSyncInitializeFn = fn int(XDisplay*, int*, int*);
alias XShapeCombineMaskFn = fn void(XDisplay*, XWindow, int, int, int, Pixmap, int);
alias XShapeCombineRegionFn = fn void(XDisplay*, XWindow, int, int, int, Region, int);

XcursorImageCreateFn     xcursor_image_create_fn;
XcursorImageDestroyFn    xcursor_image_destroy_fn;
XcursorImageLoadCursorFn xcursor_image_load_cursor_fn;
XISelectEventsFn         xi_select_events_fn;
XSyncIntToValueFn        xsync_int_to_value_fn;
XSyncSetCounterFn        xsync_set_counter_fn;
XSyncCreateCounterFn     xsync_create_counter_fn;
XSyncDestroyCounterFn    xsync_destroy_counter_fn;
XSyncInitializeFn        xsync_initialize_fn;
XShapeCombineMaskFn      xshape_combine_mask_fn;
XShapeCombineRegionFn    xshape_combine_region_fn;

void* cursor_handle;
void* xi_handle;
void* ext_handle;

struct XImageHeader
{
	int width;
	int height;
	int xoffset;
	int format;
	char* data;
}

struct XcursorImage
{
	uint version;
	uint size;
	uint width, height;
	uint xhot, yhot;
	uint delay;
	uint* pixels;
}
struct XIEventMask
{
	int deviceid;
	int mask_len;
	char* mask;
}
struct XIValuatorState
{
	int mask_len;
	char* mask;
	double* values;
}
struct XIRawEvent
{
	int type;
	CULong serial;
	int send_event;
	XDisplay* display;
	int extension;
	int evtype;
	Time_t time;
	int deviceid;
	int sourceid;
	int detail;
	int flags;
	XIValuatorState valuators;
	double* raw_values;
}
// Predefined X11 atom, see X11/Xatom.h
const Atom XA_CARDINAL = 6;

struct XSyncValue
{
	int hi;
	uint lo;
}
alias XSyncCounter = XlibId;

struct XAspect
{
	int x, y;
}
struct X11WindowHints
{
	CULong flags, functions, decorations, status;
	CLong input_mode;
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
	XVisual* visual;
	XWindow root;
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

struct XClassHint { ZString res_name; ZString res_class; }

struct XWMHints
{
	CLong flags;
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

struct XAnyEvent { int type; CULong serial; int send_event; XDisplay* display; XWindow window; }
struct XKeyEvent { int type; CULong serial; int send_event; XDisplay* display; XWindow window; XWindow root; XWindow subwindow; Time_t time; int x, y; int x_root, y_root; uint state; uint keycode; int same_screen; }
struct XButtonEvent { int type; CULong serial; int send_event; XDisplay* display; XWindow window; XWindow root; XWindow subwindow; Time_t time; int x, y; int x_root, y_root; uint state; uint button; int same_screen; }
struct XMotionEvent { int type; CULong serial; int send_event; XDisplay* display; XWindow window; XWindow root; XWindow subwindow; Time_t time; int x, y; int x_root, y_root; uint state; char is_hint; int same_screen; }
struct XCrossingEvent { int type; CULong serial; int send_event; XDisplay* display; XWindow window; XWindow root; XWindow subwindow; Time_t time; int x, y; int x_root, y_root; int mode; int detail; int same_screen; int focus; uint state; }
struct XFocusChangeEvent { int type; CULong serial; int send_event; XDisplay* display; XWindow window; int mode; int detail; }
struct XExposeEvent { int type; CULong serial; int send_event; XDisplay* display; XWindow window; int x, y, width, height; int count; }
struct XConfigureEvent { int type; CULong serial; int send_event; XDisplay* display; XWindow event; XWindow window; int x, y, width, height; int border_width; XWindow above; int override_redirect; }
struct XPropertyEvent { int type; CULong serial; int send_event; XDisplay* display; XWindow window; Atom atom; Time_t time; int state; }
struct XClientMessageDataL { CLong[5] l; }
struct XClientMessageEvent { int type; CULong serial; int send_event; XDisplay* display; XWindow window; Atom message_type; int format; XClientMessageDataL data; }
struct XSelectionRequestEvent { int type; CULong serial; int send_event; XDisplay* display; XWindow owner; XWindow requestor; Atom selection; Atom target; Atom property; Time_t time; }
struct XSelectionEvent { int type; CULong serial; int send_event; XDisplay* display; XWindow requestor; Atom selection; Atom target; Atom property; Time_t time; }
struct XReparentEvent { int type; CULong serial; int send_event; XDisplay* display; XWindow event; XWindow window; XWindow parent; int x, y; int override_redirect; }
struct XMapEvent { int type; CULong serial; int send_event; XDisplay* display; XWindow event; XWindow window; int override_redirect; }
struct XUnmapEvent { int type; CULong serial; int send_event; XDisplay* display; XWindow event; XWindow window; int from_configure; }
struct XErrorEvent { int type; XDisplay* display; CULong resourceid; CULong serial; char error_code; char request_code; char minor_code; }
struct XGenericEventCookie { int type; CULong serial; int send_event; XDisplay* display; int extension; int evtype; uint cookie_data_len; void* data; }

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

typedef XDisplay = void;
typedef XVisual = void;
alias XWindow = CULong;
alias XDrawable = CULong;
alias XPointer = char*;
alias RROutput = CULong;
alias XCursor = CULong;
alias XlibIC = void*;
alias XlibIM = void*;
typedef Screen = void;
alias Colormap = CULong;
alias Pixmap = CULong;
alias GContext = void*;
alias Atom = CULong;
alias VisualID = CULong;
alias KeySym = CULong;
alias KeyCode = char;
alias XlibId = CULong;
alias Region = void*;
alias XIMStyle = CULong;
alias RRMode = CULong;
alias RRCrtc = CULong;
alias GLXContext = void*;
alias GLXFBConfig = void*;
alias GLXDrawable = CULong;
alias GLXWindow = CULong;

struct XRRScreenResources
{
	Time_t timestamp;
	Time_t configTimestamp;
	int ncrtc;
	RRCrtc* crtcs;
	int noutput;
	RROutput* outputs;
	int nmode;
	XRRModeInfoNative* modes;
}

struct XRRModeInfoNative
{
	RRMode id;
	uint width;
	uint height;
	CULong dotClock;
	uint hSyncStart;
	uint hSyncEnd;
	uint hTotal;
	uint hSkew;
	uint vSyncStart;
	uint vSyncEnd;
	uint vTotal;
	char* name;
	uint nameLength;
	CULong modeFlags;
}

struct XRRCrtcInfo
{
	Time_t timestamp;
	int x, y;
	uint width, height;
	RRMode mode;
	int rotation;
	int noutput;
	RROutput* outputs;
	int rotations;
	int npossible;
	RROutput* possible;
}
struct GfxXRROutputInfo
{
	Time_t timestamp;
	RRCrtc crtc;
	ZString name;
	int nameLen;
	CULong mm_width;
	CULong mm_height;
	int connection;
	int subpixel_order;
	int ncrtc;
	RRCrtc* crtcs;
	int nclone;
	RROutput* clones;
	int nmode;
	int npreferred;
	RRMode* modes;
}
struct XRRCrtcGamma
{
	int size;
	ushort* red;
	ushort* green;
	ushort* blue;
}

struct XVisualInfo
{
	XVisual* visual;
	VisualID visualid;
	int screen;
	int depth;
	int c_class;
	CULong red_mask, green_mask, blue_mask;
	int colormap_size;
	int bits_per_rgb;
}

fn void dlclose_cursor_xi_ext()
{
	if (cursor_handle) libc::dlclose(cursor_handle);
	if (xi_handle) libc::dlclose(xi_handle);
	if (ext_handle) libc::dlclose(ext_handle);
}

fn void dload_cursor_xi_ext()
{
	cursor_handle = libc::dlopen("libXcursor.so.1", libc::RTLD_LAZY | libc::RTLD_LOCAL);
	if (cursor_handle)
	{
		xcursor_image_create_fn      = libc::dlsym(cursor_handle, "XcursorImageCreate");
		xcursor_image_destroy_fn     = libc::dlsym(cursor_handle, "XcursorImageDestroy");
		xcursor_image_load_cursor_fn = libc::dlsym(cursor_handle, "XcursorImageLoadCursor");
	}
	xi_handle = libc::dlopen("libXi.so.6", libc::RTLD_LAZY | libc::RTLD_LOCAL);
	if (xi_handle) xi_select_events_fn = libc::dlsym(xi_handle, "XISelectEvents");
	ext_handle = libc::dlopen("libXext.so.6", libc::RTLD_LAZY | libc::RTLD_LOCAL);
	if (ext_handle)
	{
		xsync_create_counter_fn = libc::dlsym(ext_handle, "XSyncCreateCounter");
		xsync_int_to_value_fn = libc::dlsym(ext_handle, "XSyncIntToValue");
		xsync_set_counter_fn = libc::dlsym(ext_handle, "XSyncSetCounter");
		xsync_destroy_counter_fn = libc::dlsym(ext_handle, "XSyncDestroyCounter");
		xsync_initialize_fn = libc::dlsym(ext_handle, "XSyncInitialize");
		xshape_combine_region_fn = libc::dlsym(ext_handle, "XShapeCombineRegion");
		xshape_combine_mask_fn = libc::dlsym(ext_handle, "XShapeCombineMask");
	}
}
extern fn XDisplay* xOpenDisplay(ZString name) @cname("XOpenDisplay");
extern fn int xCloseDisplay(XDisplay* d) @cname("XCloseDisplay");
extern fn int xInitThreads() @cname("XInitThreads");
extern fn XWindow xDefaultRootWindow(XDisplay* d) @cname("XDefaultRootWindow");
extern fn XWindow xRootWindow(XDisplay* d, int screen) @cname("XRootWindow");
extern fn int xDefaultScreen(XDisplay* d) @cname("XDefaultScreen");
extern fn XVisual* xDefaultVisual(XDisplay* d, int screen) @cname("XDefaultVisual");
extern fn int xDefaultDepth(XDisplay* d, int screen) @cname("XDefaultDepth");
extern fn GContext xDefaultGC(XDisplay* d, int screen) @cname("XDefaultGC");
extern fn Colormap xCreateColormap(XDisplay* d, XWindow win, XVisual* visual, int alloc) @cname("XCreateColormap");
extern fn int xFreeColors(XDisplay* d, Colormap cm, CULong* pixels, int n, CULong planes) @cname("XFreeColors");
extern fn XWindow xCreateWindow(XDisplay* d, XWindow parent, int x, int y, uint w, uint h, uint border_width, int depth, uint c_class, XVisual* visual, CULong valuemask, XSetWindowAttributes* attrs) @cname("XCreateWindow");
extern fn int xDestroyWindow(XDisplay* d, XWindow w) @cname("XDestroyWindow");
extern fn int xMapWindow(XDisplay* d, XWindow w) @cname("XMapWindow");
extern fn int xMapRaised(XDisplay* d, XWindow w) @cname("XMapRaised");
extern fn int xUnmapWindow(XDisplay* d, XWindow w) @cname("XUnmapWindow");
extern fn int xMoveWindow(XDisplay* d, XWindow w, int x, int y) @cname("XMoveWindow");
extern fn int xResizeWindow(XDisplay* d, XWindow w, uint width, uint height) @cname("XResizeWindow");
extern fn int xRaiseWindow(XDisplay* d, XWindow w) @cname("XRaiseWindow");
extern fn GContext xCreateGC(XDisplay* d, XDrawable draw, CULong valuemask, void* values) @cname("XCreateGC");
extern fn int xFreeGC(XDisplay* d, GContext gc) @cname("XFreeGC");
extern fn int xSaveContext(XDisplay* d, XlibId xid, int context, XPointer data) @cname("XSaveContext");
extern fn int xFindContext(XDisplay* d, XlibId xid, int context, XPointer* data_return) @cname("XFindContext");
extern fn int xDeleteContext(XDisplay* d, XlibId xid, int context) @cname("XDeleteContext");
extern fn int xUniqueContext() @cname("XrmUniqueQuark");
extern fn XlibIC xCreateIC(XlibIM im, ...) @cname("XCreateIC");
extern fn int xDestroyIC(XlibIC ic) @cname("XDestroyIC");
extern fn int xSetICFocus(XlibIC ic) @cname("XSetICFocus");
extern fn int xUnsetICFocus(XlibIC ic) @cname("XUnsetICFocus");
extern fn XlibIM xOpenIM(XDisplay* d, void* db, ZString res_name, ZString res_class) @cname("XOpenIM");
extern fn int xCloseIM(XlibIM im) @cname("XCloseIM");
extern fn ZString xGetIMValues(XlibIM im, ...) @cname("XGetIMValues");
extern fn ZString xSetIMValues(XlibIM im, ...) @cname("XSetIMValues");
extern fn int xRegisterIMInstantiateCallback(XDisplay* d, void* db, ZString res_name, ZString res_class, void* callback, XPointer client_data) @cname("XRegisterIMInstantiateCallback");
extern fn int xUnregisterIMInstantiateCallback(XDisplay* d, void* db, ZString res_name, ZString res_class, void* callback, XPointer client_data) @cname("XUnregisterIMInstantiateCallback");
extern fn ZString xSetLocaleModifiers(ZString modifier_list) @cname("XSetLocaleModifiers");
extern fn sz xutf8LookupString(XlibIC ic, XKeyEvent* event, char* buffer, int bytes, KeySym* keysym, int* status) @cname("Xutf8LookupString");
extern fn int xutf8SetWMProperties(XDisplay* d, XWindow w, ZString window_name, ZString icon_name, ZString* argv, int argc, XSizeHints* hints, XWMHints* wm_hints, void* class_hints) @cname("Xutf8SetWMProperties");
extern fn int xSetClassHint(XDisplay* d, XWindow w, XClassHint* hint) @cname("XSetClassHint");
extern fn int xSetWMHints(XDisplay* d, XWindow w, XWMHints* hints) @cname("XSetWMHints");
extern fn XWMHints* xGetWMHints(XDisplay* d, XWindow w) @cname("XGetWMHints");
extern fn int xSetWMProtocols(XDisplay* d, XWindow w, Atom* protocols, int count) @cname("XSetWMProtocols");
extern fn int xSetWMSizeHints(XDisplay* d, XDrawable w, XSizeHints* hints, Atom property) @cname("XSetWMSizeHints");
extern fn int xGetWMNormalHints(XDisplay* d, XWindow w, XSizeHints* hints, CLong* flags) @cname("XGetWMNormalHints");
extern fn int xSetWMNormalHints(XDisplay* d, XWindow w, XSizeHints* hints) @cname("XSetWMNormalHints");
extern fn int xSelectInput(XDisplay* d, XDrawable w, CLong event_mask) @cname("XSelectInput");
extern fn int xStoreName(XDisplay* d, XWindow w, ZString name) @cname("XStoreName");
extern fn int xSetWindowBackground(XDisplay* d, XWindow w, CULong background) @cname("XSetWindowBackground");
extern fn int xSetWindowBackgroundPixmap(XDisplay* d, XWindow w, Pixmap pixmap) @cname("XSetWindowBackgroundPixmap");
extern fn int xClearWindow(XDisplay* d, XWindow w) @cname("XClearWindow");
extern fn int xFlush(XDisplay* d) @cname("XFlush");
extern fn int xSync(XDisplay* d, int discard) @cname("XSync");
extern fn int xPending(XDisplay* d) @cname("XPending");
extern fn int xConnectionNumber(XDisplay* d) @cname("XConnectionNumber");
extern fn int xQLength(XDisplay* d) @cname("XQLength");
extern fn int xEventsQueued(XDisplay* d, int mode) @cname("XEventsQueued");
extern fn int xNextEvent(XDisplay* d, XEvent* ev) @cname("XNextEvent");
extern fn int xPeekEvent(XDisplay* d, XEvent* ev) @cname("XPeekEvent");
extern fn int xCheckTypedWindowEvent(XDisplay* d, XWindow w, int event_type, XEvent* ev) @cname("XCheckTypedWindowEvent");
extern fn int xFilterEvent(XEvent* ev, XWindow w) @cname("XFilterEvent");
extern fn int xSendEvent(XDisplay* d, XWindow w, int propagate, CLong event_mask, XEvent* ev) @cname("XSendEvent");
extern fn int xGetEventData(XDisplay* d, XGenericEventCookie* cookie) @cname("XGetEventData");
extern fn int xFreeEventData(XDisplay* d, XGenericEventCookie* cookie) @cname("XFreeEventData");
extern fn int xGetWindowAttributes(XDisplay* d, XWindow w, XWindowAttributes* attrs) @cname("XGetWindowAttributes");
extern fn int xChangeProperty(XDisplay* d, XWindow w, Atom property, Atom type, int format, int mode, char* data, int nelements) @cname("XChangeProperty");
extern fn int xGetWindowProperty(XDisplay* d, XWindow w, Atom property, CLong offset, CLong length, int delet, Atom req_type, Atom* actual_type_return, int* actual_format_return, CULong* nitems, CULong* bytes_after_return, char** prop_return) @cname("XGetWindowProperty");
extern fn int xDeleteProperty(XDisplay* d, XWindow w, Atom property) @cname("XDeleteProperty");
extern fn Atom xInternAtom(XDisplay* d, ZString name, int only_if_exists) @cname("XInternAtom");
extern fn int xQueryExtension(XDisplay* d, ZString name, int* major_opcode_return, int* first_event_return, int* first_error_return) @cname("XQueryExtension");
extern fn int xConvertSelection(XDisplay* d, Atom selection, Atom target, Atom property, XWindow requestor, Time_t time) @cname("XConvertSelection");
extern fn XWindow xGetSelectionOwner(XDisplay* d, Atom selection) @cname("XGetSelectionOwner");
extern fn int xSetSelectionOwner(XDisplay* d, Atom selection, XWindow owner, Time_t time) @cname("XSetSelectionOwner");
extern fn int xFree(void* data) @cname("XFree");
extern fn int xSetErrorHandler(void* handler) @cname("XSetErrorHandler");
extern fn int xGetErrorText(XDisplay* d, int code, char* buffer, int len) @cname("XGetErrorText");
extern fn int xQueryPointer(XDisplay* d, XWindow w, XWindow* root_return, XWindow* child_return, int* root_x_return, int* root_y_return, int* win_x_return, int* win_y_return, uint* mask) @cname("XQueryPointer");
extern fn int xWarpPointer(XDisplay* d, XWindow src, XWindow dest, int src_x, int src_y, uint src_width, uint src_height, int dest_x, int dest_y) @cname("XWarpPointer");
extern fn int xTranslateCoordinates(XDisplay* d, XWindow src, XWindow dest, int src_x, int src_y, int* dest_x_return, int* dest_y_return, XWindow* child_return) @cname("XTranslateCoordinates");
extern fn int xSetInputFocus(XDisplay* d, XWindow w, int revert, Time_t time) @cname("XSetInputFocus");
extern fn int xGrabPointer(XDisplay* d, XWindow w, int owner_events, uint event_mask, int pointer_mode, int keyboard_mode, XWindow confine_to, XCursor cursor, Time_t time) @cname("XGrabPointer");
extern fn int xUngrabPointer(XDisplay* d, Time_t time) @cname("XUngrabPointer");
extern fn int xIconifyWindow(XDisplay* d, XWindow w, int screen) @cname("XIconifyWindow");
extern fn int xDefineCursor(XDisplay* d, XWindow w, XCursor cursor) @cname("XDefineCursor");
extern fn int xFreeCursor(XDisplay* d, XCursor cursor) @cname("XFreeCursor");
extern fn XCursor xCreateFontCursor(XDisplay* d, uint shape) @cname("XCreateFontCursor");
extern fn KeySym xkbKeycodeToKeysym(XDisplay* d, KeyCode kc, int group, int level) @cname("XkbKeycodeToKeysym");
extern fn int xkbGetState(XDisplay* d, uint deviceSpec, void* rtrnState) @cname("XkbGetState");
extern fn void* xkbGetMap(XDisplay* d, uint which, uint deviceSpec) @cname("XkbGetMap");
extern fn int xkbGetNames(XDisplay* d, uint which, void* xkb) @cname("XkbGetNames");
extern fn void* xkbGetKeyboardByName(XDisplay* d, uint deviceSpec, void* names, uint want, uint need, int load) @cname("XkbGetKeyboardByName");
extern fn int xkbFreeKeyboard(void* xkb, uint which, int freeDesc) @cname("XkbFreeKeyboard");
extern fn int xMatchVisualInfo(XDisplay* d, int screen, int depth, int c_class, XVisualInfo* vinfo_return) @cname("XMatchVisualInfo");
extern fn int xChangeWindowAttributes(XDisplay* d, XWindow w, CULong valuemask, XSetWindowAttributes* attrs) @cname("XChangeWindowAttributes");
extern fn Region xCreateRegion() @cname("XCreateRegion");
extern fn int xDestroyRegion(Region r) @cname("XDestroyRegion");
extern fn XCursor xCreatePixmap(XDisplay* d, XDrawable draw, uint w, uint h, uint depth) @cname("XCreatePixmap");
extern fn void* xCreateImage(XDisplay* d, XVisual* visual, uint depth, int format, int offset, char* data, uint w, uint h, int bitmap_pad, int bytes_per_line) @cname("XCreateImage");
extern fn int xPutImage(XDisplay* d, XDrawable draw, GContext gc, void* image, int src_x, int src_y, int dest_x, int dest_y, uint w, uint h) @cname("XPutImage");
extern fn int xDestroyImage(void* image) @cname("XDestroyImage");
extern fn ZString xResourceManagerString(XDisplay* d) @cname("XResourceManagerString");
extern fn void* xrmGetStringDatabase(ZString data) @cname("XrmGetStringDatabase");
extern fn int xrmGetResource(void* db, ZString name, ZString c_class, ZString* str_type_return, void* value_return) @cname("XrmGetResource");
extern fn int xrmDestroyDatabase(void* db) @cname("XrmDestroyDatabase");
extern fn XRRScreenResources* xRRGetScreenResourcesCurrent(XDisplay* d, XWindow w) @cname("XRRGetScreenResourcesCurrent");
extern fn int xRRFreeScreenResources(XRRScreenResources* res) @cname("XRRFreeScreenResources");
extern fn RROutput xRRGetOutputPrimary(XDisplay* d, XWindow w) @cname("XRRGetOutputPrimary");
extern fn GfxXRROutputInfo* xRRGetOutputInfo(XDisplay* d, XRRScreenResources* res, RROutput output) @cname("XRRGetOutputInfo");
extern fn int xRRFreeOutputInfo(GfxXRROutputInfo* info) @cname("XRRFreeOutputInfo");
extern fn XRRCrtcInfo* xRRGetCrtcInfo(XDisplay* d, XRRScreenResources* res, RRCrtc crtc) @cname("XRRGetCrtcInfo");
extern fn int xRRFreeCrtcInfo(XRRCrtcInfo* info) @cname("XRRFreeCrtcInfo");
extern fn int xRRSetCrtcConfig(XDisplay* d, XRRScreenResources* res, RRCrtc crtc, Time_t time, int x, int y, RRMode mode, int rotation, RROutput* outputs, int noutputs) @cname("XRRSetCrtcConfig");
extern fn int xRRGetCrtcGammaSize(XDisplay* d, RRCrtc crtc) @cname("XRRGetCrtcGammaSize");
extern fn XRRCrtcGamma* xRRGetCrtcGamma(XDisplay* d, RRCrtc crtc) @cname("XRRGetCrtcGamma");
extern fn XRRCrtcGamma* xRRAllocGamma(int size) @cname("XRRAllocGamma");
extern fn int xRRSetCrtcGamma(XDisplay* d, RRCrtc crtc, XRRCrtcGamma* gamma) @cname("XRRSetCrtcGamma");
extern fn int xRRFreeGamma(XRRCrtcGamma* gamma) @cname("XRRFreeGamma");
extern fn int xRRQueryExtension(XDisplay* d, int* event_base_return, int* error_base_return) @cname("XRRQueryExtension");
extern fn int xRRSelectInput(XDisplay* d, XWindow w, int mask) @cname("XRRSelectInput");

extern fn VisualID xVisualIDFromVisual(XVisual* visual) @cname("XVisualIDFromVisual");

// --- XIm
alias XImCallbackProc = fn void(void* im, void* client_data, void* call_data);
struct XImCallback
{
	XPointer client_data;
	XImCallbackProc callback;
}

struct XImStyles
{
	ushort count_styles;
	XIMStyle* supported_styles;
}

alias XImInstantiateCallbackProc = fn void(XDisplay* d, XPointer client_data, XPointer call_data);


// core GLX 1.0 attributes
const GLX_USE_GL           = 1;
const GLX_BUFFER_SIZE      = 2;
const GLX_LEVEL            = 3;
const GLX_RGBA             = 4;
const GLX_DOUBLEBUFFER     = 5;
const GLX_STEREO           = 6;
const GLX_AUX_BUFFERS      = 7;
const GLX_RED_SIZE         = 8;
const GLX_GREEN_SIZE       = 9;
const GLX_BLUE_SIZE        = 10;
const GLX_ALPHA_SIZE       = 11;
const GLX_DEPTH_SIZE       = 12;
const GLX_STENCIL_SIZE     = 13;
const GLX_ACCUM_RED_SIZE   = 14;
const GLX_ACCUM_GREEN_SIZE = 15;
const GLX_ACCUM_BLUE_SIZE  = 16;
const GLX_ACCUM_ALPHA_SIZE = 17;

// GLX 1.3 FBConfig attributes
const GLX_CONFIG_CAVEAT     = 0x20;
const GLX_X_VISUAL_TYPE     = 0x22;
const GLX_DRAWABLE_TYPE     = 0x8010;
const GLX_RENDER_TYPE       = 0x8011;
const GLX_X_RENDERABLE      = 0x8012;
const GLX_FBCONFIG_ID       = 0x8013;
const GLX_RGBA_TYPE         = 0x8014;
const GLX_COLOR_INDEX_TYPE  = 0x8015;

// GLX_X_VISUAL_TYPE values
const GLX_NONE         = 0x8000;
const GLX_TRUE_COLOR   = 0x8002;
const GLX_DIRECT_COLOR = 0x8003;
const GLX_PSEUDO_COLOR = 0x8004;
const GLX_STATIC_COLOR = 0x8005;
const GLX_GRAY_SCALE   = 0x8006;
const GLX_STATIC_GRAY  = 0x8007;

// GLX_DRAWABLE_TYPE / GLX_RENDER_TYPE bits
const GLX_RGBA_BIT        = 0x00000001;
const GLX_COLOR_INDEX_BIT = 0x00000002;
const GLX_WINDOW_BIT      = 0x00000001;
const GLX_PIXMAP_BIT      = 0x00000002;
const GLX_PBUFFER_BIT     = 0x00000004;

// GLX_ARB_multisample
const GLX_SAMPLE_BUFFERS = 100000;
const GLX_SAMPLES        = 100001;

// GLX_ARB_create_context and friends
const GLX_CONTEXT_MAJOR_VERSION_ARB             = 0x2091;
const GLX_CONTEXT_MINOR_VERSION_ARB             = 0x2092;
const GLX_CONTEXT_FLAGS_ARB                     = 0x2094;
const GLX_CONTEXT_PROFILE_MASK_ARB              = 0x9126;
const GLX_CONTEXT_CORE_PROFILE_BIT_ARB          = 0x00000001;
const GLX_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB = 0x00000002;
const GLX_CONTEXT_DEBUG_BIT_ARB                 = 0x00000001;
const GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB    = 0x00000002;
const GLX_CONTEXT_ES_PROFILE_BIT_EXT            = 0x00000004;
const GLX_CONTEXT_ROBUST_ACCESS_BIT_ARB         = 0x00000004;
const GLX_CONTEXT_RELEASE_BEHAVIOR_ARB          = 0x2097;
const GLX_CONTEXT_RELEASE_BEHAVIOR_NONE_ARB     = 0x0;
const GLX_CONTEXT_RELEASE_BEHAVIOR_FLUSH_ARB    = 0x2098;
const GLX_CONTEXT_OPENGL_NO_ERROR_ARB           = 0x31B3;

// GLX_ARB_framebuffer_sRGB / GLX_EXT_framebuffer_sRGB — same value, different extension namespace
const GLX_FRAMEBUFFER_SRGB_CAPABLE_ARB = 0x20B2;
const GLX_FRAMEBUFFER_SRGB_CAPABLE_EXT = 0x20B2;
