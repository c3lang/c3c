module std::gfx @feat(X11_WAYLAND) @link("X11", "Xext", "Xrandr", "GL", "wayland-client", "wayland-cursor", "wayland-egl", "xkbcommon", "EGL");
import std::collections::list, std::math, libc;

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
alias Time_t = CULong;
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
alias TODO_RENAME_glXCreateContextAttribsARBProc = fn GLXContext(XDisplay*, GLXFBConfig, GLXContext, int, int*);
alias TODO_RENAME_glXQueryExtensionsStringProc = fn ZString(XDisplay*, int);
alias TODO_RENAME_glXChooseVisualProc = fn XVisualInfo*(XDisplay*, int, int*);
alias TODO_RENAME_glXCreateContextProc = fn GLXContext(XDisplay*, XVisualInfo*, GLXContext, int);
alias TODO_RENAME_glXMakeCurrentProc = fn int(XDisplay*, GLXDrawable, GLXContext);
alias TODO_RENAME_glXGetCurrentContextProc = fn GLXContext();
alias TODO_RENAME_glXSwapBuffersProc = fn void(XDisplay*, GLXDrawable);
alias TODO_RENAME_glXSwapIntervalEXTProc = fn void(XDisplay*, GLXDrawable, int);
alias TODO_RENAME_glXGetProcAddressProc = fn void*(ZString);
alias TODO_RENAME_glXGetVisualFromFBConfigProc = fn XVisualInfo*(XDisplay*, GLXFBConfig);
alias TODO_RENAME_glXGetFBConfigAttribProc = fn int(XDisplay*, GLXFBConfig, int, int*);
alias TODO_RENAME_glXGetProcAddressARBProc = fn void*(ZString);
alias TODO_RENAME_glXChooseFBConfigProc = fn GLXFBConfig*(XDisplay*, int, int*, int*);
alias TODO_RENAME_glXDestroyContextProc = fn void(XDisplay*, GLXContext);
alias TODO_RENAME_glXCreateWindowProc = fn GLXWindow(XDisplay*, GLXFBConfig, XWindow, int*);
alias TODO_RENAME_glXDestroyWindowProc = fn void(XDisplay*, GLXWindow);
alias TODO_RENAME_glXSwapIntervalNativeProc = fn int(int);

TODO_RENAME_glXCreateContextAttribsARBProc todo_fix_glXCreateContextAttribsARB;
TODO_RENAME_glXQueryExtensionsStringProc todo_fix_glXQueryExtensionsString;
TODO_RENAME_glXChooseVisualProc todo_fix_glXChooseVisual;
TODO_RENAME_glXCreateContextProc todo_fix_glXCreateContext;
TODO_RENAME_glXMakeCurrentProc todo_fix_glXMakeCurrent;
TODO_RENAME_glXGetCurrentContextProc todo_fix_glXGetCurrentContext;
TODO_RENAME_glXSwapBuffersProc todo_fix_glXSwapBuffers;
TODO_RENAME_glXSwapIntervalEXTProc todo_fix_glXSwapIntervalEXT;
TODO_RENAME_glXGetVisualFromFBConfigProc todo_fix_glXGetVisualFromFBConfig;
TODO_RENAME_glXGetFBConfigAttribProc todo_fix_glXGetFBConfigAttrib;
TODO_RENAME_glXGetProcAddressARBProc todo_fix_glXGetProcAddressARB;
TODO_RENAME_glXChooseFBConfigProc todo_fix_glXChooseFBConfig;
TODO_RENAME_glXDestroyContextProc todo_fix_glXDestroyContext;
TODO_RENAME_glXCreateWindowProc todo_fix_glXCreateWindow;
TODO_RENAME_glXDestroyWindowProc todo_fix_glXDestroyWindow;
TODO_RENAME_glXSwapIntervalNativeProc todo_fix_glXSwapIntervalNative;


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



const uint WL_MARSHAL_FLAG_DESTROY = 1;


// ---- listener vtables (field order matches each protocol's `_listener` struct) ----


// private
fn void rgfwDoNothing() {}



// private




// public
fn void? todo_fix_createWindowPlatform_Wayland(String name, GfxWindowFlags flags, GfxWindow* win) => @pool()
{
	todo_fix_debugCallback(TODO_RENAME_TYPE_WARNING, TODO_RENAME_WARNING_WAYLAND, "RGFW Wayland support is experimental");

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





module std::gfx @feat(X11_WAYLAND) @link("wayland-client", "wayland-cursor", "wayland-egl", "xkbcommon", "EGL");
import std::math, libc;
// ==================== TODO_RENAME_DYNAMIC runtime dispatch layer ====================
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
			if (libc::errno() == EAGAIN)
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

			if (waitMS != TODO_RENAME_EVENT_WAIT_NEXT)
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

			if (waitMS != TODO_RENAME_EVENT_WAIT_NEXT)
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
