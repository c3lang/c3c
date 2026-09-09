module std::gfx @feat(WAYLAND) @link("wayland-client", "wayland-cursor", "wayland-egl", "xkbcommon", "EGL");
import std::collections::list, std::math, libc;


extern fn int clock_gettime(int clock_id, Timespec* ts);

struct Timespec { CLong tv_sec; CLong tv_nsec; }

// ---- POSIX / libc bits used by the Wayland backend ----
extern fn int shmOpen(ZString name, int oflag, int mode) @cname("shm_open");
extern fn int shmUnlink(ZString name) @cname("shm_unlink");
extern fn int ftruncate(int fd, sz length);
extern fn int poll(Pollfd* fds, CUInt nfds, int timeout);

struct Pollfd { int fd; short events; short revents; }

const int O_RDWR = 0x0002;
const int O_CREAT = 0x0040;
const int O_EXCL = 0x0080;
const int POLLIN = 0x0001;

// ---- Wayland client core (real libwayland-client ABI) ----


const uint WL_MARSHAL_FLAG_DESTROY = 1;


// xdg-shell / extension protocol interface tables: NOT part of libwayland-client;
// these come from wayland-scanner-generated protocol code (wayland-protocols)
// that a real build must generate and link in, exactly like the original C
// header's `#include "xdg-shell.h"` implies.

// ---- listener vtables (field order matches each protocol's `_listener` struct) ----


// private
fn void rgfwDoNothing() {}

struct TodoRenameGlContext { void* ctx; void* format; }
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
	void* window;
	void* view;
	void* mouse;
	void* delegate;
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




// public


// public
fn void? todo_fix_createWindowPlatform(String name, GfxWindowFlags flags, GfxWindow* win) => @pool()
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

	if (_gfx_ctx.decorationManager != null)
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

struct TodoRenameAttribStack
{
	int* attribs;
	sz count;
	sz max;
}






