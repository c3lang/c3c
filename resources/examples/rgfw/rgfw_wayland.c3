module std::gfx @feat(WAYLAND) @link("wayland-client", "wayland-cursor", "wayland-egl", "xkbcommon", "EGL");
import std::collections::list, std::math, libc;

// C3 port of the Wayland/EGL RGFW backend from /Users/lerno/Downloads/AGFW.h (RGFW).
// Mechanical, line-by-line style translation of the RGFW_WAYLAND implementation.
// Standalone alternative to rgfw.c3 (macOS) / rgfw_x11.c3 - only ONE rgfw*.c3 file
// should be included for a given target, matching how the original C header only
// ever compiles one #ifdef backend at a time.
//
// Like the original C header (which does `#include "xdg-shell.h"` etc. and expects
// the caller to generate+compile those files with wayland-scanner), this file
// declares the xdg-shell/decoration/relative-pointer/pointer-constraints/xdg-output/
// toplevel-icon/pointer-warp `_interface` descriptor tables as `extern` - a real
// build needs wayland-scanner-generated protocol code (from wayland-protocols)
// compiled and linked in alongside this file to supply those tables. Everything
// built on top of them here (the `xdg_*`/`zxdg_*`/`zwp_*`/`wp_*` request wrappers)
// is reimplemented directly against `wl_proxy_marshal_flags` using each protocol's
// stable request ordering, since those wrapper functions are normally generated as
// `static inline` (no linkable symbol to bind to).


extern fn int clock_gettime(int clock_id, Timespec* ts);

struct Timespec { CLong tv_sec; CLong tv_nsec; }

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
extern fn ZString getenv(ZString name);
struct Pollfd { int fd; short events; short revents; }
const int PROT_READ = 0x1;
const int PROT_WRITE = 0x2;
const int MAP_SHARED = 0x01;
const void* MAP_FAILED = (void*)(uptr)(-1);
const int O_RDWR = 0x0002;
const int O_CREAT = 0x0040;
const int O_EXCL = 0x0080;
const int POLLIN = 0x0001;

// ---- Wayland client core (real libwayland-client ABI) ----


extern fn int* rgfwErrnoLocation() @cname("__errno_location");
const int RGFW_EAGAIN = 11;

const uint WL_MARSHAL_FLAG_DESTROY = 1;


// xdg-shell / extension protocol interface tables: NOT part of libwayland-client;
// these come from wayland-scanner-generated protocol code (wayland-protocols)
// that a real build must generate and link in, exactly like the original C
// header's `#include "xdg-shell.h"` implies.

// ---- listener vtables (field order matches each protocol's `_listener` struct) ----


// private
fn void rgfwDoNothing() {}

struct RgfwGlContext { void* ctx; void* format; }
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
	void* window;
	void* view;
	void* mouse;
	void* delegate;
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


const int VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR = 1000006000;
const VkResult VK_ERROR_EXTENSION_NOT_PRESENT = -7;
struct VkWaylandSurfaceCreateInfoKHR { int sType; void* pNext; uint flags; WlDisplay* display; WlProxy* surface; }
alias RgfwVkCreateWaylandSurfaceKHRProc = fn VkResult(VkInstance instance, VkWaylandSurfaceCreateInfoKHR* createInfo, void* allocator, VkSurfaceKHR* surface);
ZString[2] rgfw_vulkanExtensions = {"VK_KHR_surface", "VK_KHR_wayland_surface"};


// public
fn VkResult rgfw_window_createSurface_Vulkan(GfxWindow* win, VkInstance instance, VkSurfaceKHR* surface)
{
	if (surface == null || win == null) return VK_ERROR_EXTENSION_NOT_PRESENT;
	*surface = 0;
	GfxFn raw = rgfw_getInstanceProcAddress_Vulkan(instance, "vkCreateWaylandSurfaceKHR");
	if (raw == null) return VK_ERROR_EXTENSION_NOT_PRESENT;
	VkWaylandSurfaceCreateInfoKHR createInfo;
	createInfo.sType = VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR;
	createInfo.pNext = null;
	createInfo.flags = 0;
	createInfo.display = _gfx_ctx.wlDisplay;
	createInfo.surface = win.src.surface;
	return ((RgfwVkCreateWaylandSurfaceKHRProc)raw)(instance, &createInfo, null, surface);
}

alias RgfwVkGetPhysicalDeviceWaylandPresentationSupportKHRProc = fn uint(VkPhysicalDevice, uint, WlDisplay*);

// public
fn bool rgfw_getPhysicalDevicePresentationSupport_Vulkan(VkInstance instance, VkPhysicalDevice physicalDevice, uint queueFamilyIndex)
{
	GfxFn raw = rgfw_getInstanceProcAddress_Vulkan(instance, "vkGetPhysicalDeviceWaylandPresentationSupportKHR");
	if (raw == null) return false;
	uint out = ((RgfwVkGetPhysicalDeviceWaylandPresentationSupportKHRProc)raw)(physicalDevice, queueFamilyIndex, _gfx_ctx.wlDisplay);
	return out != 0;
}

alias WGPUInstance = void*;
alias WGPUSurface = void*;
alias WGPUSType = int;
const WGPUSType WGPU_STYPE_SURFACE_SOURCE_WAYLAND_SURFACE = 0x00000007;
struct WgpuChainedStruct { WgpuChainedStruct* next; WGPUSType sType; }
struct WgpuSurfaceDescriptor { WgpuChainedStruct* nextInChain; }
struct WgpuSurfaceSourceWaylandSurface { WgpuChainedStruct chain; WlDisplay* display; WlProxy* surface; }
extern fn WGPUSurface wgpuInstanceCreateSurface(WGPUInstance instance, WgpuSurfaceDescriptor* descriptor) @cname("wgpuInstanceCreateSurface");


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
	mem::copy(dataPtr, &buf, n);
	dataPtr[length - 1] = 0;
	_gfx_ctx.unixClipboard.data = dataPtr;
	_gfx_ctx.unixClipboard.type = TEXT;
	_gfx_ctx.unixClipboard.length = length;

	close(pfds[0]);
	wlDataOfferDestroy(offer);
}


WlRegistryListener rgfw_registryListener = { .global = &rgfw_wlGlobalRegistryHandler, .globalRemove = &rgfw_wlGlobalRegistryRemove };


// public
fn void? rgfw_createWindowPlatform(String name, GfxWindowFlags flags, GfxWindow* win) => @pool()
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

	if (_gfx_ctx.decorationManager != null)
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


struct RgfwAttribStack
{
	int* attribs;
	sz count;
	sz max;
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

// public
fn bool rgfw_extensionSupportedPlatform_EGL(ZString extension, sz len)
{
	if (rgfw_eglQueryString == null || _gfx_ctx.egl_display == null) return false;
	ZString extensions = rgfw_eglQueryString((EglDisplay)_gfx_ctx.egl_display, 0x3055 /* EGL_EXTENSIONS */);
	return extensions != null && rgfw_extensionSupportedStr(extensions, extension, len);
}




