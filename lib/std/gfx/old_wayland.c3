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


const int VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR = 1000006000;
const VkResult VK_ERROR_EXTENSION_NOT_PRESENT = -7;
struct VkWaylandSurfaceCreateInfoKHR { int sType; void* pNext; uint flags; WlDisplay* display; WlProxy* surface; }
alias TodoRenameVkCreateWaylandSurfaceKHRProc = fn VkResult(VkInstance instance, VkWaylandSurfaceCreateInfoKHR* createInfo, void* allocator, VkSurfaceKHR* surface);
ZString[2] todo_fix_vulkanExtensions = {"VK_KHR_surface", "VK_KHR_wayland_surface"};


// public
fn VkResult todo_fix_window_createSurface_Vulkan(GfxWindow* win, VkInstance instance, VkSurfaceKHR* surface)
{
	if (surface == null || win == null) return VK_ERROR_EXTENSION_NOT_PRESENT;
	*surface = 0;
	GfxFn raw = todo_fix_getInstanceProcAddress_Vulkan(instance, "vkCreateWaylandSurfaceKHR");
	if (raw == null) return VK_ERROR_EXTENSION_NOT_PRESENT;
	VkWaylandSurfaceCreateInfoKHR createInfo;
	createInfo.sType = VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR;
	createInfo.pNext = null;
	createInfo.flags = 0;
	createInfo.display = _gfx_ctx.wlDisplay;
	createInfo.surface = win.src.surface;
	return ((TodoRenameVkCreateWaylandSurfaceKHRProc)raw)(instance, &createInfo, null, surface);
}

alias TodoRenameVkGetPhysicalDeviceWaylandPresentationSupportKHRProc = fn uint(VkPhysicalDevice, uint, WlDisplay*);

// public
fn bool todo_fix_getPhysicalDevicePresentationSupport_Vulkan(VkInstance instance, VkPhysicalDevice physicalDevice, uint queueFamilyIndex)
{
	GfxFn raw = todo_fix_getInstanceProcAddress_Vulkan(instance, "vkGetPhysicalDeviceWaylandPresentationSupportKHR");
	if (raw == null) return false;
	uint out = ((TodoRenameVkGetPhysicalDeviceWaylandPresentationSupportKHRProc)raw)(physicalDevice, queueFamilyIndex, _gfx_ctx.wlDisplay);
	return out != 0;
}


const WGPUSType WGPU_STYPE_SURFACE_SOURCE_WAYLAND_SURFACE = 0x00000007;
struct WgpuSurfaceSourceWaylandSurface { WgpuChainedStruct chain; WlDisplay* display; WlProxy* surface; }
extern fn WGPUSurface wgpuInstanceCreateSurface(WGPUInstance instance, WgpuSurfaceDescriptor* descriptor) @cname("wgpuInstanceCreateSurface");


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


WlRegistryListener todo_fix_registryListener = { .global = &todo_fix_wlGlobalRegistryHandler, .globalRemove = &todo_fix_wlGlobalRegistryRemove };


// public
fn void? todo_fix_createWindowPlatform(String name, GfxWindowFlags flags, GfxWindow* win) => @pool()
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


WlDataSourceListener todo_fix_dataSourceListener = {
	.target = &todo_fix_wlDataSourceNothingStr, .action = &todo_fix_wlDataSourceNothingU32,
	.dndDropPerformed = &todo_fix_wlDataSourceNothing, .dndFinished = &todo_fix_wlDataSourceNothing,
	.send = &todo_fix_wlDataSourceSend, .cancelled = &todo_fix_wlDataSourceCancelled
};


struct TodoRenameAttribStack
{
	int* attribs;
	sz count;
	sz max;
}




// public
fn bool todo_fix_extensionSupportedPlatform_EGL(ZString extension, sz len)
{
	if (todo_fix_eglQueryString == null || _gfx_ctx.egl_display == null) return false;
	ZString extensions = todo_fix_eglQueryString((EglDisplay)_gfx_ctx.egl_display, 0x3055 /* EGL_EXTENSIONS */);
	return extensions != null && todo_fix_extensionSupportedStr(extensions, extension, len);
}




