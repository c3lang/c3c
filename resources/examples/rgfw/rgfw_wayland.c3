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



extern fn void* memcpy(void* dest, void* src, sz count);
extern fn void* memset(void* dest, int value, sz count);
extern fn char* strstr(char* haystack, char* needle);
extern fn float powf(float x, float y);
extern fn int chdir(char* path);
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
const int O_CREAT = 0x0200;
const int O_EXCL = 0x0800;
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

const RgfwGfxContextType RGFW_GFX_NATIVE_OPEN_GL = 1;
const RgfwGfxContextType RGFW_GFX_EGL = 2;
const RgfwGfxContextType RGFW_GFX_OWNED_BY_RGFW = 4;

struct RgfwNativeImage
{
	GfxFormat format;
	char* buffer;
	void* native0;
	void* native1;
	void* native2;
	WlProxy* pool; // wl_shm_pool*
	int fd;
	WlProxy* wlBuffer; // wl_buffer*
}

struct RgfwWindowSrc
{
	void* window;
	void* view;
	void* mouse;
	void* delegate;
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

// public
fn bool rgfw_loadGL()
{
	// Wayland only ever renders through EGL; "OpenGL" here just means EGL is ready.
	return load_egl();
}



const int RTLD_LAZY = 0x1;
const int RTLD_LOCAL = 0x4;
const int VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR = 1000006000;
const VkResult VK_ERROR_EXTENSION_NOT_PRESENT = -7;
struct VkWaylandSurfaceCreateInfoKHR { int sType; void* pNext; uint flags; WlDisplay* display; WlProxy* surface; }
alias RgfwVkCreateWaylandSurfaceKHRProc = fn VkResult(VkInstance instance, VkWaylandSurfaceCreateInfoKHR* createInfo, void* allocator, VkSurfaceKHR* surface);
ZString[2] rgfw_vulkanExtensions = {"VK_KHR_surface", "VK_KHR_wayland_surface"};

// public
fn bool rgfw_loadVulkan()
{
	if (_gfx_ctx.vulkan_handle != null) return true;
	ZString[2] names = {"libvulkan.so.1", "libvulkan.so"};
	for (sz i = 0; i < 2; i++)
	{
		_gfx_ctx.vulkan_handle = libc::dlopen(names[i], RTLD_LAZY | RTLD_LOCAL);
		if (_gfx_ctx.vulkan_handle != null) break;
	}
	if (_gfx_ctx.vulkan_handle == null) return false;
	void* proc = libc::dlsym(_gfx_ctx.vulkan_handle, "vkGetInstanceProcAddr");
	if (proc == null) return false;
	_gfx_ctx.vkGetInstanceProcAddress = (GfxVkGetInstanceProcAddressProc)proc;
	return true;
}

// public
fn void rgfw_unloadVulkan()
{
	if (_gfx_ctx.vulkan_handle == null) return;
	libc::dlclose(_gfx_ctx.vulkan_handle);
	_gfx_ctx.vulkan_handle = null;
	_gfx_ctx.vkGetInstanceProcAddress = null;
}



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

// public
fn WGPUSurface rgfw_window_createSurface_WebGPU(GfxWindow* window, WGPUInstance instance)
{
	WgpuSurfaceSourceWaylandSurface fromWl;
	fromWl.chain.next = null;
	fromWl.chain.sType = WGPU_STYPE_SURFACE_SOURCE_WAYLAND_SURFACE;
	fromWl.display = _gfx_ctx.wlDisplay;
	fromWl.surface = window.src.surface;
	WgpuSurfaceDescriptor surfaceDesc;
	surfaceDesc.nextInChain = (WgpuChainedStruct*)&fromWl;
	return wgpuInstanceCreateSurface(instance, &surfaceDesc);
}




// private
fn void rgfw_wlSetOpaque(GfxWindow* win)
{
	WlProxy* region = wlCompositorCreateRegion(_gfx_ctx.compositor);
	if (region == null) return;
	wlRegionAdd(region, 0, 0, win.width, win.height);
	wlSurfaceSetOpaqueRegion(win.src.surface, region);
	wlRegionDestroy(region);
}

// private
fn void rgfw_toggleWaylandMaximized(GfxWindow* win, bool maximized)
{
	win.src.maximized = maximized;
	if (maximized) { xdgToplevelSetMaximized(win.src.xdgToplevel); }
	else { xdgToplevelUnsetMaximized(win.src.xdgToplevel); }
}

// private
fn void rgfw_wlXdgWmBasePingHandler(void* data, WlProxy* wmBase, uint serial) { xdgWmBasePong(wmBase, serial); }

// private
fn void rgfw_wlXdgSurfaceConfigureHandler(void* data, WlProxy* xdgSurface, uint serial)
{
	xdgSurfaceAckConfigure(xdgSurface, serial);
	GfxWindow* win = (GfxWindow*)data;
	if (win == null)
	{
		win = _gfx_ctx.kbOwner;
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
		_window_resized_callback(win, win.width, win.height);
		rgfw_window_resize(win, win.width, win.height);
		if (!(win.internal.flags.transparent)) rgfw_wlSetOpaque(win);
	}

	win.src.configured = true;
}

// private
fn void rgfw_wlXdgToplevelConfigureHandler(void* data, WlProxy* toplevel, int width, int height, WlArray* states)
{
	GfxWindow* win = (GfxWindow*)data;
	win.src.pendingActivated = false;
	win.src.pendingMaximized = false;
	win.src.resizing = false;

	if (states != null)
	{
		uint* state = (uint*)states.data;
		sz count = states.size / uint::size;
		for (sz i = 0; i < count; i++)
		{
			switch (state[i])
			{
				case 4: // XDG_TOPLEVEL_STATE_ACTIVATED
					win.src.pendingActivated = true;
				case 1: // XDG_TOPLEVEL_STATE_MAXIMIZED
					win.src.pendingMaximized = true;
				default:
					break;
			}
		}
	}

	if (width != 0 && height != 0 && (win.width != width || win.height != height))
	{
		win.src.resizing = true;
		win.src.w = width; win.width = width;
		win.src.h = height; win.height = height;
	}
}

// private
fn void rgfw_wlXdgToplevelCloseHandler(void* data, WlProxy* toplevel)
{
	GfxWindow* win = (GfxWindow*)data;
	if (!win.internal.shouldClose) _window_close_callback(win);
}

// private
fn void rgfw_wlXdgDecorationConfigureHandler(void* data, WlProxy* decoration, uint mode)
{
	GfxWindow* win = (GfxWindow*)data;
	if (mode != win.src.decorationMode) win.src.decorationMode = mode;
}

// private
fn void rgfw_wlShmFormatHandler(void* data, WlProxy* shm, uint format) {}

// private
fn void rgfw_wlRelativePointerMotion(void* data, WlProxy* relPointer, uint timeHi, uint timeLo, WlFixed dx, WlFixed dy, WlFixed dxUnaccel, WlFixed dyUnaccel)
{
	GfxContext* info = data;
	GfxWindow* win = info.mouseOwner;
	if (win == null) return;
	float vecX = (float)wlFixedToDouble(dx);
	float vecY = (float)wlFixedToDouble(dy);
	_raw_motion_callback(win, vecX, vecY);
}

// private
fn void rgfw_wlPointerLocked(void* data, WlProxy* lockedPointer)
{
	GfxContext* info = data;
	wlPointerSetCursor(info.wlPointer, info.mouseEnterSerial, null, 0, 0);
}

// private
fn void rgfw_wlPointerEnter(void* data, WlProxy* pointer, uint serial, WlProxy* surface, WlFixed surfaceX, WlFixed surfaceY)
{
	GfxContext* info = data;
	GfxWindow* win = (GfxWindow*)wlSurfaceGetUserData(surface);
	info.mouseEnterSerial = serial;
	win.internal.mouseInside = true;
	info.windowState.mouseEnter = true;
	info.mouseOwner = win;

	if (win.src.usingCustomCursor) { wlPointerSetCursor(pointer, serial, win.src.customCursorSurface, 0, 0); }
	else { win.set_mouse_default(); }

	int x = (int)wlFixedToDouble(surfaceX);
	int y = (int)wlFixedToDouble(surfaceY);
	_mouse_notify_callback(win, x, y, true);
}

// private
fn void rgfw_wlPointerLeave(void* data, WlProxy* pointer, uint serial, WlProxy* surface)
{
	GfxWindow* win = (GfxWindow*)wlSurfaceGetUserData(surface);
	GfxContext* info = data;
	if (info.mouseOwner == win) info.mouseOwner = null;
	_mouse_notify_callback(win, ...win.internal.last_mouse, false);
}

// private
fn void rgfw_wlPointerMotion(void* data, WlProxy* pointer, uint time, WlFixed x, WlFixed y)
{
	GfxContext* info = data;
	GfxWindow* win = info.mouseOwner;
	if (win == null) return;
	int cx = (int)wlFixedToDouble(x);
	int cy = (int)wlFixedToDouble(y);
	_mouse_motion_callback(win, cx, cy);
}

// private
fn void rgfw_wlPointerButton(void* data, WlProxy* pointer, uint serial, uint time, uint button, uint state)
{
	GfxContext* info = data;
	GfxWindow* win = info.mouseOwner;
	if (win == null) return;
	uint b = button - 0x110;
	if (b == 1) { b = 2; }
	else if (b == 2) { b = 1; }
	rgfw_mouseButtonCallback(win, (GfxMouseButton)b, !!state);
}

// private
fn void rgfw_wlPointerAxis(void* data, WlProxy* pointer, uint time, uint axis, WlFixed value)
{
	GfxContext* info = data;
	GfxWindow* win = info.mouseOwner;
	if (win == null) return;
	float scrollX = 0.0f;
	float scrollY = 0.0f;
	if (!(win.internal.enabledEvents & RGFW_MOUSE_SCROLL_FLAG)) return;
	if (axis == WL_POINTER_AXIS_HORIZONTAL_SCROLL) { scrollX = (float)(-wlFixedToDouble(value) / 10.0); }
	else if (axis == WL_POINTER_AXIS_VERTICAL_SCROLL) { scrollY = (float)(-wlFixedToDouble(value) / 10.0); }
	rgfw_mouseScrollCallback(win, scrollX, scrollY);
}

// private
fn void rgfw_wlKeyboardKeymap(void* data, WlProxy* keyboard, uint format, int fd, uint size)
{
	GfxContext* info = data;
	ZString keymapString = (ZString)mmap(null, size, PROT_READ, MAP_SHARED, fd, 0);
	xkbKeymapUnref(info.keymap);
	info.keymap = xkbKeymapNewFromString(info.xkbContext, keymapString, XKB_KEYMAP_FORMAT_TEXT_V1, XKB_KEYMAP_COMPILE_NO_FLAGS);
	munmap(keymapString, size);
	close(fd);
	xkbStateUnref(info.xkbState);
	info.xkbState = xkbStateNew(info.keymap);

	ZString locale = getenv("LC_ALL");
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

// private
fn void rgfw_wlKeyboardEnter(void* data, WlProxy* keyboard, uint serial, WlProxy* surface, WlArray* keys)
{
	GfxContext* info = data;
	GfxWindow* win = (GfxWindow*)wlSurfaceGetUserData(surface);
	info.kbOwner = win;

	if (info.dataDevice != null && win.src.dataSource != null) wlDataDeviceSetSelection(info.dataDevice, win.src.dataSource, serial);
	if (win.src.minimized == true) win.src.minimized = false;
	rgfw_windowFocusCallback(win, true);
}

// private
fn void rgfw_wlKeyboardLeave(void* data, WlProxy* keyboard, uint serial, WlProxy* surface)
{
	GfxContext* info = data;
	GfxWindow* win = (GfxWindow*)wlSurfaceGetUserData(surface);
	if (info.kbOwner == win) info.kbOwner = null;
	rgfw_windowFocusCallback(win, false);
}

// private
fn XkbKeysym rgfw_wlComposeSymbol(GfxContext* info, XkbKeysym sym)
{
	if (sym == XKB_KEY_NO_SYMBOL || info.composeState == null) return sym;
	if (xkbComposeStateFeed(info.composeState, sym) != XKB_COMPOSE_FEED_ACCEPTED) return sym;
	int status = xkbComposeStateGetStatus(info.composeState);
	if (status == XKB_COMPOSE_COMPOSED) return xkbComposeStateGetOneSym(info.composeState);
	if (status == XKB_COMPOSE_COMPOSING || status == XKB_COMPOSE_CANCELLED) return XKB_KEY_NO_SYMBOL;
	return sym;
}

// private
fn void rgfw_wlSendKeyEvent(uint key)
{
	XkbKeysym* keysyms;
	if (xkbStateKeyGetSyms(_gfx_ctx.xkbState, key + 8, &keysyms) == 1)
	{
		XkbKeysym keysym = rgfw_wlComposeSymbol(&_gfx_ctx, keysyms[0]);
		uint codepoint = xkbKeysymToUtf32(keysym);
		if (codepoint != 0) rgfw_keyCharCallback(_gfx_ctx.kbOwner, codepoint);
	}
}

// private
fn void rgfw_wlKeyboardKey(void* data, WlProxy* keyboard, uint serial, uint time, uint key, uint state)
{
	GfxContext* info = data;
	if (info.kbOwner == null) return;

	GfxWindow* win = info.kbOwner;
	RgfwKey rgfwKey = rgfw_apiKeyToRGFW(key + 8);

	rgfw_keyUpdateKeyMods(win, xkbKeymapModGetIndex(info.keymap, "Lock") != 0, xkbKeymapModGetIndex(info.keymap, "Mod2") != 0, xkbKeymapModGetIndex(info.keymap, "ScrollLock") != 0);
	rgfw_keyCallback(win, rgfwKey, win.internal.mod, is_key_down(rgfwKey) && state != 0, !!state);

	info.lastKey = state != 0 ? key : 0;
	info.lastKeyTime = time + (uint)_gfx_ctx.wlRepeatInfoDelay;
	if (state != 0) rgfw_wlSendKeyEvent(_gfx_ctx.lastKey);
}

// private
fn void rgfw_wlKeyboardModifiers(void* data, WlProxy* keyboard, uint serial, uint modsDepressed, uint modsLatched, uint modsLocked, uint group)
{
	GfxContext* info = data;
	xkbStateUpdateMask(info.xkbState, modsDepressed, modsLatched, modsLocked, 0, 0, group);
}

// private
fn void rgfw_wlKeyboardRepeatInfo(void* data, WlProxy* keyboard, int rate, int delay)
{
	_gfx_ctx.wlRepeatInfoRate = rate;
	_gfx_ctx.wlRepeatInfoDelay = delay;
}

WlPointerListener rgfw_wlPointerListener = {
	.enter = &rgfw_wlPointerEnter, .leave = &rgfw_wlPointerLeave, .motion = &rgfw_wlPointerMotion,
	.button = &rgfw_wlPointerButton, .axis = &rgfw_wlPointerAxis
};
WlKeyboardListener rgfw_wlKeyboardListener = {
	.keymap = &rgfw_wlKeyboardKeymap, .enter = &rgfw_wlKeyboardEnter, .leave = &rgfw_wlKeyboardLeave,
	.key = &rgfw_wlKeyboardKey, .modifiers = &rgfw_wlKeyboardModifiers, .repeatInfo = &rgfw_wlKeyboardRepeatInfo
};

// private
fn void rgfw_wlSeatCapabilities(void* data, WlProxy* seat, uint capabilities)
{
	GfxContext* info = data;

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

// private
fn void rgfw_wlSeatName(void* data, WlProxy* seat, ZString name) {}

WlSeatListener rgfw_wlSeatListener = { .capabilities = &rgfw_wlSeatCapabilities, .name = &rgfw_wlSeatName };
WlShmListener rgfw_wlShmListener = { .format = &rgfw_wlShmFormatHandler };

// private
fn void rgfw_wlOutputSetGeometry(void* data, WlProxy* output, int x, int y, int physW, int physH, int subpixel, ZString make, ZString model, int transform)
{
	GfxMonitor* mon = &((GfxMonitorNode*)data).mon;
	mon.origin = { x, y };
	mon.phys_size = { physW / 25.4f, physH / 25.4f };
}

// private
fn void rgfw_wlOutputHandleMode(void* data, WlProxy* output, uint flags, int width, int height, int refresh)
{
	GfxMonitorNode* node = (GfxMonitorNode*)data;
	GfxMonitor* mon = &node.mon;

	GfxMonitorMode mode;
	mode.w = width;
	mode.h = height;
	mode.refresh_rate = (float)refresh / 1000.0f;
	mode.src = output;

	node.modeCount += 1;
	GfxMonitorMode* modes = malloc(node.modeCount * GfxMonitorMode::size);
	if (node.modeCount > 1)
	{
		modes[:node.modeCount - 1] = node.modes[:node.modeCount - 1];
		free(node.modes);
	}
	modes[node.modeCount - 1] = mode;
	node.modes = modes;

	if (flags & WL_OUTPUT_MODE_CURRENT) mon.mode = mode;
}

// private
fn void rgfw_wlOutputSetScale(void* data, WlProxy* output, int factor)
{
	GfxMonitor* mon = &((GfxMonitorNode*)data).mon;
	mon.scale = (Vec2)factor;
}

// private
fn void rgfw_wlOutputSetName(void* data, WlProxy* output, ZString name)
{
	GfxMonitor* mon = &((GfxMonitorNode*)data).mon;
	sz n = name.len();
	if (n > 127) n = 127;
	rgfw_memcpy(&mon.name, name, n);
	mon.name[n] = 0;
}

// private
fn void rgfw_wlOutputDescription(void* data, WlProxy* output, ZString description) {}

// private
fn void rgfw_xdgOutputLogicalPos(void* data, WlProxy* xdgOutput, int x, int y)
{
	GfxMonitor* mon = &((GfxMonitorNode*)data).mon;
	mon.origin = { x, y };
}

// private
fn void rgfw_xdgOutputLogicalSize(void* data, WlProxy* xdgOutput, int width, int height)
{
	GfxMonitor* mon = &((GfxMonitorNode*)data).mon;
	float monFloatWidth = (float)mon.mode.w;
	float scaleX = monFloatWidth / (float)width;
	float dpi = scaleX * 96.0f;
	mon.pixelRatio = dpi >= 192.0f ? 2.0f : 1.0f;
	mon.mode.w = width;
	mon.mode.h = height;
}

// private
fn void rgfw_xdgOutputNothing(void* data, WlProxy* xdgOutput) {}
// private
fn void rgfw_xdgOutputNothingStr(void* data, WlProxy* xdgOutput, ZString s) {}

// private
fn void rgfw_wlOutputHandleDone(void* data, WlProxy* output)
{
	GfxMonitorNode* node = (GfxMonitorNode*)data;
	GfxMonitor* mon = &node.mon;

	if (mon.phys_size.x <= 0 || mon.phys_size.y <= 0)
	{
		mon.phys_size = { mon.mode.w / 96.0f, mon.mode.h / 96.0f };
	}

	if (node.disconnected == false) return;
	node.disconnected = true;
	rgfw_monitorCallback(_gfx_ctx.root, mon, true);
}

WlOutputListener rgfw_wlOutputListener = {
	.geometry = &rgfw_wlOutputSetGeometry, .mode = &rgfw_wlOutputHandleMode, .done = &rgfw_wlOutputHandleDone,
	.scale = &rgfw_wlOutputSetScale, .name = &rgfw_wlOutputSetName, .description = &rgfw_wlOutputDescription
};
ZxdgOutputV1Listener rgfw_xdgOutputListener = {
	.name = &rgfw_xdgOutputNothingStr, .done = &rgfw_xdgOutputNothing, .description = &rgfw_xdgOutputNothingStr,
	.logicalPosition = &rgfw_xdgOutputLogicalPos, .logicalSize = &rgfw_xdgOutputLogicalSize
};

// private
fn void rgfw_wlCreateOutputs(WlProxy* registry, uint id)
{
	// wl_output name/description events (used by rgfw_wlOutputSetName below) require version 4+.
	// If wl_seat hasn't been bound yet (compositor announced wl_output first), fall back to
	// version 4 instead of calling wlProxyGetVersion(null), which would crash.
	uint fallbackVersion = 4;
	WlProxy* output = wlRegistryBind(registry, id, &wlOutputInterface, _gfx_ctx.seat == null ? fallbackVersion : wlProxyGetVersion(_gfx_ctx.seat));
	if (output == null) return;

	GfxMonitor mon;
	char[10] defaultName;
	int n = 0;
	ZString prefix = "monitor-";
	while (prefix[n] != 0) { defaultName[n] = prefix[n]; n++; }
	defaultName[n] = (char)('0' + (_gfx_ctx.monitors.count % 10));
	defaultName[n + 1] = 0;
	rgfw_memcpy(&mon.name, &defaultName, (sz)n + 2);

	mon.scale = (Vec2)1;
	mon.pixelRatio = 1.0f;

	GfxMonitorNode* node = rgfw_monitors_add(&mon);
	if (node == null) return;

	node.modeCount = 0;
	node.disconnected = true;
	node.id = id;
	node.output = output;

	wlOutputSetUserData(output, node);
	wlOutputAddListener(output, &rgfw_wlOutputListener, node);

	if (_gfx_ctx.xdgOutputManager == null) return;

	node.xdgOutput = zxdgOutputManagerV1GetXdgOutput(_gfx_ctx.xdgOutputManager, node.output);
	zxdgOutputV1AddListener(node.xdgOutput, &rgfw_xdgOutputListener, node);
}

// private
fn void rgfw_wlSurfaceEnter(void* data, WlProxy* surface, WlProxy* output)
{
	GfxWindow* win = (GfxWindow*)data;
	GfxMonitorNode* node = (GfxMonitorNode*)wlOutputGetUserData(output);
	if (node == null) return;
	win.src.activeMonitor = node;
}

// private
fn void rgfw_wlSurfaceLeave(void* data, WlProxy* surface, WlProxy* output) {}
// private
fn void rgfw_wlSurfacePrefScale(void* data, WlProxy* surface, int scale) {}
// private
fn void rgfw_wlSurfacePrefTransform(void* data, WlProxy* surface, uint transform) {}

// private
fn void rgfw_wlDataSourceSend(void* data, WlProxy* source, ZString mimeType, int fd)
{
	if (mimeType && strstr(mimeType, "text/plain") != null)
	{
		sz length = _gfx_ctx.unixClipboard.length;
		if (_gfx_ctx.unixClipboard.data[0] == 0) length -= 1;
		write(fd, (void*)_gfx_ctx.unixClipboard.data, length);
	}
	close(fd);
}

// private
fn void rgfw_wlDataSourceCancelled(void* data, WlProxy* source)
{
	GfxContext* info = data;
	if (info.kbOwner != null && info.kbOwner.src.dataSource == source) info.kbOwner.src.dataSource = null;
	wlDataSourceDestroy(source);
}

// private
fn void rgfw_wlDataSourceNothingStr(void* data, WlProxy* source, ZString s) {}
// private
fn void rgfw_wlDataSourceNothing(void* data, WlProxy* source) {}
// private
fn void rgfw_wlDataSourceNothingU32(void* data, WlProxy* source, uint u) {}

// private
fn void rgfw_wlDataOfferNothingStr(void* data, WlProxy* offer, ZString s) {}
// private
fn void rgfw_wlDataOfferNothingU32(void* data, WlProxy* offer, uint u) {}

// private
fn void rgfw_wlDataDeviceDataOffer(void* data, WlProxy* device, WlProxy* offer)
{
	static WlDataOfferListener listener = { .offer = &rgfw_wlDataOfferNothingStr, .sourceActions = &rgfw_wlDataOfferNothingU32, .action = &rgfw_wlDataOfferNothingU32 };
	wlDataOfferAddListener(offer, &listener, null);
}

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

	if (_gfx_ctx.unixClipboard != null) rgfw_free_impl(_gfx_ctx.unixClipboard);
	_gfx_ctx.unixClipboard = (GfxDataTransfer*)rgfw_alloc_impl(GfxDataTransfer::size + (sz)n);
	char* dataPtr = (char*)_gfx_ctx.unixClipboard + GfxDataTransfer::size - 1;
	rgfw_memcpy(dataPtr, &buf, (sz)n);
	dataPtr[length - 1] = 0;
	_gfx_ctx.unixClipboard.data = dataPtr;
	_gfx_ctx.unixClipboard.type = RGFW_DATA_TEXT;
	_gfx_ctx.unixClipboard.length = length;

	close(pfds[0]);
	wlDataOfferDestroy(offer);
}

// private
fn void rgfw_wlDataDeviceNothing(void* data, WlProxy* device) {}
// private
fn void rgfw_wlDataDeviceNothingEnter(void* data, WlProxy* device, uint serial, WlProxy* surface, WlFixed x, WlFixed y, WlProxy* offer) {}
// private
fn void rgfw_wlDataDeviceNothingMotion(void* data, WlProxy* device, uint time, WlFixed x, WlFixed y) {}

// private
fn void rgfw_wlGlobalRegistryHandler(void* data, WlProxy* registry, uint id, ZString iface, uint version)
{
	GfxContext* info = data;

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

// private
fn void rgfw_wlGlobalRegistryRemove(void* data, WlProxy* registry, uint id)
{
	GfxContext* info = data;
	GfxMonitorNode* prev = info.monitors.list.head;
	GfxMonitorNode* node = null;
	if (prev == null) return;

	if (prev.id != id)
	{
		while (prev.next != null && prev.next.id != id) prev = prev.next;
		if (prev.next == null) return;
		node = prev.next;
	}
	else { node = prev; }

	rgfw_monitorCallback(_gfx_ctx.root, &node.mon, false);
	_remove_monitor(node, prev);
}

WlRegistryListener rgfw_registryListener = { .global = &rgfw_wlGlobalRegistryHandler, .globalRemove = &rgfw_wlGlobalRegistryRemove };

// private
fn void rgfw_wlRandname(char* buf)
{
	Timespec ts;
	clock_gettime(0, &ts);
	CLong r = ts.tv_nsec;
	for (int i = 0; i < 6; i++)
	{
		buf[i] = (char)('A' + (r & 15) + (r & 16) * 2);
		r >>= 5;
	}
}

// private
fn int rgfw_wlAnonymousShmOpen()
{
	char[22] name = "/RGFW-wayland-XXXXXX\0\0";
	int retries = 100;
	do
	{
		rgfw_wlRandname((char*)&name + ((ZString)&name).len() - 6);
		retries--;
		int fd = shmOpen((ZString)&name, O_RDWR | O_CREAT | O_EXCL, 384);
		if (fd >= 0) { shmUnlink((ZString)&name); return fd; }
	} while (retries > 0);
	return -1;
}

// private
fn int rgfw_wlCreateShmFile(sz size)
{
	int fd = rgfw_wlAnonymousShmOpen();
	if (fd < 0) return fd;
	if (ftruncate(fd, size) < 0) { close(fd); return -1; }
	return fd;
}


// public
fn void? GfxSurface.init(&self, void* data, int w, int h, GfxFormat format, GfxWindow* window = null)
{
	self.data = data;
	self.size = { w, h };
	self.format = format;

	int size = w * h * 4;
	int fd = rgfw_wlCreateShmFile((sz)size);
	if (fd < 0)
	{
		log::error("Failed to create a buffer", category: log::CATEGORY_VIDEO);
		return SURFACE_CREATION_FAILED~;
	}
	defer catch self.release();
	self.native.fd = fd;
	self.native.format = BGRA8;
	self.native.pool = wlShmCreatePool(_gfx_ctx.shm, fd, (int)size);
	self.native.buffer = mmap(null, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
	if (self.native.buffer == MAP_FAILED)
	{
		log::error("mmap failed", category: log::CATEGORY_VIDEO);
		self.native.buffer = null;
		return SURFACE_CREATION_FAILED~;
	}
}

// public
fn void rgfw_window_blitSurface(GfxWindow* win, GfxSurface* surface)
{
	IntVec2 min = math::min(win.size, surface.size);
	surface.native.wlBuffer = wlShmPoolCreateBuffer(surface.native.pool, 0, ...min, surface.size.x * 4, WL_SHM_FORMAT_ARGB8888);

	wlSurfaceAttach(win.src.surface, surface.native.wlBuffer, 0, 0);
	wlSurfaceDamage(win.src.surface, 0, 0, ...min);
	wlSurfaceCommit(win.src.surface);
	wlBufferDestroy(surface.native.wlBuffer);
	surface.native.wlBuffer = null;
}

// public
fn void GfxSurface.release(&self)
{
	if (!self.data) return;
	if (self.native.pool) wlShmPoolDestroy(self.native.pool);
	if (self.native.fd) close(self.native.fd);
	if (self.native.buffer) munmap(self.native.buffer, (sz)(self.size.x * self.size.y * 4));
	*self = {};
}

// public
fn void rgfw_window_setBorder(GfxWindow* win, bool border)
{
	win.internal.flags.no_border = !border;
	if (_gfx_ctx.decorationManager != null)
	{
		zxdgToplevelDecorationV1SetMode(win.src.decoration, border ? ZXDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE : ZXDG_TOPLEVEL_DECORATION_V1_MODE_CLIENT_SIDE);
	}
}

// public
fn void rgfw_window_setRawMouseModePlatform(GfxWindow* win, bool state)
{
	if (_gfx_ctx.relativePointerManager == null) return;

	if (state == false)
	{
		if (_gfx_ctx.relativePointer != null) zwpRelativePointerV1Destroy(_gfx_ctx.relativePointer);
		_gfx_ctx.relativePointer = null;
		return;
	}
	if (_gfx_ctx.relativePointer != null) return;

	_gfx_ctx.relativePointer = zwpRelativePointerManagerV1GetRelativePointer(_gfx_ctx.relativePointerManager, _gfx_ctx.wlPointer);
	static ZwpRelativePointerV1Listener listener = { .relativeMotion = &rgfw_wlRelativePointerMotion };
	zwpRelativePointerV1AddListener(_gfx_ctx.relativePointer, &listener, &_gfx_ctx);
}

// private
fn void rgfw_wlUnlockedNoop(void* data, WlProxy* p) {}

// public
fn void rgfw_window_captureMousePlatform(GfxWindow* win, bool state) => rgfw_window_captureMousePlatform_Wayland(win, state);


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

// public
fn bool rgfw_getGlobalMouse(int* x, int* y)
{
	if (x != null) *x = 0;
	if (y != null) *y = 0;
	return false;
}

// public
fn RgfwKey rgfw_physicalToMappedKey(RgfwKey key)
{
	uint keycode = rgfw_rgfwToApiKey(key);
	XkbKeycode kc = keycode + 8;
	XkbKeysym sym = xkbStateKeyGetOneSym(_gfx_ctx.xkbState, kc);
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

// public
fn IntVec2 rgfw_window_fetchSize(GfxWindow* win) => win.get_size();

// public
fn void rgfw_pollEvents()
{
	rgfw_resetPrevState();

	while (wlDisplayFlush(_gfx_ctx.wlDisplay) == -1)
	{
		if (*rgfwErrnoLocation() == RGFW_EAGAIN)
		{
			if (wlDisplayDispatchPending(_gfx_ctx.wlDisplay) == -1) return;
		}
		else { return; }
	}

	if (_gfx_ctx.wlRepeatInfoRate != 0 && _gfx_ctx.lastKey != 0)
	{
		uint now = (uint)(rgfw_unix_getTimeNS() / 1000000);
		if (now > _gfx_ctx.lastKeyTime)
		{
			rgfw_wlSendKeyEvent(_gfx_ctx.lastKey);
			_gfx_ctx.lastKeyTime = now + 1000 / (uint)_gfx_ctx.wlRepeatInfoRate;
		}
	}

	Pollfd fds;
	fds.fd = wlDisplayGetFd(_gfx_ctx.wlDisplay);
	fds.events = (short)POLLIN;

	while (true)
	{
		while (wlDisplayPrepareRead(_gfx_ctx.wlDisplay) != 0)
		{
			if (wlDisplayDispatchPending(_gfx_ctx.wlDisplay) > 0) return;
		}

		if (poll(&fds, 1, 0) == 0) { wlDisplayCancelRead(_gfx_ctx.wlDisplay); return; }

		if (fds.revents & POLLIN)
		{
			wlDisplayReadEvents(_gfx_ctx.wlDisplay);
			if (wlDisplayDispatchPending(_gfx_ctx.wlDisplay) > 0) return;
		}
		else { wlDisplayCancelRead(_gfx_ctx.wlDisplay); }
	}
}

// public
fn void rgfw_window_movePlatform(GfxWindow* win, int x, int y) { win.x = x; win.y = y; }

// public
fn void rgfw_window_resizePlatform(GfxWindow* win, int w, int h)
{
	win.width = w; win.height = h;
	if (_gfx_ctx.compositor != null)
	{
		xdgSurfaceSetWindowGeometry(win.src.xdgSurface, 0, 0, win.width, win.height);
		if (win.src.ctx.egl != null && win.src.ctx.egl.eglWindow != null) wlEglWindowResize((WlEglWindow*)win.src.ctx.egl.eglWindow, w, h, 0, 0);
	}
}

// public
fn void rgfw_window_setAspectRatio(GfxWindow* win, int w, int h)
{
	if (w == 0 && h == 0) return;
	xdgToplevelSetMaxSize(win.src.xdgToplevel, w, h);
}

// public
fn void rgfw_window_setMinSize(GfxWindow* win, int w, int h) { xdgToplevelSetMinSize(win.src.xdgToplevel, w, h); }
// public
fn void rgfw_window_setMaxSize(GfxWindow* win, int w, int h) { xdgToplevelSetMaxSize(win.src.xdgToplevel, w, h); }
// public
fn void rgfw_window_maximizePlatform(GfxWindow* win) => rgfw_toggleWaylandMaximized(win, true);
// public
fn void rgfw_window_focus(GfxWindow* win) {}
// public
fn void rgfw_window_raise(GfxWindow* win) {}

// public
fn void rgfw_window_setFullscreen(GfxWindow* win, bool fullscreen)
{
	if (fullscreen)
	{
		win.internal.flags.fullscreen = true;
		win.internal.old_bounds = win.bounds;
		xdgToplevelSetFullscreen(win.src.xdgToplevel, null);
	}
	else
	{
		win.internal.flags.fullscreen = false;
		xdgToplevelUnsetFullscreen(win.src.xdgToplevel);
	}
}

// public
fn void rgfw_window_setFloating(GfxWindow* win, bool floating) {}
// public
fn void rgfw_window_setOpacity(GfxWindow* win, char opacity) {}

// public
fn void rgfw_window_minimizePlatform(GfxWindow* win)
{
	if (rgfw_window_isMaximized(win)) return;
	win.internal.old_bounds = win.bounds;
	win.src.minimized = true;
	xdgToplevelSetMinimized(win.src.xdgToplevel);
}

// public
fn void rgfw_window_restorePlatform(GfxWindow* win) => rgfw_toggleWaylandMaximized(win, false);
// public
fn bool GfxWindow.is_floating(&win) => !win.is_fullscreen() && !rgfw_window_isMaximized(win);

// public
fn void GfxWindow.set_name(&self, String name)
{
	if (_gfx_ctx.compositor) @pool()
	{
		xdgToplevelSetTitle(self.src.xdgToplevel, name.zstr_copy(tmem));
	};
}

// public
fn void rgfw_window_setMousePassthrough(GfxWindow* win, bool passthrough) {}

// public
fn bool GfxWindow.set_icon(&win, char* data, int w, int h, GfxFormat format, GfxWindowIconType type = BOTH)
{
	if (_gfx_ctx.iconManager == null || w != h) return false;

	if (win.src.icon != null) { xdgToplevelIconV1Destroy(win.src.icon); win.src.icon = null; }

	GfxSurface*? surface = create_surface(data, w, h, format);
	if (catch surface) return false;

	int minW = w < surface.size.x ? w : surface.size.x;
	int minH = h < surface.size.y ? h : surface.size.y;
	copy_image_data(surface.native.buffer, minW, minH, surface.native.format, surface.data, surface.format, null);

	win.src.icon = xdgToplevelIconManagerV1CreateIcon(_gfx_ctx.iconManager);
	surface.native.wlBuffer = wlShmPoolCreateBuffer(surface.native.pool, 0, w, h, w * 4, WL_SHM_FORMAT_ARGB8888);
	xdgToplevelIconV1AddBuffer(win.src.icon, surface.native.wlBuffer, 1);
	xdgToplevelIconManagerV1SetIcon(_gfx_ctx.iconManager, win.src.xdgToplevel, win.src.icon);

	surface.destroy();
	return true;
}

// public
fn RgfwMouse* rgfw_createMouseStandard(GfxMouseIcon mouse)
{
	ZString cursorName;
	switch (mouse)
	{
		case NORMAL: cursorName = "left_ptr";
		case ARROW: cursorName = "left_ptr";
		case IBEAM: cursorName = "xterm";
		case CROSSHAIR: cursorName = "crosshair";
		case POINTING_HAND: cursorName = "hand2";
		case RESIZE_EW: cursorName = "sb_h_double_arrow";
		case RESIZE_NS: cursorName = "sb_v_double_arrow";
		case RESIZE_NWSE: cursorName = "top_left_corner";
		case RESIZE_NESW: cursorName = "top_right_corner";
		case RESIZE_NW: cursorName = "top_left_corner";
		case RESIZE_N: cursorName = "top_side";
		case RESIZE_NE: cursorName = "top_right_corner";
		case RESIZE_E: cursorName = "right_side";
		case RESIZE_SE: cursorName = "bottom_right_corner";
		case RESIZE_S: cursorName = "bottom_side";
		case RESIZE_SW: cursorName = "bottom_left_corner";
		case RESIZE_W: cursorName = "left_side";
		case RESIZE_ALL: cursorName = "fleur";
		case NOT_ALLOWED: cursorName = "not-allowed";
		case WAIT: cursorName = "watch";
		case PROGRESS: cursorName = "watch";
		default: return null;
	}

	WlCursor* wlcursor = wlCursorThemeGetCursor(_gfx_ctx.wlCursorTheme, cursorName);
	if (wlcursor == null) return null;
	WlCursorImage* cursorImage = wlcursor.images[0];
	WlProxy* cursorBuffer = wlCursorImageGetBuffer(cursorImage);

	GfxSurface* surface = mem::new(GfxSurface);
	surface.size = { (int)cursorImage.width, (int)cursorImage.height };
	surface.native.wlBuffer = cursorBuffer;
	return (RgfwMouse*)surface;
}

// public
fn RgfwMouse* rgfw_createMouse(char* data, int w, int h, GfxFormat format)
{
	GfxSurface*? surface = create_surface(data, w, h, format);
	if (catch surface) return null;

	surface.native.wlBuffer = wlShmPoolCreateBuffer(surface.native.pool, 0, ...surface.size, surface.size.x * 4, WL_SHM_FORMAT_ARGB8888);
	int minW = w < surface.size.x ? w : surface.size.x;
	int minH = h < surface.size.y ? h : surface.size.y;
	copy_image_data(surface.native.buffer, minW, minH, surface.native.format, surface.data, surface.format, null);
	return (RgfwMouse*)surface;
}

// public
fn bool rgfw_window_setMousePlatform(GfxWindow* win, RgfwMouse* mouse)
{
	GfxSurface* surface = (GfxSurface*)mouse;
	win.src.usingCustomCursor = true;
	wlSurfaceAttach(win.src.customCursorSurface, surface.native.wlBuffer, 0, 0);
	wlSurfaceDamage(win.src.customCursorSurface, 0, 0, ...surface.size);
	wlSurfaceCommit(win.src.customCursorSurface);
	return true;
}

// public
fn void rgfw_freeMouse(RgfwMouse* mouse)
{
	if (mouse == null) return;
	GfxSurface* surface = (GfxSurface*)mouse;
	if (surface.native.buffer != null && surface.native.wlBuffer != null) wlBufferDestroy(surface.native.wlBuffer);
	surface.destroy();
}

// public
fn void rgfw_window_moveMouse(GfxWindow* win, int x, int y)
{
	if (_gfx_ctx.wpPointerWarp != null)
	{
		wpPointerWarpV1WarpPointer(_gfx_ctx.wpPointerWarp, win.src.surface, _gfx_ctx.wlPointer, wlFixedFromInt(x), wlFixedFromInt(y), _gfx_ctx.mouseEnterSerial);
	}
}

// public
fn void rgfw_window_hide(GfxWindow* win)
{
	wlSurfaceAttach(win.src.surface, null, 0, 0);
	wlSurfaceCommit(win.src.surface);
	win.internal.flags.hide = true;
}

// public
fn void rgfw_window_showPlatform(GfxWindow* win)
{
	win.internal.flags.hide = false;
	wlSurfaceCommit(win.src.surface);
}

// public
fn void rgfw_window_flash(GfxWindow* win, GfxFlashRequest request) {}

// public
fn bool rgfw_readClipboardPtr(RgfwDataTransferType requestedType, char* buffer, sz capacity, GfxDataTransfer* data)
{
	if (requestedType != RGFW_DATA_TEXT) return false;
	if (_gfx_ctx.unixClipboard == null || _gfx_ctx.unixClipboard.length == 0)
	{
		data.length = 0;
		data.type = RGFW_DATA_NONE;
		return false;
	}
	data.length = _gfx_ctx.unixClipboard.length;
	data.type = RGFW_DATA_TEXT;
	if (buffer == null) return true;
	if (_gfx_ctx.unixClipboard.length > capacity) return false;
	rgfw_memcpy(buffer, (void*)_gfx_ctx.unixClipboard.data, _gfx_ctx.unixClipboard.length);
	data.data = buffer;
	return true;
}

WlDataSourceListener rgfw_dataSourceListener = {
	.target = &rgfw_wlDataSourceNothingStr, .action = &rgfw_wlDataSourceNothingU32,
	.dndDropPerformed = &rgfw_wlDataSourceNothing, .dndFinished = &rgfw_wlDataSourceNothing,
	.send = &rgfw_wlDataSourceSend, .cancelled = &rgfw_wlDataSourceCancelled
};

// public
fn bool rgfw_writeClipboard(GfxDataTransfer* data)
{
	if (_gfx_ctx.dataDeviceManager == null) return false;
	if (_gfx_ctx.unixClipboard != null) { rgfw_free_impl(_gfx_ctx.unixClipboard); _gfx_ctx.unixClipboard = null; }

	_gfx_ctx.unixClipboard = (GfxDataTransfer*)rgfw_alloc_impl(GfxDataTransfer::size + data.length);
	sz length = data.length;
	if (data.data[length - 1] != 0) length += 1;

	char* dataPtr = (char*)_gfx_ctx.unixClipboard + GfxDataTransfer::size - 1;
	rgfw_memcpy(dataPtr, (void*)data.data, data.length);
	dataPtr[length - 1] = 0;
	_gfx_ctx.unixClipboard.data = dataPtr;
	_gfx_ctx.unixClipboard.type = RGFW_DATA_TEXT;
	_gfx_ctx.unixClipboard.length = length;

	GfxWindow* win = _gfx_ctx.kbOwner;
	if (win.src.dataSource != null) { wlDataSourceDestroy(win.src.dataSource); win.src.dataSource = null; }

	win.src.dataSource = wlDataDeviceManagerCreateDataSource(_gfx_ctx.dataDeviceManager);
	if (win.src.dataSource == null)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_CLIPBOARD, "Could not create clipboard data source");
		return false;
	}

	wlDataSourceAddListener(win.src.dataSource, &rgfw_dataSourceListener, &_gfx_ctx);
	wlDataSourceOffer(win.src.dataSource, "text/plain;charset=utf-8");
	return true;
}

// public
fn bool rgfw_window_isHidden(GfxWindow* win) => false;
// public
fn bool rgfw_window_isMinimized(GfxWindow* win) => win.src.minimized;
// public
fn bool rgfw_window_isMaximized(GfxWindow* win) => win.src.maximized;
// public
fn void rgfw_pollMonitors() { _gfx_ctx.monitors.primary = _gfx_ctx.monitors.list.head; }

// public
fn void rgfw_monitorNode_free(GfxMonitorNode* node)
{
	if (node.output != null) wlOutputDestroy(node.output);
	if (node.xdgOutput != null) zxdgOutputV1Destroy(node.xdgOutput);
	if (node.modes != null) { rgfw_free_impl(node.modes); node.modeCount = 0; }
}

// public
fn bool rgfw_monitor_getWorkarea(GfxMonitor* monitor, int* x, int* y, int* width, int* height)
{
	if (x != null) *x = monitor.origin.x;
	if (y != null) *y = monitor.origin.y;
	if (width != null) *width = monitor.mode.w;
	if (height != null) *height = monitor.mode.h;
	return true;
}

// public
fn GfxMonitorMode[] rgfw_monitor_getModesPtr(GfxMonitor* monitor)
{
	List{GfxMonitorMode} modes;
	for (int i = 0; i < monitor.node.modeCount; i++)
	{
		modes.push(monitor.node.modes[i]);
	}
	return modes.array_view();
}

// public
fn sz rgfw_monitor_getGammaRampPtr(GfxMonitor* monitor, RgfwGammaRamp* ramp) => 0;
// public
fn bool rgfw_monitor_setGammaRamp(GfxMonitor* monitor, RgfwGammaRamp* ramp) => false;

// public
fn bool rgfw_monitor_requestMode(GfxMonitor* mon, GfxMonitorMode* mode, GfxMonitorModeRequest request)
{
	for (sz i = 0; i < mon.node.modeCount; i++)
	{
		if (rgfw_monitorModeCompare(mode, &mon.node.modes[i], request) == false) continue;
		rgfw_monitor_setMode(mon, &mon.node.modes[i]);
		return true;
	}
	return false;
}

// public
fn bool rgfw_monitor_setMode(GfxMonitor* mon, GfxMonitorMode* mode) => false;

// public
fn GfxMonitor* find_monitor(GfxWindow* win)
{
	if (win.src.activeMonitor == null) return rgfw_getPrimaryMonitor();
	return &win.src.activeMonitor.mon;
}

// public
fn ulong rgfw_unix_getTimeNS()
{
	Timespec ts;
	clock_gettime(1 /* CLOCK_MONOTONIC */, &ts);
	return (ulong)ts.tv_sec * 1000000000 + (ulong)ts.tv_nsec;
}

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

// public
fn bool rgfw_extensionSupportedPlatform_OpenGL(ZString extension, sz len) => rgfw_extensionSupportedPlatform_EGL(extension, len);


// public
fn GfxFn rgfw_getProcAddress_OpenGL(ZString procname) => rgfw_getProcAddress_EGL(procname);

// public
fn bool rgfw_window_createContextPtr_EGL(GfxWindow* win, RgfwEglContext* ctx, RgfwGlHints* hints)
{
	if (win == null || ctx == null || hints == null) return false;
	if (load_egl() == false) return false;
	if (_gfx_ctx.egl_display == null)
	{
		_gfx_ctx.egl_display = rgfw_eglGetDisplay((EglNativeDisplayType)_gfx_ctx.wlDisplay);
		if (_gfx_ctx.egl_display == null) return false;
		EglInt major = 0;
		EglInt minor = 0;
		if (rgfw_eglInitialize((EglDisplay)_gfx_ctx.egl_display, &major, &minor) == 0) return false;
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
	if (rgfw_eglChooseConfig((EglDisplay)_gfx_ctx.egl_display, (EglInt*)&cfg, &config, 1, &num) == 0 || num == 0) return false;
	rgfw_eglBindApi(hints.profile == RGFW_GL_ES ? (uint)EGL_OPENGL_ES_API : (uint)EGL_OPENGL_API);

	EglInt[8] ctxAttribs;
	RgfwAttribStack ctxStack;
	rgfw_attribStack_init(&ctxStack, (int*)&ctxAttribs, 8);
	if (hints.profile == RGFW_GL_ES) rgfw_attribStack_pushAttribs(&ctxStack, EGL_CONTEXT_CLIENT_VERSION, hints.major != 0 ? hints.major : 2);
	rgfw_attribStack_pushAttribs(&ctxStack, EGL_NONE, EGL_NONE);

	EglContext share = hints.shareEGL != null ? (EglContext)hints.shareEGL.ctx : null;
	ctx.ctx = rgfw_eglCreateContext((EglDisplay)_gfx_ctx.egl_display, config, share, (EglInt*)&ctxAttribs);
	if (ctx.ctx == null) return false;

	WlEglWindow* eglWindow = wlEglWindowCreate(win.src.surface, win.width, win.height);
	ctx.eglWindow = eglWindow;
	ctx.surface = rgfw_eglCreateWindowSurface((EglDisplay)_gfx_ctx.egl_display, config, (EglNativeWindowType)eglWindow, null);
	if (ctx.surface == null)
	{
		rgfw_eglDestroyContext((EglDisplay)_gfx_ctx.egl_display, (EglContext)ctx.ctx);
		ctx.ctx = null;
		return false;
	}
	win.src.ctx.egl = ctx;
	win.src.gfxType = RGFW_GFX_EGL;
	rgfw_window_makeCurrentContext_EGL(win);
	return true;
}

// public
fn void rgfw_window_deleteContextPtr_EGL(GfxWindow* win, RgfwEglContext* ctx)
{
	if (ctx == null || _gfx_ctx.egl_display == null) return;
	if (ctx.surface != null) rgfw_eglDestroySurface((EglDisplay)_gfx_ctx.egl_display, (EglSurface)ctx.surface);
	if (ctx.ctx != null) rgfw_eglDestroyContext((EglDisplay)_gfx_ctx.egl_display, (EglContext)ctx.ctx);
	if (ctx.eglWindow != null) wlEglWindowDestroy((WlEglWindow*)ctx.eglWindow);
	ctx.surface = null;
	ctx.ctx = null;
	ctx.eglWindow = null;
	if (win != null) win.src.ctx.egl = null;
}



// public
fn bool rgfw_window_createContextPtr_OpenGL(GfxWindow* win, RgfwGlContext* ctx, RgfwGlHints* hints)
{
	bool out = rgfw_window_createContextPtr_EGL(win, (RgfwEglContext*)ctx, hints);
	win.src.gfxType = RGFW_GFX_NATIVE_OPEN_GL;
	rgfw_window_swapInterval_OpenGL(win, 0);
	return out;
}
// public
fn void rgfw_window_deleteContextPtr_OpenGL(GfxWindow* win, RgfwGlContext* ctx) { rgfw_window_deleteContextPtr_EGL(win, (RgfwEglContext*)ctx); win.src.ctx.native = null; }
// public
fn void rgfw_window_makeCurrentContext_OpenGL(GfxWindow* win) { rgfw_window_makeCurrentContext_EGL(win); }
// public
fn void* rgfw_getCurrentContext_OpenGL() => rgfw_getCurrentContext_EGL();
// public
fn void rgfw_window_swapBuffers_OpenGL(GfxWindow* win) { rgfw_window_swapBuffers_EGL(win); }
// public
fn void rgfw_window_swapInterval_OpenGL(GfxWindow* win, int swapInterval) { rgfw_window_swapInterval_EGL(win, swapInterval); }

// public
fn int rgfw_initPlatform(String class_name, GfxInitOption flags)
{
	_gfx_ctx.wlDisplay = wlDisplayConnect(null);
	if (_gfx_ctx.wlDisplay == null)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_WAYLAND, "Failed to load Wayland display");
		return -1;
	}

	_gfx_ctx.compositor = null;
	_gfx_ctx.registry = wlDisplayGetRegistry(_gfx_ctx.wlDisplay);
	wlRegistryAddListener(_gfx_ctx.registry, &rgfw_registryListener, &_gfx_ctx);
	wlDisplayRoundtrip(_gfx_ctx.wlDisplay);

	if (_gfx_ctx.compositor == null)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_WAYLAND, "Can't find compositor.");
		return -1;
	}

	if (_gfx_ctx.wlCursorTheme == null)
	{
		_gfx_ctx.wlCursorTheme = wlCursorThemeLoad(null, 24, _gfx_ctx.shm);
		_gfx_ctx.cursorSurface = wlCompositorCreateSurface(_gfx_ctx.compositor);
	}

	char[4] blk = {0, 0, 0, 0};
	_gfx_ctx.hiddenMouse = rgfw_createMouse((char*)&blk, 1, 1, RGBA8);

	static XdgWmBaseListener wmBaseListener = { .ping = &rgfw_wlXdgWmBasePingHandler };
	xdgWmBaseAddListener(_gfx_ctx.xdgWmBase, &wmBaseListener, null);

	_gfx_ctx.xkbContext = xkbContextNew(XKB_CONTEXT_NO_FLAGS);

	static WlDataDeviceListener dataDeviceListener = {
		.dataOffer = &rgfw_wlDataDeviceDataOffer, .enter = &rgfw_wlDataDeviceNothingEnter, .leave = &rgfw_wlDataDeviceNothing,
		.motion = &rgfw_wlDataDeviceNothingMotion, .drop = &rgfw_wlDataDeviceNothing, .selection = &rgfw_wlDataDeviceSelection
	};

	if (_gfx_ctx.seat != null && _gfx_ctx.dataDeviceManager != null)
	{
		_gfx_ctx.dataDevice = wlDataDeviceManagerGetDataDevice(_gfx_ctx.dataDeviceManager, _gfx_ctx.seat);
		wlDataDeviceAddListener(_gfx_ctx.dataDevice, &dataDeviceListener, null);
	}

	return 0;
}

// public
fn void rgfw_deinitPlatform()
{
	if (_gfx_ctx.unixClipboard != null) { rgfw_free_impl(_gfx_ctx.unixClipboard); _gfx_ctx.unixClipboard = null; }
	if (_gfx_ctx.wlPointer != null) wlPointerDestroy(_gfx_ctx.wlPointer);
	if (_gfx_ctx.wlKeyboard != null) wlKeyboardDestroy(_gfx_ctx.wlKeyboard);
	if (_gfx_ctx.registry != null) wlRegistryDestroy(_gfx_ctx.registry);
	if (_gfx_ctx.decorationManager != null) zxdgDecorationManagerV1Destroy(_gfx_ctx.decorationManager);
	if (_gfx_ctx.relativePointerManager != null) zwpRelativePointerManagerV1Destroy(_gfx_ctx.relativePointerManager);
	if (_gfx_ctx.relativePointer != null) zwpRelativePointerV1Destroy(_gfx_ctx.relativePointer);
	if (_gfx_ctx.constraintManager != null) zwpPointerConstraintsV1Destroy(_gfx_ctx.constraintManager);
	if (_gfx_ctx.iconManager != null) xdgToplevelIconManagerV1Destroy(_gfx_ctx.iconManager);
	if (_gfx_ctx.xdgOutputManager != null) zxdgOutputManagerV1Destroy(_gfx_ctx.xdgOutputManager);
	if (_gfx_ctx.dataDeviceManager != null) wlDataDeviceManagerDestroy(_gfx_ctx.dataDeviceManager);
	if (_gfx_ctx.dataDevice != null) wlDataDeviceDestroy(_gfx_ctx.dataDevice);
	if (_gfx_ctx.wlCursorTheme != null) wlCursorThemeDestroy(_gfx_ctx.wlCursorTheme);
	if (_gfx_ctx.wpPointerWarp != null) wpPointerWarpV1Destroy(_gfx_ctx.wpPointerWarp);
	if (_gfx_ctx.hiddenMouse != null) rgfw_freeMouse(_gfx_ctx.hiddenMouse);
	if (_gfx_ctx.cursorSurface != null) wlSurfaceDestroy(_gfx_ctx.cursorSurface);
	if (_gfx_ctx.shm != null) wlShmDestroy(_gfx_ctx.shm);
	if (_gfx_ctx.seat != null) wlSeatRelease(_gfx_ctx.seat);
	if (_gfx_ctx.xdgWmBase != null) xdgWmBaseDestroy(_gfx_ctx.xdgWmBase);
	if (_gfx_ctx.compositor != null) wlCompositorDestroy(_gfx_ctx.compositor);
	if (_gfx_ctx.wlDisplay != null) wlDisplayDisconnect(_gfx_ctx.wlDisplay);
}

// public
fn void rgfw_initKeycodesPlatform()
{
	// Wayland keycodes come directly from the evdev-based xkb keymap (key+8);
	// no separate keycode table walk is needed the way X11's is.
}

// public
fn void rgfw_window_closePlatform(GfxWindow* win)
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
