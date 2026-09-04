module gfx @link("wayland-client", "wayland-cursor", "wayland-egl", "xkbcommon", "EGL");

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

const bool RGFW_BUILD_MACOS = false;
const bool RGFW_BUILD_X11 = false;
const bool RGFW_BUILD_WAYLAND = true;
const bool RGFW_BUILD_WINDOWS = false;
const bool RGFW_BUILD_EGL = true;

extern fn void* malloc(usz size);
extern fn void free(void* ptr);
extern fn void* memcpy(void* dest, void* src, usz count);
extern fn void* memset(void* dest, int value, usz count);
extern fn usz strlen(char* str);
extern fn char* strstr(char* haystack, char* needle);
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
	RgfwFormat format;
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
	RgfwDataTransfer* unixClipboard;
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

fn bool rgfw_loadGL()
{
	// Wayland only ever renders through EGL; "OpenGL" here just means EGL is ready.
	return rgfw_loadEGL();
}

fn void rgfw_unloadGL()
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
const int VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR = 1000006000;
const VkResult VK_ERROR_EXTENSION_NOT_PRESENT = -7;
struct VkWaylandSurfaceCreateInfoKHR { int sType; void* pNext; uint flags; WlDisplay* display; WlProxy* surface; }
alias RgfwVkCreateWaylandSurfaceKHRProc = fn VkResult(VkInstance instance, VkWaylandSurfaceCreateInfoKHR* createInfo, void* allocator, VkSurfaceKHR* surface);
char*[2] rgfw_vulkanExtensions = {"VK_KHR_surface", "VK_KHR_wayland_surface"};

fn bool rgfw_loadVulkan()
{
	if (_rgfw.vulkan_handle != null) return RGFW_TRUE;
	char*[2] names = {"libvulkan.so.1", "libvulkan.so"};
	for (usz i = 0; i < 2; i++)
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
	RgfwProc raw = rgfw_getInstanceProcAddress_Vulkan(instance, "vkCreateWaylandSurfaceKHR");
	if (raw == null) return VK_ERROR_EXTENSION_NOT_PRESENT;
	VkWaylandSurfaceCreateInfoKHR createInfo;
	createInfo.sType = VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR;
	createInfo.pNext = null;
	createInfo.flags = 0;
	createInfo.display = _rgfw.wlDisplay;
	createInfo.surface = win.src.surface;
	return ((RgfwVkCreateWaylandSurfaceKHRProc)raw)(instance, &createInfo, null, surface);
}

alias RgfwVkGetPhysicalDeviceWaylandPresentationSupportKHRProc = fn uint(VkPhysicalDevice, uint, WlDisplay*);

fn bool rgfw_getPhysicalDevicePresentationSupport_Vulkan(VkInstance instance, VkPhysicalDevice physicalDevice, uint queueFamilyIndex)
{
	RgfwProc raw = rgfw_getInstanceProcAddress_Vulkan(instance, "vkGetPhysicalDeviceWaylandPresentationSupportKHR");
	if (raw == null) return RGFW_FALSE;
	uint out = ((RgfwVkGetPhysicalDeviceWaylandPresentationSupportKHRProc)raw)(physicalDevice, queueFamilyIndex, _rgfw.wlDisplay);
	return rgfw_bool(out != 0);
}

alias WGPUInstance = void*;
alias WGPUSurface = void*;
alias WGPUSType = int;
const WGPUSType WGPU_STYPE_SURFACE_SOURCE_WAYLAND_SURFACE = 0x00000007;
struct WgpuChainedStruct { WgpuChainedStruct* next; WGPUSType sType; }
struct WgpuSurfaceDescriptor { WgpuChainedStruct* nextInChain; }
struct WgpuSurfaceSourceWaylandSurface { WgpuChainedStruct chain; WlDisplay* display; WlProxy* surface; }
extern fn WGPUSurface wgpuInstanceCreateSurface(WGPUInstance instance, WgpuSurfaceDescriptor* descriptor) @cname("wgpuInstanceCreateSurface");

fn WGPUSurface rgfw_window_createSurface_WebGPU(RgfwWindow* window, WGPUInstance instance)
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

fn RgfwFormat rgfw_nativeFormat() => RGFW_FORMAT_BGRA8;

fn bool rgfw_createSurfacePtr(char* data, int w, int h, RgfwFormat format, RgfwSurface* surface)
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

fn bool rgfw_window_createSurfacePtr(RgfwWindow* win, char* data, int w, int h, RgfwFormat format, RgfwSurface* surface) => rgfw_createSurfacePtr(data, w, h, format, surface);

fn void rgfw_window_blitSurface(RgfwWindow* win, RgfwSurface* surface)
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

fn void rgfw_surface_freePtr(RgfwSurface* surface)
{
	if (surface.native.pool != null) wlShmPoolDestroy(surface.native.pool);
	if (surface.native.fd != 0) close(surface.native.fd);
	if (surface.native.buffer != null) munmap(surface.native.buffer, (usz)(surface.w * surface.h * 4));
}

fn void rgfw_window_setBorder(RgfwWindow* win, bool border)
{
	rgfw_setBit(&win.internal.flags, RGFW_WINDOW_NO_BORDER, rgfw_bool(!border));
	if (_rgfw.decorationManager != null)
	{
		zxdgToplevelDecorationV1SetMode(win.src.decoration, border ? ZXDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE : ZXDG_TOPLEVEL_DECORATION_V1_MODE_CLIENT_SIDE);
	}
}

fn void rgfw_window_setRawMouseModePlatform(RgfwWindow* win, bool state)
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

fn void rgfw_window_captureMousePlatform(RgfwWindow* win, bool state)
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

fn RgfwWindow* rgfw_createWindowPlatform(char* name, RgfwWindowFlags flags, RgfwWindow* win)
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

fn bool rgfw_getGlobalMouse(int* x, int* y)
{
	if (x != null) *x = 0;
	if (y != null) *y = 0;
	return RGFW_FALSE;
}

fn RgfwKey rgfw_physicalToMappedKey(RgfwKey key)
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

fn bool rgfw_window_fetchSize(RgfwWindow* win, int* w, int* h) => rgfw_window_getSize(win, w, h);

fn void rgfw_pollEvents()
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

fn void rgfw_window_movePlatform(RgfwWindow* win, int x, int y) { win.x = x; win.y = y; }

fn void rgfw_window_resizePlatform(RgfwWindow* win, int w, int h)
{
	win.w = w; win.h = h;
	if (_rgfw.compositor != null)
	{
		xdgSurfaceSetWindowGeometry(win.src.xdgSurface, 0, 0, win.w, win.h);
		if (win.src.ctx.egl != null && win.src.ctx.egl.eglWindow != null) wlEglWindowResize((WlEglWindow*)win.src.ctx.egl.eglWindow, w, h, 0, 0);
	}
}

fn void rgfw_window_setAspectRatio(RgfwWindow* win, int w, int h)
{
	if (w == 0 && h == 0) return;
	xdgToplevelSetMaxSize(win.src.xdgToplevel, w, h);
}

fn void rgfw_window_setMinSize(RgfwWindow* win, int w, int h) { xdgToplevelSetMinSize(win.src.xdgToplevel, w, h); }
fn void rgfw_window_setMaxSize(RgfwWindow* win, int w, int h) { xdgToplevelSetMaxSize(win.src.xdgToplevel, w, h); }
fn void rgfw_window_maximizePlatform(RgfwWindow* win) => rgfw_toggleWaylandMaximized(win, 1);
fn void rgfw_window_focus(RgfwWindow* win) {}
fn void rgfw_window_raise(RgfwWindow* win) {}

fn void rgfw_window_setFullscreen(RgfwWindow* win, bool fullscreen)
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

fn void rgfw_window_setFloating(RgfwWindow* win, bool floating) {}
fn void rgfw_window_setOpacity(RgfwWindow* win, char opacity) {}

fn void rgfw_window_minimizePlatform(RgfwWindow* win)
{
	if (rgfw_window_isMaximized(win)) return;
	win.internal.oldX = win.x; win.internal.oldY = win.y; win.internal.oldW = win.w; win.internal.oldH = win.h;
	win.src.minimized = RGFW_TRUE;
	xdgToplevelSetMinimized(win.src.xdgToplevel);
}

fn void rgfw_window_restorePlatform(RgfwWindow* win) => rgfw_toggleWaylandMaximized(win, RGFW_FALSE);
fn bool rgfw_window_isFloating(RgfwWindow* win) => rgfw_bool(!rgfw_window_isFullscreen(win) && !rgfw_window_isMaximized(win));

fn void rgfw_window_setName(RgfwWindow* win, char* name)
{
	if (name == null) name = "";
	if (_rgfw.compositor != null) xdgToplevelSetTitle(win.src.xdgToplevel, name);
}

fn void rgfw_window_setMousePassthrough(RgfwWindow* win, bool passthrough) {}

fn bool rgfw_window_setIconEx(RgfwWindow* win, char* data, int w, int h, RgfwFormat format, RgfwIcon type)
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

fn RgfwMouse* rgfw_createMouseStandard(RgfwMouseIcon mouse)
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

fn RgfwMouse* rgfw_createMouse(char* data, int w, int h, RgfwFormat format)
{
	RgfwSurface* surface = rgfw_createSurface(data, w, h, format);
	if (surface == null) return null;

	surface.native.wlBuffer = wlShmPoolCreateBuffer(surface.native.pool, 0, surface.w, surface.h, surface.w * 4, WL_SHM_FORMAT_ARGB8888);
	int minW = w < surface.w ? w : surface.w;
	int minH = h < surface.h ? h : surface.h;
	rgfw_copyImageData(surface.native.buffer, minW, minH, surface.native.format, surface.data, surface.format, null);
	return (RgfwMouse*)surface;
}

fn bool rgfw_window_setMousePlatform(RgfwWindow* win, RgfwMouse* mouse)
{
	RgfwSurface* surface = (RgfwSurface*)mouse;
	win.src.usingCustomCursor = RGFW_TRUE;
	wlSurfaceAttach(win.src.customCursorSurface, surface.native.wlBuffer, 0, 0);
	wlSurfaceDamage(win.src.customCursorSurface, 0, 0, surface.w, surface.h);
	wlSurfaceCommit(win.src.customCursorSurface);
	return RGFW_TRUE;
}

fn void rgfw_freeMouse(RgfwMouse* mouse)
{
	if (mouse == null) return;
	RgfwSurface* surface = (RgfwSurface*)mouse;
	if (surface.native.buffer != null && surface.native.wlBuffer != null) wlBufferDestroy(surface.native.wlBuffer);
	rgfw_surface_free(surface);
}

fn void rgfw_window_moveMouse(RgfwWindow* win, int x, int y)
{
	if (_rgfw.wpPointerWarp != null)
	{
		wpPointerWarpV1WarpPointer(_rgfw.wpPointerWarp, win.src.surface, _rgfw.wlPointer, wlFixedFromInt(x), wlFixedFromInt(y), _rgfw.mouseEnterSerial);
	}
}

fn void rgfw_window_hide(RgfwWindow* win)
{
	wlSurfaceAttach(win.src.surface, null, 0, 0);
	wlSurfaceCommit(win.src.surface);
	win.internal.flags |= RGFW_WINDOW_HIDE;
}

fn void rgfw_window_showPlatform(RgfwWindow* win)
{
	win.internal.flags &= ~(uint)RGFW_WINDOW_HIDE;
	wlSurfaceCommit(win.src.surface);
}

fn void rgfw_window_flash(RgfwWindow* win, RgfwFlashRequest request) {}

fn bool rgfw_readClipboardPtr(RgfwDataTransferType requestedType, char* buffer, usz capacity, RgfwDataTransfer* data)
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

fn bool rgfw_writeClipboard(RgfwDataTransfer* data)
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

fn bool rgfw_window_isHidden(RgfwWindow* win) => RGFW_FALSE;
fn bool rgfw_window_isMinimized(RgfwWindow* win) => win.src.minimized;
fn bool rgfw_window_isMaximized(RgfwWindow* win) => win.src.maximized;
fn void rgfw_pollMonitors() { _rgfw.monitors.primary = _rgfw.monitors.list.head; }

fn void rgfw_monitorNode_free(RgfwMonitorNode* node)
{
	if (node.output != null) wlOutputDestroy(node.output);
	if (node.xdgOutput != null) zxdgOutputV1Destroy(node.xdgOutput);
	if (node.modes != null) { rgfw_free_impl(node.modes); node.modeCount = 0; }
}

fn bool rgfw_monitor_getWorkarea(RgfwMonitor* monitor, int* x, int* y, int* width, int* height)
{
	if (x != null) *x = monitor.x;
	if (y != null) *y = monitor.y;
	if (width != null) *width = monitor.mode.w;
	if (height != null) *height = monitor.mode.h;
	return RGFW_TRUE;
}

fn usz rgfw_monitor_getModesPtr(RgfwMonitor* monitor, RgfwMonitorMode** modes)
{
	if (modes != null) rgfw_memcpy(*modes, monitor.node.modes, monitor.node.modeCount * RgfwMonitorMode::size);
	return monitor.node.modeCount;
}

fn usz rgfw_monitor_getGammaRampPtr(RgfwMonitor* monitor, RgfwGammaRamp* ramp) => 0;
fn bool rgfw_monitor_setGammaRamp(RgfwMonitor* monitor, RgfwGammaRamp* ramp) => RGFW_FALSE;

fn bool rgfw_monitor_requestMode(RgfwMonitor* mon, RgfwMonitorMode* mode, RgfwModeRequest request)
{
	for (usz i = 0; i < mon.node.modeCount; i++)
	{
		if (rgfw_monitorModeCompare(mode, &mon.node.modes[i], request) == RGFW_FALSE) continue;
		rgfw_monitor_setMode(mon, &mon.node.modes[i]);
		return RGFW_TRUE;
	}
	return RGFW_FALSE;
}

fn bool rgfw_monitor_setMode(RgfwMonitor* mon, RgfwMonitorMode* mode) => RGFW_FALSE;

fn RgfwMonitor* rgfw_window_getMonitor(RgfwWindow* win)
{
	if (win.src.activeMonitor == null) return rgfw_getPrimaryMonitor();
	return &win.src.activeMonitor.mon;
}

fn ulong rgfw_unix_getTimeNS()
{
	Timespec ts;
	clock_gettime(1 /* CLOCK_MONOTONIC */, &ts);
	return (ulong)ts.tv_sec * 1000000000 + (ulong)ts.tv_nsec;
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

fn bool rgfw_extensionSupportedPlatform_OpenGL(char* extension, usz len) => rgfw_extensionSupportedPlatform_EGL(extension, len);

fn RgfwProc rgfw_getProcAddress_EGL(char* procname)
{
	if (rgfw_eglGetProcAddress == null) return null;
	return rgfw_eglGetProcAddress(procname);
}
fn RgfwProc rgfw_getProcAddress_OpenGL(char* procname) => rgfw_getProcAddress_EGL(procname);

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

fn bool rgfw_window_createContextPtr_OpenGL(RgfwWindow* win, RgfwGlContext* ctx, RgfwGlHints* hints)
{
	bool out = rgfw_window_createContextPtr_EGL(win, (RgfwEglContext*)ctx, hints);
	win.src.gfxType = RGFW_GFX_NATIVE_OPEN_GL;
	rgfw_window_swapInterval_OpenGL(win, 0);
	return out;
}
fn void rgfw_window_deleteContextPtr_OpenGL(RgfwWindow* win, RgfwGlContext* ctx) { rgfw_window_deleteContextPtr_EGL(win, (RgfwEglContext*)ctx); win.src.ctx.native = null; }
fn void rgfw_window_makeCurrentContext_OpenGL(RgfwWindow* win) { rgfw_window_makeCurrentContext_EGL(win); }
fn void* rgfw_getCurrentContext_OpenGL() => rgfw_getCurrentContext_EGL();
fn void rgfw_window_swapBuffers_OpenGL(RgfwWindow* win) { rgfw_window_swapBuffers_EGL(win); }
fn void rgfw_window_swapInterval_OpenGL(RgfwWindow* win, int swapInterval) { rgfw_window_swapInterval_EGL(win, swapInterval); }

fn int rgfw_initPlatform(char* className, RgfwInitFlags flags)
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

fn void rgfw_deinitPlatform()
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

fn void rgfw_initKeycodesPlatform()
{
	// Wayland keycodes come directly from the evdev-based xkb keymap (key+8);
	// no separate keycode table walk is needed the way X11's is.
}

fn void rgfw_window_closePlatform(RgfwWindow* win)
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
