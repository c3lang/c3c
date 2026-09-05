module std::gfx @link("wayland-client", "wayland-cursor", "wayland-egl", "xkbcommon", "EGL");

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

struct RgfwMonitorNode
{
	GfxMonitor mon;
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



struct RgfwInfo
{
	GfxWindow* root;
	int windowCount;
	RgfwMouse* hiddenMouse;
	RgfwMouse*[21] standardMice;
	RgfwDebugFunc debugCallbackSrc;
	GfxGenericFunc[25] callbacks;
	GfxEvent[MAX_EVENTS] events;
	int eventBottom;
	int eventLen;
	bool queueEvents;
	bool polledEvents;
	uint[RGFW_KEY_LAST] apiKeycodes;
	RgfwKey[256] keycodes;
	bool stopCheckEvents_bool;
	ulong timerOffset;
	GfxDataTransfer* clipboard;
	bool dndBuild;
	GfxDataDropNode* dndRoot;
	GfxDataDropNode* dndCur;
	GfxMonitors monitors;
	GfxWindow* current;
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
	GfxDataTransfer* unixClipboard;
	GfxWindow* mouseOwner;
	GfxWindow* kbOwner;
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
fn usz rgfw_sizeofSurface() { return GfxSurface::size; }
fn usz rgfw_sizeofWindow() { return GfxWindow::size; }
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
	if (_rgfw.egl_handle != null) return true;
	char*[5] names = {"libEGL.dylib", "libEGL.1.dylib", "libEGL.so.1", "libEGL.so", "EGL.dll"};
	for (usz i = 0; i < 5; i++)
	{
		_rgfw.egl_handle = dlopen(names[i], RTLD_LAZY | RTLD_LOCAL);
		if (_rgfw.egl_handle != null) break;
	}
	if (_rgfw.egl_handle == null) return false;
	void* getProc = dlsym(_rgfw.egl_handle, "eglGetProcAddress");
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
	if (_rgfw.vulkan_handle != null) return true;
	char*[2] names = {"libvulkan.so.1", "libvulkan.so"};
	for (usz i = 0; i < 2; i++)
	{
		_rgfw.vulkan_handle = dlopen(names[i], RTLD_LAZY | RTLD_LOCAL);
		if (_rgfw.vulkan_handle != null) break;
	}
	if (_rgfw.vulkan_handle == null) return false;
	void* proc = dlsym(_rgfw.vulkan_handle, "vkGetInstanceProcAddr");
	if (proc == null) return false;
	_rgfw.vkGetInstanceProcAddress = (RgfwVkGetInstanceProcAddressProc)proc;
	return true;
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

fn VkResult rgfw_window_createSurface_Vulkan(GfxWindow* win, VkInstance instance, VkSurfaceKHR* surface)
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
	if (raw == null) return false;
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

fn WGPUSurface rgfw_window_createSurface_WebGPU(GfxWindow* window, WGPUInstance instance)
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


fn int rgfw_init_ptr(char* className, RgfwInitFlags flags, RgfwInfo* info)
{
	if (info == _rgfw || info == null) return 1;
	if (className == null) className = "RGFW";
	rgfw_setInfo(info);
	rgfw_memzero(_rgfw, RgfwInfo::size);
	if (flags & RGFW_INIT_OPEN_GL && rgfw_loadGL() == false) return -1;
	if (flags & RGFW_INIT_EGL && rgfw_loadEGL() == false) return -1;
	if (flags & RGFW_INIT_VULKAN && rgfw_loadVulkan() == false) return -1;
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


fn GfxWindow* rgfw_createWindowPtr(char* name, int x, int y, int w, int h, RgfwWindowFlags flags, GfxWindow* win)
{
	if (win == null || _rgfw == null) return null;
	if (name == null) name = "";
	rgfw_memzero(win, GfxWindow::size);
	_rgfw.windowCount++;
	if (_rgfw.root == null) rgfw_setRootWindow(win);
	win.x = x; win.y = y; win.w = w; win.h = h;
	win.internal.mouse = _rgfw.standardMice[(usz)RGFW_MOUSE_NORMAL];
	win.internal.flags = flags;
	win.internal.enabledEvents = RGFW_ALL_EVENT_FLAGS;
	RgfwWindowFlags reservedFlags = flags & RGFW_WINDOW_SCALE_TO_MONITOR;
	GfxWindow* ret = rgfw_createWindowPlatform(name, flags, win);
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


fn void rgfw_window_closePtr(GfxWindow* win)
{
	if (win == null) return;
	if (win.internal.captureMouse) rgfw_window_captureMouse(win, false);
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
	GfxDataDropNode* node = _rgfw.dndRoot;
	while (node != null)
	{
		GfxDataDropNode* next = node.next;
		rgfw_free_impl(node);
		node = next;
	}
	_rgfw.dndRoot = null;
	_rgfw.dndCur = null;
}


fn RgfwWindowSrc* rgfw_window_getSrc(GfxWindow* win) => &win.src;
fn bool rgfw_window_getPosition(GfxWindow* win, int* x, int* y) { if (x != null) *x = win.x; if (y != null) *y = win.y; return true; }
fn bool rgfw_window_getSize(GfxWindow* win, int* w, int* h) { if (w != null) *w = win.w; if (h != null) *h = win.h; return true; }
fn uint rgfw_window_getFlags(GfxWindow* win) => win.internal.flags;
fn RgfwKey rgfw_window_getExitKey(GfxWindow* win) => win.internal.exitKey;
fn void rgfw_window_setExitKey(GfxWindow* win, RgfwKey key) { win.internal.exitKey = key; }
fn void rgfw_window_setEnabledEvents(GfxWindow* win, RgfwEventFlag events) { win.internal.enabledEvents = events; }
fn RgfwEventFlag rgfw_window_getEnabledEvents(GfxWindow* win) => win.internal.enabledEvents;
fn void rgfw_window_setDisabledEvents(GfxWindow* win, RgfwEventFlag events) { rgfw_window_setEnabledEvents(win, RGFW_ALL_EVENT_FLAGS & ~events); }
fn void rgfw_window_setEventState(GfxWindow* win, RgfwEventFlag event, bool state) { rgfw_setBit(&win.internal.enabledEvents, event, state); }
fn void* rgfw_window_getUserPtr(GfxWindow* win) => win.userPtr;
fn void rgfw_window_setUserPtr(GfxWindow* win, void* ptr) { win.userPtr = ptr; }

fn bool rgfw_window_getSizeInPixels(GfxWindow* win, int* w, int* h)
{
	GfxMonitor* mon = rgfw_window_getMonitor(win);
	if (mon == null) return false;
	if (w != null) *w = (int)((float)win.w * mon.pixelRatio);
	if (h != null) *h = (int)((float)win.h * mon.pixelRatio);
	return true;
}

fn bool rgfw_window_didMouseLeave(GfxWindow* win) => rgfw_bool(_rgfw.windowState.winLeave == win && _rgfw.windowState.mouseLeave);
fn bool rgfw_window_didMouseEnter(GfxWindow* win) => rgfw_bool(_rgfw.windowState.win == win && _rgfw.windowState.mouseEnter);
fn bool rgfw_window_isMouseInside(GfxWindow* win) => win.internal.mouseInside;
fn bool rgfw_window_isDataDragging(GfxWindow* win) => rgfw_window_getDataDrag(win, null, null);
fn bool rgfw_window_didDataDrop(GfxWindow* win) => rgfw_bool(rgfw_window_getDataDrop(win) != null);

fn bool rgfw_window_getDataDrag(GfxWindow* win, int* x, int* y)
{
	if (_rgfw.windowState.win != win || _rgfw.windowState.dataDragging == false) return false;
	if (x != null) *x = _rgfw.windowState.dropX;
	if (y != null) *y = _rgfw.windowState.dropY;
	return true;
}

fn GfxDataDropNode* rgfw_window_getDataDrop(GfxWindow* win)
{
	if (_rgfw.windowState.win != win || _rgfw.windowState.dataDrop == false) return null;
	return _rgfw.dndRoot;
}

fn void rgfw_setRootWindow(GfxWindow* win) { _rgfw.root = win; }
fn GfxWindow* rgfw_getRootWindow() { return _rgfw.root; }

fn void rgfw_setBit(uint* value, uint mask, bool set)
{
	if (set) { *value |= mask; }
	else { *value &= ~mask; }
}


fn void rgfw_window_center(GfxWindow* win)
{
	GfxMonitor* mon = rgfw_window_getMonitor(win);
	if (mon == null) return;
	rgfw_window_move(win, mon.x + ((mon.mode.w - win.w) / 2), mon.y + ((mon.mode.h - win.h) / 2));
}

fn bool rgfw_monitor_scaleToWindow(GfxMonitor* mon, GfxWindow* win)
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


RgfwColorLayout[6] rgfw_layouts = {
	{0, 1, 2, 3, 3},
	{2, 1, 0, 3, 3},
	{0, 1, 2, 3, 4},
	{1, 2, 3, 0, 4},
	{2, 1, 0, 3, 4},
	{3, 2, 1, 0, 4},
};



fn void rgfw_surface_setConvertFunc(GfxSurface* surface, RgfwConvertImageDataFunc func) { surface.convertFunc = func; }
fn RgfwNativeImage* rgfw_surface_getNativeImage(GfxSurface* surface) => &surface.native;
fn void rgfw_surface_free(GfxSurface* surface) { rgfw_surface_freePtr(surface); rgfw_free_impl(surface); }

fn GfxSurface* rgfw_window_createSurface(GfxWindow* win, char* data, int w, int h, RgfwFormat format)
{
	GfxSurface* surface = (GfxSurface*)rgfw_alloc_impl(GfxSurface::size);
	if (surface == null) return null;
	rgfw_memzero(surface, GfxSurface::size);
	rgfw_window_createSurfacePtr(win, data, w, h, format, surface);
	return surface;
}

fn RgfwMonitorNode* rgfw_monitors_add(GfxMonitor* mon)
{
	RgfwMonitorNode* node = (RgfwMonitorNode*)rgfw_alloc_impl(RgfwMonitorNode::size);
	if (node == null) return null;
	rgfw_memzero(node, RgfwMonitorNode::size);
	if (_rgfw.monitors.list.head == null) { _rgfw.monitors.list.head = node; }
	else { _rgfw.monitors.list.cur.next = node; }
	_rgfw.monitors.list.cur = node;
	if (mon != null) node.mon = *mon;
	node.mon.node = node;
	node.disconnected = false;
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
		if (node.disconnected != false)
		{
			rgfw_monitorCallback(_rgfw.root, &node.mon, false);
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

fn GfxMonitor** rgfw_getMonitors(usz* len)
{
	if (len != null) *len = 0;
	usz count = 0;
	if (rgfw_getMonitorsPtr(0, null, &count) == false || count == 0) return null;
	GfxMonitor** monitors = (GfxMonitor**)rgfw_alloc_impl(GfxMonitor*::size * count);
	if (rgfw_getMonitorsPtr(count, monitors, &count) == false)
	{
		rgfw_free_impl(monitors);
		return null;
	}
	if (len != null) *len = count;
	return monitors;
}

fn bool rgfw_getMonitorsPtr(usz max, GfxMonitor** monitors, usz* len)
{
	if (len != null) *len = _rgfw.monitors.count;
	if (monitors == null || max == 0) return true;
	if (len != null) *len = max;
	usz i = 0;
	RgfwMonitorNode* cur_node = _rgfw.monitors.list.head;
	while (cur_node != null && i < max)
	{
		monitors[i] = &cur_node.mon;
		i++;
		cur_node = cur_node.next;
	}
	return true;
}

fn GfxMonitor* rgfw_getPrimaryMonitor()
{
	if (_rgfw.monitors.primary == null) _rgfw.monitors.primary = _rgfw.monitors.list.head;
	if (_rgfw.monitors.primary == null) return null;
	return &_rgfw.monitors.primary.mon;
}

fn RgfwMonitorMode* rgfw_monitor_getModes(GfxMonitor* monitor, usz* count)
{
	usz num = rgfw_monitor_getModesPtr(monitor, null);
	RgfwMonitorMode* modes = (RgfwMonitorMode*)rgfw_alloc_impl(num * RgfwMonitorMode::size);
	num = rgfw_monitor_getModesPtr(monitor, &modes);
	if (count != null) *count = num;
	return modes;
}

fn void rgfw_freeModes(RgfwMonitorMode* modes) { rgfw_free_impl(modes); }

fn bool rgfw_monitor_findClosestMode(GfxMonitor* monitor, RgfwMonitorMode* mode, RgfwMonitorMode* closest)
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
	return chosen == null ? false : true;
}

fn bool rgfw_monitor_getPosition(GfxMonitor* monitor, int* x, int* y) { if (x != null) *x = monitor.x; if (y != null) *y = monitor.y; return true; }
fn char* rgfw_monitor_getName(GfxMonitor* monitor) => &monitor.name[0];
fn bool rgfw_monitor_getScale(GfxMonitor* monitor, float* x, float* y) { if (x != null) *x = monitor.scaleX; if (y != null) *y = monitor.scaleY; return true; }
fn bool rgfw_monitor_getPhysicalSize(GfxMonitor* monitor, float* w, float* h) { if (w != null) *w = monitor.physW; if (h != null) *h = monitor.physH; return true; }
fn void rgfw_monitor_setUserPtr(GfxMonitor* monitor, void* userPtr) { monitor.userPtr = userPtr; }
fn void* rgfw_monitor_getUserPtr(GfxMonitor* monitor) => monitor.userPtr;
fn bool rgfw_monitor_getMode(GfxMonitor* monitor, RgfwMonitorMode* mode) { if (mode != null) *mode = monitor.mode; return true; }

fn RgfwGammaRamp* rgfw_monitor_getGammaRamp(GfxMonitor* monitor)
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

fn bool rgfw_monitor_setGammaPtr(GfxMonitor* monitor, float gamma, ushort* ptr, usz count)
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

fn bool rgfw_monitor_setGamma(GfxMonitor* monitor, float gamma)
{
	usz count = rgfw_monitor_getGammaRampPtr(monitor, null);
	ushort* ptr = (ushort*)rgfw_alloc_impl(count * ushort::size);
	bool ret = rgfw_monitor_setGammaPtr(monitor, gamma, ptr, count);
	rgfw_free_impl(ptr);
	return ret;
}


fn void rgfw_window_captureMouse(GfxWindow* win, bool state) { win.internal.captureMouse = state; rgfw_window_captureMousePlatform(win, state); }
fn void rgfw_window_setRawMouseMode(GfxWindow* win, bool state) { win.internal.rawMouse = state; rgfw_window_setRawMouseModePlatform(win, state); }
fn void rgfw_setRawMouseMode(bool state) { _rgfw.rawMouse = state; if (_rgfw.root != null) rgfw_window_setRawMouseModePlatform(_rgfw.root, state); }
fn void rgfw_window_captureRawMouse(GfxWindow* win, bool state) { rgfw_window_captureMouse(win, state); rgfw_window_setRawMouseMode(win, state); }
fn bool rgfw_window_isRawMouseMode(GfxWindow* win) => rgfw_bool(win.internal.rawMouse);
fn bool rgfw_window_isCaptured(GfxWindow* win) => rgfw_bool(win.internal.captureMouse);

fn void rgfw_keyUpdateKeyMod(GfxWindow* win, RgfwKeymod mod, bool value) { if (value) { win.internal.mod |= mod; } else { win.internal.mod &= ~mod; } }
fn void rgfw_keyUpdateKeyModsEx(GfxWindow* win, bool capital, bool numlock, bool control, bool alt, bool shift, bool super, bool scroll)
{
	rgfw_keyUpdateKeyMod(win, RGFW_MOD_CAPS_LOCK, capital);
	rgfw_keyUpdateKeyMod(win, RGFW_MOD_NUM_LOCK, numlock);
	rgfw_keyUpdateKeyMod(win, RGFW_MOD_CONTROL, control);
	rgfw_keyUpdateKeyMod(win, RGFW_MOD_ALT, alt);
	rgfw_keyUpdateKeyMod(win, RGFW_MOD_SHIFT, shift);
	rgfw_keyUpdateKeyMod(win, RGFW_MOD_SUPER, super);
	rgfw_keyUpdateKeyMod(win, RGFW_MOD_SCROLL_LOCK, scroll);
}

fn void rgfw_keyUpdateKeyMods(GfxWindow* win, bool capital, bool numlock, bool scroll)
{
	rgfw_keyUpdateKeyModsEx(win, capital, numlock,
		rgfw_bool(is_key_down(RGFW_KEY_CONTROL_L) || is_key_down(RGFW_KEY_CONTROL_R)),
		rgfw_bool(is_key_down(RGFW_KEY_ALT_L) || is_key_down(RGFW_KEY_ALT_R)),
		rgfw_bool(is_key_down(RGFW_KEY_SHIFT_L) || is_key_down(RGFW_KEY_SHIFT_R)),
		rgfw_bool(is_key_down(RGFW_KEY_SUPER_L) || is_key_down(RGFW_KEY_SUPER_R)),
		scroll);
}

fn void rgfw_window_showMouseFlags(GfxWindow* win, bool show)
{
	if (show && (win.internal.flags & RGFW_WINDOW_HIDE_MOUSE)) { win.internal.flags ^= RGFW_WINDOW_HIDE_MOUSE; }
	else if (!show && !(win.internal.flags & RGFW_WINDOW_HIDE_MOUSE)) { win.internal.flags |= RGFW_WINDOW_HIDE_MOUSE; }
}

fn bool rgfw_window_isMouseHidden(GfxWindow* win) => rgfw_bool(win.internal.flags & RGFW_WINDOW_HIDE_MOUSE);
fn bool rgfw_window_borderless(GfxWindow* win) => rgfw_bool(win.internal.flags & RGFW_WINDOW_NO_BORDER);
fn bool rgfw_window_isFullscreen(GfxWindow* win) => rgfw_bool(win.internal.flags & RGFW_WINDOW_FULLSCREEN);
fn bool rgfw_window_allowsDND(GfxWindow* win) => rgfw_bool(win.internal.flags & RGFW_WINDOW_ALLOW_DND);
fn bool rgfw_window_setMouseDefault(GfxWindow* win) => rgfw_window_setMouseStandard(win, RGFW_MOUSE_NORMAL);
fn bool rgfw_window_setMouseStandard(GfxWindow* win, RgfwMouseIcon icon) => rgfw_window_setMouse(win, _rgfw.standardMice[(usz)icon]);
fn bool rgfw_window_setMouse(GfxWindow* win, RgfwMouse* mouse) { if (mouse != _rgfw.hiddenMouse) win.internal.mouse = mouse; return rgfw_window_setMousePlatform(win, mouse); }
fn void rgfw_window_setDND(GfxWindow* win, bool allow) { rgfw_setBit(&win.internal.flags, RGFW_WINDOW_ALLOW_DND, allow); }
fn void rgfw_window_showMouse(GfxWindow* win, bool show) { rgfw_window_showMouseFlags(win, show); if (show == false) { rgfw_window_setMouse(win, _rgfw.hiddenMouse); } else { rgfw_window_setMouse(win, win.internal.mouse); } }

fn bool rgfw_isLatin(char* string, usz length)
{
	for (usz i = 0; i < length; i++) if ((char)string[i] >= 0x80) return true;
	return false;
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

fn void rgfw_window_show(GfxWindow* win) { if (win.internal.flags & RGFW_WINDOW_FOCUS_ON_SHOW) rgfw_window_focus(win); win.internal.flags &= ~((uint)RGFW_WINDOW_HIDE); rgfw_window_showPlatform(win); if (win.internal.flags & RGFW_WINDOW_MAXIMIZE) rgfw_window_maximize(win); }
fn void rgfw_window_restore(GfxWindow* win) { rgfw_window_restorePlatform(win); win.internal.flags &= ~((uint)RGFW_WINDOW_MAXIMIZE); rgfw_window_show(win); rgfw_window_move(win, win.internal.oldX, win.internal.oldY); rgfw_window_resize(win, win.internal.oldW, win.internal.oldH); }
fn void rgfw_window_minimize(GfxWindow* win) { rgfw_window_minimizePlatform(win); win.internal.flags &= ~((uint)RGFW_WINDOW_MAXIMIZE); }
fn void rgfw_window_maximize(GfxWindow* win) { win.internal.oldX = win.x; win.internal.oldY = win.y; win.internal.oldW = win.w; win.internal.oldH = win.h; win.internal.flags |= RGFW_WINDOW_MAXIMIZE; rgfw_window_fetchSize(win, null, null); rgfw_window_maximizePlatform(win); }
fn void rgfw_window_move(GfxWindow* win, int x, int y) { win.x = x; win.y = y; rgfw_window_movePlatform(win, x, y); win.internal.flags &= ~((uint)RGFW_WINDOW_MAXIMIZE); }
fn void rgfw_window_resize(GfxWindow* win, int w, int h) { win.w = w; win.h = h; rgfw_window_resizePlatform(win, w, h); win.internal.flags &= ~((uint)RGFW_WINDOW_MAXIMIZE); }

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

fn void rgfw_wlSetOpaque(GfxWindow* win)
{
	WlProxy* region = wlCompositorCreateRegion(_rgfw.compositor);
	if (region == null) return;
	wlRegionAdd(region, 0, 0, win.w, win.h);
	wlSurfaceSetOpaqueRegion(win.src.surface, region);
	wlRegionDestroy(region);
}

fn void rgfw_toggleWaylandMaximized(GfxWindow* win, bool maximized)
{
	win.src.maximized = maximized;
	if (maximized) { xdgToplevelSetMaximized(win.src.xdgToplevel); }
	else { xdgToplevelUnsetMaximized(win.src.xdgToplevel); }
}

fn void rgfw_wlXdgWmBasePingHandler(void* data, WlProxy* wmBase, uint serial) { xdgWmBasePong(wmBase, serial); }

fn void rgfw_wlXdgSurfaceConfigureHandler(void* data, WlProxy* xdgSurface, uint serial)
{
	xdgSurfaceAckConfigure(xdgSurface, serial);
	GfxWindow* win = (GfxWindow*)data;
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

	win.src.configured = true;
}

fn void rgfw_wlXdgToplevelConfigureHandler(void* data, WlProxy* toplevel, int width, int height, WlArray* states)
{
	GfxWindow* win = (GfxWindow*)data;
	win.src.pendingActivated = false;
	win.src.pendingMaximized = false;
	win.src.resizing = false;

	if (states != null)
	{
		uint* state = (uint*)states.data;
		usz count = states.size / uint::size;
		for (usz i = 0; i < count; i++)
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

	if (width != 0 && height != 0 && (win.w != width || win.h != height))
	{
		win.src.resizing = true;
		win.src.w = width; win.w = width;
		win.src.h = height; win.h = height;
	}
}

fn void rgfw_wlXdgToplevelCloseHandler(void* data, WlProxy* toplevel)
{
	GfxWindow* win = (GfxWindow*)data;
	if (!win.internal.shouldClose) rgfw_windowCloseCallback(win);
}

fn void rgfw_wlXdgDecorationConfigureHandler(void* data, WlProxy* decoration, uint mode)
{
	GfxWindow* win = (GfxWindow*)data;
	if (mode != win.src.decorationMode) win.src.decorationMode = mode;
}

fn void rgfw_wlShmFormatHandler(void* data, WlProxy* shm, uint format) {}

fn void rgfw_wlRelativePointerMotion(void* data, WlProxy* relPointer, uint timeHi, uint timeLo, WlFixed dx, WlFixed dy, WlFixed dxUnaccel, WlFixed dyUnaccel)
{
	RgfwInfo* info = (RgfwInfo*)data;
	GfxWindow* win = info.mouseOwner;
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
	GfxWindow* win = (GfxWindow*)wlSurfaceGetUserData(surface);
	info.mouseEnterSerial = serial;
	win.internal.mouseInside = true;
	info.windowState.mouseEnter = true;
	info.mouseOwner = win;

	if (win.src.usingCustomCursor) { wlPointerSetCursor(pointer, serial, win.src.customCursorSurface, 0, 0); }
	else { rgfw_window_setMouseDefault(win); }

	int x = (int)wlFixedToDouble(surfaceX);
	int y = (int)wlFixedToDouble(surfaceY);
	rgfw_mouseNotifyCallback(win, x, y, true);
}

fn void rgfw_wlPointerLeave(void* data, WlProxy* pointer, uint serial, WlProxy* surface)
{
	GfxWindow* win = (GfxWindow*)wlSurfaceGetUserData(surface);
	RgfwInfo* info = (RgfwInfo*)data;
	if (info.mouseOwner == win) info.mouseOwner = null;
	rgfw_mouseNotifyCallback(win, win.internal.lastMouseX, win.internal.lastMouseY, false);
}

fn void rgfw_wlPointerMotion(void* data, WlProxy* pointer, uint time, WlFixed x, WlFixed y)
{
	RgfwInfo* info = (RgfwInfo*)data;
	GfxWindow* win = info.mouseOwner;
	if (win == null) return;
	int cx = (int)wlFixedToDouble(x);
	int cy = (int)wlFixedToDouble(y);
	rgfw_mouseMotionCallback(win, cx, cy);
}

fn void rgfw_wlPointerButton(void* data, WlProxy* pointer, uint serial, uint time, uint button, uint state)
{
	RgfwInfo* info = (RgfwInfo*)data;
	GfxWindow* win = info.mouseOwner;
	if (win == null) return;
	uint b = button - 0x110;
	if (b == 1) { b = 2; }
	else if (b == 2) { b = 1; }
	rgfw_mouseButtonCallback(win, (RgfwMouseButton)b, rgfw_bool(state));
}

fn void rgfw_wlPointerAxis(void* data, WlProxy* pointer, uint time, uint axis, WlFixed value)
{
	RgfwInfo* info = (RgfwInfo*)data;
	GfxWindow* win = info.mouseOwner;
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
	GfxWindow* win = (GfxWindow*)wlSurfaceGetUserData(surface);
	info.kbOwner = win;

	if (info.dataDevice != null && win.src.dataSource != null) wlDataDeviceSetSelection(info.dataDevice, win.src.dataSource, serial);
	if (win.src.minimized == true) win.src.minimized = false;
	rgfw_windowFocusCallback(win, true);
}

fn void rgfw_wlKeyboardLeave(void* data, WlProxy* keyboard, uint serial, WlProxy* surface)
{
	RgfwInfo* info = (RgfwInfo*)data;
	GfxWindow* win = (GfxWindow*)wlSurfaceGetUserData(surface);
	if (info.kbOwner == win) info.kbOwner = null;
	rgfw_windowFocusCallback(win, false);
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

	GfxWindow* win = info.kbOwner;
	RgfwKey rgfwKey = rgfw_apiKeyToRGFW(key + 8);

	rgfw_keyUpdateKeyMods(win, rgfw_bool(xkbKeymapModGetIndex(info.keymap, "Lock") != 0), rgfw_bool(xkbKeymapModGetIndex(info.keymap, "Mod2") != 0), rgfw_bool(xkbKeymapModGetIndex(info.keymap, "ScrollLock") != 0));
	rgfw_keyCallback(win, rgfwKey, win.internal.mod, rgfw_bool(is_key_down(rgfwKey) && state != 0), rgfw_bool(state));

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
	GfxMonitor* mon = &((RgfwMonitorNode*)data).mon;
	mon.x = x;
	mon.y = y;
	mon.physW = (float)physW / 25.4f;
	mon.physH = (float)physH / 25.4f;
}

fn void rgfw_wlOutputHandleMode(void* data, WlProxy* output, uint flags, int width, int height, int refresh)
{
	RgfwMonitorNode* node = (RgfwMonitorNode*)data;
	GfxMonitor* mon = &node.mon;

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
	GfxMonitor* mon = &((RgfwMonitorNode*)data).mon;
	mon.scaleX = (float)factor;
	mon.scaleY = (float)factor;
}

fn void rgfw_wlOutputSetName(void* data, WlProxy* output, char* name)
{
	GfxMonitor* mon = &((RgfwMonitorNode*)data).mon;
	usz n = strlen(name);
	if (n > 127) n = 127;
	rgfw_memcpy(&mon.name, name, n);
	mon.name[n] = 0;
}

fn void rgfw_wlOutputDescription(void* data, WlProxy* output, char* description) {}

fn void rgfw_xdgOutputLogicalPos(void* data, WlProxy* xdgOutput, int x, int y)
{
	GfxMonitor* mon = &((RgfwMonitorNode*)data).mon;
	mon.x = x;
	mon.y = y;
}

fn void rgfw_xdgOutputLogicalSize(void* data, WlProxy* xdgOutput, int width, int height)
{
	GfxMonitor* mon = &((RgfwMonitorNode*)data).mon;
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
	GfxMonitor* mon = &node.mon;

	if (mon.physW <= 0 || mon.physH <= 0)
	{
		mon.physW = (float)mon.mode.w / 96.0f;
		mon.physH = (float)mon.mode.h / 96.0f;
	}

	if (node.disconnected == false) return;
	node.disconnected = true;
	rgfw_monitorCallback(_rgfw.root, mon, true);
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

	GfxMonitor mon;
	rgfw_memzero(&mon, GfxMonitor::size);
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
	node.disconnected = true;
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
	GfxWindow* win = (GfxWindow*)data;
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
	_rgfw.unixClipboard = (GfxDataTransfer*)rgfw_alloc_impl(GfxDataTransfer::size + (usz)n);
	char* dataPtr = (char*)_rgfw.unixClipboard + GfxDataTransfer::size - 1;
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

	rgfw_monitorCallback(_rgfw.root, &node.mon, false);
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

fn bool rgfw_createSurfacePtr(char* data, int w, int h, RgfwFormat format, GfxSurface* surface)
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
		return false;
	}

	surface.native.pool = wlShmCreatePool(_rgfw.shm, fd, (int)size);
	surface.native.buffer = mmap(null, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
	if (surface.native.buffer == MAP_FAILED)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_BUFFER, "mmap failed.");
		return false;
	}

	surface.native.fd = fd;
	surface.native.format = RGFW_FORMAT_BGRA8;
	return true;
}

fn bool rgfw_window_createSurfacePtr(GfxWindow* win, char* data, int w, int h, RgfwFormat format, GfxSurface* surface) => rgfw_createSurfacePtr(data, w, h, format, surface);

fn void rgfw_window_blitSurface(GfxWindow* win, GfxSurface* surface)
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

fn void rgfw_surface_freePtr(GfxSurface* surface)
{
	if (surface.native.pool != null) wlShmPoolDestroy(surface.native.pool);
	if (surface.native.fd != 0) close(surface.native.fd);
	if (surface.native.buffer != null) munmap(surface.native.buffer, (usz)(surface.w * surface.h * 4));
}

fn void rgfw_window_setBorder(GfxWindow* win, bool border)
{
	rgfw_setBit(&win.internal.flags, RGFW_WINDOW_NO_BORDER, rgfw_bool(!border));
	if (_rgfw.decorationManager != null)
	{
		zxdgToplevelDecorationV1SetMode(win.src.decoration, border ? ZXDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE : ZXDG_TOPLEVEL_DECORATION_V1_MODE_CLIENT_SIDE);
	}
}

fn void rgfw_window_setRawMouseModePlatform(GfxWindow* win, bool state)
{
	if (_rgfw.relativePointerManager == null) return;

	if (state == false)
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

fn void rgfw_window_captureMousePlatform(GfxWindow* win, bool state)
{
	if (_rgfw.constraintManager == null) return;

	if (state == false)
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

fn GfxWindow* rgfw_createWindowPlatform(char* name, RgfwWindowFlags flags, GfxWindow* win)
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

	while (win.src.configured == false) wlDisplayDispatch(_rgfw.wlDisplay);

	return win;
}

fn bool rgfw_getGlobalMouse(int* x, int* y)
{
	if (x != null) *x = 0;
	if (y != null) *y = 0;
	return false;
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

fn bool rgfw_window_fetchSize(GfxWindow* win, int* w, int* h) => rgfw_window_getSize(win, w, h);

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

fn void rgfw_window_movePlatform(GfxWindow* win, int x, int y) { win.x = x; win.y = y; }

fn void rgfw_window_resizePlatform(GfxWindow* win, int w, int h)
{
	win.w = w; win.h = h;
	if (_rgfw.compositor != null)
	{
		xdgSurfaceSetWindowGeometry(win.src.xdgSurface, 0, 0, win.w, win.h);
		if (win.src.ctx.egl != null && win.src.ctx.egl.eglWindow != null) wlEglWindowResize((WlEglWindow*)win.src.ctx.egl.eglWindow, w, h, 0, 0);
	}
}

fn void rgfw_window_setAspectRatio(GfxWindow* win, int w, int h)
{
	if (w == 0 && h == 0) return;
	xdgToplevelSetMaxSize(win.src.xdgToplevel, w, h);
}

fn void rgfw_window_setMinSize(GfxWindow* win, int w, int h) { xdgToplevelSetMinSize(win.src.xdgToplevel, w, h); }
fn void rgfw_window_setMaxSize(GfxWindow* win, int w, int h) { xdgToplevelSetMaxSize(win.src.xdgToplevel, w, h); }
fn void rgfw_window_maximizePlatform(GfxWindow* win) => rgfw_toggleWaylandMaximized(win, true);
fn void rgfw_window_focus(GfxWindow* win) {}
fn void rgfw_window_raise(GfxWindow* win) {}

fn void rgfw_window_setFullscreen(GfxWindow* win, bool fullscreen)
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

fn void rgfw_window_setFloating(GfxWindow* win, bool floating) {}
fn void rgfw_window_setOpacity(GfxWindow* win, char opacity) {}

fn void rgfw_window_minimizePlatform(GfxWindow* win)
{
	if (rgfw_window_isMaximized(win)) return;
	win.internal.oldX = win.x; win.internal.oldY = win.y; win.internal.oldW = win.w; win.internal.oldH = win.h;
	win.src.minimized = true;
	xdgToplevelSetMinimized(win.src.xdgToplevel);
}

fn void rgfw_window_restorePlatform(GfxWindow* win) => rgfw_toggleWaylandMaximized(win, false);
fn bool GfxWindow.is_floating(&win) => !rgfw_window_isFullscreen(win) && !rgfw_window_isMaximized(win);

fn void rgfw_window_setName(GfxWindow* win, char* name)
{
	if (name == null) name = "";
	if (_rgfw.compositor != null) xdgToplevelSetTitle(win.src.xdgToplevel, name);
}

fn void rgfw_window_setMousePassthrough(GfxWindow* win, bool passthrough) {}

fn bool GfxWindow.set_icon(&win, char* data, int w, int h, RgfwFormat format, GfxWindowIconType type = BOTH)
{
	if (_rgfw.iconManager == null || w != h) return false;

	if (win.src.icon != null) { xdgToplevelIconV1Destroy(win.src.icon); win.src.icon = null; }

	GfxSurface* surface = create_surface(data, w, h, format);
	if (surface == null) return false;

	int minW = w < surface.w ? w : surface.w;
	int minH = h < surface.h ? h : surface.h;
	rgfw_copyImageData(surface.native.buffer, minW, minH, surface.native.format, surface.data, surface.format, null);

	win.src.icon = xdgToplevelIconManagerV1CreateIcon(_rgfw.iconManager);
	surface.native.wlBuffer = wlShmPoolCreateBuffer(surface.native.pool, 0, w, h, w * 4, WL_SHM_FORMAT_ARGB8888);
	xdgToplevelIconV1AddBuffer(win.src.icon, surface.native.wlBuffer, 1);
	xdgToplevelIconManagerV1SetIcon(_rgfw.iconManager, win.src.xdgToplevel, win.src.icon);

	rgfw_surface_free(surface);
	return true;
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

	GfxSurface* surface = (GfxSurface*)rgfw_alloc_impl(GfxSurface::size);
	rgfw_memzero(surface, GfxSurface::size);
	surface.w = (int)cursorImage.width;
	surface.h = (int)cursorImage.height;
	surface.native.wlBuffer = cursorBuffer;
	return (RgfwMouse*)surface;
}

fn RgfwMouse* rgfw_createMouse(char* data, int w, int h, RgfwFormat format)
{
	GfxSurface* surface = create_surface(data, w, h, format);
	if (surface == null) return null;

	surface.native.wlBuffer = wlShmPoolCreateBuffer(surface.native.pool, 0, surface.w, surface.h, surface.w * 4, WL_SHM_FORMAT_ARGB8888);
	int minW = w < surface.w ? w : surface.w;
	int minH = h < surface.h ? h : surface.h;
	rgfw_copyImageData(surface.native.buffer, minW, minH, surface.native.format, surface.data, surface.format, null);
	return (RgfwMouse*)surface;
}

fn bool rgfw_window_setMousePlatform(GfxWindow* win, RgfwMouse* mouse)
{
	GfxSurface* surface = (GfxSurface*)mouse;
	win.src.usingCustomCursor = true;
	wlSurfaceAttach(win.src.customCursorSurface, surface.native.wlBuffer, 0, 0);
	wlSurfaceDamage(win.src.customCursorSurface, 0, 0, surface.w, surface.h);
	wlSurfaceCommit(win.src.customCursorSurface);
	return true;
}

fn void rgfw_freeMouse(RgfwMouse* mouse)
{
	if (mouse == null) return;
	GfxSurface* surface = (GfxSurface*)mouse;
	if (surface.native.buffer != null && surface.native.wlBuffer != null) wlBufferDestroy(surface.native.wlBuffer);
	rgfw_surface_free(surface);
}

fn void rgfw_window_moveMouse(GfxWindow* win, int x, int y)
{
	if (_rgfw.wpPointerWarp != null)
	{
		wpPointerWarpV1WarpPointer(_rgfw.wpPointerWarp, win.src.surface, _rgfw.wlPointer, wlFixedFromInt(x), wlFixedFromInt(y), _rgfw.mouseEnterSerial);
	}
}

fn void rgfw_window_hide(GfxWindow* win)
{
	wlSurfaceAttach(win.src.surface, null, 0, 0);
	wlSurfaceCommit(win.src.surface);
	win.internal.flags |= RGFW_WINDOW_HIDE;
}

fn void rgfw_window_showPlatform(GfxWindow* win)
{
	win.internal.flags &= ~(uint)RGFW_WINDOW_HIDE;
	wlSurfaceCommit(win.src.surface);
}

fn void rgfw_window_flash(GfxWindow* win, GfxFlashRequest request) {}

fn bool rgfw_readClipboardPtr(RgfwDataTransferType requestedType, char* buffer, usz capacity, GfxDataTransfer* data)
{
	if (requestedType != RGFW_DATA_TEXT) return false;
	if (_rgfw.unixClipboard == null || _rgfw.unixClipboard.length == 0)
	{
		data.length = 0;
		data.type = RGFW_DATA_NONE;
		return false;
	}
	data.length = _rgfw.unixClipboard.length;
	data.type = RGFW_DATA_TEXT;
	if (buffer == null) return true;
	if (_rgfw.unixClipboard.length > capacity) return false;
	rgfw_memcpy(buffer, (void*)_rgfw.unixClipboard.data, _rgfw.unixClipboard.length);
	data.data = buffer;
	return true;
}

WlDataSourceListener rgfw_dataSourceListener = {
	.target = &rgfw_wlDataSourceNothingStr, .action = &rgfw_wlDataSourceNothingU32,
	.dndDropPerformed = &rgfw_wlDataSourceNothing, .dndFinished = &rgfw_wlDataSourceNothing,
	.send = &rgfw_wlDataSourceSend, .cancelled = &rgfw_wlDataSourceCancelled
};

fn bool rgfw_writeClipboard(GfxDataTransfer* data)
{
	if (_rgfw.dataDeviceManager == null) return false;
	if (_rgfw.unixClipboard != null) { rgfw_free_impl(_rgfw.unixClipboard); _rgfw.unixClipboard = null; }

	_rgfw.unixClipboard = (GfxDataTransfer*)rgfw_alloc_impl(GfxDataTransfer::size + data.length);
	usz length = data.length;
	if (data.data[length - 1] != 0) length += 1;

	char* dataPtr = (char*)_rgfw.unixClipboard + GfxDataTransfer::size - 1;
	rgfw_memcpy(dataPtr, (void*)data.data, data.length);
	dataPtr[length - 1] = 0;
	_rgfw.unixClipboard.data = dataPtr;
	_rgfw.unixClipboard.type = RGFW_DATA_TEXT;
	_rgfw.unixClipboard.length = length;

	GfxWindow* win = _rgfw.kbOwner;
	if (win.src.dataSource != null) { wlDataSourceDestroy(win.src.dataSource); win.src.dataSource = null; }

	win.src.dataSource = wlDataDeviceManagerCreateDataSource(_rgfw.dataDeviceManager);
	if (win.src.dataSource == null)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_CLIPBOARD, "Could not create clipboard data source");
		return false;
	}

	wlDataSourceAddListener(win.src.dataSource, &rgfw_dataSourceListener, _rgfw);
	wlDataSourceOffer(win.src.dataSource, "text/plain;charset=utf-8");
	return true;
}

fn bool rgfw_window_isHidden(GfxWindow* win) => false;
fn bool rgfw_window_isMinimized(GfxWindow* win) => win.src.minimized;
fn bool rgfw_window_isMaximized(GfxWindow* win) => win.src.maximized;
fn void rgfw_pollMonitors() { _rgfw.monitors.primary = _rgfw.monitors.list.head; }

fn void rgfw_monitorNode_free(RgfwMonitorNode* node)
{
	if (node.output != null) wlOutputDestroy(node.output);
	if (node.xdgOutput != null) zxdgOutputV1Destroy(node.xdgOutput);
	if (node.modes != null) { rgfw_free_impl(node.modes); node.modeCount = 0; }
}

fn bool rgfw_monitor_getWorkarea(GfxMonitor* monitor, int* x, int* y, int* width, int* height)
{
	if (x != null) *x = monitor.x;
	if (y != null) *y = monitor.y;
	if (width != null) *width = monitor.mode.w;
	if (height != null) *height = monitor.mode.h;
	return true;
}

fn usz rgfw_monitor_getModesPtr(GfxMonitor* monitor, RgfwMonitorMode** modes)
{
	if (modes != null) rgfw_memcpy(*modes, monitor.node.modes, monitor.node.modeCount * RgfwMonitorMode::size);
	return monitor.node.modeCount;
}

fn usz rgfw_monitor_getGammaRampPtr(GfxMonitor* monitor, RgfwGammaRamp* ramp) => 0;
fn bool rgfw_monitor_setGammaRamp(GfxMonitor* monitor, RgfwGammaRamp* ramp) => false;

fn bool rgfw_monitor_requestMode(GfxMonitor* mon, RgfwMonitorMode* mode, RgfwModeRequest request)
{
	for (usz i = 0; i < mon.node.modeCount; i++)
	{
		if (rgfw_monitorModeCompare(mode, &mon.node.modes[i], request) == false) continue;
		rgfw_monitor_setMode(mon, &mon.node.modes[i]);
		return true;
	}
	return false;
}

fn bool rgfw_monitor_setMode(GfxMonitor* mon, RgfwMonitorMode* mode) => false;

fn GfxMonitor* rgfw_window_getMonitor(GfxWindow* win)
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
	if (extensions == null || ext == null) return false;
	while (ext[len - 1] == 0 && len > 3) len--;
	char* start = extensions;
	char* where = strstr(extensions, ext);
	while (where != null)
	{
		char* terminator = where + len;
		if ((where == start || *(where - 1) == ' ') && (*terminator == ' ' || *terminator == 0)) return true;
		where = strstr(terminator, ext);
	}
	return false;
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
	if (rgfw_eglQueryString == null || _rgfw.egl_display == null) return false;
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

fn bool rgfw_window_createContextPtr_EGL(GfxWindow* win, RgfwEglContext* ctx, RgfwGlHints* hints)
{
	if (win == null || ctx == null || hints == null) return false;
	if (rgfw_loadEGL() == false) return false;
	if (_rgfw.egl_display == null)
	{
		_rgfw.egl_display = rgfw_eglGetDisplay((EglNativeDisplayType)_rgfw.wlDisplay);
		if (_rgfw.egl_display == null) return false;
		EglInt major = 0;
		EglInt minor = 0;
		if (rgfw_eglInitialize((EglDisplay)_rgfw.egl_display, &major, &minor) == 0) return false;
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
	if (rgfw_eglChooseConfig((EglDisplay)_rgfw.egl_display, (EglInt*)&cfg, &config, 1, &num) == 0 || num == 0) return false;
	rgfw_eglBindApi(hints.profile == RGFW_GL_ES ? (uint)EGL_OPENGL_ES_API : (uint)EGL_OPENGL_API);

	EglInt[8] ctxAttribs;
	RgfwAttribStack ctxStack;
	rgfw_attribStack_init(&ctxStack, (int*)&ctxAttribs, 8);
	if (hints.profile == RGFW_GL_ES) rgfw_attribStack_pushAttribs(&ctxStack, EGL_CONTEXT_CLIENT_VERSION, hints.major != 0 ? hints.major : 2);
	rgfw_attribStack_pushAttribs(&ctxStack, EGL_NONE, EGL_NONE);

	EglContext share = hints.shareEGL != null ? (EglContext)hints.shareEGL.ctx : null;
	ctx.ctx = rgfw_eglCreateContext((EglDisplay)_rgfw.egl_display, config, share, (EglInt*)&ctxAttribs);
	if (ctx.ctx == null) return false;

	WlEglWindow* eglWindow = wlEglWindowCreate(win.src.surface, win.w, win.h);
	ctx.eglWindow = eglWindow;
	ctx.surface = rgfw_eglCreateWindowSurface((EglDisplay)_rgfw.egl_display, config, (EglNativeWindowType)eglWindow, null);
	if (ctx.surface == null)
	{
		rgfw_eglDestroyContext((EglDisplay)_rgfw.egl_display, (EglContext)ctx.ctx);
		ctx.ctx = null;
		return false;
	}
	win.src.ctx.egl = ctx;
	win.src.gfxType = RGFW_GFX_EGL;
	rgfw_window_makeCurrentContext_EGL(win);
	return true;
}

fn void rgfw_window_deleteContextPtr_EGL(GfxWindow* win, RgfwEglContext* ctx)
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

fn void rgfw_window_makeCurrentContext_EGL(GfxWindow* win)
{
	if (_rgfw.egl_display == null || rgfw_eglMakeCurrent == null) return;
	if (win == null || win.src.ctx.egl == null) { rgfw_eglMakeCurrent((EglDisplay)_rgfw.egl_display, null, null, null); return; }
	RgfwEglContext* ctx = win.src.ctx.egl;
	rgfw_eglMakeCurrent((EglDisplay)_rgfw.egl_display, (EglSurface)ctx.surface, (EglSurface)ctx.surface, (EglContext)ctx.ctx);
}

fn void* rgfw_getCurrentContext_EGL() => rgfw_eglGetCurrentContext != null ? rgfw_eglGetCurrentContext() : null;
fn void rgfw_window_swapBuffers_EGL(GfxWindow* win) { if (win != null && win.src.ctx.egl != null && rgfw_eglSwapBuffers != null) rgfw_eglSwapBuffers((EglDisplay)_rgfw.egl_display, (EglSurface)win.src.ctx.egl.surface); }
fn void rgfw_window_swapInterval_EGL(GfxWindow* win, int swapInterval) { if (rgfw_eglSwapInterval != null) rgfw_eglSwapInterval((EglDisplay)_rgfw.egl_display, swapInterval); }

fn bool rgfw_window_createContextPtr_OpenGL(GfxWindow* win, RgfwGlContext* ctx, RgfwGlHints* hints)
{
	bool out = rgfw_window_createContextPtr_EGL(win, (RgfwEglContext*)ctx, hints);
	win.src.gfxType = RGFW_GFX_NATIVE_OPEN_GL;
	rgfw_window_swapInterval_OpenGL(win, 0);
	return out;
}
fn void rgfw_window_deleteContextPtr_OpenGL(GfxWindow* win, RgfwGlContext* ctx) { rgfw_window_deleteContextPtr_EGL(win, (RgfwEglContext*)ctx); win.src.ctx.native = null; }
fn void rgfw_window_makeCurrentContext_OpenGL(GfxWindow* win) { rgfw_window_makeCurrentContext_EGL(win); }
fn void* rgfw_getCurrentContext_OpenGL() => rgfw_getCurrentContext_EGL();
fn void rgfw_window_swapBuffers_OpenGL(GfxWindow* win) { rgfw_window_swapBuffers_EGL(win); }
fn void rgfw_window_swapInterval_OpenGL(GfxWindow* win, int swapInterval) { rgfw_window_swapInterval_EGL(win, swapInterval); }

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
