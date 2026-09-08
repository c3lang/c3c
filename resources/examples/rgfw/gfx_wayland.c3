module std::gfx @feat(WAYLAND, X11_WAYLAND) @private @link("wayland-client", "wayland-cursor", "wayland-egl", "xkbcommon", "EGL");
import std::math, std::collections::list, libc;

// public
fn void rgfw_window_captureMousePlatform_Wayland(GfxWindow* win, bool state)
{
	if (!_gfx_ctx.constraintManager) return;

	if (!state)
	{
		if (win.src.lockedPointer) zwpLockedPointerV1Destroy(win.src.lockedPointer);
		win.src.lockedPointer = null;
		return;
	}
	if (win.src.lockedPointer) return;

	win.src.lockedPointer = zwpPointerConstraintsV1LockPointer(_gfx_ctx.constraintManager, win.src.surface, _gfx_ctx.wlPointer, null, ZWP_POINTER_CONSTRAINTS_V1_LIFETIME_PERSISTENT);
	static ZwpLockedPointerV1Listener listener = { .locked = &rgfw_wlPointerLocked, .unlocked = &rgfw_wlUnlockedNoop };
	zwpLockedPointerV1AddListener(win.src.lockedPointer, &listener, &_gfx_ctx);
}

// public
fn sz rgfw_monitor_getGammaRampPtr_Wayland(GfxMonitor* monitor, RgfwGammaRamp* ramp) => 0;

fn bool rgfw_monitor_setGammaRamp_Wayland(GfxMonitor* monitor, RgfwGammaRamp* ramp) => false;

// public
fn bool rgfw_monitor_setMode_Wayland(GfxMonitor* mon, GfxMonitorMode* mode) => false;

// public
fn GfxMonitorMode[] rgfw_monitor_getModesPtr_Wayland(GfxMonitor* monitor)
{
	List{GfxMonitorMode} modes;
	for (int i = 0; i < monitor.node.modeCount; i++)
	{
		modes.push(monitor.node.modes[i]);
	}
	return modes.array_view();
}

XdgSurfaceListener rgfw_xdgSurfaceListener = { .configure = &rgfw_wlXdgSurfaceConfigureHandler };
WlSurfaceListener rgfw_windowSurfaceListener = {
	.enter = &rgfw_wlSurfaceEnter, .leave = &rgfw_wlSurfaceLeave,
	.preferredBufferScale = &rgfw_wlSurfacePrefScale, .preferredBufferTransform = &rgfw_wlSurfacePrefTransform
};
XdgToplevelListener rgfw_xdgToplevelListener = { .configure = &rgfw_wlXdgToplevelConfigureHandler, .close = &rgfw_wlXdgToplevelCloseHandler };
ZxdgToplevelDecorationV1Listener rgfw_xdgDecorationListener = { .configure = &rgfw_wlXdgDecorationConfigureHandler };

typedef WlDisplay = void;
typedef WlProxy = void;
alias WlFixed = int;
struct WlMessage
{
	ZString name;
	ZString signature;
	void* types;
}
struct WlInterface
{
	ZString name;
	int version;
	int methodCount;
	WlMessage* methods;
	int eventCount;
	WlMessage* events;
}
struct WlArray
{
	sz size;
	sz alloc;
	void* data;
}

extern fn WlDisplay* wlDisplayConnect(ZString name) @cname("wl_display_connect");
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
extern fn WlProxy* wlProxyMarshalFlags(WlProxy* proxy, uint opcode, WlInterface* iface, uint version, uint flags, ...) @cname("wl_proxy_marshal_flags");
extern fn int wlProxyAddListener(WlProxy* proxy, void* implementation, void* data) @cname("wl_proxy_add_listener");
extern fn void* wlProxyGetUserData(WlProxy* proxy) @cname("wl_proxy_get_user_data");
extern fn void wlProxySetUserData(WlProxy* proxy, void* data) @cname("wl_proxy_set_user_data");
extern fn uint wlProxyGetVersion(WlProxy* proxy) @cname("wl_proxy_get_version");
extern fn void wlProxyDestroy(WlProxy* proxy) @cname("wl_proxy_destroy");
extern fn void wlProxyMarshal(WlProxy* proxy, uint opcode, ...) @cname("wl_proxy_marshal");

macro WlFixed wlFixedFromInt(int i) => (WlFixed)(i * 256);
macro int wlFixedToInt(WlFixed f) => f / 256;
macro double wlFixedToDouble(WlFixed f) => (double)f / 256.0;
macro WlFixed wlFixedFromDouble(double d) => (WlFixed)(d * 256.0);

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

struct WlRegistryListener
{
	void* global;
	void* globalRemove;
}
struct WlSeatListener
{
	void* capabilities;
	void* name;
}
struct WlShmListener
{
	void* format;
}
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

struct WlSurfaceListener
{
	void* enter;
	void* leave;
	void* preferredBufferScale;
	void* preferredBufferTransform;
}
struct WlDataSourceListener
{
	void* target;
	void* send;
	void* cancelled;
	void* dndDropPerformed;
	void* dndFinished;
	void* action;
}
struct WlDataOfferListener
{
	void* offer;
	void* sourceActions;
	void* action;
}
struct WlDataDeviceListener
{
	void* dataOffer;
	void* enter;
	void* leave;
	void* motion;
	void* drop;
	void* selection;
}
struct XdgWmBaseListener
{
	void* ping;
}
struct XdgSurfaceListener
{
	void* configure;
}
struct XdgToplevelListener
{
	void* configure;
	void* close;
	void* configureBounds;
	void* wmCapabilities;
}
struct ZxdgToplevelDecorationV1Listener
{
	void* configure;
}
struct ZwpRelativePointerV1Listener
{
	void* relativeMotion;
}
struct ZwpLockedPointerV1Listener
{
	void* locked;
	void* unlocked;
}
struct ZxdgOutputV1Listener
{
	void* logicalPosition;
	void* logicalSize;
	void* done;
	void* name;
	void* description;
}

// ---- wl_cursor (libwayland-cursor) ----
typedef WlCursorTheme = void;
struct WlCursorImage
{
	uint width;
	uint height;
	uint hotspotX;
	uint hotspotY;
	uint delay;
}
struct WlCursor
{
	uint imageCount;
	WlCursorImage** images;
	ZString name;
}
extern fn WlCursorTheme* wlCursorThemeLoad(ZString name, int size, void* shm) @cname("wl_cursor_theme_load");
extern fn void wlCursorThemeDestroy(WlCursorTheme* theme) @cname("wl_cursor_theme_destroy");
extern fn WlCursor* wlCursorThemeGetCursor(WlCursorTheme* theme, ZString name) @cname("wl_cursor_theme_get_cursor");
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
extern fn XkbKeymap* xkbKeymapNewFromString(XkbContext* ctx, ZString string, int format, int flags) @cname("xkb_keymap_new_from_string");
extern fn void xkbKeymapUnref(XkbKeymap* keymap) @cname("xkb_keymap_unref");
extern fn int xkbKeymapModGetIndex(XkbKeymap* keymap, ZString name) @cname("xkb_keymap_mod_get_index");
extern fn XkbState* xkbStateNew(XkbKeymap* keymap) @cname("xkb_state_new");
extern fn void xkbStateUnref(XkbState* state) @cname("xkb_state_unref");
extern fn uint xkbStateUpdateMask(XkbState* state, uint depressed, uint latched, uint locked, uint depressedLayout, uint latchedLayout, uint lockedLayout) @cname("xkb_state_update_mask");
extern fn int xkbStateKeyGetSyms(XkbState* state, XkbKeycode key, XkbKeysym** syms) @cname("xkb_state_key_get_syms");
extern fn XkbKeysym xkbStateKeyGetOneSym(XkbState* state, XkbKeycode key) @cname("xkb_state_key_get_one_sym");
extern fn uint xkbKeysymToUtf32(XkbKeysym sym) @cname("xkb_keysym_to_utf32");
extern fn XkbComposeTable* xkbComposeTableNewFromLocale(XkbContext* ctx, ZString locale, int flags) @cname("xkb_compose_table_new_from_locale");
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

fn void wlDataSourceOffer(WlProxy* source, ZString mimeType) { wlProxyMarshalFlags(source, 0, null, wlProxyGetVersion(source), 0, mimeType); }
fn int wlDataSourceAddListener(WlProxy* source, WlDataSourceListener* listener, void* data) => wlProxyAddListener(source, listener, data);
fn void wlDataSourceDestroy(WlProxy* source) { wlProxyMarshalFlags(source, 1, null, wlProxyGetVersion(source), WL_MARSHAL_FLAG_DESTROY); }

fn int wlDataOfferAddListener(WlProxy* offer, WlDataOfferListener* listener, void* data) => wlProxyAddListener(offer, listener, data);
fn void wlDataOfferReceive(WlProxy* offer, ZString mimeType, int fd) { wlProxyMarshalFlags(offer, 1, null, wlProxyGetVersion(offer), 0, mimeType, fd); }
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
fn void xdgToplevelSetTitle(WlProxy* toplevel, ZString title) { wlProxyMarshalFlags(toplevel, 2, null, wlProxyGetVersion(toplevel), 0, title); }
fn void xdgToplevelSetAppId(WlProxy* toplevel, ZString appId) { wlProxyMarshalFlags(toplevel, 3, null, wlProxyGetVersion(toplevel), 0, appId); }
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

// private
fn RgfwMouse* rgfw_createMouse_Wayland(char* data, int w, int h, GfxFormat format)
{
	GfxSurface*? surface = create_surface(data, w, h, format);
	if (catch surface) return null;

	surface.native.wlBuffer = wlShmPoolCreateBuffer(surface.native.pool, 0, ...surface.size, surface.size.x * 4, WL_SHM_FORMAT_ARGB8888);
	int minW = w < surface.size.x ? w : surface.size.x;
	int minH = h < surface.size.y ? h : surface.size.y;
	copy_image_data(surface.native.buffer, minW, minH, surface.native.format, surface.data, surface.format, null);
	return (RgfwMouse*)surface;
}

// private
fn RgfwMouse* rgfw_createMouseStandard_Wayland(GfxMouseIcon mouse)
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

// private
fn void rgfw_deinitPlatform_Wayland()
{
	if (_gfx_ctx.unixClipboard != null) { free(_gfx_ctx.unixClipboard); _gfx_ctx.unixClipboard = null; }
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

// private
fn bool rgfw_extensionSupportedPlatform_OpenGL_Wayland(ZString extension, sz len) => rgfw_extensionSupportedPlatform_EGL(extension, len);

// private
fn void rgfw_freeMouse_Wayland(RgfwMouse* mouse)
{
	if (mouse == null) return;
	GfxSurface* surface = (GfxSurface*)mouse;
	if (surface.native.buffer != null && surface.native.wlBuffer != null) wlBufferDestroy(surface.native.wlBuffer);
	surface.destroy();
}

// private
fn void* rgfw_getCurrentContext_OpenGL_Wayland() => rgfw_getCurrentContext_EGL();

// private
fn bool rgfw_getGlobalMouse_Wayland(int* x, int* y)
{
	if (x != null) *x = 0;
	if (y != null) *y = 0;
	return false;
}

// private
fn GfxFn rgfw_getProcAddress_OpenGL_Wayland(ZString procname) => rgfw_getProcAddress_EGL(procname);

// private
fn void rgfw_initKeycodesPlatform_Wayland()
{
	// Wayland keycodes come directly from the evdev-based xkb keymap (key+8);
	// no separate keycode table walk is needed the way X11's is.
}

// private
fn int rgfw_initPlatform_Wayland(String class_name, GfxInitOption flags)
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

// private
fn bool rgfw_loadGL_Wayland()
{
	// Wayland only ever renders through EGL; "OpenGL" here just means EGL is ready.
	return load_egl();
}

// private
fn void rgfw_monitorNode_free_Wayland(GfxMonitorNode* node)
{
	if (node.output != null) wlOutputDestroy(node.output);
	if (node.xdgOutput != null) zxdgOutputV1Destroy(node.xdgOutput);
	if (node.modes != null) { free(node.modes); node.modeCount = 0; }
}

// private
fn bool rgfw_monitor_getWorkarea_Wayland(GfxMonitor* monitor, int* x, int* y, int* width, int* height)
{
	if (x != null) *x = monitor.origin.x;
	if (y != null) *y = monitor.origin.y;
	if (width != null) *width = monitor.mode.w;
	if (height != null) *height = monitor.mode.h;
	return true;
}

// private
fn bool rgfw_monitor_requestMode_Wayland(GfxMonitor* mon, GfxMonitorMode* mode, GfxMonitorModeRequest request)
{
	for (sz i = 0; i < mon.node.modeCount; i++)
	{
		if (rgfw_monitorModeCompare(mode, &mon.node.modes[i], request) == false) continue;
		rgfw_monitor_setMode(mon, &mon.node.modes[i]);
		return true;
	}
	return false;
}

// private
fn GfxKey rgfw_physicalToMappedKey_Wayland(GfxKey key)
{
	uint keycode = rgfw_rgfwToApiKey(key);
	XkbKeycode kc = keycode + 8;
	XkbKeysym sym = xkbStateKeyGetOneSym(_gfx_ctx.xkbState, kc);
	if (sym < 256) return (GfxKey)sym;

	switch (sym)
	{
		case XKB_KEY_F1:            return F1;
		case XKB_KEY_F2:            return F2;
		case XKB_KEY_F3:            return F3;
		case XKB_KEY_F4:            return F4;
		case XKB_KEY_F5:            return F5;
		case XKB_KEY_F6:            return F6;
		case XKB_KEY_F7:            return F7;
		case XKB_KEY_F8:            return F8;
		case XKB_KEY_F9:            return F9;
		case XKB_KEY_F10:           return F10;
		case XKB_KEY_F11:           return F11;
		case XKB_KEY_F12:           return F12;
		case XKB_KEY_F13:           return F13;
		case XKB_KEY_F14:           return F14;
		case XKB_KEY_F15:           return F15;
		case XKB_KEY_F16:           return F16;
		case XKB_KEY_F17:           return F17;
		case XKB_KEY_F18:           return F18;
		case XKB_KEY_F19:           return F19;
		case XKB_KEY_F20:           return F20;
		case XKB_KEY_F21:           return F21;
		case XKB_KEY_F22:           return F22;
		case XKB_KEY_F23:           return F23;
		case XKB_KEY_F24:           return F24;
		case XKB_KEY_F25:           return F25;
		case XKB_KEY_SHIFT_L:       return SHIFT_L;
		case XKB_KEY_SHIFT_R:       return SHIFT_R;
		case XKB_KEY_CONTROL_L:     return CONTROL_L;
		case XKB_KEY_CONTROL_R:     return CONTROL_R;
		case XKB_KEY_ALT_L:         return ALT_L;
		case XKB_KEY_ALT_R:         return ALT_R;
		case XKB_KEY_SUPER_L:       return SUPER_L;
		case XKB_KEY_SUPER_R:       return SUPER_R;
		case XKB_KEY_CAPS_LOCK:     return CAPS_LOCK;
		case XKB_KEY_NUM_LOCK:      return NUM_LOCK;
		case XKB_KEY_SCROLL_LOCK:   return SCROLL_LOCK;
		case XKB_KEY_UP:            return UP;
		case XKB_KEY_DOWN:          return DOWN;
		case XKB_KEY_LEFT:          return LEFT;
		case XKB_KEY_RIGHT:         return RIGHT;
		case XKB_KEY_HOME:          return HOME;
		case XKB_KEY_END:           return END;
		case XKB_KEY_PAGE_UP:       return PAGE_UP;
		case XKB_KEY_PAGE_DOWN:     return PAGE_DOWN;
		case XKB_KEY_INSERT:        return INSERT;
		case XKB_KEY_MENU:          return MENU;
		case XKB_KEY_KP_ADD:        return PAD_PLUS;
		case XKB_KEY_KP_SUBTRACT:   return PAD_MINUS;
		case XKB_KEY_KP_MULTIPLY:   return PAD_MULTIPLY;
		case XKB_KEY_KP_DIVIDE:     return PAD_SLASH;
		case XKB_KEY_KP_EQUAL:      return PAD_EQUAL;
		case XKB_KEY_KP_ENTER:      return PAD_RETURN;
		case XKB_KEY_KP_DECIMAL:    return PAD_PERIOD;
		case XKB_KEY_KP_0:          return PAD0;
		case XKB_KEY_KP_1:          return PAD1;
		case XKB_KEY_KP_2:          return PAD2;
		case XKB_KEY_KP_3:          return PAD3;
		case XKB_KEY_KP_4:          return PAD4;
		case XKB_KEY_KP_5:          return PAD5;
		case XKB_KEY_KP_6:          return PAD6;
		case XKB_KEY_KP_7:          return PAD7;
		case XKB_KEY_KP_8:          return PAD8;
		case XKB_KEY_KP_9:          return PAD9;
		case XKB_KEY_PRINT:         return PRINT_SCREEN;
		case XKB_KEY_PAUSE:         return PAUSE;
		default: break;
	}
	return NULL;
}

// private
fn void rgfw_pollEvents_Wayland()
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

// private
fn void rgfw_pollMonitors_Wayland() { _gfx_ctx.monitors.primary = _gfx_ctx.monitors.list.head; }

// private
fn bool rgfw_readClipboardPtr_Wayland(RgfwDataTransferType requestedType, char* buffer, sz capacity, GfxDataTransfer* data)
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
	mem::copy(buffer, (void*)_gfx_ctx.unixClipboard.data, _gfx_ctx.unixClipboard.length);
	data.data = buffer;
	return true;
}

// private
fn void rgfw_window_blitSurface_Wayland(GfxWindow* win, GfxSurface* surface)
{
	IntVec2 min = math::min(win.size, surface.size);
	surface.native.wlBuffer = wlShmPoolCreateBuffer(surface.native.pool, 0, ...min, surface.size.x * 4, WL_SHM_FORMAT_ARGB8888);

	wlSurfaceAttach(win.src.surface, surface.native.wlBuffer, 0, 0);
	wlSurfaceDamage(win.src.surface, 0, 0, ...min);
	wlSurfaceCommit(win.src.surface);
	wlBufferDestroy(surface.native.wlBuffer);
	surface.native.wlBuffer = null;
}

// private
fn void rgfw_window_closePlatform_Wayland(GfxWindow* win)
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

// private
fn bool rgfw_window_createContextPtr_OpenGL_Wayland(GfxWindow* win, RgfwGlContext* ctx, RgfwGlHints* hints)
{
	bool out = rgfw_window_createContextPtr_EGL(win, (RgfwEglContext*)ctx, hints);
	win.src.gfxType = RGFW_GFX_NATIVE_OPEN_GL;
	win.opengl_swap_interval(0);
	return out;
}

// public
fn void rgfw_window_swapInterval_OpenGL_Wayland(GfxWindow* win, int swapInterval)
{
	rgfw_window_swapInterval_EGL(win, swapInterval);
}


// private
fn WGPUSurface rgfw_window_createSurface_WebGPU_Wayland(GfxWindow* window, WGPUInstance instance)
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
fn void rgfw_window_deleteContextPtr_OpenGL_Wayland(GfxWindow* win, RgfwGlContext* ctx) { rgfw_window_deleteContextPtr_EGL(win, (RgfwEglContext*)ctx); win.src.ctx.native = null; }

// private
fn IntVec2 rgfw_window_fetchSize_Wayland(GfxWindow* win) => win.get_size();

// private
fn void rgfw_window_flash_Wayland(GfxWindow* win, GfxFlashRequest request) {}

// private
fn void rgfw_window_focus_Wayland(GfxWindow* win) {}

// private
fn void rgfw_window_hide_Wayland(GfxWindow* win)
{
	wlSurfaceAttach(win.src.surface, null, 0, 0);
	wlSurfaceCommit(win.src.surface);
	win.internal.flags.hide = true;
}

// private
fn bool _window_is_hidden_wayland(GfxWindow* win) => false;

// private
fn bool _window_is_maximized_wayland(GfxWindow* win) => win.src.maximized;

// private
fn bool _window_is_minimized_wayland(GfxWindow* win) => win.src.minimized;

// private
fn void rgfw_window_makeCurrentContext_OpenGL_Wayland(GfxWindow* win) { rgfw_window_makeCurrentContext_EGL(win); }

// private
fn void rgfw_window_maximizePlatform_Wayland(GfxWindow* win) => rgfw_toggleWaylandMaximized(win, true);

// private
fn void rgfw_window_minimizePlatform_Wayland(GfxWindow* win)
{
	if (_window_is_maximized_native(win)) return;
	win.internal.old_bounds = win.bounds;
	win.src.minimized = true;
	xdgToplevelSetMinimized(win.src.xdgToplevel);
}

// private
fn void rgfw_window_moveMouse_Wayland(GfxWindow* win, int x, int y)
{
	if (_gfx_ctx.wpPointerWarp != null)
	{
		wpPointerWarpV1WarpPointer(_gfx_ctx.wpPointerWarp, win.src.surface, _gfx_ctx.wlPointer, wlFixedFromInt(x), wlFixedFromInt(y), _gfx_ctx.mouseEnterSerial);
	}
}

// private
fn void rgfw_window_movePlatform_Wayland(GfxWindow* win, int x, int y) { win.x = x; win.y = y; }

// private
fn void rgfw_window_raise_Wayland(GfxWindow* win) {}

// private
fn void _window_resize_wayland(GfxWindow* win, int w, int h)
{
	win.width = w; win.height = h;
	if (_gfx_ctx.compositor != null)
	{
		xdgSurfaceSetWindowGeometry(win.src.xdgSurface, 0, 0, win.width, win.height);
		if (win.src.ctx.egl != null && win.src.ctx.egl.eglWindow != null) wlEglWindowResize((WlEglWindow*)win.src.ctx.egl.eglWindow, w, h, 0, 0);
	}
}

// private
fn void rgfw_window_restorePlatform_Wayland(GfxWindow* win) => rgfw_toggleWaylandMaximized(win, false);

// private
fn void rgfw_window_setAspectRatio_Wayland(GfxWindow* win, int w, int h)
{
	if (w == 0 && h == 0) return;
	xdgToplevelSetMaxSize(win.src.xdgToplevel, w, h);
}

// private
fn void rgfw_window_setBorder_Wayland(GfxWindow* win, bool border)
{
	win.internal.flags.no_border = !border;
	if (_gfx_ctx.decorationManager != null)
	{
		zxdgToplevelDecorationV1SetMode(win.src.decoration, border ? ZXDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE : ZXDG_TOPLEVEL_DECORATION_V1_MODE_CLIENT_SIDE);
	}
}

// private
fn void rgfw_window_setFloating_Wayland(GfxWindow* win, bool floating) {}

// private
fn void _window_set_fullscreen_wayland(GfxWindow* win, bool fullscreen)
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

// private
fn void _window_set_max_size_wayland(GfxWindow* win, int w, int h) { xdgToplevelSetMaxSize(win.src.xdgToplevel, w, h); }

// private
fn void _window_set_min_size_wayland(GfxWindow* win, int w, int h) { xdgToplevelSetMinSize(win.src.xdgToplevel, w, h); }

// private
fn void rgfw_window_setMousePassthrough_Wayland(GfxWindow* win, bool passthrough) {}

// private
fn bool rgfw_window_setMousePlatform_Wayland(GfxWindow* win, RgfwMouse* mouse)
{
	GfxSurface* surface = (GfxSurface*)mouse;
	win.src.usingCustomCursor = true;
	wlSurfaceAttach(win.src.customCursorSurface, surface.native.wlBuffer, 0, 0);
	wlSurfaceDamage(win.src.customCursorSurface, 0, 0, ...surface.size);
	wlSurfaceCommit(win.src.customCursorSurface);
	return true;
}

// private
fn void rgfw_window_setOpacity_Wayland(GfxWindow* win, char opacity) {}

// private
fn void rgfw_window_setRawMouseModePlatform_Wayland(GfxWindow* win, bool state)
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
fn void rgfw_window_swapBuffers_OpenGL_Wayland(GfxWindow* win) { rgfw_window_swapBuffers_EGL(win); }

fn void rgfw_window_showPlatform_Wayland(GfxWindow* win)
{
	win.internal.flags.hide = false;
	wlSurfaceCommit(win.src.surface);
}

// private
fn bool rgfw_writeClipboard_Wayland(GfxDataTransfer* data)
{
	if (_gfx_ctx.dataDeviceManager == null) return false;
	if (_gfx_ctx.unixClipboard != null) { free(_gfx_ctx.unixClipboard); _gfx_ctx.unixClipboard = null; }

	_gfx_ctx.unixClipboard = mem::new_with_padding(GfxDataTransfer, data.length);
	sz length = data.length;
	if (data.data[length - 1] != 0) length += 1;

	char* dataPtr = (char*)_gfx_ctx.unixClipboard + GfxDataTransfer::size - 1;
	mem::copy(dataPtr, (void*)data.data, data.length);
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

// private
fn void? rgfw_createSurfacePtr_Wayland(void* data, int w, int h, GfxFormat format, GfxSurface* self, GfxWindow* window)
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
	self.native.pool = wlShmCreatePool(_gfx_ctx.shm, fd, (int)size);
	self.native.buffer = mmap(null, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
	self.native.fd = fd;
	self.native.format = BGRA8;

	if (self.native.buffer == MAP_FAILED)
	{
		log::error("mmap failed", category: log::CATEGORY_VIDEO);
		self.native.buffer = null;
		return SURFACE_CREATION_FAILED~;
	}
}

// private
fn void rgfw_surface_freePtr_Wayland(GfxSurface* surface)
{
	if (!surface.data) return;
	if (surface.native.pool) wlShmPoolDestroy(surface.native.pool);
	if (surface.native.fd) close(surface.native.fd);
	if (surface.native.buffer) munmap(surface.native.buffer, (sz)(surface.size.x * surface.size.y * 4));
	*surface = {};
}

// private
fn bool _window_is_floating_wayland(GfxWindow* win) => !win.is_fullscreen() && !_window_is_maximized_native(win);

// private
fn void rgfw_window_setName_Wayland(GfxWindow* win, String name)
{
	if (_gfx_ctx.compositor) @pool()
	{
		xdgToplevelSetTitle(win.src.xdgToplevel, name.zstr_copy(tmem));
	};
}

// private
fn bool _window_set_icon_wayland(GfxWindow* win, char* data, int w, int h, GfxFormat format, GfxWindowIconType type)
{
	if (_gfx_ctx.iconManager == null || w != h) return false;

	if (win.src.icon != null) { xdgToplevelIconV1Destroy(win.src.icon); win.src.icon = null; }

	GfxSurface*? surface = create_surface(data, w, h, format);
	if (catch surface) return false;
	defer surface.destroy();
	int minW = w < surface.size.x ? w : surface.size.x;
	int minH = h < surface.size.y ? h : surface.size.y;
	copy_image_data(surface.native.buffer, minW, minH, surface.native.format, surface.data, surface.format, null);

	win.src.icon = xdgToplevelIconManagerV1CreateIcon(_gfx_ctx.iconManager);
	surface.native.wlBuffer = wlShmPoolCreateBuffer(surface.native.pool, 0, w, h, w * 4, WL_SHM_FORMAT_ARGB8888);
	xdgToplevelIconV1AddBuffer(win.src.icon, surface.native.wlBuffer, 1);
	xdgToplevelIconManagerV1SetIcon(_gfx_ctx.iconManager, win.src.xdgToplevel, win.src.icon);

	return true;
}

// private
fn GfxMonitor* rgfw_window_getMonitor_Wayland(GfxWindow* win)
{
	if (win.src.activeMonitor == null) return get_primary_monitor() ?? null;
	return &win.src.activeMonitor.mon;
}

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
fn void rgfw_toggleWaylandMaximized(GfxWindow* win, bool maximized)
{
	win.src.maximized = maximized;
	if (maximized) { xdgToplevelSetMaximized(win.src.xdgToplevel); }
	else { xdgToplevelUnsetMaximized(win.src.xdgToplevel); }
}

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
	mem::copy(&mon.name, &defaultName, (sz)n + 2);

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
fn int rgfw_wlCreateShmFile(sz size)
{
	int fd = rgfw_wlAnonymousShmOpen();
	if (fd < 0) return fd;
	if (ftruncate(fd, size) < 0) { close(fd); return -1; }
	return fd;
}

// private
fn void rgfw_wlDataDeviceDataOffer(void* data, WlProxy* device, WlProxy* offer)
{
	static WlDataOfferListener listener = { .offer = &rgfw_wlDataOfferNothingStr, .sourceActions = &rgfw_wlDataOfferNothingU32, .action = &rgfw_wlDataOfferNothingU32 };
	wlDataOfferAddListener(offer, &listener, null);
}

// private
fn void rgfw_wlDataDeviceNothing(void* data, WlProxy* device) {}

// private
fn void rgfw_wlDataDeviceNothingEnter(void* data, WlProxy* device, uint serial, WlProxy* surface, WlFixed x, WlFixed y, WlProxy* offer) {}

// private
fn void rgfw_wlDataDeviceNothingMotion(void* data, WlProxy* device, uint time, WlFixed x, WlFixed y) {}

// private
fn void rgfw_wlDataOfferNothingStr(void* data, WlProxy* offer, ZString s) {}

// private
fn void rgfw_wlDataOfferNothingU32(void* data, WlProxy* offer, uint u) {}

// private
fn void rgfw_wlDataSourceCancelled(void* data, WlProxy* source)
{
	GfxContext* info = data;
	if (info.kbOwner != null && info.kbOwner.src.dataSource == source) info.kbOwner.src.dataSource = null;
	wlDataSourceDestroy(source);
}

// private
fn void rgfw_wlDataSourceNothing(void* data, WlProxy* source) {}

// private
fn void rgfw_wlDataSourceNothingStr(void* data, WlProxy* source, ZString s) {}

// private
fn void rgfw_wlDataSourceNothingU32(void* data, WlProxy* source, uint u) {}

// private
fn void rgfw_wlDataSourceSend(void* data, WlProxy* source, ZString mimeType, int fd)
{
	if (mimeType && libc::strstr(mimeType, "text/plain") != null)
	{
		sz length = _gfx_ctx.unixClipboard.length;
		if (_gfx_ctx.unixClipboard.data[0] == 0) length -= 1;
		write(fd, (void*)_gfx_ctx.unixClipboard.data, length);
	}
	close(fd);
}

// private
fn void rgfw_wlGlobalRegistryHandler(void* data, WlProxy* registry, uint id, ZString iface, uint version)
{
	GfxContext* info = data;

	if (libc::strstr(iface, "wl_compositor") == iface) { info.compositor = wlRegistryBind(registry, id, &wlCompositorInterface, 4); }
	else if (libc::strstr(iface, "xdg_wm_base") == iface) { info.xdgWmBase = wlRegistryBind(registry, id, &xdgWmBaseInterface, 1); }
	else if (libc::strstr(iface, zxdgDecorationManagerV1Interface.name) == iface) { info.decorationManager = wlRegistryBind(registry, id, &zxdgDecorationManagerV1Interface, 1); }
	else if (libc::strstr(iface, zwpPointerConstraintsV1Interface.name) == iface) { info.constraintManager = wlRegistryBind(registry, id, &zwpPointerConstraintsV1Interface, 1); }
	else if (libc::strstr(iface, zwpRelativePointerManagerV1Interface.name) == iface) { info.relativePointerManager = wlRegistryBind(registry, id, &zwpRelativePointerManagerV1Interface, 1); }
	else if (libc::strstr(iface, xdgToplevelIconManagerV1Interface.name) == iface) { info.iconManager = wlRegistryBind(registry, id, &xdgToplevelIconManagerV1Interface, 1); }
	else if (libc::strstr(iface, "wl_shm") == iface)
	{
		info.shm = wlRegistryBind(registry, id, &wlShmInterface, 1);
		wlShmAddListener(info.shm, &rgfw_wlShmListener, info);
	}
	else if (libc::strstr(iface, "wl_seat") == iface)
	{
		info.seat = wlRegistryBind(registry, id, &wlSeatInterface, version < 4 ? 3 : 4);
		wlSeatAddListener(info.seat, &rgfw_wlSeatListener, info);
	}
	else if (libc::strstr(iface, zxdgOutputManagerV1Interface.name) == iface) { info.xdgOutputManager = wlRegistryBind(registry, id, &zxdgOutputManagerV1Interface, 1); }
	else if (libc::strstr(iface, "wl_output") == iface) { rgfw_wlCreateOutputs(registry, id); }
	else if (libc::strstr(iface, wpPointerWarpV1Interface.name) == iface) { info.wpPointerWarp = wlRegistryBind(registry, id, &wpPointerWarpV1Interface, 1); }
	else if (libc::strstr(iface, "wl_data_device_manager") == iface) { info.dataDeviceManager = wlRegistryBind(registry, id, &wlDataDeviceManagerInterface, 1); }
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
fn void rgfw_wlKeyboardKey(void* data, WlProxy* keyboard, uint serial, uint time, uint key, uint state)
{
	GfxContext* info = data;
	if (info.kbOwner == null) return;

	GfxWindow* win = info.kbOwner;
	GfxKey rgfwKey = rgfw_apiKeyToRGFW(key + 8);

	rgfw_keyUpdateKeyMods(win, xkbKeymapModGetIndex(info.keymap, "Lock") != 0, xkbKeymapModGetIndex(info.keymap, "Mod2") != 0, xkbKeymapModGetIndex(info.keymap, "ScrollLock") != 0);
	rgfw_keyCallback(win, rgfwKey, win.internal.mod, is_key_down(rgfwKey) && state != 0, !!state);

	info.lastKey = state != 0 ? key : 0;
	info.lastKeyTime = time + (uint)_gfx_ctx.wlRepeatInfoDelay;
	if (state != 0) rgfw_wlSendKeyEvent(_gfx_ctx.lastKey);
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
fn void rgfw_wlKeyboardLeave(void* data, WlProxy* keyboard, uint serial, WlProxy* surface)
{
	GfxContext* info = data;
	GfxWindow* win = (GfxWindow*)wlSurfaceGetUserData(surface);
	if (info.kbOwner == win) info.kbOwner = null;
	rgfw_windowFocusCallback(win, false);
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

// private
fn void rgfw_wlOutputDescription(void* data, WlProxy* output, ZString description) {}

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
fn void rgfw_wlOutputSetGeometry(void* data, WlProxy* output, int x, int y, int physW, int physH, int subpixel, ZString make, ZString model, int transform)
{
	GfxMonitor* mon = &((GfxMonitorNode*)data).mon;
	mon.origin = { x, y };
	mon.phys_size = { physW / 25.4f, physH / 25.4f };
}

// private
fn void rgfw_wlOutputSetName(void* data, WlProxy* output, ZString name)
{
	GfxMonitor* mon = &((GfxMonitorNode*)data).mon;
	sz n = name.len();
	if (n > 127) n = 127;
	mem::copy(&mon.name, name, n);
	mon.name[n] = 0;
}

// private
fn void rgfw_wlOutputSetScale(void* data, WlProxy* output, int factor)
{
	GfxMonitor* mon = &((GfxMonitorNode*)data).mon;
	mon.scale = (Vec2)factor;
}

// private
fn void rgfw_wlPointerAxis(void* data, WlProxy* pointer, uint time, uint axis, WlFixed value)
{
	GfxContext* info = data;
	GfxWindow* win = info.mouseOwner;
	if (win == null) return;
	float scrollX = 0.0f;
	float scrollY = 0.0f;
	if (!(win.internal.enabled_events & RGFW_MOUSE_SCROLL_FLAG)) return;
	if (axis == WL_POINTER_AXIS_HORIZONTAL_SCROLL) { scrollX = (float)(-wlFixedToDouble(value) / 10.0); }
	else if (axis == WL_POINTER_AXIS_VERTICAL_SCROLL) { scrollY = (float)(-wlFixedToDouble(value) / 10.0); }
	rgfw_mouseScrollCallback(win, scrollX, scrollY);
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
fn void rgfw_wlPointerLeave(void* data, WlProxy* pointer, uint serial, WlProxy* surface)
{
	GfxWindow* win = (GfxWindow*)wlSurfaceGetUserData(surface);
	GfxContext* info = data;
	if (info.mouseOwner == win) info.mouseOwner = null;
	_mouse_notify_callback(win, ...win.internal.last_mouse, false);
}

// private
fn void rgfw_wlPointerLocked(void* data, WlProxy* lockedPointer)
{
	GfxContext* info = data;
	wlPointerSetCursor(info.wlPointer, info.mouseEnterSerial, null, 0, 0);
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
fn void rgfw_wlSetOpaque(GfxWindow* win)
{
	WlProxy* region = wlCompositorCreateRegion(_gfx_ctx.compositor);
	if (region == null) return;
	wlRegionAdd(region, 0, 0, win.width, win.height);
	wlSurfaceSetOpaqueRegion(win.src.surface, region);
	wlRegionDestroy(region);
}

// private
fn void rgfw_wlShmFormatHandler(void* data, WlProxy* shm, uint format) {}

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
fn void rgfw_wlUnlockedNoop(void* data, WlProxy* p) {}

// private
fn void rgfw_wlXdgDecorationConfigureHandler(void* data, WlProxy* decoration, uint mode)
{
	GfxWindow* win = (GfxWindow*)data;
	if (mode != win.src.decorationMode) win.src.decorationMode = mode;
}

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
		win.resize(...win.size);
		if (!(win.internal.flags.transparent)) rgfw_wlSetOpaque(win);
	}

	win.src.configured = true;
}

// private
fn void rgfw_wlXdgToplevelCloseHandler(void* data, WlProxy* toplevel)
{
	GfxWindow* win = (GfxWindow*)data;
	if (!win.internal.shouldClose) _window_close_callback(win);
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
		win.size = { width, height };
		win.src.size = win.size;
	}
}

// private
fn void rgfw_wlXdgWmBasePingHandler(void* data, WlProxy* wmBase, uint serial) { xdgWmBasePong(wmBase, serial); }

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
