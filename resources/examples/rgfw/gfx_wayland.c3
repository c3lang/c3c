module std::gfx @feat(WAYLAND, X11_WAYLAND) @private @link("wayland-client", "wayland-cursor", "wayland-egl", "xkbcommon", "EGL");

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
