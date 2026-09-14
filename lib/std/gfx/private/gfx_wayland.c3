module std::gfx @feat(BSD, LINUX) @feat(WAYLAND) @private;
import std::math, std::collections, libc, std::os::posix, std::net::os, std::os::env, std::time::clock, std::time;

const uint WL_MARSHAL_FLAG_DESTROY = 1;

fn void _do_nothing() {}

fn void _window_capture_mouse_wayland(GfxWindow* win, bool state)
{
	if (!_gfx.constraintManager) return;

	if (!state)
	{
		if (win.src.lockedPointer) zwpLockedPointerV1Destroy(win.src.lockedPointer);
		win.src.lockedPointer = null;
		return;
	}
	if (win.src.lockedPointer) return;

	win.src.lockedPointer = zwpPointerConstraintsV1LockPointer(_gfx.constraintManager, win.src.surface, _gfx.wlPointer, null, ZWP_POINTER_CONSTRAINTS_V1_LIFETIME_PERSISTENT);
	static ZwpLockedPointerV1Listener listener = { .locked = &_wl_pointer_locked_cb, .unlocked = &_wl_unlocked_noop_cb };
	zwpLockedPointerV1AddListener(win.src.lockedPointer, &listener, &_gfx);
}


fn GfxGammaRamp _monitor_get_gamma_ramp_wayland(GfxMonitor* monitor, Allocator a) => {};
fn void? _monitor_set_gamma_ramp_wayland(GfxMonitor* monitor, GfxGammaRamp ramp) => NOT_IMPLEMENTED~;
fn void? _monitor_set_gamma_wayland(GfxMonitor* monitor, float gamma) => NOT_IMPLEMENTED~;
fn void? _monitor_set_mode_wayland(GfxMonitor* mon, GfxMonitorMode* mode) => NOT_IMPLEMENTED~;

fn GfxMonitorMode[] _monitor_get_modes_wayland(GfxMonitor* monitor)
{
	List{GfxMonitorMode} modes;
	for (int i = 0; i < monitor.node.mode_count; i++)
	{
		modes.push(monitor.node.modes[i]);
	}
	return modes.array_view();
}

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


macro WlFixed wlFixedFromInt(int i) => (WlFixed)(i * 256);
macro int WlFixed.to_int(self) => self / 256;
macro double WlFixed.to_double(self) => (double)self / 256.0;
macro WlFixed wlFixedFromDouble(double d) => (WlFixed)(d * 256.0);


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


// ---- wl_egl (libwayland-egl) ----

// ---- xkbcommon ----

// ==== thin wl_proxy_marshal_flags wrappers (normally wayland-scanner "static inline") ====
fn WlProxy* wlDisplayGetRegistry(WlDisplay* display) => wlProxyMarshalFlags((WlProxy*)display, 1, &wlRegistryInterface, wlProxyGetVersion((WlProxy*)display), 0, null);
fn void* wlRegistryBind(WlProxy* registry, uint name, WlInterface* iface, uint version) => (void*)wlProxyMarshalFlags(registry, 0, iface, version, 0, name, iface.name, version, null);
fn int wl_registry_add_listener(WlProxy* registry, WlRegistryListener* listener, void* data) => wlProxyAddListener(registry, listener, data);
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
fn void _xdg_surface_set_window_geometry(WlProxy* surface, int x, int y, int w, int h) { wlProxyMarshalFlags(surface, 3, null, wlProxyGetVersion(surface), 0, x, y, w, h); }
fn void xdgSurfaceDestroy(WlProxy* surface) { wlProxyMarshalFlags(surface, 0, null, wlProxyGetVersion(surface), WL_MARSHAL_FLAG_DESTROY); }

fn void xdgToplevelAddListener(WlProxy* toplevel, XdgToplevelListener* listener, void* data) { wlProxyAddListener(toplevel, listener, data); }
fn void xdgToplevelSetTitle(WlProxy* toplevel, ZString title) { wlProxyMarshalFlags(toplevel, 2, null, wlProxyGetVersion(toplevel), 0, title); }
fn void _xdg_toplevel_set_app_id(WlProxy* toplevel, ZString appId) { wlProxyMarshalFlags(toplevel, 3, null, wlProxyGetVersion(toplevel), 0, appId); }
fn void _xdg_toplevel_set_max_size(WlProxy* toplevel, int w, int h) { wlProxyMarshalFlags(toplevel, 7, null, wlProxyGetVersion(toplevel), 0, w, h); }
fn void xdgToplevelSetMinSize(WlProxy* toplevel, int w, int h) { wlProxyMarshalFlags(toplevel, 8, null, wlProxyGetVersion(toplevel), 0, w, h); }
fn void xdgToplevelSetMaximized(WlProxy* toplevel) { wlProxyMarshalFlags(toplevel, 9, null, wlProxyGetVersion(toplevel), 0); }
fn void _xdg_toplevel_unset_maximized(WlProxy* toplevel) { wlProxyMarshalFlags(toplevel, 10, null, wlProxyGetVersion(toplevel), 0); }
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
fn void _wp_pointer_warp_v1_warp_pointer(WlProxy* warp, WlProxy* surface, WlProxy* pointer, WlFixed x, WlFixed y, uint serial) { wlProxyMarshalFlags(warp, 1, null, wlProxyGetVersion(warp), 0, surface, pointer, x, y, serial); }
fn void wpPointerWarpV1Destroy(WlProxy* warp) { wlProxyMarshalFlags(warp, 0, null, wlProxyGetVersion(warp), WL_MARSHAL_FLAG_DESTROY); }

const uint WL_SEAT_CAPABILITY_POINTER = 1;
const uint WL_SEAT_CAPABILITY_KEYBOARD = 2;
const uint WL_POINTER_AXIS_VERTICAL_SCROLL = 0;
const uint WL_POINTER_AXIS_HORIZONTAL_SCROLL = 1;
const uint WL_OUTPUT_MODE_CURRENT = 1;
const uint WL_SHM_FORMAT_ARGB8888 = 0;

fn GfxMouse*? _create_mouse_wayland(char* data, GfxSize size, GfxFormat format)
{
	GfxSurface* surface = create_surface(data, ...size, format)!;
	surface.native.wlBuffer = wlShmPoolCreateBuffer(surface.native.pool, 0, ...surface.size, surface.size.x * 4, WL_SHM_FORMAT_ARGB8888);
	GfxSize min = math::min(size, surface.size);
	copy_image_data(surface.native.buffer, min.x, min.y, surface.native.format, surface.data, surface.format, null);
	return (GfxMouse*)surface;
}

fn GfxMouse*? _create_mouse_standard_wayland(GfxMouseIcon mouse)
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
		default: return MOUSE_NOT_SUPPORTED~;
	}

	WlCursor* wlcursor = wlCursorThemeGetCursor(_gfx.wlCursorTheme, cursorName);
	if (!wlcursor) return MOUSE_NOT_SUPPORTED~;
	WlCursorImage* cursorImage = wlcursor.images[0];
	WlProxy* cursorBuffer = wlCursorImageGetBuffer(cursorImage);

	GfxSurface* surface = mem::new(GfxSurface);
	surface.size = { (int)cursorImage.width, (int)cursorImage.height };
	surface.native.wlBuffer = cursorBuffer;
	return (GfxMouse*)surface;
}

fn void _destroy_wayland()
{
	if (_gfx.unixClipboard) free(_gfx.unixClipboard);
	if (_gfx.wlPointer) wlPointerDestroy(_gfx.wlPointer);
	if (_gfx.wlKeyboard) wlKeyboardDestroy(_gfx.wlKeyboard);
	if (_gfx.registry) wlRegistryDestroy(_gfx.registry);
	if (_gfx.decorationManager) zxdgDecorationManagerV1Destroy(_gfx.decorationManager);
	if (_gfx.relativePointerManager) zwpRelativePointerManagerV1Destroy(_gfx.relativePointerManager);
	if (_gfx.relativePointer) zwpRelativePointerV1Destroy(_gfx.relativePointer);
	if (_gfx.constraintManager) zwpPointerConstraintsV1Destroy(_gfx.constraintManager);
	if (_gfx.iconManager) xdgToplevelIconManagerV1Destroy(_gfx.iconManager);
	if (_gfx.xdgOutputManager) zxdgOutputManagerV1Destroy(_gfx.xdgOutputManager);
	if (_gfx.dataDeviceManager) wlDataDeviceManagerDestroy(_gfx.dataDeviceManager);
	if (_gfx.dataDevice) wlDataDeviceDestroy(_gfx.dataDevice);
	if (_gfx.wlCursorTheme) wlCursorThemeDestroy(_gfx.wlCursorTheme);
	if (_gfx.wp_pointer_warp) wpPointerWarpV1Destroy(_gfx.wp_pointer_warp);
	if (_gfx.hiddenMouse) _free_mouse(_gfx.hiddenMouse);
	if (_gfx.cursorSurface) wlSurfaceDestroy(_gfx.cursorSurface);
	if (_gfx.shm) wlShmDestroy(_gfx.shm);
	if (_gfx.seat) wlSeatRelease(_gfx.seat);
	if (_gfx.xdgWmBase) xdgWmBaseDestroy(_gfx.xdgWmBase);
	if (_gfx.compositor) wlCompositorDestroy(_gfx.compositor);
	if (_gfx.wlDisplay) wlDisplayDisconnect(_gfx.wlDisplay);
}

fn bool _opengl_extension_is_supported_wayland(String extension)
{
	return egl_extension_is_supported(extension);
}

// private
fn void _free_mouse_wayland(GfxMouse* mouse)
{
	GfxSurface* surface = (GfxSurface*)mouse;
	if (surface.native.buffer && surface.native.wlBuffer) wlBufferDestroy(surface.native.wlBuffer);
	surface.destroy();
}

fn void* _get_current_opengl_context_wayland() => _get_current_egl_context();

fn GfxPoint _get_global_mouse_wayland()
{
	return { -1, -1 };
}


fn void _init_keycodes_native_wayland()
{
	// Wayland keycodes come directly from the evdev-based xkb keymap (key+8);
	// no separate keycode table walk is needed the way X11's is.
}

// private
fn void? _init_wayland(String class_name, GfxInitOption flags)
{
	_gfx.wlDisplay = wlDisplayConnect(null);
	if (!_gfx.wlDisplay) return WAYLAND_DISPLAY_NOT_LOADED~;

	_gfx.compositor = null;
	_gfx.registry = wlDisplayGetRegistry(_gfx.wlDisplay);
	static WlRegistryListener listener = { .global = &_wl_global_registry_cb, .globalRemove = &_wl_global_registry_remove_cb };

	wl_registry_add_listener(_gfx.registry, &listener, &_gfx);
	wlDisplayRoundtrip(_gfx.wlDisplay);

	if (!_gfx.compositor) return WAYLAND_COMPOSITOR_NOT_FOUND~;

	if (!_gfx.wlCursorTheme)
	{
		_gfx.wlCursorTheme = wlCursorThemeLoad(null, 24, _gfx.shm);
		_gfx.cursorSurface = wlCompositorCreateSurface(_gfx.compositor);
	}

	char[4] blk;
	_gfx.hiddenMouse = _create_mouse_native(&blk, { 1, 1 }, RGBA8) ?? null;

	static XdgWmBaseListener wmBaseListener = { .ping = &_wl_xdg_wm_base_ping_cb };
	xdgWmBaseAddListener(_gfx.xdgWmBase, &wmBaseListener, null);

	_gfx.xkbContext = xkbContextNew(XKB_CONTEXT_NO_FLAGS);

	static WlDataDeviceListener dataDeviceListener = {
		.dataOffer = &_wl_data_device_data_offer, .enter = &_wl_data_device_nothing_enter_cb, .leave = &_wl_data_device_nothing_cb,
		.motion = &_wl_data_device_nothing_motion_cb, .drop = &_wl_data_device_nothing_cb, .selection = &_wl_data_device_selection_cb
	};

	if (_gfx.seat && _gfx.dataDeviceManager)
	{
		_gfx.dataDevice = wlDataDeviceManagerGetDataDevice(_gfx.dataDeviceManager, _gfx.seat);
		wlDataDeviceAddListener(_gfx.dataDevice, &dataDeviceListener, null);
	}
}

// private
fn void? _load_opengl_wayland()
{
	// Wayland only ever renders through EGL; "OpenGL" here just means EGL is ready.
	return load_egl();
}

fn void? _create_window_wayland(String name, GfxWindowFlags flags, GfxWindow* win) => @pool()
{
	win.src.surface = wlCompositorCreateSurface(_gfx.compositor);
	static WlSurfaceListener listener = {
		.enter = &_wl_surface_enter_cb,
		.leave = &_wl_surface_leave_cb,
		.preferredBufferScale = &_wl_surface_pref_scale_cb,
		.preferredBufferTransform = &_wl_surface_pref_transform_cb
	};
	wlSurfaceAddListener(win.src.surface, &listener, win);
	wlSurfaceSetUserData(win.src.surface, win);

	win.src.customCursorSurface = wlCompositorCreateSurface(_gfx.compositor);

	win.src.xdgSurface = xdgWmBaseGetXdgSurface(_gfx.xdgWmBase, win.src.surface);
	static XdgSurfaceListener surface_listener = { .configure = &_wl_xdg_surface_conf_cb };

	xdgSurfaceAddListener(win.src.xdgSurface, &surface_listener, win);
	xdgWmBaseSetUserData(_gfx.xdgWmBase, win);

	win.src.xdgToplevel = xdgSurfaceGetToplevel(win.src.xdgSurface);
	_xdg_toplevel_set_app_id(win.src.xdgToplevel, name.zstr_copy(tmem));
	_xdg_surface_set_window_geometry(win.src.xdgSurface, 0, 0, win.width, win.height);

	if (!win.internal.flags.transparent) _wl_set_opaque(win);

	static XdgToplevelListener toplevel_listener = { .configure = &_wl_xdg_toplevel_conf_cb, .close = &_wl_xdg_toplevel_close_cb };

	xdgToplevelAddListener(win.src.xdgToplevel, &toplevel_listener, win);

	if (_gfx.decorationManager)
	{
		uint decorationMode = ZXDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE;
		win.src.decoration = zxdgDecorationManagerV1GetToplevelDecoration(_gfx.decorationManager, win.src.xdgToplevel);
		static ZxdgToplevelDecorationV1Listener decoration_listener = { .configure = &_wl_xdg_dec_conf_cb };

		zxdgToplevelDecorationV1AddListener(win.src.decoration, &decoration_listener, win);
		if (flags.no_border) decorationMode = ZXDG_TOPLEVEL_DECORATION_V1_MODE_CLIENT_SIDE;
		zxdgToplevelDecorationV1SetMode(win.src.decoration, decorationMode);
	}

	if (_gfx.iconManager) xdgToplevelIconManagerV1SetIcon(_gfx.iconManager, win.src.xdgToplevel, null);

	wlSurfaceCommit(win.src.surface);

	while (!win.src.configured) wlDisplayDispatch(_gfx.wlDisplay);

	_window_set_flags(win, flags, {});
	win.src.backing = {};
	if (flags.open_gl) _window_create_opengl_context_native(win, get_global_opengl_hints())!;
	if (flags.egl) _window_create_egl_context_native(win, get_global_opengl_hints())!;
	if (!flags.open_gl && !flags.egl)
	{
		sz blank_size = (sz)win.width * win.height * 3;
		char* blank = calloc(blank_size);
		GfxSurface* blank_surface = create_surface(blank, ...win.size, BGR8, win)!;
		win.blit_surface(blank_surface);
		free(blank);
		blank_surface.release();
	}

}

fn void _monitor_node_free_wayland(GfxMonitorNode* node)
{
	if (node.output) wlOutputDestroy(node.output);
	if (node.xdg_output) zxdgOutputV1Destroy(node.xdg_output);
	if (node.modes)
	{
		free(node.modes);
		node.mode_count = 0;
	}
}

fn IntRect _monitor_get_workarea_wayland(GfxMonitor* monitor)
{
	return { ...monitor.origin, ...monitor.mode.size };
}

fn void? _monitor_request_mode_wayland(GfxMonitor* mon, GfxMonitorMode* mode, GfxMonitorModeRequest request)
{
	for (sz i = 0; i < mon.node.mode_count; i++)
	{
		if (!mode.equals_request(&mon.node.modes[i], request)) continue;
		(void)_monitor_set_mode_native(mon, &mon.node.modes[i]);
		return;
	}
	return MONITOR_MODE_CHANGE_FAILED~;
}

// private
fn GfxKey _physical_to_mapped_key_native_wayland(GfxKey key)
{
	XkbKeycode kc = key.to_api() + 8;
	XkbKeysym sym = xkbStateKeyGetOneSym(_gfx.xkbState, kc);
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

fn void _wait_for_event_wayland(Posix_pollfd[] fds, Duration duration)
{
	fds[0].fd = wlDisplayGetFd(_gfx.wlDisplay);

	// empty the queue
	while (wlDisplayPrepareRead(_gfx.wlDisplay))
	{
		// error occured when dispatching the queue
		if (wlDisplayDispatchPending(_gfx.wlDisplay) == -1) return;
	}

	// send any pending requests to the compositor
	while (wlDisplayFlush(_gfx.wlDisplay) == -1)
	{
		// queue is full dispatch them
		if (libc::errno() == EAGAIN)
		{
			if (wlDisplayDispatchPending(_gfx.wlDisplay) == -1) return;
		}
		else
		{
			return;
		}
	}

	bool infinity = duration == WAIT_FOREVER;
	bool once = true;
	Clock start = clock::now();
	while (!wlDisplayDispatchPending(_gfx.wlDisplay))
	{
		Duration diff = duration - start.to_now().to_duration();
		long ms = diff.to_ms();
		if (ms < 0 && once)
		{
			once = false;
			ms = 0;
		}
		if (ms < 0 || os::poll(&fds[0], 1, infinity || ms > CInt::max ? -1 : (CInt)ms) <= 0)
		{
			wlDisplayCancelRead(_gfx.wlDisplay);
			break;
		}
		once = false;
		if (wlDisplayReadEvents(_gfx.wlDisplay) == -1) return;
	}

	// queue contains events from read, dispatch them
	if (wlDisplayDispatchPending(_gfx.wlDisplay) == -1) return;
}

// private
fn void _poll_events_wayland()
{
	_reset_prev_state();

	while (wlDisplayFlush(_gfx.wlDisplay) == -1)
	{
		if (libc::errno() == EAGAIN)
		{
			if (wlDisplayDispatchPending(_gfx.wlDisplay) == -1) return;
		}
		else { return; }
	}

	if (_gfx.wlRepeatInfoRate != 0 && _gfx.lastKey != 0)
	{
		if (_gfx.lastKeyTime.to_now() >= time::NANO_DURATION_ZERO)
		{
			_wl_send_key_event(_gfx.lastKey);
			_gfx.lastKeyTime = clock::now() + time::from_float(1.0 / _gfx.wlRepeatInfoRate).to_nano();
		}
	}

	Posix_pollfd fds;
	fds.fd = wlDisplayGetFd(_gfx.wlDisplay);
	fds.events = (short)os::POLLIN;

	while (true)
	{
		while (wlDisplayPrepareRead(_gfx.wlDisplay) != 0)
		{
			if (wlDisplayDispatchPending(_gfx.wlDisplay) > 0) return;
		}

		if (os::poll(&fds, 1, 0) == 0) { wlDisplayCancelRead(_gfx.wlDisplay); return; }

		if (fds.revents & os::POLLIN)
		{
			wlDisplayReadEvents(_gfx.wlDisplay);
			if (wlDisplayDispatchPending(_gfx.wlDisplay) > 0) return;
		}
		else { wlDisplayCancelRead(_gfx.wlDisplay); }
	}
}

// private
fn void _poll_monitors_wayland()
{
	_gfx.monitors.primary = _gfx.monitors.list.head;
}

fn GfxDataTransfer? _read_clipboard_wayland(Allocator allocator, GfxDataTransferType type)
{
	if (type != TEXT || !_gfx.unixClipboard || !_gfx.unixClipboard.text) return CLIPBOARD_DID_NOT_CONTAIN_TYPE~;

	return { .type = TEXT, .text = _gfx.unixClipboard.text.copy(allocator) };
}

// private
fn void _window_blit_surface_wayland(GfxWindow* win, GfxSurface* surface)
{
	GfxPoint min = math::min(win.size, surface.size);
	surface.native.wlBuffer = wlShmPoolCreateBuffer(surface.native.pool, 0, ...min, surface.size.x * 4, WL_SHM_FORMAT_ARGB8888);
	copy_image_data(surface.native.buffer, min.x, min.y, surface.native.format, surface.data, surface.format, surface.convert_fn);

	wlSurfaceAttach(win.src.surface, surface.native.wlBuffer, 0, 0);
	wlSurfaceDamage(win.src.surface, 0, 0, ...min);
	wlSurfaceCommit(win.src.surface);
	wlBufferDestroy(surface.native.wlBuffer);
	surface.native.wlBuffer = null;
}

// private
fn void _window_close_wayland(GfxWindow* win)
{
	if (win.src.decoration) zxdgToplevelDecorationV1Destroy(win.src.decoration);
	if (win.src.xdgToplevel) xdgToplevelDestroy(win.src.xdgToplevel);
	wlSurfaceDestroy(win.src.customCursorSurface);
	if (win.src.lockedPointer) zwpLockedPointerV1Destroy(win.src.lockedPointer);
	if (win.src.icon) xdgToplevelIconV1Destroy(win.src.icon);
	xdgSurfaceDestroy(win.src.xdgSurface);
	wlSurfaceDestroy(win.src.surface);
}

fn void? _window_init_opengl_context_wayland(GfxWindow* win, GfxGlContext* context, GfxGlHints* hints)
{
	_window_init_egl_context_wayland(win, (GfxEglContext*)context, hints)!;
	win.src.backing = { .opengl };
	win.opengl_swap_interval(0);
}

// public
fn void _window_opengl_swap_interval_wayland(GfxWindow* win, int swapInterval)
{
	_window_egl_swap_interval(win, swapInterval);
}


fn WGPUSurface _window_create_wgpu_surface_wayland(GfxWindow* window, WGPUInstance instance)
{
	WgpuSurfaceSourceWaylandSurface fromWl;
	fromWl.chain.next = null;
	fromWl.chain.sType = WGPU_STYPE_SURFACE_SOURCE_WAYLAND_SURFACE;
	fromWl.display = _gfx.wlDisplay;
	fromWl.surface = window.src.surface;
	WgpuSurfaceDescriptor surfaceDesc;
	surfaceDesc.nextInChain = (WgpuChainedStruct*)&fromWl;
	return wgpuInstanceCreateSurface(instance, &surfaceDesc);
}

fn void _window_release_opengl_context_wayland(GfxWindow* win, GfxGlContext* ctx)
{
	_window_release_egl_context_wayland(win, (GfxEglContext*)ctx);
	win.src.ctx.native = null;
}

fn GfxPoint _window_fetch_size_wayland(GfxWindow* win) => win.get_size();
fn void _window_flash_wayland(GfxWindow* win, GfxFlashRequest request) {}
fn void _window_focus_wayland(GfxWindow* win) {}
fn bool _window_is_hidden_wayland(GfxWindow* win) => false;
fn bool _window_is_maximized_wayland(GfxWindow* win) => win.src.maximized;
fn bool _window_is_minimized_wayland(GfxWindow* win) => win.src.minimized;
fn void _window_raise_wayland(GfxWindow* win) {}
fn void _window_set_floating_wayland(GfxWindow* win, bool floating) {}
fn void _window_set_opacity_wayland(GfxWindow* win, char opacity) {}
fn void _window_set_mouse_passthrough_wayland(GfxWindow* win, bool passthrough) {}

fn void _window_hide_wayland(GfxWindow* win)
{
	wlSurfaceAttach(win.src.surface, null, 0, 0);
	wlSurfaceCommit(win.src.surface);
	win.internal.flags.hide = true;
}

fn void _window_make_opengl_current_context_wayland(GfxWindow* win)
{
	_window_make_current_egl_context(win);
}

fn void _window_maximize_wayland(GfxWindow* win)
{
	_toggle_wayland_maximized(win, true);
}

fn void _window_minimize_wayland(GfxWindow* win)
{
	if (_window_is_maximized_native(win)) return;
	win.internal.old_bounds = win.bounds;
	win.src.minimized = true;
	xdgToplevelSetMinimized(win.src.xdgToplevel);
}

fn void _window_move_mouse_wayland(GfxWindow* win, GfxPoint where)
{
	if (!_gfx.wp_pointer_warp) return;
	_wp_pointer_warp_v1_warp_pointer(_gfx.wp_pointer_warp, win.src.surface, _gfx.wlPointer, wlFixedFromInt(where.x), wlFixedFromInt(where.y), _gfx.mouseEnterSerial);
}

fn void _window_move_wayland(GfxWindow* win, GfxPoint where)
{
	win.origin = where;
}

fn void _window_resize_wayland(GfxWindow* win, GfxSize size)
{
	win.size = size;
	if (_gfx.compositor != null)
	{
		_xdg_surface_set_window_geometry(win.src.xdgSurface, 0, 0, ...size);
		if (win.src.ctx.egl && win.src.ctx.egl.egl_window) wl_egl_window_resize((WlEglWindow*)win.src.ctx.egl.egl_window, ...size, 0, 0);
	}
}

fn void _window_restore_wayland(GfxWindow* win)
{
	_toggle_wayland_maximized(win, false);
}

fn void _window_set_aspect_ratio_wayland(GfxWindow* win, int w, int h)
{
	if (w == 0 && h == 0) return;
	_xdg_toplevel_set_max_size(win.src.xdgToplevel, w, h);
}

fn void _window_set_border_wayland(GfxWindow* win, bool border)
{
	win.internal.flags.no_border = !border;
	if (_gfx.decorationManager)
	{
		zxdgToplevelDecorationV1SetMode(win.src.decoration, border ? ZXDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE : ZXDG_TOPLEVEL_DECORATION_V1_MODE_CLIENT_SIDE);
	}
}

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

fn void _window_set_max_size_wayland(GfxWindow* win, GfxSize size)
{
	_xdg_toplevel_set_max_size(win.src.xdgToplevel, ...size);
}

fn void _window_set_min_size_wayland(GfxWindow* win, GfxSize size)
{
	xdgToplevelSetMinSize(win.src.xdgToplevel, ...size);
}

fn bool _window_set_mouse_wayland(GfxWindow* win, GfxMouse* mouse)
{
	GfxSurface* surface = (GfxSurface*)mouse;
	win.src.usingCustomCursor = true;
	wlSurfaceAttach(win.src.customCursorSurface, surface.native.wlBuffer, 0, 0);
	wlSurfaceDamage(win.src.customCursorSurface, 0, 0, ...surface.size);
	wlSurfaceCommit(win.src.customCursorSurface);
	return true;
}


// private
fn void _window_set_raw_mouse_mode_wayland(GfxWindow* win, bool state)
{
	if (!_gfx.relativePointerManager) return;

	if (!state)
	{
		if (_gfx.relativePointer) zwpRelativePointerV1Destroy(_gfx.relativePointer);
		_gfx.relativePointer = null;
		return;
	}
	if (_gfx.relativePointer) return;

	_gfx.relativePointer = zwpRelativePointerManagerV1GetRelativePointer(_gfx.relativePointerManager, _gfx.wlPointer);
	static ZwpRelativePointerV1Listener listener = { .relativeMotion = &_wl_rel_pointer_motion_cb };
	zwpRelativePointerV1AddListener(_gfx.relativePointer, &listener, &_gfx);
}

fn void _window_opengl_swap_buffers_wayland(GfxWindow* win)
{
	_window_swap_egl_buffers(win);
}

fn void _window_show_wayland(GfxWindow* win)
{
	win.internal.flags.hide = false;
	wlSurfaceCommit(win.src.surface);
}

fn void? _write_clipboard_wayland(GfxDataTransfer* data)
{
	if (!_gfx.dataDeviceManager) return CLIPBOARD_COULD_NOT_BE_OPENED~;
	if (_gfx.unixClipboard)
	{
		free(_gfx.unixClipboard);
		_gfx.unixClipboard = null;
	}

	sz length = data.text.len;
	_gfx.unixClipboard = mem::new_with_padding(GfxDataTransfer, length + 1);
	char* data_ptr = (char*)_gfx.unixClipboard + GfxDataTransfer::size - 1;
	String s = (String)data_ptr[:length];
	s[..] = data.text[..];
	_gfx.unixClipboard.text = s;
	_gfx.unixClipboard.type = TEXT;

	GfxWindow* win = _gfx.kbOwner;
	if (win.src.dataSource)
	{
		wlDataSourceDestroy(win.src.dataSource);
		win.src.dataSource = null;
	}

	win.src.dataSource = wlDataDeviceManagerCreateDataSource(_gfx.dataDeviceManager);
	if (!win.src.dataSource) return CLIPBOARD_COULD_NOT_BE_OPENED~;

	static WlDataSourceListener listener = {
		.target = &_wl_data_source_nothing_string_cb,
		.action = &_wl_data_source_nothing_uint_cb,
		.dndDropPerformed = &_wl_data_source_nothing_cb,
		.dndFinished = &_wl_data_source_nothing_cb,
		.send = &_wl_data_source_send_cb,
		.cancelled = &_wl_data_source_cancelled_cb
	};

	wlDataSourceAddListener(win.src.dataSource, &listener, &_gfx);
	wlDataSourceOffer(win.src.dataSource, "text/plain;charset=utf-8");
}

// private
fn void? _init_surface_wayland(GfxSurface* surface, void* data, int w, int h, GfxFormat format, GfxWindow* window)
{
	surface.data = data;
	surface.size = { w, h };
	surface.format = format;

	int size = w * h * 4;
	int fd = _wl_create_shm_file((sz)size);
	if (fd < 0) return SURFACE_CREATION_FAILED~;
	defer catch surface.release();
	surface.native.pool = wlShmCreatePool(_gfx.shm, fd, (int)size);
	surface.native.buffer = posix::mmap(null, (usz)size, posix::PROT_READ | posix::PROT_WRITE, posix::MAP_SHARED, fd, 0);
	surface.native.fd = fd;
	surface.native.format = BGRA8;

	if (surface.native.buffer == posix::MAP_FAILED)
	{
		surface.native.buffer = null;
		return SURFACE_CREATION_FAILED~;
	}
}

fn void _surface_release_wayland(GfxSurface* surface)
{
	if (!surface.data) return;
	if (surface.native.pool) wlShmPoolDestroy(surface.native.pool);
	if (surface.native.fd) libc::close(surface.native.fd);
	if (surface.native.buffer) posix::munmap(surface.native.buffer, (usz)(surface.size.x * surface.size.y * 4));
	*surface = {};
}

fn bool _window_is_floating_wayland(GfxWindow* win) => !win.is_fullscreen() && !_window_is_maximized_native(win);

fn void _window_set_name_wayland(GfxWindow* win, String name)
{
	if (_gfx.compositor) @pool()
	{
		xdgToplevelSetTitle(win.src.xdgToplevel, name.zstr_copy(tmem));
	};
}

// private
fn bool _window_set_icon_wayland(GfxWindow* win, char* data, int w, int h, GfxFormat format, GfxWindowIconType type)
{
	if (!_gfx.iconManager || w != h) return false;

	if (win.src.icon != null) { xdgToplevelIconV1Destroy(win.src.icon); win.src.icon = null; }

	GfxSurface*? surface = create_surface(data, w, h, format);
	if (catch surface) return false;
	defer surface.destroy();
	int minW = w < surface.size.x ? w : surface.size.x;
	int minH = h < surface.size.y ? h : surface.size.y;
	copy_image_data(surface.native.buffer, minW, minH, surface.native.format, surface.data, surface.format, null);

	win.src.icon = xdgToplevelIconManagerV1CreateIcon(_gfx.iconManager);
	surface.native.wlBuffer = wlShmPoolCreateBuffer(surface.native.pool, 0, w, h, w * 4, WL_SHM_FORMAT_ARGB8888);
	xdgToplevelIconV1AddBuffer(win.src.icon, surface.native.wlBuffer, 1);
	xdgToplevelIconManagerV1SetIcon(_gfx.iconManager, win.src.xdgToplevel, win.src.icon);

	return true;
}

fn GfxMonitor*? _window_find_monitor_wayland(GfxWindow* win)
{
	if (!win.src.activeMonitor) return get_primary_monitor();
	return &win.src.activeMonitor.mon;
}

fn void _wl_pointer_enter(void* data, WlProxy* pointer, uint serial, WlProxy* surface, WlFixed surface_x, WlFixed surface_y)
{
	GfxContext* info = data;
	GfxWindow* win = (GfxWindow*)wlSurfaceGetUserData(surface);
	info.mouseEnterSerial = serial;
	win.internal.mouse_inside = true;
	info.window_state.mouse_enter = true;
	info.mouseOwner = win;

	if (win.src.usingCustomCursor)
	{
		wlPointerSetCursor(pointer, serial, win.src.customCursorSurface, 0, 0);
	}
	else
	{
		win.set_mouse_default();
	}

	int x = (int)surface_x.to_double();
	int y = (int)surface_y.to_double();
	_mouse_notify_callback(win, { x, y }, true);
}

fn void _toggle_wayland_maximized(GfxWindow* win, bool maximized)
{
	win.src.maximized = maximized;
	if (maximized)
	{
		xdgToplevelSetMaximized(win.src.xdgToplevel);
	}
	else
	{
		_xdg_toplevel_unset_maximized(win.src.xdgToplevel);
	}
}

fn void? _window_init_egl_context_wayland(GfxWindow* win, GfxEglContext* ctx, GfxGlHints* hints)
{
	_load_egl_native()!;

	if (!_gfx.egl.display)
	{
		_gfx.egl.display = egl_get_display_fn(_gfx.wlDisplay);
		if (!_gfx.egl.display) return EGL_DISPLAY_NOT_FOUND~;
		EglInt major, minor;
		if (!egl_initialize_fn(_gfx.egl.display, &major, &minor)) return EGL_DISPLAY_FAILED_TO_INITIALIZE~;
	}

	FixedList{EglInt, 32} stack;
	stack.push_all( {
		EGL_SURFACE_TYPE, EGL_WINDOW_BIT,
		EGL_RED_SIZE, hints.red,
		EGL_GREEN_SIZE, hints.green,
		EGL_BLUE_SIZE, hints.blue,
		EGL_ALPHA_SIZE, hints.alpha,
		EGL_DEPTH_SIZE, hints.depth,
		EGL_STENCIL_SIZE, hints.stencil,
		EGL_RENDERABLE_TYPE, hints.profile == ES ? EGL_OPENGL_ES2_BIT : EGL_OPENGL_BIT,
		EGL_NONE, EGL_NONE,
	});
	EglConfig config = null;
	EglInt num = 0;
	if (!egl_choose_config_fn((EglDisplay)_gfx.egl.display, stack.array_view(), &config, 1, &num) || !num) return EGL_CONFIG_NOT_FOUND~;
	egl_bind_api_fn(hints.profile == ES ? EGL_OPENGL_ES_API : EGL_OPENGL_API);

	stack.clear();
	if (hints.profile == ES) stack.push_all({ EGL_CONTEXT_CLIENT_VERSION, hints.major != 0 ? hints.major : 2 });
	stack.push_all({ EGL_NONE, EGL_NONE });

	EglContext share = hints.share_egl ? hints.share_egl.ctx : null;
	ctx.ctx = egl_create_context_fn(_gfx.egl.display, config, share, stack.array_view());
	if (!ctx.ctx) return EGL_CTX_CREATION_FAILED~;
	defer catch
	{
		egl_destroy_context_fn(_gfx.egl.display, ctx.ctx);
		ctx.ctx = null;
	}
	WlEglWindow* egl_window = wl_egl_window_create(win.src.surface, win.width, win.height);
	ctx.egl_window = egl_window;
	ctx.surface = egl_create_window_surface_fn(_gfx.egl.display, config, egl_window, null);
	if (!ctx.surface) return EGL_SURFACE_CREATION_FAILED~;
	win.src.ctx.egl = ctx;
	win.src.backing = { .egl };
	_window_make_current_egl_context(win);
}


fn void _window_release_egl_context_wayland(GfxWindow* win, GfxEglContext* ctx)
{
	if (ctx.surface) egl_destroy_surface_fn(_gfx.egl.display, ctx.surface);
	if (ctx.ctx) egl_destroy_context_fn(_gfx.egl.display, ctx.ctx);
	if (ctx.egl_window) wl_egl_window_destroy(ctx.egl_window);
	ctx.surface = null;
	ctx.ctx = null;
	ctx.egl_window = null;
	if (win) win.src.ctx.egl = null;
}

fn int _wl_anonymous_shm_open()
{
	for (int i = 0; i < 100; i++) @pool()
	{
		ZString filename = (ZString)string::tformat("/c3gfx-wayland-%x\0\0", rand(0xFFFFFFF));
		int fd = libc::shm_open(filename, libc::O_RDWR | libc::O_CREAT | libc::O_EXCL, 384);
		if (fd >= 0)
		{
			libc::shm_unlink(filename);
			return fd;
		}
	};
	return -1;
}

fn XkbKeysym _wl_compose_symbol(GfxContext* info, XkbKeysym sym)
{
	if (sym == XKB_KEY_NO_SYMBOL || !info.composeState) return sym;
	if (xkbComposeStateFeed(info.composeState, sym) != XKB_COMPOSE_FEED_ACCEPTED) return sym;
	int status = xkbComposeStateGetStatus(info.composeState);
	if (status == XKB_COMPOSE_COMPOSED) return xkbComposeStateGetOneSym(info.composeState);
	if (status == XKB_COMPOSE_COMPOSING || status == XKB_COMPOSE_CANCELLED) return XKB_KEY_NO_SYMBOL;
	return sym;
}

fn void _wl_create_outputs(WlProxy* registry, uint id)
{
	// wl_output name/description events require version 4+.
	// If wl_seat hasn't been bound yet (compositor announced wl_output first), fall back to
	// version 4 instead of calling wlProxyGetVersion(null), which would crash.
	uint fallbackVersion = 4;
	WlProxy* output = wlRegistryBind(registry, id, &wlOutputInterface, _gfx.seat == null ? fallbackVersion : wlProxyGetVersion(_gfx.seat));
	if (output == null) return;

	GfxMonitor mon;
	ZString s = string::tformat_zstr("monitor-%02d", _gfx.monitors.count);
	mem::copy(&mon.name, s, s.len() + 1);

	mon.scale = (Vec2)1;
	mon.pixelRatio = 1.0f;

	GfxMonitorNode* node = _monitor_add(&mon);
	if (node == null) return;

	node.mode_count = 0;
	node.disconnected = true;
	node.id = id;
	node.output = output;

	wlOutputSetUserData(output, node);

	static WlOutputListener wl_output_listener =
	{
		.geometry = &_wl_output_set_geometry_cb,
		.mode = &_wl_output_handle_mode_cb,
		.done = &_wl_output_handle_done_cb,
		.scale = &_wl_output_set_scale_cb,
		.name = &_wl_output_set_name_cb,
		.description = &_wl_output_desc_cb
	};

	wlOutputAddListener(output, &wl_output_listener, node);

	if (!_gfx.xdgOutputManager) return;

	node.xdg_output = zxdgOutputManagerV1GetXdgOutput(_gfx.xdgOutputManager, node.output);
	
	static ZxdgOutputV1Listener xdg_output_listener = {
		.name = &_wl_xdg_output_nothing_string_cb,
		.done = &_wl_xdg_output_nothing_cb, 
		.description = &_wl_xdg_output_nothing_string_cb,
		.logicalPosition = &_wl_xdg_output_logical_pos_cb, 
		.logicalSize = &_wl_xdg_output_logical_size_cb
	};

	zxdgOutputV1AddListener(node.xdg_output, &xdg_output_listener, node);
}

fn int _wl_create_shm_file(sz size)
{
	int fd = _wl_anonymous_shm_open();
	if (fd < 0) return fd;
	if (libc::ftruncate(fd, size) < 0)
	{
		libc::close(fd);
		return -1;
	}
	return fd;
}

fn void _wl_data_device_data_offer(void* data, WlProxy* device, WlProxy* offer)
{
	static WlDataOfferListener listener = {
		.offer = &_wl_data_offer_nothing_string_cb,
		.sourceActions = &_wl_data_offer_nothing_uint_cb,
		.action = &_wl_data_offer_nothing_uint_cb };
	wlDataOfferAddListener(offer, &listener, null);
}

fn void _wl_data_device_nothing_cb(void* data, WlProxy* device) {}
fn void _wl_data_device_nothing_enter_cb(void* data, WlProxy* device, uint serial, WlProxy* surface, WlFixed x, WlFixed y, WlProxy* offer) {}
fn void _wl_data_device_nothing_motion_cb(void* data, WlProxy* device, uint time, WlFixed x, WlFixed y) {}
fn void _wl_data_offer_nothing_string_cb(void* data, WlProxy* offer, ZString s) {}
fn void _wl_data_offer_nothing_uint_cb(void* data, WlProxy* offer, uint u) {}
fn void _wl_data_source_nothing_cb(void* data, WlProxy* source) {}
fn void _wl_data_source_nothing_string_cb(void* data, WlProxy* source, ZString s) {}
fn void _wl_data_source_nothing_uint_cb(void* data, WlProxy* source, uint u) {}
fn void _wl_xdg_output_nothing_cb(void* data, WlProxy* xdgOutput) {}
fn void _wl_xdg_output_nothing_string_cb(void* data, WlProxy* xdgOutput, ZString s) {}

fn void _wl_seat_name_cb(void* data, WlProxy* seat, ZString name) {}
fn void _wl_surface_leave_cb(void* data, WlProxy* surface, WlProxy* output) {}
fn void _wl_surface_pref_scale_cb(void* data, WlProxy* surface, int scale) {}
fn void _wl_surface_pref_transform_cb(void* data, WlProxy* surface, uint transform) {}
fn void _wl_unlocked_noop_cb(void* data, WlProxy* p) {}
fn void _wl_output_desc_cb(void* data, WlProxy* output, ZString description) {}
fn void _wl_shm_format_cb(void* data, WlProxy* shm, uint format) {}


fn void _wl_data_source_cancelled_cb(void* data, WlProxy* source)
{
	GfxContext* info = data;
	if (info.kbOwner && info.kbOwner.src.dataSource == source) info.kbOwner.src.dataSource = null;
	wlDataSourceDestroy(source);
}

fn void _wl_data_source_send_cb(void* data, WlProxy* source, ZString mime_type, int fd)
{
	if (mime_type && mime_type.str_view().contains("text/plain"))
	{
		String s = _gfx.unixClipboard.text;
		libc::write(fd, s, s.len);
	}
	libc::close(fd);
}

fn void _wl_global_registry_cb(void* data, WlProxy* registry, uint id, ZString iface_str, uint version)
{
	GfxContext* info = data;
	String iface = iface_str.str_view();
	switch (iface)
	{
		case "wl_compositor":
			info.compositor = wlRegistryBind(registry, id, &wlCompositorInterface, 4);
		case "xdg_wm_base":
			info.xdgWmBase = wlRegistryBind(registry, id, &xdgWmBaseInterface, 1);
		case zxdgDecorationManagerV1Interface.name.str_view():
			info.decorationManager = wlRegistryBind(registry, id, &zxdgDecorationManagerV1Interface, 1);
		case zwpPointerConstraintsV1Interface.name.str_view():
			info.constraintManager = wlRegistryBind(registry, id, &zwpPointerConstraintsV1Interface, 1);
		case zwpRelativePointerManagerV1Interface.name.str_view():
			info.relativePointerManager = wlRegistryBind(registry, id, &zwpRelativePointerManagerV1Interface, 1);
		case xdgToplevelIconManagerV1Interface.name.str_view():
			info.iconManager = wlRegistryBind(registry, id, &xdgToplevelIconManagerV1Interface, 1);
		case "wl_shm":
			info.shm = wlRegistryBind(registry, id, &wlShmInterface, 1);
			static WlShmListener listener = { .format = &_wl_shm_format_cb };
			wlShmAddListener(info.shm, &listener, info);
		case "wl_seat":
			info.seat = wlRegistryBind(registry, id, &wlSeatInterface, version < 4 ? 3 : 4);
			static WlSeatListener listener = { .capabilities = &_wl_seat_capabilities_cb, .name = &_wl_seat_name_cb };
			wlSeatAddListener(info.seat, &listener, info);
		case zxdgOutputManagerV1Interface.name.str_view():
			info.xdgOutputManager = wlRegistryBind(registry, id, &zxdgOutputManagerV1Interface, 1);
		case "wl_output":
			_wl_create_outputs(registry, id);
		case wpPointerWarpV1Interface.name.str_view():
			info.wp_pointer_warp = wlRegistryBind(registry, id, &wpPointerWarpV1Interface, 1);
		case "wl_data_device_manager":
			info.dataDeviceManager = wlRegistryBind(registry, id, &wlDataDeviceManagerInterface, 1);
	}
}

fn void _wl_global_registry_remove_cb(void* data, WlProxy* registry, uint id)
{
	GfxContext* info = data;
	GfxMonitorNode* prev = null;
	GfxMonitorNode* node = info.monitors.list.head;
	while (node)
	{
		if (node.id == id)
		{
			_monitor_callback(_gfx.root, &node.mon, false);
			_remove_monitor(node, prev);
			return;
		}
		prev = node;
		node = node.next;
	}
}

fn void _wl_keyboard_enter_cb(void* data, WlProxy* keyboard, uint serial, WlProxy* surface, WlArray* keys)
{
	GfxContext* info = data;
	GfxWindow* win = (GfxWindow*)wlSurfaceGetUserData(surface);
	info.kbOwner = win;

	if (info.dataDevice && win.src.dataSource) wlDataDeviceSetSelection(info.dataDevice, win.src.dataSource, serial);
	if (win.src.minimized) win.src.minimized = false;
	_window_focus_callback(win, true);
}

fn void _wl_keyboard_key_cb(void* data, WlProxy* keyboard, uint serial, Clock time, uint key, uint state)
{
	GfxContext* info = data;
	if (!info.kbOwner) return;

	GfxWindow* win = info.kbOwner;
	GfxKey gfx_key = key_from_api(key + 8);

	_key_update_key_mods(win, xkbKeymapModGetIndex(info.keymap, "Lock") != 0, xkbKeymapModGetIndex(info.keymap, "Mod2") != 0, xkbKeymapModGetIndex(info.keymap, "ScrollLock") != 0);
	_key_callback(win, gfx_key, win.internal.mod, is_key_down(gfx_key) && state, !!state);

	info.lastKey = state ? key : 0;
	info.lastKeyTime = time + time::ms(_gfx.wlRepeatInfoDelay);
	if (state) _wl_send_key_event(_gfx.lastKey);
}

fn void _wl_keyboard_keymap_cb(void* data, WlProxy* keyboard, uint format, int fd, uint size) => @pool()
{
	GfxContext* info = data;
	ZString keymapString = (ZString)posix::mmap(null, size, posix::PROT_READ, posix::MAP_SHARED, fd, 0);
	xkbKeymapUnref(info.keymap);
	info.keymap = xkbKeymapNewFromString(info.xkbContext, keymapString, XKB_KEYMAP_FORMAT_TEXT_V1, XKB_KEYMAP_COMPILE_NO_FLAGS);
	posix::munmap(keymapString, size);
	libc::close(fd);
	xkbStateUnref(info.xkbState);
	info.xkbState = xkbStateNew(info.keymap);

	String locale = env::tget_var("LC_ALL") ?? env::tget_var("LC_CTYPE") ?? env::tget_var("LANG") ?? "C";

	XkbComposeTable* composeTable = xkbComposeTableNewFromLocale(info.xkbContext, (ZString)locale, XKB_COMPOSE_COMPILE_NO_FLAGS);
	if (composeTable)
	{
		info.composeState = xkbComposeStateNew(composeTable, XKB_COMPOSE_STATE_NO_FLAGS);
		xkbComposeTableUnref(composeTable);
	}
}

fn void _wl_keyboard_leave_cb(void* data, WlProxy* keyboard, uint serial, WlProxy* surface)
{
	GfxContext* info = data;
	GfxWindow* win = (GfxWindow*)wlSurfaceGetUserData(surface);
	if (info.kbOwner == win) info.kbOwner = null;
	_window_focus_callback(win, false);
}

fn void _wl_keyboard_mods_cb(void* data, WlProxy* keyboard, uint serial, uint modsDepressed, uint modsLatched, uint modsLocked, uint group)
{
	GfxContext* info = data;
	xkbStateUpdateMask(info.xkbState, modsDepressed, modsLatched, modsLocked, 0, 0, group);
}

fn void _wl_keyboard_repeat_info_cb(void* data, WlProxy* keyboard, int rate, int delay)
{
	_gfx.wlRepeatInfoRate = rate;
	_gfx.wlRepeatInfoDelay = delay;
}


fn void _wl_output_handle_done_cb(void* data, WlProxy* output)
{
	GfxMonitorNode* node = (GfxMonitorNode*)data;
	GfxMonitor* mon = &node.mon;

	if (mon.phys_size.x <= 0 || mon.phys_size.y <= 0)
	{
		mon.phys_size = (Vec2)mon.mode.size / 96.0f;
	}

	if (!node.disconnected) return;
	node.disconnected = false;
	_monitor_callback(_gfx.root, mon, true);
}

fn void _wl_output_handle_mode_cb(void* data, WlProxy* output, uint flags, int width, int height, int refresh)
{
	GfxMonitorNode* node = (GfxMonitorNode*)data;
	GfxMonitor* mon = &node.mon;

	GfxMonitorMode mode;
	mode.size = { width, height };
	mode.refresh_rate = refresh / 1000.0f;
	mode.src = output;

	node.mode_count += 1;
	GfxMonitorMode* modes = malloc(node.mode_count * GfxMonitorMode::size);
	if (node.mode_count > 1)
	{
		modes[:node.mode_count - 1] = node.modes[:node.mode_count - 1];
		free(node.modes);
	}
	modes[node.mode_count - 1] = mode;
	node.modes = modes;

	if (flags & WL_OUTPUT_MODE_CURRENT) mon.mode = mode;
}

fn void _wl_output_set_geometry_cb(void* data, WlProxy* output, int x, int y, int physW, int physH, int subpixel, ZString make, ZString model, int transform)
{
	GfxMonitor* mon = &((GfxMonitorNode*)data).mon;
	mon.origin = { x, y };
	mon.phys_size = { physW / 25.4f, physH / 25.4f };
}

fn void _wl_output_set_name_cb(void* data, WlProxy* output, ZString name)
{
	GfxMonitor* mon = &((GfxMonitorNode*)data).mon;
	sz n = name.len();
	if (n > 127) n = 127;
	mem::copy(&mon.name, name, n);
	mon.name[n] = 0;
}

fn void _wl_output_set_scale_cb(void* data, WlProxy* output, int factor)
{
	GfxMonitor* mon = &((GfxMonitorNode*)data).mon;
	mon.scale = (Vec2)factor;
}

fn void _wl_pointer_axis_cb(void* data, WlProxy* pointer, uint time, uint axis, WlFixed value)
{
	GfxContext* info = data;
	GfxWindow* win = info.mouseOwner;
	if (!win) return;
	Vec2 scroll = {};
	if (!(win.internal.enabled_events.mouse_scroll)) return;
	if (axis == WL_POINTER_AXIS_HORIZONTAL_SCROLL)
	{
		scroll.x = (float)(-value.to_double() / 10.0);
	}
	else if (axis == WL_POINTER_AXIS_VERTICAL_SCROLL)
	{
		scroll.y = (float)(-value.to_double() / 10.0);
	}
	_mouse_scroll_callback(win, scroll);
}

fn void _wl_pointer_button_cb(void* data, WlProxy* pointer, uint serial, uint time, uint button, uint state)
{
	GfxContext* info = data;
	GfxWindow* win = info.mouseOwner;
	if (!win) return;
	uint b = button - 0x110;
	switch (b)
	{
		case 1: b = 2;
		case 2: b = 1;
	}
	_mouse_button_callback(win, (GfxMouseButton)b, !!state);
}

fn void _wl_pointer_leave_cb(void* data, WlProxy* pointer, uint serial, WlProxy* surface)
{
	GfxWindow* win = (GfxWindow*)wlSurfaceGetUserData(surface);
	GfxContext* info = data;
	if (info.mouseOwner == win) info.mouseOwner = null;
	_mouse_notify_callback(win, win.internal.last_mouse, false);
}

fn void _wl_pointer_locked_cb(void* data, WlProxy* lockedPointer)
{
	GfxContext* info = data;
	wlPointerSetCursor(info.wlPointer, info.mouseEnterSerial, null, 0, 0);
}

fn void _wl_pointer_motion_cb(void* data, WlProxy* pointer, uint time, WlFixed x, WlFixed y)
{
	GfxContext* info = data;
	GfxWindow* win = info.mouseOwner;
	if (!win) return;
	int cx = (int)x.to_double();
	int cy = (int)y.to_double();
	_mouse_motion_callback(win, { cx, cy });
}

fn void _wl_rel_pointer_motion_cb(void* data, WlProxy* relPointer, uint timeHi, uint timeLo, WlFixed dx, WlFixed dy, WlFixed dxUnaccel, WlFixed dyUnaccel)
{
	GfxContext* info = data;
	GfxWindow* win = info.mouseOwner;
	if (!win) return;
	_raw_motion_callback(win, { (float)dx.to_double(), (float)dy.to_double() });
}

fn void _wl_seat_capabilities_cb(void* data, WlProxy* seat, uint capabilities)
{
	GfxContext* info = data;

	if (capabilities & WL_SEAT_CAPABILITY_POINTER && !info.wlPointer)
	{
		info.wlPointer = wlSeatGetPointer(seat);
		static WlPointerListener listener = {
			.enter = &_wl_pointer_enter,
			.leave = &_wl_pointer_leave_cb,
			.motion = &_wl_pointer_motion_cb,
			.button = &_wl_pointer_button_cb,
			.axis = &_wl_pointer_axis_cb
		};
		wlPointerAddListener(info.wlPointer, &listener, info);
	}
	if (capabilities & WL_SEAT_CAPABILITY_KEYBOARD && !info.wlKeyboard)
	{
		info.wlKeyboard = wlSeatGetKeyboard(seat);
		static WlKeyboardListener listener = {
			.keymap = &_wl_keyboard_keymap_cb,
			.enter = &_wl_keyboard_enter_cb,
			.leave = &_wl_keyboard_leave_cb,
			.key = &_wl_keyboard_key_cb,
			.modifiers = &_wl_keyboard_mods_cb,
			.repeatInfo = &_wl_keyboard_repeat_info_cb
		};
		wlKeyboardAddListener(info.wlKeyboard, &listener, info);
	}
	if (!(capabilities & WL_SEAT_CAPABILITY_POINTER) && info.wlPointer) wlPointerDestroy(info.wlPointer);
	if (!(capabilities & WL_SEAT_CAPABILITY_KEYBOARD) && info.wlKeyboard) wlKeyboardDestroy(info.wlKeyboard);
}

fn void _wl_surface_enter_cb(void* data, WlProxy* surface, WlProxy* output)
{
	GfxWindow* win = (GfxWindow*)data;
	GfxMonitorNode* node = (GfxMonitorNode*)wlOutputGetUserData(output);
	if (!node) return;
	win.src.activeMonitor = node;
}

fn void _wl_xdg_dec_conf_cb(void* data, WlProxy* decoration, uint mode)
{
	GfxWindow* win = (GfxWindow*)data;
	if (mode != win.src.decorationMode) win.src.decorationMode = mode;
}


fn void _wl_xdg_surface_conf_cb(void* data, WlProxy* xdgSurface, uint serial)
{
	xdgSurfaceAckConfigure(xdgSurface, serial);
	GfxWindow* win = (GfxWindow*)data ?: _gfx.kbOwner;
	if (!win) return;

	if (win.src.activated != win.src.pendingActivated) win.src.activated = win.src.pendingActivated;

	if (win.src.maximized != win.src.pendingMaximized)
	{
		_toggle_wayland_maximized(win, win.src.pendingMaximized);
		_window_check_mode(win);
	}

	if (win.src.resizing)
	{
		_window_resized_callback(win, win.size);
		win.resize(win.size);
		if (!(win.internal.flags.transparent)) _wl_set_opaque(win);
	}

	win.src.configured = true;
}

fn void _wl_xdg_toplevel_close_cb(void* data, WlProxy* toplevel)
{
	GfxWindow* win = (GfxWindow*)data;
	if (!win.internal.shouldClose) _window_close_callback(win);
}

fn void _wl_xdg_toplevel_conf_cb(void* data, WlProxy* toplevel, int width, int height, WlArray* states)
{
	GfxWindow* win = (GfxWindow*)data;
	win.src.pendingActivated = false;
	win.src.pendingMaximized = false;
	win.src.resizing = false;

	if (states)
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

fn void _wl_xdg_wm_base_ping_cb(void* data, WlProxy* wmBase, uint serial)
{
	xdgWmBasePong(wmBase, serial);
}

fn void _wl_xdg_output_logical_pos_cb(void* data, WlProxy* xdgOutput, int x, int y)
{
	GfxMonitor* mon = &((GfxMonitorNode*)data).mon;
	mon.origin = { x, y };
}

fn void _wl_xdg_output_logical_size_cb(void* data, WlProxy* xdgOutput, int width, int height)
{
	GfxMonitor* mon = &((GfxMonitorNode*)data).mon;
	float monFloatWidth = (float)mon.mode.size.x;
	float scaleX = monFloatWidth / (float)width;
	float dpi = scaleX * 96.0f;
	mon.pixelRatio = dpi >= 192.0f ? 2.0f : 1.0f;
	mon.mode.size = { width, height };
}

fn void _wl_data_device_selection_cb(void* data, WlProxy* device, WlProxy* offer)
{
	if (!offer) return;

	int[2] pfds;
	posix::pipe(&pfds);
	wlDataOfferReceive(offer, "text/plain", pfds[1]);
	libc::close(pfds[1]);
	wlDisplayRoundtrip(_gfx.wlDisplay);

	char[1024] buf;
	sz n = libc::read(pfds[0], &buf, 1024);
	if (n <= 0)
	{
		libc::close(pfds[0]);
		wlDataOfferDestroy(offer);
		return;
	}

	sz length = (sz)n;
	if (buf[n - 1] != 0) length += 1;

	if (_gfx.unixClipboard != null) free(_gfx.unixClipboard);
	_gfx.unixClipboard = mem::new_with_padding(GfxDataTransfer, length);
	char* data_ptr = (char*)_gfx.unixClipboard + GfxDataTransfer::size - 1;
	String s = (String)data_ptr[:length - 1];
	data_ptr[:s.len] = buf[:s.len];
	_gfx.unixClipboard.text = s;
	_gfx.unixClipboard.type = TEXT;

	libc::close(pfds[0]);
	wlDataOfferDestroy(offer);
}



// -- helpers
fn void _wl_send_key_event(uint key)
{
	XkbKeysym* keysyms;
	if (xkbStateKeyGetSyms(_gfx.xkbState, key + 8, &keysyms) == 1)
	{
		XkbKeysym keysym = _wl_compose_symbol(&_gfx, keysyms[0]);
		uint codepoint = xkbKeysymToUtf32(keysym);
		if (codepoint != 0) _key_char_callback(_gfx.kbOwner, codepoint);
	}
}

fn void _wl_set_opaque(GfxWindow* win)
{
	WlProxy* region = wlCompositorCreateRegion(_gfx.compositor);
	if (!region) return;
	wlRegionAdd(region, 0, 0, win.width, win.height);
	wlSurfaceSetOpaqueRegion(win.src.surface, region);
	wlRegionDestroy(region);
}

// -- wgpu
const WGPUSType WGPU_STYPE_SURFACE_SOURCE_WAYLAND_SURFACE = 0x00000007;
struct WgpuSurfaceSourceWaylandSurface
{
	WgpuChainedStruct chain;
	WlDisplay* display;
	WlProxy* surface;
}

// --- Vulkan


alias VkGetPhysicalDeviceWaylandPresentationSupportKHRProc = fn uint(VkPhysicalDevice, uint, WlDisplay*);

fn bool _vulkan_get_physical_device_presentation_support_wayland(VkInstance instance, VkPhysicalDevice physical_device, uint queue_family_index)
{
	GfxFn raw = _get_instance_proc_vulkan(instance, "vkGetPhysicalDeviceWaylandPresentationSupportKHR");
	if (raw == null) return false;
	uint out = ((VkGetPhysicalDeviceWaylandPresentationSupportKHRProc)raw)(physical_device, queue_family_index, _gfx.wlDisplay);
	return out != 0;
}

struct VkWaylandSurfaceCreateInfoKHR
{
	int sType;
	void* pNext;
	uint flags;
	WlDisplay* display;
	WlProxy* surface;
}

fn VkSurfaceKHR? _window_create_vulkan_surface_wayland(GfxWindow* win, VkInstance instance) @private
{
	VkSurfaceKHR surface;
	VkCreateSurfaceKHRProc proc = (VkCreateSurfaceKHRProc)_get_instance_proc_vulkan(instance, "vkCreateWaylandSurfaceKHR");
	if (!proc) return VULKAN_NOT_LOADED~;
	VkWaylandSurfaceCreateInfoKHR create_info = {
		.sType = VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR,
		.display = _gfx.wlDisplay,
		.surface = win.src.surface
	};
	if (proc(instance, &create_info, null, &surface)) return SURFACE_CREATION_FAILED~;
	return surface;
}

// xdg_wm_base
WlMessage[4] xdg_wm_base_requests = {
	{ "destroy", "", null },
	{ "create_positioner", "n", null },
	{ "get_xdg_surface", "no", null },
	{ "pong", "u", null },
};
WlMessage[1] xdg_wm_base_events = {
	{ "ping", "u", null },
};
WlInterface xdgWmBaseInterface @weak @export("xdg_wm_base_interface") = {
	.name = "xdg_wm_base", .version = 7,
	.methodCount = 4, .methods = &xdg_wm_base_requests[0],
	.eventCount = 1, .events = &xdg_wm_base_events[0],
};

// xdg_surface
WlMessage[5] xdg_surface_requests = {
	{ "destroy", "", null },
	{ "get_toplevel", "n", null },
	{ "get_popup", "n?oo", null },
	{ "set_window_geometry", "iiii", null },
	{ "ack_configure", "u", null },
};
WlMessage[1] xdg_surface_events = {
	{ "configure", "u", null },
};
WlInterface xdgSurfaceInterface @weak @export("xdg_surface_interface") = {
	.name = "xdg_surface", .version = 7,
	.methodCount = 5, .methods = &xdg_surface_requests[0],
	.eventCount = 1, .events = &xdg_surface_events[0],
};

// xdg_toplevel
WlMessage[14] xdg_toplevel_requests = {
	{ "destroy", "", null },
	{ "set_parent", "?o", null },
	{ "set_title", "s", null },
	{ "set_app_id", "s", null },
	{ "show_window_menu", "ouii", null },
	{ "move", "ou", null },
	{ "resize", "ouu", null },
	{ "set_max_size", "ii", null },
	{ "set_min_size", "ii", null },
	{ "set_maximized", "", null },
	{ "unset_maximized", "", null },
	{ "set_fullscreen", "?o", null },
	{ "unset_fullscreen", "", null },
	{ "set_minimized", "", null },
};
WlMessage[4] xdg_toplevel_events = {
	{ "configure", "iia", null },
	{ "close", "", null },
	{ "configure_bounds", "4ii", null },
	{ "wm_capabilities", "5a", null },
};
WlInterface xdgToplevelInterface @weak @export("xdg_toplevel_interface") = {
	.name = "xdg_toplevel", .version = 7,
	.methodCount = 14, .methods = &xdg_toplevel_requests[0],
	.eventCount = 4, .events = &xdg_toplevel_events[0],
};

// zxdg_decoration_manager_v1
WlMessage[2] zxdg_decoration_manager_v1_requests = {
	{ "destroy", "", null },
	{ "get_toplevel_decoration", "no", null },
};
WlInterface zxdgDecorationManagerV1Interface @weak @export("zxdg_decoration_manager_v1_interface") = {
	.name = "zxdg_decoration_manager_v1", .version = 2,
	.methodCount = 2, .methods = &zxdg_decoration_manager_v1_requests[0],
	.eventCount = 0, .events = null,
};

WlMessage[3] zxdg_toplevel_decoration_v1_requests = {
	{ "destroy", "", null },
	{ "set_mode", "u", null },
	{ "unset_mode", "", null },
};
WlMessage[1] zxdg_toplevel_decoration_v1_events = {
	{ "configure", "u", null },
};
WlInterface zxdgToplevelDecorationV1Interface @weak @export("zxdg_toplevel_decoration_v1_interface") = {
	.name = "zxdg_toplevel_decoration_v1", .version = 2,
	.methodCount = 3, .methods = &zxdg_toplevel_decoration_v1_requests[0],
	.eventCount = 1, .events = &zxdg_toplevel_decoration_v1_events[0],
};

// zwp_pointer_constraints_v1
WlMessage[3] zwp_pointer_constraints_v1_requests = {
	{ "destroy", "", null },
	{ "lock_pointer", "noo?ou", null },
	{ "confine_pointer", "noo?ou", null },
};
WlInterface zwpPointerConstraintsV1Interface @weak @export("zwp_pointer_constraints_v1_interface") = {
	.name = "zwp_pointer_constraints_v1", .version = 1,
	.methodCount = 3, .methods = &zwp_pointer_constraints_v1_requests[0],
	.eventCount = 0, .events = null,
};

WlMessage[3] zwp_locked_pointer_v1_requests = {
	{ "destroy", "", null },
	{ "set_cursor_position_hint", "ff", null },
	{ "set_region", "?o", null },
};
WlMessage[2] zwp_locked_pointer_v1_events = {
	{ "locked", "", null },
	{ "unlocked", "", null },
};
WlInterface zwpLockedPointerV1Interface @weak @export("zwp_locked_pointer_v1_interface") = {
	.name = "zwp_locked_pointer_v1", .version = 1,
	.methodCount = 3, .methods = &zwp_locked_pointer_v1_requests[0],
	.eventCount = 2, .events = &zwp_locked_pointer_v1_events[0],
};

// zwp_relative_pointer_manager_v1
WlMessage[2] zwp_relative_pointer_manager_v1_requests = {
	{ "destroy", "", null },
	{ "get_relative_pointer", "no", null },
};
WlInterface zwpRelativePointerManagerV1Interface @weak @export("zwp_relative_pointer_manager_v1_interface") = {
	.name = "zwp_relative_pointer_manager_v1", .version = 1,
	.methodCount = 2, .methods = &zwp_relative_pointer_manager_v1_requests[0],
	.eventCount = 0, .events = null,
};

WlMessage[1] zwp_relative_pointer_v1_requests = {
	{ "destroy", "", null },
};
WlMessage[1] zwp_relative_pointer_v1_events = {
	{ "relative_motion", "uuffff", null },
};
WlInterface zwpRelativePointerV1Interface @weak @export("zwp_relative_pointer_v1_interface") = {
	.name = "zwp_relative_pointer_v1", .version = 1,
	.methodCount = 1, .methods = &zwp_relative_pointer_v1_requests[0],
	.eventCount = 1, .events = &zwp_relative_pointer_v1_events[0],
};

// zxdg_output_manager_v1
WlMessage[2] zxdg_output_manager_v1_requests = {
	{ "destroy", "", null },
	{ "get_xdg_output", "no", null },
};
WlInterface zxdgOutputManagerV1Interface @weak @export("zxdg_output_manager_v1_interface") = {
	.name = "zxdg_output_manager_v1", .version = 3,
	.methodCount = 2, .methods = &zxdg_output_manager_v1_requests[0],
	.eventCount = 0, .events = null,
};

WlMessage[1] zxdg_output_v1_requests = {
	{ "destroy", "", null },
};
WlMessage[5] zxdg_output_v1_events = {
	{ "logical_position", "ii", null },
	{ "logical_size", "ii", null },
	{ "done", "", null },
	{ "name", "2s", null },
	{ "description", "2s", null },
};
WlInterface zxdgOutputV1Interface @weak @export("zxdg_output_v1_interface") = {
	.name = "zxdg_output_v1", .version = 3,
	.methodCount = 1, .methods = &zxdg_output_v1_requests[0],
	.eventCount = 5, .events = &zxdg_output_v1_events[0],
};

// xdg_toplevel_icon_manager_v1
WlMessage[3] xdg_toplevel_icon_manager_v1_requests = {
	{ "destroy", "", null },
	{ "create_icon", "n", null },
	{ "set_icon", "o?o", null },
};
WlMessage[2] xdg_toplevel_icon_manager_v1_events = {
	{ "icon_size", "i", null },
	{ "done", "", null },
};
WlInterface xdgToplevelIconManagerV1Interface @weak @export("xdg_toplevel_icon_manager_v1_interface") = {
	.name = "xdg_toplevel_icon_manager_v1", .version = 1,
	.methodCount = 3, .methods = &xdg_toplevel_icon_manager_v1_requests[0],
	.eventCount = 2, .events = &xdg_toplevel_icon_manager_v1_events[0],
};

WlMessage[3] xdg_toplevel_icon_v1_requests = {
	{ "destroy", "", null },
	{ "set_name", "s", null },
	{ "add_buffer", "oi", null },
};
WlInterface xdgToplevelIconV1Interface @weak @export("xdg_toplevel_icon_v1_interface") = {
	.name = "xdg_toplevel_icon_v1", .version = 1,
	.methodCount = 3, .methods = &xdg_toplevel_icon_v1_requests[0],
	.eventCount = 0, .events = null,
};

// wp_pointer_warp_v1
WlMessage[2] wp_pointer_warp_v1_requests = {
	{ "destroy", "", null },
	{ "warp_pointer", "ooffu", null },
};
WlInterface wpPointerWarpV1Interface @weak @export("wp_pointer_warp_v1_interface") = {
	.name = "wp_pointer_warp_v1", .version = 1,
	.methodCount = 2, .methods = &wp_pointer_warp_v1_requests[0],
	.eventCount = 0, .events = null,
};