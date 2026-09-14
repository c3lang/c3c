module std::gfx @feat(LINUX, BSD, MACOS, WASM, WIN32) @private;
import libc, std::math, std::collections::list, std::os, std::gfx::x11 @public, std::time;

const MAX_EVENTS = 32;
const PREALLOCATED_MONITORS = 0;

const WAYLAND_ONLY = $feat(WAYLAND) && $feat(NO_X11);
const X11_ONLY = !$feat(WAYLAND);
const X11_WAYLAND = $feat(BSD) || $feat(LINUX);

struct GfxContext
{
	bool initialized;
	List{GfxWindow} windows;
	GfxWindow* root;
	int active_windows;
	GfxMouse* hiddenMouse;
	GfxMouse*[21] standardMice;
	GfxGenericFunc[25] callbacks;
	GfxEvent[MAX_EVENTS] events;
	int eventBottom;
	int eventLen;
	bool queueEvents;
	bool polledEvents;
	uint[GFX_KEYS] apiKeycodes;
	GfxKey[$feat(WIN32) ? 512 : 256] keycodes;
	bool stop_check_events;
	ulong timerOffset;
	int[3] eventWait_forceStop @feat(LINUX, BSD);
	GfxDataTransfer* clipboard;
	bool dnd_build;
	GfxDataDropNode* dndRoot;
	GfxDataDropNode* dndCur;

	struct monitors
	{
		GfxMonitorList list;
		sz count;
		GfxMonitorNode* primary;
	}
	GfxWindow* current;

	struct gl
	{
		CFBundleRef native_handle @feat(MACOS);
		DylibHandle native_handle @feat(!MACOS);
		GfxGetProcFn get_proc_addr_fn;
	}
	DylibHandle vulkan_handle;
	GfxVkGetInstanceProcAddressProc vk_get_instance_proc_address;
	struct egl
	{
		void* display;
		DylibHandle handle;
		GfxGetProcFn get_proc_addr_fn;
	}
	struct @feat(WIN32)
	{
		Win32_HINSTANCE instance;
		Win32_WNDCLASSW wndClass;
		ushort[256]     wideClassName;
		Win32_HWND      helper_window;
	}
	struct @feat(WASM)
	{
		int usingAsyncify;
	}
	struct @feat(MACOS)
	{
		void* x11_handle;
		void* x11_display;
		CULong x11_root;
		void* wayland_handle;
		void* wl_display;
		ObjId ns_app;
		void* ns_pool;
		NSUInteger windowNumberSeed;
	}
	GfxDataTransfer* unixClipboard @feat(LINUX, BSD);

	struct @feat(WAYLAND)
	{
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
		WlProxy* wp_pointer_warp;
		WlProxy* dataDeviceManager;
		WlProxy* dataDevice;
		WlCursorTheme* wlCursorTheme;
		WlProxy* cursorSurface;
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
		Clock lastKeyTime;
	}
	struct @feat(LINUX, BSD) @feat(!NO_X11)
	{
		XDisplay* display;
		int context;
		XWindow helper_window;
		String class_name;
		XlibIM im;
		int xrandrEventBase;
		XWindow x11Source;
		int x11Version;
		int x11Format;
		GfxDataTransferType x11TransferType;
		XErrorEvent* x11Error;
		Atom netCurrentDesktop, net_wm_state_maximized_vert, net_wm_state_maximized_horz, netWmBypassCompositor;
		Atom netWmState, motifWmHints, wmDeleteWindow, netWmSyncRequestCounter, netWmSyncRequest;
		Atom atomPair, multiple, targets, saveTargets, utf8String;
		Atom xdndTypeList, xdndSelection, xdndAware, xdndEnter, xdndPosition, xdndStatus, xdndLeave, xdndDrop, xdndFinished, xdndActionCopy;
		Atom wmProtocols, wmState, netWmStateFullscreen, netWmStateAbove, netWmWindowOpacity;
		Atom netWmName, netWmIcon, xselData, clipboardAtom, clipboardManager;
		Atom xtextUriList, xtextPlain, netWorkarea;
	}
	bool use_wayland @feat(WAYLAND & !NO_X11);
	bool raw_mouse;
	struct window_state
	{
		GfxPoint drop;
		bool mouse_enter;
		bool dataDragging;
		bool dataDrop;
		bool mouse_leave;
		GfxWindow* win;
		GfxWindow* win_leave;
		sz dataLength;
	}
	GfxKeyState[8] mouseButtons;
	GfxKeyState[GFX_KEYS] keyboard;
	Vec2 scroll;
	Vec2 vector;
}

GfxContext _gfx;

bitstruct GfxBacking : char
{
	bool egl;
	bool opengl;
	bool owned;
}

union GfxBackingContext
{
	GfxGlContext* native;
	GfxEglContext* egl;
}

struct GfxMonitorList
{
	GfxMonitorNode* head;
	GfxMonitorNode* cur;
}

struct GfxKeyState
{
	bool current;
	bool prev;
}

struct GfxWindowInternal
{
	int id;
	GfxKey exit_key;
	GfxPoint last_mouse;
	bool is_collected;
	bool shouldClose;
	bool raw_mouse;
	bool capture_mouse;
	bool inFocus;
	bool mouse_inside;
	GfxKeyModifier mod;
	GfxEventMask enabled_events;
	GfxWindowFlags flags;
	IntRect old_bounds;
	GfxMonitorMode oldMode;
	GfxMouse* mouse;
}


struct GfxMonitorNode
{
	GfxMonitor mon;
	bool disconnected;
	GfxMonitorNode* next;
	void* native0;
	void* native1;
	ulong nativeId0;
	ulong nativeId1;
	struct @feat(WAYLAND)
	{
		uint id;
		WlProxy* output;
		WlProxy* xdgOutput;
		GfxMonitorMode* modes;
		sz mode_count;
	}
	struct @feat(WIN32)
	{
		Win32_HMONITOR hMonitor;
		ushort[32] adapterName;
		ushort[32] deviceName;
	}
}

alias GfxGetProcFn = fn void*(ZString);

fn void _init_keycodes()
{
	_gfx.keycodes = {};
	_init_keycodes_native();
	for (int i = 0; i < GFX_KEYS; i++)
	{
		for (int y = 0; y < _gfx.keycodes.len; y++)
		{
			if (_gfx.keycodes[y] == (GfxKey)i)
			{
				_gfx.apiKeycodes[i] = (uint)y;
				break;
			}
		}
	}
	reset_key();
}

fn void _split_bpp(uint bpp, GfxMonitorMode* mode)
{
	if (bpp == 32) bpp = 24;
	mode.rgb.r = mode.rgb.g = mode.rgb.b = (char)(bpp / 3);
	uint delta = bpp - (mode.rgb.r * 3);
	if (delta >= 1) mode.rgb.g = mode.rgb.g + 1;
	if (delta == 2) mode.rgb.r = mode.rgb.r + 1;
}

fn float _gamma_for_index(float gamma, sz index, sz count)
{
	float value = index / (count - 1.0f);
	value = math::pow(value, 1.0f / gamma) * 65535.0f + 0.5f;
	return min(value, 65535.0f);
}

fn bool _extension_is_in_list(String extensions, String extension)
{
	if (!extensions || !extension) return false;
	Splitter split = extensions.tokenize(" ");
	while (try s = split.next())
	{
		if (s == extension) return true;
	}
	return false;
}

fn void _event_queue_call_push(GfxEvent* event)
{
	if (_gfx.callbacks[event.type]) _gfx.callbacks[event.type](event);
	event_queue_push(event);
}


fn void _reset_prev_state()
{
	foreach (&entry : _gfx.keyboard) entry.prev = entry.current;
	foreach (mouse : GfxMouseButton::values)
	{
		_gfx.mouseButtons[mouse.ordinal].prev = _gfx.mouseButtons[mouse.ordinal].current;
	}
	_gfx.scroll = {};
	_gfx.vector = {};
	_gfx.window_state = {};
	GfxDataDropNode* node = _gfx.dndRoot;
	while (node != null)
	{
		GfxDataDropNode* next = node.next;
		free(node);
		node = next;
	}
	_gfx.dndRoot = null;
	_gfx.dndCur = null;
}

<*
 @require !win.internal.is_collected : "Releasing already collected window"
*>
fn void _window_release(GfxWindow* win)
{
	if (win.internal.is_collected) return;
	if (win.internal.capture_mouse) win.set_capture_mouse(false);
	if (win.src.backing.egl && win.src.ctx.egl != null)
	{
		_window_destroy_egl_context_native(win, win.src.ctx.egl);
		win.src.ctx.egl = null;
	}
	if (win.src.backing.opengl && win.src.ctx.native != null)
	{
		_window_destroy_opengl_context_native(win, win.src.ctx.native);
		win.src.ctx.native = null;
	}
	_window_close_native(win);
	if (_gfx.clipboard) free(_gfx.clipboard);
	_gfx.clipboard = null;
	_gfx.active_windows--;
	*win = { .internal = { .is_collected = true, .id = win.internal.id } };
	log::debug("A window was freed", category: log::CATEGORY_VIDEO);
}

fn GfxWindow*? _create_window(String name, int x, int y, int w, int h, GfxWindowFlags flags)
{
	if (!_gfx.initialized) return GFX_CONTEXT_NOT_INITIALIZED~;

	GfxWindow* win;
	// All full!
	if (_gfx.active_windows == _gfx.windows.len())
	{
		_gfx.windows.push({ .internal.id = (int)_gfx.windows.len() });
		win = &_gfx.windows[^1];
	}
	else
	{
		foreach (int i, &existing : _gfx.windows)
		{
			if (existing.internal.is_collected)
			{
				*existing = { .internal.id = i };
				win = existing;
				break;
			}
		}
	}

	assert (win, "A window should always be found");
	defer catch win.internal.is_collected = true;

	_gfx.active_windows++;
	if (!_gfx.root) set_root_window(win);
	win.bounds = { x, y, w, h };
	win.internal.mouse = _gfx.standardMice[GfxMouseIcon.NORMAL.ordinal];
	win.internal.flags = flags;
	win.internal.enabled_events = ALL_EVENTS;
	_create_window_native(name, flags, win)!;
	if (!flags.hide_mouse) win.set_mouse_default();

	win.set_name(name);
	switch
	{
		case !flags.hide:
			flags.hide = true;
			win.show();
		case flags.maximize || flags.fullscreen:
			win.hide();
	}
	return win;
}


fn void _window_set_flags(GfxWindow* win, GfxWindowFlags flags, GfxWindowFlags cmpFlags) @private
{
	switch
	{
		case flags.no_border: win.set_border(false);
		case cmpFlags.no_border: win.set_border(true);
	}
	switch
	{
		case flags.maximize: win.maximize();
		case cmpFlags.maximize: win.restore();
	}
	switch
	{
		case flags.minimize: win.minimize();
		case cmpFlags.minimize: win.restore();
	}
	if (flags.center) win.center_on_screen();
	if (flags.center_cursor) _window_move_mouse(win, win.origin + win.size / 2);
	switch
	{
		case flags.fullscreen: _window_set_fullscreen_native(win, true);
		case cmpFlags.fullscreen: _window_set_fullscreen_native(win, false);
	}
	if (flags.hide_mouse)
	{
		win.show_mouse(false);
	}
	else if (cmpFlags.hide_mouse)
	{
		win.show_mouse(true);
	}
	if (flags.hide)
	{
		win.hide();
	}
	else if (cmpFlags.hide)
	{
		win.show();
	}
	if (flags.floating)
	{
		_window_set_floating_native(win, true);
	}
	else if (cmpFlags.floating)
	{
		_window_set_floating_native(win, false);
	}
	if (flags.raw_mouse)
	{
		win.set_raw_mouse_mode(true);
	}
	else if (cmpFlags.raw_mouse)
	{
		win.set_raw_mouse_mode(false);
	}
	if (flags.capture_mouse)
	{
		win.set_capture_raw_mouse(true);
	}
	else if (cmpFlags.capture_mouse)
	{
		win.set_capture_mouse(false);
	}
	if (flags.focus) _window_focus_native(win);
	if (flags.scale_to_monitor) win.scale_to_monitor();
	switch (true)
	{
		case flags.no_resize:
			_window_set_max_size_native(win, win.size);
			_window_set_min_size_native(win, win.size);
		case cmpFlags.no_resize:
			_window_set_max_size_native(win, {});
			_window_set_min_size_native(win, {});
	}
	win.internal.flags = flags;
}

fn void _mouse_button_callback(GfxWindow* win, GfxMouseButton button, bool press)
{
	GfxEvent event;
	if (press)
	{
		if (!win.internal.enabled_events.mouse_button_pressed) return;
		event.type = MOUSE_BUTTON_PRESSED;
	}
	else
	{
		if (!win.internal.enabled_events.mouse_button_released) return;
		event.type = MOUSE_BUTTON_RELEASED;
	}
	_gfx.mouseButtons[(sz)button].prev = _gfx.mouseButtons[(sz)button].current;
	_gfx.mouseButtons[(sz)button].current = press;
	event.button.value = button;
	event.button.state = press;
	event.common.win = win;
	_event_queue_call_push(&event);
}

fn void _window_maximized_callback(GfxWindow* win, IntRect bounds)
{
	win.internal.flags.maximize = true;
	win.bounds = bounds;
	if (!win.internal.enabled_events.window_maximized) return;
	GfxEvent event;
	event.type = WINDOW_MAXIMIZED;
	event.update.bounds = bounds;
	event.common.win = win;
	_event_queue_call_push(&event);
}

fn void _window_focus_callback(GfxWindow* win, bool inFocus)
{
	win.internal.inFocus = inFocus;
	if (win.internal.capture_mouse) _window_set_capture_mouse_native(win, inFocus);
	GfxEvent event;
	event.common.win = win;
	event.focus.state = inFocus;
	if (inFocus == true)
	{
		if (win.internal.flags.fullscreen) _window_raise_native(win);
		event.type = WINDOW_FOCUS_IN;
	}
	else
	{
		if (win.internal.flags.fullscreen) win.minimize();
		for (int i = 0; i < GFX_KEYS; i++)
		{
			GfxKey key = (GfxKey)i;
			if (!is_key_down(key)) continue;
			_gfx.keyboard[i].current = false;
			if (win.internal.enabled_events.key_released) _key_callback(win, key, win.internal.mod, false, false);
		}
		reset_key();
		event.type = WINDOW_FOCUS_OUT;
	}
	_event_queue_call_push(&event);
}

fn void _data_drag_callback(GfxWindow* win, GfxDataTransferType dataType, GfxDndActionType action, GfxPoint pos)
{
	if (!win.internal.enabled_events.data_drag || !win.internal.flags.allow_dnd) return;
	_gfx.window_state.win = win;
	_gfx.window_state.dataDragging = true;
	_gfx.window_state.drop = pos;
	GfxEvent event;
	event.type = DATA_DRAG;
	event.drag.pos = pos;
	event.drag.action = action;
	event.drag.dataType = dataType;
	event.common.win = win;
	_event_queue_call_push(&event);
}

fn void _window_minimized_callback(GfxWindow* win)
{
	win.internal.flags.minimize = true;
	if (!win.internal.enabled_events.window_minimized) return;
	GfxEvent event;
	event.type = WINDOW_MINIMIZED;
	event.update.bounds = win.bounds;
	event.common.win = win;
	_event_queue_call_push(&event);
}

fn void _monitor_callback(GfxWindow* win, GfxMonitor* monitor, bool connected)
{
	if (win)
	{
		if (connected && !win.internal.enabled_events.monitor_connected) return;
		if (!connected && !win.internal.enabled_events.monitor_disconnected) return;
	}
	GfxEvent event;
	event.type = connected ? MONITOR_CONNECTED : MONITOR_DISCONNECTED;
	event.monitor.monitor = monitor;
	event.monitor.state = connected;
	event.common.win = win;
	_event_queue_call_push(&event);
}

fn void _scale_updated_callback(GfxWindow* win, Vec2 scale)
{
	if (!win.internal.enabled_events.scale_updated) return;
	GfxEvent event;
	event.type = SCALE_UPDATED;
	event.scale.scale = scale;
	event.common.win = win;
	_event_queue_call_push(&event);
}


fn void _mouse_scroll_callback(GfxWindow* win, Vec2 delta)
{
	if (!win.internal.enabled_events.mouse_scroll) return;
	_gfx.scroll = delta;
	GfxEvent event;
	event.type = MOUSE_SCROLL;
	event.delta.vector = delta;
	event.common.win = win;
	_event_queue_call_push(&event);
}

fn void _window_refresh_callback(GfxWindow* win, IntRect bounds)
{
	if (!win.internal.enabled_events.window_refresh) return;
	GfxEvent event;
	event.type = WINDOW_REFRESH;
	event.update.bounds = bounds;
	event.common.win = win;
	_event_queue_call_push(&event);
}

fn void _raw_motion_callback(GfxWindow* win, Vec2 delta)
{
	_gfx.vector = delta;
	if (!win.internal.enabled_events.mouse_raw_motion) return;
	GfxEvent event;
	event.type = MOUSE_RAW_MOTION;
	event.delta.vector = delta;
	event.common.win = win;
	_event_queue_call_push(&event);
}

fn void _window_resized_callback(GfxWindow* win, GfxSize size)
{
	win.size = size;
	if (!win.internal.enabled_events.window_resized) return;
	GfxEvent event;
	event.type = WINDOW_RESIZED;
	event.update.bounds = win.bounds;
	event.common.win = win;
	_event_queue_call_push(&event);
}

fn void _window_moved_callback(GfxWindow* win, GfxPoint where)
{
	win.origin = where;
	if (!win.internal.enabled_events.window_moved) return;
	GfxEvent event;
	event.type = WINDOW_MOVED;
	event.update.bounds = win.bounds;
	event.common.win = win;
	_event_queue_call_push(&event);
}

fn void _window_check_mode(GfxWindow* win)
{
	switch
	{
		case _window_is_minimized_native(win) && win.internal.enabled_events.window_minimized:
			_window_minimized_callback(win);
		case _window_is_maximized_native(win) && win.internal.enabled_events.window_maximized:
			_window_maximized_callback(win, win.bounds);
		case (win.internal.flags.minimize && !_window_is_maximized_native(win)) ||
		(win.internal.flags.maximize && !_window_is_maximized_native(win)):
			if (win.internal.enabled_events.window_restored) _window_restored_callback(win, win.bounds);
	}
}

fn void _data_drop_callback(GfxWindow* win, char* data, sz length, GfxDataTransferType dataType)
{
	if (!win.internal.enabled_events.data_drop || !win.internal.flags.allow_dnd) return;
	_gfx.window_state.win = win;
	_gfx.window_state.dataDrop = true;
	_gfx.window_state.dataLength = length;
	GfxDataDropNode node = { .data = data, .length = length, .type = dataType };
	GfxEvent event;
	event.type = DATA_DROP;
	event.drop.value = &node;
	event.common.win = win;
	if (_gfx.callbacks[(sz)event.type] != null) _gfx.callbacks[(sz)event.type](&event);
	if (!_gfx.queueEvents && !_gfx.dnd_build) return;
	char* dataCopy = malloc(length);
	mem::copy(dataCopy, data, length);
	node.data = dataCopy;
	GfxDataDropNode* heapNode = mem::new(GfxDataDropNode);
	*heapNode = node;
	if (_gfx.dndRoot == null)
	{
		_gfx.dndRoot = heapNode;
		_gfx.dndCur = heapNode;
	}
	else
	{
		_gfx.dndCur.next = heapNode;
		_gfx.dndCur = heapNode;
	}
	event.drop.value = heapNode;
	event_queue_push(&event);
}

fn void _key_callback(GfxWindow* win, GfxKey key, GfxKeyModifier mod, bool repeat, bool state)
{
	GfxEvent event;
	if (state)
	{
		if (!win.internal.enabled_events.key_pressed) return;
		event.type = KEY_PRESSED;
	}
	else
	{
		if (!win.internal.enabled_events.key_released) return;
		event.type = KEY_RELEASED;
	}
	_gfx.keyboard[(sz)key].prev = _gfx.keyboard[(sz)key].current;
	_gfx.keyboard[(sz)key].current = state;
	event.key.value = key; event.key.repeat = repeat; event.key.mod = mod; event.key.state = state;
	event.common.win = win;
	_event_queue_call_push(&event);
}

fn void _mouse_motion_callback(GfxWindow* win, GfxPoint where)
{
	win.internal.last_mouse = where;
	if (!win.internal.enabled_events.mouse_motion) return;
	GfxEvent event;
	event.type = MOUSE_MOTION;
	event.mouse.pos = where;
	event.mouse.in_window = win.internal.mouse_inside;
	event.common.win = win;
	_event_queue_call_push(&event);
}

fn void _mouse_notify_callback(GfxWindow* win, GfxPoint where, bool status)
{
	win.internal.mouse_inside = status;
	_gfx.window_state.win = win;
	win.internal.last_mouse = where;
	GfxEvent event;
	event.common.win = win;
	event.mouse.pos = where;
	event.mouse.in_window = win.internal.mouse_inside;
	if (status)
	{
		if (!win.internal.enabled_events.mouse_enter) return;
		_gfx.window_state.mouse_enter = true;
		event.type = MOUSE_ENTER;
	}
	else
	{
		if (!win.internal.enabled_events.mouse_leave) return;
		_gfx.window_state.win_leave = win;
		_gfx.window_state.mouse_leave = true;
		event.type = MOUSE_LEAVE;
	}
	_event_queue_call_push(&event);
}

fn void _key_char_callback(GfxWindow* win, uint codepoint)
{
	if (!win.internal.enabled_events.key_char) return;
	GfxEvent event;
	event.type = KEY_CHAR;
	event.keyChar.value = codepoint;
	event.common.win = win;
	_event_queue_call_push(&event);
}

fn void _window_restored_callback(GfxWindow* win, IntRect bounds)
{
	win.internal.flags.minimize = false;
	win.bounds = bounds;
	if (!_window_is_maximized_native(win)) win.internal.flags.maximize = false;
	if (!win.internal.enabled_events.window_restored) return;
	GfxEvent event;
	event.type = WINDOW_RESTORED;
	event.update.bounds = bounds;
	event.common.win = win;
	_event_queue_call_push(&event);
}
fn void _window_close_callback(GfxWindow* win)
{
	win.internal.shouldClose = true;
	if (!win.internal.enabled_events.window_close) return;
	GfxEvent event;
	event.type = WINDOW_CLOSE;
	event.common.win = win;
	_event_queue_call_push(&event);
}

fn void _key_update_key_mods_ex(GfxWindow* win, bool capital, bool numlock, bool control, bool alt, bool shift, bool super, bool scroll)
{
	win.update_key_modifier(KEYMOD_CAPS_LOCK, capital);
	win.update_key_modifier(KEYMOD_NUM_LOCK, numlock);
	win.update_key_modifier(KEYMOD_CONTROL, control);
	win.update_key_modifier(KEYMOD_ALT, alt);
	win.update_key_modifier(KEYMOD_SHIFT, shift);
	win.update_key_modifier(KEYMOD_SUPER, super);
	win.update_key_modifier(KEYMOD_SCROLL_LOCK, scroll);
}

// public
fn void _key_update_key_mods(GfxWindow* win, bool capital, bool numlock, bool scroll)
{
	_key_update_key_mods_ex(win, capital, numlock,
		is_key_down(CONTROL_L) || is_key_down(CONTROL_R),
		is_key_down(ALT_L)     || is_key_down(ALT_R),
		is_key_down(SHIFT_L)   || is_key_down(SHIFT_R),
		is_key_down(SUPER_L)   || is_key_down(SUPER_R),
		scroll);
}



fn GfxMonitorNode* _monitor_add(GfxMonitor* mon)
{
	GfxMonitorNode* node = mem::new(GfxMonitorNode);
	if (!_gfx.monitors.list.head)
	{
		_gfx.monitors.list.head = node;
	}
	else
	{
		_gfx.monitors.list.cur.next = node;
	}
	_gfx.monitors.list.cur = node;
	if (mon) node.mon = *mon;
	node.mon.node = node;
	node.disconnected = false;
	_gfx.monitors.count += 1;
	return node;
}



fn void _remove_monitor(GfxMonitorNode* node, GfxMonitorNode* prev)
{
	_gfx.monitors.count--;
	_monitor_node_free_native(node);
	if (prev)
	{
		prev.next = node.next;
	}
	else
	{
		_gfx.monitors.list.head = node.next;
	}
	if (_gfx.monitors.primary == node) _gfx.monitors.primary = null;
	node.next = null;
	free(node);
}

fn void _window_show_mouse_flags(GfxWindow* win, bool show)
{
	if (show && win.internal.flags.hide_mouse)
	{
		win.internal.flags.hide_mouse = false;
	}
	else if (!show && !win.internal.flags.hide_mouse)
	{
		win.internal.flags.hide_mouse = true;
	}
}

fn void _window_show_mouse_native(GfxWindow* win, bool show) @feat(!WASM)
{
	_window_show_mouse_flags(win, show);
	if (show)
	{
		win.set_mouse(win.internal.mouse);
	}
	else
	{
		win.set_mouse(_gfx.hiddenMouse);
	}
}



// --- vulkan

