module std::gfx @feat(LINUX, BSD, MACOS, WASM, WIN32) @feat(!(BSD | LINUX) | WAYLAND | !NO_X11) @private;
import libc, std::math, std::collections::list, std::os, std::gfx::x11 @public, std::gfx::egl @public, std::time;
import std::gfx::wl @public;

const MAX_EVENTS = 32;
const PREALLOCATED_MONITORS = 0;

const X11 = ($feat(BSD) || $feat(LINUX)) && !$feat(NO_X11);
const WAYLAND = ($feat(BSD) || $feat(LINUX)) && $feat(WAYLAND);
const WAYLAND_ONLY = WAYLAND && !X11;
const X11_ONLY = !WAYLAND && X11;
const X11_OR_WAYLAND = WAYLAND && X11;
const NO_GFX_UNIX = !WAYLAND && !X11;

struct GfxContext
{
	bool initialized;
	List{GfxWindow} windows;
	GfxWindow* root;
	int active_windows;
	GfxMouse* hidden_mouse;
	GfxMouse*[21] standard_mice;
	GfxEventCallbacks callbacks;
	GfxEvent[MAX_EVENTS] events;
	int event_bottom;
	int event_len;
	bool event_error_reported;
	bool queue_events;
	bool polled_events;
	uint[GFX_KEYS] api_keycodes;
	GfxKey[$feat(WIN32) ? 512 : 256] keycodes;
	GfxDataTransfer* clipboard;
	bool dnd_build;
	GfxDataDropNode* dnd_root;
	GfxDataDropNode* dnd_cur;

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

	GfxDataTransfer* unix_clipboard @feat(LINUX, BSD);

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
	bool should_close;
	bool raw_mouse;
	bool capture_mouse;
	bool in_focus;
	bool mouse_inside;
	GfxKeyModifier mod;
	GfxEventMask enabled_events;
	GfxWindowFlags flags;
	GfxRect old_bounds;
	GfxMonitorMode oldMode;
	GfxMouse* mouse;
}


struct GfxMonitorNode
{
	GfxMonitor mon;
	bool disconnected;
	GfxMonitorNode* next;
	struct @feat(WAYLAND)
	{
		uint id;
		WlProxy* output;
		WlProxy* xdg_output;
		GfxMonitorMode* modes;
		sz mode_count;
	}
	struct @feat(BSD, LINUX) @feat(!NO_X11)
	{
		RROutput rr_output;
		RRCrtc crtc;
	}
	struct @feat(WIN32)
	{
		Win32_HMONITOR monitor;
		Char16[32]     adapter_name;
		Char16[32]     device_name;
	}
	struct @feat(MACOS)
	{
		void* screen;
		CGDirectDisplayID display;
		uint num;
	}
}


alias GfxGetProcFn = fn void*(ZString);

<*
 The evdev keycode table shared by X11 and Wayland.

 X11 keycodes and Wayland's `wl_keyboard.key` + 8 are the same evdev values, so
 both backends use this one table rather than querying the server (which would
 not work under Wayland).
*>
fn void _init_unix_keycodes() @feat(LINUX, BSD)
{
	_gfx.keycodes = {
		[  9] = ESCAPE,           [ 10] = NUM1,             [ 11] = NUM2,
		[ 12] = NUM3,             [ 13] = NUM4,             [ 14] = NUM5,
		[ 15] = NUM6,             [ 16] = NUM7,             [ 17] = NUM8,
		[ 18] = NUM9,             [ 19] = NUM0,             [ 20] = MINUS,
		[ 21] = EQUALS,           [ 22] = BACK_SPACE,       [ 23] = TAB,
		[ 24] = Q,                [ 25] = W,                [ 26] = E,
		[ 27] = R,                [ 28] = T,                [ 29] = Y,
		[ 30] = U,                [ 31] = I,                [ 32] = O,
		[ 33] = P,                [ 34] = BRACKET,          [ 35] = CLOSE_BRACKET,
		[ 36] = RETURN,           [ 37] = CONTROL_L,        [ 38] = A,
		[ 39] = S,                [ 40] = D,                [ 41] = F,
		[ 42] = G,                [ 43] = H,                [ 44] = J,
		[ 45] = K,                [ 46] = L,                [ 47] = SEMICOLON,
		[ 48] = APOSTROPHE,       [ 49] = BACKTICK,         [ 50] = SHIFT_L,
		[ 51] = BACK_SLASH,       [ 52] = Z,                [ 53] = X,
		[ 54] = C,                [ 55] = V,                [ 56] = B,
		[ 57] = N,                [ 58] = M,                [ 59] = COMMA,
		[ 60] = PERIOD,           [ 61] = SLASH,            [ 62] = SHIFT_R,
		[ 63] = PAD_MULTIPLY,     [ 64] = ALT_L,            [ 65] = SPACE,
		[ 66] = CAPS_LOCK,        [ 67] = F1,               [ 68] = F2,
		[ 69] = F3,               [ 70] = F4,               [ 71] = F5,
		[ 72] = F6,               [ 73] = F7,               [ 74] = F8,
		[ 75] = F9,               [ 76] = F10,              [ 77] = NUM_LOCK,
		[ 78] = SCROLL_LOCK,      [ 81] = PAD9,             [ 82] = PAD_MINUS,
		[ 83] = PAD4,             [ 84] = PAD5,             [ 85] = PAD6,
		[ 86] = PAD_PLUS,         [ 87] = PAD1,             [ 88] = PAD2,
		[ 89] = PAD3,             [ 90] = PAD0,             [ 91] = PAD_PERIOD,
		[ 95] = F11,              [ 96] = F12,              [104] = PAD_RETURN,
		[105] = CONTROL_R,        [106] = PAD_SLASH,        [107] = PRINT_SCREEN,
		[108] = ALT_R,            [110] = HOME,             [111] = UP,
		[112] = PAGE_UP,          [113] = LEFT,             [114] = RIGHT,
		[115] = END,              [116] = DOWN,             [117] = PAGE_DOWN,
		[118] = INSERT,           [119] = DELETE,           [128] = PAUSE,
		[133] = SUPER_L,          [134] = SUPER_R,          [142] = PAD_EQUAL,
		[161] = WORLD1,           [162] = WORLD2,           [191] = F13,
		[192] = F14,              [193] = F15,              [194] = F16,
		[195] = F17,              [196] = F18,              [197] = F19,
		[198] = F20,              [199] = F21,              [200] = F22,
		[201] = F23,              [202] = F24,              [203] = F25,
	};
}

fn void _init_keycodes()
{
	_gfx.keycodes = {};
	_init_keycodes_platform();
	for (int i = 0; i < GFX_KEYS; i++)
	{
		for (int y = 0; y < _gfx.keycodes.len; y++)
		{
			if (_gfx.keycodes[y] == (GfxKey)i)
			{
				_gfx.api_keycodes[i] = (uint)y;
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

<*
 Remove and return the oldest queued event belonging to a window.

 Events for other windows are rotated to the back of the queue rather than discarded.

 @param [in] win : "The window to filter on"
 @return "The oldest event for this window, or a window-independent event"
 @return? NO_MORE_ELEMENT : "If no matching event is queued"
*>
fn GfxEvent? _window_event_queue_pop(GfxWindow* win)
{
	GfxEvent ev = event_queue_pop()!;
	for (int i = 1; i < _gfx.event_len && ev.common.win != win && ev.common.win != null; i++)
	{
		event_queue_push(&ev);
		ev = event_queue_pop()!!;
	}
	if (ev.common.win != win && ev.common.win != null) return NO_MORE_ELEMENT~;
	return ev;
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
	GfxDataDropNode* node = _gfx.dnd_root;
	while (node != null)
	{
		GfxDataDropNode* next = node.next;
		free(node);
		node = next;
	}
	_gfx.dnd_root = null;
	_gfx.dnd_cur = null;
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
		egl::destroy_context_in_window(win.src.ctx.egl, win);
		win.src.ctx.egl = null;
	}
	if (win.src.backing.opengl && win.src.ctx.native != null)
	{
		_window_destroy_opengl_context(win, win.src.ctx.native);
		win.src.ctx.native = null;
	}
	_window_destroy_platform(win);
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
	win.internal.mouse = _gfx.standard_mice[GfxMouseIcon.NORMAL.ordinal];
	win.internal.flags = flags;
	win.internal.enabled_events = ALL_EVENTS;
	_window_create_platform(win, name, flags)!;
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
		case flags.fullscreen: _window_set_fullscreen(win, true);
		case cmpFlags.fullscreen: _window_set_fullscreen(win, false);
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
		_window_set_floating(win, true);
	}
	else if (cmpFlags.floating)
	{
		_window_set_floating(win, false);
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
	if (flags.focus) _window_focus(win);
	if (flags.scale_to_monitor) win.scale_to_monitor();
	switch (true)
	{
		case flags.no_resize:
			_window_set_max_size(win, win.size);
			_window_set_min_size(win, win.size);
		case cmpFlags.no_resize:
			_window_set_max_size(win, {});
			_window_set_min_size(win, {});
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

fn void _window_maximized_callback(GfxWindow* win, GfxRect bounds)
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

fn void _window_focus_callback(GfxWindow* win, bool in_focus)
{
	win.internal.in_focus = in_focus;
	if (win.internal.capture_mouse) _window_set_capture_mouse_platform(win, in_focus);
	GfxEvent event;
	event.common.win = win;
	event.focus.state = in_focus;
	if (in_focus == true)
	{
		if (win.internal.flags.fullscreen) _window_raise(win);
		event.type = WINDOW_FOCUS_IN;
	}
	else
	{
		if (win.internal.flags.fullscreen) win.minimize();
		_release_all_keys(win);
		event.type = WINDOW_FOCUS_OUT;
	}
	_event_queue_call_push(&event);
}

<*
 Report every currently held key as released, then clear all keyboard state.

 Used whenever key-up events can no longer be expected to arrive: losing focus, and
 on Win32 entering a modal resize/menu loop, which runs its own message pump.

 @require win != null
*>
fn void _release_all_keys(GfxWindow* win)
{
	for (int i = 0; i < GFX_KEYS; i++)
	{
		GfxKey key = (GfxKey)i;
		if (!is_key_down(key)) continue;
		_gfx.keyboard[i].current = false;
		if (win.internal.enabled_events.key_released) _key_callback(win, key, win.internal.mod, false, false);
	}
	reset_key();
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

fn void _window_refresh_callback(GfxWindow* win, GfxRect bounds)
{
	if (GfxRepaintFn f = win.repaint_fn) f(win, bounds);
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
		case _window_is_minimized(win) && win.internal.enabled_events.window_minimized:
			_window_minimized_callback(win);
		case _window_is_maximized(win) && win.internal.enabled_events.window_maximized:
			_window_maximized_callback(win, win.bounds);
		case (win.internal.flags.minimize && !_window_is_maximized(win)) ||
		(win.internal.flags.maximize && !_window_is_maximized(win)):
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
	if (_gfx.callbacks[event.type] != null) _gfx.callbacks[event.type](&event);
	if (!_gfx.queue_events && !_gfx.dnd_build) return;
	char* dataCopy = malloc(length);
	mem::copy(dataCopy, data, length);
	node.data = dataCopy;
	GfxDataDropNode* heapNode = mem::new(GfxDataDropNode);
	*heapNode = node;
	if (_gfx.dnd_root == null)
	{
		_gfx.dnd_root = heapNode;
		_gfx.dnd_cur = heapNode;
	}
	else
	{
		_gfx.dnd_cur.next = heapNode;
		_gfx.dnd_cur = heapNode;
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

fn void _window_restored_callback(GfxWindow* win, GfxRect bounds)
{
	win.internal.flags.minimize = false;
	win.bounds = bounds;
	if (!_window_is_maximized(win)) win.internal.flags.maximize = false;
	if (!win.internal.enabled_events.window_restored) return;
	GfxEvent event;
	event.type = WINDOW_RESTORED;
	event.update.bounds = bounds;
	event.common.win = win;
	_event_queue_call_push(&event);
}
fn void _window_close_callback(GfxWindow* win)
{
	win.internal.should_close = true;
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

fn void _key_update_key_mods(GfxWindow* win, bool capital, bool numlock, bool scroll)
{
	_key_update_key_mods_ex(win, capital, numlock,
		is_key_down(CONTROL_L) || is_key_down(CONTROL_R),
		is_key_down(ALT_L)     || is_key_down(ALT_R),
		is_key_down(SHIFT_L)   || is_key_down(SHIFT_R),
		is_key_down(SUPER_L)   || is_key_down(SUPER_R),
		scroll);
}



<*
 @ensure return != null
*>
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
	_monitor_node_free(node);
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
		win.set_mouse(_gfx.hidden_mouse);
	}
}



// --- vulkan

