module std::gfx @feat(MACOS, X11, X11_WAYLAND, WAYLAND, WASM, WIN32) @private;
import libc, std::math;

fn void*? _dlopen_any(ZString... names) @feat(POSIX)
{
	foreach (name : names)
	{
		void* proc = libc::dlopen(name, libc::RTLD_LAZY | libc::RTLD_LOCAL);
		if (proc) return proc;
	}
	return NOT_FOUND~;
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
	if (flags.center) todo_fix_window_center(win);
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
		todo_fix_window_setFloating(win, true);
	}
	else if (cmpFlags.floating)
	{
		todo_fix_window_setFloating(win, false);
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
		if (!(win.internal.enabled_events & TODO_RENAME_MOUSE_BUTTON_PRESSED_FLAG)) return;
		event.type = MOUSE_BUTTON_PRESSED;
	}
	else
	{
		if (!(win.internal.enabled_events & TODO_RENAME_MOUSE_BUTTON_RELEASED_FLAG)) return;
		event.type = MOUSE_BUTTON_RELEASED;
	}
	_gfx_ctx.mouseButtons[(sz)button].prev = _gfx_ctx.mouseButtons[(sz)button].current;
	_gfx_ctx.mouseButtons[(sz)button].current = press;
	event.button.value = button;
	event.button.state = press;
	event.common.win = win;
	_event_queue_call_push(&event);
}

fn void _window_maximized_callback(GfxWindow* win, IntRect bounds)
{
	win.internal.flags.maximize = true;
	win.bounds = bounds;
	if (!(win.internal.enabled_events & TODO_RENAME_WINDOW_MAXIMIZED_FLAG)) return;
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
		if (win.internal.flags.fullscreen) todo_fix_window_raise(win);
		event.type = WINDOW_FOCUS_IN;
	}
	else
	{
		if (win.internal.flags.fullscreen) win.minimize();
		for (int i = 0; i < GFX_KEYS; i++)
		{
			GfxKey key = (GfxKey)i;
			if (!is_key_down(key)) continue;
			_gfx_ctx.keyboard[i].current = false;
			if (win.internal.enabled_events & TODO_RENAME_KEY_RELEASED_FLAG) _key_callback(win, key, win.internal.mod, false, false);
		}
		todo_fix_resetKey();
		event.type = WINDOW_FOCUS_OUT;
	}
	_event_queue_call_push(&event);
}

fn void _data_drag_callback(GfxWindow* win, GfxDataTransferType dataType, GfxDndActionType action, GfxPoint pos)
{
	if (!(win.internal.enabled_events & TODO_RENAME_DATA_DRAG_FLAG) || !win.internal.flags.allow_dnd) return;
	_gfx_ctx.window_state.win = win;
	_gfx_ctx.window_state.dataDragging = true;
	_gfx_ctx.window_state.drop = pos;
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
	if (!(win.internal.enabled_events & TODO_RENAME_WINDOW_MINIMIZED_FLAG)) return;
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
		if (connected && !(win.internal.enabled_events & TODO_RENAME_MONITOR_CONNECTED_FLAG)) return;
		if (!connected && !(win.internal.enabled_events & TODO_RENAME_MONITOR_DISCONNECTED_FLAG)) return;
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
	if (!(win.internal.enabled_events & TODO_RENAME_SCALE_UPDATED_FLAG)) return;
	GfxEvent event;
	event.type = SCALE_UPDATED;
	event.scale.scale = scale;
	event.common.win = win;
	_event_queue_call_push(&event);
}


fn void _mouse_scroll_callback(GfxWindow* win, Vec2 delta)
{
	if (!(win.internal.enabled_events & TODO_RENAME_MOUSE_SCROLL_FLAG)) return;
	_gfx_ctx.scroll = delta;
	GfxEvent event;
	event.type = MOUSE_SCROLL;
	event.delta.vector = delta;
	event.common.win = win;
	_event_queue_call_push(&event);
}

fn void _window_refresh_callback(GfxWindow* win, IntRect bounds)
{
	if (!(win.internal.enabled_events & TODO_RENAME_WINDOW_REFRESH_FLAG)) return;
	GfxEvent event;
	event.type = WINDOW_REFRESH;
	event.update.bounds = bounds;
	event.common.win = win;
	_event_queue_call_push(&event);
}

fn void _raw_motion_callback(GfxWindow* win, Vec2 delta)
{
	_gfx_ctx.vector = delta;
	if (!(win.internal.enabled_events & TODO_RENAME_MOUSE_RAW_MOTION_FLAG)) return;
	GfxEvent event;
	event.type = MOUSE_RAW_MOTION;
	event.delta.vector = delta;
	event.common.win = win;
	_event_queue_call_push(&event);
}

fn void _window_resized_callback(GfxWindow* win, GfxSize size)
{
	win.size = size;
	if (!(win.internal.enabled_events & TODO_RENAME_WINDOW_RESIZED_FLAG)) return;
	GfxEvent event;
	event.type = WINDOW_RESIZED;
	event.update.bounds = win.bounds;
	event.common.win = win;
	_event_queue_call_push(&event);
}

fn void _window_moved_callback(GfxWindow* win, GfxPoint where)
{
	win.origin = where;
	if (!(win.internal.enabled_events & TODO_RENAME_WINDOW_MOVED_FLAG)) return;
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
		case _window_is_minimized_native(win) && (win.internal.enabled_events & TODO_RENAME_WINDOW_MINIMIZED_FLAG):
			_window_minimized_callback(win);
		case _window_is_maximized_native(win)&& (win.internal.enabled_events & TODO_RENAME_WINDOW_MAXIMIZED_FLAG):
			_window_maximized_callback(win, win.bounds);
		case (win.internal.flags.minimize && !_window_is_maximized_native(win)) ||
		(win.internal.flags.maximize && !_window_is_maximized_native(win)):
			if (win.internal.enabled_events & TODO_RENAME_WINDOW_RESTORED_FLAG) _window_restored_callback(win, win.bounds);
	}
}

fn void _data_drop_callback(GfxWindow* win, char* data, sz length, GfxDataTransferType dataType)
{
	if (!(win.internal.enabled_events & TODO_RENAME_DATA_DROP_FLAG) || !win.internal.flags.allow_dnd) return;
	_gfx_ctx.window_state.win = win;
	_gfx_ctx.window_state.dataDrop = true;
	_gfx_ctx.window_state.dataLength = length;
	GfxDataDropNode node = { .data = data, .length = length, .type = dataType };
	GfxEvent event;
	event.type = DATA_DROP;
	event.drop.value = &node;
	event.common.win = win;
	if (_gfx_ctx.callbacks[(sz)event.type] != null) _gfx_ctx.callbacks[(sz)event.type](&event);
	if (!_gfx_ctx.queueEvents && !_gfx_ctx.dnd_build) return;
	char* dataCopy = malloc(length);
	mem::copy(dataCopy, data, length);
	node.data = dataCopy;
	GfxDataDropNode* heapNode = mem::new(GfxDataDropNode);
	*heapNode = node;
	if (_gfx_ctx.dndRoot == null)
	{
		_gfx_ctx.dndRoot = heapNode;
		_gfx_ctx.dndCur = heapNode;
	}
	else
	{
		_gfx_ctx.dndCur.next = heapNode;
		_gfx_ctx.dndCur = heapNode;
	}
	event.drop.value = heapNode;
	event_queue_push(&event);
}

fn void _key_callback(GfxWindow* win, GfxKey key, GfxKeyModifier mod, bool repeat, bool state)
{
	GfxEvent event;
	if (state)
	{
		if (!(win.internal.enabled_events & TODO_RENAME_KEY_PRESSED_FLAG)) return;
		event.type = KEY_PRESSED;
	}
	else
	{
		if (!(win.internal.enabled_events & TODO_RENAME_KEY_RELEASED_FLAG)) return;
		event.type = KEY_RELEASED;
	}
	_gfx_ctx.keyboard[(sz)key].prev = _gfx_ctx.keyboard[(sz)key].current;
	_gfx_ctx.keyboard[(sz)key].current = state;
	event.key.value = key; event.key.repeat = repeat; event.key.mod = mod; event.key.state = state;
	event.common.win = win;
	_event_queue_call_push(&event);
}

fn void _mouse_motion_callback(GfxWindow* win, GfxPoint where)
{
	win.internal.last_mouse = where;
	if (!(win.internal.enabled_events & TODO_RENAME_MOUSE_MOTION_FLAG)) return;
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
	_gfx_ctx.window_state.win = win;
	win.internal.last_mouse = where;
	GfxEvent event;
	event.common.win = win;
	event.mouse.pos = where;
	event.mouse.in_window = win.internal.mouse_inside;
	if (status)
	{
		if (!(win.internal.enabled_events & TODO_RENAME_MOUSE_ENTER_FLAG)) return;
		_gfx_ctx.window_state.mouse_enter = true;
		event.type = MOUSE_ENTER;
	}
	else
	{
		if (!(win.internal.enabled_events & TODO_RENAME_MOUSE_LEAVE_FLAG)) return;
		_gfx_ctx.window_state.win_leave = win;
		_gfx_ctx.window_state.mouse_leave = true;
		event.type = MOUSE_LEAVE;
	}
	_event_queue_call_push(&event);
}

fn void _key_char_callback(GfxWindow* win, uint codepoint)
{
	if (!(win.internal.enabled_events & TODO_RENAME_KEY_CHAR_FLAG)) return;
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
	if (win.internal.enabled_events & TODO_RENAME_WINDOW_RESTORED_FLAG == 0) return;
	GfxEvent event;
	event.type = WINDOW_RESTORED;
	event.update.bounds = bounds;
	event.common.win = win;
	_event_queue_call_push(&event);
}
fn void _window_close_callback(GfxWindow* win)
{
	win.internal.shouldClose = true;
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
	if (!_gfx_ctx.monitors.list.head)
	{
		_gfx_ctx.monitors.list.head = node;
	}
	else
	{
		_gfx_ctx.monitors.list.cur.next = node;
	}
	_gfx_ctx.monitors.list.cur = node;
	if (mon) node.mon = *mon;
	node.mon.node = node;
	node.disconnected = false;
	_gfx_ctx.monitors.count += 1;
	return node;
}


// To remove
fn void todo_fix_debugCallback(TodoRenameDebugType type, TodoRenameErrorCode code, ZString msg)
{
	TodoRenameDebugInfo info;
	info.type = type;
	info.code = code;
	info.msg = msg;
	if (_gfx_ctx.debugCallbackSrc != null) _gfx_ctx.debugCallbackSrc(&info);
}

// To remove
fn TodoRenameDebugFunc todo_fix_setDebugCallback(TodoRenameDebugFunc func)
{
	TodoRenameDebugFunc prev = _gfx_ctx.debugCallbackSrc;
	_gfx_ctx.debugCallbackSrc = func;
	return prev;
}

fn void _remove_monitor(GfxMonitorNode* node, GfxMonitorNode* prev)
{
	_gfx_ctx.monitors.count--;
	todo_fix_monitorNode_free(node);
	if (prev != node)
	{
		prev.next = node.next;
	}
	else
	{
		_gfx_ctx.monitors.list.head = node.next;
	}
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
		win.set_mouse(_gfx_ctx.hiddenMouse);
	}
}


// public
fn uint todo_fix_decodeUTF8(ZString string, sz* starting_index)
{
	uint[6] offsets = {0x00000000u, 0x00003080u, 0x000e2080u, 0x03c82080u, 0xfa082080u, 0x82082080u};
	uint codepoint = (char)string[*starting_index];
	sz count = 1;
	while ((string[count + *starting_index] & 0xc0) == 0x80)
	{
		codepoint = (codepoint << 6) + (char)string[count + *starting_index];
		count++;
	}
	*starting_index += count;
	return codepoint - offsets[count - 1];
}

// --- vulkan

