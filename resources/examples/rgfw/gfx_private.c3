module std::gfx @private;
import libc;

fn void*? _dlopen_any(ZString... names) @feat(POSIX)
{
	foreach (name : names)
	{
		void* proc = libc::dlopen(name, libc::RTLD_LAZY | libc::RTLD_LOCAL);
		if (proc) return proc;
	}
	return NOT_FOUND~;
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
fn void rgfw_debugCallback(RgfwDebugType type, RgfwErrorCode code, ZString msg)
{
	RgfwDebugInfo info;
	info.type = type;
	info.code = code;
	info.msg = msg;
	if (_gfx_ctx.debugCallbackSrc != null) _gfx_ctx.debugCallbackSrc(&info);
}

// To remove
fn RgfwDebugFunc rgfw_setDebugCallback(RgfwDebugFunc func)
{
	RgfwDebugFunc prev = _gfx_ctx.debugCallbackSrc;
	_gfx_ctx.debugCallbackSrc = func;
	return prev;
}

fn void _remove_monitor(GfxMonitorNode* node, GfxMonitorNode* prev)
{
	_gfx_ctx.monitors.count--;
	rgfw_monitorNode_free(node);
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
fn uint rgfw_decodeUTF8(ZString string, sz* starting_index)
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
