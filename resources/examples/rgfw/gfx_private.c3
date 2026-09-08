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

fn void _window_show_mouse_native(GfxWindow* win, bool show) @feat(!WASM)
{
	rgfw_window_showMouseFlags(win, show);
	if (show)
	{
		win.set_mouse(win.internal.mouse);
	}
	else
	{
		win.set_mouse(_gfx_ctx.hiddenMouse);
	}
}
