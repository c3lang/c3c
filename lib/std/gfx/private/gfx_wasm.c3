module std::gfx @feat(WASM) @private;
import std::math;

fn void _window_show_native(GfxWindow* win) {}
fn bool _window_is_hidden_native(GfxWindow* win) => false;
fn bool _window_is_minimized_native(GfxWindow* win) => false;
fn bool _window_is_maximized_native(GfxWindow* win) => false;
fn void _window_hide_native(GfxWindow* win) {}
fn void _window_flash_native(GfxWindow* win, GfxFlashRequest request) {}
fn void _window_move_native(GfxWindow* win, GfxPoint where) {}
fn void _window_restore_native(GfxWindow* win) {}
fn void _window_set_border_native(GfxWindow* win, bool border) {}
fn bool _window_set_icon_native(GfxWindow* win, void* data, int w, int h, GfxFormat format, GfxWindowIconType type) => false;
fn bool _window_is_floating_native(GfxWindow* win) => false;
fn void _window_set_min_size_native(GfxWindow* win, GfxSize size) {}
fn void _window_focus_native(GfxWindow* win) {}
fn void _window_set_max_size_native(GfxWindow* win, GfxSize size) {}

fn void _window_maximize_native(GfxWindow* win)
{
	GfxMonitor* mon = find_monitor(win);
	if (mon) win.resize(mon.mode.size);
	win.move({});
}

fn void _window_resize_native(GfxWindow* win, GfxSize size)
{
	rgfwJsSetCanvasSize(...size);
}

fn void _window_show_mouse_native(GfxWindow* win, bool show)
{
	_window_show_mouse_flags(win, show);
	if (show)
	{
		win.set_mouse_default();
	}
	else
	{
		rgfwJsHideCursor();
	}
}

fn void _window_set_fullscreen_native(GfxWindow* win, bool fullscreen)
{
	if (fullscreen)
	{
		win.internal.flags.fullscreen = true;
		rgfwJsRequestFullscreen();
		return;
	}
	win.internal.flags.fullscreen = false;
	rgfwJsExitFullscreen();
}

fn GfxPoint _get_global_mouse_native()
{
	return { rgfwJsGetMouseX(), rgfwJsGetMouseY() };
}

fn GfxMonitorMode[] _monitor_get_modes_native(GfxMonitor* mon) => {};

// --- exported

fn void _emscripten_on_drop(char* file, sz size) @export("c3gfx_on_drop")
{
	todo_fix_dataDropCallback(_gfx_ctx.root, file, size, FILE);
}

fn void _emscripten_on_fullscreenchange(int screenW, int screenH, bool entering) @export("c3gfx_on_fullscreenchange")
{
	if (!(_gfx_ctx.root.internal.enabled_events & TODO_RENAME_WINDOW_RESIZED_FLAG)) return;
	static GfxPoint original;
	if (entering)
	{
		original = _gfx_ctx.root.size;
		_gfx_ctx.root.size = { screenW, screenH };
	}
	else
	{
		_gfx_ctx.root.size = original;
	}
	rgfwJsFocusCanvas();
	rgfwJsSetCanvasSize(..._gfx_ctx.root.size);
	_window_resized_callback(_gfx_ctx.root, _gfx_ctx.root.size);
}

fn void _emscripten_on_mousedown(int button) @export("c3gfx_on_mousedown")
{
	if (button > 2) button += 2;
	todo_fix_mouseButtonCallback(_gfx_ctx.root, (GfxMouseButton)button, true);
}

// public
fn void? GfxSurface.init(&self, void* data, int w, int h, GfxFormat format, GfxWindow* window = null)
{
	self.data = data;
	self.size = { w, h };
	self.format = format;
}