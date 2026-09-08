module std::gfx @feat(WASM) @private;
import std::math;

fn void _window_show_native(GfxWindow* win) {}
fn bool _window_is_hidden_native(GfxWindow* win) => false;
fn bool _window_is_minimized_native(GfxWindow* win) => false;
fn bool _window_is_maximized_native(GfxWindow* win) => false;
fn void _window_hide_native(GfxWindow* win) {}
fn void _window_flash_native(GfxWindow* win, GfxFlashRequest request) {}
fn void _window_move_native(GfxWindow* win, int x, int y) {}
fn void _window_restore_native(GfxWindow* win) {}
fn void _window_set_border_native(GfxWindow* win, bool border) {}
fn bool _window_set_icon_native(GfxWindow* win, void* data, int w, int h, GfxFormat format, GfxWindowIconType type) => false;
fn bool _window_is_floating_native(GfxWindow* win) => false;
fn void _window_set_min_size_native(GfxWindow* win, int w, int h) {}

fn void _window_resize_native(GfxWindow* win, int w, int h)
{
	rgfwJsSetCanvasSize(w, h);
}

fn void _window_show_mouse_native(GfxWindow* win, bool show)
{
	rgfw_window_showMouseFlags(win, show);
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

// --- exported

fn void _emscripten_on_drop(char* file, sz size) @export("c3gfx_on_drop")
{
	rgfw_dataDropCallback(_gfx_ctx.root, file, size, FILE);
}

fn void _emscripten_on_fullscreenchange(int screenW, int screenH, bool entering) @export("c3gfx_on_fullscreenchange")
{
	if (!(_gfx_ctx.root.internal.enabled_events & RGFW_WINDOW_RESIZED_FLAG)) return;
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
	rgfw_mouseButtonCallback(_gfx_ctx.root, (GfxMouseButton)button, true);
}
