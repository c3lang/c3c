module std::gfx @feat(WASM) @private;
import std::math;

struct GfxGlContext
{
	void* ctx;
	void* format;
}

struct GfxEglContext
{
	void* ctx;
	void* surface;
	void* egl_window;
}

struct GfxWindowSrc
{
	inline IntRect bounds;
	void* window;
	void* view;
	void* mouse;
	void* delegate;
	GfxBackingContext ctx;
	TodoRenameGfxContextType gfxType;
}

fn void _destroy_native() {}
fn void _free_mouse(TodoRenameMouse* mouse) {}
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
fn void _window_move_mouse(GfxWindow* win, GfxPoint where) {}

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

fn void _window_blit_surface_native(GfxWindow* win, GfxSurface* surface)
{
	int minH = win.height < surface.size.y ? win.height : surface.size.y;
	copy_image_data(surface.data, surface.size.x, minH, RGBA8, surface.data, surface.format, surface.convert_fn);
	rgfwJsBlitSurface(surface.data, surface.size.x, surface.size.y, minH);
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


fn void? _window_init_opengl_context_native(GfxWindow* win, GfxGlContext* ctx, GfxGlHints* hints)
{
	win.src.ctx.native = ctx;
	win.src.gfxType = TODO_RENAME_GFX_NATIVE_OPEN_GL;
	int handle = rgfwJsWebglCreateContext(hints);
	if (!handle) return OPENGL_CTX_CREATION_FAILED~;
	ctx.ctx = (void*)(uptr)handle;
	rgfwJsWebglMakeCurrent(handle);
	win.opengl_swap_interval(0);
}

fn void _window_release_opengl_context_native(GfxWindow* win, GfxGlContext* ctx)
{
	rgfwJsWebglDestroyContext((int)(uptr)ctx.ctx);
	win.src.ctx.native.ctx = null;
}

fn GfxEglContext*? _window_create_egl_context_native(GfxWindow* win, GfxGlHints* hints)
{
	return NOT_IMPLEMENTED~;
}

fn void _window_release_egl_context_native(GfxWindow* win, GfxEglContext* ctx) {}

fn void? _init_native(String class_name, GfxInitOption flags)
{
	_gfx_ctx.usingAsyncify = -1;
	GfxMonitor monitor;
	monitor.origin = {};
	monitor.pixelRatio = (float)rgfwJsDevicePixelRatio();
	monitor.mode.size = { rgfwJsInnerWidth(), rgfwJsInnerHeight() };
	monitor.phys_size = {
		(float)(int)(monitor.mode.size.x * monitor.pixelRatio + 0.5f),
		(float)(int)(monitor.mode.size.y * monitor.pixelRatio + 0.5f)
	};

	float dpi = 96.0f * monitor.pixelRatio;
	monitor.scale = (Vec2)(dpi / 96.0f);

	todo_fix_splitBPP(32, &monitor.mode);
	monitor.mode.refresh_rate = 0;

	GfxMonitorNode* node = _monitor_add(&monitor);
	if (node)
	{
		_gfx_ctx.monitors.primary = node;
		todo_fix_monitorCallback(_gfx_ctx.root, &node.mon, true);
	}
}

fn void _surface_release_native(GfxSurface* surface)
{
	*surface = {};
}

fn void? _write_clipboard_native(GfxDataTransfer* data)
{
	rgfwJsWriteClipboard(data.data, data.length);
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
fn void? _surface_init_native(GfxSurface* surface, void* data, int w, int h, GfxFormat format, GfxWindow* window)
{
	surface.data = data;
	surface.size = { w, h };
	surface.format = format;
}