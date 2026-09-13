module std::gfx @feat(WASM) @private;
import std::math, std::time;

struct GfxGlContext
{
	void* ctx;
	void* format;
}

struct GfxNativeImage
{
	GfxFormat format;
	char* buffer;
	void* native0;
	void* native1;
	void* native2;
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
	GfxBacking backing;
}

fn void _destroy_native() {}

fn void _stop_check_events_native()
{
	_gfx_ctx.stop_check_events = true;
}

fn GfxMouse*? _create_mouse_native(void* data, GfxSize size, GfxFormat format) => NOT_IMPLEMENTED~;

fn IntRect _monitor_get_workarea(GfxMonitor* monitor) => { -1, -1, -1, -1 };
fn void? _monitor_request_mode_native(GfxMonitor* mon, GfxMonitorMode* mode, GfxMonitorModeRequest request) => NOT_IMPLEMENTED~;
fn void? _monitor_set_mode_native(GfxMonitor* mon, GfxMonitorMode* mode) => NOT_IMPLEMENTED~;
fn GfxGammaRamp? _monitor_get_gamma_ramp_native(GfxMonitor* monitor, Allocator a) => NOT_IMPLEMENTED~;
fn void? _monitor_set_gamma_ramp_native(GfxMonitor* monitor, GfxGammaRamp ramp) => NOT_IMPLEMENTED~;
fn void? _monitor_set_gamma_native(GfxMonitor* monitor, float gamma) => NOT_IMPLEMENTED~;
fn void _monitor_node_free_native(GfxMonitorNode* node) {}

fn GfxDataTransfer? _read_clipboard_native(Allocator allocator, GfxDataTransferType requestedType) => NOT_IMPLEMENTED~;

fn void _poll_monitors_native() {}
fn void _free_mouse(GfxMouse* mouse) {}
fn void _wait_for_event_native(Duration duration) {}
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
fn void _window_set_aspect_ratio_native(GfxWindow* win, int w, int h) {}
fn void _window_minimize_native(GfxWindow* win) {}
fn void _window_raise_native(GfxWindow* win) {}
fn void _window_set_floating_native(GfxWindow* win, bool floating) {}
fn void _window_close_native(GfxWindow* win) {}
fn void _window_set_raw_mouse_mode_native(GfxWindow* win, bool state) {}


fn bool _window_set_mouse_native(GfxWindow* win, GfxMouse* mouse)
{
	ZString name = (ZString)mouse;
	gfxJsSetCursor(name, name.len());
	return true;
}

fn void _window_set_opacity_native(GfxWindow* win, char opacity)
{
	gfxJsSetOpacity((float)opacity / 255.0f);
}

fn void _window_maximize_native(GfxWindow* win)
{
	(void)win.resize(_window_find_monitor_native(win).mode.size);
	win.move({});
}

fn GfxMonitor*? _window_find_monitor_native(GfxWindow* win) => get_primary_monitor();

fn void _window_resize_native(GfxWindow* win, GfxSize size)
{
	gfxJsSetCanvasSize(...size);
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
		gfxJsHideCursor();
	}
}

fn void _window_blit_surface_native(GfxWindow* win, GfxSurface* surface)
{
	int minH = win.height < surface.size.y ? win.height : surface.size.y;
	copy_image_data(surface.data, surface.size.x, minH, RGBA8, surface.data, surface.format, surface.convert_fn);
	gfxJsBlitSurface(surface.data, surface.size.x, surface.size.y, minH);
}

fn void _window_set_capture_mouse_native(GfxWindow* win, bool state)
{
	if (state)
	{
		gfxJsRequestPointerLock();
	}
	else
	{
		gfxJsExitPointerLock();
	}
}


fn void _window_set_fullscreen_native(GfxWindow* win, bool fullscreen)
{
	if (fullscreen)
	{
		win.internal.flags.fullscreen = true;
		gfxJsRequestFullscreen();
		return;
	}
	win.internal.flags.fullscreen = false;
	gfxJsExitFullscreen();
}

fn GfxPoint _get_global_mouse_native()
{
	return { gfxJsGetMouseX(), gfxJsGetMouseY() };
}

fn void _window_set_name_native(GfxWindow* win, String name)
{
	gfxJsSetWindowTitle((ZString)name, name.len);
}

fn void _window_set_mouse_passthrough_native(GfxWindow* win, bool passthrough)
{
	gfxJsSetPointerEvents((int)passthrough);
}

fn bool _vulkan_get_physical_device_presentation_support_native(VkInstance instance, VkPhysicalDevice physical_device, uint queue_family_index) => false;

fn bool _opengl_extension_is_supported_native(String extension)
{
	return gfxJsExtensionSupported((ZString)extension.ptr, extension.len) != 0;
}

fn GfxFn _get_opengl_proc_native(ZString procname)
{
	return gfxJsGetProcAddress(procname);
}

fn void? _window_init_opengl_context_native(GfxWindow* win, GfxGlContext* ctx, GfxGlHints* hints)
{
	win.src.ctx.native = ctx;
	win.src.backing = { .opengl };
	int handle = gfxJsWebglCreateContext(hints);
	if (!handle) return OPENGL_CTX_CREATION_FAILED~;
	ctx.ctx = (void*)(uptr)handle;
	gfxJsWebglMakeCurrent(handle);
	win.opengl_swap_interval(0);
}

struct WgpuEmscriptenSurfaceSourceCanvasHTMLSelector
{
	WgpuChainedStruct chain;
	WgpuStringView selector;
}

fn WGPUSurface _window_create_wgpu_surface_native(GfxWindow* window, WGPUInstance instance)
{
	WgpuEmscriptenSurfaceSourceCanvasHTMLSelector canvasDesc;
	canvasDesc.chain.next = null;
	canvasDesc.chain.sType = WGPU_STYPE_EMSCRIPTEN_SURFACE_SOURCE_CANVAS_HTML_SELECTOR;
	canvasDesc.selector.data = "#canvas";
	canvasDesc.selector.length = 7;
	WgpuSurfaceDescriptor surfaceDesc;
	surfaceDesc.nextInChain = (WgpuChainedStruct*)&canvasDesc;
	return wgpuInstanceCreateSurface(instance, &surfaceDesc);
}

fn GfxPoint _window_fetch_size_native(GfxWindow* win)
{
	return win.get_size();
}

fn void _window_release_opengl_context_native(GfxWindow* win, GfxGlContext* ctx)
{
	gfxJsWebglDestroyContext((int)(uptr)ctx.ctx);
	win.src.ctx.native.ctx = null;
}

fn void _window_opengl_swap_buffers_native(GfxWindow* win)
{
	gfxJsWebglCommitFrame();
}

fn GfxEglContext*? _window_create_egl_context_native(GfxWindow* win, GfxGlHints* hints)
{
	return NOT_IMPLEMENTED~;
}

fn void? _create_window_native(String name, GfxWindowFlags flags, GfxWindow* win)
{
	gfxJsSetCanvasSize(win.width, win.height);
	gfxJsSetWindowTitle((ZString)name, name.len);
	gfxJsSetupWindowListeners();

	if (flags.allow_dnd) win.internal.flags.allow_dnd = true;

	gfxJsSetupDropListeners();

	_window_set_flags(win, flags, {});
	win.src.backing = {};
	if (flags.open_gl) _window_create_opengl_context_native(win, get_global_opengl_hints())!;
	if (flags.egl) _window_create_egl_context_native(win, get_global_opengl_hints())!;
}

fn void _window_release_egl_context_native(GfxWindow* win, GfxEglContext* ctx) {}

fn void? _init_native(String class_name, GfxInitOption flags)
{
	_gfx_ctx.usingAsyncify = -1;
	GfxMonitor monitor;
	monitor.origin = {};
	monitor.pixelRatio = (float)gfxJsDevicePixelRatio();
	monitor.mode.size = { gfxJsInnerWidth(), gfxJsInnerHeight() };
	monitor.phys_size = {
		(float)(int)(monitor.mode.size.x * monitor.pixelRatio + 0.5f),
		(float)(int)(monitor.mode.size.y * monitor.pixelRatio + 0.5f)
	};

	float dpi = 96.0f * monitor.pixelRatio;
	monitor.scale = (Vec2)(dpi / 96.0f);

	_split_bpp(32, &monitor.mode);
	monitor.mode.refresh_rate = 0;

	GfxMonitorNode* node = _monitor_add(&monitor);
	if (node)
	{
		_gfx_ctx.monitors.primary = node;
		_monitor_callback(_gfx_ctx.root, &node.mon, true);
	}
}

fn void _poll_events_native()
{
	if (_gfx_ctx.usingAsyncify == -1) _gfx_ctx.usingAsyncify = gfxJsUsingAsyncify();
	_reset_prev_state();
	if (_gfx_ctx.usingAsyncify) gfxJsSleep(0);
}

fn void _surface_release_native(GfxSurface* surface)
{
	*surface = {};
}

fn void? _write_clipboard_native(GfxDataTransfer* data)
{
	gfxJsWriteClipboard(data.text, data.text.len);
}


fn GfxMonitorMode[] _monitor_get_modes_native(GfxMonitor* mon) => {};

// --- exported

fn void _emscripten_on_drop(char* file, sz size) @export("c3gfx_on_drop")
{
	_data_drop_callback(_gfx_ctx.root, file, size, FILE);
}

fn void _emscripten_on_fullscreenchange(int screenW, int screenH, bool entering) @export("c3gfx_on_fullscreenchange")
{
	if (!(_gfx_ctx.root.internal.enabled_events.window_resized)) return;
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
	gfxJsFocusCanvas();
	gfxJsSetCanvasSize(..._gfx_ctx.root.size);
	_window_resized_callback(_gfx_ctx.root, _gfx_ctx.root.size);
}

fn void _emscripten_on_mousedown(int button) @export("c3gfx_on_mousedown")
{
	if (button > 2) button += 2;
	_mouse_button_callback(_gfx_ctx.root, (GfxMouseButton)button, true);
}

// public
fn void? _surface_init_native(GfxSurface* surface, void* data, int w, int h, GfxFormat format, GfxWindow* window)
{
	surface.data = data;
	surface.size = { w, h };
	surface.format = format;
}

fn void _window_make_opengl_current_context_native(GfxWindow* win)
{
	if (!win)
	{
		gfxJsWebglMakeCurrent(0);
	}
	else
	{
		gfxJsWebglMakeCurrent((int)(uptr)win.src.ctx.native.ctx);
	}
}

fn void* _get_current_opengl_context_native()
{
	return (void*)(uptr)gfxJsWebglGetCurrentContext();
}

fn GfxMouse*? _create_standard_mouse_native(GfxMouseIcon mouse)
{
	ZString cursor_name;
	switch (mouse)
	{
		case NORMAL: cursor_name = "default";
		case ARROW: cursor_name = "default";
		case IBEAM: cursor_name = "text";
		case CROSSHAIR: cursor_name = "crosshair";
		case POINTING_HAND: cursor_name = "pointer";
		case RESIZE_EW: cursor_name = "ew-resize";
		case RESIZE_NS: cursor_name = "ns-resize";
		case RESIZE_NWSE: cursor_name = "nwse-resize";
		case RESIZE_NESW: cursor_name = "nesw-resize";
		case RESIZE_NW: cursor_name = "nw-resize";
		case RESIZE_N: cursor_name = "n-resize";
		case RESIZE_NE: cursor_name = "ne-resize";
		case RESIZE_E: cursor_name = "e-resize";
		case RESIZE_SE: cursor_name = "se-resize";
		case RESIZE_S: cursor_name = "s-resize";
		case RESIZE_SW: cursor_name = "sw-resize";
		case RESIZE_W: cursor_name = "w-resize";
		case RESIZE_ALL: cursor_name = "move";
		case NOT_ALLOWED: cursor_name = "not-allowed";
		case WAIT: cursor_name = "wait";
		case PROGRESS: cursor_name = "progress";
		default: return MOUSE_NOT_SUPPORTED~;
	}
	return (GfxMouse*)cursor_name;
}

fn GfxKey _physical_to_mapped_key_native(GfxKey key) => key;

fn void _init_keycodes_native()
{
	_gfx_ctx.keycodes = {
		[DOM_VK_BACK_QUOTE] = BACKTICK,         [DOM_VK_0] = NUM0,                      [DOM_VK_1] = NUM1,
		[DOM_VK_2] = NUM2,                      [DOM_VK_3] = NUM3,                      [DOM_VK_4] = NUM4,
		[DOM_VK_5] = NUM5,                      [DOM_VK_6] = NUM6,                      [DOM_VK_7] = NUM7,
		[DOM_VK_8] = NUM8,                      [DOM_VK_9] = NUM9,                      [DOM_VK_SPACE] = SPACE,
		[DOM_VK_A] = A,                         [DOM_VK_B] = B,                         [DOM_VK_C] = C,
		[DOM_VK_D] = D,                         [DOM_VK_E] = E,                         [DOM_VK_F] = F,
		[DOM_VK_G] = G,                         [DOM_VK_H] = H,                         [DOM_VK_I] = I,
		[DOM_VK_J] = J,                         [DOM_VK_K] = K,                         [DOM_VK_L] = L,
		[DOM_VK_M] = M,                         [DOM_VK_N] = N,                         [DOM_VK_O] = O,
		[DOM_VK_P] = P,                         [DOM_VK_Q] = Q,                         [DOM_VK_R] = R,
		[DOM_VK_S] = S,                         [DOM_VK_T] = T,                         [DOM_VK_U] = U,
		[DOM_VK_V] = V,                         [DOM_VK_W] = W,                         [DOM_VK_X] = X,
		[DOM_VK_Y] = Y,                         [DOM_VK_Z] = Z,                         [DOM_VK_PERIOD] = PERIOD,
		[DOM_VK_COMMA] = COMMA,                 [DOM_VK_SLASH] = SLASH,                 [DOM_VK_OPEN_BRACKET] = BRACKET,
		[DOM_VK_CLOSE_BRACKET] = CLOSE_BRACKET, [DOM_VK_SEMICOLON] = SEMICOLON,         [DOM_VK_QUOTE] = APOSTROPHE,
		[DOM_VK_BACK_SLASH] = BACK_SLASH,       [DOM_VK_RETURN] = RETURN,               [DOM_VK_DELETE] = DELETE,
		[DOM_VK_NUM_LOCK] = NUM_LOCK,           [DOM_VK_DIVIDE] = PAD_SLASH,            [DOM_VK_MULTIPLY] = PAD_MULTIPLY,
		[DOM_VK_SUBTRACT] = PAD_MINUS,          [DOM_VK_NUMPAD1] = PAD1,                [DOM_VK_NUMPAD2] = PAD2,
		[DOM_VK_NUMPAD3] = PAD3,                [DOM_VK_NUMPAD4] = PAD4,                [DOM_VK_NUMPAD5] = PAD5,
		[DOM_VK_NUMPAD6] = PAD6,                [DOM_VK_NUMPAD9] = PAD9,                [DOM_VK_NUMPAD0] = PAD0,
		[DOM_VK_DECIMAL] = PAD_PERIOD,          [DOM_VK_HYPHEN_MINUS] = MINUS,          [DOM_VK_EQUALS] = EQUALS,
		[DOM_VK_BACK_SPACE] = BACK_SPACE,       [DOM_VK_TAB] = TAB,                     [DOM_VK_CAPS_LOCK] = CAPS_LOCK,
		[DOM_VK_SHIFT] = SHIFT_L,               [DOM_VK_CONTROL] = CONTROL_L,           [DOM_VK_ALT] = ALT_L,
		[DOM_VK_META] = SUPER_L,                [DOM_VK_F1] = F1,                       [DOM_VK_F2] = F2,
		[DOM_VK_F3] = F3,                       [DOM_VK_F4] = F4,                       [DOM_VK_F5] = F5,
		[DOM_VK_F6] = F6,                       [DOM_VK_F7] = F7,                       [DOM_VK_F8] = F8,
		[DOM_VK_F9] = F9,                       [DOM_VK_F10] = F10,                     [DOM_VK_F11] = F11,
		[DOM_VK_F12] = F12,                     [DOM_VK_UP] = UP,                       [DOM_VK_DOWN] = DOWN,
		[DOM_VK_LEFT] = LEFT,                   [DOM_VK_RIGHT] = RIGHT,                 [DOM_VK_INSERT] = INSERT,
		[DOM_VK_END] = END,                     [DOM_VK_PAGE_UP] = PAGE_UP,             [DOM_VK_PAGE_DOWN] = PAGE_DOWN,
		[DOM_VK_ESCAPE] = ESCAPE,               [DOM_VK_HOME] = HOME,                   [DOM_VK_SCROLL_LOCK] = SCROLL_LOCK,
		[DOM_VK_PRINTSCREEN] = PRINT_SCREEN,    [DOM_VK_PAUSE] = PAUSE,                 [DOM_VK_F13] = F13,
		[DOM_VK_F14] = F14,                     [DOM_VK_F15] = F15,                     [DOM_VK_F16] = F16,
		[DOM_VK_F17] = F17,                     [DOM_VK_F18] = F18,                     [DOM_VK_F19] = F19,
		[DOM_VK_F20] = F20,                     [DOM_VK_F21] = F21,                     [DOM_VK_F22] = F22,
		[DOM_VK_F23] = F23,                     [DOM_VK_F24] = F24,
	};
}

// ---

// TODO All of this is horribly broken
extern fn void gfxJsSetCanvasSize(int w, int h) @wasm("env", "emscripten_set_canvas_size");
extern fn void gfxJsSetWindowTitle(ZString name, sz len) @wasm("env", "emscripten_set_window_title");
extern fn void gfxJsSetupWindowListeners() @wasm("env", "emscripten_setup_window_listeners");
extern fn void gfxJsSetupDropListeners() @wasm("env", "emscripten_setup_drop_listeners");

// Each of the following registers exactly one DOM/window listener, mirroring
// _createWindowPlatform's emscripten_set_*_callback calls one-for-one so

extern fn void gfxJsSetResizeCallback() @wasm("env", "emscripten_set_resize_callback");
extern fn void gfxJsSetFullscreenChangeCallback() @wasm("env", "emscripten_set_fullscreenchange_callback");
extern fn void gfxJsSetMouseMoveCallback() @wasm("env", "emscripten_set_mousemove_callback");
extern fn void gfxJsSetTouchStartCallback() @wasm("env", "emscripten_set_touchstart_callback");
extern fn void gfxJsSetTouchEndCallback() @wasm("env", "emscripten_set_touchend_callback");
extern fn void gfxJsSetTouchMoveCallback() @wasm("env", "emscripten_set_touchmove_callback");

extern fn void gfxJsSetTouchCancelCallback() @wasm("env", "emscripten_set_touchcancel_callback");
extern fn void gfxJsSetMouseDownCallback() @wasm("env", "emscripten_set_mousedown_callback");
extern fn void gfxJsSetMouseUpCallback() @wasm("env", "emscripten_set_mouseup_callback");
extern fn void gfxJsSetWheelCallback() @wasm("env", "emscripten_set_wheel_callback");
extern fn void gfxJsSetFocusInCallback() @wasm("env", "emscripten_set_focusin_callback");
extern fn void gfxJsSetFocusOutCallback() @wasm("env", "emscripten_set_focusout_callback");

extern fn void gfxJsSetKeyCallbacks() @wasm("env", "emscripten_set_key_callbacks");
extern fn void gfxJsSetDropCallbacks() @wasm("env", "emscripten_set_drop_callbacks");
// Needs `EM_ASM("Module.canvas.focus();")` after every fullscreen
// transition so keyboard events keep targeting the canvas.
extern fn void gfxJsFocusCanvas() @wasm("env", "emscripten_focus_canvas");
extern fn void gfxJsBlitSurface(char* data, int w, int h, int minH) @wasm("env", "emscripten_blit_surface");
extern fn void gfxJsSetCursor(ZString name, sz len) @wasm("env", "emscripten_set_cursor");
extern fn void gfxJsHideCursor() @wasm("env", "emscripten_hide_cursor");
extern fn int gfxJsGetMouseX() @wasm("env", "emscripten_get_mouse_x");
extern fn int gfxJsGetMouseY() @wasm("env", "emscripten_get_mouse_y");
extern fn void gfxJsSetPointerEvents(int passthrough) @wasm("env", "emscripten_set_pointer_events");
extern fn void gfxJsWriteClipboard(char* data, sz len) @wasm("env", "emscripten_write_clipboard");
extern fn int gfxJsUsingAsyncify() @wasm("env", "emscripten_using_asyncify");
extern fn void gfxJsSleep(int ms) @wasm("env", "emscripten_sleep");
extern fn double gfxJsDevicePixelRatio() @wasm("env", "emscripten_device_pixel_ratio");
extern fn int gfxJsInnerWidth() @wasm("env", "emscripten_inner_width");
extern fn int gfxJsInnerHeight() @wasm("env", "emscripten_inner_height");
extern fn void gfxJsRequestPointerLock() @wasm("env", "emscripten_request_pointer_lock");
extern fn void gfxJsExitPointerLock() @wasm("env", "emscripten_exit_pointer_lock");
extern fn void gfxJsRequestFullscreen() @wasm("env", "emscripten_request_fullscreen");
extern fn void gfxJsExitFullscreen() @wasm("env", "emscripten_exit_fullscreen");
extern fn void gfxJsSetOpacity(float opacity) @wasm("env", "emscripten_set_opacity");
extern fn int gfxJsWebglCreateContext(GfxGlHints* hints) @wasm("env", "emscripten_webgl_create_context");
extern fn void gfxJsWebglDestroyContext(int ctx) @wasm("env", "emscripten_webgl_destroy_context");
extern fn void gfxJsWebglMakeCurrent(int ctx) @wasm("env", "emscripten_webgl_make_current");
extern fn void gfxJsWebglCommitFrame() @wasm("env", "emscripten_webgl_commit_frame");
extern fn int gfxJsWebglGetCurrentContext() @wasm("env", "emscripten_webgl_get_current_context");
extern fn int gfxJsExtensionSupported(ZString name, sz len) @wasm("env", "emscripten_extension_supported");
extern fn GfxFn gfxJsGetProcAddress(ZString name) @wasm("env", "emscripten_webgl_get_proc_address");
extern fn void gfxJsMkdir(ZString name) @wasm("env", "emscripten_mkdir");
extern fn void gfxJsWriteFile(ZString path, char* data, sz len) @wasm("env", "emscripten_write_file");
