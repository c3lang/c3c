module std::gfx::wasm @feat(WASM) @private;
import std::math, std::time;
import std::gfx::egl @public;

// --- Gfx

fn void destroy_platform() {}

fn void poll_events_platform()
{
	if (gfx::_gfx.usingAsyncify == -1) gfx::_gfx.usingAsyncify = gfx::gfxJsUsingAsyncify();
	gfx::_reset_prev_state();
	if (gfx::_gfx.usingAsyncify) gfx::gfxJsSleep(0);
}

// --- GfxMonitor functions

fn GfxGammaRamp? monitor_get_gamma_ramp(GfxMonitor* monitor, Allocator a) => NOT_IMPLEMENTED~;

fn GfxMonitorMode[] monitor_get_modes(GfxMonitor* mon) => {};

fn IntRect monitor_get_workarea(GfxMonitor* monitor) => { -1, -1, -1, -1 };

fn void monitor_node_free(GfxMonitorNode* node) {}

fn void? monitor_request_mode(GfxMonitor* mon, GfxMonitorMode* mode, GfxMonitorModeRequest request) => NOT_IMPLEMENTED~;

fn void? monitor_set_gamma(GfxMonitor* monitor, float gamma) => NOT_IMPLEMENTED~;

fn void? monitor_set_gamma_ramp(GfxMonitor* monitor, GfxGammaRamp ramp) => NOT_IMPLEMENTED~;

fn void? monitor_set_mode(GfxMonitor* mon, GfxMonitorMode* mode) => NOT_IMPLEMENTED~;

fn void poll_monitors() {}

// --- GfxMouse and mouse related

fn GfxMouse*? create_mouse(void* data, GfxSize size, GfxFormat format) => NOT_IMPLEMENTED~;

fn GfxMouse*? create_standard_mouse(GfxMouseIcon mouse)
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
		default: return gfx::MOUSE_NOT_SUPPORTED~;
	}
	return (GfxMouse*)cursor_name;
}

fn void free_mouse(GfxMouse* mouse) {}

fn GfxPoint get_global_mouse()
{
	return { gfx::gfxJsGetMouseX(), gfx::gfxJsGetMouseY() };
}

// --- GfxWindow

fn void window_blit_surface(GfxWindow* win, GfxSurface* surface)
{
	int minH = win.height < surface.size.y ? win.height : surface.size.y;
	gfx::copy_image_data(surface.data, surface.size.x, minH, RGBA8, surface.data, surface.format, surface.convert_fn);
	gfx::gfxJsBlitSurface(surface.data, surface.size.x, surface.size.y, minH);
}

fn void? window_create_platform(GfxWindow* win, String name, GfxWindowFlags flags)
{
	gfx::gfxJsSetCanvasSize(win.width, win.height);
	gfx::gfxJsSetWindowTitle((ZString)name, name.len);
	gfx::gfxJsSetupWindowListeners();

	if (flags.allow_dnd) win.internal.flags.allow_dnd = true;

	gfx::gfxJsSetupDropListeners();

	gfx::_window_set_flags(win, flags, {});
	win.src.backing = {};
	if (flags.open_gl) gfx::_window_create_opengl_context_native(win, gfx::get_global_opengl_hints())!;
	if (flags.egl) egl::create_context(win, gfx::get_global_opengl_hints())!;
}

fn void window_destroy_platform(GfxWindow* win) {}

fn GfxPoint window_fetch_size(GfxWindow* win)
{
	return win.get_size();
}

fn GfxMonitor*? window_find_monitor(GfxWindow* win) => gfx::get_primary_monitor();

fn void window_flash(GfxWindow* win, GfxFlashRequest request) {}

fn void window_focus(GfxWindow* win) {}

fn void window_hide(GfxWindow* win) {}

fn bool window_is_floating(GfxWindow* win) => false;

fn bool window_is_hidden(GfxWindow* win) => false;

fn bool window_is_maximized(GfxWindow* win) => false;

fn bool window_is_minimized(GfxWindow* win) => false;

fn void window_maximize_platform(GfxWindow* win)
{
	(void)win.resize(window_find_monitor(win).mode.size);
	win.move({});
}

fn void window_minimize_platform(GfxWindow* win) {}

fn void window_move_mouse(GfxWindow* win, GfxPoint where) {}

fn void window_move_platform(GfxWindow* win, GfxPoint where) {}

fn void window_raise(GfxWindow* win) {}

fn void window_resize_platform(GfxWindow* win, GfxSize size)
{
	gfx::gfxJsSetCanvasSize(...size);
}

fn void window_restore_platform(GfxWindow* win) {}

fn void window_set_aspect_ratio(GfxWindow* win, int w, int h) {}

fn void window_set_border(GfxWindow* win, bool border) {}

fn void window_set_capture_mouse_platform(GfxWindow* win, bool state)
{
	if (state)
	{
		gfx::gfxJsRequestPointerLock();
	}
	else
	{
		gfx::gfxJsExitPointerLock();
	}
}

fn void window_set_floating(GfxWindow* win, bool floating) {}

fn void window_set_fullscreen(GfxWindow* win, bool fullscreen)
{
	if (fullscreen)
	{
		win.internal.flags.fullscreen = true;
		gfx::gfxJsRequestFullscreen();
		return;
	}
	win.internal.flags.fullscreen = false;
	gfx::gfxJsExitFullscreen();
}

fn bool window_set_icon(GfxWindow* win, void* data, int w, int h, GfxFormat format, GfxWindowIconType type) => false;

fn void window_set_max_size(GfxWindow* win, GfxSize size) {}

fn void window_set_min_size(GfxWindow* win, GfxSize size) {}

fn void window_set_mouse_passthrough(GfxWindow* win, bool passthrough)
{
	gfx::gfxJsSetPointerEvents((int)passthrough);
}

fn void window_set_mouse_platform(GfxWindow* win, GfxMouse* mouse)
{
	ZString name = (ZString)mouse;
	gfx::gfxJsSetCursor(name, name.len());
}

fn void window_set_name(GfxWindow* win, String name)
{
	gfx::gfxJsSetWindowTitle((ZString)name, name.len);
}

fn void window_set_opacity(GfxWindow* win, float opacity)
{
	gfx::gfxJsSetOpacity(opacity);
}

fn void window_set_raw_mouse_mode_platform(GfxWindow* win, bool state) {}

fn void window_show_platform(GfxWindow* win) {}

// Window - opengl

fn void? window_init_opengl_context_platform(GfxWindow* win, GfxGlContext* ctx, GfxGlHints* hints)
{
	win.src.ctx.native = ctx;
	win.src.backing = { .opengl };
	int handle = gfx::gfxJsWebglCreateContext(hints);
	if (!handle) return gfx::OPENGL_CTX_CREATION_FAILED~;
	ctx.ctx = (void*)(uptr)handle;
	gfx::gfxJsWebglMakeCurrent(handle);
	gfx::_window_set_opengl_swap_interval(win, 0);
}

fn void window_make_current_opengl_context(GfxWindow* win)
{
	if (!win)
	{
		gfx::gfxJsWebglMakeCurrent(0);
	}
	else
	{
		gfx::gfxJsWebglMakeCurrent((int)(uptr)win.src.ctx.native.ctx);
	}
}

fn void window_opengl_swap_buffers(GfxWindow* win)
{
	gfx::gfxJsWebglCommitFrame();
}

fn void window_release_opengl_context_platform(GfxWindow* win, GfxGlContext* ctx)
{
	gfx::gfxJsWebglDestroyContext((int)(uptr)ctx.ctx);
	win.src.ctx.native.ctx = null;
}

// Window WGPU

fn WGPUSurface window_create_wgpu_surface(GfxWindow* window, WGPUInstance instance)
{
	WgpuEmscriptenSurfaceSourceCanvasHTMLSelector canvasDesc;
	canvasDesc.chain.next = null;
	canvasDesc.chain.sType = gfx::WGPU_STYPE_EMSCRIPTEN_SURFACE_SOURCE_CANVAS_HTML_SELECTOR;
	canvasDesc.selector.data = "#canvas";
	canvasDesc.selector.length = 7;
	WgpuSurfaceDescriptor surfaceDesc;
	surfaceDesc.nextInChain = (WgpuChainedStruct*)&canvasDesc;
	return gfx::wgpuInstanceCreateSurface(instance, &surfaceDesc);
}

// --- Clipboard

fn GfxDataTransfer? read_clipboard(Allocator allocator, GfxDataTransferType requestedType) => NOT_IMPLEMENTED~;

fn void? write_clipboard(GfxDataTransfer* data)
{
	gfx::gfxJsWriteClipboard(data.text, data.text.len);
}

// --- GfxSurface

fn void? surface_init(GfxSurface* surface, void* data, int w, int h, GfxFormat format, GfxWindow* window)
{
	surface.data = data;
	surface.size = { w, h };
	surface.format = format;
}

fn void surface_release(GfxSurface* surface)
{
	*surface = {};
}

// --- Vulkan

fn bool vulkan_get_physical_device_presentation_support(VkInstance instance, VkPhysicalDevice physical_device, uint queue_family_index) => false;

// --- Opengl

fn void* get_current_opengl_context()
{
	return (void*)(uptr)gfx::gfxJsWebglGetCurrentContext();
}

fn void* get_opengl_proc(ZString procname)
{
	return gfx::gfxJsGetProcAddress(procname);
}

fn bool opengl_extension_is_supported_platform(String extension)
{
	return gfx::gfxJsExtensionSupported((ZString)extension.ptr, extension.len) != 0;
}

// --- Keys

fn void init_keycodes_platform()
{
	gfx::_gfx.keycodes = {
		[DomKey.VK_BACK_QUOTE] = BACKTICK,         [DomKey.VK_0] = NUM0,                      [DomKey.VK_1] = NUM1,
		[DomKey.VK_2] = NUM2,                      [DomKey.VK_3] = NUM3,                      [DomKey.VK_4] = NUM4,
		[DomKey.VK_5] = NUM5,                      [DomKey.VK_6] = NUM6,                      [DomKey.VK_7] = NUM7,
		[DomKey.VK_8] = NUM8,                      [DomKey.VK_9] = NUM9,                      [DomKey.VK_SPACE] = SPACE,
		[DomKey.VK_A] = A,                         [DomKey.VK_B] = B,                         [DomKey.VK_C] = C,
		[DomKey.VK_D] = D,                         [DomKey.VK_E] = E,                         [DomKey.VK_F] = F,
		[DomKey.VK_G] = G,                         [DomKey.VK_H] = H,                         [DomKey.VK_I] = I,
		[DomKey.VK_J] = J,                         [DomKey.VK_K] = K,                         [DomKey.VK_L] = L,
		[DomKey.VK_M] = M,                         [DomKey.VK_N] = N,                         [DomKey.VK_O] = O,
		[DomKey.VK_P] = P,                         [DomKey.VK_Q] = Q,                         [DomKey.VK_R] = R,
		[DomKey.VK_S] = S,                         [DomKey.VK_T] = T,                         [DomKey.VK_U] = U,
		[DomKey.VK_V] = V,                         [DomKey.VK_W] = W,                         [DomKey.VK_X] = X,
		[DomKey.VK_Y] = Y,                         [DomKey.VK_Z] = Z,                         [DomKey.VK_PERIOD] = PERIOD,
		[DomKey.VK_COMMA] = COMMA,                 [DomKey.VK_SLASH] = SLASH,                 [DomKey.VK_OPEN_BRACKET] = BRACKET,
		[DomKey.VK_CLOSE_BRACKET] = CLOSE_BRACKET, [DomKey.VK_SEMICOLON] = SEMICOLON,         [DomKey.VK_QUOTE] = APOSTROPHE,
		[DomKey.VK_BACK_SLASH] = BACK_SLASH,       [DomKey.VK_RETURN] = RETURN,               [DomKey.VK_DELETE] = DELETE,
		[DomKey.VK_NUM_LOCK] = NUM_LOCK,           [DomKey.VK_DIVIDE] = PAD_SLASH,            [DomKey.VK_MULTIPLY] = PAD_MULTIPLY,
		[DomKey.VK_SUBTRACT] = PAD_MINUS,          [DomKey.VK_NUMPAD1] = PAD1,                [DomKey.VK_NUMPAD2] = PAD2,
		[DomKey.VK_NUMPAD3] = PAD3,                [DomKey.VK_NUMPAD4] = PAD4,                [DomKey.VK_NUMPAD5] = PAD5,
		[DomKey.VK_NUMPAD6] = PAD6,                [DomKey.VK_NUMPAD9] = PAD9,                [DomKey.VK_NUMPAD0] = PAD0,
		[DomKey.VK_DECIMAL] = PAD_PERIOD,          [DomKey.VK_HYPHEN_MINUS] = MINUS,          [DomKey.VK_EQUALS] = EQUALS,
		[DomKey.VK_BACK_SPACE] = BACK_SPACE,       [DomKey.VK_TAB] = TAB,                     [DomKey.VK_CAPS_LOCK] = CAPS_LOCK,
		[DomKey.VK_SHIFT] = SHIFT_L,               [DomKey.VK_CONTROL] = CONTROL_L,           [DomKey.VK_ALT] = ALT_L,
		[DomKey.VK_META] = SUPER_L,                [DomKey.VK_F1] = F1,                       [DomKey.VK_F2] = F2,
		[DomKey.VK_F3] = F3,                       [DomKey.VK_F4] = F4,                       [DomKey.VK_F5] = F5,
		[DomKey.VK_F6] = F6,                       [DomKey.VK_F7] = F7,                       [DomKey.VK_F8] = F8,
		[DomKey.VK_F9] = F9,                       [DomKey.VK_F10] = F10,                     [DomKey.VK_F11] = F11,
		[DomKey.VK_F12] = F12,                     [DomKey.VK_UP] = UP,                       [DomKey.VK_DOWN] = DOWN,
		[DomKey.VK_LEFT] = LEFT,                   [DomKey.VK_RIGHT] = RIGHT,                 [DomKey.VK_INSERT] = INSERT,
		[DomKey.VK_END] = END,                     [DomKey.VK_PAGE_UP] = PAGE_UP,             [DomKey.VK_PAGE_DOWN] = PAGE_DOWN,
		[DomKey.VK_ESCAPE] = ESCAPE,               [DomKey.VK_HOME] = HOME,                   [DomKey.VK_SCROLL_LOCK] = SCROLL_LOCK,
		[DomKey.VK_PRINTSCREEN] = PRINT_SCREEN,    [DomKey.VK_PAUSE] = PAUSE,                 [DomKey.VK_F13] = F13,
		[DomKey.VK_F14] = F14,                     [DomKey.VK_F15] = F15,                     [DomKey.VK_F16] = F16,
		[DomKey.VK_F17] = F17,                     [DomKey.VK_F18] = F18,                     [DomKey.VK_F19] = F19,
		[DomKey.VK_F20] = F20,                     [DomKey.VK_F21] = F21,                     [DomKey.VK_F22] = F22,
		[DomKey.VK_F23] = F23,                     [DomKey.VK_F24] = F24,
	};
}

fn GfxKey physical_to_mapped_key(GfxKey key) => key;

// ---- DOM virtual key codes ----
constset DomKey : inline int
{
	VK_BACK_SPACE    = 8,
	VK_TAB           = 9,
	VK_RETURN        = 13,
	VK_SHIFT         = 16,
	VK_CONTROL       = 17,
	VK_ALT           = 18,
	VK_PAUSE         = 19,
	VK_CAPS_LOCK     = 20,
	VK_ESCAPE        = 27,
	VK_SPACE         = 32,
	VK_PAGE_UP       = 33,
	VK_PAGE_DOWN     = 34,
	VK_END           = 35,
	VK_HOME          = 36,
	VK_LEFT          = 37,
	VK_UP            = 38,
	VK_RIGHT         = 39,
	VK_DOWN          = 40,
	VK_PRINTSCREEN   = 44,
	VK_INSERT        = 45,
	VK_DELETE        = 46,
	VK_0             = 48,
	VK_1             = 49,
	VK_2             = 50,
	VK_3             = 51,
	VK_4             = 52,
	VK_5             = 53,
	VK_6             = 54,
	VK_7             = 55,
	VK_8             = 56,
	VK_9             = 57,
	VK_EQUALS        = 61,
	VK_A             = 65,
	VK_B             = 66,
	VK_C             = 67,
	VK_D             = 68,
	VK_E             = 69,
	VK_F             = 70,
	VK_G             = 71,
	VK_H             = 72,
	VK_I             = 73,
	VK_J             = 74,
	VK_K             = 75,
	VK_L             = 76,
	VK_M             = 77,
	VK_N             = 78,
	VK_O             = 79,
	VK_P             = 80,
	VK_Q             = 81,
	VK_R             = 82,
	VK_S             = 83,
	VK_T             = 84,
	VK_U             = 85,
	VK_V             = 86,
	VK_W             = 87,
	VK_X             = 88,
	VK_Y             = 89,
	VK_Z             = 90,
	VK_META          = 91,
	VK_NUMPAD0       = 96,
	VK_NUMPAD1       = 97,
	VK_NUMPAD2       = 98,
	VK_NUMPAD3       = 99,
	VK_NUMPAD4       = 100,
	VK_NUMPAD5       = 101,
	VK_NUMPAD6       = 102,
	VK_NUMPAD9       = 105,
	VK_MULTIPLY      = 106,
	VK_SUBTRACT      = 109,
	VK_DECIMAL       = 110,
	VK_DIVIDE        = 111,
	VK_F1            = 112,
	VK_F2            = 113,
	VK_F3            = 114,
	VK_F4            = 115,
	VK_F5            = 116,
	VK_F6            = 117,
	VK_F7            = 118,
	VK_F8            = 119,
	VK_F9            = 120,
	VK_F10           = 121,
	VK_F11           = 122,
	VK_F12           = 123,
	VK_F13           = 124,
	VK_F14           = 125,
	VK_F15           = 126,
	VK_F16           = 127,
	VK_F17           = 128,
	VK_F18           = 129,
	VK_F19           = 130,
	VK_F20           = 131,
	VK_F21           = 132,
	VK_F22           = 133,
	VK_F23           = 134,
	VK_F24           = 135,
	VK_NUM_LOCK      = 144,
	VK_SCROLL_LOCK   = 145,
	VK_HYPHEN_MINUS  = 173,
	VK_SEMICOLON     = 186,
	VK_COMMA         = 188,
	VK_PERIOD        = 190,
	VK_SLASH         = 191,
	VK_BACK_QUOTE    = 192,
	VK_OPEN_BRACKET  = 219,
	VK_BACK_SLASH    = 220,
	VK_CLOSE_BRACKET = 221,
	VK_QUOTE         = 222,
}

module std::gfx @feat(WASM) @private;
import std::math, std::time;
import std::gfx::egl @public;
import std::gfx::wasm @public;

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

struct WgpuEmscriptenSurfaceSourceCanvasHTMLSelector
{
	WgpuChainedStruct chain;
	WgpuStringView selector;
}

fn void? _init_native(String class_name, GfxInitOption flags)
{
	_gfx.usingAsyncify = -1;
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
		_gfx.monitors.primary = node;
		_monitor_callback(_gfx.root, &node.mon, true);
	}
}

// --- exported

fn void _emscripten_on_drop(char* file, sz size) @export("c3gfx_on_drop")
{
	_data_drop_callback(_gfx.root, file, size, FILE);
}

fn void _emscripten_on_fullscreenchange(int screenW, int screenH, bool entering) @export("c3gfx_on_fullscreenchange")
{
	if (!(_gfx.root.internal.enabled_events.window_resized)) return;
	static GfxPoint original;
	if (entering)
	{
		original = _gfx.root.size;
		_gfx.root.size = { screenW, screenH };
	}
	else
	{
		_gfx.root.size = original;
	}
	gfxJsFocusCanvas();
	gfxJsSetCanvasSize(..._gfx.root.size);
	_window_resized_callback(_gfx.root, _gfx.root.size);
}

fn void _emscripten_on_mousedown(int button) @export("c3gfx_on_mousedown")
{
	if (button > 2) button += 2;
	_mouse_button_callback(_gfx.root, (GfxMouseButton)button, true);
}

fn void handle_key_event_callback(ZString code, uint codepoint, bool press) @export("c3gfx_handleKeyEvent")
{
	ZString iCode = code;
	uint hash = 0;
	while (iCode[0] != 0) { hash = ((hash ^ 0x7E057D79u) << 3) ^ (uint)iCode[0]; iCode++; }

	GfxKey physicalKey = _wasm_physical_to_gfx(hash);
	_key_callback(_gfx.root, physicalKey, _gfx.root.internal.mod, is_key_down(physicalKey) && press, press);
	if (press) _key_char_callback(_gfx.root, codepoint);
}

fn void handle_key_mods_callback(bool capital, bool numlock, bool control, bool alt, bool shift, bool super, bool scroll) @export("c3gfx_handleKeyMods")
{
	_key_update_key_mods_ex(_gfx.root, capital, numlock, control, alt, shift, super, scroll);
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

fn void gfxEmscriptenOnResize(int w, int h) @export("c3gfx_on_resize")
{
	_window_resized_callback(_gfx.root, { w, h });
}
fn void gfxEmscriptenOnFocusIn() @export("c3gfx_on_focusin") { _window_focus_callback(_gfx.root, true); }

fn void gfxEmscriptenOnFocusOut() @export("c3gfx_on_focusout") { _window_focus_callback(_gfx.root, false); }

fn void gfxEmscriptenOnMouseMove(int x, int y, int movementX, int movementY) @export("c3gfx_on_mousemove")
{
	_mouse_motion_callback(_gfx.root, { x, y });
	_raw_motion_callback(_gfx.root, { (float)movementX, (float)movementY });
}

fn void gfxEmscriptenOnMouseUp(int button) @export("c3gfx_on_mouseup")
{
	if (button > 2) button += 2;
	_mouse_button_callback(_gfx.root, (GfxMouseButton)button, false);
}

fn void gfxEmscriptenOnWheel(float deltaX, float deltaY) @export("c3gfx_on_wheel") { _mouse_scroll_callback(_gfx.root, { deltaX, deltaY }); }

fn void gfxEmscriptenOnTouch(int numTouches, int* xs, int* ys, char down) @export("c3gfx_on_touch")
{
	// down == 255 is the sentinel for touchmove (no button state change).
	if (down == 255)
	{
		if (!_gfx.root.internal.enabled_events.mouse_motion) return;
		for (int i = 0; i < numTouches; i++)
		{
			_mouse_motion_callback(_gfx.root, { xs[i], ys[i] });
			_raw_motion_callback(_gfx.root, {});
		}
		return;
	}

	if (down)
	{
		if (!_gfx.root.internal.enabled_events.mouse_button_pressed) return;
	}
	else
	{
		if (!_gfx.root.internal.enabled_events.mouse_button_released) return;
	}

	for (int i = 0; i < numTouches; i++)
	{
		_mouse_motion_callback(_gfx.root, { xs[i], ys[i] });
		_raw_motion_callback(_gfx.root, { });
		_mouse_button_callback(_gfx.root, LEFT, (bool)down);
	}
}

fn GfxKey _wasm_physical_to_gfx(uint hash)
{
	switch (hash)
	{
		case 0x67243A2Du: return ESCAPE;
		case 0x67251058u: return NUM0; case 0x67251059u: return NUM1; case 0x6725105Au: return NUM2;
		case 0x6725105Bu: return NUM3; case 0x6725105Cu: return NUM4; case 0x6725105Du: return NUM5;
		case 0x6725105Eu: return NUM6; case 0x6725105Fu: return NUM7; case 0x67251050u: return NUM8;
		case 0x67251051u: return NUM9;
		case 0x92E14DD3u: return MINUS; case 0x92E1FBACu: return EQUALS;
		case 0x36BF1CB5u: return BACK_SPACE; case 0x7B8E51E2u: return TAB;
		case 0x2C595B51u: return Q; case 0x2C595B57u: return W; case 0x2C595B45u: return E;
		case 0x2C595B52u: return R; case 0x2C595B54u: return T; case 0x2C595B59u: return Y;
		case 0x2C595B55u: return U; case 0x2C595B4Fu: return O; case 0x2C595B50u: return P;
		case 0x45D8158Cu: return CLOSE_BRACKET; case 0xDEEABF7Cu: return BRACKET;
		case 0x92E1C5D2u: return RETURN; case 0xE058958Cu: return CONTROL_L;
		case 0x2C595B41u: return A; case 0x2C595B53u: return S; case 0x2C595B44u: return D;
		case 0x2C595B46u: return F; case 0x2C595B47u: return G; case 0x2C595B48u: return H;
		case 0x2C595B4Au: return J; case 0x2C595B4Bu: return K; case 0x2C595B4Cu: return L;
		case 0x2707219Eu: return SEMICOLON; case 0x92E0B58Du: return APOSTROPHE;
		case 0x36BF358Du: return BACKTICK; case 0x26B1958Cu: return SHIFT_L;
		case 0x36BF2438u: return BACK_SLASH;
		case 0x2C595B5Au: return Z; case 0x2C595B58u: return X; case 0x2C595B43u: return C;
		case 0x2C595B56u: return V; case 0x2C595B42u: return B; case 0x2C595B4Eu: return N;
		case 0x2C595B4Du: return M;
		case 0x92E1A1C1u: return COMMA; case 0x672FFAD4u: return PERIOD; case 0x92E0A438u: return SLASH;
		case 0xC5A6BF7Cu: return SHIFT_R; case 0x5D64DA91u: return PAD_MULTIPLY;
		case 0xC914958Cu: return ALT_L; case 0x92E09CB5u: return SPACE;
		case 0xB8FAE73Bu: return CAPS_LOCK;
		case 0x7174B789u: return F1; case 0x7174B78Au: return F2; case 0x7174B78Bu: return F3;
		case 0x7174B78Cu: return F4; case 0x7174B78Du: return F5; case 0x7174B78Eu: return F6;
		case 0x7174B78Fu: return F7; case 0x7174B780u: return F8; case 0x7174B781u: return F9;
		case 0x7B8E57B0u: return F10;
		case 0xC925FCDFu: return PAD_MULTIPLY; case 0xC925FCD0u: return PAD8; case 0xC925FCD1u: return PAD9;
		case 0x5EA3E8A4u: return MINUS;
		case 0xC925FCDCu: return PAD4; case 0xC925FCDDu: return PAD5; case 0xC925FCDEu: return PAD6;
		case 0xC925FCD9u: return PAD1; case 0xC925FCDAu: return PAD2; case 0xC925FCDBu: return PAD3;
		case 0xC925FCD8u: return PAD0; case 0x95852DACu: return PERIOD;
		case 0x7B8E57B1u: return F11; case 0x7B8E57B2u: return F12;
		case 0x7B8E57B3u: return F13; case 0x7B8E57B4u: return F14; case 0x7B8E57B5u: return F15;
		case 0x7B8E57B6u: return F16; case 0x7B8E57B7u: return F17; case 0x7B8E57B8u: return F18;
		case 0x7B8E57B9u: return F19; case 0x7B8E57A8u: return F20; case 0x7B8E57A9u: return F21;
		case 0x7B8E57AAu: return F22; case 0x7B8E57ABu: return F23;
		case 0x7393FBACu: return PAD_RETURN; case 0xB88EBF7Cu: return ALT_R;
		case 0xC925873Bu: return NUM_LOCK; case 0x2C595F45u: return HOME;
		case 0xC91BB690u: return UP; case 0x672F9210u: return PAGE_UP;
		case 0x3799258Cu: return LEFT; case 0x4CE33F7Cu: return RIGHT;
		case 0x7B8E55DCu: return END; case 0x3799379Eu: return DOWN; case 0xBA90179Eu: return PAGE_DOWN;
		case 0x6723CB2Cu: return INSERT; case 0x6725C50Du: return DELETE;
		case 0x6723658Cu: return SUPER_L; case 0x39643F7Cu: return SUPER_R;
		case 0x380B9C8Cu: return PAD_PLUS;
		default: return NULL;
	}
}


fn void _wait_for_event_native(Duration duration) {}
