module std::gfx::wasm @feat(WASM) @private;
import std::math, std::time;
import std::gfx::egl @public;

// --- Gfx

fn void? init_platform(String class_name, GfxInitOption flags)
{
	gfx::_gfx.using_asyncify = -1;
	GfxMonitor monitor;
	monitor.origin = {};
	monitor.pixel_ratio = (float)gfx::gfxJsDevicePixelRatio();
	monitor.mode.size = { gfx::gfxJsInnerWidth(), gfx::gfxJsInnerHeight() };
	monitor.phys_size = {
		(float)(int)(monitor.mode.size.x * monitor.pixel_ratio + 0.5f),
		(float)(int)(monitor.mode.size.y * monitor.pixel_ratio + 0.5f)
	};

	float dpi = 96.0f * monitor.pixel_ratio;
	monitor.scale = (Vec2)(dpi / 96.0f);

	gfx::_split_bpp(32, &monitor.mode);
	monitor.mode.refresh_rate = 0;

	GfxMonitorNode* node = gfx::_monitor_add(&monitor);
	gfx::_gfx.monitors.primary = node;
	gfx::_monitor_callback(gfx::_gfx.root, &node.mon, true);
}

fn void destroy_platform() {}

fn void poll_events_platform()
{
	if (gfx::_gfx.using_asyncify == -1) gfx::_gfx.using_asyncify = gfx::gfxJsUsingAsyncify();
	gfx::_reset_prev_state();
	if (gfx::_gfx.using_asyncify) gfx::gfxJsSleep(0);
	if (gfx::_pending_resize_w > 0 && gfx::_pending_resize_h > 0)
	{
		gfx::emscripten_set_canvas_element_size(gfx::_wasm_target(), gfx::_pending_resize_w, gfx::_pending_resize_h);
		gfx::_pending_resize_w = 0;
		gfx::_pending_resize_h = 0;
	}
}

fn void _wait_platform(Duration duration)
{
	poll_events_platform();
}


// --- GfxMonitor functions

fn GfxGammaRamp? monitor_get_gamma_ramp(GfxMonitor* monitor, Allocator a) => NOT_IMPLEMENTED~;

fn GfxMonitorMode[] monitor_get_modes(GfxMonitor* mon) => {};

fn GfxRect monitor_get_workarea(GfxMonitor* monitor) => { -1, -1, -1, -1 };

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
	int min_h = win.height < surface.size.y ? win.height : surface.size.y;
	gfx::copy_image_data(surface.data, surface.size.x, min_h, RGBA8, surface.data, surface.format, surface.convert_fn);
	gfx::gfxJsBlitSurface(surface.data, surface.size.x, surface.size.y, min_h);
}

fn void? window_create_platform(GfxWindow* win, String name, GfxWindowFlags flags)
{
	win.src.window = (void*)((name.len > 0 && name[0] == '#') ? (ZString)name : gfx::DEFAULT_CANVAS_TARGET);

	if (flags.maximize)
	{
		win.width = gfx::gfxJsInnerWidth();
		win.height = gfx::gfxJsInnerHeight();
	}

	gfx::gfxJsSetCanvasSize(win, win.width, win.height);
	gfx::gfxJsSetWindowTitle((ZString)name, name.len);
	gfx::gfxJsSetupWindowListeners(win);

	if (flags.allow_dnd) win.internal.flags.allow_dnd = true;

	gfx::gfxJsSetupDropListeners(win);

	gfx::_window_set_flags(win, flags, {});
	win.src.backing = {};
	if (flags.open_gl) win.create_opengl_context(gfx::get_global_opengl_hints())!;
	if (flags.egl) egl::create_context(win, gfx::get_global_opengl_hints())!;
}

fn void window_destroy_platform(GfxWindow* win)
{
	gfx::gfxJsRemoveWindowListeners(win);
}

fn GfxPoint window_fetch_size(GfxWindow* win)
{
	return win.get_size();
}

fn GfxMonitor*? window_find_monitor(GfxWindow* win) => gfx::get_primary_monitor();

fn void window_flash(GfxWindow* win, GfxFlashRequest request) {}

fn void window_focus(GfxWindow* win)
{
	gfx::gfxJsFocusCanvas(win);
}

fn void window_hide(GfxWindow* win) {}

fn bool window_is_floating(GfxWindow* win) => false;

fn bool window_is_hidden(GfxWindow* win) => false;

fn bool window_is_maximized(GfxWindow* win) => win.internal.flags.maximize;

fn bool window_is_minimized(GfxWindow* win) => false;

fn void window_maximize_platform(GfxWindow* win)
{
	int w = gfx::gfxJsInnerWidth();
	int h = gfx::gfxJsInnerHeight();
	gfx::gfxJsSetCanvasSize(win, w, h);
	gfx::_window_resized_callback(win, { w, h });
}

fn void window_minimize_platform(GfxWindow* win) {}

fn void window_move_mouse(GfxWindow* win, GfxPoint where) {}

fn void window_move_platform(GfxWindow* win, GfxPoint where) {}

fn void window_raise(GfxWindow* win) {}

fn void window_resize_platform(GfxWindow* win, GfxSize size)
{
	gfx::gfxJsSetCanvasSize(win, ...size);
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

fn void window_set_fullscreen_platform(GfxWindow* win, bool fullscreen)
{
	if (fullscreen)
	{
		gfx::gfxJsRequestFullscreen();
		return;
	}
	gfx::gfxJsExitFullscreen();
}

fn bool window_set_icon(GfxWindow* win, void* data, int w, int h, GfxFormat format, GfxWindowIconType type) => false;

fn void window_set_max_size(GfxWindow* win, GfxSize size) {}

fn void window_set_min_size(GfxWindow* win, GfxSize size) {}

fn void window_set_mouse_passthrough(GfxWindow* win, bool passthrough)
{
	gfx::gfxJsSetPointerEvents(win, passthrough);
}

fn void window_set_mouse_platform(GfxWindow* win, GfxMouse* mouse)
{
	ZString name = (ZString)mouse;
	gfx::gfxJsSetCursor(win, name, name.len());
}

fn void window_set_name(GfxWindow* win, String name)
{
	gfx::gfxJsSetWindowTitle((ZString)name, name.len);
}

fn void window_set_opacity(GfxWindow* win, float opacity)
{
	gfx::gfxJsSetOpacity(win, opacity);
}

fn void window_set_raw_mouse_mode_platform(GfxWindow* win, bool state) {}

fn void window_show_platform(GfxWindow* win) {}

// Window - opengl

fn void? window_init_opengl_context_platform(GfxWindow* win, GfxGlContext* ctx, GfxGlHints* hints)
{
	win.src.ctx.native = ctx;
	win.src.backing = { .opengl };
	int handle = gfx::gfxJsWebglCreateContext(win, hints);
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

fn void window_set_opengl_swap_interval(GfxWindow* win, int swap_interval) {}

// Window WGPU

fn WGPUSurface? window_create_wgpu_surface(GfxWindow* window, WGPUInstance instance)
{
	WgpuEmscriptenSurfaceSourceCanvasHTMLSelector canvas_desc;
	canvas_desc.chain.next = null;
	canvas_desc.chain.sType = gfx::WGPU_STYPE_EMSCRIPTEN_SURFACE_SOURCE_CANVAS_HTML_SELECTOR;
	ZString target = gfx::_wasm_target(window);
	canvas_desc.selector.data = target;
	canvas_desc.selector.length = target.len();
	WgpuSurfaceDescriptor surface_desc;
	surface_desc.nextInChain = (WgpuChainedStruct*)&canvas_desc;
	return gfx::wgpuInstanceCreateSurface(instance, &surface_desc) ?: gfx::SURFACE_CREATION_FAILED~;
}

// --- Clipboard

fn GfxDataTransfer? read_clipboard(Allocator allocator, GfxDataTransferType requested_type) => NOT_IMPLEMENTED~;

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

fn bool get_vulkan_physical_device_presentation_support(VkInstance instance, VkPhysicalDevice physical_device, uint queue_family_index) => false;

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

const ZString DEFAULT_CANVAS_TARGET = "#canvas";

fn ZString _wasm_target(GfxWindow* win = null)
{
	if (win && win.src.window) return (ZString)win.src.window;
	if (_gfx.root && _gfx.root.src.window) return (ZString)_gfx.root.src.window;
	return DEFAULT_CANVAS_TARGET;
}

int _pending_resize_w;
int _pending_resize_h;
int _global_mouse_x;
int _global_mouse_y;

fn void _window_show_mouse_native(GfxWindow* win, bool show)
{
	_window_show_mouse_flags(win, show);
	if (show)
	{
		win.set_mouse_default();
	}
	else
	{
		gfxJsHideCursor(win);
	}
}

struct WgpuEmscriptenSurfaceSourceCanvasHTMLSelector
{
	WgpuChainedStruct chain;
	WgpuStringView selector;
}


// --- exported

fn void _emscripten_on_drop(char* file, sz size) @export("c3gfx_on_drop")
{
	_data_drop_callback(_gfx.root, file[:size], FILE);
}

fn void _emscripten_on_fullscreenchange(int screen_w, int screen_h, bool entering) @export("c3gfx_on_fullscreenchange")
{
	if (!(_gfx.root.internal.enabled_events.window_resized)) return;
	static GfxPoint original;
	if (entering)
	{
		original = _gfx.root.size;
		_gfx.root.size = { screen_w, screen_h };
	}
	else
	{
		_gfx.root.size = original;
	}
	gfxJsFocusCanvas();
	gfxJsSetCanvasSize(_gfx.root, ..._gfx.root.size);
	_window_resized_callback(_gfx.root, _gfx.root.size);
}

fn void _emscripten_on_mousedown(int button) @export("c3gfx_on_mousedown")
{
	if (button > 2) button += 2;
	_mouse_button_callback(_gfx.root, (GfxMouseButton)button, true);
}

fn void handle_key_event_callback(ZString code, uint codepoint, bool press) @export("c3gfx_handleKeyEvent")
{
	ZString i_code = code;
	uint hash = 0;
	while (i_code[0] != 0) { hash = ((hash ^ 0x7E057D79u) << 3) ^ (uint)i_code[0]; i_code++; }

	GfxKey physical_key = _wasm_physical_to_gfx(hash);
	_key_callback(_gfx.root, physical_key, _gfx.root.internal.mod, is_key_down(physical_key) && press, press);
	if (press) _key_char_callback(_gfx.root, codepoint);
}

fn void handle_key_mods_callback(bool capital, bool numlock, bool control, bool alt, bool shift, bool super, bool scroll) @export("c3gfx_handleKeyMods")
{
	_key_update_key_mods_ex(_gfx.root, capital, numlock, control, alt, shift, super, scroll);
}

// ---

// Real Emscripten C HTML5 APIs
extern fn int emscripten_set_canvas_element_size(ZString target, int width, int height) @cname("emscripten_set_canvas_element_size");
extern fn void emscripten_set_window_title(ZString title) @cname("emscripten_set_window_title");
extern fn int emscripten_set_resize_callback_on_thread(ZString target, void* userData, int useCapture, void* callback, void* targetThread) @cname("emscripten_set_resize_callback_on_thread");
extern fn int emscripten_set_fullscreenchange_callback_on_thread(ZString target, void* userData, int useCapture, void* callback, void* targetThread) @cname("emscripten_set_fullscreenchange_callback_on_thread");
extern fn int emscripten_set_mousemove_callback_on_thread(ZString target, void* userData, int useCapture, void* callback, void* targetThread) @cname("emscripten_set_mousemove_callback_on_thread");
extern fn int emscripten_set_mousedown_callback_on_thread(ZString target, void* userData, int useCapture, void* callback, void* targetThread) @cname("emscripten_set_mousedown_callback_on_thread");
extern fn int emscripten_set_mouseup_callback_on_thread(ZString target, void* userData, int useCapture, void* callback, void* targetThread) @cname("emscripten_set_mouseup_callback_on_thread");
extern fn int emscripten_set_wheel_callback_on_thread(ZString target, void* userData, int useCapture, void* callback, void* targetThread) @cname("emscripten_set_wheel_callback_on_thread");
extern fn int emscripten_set_focusin_callback_on_thread(ZString target, void* userData, int useCapture, void* callback, void* targetThread) @cname("emscripten_set_focusin_callback_on_thread");
extern fn int emscripten_set_focusout_callback_on_thread(ZString target, void* userData, int useCapture, void* callback, void* targetThread) @cname("emscripten_set_focusout_callback_on_thread");
extern fn int emscripten_set_touchstart_callback_on_thread(ZString target, void* userData, int useCapture, void* callback, void* targetThread) @cname("emscripten_set_touchstart_callback_on_thread");
extern fn int emscripten_set_touchmove_callback_on_thread(ZString target, void* userData, int useCapture, void* callback, void* targetThread) @cname("emscripten_set_touchmove_callback_on_thread");
extern fn int emscripten_set_touchend_callback_on_thread(ZString target, void* userData, int useCapture, void* callback, void* targetThread) @cname("emscripten_set_touchend_callback_on_thread");
extern fn int emscripten_set_touchcancel_callback_on_thread(ZString target, void* userData, int useCapture, void* callback, void* targetThread) @cname("emscripten_set_touchcancel_callback_on_thread");
extern fn int emscripten_set_keydown_callback_on_thread(ZString target, void* userData, int useCapture, void* callback, void* targetThread) @cname("emscripten_set_keydown_callback_on_thread");
extern fn int emscripten_set_keyup_callback_on_thread(ZString target, void* userData, int useCapture, void* callback, void* targetThread) @cname("emscripten_set_keyup_callback_on_thread");

extern fn int emscripten_request_pointerlock(ZString target, int defer_event) @cname("emscripten_request_pointerlock");
extern fn int emscripten_exit_pointerlock() @cname("emscripten_exit_pointerlock");
extern fn void emscripten_sleep(uint ms) @cname("emscripten_sleep");

extern fn int emscripten_webgl_make_context_current(int context) @cname("emscripten_webgl_make_context_current");
extern fn int emscripten_webgl_destroy_context(int context) @cname("emscripten_webgl_destroy_context");
extern fn int emscripten_webgl_commit_frame() @cname("emscripten_webgl_commit_frame");
extern fn int emscripten_webgl_get_current_context() @cname("emscripten_webgl_get_current_context");
extern fn void* emscripten_webgl_get_proc_address(ZString name) @cname("emscripten_webgl_get_proc_address");

struct EmscriptenWebGLContextAttributes
{
	bool alpha;
	bool depth;
	bool stencil;
	bool antialias;
	bool premultipliedAlpha;
	bool preserveDrawingBuffer;
	int powerPreference;
	bool failIfMajorPerformanceCaveat;
	int majorVersion;
	int minorVersion;
	bool enableExtensionsByDefault;
	bool explicitSwapControl;
	int proxyContextToMainThread;
	bool renderViaOffscreenBackBuffer;
	bool desynchronized;
}

extern fn void emscripten_webgl_init_context_attributes(EmscriptenWebGLContextAttributes* attrs) @cname("emscripten_webgl_init_context_attributes");
extern fn int emscripten_webgl_create_context(ZString target, EmscriptenWebGLContextAttributes* attrs) @cname("emscripten_webgl_create_context");

// Emscripten libc script evaluation (direct C equivalent of EM_ASM)
extern fn void emscripten_run_script(ZString script) @cname("emscripten_run_script");
extern fn int emscripten_run_script_int(ZString script) @cname("emscripten_run_script_int");

// RGFW EM_ASM macros ported directly to emscripten_run_script
fn double gfxJsDevicePixelRatio() => (double)emscripten_run_script_int("window.devicePixelRatio || 1;");
fn int gfxJsInnerWidth() => emscripten_run_script_int("window.innerWidth || 0;");
fn int gfxJsInnerHeight() => emscripten_run_script_int("window.innerHeight || 0;");
fn int gfxJsUsingAsyncify() => emscripten_run_script_int("'Asyncify' in Module;");

fn void gfxJsSetCanvasSize(GfxWindow* win, int w, int h) => (void)emscripten_set_canvas_element_size(_wasm_target(win), w, h);
fn void gfxJsSetWindowTitle(ZString name, sz len) => emscripten_set_window_title(name);
fn void gfxJsSleep(int ms) => emscripten_sleep((uint)ms);
fn void gfxJsFocusCanvas(GfxWindow* win = null) => emscripten_run_script(string::tformat_zstr("var el = document.querySelector('%s') || Module.canvas; if (el) el.focus();", _wasm_target(win)));
fn void gfxJsSetCursor(GfxWindow* win, ZString name, sz len) => emscripten_run_script(string::tformat_zstr("var el = document.querySelector('%s') || Module.canvas; if (el) el.style.cursor = '%s';", _wasm_target(win), name));
fn void gfxJsHideCursor(GfxWindow* win = null) => emscripten_run_script(string::tformat_zstr("var el = document.querySelector('%s') || Module.canvas; if (el) el.style.cursor = 'none';", _wasm_target(win)));
fn void gfxJsRequestPointerLock(GfxWindow* win = null) => (void)emscripten_request_pointerlock(_wasm_target(win), 1);
fn void gfxJsExitPointerLock() => (void)emscripten_exit_pointerlock();
fn void gfxJsRequestFullscreen() => emscripten_run_script("Module.requestFullscreen(false, true);");
fn void gfxJsExitFullscreen() => emscripten_run_script("Module.exitFullscreen(false, true);");
fn void gfxJsSetOpacity(GfxWindow* win, float opacity) => emscripten_run_script(string::tformat_zstr("var el = document.querySelector('%s') || Module.canvas; if (el) el.style.opacity = %f;", _wasm_target(win), opacity));

fn int gfxJsWebglCreateContext(GfxWindow* win, GfxGlHints* hints)
{
	EmscriptenWebGLContextAttributes attrs;
	emscripten_webgl_init_context_attributes(&attrs);
	attrs.majorVersion = (hints && hints.major == 2) ? 1 : 2;
	attrs.depth = hints ? hints.depth > 0 : true;
	attrs.stencil = hints ? hints.stencil > 0 : true;
	attrs.antialias = hints ? hints.samples > 0 : true;
	attrs.explicitSwapControl = false;
	return emscripten_webgl_create_context(_wasm_target(win), &attrs);
}
fn void gfxJsWebglDestroyContext(int ctx) => (void)emscripten_webgl_destroy_context(ctx);
fn void gfxJsWebglMakeCurrent(int ctx) => (void)emscripten_webgl_make_context_current(ctx);
fn void gfxJsWebglCommitFrame() => (void)emscripten_webgl_commit_frame();
fn int gfxJsWebglGetCurrentContext() => emscripten_webgl_get_current_context();
fn int gfxJsExtensionSupported(ZString name, sz len) => 0;
fn void* gfxJsGetProcAddress(ZString name) => emscripten_webgl_get_proc_address(name);

fn void gfxJsBlitSurface(char* data, int w, int h, int min_h) {}
fn int gfxJsGetMouseX() => _global_mouse_x;
fn int gfxJsGetMouseY() => _global_mouse_y;
fn void gfxJsSetPointerEvents(GfxWindow* win, bool passthrough)
{
	emscripten_run_script(string::tformat_zstr(
		"var el = document.querySelector('%s') || Module.canvas; if (el) el.style.pointerEvents = %s;",
		_wasm_target(win),
		passthrough ? "'none'" : "'auto'"
	));
}
fn void gfxJsWriteClipboard(char* data, sz len) {}

// RGFW's RGFW_createWindowPlatform callback loader
fn void gfxJsSetupWindowListeners(GfxWindow* win)
{
	void* thread_ctx = (void*)2; // EM_CALLBACK_THREAD_CONTEXT_CALLING_THREAD
	ZString target_doc = (ZString)(uptr)1; // EMSCRIPTEN_EVENT_TARGET_DOCUMENT
	ZString target_win = (ZString)(uptr)2; // EMSCRIPTEN_EVENT_TARGET_WINDOW
	ZString target_canvas = _wasm_target(win);

	emscripten_set_resize_callback_on_thread(target_win, null, 0, &c3_on_resize, thread_ctx);
	emscripten_set_fullscreenchange_callback_on_thread(target_doc, null, 0, &c3_on_fullscreenchange, thread_ctx);
	emscripten_set_mousemove_callback_on_thread(target_canvas, null, 0, &c3_on_mousemove, thread_ctx);
	emscripten_set_touchstart_callback_on_thread(target_canvas, null, 0, &c3_on_touchstart, thread_ctx);
	emscripten_set_touchend_callback_on_thread(target_canvas, null, 0, &c3_on_touchend, thread_ctx);
	emscripten_set_touchmove_callback_on_thread(target_canvas, null, 0, &c3_on_touchmove, thread_ctx);
	emscripten_set_touchcancel_callback_on_thread(target_canvas, null, 0, &c3_on_touchcancel, thread_ctx);
	emscripten_set_mousedown_callback_on_thread(target_canvas, null, 0, &c3_on_mousedown, thread_ctx);
	emscripten_set_mouseup_callback_on_thread(target_win, null, 0, &c3_on_mouseup, thread_ctx);
	emscripten_set_wheel_callback_on_thread(target_canvas, null, 0, &c3_on_wheel, thread_ctx);
	emscripten_set_focusin_callback_on_thread(target_win, null, 0, &c3_on_focusin, thread_ctx);
	emscripten_set_focusout_callback_on_thread(target_win, null, 0, &c3_on_focusout, thread_ctx);
	emscripten_set_keydown_callback_on_thread(target_win, null, 1, &c3_on_keydown, thread_ctx);
	emscripten_set_keyup_callback_on_thread(target_win, null, 1, &c3_on_keyup, thread_ctx);
}

fn void gfxJsRemoveWindowListeners(GfxWindow* win = null)
{
	void* thread_ctx = (void*)2;
	ZString target_doc = (ZString)(uptr)1;
	ZString target_win = (ZString)(uptr)2;
	ZString target_canvas = _wasm_target(win);

	emscripten_set_resize_callback_on_thread(target_win, null, 0, null, thread_ctx);
	emscripten_set_fullscreenchange_callback_on_thread(target_doc, null, 0, null, thread_ctx);
	emscripten_set_mousemove_callback_on_thread(target_canvas, null, 0, null, thread_ctx);
	emscripten_set_touchstart_callback_on_thread(target_canvas, null, 0, null, thread_ctx);
	emscripten_set_touchend_callback_on_thread(target_canvas, null, 0, null, thread_ctx);
	emscripten_set_touchmove_callback_on_thread(target_canvas, null, 0, null, thread_ctx);
	emscripten_set_touchcancel_callback_on_thread(target_canvas, null, 0, null, thread_ctx);
	emscripten_set_mousedown_callback_on_thread(target_canvas, null, 0, null, thread_ctx);
	emscripten_set_mouseup_callback_on_thread(target_win, null, 0, null, thread_ctx);
	emscripten_set_wheel_callback_on_thread(target_canvas, null, 0, null, thread_ctx);
	emscripten_set_focusin_callback_on_thread(target_win, null, 0, null, thread_ctx);
	emscripten_set_focusout_callback_on_thread(target_win, null, 0, null, thread_ctx);
	emscripten_set_keydown_callback_on_thread(target_win, null, 1, null, thread_ctx);
	emscripten_set_keyup_callback_on_thread(target_win, null, 1, null, thread_ctx);
}

// Direct port of RGFW's drop listener EM_ASM (RGFW.h)
fn void gfxJsSetupDropListeners(GfxWindow* win = null)
{
	emscripten_run_script(string::tformat_zstr(
		"var canvas = document.querySelector('%s') || Module.canvas;"
		"if (canvas) {"
		"  canvas.addEventListener('dragover', (e) => { e.preventDefault(); return false; }, true);"
		"}",
		_wasm_target(win)
	));
}

// Emscripten HTML5 C callback trampolines (RGFW.h)
struct EmscriptenUiEvent { int detail; int documentBodyClientWidth; int documentBodyClientHeight; int windowInnerWidth; int windowInnerHeight; int windowOuterWidth; int windowOuterHeight; int scrollTop; int scrollLeft; }
struct EmscriptenMouseEvent
{
	double timestamp;
	int screenX;
	int screenY;
	int clientX;
	int clientY;
	bool ctrlKey;
	bool shiftKey;
	bool altKey;
	bool metaKey;
	ushort button;
	ushort buttons;
	int movementX;
	int movementY;
	int targetX;
	int targetY;
	int canvasOffsetX;
	int canvasOffsetY;
	int padding;
}

struct EmscriptenWheelEvent
{
	EmscriptenMouseEvent mouse;
	double deltaX;
	double deltaY;
	double deltaZ;
	uint deltaMode;
}

struct EmscriptenFocusEvent
{
	char[128] nodeName;
	char[128] id;
}

struct EmscriptenKeyboardEvent
{
	double timestamp;
	uint location;
	bool ctrlKey;
	bool shiftKey;
	bool altKey;
	bool metaKey;
	bool repeat;
	uint charCode;
	uint keyCode;
	uint which;
	char[32] key;
	char[32] code;
	char[32] charValue;
	char[32] locale;
}

struct EmscriptenTouchPoint
{
	int identifier;
	int screenX;
	int screenY;
	int clientX;
	int clientY;
	int pageX;
	int pageY;
	bool isChanged;
	bool onTarget;
	int targetX;
	int targetY;
}

struct EmscriptenTouchEvent
{
	double timestamp;
	int numTouches;
	bool ctrlKey;
	bool shiftKey;
	bool altKey;
	bool metaKey;
	EmscriptenTouchPoint[32] touches;
}

struct EmscriptenFullscreenChangeEvent
{
	bool isFullscreen;
	bool fullscreenEnabled;
	char[128] nodeName;
	char[128] id;
	int elementWidth;
	int elementHeight;
	int screenWidth;
	int screenHeight;
}

extern fn int emscripten_get_canvas_element_size(ZString target, int* width, int* height) @cname("emscripten_get_canvas_element_size");

fn int c3_on_resize(int event_type, EmscriptenUiEvent* e, void* user_data)
{
	if (!_gfx.root) return 1;
	int w;
	int h;
	if (_gfx.root.internal.flags.maximize)
	{
		w = e.windowInnerWidth;
		h = e.windowInnerHeight;
		_pending_resize_w = w;
		_pending_resize_h = h;
	}
	else
	{
		emscripten_get_canvas_element_size(_wasm_target(), &w, &h);
	}
	if (w > 0 && h > 0)
	{
		_window_resized_callback(_gfx.root, { w, h });
	}
	return 1;
}

fn int c3_on_fullscreenchange(int event_type, EmscriptenFullscreenChangeEvent* e, void* user_data)
{
	gfxJsFocusCanvas();
	emscripten_set_canvas_element_size(_wasm_target(), e.screenWidth, e.screenHeight);
	_window_resized_callback(_gfx.root, { e.screenWidth, e.screenHeight });
	return 1;
}

fn int c3_on_focusin(int event_type, EmscriptenFocusEvent* e, void* user_data)
{
	_window_focus_callback(_gfx.root, true);
	return 1;
}

fn int c3_on_focusout(int event_type, EmscriptenFocusEvent* e, void* user_data)
{
	_window_focus_callback(_gfx.root, false);
	return 1;
}

extern fn int emscripten_get_element_css_size(ZString target, double* width, double* height) @cname("emscripten_get_element_css_size");

fn GfxPoint _wasm_pos(int target_x, int target_y)
{
	double cw = 0;
	double ch = 0;
	emscripten_get_element_css_size(_wasm_target(), &cw, &ch);

	int x = target_x;
	int y = target_y;
	if (cw > 0.0 && ch > 0.0 && _gfx.root)
	{
		x = (int)((double)target_x * (double)_gfx.root.width / cw);
		y = (int)((double)target_y * (double)_gfx.root.height / ch);
	}
	return { x, y };
}

fn GfxPoint _wasm_mouse_pos(EmscriptenMouseEvent* e)
{
	_global_mouse_x = e.screenX;
	_global_mouse_y = e.screenY;
	return _wasm_pos(e.targetX, e.targetY);
}

fn int c3_on_mousemove(int event_type, EmscriptenMouseEvent* e, void* user_data)
{
	GfxPoint pos = _wasm_mouse_pos(e);
	_mouse_motion_callback(_gfx.root, pos);
	_raw_motion_callback(_gfx.root, { (float)e.movementX, (float)e.movementY });
	return 1;
}

fn int c3_on_mousedown(int event_type, EmscriptenMouseEvent* e, void* user_data)
{
	GfxPoint pos = _wasm_mouse_pos(e);
	_mouse_motion_callback(_gfx.root, pos);
	int b = (int)e.button;
	if (b > 2) b += 2;
	_mouse_button_callback(_gfx.root, (GfxMouseButton)b, true);
	return 1;
}

fn int c3_on_mouseup(int event_type, EmscriptenMouseEvent* e, void* user_data)
{
	GfxPoint pos = _wasm_mouse_pos(e);
	_mouse_motion_callback(_gfx.root, pos);
	int b = (int)e.button;
	if (b > 2) b += 2;
	_mouse_button_callback(_gfx.root, (GfxMouseButton)b, false);
	return 1;
}

fn int c3_on_wheel(int event_type, EmscriptenWheelEvent* e, void* user_data)
{
	_mouse_scroll_callback(_gfx.root, { (float)e.deltaX, -(float)e.deltaY });
	return 1;
}

fn int c3_on_keydown(int event_type, EmscriptenKeyboardEvent* e, void* user_data)
{
	_key_update_key_mods_ex(_gfx.root, false, false, e.ctrlKey, e.altKey, e.shiftKey, e.metaKey, false);
	uint codepoint = (uint)e.key[0];
	if (codepoint < 0x7f && e.key[1] != 0) codepoint = 0;
	handle_key_event_callback((ZString)&e.code, codepoint, true);
	return 1;
}

fn int c3_on_keyup(int event_type, EmscriptenKeyboardEvent* e, void* user_data)
{
	_key_update_key_mods_ex(_gfx.root, false, false, e.ctrlKey, e.altKey, e.shiftKey, e.metaKey, false);
	handle_key_event_callback((ZString)&e.code, 0, false);
	return 1;
}

fn int c3_on_touchstart(int event_type, EmscriptenTouchEvent* e, void* user_data)
{
	for (int i = 0; i < e.numTouches; i++)
	{
		_mouse_motion_callback(_gfx.root, _wasm_pos(e.touches[i].targetX, e.touches[i].targetY));
		_mouse_button_callback(_gfx.root, LEFT, true);
	}
	return 1;
}

fn int c3_on_touchmove(int event_type, EmscriptenTouchEvent* e, void* user_data)
{
	for (int i = 0; i < e.numTouches; i++)
	{
		_mouse_motion_callback(_gfx.root, _wasm_pos(e.touches[i].targetX, e.touches[i].targetY));
	}
	return 1;
}

fn int c3_on_touchend(int event_type, EmscriptenTouchEvent* e, void* user_data)
{
	for (int i = 0; i < e.numTouches; i++)
	{
		_mouse_button_callback(_gfx.root, LEFT, false);
	}
	return 1;
}

fn int c3_on_touchcancel(int event_type, EmscriptenTouchEvent* e, void* user_data)
{
	for (int i = 0; i < e.numTouches; i++)
	{
		_mouse_button_callback(_gfx.root, LEFT, false);
	}
	return 1;
}

fn void gfxEmscriptenOnResize(int w, int h) @export("c3gfx_on_resize")
{
	_window_resized_callback(_gfx.root, { w, h });
}
fn void gfxEmscriptenOnFocusIn() @export("c3gfx_on_focusin") { _window_focus_callback(_gfx.root, true); }

fn void gfxEmscriptenOnFocusOut() @export("c3gfx_on_focusout") { _window_focus_callback(_gfx.root, false); }

fn void gfxEmscriptenOnMouseMove(int x, int y, int movement_x, int movement_y) @export("c3gfx_on_mousemove")
{
	_mouse_motion_callback(_gfx.root, { x, y });
	_raw_motion_callback(_gfx.root, { (float)movement_x, (float)movement_y });
}

fn void gfxEmscriptenOnMouseUp(int button) @export("c3gfx_on_mouseup")
{
	if (button > 2) button += 2;
	_mouse_button_callback(_gfx.root, (GfxMouseButton)button, false);
}

fn void gfxEmscriptenOnWheel(float delta_x, float delta_y) @export("c3gfx_on_wheel") { _mouse_scroll_callback(_gfx.root, { delta_x, delta_y }); }

fn void gfxEmscriptenOnTouch(int num_touches, int* xs, int* ys, char down) @export("c3gfx_on_touch")
{
	// down == 255 is the sentinel for touchmove (no button state change).
	if (down == 255)
	{
		if (!_gfx.root.internal.enabled_events.mouse_motion) return;
		for (int i = 0; i < num_touches; i++)
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

	for (int i = 0; i < num_touches; i++)
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


