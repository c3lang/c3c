module std::gfx @feat(WASM);
import std::math;
// C3 port of the WASM/WebGL RGFW backend from /Users/lerno/Downloads/AGFW.h (RGFW).
// Mechanical, line-by-line style translation of the RGFW_WASM implementation.
// Standalone alternative to rgfw.c3 (macOS) / rgfw_x11.c3 / rgfw_wayland.c3 /
// rgfw_windows.c3 - only ONE rgfw*.c3 file should be included for a given
// target, matching how the original C header only ever compiles one #ifdef
// backend at a time.
//
// The original relies on Emscripten's EM_ASM (inline JS) and emscripten_*
// runtime; C3's wasm target has no Emscripten runtime, so every DOM/JS touch
// point here is a named `extern fn ... @wasm("env", "name")` import that a
// small JS host must implement (canvas sizing, cursor style, fullscreen,
// clipboard, WebGL context, DOM event wiring), and the functions RGFW expects
// JS to call back into (key events, drag-drop) are marked @export so the host
// can find them.

extern fn char* strstr(char* haystack, char* needle);
extern fn int clock_gettime(int clock_id, Timespec* ts);

struct Timespec { long tv_sec; CLong tv_nsec; }


struct RgfwGlContext { void* ctx; void* format; }
struct RgfwEglContext { void* ctx; void* surface; void* eglWindow; }

const RgfwGlReleaseBehavior RGFW_GL_RELEASE_FLUSH = 0;
const RgfwGlReleaseBehavior RGFW_GL_RELEASE_NONE = 1;

const RgfwGlProfile RGFW_GL_CORE = 0;
const RgfwGlProfile RGFW_GL_FORWARD_COMPATIBILITY = 1;
const RgfwGlProfile RGFW_GL_COMPATIBILITY = 2;
const RgfwGlProfile RGFW_GL_ES = 3;
const RgfwGlProfile RGFW_GL_WEB = 4;

const RgfwGlRenderer RGFW_GL_ACCELERATED = 0;
const RgfwGlRenderer RGFW_GL_SOFTWARE = 1;

struct RgfwGlHints
{
	int stencil;
	int samples;
	int stereo;
	int auxBuffers;
	int doubleBuffer;
	int red, green, blue, alpha;
	int depth;
	int accumRed, accumGreen, accumBlue, accumAlpha;
	bool sRGB;
	bool robustness;
	bool debug;
	bool noError;
	RgfwGlReleaseBehavior releaseBehavior;
	RgfwGlProfile profile;
	int major, minor;
	RgfwGlContext* share;
	RgfwEglContext* shareEGL;
	RgfwGlRenderer renderer;
}

union RgfwGfxContext
{
	RgfwGlContext* native;
	RgfwEglContext* egl;
}

const RgfwGfxContextType RGFW_GFX_NATIVE_OPEN_GL = 1;
const RgfwGfxContextType RGFW_GFX_EGL = 2;
const RgfwGfxContextType RGFW_GFX_OWNED_BY_RGFW = 4;

struct GfxNativeImage
{
	GfxFormat format;
	char* buffer;
	void* native0;
	void* native1;
	void* native2;
}

struct RgfwWindowSrc
{
	inline IntRect bounds;
	void* window;
	void* view;
	void* mouse;
	void* delegate;
	RgfwGfxContext ctx;
	RgfwGfxContextType gfxType;
}

// public
fn bool rgfw_loadGL() => true;
// public
fn bool rgfw_getPhysicalDevicePresentationSupport_Vulkan(VkInstance instance, VkPhysicalDevice physicalDevice, uint queueFamilyIndex) => false;

alias WGPUInstance = void*;
alias WGPUSurface = void*;
alias WGPUSType = int;
const WGPUSType WGPU_STYPE_EMSCRIPTEN_SURFACE_SOURCE_CANVAS_HTML_SELECTOR = 0x00040000;
struct WgpuChainedStruct { WgpuChainedStruct* next; WGPUSType sType; }
struct WgpuStringView { char* data; sz length; }
struct WgpuSurfaceDescriptor { WgpuChainedStruct* nextInChain; }
struct WgpuEmscriptenSurfaceSourceCanvasHTMLSelector { WgpuChainedStruct chain; WgpuStringView selector; }
extern fn WGPUSurface wgpuInstanceCreateSurface(WGPUInstance instance, WgpuSurfaceDescriptor* descriptor) @wasm("env", "wgpuInstanceCreateSurface");

// public
fn WGPUSurface rgfw_window_createSurface_WebGPU(GfxWindow* window, WGPUInstance instance)
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


fn void _window_set_aspect_ratio(GfxWindow* win, int w, int h) @private {}

// ---- JS host imports (no Emscripten runtime under C3's wasm target) ----
extern fn void rgfwJsSetCanvasSize(int w, int h) @wasm("env", "rgfw_set_canvas_size");
extern fn void rgfwJsSetWindowTitle(ZString name, sz len) @wasm("env", "rgfw_set_window_title");
extern fn void rgfwJsSetupWindowListeners() @wasm("env", "rgfw_setup_window_listeners");
extern fn void rgfwJsSetupDropListeners() @wasm("env", "rgfw_setup_drop_listeners");
extern fn void rgfwJsBlitSurface(char* data, int w, int h, int minH) @wasm("env", "rgfw_blit_surface");
extern fn void rgfwJsSetCursor(ZString name, sz len) @wasm("env", "rgfw_set_cursor");
extern fn void rgfwJsHideCursor() @wasm("env", "rgfw_hide_cursor");
extern fn int rgfwJsGetMouseX() @wasm("env", "rgfw_get_mouse_x");
extern fn int rgfwJsGetMouseY() @wasm("env", "rgfw_get_mouse_y");
extern fn void rgfwJsSetPointerEvents(int passthrough) @wasm("env", "rgfw_set_pointer_events");
extern fn void rgfwJsWriteClipboard(char* data, sz len) @wasm("env", "rgfw_write_clipboard");
extern fn int rgfwJsUsingAsyncify() @wasm("env", "rgfw_using_asyncify");
extern fn void rgfwJsSleep(int ms) @wasm("env", "rgfw_sleep");
extern fn double rgfwJsDevicePixelRatio() @wasm("env", "rgfw_device_pixel_ratio");
extern fn int rgfwJsInnerWidth() @wasm("env", "rgfw_inner_width");
extern fn int rgfwJsInnerHeight() @wasm("env", "rgfw_inner_height");
extern fn void rgfwJsRequestPointerLock() @wasm("env", "rgfw_request_pointer_lock");
extern fn void rgfwJsExitPointerLock() @wasm("env", "rgfw_exit_pointer_lock");
extern fn void rgfwJsRequestFullscreen() @wasm("env", "rgfw_request_fullscreen");
extern fn void rgfwJsExitFullscreen() @wasm("env", "rgfw_exit_fullscreen");
extern fn void rgfwJsSetOpacity(float opacity) @wasm("env", "rgfw_set_opacity");
extern fn int rgfwJsWebglCreateContext(RgfwGlHints* hints) @wasm("env", "rgfw_webgl_create_context");
extern fn void rgfwJsWebglDestroyContext(int ctx) @wasm("env", "rgfw_webgl_destroy_context");
extern fn void rgfwJsWebglMakeCurrent(int ctx) @wasm("env", "rgfw_webgl_make_current");
extern fn void rgfwJsWebglCommitFrame() @wasm("env", "rgfw_webgl_commit_frame");
extern fn int rgfwJsWebglGetCurrentContext() @wasm("env", "rgfw_webgl_get_current_context");
extern fn int rgfwJsExtensionSupported(ZString name, sz len) @wasm("env", "rgfw_extension_supported");
extern fn GfxFn rgfwJsGetProcAddress(ZString name) @wasm("env", "rgfw_webgl_get_proc_address");
extern fn void rgfwJsMkdir(ZString name) @wasm("env", "rgfw_mkdir");
extern fn void rgfwJsWriteFile(ZString path, char* data, sz len) @wasm("env", "rgfw_write_file");

const int DOM_VK_BACK_QUOTE = 192; const int DOM_VK_0 = 48; const int DOM_VK_1 = 49; const int DOM_VK_2 = 50;
const int DOM_VK_3 = 51; const int DOM_VK_4 = 52; const int DOM_VK_5 = 53; const int DOM_VK_6 = 54;
const int DOM_VK_7 = 55; const int DOM_VK_8 = 56; const int DOM_VK_9 = 57; const int DOM_VK_SPACE = 32;
const int DOM_VK_A = 65; const int DOM_VK_B = 66; const int DOM_VK_C = 67; const int DOM_VK_D = 68;
const int DOM_VK_E = 69; const int DOM_VK_F = 70; const int DOM_VK_G = 71; const int DOM_VK_H = 72;
const int DOM_VK_I = 73; const int DOM_VK_J = 74; const int DOM_VK_K = 75; const int DOM_VK_L = 76;
const int DOM_VK_M = 77; const int DOM_VK_N = 78; const int DOM_VK_O = 79; const int DOM_VK_P = 80;
const int DOM_VK_Q = 81; const int DOM_VK_R = 82; const int DOM_VK_S = 83; const int DOM_VK_T = 84;
const int DOM_VK_U = 85; const int DOM_VK_V = 86; const int DOM_VK_W = 87; const int DOM_VK_X = 88;
const int DOM_VK_Y = 89; const int DOM_VK_Z = 90;
const int DOM_VK_PERIOD = 190; const int DOM_VK_COMMA = 188; const int DOM_VK_SLASH = 191;
const int DOM_VK_OPEN_BRACKET = 219; const int DOM_VK_CLOSE_BRACKET = 221; const int DOM_VK_SEMICOLON = 186;
const int DOM_VK_QUOTE = 222; const int DOM_VK_BACK_SLASH = 220; const int DOM_VK_RETURN = 13;
const int DOM_VK_DELETE = 46; const int DOM_VK_NUM_LOCK = 144; const int DOM_VK_DIVIDE = 111;
const int DOM_VK_MULTIPLY = 106; const int DOM_VK_SUBTRACT = 109;
const int DOM_VK_NUMPAD1 = 97; const int DOM_VK_NUMPAD2 = 98; const int DOM_VK_NUMPAD3 = 99;
const int DOM_VK_NUMPAD4 = 100; const int DOM_VK_NUMPAD5 = 101; const int DOM_VK_NUMPAD6 = 102;
const int DOM_VK_NUMPAD9 = 105; const int DOM_VK_NUMPAD0 = 96; const int DOM_VK_DECIMAL = 110;
const int DOM_VK_HYPHEN_MINUS = 173; const int DOM_VK_EQUALS = 61;
const int DOM_VK_BACK_SPACE = 8; const int DOM_VK_TAB = 9; const int DOM_VK_CAPS_LOCK = 20;
const int DOM_VK_SHIFT = 16; const int DOM_VK_CONTROL = 17; const int DOM_VK_ALT = 18; const int DOM_VK_META = 91;
const int DOM_VK_F1 = 112; const int DOM_VK_F2 = 113; const int DOM_VK_F3 = 114; const int DOM_VK_F4 = 115;
const int DOM_VK_F5 = 116; const int DOM_VK_F6 = 117; const int DOM_VK_F7 = 118; const int DOM_VK_F8 = 119;
const int DOM_VK_F9 = 120; const int DOM_VK_F10 = 121; const int DOM_VK_F11 = 122; const int DOM_VK_F12 = 123;
const int DOM_VK_F13 = 124; const int DOM_VK_F14 = 125; const int DOM_VK_F15 = 126; const int DOM_VK_F16 = 127;
const int DOM_VK_F17 = 128; const int DOM_VK_F18 = 129; const int DOM_VK_F19 = 130; const int DOM_VK_F20 = 131;
const int DOM_VK_F21 = 132; const int DOM_VK_F22 = 133; const int DOM_VK_F23 = 134; const int DOM_VK_F24 = 135;
const int DOM_VK_UP = 38; const int DOM_VK_DOWN = 40; const int DOM_VK_LEFT = 37; const int DOM_VK_RIGHT = 39;
const int DOM_VK_INSERT = 45; const int DOM_VK_END = 35; const int DOM_VK_PAGE_UP = 33; const int DOM_VK_PAGE_DOWN = 34;
const int DOM_VK_ESCAPE = 27; const int DOM_VK_HOME = 36; const int DOM_VK_SCROLL_LOCK = 145;
const int DOM_VK_PRINTSCREEN = 44; const int DOM_VK_PAUSE = 19;

// private
fn RgfwKey rgfw_wasmPhysicalToRGFW(uint hash)
{
	switch (hash)
	{
		case 0x67243A2Du: return RGFW_KEY_ESCAPE;
		case 0x67251058u: return RGFW_KEY0; case 0x67251059u: return RGFW_KEY1; case 0x6725105Au: return RGFW_KEY2;
		case 0x6725105Bu: return RGFW_KEY3; case 0x6725105Cu: return RGFW_KEY4; case 0x6725105Du: return RGFW_KEY5;
		case 0x6725105Eu: return RGFW_KEY6; case 0x6725105Fu: return RGFW_KEY7; case 0x67251050u: return RGFW_KEY8;
		case 0x67251051u: return RGFW_KEY9;
		case 0x92E14DD3u: return RGFW_KEY_MINUS; case 0x92E1FBACu: return RGFW_KEY_EQUALS;
		case 0x36BF1CB5u: return RGFW_KEY_BACK_SPACE; case 0x7B8E51E2u: return RGFW_KEY_TAB;
		case 0x2C595B51u: return RGFW_KEY_Q; case 0x2C595B57u: return RGFW_KEY_W; case 0x2C595B45u: return RGFW_KEY_E;
		case 0x2C595B52u: return RGFW_KEY_R; case 0x2C595B54u: return RGFW_KEY_T; case 0x2C595B59u: return RGFW_KEY_Y;
		case 0x2C595B55u: return RGFW_KEY_U; case 0x2C595B4Fu: return RGFW_KEY_O; case 0x2C595B50u: return RGFW_KEY_P;
		case 0x45D8158Cu: return RGFW_KEY_CLOSE_BRACKET; case 0xDEEABF7Cu: return RGFW_KEY_BRACKET;
		case 0x92E1C5D2u: return RGFW_KEY_RETURN; case 0xE058958Cu: return RGFW_KEY_CONTROL_L;
		case 0x2C595B41u: return RGFW_KEY_A; case 0x2C595B53u: return RGFW_KEY_S; case 0x2C595B44u: return RGFW_KEY_D;
		case 0x2C595B46u: return RGFW_KEY_F; case 0x2C595B47u: return RGFW_KEY_G; case 0x2C595B48u: return RGFW_KEY_H;
		case 0x2C595B4Au: return RGFW_KEY_J; case 0x2C595B4Bu: return RGFW_KEY_K; case 0x2C595B4Cu: return RGFW_KEY_L;
		case 0x2707219Eu: return RGFW_KEY_SEMICOLON; case 0x92E0B58Du: return RGFW_KEY_APOSTROPHE;
		case 0x36BF358Du: return RGFW_KEY_BACKTICK; case 0x26B1958Cu: return RGFW_KEY_SHIFT_L;
		case 0x36BF2438u: return RGFW_KEY_BACK_SLASH;
		case 0x2C595B5Au: return RGFW_KEY_Z; case 0x2C595B58u: return RGFW_KEY_X; case 0x2C595B43u: return RGFW_KEY_C;
		case 0x2C595B56u: return RGFW_KEY_V; case 0x2C595B42u: return RGFW_KEY_B; case 0x2C595B4Eu: return RGFW_KEY_N;
		case 0x2C595B4Du: return RGFW_KEY_M;
		case 0x92E1A1C1u: return RGFW_KEY_COMMA; case 0x672FFAD4u: return RGFW_KEY_PERIOD; case 0x92E0A438u: return RGFW_KEY_SLASH;
		case 0xC5A6BF7Cu: return RGFW_KEY_SHIFT_R; case 0x5D64DA91u: return RGFW_KEY_PAD_MULTIPLY;
		case 0xC914958Cu: return RGFW_KEY_ALT_L; case 0x92E09CB5u: return RGFW_KEY_SPACE;
		case 0xB8FAE73Bu: return RGFW_KEY_CAPS_LOCK;
		case 0x7174B789u: return RGFW_KEY_F1; case 0x7174B78Au: return RGFW_KEY_F2; case 0x7174B78Bu: return RGFW_KEY_F3;
		case 0x7174B78Cu: return RGFW_KEY_F4; case 0x7174B78Du: return RGFW_KEY_F5; case 0x7174B78Eu: return RGFW_KEY_F6;
		case 0x7174B78Fu: return RGFW_KEY_F7; case 0x7174B780u: return RGFW_KEY_F8; case 0x7174B781u: return RGFW_KEY_F9;
		case 0x7B8E57B0u: return RGFW_KEY_F10;
		case 0xC925FCDFu: return RGFW_KEY_PAD_MULTIPLY; case 0xC925FCD0u: return RGFW_KEY_PAD8; case 0xC925FCD1u: return RGFW_KEY_PAD9;
		case 0x5EA3E8A4u: return RGFW_KEY_MINUS;
		case 0xC925FCDCu: return RGFW_KEY_PAD4; case 0xC925FCDDu: return RGFW_KEY_PAD5; case 0xC925FCDEu: return RGFW_KEY_PAD6;
		case 0xC925FCD9u: return RGFW_KEY_PAD1; case 0xC925FCDAu: return RGFW_KEY_PAD2; case 0xC925FCDBu: return RGFW_KEY_PAD3;
		case 0xC925FCD8u: return RGFW_KEY_PAD0; case 0x95852DACu: return RGFW_KEY_PERIOD;
		case 0x7B8E57B1u: return RGFW_KEY_F11; case 0x7B8E57B2u: return RGFW_KEY_F12;
		case 0x7B8E57B3u: return RGFW_KEY_F13; case 0x7B8E57B4u: return RGFW_KEY_F14; case 0x7B8E57B5u: return RGFW_KEY_F15;
		case 0x7B8E57B6u: return RGFW_KEY_F16; case 0x7B8E57B7u: return RGFW_KEY_F17; case 0x7B8E57B8u: return RGFW_KEY_F18;
		case 0x7B8E57B9u: return RGFW_KEY_F19; case 0x7B8E57A8u: return RGFW_KEY_F20; case 0x7B8E57A9u: return RGFW_KEY_F21;
		case 0x7B8E57AAu: return RGFW_KEY_F22; case 0x7B8E57ABu: return RGFW_KEY_F23;
		case 0x7393FBACu: return RGFW_KEY_PAD_RETURN; case 0xB88EBF7Cu: return RGFW_KEY_ALT_R;
		case 0xC925873Bu: return RGFW_KEY_NUM_LOCK; case 0x2C595F45u: return RGFW_KEY_HOME;
		case 0xC91BB690u: return RGFW_KEY_UP; case 0x672F9210u: return RGFW_KEY_PAGE_UP;
		case 0x3799258Cu: return RGFW_KEY_LEFT; case 0x4CE33F7Cu: return RGFW_KEY_RIGHT;
		case 0x7B8E55DCu: return RGFW_KEY_END; case 0x3799379Eu: return RGFW_KEY_DOWN; case 0xBA90179Eu: return RGFW_KEY_PAGE_DOWN;
		case 0x6723CB2Cu: return RGFW_KEY_INSERT; case 0x6725C50Du: return RGFW_KEY_DELETE;
		case 0x6723658Cu: return RGFW_KEY_SUPER_L; case 0x39643F7Cu: return RGFW_KEY_SUPER_R;
		case 0x380B9C8Cu: return RGFW_KEY_PAD_PLUS;
		default: return RGFW_KEY_NULL;
	}
}

// private
fn void rgfwEmscriptenOnResize(int w, int h) @export("rgfw_on_resize")
{
	_window_resized_callback(_gfx_ctx.root, w, h);
}

// private
fn void rgfwEmscriptenOnFullscreenChange(int screenW, int screenH, bool entering) @export("rgfw_on_fullscreenchange")
{
	if (!(_gfx_ctx.root.internal.enabledEvents & RGFW_WINDOW_RESIZED_FLAG)) return;
	static IntVec2 original;
	if (entering)
	{
		original = _gfx_ctx.root.size;
		_gfx_ctx.root.size = { screenW, screenH };
	}
	else
	{
		_gfx_ctx.root.size = original;
	}
	rgfwJsSetCanvasSize(..._gfx_ctx.root.size);
	_window_resized_callback(_gfx_ctx.root, ..._gfx_ctx.root.size);
}

// private
fn void rgfwEmscriptenOnFocusIn() @export("rgfw_on_focusin") { rgfw_windowFocusCallback(_gfx_ctx.root, true); }
// private
fn void rgfwEmscriptenOnFocusOut() @export("rgfw_on_focusout") { rgfw_windowFocusCallback(_gfx_ctx.root, false); }

// private
fn void rgfwEmscriptenOnMouseMove(int x, int y, int movementX, int movementY) @export("rgfw_on_mousemove")
{
	_mouse_motion_callback(_gfx_ctx.root, x, y);
	_raw_motion_callback(_gfx_ctx.root, (float)movementX, (float)movementY);
}

// private
fn void rgfwEmscriptenOnMouseDown(int button) @export("rgfw_on_mousedown")
{
	if (button > 2) button += 2;
	rgfw_mouseButtonCallback(_gfx_ctx.root, (GfxMouseButton)button, true);
}

// private
fn void rgfwEmscriptenOnMouseUp(int button) @export("rgfw_on_mouseup")
{
	if (button > 2) button += 2;
	rgfw_mouseButtonCallback(_gfx_ctx.root, (GfxMouseButton)button, false);
}

// private
fn void rgfwEmscriptenOnWheel(float deltaX, float deltaY) @export("rgfw_on_wheel") { rgfw_mouseScrollCallback(_gfx_ctx.root, deltaX, deltaY); }

// private
fn void rgfwEmscriptenOnTouch(int numTouches, int* xs, int* ys, char down) @export("rgfw_on_touch")
{
	// down == 255 is the sentinel for touchmove (no button state change).
	if (down == 255)
	{
		if (!(_gfx_ctx.root.internal.enabledEvents & RGFW_MOUSE_MOTION_FLAG)) return;
		for (int i = 0; i < numTouches; i++)
		{
			_mouse_motion_callback(_gfx_ctx.root, xs[i], ys[i]);
			_raw_motion_callback(_gfx_ctx.root, 0.0f, 0.0f);
		}
		return;
	}

	if (down)
	{
		if (!(_gfx_ctx.root.internal.enabledEvents & RGFW_MOUSE_BUTTON_PRESSED_FLAG)) return;
	}
	else
	{
		if (!(_gfx_ctx.root.internal.enabledEvents & RGFW_MOUSE_BUTTON_RELEASED_FLAG)) return;
	}

	for (int i = 0; i < numTouches; i++)
	{
		_mouse_motion_callback(_gfx_ctx.root, xs[i], ys[i]);
		_raw_motion_callback(_gfx_ctx.root, 0.0f, 0.0f);
		rgfw_mouseButtonCallback(_gfx_ctx.root, LEFT, (bool)down);
	}
}

// private
fn void rgfw_handleKeyEvent(ZString code, uint codepoint, bool press) @export("rgfw_handleKeyEvent")
{
	ZString iCode = code;
	uint hash = 0;
	while (iCode[0] != 0) { hash = ((hash ^ 0x7E057D79u) << 3) ^ (uint)iCode[0]; iCode++; }

	RgfwKey physicalKey = rgfw_wasmPhysicalToRGFW(hash);
	rgfw_keyCallback(_gfx_ctx.root, physicalKey, _gfx_ctx.root.internal.mod, is_key_down(physicalKey) && press, press);
	if (press) rgfw_keyCharCallback(_gfx_ctx.root, codepoint);
}

// private
fn void rgfw_handleKeyMods(bool capital, bool numlock, bool control, bool alt, bool shift, bool super, bool scroll) @export("rgfw_handleKeyMods")
{
	rgfw_keyUpdateKeyModsEx(_gfx_ctx.root, capital, numlock, control, alt, shift, super, scroll);
}

// private
fn void rgfw_emscriptenOnDrop(char* file, sz size) @export("rgfw_onDrop") { rgfw_dataDropCallback(_gfx_ctx.root, file, size, RGFW_DATA_FILE); }

// public
fn void rgfw_stopCheckEvents() { _gfx_ctx.stopCheckEvents_bool = true; }

// public
fn void? GfxSurface.init(&self, void* data, int w, int h, GfxFormat format, GfxWindow* window = null)
{
	self.data = data;
	self.size = { w, h };
	self.format = format;
}

// public
fn void rgfw_window_blitSurface(GfxWindow* win, GfxSurface* surface)
{
	int minH = win.height < surface.size.y ? win.height : surface.size.y;
	copy_image_data(surface.data, surface.size.x, minH, RGBA8, surface.data, surface.format, surface.convertFunc);
	rgfwJsBlitSurface(surface.data, surface.size.x, surface.size.y, minH);
}

// public
fn void GfxSurface.release(&self) { *self = {}; }

// public
fn void rgfw_initKeycodesPlatform()
{
	_gfx_ctx.keycodes[DOM_VK_BACK_QUOTE] = RGFW_KEY_BACKTICK;
	_gfx_ctx.keycodes[DOM_VK_0] = RGFW_KEY0; _gfx_ctx.keycodes[DOM_VK_1] = RGFW_KEY1; _gfx_ctx.keycodes[DOM_VK_2] = RGFW_KEY2;
	_gfx_ctx.keycodes[DOM_VK_3] = RGFW_KEY3; _gfx_ctx.keycodes[DOM_VK_4] = RGFW_KEY4; _gfx_ctx.keycodes[DOM_VK_5] = RGFW_KEY5;
	_gfx_ctx.keycodes[DOM_VK_6] = RGFW_KEY6; _gfx_ctx.keycodes[DOM_VK_7] = RGFW_KEY7; _gfx_ctx.keycodes[DOM_VK_8] = RGFW_KEY8;
	_gfx_ctx.keycodes[DOM_VK_9] = RGFW_KEY9; _gfx_ctx.keycodes[DOM_VK_SPACE] = RGFW_KEY_SPACE;
	_gfx_ctx.keycodes[DOM_VK_A] = RGFW_KEY_A; _gfx_ctx.keycodes[DOM_VK_B] = RGFW_KEY_B; _gfx_ctx.keycodes[DOM_VK_C] = RGFW_KEY_C;
	_gfx_ctx.keycodes[DOM_VK_D] = RGFW_KEY_D; _gfx_ctx.keycodes[DOM_VK_E] = RGFW_KEY_E; _gfx_ctx.keycodes[DOM_VK_F] = RGFW_KEY_F;
	_gfx_ctx.keycodes[DOM_VK_G] = RGFW_KEY_G; _gfx_ctx.keycodes[DOM_VK_H] = RGFW_KEY_H; _gfx_ctx.keycodes[DOM_VK_I] = RGFW_KEY_I;
	_gfx_ctx.keycodes[DOM_VK_J] = RGFW_KEY_J; _gfx_ctx.keycodes[DOM_VK_K] = RGFW_KEY_K; _gfx_ctx.keycodes[DOM_VK_L] = RGFW_KEY_L;
	_gfx_ctx.keycodes[DOM_VK_M] = RGFW_KEY_M; _gfx_ctx.keycodes[DOM_VK_N] = RGFW_KEY_N; _gfx_ctx.keycodes[DOM_VK_O] = RGFW_KEY_O;
	_gfx_ctx.keycodes[DOM_VK_P] = RGFW_KEY_P; _gfx_ctx.keycodes[DOM_VK_Q] = RGFW_KEY_Q; _gfx_ctx.keycodes[DOM_VK_R] = RGFW_KEY_R;
	_gfx_ctx.keycodes[DOM_VK_S] = RGFW_KEY_S; _gfx_ctx.keycodes[DOM_VK_T] = RGFW_KEY_T; _gfx_ctx.keycodes[DOM_VK_U] = RGFW_KEY_U;
	_gfx_ctx.keycodes[DOM_VK_V] = RGFW_KEY_V; _gfx_ctx.keycodes[DOM_VK_W] = RGFW_KEY_W; _gfx_ctx.keycodes[DOM_VK_X] = RGFW_KEY_X;
	_gfx_ctx.keycodes[DOM_VK_Y] = RGFW_KEY_Y; _gfx_ctx.keycodes[DOM_VK_Z] = RGFW_KEY_Z;
	_gfx_ctx.keycodes[DOM_VK_PERIOD] = RGFW_KEY_PERIOD; _gfx_ctx.keycodes[DOM_VK_COMMA] = RGFW_KEY_COMMA;
	_gfx_ctx.keycodes[DOM_VK_SLASH] = RGFW_KEY_SLASH; _gfx_ctx.keycodes[DOM_VK_OPEN_BRACKET] = RGFW_KEY_BRACKET;
	_gfx_ctx.keycodes[DOM_VK_CLOSE_BRACKET] = RGFW_KEY_CLOSE_BRACKET; _gfx_ctx.keycodes[DOM_VK_SEMICOLON] = RGFW_KEY_SEMICOLON;
	_gfx_ctx.keycodes[DOM_VK_QUOTE] = RGFW_KEY_APOSTROPHE; _gfx_ctx.keycodes[DOM_VK_BACK_SLASH] = RGFW_KEY_BACK_SLASH;
	_gfx_ctx.keycodes[DOM_VK_RETURN] = RGFW_KEY_RETURN; _gfx_ctx.keycodes[DOM_VK_DELETE] = RGFW_KEY_DELETE;
	_gfx_ctx.keycodes[DOM_VK_NUM_LOCK] = RGFW_KEY_NUM_LOCK; _gfx_ctx.keycodes[DOM_VK_DIVIDE] = RGFW_KEY_PAD_SLASH;
	_gfx_ctx.keycodes[DOM_VK_MULTIPLY] = RGFW_KEY_PAD_MULTIPLY; _gfx_ctx.keycodes[DOM_VK_SUBTRACT] = RGFW_KEY_PAD_MINUS;
	_gfx_ctx.keycodes[DOM_VK_NUMPAD1] = RGFW_KEY_PAD1; _gfx_ctx.keycodes[DOM_VK_NUMPAD2] = RGFW_KEY_PAD2;
	_gfx_ctx.keycodes[DOM_VK_NUMPAD3] = RGFW_KEY_PAD3; _gfx_ctx.keycodes[DOM_VK_NUMPAD4] = RGFW_KEY_PAD4;
	_gfx_ctx.keycodes[DOM_VK_NUMPAD5] = RGFW_KEY_PAD5; _gfx_ctx.keycodes[DOM_VK_NUMPAD6] = RGFW_KEY_PAD6;
	_gfx_ctx.keycodes[DOM_VK_NUMPAD9] = RGFW_KEY_PAD9; _gfx_ctx.keycodes[DOM_VK_NUMPAD0] = RGFW_KEY_PAD0;
	_gfx_ctx.keycodes[DOM_VK_DECIMAL] = RGFW_KEY_PAD_PERIOD; _gfx_ctx.keycodes[DOM_VK_HYPHEN_MINUS] = RGFW_KEY_MINUS;
	_gfx_ctx.keycodes[DOM_VK_EQUALS] = RGFW_KEY_EQUALS; _gfx_ctx.keycodes[DOM_VK_BACK_SPACE] = RGFW_KEY_BACK_SPACE;
	_gfx_ctx.keycodes[DOM_VK_TAB] = RGFW_KEY_TAB; _gfx_ctx.keycodes[DOM_VK_CAPS_LOCK] = RGFW_KEY_CAPS_LOCK;
	_gfx_ctx.keycodes[DOM_VK_SHIFT] = RGFW_KEY_SHIFT_L; _gfx_ctx.keycodes[DOM_VK_CONTROL] = RGFW_KEY_CONTROL_L;
	_gfx_ctx.keycodes[DOM_VK_ALT] = RGFW_KEY_ALT_L; _gfx_ctx.keycodes[DOM_VK_META] = RGFW_KEY_SUPER_L;
	_gfx_ctx.keycodes[DOM_VK_F1] = RGFW_KEY_F1; _gfx_ctx.keycodes[DOM_VK_F2] = RGFW_KEY_F2; _gfx_ctx.keycodes[DOM_VK_F3] = RGFW_KEY_F3;
	_gfx_ctx.keycodes[DOM_VK_F4] = RGFW_KEY_F4; _gfx_ctx.keycodes[DOM_VK_F5] = RGFW_KEY_F5; _gfx_ctx.keycodes[DOM_VK_F6] = RGFW_KEY_F6;
	_gfx_ctx.keycodes[DOM_VK_F7] = RGFW_KEY_F7; _gfx_ctx.keycodes[DOM_VK_F8] = RGFW_KEY_F8; _gfx_ctx.keycodes[DOM_VK_F9] = RGFW_KEY_F9;
	_gfx_ctx.keycodes[DOM_VK_F10] = RGFW_KEY_F10; _gfx_ctx.keycodes[DOM_VK_F11] = RGFW_KEY_F11; _gfx_ctx.keycodes[DOM_VK_F12] = RGFW_KEY_F12;
	_gfx_ctx.keycodes[DOM_VK_UP] = RGFW_KEY_UP; _gfx_ctx.keycodes[DOM_VK_DOWN] = RGFW_KEY_DOWN;
	_gfx_ctx.keycodes[DOM_VK_LEFT] = RGFW_KEY_LEFT; _gfx_ctx.keycodes[DOM_VK_RIGHT] = RGFW_KEY_RIGHT;
	_gfx_ctx.keycodes[DOM_VK_INSERT] = RGFW_KEY_INSERT; _gfx_ctx.keycodes[DOM_VK_END] = RGFW_KEY_END;
	_gfx_ctx.keycodes[DOM_VK_PAGE_UP] = RGFW_KEY_PAGE_UP; _gfx_ctx.keycodes[DOM_VK_PAGE_DOWN] = RGFW_KEY_PAGE_DOWN;
	_gfx_ctx.keycodes[DOM_VK_ESCAPE] = RGFW_KEY_ESCAPE; _gfx_ctx.keycodes[DOM_VK_HOME] = RGFW_KEY_HOME;
	_gfx_ctx.keycodes[DOM_VK_SCROLL_LOCK] = RGFW_KEY_SCROLL_LOCK; _gfx_ctx.keycodes[DOM_VK_PRINTSCREEN] = RGFW_KEY_PRINT_SCREEN;
	_gfx_ctx.keycodes[DOM_VK_PAUSE] = RGFW_KEY_PAUSE;
	_gfx_ctx.keycodes[DOM_VK_F13] = RGFW_KEY_F13; _gfx_ctx.keycodes[DOM_VK_F14] = RGFW_KEY_F14; _gfx_ctx.keycodes[DOM_VK_F15] = RGFW_KEY_F15;
	_gfx_ctx.keycodes[DOM_VK_F16] = RGFW_KEY_F16; _gfx_ctx.keycodes[DOM_VK_F17] = RGFW_KEY_F17; _gfx_ctx.keycodes[DOM_VK_F18] = RGFW_KEY_F18;
	_gfx_ctx.keycodes[DOM_VK_F19] = RGFW_KEY_F19; _gfx_ctx.keycodes[DOM_VK_F20] = RGFW_KEY_F20; _gfx_ctx.keycodes[DOM_VK_F21] = RGFW_KEY_F21;
	_gfx_ctx.keycodes[DOM_VK_F22] = RGFW_KEY_F22; _gfx_ctx.keycodes[DOM_VK_F23] = RGFW_KEY_F23; _gfx_ctx.keycodes[DOM_VK_F24] = RGFW_KEY_F24;
}

// public
fn int rgfw_initPlatform(String class_name, GfxInitOption flags)
{
	_gfx_ctx.usingAsyncify = -1;
	GfxMonitor monitor;
	monitor.origin = {};
	monitor.pixelRatio = (float)rgfwJsDevicePixelRatio();
	monitor.mode.w = rgfwJsInnerWidth();
	monitor.mode.h = rgfwJsInnerHeight();
	monitor.phys_size = {
		(float)(int)(monitor.mode.w * monitor.pixelRatio + 0.5f),
		(float)(int)(monitor.mode.h * monitor.pixelRatio + 0.5f)
	};

	float dpi = 96.0f * monitor.pixelRatio;
	monitor.scale = (Vec2)(dpi / 96.0f);

	rgfw_splitBPP(32, &monitor.mode);
	monitor.mode.refresh_rate = 0;

	GfxMonitorNode* node = rgfw_monitors_add(&monitor);
	if (node != null)
	{
		_gfx_ctx.monitors.primary = node;
		rgfw_monitorCallback(_gfx_ctx.root, &node.mon, true);
	}
	return 0;
}

// public
fn void? rgfw_createWindowPlatform(String name, GfxWindowFlags flags, GfxWindow* win)
{
	rgfwJsSetCanvasSize(win.width, win.height);
	rgfwJsSetWindowTitle((ZString)name, name.len);
	rgfwJsSetupWindowListeners();

	if (flags.allow_dnd) win.internal.flags.allow_dnd = true;

	rgfwJsSetupDropListeners();
}

// public
fn RgfwKey rgfw_physicalToMappedKey(RgfwKey key) => key;
// public
fn IntVec2 rgfw_window_fetchSize(GfxWindow* win) => win.get_size();

// public
fn void rgfw_pollEvents()
{
	if (_gfx_ctx.usingAsyncify == -1) _gfx_ctx.usingAsyncify = rgfwJsUsingAsyncify();
	rgfw_resetPrevState();
	if (_gfx_ctx.usingAsyncify) rgfwJsSleep(0);
}

// public
fn void rgfw_window_resizePlatform(GfxWindow* win, int w, int h) { rgfwJsSetCanvasSize(w, h); }

// public
fn RgfwMouse* rgfw_createMouseStandard(GfxMouseIcon mouse)
{
	ZString cursorName;
	switch (mouse)
	{
		case NORMAL: cursorName = "default";
		case ARROW: cursorName = "default";
		case IBEAM: cursorName = "text";
		case CROSSHAIR: cursorName = "crosshair";
		case POINTING_HAND: cursorName = "pointer";
		case RESIZE_EW: cursorName = "ew-resize";
		case RESIZE_NS: cursorName = "ns-resize";
		case RESIZE_NWSE: cursorName = "nwse-resize";
		case RESIZE_NESW: cursorName = "nesw-resize";
		case RESIZE_NW: cursorName = "nw-resize";
		case RESIZE_N: cursorName = "n-resize";
		case RESIZE_NE: cursorName = "ne-resize";
		case RESIZE_E: cursorName = "e-resize";
		case RESIZE_SE: cursorName = "se-resize";
		case RESIZE_S: cursorName = "s-resize";
		case RESIZE_SW: cursorName = "sw-resize";
		case RESIZE_W: cursorName = "w-resize";
		case RESIZE_ALL: cursorName = "move";
		case NOT_ALLOWED: cursorName = "not-allowed";
		case WAIT: cursorName = "wait";
		case PROGRESS: cursorName = "progress";
		default: return null;
	}
	return (RgfwMouse*)cursorName;
}

// public
fn void rgfw_window_moveMouse(GfxWindow* win, int x, int y) {}
// public
fn RgfwMouse* rgfw_createMouse(char* data, int w, int h, GfxFormat format) => null;

// public
fn bool rgfw_window_setMousePlatform(GfxWindow* win, RgfwMouse* mouse)
{
	ZString name = (ZString)mouse;
	rgfwJsSetCursor(name, name.len());
	return true;
}

// public
fn void rgfw_freeMouse(RgfwMouse* mouse) {}

// public
fn bool rgfw_getGlobalMouse(int* x, int* y)
{
	if (x != null) *x = rgfwJsGetMouseX();
	if (y != null) *y = rgfwJsGetMouseY();
	return true;
}

// public
fn void rgfw_window_setMousePassthrough(GfxWindow* win, char passthrough) { rgfwJsSetPointerEvents(passthrough); }

// public
fn bool rgfw_writeClipboard(GfxDataTransfer* data)
{
	rgfwJsWriteClipboard(data.data, data.length);
	return true;
}

// public
fn bool rgfw_readClipboardPtr(RgfwDataTransferType requestedType, char* buffer, sz capacity, GfxDataTransfer* data)
{
	if (requestedType != RGFW_DATA_TEXT) return false;
	return false;
}

// public
fn bool rgfw_window_createContextPtr_OpenGL(GfxWindow* win, RgfwGlContext* ctx, RgfwGlHints* hints)
{
	win.src.ctx.native = ctx;
	win.src.gfxType = RGFW_GFX_NATIVE_OPEN_GL;

	int handle = rgfwJsWebglCreateContext(hints);
	if (handle == 0)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_OPEN_GL_CONTEXT, "Failed to create an OpenGL Context.");
		return false;
	}
	ctx.ctx = (void*)(uptr)handle;
	rgfwJsWebglMakeCurrent(handle);
	rgfw_window_swapInterval_OpenGL(win, 0);
	return true;
}

// public
fn void rgfw_window_deleteContextPtr_OpenGL(GfxWindow* win, RgfwGlContext* ctx)
{
	rgfwJsWebglDestroyContext((int)(uptr)ctx.ctx);
	win.src.ctx.native.ctx = null;
	rgfw_debugCallback(RGFW_TYPE_INFO, RGFW_INFO_OPEN_GL, "OpenGL context freed.");
}


// public
fn void rgfw_window_makeCurrentContext_OpenGL(GfxWindow* win)
{
	if (win == null) { rgfwJsWebglMakeCurrent(0); }
	else { rgfwJsWebglMakeCurrent((int)(uptr)win.src.ctx.native.ctx); }
}

// public
fn void rgfw_window_swapBuffers_OpenGL(GfxWindow* win) { rgfwJsWebglCommitFrame(); }
// public
fn void* rgfw_getCurrentContext_OpenGL() => (void*)(uptr)rgfwJsWebglGetCurrentContext();
// public
fn bool rgfw_extensionSupportedPlatform_OpenGL(ZString extension, sz len) => rgfwJsExtensionSupported(extension, len) != 0;
// public
fn GfxFn rgfw_getProcAddress_OpenGL(ZString procname) => rgfwJsGetProcAddress(procname);
// public
fn void rgfw_window_swapInterval_OpenGL(GfxWindow* win, int swapInterval) {}

// public
fn void rgfw_deinitPlatform() {}
// public
fn void rgfw_window_closePlatform(GfxWindow* win) {}

// public
fn void rgfw_window_setRawMouseModePlatform(GfxWindow* win, bool state) {}
// public
fn void rgfw_window_captureMousePlatform(GfxWindow* win, bool state)
{
	if (state) { rgfwJsRequestPointerLock(); }
	else { rgfwJsExitPointerLock(); }
}

// public
fn void GfxWindow.set_name(&self, String name)
{
	rgfwJsSetWindowTitle((ZString)name, name.len);
}

// public
fn void rgfw_window_maximizePlatform(GfxWindow* win)
{
	GfxMonitor* mon = find_monitor(win);
	if (mon != null) rgfw_window_resize(win, mon.mode.w, mon.mode.h);
	rgfw_window_move(win, 0, 0);
}

// public
fn void rgfw_window_setFullscreen(GfxWindow* win, bool fullscreen)
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

// public
fn void rgfw_window_setOpacity(GfxWindow* win, char opacity) { rgfwJsSetOpacity((float)opacity / 255.0f); }

// public
fn GfxMonitor* find_monitor(GfxWindow* win) => rgfw_getPrimaryMonitor();

// ---- unsupported on WASM (match the original's no-op stubs) ----
// public
fn void rgfw_pollMonitors() {}
// public
fn void rgfw_monitorNode_free(GfxMonitorNode* node) {}
// public
fn void rgfw_window_focus(GfxWindow* win) {}
// public
fn void rgfw_window_raise(GfxWindow* win) {}
// public
fn bool rgfw_monitor_requestMode(GfxMonitor* mon, GfxMonitorMode* mode, GfxMonitorModeRequest request) => false;
// public
fn bool rgfw_monitor_getWorkarea(GfxMonitor* monitor, int* x, int* y, int* width, int* height) => false;
// public
fn sz rgfw_monitor_getGammaRampPtr(GfxMonitor* monitor, RgfwGammaRamp* ramp) => 0;
// public
fn bool rgfw_monitor_setGammaRamp(GfxMonitor* monitor, RgfwGammaRamp* ramp) => false;
// public
fn GfxMonitorMode[] rgfw_monitor_getModesPtr(GfxMonitor* mon) => {};
// public
fn bool rgfw_monitor_setMode(GfxMonitor* mon, GfxMonitorMode* mode) => false;
// public
fn void rgfw_window_movePlatform(GfxWindow* win, int x, int y) {}
// public
fn void rgfw_window_setMinSize(GfxWindow* win, int w, int h) {}
// public
fn void rgfw_window_setMaxSize(GfxWindow* win, int w, int h) {}
// public
fn void rgfw_window_minimizePlatform(GfxWindow* win) {}
// public
fn void rgfw_window_restorePlatform(GfxWindow* win) {}
// public
fn void rgfw_window_setFloating(GfxWindow* win, bool floating) {}
// public
fn void rgfw_window_setBorder(GfxWindow* win, bool border) {}
// public
fn bool GfxWindow.set_icon(&win, char* data, int w, int h, GfxFormat format, GfxWindowIconType type = BOTH) => false;
// public
fn void rgfw_window_hide(GfxWindow* win) {}
// public
fn void rgfw_window_showPlatform(GfxWindow* win) {}
// public
fn void rgfw_window_flash(GfxWindow* win, GfxFlashRequest request) {}
// public
fn bool rgfw_window_isHidden(GfxWindow* win) => false;
// public
fn bool rgfw_window_isMinimized(GfxWindow* win) => false;
// public
fn bool rgfw_window_isMaximized(GfxWindow* win) => false;
// public
fn bool GfxWindow.is_floating(&win) => false;
// public
fn void rgfw_waitForEvent(int waitMS) {}

// public
fn bool load_egl() => false;
// public
fn bool rgfw_loadVulkan() => false;
// public
fn void rgfw_unloadVulkan() {}
