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

extern fn void* memcpy(void* dest, void* src, sz count);
extern fn void* memset(void* dest, int value, sz count);
extern fn char* strstr(char* haystack, char* needle);
extern fn float powf(float x, float y);
extern fn int clock_gettime(int clock_id, Timespec* ts);

struct Timespec { long tv_sec; long tv_nsec; }


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

struct RgfwNativeImage
{
	GfxFormat format;
	char* buffer;
	void* native0;
	void* native1;
	void* native2;
}

struct RgfwWindowSrc
{
	void* window;
	void* view;
	void* mouse;
	void* delegate;
	RgfwGfxContext ctx;
	RgfwGfxContextType gfxType;
	int x, y, w, h;
}


RgfwInfo* _rgfw;
RgfwInfo _rgfw_global;

fn void rgfw_setInfo(RgfwInfo* info) { _rgfw = info; }
fn RgfwInfo* rgfw_getInfo() { return _rgfw; }
fn void* rgfw_alloc(sz size) { return rgfw_alloc_impl(size); }
fn void rgfw_free(void* ptr) { rgfw_free_impl(ptr); }
fn sz rgfw_sizeofInfo() { return RgfwInfo::size; }
fn sz rgfw_sizeofNativeImage() { return RgfwNativeImage::size; }
fn sz rgfw_sizeofSurface() { return GfxSurface::size; }
fn sz rgfw_sizeofWindow() { return GfxWindow::size; }
fn sz rgfw_sizeofWindowSrc() { return RgfwWindowSrc::size; }

fn bool rgfw_loadGL() => true;
fn void rgfw_unloadGL() {}
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

fn void rgfw_debugCallback(RgfwDebugType type, RgfwErrorCode code, ZString msg)
{
	RgfwDebugInfo info;
	info.type = type;
	info.code = code;
	info.msg = msg;
	if (_rgfw != null && _rgfw.debugCallbackSrc != null) _rgfw.debugCallbackSrc(&info);
}

fn RgfwDebugFunc rgfw_setDebugCallback(RgfwDebugFunc func)
{
	RgfwDebugFunc prev = _rgfw.debugCallbackSrc;
	_rgfw.debugCallbackSrc = func;
	return prev;
}

fn int rgfw_init_ptr(ZString className, RgfwInitFlags flags, RgfwInfo* info)
{
	if (info == _rgfw || info == null) return 1;
	if (className == null) className = "RGFW";
	rgfw_setInfo(info);
	rgfw_memzero(_rgfw, RgfwInfo::size);
	if (flags & RGFW_INIT_OPEN_GL && rgfw_loadGL() == false)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_FAILED_FUNC_LOAD, "Failed to load the OpenGL library");
		return -1;
	}
	if (flags & RGFW_INIT_EGL && rgfw_loadEGL() == false)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_FAILED_FUNC_LOAD, "Failed to load the EGL library");
		return -1;
	}
	if (flags & RGFW_INIT_VULKAN && rgfw_loadVulkan() == false)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_FAILED_FUNC_LOAD, "Failed to load the Vulkan library");
		return -1;
	}
	rgfw_initKeycodes();
	int out = rgfw_initPlatform(className, flags);
	if (out != 0)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_INFO_GLOBAL, "failed to initialize global context");
		rgfw_deinitPlatform();
		rgfw_memzero(_rgfw, RgfwInfo::size);
		rgfw_setInfo(null);
		return out;
	}
	for (sz i = 0; i < (sz)RGFW_MOUSE_ICON_COUNT; i++)
	{
		_rgfw.standardMice[i] = rgfw_createMouseStandard((RgfwMouseIcon)i);
	}
	rgfw_pollMonitors();
	rgfw_debugCallback(RGFW_TYPE_INFO, RGFW_INFO_GLOBAL, "global context initialized");
	return out;
}

fn void rgfw_deinit_ptr(RgfwInfo* info)
{
	if (info == null) return;
	rgfw_setInfo(info);
	for (RgfwMouseIcon i = 0; i < RGFW_MOUSE_ICON_COUNT; i++)
	{
		if (_rgfw.standardMice[(sz)i] != null) rgfw_freeMouse(_rgfw.standardMice[(sz)i]);
	}
	rgfw_freeMonitors();
	rgfw_debugCallback(RGFW_TYPE_INFO, RGFW_INFO_GLOBAL, "global context deinitialized");
	rgfw_deinitPlatform();
	rgfw_unloadEGL();
	rgfw_unloadGL();
	_rgfw.root = null;
	_rgfw.windowCount = 0;
	rgfw_setInfo(null);
}



fn GfxWindow* rgfw_createWindowPtr(ZString name, int x, int y, int w, int h, RgfwWindowFlags flags, GfxWindow* win)
{
	if (win == null || _rgfw == null) return null;
	if (name == null) name = "";
	rgfw_memzero(win, GfxWindow::size);
	_rgfw.windowCount++;
	if (_rgfw.root == null) rgfw_setRootWindow(win);
	win.x = x; win.y = y; win.width = w; win.height = h;
	win.internal.mouse = _rgfw.standardMice[(sz)RGFW_MOUSE_NORMAL];
	win.internal.flags = flags;
	win.internal.enabledEvents = RGFW_ALL_EVENT_FLAGS;
	RgfwWindowFlags reservedFlags = flags & RGFW_WINDOW_SCALE_TO_MONITOR;
	GfxWindow* ret = rgfw_createWindowPlatform(name, flags, win);
	if (ret == null)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_PLATFORM, "failed to create a window");
		return null;
	}
	flags |= reservedFlags;
	_window_set_flags(win, flags, 0);
	if (flags & RGFW_WINDOW_OPEN_GL) rgfw_window_createContext_OpenGL(win, rgfw_getGlobalHints_OpenGL());
	if (!(flags & RGFW_WINDOW_HIDE_MOUSE)) rgfw_window_setMouseDefault(win);
	rgfw_window_setName(win, name);
	if (!(flags & RGFW_WINDOW_HIDE))
	{
		flags |= RGFW_WINDOW_HIDE;
		show(win);
	}
	else if ((flags & RGFW_WINDOW_MAXIMIZE) || (flags & RGFW_WINDOW_FULLSCREEN))
	{
		rgfw_window_hide(win);
	}
	return ret;
}


fn void rgfw_window_closePtr(GfxWindow* win)
{
	if (win == null) return;
	if (win.internal.captureMouse) rgfw_window_captureMouse(win, false);
	if ((win.src.gfxType & RGFW_GFX_NATIVE_OPEN_GL) && win.src.ctx.native != null)
	{
		rgfw_window_deleteContext_OpenGL(win, win.src.ctx.native);
		win.src.ctx.native = null;
	}
	rgfw_window_closePlatform(win);
	if (_rgfw.clipboard != null) rgfw_free_impl(_rgfw.clipboard);
	_rgfw.clipboard = null;
	_rgfw.windowCount--;
	rgfw_debugCallback(RGFW_TYPE_INFO, RGFW_INFO_WINDOW, "a window was freed");
}




fn bool rgfw_window_createSurfacePtr(GfxWindow* win, char* data, int w, int h, GfxFormat format, GfxSurface* surface)
{
	return rgfw_createSurfacePtr(data, w, h, format, surface);
}

fn void rgfw_window_setDND(GfxWindow* win, bool allow) { rgfw_setBit(&win.internal.flags, RGFW_WINDOW_ALLOW_DND, allow); }


fn void rgfw_window_setAspectRatio(GfxWindow* win, int w, int h) {}
fn GfxFormat rgfw_nativeFormat() => RGBA8;

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

fn void rgfwEmscriptenOnResize(int w, int h) @export("rgfw_on_resize")
{
	_window_resized_callback(_rgfw.root, w, h);
}

fn void rgfwEmscriptenOnFullscreenChange(int screenW, int screenH, bool entering) @export("rgfw_on_fullscreenchange")
{
	if (!(_rgfw.root.internal.enabledEvents & RGFW_WINDOW_RESIZED_FLAG)) return;
	static IntVec2 original;
	if (entering)
	{
		original = _rgfw.root.size;
		_rgfw.root.size = { screenW, screenH };
	}
	else
	{
		_rgfw.root.size = original;
	}
	rgfwJsSetCanvasSize(..._rgfw.root.size);
	_window_resized_callback(_rgfw.root, ..._rgfw.root.size);
}

fn void rgfwEmscriptenOnFocusIn() @export("rgfw_on_focusin") { rgfw_windowFocusCallback(_rgfw.root, true); }
fn void rgfwEmscriptenOnFocusOut() @export("rgfw_on_focusout") { rgfw_windowFocusCallback(_rgfw.root, false); }

fn void rgfwEmscriptenOnMouseMove(int x, int y, int movementX, int movementY) @export("rgfw_on_mousemove")
{
	_mouse_motion_callback(_rgfw.root, x, y);
	_raw_motion_callback(_rgfw.root, (float)movementX, (float)movementY);
}

fn void rgfwEmscriptenOnMouseDown(int button) @export("rgfw_on_mousedown")
{
	if (button > 2) button += 2;
	rgfw_mouseButtonCallback(_rgfw.root, (RgfwMouseButton)button, true);
}

fn void rgfwEmscriptenOnMouseUp(int button) @export("rgfw_on_mouseup")
{
	if (button > 2) button += 2;
	rgfw_mouseButtonCallback(_rgfw.root, (RgfwMouseButton)button, false);
}

fn void rgfwEmscriptenOnWheel(float deltaX, float deltaY) @export("rgfw_on_wheel") { rgfw_mouseScrollCallback(_rgfw.root, deltaX, deltaY); }

fn void rgfwEmscriptenOnTouch(int numTouches, int* xs, int* ys, char down) @export("rgfw_on_touch")
{
	// down == 255 is the sentinel for touchmove (no button state change).
	if (down == 255)
	{
		if (!(_rgfw.root.internal.enabledEvents & RGFW_MOUSE_MOTION_FLAG)) return;
		for (int i = 0; i < numTouches; i++)
		{
			_mouse_motion_callback(_rgfw.root, xs[i], ys[i]);
			_raw_motion_callback(_rgfw.root, 0.0f, 0.0f);
		}
		return;
	}

	if (down)
	{
		if (!(_rgfw.root.internal.enabledEvents & RGFW_MOUSE_BUTTON_PRESSED_FLAG)) return;
	}
	else
	{
		if (!(_rgfw.root.internal.enabledEvents & RGFW_MOUSE_BUTTON_RELEASED_FLAG)) return;
	}

	for (int i = 0; i < numTouches; i++)
	{
		_mouse_motion_callback(_rgfw.root, xs[i], ys[i]);
		_raw_motion_callback(_rgfw.root, 0.0f, 0.0f);
		rgfw_mouseButtonCallback(_rgfw.root, RGFW_MOUSE_LEFT, (bool)down);
	}
}

fn void rgfw_handleKeyEvent(ZString code, uint codepoint, bool press) @export("rgfw_handleKeyEvent")
{
	ZString iCode = code;
	uint hash = 0;
	while (iCode[0] != 0) { hash = ((hash ^ 0x7E057D79u) << 3) ^ (uint)iCode[0]; iCode++; }

	RgfwKey physicalKey = rgfw_wasmPhysicalToRGFW(hash);
	rgfw_keyCallback(_rgfw.root, physicalKey, _rgfw.root.internal.mod, is_key_down(physicalKey) && press, press);
	if (press) rgfw_keyCharCallback(_rgfw.root, codepoint);
}

fn void rgfw_handleKeyMods(bool capital, bool numlock, bool control, bool alt, bool shift, bool super, bool scroll) @export("rgfw_handleKeyMods")
{
	rgfw_keyUpdateKeyModsEx(_rgfw.root, capital, numlock, control, alt, shift, super, scroll);
}

fn void rgfw_emscriptenOnDrop(char* file, sz size) @export("rgfw_onDrop") { rgfw_dataDropCallback(_rgfw.root, file, size, RGFW_DATA_FILE); }

fn void rgfw_stopCheckEvents() { _rgfw.stopCheckEvents_bool = true; }

fn bool rgfw_createSurfacePtr(char* data, int w, int h, GfxFormat format, GfxSurface* surface)
{
	surface.data = data;
	surface.size = { w, h };
	surface.format = format;
	return true;
}

fn void rgfw_window_blitSurface(GfxWindow* win, GfxSurface* surface)
{
	int minH = win.height < surface.size.y ? win.height : surface.size.y;
	copy_image_data(surface.data, surface.size.x, minH, RGBA8, surface.data, surface.format, surface.convertFunc);
	rgfwJsBlitSurface(surface.data, surface.size.x, surface.size.y, minH);
}

fn void rgfw_surface_freePtr(GfxSurface* surface) {}

fn void rgfw_initKeycodesPlatform()
{
	_rgfw.keycodes[DOM_VK_BACK_QUOTE] = RGFW_KEY_BACKTICK;
	_rgfw.keycodes[DOM_VK_0] = RGFW_KEY0; _rgfw.keycodes[DOM_VK_1] = RGFW_KEY1; _rgfw.keycodes[DOM_VK_2] = RGFW_KEY2;
	_rgfw.keycodes[DOM_VK_3] = RGFW_KEY3; _rgfw.keycodes[DOM_VK_4] = RGFW_KEY4; _rgfw.keycodes[DOM_VK_5] = RGFW_KEY5;
	_rgfw.keycodes[DOM_VK_6] = RGFW_KEY6; _rgfw.keycodes[DOM_VK_7] = RGFW_KEY7; _rgfw.keycodes[DOM_VK_8] = RGFW_KEY8;
	_rgfw.keycodes[DOM_VK_9] = RGFW_KEY9; _rgfw.keycodes[DOM_VK_SPACE] = RGFW_KEY_SPACE;
	_rgfw.keycodes[DOM_VK_A] = RGFW_KEY_A; _rgfw.keycodes[DOM_VK_B] = RGFW_KEY_B; _rgfw.keycodes[DOM_VK_C] = RGFW_KEY_C;
	_rgfw.keycodes[DOM_VK_D] = RGFW_KEY_D; _rgfw.keycodes[DOM_VK_E] = RGFW_KEY_E; _rgfw.keycodes[DOM_VK_F] = RGFW_KEY_F;
	_rgfw.keycodes[DOM_VK_G] = RGFW_KEY_G; _rgfw.keycodes[DOM_VK_H] = RGFW_KEY_H; _rgfw.keycodes[DOM_VK_I] = RGFW_KEY_I;
	_rgfw.keycodes[DOM_VK_J] = RGFW_KEY_J; _rgfw.keycodes[DOM_VK_K] = RGFW_KEY_K; _rgfw.keycodes[DOM_VK_L] = RGFW_KEY_L;
	_rgfw.keycodes[DOM_VK_M] = RGFW_KEY_M; _rgfw.keycodes[DOM_VK_N] = RGFW_KEY_N; _rgfw.keycodes[DOM_VK_O] = RGFW_KEY_O;
	_rgfw.keycodes[DOM_VK_P] = RGFW_KEY_P; _rgfw.keycodes[DOM_VK_Q] = RGFW_KEY_Q; _rgfw.keycodes[DOM_VK_R] = RGFW_KEY_R;
	_rgfw.keycodes[DOM_VK_S] = RGFW_KEY_S; _rgfw.keycodes[DOM_VK_T] = RGFW_KEY_T; _rgfw.keycodes[DOM_VK_U] = RGFW_KEY_U;
	_rgfw.keycodes[DOM_VK_V] = RGFW_KEY_V; _rgfw.keycodes[DOM_VK_W] = RGFW_KEY_W; _rgfw.keycodes[DOM_VK_X] = RGFW_KEY_X;
	_rgfw.keycodes[DOM_VK_Y] = RGFW_KEY_Y; _rgfw.keycodes[DOM_VK_Z] = RGFW_KEY_Z;
	_rgfw.keycodes[DOM_VK_PERIOD] = RGFW_KEY_PERIOD; _rgfw.keycodes[DOM_VK_COMMA] = RGFW_KEY_COMMA;
	_rgfw.keycodes[DOM_VK_SLASH] = RGFW_KEY_SLASH; _rgfw.keycodes[DOM_VK_OPEN_BRACKET] = RGFW_KEY_BRACKET;
	_rgfw.keycodes[DOM_VK_CLOSE_BRACKET] = RGFW_KEY_CLOSE_BRACKET; _rgfw.keycodes[DOM_VK_SEMICOLON] = RGFW_KEY_SEMICOLON;
	_rgfw.keycodes[DOM_VK_QUOTE] = RGFW_KEY_APOSTROPHE; _rgfw.keycodes[DOM_VK_BACK_SLASH] = RGFW_KEY_BACK_SLASH;
	_rgfw.keycodes[DOM_VK_RETURN] = RGFW_KEY_RETURN; _rgfw.keycodes[DOM_VK_DELETE] = RGFW_KEY_DELETE;
	_rgfw.keycodes[DOM_VK_NUM_LOCK] = RGFW_KEY_NUM_LOCK; _rgfw.keycodes[DOM_VK_DIVIDE] = RGFW_KEY_PAD_SLASH;
	_rgfw.keycodes[DOM_VK_MULTIPLY] = RGFW_KEY_PAD_MULTIPLY; _rgfw.keycodes[DOM_VK_SUBTRACT] = RGFW_KEY_PAD_MINUS;
	_rgfw.keycodes[DOM_VK_NUMPAD1] = RGFW_KEY_PAD1; _rgfw.keycodes[DOM_VK_NUMPAD2] = RGFW_KEY_PAD2;
	_rgfw.keycodes[DOM_VK_NUMPAD3] = RGFW_KEY_PAD3; _rgfw.keycodes[DOM_VK_NUMPAD4] = RGFW_KEY_PAD4;
	_rgfw.keycodes[DOM_VK_NUMPAD5] = RGFW_KEY_PAD5; _rgfw.keycodes[DOM_VK_NUMPAD6] = RGFW_KEY_PAD6;
	_rgfw.keycodes[DOM_VK_NUMPAD9] = RGFW_KEY_PAD9; _rgfw.keycodes[DOM_VK_NUMPAD0] = RGFW_KEY_PAD0;
	_rgfw.keycodes[DOM_VK_DECIMAL] = RGFW_KEY_PAD_PERIOD; _rgfw.keycodes[DOM_VK_HYPHEN_MINUS] = RGFW_KEY_MINUS;
	_rgfw.keycodes[DOM_VK_EQUALS] = RGFW_KEY_EQUALS; _rgfw.keycodes[DOM_VK_BACK_SPACE] = RGFW_KEY_BACK_SPACE;
	_rgfw.keycodes[DOM_VK_TAB] = RGFW_KEY_TAB; _rgfw.keycodes[DOM_VK_CAPS_LOCK] = RGFW_KEY_CAPS_LOCK;
	_rgfw.keycodes[DOM_VK_SHIFT] = RGFW_KEY_SHIFT_L; _rgfw.keycodes[DOM_VK_CONTROL] = RGFW_KEY_CONTROL_L;
	_rgfw.keycodes[DOM_VK_ALT] = RGFW_KEY_ALT_L; _rgfw.keycodes[DOM_VK_META] = RGFW_KEY_SUPER_L;
	_rgfw.keycodes[DOM_VK_F1] = RGFW_KEY_F1; _rgfw.keycodes[DOM_VK_F2] = RGFW_KEY_F2; _rgfw.keycodes[DOM_VK_F3] = RGFW_KEY_F3;
	_rgfw.keycodes[DOM_VK_F4] = RGFW_KEY_F4; _rgfw.keycodes[DOM_VK_F5] = RGFW_KEY_F5; _rgfw.keycodes[DOM_VK_F6] = RGFW_KEY_F6;
	_rgfw.keycodes[DOM_VK_F7] = RGFW_KEY_F7; _rgfw.keycodes[DOM_VK_F8] = RGFW_KEY_F8; _rgfw.keycodes[DOM_VK_F9] = RGFW_KEY_F9;
	_rgfw.keycodes[DOM_VK_F10] = RGFW_KEY_F10; _rgfw.keycodes[DOM_VK_F11] = RGFW_KEY_F11; _rgfw.keycodes[DOM_VK_F12] = RGFW_KEY_F12;
	_rgfw.keycodes[DOM_VK_UP] = RGFW_KEY_UP; _rgfw.keycodes[DOM_VK_DOWN] = RGFW_KEY_DOWN;
	_rgfw.keycodes[DOM_VK_LEFT] = RGFW_KEY_LEFT; _rgfw.keycodes[DOM_VK_RIGHT] = RGFW_KEY_RIGHT;
	_rgfw.keycodes[DOM_VK_INSERT] = RGFW_KEY_INSERT; _rgfw.keycodes[DOM_VK_END] = RGFW_KEY_END;
	_rgfw.keycodes[DOM_VK_PAGE_UP] = RGFW_KEY_PAGE_UP; _rgfw.keycodes[DOM_VK_PAGE_DOWN] = RGFW_KEY_PAGE_DOWN;
	_rgfw.keycodes[DOM_VK_ESCAPE] = RGFW_KEY_ESCAPE; _rgfw.keycodes[DOM_VK_HOME] = RGFW_KEY_HOME;
	_rgfw.keycodes[DOM_VK_SCROLL_LOCK] = RGFW_KEY_SCROLL_LOCK; _rgfw.keycodes[DOM_VK_PRINTSCREEN] = RGFW_KEY_PRINT_SCREEN;
	_rgfw.keycodes[DOM_VK_PAUSE] = RGFW_KEY_PAUSE;
	_rgfw.keycodes[DOM_VK_F13] = RGFW_KEY_F13; _rgfw.keycodes[DOM_VK_F14] = RGFW_KEY_F14; _rgfw.keycodes[DOM_VK_F15] = RGFW_KEY_F15;
	_rgfw.keycodes[DOM_VK_F16] = RGFW_KEY_F16; _rgfw.keycodes[DOM_VK_F17] = RGFW_KEY_F17; _rgfw.keycodes[DOM_VK_F18] = RGFW_KEY_F18;
	_rgfw.keycodes[DOM_VK_F19] = RGFW_KEY_F19; _rgfw.keycodes[DOM_VK_F20] = RGFW_KEY_F20; _rgfw.keycodes[DOM_VK_F21] = RGFW_KEY_F21;
	_rgfw.keycodes[DOM_VK_F22] = RGFW_KEY_F22; _rgfw.keycodes[DOM_VK_F23] = RGFW_KEY_F23; _rgfw.keycodes[DOM_VK_F24] = RGFW_KEY_F24;
}

fn int rgfw_initPlatform(ZString className, RgfwInitFlags flags)
{
	_rgfw.usingAsyncify = -1;
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
		_rgfw.monitors.primary = node;
		rgfw_monitorCallback(_rgfw.root, &node.mon, true);
	}
	return 0;
}

fn GfxWindow* rgfw_createWindowPlatform(ZString name, RgfwWindowFlags flags, GfxWindow* win)
{
	rgfwJsSetCanvasSize(win.width, win.height);
	rgfwJsSetWindowTitle(name, name.len());
	rgfwJsSetupWindowListeners();

	if (flags & RGFW_WINDOW_ALLOW_DND) win.internal.flags |= RGFW_WINDOW_ALLOW_DND;

	rgfwJsSetupDropListeners();
	return win;
}

fn RgfwKey rgfw_physicalToMappedKey(RgfwKey key) => key;
fn IntVec2 rgfw_window_fetchSize(GfxWindow* win) => win.get_size();

fn void rgfw_pollEvents()
{
	if (_rgfw.usingAsyncify == -1) _rgfw.usingAsyncify = rgfwJsUsingAsyncify();
	rgfw_resetPrevState();
	if (_rgfw.usingAsyncify) rgfwJsSleep(0);
}

fn void rgfw_window_resizePlatform(GfxWindow* win, int w, int h) { rgfwJsSetCanvasSize(w, h); }

fn RgfwMouse* rgfw_createMouseStandard(RgfwMouseIcon mouse)
{
	ZString cursorName;
	switch (mouse)
	{
		case RGFW_MOUSE_NORMAL: cursorName = "default"; case RGFW_MOUSE_ARROW: cursorName = "default";
		case RGFW_MOUSE_IBEAM: cursorName = "text";
		case RGFW_MOUSE_CROSSHAIR: cursorName = "crosshair";
		case RGFW_MOUSE_POINTING_HAND: cursorName = "pointer";
		case RGFW_MOUSE_RESIZE_EW: cursorName = "ew-resize";
		case RGFW_MOUSE_RESIZE_NS: cursorName = "ns-resize";
		case RGFW_MOUSE_RESIZE_NWSE: cursorName = "nwse-resize";
		case RGFW_MOUSE_RESIZE_NESW: cursorName = "nesw-resize";
		case RGFW_MOUSE_RESIZE_NW: cursorName = "nw-resize";
		case RGFW_MOUSE_RESIZE_N: cursorName = "n-resize";
		case RGFW_MOUSE_RESIZE_NE: cursorName = "ne-resize";
		case RGFW_MOUSE_RESIZE_E: cursorName = "e-resize";
		case RGFW_MOUSE_RESIZE_SE: cursorName = "se-resize";
		case RGFW_MOUSE_RESIZE_S: cursorName = "s-resize";
		case RGFW_MOUSE_RESIZE_SW: cursorName = "sw-resize";
		case RGFW_MOUSE_RESIZE_W: cursorName = "w-resize";
		case RGFW_MOUSE_RESIZE_ALL: cursorName = "move";
		case RGFW_MOUSE_NOT_ALLOWED: cursorName = "not-allowed";
		case RGFW_MOUSE_WAIT: cursorName = "wait";
		case RGFW_MOUSE_PROGRESS: cursorName = "progress";
		default: return null;
	}
	return (RgfwMouse*)cursorName;
}

fn void rgfw_window_moveMouse(GfxWindow* win, int x, int y) {}
fn RgfwMouse* rgfw_createMouse(char* data, int w, int h, GfxFormat format) => null;

fn bool rgfw_window_setMousePlatform(GfxWindow* win, RgfwMouse* mouse)
{
	ZString name = (ZString)mouse;
	rgfwJsSetCursor(name, name.len());
	return true;
}

fn void rgfw_freeMouse(RgfwMouse* mouse) {}

fn void rgfw_window_showMouse(GfxWindow* win, bool show)
{
	rgfw_window_showMouseFlags(win, show);
	if (show) { rgfw_window_setMouseDefault(win); }
	else { rgfwJsHideCursor(); }
}

fn bool rgfw_getGlobalMouse(int* x, int* y)
{
	if (x != null) *x = rgfwJsGetMouseX();
	if (y != null) *y = rgfwJsGetMouseY();
	return true;
}

fn void rgfw_window_setMousePassthrough(GfxWindow* win, char passthrough) { rgfwJsSetPointerEvents(passthrough); }

fn bool rgfw_writeClipboard(GfxDataTransfer* data)
{
	rgfwJsWriteClipboard(data.data, data.length);
	return true;
}

fn bool rgfw_readClipboardPtr(RgfwDataTransferType requestedType, char* buffer, sz capacity, GfxDataTransfer* data)
{
	if (requestedType != RGFW_DATA_TEXT) return false;
	return false;
}

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

fn void rgfw_window_deleteContextPtr_OpenGL(GfxWindow* win, RgfwGlContext* ctx)
{
	rgfwJsWebglDestroyContext((int)(uptr)ctx.ctx);
	win.src.ctx.native.ctx = null;
	rgfw_debugCallback(RGFW_TYPE_INFO, RGFW_INFO_OPEN_GL, "OpenGL context freed.");
}

RgfwGlHints rgfw_global_hints_open_gl_src = {
	0, 0, 0, 0, 1, 8, 8, 8, 8, 24, 0, 0, 0, 0,
	false, false, false, false, RGFW_GL_RELEASE_NONE,
	RGFW_GL_CORE, 1, 0, null, null, RGFW_GL_ACCELERATED
};
RgfwGlHints* rgfw_global_hints_open_gl = &rgfw_global_hints_open_gl_src;

fn void rgfw_resetGlobalHints_OpenGL() { rgfw_global_hints_open_gl_src = {0, 0, 0, 0, 1, 8, 8, 8, 8, 24, 0, 0, 0, 0, false, false, false, false, RGFW_GL_RELEASE_NONE, RGFW_GL_CORE, 1, 0, null, null, RGFW_GL_ACCELERATED}; }
fn void rgfw_setGlobalHints_OpenGL(RgfwGlHints* hints) { rgfw_global_hints_open_gl = hints; }
fn RgfwGlHints* rgfw_getGlobalHints_OpenGL() { return rgfw_global_hints_open_gl; }

fn RgfwGlContext* rgfw_window_createContext_OpenGL(GfxWindow* win, RgfwGlHints* hints)
{
	RgfwGlContext* ctx = (RgfwGlContext*)rgfw_alloc_impl(RgfwGlContext::size);
	if (ctx == null) return null;
	rgfw_memzero(ctx, RgfwGlContext::size);
	if (rgfw_window_createContextPtr_OpenGL(win, ctx, hints) == false)
	{
		rgfw_free_impl(ctx);
		if (win != null) win.src.ctx.native = null;
		return null;
	}
	win.src.gfxType |= RGFW_GFX_OWNED_BY_RGFW;
	return ctx;
}

fn void rgfw_window_deleteContext_OpenGL(GfxWindow* win, RgfwGlContext* ctx)
{
	rgfw_window_deleteContextPtr_OpenGL(win, ctx);
	if (win != null && (win.src.gfxType & RGFW_GFX_OWNED_BY_RGFW)) rgfw_free_impl(ctx);
}

fn void rgfw_window_makeCurrentContext_OpenGL(GfxWindow* win)
{
	if (win == null) { rgfwJsWebglMakeCurrent(0); }
	else { rgfwJsWebglMakeCurrent((int)(uptr)win.src.ctx.native.ctx); }
}

fn void rgfw_window_swapBuffers_OpenGL(GfxWindow* win) { rgfwJsWebglCommitFrame(); }
fn void* rgfw_getCurrentContext_OpenGL() => (void*)(uptr)rgfwJsWebglGetCurrentContext();
fn bool rgfw_extensionSupportedPlatform_OpenGL(ZString extension, sz len) => rgfwJsExtensionSupported(extension, len) != 0;
fn GfxFn rgfw_getProcAddress_OpenGL(ZString procname) => rgfwJsGetProcAddress(procname);
fn void rgfw_window_swapInterval_OpenGL(GfxWindow* win, int swapInterval) {}

fn void rgfw_deinitPlatform() {}
fn void rgfw_window_closePlatform(GfxWindow* win) {}

fn void rgfw_window_setRawMouseModePlatform(GfxWindow* win, bool state) {}
fn void rgfw_window_captureMousePlatform(GfxWindow* win, bool state)
{
	if (state) { rgfwJsRequestPointerLock(); }
	else { rgfwJsExitPointerLock(); }
}

fn void rgfw_window_setName(GfxWindow* win, ZString name)
{
	if (name == null) name = "";
	rgfwJsSetWindowTitle(name, name.len());
}

fn void rgfw_window_maximizePlatform(GfxWindow* win)
{
	GfxMonitor* mon = find_monitor(win);
	if (mon != null) rgfw_window_resize(win, mon.mode.w, mon.mode.h);
	rgfw_window_move(win, 0, 0);
}

fn void rgfw_window_setFullscreen(GfxWindow* win, bool fullscreen)
{
	if (fullscreen)
	{
		win.internal.flags |= RGFW_WINDOW_FULLSCREEN;
		rgfwJsRequestFullscreen();
		return;
	}
	win.internal.flags &= ~(uint)RGFW_WINDOW_FULLSCREEN;
	rgfwJsExitFullscreen();
}

fn void rgfw_window_setOpacity(GfxWindow* win, char opacity) { rgfwJsSetOpacity((float)opacity / 255.0f); }

fn GfxMonitor* find_monitor(GfxWindow* win) => rgfw_getPrimaryMonitor();

// ---- unsupported on WASM (match the original's no-op stubs) ----
fn void rgfw_pollMonitors() {}
fn void rgfw_monitorNode_free(GfxMonitorNode* node) {}
fn void rgfw_window_focus(GfxWindow* win) {}
fn void rgfw_window_raise(GfxWindow* win) {}
fn bool rgfw_monitor_requestMode(GfxMonitor* mon, GfxMonitorMode* mode, GfxMonitorModeRequest request) => false;
fn bool rgfw_monitor_getWorkarea(GfxMonitor* monitor, int* x, int* y, int* width, int* height) => false;
fn sz rgfw_monitor_getGammaRampPtr(GfxMonitor* monitor, RgfwGammaRamp* ramp) => 0;
fn bool rgfw_monitor_setGammaRamp(GfxMonitor* monitor, RgfwGammaRamp* ramp) => false;
fn GfxMonitorMode[] rgfw_monitor_getModesPtr(GfxMonitor* mon) => {};
fn bool rgfw_monitor_setMode(GfxMonitor* mon, GfxMonitorMode* mode) => false;
fn void rgfw_window_movePlatform(GfxWindow* win, int x, int y) {}
fn void rgfw_window_setMinSize(GfxWindow* win, int w, int h) {}
fn void rgfw_window_setMaxSize(GfxWindow* win, int w, int h) {}
fn void rgfw_window_minimizePlatform(GfxWindow* win) {}
fn void rgfw_window_restorePlatform(GfxWindow* win) {}
fn void rgfw_window_setFloating(GfxWindow* win, bool floating) {}
fn void rgfw_window_setBorder(GfxWindow* win, bool border) {}
fn bool GfxWindow.set_icon(&win, char* data, int w, int h, GfxFormat format, GfxWindowIconType type = BOTH) => false;
fn void rgfw_window_hide(GfxWindow* win) {}
fn void rgfw_window_showPlatform(GfxWindow* win) {}
fn void rgfw_window_flash(GfxWindow* win, GfxFlashRequest request) {}
fn bool rgfw_window_isHidden(GfxWindow* win) => false;
fn bool rgfw_window_isMinimized(GfxWindow* win) => false;
fn bool rgfw_window_isMaximized(GfxWindow* win) => false;
fn bool GfxWindow.is_floating(&win) => false;
fn void rgfw_waitForEvent(int waitMS) {}

fn bool rgfw_loadEGL() => false;
fn void rgfw_unloadEGL() {}
fn bool rgfw_loadVulkan() => false;
fn void rgfw_unloadVulkan() {}
