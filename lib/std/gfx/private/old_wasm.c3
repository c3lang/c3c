module std::gfx @feat(WASM);
import std::math;
extern fn char* strstr(char* haystack, char* needle);






struct GfxNativeImage
{
	GfxFormat format;
	char* buffer;
	void* native0;
	void* native1;
	void* native2;
}



// public
fn bool _vulkan_get_physical_device_presentation_support_native(VkInstance instance, VkPhysicalDevice physical_device, uint queue_family_index) => false;

const WGPUSType WGPU_STYPE_EMSCRIPTEN_SURFACE_SOURCE_CANVAS_HTML_SELECTOR = 0x00040000;
struct WgpuEmscriptenSurfaceSourceCanvasHTMLSelector { WgpuChainedStruct chain; WgpuStringView selector; }

// public
fn WGPUSurface todo_fix_window_createSurface_WebGPU(GfxWindow* window, WGPUInstance instance)
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


fn void _window_set_aspect_ratio_native(GfxWindow* win, int w, int h) @private {}

// TODO All of this is horribly broken
extern fn void rgfwJsSetCanvasSize(int w, int h) @wasm("env", "emscripten_set_canvas_size");
extern fn void rgfwJsSetWindowTitle(ZString name, sz len) @wasm("env", "emscripten_set_window_title");
extern fn void rgfwJsSetupWindowListeners() @wasm("env", "emscripten_setup_window_listeners");
extern fn void rgfwJsSetupDropListeners() @wasm("env", "emscripten_setup_drop_listeners");

// Each of the following registers exactly one DOM/window listener, mirroring
// TODO_RENAME_createWindowPlatform's emscripten_set_*_callback calls one-for-one so
// the JS host implementation can be checked line-by-line against RGFW.h.
extern fn void rgfwJsSetResizeCallback() @wasm("env", "emscripten_set_resize_callback");
extern fn void rgfwJsSetFullscreenChangeCallback() @wasm("env", "emscripten_set_fullscreenchange_callback");
extern fn void rgfwJsSetMouseMoveCallback() @wasm("env", "emscripten_set_mousemove_callback");
extern fn void rgfwJsSetTouchStartCallback() @wasm("env", "emscripten_set_touchstart_callback");
extern fn void rgfwJsSetTouchEndCallback() @wasm("env", "emscripten_set_touchend_callback");
extern fn void rgfwJsSetTouchMoveCallback() @wasm("env", "emscripten_set_touchmove_callback");
// RGFW's own Emscripten_on_touchcancel is a no-op; registered anyway to match
// RGFW.h exactly (the JS host's listener may itself do nothing).
extern fn void rgfwJsSetTouchCancelCallback() @wasm("env", "emscripten_set_touchcancel_callback");
extern fn void rgfwJsSetMouseDownCallback() @wasm("env", "emscripten_set_mousedown_callback");
extern fn void rgfwJsSetMouseUpCallback() @wasm("env", "emscripten_set_mouseup_callback");
extern fn void rgfwJsSetWheelCallback() @wasm("env", "emscripten_set_wheel_callback");
extern fn void rgfwJsSetFocusInCallback() @wasm("env", "emscripten_set_focusin_callback");
extern fn void rgfwJsSetFocusOutCallback() @wasm("env", "emscripten_set_focusout_callback");
// Combined registrations: RGFW.h wires each of these pairs from a single
// EM_ASM block (keydown+keyup; drop+dragover) rather than as separate
// emscripten_set_*_callback calls, so one host call covers each pair.
extern fn void rgfwJsSetKeyCallbacks() @wasm("env", "emscripten_set_key_callbacks");
extern fn void rgfwJsSetDropCallbacks() @wasm("env", "emscripten_set_drop_callbacks");
// RGFW.h calls `EM_ASM("Module.canvas.focus();")` after every fullscreen
// transition so keyboard events keep targeting the canvas.
extern fn void rgfwJsFocusCanvas() @wasm("env", "emscripten_focus_canvas");
extern fn void rgfwJsBlitSurface(char* data, int w, int h, int minH) @wasm("env", "emscripten_blit_surface");
extern fn void rgfwJsSetCursor(ZString name, sz len) @wasm("env", "emscripten_set_cursor");
extern fn void rgfwJsHideCursor() @wasm("env", "emscripten_hide_cursor");
extern fn int rgfwJsGetMouseX() @wasm("env", "emscripten_get_mouse_x");
extern fn int rgfwJsGetMouseY() @wasm("env", "emscripten_get_mouse_y");
extern fn void rgfwJsSetPointerEvents(int passthrough) @wasm("env", "emscripten_set_pointer_events");
extern fn void rgfwJsWriteClipboard(char* data, sz len) @wasm("env", "emscripten_write_clipboard");
extern fn int rgfwJsUsingAsyncify() @wasm("env", "emscripten_using_asyncify");
extern fn void rgfwJsSleep(int ms) @wasm("env", "emscripten_sleep");
extern fn double rgfwJsDevicePixelRatio() @wasm("env", "emscripten_device_pixel_ratio");
extern fn int rgfwJsInnerWidth() @wasm("env", "emscripten_inner_width");
extern fn int rgfwJsInnerHeight() @wasm("env", "emscripten_inner_height");
extern fn void rgfwJsRequestPointerLock() @wasm("env", "emscripten_request_pointer_lock");
extern fn void rgfwJsExitPointerLock() @wasm("env", "emscripten_exit_pointer_lock");
extern fn void rgfwJsRequestFullscreen() @wasm("env", "emscripten_request_fullscreen");
extern fn void rgfwJsExitFullscreen() @wasm("env", "emscripten_exit_fullscreen");
extern fn void rgfwJsSetOpacity(float opacity) @wasm("env", "emscripten_set_opacity");
extern fn int rgfwJsWebglCreateContext(GfxGlHints* hints) @wasm("env", "emscripten_webgl_create_context");
extern fn void rgfwJsWebglDestroyContext(int ctx) @wasm("env", "emscripten_webgl_destroy_context");
extern fn void rgfwJsWebglMakeCurrent(int ctx) @wasm("env", "emscripten_webgl_make_current");
extern fn void rgfwJsWebglCommitFrame() @wasm("env", "emscripten_webgl_commit_frame");
extern fn int rgfwJsWebglGetCurrentContext() @wasm("env", "emscripten_webgl_get_current_context");
extern fn int rgfwJsExtensionSupported(ZString name, sz len) @wasm("env", "emscripten_extension_supported");
extern fn GfxFn rgfwJsGetProcAddress(ZString name) @wasm("env", "emscripten_webgl_get_proc_address");
extern fn void rgfwJsMkdir(ZString name) @wasm("env", "emscripten_mkdir");
extern fn void rgfwJsWriteFile(ZString path, char* data, sz len) @wasm("env", "emscripten_write_file");

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
fn GfxKey todo_fix_wasmPhysicalToRGFW(uint hash)
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

// private
fn void rgfwEmscriptenOnResize(int w, int h) @export("c3gfx_on_resize")
{
	_window_resized_callback(_gfx_ctx.root, { w, h });
}

// private

// private
fn void rgfwEmscriptenOnFocusIn() @export("c3gfx_on_focusin") { todo_fix_windowFocusCallback(_gfx_ctx.root, true); }
// private
fn void rgfwEmscriptenOnFocusOut() @export("c3gfx_on_focusout") { todo_fix_windowFocusCallback(_gfx_ctx.root, false); }

// private
fn void rgfwEmscriptenOnMouseMove(int x, int y, int movementX, int movementY) @export("c3gfx_on_mousemove")
{
	_mouse_motion_callback(_gfx_ctx.root, { x, y });
	_raw_motion_callback(_gfx_ctx.root, { (float)movementX, (float)movementY });
}

// private
fn void rgfwEmscriptenOnMouseUp(int button) @export("c3gfx_on_mouseup")
{
	if (button > 2) button += 2;
	todo_fix_mouseButtonCallback(_gfx_ctx.root, (GfxMouseButton)button, false);
}

// private
fn void rgfwEmscriptenOnWheel(float deltaX, float deltaY) @export("c3gfx_on_wheel") { todo_fix_mouseScrollCallback(_gfx_ctx.root, { deltaX, deltaY }); }

// private
fn void rgfwEmscriptenOnTouch(int numTouches, int* xs, int* ys, char down) @export("c3gfx_on_touch")
{
	// down == 255 is the sentinel for touchmove (no button state change).
	if (down == 255)
	{
		if (!(_gfx_ctx.root.internal.enabled_events & TODO_RENAME_MOUSE_MOTION_FLAG)) return;
		for (int i = 0; i < numTouches; i++)
		{
			_mouse_motion_callback(_gfx_ctx.root, { xs[i], ys[i] });
			_raw_motion_callback(_gfx_ctx.root, {});
		}
		return;
	}

	if (down)
	{
		if (!(_gfx_ctx.root.internal.enabled_events & TODO_RENAME_MOUSE_BUTTON_PRESSED_FLAG)) return;
	}
	else
	{
		if (!(_gfx_ctx.root.internal.enabled_events & TODO_RENAME_MOUSE_BUTTON_RELEASED_FLAG)) return;
	}

	for (int i = 0; i < numTouches; i++)
	{
		_mouse_motion_callback(_gfx_ctx.root, { xs[i], ys[i] });
		_raw_motion_callback(_gfx_ctx.root, { });
		todo_fix_mouseButtonCallback(_gfx_ctx.root, LEFT, (bool)down);
	}
}

// private
fn void todo_fix_handleKeyEvent(ZString code, uint codepoint, bool press) @export("c3gfx_handleKeyEvent")
{
	ZString iCode = code;
	uint hash = 0;
	while (iCode[0] != 0) { hash = ((hash ^ 0x7E057D79u) << 3) ^ (uint)iCode[0]; iCode++; }

	GfxKey physicalKey = todo_fix_wasmPhysicalToRGFW(hash);
	todo_fix_keyCallback(_gfx_ctx.root, physicalKey, _gfx_ctx.root.internal.mod, is_key_down(physicalKey) && press, press);
	if (press) todo_fix_keyCharCallback(_gfx_ctx.root, codepoint);
}

// private
fn void todo_fix_handleKeyMods(bool capital, bool numlock, bool control, bool alt, bool shift, bool super, bool scroll) @export("c3gfx_handleKeyMods")
{
	_key_update_key_mods_ex(_gfx_ctx.root, capital, numlock, control, alt, shift, super, scroll);
}

// private

// public
fn void todo_fix_stopCheckEvents() { _gfx_ctx.stopCheckEvents_bool = true; }



// public
fn void todo_fix_window_blitSurface(GfxWindow* win, GfxSurface* surface)
{
	int minH = win.height < surface.size.y ? win.height : surface.size.y;
	copy_image_data(surface.data, surface.size.x, minH, RGBA8, surface.data, surface.format, surface.convert_fn);
	rgfwJsBlitSurface(surface.data, surface.size.x, surface.size.y, minH);
}

// public
fn void GfxSurface.release(&self) { *self = {}; }

// public
fn void todo_fix_initKeycodesPlatform()
{
	_gfx_ctx.keycodes[DOM_VK_BACK_QUOTE] = BACKTICK;
	_gfx_ctx.keycodes[DOM_VK_0] = NUM0; _gfx_ctx.keycodes[DOM_VK_1] = NUM1; _gfx_ctx.keycodes[DOM_VK_2] = NUM2;
	_gfx_ctx.keycodes[DOM_VK_3] = NUM3; _gfx_ctx.keycodes[DOM_VK_4] = NUM4; _gfx_ctx.keycodes[DOM_VK_5] = NUM5;
	_gfx_ctx.keycodes[DOM_VK_6] = NUM6; _gfx_ctx.keycodes[DOM_VK_7] = NUM7; _gfx_ctx.keycodes[DOM_VK_8] = NUM8;
	_gfx_ctx.keycodes[DOM_VK_9] = NUM9; _gfx_ctx.keycodes[DOM_VK_SPACE] = SPACE;
	_gfx_ctx.keycodes[DOM_VK_A] = A; _gfx_ctx.keycodes[DOM_VK_B] = B; _gfx_ctx.keycodes[DOM_VK_C] = C;
	_gfx_ctx.keycodes[DOM_VK_D] = D; _gfx_ctx.keycodes[DOM_VK_E] = E; _gfx_ctx.keycodes[DOM_VK_F] = F;
	_gfx_ctx.keycodes[DOM_VK_G] = G; _gfx_ctx.keycodes[DOM_VK_H] = H; _gfx_ctx.keycodes[DOM_VK_I] = I;
	_gfx_ctx.keycodes[DOM_VK_J] = J; _gfx_ctx.keycodes[DOM_VK_K] = K; _gfx_ctx.keycodes[DOM_VK_L] = L;
	_gfx_ctx.keycodes[DOM_VK_M] = M; _gfx_ctx.keycodes[DOM_VK_N] = N; _gfx_ctx.keycodes[DOM_VK_O] = O;
	_gfx_ctx.keycodes[DOM_VK_P] = P; _gfx_ctx.keycodes[DOM_VK_Q] = Q; _gfx_ctx.keycodes[DOM_VK_R] = R;
	_gfx_ctx.keycodes[DOM_VK_S] = S; _gfx_ctx.keycodes[DOM_VK_T] = T; _gfx_ctx.keycodes[DOM_VK_U] = U;
	_gfx_ctx.keycodes[DOM_VK_V] = V; _gfx_ctx.keycodes[DOM_VK_W] = W; _gfx_ctx.keycodes[DOM_VK_X] = X;
	_gfx_ctx.keycodes[DOM_VK_Y] = Y; _gfx_ctx.keycodes[DOM_VK_Z] = Z;
	_gfx_ctx.keycodes[DOM_VK_PERIOD] = PERIOD; _gfx_ctx.keycodes[DOM_VK_COMMA] = COMMA;
	_gfx_ctx.keycodes[DOM_VK_SLASH] = SLASH; _gfx_ctx.keycodes[DOM_VK_OPEN_BRACKET] = BRACKET;
	_gfx_ctx.keycodes[DOM_VK_CLOSE_BRACKET] = CLOSE_BRACKET; _gfx_ctx.keycodes[DOM_VK_SEMICOLON] = SEMICOLON;
	_gfx_ctx.keycodes[DOM_VK_QUOTE] = APOSTROPHE; _gfx_ctx.keycodes[DOM_VK_BACK_SLASH] = BACK_SLASH;
	_gfx_ctx.keycodes[DOM_VK_RETURN] = RETURN; _gfx_ctx.keycodes[DOM_VK_DELETE] = DELETE;
	_gfx_ctx.keycodes[DOM_VK_NUM_LOCK] = NUM_LOCK; _gfx_ctx.keycodes[DOM_VK_DIVIDE] = PAD_SLASH;
	_gfx_ctx.keycodes[DOM_VK_MULTIPLY] = PAD_MULTIPLY; _gfx_ctx.keycodes[DOM_VK_SUBTRACT] = PAD_MINUS;
	_gfx_ctx.keycodes[DOM_VK_NUMPAD1] = PAD1; _gfx_ctx.keycodes[DOM_VK_NUMPAD2] = PAD2;
	_gfx_ctx.keycodes[DOM_VK_NUMPAD3] = PAD3; _gfx_ctx.keycodes[DOM_VK_NUMPAD4] = PAD4;
	_gfx_ctx.keycodes[DOM_VK_NUMPAD5] = PAD5; _gfx_ctx.keycodes[DOM_VK_NUMPAD6] = PAD6;
	_gfx_ctx.keycodes[DOM_VK_NUMPAD9] = PAD9; _gfx_ctx.keycodes[DOM_VK_NUMPAD0] = PAD0;
	_gfx_ctx.keycodes[DOM_VK_DECIMAL] = PAD_PERIOD; _gfx_ctx.keycodes[DOM_VK_HYPHEN_MINUS] = MINUS;
	_gfx_ctx.keycodes[DOM_VK_EQUALS] = EQUALS; _gfx_ctx.keycodes[DOM_VK_BACK_SPACE] = BACK_SPACE;
	_gfx_ctx.keycodes[DOM_VK_TAB] = TAB; _gfx_ctx.keycodes[DOM_VK_CAPS_LOCK] = CAPS_LOCK;
	_gfx_ctx.keycodes[DOM_VK_SHIFT] = SHIFT_L; _gfx_ctx.keycodes[DOM_VK_CONTROL] = CONTROL_L;
	_gfx_ctx.keycodes[DOM_VK_ALT] = ALT_L; _gfx_ctx.keycodes[DOM_VK_META] = SUPER_L;
	_gfx_ctx.keycodes[DOM_VK_F1] = F1; _gfx_ctx.keycodes[DOM_VK_F2] = F2; _gfx_ctx.keycodes[DOM_VK_F3] = F3;
	_gfx_ctx.keycodes[DOM_VK_F4] = F4; _gfx_ctx.keycodes[DOM_VK_F5] = F5; _gfx_ctx.keycodes[DOM_VK_F6] = F6;
	_gfx_ctx.keycodes[DOM_VK_F7] = F7; _gfx_ctx.keycodes[DOM_VK_F8] = F8; _gfx_ctx.keycodes[DOM_VK_F9] = F9;
	_gfx_ctx.keycodes[DOM_VK_F10] = F10; _gfx_ctx.keycodes[DOM_VK_F11] = F11; _gfx_ctx.keycodes[DOM_VK_F12] = F12;
	_gfx_ctx.keycodes[DOM_VK_UP] = UP; _gfx_ctx.keycodes[DOM_VK_DOWN] = DOWN;
	_gfx_ctx.keycodes[DOM_VK_LEFT] = LEFT; _gfx_ctx.keycodes[DOM_VK_RIGHT] = RIGHT;
	_gfx_ctx.keycodes[DOM_VK_INSERT] = INSERT; _gfx_ctx.keycodes[DOM_VK_END] = END;
	_gfx_ctx.keycodes[DOM_VK_PAGE_UP] = PAGE_UP; _gfx_ctx.keycodes[DOM_VK_PAGE_DOWN] = PAGE_DOWN;
	_gfx_ctx.keycodes[DOM_VK_ESCAPE] = ESCAPE; _gfx_ctx.keycodes[DOM_VK_HOME] = HOME;
	_gfx_ctx.keycodes[DOM_VK_SCROLL_LOCK] = SCROLL_LOCK; _gfx_ctx.keycodes[DOM_VK_PRINTSCREEN] = PRINT_SCREEN;
	_gfx_ctx.keycodes[DOM_VK_PAUSE] = PAUSE;
	_gfx_ctx.keycodes[DOM_VK_F13] = F13; _gfx_ctx.keycodes[DOM_VK_F14] = F14; _gfx_ctx.keycodes[DOM_VK_F15] = F15;
	_gfx_ctx.keycodes[DOM_VK_F16] = F16; _gfx_ctx.keycodes[DOM_VK_F17] = F17; _gfx_ctx.keycodes[DOM_VK_F18] = F18;
	_gfx_ctx.keycodes[DOM_VK_F19] = F19; _gfx_ctx.keycodes[DOM_VK_F20] = F20; _gfx_ctx.keycodes[DOM_VK_F21] = F21;
	_gfx_ctx.keycodes[DOM_VK_F22] = F22; _gfx_ctx.keycodes[DOM_VK_F23] = F23; _gfx_ctx.keycodes[DOM_VK_F24] = F24;
}

// public
fn int todo_fix_initPlatform(String class_name, GfxInitOption flags)
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
	if (node != null)
	{
		_gfx_ctx.monitors.primary = node;
		todo_fix_monitorCallback(_gfx_ctx.root, &node.mon, true);
	}
	return 0;
}

// public
fn void? todo_fix_createWindowPlatform(String name, GfxWindowFlags flags, GfxWindow* win)
{
	rgfwJsSetCanvasSize(win.width, win.height);
	rgfwJsSetWindowTitle((ZString)name, name.len);
	rgfwJsSetupWindowListeners();

	if (flags.allow_dnd) win.internal.flags.allow_dnd = true;

	rgfwJsSetupDropListeners();
}

// public
fn GfxKey todo_fix_physicalToMappedKey(GfxKey key) => key;
// public
fn GfxPoint _window_fetch_size_native(GfxWindow* win) => win.get_size();

// public
fn void todo_fix_pollEvents()
{
	if (_gfx_ctx.usingAsyncify == -1) _gfx_ctx.usingAsyncify = rgfwJsUsingAsyncify();
	todo_fix_resetPrevState();
	if (_gfx_ctx.usingAsyncify) rgfwJsSleep(0);
}

// public

// public
fn TodoRenameMouse* todo_fix_createMouseStandard(GfxMouseIcon mouse)
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
	return (TodoRenameMouse*)cursorName;
}

// public
// public
fn TodoRenameMouse* todo_fix_createMouse(char* data, int w, int h, GfxFormat format) => null;

// public
fn bool _window_set_mouse_native(GfxWindow* win, TodoRenameMouse* mouse)
{
	ZString name = (ZString)mouse;
	rgfwJsSetCursor(name, name.len());
	return true;
}

// public


// public
fn void todo_fix_window_setMousePassthrough(GfxWindow* win, char passthrough) { rgfwJsSetPointerEvents(passthrough); }

// public
fn bool todo_fix_writeClipboard(GfxDataTransfer* data)
{
	rgfwJsWriteClipboard(data.data, data.length);
	return true;
}

// public
fn bool todo_fix_readClipboardPtr(GfxDataTransferType requestedType, char* buffer, sz capacity, GfxDataTransfer* data)
{
	if (requestedType != TEXT) return false;
	return false;
}

// public


// public
fn void todo_fix_window_makeCurrentContext_OpenGL(GfxWindow* win)
{
	if (win == null) { rgfwJsWebglMakeCurrent(0); }
	else { rgfwJsWebglMakeCurrent((int)(uptr)win.src.ctx.native.ctx); }
}

// public
fn void todo_fix_window_swapBuffers_OpenGL(GfxWindow* win) { rgfwJsWebglCommitFrame(); }
// public
fn void* todo_fix_getCurrentContext_OpenGL() => (void*)(uptr)rgfwJsWebglGetCurrentContext();
// public
fn bool todo_fix_extensionSupportedPlatform_OpenGL(ZString extension, sz len) => rgfwJsExtensionSupported(extension, len) != 0;
// public
fn GfxFn _get_opengl_proc_native(ZString procname) => rgfwJsGetProcAddress(procname);

// public
// public
fn void todo_fix_window_closePlatform(GfxWindow* win) {}

// public
fn void _window_set_raw_mouse_mode_native(GfxWindow* win, bool state) {}
// public
fn void _window_set_capture_mouse_native(GfxWindow* win, bool state)
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
fn void todo_fix_window_setOpacity(GfxWindow* win, char opacity) { rgfwJsSetOpacity((float)opacity / 255.0f); }

// public
fn GfxMonitor* find_monitor(GfxWindow* win) => get_primary_monitor() ?? null;

// ---- unsupported on WASM (match the original's no-op stubs) ----
// public
fn void todo_fix_pollMonitors() {}
// public
fn void todo_fix_monitorNode_free(GfxMonitorNode* node) {}
// public

// public
fn void todo_fix_window_raise(GfxWindow* win) {}
// public
fn bool todo_fix_monitor_requestMode(GfxMonitor* mon, GfxMonitorMode* mode, GfxMonitorModeRequest request) => false;
// public
fn bool todo_fix_monitor_getWorkarea(GfxMonitor* monitor, int* x, int* y, int* width, int* height) => false;
// public
fn sz todo_fix_monitor_getGammaRampPtr(GfxMonitor* monitor, TodoRenameGammaRamp* ramp) => 0;
// public
fn bool todo_fix_monitor_setGammaRamp(GfxMonitor* monitor, TodoRenameGammaRamp* ramp) => false;
// public
// public
fn bool todo_fix_monitor_setMode(GfxMonitor* mon, GfxMonitorMode* mode) => false;

fn void todo_fix_window_minimizePlatform(GfxWindow* win) {}
fn void todo_fix_window_setFloating(GfxWindow* win, bool floating) {}
fn void todo_fix_waitForEvent(int waitMS) {}

