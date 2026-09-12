module std::gfx @feat(WASM);
import std::math;


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
fn void rgfwEmscriptenOnFocusIn() @export("c3gfx_on_focusin") { _window_focus_callback(_gfx_ctx.root, true); }
// private
fn void rgfwEmscriptenOnFocusOut() @export("c3gfx_on_focusout") { _window_focus_callback(_gfx_ctx.root, false); }

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
	_mouse_button_callback(_gfx_ctx.root, (GfxMouseButton)button, false);
}

// private
fn void rgfwEmscriptenOnWheel(float deltaX, float deltaY) @export("c3gfx_on_wheel") { _mouse_scroll_callback(_gfx_ctx.root, { deltaX, deltaY }); }

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
		_mouse_button_callback(_gfx_ctx.root, LEFT, (bool)down);
	}
}

// private
fn void todo_fix_handleKeyEvent(ZString code, uint codepoint, bool press) @export("c3gfx_handleKeyEvent")
{
	ZString iCode = code;
	uint hash = 0;
	while (iCode[0] != 0) { hash = ((hash ^ 0x7E057D79u) << 3) ^ (uint)iCode[0]; iCode++; }

	GfxKey physicalKey = todo_fix_wasmPhysicalToRGFW(hash);
	_key_callback(_gfx_ctx.root, physicalKey, _gfx_ctx.root.internal.mod, is_key_down(physicalKey) && press, press);
	if (press) _key_char_callback(_gfx_ctx.root, codepoint);
}

// private
fn void todo_fix_handleKeyMods(bool capital, bool numlock, bool control, bool alt, bool shift, bool super, bool scroll) @export("c3gfx_handleKeyMods")
{
	_key_update_key_mods_ex(_gfx_ctx.root, capital, numlock, control, alt, shift, super, scroll);
}


fn GfxMouse* todo_fix_createMouse(char* data, int w, int h, GfxFormat format) => null;


fn void todo_fix_monitorNode_free(GfxMonitorNode* node) {}

// public




