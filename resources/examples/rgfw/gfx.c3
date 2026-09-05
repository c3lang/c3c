module std::gfx;
import std::math;

macro bool rgfw_bool(x) => x ? (bool)1 : (bool)0;
macro uint rgfw_bit(x) => (uint)1 << (uint)(x);
macro void* rgfw_memcpy(void* dest, void* src, sz count) => memcpy(dest, src, count);
macro void* rgfw_memzero(void* ptr, sz count) => memset(ptr, 0, count);
macro float rgfw_min(float a, float b) => a < b ? a : b;
fn void* rgfw_alloc_impl(sz size) => malloc(size);
fn void rgfw_free_impl(void* ptr) { if (ptr != null) free(ptr); }

const MAX_EVENTS = 32;
const PREALLOCATED_MONITORS = 0;

alias RgfwInitFlags = char;
alias RgfwKey = char;
alias RgfwMouseButton = char;
alias RgfwKeymod = char;
alias RgfwDndActionType = char;
alias RgfwDataTransferType = char;
alias RgfwEventType = char;
alias RgfwEventFlag = uint;
alias RgfwEventWait = int;
alias RgfwWindowFlags = uint;
alias RgfwIcon = char;
alias RgfwMouseIcon = char;
alias RgfwDebugType = char;
alias RgfwErrorCode = char;
alias RgfwGlReleaseBehavior = int;
alias RgfwGlProfile = int;
alias RgfwGlRenderer = int;
alias RgfwGfxContextType = uint;

typedef RgfwMouse = void;
alias RgfwProc = fn void();
alias GfxConvertImageDataFn = fn void(char* dest_data, char* src_data, RgfwColorLayout* srcLayout, RgfwColorLayout* destLayout, sz count);
alias GfxGenericFunc = fn void(GfxEvent* e);
alias RgfwDebugFunc = fn void(RgfwDebugInfo* info);

struct RgfwColorLayout { int r, g, b, a; int channels; }

struct RgfwGammaRamp
{
	ushort* red;
	ushort* green;
	ushort* blue;
	sz count;
}

struct GfxSurface
{
	char* data;
	int w, h;
	GfxFormat format;
	GfxConvertImageDataFn convertFunc;
	RgfwNativeImage native;
}

struct GfxMonitorMode
{
	int w, h;
	float refresh_rate;
	char red, blue, green;
	void* src;
}

struct GfxMonitor
{
	IntVec2 origin;
	char[128] name;
	float scaleX, scaleY;
	float pixelRatio;
	float physW, physH;
	GfxMonitorMode mode;
	void* userPtr;
	RgfwMonitorNode* node;
}

struct RgfwKeyState
{
	bool current;
	bool prev;
}

struct GfxWindow
{
	RgfwWindowSrc src;
	GfxWindowInternal internal;
	void* userPtr;
	int x, y, w, h;
}

struct GfxWindowInternal
{
	RgfwKey exitKey;
	int lastMouseX, lastMouseY;
	bool shouldClose;
	bool rawMouse;
	bool captureMouse;
	bool inFocus;
	bool mouseInside;
	RgfwKeymod mod;
	RgfwEventFlag enabledEvents;
	uint flags;
	int oldX, oldY, oldW, oldH;
	GfxMonitorMode oldMode;
	RgfwMouse* mouse;
}


struct RgfwWindowState
{
	bool mouseEnter;
	bool dataDragging;
	bool dataDrop;
	sz dataLength;
	int dropX, dropY;
	GfxWindow* win;
	bool mouseLeave;
	GfxWindow* winLeave;
}

struct GfxMonitors
{
	GfxMonitorList list;
	sz count;
	RgfwMonitorNode* primary;
}

struct GfxMonitorList
{
	RgfwMonitorNode* head;
	RgfwMonitorNode* cur;
}

struct GfxDataTransfer
{
	char* data;
	sz length;
	RgfwDataTransferType type;
}

struct GfxDataDropNode
{
	char* data;
	sz length;
	RgfwDataTransferType type;
	GfxDataDropNode* next;
}

struct RgfwCommonEvent { RgfwEventType type; GfxWindow* win; }
struct RgfwWindowFocusEvent { RgfwEventType type; GfxWindow* win; bool state; }
struct RgfwMouseButtonEvent { RgfwEventType type; GfxWindow* win; RgfwMouseButton value; bool state; }
struct RgfwMouseDeltaEvent { RgfwEventType type; GfxWindow* win; float x, y; }
struct RgfwMouseMotionEvent { RgfwEventType type; GfxWindow* win; int x, y; bool inWindow; }
struct RgfwKeyEvent { RgfwEventType type; GfxWindow* win; RgfwKey value; bool repeat; RgfwKeymod mod; bool state; }
struct RgfwKeyCharEvent { RgfwEventType type; GfxWindow* win; uint value; }
struct RgfwDataDropEvent { RgfwEventType type; GfxWindow* win; GfxDataDropNode* value; }
struct RgfwDataDragEvent { RgfwEventType type; GfxWindow* win; int x, y; RgfwDndActionType action; RgfwDataTransferType dataType; }
struct RgfwScaleUpdatedEvent { RgfwEventType type; GfxWindow* win; float x, y; }
struct RgfwMonitorEvent { RgfwEventType type; GfxWindow* win; GfxMonitor* monitor; bool state; }
struct RgfwWindowUpdateEvent { RgfwEventType type; GfxWindow* win; int x, y, w, h; }

union GfxEvent
{
	RgfwEventType type;
	RgfwCommonEvent common;
	RgfwWindowFocusEvent focus;
	RgfwWindowUpdateEvent update;
	RgfwMouseButtonEvent button;
	RgfwMouseDeltaEvent delta;
	RgfwMouseMotionEvent mouse;
	RgfwKeyEvent key;
	RgfwKeyCharEvent keyChar;
	RgfwDataDropEvent drop;
	RgfwDataDragEvent drag;
	RgfwScaleUpdatedEvent scale;
	RgfwMonitorEvent monitor;
}

struct RgfwCallbacks
{
	GfxGenericFunc[25] arr;
}

struct RgfwDebugInfo
{
	RgfwDebugType type;
	RgfwErrorCode code;
	char* msg;
}

constset GfxWindowIconType
{
	TASKBAR = 1,
	WINDOW = 2,
	BOTH = 3,
}

constset GfxMonitorModeRequest : char
{
	SCALE   = 0b001,
	REFRESH = 0b010,
	RGB     = 0b100,
	ALL     = 0b111,
}

const RgfwInitFlags RGFW_INIT_OPEN_GL = 1;
const RgfwInitFlags RGFW_INIT_EGL = 2;
const RgfwInitFlags RGFW_INIT_VULKAN = 4;
const RgfwInitFlags RGFW_INIT_X11 = 8;

enum GfxFormat : char (int channels, RgfwColorLayout layout)
{
	RGB8  { 3, { 0, 1, 2, 3, 3 } },
	BGR8  { 3, { 2, 1, 0, 3, 3 } },
	RGBA8 { 4, { 0, 1, 2, 3, 4 } },
	ARGB8 { 4, { 1, 2, 3, 0, 4 } },
	BGRA8 { 4, { 2, 1, 0, 3, 4 } },
	ABGR8 { 4, { 3, 2, 1, 0, 4 } },
}

const RgfwKey RGFW_KEY_NULL = 0;
const RgfwKey RGFW_KEY_ESCAPE = 27;
const RgfwKey RGFW_KEY_BACKTICK = '`';
const RgfwKey RGFW_KEY0 = '0';
const RgfwKey RGFW_KEY1 = '1';
const RgfwKey RGFW_KEY2 = '2';
const RgfwKey RGFW_KEY3 = '3';
const RgfwKey RGFW_KEY4 = '4';
const RgfwKey RGFW_KEY5 = '5';
const RgfwKey RGFW_KEY6 = '6';
const RgfwKey RGFW_KEY7 = '7';
const RgfwKey RGFW_KEY8 = '8';
const RgfwKey RGFW_KEY9 = '9';
const RgfwKey RGFW_KEY_MINUS = '-';
const RgfwKey RGFW_KEY_EQUAL = '=';
const RgfwKey RGFW_KEY_EQUALS = '=';
const RgfwKey RGFW_KEY_BACK_SPACE = 8;
const RgfwKey RGFW_KEY_TAB = 9;
const RgfwKey RGFW_KEY_SPACE = ' ';
const RgfwKey RGFW_KEY_A = 'a';
const RgfwKey RGFW_KEY_B = 'b';
const RgfwKey RGFW_KEY_C = 'c';
const RgfwKey RGFW_KEY_D = 'd';
const RgfwKey RGFW_KEY_E = 'e';
const RgfwKey RGFW_KEY_F = 'f';
const RgfwKey RGFW_KEY_G = 'g';
const RgfwKey RGFW_KEY_H = 'h';
const RgfwKey RGFW_KEY_I = 'i';
const RgfwKey RGFW_KEY_J = 'j';
const RgfwKey RGFW_KEY_K = 'k';
const RgfwKey RGFW_KEY_L = 'l';
const RgfwKey RGFW_KEY_M = 'm';
const RgfwKey RGFW_KEY_N = 'n';
const RgfwKey RGFW_KEY_O = 'o';
const RgfwKey RGFW_KEY_P = 'p';
const RgfwKey RGFW_KEY_Q = 'q';
const RgfwKey RGFW_KEY_R = 'r';
const RgfwKey RGFW_KEY_S = 's';
const RgfwKey RGFW_KEY_T = 't';
const RgfwKey RGFW_KEY_U = 'u';
const RgfwKey RGFW_KEY_V = 'v';
const RgfwKey RGFW_KEY_W = 'w';
const RgfwKey RGFW_KEY_X = 'x';
const RgfwKey RGFW_KEY_Y = 'y';
const RgfwKey RGFW_KEY_Z = 'z';
const RgfwKey RGFW_KEY_PERIOD = '.';
const RgfwKey RGFW_KEY_COMMA = ',';
const RgfwKey RGFW_KEY_SLASH = '/';
const RgfwKey RGFW_KEY_BRACKET = '[';
const RgfwKey RGFW_KEY_CLOSE_BRACKET = ']';
const RgfwKey RGFW_KEY_SEMICOLON = ';';
const RgfwKey RGFW_KEY_APOSTROPHE = '\'';
const RgfwKey RGFW_KEY_BACK_SLASH = '\\';
const RgfwKey RGFW_KEY_RETURN = 10;
const RgfwKey RGFW_KEY_ENTER = 10;
const RgfwKey RGFW_KEY_DELETE = 127;
const RgfwKey RGFW_KEY_F1 = 128;
const RgfwKey RGFW_KEY_F2 = 129;
const RgfwKey RGFW_KEY_F3 = 130;
const RgfwKey RGFW_KEY_F4 = 131;
const RgfwKey RGFW_KEY_F5 = 132;
const RgfwKey RGFW_KEY_F6 = 133;
const RgfwKey RGFW_KEY_F7 = 134;
const RgfwKey RGFW_KEY_F8 = 135;
const RgfwKey RGFW_KEY_F9 = 136;
const RgfwKey RGFW_KEY_F10 = 137;
const RgfwKey RGFW_KEY_F11 = 138;
const RgfwKey RGFW_KEY_F12 = 139;
const RgfwKey RGFW_KEY_F13 = 140;
const RgfwKey RGFW_KEY_F14 = 141;
const RgfwKey RGFW_KEY_F15 = 142;
const RgfwKey RGFW_KEY_F16 = 143;
const RgfwKey RGFW_KEY_F17 = 144;
const RgfwKey RGFW_KEY_F18 = 145;
const RgfwKey RGFW_KEY_F19 = 146;
const RgfwKey RGFW_KEY_F20 = 147;
const RgfwKey RGFW_KEY_F21 = 148;
const RgfwKey RGFW_KEY_F22 = 149;
const RgfwKey RGFW_KEY_F23 = 150;
const RgfwKey RGFW_KEY_F24 = 151;
const RgfwKey RGFW_KEY_F25 = 152;
const RgfwKey RGFW_KEY_CAPS_LOCK = 153;
const RgfwKey RGFW_KEY_SHIFT_L = 154;
const RgfwKey RGFW_KEY_CONTROL_L = 155;
const RgfwKey RGFW_KEY_ALT_L = 156;
const RgfwKey RGFW_KEY_SUPER_L = 157;
const RgfwKey RGFW_KEY_SHIFT_R = 158;
const RgfwKey RGFW_KEY_CONTROL_R = 159;
const RgfwKey RGFW_KEY_ALT_R = 160;
const RgfwKey RGFW_KEY_SUPER_R = 161;
const RgfwKey RGFW_KEY_UP = 162;
const RgfwKey RGFW_KEY_DOWN = 163;
const RgfwKey RGFW_KEY_LEFT = 164;
const RgfwKey RGFW_KEY_RIGHT = 165;
const RgfwKey RGFW_KEY_INSERT = 166;
const RgfwKey RGFW_KEY_MENU = 167;
const RgfwKey RGFW_KEY_END = 168;
const RgfwKey RGFW_KEY_HOME = 169;
const RgfwKey RGFW_KEY_PAGE_UP = 170;
const RgfwKey RGFW_KEY_PAGE_DOWN = 171;
const RgfwKey RGFW_KEY_NUM_LOCK = 172;
const RgfwKey RGFW_KEY_PAD_SLASH = 173;
const RgfwKey RGFW_KEY_PAD_MULTIPLY = 174;
const RgfwKey RGFW_KEY_PAD_PLUS = 175;
const RgfwKey RGFW_KEY_PAD_MINUS = 176;
const RgfwKey RGFW_KEY_PAD_EQUAL = 177;
const RgfwKey RGFW_KEY_PAD_EQUALS = 177;
const RgfwKey RGFW_KEY_PAD1 = 178;
const RgfwKey RGFW_KEY_PAD2 = 179;
const RgfwKey RGFW_KEY_PAD3 = 180;
const RgfwKey RGFW_KEY_PAD4 = 181;
const RgfwKey RGFW_KEY_PAD5 = 182;
const RgfwKey RGFW_KEY_PAD6 = 183;
const RgfwKey RGFW_KEY_PAD7 = 184;
const RgfwKey RGFW_KEY_PAD8 = 185;
const RgfwKey RGFW_KEY_PAD9 = 186;
const RgfwKey RGFW_KEY_PAD0 = 187;
const RgfwKey RGFW_KEY_PAD_PERIOD = 188;
const RgfwKey RGFW_KEY_PAD_RETURN = 189;
const RgfwKey RGFW_KEY_SCROLL_LOCK = 190;
const RgfwKey RGFW_KEY_PRINT_SCREEN = 191;
const RgfwKey RGFW_KEY_PAUSE = 192;
const RgfwKey RGFW_KEY_WORLD1 = 193;
const RgfwKey RGFW_KEY_WORLD2 = 194;

const RGFW_KEY_LAST = 256;

const RgfwMouseButton RGFW_MOUSE_LEFT = 0;
const RgfwMouseButton RGFW_MOUSE_MIDDLE = 1;
const RgfwMouseButton RGFW_MOUSE_RIGHT = 2;
const RgfwMouseButton RGFW_MOUSE_MISC1 = 3;
const RgfwMouseButton RGFW_MOUSE_MISC2 = 4;
const RgfwMouseButton RGFW_MOUSE_MISC3 = 5;
const RgfwMouseButton RGFW_MOUSE_MISC4 = 6;
const RgfwMouseButton RGFW_MOUSE_MISC5 = 7;
const RgfwMouseButton RGFW_MOUSE_FINAL = 8;

const RgfwKeymod RGFW_MOD_CAPS_LOCK = 1;
const RgfwKeymod RGFW_MOD_NUM_LOCK = 2;
const RgfwKeymod RGFW_MOD_CONTROL = 4;
const RgfwKeymod RGFW_MOD_ALT = 8;
const RgfwKeymod RGFW_MOD_SHIFT = 16;
const RgfwKeymod RGFW_MOD_SUPER = 32;
const RgfwKeymod RGFW_MOD_SCROLL_LOCK = 64;

const RgfwDndActionType RGFW_DND_ACTION_NONE = 0;
const RgfwDndActionType RGFW_DND_ACTION_ENTER = 1;
const RgfwDndActionType RGFW_DND_ACTION_MOVE = 2;
const RgfwDndActionType RGFW_DND_ACTION_EXIT = 3;

const RgfwDataTransferType RGFW_DATA_NONE = 0;
const RgfwDataTransferType RGFW_DATA_TEXT = 1;
const RgfwDataTransferType RGFW_DATA_FILE = 2;
const RgfwDataTransferType RGFW_DATA_URL = 3;
const RgfwDataTransferType RGFW_DATA_IMAGE = 4;
const RgfwDataTransferType RGFW_DATA_UNKNOWN = 5;

const RgfwEventType RGFW_EVENT_NONE = 0;
const RgfwEventType RGFW_KEY_PRESSED = 1;
const RgfwEventType RGFW_KEY_RELEASED = 2;
const RgfwEventType RGFW_KEY_CHAR = 3;
const RgfwEventType RGFW_MOUSE_BUTTON_PRESSED = 4;
const RgfwEventType RGFW_MOUSE_BUTTON_RELEASED = 5;
const RgfwEventType RGFW_MOUSE_SCROLL = 6;
const RgfwEventType RGFW_MOUSE_MOTION = 7;
const RgfwEventType RGFW_MOUSE_RAW_MOTION = 8;
const RgfwEventType RGFW_MOUSE_ENTER = 9;
const RgfwEventType RGFW_MOUSE_LEAVE = 10;
const RgfwEventType RGFW_WINDOW_MOVED = 11;
const RgfwEventType RGFW_WINDOW_RESIZED = 12;
const RgfwEventType RGFW_WINDOW_FOCUS_IN = 13;
const RgfwEventType RGFW_WINDOW_FOCUS_OUT = 14;
const RgfwEventType RGFW_WINDOW_REFRESH = 15;
const RgfwEventType RGFW_WINDOW_CLOSE = 16;
const RgfwEventType RGFW_WINDOW_MAXIMIZED = 17;
const RgfwEventType RGFW_WINDOW_MINIMIZED = 18;
const RgfwEventType RGFW_WINDOW_RESTORED = 19;
const RgfwEventType RGFW_DATA_DROP = 20;
const RgfwEventType RGFW_DATA_DRAG = 21;
const RgfwEventType RGFW_SCALE_UPDATED = 22;
const RgfwEventType RGFW_MONITOR_CONNECTED = 23;
const RgfwEventType RGFW_MONITOR_DISCONNECTED = 24;
const RgfwEventType RGFW_EVENT_COUNT = 25;
const RgfwEventType RGFW_MOUSE_POS_CHANGED = 7;


const RgfwEventFlag RGFW_KEY_PRESSED_FLAG = 2;
const RgfwEventFlag RGFW_KEY_RELEASED_FLAG = 4;
const RgfwEventFlag RGFW_KEY_CHAR_FLAG = 8;
const RgfwEventFlag RGFW_MOUSE_SCROLL_FLAG = 64;
const RgfwEventFlag RGFW_MOUSE_BUTTON_PRESSED_FLAG = 16;
const RgfwEventFlag RGFW_MOUSE_BUTTON_RELEASED_FLAG = 32;
const RgfwEventFlag RGFW_MOUSE_MOTION_FLAG = 128;
const RgfwEventFlag RGFW_MOUSE_RAW_MOTION_FLAG = 256;
const RgfwEventFlag RGFW_MOUSE_ENTER_FLAG = 512;
const RgfwEventFlag RGFW_MOUSE_LEAVE_FLAG = 1024;
const RgfwEventFlag RGFW_WINDOW_MOVED_FLAG = 2048;
const RgfwEventFlag RGFW_WINDOW_RESIZED_FLAG = 4096;
const RgfwEventFlag RGFW_WINDOW_FOCUS_IN_FLAG = 8192;
const RgfwEventFlag RGFW_WINDOW_FOCUS_OUT_FLAG = 16384;
const RgfwEventFlag RGFW_WINDOW_REFRESH_FLAG = 32768;
const RgfwEventFlag RGFW_WINDOW_MAXIMIZED_FLAG = 131072;
const RgfwEventFlag RGFW_WINDOW_MINIMIZED_FLAG = 262144;
const RgfwEventFlag RGFW_WINDOW_RESTORED_FLAG = 524288;
const RgfwEventFlag RGFW_SCALE_UPDATED_FLAG = 4194304;
const RgfwEventFlag RGFW_WINDOW_CLOSE_FLAG = 65536;
const RgfwEventFlag RGFW_DATA_DROP_FLAG = 1048576;
const RgfwEventFlag RGFW_DATA_DRAG_FLAG = 2097152;
const RgfwEventFlag RGFW_MONITOR_CONNECTED_FLAG = 8388608;
const RgfwEventFlag RGFW_MONITOR_DISCONNECTED_FLAG = 16777216;
const RgfwEventFlag RGFW_MOUSE_POS_CHANGED_FLAG = 128;
const RgfwEventFlag RGFW_KEY_EVENTS_FLAG = 14;
const RgfwEventFlag RGFW_MOUSE_EVENTS_FLAG = 2032;
const RgfwEventFlag RGFW_WINDOW_EVENTS_FLAG = 5150720;
const RgfwEventFlag RGFW_WINDOW_FOCUS_EVENTS_FLAG = 24576;
const RgfwEventFlag RGFW_DATA_DRAG_DROP_EVENTS_FLAG = 3145728;
const RgfwEventFlag RGFW_MONITOR_EVENTS_FLAG = 25165824;
const RgfwEventFlag RGFW_ALL_EVENT_FLAGS = 33554430;


const RgfwEventWait RGFW_EVENT_NO_WAIT = 0;
const RgfwEventWait RGFW_EVENT_WAIT_NEXT = -1;

const RgfwWindowFlags RGFW_WINDOW_NO_BORDER = 1;
const RgfwWindowFlags RGFW_WINDOW_NO_RESIZE = 2;
const RgfwWindowFlags RGFW_WINDOW_ALLOW_DND = 4;
const RgfwWindowFlags RGFW_WINDOW_HIDE_MOUSE = 8;
const RgfwWindowFlags RGFW_WINDOW_FULLSCREEN = 16;
const RgfwWindowFlags RGFW_WINDOW_TRANSLUCENT = 32;
const RgfwWindowFlags RGFW_WINDOW_TRANSPARENT = 32;
const RgfwWindowFlags RGFW_WINDOW_CENTER = 64;
const RgfwWindowFlags RGFW_WINDOW_RAW_MOUSE = 128;
const RgfwWindowFlags RGFW_WINDOW_SCALE_TO_MONITOR = 256;
const RgfwWindowFlags RGFW_WINDOW_HIDE = 512;
const RgfwWindowFlags RGFW_WINDOW_MAXIMIZE = 1024;
const RgfwWindowFlags RGFW_WINDOW_CENTER_CURSOR = 2048;
const RgfwWindowFlags RGFW_WINDOW_FLOATING = 4096;
const RgfwWindowFlags RGFW_WINDOW_FOCUS_ON_SHOW = 8192;
const RgfwWindowFlags RGFW_WINDOW_MINIMIZE = 16384;
const RgfwWindowFlags RGFW_WINDOW_FOCUS = 32768;
const RgfwWindowFlags RGFW_WINDOW_CAPTURE_MOUSE = 65536;
const RgfwWindowFlags RGFW_WINDOW_OPEN_GL = 131072;
const RgfwWindowFlags RGFW_WINDOW_EGL = 262144;
const RgfwWindowFlags RGFW_WINDOWED_FULLSCREEN = 1025;
const RgfwWindowFlags RGFW_WINDOW_CAPTURE_RAW_MOUSE = 65664;


const RgfwMouseIcon RGFW_MOUSE_NORMAL = 0;
const RgfwMouseIcon RGFW_MOUSE_ARROW = 1;
const RgfwMouseIcon RGFW_MOUSE_IBEAM = 2;
const RgfwMouseIcon RGFW_MOUSE_TEXT = 2;
const RgfwMouseIcon RGFW_MOUSE_CROSSHAIR = 3;
const RgfwMouseIcon RGFW_MOUSE_POINTING_HAND = 4;
const RgfwMouseIcon RGFW_MOUSE_RESIZE_EW = 5;
const RgfwMouseIcon RGFW_MOUSE_RESIZE_NS = 6;
const RgfwMouseIcon RGFW_MOUSE_RESIZE_NWSE = 7;
const RgfwMouseIcon RGFW_MOUSE_RESIZE_NESW = 8;
const RgfwMouseIcon RGFW_MOUSE_RESIZE_NW = 9;
const RgfwMouseIcon RGFW_MOUSE_RESIZE_N = 10;
const RgfwMouseIcon RGFW_MOUSE_RESIZE_NE = 11;
const RgfwMouseIcon RGFW_MOUSE_RESIZE_E = 12;
const RgfwMouseIcon RGFW_MOUSE_RESIZE_SE = 13;
const RgfwMouseIcon RGFW_MOUSE_RESIZE_S = 14;
const RgfwMouseIcon RGFW_MOUSE_RESIZE_SW = 15;
const RgfwMouseIcon RGFW_MOUSE_RESIZE_W = 16;
const RgfwMouseIcon RGFW_MOUSE_RESIZE_ALL = 17;
const RgfwMouseIcon RGFW_MOUSE_NOT_ALLOWED = 18;
const RgfwMouseIcon RGFW_MOUSE_WAIT = 19;
const RgfwMouseIcon RGFW_MOUSE_PROGRESS = 20;
const RgfwMouseIcon RGFW_MOUSE_ICON_COUNT = 21;
const RgfwMouseIcon RGFW_MOUSE_ICON_FINAL = 16;

constset GfxFlashRequest
{
	CANCEL,
	BRIEFLY,
	UNTIL_FOCUSED
}


const RgfwDebugType RGFW_TYPE_ERROR = 0;
const RgfwDebugType RGFW_TYPE_WARNING = 1;
const RgfwDebugType RGFW_TYPE_INFO = 2;

const RgfwErrorCode RGFW_NO_ERROR = 0;
const RgfwErrorCode RGFW_ERR_OUT_OF_MEMORY = 1;
const RgfwErrorCode RGFW_ERR_OPEN_GL_CONTEXT = 2;
const RgfwErrorCode RGFW_ERR_EGL_CONTEXT = 3;
const RgfwErrorCode RGFW_ERR_WAYLAND = 4;
const RgfwErrorCode RGFW_ERR_X11 = 5;
const RgfwErrorCode RGFW_ERR_DIRECT_X_CONTEXT = 6;
const RgfwErrorCode RGFW_ERR_IO_KIT = 7;
const RgfwErrorCode RGFW_ERR_CLIPBOARD = 8;
const RgfwErrorCode RGFW_ERR_FAILED_FUNC_LOAD = 9;
const RgfwErrorCode RGFW_ERR_BUFFER = 10;
const RgfwErrorCode RGFW_ERR_METAL = 11;
const RgfwErrorCode RGFW_ERR_PLATFORM = 12;
const RgfwErrorCode RGFW_ERR_EVENT_QUEUE = 13;
const RgfwErrorCode RGFW_ERR_NO_INIT = 14;
const RgfwErrorCode RGFW_INFO_WINDOW = 15;
const RgfwErrorCode RGFW_INFO_BUFFER = 16;
const RgfwErrorCode RGFW_INFO_GLOBAL = 17;
const RgfwErrorCode RGFW_INFO_OPEN_GL = 18;
const RgfwErrorCode RGFW_WARNING_WAYLAND = 19;
const RgfwErrorCode RGFW_WARNING_OPEN_GL = 20;

fn void convert_image_data_64(char* dest_data, char* src_data, RgfwColorLayout* srcLayout, RgfwColorLayout* destLayout, sz count, bool is64bit)
{
	char[4] rgba = {0, 0, 0, 0};
	sz i2 = 0;
	for (sz i = 0; i < count; i++)
	{
		char* src_px = &src_data[i * srcLayout.channels];
		char* dst_px = &dest_data[i2 * destLayout.channels];
		rgba[0] = src_px[srcLayout.r];
		rgba[1] = src_px[srcLayout.g];
		rgba[2] = src_px[srcLayout.b];
		rgba[3] = srcLayout.channels == 4 ? src_px[srcLayout.a] : 255;
		dst_px[destLayout.r] = rgba[0];
		dst_px[destLayout.g] = rgba[1];
		dst_px[destLayout.b] = rgba[2];
		if (destLayout.channels == 4) dst_px[destLayout.a] = rgba[3];
		i2 += 1 + (sz)is64bit;
	}
}

<*
 @require !@catch(dest_w.overflow_mul(dest_h))
*>
fn void copy_image_data_64(char* dest_data, int dest_w, int dest_h, GfxFormat dest_format, char* src_data, GfxFormat src_format, bool is64bit, GfxConvertImageDataFn func)
{
	sz count = (sz)dest_w * dest_h;
	if (src_format == dest_format)
	{
		int channels = dest_format.channels;
		mem::copy(dest_data, src_data, count * channels);
		return;
	}
	RgfwColorLayout* srcLayout = &&src_format.layout;
	RgfwColorLayout* destLayout = &&dest_format.layout;
	if (is64bit || func == null)
	{
		convert_image_data_64(dest_data, src_data, srcLayout, destLayout, count, is64bit);
	}
	else
	{
		func(dest_data, src_data, srcLayout, destLayout, count);
	}
}

fn void copy_image_data(char* dest_data, int w, int h, GfxFormat dest_format, char* src_data, GfxFormat src_format, GfxConvertImageDataFn func)
{
	copy_image_data_64(dest_data, w, h, dest_format, src_data, src_format, false, func);
}

fn int init(char* className, RgfwInitFlags flags)
{
	return rgfw_init_ptr(className, flags, &_rgfw_global);
}

fn void finalize() { rgfw_deinit_ptr(_rgfw); }

fn GfxSurface* create_surface(char* data, int w, int h, GfxFormat format)
{
	GfxSurface* surface = mem::new(GfxSurface);
	rgfw_createSurfacePtr(data, w, h, format, surface);
	return surface;
}

fn GfxWindow* create_window(char* name, int x, int y, int w, int h, RgfwWindowFlags flags)
{
	GfxWindow* win = mem::new(GfxWindow);
	return rgfw_createWindowPtr(name, x, y, w, h, flags, win);
}

fn bool is_mouse_pressed(RgfwMouseButton button) => _rgfw.mouseButtons[(sz)button].current && !_rgfw.mouseButtons[(sz)button].prev;

fn int[<2>] get_mouse(GfxWindow* win) => { win.internal.lastMouseX, win.internal.lastMouseY };

<*
 Check if a window should be closed.

 @param [&in] win
 @return "true is it is tagged as should close or its exit key is held down"
*>
fn bool window_should_close(GfxWindow* win)
{
	return win.internal.shouldClose || (win.internal.exitKey && is_key_down(win.internal.exitKey));
}

fn void GfxWindow.set_should_close(&win, bool should_close)
{
	if (should_close)
	{
		_window_close_callback(win);
	}
	else
	{
		win.internal.shouldClose = false;
	}
}

fn void window_scale_to_monitor(GfxWindow* win)
{
	GfxMonitor* monitor = find_monitor(win);
	if (monitor == null) return;
	rgfw_window_resize(win, (int)(monitor.scaleX * (float)win.w), (int)(monitor.scaleY * (float)win.h));
}

fn void window_move_to_monitor(GfxWindow* win, GfxMonitor* m)
{
	rgfw_window_move(win, m.origin.x + win.x, m.origin.y + win.y);
}

fn void GfxWindow.set_flags(&win, RgfwWindowFlags flags) { _window_set_flags(win, flags, win.internal.flags); }
fn bool GfxWindow.has_focus(&win) => win.internal.inFocus;
fn void rgfw_setBuildDND(bool state) { _rgfw.dndBuild = state; }

fn bool is_key_pressed(RgfwKey key, GfxWindow* window = null)
{
	return _rgfw.keyboard[key].current && !_rgfw.keyboard[key].prev && (!window || window.has_focus());
}
fn bool is_key_down(RgfwKey key, GfxWindow* window = null)
{
	return _rgfw.keyboard[key].current && (!window || window.has_focus());
}
fn bool is_key_released(RgfwKey key, GfxWindow* window = null)
{
	return !_rgfw.keyboard[key].current && _rgfw.keyboard[key].prev && (!window || window.has_focus());
}
fn bool is_mouse_down(RgfwMouseButton button, GfxWindow* window = null)
{
	return _rgfw.mouseButtons[(sz)button].current && (!window || window.has_focus());
}

fn bool is_mouse_released(RgfwMouseButton button, GfxWindow* window = null)
{
	return !_rgfw.mouseButtons[button].current && _rgfw.mouseButtons[button].prev && (!window || window.has_focus());
}

fn float[<2>] get_mouse_scroll() => { _rgfw.scrollX, _rgfw.scrollY };
fn float[<2>] get_mouse_vector() => { _rgfw.vectorX, _rgfw.vectorY };

fn void GfxWindow.destroy(&self)
{
	rgfw_window_closePtr(self);
	free(self);
}

fn GfxGenericFunc set_event_callback(RgfwEventType type, GfxGenericFunc func)
{
	GfxGenericFunc old = _rgfw.callbacks[type];
	_rgfw.callbacks[type] = func;
	return old;
}

fn void set_dual_event_callback(RgfwEventType type, GfxGenericFunc func, GfxGenericFunc* first, GfxGenericFunc* second)
{
	GfxGenericFunc func1 = set_event_callback(type, func);
	GfxGenericFunc func2 = set_event_callback((RgfwEventType)(type + 1), func);
	if (first != null) *first = func1;
	if (second != null) *second = func2;
}

fn void set_all_event_callback(GfxGenericFunc func, RgfwCallbacks* callbacks)
{
	for (RgfwEventType i = RGFW_EVENT_NONE + 1; i < RGFW_EVENT_COUNT; i++)
	{
		if (callbacks != null) callbacks.arr[i] = _rgfw.callbacks[i];
		set_event_callback(i, func);
	}
}


fn void rgfw_eventQueueFlush() { _rgfw.eventLen = 0; }

fn void event_queue_push(GfxEvent* event)
{
	if (!_rgfw.queueEvents) return;
	if (_rgfw.eventLen >= MAX_EVENTS)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_EVENT_QUEUE, "Event queue limit 'MAX_EVENTS' has been reached automatically flushing queue.");
		rgfw_eventQueueFlush();
		return;
	}
	int eventTop = (_rgfw.eventBottom + _rgfw.eventLen) % MAX_EVENTS;
	_rgfw.eventLen += 1;
	_rgfw.events[eventTop] = *event;
}

fn GfxEvent? event_queue_pop()
{
	if (!_rgfw.eventLen) return NO_MORE_ELEMENT~;
	GfxEvent* ev = &_rgfw.events[_rgfw.eventBottom];
	_rgfw.eventLen--;
	_rgfw.eventBottom = (_rgfw.eventBottom + 1) % MAX_EVENTS;
	return *ev;
}

fn void event_queue_call_push(GfxEvent* event)
{
	if (_rgfw.callbacks[event.type]) _rgfw.callbacks[event.type](event);
	event_queue_push(event);
}

fn GfxEvent? check_event()
{
	if (!_rgfw.eventLen && !_rgfw.polledEvents)
	{
		_rgfw.queueEvents = true;
		rgfw_pollEvents();
		_rgfw.polledEvents = true;
	}
	defer catch _rgfw.polledEvents = false;
	return check_queued_event();
}

fn GfxEvent? check_queued_event()
{
	_rgfw.queueEvents = true;
	return event_queue_pop();
}

fn GfxEvent? window_event_queue_pop(GfxWindow* win)
{
	GfxEvent ev = event_queue_pop()!;
	for (int i = 1; i < _rgfw.eventLen && ev.common.win != win && ev.common.win != null; i++)
	{
		event_queue_push(&ev);
		ev = event_queue_pop()!!;
	}
	if (ev.common.win != win && ev.common.win != null) return NO_MORE_ELEMENT~;
	return ev;
}

fn GfxEvent? check_queued_window_event(GfxWindow* win)
{
	return window_event_queue_pop(win);
}

fn GfxEvent? check_window_event(GfxWindow* win)
{
	if (!_rgfw.eventLen && !_rgfw.polledEvents)
	{
		_rgfw.queueEvents = true;
		rgfw_pollEvents();
		_rgfw.polledEvents = true;
	}
	defer catch _rgfw.polledEvents = false;
	return check_queued_window_event(win);
}

fn void _window_set_flags(GfxWindow* win, RgfwWindowFlags flags, RgfwWindowFlags cmpFlags) @private
{
	switch
	{
		case flags & RGFW_WINDOW_NO_BORDER != 0: rgfw_window_setBorder(win, false);
		case cmpFlags & RGFW_WINDOW_NO_BORDER != 0: rgfw_window_setBorder(win, true);
	}
	switch
	{
		case flags & RGFW_WINDOW_MAXIMIZE != 0: rgfw_window_maximize(win);
		case cmpFlags & RGFW_WINDOW_MAXIMIZE != 0: rgfw_window_restore(win);
	}
	switch
	{
		case flags & RGFW_WINDOW_MINIMIZE != 0: rgfw_window_minimize(win);
		case cmpFlags & RGFW_WINDOW_MINIMIZE != 0: rgfw_window_restore(win);
	}
	if (flags & RGFW_WINDOW_CENTER) rgfw_window_center(win);
	if (flags & RGFW_WINDOW_CENTER_CURSOR) rgfw_window_moveMouse(win, win.x + (win.w / 2), win.y + (win.h / 2));
	switch
	{
		case flags & RGFW_WINDOW_FULLSCREEN != 0: rgfw_window_setFullscreen(win, true);
		case cmpFlags & RGFW_WINDOW_FULLSCREEN != 0: rgfw_window_setFullscreen(win, false);
	}
	if (flags & RGFW_WINDOW_HIDE_MOUSE) { rgfw_window_showMouse(win, false); }
	else if (cmpFlags & RGFW_WINDOW_HIDE_MOUSE) { rgfw_window_showMouse(win, true); }
	if (flags & RGFW_WINDOW_HIDE) { rgfw_window_hide(win); }
	else if (cmpFlags & RGFW_WINDOW_HIDE) { show(win); }
	if (flags & RGFW_WINDOW_FLOATING) { rgfw_window_setFloating(win, true); }
	else if (cmpFlags & RGFW_WINDOW_FLOATING) { rgfw_window_setFloating(win, false); }
	if (flags & RGFW_WINDOW_RAW_MOUSE) { rgfw_window_setRawMouseMode(win, true); }
	else if (cmpFlags & RGFW_WINDOW_RAW_MOUSE) { rgfw_window_setRawMouseMode(win, false); }
	if (flags & RGFW_WINDOW_CAPTURE_MOUSE) { rgfw_window_captureRawMouse(win, true); }
	else if (cmpFlags & RGFW_WINDOW_CAPTURE_MOUSE) { rgfw_window_captureMouse(win, false); }
	if (flags & RGFW_WINDOW_FOCUS) rgfw_window_focus(win);
	if (flags & RGFW_WINDOW_SCALE_TO_MONITOR) window_scale_to_monitor(win);
	switch (true)
	{
		case flags & RGFW_WINDOW_NO_RESIZE != 0 :
			rgfw_window_setMaxSize(win, win.w, win.h);
			rgfw_window_setMinSize(win, win.w, win.h);
		case cmpFlags & RGFW_WINDOW_NO_RESIZE != 0:
			rgfw_window_setMaxSize(win, 0, 0);
			rgfw_window_setMinSize(win, 0, 0);
	}
	win.internal.flags = flags;
}

fn void _window_restored_callback(GfxWindow* win, int x, int y, int w, int h) @private
{
	win.internal.flags &= ~((uint)RGFW_WINDOW_MINIMIZE);
	win.x = x; win.y = y; win.w = w; win.h = h;
	if (rgfw_window_isMaximized(win) == false) win.internal.flags &= ~((uint)RGFW_WINDOW_MAXIMIZE);
	if (!(win.internal.enabledEvents & RGFW_WINDOW_RESTORED_FLAG)) return;
	GfxEvent event;
	event.type = RGFW_WINDOW_RESTORED;
	event.update.x = x; event.update.y = y; event.update.w = w; event.update.h = h;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void rgfw_window_checkMode(GfxWindow* win)
{
	if (rgfw_window_isMinimized(win))
	{
		rgfw_windowMinimizedCallback(win);
	}
	else if (rgfw_window_isMaximized(win))
	{
		rgfw_windowMaximizedCallback(win, win.x, win.y, win.w, win.h);
	}
	else if (((win.internal.flags & RGFW_WINDOW_MINIMIZE) && !rgfw_window_isMaximized(win)) ||
	((win.internal.flags & RGFW_WINDOW_MAXIMIZE) && !rgfw_window_isMaximized(win)))
	{
		_window_restored_callback(win, win.x, win.y, win.w, win.h);
	}
}

fn void _window_moved_callback(GfxWindow* win, int x, int y) @private
{
	win.x = x; win.y = y;
	if (!(win.internal.enabledEvents & RGFW_WINDOW_MOVED_FLAG)) return;
	GfxEvent event;
	event.type = RGFW_WINDOW_MOVED;
	event.update.x = x; event.update.y = y; event.update.w = win.w; event.update.h = win.h;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void _window_resized_callback(GfxWindow* win, int w, int h) @private
{
	win.w = w; win.h = h;
	if (!(win.internal.enabledEvents & RGFW_WINDOW_RESIZED_FLAG)) return;
	GfxEvent event;
	event.type = RGFW_WINDOW_RESIZED;
	event.update.x = win.x; event.update.y = win.y; event.update.w = w; event.update.h = h;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void _window_close_callback(GfxWindow* win) @private
{
	win.internal.shouldClose = true;
	GfxEvent event;
	event.type = RGFW_WINDOW_CLOSE;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void rgfw_mouseMotionCallback(GfxWindow* win, int x, int y)
{
	win.internal.lastMouseX = x;
	win.internal.lastMouseY = y;
	if (!(win.internal.enabledEvents & RGFW_MOUSE_MOTION_FLAG)) return;
	GfxEvent event;
	event.type = RGFW_MOUSE_MOTION;
	event.mouse.x = x; event.mouse.y = y; event.mouse.inWindow = win.internal.mouseInside;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void _raw_motion_callback(GfxWindow* win, float x, float y)
{
	_rgfw.vectorX = x; _rgfw.vectorY = y;
	if (!(win.internal.enabledEvents & RGFW_MOUSE_RAW_MOTION_FLAG)) return;
	GfxEvent event;
	event.type = RGFW_MOUSE_RAW_MOTION;
	event.delta.x = x; event.delta.y = y;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void rgfw_windowRefreshCallback(GfxWindow* win, int x, int y, int w, int h)
{
	if (!(win.internal.enabledEvents & RGFW_WINDOW_REFRESH_FLAG)) return;
	GfxEvent event;
	event.type = RGFW_WINDOW_REFRESH;
	event.update.x = x; event.update.y = y; event.update.w = w; event.update.h = h;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void rgfw_mouseButtonCallback(GfxWindow* win, RgfwMouseButton button, bool press)
{
	GfxEvent event;
	if (press)
	{
		if (!(win.internal.enabledEvents & RGFW_MOUSE_BUTTON_PRESSED_FLAG)) return;
		event.type = RGFW_MOUSE_BUTTON_PRESSED;
	}
	else
	{
		if (!(win.internal.enabledEvents & RGFW_MOUSE_BUTTON_RELEASED_FLAG)) return;
		event.type = RGFW_MOUSE_BUTTON_RELEASED;
	}
	_rgfw.mouseButtons[(sz)button].prev = _rgfw.mouseButtons[(sz)button].current;
	_rgfw.mouseButtons[(sz)button].current = press;
	event.button.value = button;
	event.button.state = press;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void rgfw_mouseScrollCallback(GfxWindow* win, float x, float y)
{
	if (!(win.internal.enabledEvents & RGFW_MOUSE_SCROLL_FLAG)) return;
	_rgfw.scrollX = x; _rgfw.scrollY = y;
	GfxEvent event;
	event.type = RGFW_MOUSE_SCROLL;
	event.delta.x = x; event.delta.y = y;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void rgfw_scaleUpdatedCallback(GfxWindow* win, float scaleX, float scaleY)
{
	if (!(win.internal.enabledEvents & RGFW_SCALE_UPDATED_FLAG)) return;
	GfxEvent event;
	event.type = RGFW_SCALE_UPDATED;
	event.scale.x = scaleX; event.scale.y = scaleY;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void rgfw_monitorCallback(GfxWindow* win, GfxMonitor* monitor, bool connected)
{
	if (win != null)
	{
		if (connected && !(win.internal.enabledEvents & RGFW_MONITOR_CONNECTED_FLAG)) return;
		if (!connected && !(win.internal.enabledEvents & RGFW_MONITOR_DISCONNECTED_FLAG)) return;
	}
	GfxEvent event;
	event.type = connected ? RGFW_MONITOR_CONNECTED : RGFW_MONITOR_DISCONNECTED;
	event.monitor.monitor = monitor;
	event.monitor.state = connected;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void rgfw_windowMaximizedCallback(GfxWindow* win, int x, int y, int w, int h)
{
	win.internal.flags |= RGFW_WINDOW_MAXIMIZE;
	win.x = x; win.y = y; win.w = w; win.h = h;
	if (!(win.internal.enabledEvents & RGFW_WINDOW_MAXIMIZED_FLAG)) return;
	GfxEvent event;
	event.type = RGFW_WINDOW_MAXIMIZED;
	event.update.x = x; event.update.y = y; event.update.w = w; event.update.h = h;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void rgfw_windowMinimizedCallback(GfxWindow* win)
{
	win.internal.flags |= RGFW_WINDOW_MINIMIZE;
	if (!(win.internal.enabledEvents & RGFW_WINDOW_MINIMIZED_FLAG)) return;
	GfxEvent event;
	event.type = RGFW_WINDOW_MINIMIZED;
	event.update.x = win.x; event.update.y = win.y; event.update.w = win.w; event.update.h = win.h;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void rgfw_windowFocusCallback(GfxWindow* win, bool inFocus)
{
	win.internal.inFocus = inFocus;
	if (win.internal.captureMouse) rgfw_window_captureMousePlatform(win, inFocus);
	GfxEvent event;
	event.common.win = win;
	event.focus.state = inFocus;
	if (inFocus == true)
	{
		if (win.internal.flags & RGFW_WINDOW_FULLSCREEN) rgfw_window_raise(win);
		event.type = RGFW_WINDOW_FOCUS_IN;
	}
	else
	{
		if (win.internal.flags & RGFW_WINDOW_FULLSCREEN) rgfw_window_minimize(win);
		for (sz key = 0; key < (sz)RGFW_KEY_LAST; key++)
		{
			if (is_key_down((RgfwKey)key) == false) continue;
			_rgfw.keyboard[key].current = false;
			if (win.internal.enabledEvents & RGFW_KEY_RELEASED_FLAG) rgfw_keyCallback(win, (RgfwKey)key, win.internal.mod, false, false);
		}
		rgfw_resetKey();
		event.type = RGFW_WINDOW_FOCUS_OUT;
	}
	event_queue_call_push(&event);
}


fn void rgfw_dataDragCallback(GfxWindow* win, RgfwDataTransferType dataType, RgfwDndActionType action, int x, int y)
{
	if (!(win.internal.enabledEvents & RGFW_DATA_DRAG_FLAG) || !(win.internal.flags & RGFW_WINDOW_ALLOW_DND)) return;
	_rgfw.windowState.win = win;
	_rgfw.windowState.dataDragging = true;
	_rgfw.windowState.dropX = x;
	_rgfw.windowState.dropY = y;
	GfxEvent event;
	event.type = RGFW_DATA_DRAG;
	event.drag.x = x; event.drag.y = y; event.drag.action = action; event.drag.dataType = dataType;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void rgfw_dataDropCallback(GfxWindow* win, char* data, sz length, RgfwDataTransferType dataType)
{
	if (!(win.internal.enabledEvents & RGFW_DATA_DROP_FLAG) || !(win.internal.flags & RGFW_WINDOW_ALLOW_DND)) return;
	_rgfw.windowState.win = win;
	_rgfw.windowState.dataDrop = true;
	_rgfw.windowState.dataLength = length;
	GfxDataDropNode node;
	rgfw_memzero(&node, GfxDataDropNode::size);
	node.data = data;
	node.length = length;
	node.type = dataType;
	GfxEvent event;
	event.type = RGFW_DATA_DROP;
	event.drop.value = &node;
	event.common.win = win;
	if (_rgfw.callbacks[(sz)event.type] != null) _rgfw.callbacks[(sz)event.type](&event);
	if (_rgfw.queueEvents == false && _rgfw.dndBuild == false) return;
	char* dataCopy = (char*)rgfw_alloc_impl(length);
	if (dataCopy != null) { rgfw_memcpy(dataCopy, data, length); node.data = dataCopy; }
	GfxDataDropNode* heapNode = (GfxDataDropNode*)rgfw_alloc_impl(GfxDataDropNode::size);
	if (heapNode == null) return;
	*heapNode = node;
	if (_rgfw.dndRoot == null) { _rgfw.dndRoot = heapNode; _rgfw.dndCur = heapNode; }
	else { _rgfw.dndCur.next = heapNode; _rgfw.dndCur = heapNode; }
	event.drop.value = heapNode;
	event_queue_push(&event);
}

fn void rgfw_keyCharCallback(GfxWindow* win, uint codepoint)
{
	if (!(win.internal.enabledEvents & RGFW_KEY_CHAR_FLAG)) return;
	GfxEvent event;
	event.type = RGFW_KEY_CHAR;
	event.keyChar.value = codepoint;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void rgfw_keyCallback(GfxWindow* win, RgfwKey key, RgfwKeymod mod, bool repeat, bool state)
{
	GfxEvent event;
	if (state)
	{
		if (!(win.internal.enabledEvents & RGFW_KEY_PRESSED_FLAG)) return;
		event.type = RGFW_KEY_PRESSED;
	}
	else
	{
		if (!(win.internal.enabledEvents & RGFW_KEY_RELEASED_FLAG)) return;
		event.type = RGFW_KEY_RELEASED;
	}
	_rgfw.keyboard[(sz)key].prev = _rgfw.keyboard[(sz)key].current;
	_rgfw.keyboard[(sz)key].current = state;
	event.key.value = key; event.key.repeat = repeat; event.key.mod = mod; event.key.state = state;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void rgfw_initKeycodes()
{
	_rgfw.keycodes = {};
	rgfw_initKeycodesPlatform();
	for (sz i = 0; i < (sz)RGFW_KEY_LAST; i++)
	{
		for (sz y = 0; y < _rgfw.keycodes.len; y++)
		{
			if (_rgfw.keycodes[y] == (RgfwKey)i)
			{
				_rgfw.apiKeycodes[i] = (uint)y;
				break;
			}
		}
	}
	rgfw_resetKey();
}

fn RgfwKey rgfw_apiKeyToRGFW(uint keycode)
{
	if (keycode >= _rgfw.keycodes.len) return RGFW_KEY_NULL;
	return _rgfw.keycodes[keycode];
}

fn uint rgfw_rgfwToApiKey(RgfwKey keycode)
{
	return _rgfw.apiKeycodes[keycode];
}

fn void rgfw_resetKey()
{
	_rgfw.keyboard = {};
}

fn void rgfw_setQueueEvents(bool queue) { _rgfw.queueEvents = queue; }

fn void rgfw_resetPrevState()
{
	for (sz i = 0; i < (sz)RGFW_KEY_LAST; i++) _rgfw.keyboard[i].prev = _rgfw.keyboard[i].current;
	for (sz i = 0; i < (sz)RGFW_MOUSE_FINAL; i++) _rgfw.mouseButtons[i].prev = _rgfw.mouseButtons[i].current;
	_rgfw.scrollX = 0.0f; _rgfw.scrollY = 0.0f;
	_rgfw.vectorX = 0.0f; _rgfw.vectorY = 0.0f;
	rgfw_memzero(&_rgfw.windowState, RgfwWindowState::size);
	GfxDataDropNode* node = _rgfw.dndRoot;
	while (node != null)
	{
		GfxDataDropNode* next = node.next;
		rgfw_free_impl(node);
		node = next;
	}
	_rgfw.dndRoot = null;
	_rgfw.dndCur = null;
}


fn RgfwWindowSrc* rgfw_window_getSrc(GfxWindow* win) => &win.src;
fn bool rgfw_window_getPosition(GfxWindow* win, int* x, int* y) { if (x != null) *x = win.x; if (y != null) *y = win.y; return true; }
fn bool rgfw_window_getSize(GfxWindow* win, int* w, int* h) { if (w != null) *w = win.w; if (h != null) *h = win.h; return true; }
fn uint rgfw_window_getFlags(GfxWindow* win) => win.internal.flags;
fn RgfwKey rgfw_window_getExitKey(GfxWindow* win) => win.internal.exitKey;
fn void rgfw_window_setExitKey(GfxWindow* win, RgfwKey key) { win.internal.exitKey = key; }
fn void rgfw_window_setEnabledEvents(GfxWindow* win, RgfwEventFlag events) { win.internal.enabledEvents = events; }
fn RgfwEventFlag rgfw_window_getEnabledEvents(GfxWindow* win) => win.internal.enabledEvents;
fn void rgfw_window_setDisabledEvents(GfxWindow* win, RgfwEventFlag events) { rgfw_window_setEnabledEvents(win, RGFW_ALL_EVENT_FLAGS & ~events); }
fn void rgfw_window_setEventState(GfxWindow* win, RgfwEventFlag event, bool state) { rgfw_setBit(&win.internal.enabledEvents, event, state); }
fn void* rgfw_window_getUserPtr(GfxWindow* win) => win.userPtr;
fn void rgfw_window_setUserPtr(GfxWindow* win, void* ptr) { win.userPtr = ptr; }

fn bool rgfw_window_getSizeInPixels(GfxWindow* win, int* w, int* h)
{
	GfxMonitor* mon = find_monitor(win);
	if (mon == null) return false;
	if (w != null) *w = (int)((float)win.w * mon.pixelRatio);
	if (h != null) *h = (int)((float)win.h * mon.pixelRatio);
	return true;
}

fn bool rgfw_window_didMouseLeave(GfxWindow* win) => _rgfw.windowState.winLeave == win && _rgfw.windowState.mouseLeave;
fn bool rgfw_window_didMouseEnter(GfxWindow* win) => _rgfw.windowState.win == win && _rgfw.windowState.mouseEnter;
fn bool rgfw_window_isMouseInside(GfxWindow* win) => win.internal.mouseInside;
fn bool rgfw_window_isDataDragging(GfxWindow* win) => rgfw_window_getDataDrag(win, null, null);
fn bool rgfw_window_didDataDrop(GfxWindow* win) => rgfw_window_getDataDrop(win) != null;

fn bool rgfw_window_getDataDrag(GfxWindow* win, int* x, int* y)
{
	if (_rgfw.windowState.win != win || _rgfw.windowState.dataDragging == false) return false;
	if (x != null) *x = _rgfw.windowState.dropX;
	if (y != null) *y = _rgfw.windowState.dropY;
	return true;
}

fn GfxDataDropNode* rgfw_window_getDataDrop(GfxWindow* win)
{
	if (_rgfw.windowState.win != win || _rgfw.windowState.dataDrop == false) return null;
	return _rgfw.dndRoot;
}

fn void rgfw_setRootWindow(GfxWindow* win) { _rgfw.root = win; }
fn GfxWindow* rgfw_getRootWindow() { return _rgfw.root; }

fn void rgfw_setBit(uint* value, uint mask, bool set)
{
	if (set) { *value |= mask; }
	else { *value &= ~mask; }
}


fn void rgfw_window_center(GfxWindow* win)
{
	GfxMonitor* mon = find_monitor(win);
	if (mon == null) return;
	rgfw_window_move(win, mon.origin.x + ((mon.mode.w - win.w) / 2), mon.origin.y + ((mon.mode.h - win.h) / 2));
}

fn bool rgfw_monitor_scaleToWindow(GfxMonitor* mon, GfxWindow* win)
{
	GfxMonitorMode mode;
	mode.w = win.w;
	mode.h = win.h;
	bool ret = rgfw_monitor_requestMode(mon, &mode, SCALE);
	rgfw_window_move(win, ...mon.origin);
	return ret;
}

fn void rgfw_splitBPP(uint bpp, GfxMonitorMode* mode)
{
	if (bpp == 32) bpp = 24;
	mode.red = mode.green = mode.blue = (char)(bpp / 3);
	uint delta = bpp - (mode.red * 3);
	if (delta >= 1) mode.green = mode.green + 1;
	if (delta == 2) mode.red = mode.red + 1;
}

fn bool rgfw_monitorModeCompare(GfxMonitorMode* mon, GfxMonitorMode* mon2, GfxMonitorModeRequest request)
{
	return rgfw_bool(((mon.w == mon2.w && mon.h == mon2.h) || !(request & SCALE)) &&
	((mon.refresh_rate == mon2.refresh_rate) || !(request & REFRESH)) &&
	((mon.red == mon2.red && mon.green == mon2.green && mon.blue == mon2.blue) || !(request & RGB)));
}

fn void rgfw_surface_setConvertFunc(GfxSurface* surface, GfxConvertImageDataFn func) { surface.convertFunc = func; }
fn RgfwNativeImage* rgfw_surface_getNativeImage(GfxSurface* surface) => &surface.native;
fn void rgfw_surface_free(GfxSurface* surface) { rgfw_surface_freePtr(surface); rgfw_free_impl(surface); }

fn GfxSurface* rgfw_window_createSurface(GfxWindow* win, char* data, int w, int h, GfxFormat format)
{
	GfxSurface* surface = (GfxSurface*)rgfw_alloc_impl(GfxSurface::size);
	if (surface == null) return null;
	rgfw_memzero(surface, GfxSurface::size);
	rgfw_window_createSurfacePtr(win, data, w, h, format, surface);
	return surface;
}


fn RgfwMonitorNode* rgfw_monitors_add(GfxMonitor* mon)
{
	RgfwMonitorNode* node = mem::new(RgfwMonitorNode);
	if (!_rgfw.monitors.list.head)
	{
		_rgfw.monitors.list.head = node;
	}
	else
	{
		_rgfw.monitors.list.cur.next = node;
	}
	_rgfw.monitors.list.cur = node;
	if (mon != null) node.mon = *mon;
	node.mon.node = node;
	node.disconnected = false;
	_rgfw.monitors.count += 1;
	return node;
}


fn void rgfw_monitors_refresh()
{
	RgfwMonitorNode* prev = _rgfw.monitors.list.head;
	RgfwMonitorNode* node = _rgfw.monitors.list.head;
	while (node != null)
	{
		RgfwMonitorNode* next = node.next;
		if (node.disconnected != false)
		{
			rgfw_monitorCallback(_rgfw.root, &node.mon, false);
			_remove_monitor(node, prev);
		}
		prev = node;
		node = next;
	}
}

fn void rgfw_freeMonitors()
{
	RgfwMonitorNode* node = _rgfw.monitors.list.head;
	while (node != null)
	{
		RgfwMonitorNode* next = node.next;
		_remove_monitor(node, node);
		node = next;
	}
}

fn GfxMonitor** rgfw_getMonitors(sz* len)
{
	if (len != null) *len = 0;
	sz count = 0;
	if (rgfw_getMonitorsPtr(0, null, &count) == false || count == 0) return null;
	GfxMonitor** monitors = (GfxMonitor**)rgfw_alloc_impl(GfxMonitor*::size * count);
	if (rgfw_getMonitorsPtr(count, monitors, &count) == false)
	{
		rgfw_free_impl(monitors);
		return null;
	}
	if (len != null) *len = count;
	return monitors;
}

fn bool rgfw_getMonitorsPtr(sz max, GfxMonitor** monitors, sz* len)
{
	if (len != null) *len = _rgfw.monitors.count;
	if (monitors == null || max == 0) return true;
	if (len != null) *len = max;
	sz i = 0;
	RgfwMonitorNode* cur_node = _rgfw.monitors.list.head;
	while (cur_node != null && i < max)
	{
		monitors[i] = &cur_node.mon;
		i++;
		cur_node = cur_node.next;
	}
	return true;
}

fn GfxMonitor* rgfw_getPrimaryMonitor()
{
	if (_rgfw.monitors.primary == null) _rgfw.monitors.primary = _rgfw.monitors.list.head;
	if (_rgfw.monitors.primary == null) return null;
	return &_rgfw.monitors.primary.mon;
}

fn GfxMonitorMode[] GfxMonitor.get_modes(&monitor, Allocator allocator)
{
	return alloc::clone_slice(allocator, rgfw_monitor_getModesPtr(monitor));
}

fn GfxMonitorMode? GfxMonitor.get_best_matching_mode(&monitor, GfxMonitorMode mode) => @pool()
{
	GfxMonitorMode* chosen;
	int top_score = 1;
	foreach (&mode2 : rgfw_monitor_getModesPtr(monitor))
	{
		int score = 0;
		if (mode.w == mode2.w && mode.h == mode2.h) score += 1000;
		if (mode.red == mode2.red && mode.green == mode2.green && mode.blue == mode2.blue) score += 100;
		if (mode.refresh_rate == mode2.refresh_rate) score += 10;
		if (score > top_score)
		{
			top_score = score;
			chosen = mode2;
		}
	}
	return chosen ? *chosen : NOT_FOUND~;
}

fn char* rgfw_monitor_getName(GfxMonitor* monitor) => &monitor.name[0];
fn bool rgfw_monitor_getScale(GfxMonitor* monitor, float* x, float* y) { if (x != null) *x = monitor.scaleX; if (y != null) *y = monitor.scaleY; return true; }
fn bool rgfw_monitor_getPhysicalSize(GfxMonitor* monitor, float* w, float* h) { if (w != null) *w = monitor.physW; if (h != null) *h = monitor.physH; return true; }
fn void rgfw_monitor_setUserPtr(GfxMonitor* monitor, void* userPtr) { monitor.userPtr = userPtr; }
fn void* rgfw_monitor_getUserPtr(GfxMonitor* monitor) => monitor.userPtr;
fn bool rgfw_monitor_getMode(GfxMonitor* monitor, GfxMonitorMode* mode) { if (mode != null) *mode = monitor.mode; return true; }

fn RgfwGammaRamp* rgfw_monitor_getGammaRamp(GfxMonitor* monitor)
{
	RgfwGammaRamp* ramp = (RgfwGammaRamp*)rgfw_alloc_impl(RgfwGammaRamp::size);
	ramp.count = rgfw_monitor_getGammaRampPtr(monitor, null);
	ramp.red = (ushort*)rgfw_alloc_impl(ushort::size * ramp.count);
	ramp.green = (ushort*)rgfw_alloc_impl(ushort::size * ramp.count);
	ramp.blue = (ushort*)rgfw_alloc_impl(ushort::size * ramp.count);
	ramp.count = rgfw_monitor_getGammaRampPtr(monitor, ramp);
	return ramp;
}

fn void rgfw_freeGammaRamp(RgfwGammaRamp* ramp)
{
	rgfw_free_impl(ramp.red);
	rgfw_free_impl(ramp.green);
	rgfw_free_impl(ramp.blue);
	rgfw_free_impl(ramp);
}

fn bool rgfw_monitor_setGammaPtr(GfxMonitor* monitor, float gamma, ushort* ptr, sz count)
{
	for (sz i = 0; i < count; i++)
	{
		float value = (float)i / (float)(count - 1);
		value = powf(value, 1.0f / gamma) * 65535.0f + 0.5f;
		value = rgfw_min(value, 65535.0f);
		ptr[i] = (ushort)value;
	}
	RgfwGammaRamp ramp;
	ramp.red = ptr; ramp.green = ptr; ramp.blue = ptr; ramp.count = count;
	return rgfw_monitor_setGammaRamp(monitor, &ramp);
}

fn bool rgfw_monitor_setGamma(GfxMonitor* monitor, float gamma)
{
	sz count = rgfw_monitor_getGammaRampPtr(monitor, null);
	ushort* ptr = (ushort*)rgfw_alloc_impl(count * ushort::size);
	bool ret = rgfw_monitor_setGammaPtr(monitor, gamma, ptr, count);
	rgfw_free_impl(ptr);
	return ret;
}


fn void rgfw_window_captureMouse(GfxWindow* win, bool state) { win.internal.captureMouse = state; rgfw_window_captureMousePlatform(win, state); }
fn void rgfw_window_setRawMouseMode(GfxWindow* win, bool state) { win.internal.rawMouse = state; rgfw_window_setRawMouseModePlatform(win, state); }
fn void rgfw_setRawMouseMode(bool state) { _rgfw.rawMouse = state; if (_rgfw.root != null) rgfw_window_setRawMouseModePlatform(_rgfw.root, state); }
fn void rgfw_window_captureRawMouse(GfxWindow* win, bool state) { rgfw_window_captureMouse(win, state); rgfw_window_setRawMouseMode(win, state); }
fn bool rgfw_window_isRawMouseMode(GfxWindow* win) => rgfw_bool(win.internal.rawMouse);
fn bool rgfw_window_isCaptured(GfxWindow* win) => rgfw_bool(win.internal.captureMouse);

fn void rgfw_keyUpdateKeyMod(GfxWindow* win, RgfwKeymod mod, bool value) { if (value) { win.internal.mod |= mod; } else { win.internal.mod &= ~mod; } }
fn void rgfw_keyUpdateKeyModsEx(GfxWindow* win, bool capital, bool numlock, bool control, bool alt, bool shift, bool super, bool scroll)
{
	rgfw_keyUpdateKeyMod(win, RGFW_MOD_CAPS_LOCK, capital);
	rgfw_keyUpdateKeyMod(win, RGFW_MOD_NUM_LOCK, numlock);
	rgfw_keyUpdateKeyMod(win, RGFW_MOD_CONTROL, control);
	rgfw_keyUpdateKeyMod(win, RGFW_MOD_ALT, alt);
	rgfw_keyUpdateKeyMod(win, RGFW_MOD_SHIFT, shift);
	rgfw_keyUpdateKeyMod(win, RGFW_MOD_SUPER, super);
	rgfw_keyUpdateKeyMod(win, RGFW_MOD_SCROLL_LOCK, scroll);
}

fn void rgfw_keyUpdateKeyMods(GfxWindow* win, bool capital, bool numlock, bool scroll)
{
	rgfw_keyUpdateKeyModsEx(win, capital, numlock,
	rgfw_bool(is_key_down(RGFW_KEY_CONTROL_L) || is_key_down(RGFW_KEY_CONTROL_R)),
	rgfw_bool(is_key_down(RGFW_KEY_ALT_L) || is_key_down(RGFW_KEY_ALT_R)),
	rgfw_bool(is_key_down(RGFW_KEY_SHIFT_L) || is_key_down(RGFW_KEY_SHIFT_R)),
	rgfw_bool(is_key_down(RGFW_KEY_SUPER_L) || is_key_down(RGFW_KEY_SUPER_R)),
	scroll);
}

fn bool GfxWindow.is_mouse_hidden(&win) => win.internal.flags & RGFW_WINDOW_HIDE_MOUSE != 0;
fn bool GfxWindow.is_borderless(&win) => win.internal.flags & RGFW_WINDOW_NO_BORDER != 0;
fn bool GfxWindow.is_fullscreen(&win) => win.internal.flags & RGFW_WINDOW_FULLSCREEN != 0;
fn bool GfxWindow.allows_dnd(&win) => win.internal.flags & RGFW_WINDOW_ALLOW_DND != 0;


fn void rgfw_window_showMouseFlags(GfxWindow* win, bool show)
{
	if (show && (win.internal.flags & RGFW_WINDOW_HIDE_MOUSE)) { win.internal.flags ^= RGFW_WINDOW_HIDE_MOUSE; }
	else if (!show && !(win.internal.flags & RGFW_WINDOW_HIDE_MOUSE)) { win.internal.flags |= RGFW_WINDOW_HIDE_MOUSE; }
}

fn bool rgfw_window_setMouseDefault(GfxWindow* win) => rgfw_window_setMouseStandard(win, RGFW_MOUSE_NORMAL);
fn bool rgfw_window_setMouseStandard(GfxWindow* win, RgfwMouseIcon icon) => rgfw_window_setMouse(win, _rgfw.standardMice[(sz)icon]);
fn bool rgfw_window_setMouse(GfxWindow* win, RgfwMouse* mouse) { if (mouse != _rgfw.hiddenMouse) win.internal.mouse = mouse; return rgfw_window_setMousePlatform(win, mouse); }

fn bool rgfw_isLatin(char* string, sz length)
{
	for (sz i = 0; i < length; i++) if ((char)string[i] >= 0x80) return true;
	return false;
}

fn uint rgfw_decodeUTF8(char* string, sz* starting_index)
{
	uint[6] offsets = {0x00000000u, 0x00003080u, 0x000e2080u, 0x03c82080u, 0xfa082080u, 0x82082080u};
	uint codepoint = (char)string[*starting_index];
	sz count = 1;
	while ((string[count + *starting_index] & 0xc0) == 0x80)
	{
		codepoint = (codepoint << 6) + (char)string[count + *starting_index];
		count++;
	}
	*starting_index += count;
	return codepoint - offsets[count - 1];
}

fn void show(GfxWindow* win) { if (win.internal.flags & RGFW_WINDOW_FOCUS_ON_SHOW) rgfw_window_focus(win); win.internal.flags &= ~((uint)RGFW_WINDOW_HIDE); rgfw_window_showPlatform(win); if (win.internal.flags & RGFW_WINDOW_MAXIMIZE) rgfw_window_maximize(win); }
fn void rgfw_window_restore(GfxWindow* win) { rgfw_window_restorePlatform(win); win.internal.flags &= ~((uint)RGFW_WINDOW_MAXIMIZE); show(win); rgfw_window_move(win, win.internal.oldX, win.internal.oldY); rgfw_window_resize(win, win.internal.oldW, win.internal.oldH); }
fn void rgfw_window_minimize(GfxWindow* win) { rgfw_window_minimizePlatform(win); win.internal.flags &= ~((uint)RGFW_WINDOW_MAXIMIZE); }
fn void rgfw_window_maximize(GfxWindow* win) { win.internal.oldX = win.x; win.internal.oldY = win.y; win.internal.oldW = win.w; win.internal.oldH = win.h; win.internal.flags |= RGFW_WINDOW_MAXIMIZE; rgfw_window_fetchSize(win, null, null); rgfw_window_maximizePlatform(win); }
fn void rgfw_window_move(GfxWindow* win, int x, int y) { win.x = x; win.y = y; rgfw_window_movePlatform(win, x, y); win.internal.flags &= ~((uint)RGFW_WINDOW_MAXIMIZE); }
fn void rgfw_window_resize(GfxWindow* win, int w, int h) { win.w = w; win.h = h; rgfw_window_resizePlatform(win, w, h); win.internal.flags &= ~((uint)RGFW_WINDOW_MAXIMIZE); }

module std::gfx @private;

fn void _remove_monitor(RgfwMonitorNode* node, RgfwMonitorNode* prev)
{
	_rgfw.monitors.count -= 1;
	rgfw_monitorNode_free(node);
	if (prev != node)
	{
		prev.next = node.next;
	}
	else
	{
		_rgfw.monitors.list.head = node.next;
	}
	node.next = null;
	free(node);
}

fn void _mouse_notify_callback(GfxWindow* win, int x, int y, bool status)
{
	win.internal.mouseInside = status;
	_rgfw.windowState.win = win;
	win.internal.lastMouseX = x;
	win.internal.lastMouseY = y;
	GfxEvent event;
	event.common.win = win;
	event.mouse.x = x; event.mouse.y = y; event.mouse.inWindow = win.internal.mouseInside;
	if (status)
	{
		if (!(win.internal.enabledEvents & RGFW_MOUSE_ENTER_FLAG)) return;
		_rgfw.windowState.mouseEnter = true;
		event.type = RGFW_MOUSE_ENTER;
	}
	else
	{
		if (!(win.internal.enabledEvents & RGFW_MOUSE_LEAVE_FLAG)) return;
		_rgfw.windowState.winLeave = win;
		_rgfw.windowState.mouseLeave = true;
		event.type = RGFW_MOUSE_LEAVE;
	}
	event_queue_call_push(&event);
}
