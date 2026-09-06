module std::gfx;
import std::math;

const bool RGFW_BUILD_MACOS = $feat(MACOS);
const bool RGFW_BUILD_X11 = $feat(X11) || $feat(X11_WAYLAND);
const bool RGFW_BUILD_WAYLAND = $feat(WAYLAND);
const bool RGFW_BUILD_WINDOWS = $feat(WIN32);
const bool RGFW_BUILD_EGL = $feat(X11_WAYLAND) || $feat(X11) || $feat(WAYLAND) || $feat(MACOS);
const bool RGFW_BUILD_WASM = $feat(WASM);


macro void* rgfw_memcpy(void* dest, void* src, sz count) => memcpy(dest, src, count);
// public
fn void* rgfw_alloc_impl(sz size) => malloc(size);
// public
fn void rgfw_free_impl(void* ptr) { if (ptr != null) free(ptr); }
// public
fn void* rgfw_alloc(sz size) { return rgfw_alloc_impl(size); }
// public
fn void rgfw_free(void* ptr) { rgfw_free_impl(ptr); }
// public
fn void rgfw_debugCallback(RgfwDebugType type, RgfwErrorCode code, ZString msg)
{
	RgfwDebugInfo info;
	info.type = type;
	info.code = code;
	info.msg = msg;
	if (_gfx_ctx.debugCallbackSrc != null) _gfx_ctx.debugCallbackSrc(&info);
}

// public
fn RgfwDebugFunc rgfw_setDebugCallback(RgfwDebugFunc func)
{
	RgfwDebugFunc prev = _gfx_ctx.debugCallbackSrc;
	_gfx_ctx.debugCallbackSrc = func;
	return prev;
}

const MAX_EVENTS = 32;
const PREALLOCATED_MONITORS = 0;

alias RgfwKey = char;
alias RgfwKeymod = char;
alias RgfwDndActionType = char;
alias RgfwDataTransferType = char;
alias RgfwEventFlag = uint;
alias RgfwEventWait = int;
alias RgfwIcon = char;
alias RgfwDebugType = char;
alias RgfwErrorCode = char;
alias RgfwGlReleaseBehavior = int;
alias RgfwGlProfile = int;
alias RgfwGlRenderer = int;
alias RgfwGfxContextType = uint;

typedef RgfwMouse = void;
alias GfxFn = fn void();
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
	IntVec2 size;
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
	Vec2 scale;
	float pixelRatio;
	Vec2 phys_size;
	GfxMonitorMode mode;
	void* userPtr;
	GfxMonitorNode* node;
}

struct RgfwKeyState
{
	bool current;
	bool prev;
}

struct GfxWindow
{
	inline IntRect bounds;
	RgfwWindowSrc src;
	GfxWindowInternal internal;
	void* userPtr;
}

struct GfxMonitorNode
{
	GfxMonitor mon;
	bool disconnected;
	GfxMonitorNode* next;
	void* native0;
	void* native1;
	ulong nativeId0;
	ulong nativeId1;
	struct @feat(X11_WAYLAND, WAYLAND)
	{
		uint id;
		WlProxy* output;
		WlProxy* xdgOutput;
		GfxMonitorMode* modes;
		sz modeCount;
	}
	struct @feat(WIN32)
	{
		void* hMonitor; // HMONITOR
		ushort[32] adapterName;
		ushort[32] deviceName;
	}
}

struct GfxWindowInternal
{
	RgfwKey exitKey;
	IntVec2 last_mouse;
	bool shouldClose;
	bool rawMouse;
	bool captureMouse;
	bool inFocus;
	bool mouseInside;
	RgfwKeymod mod;
	RgfwEventFlag enabledEvents;
	GfxWindowFlags flags;
	IntRect old_bounds;
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
	GfxMonitorNode* primary;
}

struct GfxMonitorList
{
	GfxMonitorNode* head;
	GfxMonitorNode* cur;
}

struct GfxDataTransfer
{
	char* data;
	sz length;
	RgfwDataTransferType type;
}

struct RgfwInfo // WASM
{
	bool initialized;
	GfxWindow* root;
	int windowCount;
	RgfwMouse* hiddenMouse;
	RgfwMouse*[21] standardMice;
	RgfwDebugFunc debugCallbackSrc;
	GfxGenericFunc[25] callbacks;
	GfxEvent[MAX_EVENTS] events;
	int eventBottom;
	int eventLen;
	bool queueEvents;
	bool polledEvents;
	uint[RGFW_KEY_LAST] apiKeycodes;
	RgfwKey[$feat(WIN32) ? 512 : 256] keycodes;
	bool stopCheckEvents_bool;
	ulong timerOffset;
	int[3] eventWait_forceStop @feat(X11_WAYLAND);
	GfxDataTransfer* clipboard;
	bool dndBuild;
	GfxDataDropNode* dndRoot;
	GfxDataDropNode* dndCur;

	GfxMonitors monitors;
	GfxWindow* current;
	void* nativeGL_handle;
	void* glGetProcAddress;
	void* vulkan_handle;
	GfxVkGetInstanceProcAddressProc vkGetInstanceProcAddress;
	struct @feat(!WASM)
	{
		void* egl_display;
		void* egl_handle;
		void* eglGetProcAddress;
	}
	struct @feat(WIN32)
	{
		void* instance; // HINSTANCE
		WndClassW wndClass;
		ushort[256] wideClassName;
		void* helperWindow; // HWND
	}
	struct @feat(WASM)
	{
		int usingAsyncify;
	}
	struct @feat(MACOS)
	{
		void* x11_handle;
		void* x11_display;
		CULong x11_root;
		void* wayland_handle;
		void* wl_display;
		ObjcId nsApp;
		ObjcId nsPool;
		NSUInteger windowNumberSeed;
	}
	GfxDataTransfer* unixClipboard @feat(WAYLAND, X11_WAYLAND, X11);

	struct @feat(WAYLAND, X11_WAYLAND)
	{
		WlDisplay* wlDisplay;
		WlProxy* registry;
		WlProxy* compositor;
		WlProxy* seat;
		WlProxy* shm;
		WlProxy* wlPointer;
		WlProxy* wlKeyboard;
		WlProxy* xdgWmBase;
		WlProxy* decorationManager;
		WlProxy* constraintManager;
		WlProxy* relativePointerManager;
		WlProxy* relativePointer;
		WlProxy* iconManager;
		WlProxy* xdgOutputManager;
		WlProxy* wpPointerWarp;
		WlProxy* dataDeviceManager;
		WlProxy* dataDevice;
		WlCursorTheme* wlCursorTheme;
		WlProxy* cursorSurface;
		GfxWindow* mouseOwner;
		GfxWindow* kbOwner;
		uint mouseEnterSerial;
		XkbContext* xkbContext;
		XkbKeymap* keymap;
		XkbState* xkbState;
		XkbComposeState* composeState;
		int wlRepeatInfoRate;
		int wlRepeatInfoDelay;
		uint lastKey;
		uint lastKeyTime;
	}
	struct @feat(X11, X11_WAYLAND)
	{
		GfxXDisplay* display;
		int context;
		GfxXWindow helperWindow;
		String class_name;
		XlibIM im;
		int xrandrEventBase;
		GfxXWindow x11Source;
		int x11Version;
		int x11Format;
		RgfwDataTransferType x11TransferType;
		XErrorEvent* x11Error;
		Atom netCurrentDesktop, netWmStateMaximizedVert, netWmStateMaximizedHorz, netWmBypassCompositor;
		Atom netWmState, motifWmHints, wmDeleteWindow, netWmSyncRequestCounter, netWmSyncRequest;
		Atom atomPair, multiple, targets, saveTargets, utf8String;
		Atom xdndTypeList, xdndSelection, xdndAware, xdndEnter, xdndPosition, xdndStatus, xdndLeave, xdndDrop, xdndFinished, xdndActionCopy;
		Atom wmProtocols, wmState, netWmStateFullscreen, netWmStateAbove, netWmWindowOpacity;
		Atom netWmName, netWmIcon, xselData, clipboardAtom, clipboardManager;
		Atom xtextUriList, xtextPlain, netWorkarea;
	}
	bool useWaylandBool @feat(X11_WAYLAND);
	bool rawMouse;
	RgfwWindowState windowState;
	RgfwKeyState[8] mouseButtons;
	RgfwKeyState[RGFW_KEY_LAST] keyboard;
	float scrollX, scrollY;
	float vectorX, vectorY;
}


struct GfxDataDropNode
{
	char* data;
	sz length;
	RgfwDataTransferType type;
	GfxDataDropNode* next;
}

struct RgfwCommonEvent { GfxEventType type; GfxWindow* win; }
struct RgfwWindowFocusEvent { GfxEventType type; GfxWindow* win; bool state; }
struct RgfwMouseButtonEvent { GfxEventType type; GfxWindow* win; GfxMouseButton value; bool state; }
struct RgfwMouseDeltaEvent { GfxEventType type; GfxWindow* win; float x, y; }
struct RgfwMouseMotionEvent { GfxEventType type; GfxWindow* win; int x, y; bool inWindow; }
struct RgfwKeyEvent { GfxEventType type; GfxWindow* win; RgfwKey value; bool repeat; RgfwKeymod mod; bool state; }
struct RgfwKeyCharEvent { GfxEventType type; GfxWindow* win; uint value; }
struct RgfwDataDropEvent { GfxEventType type; GfxWindow* win; GfxDataDropNode* value; }
struct RgfwDataDragEvent { GfxEventType type; GfxWindow* win; int x, y; RgfwDndActionType action; RgfwDataTransferType dataType; }
struct RgfwScaleUpdatedEvent { GfxEventType type; GfxWindow* win; float x, y; }
struct RgfwMonitorEvent { GfxEventType type; GfxWindow* win; GfxMonitor* monitor; bool state; }
struct RgfwWindowUpdateEvent { GfxEventType type; GfxWindow* win; int x, y, w, h; }

union GfxEvent
{
	GfxEventType type;
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
	ZString msg;
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

constset GfxInitOption : char
{
	NONE         = 0b0000,
	INIT_OPEN_GL = 0b0001,
	INIT_EGL     = 0b0010,
	INIT_VULKAN  = 0b0100,
	ENABLE_X11   = 0b1000,
}

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

enum GfxMouseButton
{
	LEFT,
	MIDDLE,
	RIGHT,
	MISC1,
	MISC2,
	MISC3,
	MISC4,
	MISC5,
}

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

enum GfxEventType : char
{
	NONE,
	KEY_PRESSED,
	KEY_RELEASED,
	KEY_CHAR,
	MOUSE_BUTTON_PRESSED,
	MOUSE_BUTTON_RELEASED,
	MOUSE_SCROLL,
	MOUSE_MOTION,
	MOUSE_RAW_MOTION,
	MOUSE_ENTER,
	MOUSE_LEAVE,
	WINDOW_MOVED,
	WINDOW_RESIZED,
	WINDOW_FOCUS_IN,
	WINDOW_FOCUS_OUT,
	WINDOW_REFRESH,
	WINDOW_CLOSE,
	WINDOW_MAXIMIZED,
	WINDOW_MINIMIZED,
	WINDOW_RESTORED,
	DATA_DROP,
	DATA_DRAG,
	SCALE_UPDATED,
	MONITOR_CONNECTED,
	MONITOR_DISCONNECTED,
}

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

bitstruct GfxWindowFlags : uint
{
	bool no_border;
	bool no_resize;
	bool allow_dnd;
	bool hide_mouse;
	bool fullscreen;
	bool transparent;
	bool center;
	bool raw_mouse;
	bool scale_to_monitor;
	bool hide;
	bool maximize;
	bool center_cursor;
	bool floating;
	bool focus_on_show;
	bool minimize;
	bool focus;
	bool capture_mouse;
	bool open_gl;
	bool egl;
}

const GfxWindowFlags WINDOWED_FULLSCREEN = { .no_border, .maximize };
const GfxWindowFlags CAPTURE_RAW_MOUSE = { .capture_mouse, .raw_mouse };


enum GfxMouseIcon : char
{
	NORMAL,
	ARROW,
	IBEAM,
	CROSSHAIR,
	POINTING_HAND,
	RESIZE_EW,
	RESIZE_NS,
	RESIZE_NWSE,
	RESIZE_NESW,
	RESIZE_NW,
	RESIZE_N,
	RESIZE_NE,
	RESIZE_E,
	RESIZE_SE,
	RESIZE_S,
	RESIZE_SW,
	RESIZE_W,
	RESIZE_ALL,
	NOT_ALLOWED,
	WAIT,
	PROGRESS,
}

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


// public
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
// public
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

// public
fn void copy_image_data(char* dest_data, int w, int h, GfxFormat dest_format, char* src_data, GfxFormat src_format, GfxConvertImageDataFn func)
{
	copy_image_data_64(dest_data, w, h, dest_format, src_data, src_format, false, func);
}

// public
fn int init(String class_name, GfxInitOption options)
{
	if (_gfx_ctx.initialized) return 1;
	class_name = class_name ?: "C3GFX";
	_gfx_ctx = { .initialized = true };
	if (options & INIT_OPEN_GL && !rgfw_loadGL())
	{
		log::error("Failed to load the OpenGL library", category: log::CATEGORY_VIDEO);
		return -1;
	}
	if (options & INIT_EGL && !load_egl())
	{
		log::error("Failed to load the EGL library", category: log::CATEGORY_VIDEO);
		return -1;
	}
	if (options & INIT_VULKAN && !rgfw_loadVulkan())
	{
		log::error("Failed to load the Vulkan library", category: log::CATEGORY_VIDEO);
		return -1;
	}
	rgfw_initKeycodes();
	int out = rgfw_initPlatform(class_name, options);
	if (out != 0)
	{
		log::error("Failed to initialize global context", category: log::CATEGORY_VIDEO);
		rgfw_deinitPlatform();
		_gfx_ctx = {};
		return out;
	}
	foreach (icon : GfxMouseIcon::values)
	{
		_gfx_ctx.standardMice[icon.ordinal] = rgfw_createMouseStandard(icon);
	}
	rgfw_pollMonitors();
	log::info("Global context initialized", category: log::CATEGORY_VIDEO);
	return out;
}

// public
fn void finalize()
{
	if (!_gfx_ctx.initialized) return;
	foreach (icon : GfxMouseIcon::values)
	{
		if (_gfx_ctx.standardMice[icon.ordinal]) rgfw_freeMouse(_gfx_ctx.standardMice[icon.ordinal]);
	}
	rgfw_freeMonitors();
	log::info("Global context destroyed", category: log::CATEGORY_VIDEO);
	rgfw_deinitPlatform();
	unload_egl();
	unload_gl();
	_gfx_ctx.root = null;
	_gfx_ctx.windowCount = 0;
	_gfx_ctx = {};
}

// public
fn GfxSurface* create_surface(char* data, int w, int h, GfxFormat format)
{
	GfxSurface* surface = mem::new(GfxSurface);
	rgfw_createSurfacePtr(data, w, h, format, surface);
	return surface;
}

// public
fn GfxWindow* create_window(ZString name, int x, int y, int w, int h, GfxWindowFlags flags)
{
	GfxWindow* win = mem::new(GfxWindow);
	return rgfw_createWindowPtr(name, x, y, w, h, flags, win);
}

// public
fn bool is_mouse_pressed(GfxMouseButton button) => _gfx_ctx.mouseButtons[(sz)button].current && !_gfx_ctx.mouseButtons[(sz)button].prev;

// public
fn IntVec2 GfxWindow.get_mouse(&win) => win.internal.last_mouse;

<*
 Check if a window should be closed.

 @param [&in] win
 @return "true is it is tagged as should close or its exit key is held down"
*>
// public
fn bool window_should_close(GfxWindow* win)
{
	return win.internal.shouldClose || (win.internal.exitKey && is_key_down(win.internal.exitKey));
}

// public
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

// public
fn void window_scale_to_monitor(GfxWindow* win)
{
	GfxMonitor* monitor = find_monitor(win);
	if (monitor == null) return;
	rgfw_window_resize(win, ...(IntVec2)(monitor.scale * (Vec2)win.size));
}

// public
fn void window_move_to_monitor(GfxWindow* win, GfxMonitor* m)
{
	rgfw_window_move(win, ...(m.origin + win.origin));
}

// public
fn void GfxWindow.set_flags(&win, GfxWindowFlags flags) { _window_set_flags(win, flags, win.internal.flags); }
// public
fn bool GfxWindow.has_focus(&win) => win.internal.inFocus;
// public
fn void rgfw_setBuildDND(bool state) { _gfx_ctx.dndBuild = state; }

// public
fn bool is_key_pressed(RgfwKey key, GfxWindow* window = null)
{
	return _gfx_ctx.keyboard[key].current && !_gfx_ctx.keyboard[key].prev && (!window || window.has_focus());
}
// public
fn bool is_key_down(RgfwKey key, GfxWindow* window = null)
{
	return _gfx_ctx.keyboard[key].current && (!window || window.has_focus());
}
// public
fn bool is_key_released(RgfwKey key, GfxWindow* window = null)
{
	return !_gfx_ctx.keyboard[key].current && _gfx_ctx.keyboard[key].prev && (!window || window.has_focus());
}
// public
fn bool is_mouse_down(GfxMouseButton button, GfxWindow* window = null)
{
	return _gfx_ctx.mouseButtons[(sz)button].current && (!window || window.has_focus());
}

// public
fn bool is_mouse_released(GfxMouseButton button, GfxWindow* window = null)
{
	return !_gfx_ctx.mouseButtons[button].current && _gfx_ctx.mouseButtons[button].prev && (!window || window.has_focus());
}

// public
fn float[<2>] get_mouse_scroll() => { _gfx_ctx.scrollX, _gfx_ctx.scrollY };
// public
fn float[<2>] get_mouse_vector() => { _gfx_ctx.vectorX, _gfx_ctx.vectorY };

// public
fn void GfxWindow.destroy(&self)
{
	rgfw_window_closePtr(self);
	free(self);
}

// public
fn void rgfw_window_closePtr(GfxWindow* win)
{
	if (!win) return;
	if (win.internal.captureMouse) rgfw_window_captureMouse(win, false);
	$if $feat(MACOS):
	if ((win.src.gfxType & RGFW_GFX_EGL) && win.src.ctx.egl != null)
	{
		rgfw_window_deleteContext_EGL(win, win.src.ctx.egl);
		win.src.ctx.egl = null;
	}
	if ((win.src.gfxType & RGFW_GFX_NATIVE_OPEN_GL) && win.src.ctx.native != null)
	{
		rgfw_window_deleteContext_OpenGL(win, win.src.ctx.native);
		win.src.ctx.native = null;
	}
	$endif
	$if $feat(WASM) || $feat(WIN32):
	if ((win.src.gfxType & RGFW_GFX_NATIVE_OPEN_GL) && win.src.ctx.native != null)
	{
		rgfw_window_deleteContext_OpenGL(win, win.src.ctx.native);
		win.src.ctx.native = null;
	}
	$endif
	rgfw_window_closePlatform(win);
	if (_gfx_ctx.clipboard) free(_gfx_ctx.clipboard);
	_gfx_ctx.clipboard = null;
	_gfx_ctx.windowCount--;
	log::debug("A window was freed", category: log::CATEGORY_VIDEO);
}

// public
fn GfxGenericFunc set_event_callback(GfxEventType type, GfxGenericFunc func)
{
	GfxGenericFunc old = _gfx_ctx.callbacks[type];
	_gfx_ctx.callbacks[type] = func;
	return old;
}

// public
fn void set_dual_event_callback(GfxEventType type, GfxGenericFunc func, GfxGenericFunc* first, GfxGenericFunc* second)
{
	GfxGenericFunc func1 = set_event_callback(type, func);
	GfxGenericFunc func2 = set_event_callback((GfxEventType)(type.ordinal + 1), func);
	if (first != null) *first = func1;
	if (second != null) *second = func2;
}

// public
fn void set_all_event_callback(GfxGenericFunc func, RgfwCallbacks* callbacks)
{
	foreach (i : GfxEventType::values[1..])
	{
		if (callbacks != null) callbacks.arr[i.ordinal] = _gfx_ctx.callbacks[i.ordinal];
		set_event_callback(i, func);
	}
}


// public
fn void rgfw_eventQueueFlush() { _gfx_ctx.eventLen = 0; }

// public
fn void event_queue_push(GfxEvent* event)
{
	if (!_gfx_ctx.queueEvents) return;
	if (_gfx_ctx.eventLen >= MAX_EVENTS)
	{
		log::error("Event queue limit 'MAX_EVENTS' has been reached automatically flushing queue.", category: log::CATEGORY_VIDEO);
		rgfw_eventQueueFlush();
		return;
	}
	int eventTop = (_gfx_ctx.eventBottom + _gfx_ctx.eventLen) % MAX_EVENTS;
	_gfx_ctx.eventLen += 1;
	_gfx_ctx.events[eventTop] = *event;
}

// public
fn GfxEvent? event_queue_pop()
{
	if (!_gfx_ctx.eventLen) return NO_MORE_ELEMENT~;
	GfxEvent* ev = &_gfx_ctx.events[_gfx_ctx.eventBottom];
	_gfx_ctx.eventLen--;
	_gfx_ctx.eventBottom = (_gfx_ctx.eventBottom + 1) % MAX_EVENTS;
	return *ev;
}


// public
fn GfxEvent? check_event()
{
	if (!_gfx_ctx.eventLen && !_gfx_ctx.polledEvents)
	{
		_gfx_ctx.queueEvents = true;
		rgfw_pollEvents();
		_gfx_ctx.polledEvents = true;
	}
	defer catch _gfx_ctx.polledEvents = false;
	return check_queued_event();
}

// public
fn GfxEvent? check_queued_event()
{
	_gfx_ctx.queueEvents = true;
	return event_queue_pop();
}

// public
fn GfxEvent? window_event_queue_pop(GfxWindow* win)
{
	GfxEvent ev = event_queue_pop()!;
	for (int i = 1; i < _gfx_ctx.eventLen && ev.common.win != win && ev.common.win != null; i++)
	{
		event_queue_push(&ev);
		ev = event_queue_pop()!!;
	}
	if (ev.common.win != win && ev.common.win != null) return NO_MORE_ELEMENT~;
	return ev;
}

// public
fn GfxEvent? check_queued_window_event(GfxWindow* win)
{
	return window_event_queue_pop(win);
}

// public
fn GfxEvent? check_window_event(GfxWindow* win)
{
	if (!_gfx_ctx.eventLen && !_gfx_ctx.polledEvents)
	{
		_gfx_ctx.queueEvents = true;
		rgfw_pollEvents();
		_gfx_ctx.polledEvents = true;
	}
	defer catch _gfx_ctx.polledEvents = false;
	return check_queued_window_event(win);
}

// public
fn void _window_set_flags(GfxWindow* win, GfxWindowFlags flags, GfxWindowFlags cmpFlags) @private
{
	switch
	{
		case flags.no_border: rgfw_window_setBorder(win, false);
		case cmpFlags.no_border: rgfw_window_setBorder(win, true);
	}
	switch
	{
		case flags.maximize: rgfw_window_maximize(win);
		case cmpFlags.maximize: restore_window(win);
	}
	switch
	{
		case flags.minimize: rgfw_window_minimize(win);
		case cmpFlags.minimize: restore_window(win);
	}
	if (flags.center) rgfw_window_center(win);
	if (flags.center_cursor) rgfw_window_moveMouse(win, win.x + (win.width / 2), win.y + (win.height / 2));
	switch
	{
		case flags.fullscreen: rgfw_window_setFullscreen(win, true);
		case cmpFlags.fullscreen: rgfw_window_setFullscreen(win, false);
	}
	if (flags.hide_mouse) { rgfw_window_showMouse(win, false); }
	else if (cmpFlags.hide_mouse) { rgfw_window_showMouse(win, true); }
	if (flags.hide) { rgfw_window_hide(win); }
	else if (cmpFlags.hide) { show(win); }
	if (flags.floating) { rgfw_window_setFloating(win, true); }
	else if (cmpFlags.floating) { rgfw_window_setFloating(win, false); }
	if (flags.raw_mouse) { rgfw_window_setRawMouseMode(win, true); }
	else if (cmpFlags.raw_mouse) { rgfw_window_setRawMouseMode(win, false); }
	if (flags.capture_mouse) { rgfw_window_captureRawMouse(win, true); }
	else if (cmpFlags.capture_mouse) { rgfw_window_captureMouse(win, false); }
	if (flags.focus) rgfw_window_focus(win);
	if (flags.scale_to_monitor) window_scale_to_monitor(win);
	switch (true)
	{
		case flags.no_resize:
			rgfw_window_setMaxSize(win, win.width, win.height);
			rgfw_window_setMinSize(win, win.width, win.height);
		case cmpFlags.no_resize:
			rgfw_window_setMaxSize(win, 0, 0);
			rgfw_window_setMinSize(win, 0, 0);
	}
	win.internal.flags = flags;
}

// public
fn void _window_restored_callback(GfxWindow* win, int x, int y, int w, int h) @private
{
	win.internal.flags.minimize = false;
	win.bounds = { x, y, w, h };
	if (rgfw_window_isMaximized(win) == false) win.internal.flags.maximize = false;
	if (!(win.internal.enabledEvents & RGFW_WINDOW_RESTORED_FLAG)) return;
	GfxEvent event;
	event.type = WINDOW_RESTORED;
	event.update.x = x; event.update.y = y; event.update.w = w; event.update.h = h;
	event.common.win = win;
	_event_queue_call_push(&event);
}

// private
fn void rgfw_window_checkMode(GfxWindow* win)
{
	if (rgfw_window_isMinimized(win))
	{
		rgfw_windowMinimizedCallback(win);
	}
	else if (rgfw_window_isMaximized(win))
	{
		rgfw_windowMaximizedCallback(win, win.x, win.y, win.width, win.height);
	}
	else if (((win.internal.flags.minimize) && !rgfw_window_isMaximized(win)) ||
	((win.internal.flags.maximize) && !rgfw_window_isMaximized(win)))
	{
		_window_restored_callback(win, win.x, win.y, win.width, win.height);
	}
}

// public
fn void _window_moved_callback(GfxWindow* win, int x, int y) @private
{
	win.x = x; win.y = y;
	if (!(win.internal.enabledEvents & RGFW_WINDOW_MOVED_FLAG)) return;
	GfxEvent event;
	event.type = WINDOW_MOVED;
	event.update.x = x; event.update.y = y; event.update.w = win.width; event.update.h = win.height;
	event.common.win = win;
	_event_queue_call_push(&event);
}

// public
fn void _window_resized_callback(GfxWindow* win, int w, int h) @private
{
	win.width = w; win.height = h;
	if (!(win.internal.enabledEvents & RGFW_WINDOW_RESIZED_FLAG)) return;
	GfxEvent event;
	event.type = WINDOW_RESIZED;
	event.update.x = win.x; event.update.y = win.y; event.update.w = w; event.update.h = h;
	event.common.win = win;
	_event_queue_call_push(&event);
}

// public
fn void _window_close_callback(GfxWindow* win) @private
{
	win.internal.shouldClose = true;
	GfxEvent event;
	event.type = WINDOW_CLOSE;
	event.common.win = win;
	_event_queue_call_push(&event);
}


// public
fn void _raw_motion_callback(GfxWindow* win, float x, float y)
{
	_gfx_ctx.vectorX = x; _gfx_ctx.vectorY = y;
	if (!(win.internal.enabledEvents & RGFW_MOUSE_RAW_MOTION_FLAG)) return;
	GfxEvent event;
	event.type = MOUSE_RAW_MOTION;
	event.delta.x = x; event.delta.y = y;
	event.common.win = win;
	_event_queue_call_push(&event);
}

// public
fn void rgfw_windowRefreshCallback(GfxWindow* win, int x, int y, int w, int h)
{
	if (!(win.internal.enabledEvents & RGFW_WINDOW_REFRESH_FLAG)) return;
	GfxEvent event;
	event.type = WINDOW_REFRESH;
	event.update.x = x; event.update.y = y; event.update.w = w; event.update.h = h;
	event.common.win = win;
	_event_queue_call_push(&event);
}

// public
fn void rgfw_mouseButtonCallback(GfxWindow* win, GfxMouseButton button, bool press)
{
	GfxEvent event;
	if (press)
	{
		if (!(win.internal.enabledEvents & RGFW_MOUSE_BUTTON_PRESSED_FLAG)) return;
		event.type = MOUSE_BUTTON_PRESSED;
	}
	else
	{
		if (!(win.internal.enabledEvents & RGFW_MOUSE_BUTTON_RELEASED_FLAG)) return;
		event.type = MOUSE_BUTTON_RELEASED;
	}
	_gfx_ctx.mouseButtons[(sz)button].prev = _gfx_ctx.mouseButtons[(sz)button].current;
	_gfx_ctx.mouseButtons[(sz)button].current = press;
	event.button.value = button;
	event.button.state = press;
	event.common.win = win;
	_event_queue_call_push(&event);
}

// public
fn void rgfw_mouseScrollCallback(GfxWindow* win, float x, float y)
{
	if (!(win.internal.enabledEvents & RGFW_MOUSE_SCROLL_FLAG)) return;
	_gfx_ctx.scrollX = x; _gfx_ctx.scrollY = y;
	GfxEvent event;
	event.type = MOUSE_SCROLL;
	event.delta.x = x; event.delta.y = y;
	event.common.win = win;
	_event_queue_call_push(&event);
}

// public
fn void rgfw_scaleUpdatedCallback(GfxWindow* win, float scaleX, float scaleY)
{
	if (!(win.internal.enabledEvents & RGFW_SCALE_UPDATED_FLAG)) return;
	GfxEvent event;
	event.type = SCALE_UPDATED;
	event.scale.x = scaleX; event.scale.y = scaleY;
	event.common.win = win;
	_event_queue_call_push(&event);
}

// public
fn void rgfw_monitorCallback(GfxWindow* win, GfxMonitor* monitor, bool connected)
{
	if (win != null)
	{
		if (connected && !(win.internal.enabledEvents & RGFW_MONITOR_CONNECTED_FLAG)) return;
		if (!connected && !(win.internal.enabledEvents & RGFW_MONITOR_DISCONNECTED_FLAG)) return;
	}
	GfxEvent event;
	event.type = connected ? MONITOR_CONNECTED : MONITOR_DISCONNECTED;
	event.monitor.monitor = monitor;
	event.monitor.state = connected;
	event.common.win = win;
	_event_queue_call_push(&event);
}

// public
fn void rgfw_windowMaximizedCallback(GfxWindow* win, int x, int y, int w, int h)
{
	win.internal.flags.maximize = true;
	win.x = x; win.y = y; win.width = w; win.height = h;
	if (!(win.internal.enabledEvents & RGFW_WINDOW_MAXIMIZED_FLAG)) return;
	GfxEvent event;
	event.type = WINDOW_MAXIMIZED;
	event.update.x = x; event.update.y = y; event.update.w = w; event.update.h = h;
	event.common.win = win;
	_event_queue_call_push(&event);
}

// public
fn void rgfw_windowMinimizedCallback(GfxWindow* win)
{
	win.internal.flags.minimize = true;
	if (!(win.internal.enabledEvents & RGFW_WINDOW_MINIMIZED_FLAG)) return;
	GfxEvent event;
	event.type = WINDOW_MINIMIZED;
	event.update.x = win.x; event.update.y = win.y; event.update.w = win.width; event.update.h = win.height;
	event.common.win = win;
	_event_queue_call_push(&event);
}

// public
fn void rgfw_windowFocusCallback(GfxWindow* win, bool inFocus)
{
	win.internal.inFocus = inFocus;
	if (win.internal.captureMouse) rgfw_window_captureMousePlatform(win, inFocus);
	GfxEvent event;
	event.common.win = win;
	event.focus.state = inFocus;
	if (inFocus == true)
	{
		if (win.internal.flags.fullscreen) rgfw_window_raise(win);
		event.type = WINDOW_FOCUS_IN;
	}
	else
	{
		if (win.internal.flags.fullscreen) rgfw_window_minimize(win);
		for (sz key = 0; key < (sz)RGFW_KEY_LAST; key++)
		{
			if (is_key_down((RgfwKey)key) == false) continue;
			_gfx_ctx.keyboard[key].current = false;
			if (win.internal.enabledEvents & RGFW_KEY_RELEASED_FLAG) rgfw_keyCallback(win, (RgfwKey)key, win.internal.mod, false, false);
		}
		rgfw_resetKey();
		event.type = WINDOW_FOCUS_OUT;
	}
	_event_queue_call_push(&event);
}


// public
fn void rgfw_dataDragCallback(GfxWindow* win, RgfwDataTransferType dataType, RgfwDndActionType action, int x, int y)
{
	if (!(win.internal.enabledEvents & RGFW_DATA_DRAG_FLAG) || !win.internal.flags.allow_dnd) return;
	_gfx_ctx.windowState.win = win;
	_gfx_ctx.windowState.dataDragging = true;
	_gfx_ctx.windowState.dropX = x;
	_gfx_ctx.windowState.dropY = y;
	GfxEvent event;
	event.type = DATA_DRAG;
	event.drag.x = x; event.drag.y = y; event.drag.action = action; event.drag.dataType = dataType;
	event.common.win = win;
	_event_queue_call_push(&event);
}

// public
fn void rgfw_dataDropCallback(GfxWindow* win, char* data, sz length, RgfwDataTransferType dataType)
{
	if (!(win.internal.enabledEvents & RGFW_DATA_DROP_FLAG) || !win.internal.flags.allow_dnd) return;
	_gfx_ctx.windowState.win = win;
	_gfx_ctx.windowState.dataDrop = true;
	_gfx_ctx.windowState.dataLength = length;
	GfxDataDropNode node = { .data = data, .length = length, .type = dataType };
	GfxEvent event;
	event.type = DATA_DROP;
	event.drop.value = &node;
	event.common.win = win;
	if (_gfx_ctx.callbacks[(sz)event.type] != null) _gfx_ctx.callbacks[(sz)event.type](&event);
	if (_gfx_ctx.queueEvents == false && _gfx_ctx.dndBuild == false) return;
	char* dataCopy = (char*)rgfw_alloc_impl(length);
	if (dataCopy != null) { rgfw_memcpy(dataCopy, data, length); node.data = dataCopy; }
	GfxDataDropNode* heapNode = (GfxDataDropNode*)rgfw_alloc_impl(GfxDataDropNode::size);
	if (heapNode == null) return;
	*heapNode = node;
	if (_gfx_ctx.dndRoot == null) { _gfx_ctx.dndRoot = heapNode; _gfx_ctx.dndCur = heapNode; }
	else { _gfx_ctx.dndCur.next = heapNode; _gfx_ctx.dndCur = heapNode; }
	event.drop.value = heapNode;
	event_queue_push(&event);
}

// public
fn void rgfw_keyCharCallback(GfxWindow* win, uint codepoint)
{
	if (!(win.internal.enabledEvents & RGFW_KEY_CHAR_FLAG)) return;
	GfxEvent event;
	event.type = KEY_CHAR;
	event.keyChar.value = codepoint;
	event.common.win = win;
	_event_queue_call_push(&event);
}

// public
fn void rgfw_keyCallback(GfxWindow* win, RgfwKey key, RgfwKeymod mod, bool repeat, bool state)
{
	GfxEvent event;
	if (state)
	{
		if (!(win.internal.enabledEvents & RGFW_KEY_PRESSED_FLAG)) return;
		event.type = KEY_PRESSED;
	}
	else
	{
		if (!(win.internal.enabledEvents & RGFW_KEY_RELEASED_FLAG)) return;
		event.type = KEY_RELEASED;
	}
	_gfx_ctx.keyboard[(sz)key].prev = _gfx_ctx.keyboard[(sz)key].current;
	_gfx_ctx.keyboard[(sz)key].current = state;
	event.key.value = key; event.key.repeat = repeat; event.key.mod = mod; event.key.state = state;
	event.common.win = win;
	_event_queue_call_push(&event);
}

// public
fn void rgfw_initKeycodes()
{
	_gfx_ctx.keycodes = {};
	rgfw_initKeycodesPlatform();
	for (sz i = 0; i < (sz)RGFW_KEY_LAST; i++)
	{
		for (sz y = 0; y < _gfx_ctx.keycodes.len; y++)
		{
			if (_gfx_ctx.keycodes[y] == (RgfwKey)i)
			{
				_gfx_ctx.apiKeycodes[i] = (uint)y;
				break;
			}
		}
	}
	rgfw_resetKey();
}

// public
fn RgfwKey rgfw_apiKeyToRGFW(uint keycode)
{
	if (keycode >= _gfx_ctx.keycodes.len) return RGFW_KEY_NULL;
	return _gfx_ctx.keycodes[keycode];
}

// public
fn uint rgfw_rgfwToApiKey(RgfwKey keycode)
{
	return _gfx_ctx.apiKeycodes[keycode];
}

// public
fn void rgfw_resetKey()
{
	_gfx_ctx.keyboard = {};
}

// public
fn void rgfw_setQueueEvents(bool queue) { _gfx_ctx.queueEvents = queue; }

// public
fn void rgfw_resetPrevState()
{
	for (sz i = 0; i < (sz)RGFW_KEY_LAST; i++) _gfx_ctx.keyboard[i].prev = _gfx_ctx.keyboard[i].current;
	foreach (mouse : GfxMouseButton::values)
	{
		_gfx_ctx.mouseButtons[mouse.ordinal].prev = _gfx_ctx.mouseButtons[mouse.ordinal].current;
	}
	_gfx_ctx.scrollX = 0.0f; _gfx_ctx.scrollY = 0.0f;
	_gfx_ctx.vectorX = 0.0f; _gfx_ctx.vectorY = 0.0f;
	_gfx_ctx.windowState = {};
	GfxDataDropNode* node = _gfx_ctx.dndRoot;
	while (node != null)
	{
		GfxDataDropNode* next = node.next;
		rgfw_free_impl(node);
		node = next;
	}
	_gfx_ctx.dndRoot = null;
	_gfx_ctx.dndCur = null;
}


// public
fn RgfwWindowSrc* rgfw_window_getSrc(GfxWindow* win) => &win.src;
// public
fn IntVec2 GfxWindow.get_origin(&win) => win.origin;
// public
fn IntVec2 GfxWindow.get_size(&win) => win.size;
// public
fn GfxWindowFlags rgfw_window_getFlags(GfxWindow* win) => win.internal.flags;
// public
fn RgfwKey rgfw_window_getExitKey(GfxWindow* win) => win.internal.exitKey;
// public
fn void rgfw_window_setExitKey(GfxWindow* win, RgfwKey key) { win.internal.exitKey = key; }
// public
fn void rgfw_window_setEnabledEvents(GfxWindow* win, RgfwEventFlag events) { win.internal.enabledEvents = events; }
// public
fn RgfwEventFlag rgfw_window_getEnabledEvents(GfxWindow* win) => win.internal.enabledEvents;
// public
fn void rgfw_window_setDisabledEvents(GfxWindow* win, RgfwEventFlag events) { rgfw_window_setEnabledEvents(win, RGFW_ALL_EVENT_FLAGS & ~events); }
// public
fn void rgfw_window_setEventState(GfxWindow* win, RgfwEventFlag event, bool state) { rgfw_setBit(&win.internal.enabledEvents, event, state); }
// public
fn void* rgfw_window_getUserPtr(GfxWindow* win) => win.userPtr;
// public
fn void rgfw_window_setUserPtr(GfxWindow* win, void* ptr) { win.userPtr = ptr; }

// public
fn bool rgfw_window_getSizeInPixels(GfxWindow* win, int* w, int* h)
{
	GfxMonitor* mon = find_monitor(win);
	if (mon == null) return false;
	if (w != null) *w = (int)((float)win.width * mon.pixelRatio);
	if (h != null) *h = (int)((float)win.height * mon.pixelRatio);
	return true;
}

// public
fn bool rgfw_window_didMouseLeave(GfxWindow* win) => _gfx_ctx.windowState.winLeave == win && _gfx_ctx.windowState.mouseLeave;
// public
fn bool rgfw_window_didMouseEnter(GfxWindow* win) => _gfx_ctx.windowState.win == win && _gfx_ctx.windowState.mouseEnter;
// public
fn bool rgfw_window_isMouseInside(GfxWindow* win) => win.internal.mouseInside;
// public
fn bool rgfw_window_isDataDragging(GfxWindow* win) => rgfw_window_getDataDrag(win, null, null);
// public
fn bool rgfw_window_didDataDrop(GfxWindow* win) => rgfw_window_getDataDrop(win) != null;

// public
fn bool rgfw_window_getDataDrag(GfxWindow* win, int* x, int* y)
{
	if (_gfx_ctx.windowState.win != win || _gfx_ctx.windowState.dataDragging == false) return false;
	if (x != null) *x = _gfx_ctx.windowState.dropX;
	if (y != null) *y = _gfx_ctx.windowState.dropY;
	return true;
}

// public
fn GfxDataDropNode* rgfw_window_getDataDrop(GfxWindow* win)
{
	if (_gfx_ctx.windowState.win != win || _gfx_ctx.windowState.dataDrop == false) return null;
	return _gfx_ctx.dndRoot;
}

// public
fn void rgfw_setRootWindow(GfxWindow* win) { _gfx_ctx.root = win; }
// public
fn GfxWindow* rgfw_getRootWindow() { return _gfx_ctx.root; }

// public
fn void rgfw_setBit(uint* value, uint mask, bool set)
{
	if (set) { *value |= mask; }
	else { *value &= ~mask; }
}


// public
fn void rgfw_window_center(GfxWindow* win)
{
	GfxMonitor* mon = find_monitor(win);
	if (mon == null) return;
	rgfw_window_move(win, mon.origin.x + ((mon.mode.w - win.width) / 2), mon.origin.y + ((mon.mode.h - win.height) / 2));
}

// public
fn bool rgfw_monitor_scaleToWindow(GfxMonitor* mon, GfxWindow* win)
{
	GfxMonitorMode mode;
	mode.w = win.width;
	mode.h = win.height;
	bool ret = rgfw_monitor_requestMode(mon, &mode, SCALE);
	rgfw_window_move(win, ...mon.origin);
	return ret;
}

// public
fn void rgfw_splitBPP(uint bpp, GfxMonitorMode* mode)
{
	if (bpp == 32) bpp = 24;
	mode.red = mode.green = mode.blue = (char)(bpp / 3);
	uint delta = bpp - (mode.red * 3);
	if (delta >= 1) mode.green = mode.green + 1;
	if (delta == 2) mode.red = mode.red + 1;
}

// public
fn bool rgfw_monitorModeCompare(GfxMonitorMode* mon, GfxMonitorMode* mon2, GfxMonitorModeRequest request)
{
	return ((mon.w == mon2.w && mon.h == mon2.h) || !(request & SCALE)) &&
	((mon.refresh_rate == mon2.refresh_rate) || !(request & REFRESH)) &&
	((mon.red == mon2.red && mon.green == mon2.green && mon.blue == mon2.blue) || !(request & RGB));
}

// public
fn void rgfw_surface_setConvertFunc(GfxSurface* surface, GfxConvertImageDataFn func) { surface.convertFunc = func; }
// public
fn RgfwNativeImage* rgfw_surface_getNativeImage(GfxSurface* surface) => &surface.native;
// public
fn void rgfw_surface_free(GfxSurface* surface) { rgfw_surface_freePtr(surface); rgfw_free_impl(surface); }

// public
fn GfxSurface* rgfw_window_createSurface(GfxWindow* win, char* data, int w, int h, GfxFormat format)
{
	GfxSurface* surface = mem::new(GfxSurface);
	rgfw_window_createSurfacePtr(win, data, w, h, format, surface);
	return surface;
}


// public
fn GfxMonitorNode* rgfw_monitors_add(GfxMonitor* mon)
{
	GfxMonitorNode* node = mem::new(GfxMonitorNode);
	if (!_gfx_ctx.monitors.list.head)
	{
		_gfx_ctx.monitors.list.head = node;
	}
	else
	{
		_gfx_ctx.monitors.list.cur.next = node;
	}
	_gfx_ctx.monitors.list.cur = node;
	if (mon != null) node.mon = *mon;
	node.mon.node = node;
	node.disconnected = false;
	_gfx_ctx.monitors.count += 1;
	return node;
}


// public
fn void rgfw_monitors_refresh()
{
	GfxMonitorNode* prev = _gfx_ctx.monitors.list.head;
	GfxMonitorNode* node = _gfx_ctx.monitors.list.head;
	while (node != null)
	{
		GfxMonitorNode* next = node.next;
		if (node.disconnected != false)
		{
			rgfw_monitorCallback(_gfx_ctx.root, &node.mon, false);
			_remove_monitor(node, prev);
		}
		prev = node;
		node = next;
	}
}

// public
fn void rgfw_freeMonitors()
{
	GfxMonitorNode* node = _gfx_ctx.monitors.list.head;
	while (node != null)
	{
		GfxMonitorNode* next = node.next;
		_remove_monitor(node, node);
		node = next;
	}
}

// public
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

// public
fn bool rgfw_getMonitorsPtr(sz max, GfxMonitor** monitors, sz* len)
{
	if (len != null) *len = _gfx_ctx.monitors.count;
	if (monitors == null || max == 0) return true;
	if (len != null) *len = max;
	sz i = 0;
	GfxMonitorNode* cur_node = _gfx_ctx.monitors.list.head;
	while (cur_node != null && i < max)
	{
		monitors[i] = &cur_node.mon;
		i++;
		cur_node = cur_node.next;
	}
	return true;
}

// public
fn GfxMonitor* rgfw_getPrimaryMonitor()
{
	if (_gfx_ctx.monitors.primary == null) _gfx_ctx.monitors.primary = _gfx_ctx.monitors.list.head;
	if (_gfx_ctx.monitors.primary == null) return null;
	return &_gfx_ctx.monitors.primary.mon;
}

// public
fn GfxMonitorMode[] GfxMonitor.get_modes(&monitor, Allocator allocator)
{
	return alloc::clone_slice(allocator, rgfw_monitor_getModesPtr(monitor));
}

// public
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

// public
fn ZString rgfw_monitor_getName(GfxMonitor* monitor) => (ZString)&monitor.name[0];
// public
fn void rgfw_monitor_setUserPtr(GfxMonitor* monitor, void* userPtr) { monitor.userPtr = userPtr; }
// public
fn void* rgfw_monitor_getUserPtr(GfxMonitor* monitor) => monitor.userPtr;
// public
fn bool rgfw_monitor_getMode(GfxMonitor* monitor, GfxMonitorMode* mode) { if (mode != null) *mode = monitor.mode; return true; }

// public
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

// public
fn void rgfw_freeGammaRamp(RgfwGammaRamp* ramp)
{
	rgfw_free_impl(ramp.red);
	rgfw_free_impl(ramp.green);
	rgfw_free_impl(ramp.blue);
	rgfw_free_impl(ramp);
}

// public
fn bool rgfw_monitor_setGammaPtr(GfxMonitor* monitor, float gamma, ushort* ptr, sz count)
{
	for (sz i = 0; i < count; i++)
	{
		float value = (float)i / (float)(count - 1);
		value = powf(value, 1.0f / gamma) * 65535.0f + 0.5f;
		value = min(value, 65535.0f);
		ptr[i] = (ushort)value;
	}
	RgfwGammaRamp ramp;
	ramp.red = ptr; ramp.green = ptr; ramp.blue = ptr; ramp.count = count;
	return rgfw_monitor_setGammaRamp(monitor, &ramp);
}

// public
fn bool rgfw_monitor_setGamma(GfxMonitor* monitor, float gamma)
{
	sz count = rgfw_monitor_getGammaRampPtr(monitor, null);
	ushort* ptr = (ushort*)rgfw_alloc_impl(count * ushort::size);
	bool ret = rgfw_monitor_setGammaPtr(monitor, gamma, ptr, count);
	rgfw_free_impl(ptr);
	return ret;
}

// public
fn bool GfxWindow.captures_mouse(&self) => self.internal.captureMouse;
// public
fn bool GfxWindow.is_raw_mouse(&self) => self.internal.rawMouse;





// public
fn GfxWindow* rgfw_createWindowPtr(ZString name, int x, int y, int w, int h, GfxWindowFlags flags, GfxWindow* win)
{
	if (win == null || !_gfx_ctx.initialized) return null;
	if (name == null) name = "";
	*win = {};
	_gfx_ctx.windowCount++;
	if (!_gfx_ctx.root) rgfw_setRootWindow(win);
	win.bounds = { x, y, w, h };
	win.internal.mouse = _gfx_ctx.standardMice[GfxMouseIcon.NORMAL.ordinal];
	win.internal.flags = flags;
	win.internal.enabledEvents = RGFW_ALL_EVENT_FLAGS;
	GfxWindow* ret = rgfw_createWindowPlatform(name, flags, win);
	if (!ret)
	{
		log::error("Failed to create a window", category: log::CATEGORY_VIDEO);
		return null;
	}
	_window_set_flags(win, flags, {});
	win.src.gfxType = 0;
	$if $feat(MACOS) || $feat(WIN32) || $feat(WASM):
	if (flags.open_gl)
	{
		rgfw_window_createContext_OpenGL(win, rgfw_getGlobalHints_OpenGL());
	}
	$endif
	$if $feat(MACOS):
	if (flags.egl) rgfw_window_createContext_EGL(win, rgfw_getGlobalHints_OpenGL());
	$endif
	if (!flags.hide_mouse) win.set_mouse_default();
	rgfw_window_setName(win, name);
	switch
	{
		case !flags.hide:
			flags.hide = true;
			show(win);
		case flags.maximize || flags.fullscreen:
			rgfw_window_hide(win);
	}
	return ret;
}





// public
fn GfxFormat native_format()
{
	return $feat(WASM) ? RGBA8 : BGRA8;
}


// public
fn void rgfw_window_captureMouse(GfxWindow* win, bool state) { win.internal.captureMouse = state; rgfw_window_captureMousePlatform(win, state); }
// public
fn void rgfw_window_setRawMouseMode(GfxWindow* win, bool state) { win.internal.rawMouse = state; rgfw_window_setRawMouseModePlatform(win, state); }
// public
fn void rgfw_setRawMouseMode(bool state) { _gfx_ctx.rawMouse = state; if (_gfx_ctx.root != null) rgfw_window_setRawMouseModePlatform(_gfx_ctx.root, state); }
// public
fn void rgfw_window_captureRawMouse(GfxWindow* win, bool state) { rgfw_window_captureMouse(win, state); rgfw_window_setRawMouseMode(win, state); }

// public
fn void rgfw_keyUpdateKeyMod(GfxWindow* win, RgfwKeymod mod, bool value) { if (value) { win.internal.mod |= mod; } else { win.internal.mod &= ~mod; } }
// public
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

// public
fn void rgfw_keyUpdateKeyMods(GfxWindow* win, bool capital, bool numlock, bool scroll)
{
	rgfw_keyUpdateKeyModsEx(win, capital, numlock,
		is_key_down(RGFW_KEY_CONTROL_L) || is_key_down(RGFW_KEY_CONTROL_R),
		is_key_down(RGFW_KEY_ALT_L) || is_key_down(RGFW_KEY_ALT_R),
		is_key_down(RGFW_KEY_SHIFT_L) || is_key_down(RGFW_KEY_SHIFT_R),
		is_key_down(RGFW_KEY_SUPER_L) || is_key_down(RGFW_KEY_SUPER_R),
		scroll);
}

// public
fn bool GfxWindow.is_mouse_hidden(&win) => win.internal.flags.hide_mouse;
// public
fn bool GfxWindow.is_borderless(&win) => win.internal.flags.no_border;
// public
fn bool GfxWindow.is_fullscreen(&win) => win.internal.flags.fullscreen;
// public
fn bool GfxWindow.allows_dnd(&win) => win.internal.flags.allow_dnd;


// public
fn void rgfw_window_showMouseFlags(GfxWindow* win, bool show)
{
	if (show && win.internal.flags.hide_mouse)
	{
		win.internal.flags.hide_mouse = false;
	}
	else if (!show && !win.internal.flags.hide_mouse)
	{
		win.internal.flags.hide_mouse = true;
	}
}

// public
fn bool GfxWindow.set_mouse_default(&self) => self.set_mouse_standard(NORMAL);
// public
fn bool GfxWindow.set_mouse_standard(&self, GfxMouseIcon icon) => self.set_mouse(_gfx_ctx.standardMice[(sz)icon]);
// public
fn bool GfxWindow.set_mouse(&self, RgfwMouse* mouse)
{
	if (mouse != _gfx_ctx.hiddenMouse) self.internal.mouse = mouse;
	return rgfw_window_setMousePlatform(self, mouse);
}

// public
fn bool rgfw_isLatin(ZString string, sz length)
{
	for (sz i = 0; i < length; i++) if ((char)string[i] >= 0x80) return true;
	return false;
}

// public
fn uint rgfw_decodeUTF8(ZString string, sz* starting_index)
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


// public
fn void show(GfxWindow* win)
{
	if (win.internal.flags.focus_on_show) rgfw_window_focus(win);
	win.internal.flags.hide = false;
	rgfw_window_showPlatform(win);
	if (win.internal.flags.maximize) rgfw_window_maximize(win);
}

// public
fn void restore_window(GfxWindow* win)
{
	rgfw_window_restorePlatform(win);
	win.internal.flags.maximize = false;
	show(win);
	rgfw_window_move(win, ...win.internal.old_bounds.origin);
	rgfw_window_resize(win, ...win.internal.old_bounds.size);
}
// public
fn void rgfw_window_minimize(GfxWindow* win)
{
	rgfw_window_minimizePlatform(win);
	win.internal.flags.maximize = false;
}
// public
fn void rgfw_window_maximize(GfxWindow* win)
{
	win.internal.old_bounds = win.bounds;
	win.internal.flags.maximize = true;
	rgfw_window_fetchSize(win);
	rgfw_window_maximizePlatform(win);
}
// public
fn void rgfw_window_move(GfxWindow* win, int x, int y)
{
	win.origin = { x, y };
	rgfw_window_movePlatform(win, x, y);
	win.internal.flags.maximize = false;
}
// public
fn void rgfw_window_resize(GfxWindow* win, int w, int h)
{
	win.size = { w, h };
	rgfw_window_resizePlatform(win, w, h);
	win.internal.flags.maximize = false;
}


// public
fn bool rgfw_extensionSupportedStr(ZString extensions, ZString ext, sz len) @feat(!WASM)
{
	if (extensions == null || ext == null) return false;
	while (ext[len - 1] == 0 && len > 3) len--;
	ZString start = extensions;
	ZString where = (ZString)strstr(extensions, ext);
	while (where != null)
	{
		ZString terminator = where + len;
		if (((char*)where == (char*)start || *((char*)where - 1) == ' ') && (terminator[0] == ' ' || terminator[0] == 0)) return true;
		where = (ZString)strstr(terminator, ext);
	}
	return false;
}

// public
fn void rgfw_window_setDND(GfxWindow* win, bool allow)
{
	win.internal.flags.allow_dnd = allow;
	$if $feat(WIN32):
		dragAcceptFiles(win.src.window, (int)allow);
	$endif
}

// public
fn void rgfw_window_showMouse(GfxWindow* win, bool show)
{
	rgfw_window_showMouseFlags(win, show);
	$if $feat(WASM):
	if (show)
	{
		win.set_mouse_default();
	}
	else
	{
		rgfwJsHideCursor();
	}
	$else
	if (show)
	{
		win.set_mouse(win.internal.mouse);
	}
	else
	{
		win.set_mouse(_gfx_ctx.hiddenMouse);
	}
	$endif


}



module std::gfx @private;

// private
fn void _remove_monitor(GfxMonitorNode* node, GfxMonitorNode* prev)
{
	_gfx_ctx.monitors.count -= 1;
	rgfw_monitorNode_free(node);
	if (prev != node)
	{
		prev.next = node.next;
	}
	else
	{
		_gfx_ctx.monitors.list.head = node.next;
	}
	node.next = null;
	free(node);
}

// public
fn void _mouse_notify_callback(GfxWindow* win, int x, int y, bool status)
{
	win.internal.mouseInside = status;
	_gfx_ctx.windowState.win = win;
	win.internal.last_mouse = { x, y };
	GfxEvent event;
	event.common.win = win;
	event.mouse.x = x; event.mouse.y = y; event.mouse.inWindow = win.internal.mouseInside;
	if (status)
	{
		if (!(win.internal.enabledEvents & RGFW_MOUSE_ENTER_FLAG)) return;
		_gfx_ctx.windowState.mouseEnter = true;
		event.type = MOUSE_ENTER;
	}
	else
	{
		if (!(win.internal.enabledEvents & RGFW_MOUSE_LEAVE_FLAG)) return;
		_gfx_ctx.windowState.winLeave = win;
		_gfx_ctx.windowState.mouseLeave = true;
		event.type = MOUSE_LEAVE;
	}
	_event_queue_call_push(&event);
}

// public
fn void _mouse_motion_callback(GfxWindow* win, int x, int y)
{
	win.internal.last_mouse = { x, y };
	if (!(win.internal.enabledEvents & RGFW_MOUSE_MOTION_FLAG)) return;
	GfxEvent event;
	event.type = MOUSE_MOTION;
	event.mouse.x = x; event.mouse.y = y; event.mouse.inWindow = win.internal.mouseInside;
	event.common.win = win;
	_event_queue_call_push(&event);
}
fn void _event_queue_call_push(GfxEvent* event)
{
	if (_gfx_ctx.callbacks[event.type]) _gfx_ctx.callbacks[event.type](event);
	event_queue_push(event);
}

RgfwInfo _gfx_ctx;
