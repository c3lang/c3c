module std::gfx @feat(MACOS, X11, X11_WAYLAND, WAYLAND, WASM, WIN32);

import std::math, std::os::macos, libc;

alias GfxPoint = int[<2>];
alias GfxSize = int[<2>];

excuse
{
	GFX_CONTEXT_NOT_INITIALIZED,
	SURFACE_CREATION_FAILED,
	MISSING_VULKAN_LIB,
	MISSING_OPENGL_LIB,
	MISSING_EGL_LIB,
	MISSING_VK_GET_INSTANCE_PROC_ADDR,
	MISSING_GL_GET_PROC_ADDR,
	MISSING_EGL_GET_PROC_ADDR,
	EGL_BINDING_FAILED,
	X11_WINDOW_CREATION_FAILED,
	VULKAN_NOT_LOADED,
}

const MAX_EVENTS = 32;
const PREALLOCATED_MONITORS = 0;

struct GfxSurface
{
	void* data;
	GfxSize size;
	GfxFormat format;
	GfxConvertImageDataFn convert_fn;
	GfxNativeImage native;
}

struct GfxMonitorMode
{
	GfxSize size;
	float refresh_rate;
	char[<3>] rgb;
	void* src;
}

struct GfxMonitor
{
	GfxPoint origin;
	char[128] name;
	Vec2 scale;
	float pixelRatio;
	Vec2 phys_size;
	GfxMonitorMode mode;
	void* user_ptr;
	GfxMonitorNode* node;
}

struct GfxKeyState
{
	bool current;
	bool prev;
}

struct GfxWindow
{
	inline IntRect bounds;
	GfxWindowSrc src;
	GfxWindowInternal internal;
	void* user_ptr;
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
	GfxKey exit_key;
	GfxPoint last_mouse;
	bool shouldClose;
	bool raw_mouse;
	bool capture_mouse;
	bool inFocus;
	bool mouse_inside;
	GfxKeyModifier mod;
	TodoRenameEventFlag enabled_events;
	GfxWindowFlags flags;
	IntRect old_bounds;
	GfxMonitorMode oldMode;
	TodoRenameMouse* mouse;
}


struct GfxWindowState
{
	GfxPoint drop;
	bool mouse_enter;
	bool dataDragging;
	bool dataDrop;
	bool mouse_leave;
	GfxWindow* win;
	GfxWindow* win_leave;
	sz dataLength;
}

union GfxBackingContext
{
	GfxGlContext* native;
	GfxEglContext* egl;
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
	GfxDataTransferType type;
}

struct GfxContext
{
	bool initialized;
	GfxWindow* root;
	int windowCount;
	TodoRenameMouse* hiddenMouse;
	TodoRenameMouse*[21] standardMice;
	TodoRenameDebugFunc debugCallbackSrc;
	GfxGenericFunc[25] callbacks;
	GfxEvent[MAX_EVENTS] events;
	int eventBottom;
	int eventLen;
	bool queueEvents;
	bool polledEvents;
	uint[GFX_KEYS] apiKeycodes;
	GfxKey[$feat(WIN32) ? 512 : 256] keycodes;
	bool stopCheckEvents_bool;
	ulong timerOffset;
	int[3] eventWait_forceStop @feat(X11_WAYLAND);
	GfxDataTransfer* clipboard;
	bool dnd_build;
	GfxDataDropNode* dndRoot;
	GfxDataDropNode* dndCur;

	GfxMonitors monitors;
	GfxWindow* current;
	void* nativeGL_handle;
	void* glGetProcAddress;
	void* vulkan_handle;
	GfxVkGetInstanceProcAddressProc vk_get_instance_proc_address;
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
		WlProxy* wp_pointer_warp;
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
		XDisplay* display;
		int context;
		XWindow helperWindow;
		String class_name;
		XlibIM im;
		int xrandrEventBase;
		XWindow x11Source;
		int x11Version;
		int x11Format;
		GfxDataTransferType x11TransferType;
		XErrorEvent* x11Error;
		Atom netCurrentDesktop, net_wm_state_maximized_vert, net_wm_state_maximized_horz, netWmBypassCompositor;
		Atom netWmState, motifWmHints, wmDeleteWindow, netWmSyncRequestCounter, netWmSyncRequest;
		Atom atomPair, multiple, targets, saveTargets, utf8String;
		Atom xdndTypeList, xdndSelection, xdndAware, xdndEnter, xdndPosition, xdndStatus, xdndLeave, xdndDrop, xdndFinished, xdndActionCopy;
		Atom wmProtocols, wmState, netWmStateFullscreen, netWmStateAbove, netWmWindowOpacity;
		Atom netWmName, netWmIcon, xselData, clipboardAtom, clipboardManager;
		Atom xtextUriList, xtextPlain, netWorkarea;
	}
	bool useWaylandBool @feat(X11_WAYLAND);
	bool raw_mouse;
	GfxWindowState window_state;
	GfxKeyState[8] mouseButtons;
	GfxKeyState[GFX_KEYS] keyboard;
	Vec2 scroll;
	Vec2 vector;
}


struct GfxDataDropNode
{
	char* data;
	sz length;
	GfxDataTransferType type;
	GfxDataDropNode* next;
}

struct GfxCommonEvent
{
	GfxEventType type;
	GfxWindow* win;
}
struct GfxWindowFocusEvent
{
	inline GfxCommonEvent common;
	bool state;
}
struct GfxMouseButtonEvent
{
	inline GfxCommonEvent common;
	GfxMouseButton value;
	bool state;
}
struct GfxMouseDeltaEvent
{
	inline GfxCommonEvent common;
	Vec2 vector;
}
struct GfxMouseMotionEvent
{
	inline GfxCommonEvent common;
	GfxPoint pos;
	bool in_window;
}
struct GfxKeyEvent
{
	inline GfxCommonEvent common;
	GfxKey value;
	bool repeat;
	GfxKeyModifier mod;
	bool state;
}
struct GfxKeyCharEvent
{
	inline GfxCommonEvent common;
	uint value;
}
struct GfxDataDropEvent
{
	inline GfxCommonEvent common;
	GfxDataDropNode* value;
}
struct GfxDataDragEvent
{
	inline GfxCommonEvent common;
	GfxPoint pos;
	GfxDndActionType action;
	GfxDataTransferType dataType;
}
struct GfxScaleUpdatedEvent
{
	inline GfxCommonEvent common;
	Vec2 scale;
}
struct GfxMonitorEvent
{
	inline GfxCommonEvent common;
	GfxMonitor* monitor;
	bool state;
}
struct GfxWindowUpdateEvent
{
	inline GfxCommonEvent common;
	IntRect bounds;
}

union GfxEvent
{
	GfxEventType type;
	GfxCommonEvent common;
	GfxWindowFocusEvent focus;
	GfxWindowUpdateEvent update;
	GfxMouseButtonEvent button;
	GfxMouseDeltaEvent delta;
	GfxMouseMotionEvent mouse;
	GfxKeyEvent key;
	GfxKeyCharEvent keyChar;
	GfxDataDropEvent drop;
	GfxDataDragEvent drag;
	GfxScaleUpdatedEvent scale;
	GfxMonitorEvent monitor;
}

struct TodoRenameCallbacks
{
	GfxGenericFunc[25] arr;
}

struct TodoRenameDebugInfo
{
	TodoRenameDebugType type;
	TodoRenameErrorCode code;
	ZString msg;
}

constset GfxWindowIconType
{
	TASKBAR  = 1,
	WINDOW   = 2,
	BOTH     = 3,
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

enum GfxFormat : char (int channels, GfxColorLayout layout)
{
	RGB8  { 3, { 0, 1, 2, 3, 3 } },
	BGR8  { 3, { 2, 1, 0, 3, 3 } },
	RGBA8 { 4, { 0, 1, 2, 3, 4 } },
	ARGB8 { 4, { 1, 2, 3, 0, 4 } },
	BGRA8 { 4, { 2, 1, 0, 3, 4 } },
	ABGR8 { 4, { 3, 2, 1, 0, 4 } },
}

constset GfxKey : inline char
{
	NULL            = 0,
	ESCAPE          = 27,
	BACKTICK        = '`',
	NUM0            = '0',
	NUM1            = '1',
	NUM2            = '2',
	NUM3            = '3',
	NUM4            = '4',
	NUM5            = '5',
	NUM6            = '6',
	NUM7            = '7',
	NUM8            = '8',
	NUM9            = '9',
	MINUS           = '-',
	EQUAL           = '=',
	EQUALS          = '=',
	BACK_SPACE      = 8,
	TAB             = 9,
	SPACE           = ' ',
	A               = 'a',
	B               = 'b',
	C               = 'c',
	D               = 'd',
	E               = 'e',
	F               = 'f',
	G               = 'g',
	H               = 'h',
	I               = 'i',
	J               = 'j',
	K               = 'k',
	L               = 'l',
	M               = 'm',
	N               = 'n',
	O               = 'o',
	P               = 'p',
	Q               = 'q',
	R               = 'r',
	S               = 's',
	T               = 't',
	U               = 'u',
	V               = 'v',
	W               = 'w',
	X               = 'x',
	Y               = 'y',
	Z               = 'z',
	PERIOD          = '.',
	COMMA           = ',',
	SLASH           = '/',
	BRACKET         = '[',
	CLOSE_BRACKET   = ']',
	SEMICOLON       = ';',
	APOSTROPHE      = '\'',
	BACK_SLASH      = '\\',
	RETURN          = 10,
	ENTER           = 10,
	DELETE          = 127,
	F1              = 128,
	F2              = 129,
	F3              = 130,
	F4              = 131,
	F5              = 132,
	F6              = 133,
	F7              = 134,
	F8              = 135,
	F9              = 136,
	F10             = 137,
	F11             = 138,
	F12             = 139,
	F13             = 140,
	F14             = 141,
	F15             = 142,
	F16             = 143,
	F17             = 144,
	F18             = 145,
	F19             = 146,
	F20             = 147,
	F21             = 148,
	F22             = 149,
	F23             = 150,
	F24             = 151,
	F25             = 152,
	CAPS_LOCK       = 153,
	SHIFT_L         = 154,
	CONTROL_L       = 155,
	ALT_L           = 156,
	SUPER_L         = 157,
	SHIFT_R         = 158,
	CONTROL_R       = 159,
	ALT_R           = 160,
	SUPER_R         = 161,
	UP              = 162,
	DOWN            = 163,
	LEFT            = 164,
	RIGHT           = 165,
	INSERT          = 166,
	MENU            = 167,
	END             = 168,
	HOME            = 169,
	PAGE_UP         = 170,
	PAGE_DOWN       = 171,
	NUM_LOCK        = 172,
	PAD_SLASH       = 173,
	PAD_MULTIPLY    = 174,
	PAD_PLUS        = 175,
	PAD_MINUS       = 176,
	PAD_EQUAL       = 177,
	PAD_EQUALS      = 177,
	PAD1            = 178,
	PAD2            = 179,
	PAD3            = 180,
	PAD4            = 181,
	PAD5            = 182,
	PAD6            = 183,
	PAD7            = 184,
	PAD8            = 185,
	PAD9            = 186,
	PAD0            = 187,
	PAD_PERIOD      = 188,
	PAD_RETURN      = 189,
	SCROLL_LOCK     = 190,
	PRINT_SCREEN    = 191,
	PAUSE           = 192,
	WORLD1          = 193,
	WORLD2          = 194,

}

const GFX_KEYS = 256;

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

bitstruct GfxKeyModifier : char
{
	bool caps_lock;
	bool num_lock;
	bool control;
	bool alt;
	bool shift;
	bool super;
	bool scroll_lock;
}

const GfxKeyModifier KEYMOD_CAPS_LOCK    = { .caps_lock   };
const GfxKeyModifier KEYMOD_NUM_LOCK     = { .num_lock    };
const GfxKeyModifier KEYMOD_CONTROL      = { .control     };
const GfxKeyModifier KEYMOD_ALT          = { .alt         };
const GfxKeyModifier KEYMOD_SHIFT        = { .shift       };
const GfxKeyModifier KEYMOD_SUPER        = { .super       };
const GfxKeyModifier KEYMOD_SCROLL_LOCK  = { .scroll_lock };

enum GfxDndActionType : char
{
	NONE,
	ENTER,
	MOVE,
	EXIT
}

enum GfxDataTransferType
{
	NONE,
	TEXT,
	FILE,
	URL,
	IMAGE,
	UNKNOWN
}

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

const TodoRenameEventFlag TODO_RENAME_KEY_PRESSED_FLAG = 2;
const TodoRenameEventFlag TODO_RENAME_KEY_RELEASED_FLAG = 4;
const TodoRenameEventFlag TODO_RENAME_KEY_CHAR_FLAG = 8;
const TodoRenameEventFlag TODO_RENAME_MOUSE_SCROLL_FLAG = 64;
const TodoRenameEventFlag TODO_RENAME_MOUSE_BUTTON_PRESSED_FLAG = 16;
const TodoRenameEventFlag TODO_RENAME_MOUSE_BUTTON_RELEASED_FLAG = 32;
const TodoRenameEventFlag TODO_RENAME_MOUSE_MOTION_FLAG = 128;
const TodoRenameEventFlag TODO_RENAME_MOUSE_RAW_MOTION_FLAG = 256;
const TodoRenameEventFlag TODO_RENAME_MOUSE_ENTER_FLAG = 512;
const TodoRenameEventFlag TODO_RENAME_MOUSE_LEAVE_FLAG = 1024;
const TodoRenameEventFlag TODO_RENAME_WINDOW_MOVED_FLAG = 2048;
const TodoRenameEventFlag TODO_RENAME_WINDOW_RESIZED_FLAG = 4096;
const TodoRenameEventFlag TODO_RENAME_WINDOW_FOCUS_IN_FLAG = 8192;
const TodoRenameEventFlag TODO_RENAME_WINDOW_FOCUS_OUT_FLAG = 16384;
const TodoRenameEventFlag TODO_RENAME_WINDOW_REFRESH_FLAG = 32768;
const TodoRenameEventFlag TODO_RENAME_WINDOW_MAXIMIZED_FLAG = 131072;
const TodoRenameEventFlag TODO_RENAME_WINDOW_MINIMIZED_FLAG = 262144;
const TodoRenameEventFlag TODO_RENAME_WINDOW_RESTORED_FLAG = 524288;
const TodoRenameEventFlag TODO_RENAME_SCALE_UPDATED_FLAG = 4194304;
const TodoRenameEventFlag TODO_RENAME_WINDOW_CLOSE_FLAG = 65536;
const TodoRenameEventFlag TODO_RENAME_DATA_DROP_FLAG = 1048576;
const TodoRenameEventFlag TODO_RENAME_DATA_DRAG_FLAG = 2097152;
const TodoRenameEventFlag TODO_RENAME_MONITOR_CONNECTED_FLAG = 8388608;
const TodoRenameEventFlag TODO_RENAME_MONITOR_DISCONNECTED_FLAG = 16777216;
const TodoRenameEventFlag TODO_RENAME_MOUSE_POS_CHANGED_FLAG = 128;
const TodoRenameEventFlag TODO_RENAME_KEY_EVENTS_FLAG = 14;
const TodoRenameEventFlag TODO_RENAME_MOUSE_EVENTS_FLAG = 2032;
const TodoRenameEventFlag TODO_RENAME_WINDOW_EVENTS_FLAG = 5150720;
const TodoRenameEventFlag TODO_RENAME_WINDOW_FOCUS_EVENTS_FLAG = 24576;
const TodoRenameEventFlag TODO_RENAME_DATA_DRAG_DROP_EVENTS_FLAG = 3145728;
const TodoRenameEventFlag TODO_RENAME_MONITOR_EVENTS_FLAG = 25165824;
const TodoRenameEventFlag TODO_RENAME_ALL_EVENT_FLAGS = 33554430;


const TodoRenameEventWait TODO_RENAME_EVENT_NO_WAIT = 0;
const TodoRenameEventWait TODO_RENAME_EVENT_WAIT_NEXT = -1;

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


const TodoRenameDebugType TODO_RENAME_TYPE_ERROR = 0;
const TodoRenameDebugType TODO_RENAME_TYPE_WARNING = 1;
const TodoRenameDebugType TODO_RENAME_TYPE_INFO = 2;

const TodoRenameErrorCode TODO_RENAME_NO_ERROR = 0;
const TodoRenameErrorCode TODO_RENAME_ERR_OUT_OF_MEMORY = 1;
const TodoRenameErrorCode TODO_RENAME_ERR_OPEN_GL_CONTEXT = 2;
const TodoRenameErrorCode TODO_RENAME_ERR_EGL_CONTEXT = 3;
const TodoRenameErrorCode TODO_RENAME_ERR_WAYLAND = 4;
const TodoRenameErrorCode TODO_RENAME_ERR_X11 = 5;
const TodoRenameErrorCode TODO_RENAME_ERR_DIRECT_X_CONTEXT = 6;
const TodoRenameErrorCode TODO_RENAME_ERR_IO_KIT = 7;
const TodoRenameErrorCode TODO_RENAME_ERR_CLIPBOARD = 8;
const TodoRenameErrorCode TODO_RENAME_ERR_FAILED_FUNC_LOAD = 9;
const TodoRenameErrorCode TODO_RENAME_ERR_BUFFER = 10;
const TodoRenameErrorCode TODO_RENAME_ERR_METAL = 11;
const TodoRenameErrorCode TODO_RENAME_ERR_PLATFORM = 12;
const TodoRenameErrorCode TODO_RENAME_ERR_EVENT_QUEUE = 13;
const TodoRenameErrorCode TODO_RENAME_ERR_NO_INIT = 14;
const TodoRenameErrorCode TODO_RENAME_INFO_WINDOW = 15;
const TodoRenameErrorCode TODO_RENAME_INFO_BUFFER = 16;
const TodoRenameErrorCode TODO_RENAME_INFO_GLOBAL = 17;
const TodoRenameErrorCode TODO_RENAME_INFO_OPEN_GL = 18;
const TodoRenameErrorCode TODO_RENAME_WARNING_WAYLAND = 19;
const TodoRenameErrorCode TODO_RENAME_WARNING_OPEN_GL = 20;


// public
fn void convert_image_data_64(char* dest_data, char* src_data, GfxColorLayout* srcLayout, GfxColorLayout* destLayout, sz count, bool is64bit)
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
	GfxColorLayout* srcLayout = &&src_format.layout;
	GfxColorLayout* destLayout = &&dest_format.layout;
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
	if (options & INIT_OPEN_GL && @catch(load_opengl()))
	{
		log::error("Failed to load the OpenGL library", category: log::CATEGORY_VIDEO);
		return -1;
	}
	if (options & INIT_EGL && @catch(_load_egl_native()))
	{
		log::error("Failed to load the EGL library", category: log::CATEGORY_VIDEO);
		return -1;
	}
	if (options & INIT_VULKAN && @catch(_load_vulkan_native()))
	{
		log::error("Failed to load the Vulkan library", category: log::CATEGORY_VIDEO);
		return -1;
	}
	todo_fix_initKeycodes();
	int out = todo_fix_initPlatform(class_name, options);
	if (out != 0)
	{
		log::error("Failed to initialize global context", category: log::CATEGORY_VIDEO);
		todo_fix_deinitPlatform();
		_gfx_ctx = {};
		return out;
	}
	foreach (icon : GfxMouseIcon::values)
	{
		_gfx_ctx.standardMice[icon.ordinal] = todo_fix_createMouseStandard(icon);
	}
	todo_fix_pollMonitors();
	log::info("Global context initialized", category: log::CATEGORY_VIDEO);
	return out;
}

// public
fn void finalize()
{
	if (!_gfx_ctx.initialized) return;
	foreach (icon : GfxMouseIcon::values)
	{
		if (_gfx_ctx.standardMice[icon.ordinal]) todo_fix_freeMouse(_gfx_ctx.standardMice[icon.ordinal]);
	}
	free_monitors();
	log::info("Global context destroyed", category: log::CATEGORY_VIDEO);
	todo_fix_deinitPlatform();
	unload_egl();
	unload_opengl();
	_gfx_ctx.root = null;
	_gfx_ctx.windowCount = 0;
	_gfx_ctx = {};
}

// public
fn GfxSurface*? create_surface(char* data, int w, int h, GfxFormat format, GfxWindow* window = null)
{
	GfxSurface* s = mem::new(GfxSurface);
	defer catch free(s);
	s.init(data, w, h, format, window: window)!;
	return s;
}

// public
fn GfxWindow*? create_window(String name, int x, int y, int w, int h, GfxWindowFlags flags)
{
	GfxWindow* win = mem::new(GfxWindow);
	defer catch free(win);
	win.init(name, x, y, w, h, flags)!;
	return win;
}

fn bool is_mouse_pressed(GfxMouseButton button) => _gfx_ctx.mouseButtons[(sz)button].current && !_gfx_ctx.mouseButtons[(sz)button].prev;


fn void GfxWindow.flash(&self, GfxFlashRequest request) => _window_flash_native(self, request);

<*
 Get the (last) mouse position inside of a window.
*>
fn GfxPoint GfxWindow.get_mouse(&win) => win.internal.last_mouse;

<*
 Check if a window should be closed.

 @param [&in] self
 @return "true is it is tagged as should close or its exit key is held down"
*>
fn bool GfxWindow.should_close(&self)
{
	return self.internal.shouldClose || (self.internal.exit_key && is_key_down(self.internal.exit_key));
}

fn void GfxWindow.focus(&self)
{
	_window_focus_native(self);
}

fn void GfxWindow.set_should_close(&win, bool should_close)
{
	if (!should_close)
	{
		win.internal.shouldClose = false;
		return;
	}
	_window_close_callback(win);
}

fn GfxWindowSrc* GfxWindow.get_src(&self)
{
	return &self.src;
}

fn GfxPoint GfxWindow.get_origin(&win)
{
	return win.origin;
}

fn GfxPoint GfxWindow.get_size(&self)
{
	return self.size;
}

fn GfxWindowFlags GfxWindow.get_flags(&self)
{
	return self.internal.flags;
}

fn void GfxWindow.set_fullscreen(&self, bool fullscreen)
{
	_window_set_fullscreen_native(self, fullscreen);
}
fn GfxKey GfxWindow.get_exit_key(&self)
{
	return self.internal.exit_key;
}

fn void GfxWindow.set_exit_key(&self, GfxKey key)
{
	self.internal.exit_key = key;
}

fn void GfxWindow.set_enabled_events(&self, TodoRenameEventFlag events)
{
	self.internal.enabled_events = events;
}

fn TodoRenameEventFlag GfxWindow.get_enabled_events(&self)
{
	return self.internal.enabled_events;
}

fn void GfxWindow.set_disabled_events(&self, TodoRenameEventFlag events)
{
	self.internal.enabled_events = TODO_RENAME_ALL_EVENT_FLAGS & ~events;
}


fn void GfxWindow.scale_to_monitor(&self)
{
	if (GfxMonitor* monitor = find_monitor(self)) self.resize((GfxSize)(monitor.scale * (Vec2)self.size));
}

fn void GfxWindow.move_to_monitor(&self, GfxMonitor* m)
{
	self.move(m.origin + self.origin);
}

fn void GfxWindow.set_min_size(&self, GfxSize size)
{
	_window_set_min_size_native(self, size) @inline;
}

fn void GfxWindow.set_max_size(&self, GfxSize size)
{
	_window_set_max_size_native(self, size) @inline;
}

fn void GfxWindow.set_border(&self, bool border)
{
	_window_set_border_native(self, border);
}


fn void GfxWindow.set_flags(&win, GfxWindowFlags flags)
{
	_window_set_flags(win, flags, win.internal.flags);
}

fn bool GfxWindow.has_focus(&win) => win.internal.inFocus;


<*
 Completely release a window and free its pointer.
*>
fn void GfxWindow.destroy(&self)
{
	self.release();
	free(self);
}
<*
 Toggles building the drag-and-drop (DND) linked list
 @param allow : "set to true to allow DND building"
*>
fn void set_build_dnd(bool allow)
{
	_gfx_ctx.dnd_build = allow;
}

// public
fn bool is_key_pressed(GfxKey key, GfxWindow* window = null)
{
	return _gfx_ctx.keyboard[key].current && !_gfx_ctx.keyboard[key].prev && (!window || window.has_focus());
}
// public
fn bool is_key_down(GfxKey key, GfxWindow* window = null)
{
	return _gfx_ctx.keyboard[key].current && (!window || window.has_focus());
}
// public
fn bool is_key_released(GfxKey key, GfxWindow* window = null)
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

fn Vec2 get_mouse_scroll() => _gfx_ctx.scroll;
fn Vec2 get_mouse_delta() => _gfx_ctx.vector;




// public
fn void GfxWindow.release(&self)
{
	if (self.internal.capture_mouse) self.set_capture_mouse(false);
	$if $feat(MACOS):
	if ((self.src.gfxType & TODO_RENAME_GFX_EGL) && self.src.ctx.egl != null)
	{
		todo_fix_window_deleteContext_EGL(self, self.src.ctx.egl);
		self.src.ctx.egl = null;
	}
	if ((self.src.gfxType & TODO_RENAME_GFX_NATIVE_OPEN_GL) && self.src.ctx.native != null)
	{
		todo_fix_window_deleteContext_OpenGL(self, self.src.ctx.native);
		self.src.ctx.native = null;
	}
	$endif
	$if $feat(WASM) || $feat(WIN32):
	if ((self.src.gfxType & TODO_RENAME_GFX_NATIVE_OPEN_GL) && self.src.ctx.native != null)
	{
		todo_fix_window_deleteContext_OpenGL(self, self.src.ctx.native);
		self.src.ctx.native = null;
	}
	$endif
	todo_fix_window_closePlatform(self);
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
fn void set_all_event_callback(GfxGenericFunc func, TodoRenameCallbacks* callbacks)
{
	foreach (i : GfxEventType::values[1..])
	{
		if (callbacks != null) callbacks.arr[i.ordinal] = _gfx_ctx.callbacks[i.ordinal];
		set_event_callback(i, func);
	}
}


// public
fn void todo_fix_eventQueueFlush() { _gfx_ctx.eventLen = 0; }

// public
fn void event_queue_push(GfxEvent* event)
{
	if (!_gfx_ctx.queueEvents) return;
	if (_gfx_ctx.eventLen >= MAX_EVENTS)
	{
		log::error("Event queue limit 'MAX_EVENTS' has been reached automatically flushing queue.", category: log::CATEGORY_VIDEO);
		todo_fix_eventQueueFlush();
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
		todo_fix_pollEvents();
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
		todo_fix_pollEvents();
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
		case flags.no_border: win.set_border(false);
		case cmpFlags.no_border: win.set_border(true);
	}
	switch
	{
		case flags.maximize: win.maximize();
		case cmpFlags.maximize: win.restore();
	}
	switch
	{
		case flags.minimize: win.minimize();
		case cmpFlags.minimize: win.restore();
	}
	if (flags.center) todo_fix_window_center(win);
	if (flags.center_cursor) _window_move_mouse(win, win.origin + win.size / 2);
	switch
	{
		case flags.fullscreen: _window_set_fullscreen_native(win, true);
		case cmpFlags.fullscreen: _window_set_fullscreen_native(win, false);
	}
	if (flags.hide_mouse)
	{
		win.show_mouse(false);
	}
	else if (cmpFlags.hide_mouse)
	{
		win.show_mouse(true);
	}
	if (flags.hide)
	{
		win.hide();
	}
	else if (cmpFlags.hide)
	{
		win.show();
	}
	if (flags.floating)
	{
		todo_fix_window_setFloating(win, true);
	}
	else if (cmpFlags.floating)
	{
		todo_fix_window_setFloating(win, false);
	}
	if (flags.raw_mouse)
	{
		win.set_raw_mouse_mode(true);
	}
	else if (cmpFlags.raw_mouse)
	{
		win.set_raw_mouse_mode(false);
	}
	if (flags.capture_mouse)
	{
		win.set_capture_raw_mouse(true);
	}
	else if (cmpFlags.capture_mouse)
	{
		win.set_capture_mouse(false);
	}
	if (flags.focus) _window_focus_native(win);
	if (flags.scale_to_monitor) win.scale_to_monitor();
	switch (true)
	{
		case flags.no_resize:
			_window_set_max_size_native(win, win.size);
			_window_set_min_size_native(win, win.size);
		case cmpFlags.no_resize:
			_window_set_max_size_native(win, {});
			_window_set_min_size_native(win, {});
	}
	win.internal.flags = flags;
}

// public
fn void _window_restored_callback(GfxWindow* win, IntRect bounds) @private
{
	win.internal.flags.minimize = false;
	win.bounds = bounds;
	if (!_window_is_maximized_native(win)) win.internal.flags.maximize = false;
	if (win.internal.enabled_events & TODO_RENAME_WINDOW_RESTORED_FLAG == 0) return;
	GfxEvent event;
	event.type = WINDOW_RESTORED;
	event.update.bounds = bounds;
	event.common.win = win;
	_event_queue_call_push(&event);
}

// private
fn void todo_fix_window_checkMode(GfxWindow* win)
{
	switch
	{
		case _window_is_minimized_native(win) && (win.internal.enabled_events & TODO_RENAME_WINDOW_MINIMIZED_FLAG):
			todo_fix_windowMinimizedCallback(win);
		case _window_is_maximized_native(win)&& (win.internal.enabled_events & TODO_RENAME_WINDOW_MAXIMIZED_FLAG):
			todo_fix_windowMaximizedCallback(win, win.bounds);
		case (win.internal.flags.minimize && !_window_is_maximized_native(win)) ||
			 (win.internal.flags.maximize && !_window_is_maximized_native(win)):
			if (win.internal.enabled_events & TODO_RENAME_WINDOW_RESTORED_FLAG) _window_restored_callback(win, win.bounds);
	}
}

// public
fn void _window_moved_callback(GfxWindow* win, GfxPoint where) @private
{
	win.origin = where;
	if (!(win.internal.enabled_events & TODO_RENAME_WINDOW_MOVED_FLAG)) return;
	GfxEvent event;
	event.type = WINDOW_MOVED;
	event.update.bounds = win.bounds;
	event.common.win = win;
	_event_queue_call_push(&event);
}

// public
fn void _window_resized_callback(GfxWindow* win, GfxSize size) @private
{
	win.size = size;
	if (!(win.internal.enabled_events & TODO_RENAME_WINDOW_RESIZED_FLAG)) return;
	GfxEvent event;
	event.type = WINDOW_RESIZED;
	event.update.bounds = win.bounds;
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
fn void _raw_motion_callback(GfxWindow* win, Vec2 delta)
{
	_gfx_ctx.vector = delta;
	if (!(win.internal.enabled_events & TODO_RENAME_MOUSE_RAW_MOTION_FLAG)) return;
	GfxEvent event;
	event.type = MOUSE_RAW_MOTION;
	event.delta.vector = delta;
	event.common.win = win;
	_event_queue_call_push(&event);
}

// public
fn void todo_fix_windowRefreshCallback(GfxWindow* win, IntRect bounds)
{
	if (!(win.internal.enabled_events & TODO_RENAME_WINDOW_REFRESH_FLAG)) return;
	GfxEvent event;
	event.type = WINDOW_REFRESH;
	event.update.bounds = bounds;
	event.common.win = win;
	_event_queue_call_push(&event);
}

// public
fn void todo_fix_mouseButtonCallback(GfxWindow* win, GfxMouseButton button, bool press)
{
	GfxEvent event;
	if (press)
	{
		if (!(win.internal.enabled_events & TODO_RENAME_MOUSE_BUTTON_PRESSED_FLAG)) return;
		event.type = MOUSE_BUTTON_PRESSED;
	}
	else
	{
		if (!(win.internal.enabled_events & TODO_RENAME_MOUSE_BUTTON_RELEASED_FLAG)) return;
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
fn void todo_fix_mouseScrollCallback(GfxWindow* win, Vec2 delta)
{
	if (!(win.internal.enabled_events & TODO_RENAME_MOUSE_SCROLL_FLAG)) return;
	_gfx_ctx.scroll = delta;
	GfxEvent event;
	event.type = MOUSE_SCROLL;
	event.delta.vector = delta;
	event.common.win = win;
	_event_queue_call_push(&event);
}

// public
fn void todo_fix_scaleUpdatedCallback(GfxWindow* win, Vec2 scale)
{
	if (!(win.internal.enabled_events & TODO_RENAME_SCALE_UPDATED_FLAG)) return;
	GfxEvent event;
	event.type = SCALE_UPDATED;
	event.scale.scale = scale;
	event.common.win = win;
	_event_queue_call_push(&event);
}

// public
fn void todo_fix_monitorCallback(GfxWindow* win, GfxMonitor* monitor, bool connected)
{
	if (win != null)
	{
		if (connected && !(win.internal.enabled_events & TODO_RENAME_MONITOR_CONNECTED_FLAG)) return;
		if (!connected && !(win.internal.enabled_events & TODO_RENAME_MONITOR_DISCONNECTED_FLAG)) return;
	}
	GfxEvent event;
	event.type = connected ? MONITOR_CONNECTED : MONITOR_DISCONNECTED;
	event.monitor.monitor = monitor;
	event.monitor.state = connected;
	event.common.win = win;
	_event_queue_call_push(&event);
}

// public
fn void todo_fix_windowMaximizedCallback(GfxWindow* win, IntRect bounds)
{
	win.internal.flags.maximize = true;
	win.bounds = bounds;
	if (!(win.internal.enabled_events & TODO_RENAME_WINDOW_MAXIMIZED_FLAG)) return;
	GfxEvent event;
	event.type = WINDOW_MAXIMIZED;
	event.update.bounds = bounds;
	event.common.win = win;
	_event_queue_call_push(&event);
}

// public
fn void todo_fix_windowMinimizedCallback(GfxWindow* win)
{
	win.internal.flags.minimize = true;
	if (!(win.internal.enabled_events & TODO_RENAME_WINDOW_MINIMIZED_FLAG)) return;
	GfxEvent event;
	event.type = WINDOW_MINIMIZED;
	event.update.bounds = win.bounds;
	event.common.win = win;
	_event_queue_call_push(&event);
}

// public
fn void todo_fix_windowFocusCallback(GfxWindow* win, bool inFocus)
{
	win.internal.inFocus = inFocus;
	if (win.internal.capture_mouse) _window_set_capture_mouse_native(win, inFocus);
	GfxEvent event;
	event.common.win = win;
	event.focus.state = inFocus;
	if (inFocus == true)
	{
		if (win.internal.flags.fullscreen) todo_fix_window_raise(win);
		event.type = WINDOW_FOCUS_IN;
	}
	else
	{
		if (win.internal.flags.fullscreen) win.minimize();
		for (int i = 0; i < GFX_KEYS; i++)
		{
			GfxKey key = (GfxKey)i;
			if (!is_key_down(key)) continue;
			_gfx_ctx.keyboard[i].current = false;
			if (win.internal.enabled_events & TODO_RENAME_KEY_RELEASED_FLAG) todo_fix_keyCallback(win, key, win.internal.mod, false, false);
		}
		todo_fix_resetKey();
		event.type = WINDOW_FOCUS_OUT;
	}
	_event_queue_call_push(&event);
}


// public
fn void todo_fix_dataDragCallback(GfxWindow* win, GfxDataTransferType dataType, GfxDndActionType action, GfxPoint pos)
{
	if (!(win.internal.enabled_events & TODO_RENAME_DATA_DRAG_FLAG) || !win.internal.flags.allow_dnd) return;
	_gfx_ctx.window_state.win = win;
	_gfx_ctx.window_state.dataDragging = true;
	_gfx_ctx.window_state.drop = pos;
	GfxEvent event;
	event.type = DATA_DRAG;
	event.drag.pos = pos;
	event.drag.action = action;
	event.drag.dataType = dataType;
	event.common.win = win;
	_event_queue_call_push(&event);
}

// public
fn void todo_fix_dataDropCallback(GfxWindow* win, char* data, sz length, GfxDataTransferType dataType)
{
	if (!(win.internal.enabled_events & TODO_RENAME_DATA_DROP_FLAG) || !win.internal.flags.allow_dnd) return;
	_gfx_ctx.window_state.win = win;
	_gfx_ctx.window_state.dataDrop = true;
	_gfx_ctx.window_state.dataLength = length;
	GfxDataDropNode node = { .data = data, .length = length, .type = dataType };
	GfxEvent event;
	event.type = DATA_DROP;
	event.drop.value = &node;
	event.common.win = win;
	if (_gfx_ctx.callbacks[(sz)event.type] != null) _gfx_ctx.callbacks[(sz)event.type](&event);
	if (!_gfx_ctx.queueEvents && !_gfx_ctx.dnd_build) return;
	char* dataCopy = malloc(length);
	mem::copy(dataCopy, data, length);
	node.data = dataCopy;
	GfxDataDropNode* heapNode = mem::new(GfxDataDropNode);
	*heapNode = node;
	if (_gfx_ctx.dndRoot == null)
	{
		_gfx_ctx.dndRoot = heapNode;
		_gfx_ctx.dndCur = heapNode;
	}
	else
	{
		_gfx_ctx.dndCur.next = heapNode;
		_gfx_ctx.dndCur = heapNode;
	}
	event.drop.value = heapNode;
	event_queue_push(&event);
}

// public
fn void todo_fix_keyCharCallback(GfxWindow* win, uint codepoint)
{
	if (!(win.internal.enabled_events & TODO_RENAME_KEY_CHAR_FLAG)) return;
	GfxEvent event;
	event.type = KEY_CHAR;
	event.keyChar.value = codepoint;
	event.common.win = win;
	_event_queue_call_push(&event);
}

// public
fn void todo_fix_keyCallback(GfxWindow* win, GfxKey key, GfxKeyModifier mod, bool repeat, bool state)
{
	GfxEvent event;
	if (state)
	{
		if (!(win.internal.enabled_events & TODO_RENAME_KEY_PRESSED_FLAG)) return;
		event.type = KEY_PRESSED;
	}
	else
	{
		if (!(win.internal.enabled_events & TODO_RENAME_KEY_RELEASED_FLAG)) return;
		event.type = KEY_RELEASED;
	}
	_gfx_ctx.keyboard[(sz)key].prev = _gfx_ctx.keyboard[(sz)key].current;
	_gfx_ctx.keyboard[(sz)key].current = state;
	event.key.value = key; event.key.repeat = repeat; event.key.mod = mod; event.key.state = state;
	event.common.win = win;
	_event_queue_call_push(&event);
}

// public
fn void todo_fix_initKeycodes()
{
	_gfx_ctx.keycodes = {};
	todo_fix_initKeycodesPlatform();
	for (int i = 0; i < GFX_KEYS; i++)
	{
		for (int y = 0; y < _gfx_ctx.keycodes.len; y++)
		{
			if (_gfx_ctx.keycodes[y] == (GfxKey)i)
			{
				_gfx_ctx.apiKeycodes[i] = (uint)y;
				break;
			}
		}
	}
	todo_fix_resetKey();
}

// public
fn GfxKey todo_fix_apiKeyToRGFW(uint keycode)
{
	if (keycode >= _gfx_ctx.keycodes.len) return NULL;
	return _gfx_ctx.keycodes[keycode];
}

// public
fn uint todo_fix_rgfwToApiKey(GfxKey keycode)
{
	return _gfx_ctx.apiKeycodes[keycode];
}

// public
fn void todo_fix_resetKey()
{
	_gfx_ctx.keyboard = {};
}

// public
fn void todo_fix_setQueueEvents(bool queue) { _gfx_ctx.queueEvents = queue; }

// public
fn void todo_fix_resetPrevState()
{
	foreach (&entry : _gfx_ctx.keyboard) entry.prev = entry.current;
	foreach (mouse : GfxMouseButton::values)
	{
		_gfx_ctx.mouseButtons[mouse.ordinal].prev = _gfx_ctx.mouseButtons[mouse.ordinal].current;
	}
	_gfx_ctx.scroll = {};
	_gfx_ctx.vector = {};
	_gfx_ctx.window_state = {};
	GfxDataDropNode* node = _gfx_ctx.dndRoot;
	while (node != null)
	{
		GfxDataDropNode* next = node.next;
		free(node);
		node = next;
	}
	_gfx_ctx.dndRoot = null;
	_gfx_ctx.dndCur = null;
}


// public
fn void* todo_fix_window_getUserPtr(GfxWindow* win) => win.user_ptr;
// public
fn void todo_fix_window_setUserPtr(GfxWindow* win, void* ptr) { win.user_ptr = ptr; }

// public
fn bool todo_fix_window_getSizeInPixels(GfxWindow* win, int* w, int* h)
{
	GfxMonitor* mon = find_monitor(win);
	if (mon == null) return false;
	if (w != null) *w = (int)((float)win.width * mon.pixelRatio);
	if (h != null) *h = (int)((float)win.height * mon.pixelRatio);
	return true;
}

// public
fn bool todo_fix_window_didMouseLeave(GfxWindow* win) => _gfx_ctx.window_state.win_leave == win && _gfx_ctx.window_state.mouse_leave;
// public
fn bool todo_fix_window_didMouseEnter(GfxWindow* win) => _gfx_ctx.window_state.win == win && _gfx_ctx.window_state.mouse_enter;
// public
fn bool todo_fix_window_isMouseInside(GfxWindow* win) => win.internal.mouse_inside;
// public
fn bool todo_fix_window_isDataDragging(GfxWindow* win) => todo_fix_window_getDataDrag(win, null, null);
// public
fn bool todo_fix_window_didDataDrop(GfxWindow* win) => todo_fix_window_getDataDrop(win) != null;

// public
fn bool todo_fix_window_getDataDrag(GfxWindow* win, int* x, int* y)
{
	if (_gfx_ctx.window_state.win != win || _gfx_ctx.window_state.dataDragging == false) return false;
	if (x != null) *x = _gfx_ctx.window_state.drop.x;
	if (y != null) *y = _gfx_ctx.window_state.drop.y;
	return true;
}

// public
fn GfxDataDropNode* todo_fix_window_getDataDrop(GfxWindow* win)
{
	if (_gfx_ctx.window_state.win != win || _gfx_ctx.window_state.dataDrop == false) return null;
	return _gfx_ctx.dndRoot;
}

// public
fn void todo_fix_setRootWindow(GfxWindow* win) { _gfx_ctx.root = win; }
// public
fn GfxWindow* todo_fix_getRootWindow() { return _gfx_ctx.root; }

// public
fn void todo_fix_setBit(uint* value, uint mask, bool set)
{
	if (set) { *value |= mask; }
	else { *value &= ~mask; }
}


// public
fn void todo_fix_window_center(GfxWindow* win)
{
	GfxMonitor* mon = find_monitor(win);
	if (!mon) return;
	win.move(mon.origin + (mon.mode.size - win.size) / 2);
}

// public
fn bool todo_fix_monitor_scaleToWindow(GfxMonitor* mon, GfxWindow* win)
{
	GfxMonitorMode mode;
	mode.size = win.size;
	bool ret = todo_fix_monitor_requestMode(mon, &mode, SCALE);
	win.move(mon.origin);
	return ret;
}

// public
fn void todo_fix_splitBPP(uint bpp, GfxMonitorMode* mode)
{
	if (bpp == 32) bpp = 24;
	mode.rgb.r = mode.rgb.g = mode.rgb.b = (char)(bpp / 3);
	uint delta = bpp - (mode.rgb.r * 3);
	if (delta >= 1) mode.rgb.g = mode.rgb.g + 1;
	if (delta == 2) mode.rgb.r = mode.rgb.r + 1;
}

fn GfxNativeImage* GfxSurface.get_native_image(&self)
{
	return &self.native;
}

fn void GfxSurface.set_convert_fn(&self, GfxConvertImageDataFn func)
{
	self.convert_fn = func;
}

<*
 @param [&drop] self : "The surface to reclaim"
*>
fn void GfxSurface.destroy(&self)
{
	self.release();
	free(self);
}

fn bool GfxMonitorMode.equals_request(&self, GfxMonitorMode* mon2, GfxMonitorModeRequest request)
{
	return (self.size == mon2.size || !(request & SCALE)) &&
	((self.refresh_rate == mon2.refresh_rate) || !(request & REFRESH)) &&
	((self.rgb == mon2.rgb) || !(request & RGB));
}

fn void refresh_monitors()
{
	GfxMonitorNode* prev = _gfx_ctx.monitors.list.head;
	GfxMonitorNode* node = _gfx_ctx.monitors.list.head;
	while (node)
	{
		GfxMonitorNode* next = node.next;
		if (!node.disconnected)
		{
			todo_fix_monitorCallback(_gfx_ctx.root, &node.mon, false);
			_remove_monitor(node, prev);
		}
		prev = node;
		node = next;
	}
}

fn void free_monitors()
{
	GfxMonitorNode* node = _gfx_ctx.monitors.list.head;
	while (node)
	{
		GfxMonitorNode* next = node.next;
		_remove_monitor(node, node);
		node = next;
	}
}

fn GfxPoint get_global_mouse()
{
	return _get_global_mouse_native();
}

fn GfxMonitor*[] get_monitors()
{
	sz count = _gfx_ctx.monitors.count;
	if (!count) return {};
	GfxMonitor*[] monitors = mem::temp_array(GfxMonitor*, count);
	sz i = 0;
	GfxMonitorNode* cur_node = _gfx_ctx.monitors.list.head;
	while (cur_node)
	{
		monitors[i++] = &cur_node.mon;
		cur_node = cur_node.next;
	}
	return monitors;
}

<*
 @return? NOT_FOUND : "If there is no monitor found"
 @ensure return != null : "Return value cannot be null"
*> 
fn GfxMonitor*? get_primary_monitor()
{
	if (!_gfx_ctx.monitors.primary) _gfx_ctx.monitors.primary = _gfx_ctx.monitors.list.head;
	if (!_gfx_ctx.monitors.primary) return NOT_FOUND~;
	return &_gfx_ctx.monitors.primary.mon;
}

// public
fn GfxMonitorMode[] GfxMonitor.get_modes(&monitor, Allocator allocator)
{
	return alloc::clone_slice(allocator, _monitor_get_modes_native(monitor));
}

// public
fn GfxMonitorMode? GfxMonitor.get_best_matching_mode(&monitor, GfxMonitorMode mode) => @pool()
{
	GfxMonitorMode* chosen;
	int top_score = 1;
	foreach (&mode2 : _monitor_get_modes_native(monitor))
	{
		int score = 0;
		if (mode.size == mode2.size) score += 1000;
		if (mode.rgb == mode2.rgb) score += 100;
		if (mode.refresh_rate == mode2.refresh_rate) score += 10;
		if (score > top_score)
		{
			top_score = score;
			chosen = mode2;
		}
	}
	return chosen ? *chosen : NOT_FOUND~;
}


fn String GfxMonitor.get_name(&self)
{
	return ((ZString)&self.name[0]).str_view();
}

fn void GfxMonitor.set_user_ptr(&self, void* user_ptr)
{
	self.user_ptr = user_ptr;
}

fn void* GfxMonitor.get_user_ptr(&self)
{
	return self.user_ptr;
}

fn GfxMonitorMode GfxMonitor.get_mode(&self)
{
	return self.mode;
}

fn bool GfxMonitor.set_gamma(&self, float gamma)
{
	sz count = todo_fix_monitor_getGammaRampPtr(self, null);
	ushort* ptr = mem::alloc_array(ushort, count);
	bool ret = todo_fix_monitor_setGammaPtr(self, gamma, ptr, count);
	free(ptr);
	return ret;
}


// public
fn TodoRenameGammaRamp* todo_fix_monitor_getGammaRamp(GfxMonitor* monitor)
{
	TodoRenameGammaRamp* ramp = mem::new(TodoRenameGammaRamp);
	ramp.count = todo_fix_monitor_getGammaRampPtr(monitor, null);
	ramp.red = mem::alloc_array(ushort, ramp.count);
	ramp.green = mem::alloc_array(ushort, ramp.count);
	ramp.blue = mem::alloc_array(ushort, ramp.count);
	ramp.count = todo_fix_monitor_getGammaRampPtr(monitor, ramp);
	return ramp;
}

// public
fn void todo_fix_freeGammaRamp(TodoRenameGammaRamp* ramp)
{
	free(ramp.red);
	free(ramp.green);
	free(ramp.blue);
	free(ramp);
}

// public
fn bool todo_fix_monitor_setGammaPtr(GfxMonitor* monitor, float gamma, ushort* ptr, sz count)
{
	for (sz i = 0; i < count; i++)
	{
		float value = (float)i / (float)(count - 1);
		value = math::pow(value, 1.0f / gamma) * 65535.0f + 0.5f;
		value = min(value, 65535.0f);
		ptr[i] = (ushort)value;
	}
	TodoRenameGammaRamp ramp;
	ramp.red = ptr; ramp.green = ptr; ramp.blue = ptr; ramp.count = count;
	return todo_fix_monitor_setGammaRamp(monitor, &ramp);
}

fn void? GfxWindow.init(&self, String name, int x, int y, int w, int h, GfxWindowFlags flags)
{
	if (!_gfx_ctx.initialized) return GFX_CONTEXT_NOT_INITIALIZED~;
	*self = {};
	_gfx_ctx.windowCount++;
	if (!_gfx_ctx.root) todo_fix_setRootWindow(self);
	self.bounds = { x, y, w, h };
	self.internal.mouse = _gfx_ctx.standardMice[GfxMouseIcon.NORMAL.ordinal];
	self.internal.flags = flags;
	self.internal.enabled_events = TODO_RENAME_ALL_EVENT_FLAGS;
	todo_fix_createWindowPlatform(name, flags, self)!;
	_window_set_flags(self, flags, {});
	self.src.gfxType = 0;
	$if $feat(MACOS) || $feat(WIN32) || $feat(WASM):
		if (flags.open_gl)
		{
			todo_fix_window_createContext_OpenGL(self, todo_fix_getGlobalHints_OpenGL());
		}
	$endif
	$if $feat(MACOS):
		if (flags.egl) todo_fix_window_createContext_EGL(self, todo_fix_getGlobalHints_OpenGL());
	$endif
	if (!flags.hide_mouse) self.set_mouse_default();
	self.set_name(name);
	switch
	{
		case !flags.hide:
			flags.hide = true;
			self.show();
		case flags.maximize || flags.fullscreen:
			self.hide();
	}
}


fn bool GfxWindow.will_capture_mouse(&self) => self.internal.capture_mouse;

fn bool GfxWindow.is_raw_mouse(&self) => self.internal.raw_mouse;

fn bool GfxWindow.is_hidden(&self)
{
	return _window_is_hidden_native(self);
}

fn bool GfxWindow.is_maximized(&self)
{
	return _window_is_maximized_native(self);
}

fn bool GfxWindow.is_floating(&self)
{
	return _window_is_floating_native(self);
}

fn bool GfxWindow.set_icon(&self, void* data_src, int w, int h, GfxFormat format, GfxWindowIconType type = BOTH)
{
	return _window_set_icon_native(self, data_src, w, h, format, type);
}

fn bool GfxWindow.is_minimized(&self)
{
	return _window_is_minimized_native(self);
}

fn void GfxWindow.set_capture_mouse(&self, bool capture)
{
	self.internal.capture_mouse = capture;
	_window_set_capture_mouse_native(self, capture);
}

macro void GfxWindow.set_aspect_ratio(&self, int w, int h)
{
	_window_set_aspect_ratio_native(self, w, h);
}

fn void GfxWindow.set_raw_mouse_mode(&self, bool enabled)
{
	self.internal.raw_mouse = enabled;
	_window_set_raw_mouse_mode_native(self, enabled);
}

fn void GfxWindow.set_capture_raw_mouse(&self, bool enabled)
{
	self.set_capture_mouse(enabled);
	self.set_raw_mouse_mode(enabled);
}

fn bool GfxWindow.is_mouse_hidden(&self)
{
	return self.internal.flags.hide_mouse;
}

fn void GfxWindow.update_key_modifier(&self, GfxKeyModifier mod, bool enable)
{
	if (enable)
	{
		self.internal.mod |= mod;
	}
	else
	{
		self.internal.mod &= ~mod;
	}
}

fn GfxFormat native_format()
{
	return $feat(WASM) ? RGBA8 : BGRA8;
}

fn bool GfxWindow.is_borderless(&win)
{
	return win.internal.flags.no_border;
}
fn bool GfxWindow.is_fullscreen(&win)
{
	return win.internal.flags.fullscreen;
}
fn bool GfxWindow.allows_dnd(&win)
{
	return win.internal.flags.allow_dnd;
}


fn bool GfxWindow.set_mouse_default(&self)
{
	return self.set_mouse_standard(NORMAL);
}

fn bool GfxWindow.set_mouse_standard(&self, GfxMouseIcon icon)
{
	return self.set_mouse(_gfx_ctx.standardMice[(sz)icon]);
}

fn bool GfxWindow.set_mouse(&self, TodoRenameMouse* mouse)
{
	if (mouse != _gfx_ctx.hiddenMouse) self.internal.mouse = mouse;
	return _window_set_mouse_native(self, mouse);
}


fn void set_raw_mouse_mode(bool enabled)
{
	_gfx_ctx.raw_mouse = enabled;
	if (_gfx_ctx.root) _window_set_raw_mouse_mode_native(_gfx_ctx.root, enabled);
}


fn void GfxWindow.hide(&self)
{
	_window_hide_native(self);
}

fn void GfxWindow.show(&self)
{
	if (self.internal.flags.focus_on_show) _window_focus_native(self);
	self.internal.flags.hide = false;
	_window_show_native(self);
	if (self.internal.flags.maximize) self.maximize();
}

fn void GfxWindow.restore(&self)
{
	_window_restore_native(self);
	self.internal.flags.maximize = false;
	self.show();
	self.move(self.internal.old_bounds.origin);
	self.resize(self.internal.old_bounds.size);
}

fn void GfxWindow.minimize(&self)
{
	todo_fix_window_minimizePlatform(self);
	self.internal.flags.maximize = false;
}

fn GfxPoint GfxWindow.fetch_size(&self)
{
	return _window_fetch_size_native(self);
}

fn void GfxWindow.maximize(&self)
{
	self.internal.old_bounds = self.bounds;
	self.internal.flags.maximize = true;
	_window_fetch_size_native(self);
	_window_maximize_native(self);
}

fn void GfxWindow.move(&self, GfxPoint where)
{
	self.origin = where;
	_window_move_native(self, where);
	self.internal.flags.maximize = false;
}

fn void GfxWindow.resize(&self, GfxSize size)
{
	self.size = size;
	_window_resize_native(self, size);
	self.internal.flags.maximize = false;
}

fn void GfxWindow.show_mouse(&self, bool show)
{
	_window_show_mouse_native(self, show);
}


fn void GfxWindow.set_dnd(&self, bool allow)
{
	self.internal.flags.allow_dnd = allow;
	$if $feat(WIN32):
		dragAcceptFiles(self.src.window, (int)allow);
	$endif
}

fn GfxFn get_opengl_proc(ZString s)
{
	return _get_opengl_proc_native(s);
}


// public
fn bool todo_fix_extensionSupportedStr(ZString extensions, ZString ext, sz len) @feat(!WASM)
{
	if (extensions == null || ext == null) return false;
	while (ext[len - 1] == 0 && len > 3) len--;
	ZString start = extensions;
	ZString where = (ZString)libc::strstr(extensions, ext);
	while (where != null)
	{
		ZString terminator = where + len;
		if (((char*)where == (char*)start || *((char*)where - 1) == ' ') && (terminator[0] == ' ' || terminator[0] == 0)) return true;
		where = (ZString)libc::strstr(terminator, ext);
	}
	return false;
}

fn void? load_egl()
{
	return _load_egl_native() @inline;
}

fn void unload_egl()
{
	_unload_egl_native() @inline;
}

fn void? load_opengl()
{
	return _load_opengl_native() @inline;

}

fn void unload_opengl()
{
	_unload_opengl_native() @inline;
}

fn void? load_vulkan()
{
	return _load_vulkan_native() @inline;
}

fn void unload_vulkan()
{
	_unload_vulkan_native() @inline;
}


alias TodoRenameEventFlag = uint;
alias TodoRenameEventWait = int;
alias TodoRenameDebugType = char;
alias TodoRenameErrorCode = char;
alias TodoRenameGlReleaseBehavior = int;
alias TodoRenameGlProfile = int;
alias TodoRenameGlRenderer = int;
alias TodoRenameGfxContextType = uint;

typedef TodoRenameMouse = void;
alias GfxFn = fn void();
alias GfxConvertImageDataFn = fn void(char* dest_data, char* src_data, GfxColorLayout* srcLayout, GfxColorLayout* destLayout, sz count);
alias GfxGenericFunc = fn void(GfxEvent* e);
alias TodoRenameDebugFunc = fn void(TodoRenameDebugInfo* info);

struct GfxColorLayout { int r, g, b, a; int channels; }

struct TodoRenameGammaRamp
{
	ushort* red;
	ushort* green;
	ushort* blue;
	sz count;
}

const TodoRenameGfxContextType TODO_RENAME_GFX_NATIVE_OPEN_GL = 1;
const TodoRenameGfxContextType TODO_RENAME_GFX_EGL = 2;
const TodoRenameGfxContextType TODO_RENAME_GFX_OWNED_BY_RGFW = 4;



// public
fn void _mouse_notify_callback(GfxWindow* win, GfxPoint where, bool status)
{
	win.internal.mouse_inside = status;
	_gfx_ctx.window_state.win = win;
	win.internal.last_mouse = where;
	GfxEvent event;
	event.common.win = win;
	event.mouse.pos = where;
	event.mouse.in_window = win.internal.mouse_inside;
	if (status)
	{
		if (!(win.internal.enabled_events & TODO_RENAME_MOUSE_ENTER_FLAG)) return;
		_gfx_ctx.window_state.mouse_enter = true;
		event.type = MOUSE_ENTER;
	}
	else
	{
		if (!(win.internal.enabled_events & TODO_RENAME_MOUSE_LEAVE_FLAG)) return;
		_gfx_ctx.window_state.win_leave = win;
		_gfx_ctx.window_state.mouse_leave = true;
		event.type = MOUSE_LEAVE;
	}
	_event_queue_call_push(&event);
}

fn void todo_fix_window_setEventState(GfxWindow* win, TodoRenameEventFlag event, bool state) { todo_fix_setBit(&win.internal.enabled_events, event, state); }

// public
fn void _mouse_motion_callback(GfxWindow* win, GfxPoint where)
{
	win.internal.last_mouse = where;
	if (!(win.internal.enabled_events & TODO_RENAME_MOUSE_MOTION_FLAG)) return;
	GfxEvent event;
	event.type = MOUSE_MOTION;
	event.mouse.pos = where;
	event.mouse.in_window = win.internal.mouse_inside;
	event.common.win = win;
	_event_queue_call_push(&event);
}

fn void _event_queue_call_push(GfxEvent* event)
{
	if (_gfx_ctx.callbacks[event.type]) _gfx_ctx.callbacks[event.type](event);
	event_queue_push(event);
}

GfxContext _gfx_ctx;

