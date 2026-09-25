// Copyright (c) 2026 Christoffer Lernö. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::gfx @feat(MACOS, LINUX, BSD, WASM, WIN32) @feat(!(BSD | LINUX) | WAYLAND | !NO_X11);
import std::math, std::os, std::os::darwin, libc, std::time, std::collections::enummap;
import std::gfx::x11 @public, std::gfx::egl @public, std::gfx::gl @public;
import std::gfx::wl @public;

// The GFX module provides basic window handling across multiple platforms.
// This implementation was strongly based on RGFW 2.0 by Riley Mabb
// Although not in `std::experimental`, consider the code largely experimental until 0.8.6

excuse
{
	NO_DATA_DRAG,
	GFX_CONTEXT_NOT_INITIALIZED,
	SURFACE_CREATION_FAILED,
	WAYLAND_DISPLAY_NOT_LOADED,
	WAYLAND_COMPOSITOR_NOT_FOUND,
	WAYLAND_LAYER_SHELL_NOT_SUPPORTED,
	X11_DISPLAY_NOT_LOADED,
	X11_WINDOW_CREATION_FAILED,
	X11_CTX_CREATION_FAILED,
	NSAPP_CREATION_FAILED,
	NSWINDOW_CREATION_FAILED,
	DELEGATE_CREATION_FAILED,
	MONITOR_MODE_CHANGE_FAILED,
	MISSING_VULKAN_LIB,
	MISSING_OPENGL_LIB,
	MISSING_VK_GET_INSTANCE_PROC_ADDR,
	MISSING_GL_GET_PROC_ADDR,
	VULKAN_NOT_LOADED,
	GLX_VISUAL_CONFIGS_NOT_FOUND,
	GLX_NO_VALID_VISUAL,
	OPENGL_CTX_CREATION_FAILED,
	OPENGL_PIXEL_FORMAT_LOAD_FAILED,
	DIRECTX_SWAP_CHAIN_CREATION_FAILED,
	CANNOT_RETRIEVE_GAMMA,
	CLIPBOARD_COULD_NOT_BE_OPENED,
	COULD_NOT_OWN_X11_CLIPBOARD,
	GAMMA_COULD_NOT_BE_SET,
	CLIPBOARD_DID_NOT_CONTAIN_TYPE,
	CLIPBOARD_WRITE_FAILED,
	FAILED_TO_CREATE_WGL_PIXEL_FORMAT,
	FAILED_TO_SET_WGL_PIXEL_FORMAT,
	FAILED_TO_LOAD_SWAP_INTERVAL_FN,
	MOUSE_NOT_SUPPORTED,
	IMAGE_COULD_NOT_LOAD,
	EGL_LIBRARY_NOT_FOUND,
	EGL_MISSING_GET_PROC_ADDR,
	EGL_BINDING_FAILED,
	EGL_PROC_LOAD_FAILED,
	EGL_CONTEXT_CREATION_FAILED,
	EGL_SURFACE_CREATION_FAILED,
	EGL_NOT_SUPPORTED,
	EGL_FAILED_TO_INIT_DISPLAY,
	EGL_DISPLAY_NOT_FOUND,
	EGL_CONFIG_NOT_FOUND,
}

const Duration WAIT_FOREVER = Duration::max;

alias GfxGenericFn = fn void(GfxEvent* e);
alias GfxConvertImageDataFn = fn void(char* dst_data, char* src_data, GfxColorLayout* src_layout, GfxColorLayout* dst_layout, sz count);
alias GfxRepaintFn = fn void(GfxWindow* w, GfxRect rect);

alias GfxPoint = int[<2>];
alias GfxSize = int[<2>];
alias GfxRect = IntRect;
typedef GfxMouse = void;

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

struct GfxColorLayout
{
	int r, g, b, a;
	int channels;
}

struct GfxGammaRamp
{
	ushort[] red;
	ushort[] green;
	ushort[] blue;
	Allocator a;
}

struct GfxMonitor
{
	GfxPoint origin;
	char[128] name;
	Vec2 scale;
	float pixel_ratio;
	Vec2 phys_size;
	GfxMonitorMode mode;
	void* user_ptr;
	GfxMonitorNode* node;
}

struct GfxWindow
{
	inline GfxRect    bounds;
	GfxRepaintFn      repaint_fn;
	GfxWindowSrc      src;
	GfxWindowInternal internal;
	void*             user_ptr;
}

struct GfxDataTransfer
{
	String text;
	GfxDataTransferType type;
}

struct GfxDataDropNode
{
	char[] data;
	GfxDataTransferType type;
	GfxDataDropNode* next;
}

struct GfxWindowSrc @feat(WASM)
{
	inline GfxRect    bounds;
	void*             window;
	void*             view;
	void*             mouse;
	void*             delegate;
	GfxBackingContext ctx;
	GfxBacking        backing;
}


struct GfxWindowSrc @feat(MACOS)
{
	inline GfxRect    bounds;
	ObjId             window;
	ObjId             view;
	ObjId             mouse;
	ObjId             delegate;
	ObjId             resize_timer;
	GfxBackingContext ctx;
	GfxBacking        backing;
}

struct GfxWindowSrc @feat(WIN32)
{
	inline GfxRect      bounds;
	Win32_HWND          window;
	Win32_HDC           hdc;
	Win32_HICON         hIconSmall;
	Win32_HICON         hIconBig;
	GfxPoint            min_size;
	GfxPoint            max_size;
	int                 aspect_ratio_w, aspect_ratio_h;
	bool                action_frame;
	int                 modal_loop_depth;
	ushort              high_surrogate;
	GfxBackingContext   ctx;
	GfxBacking          backing;
}

struct GfxWindowSrc @feat(BSD, LINUX)
{
	inline GfxRect bounds;
	GfxBackingContext ctx;
	GfxBacking backing;
	struct @feat(!NO_X11)
	{
		XWindow window;
		XWindow parent;
		GContext gc;
		XlibIC ic;
		Clock flash_end;
		XSyncCounter counter;
		XSyncValue resize_id;
		bool resize_in_progress;
	}
	struct @feat(WAYLAND)
	{
		WlProxy* surface;
		WlProxy* layer_surface;
		WlProxy* xdg_surface;
		WlProxy* xdg_toplevel;
		WlProxy* decoration;
		WlProxy* locked_pointer;
		WlProxy* icon;
		WlProxy* data_source;
		WlProxy* custom_cursor_surface;
		GfxMonitorNode* active_monitor;
		uint decoration_mode;
		bool pending_activated;
		bool activated;
		bool resizing;
		bool pending_maximized;
		bool maximized;
		bool minimized;
		bool configured;
		bool using_custom_cursor;
	}
}

struct GfxNativeImage
{
	GfxFormat format;
	char* buffer;
	struct @feat(BSD, LINUX) @feat(!NO_X11) // X11
	{
		void* bitmap ;   // XImage*
	}
	struct @feat(WAYLAND)
	{
		void* pool;      // wl_shm_pool* (Wayland)
		int fd;          // Wayland
		void* wl_buffer; // wl_buffer* (Wayland)
	}
	struct @feat(WIN32)
	{
		Win32_HBITMAP bitmap;
		Win32_HDC hdc_mem;
	}
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
	GfxDataTransferType data_type;
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
	GfxRect bounds;
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
	GfxKeyCharEvent key_char;
	GfxDataDropEvent drop;
	GfxDataDragEvent drag;
	GfxScaleUpdatedEvent scale;
	GfxMonitorEvent monitor;
}

alias GfxEventCallbacks = EnumMap{GfxEventType, GfxGenericFn};

struct GfxGlContext
{
	struct @feat(MACOS)
	{
		ObjId ctx;
		ObjId format;
	}
	struct @feat(!MACOS)
	{
		void* ctx;
		void* window @feat(!(WIN32 | WASM));
		void* format @feat(WASM);
	}
	struct @feat(WAYLAND)
	{
		GfxEglContext egl;
	}
}

struct GfxEglContext
{
	void* ctx;
	void* surface;
	void* egl_window;
}

const GfxGlHints GLOBAL_HINTS = {
	.double_buffer = 1,
	.rgba_size = (int[<4>])8,
	.depth = 24,
	.release_behavior = NONE,
	.profile = CORE,
	.major = 1,
	.minor = 0,
	.renderer = ACCELERATED,
};

GfxGlHints _global_hints_open_gl = GLOBAL_HINTS;


struct GfxGlHints
{
	int stencil;
	int samples;
	int stereo;
	int aux_buffers;
	int double_buffer;
	int[<4>] rgba_size;
	int depth;
	int[<4>] accum_size;
	bool srgb;
	bool robustness;
	bool debug;
	bool no_error;
	GfxGlReleaseBehavior release_behavior;
	GfxGlProfile profile;
	int major, minor;
	GfxGlContext* share;
	GfxEglContext* share_egl;
	GfxGlRenderer renderer;
}


constset GfxWindowIconType
{
	TASKBAR  = 1,
	WINDOW   = 2,
	BOTH     = 3,
}

enum GfxGlReleaseBehavior
{
	FLUSH,
	NONE
}

enum GfxGlProfile
{
	CORE,
	FORWARD_COMPATIBILITY,
	COMPATIBILITY,
	ES,
	WEB
}

enum GfxGlRenderer
{
	ACCELERATED,
	SOFTWARE
}

constset GfxMonitorModeRequest : char
{
	SCALE   = 0b001,
	REFRESH = 0b010,
	RGB     = 0b100,
	ALL     = 0b111,
}

bitstruct GfxInitOption : char
{
	bool init_opengl;
	bool init_egl;
	bool init_vulkan;
	bool enable_x11;
	<*
	 Opt in to the X11 _NET_WM_SYNC_REQUEST resize handshake, which lets the window
	 manager pace a resize against the frames actually drawn. Only applied to windows
	 created with `opengl`, matching SDL, which reports it working poorly with Vulkan.
	*>
	bool enable_x11_resize_sync;
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

bitstruct GfxEventMask : uint
{
	bool key_pressed           : 1;
	bool key_released          : 2;
	bool key_char              : 3;
	bool mouse_button_pressed  : 4;
	bool mouse_button_released : 5;
	bool mouse_scroll          : 6;
	bool mouse_motion          : 7;
	bool mouse_raw_motion      : 8;
	bool mouse_enter           : 9;
	bool mouse_leave           : 10;
	bool window_moved          : 11;
	bool window_resized        : 12;
	bool window_focus_in       : 13;
	bool window_focus_out      : 14;
	bool window_close          : 15;
	bool window_maximized      : 16;
	bool window_minimized      : 17;
	bool window_restored       : 18;
	bool data_drop             : 19;
	bool data_drag             : 20;
	bool scale_updated         : 21;
	bool monitor_connected     : 22;
	bool monitor_disconnected  : 23;
}

const GfxEventMask KEY_EVENTS            = { .key_pressed, .key_released, .key_char };
const GfxEventMask MOUSE_EVENTS          = { .mouse_button_pressed, .mouse_button_released, .mouse_scroll, .mouse_motion, .mouse_raw_motion, .mouse_enter, .mouse_leave };
const GfxEventMask WINDOW_EVENTS         = { .window_moved, .window_resized, .window_maximized, .window_minimized, .window_restored, .scale_updated };
const GfxEventMask WINDOW_FOCUS_EVENTS   = { .window_focus_in, .window_focus_out };
const GfxEventMask DATA_DRAG_DROP_EVENTS = { .data_drop, .data_drag };
const GfxEventMask MONITOR_EVENTS        = { .monitor_connected, .monitor_disconnected };
const GfxEventMask ALL_EVENTS            = (GfxEventMask)uint::max;



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

<*
 Convert a run of pixels from one channel layout to another.

 @param [out] dst_data : "Destination pixel buffer"
 @param [in] src_data : "Source pixel buffer"
 @param [in] src_layout : "Channel offsets of the source format"
 @param [in] dst_layout : "Channel offsets of the destination format"
 @param count : "Number of pixels to convert"
 @param is64bit : "Write every second destination pixel, for 64-bit destination rows"
 @require src_layout != null && dst_layout != null
*>
fn void convert_image_data_64(char* dst_data, char* src_data, GfxColorLayout* src_layout, GfxColorLayout* dst_layout, sz count, bool is64bit)
{
	char[4] rgba = {0, 0, 0, 0};
	sz i2 = 0;
	for (sz i = 0; i < count; i++)
	{
		char* src_px = &src_data[i * src_layout.channels];
		char* dst_px = &dst_data[i2 * dst_layout.channels];
		rgba[0] = src_px[src_layout.r];
		rgba[1] = src_px[src_layout.g];
		rgba[2] = src_px[src_layout.b];
		rgba[3] = src_layout.channels == 4 ? src_px[src_layout.a] : 255;
		dst_px[dst_layout.r] = rgba[0];
		dst_px[dst_layout.g] = rgba[1];
		dst_px[dst_layout.b] = rgba[2];
		if (dst_layout.channels == 4) dst_px[dst_layout.a] = rgba[3];
		i2 += 1 + (sz)is64bit;
	}
}

<*
 @require !@catch(dst_w.overflow_mul(dst_h))
*>
fn void copy_image_data_64(char* dst_data, int dst_w, int dst_h, GfxFormat dst_format, char* src_data, GfxFormat src_format, bool is64bit, GfxConvertImageDataFn func)
{
	sz count = (sz)dst_w * dst_h;
	if (src_format == dst_format)
	{
		if (dst_data == src_data) return;
		int channels = dst_format.channels;
		mem::copy(dst_data, src_data, count * channels);
		return;
	}
	GfxColorLayout* src_layout = &&src_format.layout;
	GfxColorLayout* dst_layout = &&dst_format.layout;
	if (is64bit || !func)
	{
		convert_image_data_64(dst_data, src_data, src_layout, dst_layout, count, is64bit);
	}
	else
	{
		func(dst_data, src_data, src_layout, dst_layout, count);
	}
}

<*
 Copy an image, converting between pixel formats if they differ.

 @param [out] dst_data : "Destination pixel buffer, at least w * h * dst_format.channels bytes"
 @param w : "Image width in pixels"
 @param h : "Image height in pixels"
 @param dst_format : "Pixel format to write"
 @param [in] src_data : "Source pixel buffer"
 @param src_format : "Pixel format of the source"
 @param func : "Optional accelerated converter, null to use the generic path"
 @require w >= 0 && h >= 0 : "Dimensions may not be negative"
 @require !@catch(w.overflow_mul(h)) : "Image dimensions overflow"
*>
fn void copy_image_data(char* dst_data, int w, int h, GfxFormat dst_format, char* src_data, GfxFormat src_format, GfxConvertImageDataFn func)
{
	copy_image_data_64(dst_data, w, h, dst_format, src_data, src_format, false, func);
}

<*
 @require !_gfx.initialized : "Already initialized"
 @require [class_name] class_name.len < 254 : "Class name is too long"
*>
fn void? init(String class_name, GfxInitOption options)
{
	class_name = class_name ?: "C3GFX";
	_gfx = { .initialized = true };
	_gfx.windows.init(mem);
	defer catch
	{
		_destroy_platform();
		_gfx = {};
	}
	_init_platform(class_name, options)!;
	if (options.init_opengl) load_opengl()!;
	if (options.init_egl) load_egl()!;
	if (options.init_vulkan) load_vulkan()!;
	_init_keycodes();
	foreach (icon : GfxMouseIcon::values)
	{
		_gfx.standard_mice[icon.ordinal] = _create_standard_mouse(icon) ?? null;
	}
	_poll_monitors();
	log::info("Global context initialized", category: log::CATEGORY_VIDEO);
}

<*
 Shut down the global context: destroy every remaining window, release the standard
 mouse cursors and monitor list, unload OpenGL and EGL, then clear the context.

 Safe to call when not initialized, in which case it does nothing.
*>
fn void finalize()
{
	if (!_gfx.initialized) return;
	foreach (&win : _gfx.windows)
	{
		if (!win.internal.is_collected) win.destroy();
	}
	assert (_gfx.active_windows == 0);
	_gfx.windows.free();

	foreach (icon : GfxMouseIcon::values)
	{
		if (_gfx.standard_mice[icon.ordinal]) _free_mouse(_gfx.standard_mice[icon.ordinal]);
	}
	free_monitors();
	log::info("Global context destroyed", category: log::CATEGORY_VIDEO);
	unload_egl();
	unload_opengl();
	_destroy_platform();
	_gfx.root = null;
	_gfx = {};
}

<*
 Re-scan the system for connected monitors and rebuild the monitor list.
*>
fn void poll_monitors()
{
	_poll_monitors();
}

<*
 Allocate a surface and initialize it from raw pixel data.

 @param [in] data : "Source pixel data, w * h pixels in the given format"
 @param w : "Surface width in pixels"
 @param h : "Surface height in pixels"
 @param format : "Pixel format of the source data"
 @param [in] window : "Window the surface will be blitted to, or null for any"
 @return "The new surface, to be released with destroy()"
 @require w >= 0 && h >= 0 : "Dimensions may not be negative"
 @ensure return != null
*>
fn GfxSurface*? create_surface(char* data, int w, int h, GfxFormat format, GfxWindow* window = null)
{
	GfxSurface* s = mem::new(GfxSurface);
	defer catch free(s);
	s.init(data, w, h, format, window: window)!;
	return s;
}

<*
 Release the platform resources held by the surface, leaving the surface itself intact.

 Use destroy() instead to also free a surface obtained from create_surface().
*>
fn void GfxSurface.release(&self)
{
	_surface_release(self);
}

<*
 Initialize an already allocated surface from raw pixel data.

 @param [in] data : "Source pixel data, w * h pixels in the given format"
 @param w : "Surface width in pixels"
 @param h : "Surface height in pixels"
 @param format : "Pixel format of the source data"
 @param [in] window : "Window the surface will be blitted to, or null for any"
 @require w >= 0 && h >= 0 : "Dimensions may not be negative"
*>
fn void? GfxSurface.init(&self, void* data, int w, int h, GfxFormat format, GfxWindow* window = null)
{
	return _surface_init(self, data, w, h, format, window);
}

<*
 Create a new window.

 @param [in] name : "The window title"
 @param x : "Starting x position"
 @param y : "Starting y position"
 @param w : "Window width"
 @param h : "Window height"
 @param flags : "Customizable window properties"

 @return "The window"

 @ensure return != null
 @require [w] w >= 0 : "Width may not be negative"
 @require [h] h >= 0 : "height may not be negative"
*>
fn GfxWindow*? create_window(String name, int x, int y, int w, int h, GfxWindowFlags flags)
{
	return _create_window(name, x, y, w, h, flags);
}

<*
 Check whether a mouse button went down during the current frame.

 Unlike is_mouse_down() this is only true on the transition, so it fires once per press.

 @param button : "The button to test"
 @return "True if the button is down now and was up on the previous poll"
*>
fn bool is_mouse_pressed(GfxMouseButton button) => _gfx.mouse_buttons[(sz)button].current && !_gfx.mouse_buttons[(sz)button].prev;

<*
 Request attention for the window, typically by flashing its taskbar entry.

 @param request : "CANCEL to stop, BRIEFLY for a single flash, UNTIL_FOCUSED to flash until focused"
*>
fn void GfxWindow.flash(&self, GfxFlashRequest request) => _window_flash(self, request);

<*
 Keep the window above other windows, or release it back to normal stacking.

 @param floating : "True to keep the window on top"
*>
fn void GfxWindow.set_floating(&self, bool floating)
{
	_window_set_floating(self, floating);
}

<*
 Set the opacity of the window on supported platforms.

 @require [opacity] opacity >= 0.0 && opacity <= 1.0 : "Opacity must be in the range 0 to 1"
*>
fn void GfxWindow.set_opacity(&self, float opacity)
{
	_window_set_opacity(self, opacity);
}

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
	return self.internal.should_close || (self.internal.exit_key && is_key_down(self.internal.exit_key));
}

<*
 Give the window input focus and bring it to the front.
*>
fn void GfxWindow.focus(&self)
{
	_window_focus(self);
}

<*
 Set or clear the window's should-close flag.

 Setting it runs the close callback, so a handler may veto the request; clearing it is
 immediate and is how a close is cancelled from inside that callback.

 @param should_close : "True to request a close, false to cancel one"
*>
fn void GfxWindow.set_should_close(&win, bool should_close)
{
	if (!should_close)
	{
		win.internal.should_close = false;
		return;
	}
	_window_close_callback(win);
}

<*
 Get the platform-specific backing data for the window.

 @return "The native handles for this window, valid until the window is destroyed"
 @ensure return != null
*>
fn GfxWindowSrc* GfxWindow.get_src(&self)
{
	return &self.src;
}

<*
 Get the position of the window's top-left corner in screen coordinates.

 @return "The window origin"
*>
fn GfxPoint GfxWindow.get_origin(&win)
{
	return win.origin;
}

<*
 Get the size of the window's content area, in screen coordinates.

 This is not necessarily pixels on a scaled display; see get_size_in_pixels().

 @return "The window size"
*>
fn GfxPoint GfxWindow.get_size(&self)
{
	return self.size;
}

<*
 Create a Vulkan surface for the window.

 @param instance : "The Vulkan instance to create the surface on"
 @return "The new VkSurfaceKHR"
*>
fn VkSurfaceKHR? GfxWindow.create_vulkan_surface(&self, VkInstance instance)
{
	return _window_create_vulkan_surface(self, instance);
}

fn String[] get_required_vulkan_extensions()
{
	return _get_required_vulkan_extensions();
}

fn bool get_vulkan_physical_device_presentation_support(VkInstance instance, VkPhysicalDevice physical_device, uint queue_family_index)
{
	return get_vulkan_physical_device_presentation_support(instance, physical_device, queue_family_index);
}

<*
 Create a WebGPU surface for the window.

 @param instance : "The WGPU instance to create the surface on"
 @return "The new WGPUSurface"
*>
fn WGPUSurface? GfxWindow.create_wgpu_surface(&self, WGPUInstance instance)
{
	return _window_create_wgpu_surface(self, instance);
}

<*
 Set the OpenGL buffer swap interval for the window.

 Requires an OpenGL context to be current. Not every platform honours this.

 @param swap_interval : "0 to disable vsync, 1 to sync to the display refresh"
*>
fn void GfxWindow.set_opengl_swap_interval(&self, int swap_interval)
{
	_window_set_opengl_swap_interval(self, swap_interval);
}

<*
 Set the EGL buffer swap interval for the window.

 @param swap_interval : "0 to disable vsync, 1 to sync to the display refresh"
*>
fn void GfxWindow.set_egl_swap_interval(&self, int swap_interval)
{
	egl::set_swap_interval(self, swap_interval);
}

<*
 Create a DirectX swap chain for the window.

 @param [in] factory : "The DXGI factory to create the swap chain from"
 @param [in] device : "The D3D device the swap chain will present with"
 @param [out] swapchain : "Receives the created swap chain"
 @require factory != null && device != null && swapchain != null
*>
fn void? GfxWindow.create_directx_swap_chain(&self, Win32_IDXGIFactory* factory, void* device, void** swapchain) @feat(WIN32)
{
	return _window_create_directx_swap_chain_native(self, factory, device, swapchain);
}

<*
 Present the window's OpenGL back buffer.
*>
fn void GfxWindow.swap_opengl_buffers(&self)
{
	_window_opengl_swap_buffers(self);
}

<*
 Present the window's EGL back buffer.
*>
fn void GfxWindow.swap_egl_buffers(&self)
{
	egl::swap_window_buffers(self);
}

<*
 Make the given window's OpenGL context current on the calling thread.

 @param [in] win : "The window whose context to make current"
*>
fn void set_current_opengl_window(GfxWindow* win)
{
	_window_make_current_opengl_context(win);
}

<*
 Make the given window's EGL context current on the calling thread.

 @param [in] win : "The window whose context to make current"
*>
fn void set_current_egl_window(GfxWindow* win)
{
	egl::set_current_window(win);
}

<*
 Get the EGL context current on the calling thread.

 @return "The current EGL context, or null if there is none"
*>
fn void* get_current_egl_context()
{
	return egl::get_current_context();
}

<*
 Get the OpenGL context current on the calling thread.

 @return "The current native OpenGL context, or null if there is none"
*>
fn void* get_current_opengl_context()
{
	return _get_current_opengl_context();
}

<*
 Check whether an OpenGL extension is available.

 Queries the GL extension list first, then any platform-specific list such as GLX or WGL.
 Requires a current OpenGL context.

 @param [in] extension : "The extension name, for example GL_ARB_multisample"
 @return "True if the extension is present"
*>
fn bool opengl_extension_is_supported(String extension)
{
	if (gl::extension_supported_base(extension, &get_opengl_proc)) return true;
	return _opengl_extension_is_supported_platform(extension);
}

<*
 Check whether an EGL extension is available.

 @param [in] extension : "The extension name, for example EGL_KHR_create_context"
 @return "True if the extension is present"
*>
fn bool egl_extension_is_supported(String extension)
{
	return egl::is_extension_supported(extension);
}

<*
 Get the window's OpenGL context.

 @return "The context, or null if the window is not backed by OpenGL"
*>
fn GfxGlContext* GfxWindow.get_opengl_context(&self)
{
	if (!self.src.backing.opengl) return null;
	return self.src.ctx.native;
}


<*
 Get the window's EGL context.

 @return "The context, or null if the window is not backed by EGL"
*>
fn GfxEglContext* GfxWindow.get_egl_context(&self)
{
	if (!self.src.backing.egl) return null;
	return self.src.ctx.egl;
}

<*
 Get the window's current property flags.

 @return "The active window flags"
*>
fn GfxWindowFlags GfxWindow.get_flags(&self)
{
	return self.internal.flags;
}

<*
 Enter or leave fullscreen on the window's current monitor.

 @param fullscreen : "True to go fullscreen, false to restore"
*>
fn void GfxWindow.set_fullscreen(&self, bool fullscreen)
{
	_window_set_fullscreen(self, fullscreen);
}

<*
 Get the key that marks the window as should-close while held.

 @return "The exit key, or NULL if none is set"
*>
fn GfxKey GfxWindow.get_exit_key(&self)
{
	return self.internal.exit_key;
}

<*
 Set a key that marks the window as should-close while held.

 @param key : "The key to use, or NULL to disable"
*>
fn void GfxWindow.set_exit_key(&self, GfxKey key)
{
	self.internal.exit_key = key;
}

<*
 Replace the set of event types the window will report.

 @param events : "Mask of event types to enable; all others are disabled"
*>
fn void GfxWindow.set_enabled_events(&self, GfxEventMask events)
{
	self.internal.enabled_events = events;
}

<*
 Get the set of event types the window currently reports.

 @return "The enabled event mask"
*>
fn GfxEventMask GfxWindow.get_enabled_events(&self)
{
	return self.internal.enabled_events;
}

<*
 Enable every event type except the given ones.

 @param events : "Mask of event types to suppress"
*>
fn void GfxWindow.set_disabled_events(&self, GfxEventMask events)
{
	self.internal.enabled_events = ALL_EVENTS & ~events;
}


<*
 Resize the window by its monitor's content scale, for crisp sizing on HiDPI displays.
*>
fn void GfxWindow.scale_to_monitor(&self)
{
	(void)self.resize((GfxSize)(_window_find_monitor(self).scale * (Vec2)self.size));
}

<*
 Move the window onto a monitor, keeping its position relative to that monitor's origin.

 @param [in] m : "The target monitor"
 @require m != null
*>
fn void GfxWindow.move_to_monitor(&self, GfxMonitor* m)
{
	self.move(m.origin + self.origin);
}

<*
 Constrain how small the user may resize the window.

 @param size : "Minimum content size; zero for no limit"
*>
fn void GfxWindow.set_min_size(&self, GfxSize size)
{
	_window_set_min_size(self, size) @inline;
}

<*
 Constrain how large the user may resize the window.

 @param size : "Maximum content size; zero for no limit"
*>
fn void GfxWindow.set_max_size(&self, GfxSize size)
{
	_window_set_max_size(self, size) @inline;
}

<*
 Show or hide the window's title bar and frame.

 @param border : "True for a bordered window, false for borderless"
*>
fn void GfxWindow.set_border(&self, bool border)
{
	_window_set_border(self, border);
}

<*
 Get the user pointer previously attached to the window.

 @return "The stored pointer, or null if none was set"
*>
fn void* GfxWindow.get_user_ptr(&self)
{
	return self.user_ptr;
}

<*
 Attach an arbitrary pointer to the window, for use in callbacks.

 @param [in] ptr : "The pointer to store; gfx never dereferences it"
*>
fn void GfxWindow.set_user_ptr(&self, void* ptr)
{
	self.user_ptr = ptr;
}

<*
 Get the window size in framebuffer pixels rather than screen coordinates.

 On a HiDPI display this is larger than get_size().

 @return "The size in pixels, or { -1, -1 } if no monitor could be determined"
*>
fn GfxPoint GfxWindow.get_size_in_pixels(&self)
{
	return (GfxPoint)((Vec2)self.size * _window_find_monitor(self).pixel_ratio) ?? { -1, -1 };
}

<*
 Check whether the mouse left this window during the current frame.

 @return "True if a mouse-leave was recorded for this window"
*>
fn bool GfxWindow.did_mouse_leave(&self)
{
	return _gfx.window_state.win_leave == self && _gfx.window_state.mouse_leave;
}

<*
 Check whether the mouse entered this window during the current frame.

 @return "True if a mouse-enter was recorded for this window"
*>
fn bool GfxWindow.did_mouse_enter(&self)
{
	return _gfx.window_state.win == self && _gfx.window_state.mouse_enter;
}

<*
 Check whether the mouse is currently over the window.

 @return "True if the pointer is inside the window"
*>
fn bool GfxWindow.is_mouse_inside(&self)
{
	return self.internal.mouse_inside;
}

<*
 Set the window title.

 @param [in] name : "The new title"
*>
fn void GfxWindow.set_name(&self, String name)
{
	_window_set_name(self, name);
}

<*
 Let mouse input fall through the window to whatever is behind it.

 @param passthrough : "True to make the window transparent to the mouse"
*>
fn void GfxWindow.set_mouse_passthrough(&self, bool passthrough)
{
	_window_set_mouse_passthrough(self, passthrough);
}

<*
 Check whether a drag-and-drop operation is currently hovering this window.

 @return "True if data is being dragged over the window"
*>
fn bool GfxWindow.is_data_dragging(&self)
{
	return _gfx.window_state.win == self && _gfx.window_state.data_dragging;
}

<*
 Check whether a drag-and-drop payload was dropped on this window.

 @return "True if a drop is available from get_data_drop()"
*>
fn bool GfxWindow.did_data_drop(&self)
{
	return self.get_data_drop() != null;
}

<*
 Get the position of an in-progress drag over this window.

 @return "The drag position in window coordinates"
 @return? NO_DATA_DRAG : "If nothing is being dragged over this window"
*>
fn GfxPoint? GfxWindow.get_data_drag(&self)
{
	if (!self.is_data_dragging()) return NO_DATA_DRAG~;
	return { _gfx.window_state.drop.x, _gfx.window_state.drop.y };
}

<*
 Apply a new set of window property flags, updating only what changed.

 @param flags : "The desired flags"
*>
fn void GfxWindow.set_flags(&win, GfxWindowFlags flags)
{
	_window_set_flags(win, flags, win.internal.flags);
}

<*
 Check whether the window currently holds input focus.

 @return "True if the window is focused"
*>
fn bool GfxWindow.has_focus(&win) => win.internal.in_focus;


<*
 Completely release a window
*>
fn void GfxWindow.destroy(&self)
{
	_window_release(self);
}

<*
 Toggles building the drag-and-drop (DND) linked list
 @param allow : "set to true to allow DND building"
*>
fn void set_build_dnd(bool allow)
{
	_gfx.dnd_build = allow;
}

<*
 Check whether a key went down during the current frame.

 Unlike is_key_down() this is only true on the transition, so it fires once per press.

 @param key : "The key to test"
 @param [in] window : "Only report if this window has focus, or null for any window"
 @return "True if the key is down now and was up on the previous poll"
*>
fn bool is_key_pressed(GfxKey key, GfxWindow* window = null)
{
	return _gfx.keyboard[key].current && !_gfx.keyboard[key].prev && (!window || window.has_focus());
}

<*
 Check whether a key is currently held.

 @param key : "The key to test"
 @param [in] window : "Only report if this window has focus, or null for any window"
 @return "True if the key is down"
*>
fn bool is_key_down(GfxKey key, GfxWindow* window = null)
{
	return _gfx.keyboard[key].current && (!window || window.has_focus());
}

<*
 Check whether a key came up during the current frame.

 @param key : "The key to test"
 @param [in] window : "Only report if this window has focus, or null for any window"
 @return "True if the key is up now and was down on the previous poll"
*>
fn bool is_key_released(GfxKey key, GfxWindow* window = null)
{
	return !_gfx.keyboard[key].current && _gfx.keyboard[key].prev && (!window || window.has_focus());
}

<*
 Check whether a mouse button is currently held.

 @param button : "The button to test"
 @param [in] window : "Only report if this window has focus, or null for any window"
 @return "True if the button is down"
*>
fn bool is_mouse_down(GfxMouseButton button, GfxWindow* window = null)
{
	return _gfx.mouse_buttons[(sz)button].current && (!window || window.has_focus());
}

<*
 Check whether a mouse button came up during the current frame.

 @param button : "The button to test"
 @param [in] window : "Only report if this window has focus, or null for any window"
 @return "True if the button is up now and was down on the previous poll"
*>
fn bool is_mouse_released(GfxMouseButton button, GfxWindow* window = null)
{
	return !_gfx.mouse_buttons[button].current && _gfx.mouse_buttons[button].prev && (!window || window.has_focus());
}

<*
 Get the scroll wheel movement accumulated during the current frame.

 @return "Scroll delta, reset on each poll"
*>
fn Vec2 get_mouse_scroll() => _gfx.scroll;
<*
 Get the raw mouse movement accumulated during the current frame.

 @return "Movement delta, reset on each poll"
*>
fn Vec2 get_mouse_delta() => _gfx.vector;



<*
 Install a callback for one event type.

 @param type : "The event type to handle"
 @param func : "The callback, or null to remove the current one"
 @return "The callback that was previously installed, or null if there was none"
*>
fn GfxGenericFn set_event_callback(GfxEventType type, GfxGenericFn func)
{
	GfxGenericFn old = _gfx.callbacks[type];
	_gfx.callbacks[type] = func;
	return old;
}

<*
 - Runs synchronously on the UI/event thread.
 - May run while poll() or wait() is still on the stack.
 - Is not queued or affected by event masks.
 - Receives a damage hint in defined coordinates.
 - Sees the latest window size.
 - Does not make a graphics context current automatically.
 - May draw/present, but must return promptly.
 - Must not destroy the window, recursively pump events, or perform potentially reentrant native mutations.
*>
fn GfxRepaintFn GfxWindow.set_repaint_callback(&self, GfxRepaintFn repaint_fn)
{
	GfxRepaintFn old = self.repaint_fn;
	self.repaint_fn = repaint_fn;
	return old;
}

<*
 Install one callback for an event type and the type that follows it.

 Used for paired events such as press/release, so a single handler covers both.

 @param type : "The first event type; the next ordinal is also set"
 @param func : "The callback to install for both"
 @param [out] first : "Receives the previous callback for type, may be null"
 @param [out] second : "Receives the previous callback for the following type, may be null"
*>
fn void set_dual_event_callback(GfxEventType type, GfxGenericFn func, GfxGenericFn* first, GfxGenericFn* second)
{
	GfxGenericFn func1 = set_event_callback(type, func);
	GfxGenericFn func2 = set_event_callback((GfxEventType)(type.ordinal + 1), func);
	if (first != null) *first = func1;
	if (second != null) *second = func2;
}

<*
 Block until an event arrives, the timeout expires, or wake() is called.

 If an event was already queued this returns immediately without blocking. When the
 wait ends because a platform event arrived, that event is dispatched before
 returning, so it is available here rather than only on the next poll().

 Returns nothing when the timeout expires, when the wait was interrupted by
 wake(), or when the arriving event was filtered out by the receiving
 window's enabled events. All three are ordinary outcomes, not errors.

 Blocking is not possible while the platform owns the loop -- see poll().

 @param duration : "How long to wait; WAIT_FOREVER to block indefinitely"
 @return "The event that ended the wait"
 @return? NO_MORE_ELEMENT : "On timeout, on interruption, or if the event was filtered"
*>
fn GfxEvent? wait(Duration duration)
{
	return _wait(duration);
}

<*
 Create a mouse cursor from raw pixel data.

 @param [in] data : "Cursor image, size.x * size.y pixels in the given format"
 @param size : "Cursor dimensions in pixels"
 @param format : "Pixel format of the image data"
 @return "The new cursor, to be released with free_mouse()"
 @return? IMAGE_COULD_NOT_LOAD : "If the data is null or the cursor could not be built"
*>
fn GfxMouse*? create_mouse(void* data, GfxSize size, GfxFormat format)
{
	return _create_mouse(data, size, format);
}


<*
 Install one callback for every event type.

 @param func : "The callback to install"
 @param [out] callbacks : "Receives the callbacks being replaced, may be null"
*>
fn void set_all_event_callback(GfxGenericFn func, GfxEventCallbacks* callbacks)
{
	foreach (i : GfxEventType::values[1..])
	{
		if (callbacks != null) callbacks.[i] = _gfx.callbacks[i];
		set_event_callback(i, func);
	}
}


<*
 Discard every event still in the queue.
*>
fn void event_queue_flush()
{
	_gfx.event_len = 0;
}

<*
 Append an event to the queue.

 Does nothing when queueing is disabled. Superseded move and resize events for the
 same window are removed before appending the newest one. If the queue is full the
 new event is dropped, with an error logged.

 @param [in] event : "The event to copy into the queue"
 @require event != null
*>
fn void event_queue_push(GfxEvent* event)
{
	if (!_gfx.queue_events) return;
	// Like SDL, discard an older superseded geometry event before appending the
	// replacement. Appending keeps its ordering relative to unrelated events.
	if ((event.type == WINDOW_RESIZED || event.type == WINDOW_MOVED || event.type == DATA_DRAG) && _gfx.event_len)
	{
		for (int i = 0; i < _gfx.event_len; i++)
		{
			int idx = (_gfx.event_bottom + i) % MAX_EVENTS;
			if (_gfx.events[idx].type != event.type || _gfx.events[idx].common.win != event.common.win) continue;
			for (int j = i; j < _gfx.event_len - 1; j++)
			{
				int to = (_gfx.event_bottom + j) % MAX_EVENTS;
				int from = (_gfx.event_bottom + j + 1) % MAX_EVENTS;
				_gfx.events[to] = _gfx.events[from];
			}
			_gfx.event_len--;
			break;
		}
	}
	if (_gfx.event_len >= MAX_EVENTS)
	{
		if (!_gfx.event_error_reported)
		{
			log::error("Event queue limit 'MAX_EVENTS' has been reached, dropping events.", category: log::CATEGORY_VIDEO);
			_gfx.event_error_reported = true;
		}
		return;
	}
	int event_top = (_gfx.event_bottom + _gfx.event_len) % MAX_EVENTS;
	_gfx.event_len += 1;
	_gfx.events[event_top] = *event;
}

<*
 Remove and return the oldest event in the queue.

 @return "The oldest queued event"
 @return? NO_MORE_ELEMENT : "If the queue is empty"
*>
fn GfxEvent? event_queue_pop()
{
	if (!_gfx.event_len) return NO_MORE_ELEMENT~;
	GfxEvent* ev = &_gfx.events[_gfx.event_bottom];
	if (_gfx.event_len-- == 1) _gfx.event_error_reported = false;

	_gfx.event_bottom = (_gfx.event_bottom + 1) % MAX_EVENTS;
	return *ev;
}


<*
 Put data on the system clipboard.

 @param [in] data : "The payload to publish"
 @require data != null
*>
fn void? write_clipboard(GfxDataTransfer* data) @maydiscard
{
	return _write_clipboard(data) @inline;
}

<*
 Read data of a given type from the system clipboard.

 @param allocator : "Allocator for the returned payload"
 @param type : "The data type to request"
 @return "The clipboard contents"
*>
fn GfxDataTransfer? read_clipboard(Allocator allocator, GfxDataTransferType type)
{
	return _read_clipboard(allocator, type) @inline;
}

<*
 Get the next event for any window, pumping the platform first if the queue is empty.

 This is the normal driver for an event loop; it both pumps the platform and drains
 the queue. The platform is pumped at most once per drain, so looping on this until
 it yields nothing costs a single pump.

 While the user drags or resizes a window, or opens a menu, the platform runs its own
 loop inside this call and it does not return until that finishes. wake()
 cannot interrupt that. To keep drawing during it, install a set_repaint_callback callback,
 which is invoked from inside the platform loop.

 If a window is given it instead picks next queued event for that window. Events belonging
 to other windows are kept, not discarded; they are rotated to the back of the
 queue and remain available to those windows.

 @param [in] window : "Optionally only get events pertaining to a particular window"
 @return "The next event"
 @return? NO_MORE_ELEMENT : "If no event is pending"
*>
fn GfxEvent? poll(GfxWindow* window = null)
{
	if (!_gfx.event_len && !_gfx.polled_events)
	{
		_gfx.queue_events = true;
		_poll_events_platform();
		_gfx.polled_events = true;
	}
	defer catch _gfx.polled_events = false;
	return peep(window);
}

<*
 Get the next queued event without pumping the platform.

 Use this to drain what is already queued without giving the platform a chance to
 deliver more; poll() is the usual choice.

 If a window is given it instead picks next queued event for that window. Events belonging
 to other windows are kept, not discarded; they are rotated to the back of the
 queue and remain available to those windows.

 @param [in] window : "Optionally the window to filter on"
 @return "The next event"
 @return? NO_MORE_ELEMENT : "If the queue is empty"
*>
fn GfxEvent? peep(GfxWindow* window = null)
{
	_gfx.queue_events = true;
	return window ? _window_event_queue_pop(window) : event_queue_pop();
}

<*
 Translate a platform keycode into a GfxKey.

 @param keycode : "The platform-specific scancode"
 @return "The matching key, or NULL if the code is unmapped"
*>
fn GfxKey key_from_api(uint keycode)
{
	if (keycode >= _gfx.keycodes.len) return NULL;
	return _gfx.keycodes[keycode];
}

<*
 Translate a GfxKey into the platform keycode it came from.

 @return "The platform-specific scancode"
*>
fn uint GfxKey.to_api(self)
{
	return _gfx.api_keycodes[self];
}

<*
 Translate a physical key into the key the current keyboard layout produces.

 On layouts such as AZERTY or Dvorak this differs from the physical key. Platforms
 without a layout API return the key unchanged.

 @return "The layout-mapped key"
*>
fn GfxKey GfxKey.to_mapped(self)
{
	return _physical_to_mapped_key(self);
}

<*
 Clear all recorded keyboard state, as if every key were released.
*>
fn void reset_key()
{
	_gfx.keyboard = {};
}


<*
 Enable or disable queueing of events.

 With queueing off, events reach callbacks only and check_event() returns nothing.

 @param queue : "True to queue events for polling"
*>
fn void set_queue_events(bool queue)
{
	_gfx.queue_events = queue;
}


<*
 Get the payload of a completed drag-and-drop on this window.

 @return "The head of the dropped-item list, or null if this window has no drop"
*>
fn GfxDataDropNode* GfxWindow.get_data_drop(&self)
{
	if (_gfx.window_state.win != self || !_gfx.window_state.data_drop) return null;
	return _gfx.dnd_root;
}


<*
 Set the window that receives context-wide callbacks such as monitor changes.

 @param [in] win : "The window to use as root"
*>
fn void set_root_window(GfxWindow* win)
{
	_gfx.root = win;
}

<*
 Get the window receiving context-wide callbacks.

 @return "The root window, or null if none is set"
*>
fn GfxWindow* get_root_window()
{
	return _gfx.root;
}

<*
 Find the monitor the window is currently on.

 @return "The monitor containing the window"
 @return? NOT_FOUND : "If no monitor could be determined"
*>
fn GfxMonitor*? GfxWindow.find_monitor(&self)
{
	return _window_find_monitor(self);
}

<*
 Centre the window on the monitor it currently occupies.
*>
fn void GfxWindow.center_on_screen(&self)
{
	GfxMonitor*? mon = _window_find_monitor(self);
	if (catch mon) return;
	GfxRect workarea = mon.get_workarea();
	(void)self.move(workarea.origin + (workarea.size - self.size) / 2);
}

<*
 Change the monitor's mode to match the size of a window.

 The window is moved to the monitor origin afterwards.

 @param [in] win : "The window whose size to match"
 @require win != null
*>
fn void? GfxMonitor.scale_to_window(&self, GfxWindow* win)
{
	GfxMonitorMode mode;
	mode.size = win.size;
	defer win.move(self.origin);
	return _monitor_request_mode(self, &mode, SCALE);
}

<*
 Set the monitor to a mode obtained from get_modes().

 @param [in] mode : "The mode to apply"
 @require mode != null
*>
fn void? GfxMonitor.set_mode_native(&self, GfxMonitorMode* mode)
{
	return _monitor_set_mode(self, mode);
}

<*
 Change the monitor mode, matching only the properties named in the request.

 @param [in] mode : "The desired mode"
 @param request : "Which of size, refresh rate and colour depth must match"
 @require mode != null
*>
fn void? GfxMonitor.request_mode(&self, GfxMonitorMode* mode, GfxMonitorModeRequest request)
{
	return _monitor_request_mode(self, mode, request);
}

<*
 Get the platform image backing the surface.

 @return "The native image handle"
 @ensure return != null
*>
fn GfxNativeImage* GfxSurface.get_native_image(&self)
{
	return &self.native;
}

<*
 Install an accelerated pixel conversion routine for this surface.

 @param func : "The converter, or null to use the generic path"
*>
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

<*
 Compare two monitor modes, testing only the properties named in the request.

 @param [in] mon2 : "The mode to compare against"
 @param request : "Which of size, refresh rate and colour depth to compare"
 @return "True if every requested property matches"
 @require mon2 != null
*>
fn bool GfxMonitorMode.equals_request(&self, GfxMonitorMode* mon2, GfxMonitorModeRequest request)
{
	return (self.size == mon2.size || !(request & SCALE)) &&
	((self.refresh_rate == mon2.refresh_rate) || !(request & REFRESH)) &&
	((self.rgb == mon2.rgb) || !(request & RGB));
}

<*
 Drop monitors marked as disconnected, firing the monitor callback for each.
*>
fn void refresh_monitors()
{
	GfxMonitorNode* prev = null;
	GfxMonitorNode* node = _gfx.monitors.list.head;
	while (node)
	{
		GfxMonitorNode* next = node.next;
		if (node.disconnected)
		{
			_monitor_callback(_gfx.root, &node.mon, false);
			_remove_monitor(node, prev);
		}
		else
		{
			prev = node;
		}
		node = next;
	}
}

<*
 Release the entire monitor list without firing callbacks.
*>
fn void free_monitors()
{
	GfxMonitorNode* node = _gfx.monitors.list.head;
	while (node)
	{
		GfxMonitorNode* next = node.next;
		_remove_monitor(node, null);
		node = next;
	}
}

<*
 Get the mouse position in screen coordinates, independent of any window.

 @return "The global cursor position"
*>
fn GfxPoint get_global_mouse()
{
	return _get_global_mouse();
}

<*
 Get every connected monitor.

 The returned slice is temp-allocated and valid until the next temp reset.

 @return "All connected monitors, empty if none are known"
*>
fn GfxMonitor*[] get_monitors()
{
	sz count = _gfx.monitors.count;
	if (!count) return {};
	GfxMonitor*[] monitors = mem::temp_array(GfxMonitor*, count);
	sz i = 0;
	GfxMonitorNode* cur_node = _gfx.monitors.list.head;
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
	if (!_gfx.monitors.primary) _gfx.monitors.primary = _gfx.monitors.list.head;
	if (!_gfx.monitors.primary) return NOT_FOUND~;
	return &_gfx.monitors.primary.mon;
}

<*
 Get every display mode the monitor supports.

 @param allocator : "Allocator for the returned slice"
 @return "The supported modes"
*>
fn GfxMonitorMode[] GfxMonitor.get_modes(&monitor, Allocator allocator)
{
	return alloc::clone_slice(allocator, _monitor_get_modes(monitor));
}

<*
 Get the monitor area not covered by OS furniture such as a taskbar.

 @return "The usable rectangle in screen coordinates"
*>
fn GfxRect GfxMonitor.get_workarea(&monitor)
{
	return _monitor_get_workarea(monitor);
}

<*
 Find the supported mode closest to a desired one.

 Modes are scored on size, colour depth and refresh rate, in that order of importance.

 @param mode : "The desired mode"
 @return "The best available match"
 @return? NOT_FOUND : "If the monitor reports no usable mode"
*>
fn GfxMonitorMode? GfxMonitor.get_best_matching_mode(&monitor, GfxMonitorMode mode) => @pool()
{
	GfxMonitorMode* chosen;
	int top_score = 1;
	foreach (&mode2 : _monitor_get_modes(monitor))
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


<*
 Get the human-readable monitor name.

 @return "The monitor name as reported by the OS"
*>
fn String GfxMonitor.get_name(&self)
{
	return ((ZString)&self.name[0]).str_view();
}

<*
 Attach an arbitrary pointer to the monitor, for use in callbacks.

 @param [in] user_ptr : "The pointer to store; gfx never dereferences it"
*>
fn void GfxMonitor.set_user_ptr(&self, void* user_ptr)
{
	self.user_ptr = user_ptr;
}

<*
 Get the user pointer previously attached to the monitor.

 @return "The stored pointer, or null if none was set"
*>
fn void* GfxMonitor.get_user_ptr(&self)
{
	return self.user_ptr;
}

<*
 Get the monitor's current display mode.

 @return "The active mode"
*>
fn GfxMonitorMode GfxMonitor.get_mode(&self)
{
	return self.mode;
}

<*
 Set the gamma of the monitor.

 @param gamma : "The gamma exponent, greater than zero. 1.0 is identity, 2.2 is standard sRGB."

 @require [gamma] gamma > 0.0 : "Gamma must be greater than zero"
*>
fn void? GfxMonitor.set_gamma(&self, float gamma) => @pool()
{
	return _monitor_set_gamma(self, gamma);
}

<*
 Upload an explicit gamma ramp to the monitor.

 Use set_gamma() instead to derive a ramp from a single exponent.

 @param ramp : "Per-channel ramps; all three must be the same length"
*>
fn void? GfxMonitor.set_gamma_ramp(&self, GfxGammaRamp ramp)
{
	return _monitor_set_gamma_ramp(self, ramp);
}

<*
 Read the monitor's current gamma ramp.

 @param allocator : "Allocator for the returned ramp"
 @return "The current ramp, to be released with free()"
*>
fn GfxGammaRamp? GfxMonitor.get_gamma_ramp(&self, Allocator allocator)
{
	return _monitor_get_gamma_ramp(self, allocator);
}

<*
 Release the memory held by a gamma ramp and clear it.

 Safe to call on an already-freed or zeroed ramp.
*>
fn void GfxGammaRamp.free(&self)
{
	if (self.red.ptr) alloc::free(self.a, self.red.ptr);
	*self = {};
}

<*
 Check whether the window confines the mouse to its bounds.

 @return "True if the mouse is captured"
*>
fn bool GfxWindow.will_capture_mouse(&self) => self.internal.capture_mouse;

<*
 Check whether the window receives unaccelerated mouse motion.

 @return "True if raw mouse mode is on"
*>
fn bool GfxWindow.is_raw_mouse(&self) => self.internal.raw_mouse;

<*
 Check whether the window is hidden.

 @return "True if the window is not currently shown"
*>
fn bool GfxWindow.is_hidden(&self)
{
	return _window_is_hidden(self);
}

<*
 Draw a surface into the window.

 @param [in] surface : "The surface to present"
 @require surface != null
*>
fn void GfxWindow.blit_surface(&self, GfxSurface* surface)
{
	_window_blit_surface(self, surface);
}

<*
 Check whether the window is maximized.

 @return "True if the window is maximized"
*>
fn bool GfxWindow.is_maximized(&self)
{
	return _window_is_maximized(self);
}

<*
 Check whether the window is kept above other windows.

 @return "True if the window is floating"
*>
fn bool GfxWindow.is_floating(&self)
{
	return _window_is_floating(self);
}

<*
 Set the window icon from raw pixel data.

 @param [in] data_src : "Icon image, w * h pixels in the given format"
 @param w : "Icon width in pixels"
 @param h : "Icon height in pixels"
 @param format : "Pixel format of the image data"
 @param type : "Where the icon applies: the window, the taskbar, or both"
 @return "True if the platform accepted the icon"
 @require w >= 0 && h >= 0 : "Dimensions may not be negative"
*>
fn bool GfxWindow.set_icon(&self, void* data_src, int w, int h, GfxFormat format, GfxWindowIconType type = BOTH)
{
	return _window_set_icon(self, data_src, w, h, format, type);
}

<*
 Check whether the window is minimized.

 On some platforms this only becomes true once the minimize animation has started.

 @return "True if the window is minimized"
*>
fn bool GfxWindow.is_minimized(&self)
{
	return _window_is_minimized(self);
}

<*
 Confine the mouse to the window, or release it.

 @param capture : "True to capture the mouse"
*>
fn void GfxWindow.set_capture_mouse(&self, bool capture)
{
	self.internal.capture_mouse = capture;
	_window_set_capture_mouse_platform(self, capture);
}

<*
 Lock the window to an aspect ratio while the user resizes it.

 @param w : "Width part of the ratio; zero to remove the constraint"
 @param h : "Height part of the ratio; zero to remove the constraint"
*>
macro void GfxWindow.set_aspect_ratio(&self, int w, int h)
{
	_window_set_aspect_ratio(self, w, h);
}

<*
 Enable unaccelerated mouse motion, suitable for camera control.

 @param enabled : "True for raw motion, false for the OS-accelerated pointer"
*>
fn void GfxWindow.set_raw_mouse_mode(&self, bool enabled)
{
	self.internal.raw_mouse = enabled;
	_window_set_raw_mouse_mode_platform(self, enabled);
}

<*
 Capture the mouse and switch to raw motion in one call.

 @param enabled : "True to capture and use raw motion, false to undo both"
*>
fn void GfxWindow.set_capture_raw_mouse(&self, bool enabled)
{
	self.set_capture_mouse(enabled);
	self.set_raw_mouse_mode(enabled);
}

<*
 Bring the window to the front of the stacking order without focusing it.
*>
fn void GfxWindow.raise(&self)
{
	return _window_raise(self);
}

<*
 Warp the mouse to a position inside the window.

 @param where : "Target position in window coordinates"
*>
fn void GfxWindow.move_mouse(&self, GfxPoint where)
{
	return _window_move_mouse(self, where);
}

<*
 Check whether the cursor is hidden over this window.

 @return "True if the cursor is hidden"
*>
fn bool GfxWindow.is_mouse_hidden(&self)
{
	return self.internal.flags.hide_mouse;
}

<*
 Set or clear a modifier in the window's recorded keyboard state.

 @param mod : "The modifier to change"
 @param enable : "True to set the modifier, false to clear it"
*>
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

<*
 Get the pixel format surfaces use natively on this platform.

 Blitting a surface in this format avoids a conversion pass.

 @return "RGBA8 on wasm, BGRA8 elsewhere"
*>
fn GfxFormat native_format()
{
	return $feat(WASM) ? RGBA8 : BGRA8;
}

<*
 Check whether the window is drawn without a title bar and frame.

 @return "True if the window is borderless"
*>
fn bool GfxWindow.is_borderless(&win)
{
	return win.internal.flags.no_border;
}
<*
 Check whether the window is fullscreen.

 @return "True if the window is fullscreen"
*>
fn bool GfxWindow.is_fullscreen(&win)
{
	return win.internal.flags.fullscreen;
}
<*
 Check whether the window accepts dropped files and data.

 @return "True if drag-and-drop is enabled"
*>
fn bool GfxWindow.allows_dnd(&win)
{
	return win.internal.flags.allow_dnd;
}


<*
 Restore the ordinary arrow cursor over this window.
*>
fn void GfxWindow.set_mouse_default(&self)
{
	self.set_mouse_standard(NORMAL);
}

<*
 Set the cursor to one of the standard system shapes.

 @param icon : "The cursor shape to use"
*>
fn void GfxWindow.set_mouse_standard(&self, GfxMouseIcon icon)
{
	self.set_mouse(_gfx.standard_mice[(sz)icon]);
}

<*
 Set the cursor shown over this window.

 Passing null does nothing. The hidden cursor is applied without being recorded, so a
 later show_mouse() restores the previous shape.

 @param [in] mouse : "The cursor to use"
*>
fn void GfxWindow.set_mouse(&self, GfxMouse* mouse)
{
	if (!mouse) return;
	if (mouse != _gfx.hidden_mouse) self.internal.mouse = mouse;
	_window_set_mouse_platform(self, mouse);
}

<*
 Release a cursor created with create_mouse().
*>
fn void GfxMouse.free_mouse(&self)
{
	return _free_mouse(self);
}

<*
 Interrupt a wait() in progress, which then returns with no event.

 Safe to call when nothing is waiting, in which case the next wait()
 returns immediately.

 This has no effect on a poll() that has entered a platform modal loop, such as an
 interactive resize or an open menu: that loop belongs to the window system and
 cannot be cancelled. Draw from a set_repaint_callback callback instead.
*>
fn void wake()
{
	_wake();
}

<*
 Enable unaccelerated mouse motion for the root window.

 @param enabled : "True for raw motion, false for the OS-accelerated pointer"
*>
fn void set_raw_mouse_mode(bool enabled)
{
	_gfx.raw_mouse = enabled;
	if (_gfx.root) _window_set_raw_mouse_mode_platform(_gfx.root, enabled);
}


<*
 Hide the window without destroying it.
*>
fn void GfxWindow.hide(&self)
{
	_window_hide(self);
}

<*
 Show a hidden window, restoring focus and maximized state if those flags are set.
*>
fn void GfxWindow.show(&self)
{
	if (self.internal.flags.focus_on_show) _window_focus(self);
	self.internal.flags.hide = false;
	_window_show_platform(self);
	if (self.internal.flags.maximize && !_window_is_maximized(self)) self.maximize();
}

<*
 Return the window to the size and position it had before being maximized or minimized.
*>
fn void GfxWindow.restore(&self)
{
	_window_restore_platform(self);
	self.internal.flags.maximize = false;
	self.show();
	$if !$feat(MACOS):
		self.move(self.internal.old_bounds.origin);
		self.resize(self.internal.old_bounds.size);
	$endif
}

<*
 Minimize the window to the taskbar or dock.
*>
fn void GfxWindow.minimize(&self)
{
	_window_minimize_platform(self);
	self.internal.flags.maximize = false;
}

<*
 Query the platform for the window's real size and update the cached value.

 Use this when the window may have been resized outside gfx.

 @return "The current size"
*>
fn GfxPoint GfxWindow.fetch_size(&self)
{
	return _window_fetch_size(self);
}

<*
 Enable or disable specific event types for the window.

 @param event : "Mask of event types to change"
 @param state : "True to enable the given types, false to disable them"
*>
fn void GfxWindow.set_event_state(&self, GfxEventMask event, bool state)
{
	if (state)
	{
		self.internal.enabled_events |= event;
	}
	else
	{
		self.internal.enabled_events &= ~event;
	}
}

<*
 Maximize the window, remembering its current bounds for restore().
*>
fn void GfxWindow.maximize(&self)
{
	self.internal.old_bounds = self.bounds;
	self.internal.flags.maximize = true;
	_window_fetch_size(self);
	_window_maximize_platform(self);
}

<*
 Move the window to a screen position, clearing its maximized state.

 @param where : "New position of the top-left corner"
*>
fn void GfxWindow.move(&self, GfxPoint where)
{
	self.origin = where;
	_window_move_platform(self, where);
	self.internal.flags.maximize = false;
}

<*
 Resize the window's content area, clearing its maximized state.

 @param size : "New content size"
*>
fn void GfxWindow.resize(&self, GfxSize size)
{
	self.size = size;
	_window_resize_platform(self, size);
	self.internal.flags.maximize = false;
}

<*
 Show or hide the cursor over this window.

 @param show : "True to show the cursor, false to hide it"
*>
fn void GfxWindow.show_mouse(&self, bool show)
{
	_window_show_mouse_native(self, show);
}

<*
 Create an OpenGL context and attach it to the window.

 The context is owned by the window and released when it is destroyed, so calling
 destroy_opengl_context() is only needed to drop it early.

 @param [in] hints : "Context attributes; use get_global_opengl_hints() for the defaults"
 @return "The new context"
 @require hints != null
*>
fn GfxGlContext*? GfxWindow.create_opengl_context(&self, GfxGlHints* hints)
{
	GfxGlContext* context = mem::new(GfxGlContext);
	defer catch free(context);
	_window_init_opengl_context_platform(self, context, hints)!;
	self.src.backing.owned = true;
	return context;
}

<*
 Release an OpenGL context attached to the window.

 @param [in] context : "The context to release"
*>
fn void GfxWindow.destroy_opengl_context(&self, GfxGlContext* context)
{
	_window_destroy_opengl_context(self, context);
}

<*
 Release an EGL context attached to the window.

 @param [in] context : "The context to release"
*>
fn void GfxWindow.destroy_egl_context(&self, GfxEglContext* context)
{
	egl::destroy_context_in_window(context, self);
}

<*
 Create an EGL context and attach it to the window.

 @param [in] hints : "Context attributes; use get_global_opengl_hints() for the defaults"
 @return "The new context"
 @require hints != null
*>
fn GfxEglContext*? GfxWindow.create_egl_context(&self, GfxGlHints* hints)
{
	return egl::create_context(self, hints);
}

<*
 Enable or disable drag-and-drop for the window.

 @param allow : "True to accept dropped files and data"
*>
fn void GfxWindow.set_dnd(&self, bool allow)
{
	_window_set_dnd(self, allow);
}

<*
 Look up an OpenGL function by name.

 Requires OpenGL to have been loaded, normally via init() with .init_opengl set.

 @param [in] s : "The function name"
 @return "The function pointer, or null if it is unavailable"
*>
fn void* get_opengl_proc(ZString s)
{
	return _get_opengl_proc(s);
}

<*
 Find an egl function.

 @param [in] name : "The name of the function to load"

 @return "the function pointer"

 @ensure return != null
*>
fn void*? find_egl_proc(ZString name)
{
	return egl::find_proc(name);
}

<*
 Get the hints used for contexts created without explicit ones.

 The returned pointer is mutable, so fields may be edited in place.

 @return "The global OpenGL hints"
 @ensure return != null
*>
fn GfxGlHints* get_global_opengl_hints()
{
	return &_global_hints_open_gl;
}

<*
 Replace the hints used for contexts created without explicit ones.

 @param [in] hints : "The hints to copy in"
 @require hints != null
*>
fn void set_global_opengl_hints(GfxGlHints* hints)
{
	_global_hints_open_gl = *hints;
}

<*
 Restore the global OpenGL hints to their built-in defaults.
*>
fn void reset_global_opengl_hints()
{
	_global_hints_open_gl = GLOBAL_HINTS;
}


<*
 Load the EGL library and resolve its entry points.

 Called by init() when .init_egl is set; only needed directly to defer the load.
*>
fn void? load_egl()
{
	return egl::load_lib() @inline;
}

<*
 Unload the EGL library and clear its entry points.
*>
fn void unload_egl()
{
	egl::unload_lib() @inline;
}

<*
 Load the OpenGL library and resolve its entry points.

 Called by init() when .init_opengl is set; only needed directly to defer the load.
*>
fn void? load_opengl()
{
	return _load_opengl() @inline;
}

<*
 Unload the OpenGL library and clear its entry points.
*>
fn void unload_opengl()
{
	_unload_opengl() @inline;
}

<*
 Load the Vulkan loader and resolve vkGetInstanceProcAddr.

 Called by init() when .init_vulkan is set; only needed directly to defer the load.
*>
fn void? load_vulkan()
{
	return _load_vulkan_native() @inline;
}

<*
 Unload the Vulkan loader and clear its entry points.
*>
fn void unload_vulkan()
{
	_unload_vulkan_native() @inline;
}
