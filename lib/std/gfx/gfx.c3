// Copyright (c) 2026 Christoffer Lernö. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::gfx @feat(MACOS, LINUX, BSD, WASM, WIN32);
import std::math, std::os, std::os::darwin, libc, std::time;
import std::gfx::x11 @public, std::gfx::egl @public;
import std::gfx::wl @public;
import std::gfx::macos @public;
import std::gfx::win32 @public;

// The GFX module provides basic window handling across multiple platforms.
// This implementation was strongly based on RGFW 2.0 by Riley Mabb

excuse
{
	NO_DATA_DRAG,
	GFX_CONTEXT_NOT_INITIALIZED,
	SURFACE_CREATION_FAILED,
	WAYLAND_DISPLAY_NOT_LOADED,
	WAYLAND_COMPOSITOR_NOT_FOUND,
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
}


const Duration WAIT_FOREVER = Duration::max;

alias GfxFn = fn void();
alias GfxGenericFunc = fn void(GfxEvent* e);
alias GfxConvertImageDataFn = fn void(char* dest_data, char* src_data, GfxColorLayout* srcLayout, GfxColorLayout* destLayout, sz count);
alias GfxPoint = int[<2>];
alias GfxSize = int[<2>];
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
	float pixelRatio;
	Vec2 phys_size;
	GfxMonitorMode mode;
	void* user_ptr;
	GfxMonitorNode* node;
}

struct GfxWindow
{
	inline IntRect bounds;
	GfxWindowSrc src;
	GfxWindowInternal internal;
	void* user_ptr;
}

struct GfxDataTransfer
{
	String text;
	GfxDataTransferType type;
}

struct GfxDataDropNode
{
	char* data;
	sz length;
	GfxDataTransferType type;
	GfxDataDropNode* next;
}

struct GfxWindowSrc @feat(WASM)
{
	inline IntRect    bounds;
	void*             window;
	void*             view;
	void*             mouse;
	void*             delegate;
	GfxBackingContext ctx;
	GfxBacking        backing;
}


struct GfxWindowSrc @feat(MACOS)
{
	inline IntRect    bounds;
	ObjId             window;
	ObjId             view;
	ObjId             mouse;
	ObjId             delegate;
	GfxBackingContext ctx;
	GfxBacking        backing;
}

struct GfxWindowSrc @feat(WIN32)
{
	inline IntRect      bounds;
	Win32_HWND          window;
	Win32_HDC           hdc;
	Win32_HICON         hIconSmall;
	Win32_HICON         hIconBig;
	GfxPoint            min_size;
	GfxPoint            max_size;
	int                 aspectRatioW, aspectRatioH;
	bool                actionFrame;
	ushort              highSurrogate;
	GfxBackingContext   ctx;
	GfxBacking          backing;
}

struct GfxWindowSrc @feat(BSD, LINUX)
{
	inline IntRect bounds;
	GfxBackingContext ctx;
	GfxBacking backing;
	struct @feat(!NO_X11)
	{
		XWindow window;
		XWindow parent;
		GContext gc;
		XlibIC ic;
		Clock flashEnd;
		long counter_value;
		XSyncCounter counter;
	}
	struct @feat(WAYLAND)
	{
		WlProxy* surface;
		WlProxy* xdgSurface;
		WlProxy* xdgToplevel;
		WlProxy* decoration;
		WlProxy* locked_pointer;
		WlProxy* icon;
		WlProxy* dataSource;
		WlProxy* customCursorSurface;
		GfxMonitorNode* activeMonitor;
		uint decorationMode;
		bool pendingActivated;
		bool activated;
		bool resizing;
		bool pendingMaximized;
		bool maximized;
		bool minimized;
		bool configured;
		bool usingCustomCursor;
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

struct GfxEventCallbacks
{
	GfxGenericFunc[25] arr;
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
	bool window_refresh        : 15;
	bool window_close          : 16;
	bool window_maximized      : 17;
	bool window_minimized      : 18;
	bool window_restored       : 19;
	bool data_drop             : 20;
	bool data_drag             : 21;
	bool scale_updated         : 22;
	bool monitor_connected     : 23;
	bool monitor_disconnected  : 24;
}

const GfxEventMask KEY_EVENTS            = { .key_pressed, .key_released, .key_char };
const GfxEventMask MOUSE_EVENTS          = { .mouse_button_pressed, .mouse_button_released, .mouse_scroll, .mouse_motion, .mouse_raw_motion, .mouse_enter, .mouse_leave };
const GfxEventMask WINDOW_EVENTS         = { .window_moved, .window_resized, .window_refresh, .window_maximized, .window_minimized, .window_restored, .scale_updated };
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

fn void copy_image_data(char* dest_data, int w, int h, GfxFormat dest_format, char* src_data, GfxFormat src_format, GfxConvertImageDataFn func)
{
	copy_image_data_64(dest_data, w, h, dest_format, src_data, src_format, false, func);
}

<*
 @require !_gfx.initialized : "Already initialized"
 @require class_name.len < 254 : "Class name is too long"
*>
fn void? init(String class_name, GfxInitOption options)
{
	class_name = class_name ?: "C3GFX";
	_gfx = { .initialized = true };
	_gfx.windows.init(mem);
	defer catch
	{
		_destroy_native();
		_gfx = {};
	}
	if (options & INIT_OPEN_GL) load_opengl()!;
	if (options & INIT_EGL) load_egl()!;
	if (options & INIT_VULKAN) load_vulkan()!;
	_init_keycodes();
	_init_native(class_name, options)!;
	foreach (icon : GfxMouseIcon::values)
	{
		_gfx.standardMice[icon.ordinal] = _create_standard_mouse(icon) ?? null;
	}
	_poll_monitors();
	log::info("Global context initialized", category: log::CATEGORY_VIDEO);
}

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
		if (_gfx.standardMice[icon.ordinal]) _free_mouse(_gfx.standardMice[icon.ordinal]);
	}
	free_monitors();
	log::info("Global context destroyed", category: log::CATEGORY_VIDEO);
	_destroy_native();
	unload_egl();
	unload_opengl();
	_gfx.root = null;
	_gfx = {};
}

fn void poll_monitors()
{
	_poll_monitors();
}

fn GfxSurface*? create_surface(char* data, int w, int h, GfxFormat format, GfxWindow* window = null)
{
	GfxSurface* s = mem::new(GfxSurface);
	defer catch free(s);
	s.init(data, w, h, format, window: window)!;
	return s;
}

fn void GfxSurface.release(&self)
{
	_surface_release_native(self);
}
fn void? GfxSurface.init(&self, void* data, int w, int h, GfxFormat format, GfxWindow* window = null)
{
	return _surface_init_native(self, data, w, h, format, window);
}

// public
fn GfxWindow*? create_window(String name, int x, int y, int w, int h, GfxWindowFlags flags)
{
	return _create_window(name, x, y, w, h, flags);
}

fn bool is_mouse_pressed(GfxMouseButton button) => _gfx.mouseButtons[(sz)button].current && !_gfx.mouseButtons[(sz)button].prev;

fn void GfxWindow.flash(&self, GfxFlashRequest request) => _window_flash_native(self, request);

fn void GfxWindow.set_floating(&self, bool floating)
{
	return _window_set_floating_native(self, floating);
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

fn void GfxWindow.focus(&self)
{
	_window_focus_native(self);
}

fn void GfxWindow.set_should_close(&win, bool should_close)
{
	if (!should_close)
	{
		win.internal.should_close = false;
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

fn VkSurfaceKHR? GfxWindow.create_vulkan_surface(&self, VkInstance instance)
{
	return _window_create_vulkan_surface_native(self, instance);
}

fn WGPUSurface GfxWindow.create_wgpu_surface(&self, WGPUInstance instance)
{
	return _window_create_wgpu_surface_native(self, instance);
}

fn void GfxWindow.set_opengl_swap_interval(&self, int swap_interval)
{
	_window_set_opengl_swap_interval(self, swap_interval);
}

fn void GfxWindow.set_egl_swap_interval(&self, int swap_interval)
{
	egl::set_swap_interval(self, swap_interval);
}

fn void? GfxWindow.create_directx_swap_chain(&self, Win32_IDXGIFactory* factory, void* device, void** swapchain) @feat(WIN32)
{
	return _window_create_directx_swap_chain_native(self, factory, device, swapchain);
}

fn void GfxWindow.swap_opengl_buffers(&self)
{
	_window_opengl_swap_buffers_native(self);
}

fn void GfxWindow.swap_egl_buffers(&self)
{
	egl::swap_window_buffers(self);
}

fn void set_current_opengl_window(GfxWindow* win)
{
	_window_make_opengl_current_context_native(win);
}

fn void set_current_egl_window(GfxWindow* win)
{
	egl::set_current_window(win);
}

fn void* get_current_egl_context()
{
	return egl::get_current_context();
}

fn void* get_current_opengl_context()
{
	return _get_current_opengl_context_native();
}

fn bool opengl_extension_is_supported(String extension)
{
	if (_opengl_extension_supported_base(extension, &_get_opengl_proc_native)) return true;
	return _opengl_extension_is_supported_native(extension);
}

fn bool egl_extension_is_supported(String extension)
{
	return egl::is_extension_supported(extension);
}

fn GfxGlContext* GfxWindow.get_opengl_context(&self)
{
	if (!self.src.backing.opengl) return null;
	return self.src.ctx.native;
}


fn GfxEglContext* GfxWindow.get_egl_context(&self)
{
	if (!self.src.backing.egl) return null;
	return self.src.ctx.egl;
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

fn void GfxWindow.set_enabled_events(&self, GfxEventMask events)
{
	self.internal.enabled_events = events;
}

fn GfxEventMask GfxWindow.get_enabled_events(&self)
{
	return self.internal.enabled_events;
}

fn void GfxWindow.set_disabled_events(&self, GfxEventMask events)
{
	self.internal.enabled_events = ALL_EVENTS & ~events;
}


fn void GfxWindow.scale_to_monitor(&self)
{
	(void)self.resize((GfxSize)(_window_find_monitor_native(self).scale * (Vec2)self.size));
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

fn void* GfxWindow.get_user_ptr(&self)
{
	return self.user_ptr;
}

fn void GfxWindow.set_user_ptr(&self, void* ptr)
{
	self.user_ptr = ptr;
}

fn GfxPoint GfxWindow.get_size_in_pixels(&self)
{
	return (GfxPoint)((Vec2)self.size * _window_find_monitor_native(self).pixelRatio) ?? { -1, -1 };
}

fn bool GfxWindow.did_mouse_leave(&self)
{
	return _gfx.window_state.win_leave == self && _gfx.window_state.mouse_leave;
}

fn bool GfxWindow.did_mouse_enter(&self)
{
	return _gfx.window_state.win == self && _gfx.window_state.mouse_enter;
}

fn bool GfxWindow.is_mouse_inside(&self)
{
	return self.internal.mouse_inside;
}

fn void GfxWindow.set_name(&self, String name)
{
	_window_set_name_native(self, name);
}

fn void GfxWindow.set_mouse_passthrough(&self, bool passthrough)
{
	_window_set_mouse_passthrough_native(self, passthrough);
}

fn bool GfxWindow.is_data_dragging(&self)
{
	return _gfx.window_state.win == self && _gfx.window_state.dataDragging;
}

fn bool GfxWindow.did_data_drop(&self)
{
	return self.get_data_drop() != null;
}

fn GfxPoint? GfxWindow.get_data_drag(&self)
{
	if (!self.is_data_dragging()) return NO_DATA_DRAG~;
	return { _gfx.window_state.drop.x, _gfx.window_state.drop.y };
}

fn void GfxWindow.set_flags(&win, GfxWindowFlags flags)
{
	_window_set_flags(win, flags, win.internal.flags);
}

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

// public
fn bool is_key_pressed(GfxKey key, GfxWindow* window = null)
{
	return _gfx.keyboard[key].current && !_gfx.keyboard[key].prev && (!window || window.has_focus());
}
// public
fn bool is_key_down(GfxKey key, GfxWindow* window = null)
{
	return _gfx.keyboard[key].current && (!window || window.has_focus());
}
// public
fn bool is_key_released(GfxKey key, GfxWindow* window = null)
{
	return !_gfx.keyboard[key].current && _gfx.keyboard[key].prev && (!window || window.has_focus());
}
// public
fn bool is_mouse_down(GfxMouseButton button, GfxWindow* window = null)
{
	return _gfx.mouseButtons[(sz)button].current && (!window || window.has_focus());
}

// public
fn bool is_mouse_released(GfxMouseButton button, GfxWindow* window = null)
{
	return !_gfx.mouseButtons[button].current && _gfx.mouseButtons[button].prev && (!window || window.has_focus());
}

fn Vec2 get_mouse_scroll() => _gfx.scroll;
fn Vec2 get_mouse_delta() => _gfx.vector;



fn GfxGenericFunc set_event_callback(GfxEventType type, GfxGenericFunc func)
{
	GfxGenericFunc old = _gfx.callbacks[type];
	_gfx.callbacks[type] = func;
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

fn void wait_for_event(Duration duration)
{
	_wait_for_event_native(duration);
}

fn GfxMouse*? create_mouse(void* data, GfxSize size, GfxFormat format)
{
	return _create_mouse(data, size, format);
}


// public
fn void set_all_event_callback(GfxGenericFunc func, GfxEventCallbacks* callbacks)
{
	foreach (i : GfxEventType::values[1..])
	{
		if (callbacks != null) callbacks.arr[i.ordinal] = _gfx.callbacks[i.ordinal];
		set_event_callback(i, func);
	}
}


fn void event_queue_flush()
{
	_gfx.eventLen = 0;
}

fn void event_queue_push(GfxEvent* event)
{
	if (!_gfx.queueEvents) return;
	if (_gfx.eventLen >= MAX_EVENTS)
	{
		log::error("Event queue limit 'MAX_EVENTS' has been reached automatically flushing queue.", category: log::CATEGORY_VIDEO);
		event_queue_flush();
		return;
	}
	int eventTop = (_gfx.eventBottom + _gfx.eventLen) % MAX_EVENTS;
	_gfx.eventLen += 1;
	_gfx.events[eventTop] = *event;
}

// public
fn GfxEvent? event_queue_pop()
{
	if (!_gfx.eventLen) return NO_MORE_ELEMENT~;
	GfxEvent* ev = &_gfx.events[_gfx.eventBottom];
	_gfx.eventLen--;
	_gfx.eventBottom = (_gfx.eventBottom + 1) % MAX_EVENTS;
	return *ev;
}


fn void? write_clipboard(GfxDataTransfer* data) @maydiscard
{
	return _write_clipboard_native(data) @inline;
}

fn GfxDataTransfer? read_clipboard(Allocator allocator, GfxDataTransferType type)
{
	return _read_clipboard_native(allocator, type) @inline;
}

// public
fn GfxEvent? check_event()
{
	if (!_gfx.eventLen && !_gfx.polledEvents)
	{
		_gfx.queueEvents = true;
		_poll_events_native();
		_gfx.polledEvents = true;
	}
	defer catch _gfx.polledEvents = false;
	return check_queued_event();
}

// public
fn GfxEvent? check_queued_event()
{
	_gfx.queueEvents = true;
	return event_queue_pop();
}

// public
fn GfxEvent? window_event_queue_pop(GfxWindow* win)
{
	GfxEvent ev = event_queue_pop()!;
	for (int i = 1; i < _gfx.eventLen && ev.common.win != win && ev.common.win != null; i++)
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
	_gfx.queueEvents = true;
	return window_event_queue_pop(win);
}

// public
fn GfxEvent? check_window_event(GfxWindow* win)
{
	if (!_gfx.eventLen && !_gfx.polledEvents)
	{
		_gfx.queueEvents = true;
		_poll_events_native();
		_gfx.polledEvents = true;
	}
	defer catch _gfx.polledEvents = false;
	return check_queued_window_event(win);
}


fn GfxKey key_from_api(uint keycode)
{
	if (keycode >= _gfx.keycodes.len) return NULL;
	return _gfx.keycodes[keycode];
}

fn uint GfxKey.to_api(self)
{
	return _gfx.apiKeycodes[self];
}

fn void reset_key()
{
	_gfx.keyboard = {};
}


fn void set_queue_events(bool queue)
{
	_gfx.queueEvents = queue;
}


fn GfxDataDropNode* GfxWindow.get_data_drop(&self)
{
	if (_gfx.window_state.win != self || !_gfx.window_state.dataDrop) return null;
	return _gfx.dndRoot;
}


fn void set_root_window(GfxWindow* win)
{
	_gfx.root = win;
}

fn GfxWindow* get_root_window()
{
	return _gfx.root;
}

fn GfxMonitor*? GfxWindow.find_monitor(&self)
{
	return _window_find_monitor_native(self);
}

fn void GfxWindow.center_on_screen(&self)
{
	GfxMonitor*? mon = _window_find_monitor_native(self);
	(void)self.move(mon.origin + (mon.mode.size - self.size) / 2);
}

fn void? GfxMonitor.scale_to_window(&self, GfxWindow* win)
{
	GfxMonitorMode mode;
	mode.size = win.size;
	defer win.move(self.origin);
	return _monitor_request_mode(self, &mode, SCALE);
}

fn void? GfxMonitor.set_mode_native(&self, GfxMonitorMode* mode)
{
	return _monitor_set_mode(self, mode);
}

fn void? GfxMonitor.request_mode(&self, GfxMonitorMode* mode, GfxMonitorModeRequest request)
{
	return _monitor_request_mode(self, mode, request);
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

fn GfxPoint get_global_mouse()
{
	return _get_global_mouse();
}

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

// public
fn GfxMonitorMode[] GfxMonitor.get_modes(&monitor, Allocator allocator)
{
	return alloc::clone_slice(allocator, _monitor_get_modes(monitor));
}

fn IntRect GfxMonitor.get_workarea(&monitor)
{
	return _monitor_get_workarea(monitor);
}
// public
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

fn void? GfxMonitor.set_gamma(&self, float gamma) => @pool()
{
	return _monitor_set_gamma(self, gamma);
}

fn void? GfxMonitor.set_gamma_ramp(&self, GfxGammaRamp ramp)
{
	return _monitor_set_gamma_ramp(self, ramp);
}

fn GfxGammaRamp? GfxMonitor.get_gamma_ramp(&self, Allocator allocator)
{
	return _monitor_get_gamma_ramp(self, allocator);
}

fn void GfxGammaRamp.free(&self)
{
	if (self.red.ptr) alloc::free(self.a, self.red.ptr);
	*self = {};
}

fn bool GfxWindow.will_capture_mouse(&self) => self.internal.capture_mouse;

fn bool GfxWindow.is_raw_mouse(&self) => self.internal.raw_mouse;

fn bool GfxWindow.is_hidden(&self)
{
	return _window_is_hidden_native(self);
}

fn void GfxWindow.blit_surface(&self, GfxSurface* surface)
{
	_window_blit_surface_native(self, surface);
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
	return self.set_mouse(_gfx.standardMice[(sz)icon]);
}

fn bool GfxWindow.set_mouse(&self, GfxMouse* mouse)
{
	if (!mouse) return false;
	if (mouse != _gfx.hiddenMouse) self.internal.mouse = mouse;
	return _window_set_mouse_native(self, mouse);
}

fn void GfxMouse.free_mouse(&self)
{
	return _free_mouse(self);
}

fn void stop_check_events()
{
	_stop_check_events_native();
}
fn void set_raw_mouse_mode(bool enabled)
{
	_gfx.raw_mouse = enabled;
	if (_gfx.root) _window_set_raw_mouse_mode_native(_gfx.root, enabled);
}


fn void GfxWindow.hide(&self)
{
	_window_hide_native(self);
}

fn void GfxWindow.show(&self)
{
	if (self.internal.flags.focus_on_show) _window_focus_native(self);
	self.internal.flags.hide = false;
	_window_show(self);
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
	_window_minimize_native(self);
	self.internal.flags.maximize = false;
}

fn GfxPoint GfxWindow.fetch_size(&self)
{
	return _window_fetch_size_native(self);
}

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

fn GfxGlContext*? GfxWindow.create_opengl_context(&self, GfxGlHints* hints)
{
	return _window_create_opengl_context_native(self, hints);
}

fn void GfxWindow.destroy_opengl_context(&self, GfxGlContext* context)
{
	_window_destroy_opengl_context_native(self, context);
}

fn void GfxWindow.destroy_egl_context(&self, GfxEglContext* context)
{
	egl::destroy_context_in_window(context, self);
}

fn GfxEglContext*? GfxWindow.create_egl_context(&self, GfxGlHints* hints)
{
	return egl::create_context(self, hints);
}

fn void GfxWindow.set_dnd(&self, bool allow)
{
	self.internal.flags.allow_dnd = allow;
	$if $feat(WIN32):
		win32::dragAcceptFiles(self.src.window, (int)allow);
	$endif
}

fn void* get_opengl_proc(ZString s)
{
	return _get_opengl_proc_native(s);
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

fn GfxGlHints* get_global_opengl_hints()
{
	return &_global_hints_open_gl;
}

fn void set_global_opengl_hints(GfxGlHints* hints)
{
	_global_hints_open_gl = *hints;
}

fn void reset_global_opengl_hints()
{
	_global_hints_open_gl = GLOBAL_HINTS;
}


fn void? load_egl()
{
	return egl::load_lib() @inline;
}

fn void unload_egl()
{
	egl::unload_lib() @inline;
}

fn void? load_opengl()
{
	$switch:
		$case $feat(MACOS):
			return macos::_load_opengl_macos();
		$case $feat(WIN32):
			return win32::_load_opengl_win32();
		$case $feat(WASM):
			return;
		$case WAYLAND_ONLY:
			return wl::_load_opengl_wayland();
		$case X11_ONLY:
			return x11::_load_opengl_x11();
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland)
			{
				return wl::_load_opengl_wayland();
			}
			return x11::_load_opengl_x11();
		$default:
			return NOT_IMPLEMENTED~;
	$endswitch
}

fn void unload_opengl()
{
	$if !$feat(MACOS):
		_gfx.gl.native_handle.close();
	$else
		_gfx.gl.native_handle = null;
	$endif
	_gfx.gl.get_proc_addr_fn = null;
}

fn void? load_vulkan()
{
	return _load_vulkan_native() @inline;
}

fn void unload_vulkan()
{
	_unload_vulkan_native() @inline;
}
