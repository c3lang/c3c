module std::gfx @feat(LINUX, BSD) @feat(!NO_X11) @private;
import std::math, std::collections, std::os, libc, std::time, std::time::clock, std::net, std::net::os;
import std::gfx::x11 @public;

const int X_TRUE = 1;
const int X_FALSE = 0;
const int NONE = 0;
const int CURRENT_TIME = 0;
const uint XKB_USE_CORE_KBD = 0x0100;
const char SHIFT_MASK = 1 << 0;
const char LOCK_MASK = 1 << 1;
const char MOD2_MASK = 1 << 4;
const char MOD3_MASK = 1 << 5;

// core GLX 1.0 attributes
const GLX_USE_GL           = 1;
const GLX_BUFFER_SIZE      = 2;
const GLX_LEVEL            = 3;
const GLX_RGBA             = 4;
const GLX_DOUBLEBUFFER     = 5;
const GLX_STEREO           = 6;
const GLX_AUX_BUFFERS      = 7;
const GLX_RED_SIZE         = 8;
const GLX_GREEN_SIZE       = 9;
const GLX_BLUE_SIZE        = 10;
const GLX_ALPHA_SIZE       = 11;
const GLX_DEPTH_SIZE       = 12;
const GLX_STENCIL_SIZE     = 13;
const GLX_ACCUM_RED_SIZE   = 14;
const GLX_ACCUM_GREEN_SIZE = 15;
const GLX_ACCUM_BLUE_SIZE  = 16;
const GLX_ACCUM_ALPHA_SIZE = 17;

// GLX 1.3 FBConfig attributes
const GLX_CONFIG_CAVEAT     = 0x20;
const GLX_X_VISUAL_TYPE     = 0x22;
const GLX_DRAWABLE_TYPE     = 0x8010;
const GLX_RENDER_TYPE       = 0x8011;
const GLX_X_RENDERABLE      = 0x8012;
const GLX_FBCONFIG_ID       = 0x8013;
const GLX_RGBA_TYPE         = 0x8014;
const GLX_COLOR_INDEX_TYPE  = 0x8015;

// GLX_X_VISUAL_TYPE values
const GLX_NONE         = 0x8000;
const GLX_TRUE_COLOR   = 0x8002;
const GLX_DIRECT_COLOR = 0x8003;
const GLX_PSEUDO_COLOR = 0x8004;
const GLX_STATIC_COLOR = 0x8005;
const GLX_GRAY_SCALE   = 0x8006;
const GLX_STATIC_GRAY  = 0x8007;

// GLX_DRAWABLE_TYPE / GLX_RENDER_TYPE bits
const GLX_RGBA_BIT        = 0x00000001;
const GLX_COLOR_INDEX_BIT = 0x00000002;
const GLX_WINDOW_BIT      = 0x00000001;
const GLX_PIXMAP_BIT      = 0x00000002;
const GLX_PBUFFER_BIT     = 0x00000004;

// GLX_ARB_multisample
const GLX_SAMPLE_BUFFERS = 100000;
const GLX_SAMPLES        = 100001;

// GLX_ARB_create_context and friends
const GLX_CONTEXT_MAJOR_VERSION_ARB             = 0x2091;
const GLX_CONTEXT_MINOR_VERSION_ARB             = 0x2092;
const GLX_CONTEXT_FLAGS_ARB                     = 0x2094;
const GLX_CONTEXT_PROFILE_MASK_ARB              = 0x9126;
const GLX_CONTEXT_CORE_PROFILE_BIT_ARB          = 0x00000001;
const GLX_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB = 0x00000002;
const GLX_CONTEXT_DEBUG_BIT_ARB                 = 0x00000001;
const GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB    = 0x00000002;
const GLX_CONTEXT_ES_PROFILE_BIT_EXT            = 0x00000004;
const GLX_CONTEXT_ROBUST_ACCESS_BIT_ARB         = 0x00000004;
const GLX_CONTEXT_RELEASE_BEHAVIOR_ARB          = 0x2097;
const GLX_CONTEXT_RELEASE_BEHAVIOR_NONE_ARB     = 0x0;
const GLX_CONTEXT_RELEASE_BEHAVIOR_FLUSH_ARB    = 0x2098;
const GLX_CONTEXT_OPENGL_NO_ERROR_ARB           = 0x31B3;

// GLX_ARB_framebuffer_sRGB / GLX_EXT_framebuffer_sRGB — same value, different extension namespace
const GLX_FRAMEBUFFER_SRGB_CAPABLE_ARB = 0x20B2;
const GLX_FRAMEBUFFER_SRGB_CAPABLE_EXT = 0x20B2;

fn void _surface_release_x11(GfxSurface* surface)
{
	if (!surface.data) return;
	if (surface.native.bitmap)
	{
		((XImageHeader*)surface.native.bitmap).data = null;
		x11::xDestroyImage(surface.native.bitmap);
	}
	if (surface.native.buffer) free(surface.native.buffer);
	*surface = {};
}

fn void _window_set_border_x11(GfxWindow* win, bool border)
{
	win.internal.flags.no_border = !border;
	X11WindowHints hints;
	hints.flags = 2;
	hints.decorations = (CULong)border;
	x11::xChangeProperty(_gfx_ctx.display, win.src.window, _gfx_ctx.motifWmHints, _gfx_ctx.motifWmHints, 32, 0, (char*)&hints, 5);
	if (!_window_is_hidden_x11(win))
	{
		win.hide();
		win.show();
	}
}

fn void _window_set_raw_mouse_mode_x11(GfxWindow* win, bool state)
{
	char[4] mask;
	if (state) xi_set_mask((char*)&mask, 6 /* XI_RawMotion */);
	XIEventMask em;
	em.deviceid = 1; // XIAllMasterDevices
	em.mask_len = 4;
	em.mask = (char*)&mask;
	if (x11::xi_select_events_fn) x11::xi_select_events_fn(_gfx_ctx.display, x11::xDefaultRootWindow(_gfx_ctx.display), &em, 1);
}

fn void _window_capture_mouse_x11(GfxWindow* win, bool state)
{
	if (state)
	{
		uint eventMask = (1 << 2) | (1 << 3) | (1 << 6); // Button press/release + pointer motion
		x11::xGrabPointer(_gfx_ctx.display, win.src.window, X_TRUE, eventMask, 1, 1, win.src.window, 0, 0);
	}
	else
	{
		x11::xUngrabPointer(_gfx_ctx.display, 0);
	}
}

fn WGPUSurface _window_create_wgpu_surface_x11(GfxWindow* window, WGPUInstance instance)
{
	WgpuSurfaceSourceXlibWindow fromXlib;
	fromXlib.chain.next = null;
	fromXlib.chain.sType = WGPU_STYPE_SURFACE_SOURCE_XLIB_WINDOW;
	fromXlib.display = _gfx_ctx.display;
	fromXlib.window = window.src.window;
	WgpuSurfaceDescriptor surfaceDesc;
	surfaceDesc.nextInChain = (WgpuChainedStruct*)&fromXlib;
	return wgpuInstanceCreateSurface(instance, &surfaceDesc);
}

// public
fn void _window_set_aspect_ratio_x11(GfxWindow* win, int w, int h)
{
	if (w == 0 && h == 0) return;
	XSizeHints hints;
	CLong flags;
	x11::xGetWMNormalHints(_gfx_ctx.display, win.src.window, &hints, &flags);
	hints.flags |= (1 << 6); // PAspect
	hints.min_aspect.x = w; hints.max_aspect.x = w;
	hints.min_aspect.y = h; hints.max_aspect.y = h;
	x11::xSetWMNormalHints(_gfx_ctx.display, win.src.window, &hints);
}

// public
fn void _window_blit_surface_x11(GfxWindow* win, GfxSurface* surface)
{
	GfxPoint min = math::min(win.size, surface.size);
	copy_image_data(surface.native.buffer, surface.size.x, min.y, surface.native.format, surface.data, surface.format, surface.convert_fn);
	x11::xPutImage(_gfx_ctx.display, win.src.window, win.src.gc, surface.native.bitmap, 0, 0, 0, 0, (uint)min.x, (uint)min.y);
}

fn GfxPoint _get_global_mouse_x11()
{
	int x, y, fX, fY;
	uint z;
	XWindow window1, window2;
	x11::xQueryPointer(_gfx_ctx.display, x11::xDefaultRootWindow(_gfx_ctx.display), &window1, &window2, &fX, &fY, &x, &y, &z);
	return { fX, fY };
}

// public
fn GfxPoint _window_fetch_size_x11(GfxWindow* win)
{
	XWindowAttributes attribs;
	x11::xGetWindowAttributes(_gfx_ctx.display, win.src.window, &attribs);
	win.size = { attribs.width, attribs.height };
	return win.get_size();
}

// public
fn void _window_set_mouse_passthrough_x11(GfxWindow* win, bool passthrough)
{
	if (passthrough)
	{
		Region region = x11::xCreateRegion();
		if (x11::xshape_combine_region_fn) x11::xshape_combine_region_fn(_gfx_ctx.display, win.src.window, 2, 0, 0, region, 0);
		x11::xDestroyRegion(region);
		return;
	}
	if (x11::xshape_combine_mask_fn) x11::xshape_combine_mask_fn(_gfx_ctx.display, win.src.window, 2, 0, 0, 0, 0);
}

fn GfxGammaRamp? _monitor_get_gamma_ramp_x11(GfxMonitor* monitor, Allocator a)
{
	sz size = (sz)x11::xRRGetCrtcGammaSize(_gfx_ctx.display, monitor.node.nativeId1);
	if (!size) return CANNOT_RETRIEVE_GAMMA~;
	XRRCrtcGamma* gamma = x11::xRRGetCrtcGamma(_gfx_ctx.display, monitor.node.nativeId1);
	if (!gamma) return CANNOT_RETRIEVE_GAMMA~;
	defer x11::xRRFreeGamma(gamma);
	ushort[] rgb = alloc::alloc_array(a, ushort, size * 3);
	ushort[] red = rgb[:size];
	ushort[] green = rgb[size:size];
	ushort[] blue = rgb[size * 2:size];
	red[:size] = gamma.red[:size];
	green[:size] = gamma.green[:size];
	blue[:size] = gamma.blue[:size];
	return { red, green, blue, a };
}

fn void? monitor_set_gamma_ramp_x11(GfxMonitor* monitor, GfxGammaRamp ramp)
{
	sz size = (sz)x11::xRRGetCrtcGammaSize(_gfx_ctx.display, monitor.node.nativeId1);
	if (size != ramp.red.len) return GAMMA_COULD_NOT_BE_SET~;

	XRRCrtcGamma* gamma = x11::xRRAllocGamma((int)size);
	if (!gamma) return GAMMA_COULD_NOT_BE_SET~;
	defer x11::xRRFreeGamma(gamma);

	gamma.red[:size] = ramp.red[..];
	gamma.green[:size] = ramp.green[..];
	gamma.blue[:size] = ramp.blue[..];
	x11::xRRSetCrtcGamma(_gfx_ctx.display, monitor.node.nativeId1, gamma);
}

fn void? monitor_set_gamma_x11(GfxMonitor* monitor, float gamma)
{
	sz size = (sz)x11::xRRGetCrtcGammaSize(_gfx_ctx.display, monitor.node.nativeId1);
	if (!size) return GAMMA_COULD_NOT_BE_SET~;

	XRRCrtcGamma* gamma_data = x11::xRRAllocGamma((int)size);
	defer x11::xRRFreeGamma(gamma_data);
	for (sz i = 0; i < size; i++)
	{
		ushort g = (ushort)_gamma_for_index(gamma, i, size);
		gamma_data.red[i] = g;
		gamma_data.green[i] = g;
		gamma_data.blue[i] = g;
	}
	x11::xRRSetCrtcGamma(_gfx_ctx.display, monitor.node.nativeId1, gamma_data);
}

fn GfxMouse*? _create_mouse_x11(char* data, GfxSize size, GfxFormat format)
{
	if (!x11::xcursor_image_create_fn) return IMAGE_COULD_NOT_LOAD~;
	XcursorImage* native = x11::xcursor_image_create_fn(...size);
	if (!native) return IMAGE_COULD_NOT_LOAD~;
	defer x11::xcursor_image_destroy_fn(native);
	sz bytesize = (sz)size.x * size.y * 4;
	native.xhot = 0;
	native.yhot = 0;
	mem::clear(native.pixels, bytesize);
	mem::copy(native.pixels, data, bytesize);
	XCursor cursor = x11::xcursor_image_load_cursor_fn(_gfx_ctx.display, native);
	if (!cursor) return MOUSE_NOT_SUPPORTED~;
	return (GfxMouse*)cursor;
}

fn GfxMouse*? _create_mouse_standard_x11(GfxMouseIcon mouse)
{
	uint mouseIcon = 0;
	switch (mouse)
	{
		case NORMAL: mouseIcon = 68; // XC_left_ptr
		case ARROW: mouseIcon = 68;
		case IBEAM: mouseIcon = 152; // XC_xterm
		case WAIT: mouseIcon = 150; // XC_watch
		case CROSSHAIR: mouseIcon = 130; // XC_tcross
		case PROGRESS: mouseIcon = 150;
		case RESIZE_NWSE: mouseIcon = 134;
		case RESIZE_NESW: mouseIcon = 136;
		case RESIZE_EW: mouseIcon = 108;
		case RESIZE_NS: mouseIcon = 116;
		case RESIZE_NW: mouseIcon = 134;
		case RESIZE_N: mouseIcon = 138;
		case RESIZE_NE: mouseIcon = 136;
		case RESIZE_E: mouseIcon = 96;
		case RESIZE_SE: mouseIcon = 14;
		case RESIZE_S: mouseIcon = 16;
		case RESIZE_SW: mouseIcon = 12;
		case RESIZE_W: mouseIcon = 70;
		case RESIZE_ALL: mouseIcon = 52; // XC_fleur
		case NOT_ALLOWED: mouseIcon = 88; // XC_pirate
		case POINTING_HAND: mouseIcon = 60; // XC_hand2
		default: return MOUSE_NOT_SUPPORTED~;
	}
	XCursor cursor = x11::xCreateFontCursor(_gfx_ctx.display, mouseIcon);
	if (!cursor) return MOUSE_NOT_SUPPORTED~;
	return (GfxMouse*)cursor;
}


fn void? _x_create_window(XVisualInfo visual, String name, GfxWindowFlags flags, GfxWindow* win)
{
	if (!name) name = "C3GFX";
	// KeyPress | KeyRelease | ButtonPress | ButtonRelease | PointerMotion | StructureNotify | FocusChange |
	// LeaveWindow | EnterWindow | Exposure | VisibilityChange | PropertyChange
	CLong eventMask = (1 << 0) | (1 << 1) | (1 << 2) | (1 << 3) | (1 << 6) | (1 << 17) | (1 << 21) |
	(1 << 5) | (1 << 4) | (1 << 15) | (1 << 16) | (1 << 22);

	XSetWindowAttributes swa;
	win.src.parent = x11::xDefaultRootWindow(_gfx_ctx.display);

	Colormap cmap = x11::xCreateColormap(_gfx_ctx.display, win.src.parent, visual.visual, 0);
	swa.colormap = cmap;
	swa.event_mask = eventMask;
	swa.background_pixmap = 0;

	win.src.window = x11::xCreateWindow(_gfx_ctx.display, win.src.parent, win.x, win.y, (uint)win.width, (uint)win.height,
	0, visual.depth, 1 /* InputOutput */, visual.visual, (1 << 3) | (1 << 13) | (1 << 11), &swa);
	// CWBorderPixel | CWColormap | CWEventMask
	if (!win.src.window) return X11_WINDOW_CREATION_FAILED~;

	win.src.flashEnd = {};
	x11::xFreeColors(_gfx_ctx.display, cmap, null, 0, 0);
	x11::xSaveContext(_gfx_ctx.display, win.src.window, _gfx_ctx.context, (XPointer)win);
	win.src.gc = x11::xCreateGC(_gfx_ctx.display, win.src.window, 0, null);

	if (_gfx_ctx.im != null)
	{
		XImCallback callback;
		callback.callback = &_x11_ic_callback;
		callback.client_data = (XPointer)win;

		win.src.ic = x11::xCreateIC(_gfx_ctx.im, "inputStyle", (CULong)(0x0008 | 0x0400) /* XIMPreeditNothing | XIMStatusNothing */,
		"clientWindow", win.src.window, "focusWindow", win.src.window,
		"destroyCallback", &callback, (char*)null);
	}

	@pool()
	{
		XClassHint hint;
		hint.res_name = name.zstr_copy(tmem);
		hint.res_class = _gfx_ctx.class_name.zstr_copy(tmem);
		x11::xSetClassHint(_gfx_ctx.display, win.src.window, &hint);
	};

	XWMHints hints;
	hints.flags = 1 << 1; // StateHint
	hints.initial_state = 1; // NormalState
	x11::xSetWMHints(_gfx_ctx.display, win.src.window, &hints);

	x11::xSelectInput(_gfx_ctx.display, win.src.window, eventMask);
	x11::xSetWMProtocols(_gfx_ctx.display, win.src.window, &_gfx_ctx.wmDeleteWindow, 1);
	win.set_name(name);
	x11::xMoveWindow(_gfx_ctx.display, win.src.window, win.x, win.y);

	if (flags.allow_dnd)
	{
		win.internal.flags.allow_dnd = true;
		char version = 5;
		x11::xChangeProperty(_gfx_ctx.display, win.src.window, _gfx_ctx.xdndAware, 4, 32, 0, &version, 1);
	}

	win.src.bounds = win.bounds;
	x11::xSetWindowBackground(_gfx_ctx.display, win.src.window, 0);
	x11::xClearWindow(_gfx_ctx.display, win.src.window);
	x11::xSetWindowBackgroundPixmap(_gfx_ctx.display, win.src.window, 0);
}

fn void? _create_window_x11(String name, GfxWindowFlags flags, GfxWindow* win)
{
	win.src.backing = {};
	if (flags.open_gl || flags.egl)
	{
		win.src.window = 0;
		if (flags.open_gl) _window_create_opengl_context_native(win, get_global_opengl_hints())!;
		if (flags.egl) _window_create_egl_context_native(win, get_global_opengl_hints())!;
	}
	else
	{
		XVisualInfo visual;
		_get_visual_for_window(&visual, win.internal.flags.transparent);
		_x_create_window(visual, name, flags, win)!;
	}
	_window_set_flags(win, flags, {});
}

fn void _destroy_x11()
{
	if (_gfx_ctx.display)
	{
		if (x11::xGetSelectionOwner(_gfx_ctx.display, _gfx_ctx.clipboardAtom) == _gfx_ctx.helper_window)
		{
			x11::xConvertSelection(_gfx_ctx.display, _gfx_ctx.clipboardManager, _gfx_ctx.saveTargets, 0, _gfx_ctx.helper_window, 0);
			while (x_handle_clipboard_selection_helper() != 0) {}
		}
		x11::xUnregisterIMInstantiateCallback(_gfx_ctx.display, null, null, null, (void*)&_x11_im_init_callback, null);
	}
	if (_gfx_ctx.im) x11::xCloseIM(_gfx_ctx.im);
	if (_gfx_ctx.unixClipboard) free(_gfx_ctx.unixClipboard);
	if (_gfx_ctx.hiddenMouse) _free_mouse(_gfx_ctx.hiddenMouse);
	if (_gfx_ctx.helper_window) x11::xDestroyWindow(_gfx_ctx.display, _gfx_ctx.helper_window);
	if (_gfx_ctx.display) x11::xCloseDisplay(_gfx_ctx.display);

	x11::dlclose_cursor_xi_ext();
}

// private
fn bool _opengl_extension_is_supported_x11(String extension)
{
	ZString extensions = _glx_query_extensions_string_fn(_gfx_ctx.display, x11::xDefaultScreen(_gfx_ctx.display));
	return extensions && _extension_is_in_list(extensions.str_view(), extension);
}

fn void _free_mouse_x11(GfxMouse* mouse)
{
	x11::xFreeCursor(_gfx_ctx.display, (XCursor)mouse);
}

// private
fn void* _get_current_opengl_context_x11() => _glx_get_current_context_fn();

// private
fn GfxFn _get_opengl_proc(ZString procname)
{
	if (!_gfx_ctx.glGetProcAddress) return null;
	return ((GlxGetProcAddressProc)_gfx_ctx.glGetProcAddress)(procname);
}

// private
fn void _init_keycodes_native_x11()
{
	// A full evdev-keycode -> _KEY table walk (xkbGetKeyboardByName) is left
	// out of this mechanical pass; keycodes[] stays zeroed (_KEY_NULL).
}

fn void? _init_x11(String class_name, GfxInitOption flags)
{
	_gfx_ctx.class_name = class_name;

	x11::xInitThreads();
	_gfx_ctx.display = x11::xOpenDisplay(null);
	if (!_gfx_ctx.display) return X11_DISPLAY_NOT_LOADED~;

	_gfx_ctx.netCurrentDesktop = x11::xInternAtom(_gfx_ctx.display, "_NET_CURRENT_DESKTOP", 0);
	_gfx_ctx.net_wm_state_maximized_vert = x11::xInternAtom(_gfx_ctx.display, "_NET_WM_STATE_MAXIMIZED_VERT", 0);
	_gfx_ctx.net_wm_state_maximized_horz = x11::xInternAtom(_gfx_ctx.display, "_NET_WM_STATE_MAXIMIZED_HORZ", 0);
	_gfx_ctx.netWmBypassCompositor = x11::xInternAtom(_gfx_ctx.display, "_NET_WM_BYPASS_COMPOSITOR", 0);
	_gfx_ctx.netWmState = x11::xInternAtom(_gfx_ctx.display, "_NET_WM_STATE", 0);
	_gfx_ctx.motifWmHints = x11::xInternAtom(_gfx_ctx.display, "_MOTIF_WM_HINTS", 0);
	_gfx_ctx.wmDeleteWindow = x11::xInternAtom(_gfx_ctx.display, "WM_DELETE_WINDOW", 0);
	_gfx_ctx.netWmSyncRequestCounter = x11::xInternAtom(_gfx_ctx.display, "_NET_WM_SYNC_REQUEST_COUNTER", 0);
	_gfx_ctx.netWmSyncRequest = x11::xInternAtom(_gfx_ctx.display, "_NET_WM_SYNC_REQUEST", 0);
	_gfx_ctx.atomPair = x11::xInternAtom(_gfx_ctx.display, "ATOM_PAIR", 0);
	_gfx_ctx.multiple = x11::xInternAtom(_gfx_ctx.display, "MULTIPLE", 0);
	_gfx_ctx.targets = x11::xInternAtom(_gfx_ctx.display, "TARGETS", 0);
	_gfx_ctx.saveTargets = x11::xInternAtom(_gfx_ctx.display, "SAVE_TARGETS", 0);
	_gfx_ctx.utf8String = x11::xInternAtom(_gfx_ctx.display, "UTF8_STRING", 0);
	_gfx_ctx.xdndTypeList = x11::xInternAtom(_gfx_ctx.display, "XdndTypeList", 0);
	_gfx_ctx.xdndSelection = x11::xInternAtom(_gfx_ctx.display, "XdndSelection", 0);
	_gfx_ctx.xdndAware = x11::xInternAtom(_gfx_ctx.display, "XdndAware", 0);
	_gfx_ctx.xdndEnter = x11::xInternAtom(_gfx_ctx.display, "XdndEnter", 0);
	_gfx_ctx.xdndPosition = x11::xInternAtom(_gfx_ctx.display, "XdndPosition", 0);
	_gfx_ctx.xdndStatus = x11::xInternAtom(_gfx_ctx.display, "XdndStatus", 0);
	_gfx_ctx.xdndLeave = x11::xInternAtom(_gfx_ctx.display, "XdndLeave", 0);
	_gfx_ctx.xdndDrop = x11::xInternAtom(_gfx_ctx.display, "XdndDrop", 0);
	_gfx_ctx.xdndFinished = x11::xInternAtom(_gfx_ctx.display, "XdndFinished", 0);
	_gfx_ctx.xdndActionCopy = x11::xInternAtom(_gfx_ctx.display, "XdndActionCopy", 0);
	_gfx_ctx.wmProtocols = x11::xInternAtom(_gfx_ctx.display, "WM_PROTOCOLS", 0);
	_gfx_ctx.wmState = x11::xInternAtom(_gfx_ctx.display, "WM_STATE", 0);
	_gfx_ctx.netWmStateFullscreen = x11::xInternAtom(_gfx_ctx.display, "_NET_WM_STATE_FULLSCREEN", 0);
	_gfx_ctx.netWmStateAbove = x11::xInternAtom(_gfx_ctx.display, "_NET_WM_STATE_ABOVE", 0);
	_gfx_ctx.netWmWindowOpacity = x11::xInternAtom(_gfx_ctx.display, "_NET_WM_WINDOW_OPACITY", 0);
	_gfx_ctx.netWmName = x11::xInternAtom(_gfx_ctx.display, "_NET_WM_NAME", 0);
	_gfx_ctx.netWmIcon = x11::xInternAtom(_gfx_ctx.display, "_NET_WM_ICON", 0);
	_gfx_ctx.xselData = x11::xInternAtom(_gfx_ctx.display, "XSEL_DATA", 0);
	_gfx_ctx.clipboardAtom = x11::xInternAtom(_gfx_ctx.display, "CLIPBOARD", 0);
	_gfx_ctx.clipboardManager = x11::xInternAtom(_gfx_ctx.display, "CLIPBOARD_MANAGER", 0);
	_gfx_ctx.netWorkarea = x11::xInternAtom(_gfx_ctx.display, "_NET_WORKAREA", 0);
	_gfx_ctx.xtextUriList = x11::xInternAtom(_gfx_ctx.display, "text/uri-list", 0);
	_gfx_ctx.xtextPlain = x11::xInternAtom(_gfx_ctx.display, "text/plain", 0);

	_gfx_ctx.context = x11::xUniqueContext();
	if (!_gfx_ctx.context) return X11_CTX_CREATION_FAILED~;

	XSetWindowAttributes wa;
	wa.event_mask = 1 << 22; // PropertyChangeMask
	_gfx_ctx.helper_window = x11::xCreateWindow(_gfx_ctx.display, x11::xDefaultRootWindow(_gfx_ctx.display), 0, 0, 1, 1, 0, 0,
		2 /* InputOnly */, x11::xDefaultVisual(_gfx_ctx.display, x11::xDefaultScreen(_gfx_ctx.display)), 1 << 11, &wa);
	if (_gfx_ctx.helper_window == 0) return X11_WINDOW_CREATION_FAILED~;

	x11::dload_cursor_xi_ext();

	char[4] blk;
	_gfx_ctx.hiddenMouse = _create_mouse_native((void*)&blk, { 1, 1 }, RGBA8) ?? null;
	_gfx_ctx.unixClipboard = null;

	x11::xSetErrorHandler(&_x_error_handler);
	x11::xSync(_gfx_ctx.display, 0);

	x11::xSetLocaleModifiers("");
	x11::xRegisterIMInstantiateCallback(_gfx_ctx.display, null, null, null, (void*)&_x11_im_init_callback, null);

	int errorBase;
	if (x11::xRRQueryExtension(_gfx_ctx.display, &_gfx_ctx.xrandrEventBase, &errorBase) != 0)
	{
		x11::xRRSelectInput(_gfx_ctx.display, x11::xRootWindow(_gfx_ctx.display, x11::xDefaultScreen(_gfx_ctx.display)), 1 << 4); // RROutputChangeNotifyMask
	}
}

// private
fn void? _load_opengl_x11()
{
	if (_gfx_ctx.native_gl_handle && _gfx_ctx.glGetProcAddress) return;
	_gfx_ctx.native_gl_handle = os::dlopen_any("libGLX.so.0", "libGL.so.1", "libGL.so") ?? MISSING_OPENGL_LIB~!;
	void* ptr = os::dlsym(_gfx_ctx.native_gl_handle, "glXGetProcAddress");
	if (!ptr) return MISSING_GL_GET_PROC_ADDR~;
	_gfx_ctx.glGetProcAddress        = ptr;
	_glx_choose_visual_fn                = os::dlsym(_gfx_ctx.native_gl_handle, "glXChooseVisual");
	_glx_create_context_fn               = os::dlsym(_gfx_ctx.native_gl_handle, "glXCreateContext");
	_glx_make_current_fn                 = os::dlsym(_gfx_ctx.native_gl_handle, "glXMakeCurrent");
	_glx_get_current_context_fn          = os::dlsym(_gfx_ctx.native_gl_handle, "glXGetCurrentContext");
	_glx_swap_buffers_fn                 = os::dlsym(_gfx_ctx.native_gl_handle, "glXSwapBuffers");
	_glx_get_visual_from_fb_config_fn    = os::dlsym(_gfx_ctx.native_gl_handle, "glXGetVisualFromFBConfig");
	_glx_get_fb_config_attrib_fn         = os::dlsym(_gfx_ctx.native_gl_handle, "glXGetFBConfigAttrib");
	_glx_get_proc_address_arb_fn         = os::dlsym(_gfx_ctx.native_gl_handle, "glXGetProcAddressARB");
	_glx_choose_fb_config_fn             = os::dlsym(_gfx_ctx.native_gl_handle, "glXChooseFBConfig");
	_glx_destroy_context_fn              = os::dlsym(_gfx_ctx.native_gl_handle, "glXDestroyContext");
	_glx_create_window_fn                = os::dlsym(_gfx_ctx.native_gl_handle, "glXCreateWindow");
	_glx_destroy_window_fn               = os::dlsym(_gfx_ctx.native_gl_handle, "glXDestroyWindow");
	_glx_query_extensions_string_fn      = os::dlsym(_gfx_ctx.native_gl_handle, "glXQueryExtensionsString");
	_glx_create_context_attribs_arb_fn   = os::dlsym(_gfx_ctx.native_gl_handle, "glXCreateContextAttribsARB");
	_glx_swap_interval_ext_fn            = os::dlsym(_gfx_ctx.native_gl_handle, "glXSwapIntervalEXT");
	if (!_glx_swap_interval_ext_fn)
	{
		ZString[2] array = {"GLX_MESA_swap_control", "GLX_SGI_swap_control"};
		foreach (name : array)
		{
			_glx_swap_interval_native_fn = os::dlsym(_gfx_ctx.native_gl_handle, name);
			if (_glx_swap_interval_native_fn) break;
		}
	}
}

fn void _monitor_node_free_x11(GfxMonitorNode* node) {}

// private
fn GfxMonitorMode[] _monitor_get_modes_x11(GfxMonitor* monitor)
{
	XRRScreenResources* res = x11::xRRGetScreenResourcesCurrent(_gfx_ctx.display, x11::xDefaultRootWindow(_gfx_ctx.display));
	if (!res) return {};
	XRRCrtcInfo* ci = x11::xRRGetCrtcInfo(_gfx_ctx.display, res, monitor.node.nativeId1);
	GfxXRROutputInfo* oi = x11::xRRGetOutputInfo(_gfx_ctx.display, res, monitor.node.nativeId0);
	sz count = (sz)oi.nmode;
	List{GfxMonitorMode} modes;
	for (int i = 0; i < oi.nmode; i++)
	{
		GfxMonitorMode mode;
		_x_get_mode(ci, res, oi.modes[i], &mode);
		modes.push(mode);
	}
	x11::xRRFreeOutputInfo(oi);
	x11::xRRFreeCrtcInfo(ci);
	x11::xRRFreeScreenResources(res);
	return modes.array_view();
}

fn IntRect _monitor_get_workarea_x11(GfxMonitor* monitor)
{
	XWindow root = x11::xDefaultRootWindow(_gfx_ctx.display);
	int areaX = monitor.origin.x;
	int areaY = monitor.origin.y;
	int areaW = monitor.mode.size.x;
	int areaH = monitor.mode.size.y;

	if (_gfx_ctx.netWorkarea != 0 && _gfx_ctx.netCurrentDesktop != 0)
	{
		Atom* extents = null;
		Atom* desktop = null;
		Atom actualType; int actualFormat; CULong extentCount, bytesAfter, count;
		x11::xGetWindowProperty(_gfx_ctx.display, root, _gfx_ctx.netWorkarea, 0, ~0, 0, 6, &actualType, &actualFormat, &extentCount, &bytesAfter, (char**)&extents);
		x11::xGetWindowProperty(_gfx_ctx.display, root, _gfx_ctx.netCurrentDesktop, 0, ~0, 0, 6, &actualType, &actualFormat, &count, &bytesAfter, (char**)&desktop);

		if (count != 0 && extentCount >= 4 && *desktop < extentCount / 4)
		{
			int globalX = (int)extents[*desktop * 4 + 0];
			int globalY = (int)extents[*desktop * 4 + 1];
			int globalW = (int)extents[*desktop * 4 + 2];
			int globalH = (int)extents[*desktop * 4 + 3];
			if (areaX < globalX) { areaW -= globalX - areaX; areaX = globalX; }
			if (areaY < globalY) { areaH -= globalY - areaY; areaY = globalY; }
			if (areaX + areaW > globalX + globalW) areaW = globalX - areaX + globalW;
			if (areaY + areaH > globalY + globalH) areaH = globalY - areaY + globalH;
		}
		if (extents != null) x11::xFree(extents);
		if (desktop != null) x11::xFree(desktop);
	}

	return { areaX, areaY, areaW, areaH };
}

fn void? _monitor_request_mode_x11(GfxMonitor* mon, GfxMonitorMode* mode, GfxMonitorModeRequest request)
{
	bool output = false;
	XRRScreenResources* res = x11::xRRGetScreenResourcesCurrent(_gfx_ctx.display, x11::xDefaultRootWindow(_gfx_ctx.display));
	if (!res) return MONITOR_MODE_CHANGE_FAILED~;
	XRRCrtcInfo* ci = x11::xRRGetCrtcInfo(_gfx_ctx.display, res, mon.node.nativeId1);
	GfxXRROutputInfo* oi = x11::xRRGetOutputInfo(_gfx_ctx.display, res, mon.node.nativeId0);
	RRMode native = 0;

	for (int i = 0; i < oi.nmode; i++)
	{
		GfxMonitorMode found_mode;
		void* mi = _x_get_mode(ci, res, oi.modes[i], &found_mode);
		if (mi == null) continue;
		if (mode.equals_request(&found_mode, request))
		{
			native = ((XRRModeInfoNative*)mi).id;
			output = true;
			mon.mode = found_mode;
			break;
		}
	}
	if (native != 0) x11::xRRSetCrtcConfig(_gfx_ctx.display, res, mon.node.nativeId1, 0, ci.x, ci.y, native, ci.rotation, ci.outputs, ci.noutput);
	x11::xRRFreeOutputInfo(oi);
	x11::xRRFreeCrtcInfo(ci);
	x11::xRRFreeScreenResources(res);
	if (!output) return MONITOR_MODE_CHANGE_FAILED~;
}

fn void? _monitor_set_mode_native_x11(GfxMonitor* mon, GfxMonitorMode* mode)
{
	bool out = false;
	XRRScreenResources* res = x11::xRRGetScreenResourcesCurrent(_gfx_ctx.display, x11::xDefaultRootWindow(_gfx_ctx.display));
	XRRCrtcInfo* ci = x11::xRRGetCrtcInfo(_gfx_ctx.display, res, mon.node.nativeId1);
	if (x11::xRRSetCrtcConfig(_gfx_ctx.display, res, mon.node.nativeId1, 0, ci.x, ci.y, (RRMode)(uptr)mode.src, ci.rotation, ci.outputs, ci.noutput) == 0) out = true;
	x11::xRRFreeCrtcInfo(ci);
	x11::xRRFreeScreenResources(res);
	if (!out) return MONITOR_MODE_CHANGE_FAILED~;
}

// private
fn GfxKey _physical_to_mapped_key_native_x11(GfxKey key)
{
	KeyCode keycode = (KeyCode)key.to_api();
	KeySym sym = x11::xkbKeycodeToKeysym(_gfx_ctx.display, keycode, 0, 0);
	if (sym < 256) return (GfxKey)sym;

	switch (sym)
	{
		case XK_F1: return F1; case XK_F2: return F2; case XK_F3: return F3;
		case XK_F4: return F4; case XK_F5: return F5; case XK_F6: return F6;
		case XK_F7: return F7; case XK_F8: return F8; case XK_F9: return F9;
		case XK_F10: return F10; case XK_F11: return F11; case XK_F12: return F12;
		case XK_F13: return F13; case XK_F14: return F14; case XK_F15: return F15;
		case XK_F16: return F16; case XK_F17: return F17; case XK_F18: return F18;
		case XK_F19: return F19; case XK_F20: return F20; case XK_F21: return F21;
		case XK_F22: return F22; case XK_F23: return F23; case XK_F24: return F24;
		case XK_F25: return F25;
		case XK_SHIFT_L: return SHIFT_L; case XK_SHIFT_R: return SHIFT_R;
		case XK_CONTROL_L: return CONTROL_L; case XK_CONTROL_R: return CONTROL_R;
		case XK_ALT_L: return ALT_L; case XK_ALT_R: return ALT_R;
		case XK_SUPER_L: return SUPER_L; case XK_SUPER_R: return SUPER_R;
		case XK_CAPS_LOCK: return CAPS_LOCK;
		case XK_NUM_LOCK: return NUM_LOCK;
		case XK_SCROLL_LOCK: return SCROLL_LOCK;
		case XK_UP: return UP; case XK_DOWN: return DOWN;
		case XK_LEFT: return LEFT; case XK_RIGHT: return RIGHT;
		case XK_HOME: return HOME; case XK_END: return END;
		case XK_PAGE_UP: return PAGE_UP; case XK_PAGE_DOWN: return PAGE_DOWN;
		case XK_INSERT: return INSERT;
		case XK_MENU: return MENU;
		case XK_KP_ADD: return PAD_PLUS;
		case XK_KP_SUBTRACT: return PAD_MINUS;
		case XK_KP_MULTIPLY: return PAD_MULTIPLY;
		case XK_KP_DIVIDE: return PAD_SLASH;
		case XK_KP_EQUAL: return PAD_EQUAL;
		case XK_KP_ENTER: return PAD_RETURN;
		case XK_KP_DECIMAL: return PAD_PERIOD;
		case XK_KP_0: return PAD0; case XK_KP_1: return PAD1;
		case XK_KP_2: return PAD2; case XK_KP_3: return PAD3;
		case XK_KP_4: return PAD4; case XK_KP_5: return PAD5;
		case XK_KP_6: return PAD6; case XK_KP_7: return PAD7;
		case XK_KP_8: return PAD8; case XK_KP_9: return PAD9;
		case XK_PRINT: return PRINT_SCREEN;
		case XK_PAUSE: return PAUSE;
		default: break;
	}

	return NULL;
}

// private
fn void _wait_for_event_x11(Posix_pollfd[] fds, Duration duration)
{
	fds[0].fd = x11::xConnectionNumber(_gfx_ctx.display);

	bool infinity = duration == WAIT_FOREVER;
	bool once = true;
	Clock start = clock::now();
	while (!x11::xPending(_gfx_ctx.display))
	{
		Duration diff = duration - start.to_now().to_duration();
		long ms = diff.to_ms();
		if (ms < 0)
		{
			if (!once) break;
			ms = 0;
		}
		if (infinity || ms > CInt::max) ms = -1;
		if (os::poll(&fds[0], 1, (CInt)ms) <= 0) break;
		once = false;
	}
}

fn void _poll_events_x11()
{
	_reset_prev_state();
	x11::xPending(_gfx_ctx.display);
	while (x11::xQLength(_gfx_ctx.display)) _x_handle_event();
}

fn void _x_handle_event()
{
	static Vec2 delta = {};

	XEvent e;
	x11::xNextEvent(_gfx_ctx.display, &e);

	if (e.type != 35 /* GenericEvent */)
	{
		delta = {};
	}

	if (e.type == _gfx_ctx.xrandrEventBase + 0 /* RRNotify */)
	{
		_poll_monitors_native();
		return;
	}

	if (e.type == 30 /* SelectionRequest */)
	{
		_x_handle_clipboard_selection(&e);
		return;
	}

	if (e.type == 35 /* GenericEvent */)
	{
		x11::xGetEventData(_gfx_ctx.display, &e.xcookie);
		if (e.xcookie.evtype == 17 /* XI_RawMotion */)
		{
			XIRawEvent* raw = (XIRawEvent*)e.xcookie.data;
			if (raw.valuators.mask_len != 0)
			{
				int index = 0;
				if (xi_mask_is_set(raw.valuators.mask, 0))
				{
					delta.x += (float)raw.raw_values[index];
					index++;
				}
				if (xi_mask_is_set(raw.valuators.mask, 1)) delta.y += (float)raw.raw_values[index];
				_gfx_ctx.vector = delta;
				_raw_motion_callback(_gfx_ctx.root, delta);
			}
		}
		x11::xFreeEventData(_gfx_ctx.display, &e.xcookie);
		return;
	}

	GfxWindow* win = null;
	if (x11::xFindContext(_gfx_ctx.display, e.xany.window, _gfx_ctx.context, (XPointer*)&win) != 0) return;

	if (win.src.flashEnd != (Clock)0)
	{
		if (win.src.flashEnd.to_now() > time::NANO_DURATION_ZERO || win.has_focus()) win.flash(CANCEL);
	}

	bool keyRepeat = false;
	if (e.type == 3 /* KeyRelease */ && x11::xEventsQueued(_gfx_ctx.display, 1) != 0)
	{
		XEvent ne;
		x11::xPeekEvent(_gfx_ctx.display, &ne);
		if (ne.type == 2 /* KeyPress */ && e.xkey.time == ne.xkey.time && e.xkey.keycode == ne.xkey.keycode)
		{
			x11::xNextEvent(_gfx_ctx.display, &e);
			keyRepeat = true;
		}
	}

	switch (e.type)
	{
		case 2: // KeyPress
			{
				if (!win.internal.enabled_events.key_pressed) return;
				GfxKey value = key_from_api(e.xkey.keycode);

				XkbStateRec state;
				x11::xkbGetState(_gfx_ctx.display, XKB_USE_CORE_KBD, &state);
				_key_update_key_mods(win, state.locked_mods & LOCK_MASK != 0, state.locked_mods & MOD2_MASK != 0, state.locked_mods & MOD3_MASK != 0);

				if (win.src.ic != null && x11::xFilterEvent(&e, 0) == 0)
				{
					char[100] buffer;
					int status;
					sz count = x11::xutf8LookupString(win.src.ic, &e.xkey, (char*)&buffer, 99, null, &status);
					if (status == 2 /* XLookupChars */ || status == 4 /* XLookupBoth */)
					{
						buffer[count] = 0;
						StringIterator iterator = ((ZString)&buffer).str_view().iterator();
						while (try c32 = iterator.next())
						{
							_key_char_callback(win, c32);
						}
					}
				}
				else
				{
					XWindow rootRet, childRet;
					int rootX, rootY, winX, winY;
					uint mask;
					x11::xQueryPointer(_gfx_ctx.display, x11::xDefaultRootWindow(_gfx_ctx.display), &rootRet, &childRet, &rootX, &rootY, &winX, &winY, &mask);
					KeySym sym = x11::xkbKeycodeToKeysym(_gfx_ctx.display, (KeyCode)e.xkey.keycode, 0, (mask & SHIFT_MASK) ? 1 : 0);

					if ((mask & LOCK_MASK) && sym >= 0x0061 /* XK_a */ && sym <= 0x007a /* XK_z */)
					{
						sym = (mask & SHIFT_MASK) ? sym + 32 : sym - 32;
					}
					if ((KeySym)(char)sym != sym) sym = 0;

					_key_char_callback(win, (uint)(char)sym);
				}

				_key_callback(win, value, win.internal.mod, keyRepeat, true);
				break;
			}
		case 3: // KeyRelease
			{
				if (!win.internal.enabled_events.key_released) return;
				GfxKey value = key_from_api(e.xkey.keycode);

				XkbStateRec state;
				x11::xkbGetState(_gfx_ctx.display, XKB_USE_CORE_KBD, &state);
				_key_update_key_mods(win, state.locked_mods & LOCK_MASK != 0, state.locked_mods & MOD2_MASK != 0, state.locked_mods & MOD3_MASK != 0);

				_key_callback(win, value, win.internal.mod, false, false);
				break;
			}
		case 4: // ButtonPress
			{
				bool scroll = e.xbutton.button >= 4 && e.xbutton.button <= 7;
				Vec2 scroll_delta;
				GfxMouseButton value;
				switch (e.xbutton.button)
				{
					case 1: value = LEFT;
					case 2: value = MIDDLE;
					case 3: value = RIGHT;
					case 4: scroll_delta = { 0, 1 };
					case 5: scroll_delta = { 0, -1 };
					case 6: scroll_delta = { 1, 0 };
					case 7: scroll_delta = { -1, 0 };
					default: value = (GfxMouseButton)(e.xbutton.button - 1 - 4);
				}
				if (scroll)
				{
					_mouse_scroll_callback(win, scroll_delta);
					break;
				}
				_mouse_button_callback(win, value, true);
				break;
			}
		case 5: // ButtonRelease
			{
				if (e.xbutton.button >= 4 && e.xbutton.button <= 7) break;
				GfxMouseButton value;
				switch (e.xbutton.button)
				{
					case 1: value = LEFT;
					case 2: value = MIDDLE;
					case 3: value = RIGHT;
					default: value = (GfxMouseButton)(e.xbutton.button - 1 - 4);
				}
				_mouse_button_callback(win, value, false);
				break;
			}
		case 6: _mouse_motion_callback(win, { e.xmotion.x, e.xmotion.y }); // MotionNotify
		case 12: _window_refresh_callback(win, { e.xexpose.x, e.xexpose.y, e.xexpose.width, e.xexpose.height }); // Expose
		case 28: // PropertyNotify
			if (e.xproperty.state == 0 /* PropertyNewValue */)
			{
				bool handled = false;
				if (e.xproperty.atom == _gfx_ctx.wmState)
				{
					if (_window_is_minimized_native(win) && !(win.internal.flags.minimize))
					{
						_window_minimized_callback(win);
						handled = true;
					}
				}
				else if (e.xproperty.atom == _gfx_ctx.netWmState)
				{
					if (_window_is_maximized_native(win) && !(win.internal.flags.maximize))
					{
						_window_maximized_callback(win, win.bounds);
						handled = true;
					}
				}
				if (!handled) _window_check_mode(win);
			}
		case 19: case 18: // MapNotify, UnmapNotify
			_window_check_mode(win);
			break;
		case 33: // ClientMessage
			{
				if ((CULong)e.xclient.data.l[0] == _gfx_ctx.wmDeleteWindow)
				{
					_window_close_callback(win);
					break;
				}

				if (!(win.internal.flags.allow_dnd) || _gfx_ctx.x11Version > 5 /* _XDND_VERSION */) return;

				GfxPoint drag;

				if (e.xclient.message_type == _gfx_ctx.xdndEnter)
				{
					CULong count;
					Atom* formats;
					Atom[3] realFormats;
					int list = (int)(e.xclient.data.l[1] & 1);

					_gfx_ctx.x11Source = (XWindow)e.xclient.data.l[0];
					_gfx_ctx.x11Version = (int)(e.xclient.data.l[1] >> 24);
					_gfx_ctx.x11Format = 0;
					if (list)
					{
						Atom actualType;
						int actualFormat;
						CULong bytesAfter;
						x11::xGetWindowProperty(_gfx_ctx.display, _gfx_ctx.x11Source, _gfx_ctx.xdndTypeList, 0, ~0, 0, 4,
						&actualType, &actualFormat, &count, &bytesAfter, (char**)&formats);
					}
					else
					{
						count = 0;
						for (sz i = 2; i < 2 + 3; i++)
						{
							if (e.xclient.data.l[i] != 0)
							{
								realFormats[count] = (CULong)e.xclient.data.l[i];
								count++;
							}
						}
						formats = (Atom*)&realFormats;
					}

					for (sz i = 0; i < (sz)count; i++)
					{
						if (formats[i] == _gfx_ctx.xtextUriList)
						{
							_gfx_ctx.x11TransferType = FILE;
						}
						else if (formats[i] == _gfx_ctx.xtextPlain)
						{
							_gfx_ctx.x11TransferType = TEXT;
						}
						else
						{
							continue;
						}

						_gfx_ctx.x11Format = (int)formats[i];
						break;
					}

					if (list != 0 && formats != null) x11::xFree(formats);

					_data_drag_callback(win, _gfx_ctx.x11TransferType, ENTER, drag);
				}
				else if (e.xclient.message_type == _gfx_ctx.xdndPosition)
				{
					int xabs = (int)((e.xclient.data.l[2] >> 16) & 0xffff);
					int yabs = (int)(e.xclient.data.l[2] & 0xffff);
					XWindow dummy;
					int xpos, ypos;

					x11::xTranslateCoordinates(_gfx_ctx.display, x11::xDefaultRootWindow(_gfx_ctx.display), win.src.window,
					xabs, yabs, &xpos, &ypos, &dummy);

					drag = { xpos, ypos };

					_mouse_motion_callback(win, drag);

					XEvent reply;
					reply.type = 33; // ClientMessage
					reply.xclient.window = _gfx_ctx.x11Source;
					reply.xclient.message_type = _gfx_ctx.xdndStatus;
					reply.xclient.format = 32;
					reply.xclient.data.l[0] = (CLong)win.src.window;
					reply.xclient.data.l[2] = 0;
					reply.xclient.data.l[3] = 0;

					if (_gfx_ctx.x11Format != 0)
					{
						reply.xclient.data.l[1] = 1;
						if (_gfx_ctx.x11Version >= 2) reply.xclient.data.l[4] = (CLong)_gfx_ctx.xdndActionCopy;
					}

					x11::xSendEvent(_gfx_ctx.display, _gfx_ctx.x11Source, 0, 0, &reply);
					x11::xFlush(_gfx_ctx.display);

					_data_drag_callback(win, _gfx_ctx.x11TransferType, MOVE, drag);
				}
				else if (e.xclient.message_type == _gfx_ctx.xdndLeave)
				{
					_data_drag_callback(win, _gfx_ctx.x11TransferType, EXIT, drag);
				}
				else if (e.xclient.message_type == _gfx_ctx.xdndDrop)
				{
					if (_gfx_ctx.x11Format != 0)
					{
						Time_t time = _gfx_ctx.x11Version >= 1 ?(Time_t)e.xclient.data.l[2] : 0; // CurrentTime
						x11::xConvertSelection(_gfx_ctx.display, _gfx_ctx.xdndSelection, (Atom)_gfx_ctx.x11Format,
						_gfx_ctx.xdndSelection, win.src.window, time);
					}
					else if (_gfx_ctx.x11Version >= 2)
					{
						XEvent reply;
						reply.type = 33; // ClientMessage
						reply.xclient.window = _gfx_ctx.x11Source;
						reply.xclient.message_type = _gfx_ctx.xdndFinished;
						reply.xclient.format = 32;
						reply.xclient.data.l[0] = (CLong)win.src.window;
						reply.xclient.data.l[1] = 0;
						reply.xclient.data.l[2] = 0;

						x11::xSendEvent(_gfx_ctx.display, _gfx_ctx.x11Source, 0, 0, &reply);
						x11::xFlush(_gfx_ctx.display);
					}
				}
				break;
			}
		case 31: // SelectionNotify
			{
				if (!win.internal.enabled_events.data_drop || e.xselection.property != _gfx_ctx.xdndSelection || !(win.internal.flags.allow_dnd)) return;

				ZString data;
				CULong result;
				Atom actualType;
				int actualFormat;
				CULong bytesAfter;

				x11::xGetWindowProperty(_gfx_ctx.display, e.xselection.requestor, e.xselection.property, 0, ~0, 0, e.xselection.target,
				&actualType, &actualFormat, &result, &bytesAfter, (char**)&data);

				if (result != 0)
				{
					_drop_url(win, data);
					if (data != null) x11::xFree(data);
				}

				if (_gfx_ctx.x11Version >= 2)
				{
					XEvent reply;
					reply.type = 33; // ClientMessage
					reply.xclient.window = _gfx_ctx.x11Source;
					reply.xclient.message_type = _gfx_ctx.xdndFinished;
					reply.xclient.format = 32;
					reply.xclient.data.l[0] = (CLong)win.src.window;
					reply.xclient.data.l[1] = (CLong)result;
					reply.xclient.data.l[2] = (CLong)_gfx_ctx.xdndActionCopy;
					x11::xSendEvent(_gfx_ctx.display, _gfx_ctx.x11Source, 0, 0, &reply);
					x11::xFlush(_gfx_ctx.display);
				}
				break;
			}
		case 22: // ConfigureNotify
			{
				if (e.xconfigure.width != win.src.width || e.xconfigure.height != win.src.height)
				{
					_window_check_mode(win);
					win.src.size = { e.xconfigure.width, e.xconfigure.height };
					_window_resized_callback(win, win.src.size);
				}
				GfxPoint configure = { e.xconfigure.x, e.xconfigure.y };

				// if the event came from the server and we're not a direct child of the root window then
				// we're using local coords which need to be translated into screen coords
				XWindow root = x11::xDefaultRootWindow(_gfx_ctx.display);
				if (e.xany.send_event == 0 && win.src.parent != root)
				{
					XWindow dummy;
					x11::xTranslateCoordinates(_gfx_ctx.display, win.src.parent, root, ...configure, &configure.x, &configure.y, &dummy);
				}

				if (configure != win.origin)
				{
					win.src.origin = configure;
					_window_moved_callback(win, configure);
				}
				return;
			}
		case 9: // FocusIn
			if (win.src.ic) x11::xSetICFocus(win.src.ic);
			_window_focus_callback(win, true);
		case 10: // FocusOut
			if (win.src.ic) x11::xUnsetICFocus(win.src.ic);
			_window_focus_callback(win, false);
		case 7: _mouse_notify_callback(win, { e.xcrossing.x, e.xcrossing.y }, true); // EnterNotify
		case 8: _mouse_notify_callback(win, win.internal.last_mouse, false); // LeaveNotify
		case 21: win.src.parent = e.xreparent.parent; // ReparentNotify
		default: break;
	}

	x11::xFlush(_gfx_ctx.display);
}

// private
fn void _poll_monitors_x11()
{
	XWindow root = x11::xDefaultRootWindow(_gfx_ctx.display);
	XRRScreenResources* res = x11::xRRGetScreenResourcesCurrent(_gfx_ctx.display, root);
	if (res == null) return;
	RROutput primary = x11::xRRGetOutputPrimary(_gfx_ctx.display, root);

	for (GfxMonitorNode* node = _gfx_ctx.monitors.list.head; node != null; node = node.next) node.disconnected = true;

	for (int i = 0; i < res.noutput; i++)
	{
		GfxMonitorNode* node = null;
		for (node = _gfx_ctx.monitors.list.head; node != null; node = node.next)
		{
			if (node.nativeId0 == res.outputs[i]) break;
		}
		if (node != null)
		{
			node.disconnected = false;
			if (node.nativeId0 == primary) _gfx_ctx.monitors.primary = node;
			continue;
		}

		GfxMonitor monitor;
		GfxXRROutputInfo* info = x11::xRRGetOutputInfo(_gfx_ctx.display, res, res.outputs[i]);
		if (info == null) continue;
		if (info.connection != 0 || info.crtc == 0) { x11::xRRFreeOutputInfo(info); continue; }

		XRRCrtcInfo* ci = x11::xRRGetCrtcInfo(_gfx_ctx.display, res, info.crtc);
		if (ci == null) continue;

		float physW = (float)info.mm_width / 25.4f;
		float physH = (float)info.mm_height / 25.4f;

		sz n = info.name.len();
		if (n > 127) n = 127;
		mem::copy(&monitor.name, info.name, n);
		monitor.name[n] = 0;

		if (physW > 0.0f && physH > 0.0f)
		{
			monitor.phys_size = { physW, physH };
		}
		else
		{
			monitor.phys_size = { ci.width / 96.0f, ci.height / 96.0f };
		}

		monitor.origin = { ci.x, ci.y };

		float dpi = 96.0f;
		_x_get_system_content_dpi(&dpi);
		monitor.scale = (Vec2)(dpi / 96.0f);
		monitor.pixelRatio = dpi >= 192.0f ? 2.0f : 1.0f;

		void* mi = _x_get_mode(ci, res, ci.mode, &monitor.mode);
		if (mi == null) break;

		x11::xRRFreeCrtcInfo(ci);
		node = _monitor_add(&monitor);
		if (node == null) break;
		node.nativeId0 = res.outputs[i];
		node.nativeId1 = info.crtc;
		if (node.nativeId0 == primary) _gfx_ctx.monitors.primary = node;
		x11::xRRFreeOutputInfo(info);
		_monitor_callback(_gfx_ctx.root, &node.mon, true);
	}

	x11::xRRFreeScreenResources(res);
	refresh_monitors();
}

// private
fn void _window_close_native_x11(GfxWindow* win)
{
	if (win.src.ic)
	{
		x11::xDestroyIC(win.src.ic);
		win.src.ic = null;
	}
	x11::xFreeGC(_gfx_ctx.display, win.src.gc);
	x11::xDeleteContext(_gfx_ctx.display, win.src.window, _gfx_ctx.context);
	x11::xDestroyWindow(_gfx_ctx.display, win.src.window);
}

fn void _window_release_opengl_context_x11(GfxWindow* win, GfxGlContext* ctx)
{
	if (win.src.ctx.native.window != win.src.window) _glx_destroy_window_fn(_gfx_ctx.display, win.src.ctx.native.window);
	_glx_destroy_context_fn(_gfx_ctx.display, ctx.ctx);
	win.src.ctx.native = null;
}

fn void _window_release_egl_context_x11(GfxWindow* win, GfxEglContext* ctx) {}

fn void _window_focus_x11(GfxWindow* win)
{
	XWindowAttributes attr;
	x11::xGetWindowAttributes(_gfx_ctx.display, win.src.window, &attr);
	if (attr.map_state != 2 /* IsViewable */) return;
	x11::xSetInputFocus(_gfx_ctx.display, win.src.window, 1 /* RevertToPointerRoot */, 0);
	x11::xFlush(_gfx_ctx.display);
}

fn void _window_hide_x11(GfxWindow* win)
{
	win.internal.flags.hide = true;
	x11::xUnmapWindow(_gfx_ctx.display, win.src.window);
	x11::xFlush(_gfx_ctx.display);
}

fn bool _window_is_hidden_x11(GfxWindow* win)
{
	XWindowAttributes windowAttributes;
	x11::xGetWindowAttributes(_gfx_ctx.display, win.src.window, &windowAttributes);
	return windowAttributes.map_state != 2;
}

// private
fn bool _window_is_maximized_x11(GfxWindow* win)
{
	Atom actualType;
	int actualFormat;
	CULong nitems, bytesAfter;
	char* propData;
	int status = x11::xGetWindowProperty(_gfx_ctx.display, win.src.window, _gfx_ctx.netWmState, 0, 1024, 0, 4, &actualType, &actualFormat, &nitems, &bytesAfter, &propData);
	if (status != 0) { if (propData != null) x11::xFree(propData); return false; }
	for (CULong i = 0; i < nitems; i++)
	{
		if ((Atom)propData[i] == _gfx_ctx.net_wm_state_maximized_vert || (Atom)propData[i] == _gfx_ctx.net_wm_state_maximized_horz)
		{
			x11::xFree(propData);
			return true;
		}
	}
	if (propData != null) x11::xFree(propData);
	return false;
}

// private
fn bool _window_is_minimized_x11(GfxWindow* win)
{
	Atom actualType;
	int actualFormat;
	CULong nitems, bytesAfter;
	char* propData;
	int status = x11::xGetWindowProperty(_gfx_ctx.display, win.src.window, _gfx_ctx.wmState, 0, 2, 0, 0, &actualType, &actualFormat, &nitems, &bytesAfter, &propData);
	if (status == 0 && nitems >= 1 && propData == (char*)(uptr)3 /* IconicState */)
	{
		x11::xFree(propData);
		return true;
	}
	if (propData != null) x11::xFree(propData);
	XWindowAttributes windowAttributes;
	x11::xGetWindowAttributes(_gfx_ctx.display, win.src.window, &windowAttributes);
	return windowAttributes.map_state != 2;
}

// private
fn void _window_make_opengl_current_x11(GfxWindow* win)
{
	if (!win)
	{
		_glx_make_current_fn(null, 0, null);
	}
	else
	{
		_glx_make_current_fn(_gfx_ctx.display, win.src.ctx.native.window, win.src.ctx.native.ctx);
	}
}

fn void _window_maximize_x11(GfxWindow* win) => _toggle_x_maximized(win, true);

fn void _window_minimize_x11(GfxWindow* win)
{
	if (_window_is_maximized_native(win)) return;
	win.internal.old_bounds = win.bounds;
	x11::xIconifyWindow(_gfx_ctx.display, win.src.window, x11::xDefaultScreen(_gfx_ctx.display));
	x11::xFlush(_gfx_ctx.display);
}

fn void _window_move_mouse_x11(GfxWindow* win, GfxPoint where)
{
	int rootX, rootY, win_x, win_y;
	uint mask;
	XWindow root_ret, child_ret;
	x11::xQueryPointer(_gfx_ctx.display, x11::xDefaultRootWindow(_gfx_ctx.display), &root_ret, &child_ret, &rootX, &rootY, &win_x, &win_y, &mask);
	win.internal.last_mouse = where - win.origin;
	if (win_x == where.x && win_y == where.y) return;
	x11::xWarpPointer(_gfx_ctx.display, 0, win.src.window, 0, 0, 0, 0, ...(where - win.origin));
}

fn void _window_move_x11(GfxWindow* win, GfxPoint where)
{
	win.origin = where;
	x11::xMoveWindow(_gfx_ctx.display, win.src.window, ...where);
}

fn void _window_raise_x11(GfxWindow* win)
{
	x11::xMapRaised(_gfx_ctx.display, win.src.window);
	_window_set_fullscreen_native(win, win.is_fullscreen());
}

// private
fn void _window_resize_x11(GfxWindow* win, GfxSize size)
{
	win.size = size;
	x11::xResizeWindow(_gfx_ctx.display, win.src.window, (uint)size.x, (uint)size.y);
	if (win.internal.flags.no_resize)
	{
		XSizeHints sh;
		sh.flags = (1 << 4) | (1 << 5);
		sh.min_width = sh.max_width = size.x;
		sh.min_height = sh.max_height = size.y;
		x11::xSetWMSizeHints(_gfx_ctx.display, win.src.window, &sh, 0);
	}
}

// private
fn void _window_restore_x11(GfxWindow* win)
{
	_toggle_x_maximized(win, false);
	x11::xFlush(_gfx_ctx.display);
}

fn void _window_set_floating_x11(GfxWindow* win, bool floating) => _window_set_x_atom(win, _gfx_ctx.netWmStateAbove, floating);

fn void _window_set_fullscreen_x11(GfxWindow* win, bool fullscreen)
{
	if (fullscreen)
	{
		win.internal.flags.fullscreen = true;
		win.internal.old_bounds = win.bounds;
	}
	else
	{
		win.internal.flags.fullscreen = false;
	}

	x11::xRaiseWindow(_gfx_ctx.display, win.src.window);
	_window_set_x_atom(win, _gfx_ctx.netWmStateFullscreen, fullscreen);

	if (!(win.internal.flags.transparent))
	{
		char value = (char)fullscreen;
		x11::xChangeProperty(_gfx_ctx.display, win.src.window, _gfx_ctx.netWmBypassCompositor, 6 /* XA_CARDINAL */, 32, 0, &value, 1);
	}
}

// private
fn void _window_set_max_size_x11(GfxWindow* win, GfxSize size)
{
	CLong flags;
	XSizeHints hints;
	x11::xGetWMNormalHints(_gfx_ctx.display, win.src.window, &hints, &flags);
	hints.flags |= 1 << 5; // PMaxSize
	hints.max_width = size.x; hints.max_height = size.y;
	x11::xSetWMNormalHints(_gfx_ctx.display, win.src.window, &hints);
}

// private
fn void _window_set_min_size_x11(GfxWindow* win, GfxSize size)
{
	CLong flags;
	XSizeHints hints;
	x11::xGetWMNormalHints(_gfx_ctx.display, win.src.window, &hints, &flags);
	hints.flags |= 1 << 4; // PMinSize
	hints.min_width = size.x; hints.min_height = size.y;
	x11::xSetWMNormalHints(_gfx_ctx.display, win.src.window, &hints);
}

fn bool _window_set_mouse_x11(GfxWindow* win, GfxMouse* mouse)
{
	x11::xDefineCursor(_gfx_ctx.display, win.src.window, (XCursor)mouse);
	return true;
}

fn void _window_set_opacity_x11(GfxWindow* win, char opacity)
{
	uint value = (uint)(4294967295.0 * (double)opacity);
	x11::xChangeProperty(_gfx_ctx.display, win.src.window, _gfx_ctx.netWmWindowOpacity, 6, 32, 0, (char*)&value, 1);
}

fn void _window_opengl_swap_buffers_x11(GfxWindow* win) { _glx_swap_buffers_fn(_gfx_ctx.display, win.src.ctx.native.window); }

// private
fn void? _write_clipboard_x11(GfxDataTransfer* data)
{
	x11::xSetSelectionOwner(_gfx_ctx.display, _gfx_ctx.clipboardAtom, _gfx_ctx.helper_window, 0);
	if (x11::xGetSelectionOwner(_gfx_ctx.display, _gfx_ctx.clipboardAtom) != _gfx_ctx.helper_window)
	{
		return COULD_NOT_OWN_X11_CLIPBOARD~;
	}

	if (_gfx_ctx.unixClipboard)
	{
		free(_gfx_ctx.unixClipboard);
		_gfx_ctx.unixClipboard = null;
	}

	_gfx_ctx.unixClipboard = mem::new_with_padding(GfxDataTransfer, data.text.len);
	char* data_ptr = (char*)_gfx_ctx.unixClipboard + GfxDataTransfer::size - 1;
	data_ptr[:data.text.len] = data.text[..];
	_gfx_ctx.unixClipboard.text = (String)data_ptr[:data.text.len];
	_gfx_ctx.unixClipboard.type = TEXT;
}

fn GfxDataTransfer? _read_clipboard_x11(Allocator allocator, GfxDataTransferType type)
{
	if (type != TEXT) return CLIPBOARD_DID_NOT_CONTAIN_TYPE~;

	if (x11::xGetSelectionOwner(_gfx_ctx.display, _gfx_ctx.clipboardAtom) == _gfx_ctx.helper_window)
	{
		if (!_gfx_ctx.unixClipboard.text) return CLIPBOARD_DID_NOT_CONTAIN_TYPE~;
		return { .type = TEXT, .text = _gfx_ctx.unixClipboard.text.copy(allocator) };
	}

	x11::xConvertSelection(_gfx_ctx.display, _gfx_ctx.clipboardAtom, _gfx_ctx.utf8String, _gfx_ctx.xselData, _gfx_ctx.helper_window, 0);
	x11::xSync(_gfx_ctx.display, 0);
	XEvent event;
	while (true)
	{
		x11::xNextEvent(_gfx_ctx.display, &event);
		if (event.type != 31 /* SelectionNotify */) continue;
		if (event.xselection.selection != _gfx_ctx.clipboardAtom || event.xselection.property == 0) return CLIPBOARD_COULD_NOT_BE_OPENED~;
		break;
	}

	Atom target;
	int format;
	CULong n, size;
	char* data;
	x11::xGetWindowProperty(_gfx_ctx.display, event.xselection.requestor, event.xselection.property, 0, ~0, 0, 0, &target, &format, &size, &n, (char**)&data);
	defer x11::xDeleteProperty(_gfx_ctx.display, event.xselection.requestor, event.xselection.property);

	if (target == _gfx_ctx.utf8String || target == 31)
	{
		defer x11::xFree(data);
		return { .type = TEXT, .text = (String)alloc::clone_slice(allocator, data[:size]) };
	}
	return CLIPBOARD_DID_NOT_CONTAIN_TYPE~;
}

// private
fn void? _window_init_opengl_context_x11(GfxWindow* win, GfxGlContext* context, GfxGlHints* hints)
{
	win.src.ctx.native = context;
	win.src.backing = { .opengl };

	bool show_window = false;
	if (win.src.window)
	{
		show_window = !_window_is_minimized_native(win);
		_window_close_native(win);
	}

	bool transparent = win.internal.flags.transparent;

	FixedList{int, 40} stack;
	stack.push_all({
		GLX_X_VISUAL_TYPE, GLX_TRUE_COLOR,
		GLX_X_RENDERABLE, 1,
		GLX_RENDER_TYPE, GLX_RGBA_BIT,
		GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT,
		GLX_DOUBLEBUFFER, 1,
		GLX_ALPHA_SIZE, hints.alpha,
		GLX_DEPTH_SIZE, hints.depth,
		GLX_STENCIL_SIZE, hints.stencil,
		GLX_STEREO, hints.stereo,
		GLX_AUX_BUFFERS, hints.auxBuffers,
		GLX_RED_SIZE, hints.red,
		GLX_GREEN_SIZE, hints.green,
		GLX_BLUE_SIZE, hints.blue,
		GLX_ACCUM_RED_SIZE, hints.accumRed,
		GLX_ACCUM_GREEN_SIZE, hints.accumGreen,
		GLX_ACCUM_BLUE_SIZE, hints.accumBlue,
		GLX_ACCUM_ALPHA_SIZE, hints.accumAlpha,
	});

	if (hints.sRGB)
	{
		if (_opengl_extension_is_supported_x11("GLX_ARB_framebuffer_sRGB")) stack.push_all({ GLX_FRAMEBUFFER_SRGB_CAPABLE_ARB, (int)hints.sRGB });
		if (_opengl_extension_is_supported_x11("GLX_EXT_framebuffer_sRGB")) stack.push_all({ GLX_FRAMEBUFFER_SRGB_CAPABLE_EXT, (int)hints.sRGB });
	}

	stack.push_all({0, 0});

	int fbcount;
	GLXFBConfig* fbc = _glx_choose_fb_config_fn(_gfx_ctx.display, x11::xDefaultScreen(_gfx_ctx.display), stack.array_view(), &fbcount);
	if (!fbc || !fbcount) return GLX_VISUAL_CONFIGS_NOT_FOUND~;

	int bestFbcIdx = -1;
	int bestDepth = 0;
	int bestSamples = 0;
	for (int i = 0; i < fbcount; i++)
	{
		XVisualInfo* vi = _glx_get_visual_from_fb_config_fn(_gfx_ctx.display, fbc[i]);
		if (vi == null) continue;
		int sampBuf, samples;
		_glx_get_fb_config_attrib_fn(_gfx_ctx.display, fbc[i], GLX_SAMPLE_BUFFERS, &sampBuf);
		_glx_get_fb_config_attrib_fn(_gfx_ctx.display, fbc[i], GLX_SAMPLES, &samples);
		if (bestFbcIdx == -1) bestFbcIdx = i;
		if ((!transparent || vi.depth == 32) && bestDepth == 0) { bestFbcIdx = i; bestDepth = vi.depth; }
		if ((!transparent || vi.depth == 32) && samples <= hints.samples && samples > bestSamples) { bestFbcIdx = i; bestDepth = vi.depth; bestSamples = samples; }
		x11::xFree(vi);
	}
	if (bestFbcIdx == -1) return GLX_NO_VALID_VISUAL~;

	GLXFBConfig bestFbc = fbc[bestFbcIdx];
	XVisualInfo* vi = _glx_get_visual_from_fb_config_fn(_gfx_ctx.display, bestFbc);
	x11::xFree(fbc);
	XVisualInfo visual = *vi;
	x11::xFree(vi);

	_x_create_window(visual, "", win.internal.flags, win)!;

	if (show_window) win.show();

	stack.clear();
	int mask = 0;
	switch (hints.profile)
	{
		case ES:                    mask |= GLX_CONTEXT_ES_PROFILE_BIT_EXT;
		case FORWARD_COMPATIBILITY: mask |= GLX_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB;
		case COMPATIBILITY:         mask |= GLX_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB;
		default:                    mask |= GLX_CONTEXT_CORE_PROFILE_BIT_ARB;
	}
	stack.push_all({ GLX_CONTEXT_PROFILE_MASK_ARB, mask });
	if (hints.minor || hints.major)
	{
		stack.push_all({ GLX_CONTEXT_MAJOR_VERSION_ARB, hints.major, GLX_CONTEXT_MINOR_VERSION_ARB, hints.minor });
	}

	if (_opengl_extension_is_supported_x11("GLX_ARB_context_flush_control"))
	{
		switch (hints.release_behavior)
		{
			case FLUSH:
				stack.push_all({ GLX_CONTEXT_RELEASE_BEHAVIOR_ARB, GLX_CONTEXT_RELEASE_BEHAVIOR_FLUSH_ARB });
			case NONE:
				stack.push_all({ GLX_CONTEXT_RELEASE_BEHAVIOR_ARB, GLX_CONTEXT_RELEASE_BEHAVIOR_NONE_ARB });
		}
	}

	int glFlags = 0;
	if (hints.debug) glFlags |= GLX_CONTEXT_DEBUG_BIT_ARB;
	if (hints.robustness && _opengl_extension_is_supported_x11("GLX_ARB_create_context_robustness")) glFlags |= GLX_CONTEXT_ROBUST_ACCESS_BIT_ARB;
	if (glFlags != 0) stack.push_all({ GLX_CONTEXT_FLAGS_ARB, glFlags });

	if (_opengl_extension_is_supported_x11("GLX_ARB_create_context_no_error")) stack.push_all({ GLX_CONTEXT_OPENGL_NO_ERROR_ARB, (int)hints.noError });

	stack.push_all({0, 0});

	GLXContext ctx = null;
	if (hints.share) ctx = hints.share.ctx;

	if (!_glx_create_context_attribs_arb_fn)
	{
		log::warn("Failed to load proc address 'glXCreateContextAttribsARB', loading a generic OpenGL context.", category: log::CATEGORY_VIDEO);
		win.src.ctx.native.ctx = _glx_create_context_fn(_gfx_ctx.display, &visual, ctx, X_TRUE);
	}
	else
	{
		_gfx_ctx.x11Error = null;
		win.src.ctx.native.ctx = _glx_create_context_attribs_arb_fn(_gfx_ctx.display, bestFbc, ctx, X_TRUE, stack.array_view());
		if (_gfx_ctx.x11Error || !win.src.ctx.native.ctx)
		{
			log::warn("Failed to create an OpenGL context with AttribsARB, loading a generic OpenGL context.", category: log::CATEGORY_VIDEO);
			win.src.ctx.native.ctx = _glx_create_context_fn(_gfx_ctx.display, &visual, ctx, X_TRUE);
		}
	}

	win.src.ctx.native.window = _glx_create_window_fn(_gfx_ctx.display, bestFbc, win.src.window, null);
	_glx_make_current_fn(_gfx_ctx.display, win.src.ctx.native.window, win.src.ctx.native.ctx);
	log::info("OpenGL context initalized.", category: log::CATEGORY_VIDEO);
	win.opengl_swap_interval(0);
}

// private
fn void _window_flash_x11(GfxWindow* win, GfxFlashRequest request)
{
	if (win.has_focus() && request != CANCEL) return;
	XWMHints* wmhints = x11::xGetWMHints(_gfx_ctx.display, win.src.window);
	if (!wmhints) return;
	if (request)
	{
		wmhints.flags |= 1 << 8; // XUrgencyHint
		if (request == BRIEFLY) win.src.flashEnd = clock::now() + time::sec(1).to_nano();
		if (request == UNTIL_FOCUSED) win.src.flashEnd = clock::FAR_FUTURE;
	}
	else
	{
		win.src.flashEnd = {};
		wmhints.flags &= ~(1 << 8);
	}
	x11::xSetWMHints(_gfx_ctx.display, win.src.window, wmhints);
	x11::xFree(wmhints);
}

// private
fn void? _init_surface_x11(GfxSurface* surface, void* data, int w, int h, GfxFormat format, GfxWindow* window)
{
	if (!window) window = _gfx_ctx.root;
	surface.data = data;
	surface.size = { w, h };
	surface.format = format;

	XWindowAttributes attrs;
	if (!x11::xGetWindowAttributes(_gfx_ctx.display, window.src.window, &attrs)) return SURFACE_CREATION_FAILED~;
	defer catch surface.release();
	surface.native.bitmap = x11::xCreateImage(_gfx_ctx.display, attrs.visual, (uint)attrs.depth, 2 /* ZPixmap */, 0, null, ...(uint[<2>])surface.size, 32, 0);
	if (!surface.native.bitmap) return SURFACE_CREATION_FAILED~;
	surface.native.buffer = malloc((sz)w * h * 4);
	((XImageHeader*)surface.native.bitmap).data = surface.native.buffer;
	surface.native.format = _x_image_get_format(surface.native.bitmap);

}

// private
fn bool _window_is_floating_x11(GfxWindow* win)
{
	Atom actualType;
	int actualFormat;
	CULong nitems, bytesAfter;
	Atom* propReturn = null;
	int status = x11::xGetWindowProperty(_gfx_ctx.display, win.src.window, _gfx_ctx.netWmState, 0, ~0, 0, 4, &actualType, &actualFormat, &nitems, &bytesAfter, (char**)&propReturn);
	if (status != 0 || actualType != 4) return false;
	for (CULong i = 0; i < nitems; i++) if (propReturn[i] == _gfx_ctx.netWmStateAbove) return true;
	if (propReturn != null) x11::xFree(propReturn);
	return false;
}

// private
fn void _window_set_name_x11(GfxWindow* win, String name) => @pool()
{
	ZString cstring = name.zstr_copy(tmem);
	x11::xutf8SetWMProperties(_gfx_ctx.display, win.src.window, cstring, cstring, null, 0, null, null, null);
	x11::xStoreName(_gfx_ctx.display, win.src.window, cstring);
	x11::xChangeProperty(_gfx_ctx.display, win.src.window, _gfx_ctx.netWmName, _gfx_ctx.utf8String, 8, 0, cstring, name.len);
}

// private
fn bool _window_set_icon_x11(GfxWindow* win, char* dataSrc, int w, int h, GfxFormat format, GfxWindowIconType type)
{
	if (dataSrc == null)
	{
		return !!x11::xChangeProperty(_gfx_ctx.display, win.src.window, _gfx_ctx.netWmIcon, 6, 32, 0, null, 0);
	}
	int count = 2 + (w * h);
	CULong* data = calloc((sz)count * CULong::size);
	data[0] = (CULong)w;
	data[1] = (CULong)h;

	copy_image_data_64((char*)&data[2], w, h, BGRA8, dataSrc, format, true, null);
	bool res = true;
	if (type & TASKBAR)
	{
		res = !!x11::xChangeProperty(_gfx_ctx.display, win.src.window, _gfx_ctx.netWmIcon, 6, 32, 0, (char*)data, count);
	}

	copy_image_data_64((char*)&data[2], w, h, BGRA8, dataSrc, format, false, null);

	if (type & WINDOW)
	{
		XWMHints wmHints;
		wmHints.flags = 1 << 2; // IconPixmapHint

		int depth = x11::xDefaultDepth(_gfx_ctx.display, x11::xDefaultScreen(_gfx_ctx.display));
		void* image = x11::xCreateImage(_gfx_ctx.display, x11::xDefaultVisual(_gfx_ctx.display, x11::xDefaultScreen(_gfx_ctx.display)),
		(uint)depth, 2 /* ZPixmap */, 0, (char*)&data[2], (uint)w, (uint)h, 32, 0);

		wmHints.icon_pixmap = x11::xCreatePixmap(_gfx_ctx.display, win.src.window, (uint)w, (uint)h, (uint)depth);
		x11::xPutImage(_gfx_ctx.display, wmHints.icon_pixmap, x11::xDefaultGC(_gfx_ctx.display, x11::xDefaultScreen(_gfx_ctx.display)), image, 0, 0, 0, 0, (uint)w, (uint)h);
		((XImageHeader*)image).data = null;
		x11::xDestroyImage(image);

		x11::xSetWMHints(_gfx_ctx.display, win.src.window, &wmHints);
	}

	free(data);
	x11::xFlush(_gfx_ctx.display);
	return res;
}

fn GfxMonitor*? _window_find_monitor_x11(GfxWindow* win)
{
	XWindowAttributes attrs;
	if (x11::xGetWindowAttributes(_gfx_ctx.display, win.src.window, &attrs) == 0) return NOT_FOUND~;
	for (GfxMonitorNode* node = _gfx_ctx.monitors.list.head; node != null; node = node.next)
	{
		if ((attrs.x < node.mon.origin.x + node.mon.mode.size.x) && (attrs.x + attrs.width > node.mon.origin.x) &&
		(attrs.y < node.mon.origin.y + node.mon.mode.size.y) && (attrs.y + attrs.height > node.mon.origin.y))
		{
			return &node.mon;
		}
	}
	return _gfx_ctx.monitors.list.head ? &_gfx_ctx.monitors.list.head.mon : NOT_FOUND~;
}

// public
fn void _window_show_x11(GfxWindow* win)
{
	win.internal.flags.hide = false;
	if (!_window_is_hidden_native(win)) return;
	x11::xMapWindow(_gfx_ctx.display, win.src.window);
	_wait_for_show_event_x11(win);
	win.move(win.origin);
	_window_set_fullscreen_native(win, win.is_fullscreen());
}

// public
fn bool _wait_for_show_event_x11(GfxWindow* win)
{
	XEvent dummy;
	while (x11::xCheckTypedWindowEvent(_gfx_ctx.display, win.src.window, 15 /* VisibilityNotify */, &dummy) == 0)
	{
		_wait_for_event_native(time::ms(100));
	}
	return true;
}

fn void _toggle_x_maximized(GfxWindow* win, bool maximized)
{
	XEvent xev;
	xev.type = 33; // ClientMessage
	xev.xclient.window = win.src.window;
	xev.xclient.message_type = _gfx_ctx.netWmState;
	xev.xclient.format = 32;
	xev.xclient.data.l[0] = (int)maximized;
	xev.xclient.data.l[1] = (CLong)_gfx_ctx.net_wm_state_maximized_horz;
	xev.xclient.data.l[2] = (CLong)_gfx_ctx.net_wm_state_maximized_vert;
	x11::xSendEvent(_gfx_ctx.display, x11::xDefaultRootWindow(_gfx_ctx.display), 0, (1 << 20) | (1 << 19), &xev);
}

fn void _drop_url(GfxWindow* win, ZString data) => @pool()
{
	if (!data) return;
	Splitter splitter = data.str_view().tokenize("\r\n");
	while (try line = splitter.next())
	{
		line = line.trim();
		if (!line) continue;
		if (line[0] == '#') continue;
		Url? url = url::parse(tmem, line);
		if (catch url) continue;
		if (url.scheme != "file") continue;
		_data_drop_callback(win, url.path.zstr_tcopy(), url.path.len + 1, FILE);
	}
}

// public
fn void _get_visual_for_window(XVisualInfo* visual, bool transparent)
{
	visual.visual = x11::xDefaultVisual(_gfx_ctx.display, x11::xDefaultScreen(_gfx_ctx.display));
	visual.depth = x11::xDefaultDepth(_gfx_ctx.display, x11::xDefaultScreen(_gfx_ctx.display));
	if (transparent)
	{
		x11::xMatchVisualInfo(_gfx_ctx.display, x11::xDefaultScreen(_gfx_ctx.display), 32, 4 /* TrueColor */, visual);
		if (visual.depth != 32)
		{
			log::warn("Failed to load a 32-bit depth.", category: log::CATEGORY_VIDEO);
		}
	}
}

// private
fn void _window_set_x_atom(GfxWindow* win, Atom netAtom, bool fullscreen)
{
	XEvent xev;
	xev.xclient.type = 33;
	xev.xclient.send_event = 1;
	xev.xclient.message_type = _gfx_ctx.netWmState;
	xev.xclient.window = win.src.window;
	xev.xclient.format = 32;
	xev.xclient.data.l[0] = (int)fullscreen;
	xev.xclient.data.l[1] = (CLong)netAtom;
	x11::xSendEvent(_gfx_ctx.display, x11::xDefaultRootWindow(_gfx_ctx.display), 0, (1 << 19) | (1 << 20), &xev);
}

fn void _x11_ic_callback(void* ic, void* client_data, void* call_data)
{
	GfxWindow* win = client_data;
	win.src.ic = null;
}

fn void _x11_im_callback(void* im, void* client_data, void* call_data)
{
	_gfx_ctx.im = null;
}

// public
fn void _x11_im_init_callback(XDisplay* d, XPointer client_data, XPointer call_data)
{
	if (_gfx_ctx.im != null) return;

	_gfx_ctx.im = x11::xOpenIM(_gfx_ctx.display, null, null, null);
	if (_gfx_ctx.im == null) return;

	bool found = false;
	XImStyles* styles = null;

	if (x11::xGetIMValues(_gfx_ctx.im, "queryInputStyle", &styles, (char*)null) != null)
	{
		found = false;
	}
	else
	{
		for (uint i = 0; i < styles.count_styles; i++)
		{
			if (styles.supported_styles[i] == (0x0008 | 0x0400) /* XIMPreeditNothing | XIMStatusNothing */)
			{
				found = true;
				break;
			}
		}
		x11::xFree(styles);
	}

	if (!found)
	{
		x11::xCloseIM(_gfx_ctx.im);
		_gfx_ctx.im = null;
		return;
	}

	XImCallback callback;
	callback.callback = &_x11_im_callback;
	callback.client_data = null;
	x11::xSetIMValues(_gfx_ctx.im, "destroyCallback", &callback, (char*)null);
}

fn int _x_error_handler(XDisplay* display, XErrorEvent* ev)
{
	char[512] errorText;
	x11::xGetErrorText(display, ev.error_code, (char*)&errorText, 512);
	log::warn("X Error callback :%s", (ZString)&errorText, category: log::CATEGORY_VIDEO);
	_gfx_ctx.x11Error = ev;
	return 0;
}

// public
fn void* _x_get_mode(XRRCrtcInfo* ci, XRRScreenResources* res, RRMode mode, GfxMonitorMode* found_mode)
{
	XRRModeInfoNative* mi = null;
	for (int j = 0; j < res.nmode; j++) if (res.modes[j].id == mode) mi = &res.modes[j];
	if (!mi) return null;

	found_mode.size = { (int)mi.width, (int)mi.height };
	if (ci.rotation == 4 || ci.rotation == 8) // RR_Rotate_90 / RR_Rotate_270
	{
		found_mode.size = found_mode.size.yx;
	}

	_split_bpp((uint)x11::xDefaultDepth(_gfx_ctx.display, x11::xDefaultScreen(_gfx_ctx.display)), found_mode);
	found_mode.src = (void*)(uptr)mode;
	found_mode.refresh_rate = 0;
	if (mi.hTotal == 0 || mi.vTotal == 0) return mi;

	uint vTotal = mi.vTotal;
	int numerator = (int)mi.dotClock;
	int denominator = (int)(mi.hTotal * vTotal);
	if (denominator <= 0) denominator = 1;
	float refresh_rate = (float)numerator / (float)denominator;
	found_mode.refresh_rate = (float)((int)(refresh_rate * 100 + 0.5f)) / 100.0f;
	return mi;
}

// public
fn void _x_get_system_content_dpi(float* dpi)
{
	if (dpi == null) return;
	float dpiOutput = 96.0f;
	ZString rms = x11::xResourceManagerString(_gfx_ctx.display);
	if (rms == null) return;
	void* db = x11::xrmGetStringDatabase(rms);
	if (db == null) return;
	ZString type = null;
	// xrmGetResource's value-out parameter is a small {len,addr} struct; omitted here.
	x11::xrmDestroyDatabase(db);
	*dpi = dpiOutput;
}

// public
fn void _x_handle_clipboard_selection(XEvent* event)
{
	XSelectionRequestEvent* request = &event.xselectionrequest;
	Atom[2] formats = {_gfx_ctx.utf8String, 31 /* XA_STRING */};

	if (request.target == _gfx_ctx.targets)
	{
		Atom[4] targets = {_gfx_ctx.targets, _gfx_ctx.multiple, _gfx_ctx.utf8String, 31};
		x11::xChangeProperty(_gfx_ctx.display, request.requestor, request.property, 4 /* XA_ATOM */, 32, 0, (char*)&targets, 4);
	}
	else if (request.target == _gfx_ctx.multiple)
	{
		Atom* targets = null;
		Atom actualType;
		int actualFormat;
		CULong count, bytesAfter;
		x11::xGetWindowProperty(_gfx_ctx.display, request.requestor, request.property, 0, ~0, 0, _gfx_ctx.atomPair,
		&actualType, &actualFormat, &count, &bytesAfter, (char**)&targets);

		for (CULong i = 0; i < count; i += 2)
		{
			if (targets[i] == _gfx_ctx.utf8String || targets[i] == 31 /* XA_STRING */)
			{
				x11::xChangeProperty(_gfx_ctx.display, request.requestor, targets[i + 1], targets[i],
				8, 0, _gfx_ctx.unixClipboard.text, (int)_gfx_ctx.unixClipboard.text.len);
			}
			else
			{
				targets[i + 1] = 0; // None
			}
		}

		x11::xChangeProperty(_gfx_ctx.display, request.requestor, request.property, _gfx_ctx.atomPair, 32, 0, (char*)targets, (int)count);
		x11::xFlush(_gfx_ctx.display);
		x11::xFree(targets);
	}
	else if (request.target == _gfx_ctx.saveTargets)
	{
		x11::xChangeProperty(_gfx_ctx.display, request.requestor, request.property, 0, 32, 0, null, 0);
	}
	else
	{
		for (sz i = 0; i < 2; i++)
		{
			if (request.target != formats[i]) continue;
			x11::xChangeProperty(_gfx_ctx.display, request.requestor, request.property, request.target, 8, 0, _gfx_ctx.unixClipboard.text, (int)_gfx_ctx.unixClipboard.text.len);
		}
	}

	XEvent reply;
	reply.type = 31; // SelectionNotify
	reply.xselection.property = request.property;
	reply.xselection.display = request.display;
	reply.xselection.requestor = request.requestor;
	reply.xselection.selection = request.selection;
	reply.xselection.target = request.target;
	reply.xselection.time = request.time;
	x11::xSendEvent(_gfx_ctx.display, request.requestor, 0, 0, &reply);
	x11::xFlush(_gfx_ctx.display);
}

// private
fn int x_handle_clipboard_selection_helper()
{
	XEvent event;
	x11::xPending(_gfx_ctx.display);
	if (x11::xQLength(_gfx_ctx.display) != 0 || x11::xEventsQueued(_gfx_ctx.display, 0) + x11::xEventsQueued(_gfx_ctx.display, 1) != 0)
	{
		x11::xNextEvent(_gfx_ctx.display, &event);
	}
	else
	{
		return 0;
	}

	if (event.type == 30) { _x_handle_clipboard_selection(&event); return 0; } // SelectionRequest
	return 0;
}

// public
fn GfxFormat _x_image_get_format(void* imagePtr)
{
	// XImage*: bits_per_pixel/red_mask/green_mask/blue_mask live at a fixed
	// offset in the real struct; here we only ever set the format ourselves
	// right after xCreateImage, so this mirrors the original's fallback path.
	return BGRA8;
}

// public
fn void _window_opengl_swap_interval_x11(GfxWindow* win, int swapInterval)
{
	switch
	{
		case _glx_swap_interval_ext_fn != null:
			_glx_swap_interval_ext_fn(_gfx_ctx.display, win.src.ctx.native.window, swapInterval);
		case _glx_swap_interval_native_fn != null:
			_glx_swap_interval_native_fn(swapInterval);
		default:
			log::warn("Failed to load swap interval function", category: log::CATEGORY_VIDEO);
	}
}


// --- wgpu

const WGPUSType WGPU_STYPE_SURFACE_SOURCE_XLIB_WINDOW = 0x00000006;
struct WgpuSurfaceSourceXlibWindow
{
	WgpuChainedStruct chain;
	XDisplay* display;
	XWindow window;
}

// --- Vulkan

struct VkXlibSurfaceCreateInfoKHR
{
	int sType;
	void* pNext;
	uint flags;
	XDisplay* dpy;
	XWindow window;
}

fn VkSurfaceKHR? _window_create_vulkan_surface_x11(GfxWindow* win, VkInstance instance) @private
{
	VkSurfaceKHR surface;
	VkCreateSurfaceKHRProc proc = (VkCreateSurfaceKHRProc)_get_instance_proc_vulkan(instance, "vkCreateXlibSurfaceKHR");
	if (!proc) return VULKAN_NOT_LOADED~;
	VkXlibSurfaceCreateInfoKHR create_info = {
		.sType = VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR,
		.dpy = _gfx_ctx.display,
		.window = win.src.window
	};
	if (proc(instance, &create_info, null, &surface)) return SURFACE_CREATION_FAILED~;
	return surface;
}

alias VkGetPhysicalDeviceXlibPresentationSupportKHRProc = fn uint(VkPhysicalDevice, uint, XDisplay*, VisualID);

// public
fn bool _vulkan_get_physical_device_presentation_support_x11(VkInstance instance, VkPhysicalDevice physical_device, uint queue_family_index)
{
	VkGetPhysicalDeviceXlibPresentationSupportKHRProc proc = (VkGetPhysicalDeviceXlibPresentationSupportKHRProc)_get_instance_proc_vulkan(instance, "vkGetPhysicalDeviceXlibPresentationSupportKHR");
	if (!proc) return false;
	XVisual* visual = x11::xDefaultVisual(_gfx_ctx.display, x11::xDefaultScreen(_gfx_ctx.display));
	uint out = proc(physical_device, queue_family_index, _gfx_ctx.display, x11::xVisualIDFromVisual(visual));
	return out != 0;
}
// --- GLX

alias GlxCreateContextAttribsARBProc = fn GLXContext(XDisplay*, GLXFBConfig, GLXContext, int, int*);
alias GlxQueryExtensionsStringProc = fn ZString(XDisplay*, int);
alias GlxChooseVisualProc = fn XVisualInfo*(XDisplay*, int, int*);
alias GlxCreateContextProc = fn GLXContext(XDisplay*, XVisualInfo*, GLXContext, int);
alias GlxMakeCurrentProc = fn int(XDisplay*, GLXDrawable, GLXContext);
alias GlxGetCurrentContextProc = fn GLXContext();
alias GlxSwapBuffersProc = fn void(XDisplay*, GLXDrawable);
alias GlxSwapIntervalEXTProc = fn void(XDisplay*, GLXDrawable, int);
alias GlxGetProcAddressProc = fn void*(ZString);
alias GlxGetVisualFromFBConfigProc = fn XVisualInfo*(XDisplay*, GLXFBConfig);
alias GlxGetFBConfigAttribProc = fn int(XDisplay*, GLXFBConfig, int, int*);
alias GlxGetProcAddressARBProc = fn void*(ZString);
alias GlxChooseFBConfigProc = fn GLXFBConfig*(XDisplay*, int, int*, int*);
alias GlxDestroyContextProc = fn void(XDisplay*, GLXContext);
alias GlxCreateWindowProc = fn GLXWindow(XDisplay*, GLXFBConfig, XWindow, int*);
alias GlxDestroyWindowProc = fn void(XDisplay*, GLXWindow);
alias GlxSwapIntervalNativeProc = fn int(int);

GlxCreateContextAttribsARBProc _glx_create_context_attribs_arb_fn;
GlxQueryExtensionsStringProc _glx_query_extensions_string_fn;
GlxChooseVisualProc _glx_choose_visual_fn;
GlxCreateContextProc _glx_create_context_fn;
GlxMakeCurrentProc _glx_make_current_fn;
GlxGetCurrentContextProc _glx_get_current_context_fn;
GlxSwapBuffersProc _glx_swap_buffers_fn;
GlxSwapIntervalEXTProc _glx_swap_interval_ext_fn;
GlxGetVisualFromFBConfigProc _glx_get_visual_from_fb_config_fn;
GlxGetFBConfigAttribProc _glx_get_fb_config_attrib_fn;
GlxGetProcAddressARBProc _glx_get_proc_address_arb_fn;
GlxChooseFBConfigProc _glx_choose_fb_config_fn;
GlxDestroyContextProc _glx_destroy_context_fn;
GlxCreateWindowProc _glx_create_window_fn;
GlxDestroyWindowProc _glx_destroy_window_fn;
GlxSwapIntervalNativeProc _glx_swap_interval_native_fn;

// --- XIm
alias XImCallbackProc = fn void(void* im, void* client_data, void* call_data);
struct XImCallback
{
	XPointer client_data;
	XImCallbackProc callback;
}

struct XImStyles
{
	ushort count_styles;
	XIMStyle* supported_styles;
}

alias XImInstantiateCallbackProc = fn void(XDisplay* d, XPointer client_data, XPointer call_data);

macro void xi_set_mask(char* mask, int event)
{
	mask[event / 8] |= (char)(1 << (event % 8));
}

macro bool xi_mask_is_set(char* mask, int event) => (mask[event / 8] & (char)(1 << (event % 8))) != 0;

const KeySym XK_F1 = 0xffbe; const KeySym XK_F2 = 0xffbf; const KeySym XK_F3 = 0xffc0; const KeySym XK_F4 = 0xffc1;
const KeySym XK_F5 = 0xffc2; const KeySym XK_F6 = 0xffc3; const KeySym XK_F7 = 0xffc4; const KeySym XK_F8 = 0xffc5;
const KeySym XK_F9 = 0xffc6; const KeySym XK_F10 = 0xffc7; const KeySym XK_F11 = 0xffc8; const KeySym XK_F12 = 0xffc9;
const KeySym XK_F13 = 0xffca; const KeySym XK_F14 = 0xffcb; const KeySym XK_F15 = 0xffcc; const KeySym XK_F16 = 0xffcd;
const KeySym XK_F17 = 0xffce; const KeySym XK_F18 = 0xffcf; const KeySym XK_F19 = 0xffd0; const KeySym XK_F20 = 0xffd1;
const KeySym XK_F21 = 0xffd2; const KeySym XK_F22 = 0xffd3; const KeySym XK_F23 = 0xffd4; const KeySym XK_F24 = 0xffd5;
const KeySym XK_F25 = 0xffd6;
const KeySym XK_SHIFT_L = 0xffe1; const KeySym XK_SHIFT_R = 0xffe2;
const KeySym XK_CONTROL_L = 0xffe3; const KeySym XK_CONTROL_R = 0xffe4;
const KeySym XK_ALT_L = 0xffe9; const KeySym XK_ALT_R = 0xffea;
const KeySym XK_SUPER_L = 0xffeb; const KeySym XK_SUPER_R = 0xffec;
const KeySym XK_CAPS_LOCK = 0xffe5; const KeySym XK_NUM_LOCK = 0xff7f; const KeySym XK_SCROLL_LOCK = 0xff14;
const KeySym XK_UP = 0xff52; const KeySym XK_DOWN = 0xff54; const KeySym XK_LEFT = 0xff51; const KeySym XK_RIGHT = 0xff53;
const KeySym XK_HOME = 0xff50; const KeySym XK_END = 0xff57;
const KeySym XK_PAGE_UP = 0xff55; const KeySym XK_PAGE_DOWN = 0xff56;
const KeySym XK_INSERT = 0xff63; const KeySym XK_MENU = 0xff67;
const KeySym XK_KP_ADD = 0xffab; const KeySym XK_KP_SUBTRACT = 0xffad;
const KeySym XK_KP_MULTIPLY = 0xffaa; const KeySym XK_KP_DIVIDE = 0xffaf; const KeySym XK_KP_EQUAL = 0xffbd;
const KeySym XK_KP_ENTER = 0xff8d; const KeySym XK_KP_DECIMAL = 0xffae;
const KeySym XK_KP_0 = 0xffb0; const KeySym XK_KP_1 = 0xffb1; const KeySym XK_KP_2 = 0xffb2; const KeySym XK_KP_3 = 0xffb3;
const KeySym XK_KP_4 = 0xffb4; const KeySym XK_KP_5 = 0xffb5; const KeySym XK_KP_6 = 0xffb6; const KeySym XK_KP_7 = 0xffb7;
const KeySym XK_KP_8 = 0xffb8; const KeySym XK_KP_9 = 0xffb9;
const KeySym XK_PRINT = 0xff61; const KeySym XK_PAUSE = 0xff13;
