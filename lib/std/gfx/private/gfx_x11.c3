module std::gfx::x11 @feat(LINUX, BSD) @feat(!NO_X11) @private;
import std::math, std::collections, std::os, libc, std::time, std::time::clock, std::net, std::net::os;
import std::gfx::egl, std::gfx @public;

const int X_TRUE = 1;
const int X_FALSE = 0;
const int NONE = 0;
const int CURRENT_TIME = 0;
const uint XKB_USE_CORE_KBD = 0x0100;
const char SHIFT_MASK = 1 << 0;
const char LOCK_MASK = 1 << 1;
const char MOD2_MASK = 1 << 4;
const char MOD3_MASK = 1 << 5;


fn void _surface_release_x11(GfxSurface* surface)
{
	if (!surface.data) return;
	if (surface.native.bitmap)
	{
		((XImageHeader*)surface.native.bitmap).data = null;
		xDestroyImage(surface.native.bitmap);
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
	xChangeProperty(gfx::_gfx.display, win.src.window, gfx::_gfx.motifWmHints, gfx::_gfx.motifWmHints, 32, 0, (char*)&hints, 5);
	if (!window_is_hidden(win))
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
	if (xi_select_events_fn) xi_select_events_fn(gfx::_gfx.display, xDefaultRootWindow(gfx::_gfx.display), &em, 1);
}

fn void _window_capture_mouse_x11(GfxWindow* win, bool state)
{
	if (state)
	{
		uint eventMask = (1 << 2) | (1 << 3) | (1 << 6); // Button press/release + pointer motion
		xGrabPointer(gfx::_gfx.display, win.src.window, X_TRUE, eventMask, 1, 1, win.src.window, 0, 0);
	}
	else
	{
		xUngrabPointer(gfx::_gfx.display, 0);
	}
}

fn WGPUSurface _window_create_wgpu_surface_x11(GfxWindow* window, WGPUInstance instance)
{
	WgpuSurfaceSourceXlibWindow fromXlib;
	fromXlib.chain.next = null;
	fromXlib.chain.sType = gfx::WGPU_STYPE_SURFACE_SOURCE_XLIB_WINDOW;
	fromXlib.display = gfx::_gfx.display;
	fromXlib.window = window.src.window;
	WgpuSurfaceDescriptor surfaceDesc;
	surfaceDesc.nextInChain = (WgpuChainedStruct*)&fromXlib;
	return gfx::wgpuInstanceCreateSurface(instance, &surfaceDesc);
}

// public
fn void _window_set_aspect_ratio_x11(GfxWindow* win, int w, int h)
{
	if (w == 0 && h == 0) return;
	XSizeHints hints;
	CLong flags;
	xGetWMNormalHints(gfx::_gfx.display, win.src.window, &hints, &flags);
	hints.flags |= (1 << 6); // PAspect
	hints.min_aspect.x = w; hints.max_aspect.x = w;
	hints.min_aspect.y = h; hints.max_aspect.y = h;
	xSetWMNormalHints(gfx::_gfx.display, win.src.window, &hints);
}

// public
fn void window_blit_surface(GfxWindow* win, GfxSurface* surface)
{
	GfxPoint min = math::min(win.size, surface.size);
	gfx::copy_image_data(surface.native.buffer, surface.size.x, min.y, surface.native.format, surface.data, surface.format, surface.convert_fn);
	xPutImage(gfx::_gfx.display, win.src.window, win.src.gc, surface.native.bitmap, 0, 0, 0, 0, (uint)min.x, (uint)min.y);
}

fn GfxPoint get_global_mouse()
{
	int x, y, fX, fY;
	uint z;
	XWindow window1, window2;
	xQueryPointer(gfx::_gfx.display, xDefaultRootWindow(gfx::_gfx.display), &window1, &window2, &fX, &fY, &x, &y, &z);
	return { fX, fY };
}

// public
fn GfxPoint window_fetch_size(GfxWindow* win)
{
	XWindowAttributes attribs;
	xGetWindowAttributes(gfx::_gfx.display, win.src.window, &attribs);
	win.size = { attribs.width, attribs.height };
	return win.get_size();
}

// public
fn void _window_set_mouse_passthrough_x11(GfxWindow* win, bool passthrough)
{
	if (passthrough)
	{
		Region region = xCreateRegion();
		if (xshape_combine_region_fn) xshape_combine_region_fn(gfx::_gfx.display, win.src.window, 2, 0, 0, region, 0);
		xDestroyRegion(region);
		return;
	}
	if (xshape_combine_mask_fn) xshape_combine_mask_fn(gfx::_gfx.display, win.src.window, 2, 0, 0, 0, 0);
}

fn GfxGammaRamp? monitor_get_gamma_ramp(GfxMonitor* monitor, Allocator a)
{
	sz size = (sz)xRRGetCrtcGammaSize(gfx::_gfx.display, monitor.node.crtc);
	if (!size) return gfx::CANNOT_RETRIEVE_GAMMA~;
	XRRCrtcGamma* gamma = xRRGetCrtcGamma(gfx::_gfx.display, monitor.node.crtc);
	if (!gamma) return gfx::CANNOT_RETRIEVE_GAMMA~;
	defer xRRFreeGamma(gamma);
	ushort[] rgb = alloc::alloc_array(a, ushort, size * 3);
	ushort[] red = rgb[:size];
	ushort[] green = rgb[size:size];
	ushort[] blue = rgb[size * 2:size];
	red[:size] = gamma.red[:size];
	green[:size] = gamma.green[:size];
	blue[:size] = gamma.blue[:size];
	return { red, green, blue, a };
}

fn void? monitor_set_gamma_ramp(GfxMonitor* monitor, GfxGammaRamp ramp)
{
	sz size = (sz)xRRGetCrtcGammaSize(gfx::_gfx.display, monitor.node.crtc);
	if (size != ramp.red.len) return gfx::GAMMA_COULD_NOT_BE_SET~;

	XRRCrtcGamma* gamma = xRRAllocGamma((int)size);
	if (!gamma) return gfx::GAMMA_COULD_NOT_BE_SET~;
	defer xRRFreeGamma(gamma);

	gamma.red[:size] = ramp.red[..];
	gamma.green[:size] = ramp.green[..];
	gamma.blue[:size] = ramp.blue[..];
	xRRSetCrtcGamma(gfx::_gfx.display, monitor.node.crtc, gamma);
}

fn void? monitor_set_gamma(GfxMonitor* monitor, float gamma)
{
	sz size = (sz)xRRGetCrtcGammaSize(gfx::_gfx.display, monitor.node.crtc);
	if (!size) return gfx::GAMMA_COULD_NOT_BE_SET~;

	XRRCrtcGamma* gamma_data = xRRAllocGamma((int)size);
	defer xRRFreeGamma(gamma_data);
	for (sz i = 0; i < size; i++)
	{
		ushort g = (ushort)gfx::_gamma_for_index(gamma, i, size);
		gamma_data.red[i] = g;
		gamma_data.green[i] = g;
		gamma_data.blue[i] = g;
	}
	xRRSetCrtcGamma(gfx::_gfx.display, monitor.node.crtc, gamma_data);
}

fn GfxMouse*? create_mouse(char* data, GfxSize size, GfxFormat format)
{
	if (!xcursor_image_create_fn) return gfx::IMAGE_COULD_NOT_LOAD~;
	XcursorImage* native = xcursor_image_create_fn(...size);
	if (!native) return gfx::IMAGE_COULD_NOT_LOAD~;
	defer xcursor_image_destroy_fn(native);
	sz bytesize = (sz)size.x * size.y * 4;
	native.xhot = 0;
	native.yhot = 0;
	mem::clear(native.pixels, bytesize);
	mem::copy(native.pixels, data, bytesize);
	XCursor cursor = xcursor_image_load_cursor_fn(gfx::_gfx.display, native);
	if (!cursor) return gfx::MOUSE_NOT_SUPPORTED~;
	return (GfxMouse*)cursor;
}

fn GfxMouse*? create_standard_mouse(GfxMouseIcon mouse)
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
		default: return gfx::MOUSE_NOT_SUPPORTED~;
	}
	XCursor cursor = xCreateFontCursor(gfx::_gfx.display, mouseIcon);
	if (!cursor) return gfx::MOUSE_NOT_SUPPORTED~;
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
	win.src.parent = xDefaultRootWindow(gfx::_gfx.display);

	Colormap cmap = xCreateColormap(gfx::_gfx.display, win.src.parent, visual.visual, 0);
	swa.colormap = cmap;
	swa.event_mask = eventMask;
	swa.background_pixmap = 0;

	win.src.window = xCreateWindow(gfx::_gfx.display, win.src.parent, win.x, win.y, (uint)win.width, (uint)win.height,
	0, visual.depth, 1 /* InputOutput */, visual.visual, (1 << 3) | (1 << 13) | (1 << 11), &swa);
	// CWBorderPixel | CWColormap | CWEventMask
	if (!win.src.window) return gfx::X11_WINDOW_CREATION_FAILED~;

	win.src.flashEnd = {};
	xFreeColors(gfx::_gfx.display, cmap, null, 0, 0);
	xSaveContext(gfx::_gfx.display, win.src.window, gfx::_gfx.context, (XPointer)win);
	win.src.gc = xCreateGC(gfx::_gfx.display, win.src.window, 0, null);

	if (gfx::_gfx.im != null)
	{
		XImCallback callback;
		callback.callback = &_x11_ic_callback;
		callback.client_data = (XPointer)win;

		win.src.ic = xCreateIC(gfx::_gfx.im, "inputStyle", (CULong)(0x0008 | 0x0400) /* XIMPreeditNothing | XIMStatusNothing */,
		"clientWindow", win.src.window, "focusWindow", win.src.window,
		"destroyCallback", &callback, (char*)null);
	}

	@pool()
	{
		XClassHint hint;
		hint.res_name = name.zstr_copy(tmem);
		hint.res_class = gfx::_gfx.class_name.zstr_copy(tmem);
		xSetClassHint(gfx::_gfx.display, win.src.window, &hint);
	};

	XWMHints hints;
	hints.flags = 1 << 1; // StateHint
	hints.initial_state = 1; // NormalState
	xSetWMHints(gfx::_gfx.display, win.src.window, &hints);

	xSelectInput(gfx::_gfx.display, win.src.window, eventMask);
	xSetWMProtocols(gfx::_gfx.display, win.src.window, &gfx::_gfx.wmDeleteWindow, 1);
	win.set_name(name);
	xMoveWindow(gfx::_gfx.display, win.src.window, win.x, win.y);

	if (flags.allow_dnd)
	{
		win.internal.flags.allow_dnd = true;
		char version = 5;
		xChangeProperty(gfx::_gfx.display, win.src.window, gfx::_gfx.xdndAware, 4, 32, 0, &version, 1);
	}

	win.src.bounds = win.bounds;
	xSetWindowBackground(gfx::_gfx.display, win.src.window, 0);
	xClearWindow(gfx::_gfx.display, win.src.window);
	xSetWindowBackgroundPixmap(gfx::_gfx.display, win.src.window, 0);
}

fn void? window_create_platform(GfxWindow* win, String name, GfxWindowFlags flags)
{
	win.src.backing = {};
	if (flags.open_gl || flags.egl)
	{
		win.src.window = 0;
		if (flags.open_gl) gfx::_window_create_opengl_context_native(win, gfx::get_global_opengl_hints())!;
		if (flags.egl) egl::create_context(win, gfx::get_global_opengl_hints())!;
	}
	else
	{
		XVisualInfo visual;
		_get_visual_for_window(&visual, win.internal.flags.transparent);
		_x_create_window(visual, name, flags, win)!;
	}
	gfx::_window_set_flags(win, flags, {});
}

fn void _destroy_x11()
{
	if (gfx::_gfx.display)
	{
		if (xGetSelectionOwner(gfx::_gfx.display, gfx::_gfx.clipboardAtom) == gfx::_gfx.helper_window)
		{
			xConvertSelection(gfx::_gfx.display, gfx::_gfx.clipboardManager, gfx::_gfx.saveTargets, 0, gfx::_gfx.helper_window, 0);
			while (x_handle_clipboard_selection_helper() != 0) {}
		}
		xUnregisterIMInstantiateCallback(gfx::_gfx.display, null, null, null, (void*)&_x11_im_init_callback, null);
	}
	if (gfx::_gfx.im) xCloseIM(gfx::_gfx.im);
	if (gfx::_gfx.unixClipboard) free(gfx::_gfx.unixClipboard);
	if (gfx::_gfx.hiddenMouse) free_mouse(gfx::_gfx.hiddenMouse);
	if (gfx::_gfx.helper_window) xDestroyWindow(gfx::_gfx.display, gfx::_gfx.helper_window);
	if (gfx::_gfx.display) xCloseDisplay(gfx::_gfx.display);

	dlclose_cursor_xi_ext();
}

// private
fn bool _opengl_extension_is_supported_x11(String extension)
{
	ZString extensions = _glx_query_extensions_string_fn(gfx::_gfx.display, xDefaultScreen(gfx::_gfx.display));
	return extensions && gfx::_extension_is_in_list(extensions.str_view(), extension);
}

fn void free_mouse(GfxMouse* mouse)
{
	xFreeCursor(gfx::_gfx.display, (XCursor)mouse);
}

// private
fn void* _get_current_opengl_context_x11() => _glx_get_current_context_fn();

// private
fn GfxFn _get_opengl_proc(ZString procname)
{
	if (!gfx::_gfx.gl.get_proc_addr_fn) return null;
	return gfx::_gfx.gl.get_proc_addr_fn(procname);
}

// private
fn void _init_keycodes_native_x11()
{
	// A full evdev-keycode -> _KEY table walk (xkbGetKeyboardByName) is left
	// out of this mechanical pass; keycodes[] stays zeroed (_KEY_NULL).
}

fn void? _init_x11(String class_name, GfxInitOption flags)
{
	gfx::_gfx.class_name = class_name;

	xInitThreads();
	gfx::_gfx.display = xOpenDisplay(null);
	if (!gfx::_gfx.display) return gfx::X11_DISPLAY_NOT_LOADED~;

	gfx::_gfx.netCurrentDesktop = xInternAtom(gfx::_gfx.display, "_NET_CURRENT_DESKTOP", 0);
	gfx::_gfx.net_wm_state_maximized_vert = xInternAtom(gfx::_gfx.display, "_NET_WM_STATE_MAXIMIZED_VERT", 0);
	gfx::_gfx.net_wm_state_maximized_horz = xInternAtom(gfx::_gfx.display, "_NET_WM_STATE_MAXIMIZED_HORZ", 0);
	gfx::_gfx.netWmBypassCompositor = xInternAtom(gfx::_gfx.display, "_NET_WM_BYPASS_COMPOSITOR", 0);
	gfx::_gfx.netWmState = xInternAtom(gfx::_gfx.display, "_NET_WM_STATE", 0);
	gfx::_gfx.motifWmHints = xInternAtom(gfx::_gfx.display, "_MOTIF_WM_HINTS", 0);
	gfx::_gfx.wmDeleteWindow = xInternAtom(gfx::_gfx.display, "WM_DELETE_WINDOW", 0);
	gfx::_gfx.netWmSyncRequestCounter = xInternAtom(gfx::_gfx.display, "_NET_WM_SYNC_REQUEST_COUNTER", 0);
	gfx::_gfx.netWmSyncRequest = xInternAtom(gfx::_gfx.display, "_NET_WM_SYNC_REQUEST", 0);
	gfx::_gfx.atomPair = xInternAtom(gfx::_gfx.display, "ATOM_PAIR", 0);
	gfx::_gfx.multiple = xInternAtom(gfx::_gfx.display, "MULTIPLE", 0);
	gfx::_gfx.targets = xInternAtom(gfx::_gfx.display, "TARGETS", 0);
	gfx::_gfx.saveTargets = xInternAtom(gfx::_gfx.display, "SAVE_TARGETS", 0);
	gfx::_gfx.utf8String = xInternAtom(gfx::_gfx.display, "UTF8_STRING", 0);
	gfx::_gfx.xdndTypeList = xInternAtom(gfx::_gfx.display, "XdndTypeList", 0);
	gfx::_gfx.xdndSelection = xInternAtom(gfx::_gfx.display, "XdndSelection", 0);
	gfx::_gfx.xdndAware = xInternAtom(gfx::_gfx.display, "XdndAware", 0);
	gfx::_gfx.xdndEnter = xInternAtom(gfx::_gfx.display, "XdndEnter", 0);
	gfx::_gfx.xdndPosition = xInternAtom(gfx::_gfx.display, "XdndPosition", 0);
	gfx::_gfx.xdndStatus = xInternAtom(gfx::_gfx.display, "XdndStatus", 0);
	gfx::_gfx.xdndLeave = xInternAtom(gfx::_gfx.display, "XdndLeave", 0);
	gfx::_gfx.xdndDrop = xInternAtom(gfx::_gfx.display, "XdndDrop", 0);
	gfx::_gfx.xdndFinished = xInternAtom(gfx::_gfx.display, "XdndFinished", 0);
	gfx::_gfx.xdndActionCopy = xInternAtom(gfx::_gfx.display, "XdndActionCopy", 0);
	gfx::_gfx.wmProtocols = xInternAtom(gfx::_gfx.display, "WM_PROTOCOLS", 0);
	gfx::_gfx.wmState = xInternAtom(gfx::_gfx.display, "WM_STATE", 0);
	gfx::_gfx.netWmStateFullscreen = xInternAtom(gfx::_gfx.display, "_NET_WM_STATE_FULLSCREEN", 0);
	gfx::_gfx.netWmStateAbove = xInternAtom(gfx::_gfx.display, "_NET_WM_STATE_ABOVE", 0);
	gfx::_gfx.netWmWindowOpacity = xInternAtom(gfx::_gfx.display, "_NET_WM_WINDOW_OPACITY", 0);
	gfx::_gfx.netWmName = xInternAtom(gfx::_gfx.display, "_NET_WM_NAME", 0);
	gfx::_gfx.netWmIcon = xInternAtom(gfx::_gfx.display, "_NET_WM_ICON", 0);
	gfx::_gfx.xselData = xInternAtom(gfx::_gfx.display, "XSEL_DATA", 0);
	gfx::_gfx.clipboardAtom = xInternAtom(gfx::_gfx.display, "CLIPBOARD", 0);
	gfx::_gfx.clipboardManager = xInternAtom(gfx::_gfx.display, "CLIPBOARD_MANAGER", 0);
	gfx::_gfx.netWorkarea = xInternAtom(gfx::_gfx.display, "_NET_WORKAREA", 0);
	gfx::_gfx.xtextUriList = xInternAtom(gfx::_gfx.display, "text/uri-list", 0);
	gfx::_gfx.xtextPlain = xInternAtom(gfx::_gfx.display, "text/plain", 0);

	gfx::_gfx.context = xUniqueContext();
	if (!gfx::_gfx.context) return gfx::X11_CTX_CREATION_FAILED~;

	XSetWindowAttributes wa;
	wa.event_mask = 1 << 22; // PropertyChangeMask
	gfx::_gfx.helper_window = xCreateWindow(gfx::_gfx.display, xDefaultRootWindow(gfx::_gfx.display), 0, 0, 1, 1, 0, 0,
		2 /* InputOnly */, xDefaultVisual(gfx::_gfx.display, xDefaultScreen(gfx::_gfx.display)), 1 << 11, &wa);
	if (gfx::_gfx.helper_window == 0) return gfx::X11_WINDOW_CREATION_FAILED~;

	dload_cursor_xi_ext();

	char[4] blk;
	gfx::_gfx.hiddenMouse = gfx::_create_mouse((void*)&blk, { 1, 1 }, RGBA8) ?? null;
	gfx::_gfx.unixClipboard = null;

	xSetErrorHandler(&_x_error_handler);
	xSync(gfx::_gfx.display, 0);

	xSetLocaleModifiers("");
	xRegisterIMInstantiateCallback(gfx::_gfx.display, null, null, null, (void*)&_x11_im_init_callback, null);

	int errorBase;
	if (xRRQueryExtension(gfx::_gfx.display, &gfx::_gfx.xrandrEventBase, &errorBase) != 0)
	{
		xRRSelectInput(gfx::_gfx.display, xRootWindow(gfx::_gfx.display, xDefaultScreen(gfx::_gfx.display)), 1 << 4); // RROutputChangeNotifyMask
	}
}

// private
fn void? _load_opengl_x11()
{
	if (gfx::_gfx.gl.native_handle && gfx::_gfx.gl.get_proc_addr_fn) return;
	gfx::_gfx.gl.native_handle = os::dlopen_any("libGLX.so.0", "libGL.so.1", "libGL.so") ?? gfx::MISSING_OPENGL_LIB~!;
	gfx::_gfx.gl.get_proc_addr_fn         = os::dlsym(gfx::_gfx.gl.native_handle, "glXGetProcAddress") ?: gfx::MISSING_GL_GET_PROC_ADDR~!;
	_glx_choose_visual_fn                = os::dlsym(gfx::_gfx.gl.native_handle, "glXChooseVisual");
	_glx_create_context_fn               = os::dlsym(gfx::_gfx.gl.native_handle, "glXCreateContext");
	_glx_make_current_fn                 = os::dlsym(gfx::_gfx.gl.native_handle, "glXMakeCurrent");
	_glx_get_current_context_fn          = os::dlsym(gfx::_gfx.gl.native_handle, "glXGetCurrentContext");
	_glx_swap_buffers_fn                 = os::dlsym(gfx::_gfx.gl.native_handle, "glXSwapBuffers");
	_glx_get_visual_from_fb_config_fn    = os::dlsym(gfx::_gfx.gl.native_handle, "glXGetVisualFromFBConfig");
	_glx_get_fb_config_attrib_fn         = os::dlsym(gfx::_gfx.gl.native_handle, "glXGetFBConfigAttrib");
	_glx_get_proc_address_arb_fn         = os::dlsym(gfx::_gfx.gl.native_handle, "glXGetProcAddressARB");
	_glx_choose_fb_config_fn             = os::dlsym(gfx::_gfx.gl.native_handle, "glXChooseFBConfig");
	_glx_destroy_context_fn              = os::dlsym(gfx::_gfx.gl.native_handle, "glXDestroyContext");
	_glx_create_window_fn                = os::dlsym(gfx::_gfx.gl.native_handle, "glXCreateWindow");
	_glx_destroy_window_fn               = os::dlsym(gfx::_gfx.gl.native_handle, "glXDestroyWindow");
	_glx_query_extensions_string_fn      = os::dlsym(gfx::_gfx.gl.native_handle, "glXQueryExtensionsString");
	_glx_create_context_attribs_arb_fn   = os::dlsym(gfx::_gfx.gl.native_handle, "glXCreateContextAttribsARB");
	_glx_swap_interval_ext_fn            = os::dlsym(gfx::_gfx.gl.native_handle, "glXSwapIntervalEXT");
	if (!_glx_swap_interval_ext_fn)
	{
		ZString[2] array = {"GLX_MESA_swap_control", "GLX_SGI_swap_control"};
		foreach (name : array)
		{
			_glx_swap_interval_native_fn = os::dlsym(gfx::_gfx.gl.native_handle, name);
			if (_glx_swap_interval_native_fn) break;
		}
	}
}

fn void monitor_node_free(GfxMonitorNode* node) {}

// private
fn GfxMonitorMode[] monitor_get_modes(GfxMonitor* monitor)
{
	XRRScreenResources* res = xRRGetScreenResourcesCurrent(gfx::_gfx.display, xDefaultRootWindow(gfx::_gfx.display));
	if (!res) return {};
	XRRCrtcInfo* ci = xRRGetCrtcInfo(gfx::_gfx.display, res, monitor.node.crtc);
	GfxXRROutputInfo* oi = xRRGetOutputInfo(gfx::_gfx.display, res, monitor.node.rr_output);
	sz count = (sz)oi.nmode;
	List{GfxMonitorMode} modes;
	for (int i = 0; i < oi.nmode; i++)
	{
		GfxMonitorMode mode;
		_x_get_mode(ci, res, oi.modes[i], &mode);
		modes.push(mode);
	}
	xRRFreeOutputInfo(oi);
	xRRFreeCrtcInfo(ci);
	xRRFreeScreenResources(res);
	return modes.array_view();
}

fn IntRect monitor_get_workarea(GfxMonitor* monitor)
{
	XWindow root = xDefaultRootWindow(gfx::_gfx.display);
	int areaX = monitor.origin.x;
	int areaY = monitor.origin.y;
	int areaW = monitor.mode.size.x;
	int areaH = monitor.mode.size.y;

	if (gfx::_gfx.netWorkarea != 0 && gfx::_gfx.netCurrentDesktop != 0)
	{
		Atom* extents = null;
		Atom* desktop = null;
		Atom actualType; int actualFormat; CULong extentCount, bytesAfter, count;
		xGetWindowProperty(gfx::_gfx.display, root, gfx::_gfx.netWorkarea, 0, ~0, 0, 6, &actualType, &actualFormat, &extentCount, &bytesAfter, (char**)&extents);
		xGetWindowProperty(gfx::_gfx.display, root, gfx::_gfx.netCurrentDesktop, 0, ~0, 0, 6, &actualType, &actualFormat, &count, &bytesAfter, (char**)&desktop);

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
		if (extents != null) xFree(extents);
		if (desktop != null) xFree(desktop);
	}

	return { areaX, areaY, areaW, areaH };
}

fn void? monitor_request_mode(GfxMonitor* mon, GfxMonitorMode* mode, GfxMonitorModeRequest request)
{
	bool output = false;
	XRRScreenResources* res = xRRGetScreenResourcesCurrent(gfx::_gfx.display, xDefaultRootWindow(gfx::_gfx.display));
	if (!res) return gfx::MONITOR_MODE_CHANGE_FAILED~;
	XRRCrtcInfo* ci = xRRGetCrtcInfo(gfx::_gfx.display, res, mon.node.crtc);
	GfxXRROutputInfo* oi = xRRGetOutputInfo(gfx::_gfx.display, res, mon.node.rr_output);
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
	if (native != 0) xRRSetCrtcConfig(gfx::_gfx.display, res, mon.node.crtc, 0, ci.x, ci.y, native, ci.rotation, ci.outputs, ci.noutput);
	xRRFreeOutputInfo(oi);
	xRRFreeCrtcInfo(ci);
	xRRFreeScreenResources(res);
	if (!output) return gfx::MONITOR_MODE_CHANGE_FAILED~;
}

fn void? monitor_set_mode(GfxMonitor* mon, GfxMonitorMode* mode)
{
	bool out = false;
	XRRScreenResources* res = xRRGetScreenResourcesCurrent(gfx::_gfx.display, xDefaultRootWindow(gfx::_gfx.display));
	XRRCrtcInfo* ci = xRRGetCrtcInfo(gfx::_gfx.display, res, mon.node.crtc);
	if (xRRSetCrtcConfig(gfx::_gfx.display, res, mon.node.crtc, 0, ci.x, ci.y, (RRMode)(uptr)mode.src, ci.rotation, ci.outputs, ci.noutput) == 0) out = true;
	xRRFreeCrtcInfo(ci);
	xRRFreeScreenResources(res);
	if (!out) return gfx::MONITOR_MODE_CHANGE_FAILED~;
}

// private
fn GfxKey _physical_to_mapped_key_native_x11(GfxKey key)
{
	KeyCode keycode = (KeyCode)key.to_api();
	KeySym sym = xkbKeycodeToKeysym(gfx::_gfx.display, keycode, 0, 0);
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
	fds[0].fd = xConnectionNumber(gfx::_gfx.display);

	bool infinity = duration == gfx::WAIT_FOREVER;
	bool once = true;
	Clock start = clock::now();
	while (!xPending(gfx::_gfx.display))
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
	gfx::_reset_prev_state();
	xPending(gfx::_gfx.display);
	while (xQLength(gfx::_gfx.display)) _x_handle_event();
}

fn void _x_handle_event()
{
	static Vec2 delta = {};

	XEvent e;
	xNextEvent(gfx::_gfx.display, &e);

	if (e.type != 35 /* GenericEvent */)
	{
		delta = {};
	}

	if (e.type == gfx::_gfx.xrandrEventBase + 0 /* RRNotify */)
	{
		poll_monitors();
		return;
	}

	if (e.type == 30 /* SelectionRequest */)
	{
		_x_handle_clipboard_selection(&e);
		return;
	}

	if (e.type == 35 /* GenericEvent */)
	{
		xGetEventData(gfx::_gfx.display, &e.xcookie);
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
				gfx::_gfx.vector = delta;
				gfx::_raw_motion_callback(gfx::_gfx.root, delta);
			}
		}
		xFreeEventData(gfx::_gfx.display, &e.xcookie);
		return;
	}

	GfxWindow* win = null;
	if (xFindContext(gfx::_gfx.display, e.xany.window, gfx::_gfx.context, (XPointer*)&win) != 0) return;

	if (win.src.flashEnd != (Clock)0)
	{
		if (win.src.flashEnd.to_now() > time::NANO_DURATION_ZERO || win.has_focus()) win.flash(CANCEL);
	}

	bool keyRepeat = false;
	if (e.type == 3 /* KeyRelease */ && xEventsQueued(gfx::_gfx.display, 1) != 0)
	{
		XEvent ne;
		xPeekEvent(gfx::_gfx.display, &ne);
		if (ne.type == 2 /* KeyPress */ && e.xkey.time == ne.xkey.time && e.xkey.keycode == ne.xkey.keycode)
		{
			xNextEvent(gfx::_gfx.display, &e);
			keyRepeat = true;
		}
	}

	switch (e.type)
	{
		case 2: // KeyPress
			{
				if (!win.internal.enabled_events.key_pressed) return;
				GfxKey value = gfx::key_from_api(e.xkey.keycode);

				XkbStateRec state;
				xkbGetState(gfx::_gfx.display, XKB_USE_CORE_KBD, &state);
				gfx::_key_update_key_mods(win, state.locked_mods & LOCK_MASK != 0, state.locked_mods & MOD2_MASK != 0, state.locked_mods & MOD3_MASK != 0);

				if (win.src.ic != null && xFilterEvent(&e, 0) == 0)
				{
					char[100] buffer;
					int status;
					sz count = xutf8LookupString(win.src.ic, &e.xkey, (char*)&buffer, 99, null, &status);
					if (status == 2 /* XLookupChars */ || status == 4 /* XLookupBoth */)
					{
						buffer[count] = 0;
						StringIterator iterator = ((ZString)&buffer).str_view().iterator();
						while (try c32 = iterator.next())
						{
							gfx::_key_char_callback(win, c32);
						}
					}
				}
				else
				{
					XWindow rootRet, childRet;
					int rootX, rootY, winX, winY;
					uint mask;
					xQueryPointer(gfx::_gfx.display, xDefaultRootWindow(gfx::_gfx.display), &rootRet, &childRet, &rootX, &rootY, &winX, &winY, &mask);
					KeySym sym = xkbKeycodeToKeysym(gfx::_gfx.display, (KeyCode)e.xkey.keycode, 0, (mask & SHIFT_MASK) ? 1 : 0);

					if ((mask & LOCK_MASK) && sym >= 0x0061 /* XK_a */ && sym <= 0x007a /* XK_z */)
					{
						sym = (mask & SHIFT_MASK) ? sym + 32 : sym - 32;
					}
					if ((KeySym)(char)sym != sym) sym = 0;

					gfx::_key_char_callback(win, (uint)(char)sym);
				}

				gfx::_key_callback(win, value, win.internal.mod, keyRepeat, true);
				break;
			}
		case 3: // KeyRelease
			{
				if (!win.internal.enabled_events.key_released) return;
				GfxKey value = gfx::key_from_api(e.xkey.keycode);

				XkbStateRec state;
				xkbGetState(gfx::_gfx.display, XKB_USE_CORE_KBD, &state);
				gfx::_key_update_key_mods(win, state.locked_mods & LOCK_MASK != 0, state.locked_mods & MOD2_MASK != 0, state.locked_mods & MOD3_MASK != 0);

				gfx::_key_callback(win, value, win.internal.mod, false, false);
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
					gfx::_mouse_scroll_callback(win, scroll_delta);
					break;
				}
				gfx::_mouse_button_callback(win, value, true);
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
				gfx::_mouse_button_callback(win, value, false);
				break;
			}
		case 6: gfx::_mouse_motion_callback(win, { e.xmotion.x, e.xmotion.y }); // MotionNotify
		case 12: gfx::_window_refresh_callback(win, { e.xexpose.x, e.xexpose.y, e.xexpose.width, e.xexpose.height }); // Expose
		case 28: // PropertyNotify
			if (e.xproperty.state == 0 /* PropertyNewValue */)
			{
				bool handled = false;
				if (e.xproperty.atom == gfx::_gfx.wmState)
				{
					if (window_is_minimized(win) && !(win.internal.flags.minimize))
					{
						gfx::_window_minimized_callback(win);
						handled = true;
					}
				}
				else if (e.xproperty.atom == gfx::_gfx.netWmState)
				{
					if (window_is_maximized(win) && !(win.internal.flags.maximize))
					{
						gfx::_window_maximized_callback(win, win.bounds);
						handled = true;
					}
				}
				if (!handled) gfx::_window_check_mode(win);
			}
		case 19: case 18: // MapNotify, UnmapNotify
			gfx::_window_check_mode(win);
			break;
		case 33: // ClientMessage
			{
				if ((CULong)e.xclient.data.l[0] == gfx::_gfx.wmDeleteWindow)
				{
					gfx::_window_close_callback(win);
					break;
				}

				if (!(win.internal.flags.allow_dnd) || gfx::_gfx.x11Version > 5 /* _XDND_VERSION */) return;

				GfxPoint drag;

				if (e.xclient.message_type == gfx::_gfx.xdndEnter)
				{
					CULong count;
					Atom* formats;
					Atom[3] realFormats;
					int list = (int)(e.xclient.data.l[1] & 1);

					gfx::_gfx.x11Source = (XWindow)e.xclient.data.l[0];
					gfx::_gfx.x11Version = (int)(e.xclient.data.l[1] >> 24);
					gfx::_gfx.x11Format = 0;
					if (list)
					{
						Atom actualType;
						int actualFormat;
						CULong bytesAfter;
						xGetWindowProperty(gfx::_gfx.display, gfx::_gfx.x11Source, gfx::_gfx.xdndTypeList, 0, ~0, 0, 4,
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
						if (formats[i] == gfx::_gfx.xtextUriList)
						{
							gfx::_gfx.x11TransferType = FILE;
						}
						else if (formats[i] == gfx::_gfx.xtextPlain)
						{
							gfx::_gfx.x11TransferType = TEXT;
						}
						else
						{
							continue;
						}

						gfx::_gfx.x11Format = (int)formats[i];
						break;
					}

					if (list != 0 && formats != null) xFree(formats);

					gfx::_data_drag_callback(win, gfx::_gfx.x11TransferType, ENTER, drag);
				}
				else if (e.xclient.message_type == gfx::_gfx.xdndPosition)
				{
					int xabs = (int)((e.xclient.data.l[2] >> 16) & 0xffff);
					int yabs = (int)(e.xclient.data.l[2] & 0xffff);
					XWindow dummy;
					int xpos, ypos;

					xTranslateCoordinates(gfx::_gfx.display, xDefaultRootWindow(gfx::_gfx.display), win.src.window,
					xabs, yabs, &xpos, &ypos, &dummy);

					drag = { xpos, ypos };

					gfx::_mouse_motion_callback(win, drag);

					XEvent reply;
					reply.type = 33; // ClientMessage
					reply.xclient.window = gfx::_gfx.x11Source;
					reply.xclient.message_type = gfx::_gfx.xdndStatus;
					reply.xclient.format = 32;
					reply.xclient.data.l[0] = (CLong)win.src.window;
					reply.xclient.data.l[2] = 0;
					reply.xclient.data.l[3] = 0;

					if (gfx::_gfx.x11Format != 0)
					{
						reply.xclient.data.l[1] = 1;
						if (gfx::_gfx.x11Version >= 2) reply.xclient.data.l[4] = (CLong)gfx::_gfx.xdndActionCopy;
					}

					xSendEvent(gfx::_gfx.display, gfx::_gfx.x11Source, 0, 0, &reply);
					xFlush(gfx::_gfx.display);

					gfx::_data_drag_callback(win, gfx::_gfx.x11TransferType, MOVE, drag);
				}
				else if (e.xclient.message_type == gfx::_gfx.xdndLeave)
				{
					gfx::_data_drag_callback(win, gfx::_gfx.x11TransferType, EXIT, drag);
				}
				else if (e.xclient.message_type == gfx::_gfx.xdndDrop)
				{
					if (gfx::_gfx.x11Format != 0)
					{
						Time_t time = gfx::_gfx.x11Version >= 1 ?(Time_t)e.xclient.data.l[2] : 0; // CurrentTime
						xConvertSelection(gfx::_gfx.display, gfx::_gfx.xdndSelection, (Atom)gfx::_gfx.x11Format,
						gfx::_gfx.xdndSelection, win.src.window, time);
					}
					else if (gfx::_gfx.x11Version >= 2)
					{
						XEvent reply;
						reply.type = 33; // ClientMessage
						reply.xclient.window = gfx::_gfx.x11Source;
						reply.xclient.message_type = gfx::_gfx.xdndFinished;
						reply.xclient.format = 32;
						reply.xclient.data.l[0] = (CLong)win.src.window;
						reply.xclient.data.l[1] = 0;
						reply.xclient.data.l[2] = 0;

						xSendEvent(gfx::_gfx.display, gfx::_gfx.x11Source, 0, 0, &reply);
						xFlush(gfx::_gfx.display);
					}
				}
				break;
			}
		case 31: // SelectionNotify
			{
				if (!win.internal.enabled_events.data_drop || e.xselection.property != gfx::_gfx.xdndSelection || !(win.internal.flags.allow_dnd)) return;

				ZString data;
				CULong result;
				Atom actualType;
				int actualFormat;
				CULong bytesAfter;

				xGetWindowProperty(gfx::_gfx.display, e.xselection.requestor, e.xselection.property, 0, ~0, 0, e.xselection.target,
				&actualType, &actualFormat, &result, &bytesAfter, (char**)&data);

				if (result != 0)
				{
					_drop_url(win, data);
					if (data != null) xFree(data);
				}

				if (gfx::_gfx.x11Version >= 2)
				{
					XEvent reply;
					reply.type = 33; // ClientMessage
					reply.xclient.window = gfx::_gfx.x11Source;
					reply.xclient.message_type = gfx::_gfx.xdndFinished;
					reply.xclient.format = 32;
					reply.xclient.data.l[0] = (CLong)win.src.window;
					reply.xclient.data.l[1] = (CLong)result;
					reply.xclient.data.l[2] = (CLong)gfx::_gfx.xdndActionCopy;
					xSendEvent(gfx::_gfx.display, gfx::_gfx.x11Source, 0, 0, &reply);
					xFlush(gfx::_gfx.display);
				}
				break;
			}
		case 22: // ConfigureNotify
			{
				if (e.xconfigure.width != win.src.width || e.xconfigure.height != win.src.height)
				{
					gfx::_window_check_mode(win);
					win.src.size = { e.xconfigure.width, e.xconfigure.height };
					gfx::_window_resized_callback(win, win.src.size);
				}
				GfxPoint configure = { e.xconfigure.x, e.xconfigure.y };

				// if the event came from the server and we're not a direct child of the root window then
				// we're using local coords which need to be translated into screen coords
				XWindow root = xDefaultRootWindow(gfx::_gfx.display);
				if (e.xany.send_event == 0 && win.src.parent != root)
				{
					XWindow dummy;
					xTranslateCoordinates(gfx::_gfx.display, win.src.parent, root, ...configure, &configure.x, &configure.y, &dummy);
				}

				if (configure != win.origin)
				{
					win.src.origin = configure;
					gfx::_window_moved_callback(win, configure);
				}
				return;
			}
		case 9: // FocusIn
			if (win.src.ic) xSetICFocus(win.src.ic);
			gfx::_window_focus_callback(win, true);
		case 10: // FocusOut
			if (win.src.ic) xUnsetICFocus(win.src.ic);
			gfx::_window_focus_callback(win, false);
		case 7: gfx::_mouse_notify_callback(win, { e.xcrossing.x, e.xcrossing.y }, true); // EnterNotify
		case 8: gfx::_mouse_notify_callback(win, win.internal.last_mouse, false); // LeaveNotify
		case 21: win.src.parent = e.xreparent.parent; // ReparentNotify
		default: break;
	}

	xFlush(gfx::_gfx.display);
}

// private
fn void poll_monitors()
{
	XWindow root = xDefaultRootWindow(gfx::_gfx.display);
	XRRScreenResources* res = xRRGetScreenResourcesCurrent(gfx::_gfx.display, root);
	if (res == null) return;
	RROutput primary = xRRGetOutputPrimary(gfx::_gfx.display, root);

	for (GfxMonitorNode* node = gfx::_gfx.monitors.list.head; node != null; node = node.next) node.disconnected = true;

	for (int i = 0; i < res.noutput; i++)
	{
		GfxMonitorNode* node = null;
		for (node = gfx::_gfx.monitors.list.head; node != null; node = node.next)
		{
			if (node.rr_output == res.outputs[i]) break;
		}
		if (node != null)
		{
			node.disconnected = false;
			if (node.rr_output == primary) gfx::_gfx.monitors.primary = node;
			continue;
		}

		GfxMonitor monitor;
		GfxXRROutputInfo* info = xRRGetOutputInfo(gfx::_gfx.display, res, res.outputs[i]);
		if (info == null) continue;
		if (info.connection != 0 || info.crtc == 0) { xRRFreeOutputInfo(info); continue; }

		XRRCrtcInfo* ci = xRRGetCrtcInfo(gfx::_gfx.display, res, info.crtc);
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

		xRRFreeCrtcInfo(ci);
		node = gfx::_monitor_add(&monitor);
		if (node == null) break;
		node.rr_output = res.outputs[i];
		node.crtc = info.crtc;
		if (node.rr_output == primary) gfx::_gfx.monitors.primary = node;
		xRRFreeOutputInfo(info);
		gfx::_monitor_callback(gfx::_gfx.root, &node.mon, true);
	}

	xRRFreeScreenResources(res);
	gfx::refresh_monitors();
}

// private
fn void window_destroy_platform(GfxWindow* win)
{
	if (win.src.ic)
	{
		xDestroyIC(win.src.ic);
		win.src.ic = null;
	}
	xFreeGC(gfx::_gfx.display, win.src.gc);
	xDeleteContext(gfx::_gfx.display, win.src.window, gfx::_gfx.context);
	xDestroyWindow(gfx::_gfx.display, win.src.window);
}

fn void _window_release_opengl_context_x11(GfxWindow* win, GfxGlContext* ctx)
{
	if (win.src.ctx.native.window != win.src.window) _glx_destroy_window_fn(gfx::_gfx.display, win.src.ctx.native.window);
	_glx_destroy_context_fn(gfx::_gfx.display, ctx.ctx);
	win.src.ctx.native = null;
}


fn void window_focus(GfxWindow* win)
{
	XWindowAttributes attr;
	xGetWindowAttributes(gfx::_gfx.display, win.src.window, &attr);
	if (attr.map_state != 2 /* IsViewable */) return;
	xSetInputFocus(gfx::_gfx.display, win.src.window, 1 /* RevertToPointerRoot */, 0);
	xFlush(gfx::_gfx.display);
}

fn void window_hide(GfxWindow* win)
{
	win.internal.flags.hide = true;
	xUnmapWindow(gfx::_gfx.display, win.src.window);
	xFlush(gfx::_gfx.display);
}

fn bool window_is_hidden(GfxWindow* win)
{
	XWindowAttributes windowAttributes;
	xGetWindowAttributes(gfx::_gfx.display, win.src.window, &windowAttributes);
	return windowAttributes.map_state != 2;
}

// private
fn bool window_is_maximized(GfxWindow* win)
{
	Atom actualType;
	int actualFormat;
	CULong nitems, bytesAfter;
	char* propData;
	int status = xGetWindowProperty(gfx::_gfx.display, win.src.window, gfx::_gfx.netWmState, 0, 1024, 0, 4, &actualType, &actualFormat, &nitems, &bytesAfter, &propData);
	if (status != 0) { if (propData != null) xFree(propData); return false; }
	for (CULong i = 0; i < nitems; i++)
	{
		if ((Atom)propData[i] == gfx::_gfx.net_wm_state_maximized_vert || (Atom)propData[i] == gfx::_gfx.net_wm_state_maximized_horz)
		{
			xFree(propData);
			return true;
		}
	}
	if (propData != null) xFree(propData);
	return false;
}

// private
fn bool window_is_minimized(GfxWindow* win)
{
	Atom actualType;
	int actualFormat;
	CULong nitems, bytesAfter;
	char* propData;
	int status = xGetWindowProperty(gfx::_gfx.display, win.src.window, gfx::_gfx.wmState, 0, 2, 0, 0, &actualType, &actualFormat, &nitems, &bytesAfter, &propData);
	if (status == 0 && nitems >= 1 && propData == (char*)(uptr)3 /* IconicState */)
	{
		xFree(propData);
		return true;
	}
	if (propData != null) xFree(propData);
	XWindowAttributes windowAttributes;
	xGetWindowAttributes(gfx::_gfx.display, win.src.window, &windowAttributes);
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
		_glx_make_current_fn(gfx::_gfx.display, win.src.ctx.native.window, win.src.ctx.native.ctx);
	}
}

fn void _window_maximize_x11(GfxWindow* win) => _toggle_x_maximized(win, true);

fn void _window_minimize_x11(GfxWindow* win)
{
	if (x11::window_is_maximized(win)) return;
	win.internal.old_bounds = win.bounds;
	xIconifyWindow(gfx::_gfx.display, win.src.window, xDefaultScreen(gfx::_gfx.display));
	xFlush(gfx::_gfx.display);
}

fn void _window_move_mouse_x11(GfxWindow* win, GfxPoint where)
{
	int rootX, rootY, win_x, win_y;
	uint mask;
	XWindow root_ret, child_ret;
	xQueryPointer(gfx::_gfx.display, xDefaultRootWindow(gfx::_gfx.display), &root_ret, &child_ret, &rootX, &rootY, &win_x, &win_y, &mask);
	win.internal.last_mouse = where - win.origin;
	if (win_x == where.x && win_y == where.y) return;
	xWarpPointer(gfx::_gfx.display, 0, win.src.window, 0, 0, 0, 0, ...(where - win.origin));
}

fn void _window_move_x11(GfxWindow* win, GfxPoint where)
{
	win.origin = where;
	xMoveWindow(gfx::_gfx.display, win.src.window, ...where);
}

fn void _window_raise_x11(GfxWindow* win)
{
	xMapRaised(gfx::_gfx.display, win.src.window);
	gfx::_window_set_fullscreen_native(win, win.is_fullscreen());
}

// private
fn void _window_resize_x11(GfxWindow* win, GfxSize size)
{
	win.size = size;
	xResizeWindow(gfx::_gfx.display, win.src.window, (uint)size.x, (uint)size.y);
	if (win.internal.flags.no_resize)
	{
		XSizeHints sh;
		sh.flags = (1 << 4) | (1 << 5);
		sh.min_width = sh.max_width = size.x;
		sh.min_height = sh.max_height = size.y;
		xSetWMSizeHints(gfx::_gfx.display, win.src.window, &sh, 0);
	}
}

// private
fn void _window_restore_x11(GfxWindow* win)
{
	_toggle_x_maximized(win, false);
	xFlush(gfx::_gfx.display);
}

fn void _window_set_floating_x11(GfxWindow* win, bool floating) => _window_set_x_atom(win, gfx::_gfx.netWmStateAbove, floating);

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

	xRaiseWindow(gfx::_gfx.display, win.src.window);
	_window_set_x_atom(win, gfx::_gfx.netWmStateFullscreen, fullscreen);

	if (!(win.internal.flags.transparent))
	{
		char value = (char)fullscreen;
		xChangeProperty(gfx::_gfx.display, win.src.window, gfx::_gfx.netWmBypassCompositor, 6 /* XA_CARDINAL */, 32, 0, &value, 1);
	}
}

// private
fn void _window_set_max_size_x11(GfxWindow* win, GfxSize size)
{
	CLong flags;
	XSizeHints hints;
	xGetWMNormalHints(gfx::_gfx.display, win.src.window, &hints, &flags);
	hints.flags |= 1 << 5; // PMaxSize
	hints.max_width = size.x; hints.max_height = size.y;
	xSetWMNormalHints(gfx::_gfx.display, win.src.window, &hints);
}

// private
fn void _window_set_min_size_x11(GfxWindow* win, GfxSize size)
{
	CLong flags;
	XSizeHints hints;
	xGetWMNormalHints(gfx::_gfx.display, win.src.window, &hints, &flags);
	hints.flags |= 1 << 4; // PMinSize
	hints.min_width = size.x; hints.min_height = size.y;
	xSetWMNormalHints(gfx::_gfx.display, win.src.window, &hints);
}

fn bool _window_set_mouse_x11(GfxWindow* win, GfxMouse* mouse)
{
	xDefineCursor(gfx::_gfx.display, win.src.window, (XCursor)mouse);
	return true;
}

fn void _window_set_opacity_x11(GfxWindow* win, float opacity)
{
	uint value = (uint)math::round(uint::max * opacity);
	xChangeProperty(gfx::_gfx.display, win.src.window, gfx::_gfx.netWmWindowOpacity, 6, 32, 0, (char*)&value, 1);
}

fn void _window_opengl_swap_buffers_x11(GfxWindow* win) { _glx_swap_buffers_fn(gfx::_gfx.display, win.src.ctx.native.window); }

// private
fn void? _write_clipboard_x11(GfxDataTransfer* data)
{
	xSetSelectionOwner(gfx::_gfx.display, gfx::_gfx.clipboardAtom, gfx::_gfx.helper_window, 0);
	if (xGetSelectionOwner(gfx::_gfx.display, gfx::_gfx.clipboardAtom) != gfx::_gfx.helper_window)
	{
		return gfx::COULD_NOT_OWN_X11_CLIPBOARD~;
	}

	if (gfx::_gfx.unixClipboard)
	{
		free(gfx::_gfx.unixClipboard);
		gfx::_gfx.unixClipboard = null;
	}

	gfx::_gfx.unixClipboard = mem::new_with_padding(GfxDataTransfer, data.text.len);
	char* data_ptr = (char*)gfx::_gfx.unixClipboard + GfxDataTransfer::size - 1;
	data_ptr[:data.text.len] = data.text[..];
	gfx::_gfx.unixClipboard.text = (String)data_ptr[:data.text.len];
	gfx::_gfx.unixClipboard.type = TEXT;
}

fn GfxDataTransfer? _read_clipboard_x11(Allocator allocator, GfxDataTransferType type)
{
	if (type != TEXT) return gfx::CLIPBOARD_DID_NOT_CONTAIN_TYPE~;

	if (xGetSelectionOwner(gfx::_gfx.display, gfx::_gfx.clipboardAtom) == gfx::_gfx.helper_window)
	{
		if (!gfx::_gfx.unixClipboard.text) return gfx::CLIPBOARD_DID_NOT_CONTAIN_TYPE~;
		return { .type = TEXT, .text = gfx::_gfx.unixClipboard.text.copy(allocator) };
	}

	xConvertSelection(gfx::_gfx.display, gfx::_gfx.clipboardAtom, gfx::_gfx.utf8String, gfx::_gfx.xselData, gfx::_gfx.helper_window, 0);
	xSync(gfx::_gfx.display, 0);
	XEvent event;
	while (true)
	{
		xNextEvent(gfx::_gfx.display, &event);
		if (event.type != 31 /* SelectionNotify */) continue;
		if (event.xselection.selection != gfx::_gfx.clipboardAtom || event.xselection.property == 0) return gfx::CLIPBOARD_COULD_NOT_BE_OPENED~;
		break;
	}

	Atom target;
	int format;
	CULong n, size;
	char* data;
	xGetWindowProperty(gfx::_gfx.display, event.xselection.requestor, event.xselection.property, 0, ~0, 0, 0, &target, &format, &size, &n, (char**)&data);
	defer xDeleteProperty(gfx::_gfx.display, event.xselection.requestor, event.xselection.property);

	if (target == gfx::_gfx.utf8String || target == 31)
	{
		defer xFree(data);
		return { .type = TEXT, .text = (String)alloc::clone_slice(allocator, data[:size]) };
	}
	return gfx::CLIPBOARD_DID_NOT_CONTAIN_TYPE~;
}

// private
fn void? _window_init_opengl_context_x11(GfxWindow* win, GfxGlContext* context, GfxGlHints* hints)
{
	win.src.ctx.native = context;
	win.src.backing = { .opengl };

	bool show_window = false;
	if (win.src.window)
	{
		show_window = !window_is_minimized(win);
		x11::window_destroy_platform(win);
	}

	bool transparent = win.internal.flags.transparent;

	FixedList{int, 40} stack;
	stack.push_all({
		GLX_X_VISUAL_TYPE, GLX_TRUE_COLOR,
		GLX_X_RENDERABLE, 1,
		GLX_RENDER_TYPE, GLX_RGBA_BIT,
		GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT,
		GLX_DOUBLEBUFFER,      1,
		GLX_ALPHA_SIZE,        hints.rgba_size.a,
		GLX_DEPTH_SIZE,        hints.depth,
		GLX_STENCIL_SIZE,      hints.stencil,
		GLX_STEREO,            hints.stereo,
		GLX_AUX_BUFFERS,       hints.auxBuffers,
		GLX_RED_SIZE,          hints.rgba_size.r,
		GLX_GREEN_SIZE,        hints.rgba_size.g,
		GLX_BLUE_SIZE,         hints.rgba_size.b,
		GLX_ACCUM_RED_SIZE,    hints.accum_size.r,
		GLX_ACCUM_GREEN_SIZE,  hints.accum_size.g,
		GLX_ACCUM_BLUE_SIZE,   hints.accum_size.b,
		GLX_ACCUM_ALPHA_SIZE,  hints.accum_size.a,
	});

	if (hints.srgb)
	{
		if (_opengl_extension_is_supported_x11("GLX_ARB_framebuffer_sRGB")) stack.push_all({ GLX_FRAMEBUFFER_SRGB_CAPABLE_ARB, (int)hints.srgb });
		if (_opengl_extension_is_supported_x11("GLX_EXT_framebuffer_sRGB")) stack.push_all({ GLX_FRAMEBUFFER_SRGB_CAPABLE_EXT, (int)hints.srgb });
	}

	stack.push_all({0, 0});

	int fbcount;
	GLXFBConfig* fbc = _glx_choose_fb_config_fn(gfx::_gfx.display, xDefaultScreen(gfx::_gfx.display), stack.array_view(), &fbcount);
	if (!fbc || !fbcount) return gfx::GLX_VISUAL_CONFIGS_NOT_FOUND~;

	int bestFbcIdx = -1;
	int bestDepth = 0;
	int bestSamples = 0;
	for (int i = 0; i < fbcount; i++)
	{
		XVisualInfo* vi = _glx_get_visual_from_fb_config_fn(gfx::_gfx.display, fbc[i]);
		if (vi == null) continue;
		int sampBuf, samples;
		_glx_get_fb_config_attrib_fn(gfx::_gfx.display, fbc[i], GLX_SAMPLE_BUFFERS, &sampBuf);
		_glx_get_fb_config_attrib_fn(gfx::_gfx.display, fbc[i], GLX_SAMPLES, &samples);
		if (bestFbcIdx == -1) bestFbcIdx = i;
		if ((!transparent || vi.depth == 32) && bestDepth == 0) { bestFbcIdx = i; bestDepth = vi.depth; }
		if ((!transparent || vi.depth == 32) && samples <= hints.samples && samples > bestSamples) { bestFbcIdx = i; bestDepth = vi.depth; bestSamples = samples; }
		xFree(vi);
	}
	if (bestFbcIdx == -1) return gfx::GLX_NO_VALID_VISUAL~;

	GLXFBConfig bestFbc = fbc[bestFbcIdx];
	XVisualInfo* vi = _glx_get_visual_from_fb_config_fn(gfx::_gfx.display, bestFbc);
	xFree(fbc);
	XVisualInfo visual = *vi;
	xFree(vi);

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

	if (_opengl_extension_is_supported_x11("GLX_ARB_create_context_no_error")) stack.push_all({ GLX_CONTEXT_OPENGL_NO_ERROR_ARB, (int)hints.no_error });

	stack.push_all({0, 0});

	GLXContext ctx = null;
	if (hints.share) ctx = hints.share.ctx;

	if (!_glx_create_context_attribs_arb_fn)
	{
		log::warn("Failed to load proc address 'glXCreateContextAttribsARB', loading a generic OpenGL context.", category: log::CATEGORY_VIDEO);
		win.src.ctx.native.ctx = _glx_create_context_fn(gfx::_gfx.display, &visual, ctx, X_TRUE);
	}
	else
	{
		gfx::_gfx.x11Error = null;
		win.src.ctx.native.ctx = _glx_create_context_attribs_arb_fn(gfx::_gfx.display, bestFbc, ctx, X_TRUE, stack.array_view());
		if (gfx::_gfx.x11Error || !win.src.ctx.native.ctx)
		{
			log::warn("Failed to create an OpenGL context with AttribsARB, loading a generic OpenGL context.", category: log::CATEGORY_VIDEO);
			win.src.ctx.native.ctx = _glx_create_context_fn(gfx::_gfx.display, &visual, ctx, X_TRUE);
		}
	}

	win.src.ctx.native.window = _glx_create_window_fn(gfx::_gfx.display, bestFbc, win.src.window, null);
	_glx_make_current_fn(gfx::_gfx.display, win.src.ctx.native.window, win.src.ctx.native.ctx);
	log::info("OpenGL context initalized.", category: log::CATEGORY_VIDEO);
	gfx::_window_set_opengl_swap_interval(win, 0);
}

// private
fn void window_flash(GfxWindow* win, GfxFlashRequest request)
{
	if (win.has_focus() && request != CANCEL) return;
	XWMHints* wmhints = xGetWMHints(gfx::_gfx.display, win.src.window);
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
	xSetWMHints(gfx::_gfx.display, win.src.window, wmhints);
	xFree(wmhints);
}

// private
fn void? _init_surface_x11(GfxSurface* surface, void* data, int w, int h, GfxFormat format, GfxWindow* window)
{
	if (!window) window = gfx::_gfx.root;
	surface.data = data;
	surface.size = { w, h };
	surface.format = format;

	XWindowAttributes attrs;
	if (!xGetWindowAttributes(gfx::_gfx.display, window.src.window, &attrs)) return gfx::SURFACE_CREATION_FAILED~;
	defer catch surface.release();
	surface.native.bitmap = xCreateImage(gfx::_gfx.display, attrs.visual, (uint)attrs.depth, 2 /* ZPixmap */, 0, null, ...(uint[<2>])surface.size, 32, 0);
	if (!surface.native.bitmap) return gfx::SURFACE_CREATION_FAILED~;
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
	int status = xGetWindowProperty(gfx::_gfx.display, win.src.window, gfx::_gfx.netWmState, 0, ~0, 0, 4, &actualType, &actualFormat, &nitems, &bytesAfter, (char**)&propReturn);
	if (status != 0 || actualType != 4) return false;
	for (CULong i = 0; i < nitems; i++) if (propReturn[i] == gfx::_gfx.netWmStateAbove) return true;
	if (propReturn != null) xFree(propReturn);
	return false;
}

// private
fn void _window_set_name_x11(GfxWindow* win, String name) => @pool()
{
	ZString cstring = name.zstr_copy(tmem);
	xutf8SetWMProperties(gfx::_gfx.display, win.src.window, cstring, cstring, null, 0, null, null, null);
	xStoreName(gfx::_gfx.display, win.src.window, cstring);
	xChangeProperty(gfx::_gfx.display, win.src.window, gfx::_gfx.netWmName, gfx::_gfx.utf8String, 8, 0, cstring, name.len);
}

// private
fn bool _window_set_icon_x11(GfxWindow* win, char* dataSrc, int w, int h, GfxFormat format, GfxWindowIconType type)
{
	if (dataSrc == null)
	{
		return !!xChangeProperty(gfx::_gfx.display, win.src.window, gfx::_gfx.netWmIcon, 6, 32, 0, null, 0);
	}
	int count = 2 + (w * h);
	CULong* data = calloc((sz)count * CULong::size);
	data[0] = (CULong)w;
	data[1] = (CULong)h;

	gfx::copy_image_data_64((char*)&data[2], w, h, BGRA8, dataSrc, format, true, null);
	bool res = true;
	if (type & TASKBAR)
	{
		res = !!xChangeProperty(gfx::_gfx.display, win.src.window, gfx::_gfx.netWmIcon, 6, 32, 0, (char*)data, count);
	}

	gfx::copy_image_data_64((char*)&data[2], w, h, BGRA8, dataSrc, format, false, null);

	if (type & WINDOW)
	{
		XWMHints wmHints;
		wmHints.flags = 1 << 2; // IconPixmapHint

		int depth = xDefaultDepth(gfx::_gfx.display, xDefaultScreen(gfx::_gfx.display));
		void* image = xCreateImage(gfx::_gfx.display, xDefaultVisual(gfx::_gfx.display, xDefaultScreen(gfx::_gfx.display)),
		(uint)depth, 2 /* ZPixmap */, 0, (char*)&data[2], (uint)w, (uint)h, 32, 0);

		wmHints.icon_pixmap = xCreatePixmap(gfx::_gfx.display, win.src.window, (uint)w, (uint)h, (uint)depth);
		xPutImage(gfx::_gfx.display, wmHints.icon_pixmap, xDefaultGC(gfx::_gfx.display, xDefaultScreen(gfx::_gfx.display)), image, 0, 0, 0, 0, (uint)w, (uint)h);
		((XImageHeader*)image).data = null;
		xDestroyImage(image);

		xSetWMHints(gfx::_gfx.display, win.src.window, &wmHints);
	}

	free(data);
	xFlush(gfx::_gfx.display);
	return res;
}

fn GfxMonitor*? _window_find_monitor_x11(GfxWindow* win)
{
	XWindowAttributes attrs;
	if (xGetWindowAttributes(gfx::_gfx.display, win.src.window, &attrs) == 0) return NOT_FOUND~;
	for (GfxMonitorNode* node = gfx::_gfx.monitors.list.head; node != null; node = node.next)
	{
		if ((attrs.x < node.mon.origin.x + node.mon.mode.size.x) && (attrs.x + attrs.width > node.mon.origin.x) &&
		(attrs.y < node.mon.origin.y + node.mon.mode.size.y) && (attrs.y + attrs.height > node.mon.origin.y))
		{
			return &node.mon;
		}
	}
	return gfx::_gfx.monitors.list.head ? &gfx::_gfx.monitors.list.head.mon : NOT_FOUND~;
}

// public
fn void window_show(GfxWindow* win)
{
	win.internal.flags.hide = false;
	if (!window_is_hidden(win)) return;
	xMapWindow(gfx::_gfx.display, win.src.window);
	_wait_for_show_event_x11(win);
	win.move(win.origin);
	gfx::_window_set_fullscreen_native(win, win.is_fullscreen());
}

// public
fn bool _wait_for_show_event_x11(GfxWindow* win)
{
	XEvent dummy;
	while (xCheckTypedWindowEvent(gfx::_gfx.display, win.src.window, 15 /* VisibilityNotify */, &dummy) == 0)
	{
		gfx::_wait_for_event_native(time::ms(100));
	}
	return true;
}

fn void _toggle_x_maximized(GfxWindow* win, bool maximized)
{
	XEvent xev;
	xev.type = 33; // ClientMessage
	xev.xclient.window = win.src.window;
	xev.xclient.message_type = gfx::_gfx.netWmState;
	xev.xclient.format = 32;
	xev.xclient.data.l[0] = (int)maximized;
	xev.xclient.data.l[1] = (CLong)gfx::_gfx.net_wm_state_maximized_horz;
	xev.xclient.data.l[2] = (CLong)gfx::_gfx.net_wm_state_maximized_vert;
	xSendEvent(gfx::_gfx.display, xDefaultRootWindow(gfx::_gfx.display), 0, (1 << 20) | (1 << 19), &xev);
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
		gfx::_data_drop_callback(win, url.path.zstr_tcopy(), url.path.len + 1, FILE);
	}
}

// public
fn void _get_visual_for_window(XVisualInfo* visual, bool transparent)
{
	visual.visual = xDefaultVisual(gfx::_gfx.display, xDefaultScreen(gfx::_gfx.display));
	visual.depth = xDefaultDepth(gfx::_gfx.display, xDefaultScreen(gfx::_gfx.display));
	if (transparent)
	{
		xMatchVisualInfo(gfx::_gfx.display, xDefaultScreen(gfx::_gfx.display), 32, 4 /* TrueColor */, visual);
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
	xev.xclient.message_type = gfx::_gfx.netWmState;
	xev.xclient.window = win.src.window;
	xev.xclient.format = 32;
	xev.xclient.data.l[0] = (int)fullscreen;
	xev.xclient.data.l[1] = (CLong)netAtom;
	xSendEvent(gfx::_gfx.display, xDefaultRootWindow(gfx::_gfx.display), 0, (1 << 19) | (1 << 20), &xev);
}

fn void _x11_ic_callback(void* ic, void* client_data, void* call_data)
{
	GfxWindow* win = client_data;
	win.src.ic = null;
}

fn void _x11_im_callback(void* im, void* client_data, void* call_data)
{
	gfx::_gfx.im = null;
}

// public
fn void _x11_im_init_callback(XDisplay* d, XPointer client_data, XPointer call_data)
{
	if (gfx::_gfx.im != null) return;

	gfx::_gfx.im = xOpenIM(gfx::_gfx.display, null, null, null);
	if (gfx::_gfx.im == null) return;

	bool found = false;
	XImStyles* styles = null;

	if (xGetIMValues(gfx::_gfx.im, "queryInputStyle", &styles, (char*)null) != null)
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
		xFree(styles);
	}

	if (!found)
	{
		xCloseIM(gfx::_gfx.im);
		gfx::_gfx.im = null;
		return;
	}

	XImCallback callback;
	callback.callback = &_x11_im_callback;
	callback.client_data = null;
	xSetIMValues(gfx::_gfx.im, "destroyCallback", &callback, (char*)null);
}

fn int _x_error_handler(XDisplay* display, XErrorEvent* ev)
{
	char[512] errorText;
	xGetErrorText(display, ev.error_code, (char*)&errorText, 512);
	log::warn("X Error callback :%s", (ZString)&errorText, category: log::CATEGORY_VIDEO);
	gfx::_gfx.x11Error = ev;
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

	gfx::_split_bpp((uint)xDefaultDepth(gfx::_gfx.display, xDefaultScreen(gfx::_gfx.display)), found_mode);
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
	ZString rms = xResourceManagerString(gfx::_gfx.display);
	if (rms == null) return;
	void* db = xrmGetStringDatabase(rms);
	if (db == null) return;
	ZString type = null;
	// xrmGetResource's value-out parameter is a small {len,addr} struct; omitted here.
	xrmDestroyDatabase(db);
	*dpi = dpiOutput;
}

// public
fn void _x_handle_clipboard_selection(XEvent* event)
{
	XSelectionRequestEvent* request = &event.xselectionrequest;
	Atom[2] formats = {gfx::_gfx.utf8String, 31 /* XA_STRING */};

	if (request.target == gfx::_gfx.targets)
	{
		Atom[4] targets = {gfx::_gfx.targets, gfx::_gfx.multiple, gfx::_gfx.utf8String, 31};
		xChangeProperty(gfx::_gfx.display, request.requestor, request.property, 4 /* XA_ATOM */, 32, 0, (char*)&targets, 4);
	}
	else if (request.target == gfx::_gfx.multiple)
	{
		Atom* targets = null;
		Atom actualType;
		int actualFormat;
		CULong count, bytesAfter;
		xGetWindowProperty(gfx::_gfx.display, request.requestor, request.property, 0, ~0, 0, gfx::_gfx.atomPair,
		&actualType, &actualFormat, &count, &bytesAfter, (char**)&targets);

		for (CULong i = 0; i < count; i += 2)
		{
			if (targets[i] == gfx::_gfx.utf8String || targets[i] == 31 /* XA_STRING */)
			{
				xChangeProperty(gfx::_gfx.display, request.requestor, targets[i + 1], targets[i],
				8, 0, gfx::_gfx.unixClipboard.text, (int)gfx::_gfx.unixClipboard.text.len);
			}
			else
			{
				targets[i + 1] = 0; // None
			}
		}

		xChangeProperty(gfx::_gfx.display, request.requestor, request.property, gfx::_gfx.atomPair, 32, 0, (char*)targets, (int)count);
		xFlush(gfx::_gfx.display);
		xFree(targets);
	}
	else if (request.target == gfx::_gfx.saveTargets)
	{
		xChangeProperty(gfx::_gfx.display, request.requestor, request.property, 0, 32, 0, null, 0);
	}
	else
	{
		for (sz i = 0; i < 2; i++)
		{
			if (request.target != formats[i]) continue;
			xChangeProperty(gfx::_gfx.display, request.requestor, request.property, request.target, 8, 0, gfx::_gfx.unixClipboard.text, (int)gfx::_gfx.unixClipboard.text.len);
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
	xSendEvent(gfx::_gfx.display, request.requestor, 0, 0, &reply);
	xFlush(gfx::_gfx.display);
}

// private
fn int x_handle_clipboard_selection_helper()
{
	XEvent event;
	xPending(gfx::_gfx.display);
	if (xQLength(gfx::_gfx.display) != 0 || xEventsQueued(gfx::_gfx.display, 0) + xEventsQueued(gfx::_gfx.display, 1) != 0)
	{
		xNextEvent(gfx::_gfx.display, &event);
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
			_glx_swap_interval_ext_fn(gfx::_gfx.display, win.src.ctx.native.window, swapInterval);
		case _glx_swap_interval_native_fn != null:
			_glx_swap_interval_native_fn(swapInterval);
		default:
			log::warn("Failed to load swap interval function", category: log::CATEGORY_VIDEO);
	}
}


// --- wgpu

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
	VkCreateSurfaceKHRProc proc = (VkCreateSurfaceKHRProc)gfx::_get_instance_proc_vulkan(instance, "vkCreateXlibSurfaceKHR");
	if (!proc) return gfx::VULKAN_NOT_LOADED~;
	VkXlibSurfaceCreateInfoKHR create_info = {
		.sType = gfx::VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR,
		.dpy = gfx::_gfx.display,
		.window = win.src.window
	};
	if (proc(instance, &create_info, null, &surface)) return gfx::SURFACE_CREATION_FAILED~;
	return surface;
}

alias VkGetPhysicalDeviceXlibPresentationSupportKHRProc = fn uint(VkPhysicalDevice, uint, XDisplay*, VisualID);

// public
fn bool _vulkan_get_physical_device_presentation_support_x11(VkInstance instance, VkPhysicalDevice physical_device, uint queue_family_index)
{
	VkGetPhysicalDeviceXlibPresentationSupportKHRProc proc = (VkGetPhysicalDeviceXlibPresentationSupportKHRProc)gfx::_get_instance_proc_vulkan(instance, "vkGetPhysicalDeviceXlibPresentationSupportKHR");
	if (!proc) return false;
	XVisual* visual = xDefaultVisual(gfx::_gfx.display, xDefaultScreen(gfx::_gfx.display));
	uint out = proc(physical_device, queue_family_index, gfx::_gfx.display, xVisualIDFromVisual(visual));
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
