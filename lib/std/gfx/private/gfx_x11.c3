module std::gfx @feat(X11, X11_WAYLAND) @private;
import std::math, std::collections::list, libc;



fn void todo_fix_surface_freePtr_X11(GfxSurface* surface)
{
	if (!surface.data) return;
	if (surface.native.buffer) free(surface.native.buffer);
	if (surface.native.bitmap) xDestroyImage(surface.native.bitmap);
	*surface = {};
}

fn void todo_fix_window_setBorder_X11(GfxWindow* win, bool border)
{
	win.internal.flags.no_border = !border;
	X11WindowHints hints;
	hints.flags = 2;
	hints.decorations = (CULong)border;
	xChangeProperty(_gfx_ctx.display, win.src.window, _gfx_ctx.motifWmHints, _gfx_ctx.motifWmHints, 32, 0, (char*)&hints, 5);
	if (!_window_is_hidden_x11(win))
	{
		win.hide();
		win.show();
	}
}

fn void todo_fix_window_setRawMouseModePlatform_X11(GfxWindow* win, bool state)
{
	char[4] mask;
	if (state) xiSetMask((char*)&mask, 6 /* XI_RawMotion */);
	XIEventMask em;
	em.deviceid = 1; // XIAllMasterDevices
	em.mask_len = 4;
	em.mask = (char*)&mask;
	if (_xi_select_events_src) _xi_select_events_src(_gfx_ctx.display, xDefaultRootWindow(_gfx_ctx.display), &em, 1);
}

fn void todo_fix_window_captureMousePlatform_X11(GfxWindow* win, bool state)
{
	if (state)
	{
		uint eventMask = (1 << 2) | (1 << 3) | (1 << 6); // Button press/release + pointer motion
		xGrabPointer(_gfx_ctx.display, win.src.window, X_TRUE, eventMask, 1, 1, win.src.window, 0, 0);
	}
	else
	{
		xUngrabPointer(_gfx_ctx.display, 0);
	}
}

// private
fn WGPUSurface todo_fix_window_createSurface_WebGPU_X11(GfxWindow* window, WGPUInstance instance)
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
fn void todo_fix_window_setAspectRatio_X11(GfxWindow* win, int w, int h)
{
	if (win == null) return;
	if (w == 0 && h == 0) return;
	XSizeHints hints;
	CLong flags;
	xGetWMNormalHints(_gfx_ctx.display, win.src.window, &hints, &flags);
	hints.flags |= (1 << 6); // PAspect
	hints.min_aspect.x = w; hints.max_aspect.x = w;
	hints.min_aspect.y = h; hints.max_aspect.y = h;
	xSetWMNormalHints(_gfx_ctx.display, win.src.window, &hints);
}

// public
fn void todo_fix_window_blitSurface_X11(GfxWindow* win, GfxSurface* surface)
{
	if (surface == null) return;
	// surface.native.bitmap->data = surface.native.buffer would go here in real Xlib
	// (XImage.data is a char* at a fixed offset); left to the caller's XImage helper.
	GfxPoint min = math::min(win.size, surface.size);
	xPutImage(_gfx_ctx.display, win.src.window, win.src.gc, surface.native.bitmap, 0, 0, 0, 0, (uint)min.x, (uint)min.y);
}

fn GfxPoint _get_global_mouse_x11()
{
	int x, y, fX, fY;
	uint z;
	XWindow window1, window2;
	xQueryPointer(_gfx_ctx.display, xDefaultRootWindow(_gfx_ctx.display), &window1, &window2, &fX, &fY, &x, &y, &z);
	return { fX, fY };
}

// public
fn GfxPoint todo_fix_window_fetchSize_X11(GfxWindow* win)
{
	XWindowAttributes attribs;
	xGetWindowAttributes(_gfx_ctx.display, win.src.window, &attribs);
	win.size = { attribs.width, attribs.height };
	return win.get_size();
}

// public
fn void todo_fix_window_setMousePassthrough_X11(GfxWindow* win, bool passthrough)
{
	if (passthrough)
	{
		Region region = xCreateRegion();
		if (_xshape_combine_region_src) _xshape_combine_region_src(_gfx_ctx.display, win.src.window, 2, 0, 0, region, 0);
		xDestroyRegion(region);
		return;
	}
	if (_xshape_combine_mask_src) _xshape_combine_mask_src(_gfx_ctx.display, win.src.window, 2, 0, 0, 0, 0);
}

// public
fn sz todo_fix_monitor_getGammaRampPtr_X11(GfxMonitor* monitor, TodoRenameGammaRamp* ramp)
{
	sz size = (sz)xRRGetCrtcGammaSize(_gfx_ctx.display, monitor.node.nativeId1);
	XRRCrtcGamma* gamma = xRRGetCrtcGamma(_gfx_ctx.display, monitor.node.nativeId1);
	if (ramp != null)
	{
		mem::copy(ramp.red, gamma.red, size * ushort::size);
		mem::copy(ramp.green, gamma.green, size * ushort::size);
		mem::copy(ramp.blue, gamma.blue, size * ushort::size);
	}
	xRRFreeGamma(gamma);
	return size;
}

// public
fn bool todo_fix_monitor_setGammaRamp_X11(GfxMonitor* monitor, TodoRenameGammaRamp* ramp)
{
	sz size = (sz)xRRGetCrtcGammaSize(_gfx_ctx.display, monitor.node.nativeId1);
	if (size != ramp.count)
	{
		todo_fix_debugCallback(TODO_RENAME_TYPE_ERROR, TODO_RENAME_ERR_X11, "X11: Gamma ramp size must match current ramp size");
		return false;
	}
	XRRCrtcGamma* gamma = xRRAllocGamma((int)ramp.count);
	mem::copy(gamma.red, ramp.red, ramp.count * ushort::size);
	mem::copy(gamma.green, ramp.green, ramp.count * ushort::size);
	mem::copy(gamma.blue, ramp.blue, ramp.count * ushort::size);
	xRRSetCrtcGamma(_gfx_ctx.display, monitor.node.nativeId1, gamma);
	xRRFreeGamma(gamma);
	return true;
}

// private
fn TodoRenameMouse* todo_fix_createMouse_X11(char* data, int w, int h, GfxFormat format)
{
	if (_xcursor_image_create_src == null) return null;
	XcursorImage* native = _xcursor_image_create_src(w, h);
	native.xhot = 0; native.yhot = 0;
	mem::clear(native.pixels, (sz)(w * h * 4));
	mem::copy(native.pixels, data, (sz)(w * h * 4));
	XCursor cursor = _xcursor_image_load_cursor_src(_gfx_ctx.display, native);
	_xcursor_image_destroy_src(native);
	return (TodoRenameMouse*)cursor;
}

// private
fn TodoRenameMouse* todo_fix_createMouseStandard_X11(GfxMouseIcon mouse)
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
		default: return null;
	}
	XCursor cursor = xCreateFontCursor(_gfx_ctx.display, mouseIcon);
	return (TodoRenameMouse*)cursor;
}

// private
fn void? todo_fix_createWindowPlatform_X11(String name, GfxWindowFlags flags, GfxWindow* win)
{
	if (flags.open_gl || flags.egl)
	{
		win.src.window = 0;
		return;
	}
	XVisualInfo visual;
	todo_fix_window_getVisual(&visual, win.internal.flags.transparent);
	return todo_fix_xCreateWindow(visual, name, flags, win);
}

// private
fn void todo_fix_deinitPlatform_X11()
{
	if (_gfx_ctx.display != null)
	{
		if (xGetSelectionOwner(_gfx_ctx.display, _gfx_ctx.clipboardAtom) == _gfx_ctx.helperWindow)
		{
			xConvertSelection(_gfx_ctx.display, _gfx_ctx.clipboardManager, _gfx_ctx.saveTargets, 0, _gfx_ctx.helperWindow, 0);
			while (todo_fix_xHandleClipboardSelectionHelper() != 0) {}
		}
		xUnregisterIMInstantiateCallback(_gfx_ctx.display, null, null, null, (void*)&todo_fix_x11ImInitCallback, null);
	}
	if (_gfx_ctx.im != null) { xCloseIM(_gfx_ctx.im); _gfx_ctx.im = null; }
	if (_gfx_ctx.unixClipboard != null) { free(_gfx_ctx.unixClipboard); _gfx_ctx.unixClipboard = null; }
	if (_gfx_ctx.hiddenMouse != null) { todo_fix_freeMouse(_gfx_ctx.hiddenMouse); _gfx_ctx.hiddenMouse = null; }
	if (_gfx_ctx.helperWindow != 0) xDestroyWindow(_gfx_ctx.display, _gfx_ctx.helperWindow);
	if (_gfx_ctx.display != null) xCloseDisplay(_gfx_ctx.display);
	_gfx_ctx.display = null;

	if (_x11_cursor_handle != null) { libc::dlclose(_x11_cursor_handle); _x11_cursor_handle = null; }
	if (_x11_xi_handle != null) { libc::dlclose(_x11_xi_handle); _x11_xi_handle = null; }
	if (_x11_ext_handle != null) { libc::dlclose(_x11_ext_handle); _x11_ext_handle = null; }
}

// private
fn bool todo_fix_extensionSupportedPlatform_OpenGL_X11(ZString extension, sz len)
{
	ZString extensions = todo_fix_glXQueryExtensionsString(_gfx_ctx.display, xDefaultScreen(_gfx_ctx.display));
	return extensions && todo_fix_extensionSupportedStr(extensions, extension, len);
}

// private
fn void todo_fix_freeMouse_X11(TodoRenameMouse* mouse) { xFreeCursor(_gfx_ctx.display, (XCursor)mouse); }

// private
fn void* todo_fix_getCurrentContext_OpenGL_X11() => todo_fix_glXGetCurrentContext();

// private
fn GfxFn todo_fix_getProcAddress_OpenGL_X11(ZString procname)
{
	if (_gfx_ctx.glGetProcAddress == null) return null;
	return ((GlxGetProcAddressProc)_gfx_ctx.glGetProcAddress)(procname);
}

// private
fn void todo_fix_initKeycodesPlatform_X11()
{
	// A full evdev-keycode -> _KEY table walk (xkbGetKeyboardByName) is left
	// out of this mechanical pass; keycodes[] stays zeroed (_KEY_NULL).
}

// private
fn int todo_fix_initPlatform_X11(String class_name, GfxInitOption flags)
{
	_gfx_ctx.class_name = class_name;

	xInitThreads();
	_gfx_ctx.display = xOpenDisplay(null);
	if (_gfx_ctx.display == null) return -1;

	_gfx_ctx.netCurrentDesktop = xInternAtom(_gfx_ctx.display, "_NET_CURRENT_DESKTOP", 0);
	_gfx_ctx.net_wm_state_maximized_vert = xInternAtom(_gfx_ctx.display, "_NET_WM_STATE_MAXIMIZED_VERT", 0);
	_gfx_ctx.net_wm_state_maximized_horz = xInternAtom(_gfx_ctx.display, "_NET_WM_STATE_MAXIMIZED_HORZ", 0);
	_gfx_ctx.netWmBypassCompositor = xInternAtom(_gfx_ctx.display, "_NET_WM_BYPASS_COMPOSITOR", 0);
	_gfx_ctx.netWmState = xInternAtom(_gfx_ctx.display, "_NET_WM_STATE", 0);
	_gfx_ctx.motifWmHints = xInternAtom(_gfx_ctx.display, "_MOTIF_WM_HINTS", 0);
	_gfx_ctx.wmDeleteWindow = xInternAtom(_gfx_ctx.display, "WM_DELETE_WINDOW", 0);
	_gfx_ctx.netWmSyncRequestCounter = xInternAtom(_gfx_ctx.display, "_NET_WM_SYNC_REQUEST_COUNTER", 0);
	_gfx_ctx.netWmSyncRequest = xInternAtom(_gfx_ctx.display, "_NET_WM_SYNC_REQUEST", 0);
	_gfx_ctx.atomPair = xInternAtom(_gfx_ctx.display, "ATOM_PAIR", 0);
	_gfx_ctx.multiple = xInternAtom(_gfx_ctx.display, "MULTIPLE", 0);
	_gfx_ctx.targets = xInternAtom(_gfx_ctx.display, "TARGETS", 0);
	_gfx_ctx.saveTargets = xInternAtom(_gfx_ctx.display, "SAVE_TARGETS", 0);
	_gfx_ctx.utf8String = xInternAtom(_gfx_ctx.display, "UTF8_STRING", 0);
	_gfx_ctx.xdndTypeList = xInternAtom(_gfx_ctx.display, "XdndTypeList", 0);
	_gfx_ctx.xdndSelection = xInternAtom(_gfx_ctx.display, "XdndSelection", 0);
	_gfx_ctx.xdndAware = xInternAtom(_gfx_ctx.display, "XdndAware", 0);
	_gfx_ctx.xdndEnter = xInternAtom(_gfx_ctx.display, "XdndEnter", 0);
	_gfx_ctx.xdndPosition = xInternAtom(_gfx_ctx.display, "XdndPosition", 0);
	_gfx_ctx.xdndStatus = xInternAtom(_gfx_ctx.display, "XdndStatus", 0);
	_gfx_ctx.xdndLeave = xInternAtom(_gfx_ctx.display, "XdndLeave", 0);
	_gfx_ctx.xdndDrop = xInternAtom(_gfx_ctx.display, "XdndDrop", 0);
	_gfx_ctx.xdndFinished = xInternAtom(_gfx_ctx.display, "XdndFinished", 0);
	_gfx_ctx.xdndActionCopy = xInternAtom(_gfx_ctx.display, "XdndActionCopy", 0);
	_gfx_ctx.wmProtocols = xInternAtom(_gfx_ctx.display, "WM_PROTOCOLS", 0);
	_gfx_ctx.wmState = xInternAtom(_gfx_ctx.display, "WM_STATE", 0);
	_gfx_ctx.netWmStateFullscreen = xInternAtom(_gfx_ctx.display, "_NET_WM_STATE_FULLSCREEN", 0);
	_gfx_ctx.netWmStateAbove = xInternAtom(_gfx_ctx.display, "_NET_WM_STATE_ABOVE", 0);
	_gfx_ctx.netWmWindowOpacity = xInternAtom(_gfx_ctx.display, "_NET_WM_WINDOW_OPACITY", 0);
	_gfx_ctx.netWmName = xInternAtom(_gfx_ctx.display, "_NET_WM_NAME", 0);
	_gfx_ctx.netWmIcon = xInternAtom(_gfx_ctx.display, "_NET_WM_ICON", 0);
	_gfx_ctx.xselData = xInternAtom(_gfx_ctx.display, "XSEL_DATA", 0);
	_gfx_ctx.clipboardAtom = xInternAtom(_gfx_ctx.display, "CLIPBOARD", 0);
	_gfx_ctx.clipboardManager = xInternAtom(_gfx_ctx.display, "CLIPBOARD_MANAGER", 0);
	_gfx_ctx.netWorkarea = xInternAtom(_gfx_ctx.display, "_NET_WORKAREA", 0);
	_gfx_ctx.xtextUriList = xInternAtom(_gfx_ctx.display, "text/uri-list", 0);
	_gfx_ctx.xtextPlain = xInternAtom(_gfx_ctx.display, "text/plain", 0);

	_gfx_ctx.context = xUniqueContext();
	if (_gfx_ctx.context == 0) return -1;

	XSetWindowAttributes wa;
	wa.event_mask = 1 << 22; // PropertyChangeMask
	_gfx_ctx.helperWindow = xCreateWindow(_gfx_ctx.display, xDefaultRootWindow(_gfx_ctx.display), 0, 0, 1, 1, 0, 0,
	2 /* InputOnly */, xDefaultVisual(_gfx_ctx.display, xDefaultScreen(_gfx_ctx.display)), 1 << 11, &wa);
	if (_gfx_ctx.helperWindow == 0) return -1;

	_x11_cursor_handle = libc::dlopen("libXcursor.so.1", libc::RTLD_LAZY | libc::RTLD_LOCAL);
	if (_x11_cursor_handle != null)
	{
		_xcursor_image_create_src = (PFN_XcursorImageCreate)libc::dlsym(_x11_cursor_handle, "XcursorImageCreate");
		_xcursor_image_destroy_src = (PFN_XcursorImageDestroy)libc::dlsym(_x11_cursor_handle, "XcursorImageDestroy");
		_xcursor_image_load_cursor_src = (PFN_XcursorImageLoadCursor)libc::dlsym(_x11_cursor_handle, "XcursorImageLoadCursor");
	}
	_x11_xi_handle = libc::dlopen("libXi.so.6", libc::RTLD_LAZY | libc::RTLD_LOCAL);
	if (_x11_xi_handle != null) _xi_select_events_src = (PFN_XISelectEvents)libc::dlsym(_x11_xi_handle, "XISelectEvents");
	_x11_ext_handle = libc::dlopen("libXext.so.6", libc::RTLD_LAZY | libc::RTLD_LOCAL);
	if (_x11_ext_handle != null)
	{
		_xsync_create_counter_src = (PFN_XSyncCreateCounter)libc::dlsym(_x11_ext_handle, "XSyncCreateCounter");
		_xsync_int_to_value_src = (PFN_XSyncIntToValue)libc::dlsym(_x11_ext_handle, "XSyncIntToValue");
		_xsync_set_counter_src = (PFN_XSyncSetCounter)libc::dlsym(_x11_ext_handle, "XSyncSetCounter");
		_xshape_combine_region_src = (PFN_XShapeCombineRegion)libc::dlsym(_x11_ext_handle, "XShapeCombineRegion");
		_xshape_combine_mask_src = (PFN_XShapeCombineMask)libc::dlsym(_x11_ext_handle, "XShapeCombineMask");
	}

	char[4] blk = {0, 0, 0, 0};
	_gfx_ctx.hiddenMouse = todo_fix_createMouse((char*)&blk, 1, 1, RGBA8);
	_gfx_ctx.unixClipboard = null;

	xSetErrorHandler((void*)&todo_fix_xErrorHandler);
	xSync(_gfx_ctx.display, 0);

	xSetLocaleModifiers("");
	xRegisterIMInstantiateCallback(_gfx_ctx.display, null, null, null, (void*)&todo_fix_x11ImInitCallback, null);

	int errorBase;
	if (xRRQueryExtension(_gfx_ctx.display, &_gfx_ctx.xrandrEventBase, &errorBase) != 0)
	{
		xRRSelectInput(_gfx_ctx.display, xRootWindow(_gfx_ctx.display, xDefaultScreen(_gfx_ctx.display)), 1 << 4); // RROutputChangeNotifyMask
	}
	return 0;
}

// private
fn void? _load_opengl_x11()
{
	if (_gfx_ctx.nativeGL_handle && _gfx_ctx.glGetProcAddress) return;
	_gfx_ctx.nativeGL_handle = _dlopen_any("libGLX.so.0", "libGL.so.1", "libGL.so") ?? MISSING_OPENGL_LIB~!;
	void* ptr = libc::dlsym(_gfx_ctx.nativeGL_handle, "glXGetProcAddress");
	if (!ptr) return MISSING_GL_GET_PROC_ADDR~;
	_gfx_ctx.glGetProcAddress        = ptr;
	todo_fix_glXChooseVisual             = libc::dlsym(_gfx_ctx.nativeGL_handle, "glXChooseVisual");
	todo_fix_glXCreateContext            = libc::dlsym(_gfx_ctx.nativeGL_handle, "glXCreateContext");
	todo_fix_glXMakeCurrent              = libc::dlsym(_gfx_ctx.nativeGL_handle, "glXMakeCurrent");
	todo_fix_glXGetCurrentContext        = libc::dlsym(_gfx_ctx.nativeGL_handle, "glXGetCurrentContext");
	todo_fix_glXSwapBuffers              = libc::dlsym(_gfx_ctx.nativeGL_handle, "glXSwapBuffers");
	todo_fix_glXGetVisualFromFBConfig    = libc::dlsym(_gfx_ctx.nativeGL_handle, "glXGetVisualFromFBConfig");
	todo_fix_glXGetFBConfigAttrib        = libc::dlsym(_gfx_ctx.nativeGL_handle, "glXGetFBConfigAttrib");
	todo_fix_glXGetProcAddressARB        = libc::dlsym(_gfx_ctx.nativeGL_handle, "glXGetProcAddressARB");
	todo_fix_glXChooseFBConfig           = libc::dlsym(_gfx_ctx.nativeGL_handle, "glXChooseFBConfig");
	todo_fix_glXDestroyContext           = libc::dlsym(_gfx_ctx.nativeGL_handle, "glXDestroyContext");
	todo_fix_glXCreateWindow             = libc::dlsym(_gfx_ctx.nativeGL_handle, "glXCreateWindow");
	todo_fix_glXDestroyWindow            = libc::dlsym(_gfx_ctx.nativeGL_handle, "glXDestroyWindow");
	todo_fix_glXQueryExtensionsString    = libc::dlsym(_gfx_ctx.nativeGL_handle, "glXQueryExtensionsString");
	todo_fix_glXCreateContextAttribsARB  = libc::dlsym(_gfx_ctx.nativeGL_handle, "glXCreateContextAttribsARB");
	todo_fix_glXSwapIntervalEXT          = libc::dlsym(_gfx_ctx.nativeGL_handle, "glXSwapIntervalEXT");
	if (!todo_fix_glXSwapIntervalEXT)
	{
		ZString[2] array = {"GLX_MESA_swap_control", "GLX_SGI_swap_control"};
		foreach (name : array)
		{
			todo_fix_glXSwapIntervalNative = libc::dlsym(_gfx_ctx.nativeGL_handle, name);
			if (todo_fix_glXSwapIntervalNative) break;
		}
	}
}

// private
fn void todo_fix_monitorNode_free_X11(GfxMonitorNode* node) {}

// private
fn GfxMonitorMode[] _monitor_get_modes_x11(GfxMonitor* monitor)
{
	XRRScreenResources* res = xRRGetScreenResourcesCurrent(_gfx_ctx.display, xDefaultRootWindow(_gfx_ctx.display));
	if (!res) return {};
	XRRCrtcInfo* ci = xRRGetCrtcInfo(_gfx_ctx.display, res, monitor.node.nativeId1);
	GfxXRROutputInfo* oi = xRRGetOutputInfo(_gfx_ctx.display, res, monitor.node.nativeId0);
	sz count = (sz)oi.nmode;
	List{GfxMonitorMode} modes;
	for (int i = 0; i < oi.nmode; i++)
	{
		GfxMonitorMode mode;
		todo_fix_xGetMode(ci, res, oi.modes[i], &mode);
		modes.push(mode);
	}
	xRRFreeOutputInfo(oi);
	xRRFreeCrtcInfo(ci);
	xRRFreeScreenResources(res);
	return modes.array_view();
}

// private
fn bool todo_fix_monitor_getWorkarea_X11(GfxMonitor* monitor, int* x, int* y, int* width, int* height)
{
	XWindow root = xDefaultRootWindow(_gfx_ctx.display);
	int areaX = monitor.origin.x;
	int areaY = monitor.origin.y;
	int areaW = monitor.mode.size.x;
	int areaH = monitor.mode.size.y;

	if (_gfx_ctx.netWorkarea != 0 && _gfx_ctx.netCurrentDesktop != 0)
	{
		Atom* extents = null;
		Atom* desktop = null;
		Atom actualType; int actualFormat; CULong extentCount, bytesAfter, count;
		xGetWindowProperty(_gfx_ctx.display, root, _gfx_ctx.netWorkarea, 0, ~0, 0, 6, &actualType, &actualFormat, &extentCount, &bytesAfter, (char**)&extents);
		xGetWindowProperty(_gfx_ctx.display, root, _gfx_ctx.netCurrentDesktop, 0, ~0, 0, 6, &actualType, &actualFormat, &count, &bytesAfter, (char**)&desktop);

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

	if (x != null) *x = areaX;
	if (y != null) *y = areaY;
	if (width != null) *width = areaW;
	if (height != null) *height = areaH;
	return true;
}

// private
fn bool todo_fix_monitor_requestMode_X11(GfxMonitor* mon, GfxMonitorMode* mode, GfxMonitorModeRequest request)
{
	bool output = false;
	XRRScreenResources* res = xRRGetScreenResourcesCurrent(_gfx_ctx.display, xDefaultRootWindow(_gfx_ctx.display));
	if (res == null) return false;
	XRRCrtcInfo* ci = xRRGetCrtcInfo(_gfx_ctx.display, res, mon.node.nativeId1);
	GfxXRROutputInfo* oi = xRRGetOutputInfo(_gfx_ctx.display, res, mon.node.nativeId0);
	RRMode native = 0;

	for (int i = 0; i < oi.nmode; i++)
	{
		GfxMonitorMode found_mode;
		void* mi = todo_fix_xGetMode(ci, res, oi.modes[i], &found_mode);
		if (mi == null) continue;
		if (mode.equals_request(&found_mode, request))
		{
			native = ((XRRModeInfoNative*)mi).id;
			output = true;
			mon.mode = found_mode;
			break;
		}
	}
	if (native != 0) xRRSetCrtcConfig(_gfx_ctx.display, res, mon.node.nativeId1, 0, ci.x, ci.y, native, ci.rotation, ci.outputs, ci.noutput);
	xRRFreeOutputInfo(oi);
	xRRFreeCrtcInfo(ci);
	xRRFreeScreenResources(res);
	return output;
}

// private
fn bool todo_fix_monitor_setMode_X11(GfxMonitor* mon, GfxMonitorMode* mode)
{
	bool out = false;
	XRRScreenResources* res = xRRGetScreenResourcesCurrent(_gfx_ctx.display, xDefaultRootWindow(_gfx_ctx.display));
	XRRCrtcInfo* ci = xRRGetCrtcInfo(_gfx_ctx.display, res, mon.node.nativeId1);
	if (xRRSetCrtcConfig(_gfx_ctx.display, res, mon.node.nativeId1, 0, ci.x, ci.y, (RRMode)(uptr)mode.src, ci.rotation, ci.outputs, ci.noutput) == 0) out = true;
	xRRFreeCrtcInfo(ci);
	xRRFreeScreenResources(res);
	return out;
}

// private
fn GfxKey todo_fix_physicalToMappedKey_X11(GfxKey key)
{
	KeyCode keycode = (KeyCode)todo_fix_rgfwToApiKey(key);
	KeySym sym = xkbKeycodeToKeysym(_gfx_ctx.display, keycode, 0, 0);
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
fn void todo_fix_pollEvents_X11()
{
	todo_fix_resetPrevState();
	xPending(_gfx_ctx.display);
	while (qLength(_gfx_ctx.display) != 0) todo_fix_xHandleEvent();
}

fn void todo_fix_xHandleEvent()
{
	static Vec2 delta = {};

	XEvent e;
	xNextEvent(_gfx_ctx.display, &e);

	if (e.type != 35 /* GenericEvent */)
	{
		delta = {};
	}

	if (e.type == _gfx_ctx.xrandrEventBase + 0 /* RRNotify */)
	{
		todo_fix_pollMonitors();
		return;
	}

	if (e.type == 30 /* SelectionRequest */)
	{
		todo_fix_xHandleClipboardSelection(&e);
		return;
	}

	if (e.type == 35 /* GenericEvent */)
	{
		xGetEventData(_gfx_ctx.display, &e.xcookie);
		if (e.xcookie.evtype == 17 /* XI_RawMotion */)
		{
			XIRawEvent* raw = (XIRawEvent*)e.xcookie.data;
			if (raw.valuators.mask_len != 0)
			{
				int index = 0;
				if (ximaskIsSet(raw.valuators.mask, 0))
				{
					delta.x += (float)raw.raw_values[index];
					index++;
				}
				if (ximaskIsSet(raw.valuators.mask, 1)) delta.y += (float)raw.raw_values[index];
				_gfx_ctx.vector = delta;
				_raw_motion_callback(_gfx_ctx.root, delta);
			}
		}
		xFreeEventData(_gfx_ctx.display, &e.xcookie);
		return;
	}

	GfxWindow* win = null;
	if (xFindContext(_gfx_ctx.display, e.xany.window, _gfx_ctx.context, (XPointer*)&win) != 0) return;

	if (win.src.flashEnd != 0)
	{
		if (win.src.flashEnd <= todo_fix_unix_getTimeNS() || win.has_focus()) win.flash(CANCEL);
	}

	bool keyRepeat = false;
	if (e.type == 3 /* KeyRelease */ && xEventsQueued(_gfx_ctx.display, 1) != 0)
	{
		XEvent ne;
		xPeekEvent(_gfx_ctx.display, &ne);
		if (ne.type == 2 /* KeyPress */ && e.xkey.time == ne.xkey.time && e.xkey.keycode == ne.xkey.keycode)
		{
			xNextEvent(_gfx_ctx.display, &e);
			keyRepeat = true;
		}
	}

	switch (e.type)
	{
		case 2: // KeyPress
			{
				if (!(win.internal.enabled_events & TODO_RENAME_KEY_PRESSED_FLAG)) return;
				GfxKey value = (GfxKey)todo_fix_apiKeyToRGFW(e.xkey.keycode);

				XkbStateRec state;
				xkbGetState(_gfx_ctx.display, XKB_USE_CORE_KBD, &state);
				_key_update_key_mods(win, state.locked_mods & LOCK_MASK != 0, state.locked_mods & MOD2_MASK != 0, state.locked_mods & MOD3_MASK != 0);

				if (win.src.ic != null && xFilterEvent(&e, 0) == 0)
				{
					char[100] buffer;
					int status;
					sz count = xutf8LookupString(win.src.ic, &e.xkey, (char*)&buffer, 99, null, &status);
					if (status == 2 /* XLookupChars */ || status == 4 /* XLookupBoth */)
					{
						buffer[count] = 0;
						sz index = 0;
						while (index < count)
						{
							todo_fix_keyCharCallback(win, todo_fix_decodeUTF8((ZString)&buffer, &index));
						}
					}
				}
				else
				{
					XWindow rootRet, childRet;
					int rootX, rootY, winX, winY;
					uint mask;
					xQueryPointer(_gfx_ctx.display, xDefaultRootWindow(_gfx_ctx.display), &rootRet, &childRet, &rootX, &rootY, &winX, &winY, &mask);
					KeySym sym = xkbKeycodeToKeysym(_gfx_ctx.display, (KeyCode)e.xkey.keycode, 0, (mask & SHIFT_MASK) ? 1 : 0);

					if ((mask & LOCK_MASK) && sym >= 0x0061 /* XK_a */ && sym <= 0x007a /* XK_z */)
					{
						sym = (mask & SHIFT_MASK) ? sym + 32 : sym - 32;
					}
					if ((KeySym)(char)sym != sym) sym = 0;

					todo_fix_keyCharCallback(win, (uint)(char)sym);
				}

				todo_fix_keyCallback(win, value, win.internal.mod, keyRepeat, true);
				break;
			}
		case 3: // KeyRelease
			{
				if (!(win.internal.enabled_events & TODO_RENAME_KEY_RELEASED_FLAG)) return;
				GfxKey value = (GfxKey)todo_fix_apiKeyToRGFW(e.xkey.keycode);

				XkbStateRec state;
				xkbGetState(_gfx_ctx.display, XKB_USE_CORE_KBD, &state);
				_key_update_key_mods(win, state.locked_mods & LOCK_MASK != 0, state.locked_mods & MOD2_MASK != 0, state.locked_mods & MOD3_MASK != 0);

				todo_fix_keyCallback(win, value, win.internal.mod, false, false);
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
					todo_fix_mouseScrollCallback(win, scroll_delta);
					break;
				}
				todo_fix_mouseButtonCallback(win, value, true);
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
				todo_fix_mouseButtonCallback(win, value, false);
				break;
			}
		case 6: _mouse_motion_callback(win, { e.xmotion.x, e.xmotion.y }); // MotionNotify
		case 12: todo_fix_windowRefreshCallback(win, { e.xexpose.x, e.xexpose.y, e.xexpose.width, e.xexpose.height }); // Expose
		case 28: // PropertyNotify
			if (e.xproperty.state == 0 /* PropertyNewValue */)
			{
				bool handled = false;
				if (e.xproperty.atom == _gfx_ctx.wmState)
				{
					if (_window_is_minimized_native(win) && !(win.internal.flags.minimize))
					{
						todo_fix_windowMinimizedCallback(win);
						handled = true;
					}
				}
				else if (e.xproperty.atom == _gfx_ctx.netWmState)
				{
					if (_window_is_maximized_native(win) && !(win.internal.flags.maximize))
					{
						todo_fix_windowMaximizedCallback(win, win.bounds);
						handled = true;
					}
				}
				if (!handled) todo_fix_window_checkMode(win);
			}
		case 19: case 18: // MapNotify, UnmapNotify
			todo_fix_window_checkMode(win);
			break;
		case 33: // ClientMessage
			{
				if ((CULong)e.xclient.data.l[0] == _gfx_ctx.wmDeleteWindow)
				{
					_window_close_callback(win);
					break;
				}

				if (!(win.internal.flags.allow_dnd) || _gfx_ctx.x11Version > 5 /* TODO_RENAME_XDND_VERSION */) return;

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
						xGetWindowProperty(_gfx_ctx.display, _gfx_ctx.x11Source, _gfx_ctx.xdndTypeList, 0, ~0, 0, 4,
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

					if (list != 0 && formats != null) xFree(formats);

					todo_fix_dataDragCallback(win, _gfx_ctx.x11TransferType, ENTER, drag);
				}
				else if (e.xclient.message_type == _gfx_ctx.xdndPosition)
				{
					int xabs = (int)((e.xclient.data.l[2] >> 16) & 0xffff);
					int yabs = (int)(e.xclient.data.l[2] & 0xffff);
					XWindow dummy;
					int xpos, ypos;

					xTranslateCoordinates(_gfx_ctx.display, xDefaultRootWindow(_gfx_ctx.display), win.src.window,
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

					xSendEvent(_gfx_ctx.display, _gfx_ctx.x11Source, 0, 0, &reply);
					xFlush(_gfx_ctx.display);

					todo_fix_dataDragCallback(win, _gfx_ctx.x11TransferType, MOVE, drag);
				}
				else if (e.xclient.message_type == _gfx_ctx.xdndLeave)
				{
					todo_fix_dataDragCallback(win, _gfx_ctx.x11TransferType, EXIT, drag);
				}
				else if (e.xclient.message_type == _gfx_ctx.xdndDrop)
				{
					if (_gfx_ctx.x11Format != 0)
					{
						Time_t time = _gfx_ctx.x11Version >= 1 ?(Time_t)e.xclient.data.l[2] : 0; // CurrentTime
						xConvertSelection(_gfx_ctx.display, _gfx_ctx.xdndSelection, (Atom)_gfx_ctx.x11Format,
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

						xSendEvent(_gfx_ctx.display, _gfx_ctx.x11Source, 0, 0, &reply);
						xFlush(_gfx_ctx.display);
					}
				}
				break;
			}
		case 31: // SelectionNotify
			{
				if (!(win.internal.enabled_events & TODO_RENAME_DATA_DROP_FLAG) || e.xselection.property != _gfx_ctx.xdndSelection || !(win.internal.flags.allow_dnd)) return;

				ZString data;
				CULong result;
				Atom actualType;
				int actualFormat;
				CULong bytesAfter;

				xGetWindowProperty(_gfx_ctx.display, e.xselection.requestor, e.xselection.property, 0, ~0, 0, e.xselection.target,
				&actualType, &actualFormat, &result, &bytesAfter, (char**)&data);

				if (result != 0)
				{
					todo_fix_unix_parseURI(win, data);
					if (data != null) xFree(data);
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
					xSendEvent(_gfx_ctx.display, _gfx_ctx.x11Source, 0, 0, &reply);
					xFlush(_gfx_ctx.display);
				}
				break;
			}
		case 22: // ConfigureNotify
			{
				if (e.xconfigure.width != win.src.width || e.xconfigure.height != win.src.height)
				{
					todo_fix_window_checkMode(win);
					win.src.size = { e.xconfigure.width, e.xconfigure.height };
					_window_resized_callback(win, win.src.size);
				}
				GfxPoint configure = { e.xconfigure.x, e.xconfigure.y };

				// if the event came from the server and we're not a direct child of the root window then
				// we're using local coords which need to be translated into screen coords
				XWindow root = xDefaultRootWindow(_gfx_ctx.display);
				if (e.xany.send_event == 0 && win.src.parent != root)
				{
					XWindow dummy;
					xTranslateCoordinates(_gfx_ctx.display, win.src.parent, root, ...configure, &configure.x, &configure.y, &dummy);
				}

				if (configure != win.origin)
				{
					win.src.origin = configure;
					_window_moved_callback(win, configure);
				}
				return;
			}
		case 9: // FocusIn
			if (win.src.ic) xSetICFocus(win.src.ic);
			todo_fix_windowFocusCallback(win, true);
		case 10: // FocusOut
			if (win.src.ic) xUnsetICFocus(win.src.ic);
			todo_fix_windowFocusCallback(win, false);
		case 7: _mouse_notify_callback(win, { e.xcrossing.x, e.xcrossing.y }, true); // EnterNotify
		case 8: _mouse_notify_callback(win, win.internal.last_mouse, false); // LeaveNotify
		case 21: win.src.parent = e.xreparent.parent; // ReparentNotify
		default: break;
	}

	xFlush(_gfx_ctx.display);
}

// private
fn void todo_fix_pollMonitors_X11()
{
	XWindow root = xDefaultRootWindow(_gfx_ctx.display);
	XRRScreenResources* res = xRRGetScreenResourcesCurrent(_gfx_ctx.display, root);
	if (res == null) return;
	RROutput primary = xRRGetOutputPrimary(_gfx_ctx.display, root);

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
		GfxXRROutputInfo* info = xRRGetOutputInfo(_gfx_ctx.display, res, res.outputs[i]);
		if (info == null) continue;
		if (info.connection != 0 || info.crtc == 0) { xRRFreeOutputInfo(info); continue; }

		XRRCrtcInfo* ci = xRRGetCrtcInfo(_gfx_ctx.display, res, info.crtc);
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
		todo_fix_xGetSystemContentDPI(&dpi);
		monitor.scale = (Vec2)(dpi / 96.0f);
		monitor.pixelRatio = dpi >= 192.0f ? 2.0f : 1.0f;

		void* mi = todo_fix_xGetMode(ci, res, ci.mode, &monitor.mode);
		if (mi == null) break;

		xRRFreeCrtcInfo(ci);
		node = _monitor_add(&monitor);
		if (node == null) break;
		node.nativeId0 = res.outputs[i];
		node.nativeId1 = info.crtc;
		if (node.nativeId0 == primary) _gfx_ctx.monitors.primary = node;
		xRRFreeOutputInfo(info);
		todo_fix_monitorCallback(_gfx_ctx.root, &node.mon, true);
	}

	xRRFreeScreenResources(res);
	refresh_monitors();
}

// private
fn void todo_fix_window_closePlatform_X11(GfxWindow* win)
{
	if (win.src.ic != null) { xDestroyIC(win.src.ic); win.src.ic = null; }
	xFreeGC(_gfx_ctx.display, win.src.gc);
	xDeleteContext(_gfx_ctx.display, win.src.window, _gfx_ctx.context);
	xDestroyWindow(_gfx_ctx.display, win.src.window);
}

// private
fn void todo_fix_window_deleteContextPtr_OpenGL_X11(GfxWindow* win, GfxGlContext* ctx)
{
	if (win.src.ctx.native.window != win.src.window) todo_fix_glXDestroyWindow(_gfx_ctx.display, win.src.ctx.native.window);
	todo_fix_glXDestroyContext(_gfx_ctx.display, ctx.ctx);
	win.src.ctx.native = null;
	todo_fix_debugCallback(TODO_RENAME_TYPE_INFO, TODO_RENAME_INFO_OPEN_GL, "OpenGL context freed.");
}

// private
fn void todo_fix_window_focus_X11(GfxWindow* win)
{
	XWindowAttributes attr;
	xGetWindowAttributes(_gfx_ctx.display, win.src.window, &attr);
	if (attr.map_state != 2 /* IsViewable */) return;
	xSetInputFocus(_gfx_ctx.display, win.src.window, 1 /* RevertToPointerRoot */, 0);
	xFlush(_gfx_ctx.display);
}

// private
fn void todo_fix_window_hide_X11(GfxWindow* win)
{
	win.internal.flags.hide = true;
	xUnmapWindow(_gfx_ctx.display, win.src.window);
	xFlush(_gfx_ctx.display);
}

// private
fn bool _window_is_hidden_x11(GfxWindow* win)
{
	XWindowAttributes windowAttributes;
	xGetWindowAttributes(_gfx_ctx.display, win.src.window, &windowAttributes);
	return windowAttributes.map_state != 2;
}

// private
fn bool _window_is_maximized_x11(GfxWindow* win)
{
	Atom actualType;
	int actualFormat;
	CULong nitems, bytesAfter;
	char* propData;
	int status = xGetWindowProperty(_gfx_ctx.display, win.src.window, _gfx_ctx.netWmState, 0, 1024, 0, 4, &actualType, &actualFormat, &nitems, &bytesAfter, &propData);
	if (status != 0) { if (propData != null) xFree(propData); return false; }
	for (CULong i = 0; i < nitems; i++)
	{
		if ((Atom)propData[i] == _gfx_ctx.net_wm_state_maximized_vert || (Atom)propData[i] == _gfx_ctx.net_wm_state_maximized_horz)
		{
			xFree(propData);
			return true;
		}
	}
	if (propData != null) xFree(propData);
	return false;
}

// private
fn bool _window_is_minimized_x11(GfxWindow* win)
{
	Atom actualType;
	int actualFormat;
	CULong nitems, bytesAfter;
	char* propData;
	int status = xGetWindowProperty(_gfx_ctx.display, win.src.window, _gfx_ctx.wmState, 0, 2, 0, 0, &actualType, &actualFormat, &nitems, &bytesAfter, &propData);
	if (status == 0 && nitems >= 1 && propData == (char*)(uptr)3 /* IconicState */)
	{
		xFree(propData);
		return true;
	}
	if (propData != null) xFree(propData);
	XWindowAttributes windowAttributes;
	xGetWindowAttributes(_gfx_ctx.display, win.src.window, &windowAttributes);
	return windowAttributes.map_state != 2;
}

// private
fn void todo_fix_window_makeCurrentContext_OpenGL_X11(GfxWindow* win)
{
	if (win == null) { todo_fix_glXMakeCurrent(null, 0, null); }
	else { todo_fix_glXMakeCurrent(_gfx_ctx.display, win.src.ctx.native.window, win.src.ctx.native.ctx); }
}

// private
fn void _window_maximize_x11(GfxWindow* win) => _toggle_x_maximized(win, true);

// private
fn void todo_fix_window_minimizePlatform_X11(GfxWindow* win)
{
	if (_window_is_maximized_native(win)) return;
	win.internal.old_bounds = win.bounds;
	xIconifyWindow(_gfx_ctx.display, win.src.window, xDefaultScreen(_gfx_ctx.display));
	xFlush(_gfx_ctx.display);
}

// private
fn void _window_move_mouse_x11(GfxWindow* win, GfxPoint where)
{
	int rootX, rootY, win_x, win_y;
	uint mask;
	XWindow root_ret, child_ret;
	xQueryPointer(_gfx_ctx.display, xDefaultRootWindow(_gfx_ctx.display), &root_ret, &child_ret, &rootX, &rootY, &win_x, &win_y, &mask);
	win.internal.last_mouse = where - win.origin;
	if (win_x == where.x && win_y == where.y) return;
	xWarpPointer(_gfx_ctx.display, 0, win.src.window, 0, 0, 0, 0, ...(where - win.origin));
}

fn void _window_move_x11(GfxWindow* win, GfxPoint where)
{
	win.origin = where;
	xMoveWindow(_gfx_ctx.display, win.src.window, ...where);
}

// private
fn void todo_fix_window_raise_X11(GfxWindow* win)
{
	xMapRaised(_gfx_ctx.display, win.src.window);
	_window_set_fullscreen_native(win, win.is_fullscreen());
}

// private
fn void _window_resize_x11(GfxWindow* win, GfxSize size)
{
	win.size = size;
	xResizeWindow(_gfx_ctx.display, win.src.window, (uint)size.x, (uint)size.y);
	if (win.internal.flags.no_resize)
	{
		XSizeHints sh;
		sh.flags = (1 << 4) | (1 << 5);
		sh.min_width = sh.max_width = size.x;
		sh.min_height = sh.max_height = size.y;
		xSetWMSizeHints(_gfx_ctx.display, win.src.window, &sh, 0);
	}
}

// private
fn void _window_restore_x11(GfxWindow* win)
{
	_toggle_x_maximized(win, false);
	xFlush(_gfx_ctx.display);
}

// private
fn void todo_fix_window_setFloating_X11(GfxWindow* win, bool floating) => todo_fix_window_setXAtom(win, _gfx_ctx.netWmStateAbove, floating);

// private
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

	xRaiseWindow(_gfx_ctx.display, win.src.window);
	todo_fix_window_setXAtom(win, _gfx_ctx.netWmStateFullscreen, fullscreen);

	if (!(win.internal.flags.transparent))
	{
		char value = (char)fullscreen;
		xChangeProperty(_gfx_ctx.display, win.src.window, _gfx_ctx.netWmBypassCompositor, 6 /* XA_CARDINAL */, 32, 0, &value, 1);
	}
}

// private
fn void _window_set_max_size_x11(GfxWindow* win, GfxSize size)
{
	CLong flags;
	XSizeHints hints;
	xGetWMNormalHints(_gfx_ctx.display, win.src.window, &hints, &flags);
	hints.flags |= 1 << 5; // PMaxSize
	hints.max_width = size.x; hints.max_height = size.y;
	xSetWMNormalHints(_gfx_ctx.display, win.src.window, &hints);
}

// private
fn void _window_set_min_size_x11(GfxWindow* win, GfxSize size)
{
	CLong flags;
	XSizeHints hints;
	xGetWMNormalHints(_gfx_ctx.display, win.src.window, &hints, &flags);
	hints.flags |= 1 << 4; // PMinSize
	hints.min_width = size.x; hints.min_height = size.y;
	xSetWMNormalHints(_gfx_ctx.display, win.src.window, &hints);
}

// private
fn bool _window_set_mouse_x11(GfxWindow* win, TodoRenameMouse* mouse)
{
	xDefineCursor(_gfx_ctx.display, win.src.window, (XCursor)mouse);
	return true;
}

// private
fn void todo_fix_window_setOpacity_X11(GfxWindow* win, char opacity)
{
	uint value = (uint)(4294967295.0 * (double)opacity);
	xChangeProperty(_gfx_ctx.display, win.src.window, _gfx_ctx.netWmWindowOpacity, 6, 32, 0, (char*)&value, 1);
}

// private
fn void todo_fix_window_swapBuffers_OpenGL_X11(GfxWindow* win) { todo_fix_glXSwapBuffers(_gfx_ctx.display, win.src.ctx.native.window); }

// private
fn bool todo_fix_writeClipboard_X11(GfxDataTransfer* data)
{
	xSetSelectionOwner(_gfx_ctx.display, _gfx_ctx.clipboardAtom, _gfx_ctx.helperWindow, 0);
	if (xGetSelectionOwner(_gfx_ctx.display, _gfx_ctx.clipboardAtom) != _gfx_ctx.helperWindow)
	{
		todo_fix_debugCallback(TODO_RENAME_TYPE_ERROR, TODO_RENAME_ERR_CLIPBOARD, "X11 failed to become owner of clipboard selection");
		return false;
	}
	if (_gfx_ctx.unixClipboard)
	{
		free(_gfx_ctx.unixClipboard);
		_gfx_ctx.unixClipboard = null;
	}

	sz length = data.length;
	if (data.data[data.length - 1] != 0) length += 1;

	_gfx_ctx.unixClipboard = mem::new_with_padding(GfxDataTransfer, data.length);
	char* dataPtr = (char*)_gfx_ctx.unixClipboard + GfxDataTransfer::size - 1;
	mem::copy(dataPtr, data.data, data.length);
	dataPtr[length - 1] = 0;
	_gfx_ctx.unixClipboard.data = dataPtr;
	_gfx_ctx.unixClipboard.type = TEXT;
	_gfx_ctx.unixClipboard.length = length;
	return true;
}

// private
fn bool todo_fix_readClipboardPtr_X11(GfxDataTransferType requestedType, char* buffer, sz capacity, GfxDataTransfer* dataTransfer)
{
	if (requestedType != TEXT) return false;
	dataTransfer.data = buffer;

	if (xGetSelectionOwner(_gfx_ctx.display, _gfx_ctx.clipboardAtom) == _gfx_ctx.helperWindow)
	{
		dataTransfer.length = _gfx_ctx.unixClipboard.length;
		if (buffer != null && _gfx_ctx.unixClipboard.data != null)
		{
			if (_gfx_ctx.unixClipboard.length > capacity) return false;
			mem::copy(buffer, (void*)_gfx_ctx.unixClipboard.data, _gfx_ctx.unixClipboard.length);
		}
		dataTransfer.type = TEXT;
		return true;
	}

	xConvertSelection(_gfx_ctx.display, _gfx_ctx.clipboardAtom, _gfx_ctx.utf8String, _gfx_ctx.xselData, _gfx_ctx.helperWindow, 0);
	xSync(_gfx_ctx.display, 0);
	XEvent event;
	while (true)
	{
		xNextEvent(_gfx_ctx.display, &event);
		if (event.type != 31 /* SelectionNotify */) continue;
		if (event.xselection.selection != _gfx_ctx.clipboardAtom || event.xselection.property == 0) return false;
		break;
	}

	Atom target;
	int format;
	CULong n, size;
	char* data;
	xGetWindowProperty(_gfx_ctx.display, event.xselection.requestor, event.xselection.property, 0, ~0, 0, 0, &target, &format, &size, &n, (char**)&data);

	bool ret = true;
	sz length = (sz)size;
	if (data[size - 1] != 0) length += 1;

	if (size > (CULong)capacity && buffer != null) { ret = false; }
	else if ((target == _gfx_ctx.utf8String || target == 31) && buffer != null)
	{
		mem::copy(buffer, data, (sz)size);
		buffer[length - 1] = 0;
		xFree(data);
	}
	else if (buffer != null) { ret = false; }

	xDeleteProperty(_gfx_ctx.display, event.xselection.requestor, event.xselection.property);
	dataTransfer.length = length;
	dataTransfer.type = TEXT;
	return ret;
}

// private
fn bool todo_fix_window_createContextPtr_OpenGL_X11(GfxWindow* win, GfxGlContext* context, GfxGlHints* hints)
{
	win.src.ctx.native = context;
	win.src.gfxType = TODO_RENAME_GFX_NATIVE_OPEN_GL;

	bool showWindow = false;
	if (win.src.window != 0)
	{
		showWindow = !_window_is_minimized_native(win);
		todo_fix_window_closePlatform(win);
	}

	bool transparent = win.internal.flags.transparent;

	int[40] visualAttribs;
	TodoRenameAttribStack stack;
	todo_fix_attribStack_init(&stack, (int*)&visualAttribs, 40);
	todo_fix_attribStack_pushAttribs(&stack, 0x22, 0x8002); // GLX_X_VISUAL_TYPE, GLX_TRUE_COLOR
	todo_fix_attribStack_pushAttribs(&stack, 0x8012, 1); // GLX_X_RENDERABLE
	todo_fix_attribStack_pushAttribs(&stack, 0x8011, 0x1); // GLX_RENDER_TYPE, GLX_RGBA_BIT
	todo_fix_attribStack_pushAttribs(&stack, 0x8010, 0x1); // GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT
	todo_fix_attribStack_pushAttribs(&stack, 5, 1); // GLX_DOUBLEBUFFER
	todo_fix_attribStack_pushAttribs(&stack, 11, hints.alpha); // GLX_ALPHA_SIZE
	todo_fix_attribStack_pushAttribs(&stack, 12, hints.depth); // GLX_DEPTH_SIZE
	todo_fix_attribStack_pushAttribs(&stack, 13, hints.stencil); // GLX_STENCIL_SIZE
	todo_fix_attribStack_pushAttribs(&stack, 6, hints.stereo); // GLX_STEREO
	todo_fix_attribStack_pushAttribs(&stack, 7, hints.auxBuffers); // GLX_AUX_BUFFERS
	todo_fix_attribStack_pushAttribs(&stack, 8, hints.red); // GLX_RED_SIZE
	todo_fix_attribStack_pushAttribs(&stack, 9, hints.green); // GLX_GREEN_SIZE
	todo_fix_attribStack_pushAttribs(&stack, 10, hints.blue); // GLX_BLUE_SIZE
	todo_fix_attribStack_pushAttribs(&stack, 14, hints.accumRed); // GLX_ACCUM_RED_SIZE
	todo_fix_attribStack_pushAttribs(&stack, 15, hints.accumGreen);
	todo_fix_attribStack_pushAttribs(&stack, 16, hints.accumBlue);
	todo_fix_attribStack_pushAttribs(&stack, 17, hints.accumAlpha);

	if (hints.sRGB)
	{
		ZString srgbArbStr = "GLX_ARB_framebuffer_sRGB";
		ZString srgbExtStr = "GLX_EXT_framebuffer_sRGB";
		if (todo_fix_extensionSupportedPlatform_OpenGL_X11(srgbArbStr, srgbArbStr.len() + 1)) todo_fix_attribStack_pushAttribs(&stack, 0x20B2, (int)hints.sRGB); // GLX_FRAMEBUFFER_SRGB_CAPABLE_ARB
		if (todo_fix_extensionSupportedPlatform_OpenGL_X11(srgbExtStr, srgbExtStr.len() + 1)) todo_fix_attribStack_pushAttribs(&stack, 0x20B2, (int)hints.sRGB); // GLX_FRAMEBUFFER_SRGB_CAPABLE_EXT
	}

	todo_fix_attribStack_pushAttribs(&stack, 0, 0);

	int fbcount;
	GLXFBConfig* fbc = todo_fix_glXChooseFBConfig(_gfx_ctx.display, xDefaultScreen(_gfx_ctx.display), (int*)&visualAttribs, &fbcount);
	if (fbc == null || fbcount == 0)
	{
		todo_fix_debugCallback(TODO_RENAME_TYPE_ERROR, TODO_RENAME_ERR_OPEN_GL_CONTEXT, "Failed to find any valid GLX visual configs.");
		return false;
	}

	int bestFbcIdx = -1;
	int bestDepth = 0;
	int bestSamples = 0;
	for (int i = 0; i < fbcount; i++)
	{
		XVisualInfo* vi = todo_fix_glXGetVisualFromFBConfig(_gfx_ctx.display, fbc[i]);
		if (vi == null) continue;
		int sampBuf, samples;
		todo_fix_glXGetFBConfigAttrib(_gfx_ctx.display, fbc[i], 100000, &sampBuf); // GLX_SAMPLE_BUFFERS
		todo_fix_glXGetFBConfigAttrib(_gfx_ctx.display, fbc[i], 100001, &samples); // GLX_SAMPLES
		if (bestFbcIdx == -1) bestFbcIdx = i;
		if ((!transparent || vi.depth == 32) && bestDepth == 0) { bestFbcIdx = i; bestDepth = vi.depth; }
		if ((!transparent || vi.depth == 32) && samples <= hints.samples && samples > bestSamples) { bestFbcIdx = i; bestDepth = vi.depth; bestSamples = samples; }
		xFree(vi);
	}
	if (bestFbcIdx == -1)
	{
		todo_fix_debugCallback(TODO_RENAME_TYPE_ERROR, TODO_RENAME_ERR_OPEN_GL_CONTEXT, "Failed to get a valid GLX visual.");
		return false;
	}

	GLXFBConfig bestFbc = fbc[bestFbcIdx];
	XVisualInfo* vi = todo_fix_glXGetVisualFromFBConfig(_gfx_ctx.display, bestFbc);
	xFree(fbc);
	XVisualInfo visual = *vi;
	xFree(vi);

	if (catch todo_fix_xCreateWindow(visual, "", win.internal.flags, win)) return false;
	if (showWindow) win.show();

	int[40] contextAttribs;
	todo_fix_attribStack_init(&stack, (int*)&contextAttribs, 40);
	int mask = 0;
	switch (hints.profile)
	{
		case TODO_RENAME_GL_ES: mask |= 0x4;
		case TODO_RENAME_GL_FORWARD_COMPATIBILITY: mask |= 0x2;
		case TODO_RENAME_GL_COMPATIBILITY: mask |= 0x2;
		default: mask |= 0x1; // GLX_CONTEXT_CORE_PROFILE_BIT_ARB
	}
	todo_fix_attribStack_pushAttribs(&stack, 0x9126, mask); // GLX_CONTEXT_PROFILE_MASK_ARB
	if (hints.minor != 0 || hints.major != 0)
	{
		todo_fix_attribStack_pushAttribs(&stack, 0x2091, hints.major);
		todo_fix_attribStack_pushAttribs(&stack, 0x2092, hints.minor);
	}

	ZString flushStr = "GLX_ARB_context_flush_control";
	if (todo_fix_extensionSupportedPlatform_OpenGL_X11(flushStr, flushStr.len() + 1))
	{
		if (hints.releaseBehavior == TODO_RENAME_GL_RELEASE_FLUSH) { todo_fix_attribStack_pushAttribs(&stack, 0x2097, 0x2098); } // GLX_CONTEXT_RELEASE_BEHAVIOR_ARB, GLX_CONTEXT_RELEASE_BEHAVIOR_FLUSH_ARB
		else if (hints.releaseBehavior == TODO_RENAME_GL_RELEASE_NONE) { todo_fix_attribStack_pushAttribs(&stack, 0x2097, 0); } // GLX_CONTEXT_RELEASE_BEHAVIOR_ARB, GLX_CONTEXT_RELEASE_BEHAVIOR_NONE_ARB
	}

	ZString robustStr = "GLX_ARB_create_context_robustness";
	int glFlags = 0;
	if (hints.debug) glFlags |= 0x00000001; // GLX_CONTEXT_DEBUG_BIT_ARB
	if (hints.robustness && todo_fix_extensionSupportedPlatform_OpenGL_X11(robustStr, robustStr.len() + 1)) glFlags |= 0x00000004; // GLX_CONTEXT_ROBUST_ACCESS_BIT_ARB
	if (glFlags != 0) todo_fix_attribStack_pushAttribs(&stack, 0x2094, glFlags); // GLX_CONTEXT_FLAGS_ARB

	ZString noErrorStr = "GLX_ARB_create_context_no_error";
	if (todo_fix_extensionSupportedPlatform_OpenGL_X11(noErrorStr, noErrorStr.len() + 1)) todo_fix_attribStack_pushAttribs(&stack, 0x31B3, (int)hints.noError); // GLX_CONTEXT_OPENGL_NO_ERROR_ARB

	todo_fix_attribStack_pushAttribs(&stack, 0, 0);

	GLXContext ctx = null;
	if (hints.share != null) ctx = hints.share.ctx;

	if (todo_fix_glXCreateContextAttribsARB == null)
	{
		todo_fix_debugCallback(TODO_RENAME_TYPE_ERROR, TODO_RENAME_ERR_OPEN_GL_CONTEXT, "Failed to load proc address 'glXCreateContextAttribsARB', loading a generic OpenGL context.");
		win.src.ctx.native.ctx = todo_fix_glXCreateContext(_gfx_ctx.display, &visual, ctx, X_TRUE);
	}
	else
	{
		_gfx_ctx.x11Error = null;
		win.src.ctx.native.ctx = todo_fix_glXCreateContextAttribsARB(_gfx_ctx.display, bestFbc, ctx, X_TRUE, (int*)&contextAttribs);
		if (_gfx_ctx.x11Error != null || win.src.ctx.native.ctx == null)
		{
			todo_fix_debugCallback(TODO_RENAME_TYPE_ERROR, TODO_RENAME_ERR_OPEN_GL_CONTEXT, "Failed to create an OpenGL context with AttribsARB, loading a generic OpenGL context.");
			win.src.ctx.native.ctx = todo_fix_glXCreateContext(_gfx_ctx.display, &visual, ctx, X_TRUE);
		}
	}

	win.src.ctx.native.window = todo_fix_glXCreateWindow(_gfx_ctx.display, bestFbc, win.src.window, null);
	todo_fix_glXMakeCurrent(_gfx_ctx.display, win.src.ctx.native.window, win.src.ctx.native.ctx);
	todo_fix_debugCallback(TODO_RENAME_TYPE_INFO, TODO_RENAME_INFO_OPEN_GL, "OpenGL context initalized.");
	win.opengl_swap_interval(0);
	return true;
}

// private
fn void todo_fix_window_flash_X11(GfxWindow* win, GfxFlashRequest request)
{
	if (win.has_focus() && request != CANCEL) return;
	XWMHints* wmhints = xGetWMHints(_gfx_ctx.display, win.src.window);
	if (wmhints == null) return;
	if (request)
	{
		wmhints.flags |= 1 << 8; // XUrgencyHint
		if (request == BRIEFLY) win.src.flashEnd = todo_fix_unix_getTimeNS() + (ulong)1e9;
		if (request == UNTIL_FOCUSED) win.src.flashEnd = (ulong)-1;
	}
	else
	{
		win.src.flashEnd = 0;
		wmhints.flags &= ~(1 << 8);
	}
	xSetWMHints(_gfx_ctx.display, win.src.window, wmhints);
	xFree(wmhints);
}

// private
fn void? todo_fix_createSurfacePtr_X11(void* data, int w, int h, GfxFormat format, GfxSurface* self, GfxWindow* window)
{
	if (!window) window = _gfx_ctx.root;
	self.data = data;
	self.size = { w, h };
	self.format = format;

	XWindowAttributes attrs;
	if (xGetWindowAttributes(_gfx_ctx.display, window.src.window, &attrs) == 0)
	{
		log::error("Failed to get window attributes.", category: log::CATEGORY_VIDEO);
		return SURFACE_CREATION_FAILED~;
	}
	defer catch self.release();
	self.native.bitmap = xCreateImage(_gfx_ctx.display, attrs.visual, (uint)attrs.depth, 2 /* ZPixmap */, 0, null, ...(uint[<2>])self.size, 32, 0);
	self.native.buffer = malloc((sz)w * h * 4);
	self.native.format = todo_fix_xImageGetFormat(self.native.bitmap);

	if (!self.native.bitmap)
	{
		log::error("Failed to create XImage.", category: log::CATEGORY_VIDEO);
		return SURFACE_CREATION_FAILED~;
	}
}

// private
fn bool _window_is_floating_x11(GfxWindow* win)
{
	Atom actualType;
	int actualFormat;
	CULong nitems, bytesAfter;
	Atom* propReturn = null;
	int status = xGetWindowProperty(_gfx_ctx.display, win.src.window, _gfx_ctx.netWmState, 0, ~0, 0, 4, &actualType, &actualFormat, &nitems, &bytesAfter, (char**)&propReturn);
	if (status != 0 || actualType != 4) return false;
	for (CULong i = 0; i < nitems; i++) if (propReturn[i] == _gfx_ctx.netWmStateAbove) return true;
	if (propReturn != null) xFree(propReturn);
	return false;
}

// private
fn void todo_fix_window_setName_X11(GfxWindow* win, String name) => @pool()
{
	ZString cstring = name.zstr_copy(tmem);
	xutf8SetWMProperties(_gfx_ctx.display, win.src.window, cstring, cstring, null, 0, null, null, null);
	xStoreName(_gfx_ctx.display, win.src.window, cstring);
	xChangeProperty(_gfx_ctx.display, win.src.window, _gfx_ctx.netWmName, _gfx_ctx.utf8String, 8, 0, cstring, name.len);
}

// private
fn bool _window_set_icon_x11(GfxWindow* win, char* dataSrc, int w, int h, GfxFormat format, GfxWindowIconType type)
{
	if (dataSrc == null)
	{
		return !!xChangeProperty(_gfx_ctx.display, win.src.window, _gfx_ctx.netWmIcon, 6, 32, 0, null, 0);
	}
	int count = 2 + (w * h);
	CULong* data = calloc((sz)count * CULong::size);
	data[0] = (CULong)w;
	data[1] = (CULong)h;

	copy_image_data_64((char*)&data[2], w, h, BGRA8, dataSrc, format, true, null);
	bool res = true;
	if (type & TASKBAR)
	{
		res = !!xChangeProperty(_gfx_ctx.display, win.src.window, _gfx_ctx.netWmIcon, 6, 32, 0, (char*)data, count);
	}

	copy_image_data_64((char*)&data[2], w, h, BGRA8, dataSrc, format, false, null);

	if (type & WINDOW)
	{
		XWMHints wmHints;
		wmHints.flags = 1 << 2; // IconPixmapHint

		int depth = xDefaultDepth(_gfx_ctx.display, xDefaultScreen(_gfx_ctx.display));
		void* image = xCreateImage(_gfx_ctx.display, xDefaultVisual(_gfx_ctx.display, xDefaultScreen(_gfx_ctx.display)),
		(uint)depth, 2 /* ZPixmap */, 0, (char*)&data[2], (uint)w, (uint)h, 32, 0);

		wmHints.icon_pixmap = xCreatePixmap(_gfx_ctx.display, win.src.window, (uint)w, (uint)h, (uint)depth);
		xPutImage(_gfx_ctx.display, wmHints.icon_pixmap, xDefaultGC(_gfx_ctx.display, xDefaultScreen(_gfx_ctx.display)), image, 0, 0, 0, 0, (uint)w, (uint)h);
		((XImageHeader*)image).data = null;
		xDestroyImage(image);

		xSetWMHints(_gfx_ctx.display, win.src.window, &wmHints);
	}

	free(data);
	xFlush(_gfx_ctx.display);
	return res;
}

// private
fn GfxMonitor* todo_fix_window_getMonitor_X11(GfxWindow* win)
{
	XWindowAttributes attrs;
	if (xGetWindowAttributes(_gfx_ctx.display, win.src.window, &attrs) == 0) return null;
	for (GfxMonitorNode* node = _gfx_ctx.monitors.list.head; node != null; node = node.next)
	{
		if ((attrs.x < node.mon.origin.x + node.mon.mode.size.x) && (attrs.x + attrs.width > node.mon.origin.x) &&
		(attrs.y < node.mon.origin.y + node.mon.mode.size.y) && (attrs.y + attrs.height > node.mon.origin.y))
		{
			return &node.mon;
		}
	}
	return _gfx_ctx.monitors.list.head != null ? &_gfx_ctx.monitors.list.head.mon : null;
}

// public
fn void todo_fix_window_showPlatform_X11(GfxWindow* win)
{
	win.internal.flags.hide = false;
	if (!_window_is_hidden_native(win)) return;
	xMapWindow(_gfx_ctx.display, win.src.window);
	win.move(win.origin);

	todo_fix_waitForShowEvent_X11(win);
	_window_set_fullscreen_native(win, win.is_fullscreen());
}

// public
fn bool todo_fix_waitForShowEvent_X11(GfxWindow* win)
{
	$if $feat(X11_WAYLAND):
		XEvent dummy;
		while (xCheckTypedWindowEvent(_gfx_ctx.display, win.src.window, 15 /* VisibilityNotify */, &dummy) == 0)
		{
			todo_fix_waitForEvent(100);
		}
		return true;
	$else
		XEvent dummy;
		while (true)
		{
			// XCheckTypedWindowEvent(display, window, VisibilityNotify, &dummy)
			if (xPending(_gfx_ctx.display) == 0) break;
			xNextEvent(_gfx_ctx.display, &dummy);
			if (dummy.type == 15 /* VisibilityNotify */ && dummy.xany.window == win.src.window) break;
		}
		return true;
	$endif
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
	xSendEvent(_gfx_ctx.display, xDefaultRootWindow(_gfx_ctx.display), 0, (1 << 20) | (1 << 19), &xev);
}

// public
fn void todo_fix_unix_parseURI(GfxWindow* win, ZString data)
{
	ZString prefix = "file://";
	ZString line;
	ZString savePtr;
	while ((line = (ZString)strtokR(data, "\r\n", (char**)&savePtr)) != null)
	{
		data = null;

		if (line[0] == '#') continue;

		ZString l;
		for (l = line; true; l++)
		{
			if ((char*)l - (char*)line > 7) { break; }
			else if (l[0] != prefix[(char*)l - (char*)line]) { break; }
			else if (l[0] == 0 && prefix[(char*)l - (char*)line] == 0)
			{
				line += 7;
				while (line[0] != '/') line++;
				break;
			}
			else if (l[0] == 0) { break; }
		}

		sz len = line.len();
		ZString path = (ZString)malloc(len + 1);

		sz index = 0;
		while (line[0] != 0)
		{
			if (line[0] == '%' && line[1] != 0 && line[2] != 0)
			{
				char[3] digits = {0, 0, 0};
				digits[0] = line[1];
				digits[1] = line[2];
				digits[2] = 0;
				path[index] = (char)strtol((char*)&digits, null, 16);
				line += 2;
			}
			else
			{
				if (index >= len) break;
				path[index] = line[0];
			}
			index++;
			line++;
		}

		path[len] = 0;
		todo_fix_dataDropCallback(win, path, len + 1, FILE);
		free(path);
	}
}


// public
fn void todo_fix_window_getVisual(XVisualInfo* visual, bool transparent)
{
	visual.visual = xDefaultVisual(_gfx_ctx.display, xDefaultScreen(_gfx_ctx.display));
	visual.depth = xDefaultDepth(_gfx_ctx.display, xDefaultScreen(_gfx_ctx.display));
	if (transparent)
	{
		xMatchVisualInfo(_gfx_ctx.display, xDefaultScreen(_gfx_ctx.display), 32, 4 /* TrueColor */, visual);
		if (visual.depth != 32) todo_fix_debugCallback(TODO_RENAME_TYPE_WARNING, TODO_RENAME_WARNING_OPEN_GL, "Failed to load a 32-bit depth.");
	}
}

// private
fn void todo_fix_window_setXAtom(GfxWindow* win, Atom netAtom, bool fullscreen)
{
	XEvent xev;
	xev.xclient.type = 33;
	xev.xclient.send_event = 1;
	xev.xclient.message_type = _gfx_ctx.netWmState;
	xev.xclient.window = win.src.window;
	xev.xclient.format = 32;
	xev.xclient.data.l[0] = (int)fullscreen;
	xev.xclient.data.l[1] = (CLong)netAtom;
	xSendEvent(_gfx_ctx.display, xDefaultRootWindow(_gfx_ctx.display), 0, (1 << 19) | (1 << 20), &xev);
}

// public
fn void todo_fix_x11IcCallback(void* ic, char* clientData, char* callData)
{
	GfxWindow* win = (GfxWindow*)(void*)clientData;
	win.src.ic = null;
}

// public
fn void todo_fix_x11ImCallback(void* im, char* clientData, char* callData)
{
	_gfx_ctx.im = null;
}

// public
fn void todo_fix_x11ImInitCallback(XDisplay* d, XPointer clientData, XPointer callData)
{
	if (_gfx_ctx.im != null) return;

	_gfx_ctx.im = xOpenIM(_gfx_ctx.display, null, null, null);
	if (_gfx_ctx.im == null) return;

	bool found = false;
	XImStyles* styles = null;

	if (xGetIMValues(_gfx_ctx.im, "queryInputStyle", &styles, (char*)null) != null)
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

	if (found == false)
	{
		xCloseIM(_gfx_ctx.im);
		_gfx_ctx.im = null;
		return;
	}

	XImCallback callback;
	callback.callback = &todo_fix_x11ImCallback;
	callback.client_data = null;
	xSetIMValues(_gfx_ctx.im, "destroyCallback", &callback, (char*)null);
}

// public
fn int todo_fix_xErrorHandler(XDisplay* display, XErrorEvent* ev)
{
	char[512] errorText;
	xGetErrorText(display, ev.error_code, (char*)&errorText, 512);
	todo_fix_debugCallback(TODO_RENAME_TYPE_ERROR, TODO_RENAME_ERR_X11, (ZString)&errorText);
	_gfx_ctx.x11Error = ev;
	return 0;
}

// public
fn void* todo_fix_xGetMode(XRRCrtcInfo* ci, XRRScreenResources* res, RRMode mode, GfxMonitorMode* found_mode)
{
	XRRModeInfoNative* mi = null;
	for (int j = 0; j < res.nmode; j++) if (res.modes[j].id == mode) mi = &res.modes[j];
	if (!mi) return null;

	found_mode.size = { (int)mi.width, (int)mi.height };
	if (ci.rotation == 4 || ci.rotation == 8) // RR_Rotate_90 / RR_Rotate_270
	{
		found_mode.size = found_mode.size.yx;
	}

	todo_fix_splitBPP((uint)xDefaultDepth(_gfx_ctx.display, xDefaultScreen(_gfx_ctx.display)), found_mode);
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
fn void todo_fix_xGetSystemContentDPI(float* dpi)
{
	if (dpi == null) return;
	float dpiOutput = 96.0f;
	ZString rms = xResourceManagerString(_gfx_ctx.display);
	if (rms == null) return;
	void* db = xrmGetStringDatabase(rms);
	if (db == null) return;
	ZString type = null;
	// xrmGetResource's value-out parameter is a small {len,addr} struct; omitted here.
	xrmDestroyDatabase(db);
	*dpi = dpiOutput;
}

// public
fn void todo_fix_xHandleClipboardSelection(XEvent* event)
{
	XSelectionRequestEvent* request = &event.xselectionrequest;
	Atom[2] formats = {_gfx_ctx.utf8String, 31 /* XA_STRING */};

	if (request.target == _gfx_ctx.targets)
	{
		Atom[4] targets = {_gfx_ctx.targets, _gfx_ctx.multiple, _gfx_ctx.utf8String, 31};
		xChangeProperty(_gfx_ctx.display, request.requestor, request.property, 4 /* XA_ATOM */, 32, 0, (char*)&targets, 4);
	}
	else if (request.target == _gfx_ctx.multiple)
	{
		Atom* targets = null;
		Atom actualType;
		int actualFormat;
		CULong count, bytesAfter;
		xGetWindowProperty(_gfx_ctx.display, request.requestor, request.property, 0, ~0, 0, _gfx_ctx.atomPair,
		&actualType, &actualFormat, &count, &bytesAfter, (char**)&targets);

		for (CULong i = 0; i < count; i += 2)
		{
			if (targets[i] == _gfx_ctx.utf8String || targets[i] == 31 /* XA_STRING */)
			{
				xChangeProperty(_gfx_ctx.display, request.requestor, targets[i + 1], targets[i],
				8, 0, _gfx_ctx.unixClipboard.data, (int)_gfx_ctx.unixClipboard.length);
			}
			else
			{
				targets[i + 1] = 0; // None
			}
		}

		xChangeProperty(_gfx_ctx.display, request.requestor, request.property, _gfx_ctx.atomPair, 32, 0, (char*)targets, (int)count);
		xFlush(_gfx_ctx.display);
		xFree(targets);
	}
	else if (request.target == _gfx_ctx.saveTargets)
	{
		xChangeProperty(_gfx_ctx.display, request.requestor, request.property, 0, 32, 0, null, 0);
	}
	else
	{
		for (sz i = 0; i < 2; i++)
		{
			if (request.target != formats[i]) continue;
			xChangeProperty(_gfx_ctx.display, request.requestor, request.property, request.target,
			8, 0, _gfx_ctx.unixClipboard.data, (int)_gfx_ctx.unixClipboard.length);
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
	xSendEvent(_gfx_ctx.display, request.requestor, 0, 0, &reply);
	xFlush(_gfx_ctx.display);
}

// private
fn int todo_fix_xHandleClipboardSelectionHelper()
{
	XEvent event;
	xPending(_gfx_ctx.display);
	if (qLength(_gfx_ctx.display) != 0 || xEventsQueued(_gfx_ctx.display, 0) + xEventsQueued(_gfx_ctx.display, 1) != 0)
	{
		xNextEvent(_gfx_ctx.display, &event);
	}
	else
	{
		return 0;
	}

	if (event.type == 30) { todo_fix_xHandleClipboardSelection(&event); return 0; } // SelectionRequest
	return 0;
}

// public
fn GfxFormat todo_fix_xImageGetFormat(void* imagePtr)
{
	// XImage*: bits_per_pixel/red_mask/green_mask/blue_mask live at a fixed
	// offset in the real struct; here we only ever set the format ourselves
	// right after xCreateImage, so this mirrors the original's fallback path.
	return BGRA8;
}

// public
fn void todo_fix_window_swapInterval_OpenGL_X11(GfxWindow* win, int swapInterval)
{
	switch
	{
		case todo_fix_glXSwapIntervalEXT != null:
			todo_fix_glXSwapIntervalEXT(_gfx_ctx.display, win.src.ctx.native.window, swapInterval);
		case todo_fix_glXSwapIntervalNative != null:
			todo_fix_glXSwapIntervalNative(swapInterval);
		default:
			log::warn("Failed to load swap interval function", category: log::CATEGORY_VIDEO);
	}
}


module std::gfx @feat(X11, X11_WAYLAND) @link("X11", "Xext", "Xrandr", "GL") @private;
import std::math, libc;

// Leading fields of the real XImage struct (ABI-compatible prefix; only used to null
// out `data` before XDestroyImage, since that buffer is owned by our own allocation).
struct XImageHeader
{
	int width;
	int height;
	int xoffset;
	int format;
	char* data;
}

struct XVisualInfo
{
	XVisual* visual;
	VisualID visualid;
	int screen;
	int depth;
	int c_class;
	CULong red_mask, green_mask, blue_mask;
	int colormap_size;
	int bits_per_rgb;
}
// X11 types

// XRandr
struct XRRScreenResources
{
	Time_t timestamp;
	Time_t configTimestamp;
	int ncrtc;
	RRCrtc* crtcs;
	int noutput;
	RROutput* outputs;
	int nmode;
	XRRModeInfoNative* modes;
}

struct XRRModeInfoNative
{
	RRMode id;
	uint width;
	uint height;
	CULong dotClock;
	uint hSyncStart;
	uint hSyncEnd;
	uint hTotal;
	uint hSkew;
	uint vSyncStart;
	uint vSyncEnd;
	uint vTotal;
	char* name;
	uint nameLength;
	CULong modeFlags;
}

struct XRRCrtcInfo
{
	Time_t timestamp;
	int x, y;
	uint width, height;
	RRMode mode;
	int rotation;
	int noutput;
	RROutput* outputs;
	int rotations;
	int npossible;
	RROutput* possible;
}
struct GfxXRROutputInfo
{
	Time_t timestamp;
	RRCrtc crtc;
	ZString name;
	int nameLen;
	CULong mm_width;
	CULong mm_height;
	int connection;
	int subpixel_order;
	int ncrtc;
	RRCrtc* crtcs;
	int nclone;
	RROutput* clones;
	int nmode;
	int npreferred;
	RRMode* modes;
}
struct XRRCrtcGamma
{
	int size;
	ushort* red;
	ushort* green;
	ushort* blue;
}

// ---- Xcursor / XInput2 / Xext sync+shape (dlopen'd like the original) ----
struct XcursorImage
{
	uint version;
	uint size;
	uint width, height;
	uint xhot, yhot;
	uint delay;
	uint* pixels;
}
struct XIEventMask
{
	int deviceid;
	int mask_len;
	char* mask;
}
struct XIValuatorState
{
	int mask_len;
	char* mask;
	double* values;
}
struct XIRawEvent
{
	int type;
	CULong serial;
	int send_event;
	XDisplay* display;
	int extension;
	int evtype;
	Time_t time;
	int deviceid;
	int sourceid;
	int detail;
	int flags;
	XIValuatorState valuators;
	double* raw_values;
}
struct XSyncValue
{
	int hi;
	uint lo;
}
alias XSyncCounter = XlibId;

struct XAspect
{
	int x, y;
}
struct X11WindowHints
{
	CULong flags, functions, decorations, status;
	CLong input_mode;
}

struct XSetWindowAttributes
{
	Pixmap background_pixmap;
	CULong background_pixel;
	Pixmap border_pixmap;
	CULong border_pixel;
	int bit_gravity;
	int win_gravity;
	int backing_store;
	CULong backing_planes;
	CULong backing_pixel;
	int save_under;
	CLong event_mask;
	CLong do_not_propagate_mask;
	int override_redirect;
	Colormap colormap;
	XCursor cursor;
}


struct XWindowAttributes
{
	int x, y;
	int width, height;
	int border_width;
	int depth;
	XVisual* visual;
	XWindow root;
	int c_class;
	int bit_gravity;
	int win_gravity;
	int backing_store;
	CULong backing_planes;
	CULong backing_pixel;
	int save_under;
	Colormap colormap;
	int map_installed;
	int map_state;
	CLong all_event_masks;
	CLong your_event_mask;
	CLong do_not_propagate_mask;
	int override_redirect;
	Screen* screen;
}

struct XkbStateRec
{
	char group;
	char locked_group;
	ushort base_group;
	ushort latched_group;
	char mods;
	char base_mods;
	char latched_mods;
	char locked_mods;
	char compat_state;
	char grab_mods;
	char compat_grab_mods;
	char lookup_mods;
	char compat_lookup_mods;
	ushort ptr_buttons;
}

struct XClassHint { ZString res_name; ZString res_class; }

struct XWMHints
{
	CLong flags;
	int input;
	int initial_state;
	Pixmap icon_pixmap;
	XWindow icon_window;
	int icon_x, icon_y;
	Pixmap icon_mask;
	XlibId window_group;
}

struct XSizeHints
{
	CLong flags;
	int x, y, width, height;
	int min_width, min_height;
	int max_width, max_height;
	int width_inc, height_inc;
	XAspect min_aspect;
	XAspect max_aspect;
	int base_width, base_height;
	int win_gravity;
}

struct XAnyEvent { int type; CULong serial; int send_event; XDisplay* display; XWindow window; }
struct XKeyEvent { int type; CULong serial; int send_event; XDisplay* display; XWindow window; XWindow root; XWindow subwindow; Time_t time; int x, y; int x_root, y_root; uint state; uint keycode; int same_screen; }
struct XButtonEvent { int type; CULong serial; int send_event; XDisplay* display; XWindow window; XWindow root; XWindow subwindow; Time_t time; int x, y; int x_root, y_root; uint state; uint button; int same_screen; }
struct XMotionEvent { int type; CULong serial; int send_event; XDisplay* display; XWindow window; XWindow root; XWindow subwindow; Time_t time; int x, y; int x_root, y_root; uint state; char is_hint; int same_screen; }
struct XCrossingEvent { int type; CULong serial; int send_event; XDisplay* display; XWindow window; XWindow root; XWindow subwindow; Time_t time; int x, y; int x_root, y_root; int mode; int detail; int same_screen; int focus; uint state; }
struct XFocusChangeEvent { int type; CULong serial; int send_event; XDisplay* display; XWindow window; int mode; int detail; }
struct XExposeEvent { int type; CULong serial; int send_event; XDisplay* display; XWindow window; int x, y, width, height; int count; }
struct XConfigureEvent { int type; CULong serial; int send_event; XDisplay* display; XWindow event; XWindow window; int x, y, width, height; int border_width; XWindow above; int override_redirect; }
struct XPropertyEvent { int type; CULong serial; int send_event; XDisplay* display; XWindow window; Atom atom; Time_t time; int state; }
struct XClientMessageDataL { CLong[5] l; }
struct XClientMessageEvent { int type; CULong serial; int send_event; XDisplay* display; XWindow window; Atom message_type; int format; XClientMessageDataL data; }
struct XSelectionRequestEvent { int type; CULong serial; int send_event; XDisplay* display; XWindow owner; XWindow requestor; Atom selection; Atom target; Atom property; Time_t time; }
struct XSelectionEvent { int type; CULong serial; int send_event; XDisplay* display; XWindow requestor; Atom selection; Atom target; Atom property; Time_t time; }
struct XReparentEvent { int type; CULong serial; int send_event; XDisplay* display; XWindow event; XWindow window; XWindow parent; int x, y; int override_redirect; }
struct XMapEvent { int type; CULong serial; int send_event; XDisplay* display; XWindow event; XWindow window; int override_redirect; }
struct XUnmapEvent { int type; CULong serial; int send_event; XDisplay* display; XWindow event; XWindow window; int from_configure; }
struct XErrorEvent { int type; XDisplay* display; CULong resourceid; CULong serial; char error_code; char request_code; char minor_code; }
struct XGenericEventCookie { int type; CULong serial; int send_event; XDisplay* display; int extension; int evtype; uint cookie_data_len; void* data; }

union XEvent
{
	int type;
	XAnyEvent xany;
	XKeyEvent xkey;
	XButtonEvent xbutton;
	XMotionEvent xmotion;
	XCrossingEvent xcrossing;
	XFocusChangeEvent xfocus;
	XExposeEvent xexpose;
	XConfigureEvent xconfigure;
	XPropertyEvent xproperty;
	XClientMessageEvent xclient;
	XSelectionRequestEvent xselectionrequest;
	XSelectionEvent xselection;
	XReparentEvent xreparent;
	XMapEvent xmap;
	XUnmapEvent xunmap;
	XErrorEvent xerror;
	XGenericEventCookie xcookie;
	CLong[24] pad;
}

typedef XDisplay = void;
typedef XVisual = void;
alias XWindow = CULong;
alias XDrawable = CULong;
alias XPointer = char*;
alias RROutput = CULong;
alias XCursor = CULong;
alias XlibIC = void*;
alias XlibIM = void*;
typedef Screen = void;
alias Colormap = CULong;
alias Pixmap = CULong;
alias GContext = void*;
alias Atom = CULong;
alias VisualID = CULong;
alias KeySym = CULong;
alias KeyCode = char;
alias XlibId = CULong;
alias Region = void*;
alias XIMStyle = CULong;
alias RRMode = CULong;
alias RRCrtc = CULong;
alias GLXContext = void*;
alias GLXFBConfig = void*;
alias GLXDrawable = CULong;
alias GLXWindow = CULong;

// X11 used functions
extern fn XDisplay* xOpenDisplay(ZString name) @cname("XOpenDisplay");
extern fn int xCloseDisplay(XDisplay* d) @cname("XCloseDisplay");
extern fn int xInitThreads() @cname("XInitThreads");
extern fn XWindow xDefaultRootWindow(XDisplay* d) @cname("XDefaultRootWindow");
extern fn XWindow xRootWindow(XDisplay* d, int screen) @cname("XRootWindow");
extern fn int xDefaultScreen(XDisplay* d) @cname("XDefaultScreen");
extern fn XVisual* xDefaultVisual(XDisplay* d, int screen) @cname("XDefaultVisual");
extern fn int xDefaultDepth(XDisplay* d, int screen) @cname("XDefaultDepth");
extern fn GContext xDefaultGC(XDisplay* d, int screen) @cname("XDefaultGC");
extern fn Colormap xCreateColormap(XDisplay* d, XWindow win, XVisual* visual, int alloc) @cname("XCreateColormap");
extern fn int xFreeColors(XDisplay* d, Colormap cm, CULong* pixels, int n, CULong planes) @cname("XFreeColors");
extern fn XWindow xCreateWindow(XDisplay* d, XWindow parent, int x, int y, uint w, uint h, uint borderWidth, int depth, uint c_class, XVisual* visual, CULong valuemask, XSetWindowAttributes* attrs) @cname("XCreateWindow");
extern fn int xDestroyWindow(XDisplay* d, XWindow w) @cname("XDestroyWindow");
extern fn int xMapWindow(XDisplay* d, XWindow w) @cname("XMapWindow");
extern fn int xMapRaised(XDisplay* d, XWindow w) @cname("XMapRaised");
extern fn int xUnmapWindow(XDisplay* d, XWindow w) @cname("XUnmapWindow");
extern fn int xMoveWindow(XDisplay* d, XWindow w, int x, int y) @cname("XMoveWindow");
extern fn int xResizeWindow(XDisplay* d, XWindow w, uint width, uint height) @cname("XResizeWindow");
extern fn int xRaiseWindow(XDisplay* d, XWindow w) @cname("XRaiseWindow");
extern fn GContext xCreateGC(XDisplay* d, XDrawable draw, CULong valuemask, void* values) @cname("XCreateGC");
extern fn int xFreeGC(XDisplay* d, GContext gc) @cname("XFreeGC");
extern fn int xSaveContext(XDisplay* d, XlibId xid, int context, XPointer data) @cname("XSaveContext");
extern fn int xFindContext(XDisplay* d, XlibId xid, int context, XPointer* dataOut) @cname("XFindContext");
extern fn int xDeleteContext(XDisplay* d, XlibId xid, int context) @cname("XDeleteContext");
extern fn int xUniqueContext() @cname("XrmUniqueQuark");
extern fn XlibIC xCreateIC(XlibIM im, ...) @cname("XCreateIC");
extern fn int xDestroyIC(XlibIC ic) @cname("XDestroyIC");
extern fn int xSetICFocus(XlibIC ic) @cname("XSetICFocus");
extern fn int xUnsetICFocus(XlibIC ic) @cname("XUnsetICFocus");
extern fn XlibIM xOpenIM(XDisplay* d, void* db, ZString res_name, ZString res_class) @cname("XOpenIM");
extern fn int xCloseIM(XlibIM im) @cname("XCloseIM");
extern fn ZString xGetIMValues(XlibIM im, ...) @cname("XGetIMValues");
extern fn ZString xSetIMValues(XlibIM im, ...) @cname("XSetIMValues");
extern fn int xRegisterIMInstantiateCallback(XDisplay* d, void* db, ZString res_name, ZString res_class, void* callback, XPointer clientData) @cname("XRegisterIMInstantiateCallback");
extern fn int xUnregisterIMInstantiateCallback(XDisplay* d, void* db, ZString res_name, ZString res_class, void* callback, XPointer clientData) @cname("XUnregisterIMInstantiateCallback");
extern fn ZString xSetLocaleModifiers(ZString modifierList) @cname("XSetLocaleModifiers");
extern fn sz xutf8LookupString(XlibIC ic, XKeyEvent* event, char* buffer, int bytes, KeySym* keysym, int* status) @cname("Xutf8LookupString");
extern fn int xutf8SetWMProperties(XDisplay* d, XWindow w, ZString windowName, ZString iconName, ZString* argv, int argc, XSizeHints* hints, XWMHints* wmHints, void* classHints) @cname("Xutf8SetWMProperties");
extern fn int xSetClassHint(XDisplay* d, XWindow w, XClassHint* hint) @cname("XSetClassHint");
extern fn int xSetWMHints(XDisplay* d, XWindow w, XWMHints* hints) @cname("XSetWMHints");
extern fn XWMHints* xGetWMHints(XDisplay* d, XWindow w) @cname("XGetWMHints");
extern fn int xSetWMProtocols(XDisplay* d, XWindow w, Atom* protocols, int count) @cname("XSetWMProtocols");
extern fn int xSetWMSizeHints(XDisplay* d, XDrawable w, XSizeHints* hints, Atom property) @cname("XSetWMSizeHints");
extern fn int xGetWMNormalHints(XDisplay* d, XWindow w, XSizeHints* hints, CLong* flags) @cname("XGetWMNormalHints");
extern fn int xSetWMNormalHints(XDisplay* d, XWindow w, XSizeHints* hints) @cname("XSetWMNormalHints");
extern fn int xSelectInput(XDisplay* d, XDrawable w, CLong eventMask) @cname("XSelectInput");
extern fn int xStoreName(XDisplay* d, XWindow w, ZString name) @cname("XStoreName");
extern fn int xSetWindowBackground(XDisplay* d, XWindow w, CULong background) @cname("XSetWindowBackground");
extern fn int xSetWindowBackgroundPixmap(XDisplay* d, XWindow w, Pixmap pixmap) @cname("XSetWindowBackgroundPixmap");
extern fn int xClearWindow(XDisplay* d, XWindow w) @cname("XClearWindow");
extern fn int xFlush(XDisplay* d) @cname("XFlush");
extern fn int xSync(XDisplay* d, int discard) @cname("XSync");
extern fn int xPending(XDisplay* d) @cname("XPending");
extern fn int xConnectionNumber(XDisplay* d) @cname("XConnectionNumber");
extern fn int qLength(XDisplay* d) @cname("XQLength");
extern fn int xEventsQueued(XDisplay* d, int mode) @cname("XEventsQueued");
extern fn int xNextEvent(XDisplay* d, XEvent* ev) @cname("XNextEvent");
extern fn int xPeekEvent(XDisplay* d, XEvent* ev) @cname("XPeekEvent");
extern fn int xCheckTypedWindowEvent(XDisplay* d, XWindow w, int eventType, XEvent* ev) @cname("XCheckTypedWindowEvent");
extern fn int xFilterEvent(XEvent* ev, XWindow w) @cname("XFilterEvent");
extern fn int xSendEvent(XDisplay* d, XWindow w, int propagate, CLong eventMask, XEvent* ev) @cname("XSendEvent");
extern fn int xGetEventData(XDisplay* d, XGenericEventCookie* cookie) @cname("XGetEventData");
extern fn int xFreeEventData(XDisplay* d, XGenericEventCookie* cookie) @cname("XFreeEventData");
extern fn int xGetWindowAttributes(XDisplay* d, XWindow w, XWindowAttributes* attrs) @cname("XGetWindowAttributes");
extern fn int xChangeProperty(XDisplay* d, XWindow w, Atom property, Atom type, int format, int mode, char* data, int nelements) @cname("XChangeProperty");
extern fn int xGetWindowProperty(XDisplay* d, XWindow w, Atom property, CLong offset, CLong length, int delet, Atom reqType, Atom* actualType, int* actualFormat, CULong* nitems, CULong* bytesAfter, char** propReturn) @cname("XGetWindowProperty");
extern fn int xDeleteProperty(XDisplay* d, XWindow w, Atom property) @cname("XDeleteProperty");
extern fn Atom xInternAtom(XDisplay* d, ZString name, int onlyIfExists) @cname("XInternAtom");
extern fn int xConvertSelection(XDisplay* d, Atom selection, Atom target, Atom property, XWindow requestor, Time_t time) @cname("XConvertSelection");
extern fn XWindow xGetSelectionOwner(XDisplay* d, Atom selection) @cname("XGetSelectionOwner");
extern fn int xSetSelectionOwner(XDisplay* d, Atom selection, XWindow owner, Time_t time) @cname("XSetSelectionOwner");
extern fn int xFree(void* data) @cname("XFree");
extern fn int xSetErrorHandler(void* handler) @cname("XSetErrorHandler");
extern fn int xGetErrorText(XDisplay* d, int code, char* buffer, int len) @cname("XGetErrorText");
extern fn int xQueryPointer(XDisplay* d, XWindow w, XWindow* rootRet, XWindow* childRet, int* rootX, int* rootY, int* winX, int* winY, uint* mask) @cname("XQueryPointer");
extern fn int xWarpPointer(XDisplay* d, XWindow src, XWindow dest, int srcX, int srcY, uint srcW, uint srcH, int destX, int destY) @cname("XWarpPointer");
extern fn int xTranslateCoordinates(XDisplay* d, XWindow src, XWindow dest, int srcX, int srcY, int* destXRet, int* destYRet, XWindow* childRet) @cname("XTranslateCoordinates");
extern fn int xSetInputFocus(XDisplay* d, XWindow w, int revert, Time_t time) @cname("XSetInputFocus");
extern fn int xGrabPointer(XDisplay* d, XWindow w, int ownerEvents, uint eventMask, int pointerMode, int keyboardMode, XWindow confineTo, XCursor cursor, Time_t time) @cname("XGrabPointer");
extern fn int xUngrabPointer(XDisplay* d, Time_t time) @cname("XUngrabPointer");
extern fn int xIconifyWindow(XDisplay* d, XWindow w, int screen) @cname("XIconifyWindow");
extern fn int xDefineCursor(XDisplay* d, XWindow w, XCursor cursor) @cname("XDefineCursor");
extern fn int xFreeCursor(XDisplay* d, XCursor cursor) @cname("XFreeCursor");
extern fn XCursor xCreateFontCursor(XDisplay* d, uint shape) @cname("XCreateFontCursor");
extern fn KeySym xkbKeycodeToKeysym(XDisplay* d, KeyCode kc, int group, int level) @cname("XkbKeycodeToKeysym");
extern fn int xkbGetState(XDisplay* d, uint deviceSpec, void* stateRet) @cname("XkbGetState");
extern fn void* xkbGetMap(XDisplay* d, uint which, uint deviceSpec) @cname("XkbGetMap");
extern fn int xkbGetNames(XDisplay* d, uint which, void* xkb) @cname("XkbGetNames");
extern fn void* xkbGetKeyboardByName(XDisplay* d, uint deviceSpec, void* names, uint want, uint need, int loadKeycodes) @cname("XkbGetKeyboardByName");
extern fn int xkbFreeKeyboard(void* xkb, uint which, int freeAll) @cname("XkbFreeKeyboard");
extern fn int xMatchVisualInfo(XDisplay* d, int screen, int depth, int c_class, XVisualInfo* visualOut) @cname("XMatchVisualInfo");
extern fn int xChangeWindowAttributes(XDisplay* d, XWindow w, CULong valuemask, XSetWindowAttributes* attrs) @cname("XChangeWindowAttributes");
extern fn Region xCreateRegion() @cname("XCreateRegion");
extern fn int xDestroyRegion(Region r) @cname("XDestroyRegion");
extern fn XCursor xCreatePixmap(XDisplay* d, XDrawable draw, uint w, uint h, uint depth) @cname("XCreatePixmap");
extern fn void* xCreateImage(XDisplay* d, XVisual* visual, uint depth, int format, int offset, char* data, uint w, uint h, int bitmapPad, int bytesPerLine) @cname("XCreateImage");
extern fn int xPutImage(XDisplay* d, XDrawable draw, GContext gc, void* image, int srcX, int srcY, int destX, int destY, uint w, uint h) @cname("XPutImage");
extern fn int xDestroyImage(void* image) @cname("XDestroyImage");
extern fn ZString xResourceManagerString(XDisplay* d) @cname("XResourceManagerString");
extern fn void* xrmGetStringDatabase(ZString data) @cname("XrmGetStringDatabase");
extern fn int xrmGetResource(void* db, ZString name, ZString c_class, ZString* typeOut, void* valueOut) @cname("XrmGetResource");
extern fn int xrmDestroyDatabase(void* db) @cname("XrmDestroyDatabase");
extern fn XRRScreenResources* xRRGetScreenResourcesCurrent(XDisplay* d, XWindow w) @cname("XRRGetScreenResourcesCurrent");
extern fn int xRRFreeScreenResources(XRRScreenResources* res) @cname("XRRFreeScreenResources");
extern fn RROutput xRRGetOutputPrimary(XDisplay* d, XWindow w) @cname("XRRGetOutputPrimary");
extern fn GfxXRROutputInfo* xRRGetOutputInfo(XDisplay* d, XRRScreenResources* res, RROutput output) @cname("XRRGetOutputInfo");
extern fn int xRRFreeOutputInfo(GfxXRROutputInfo* info) @cname("XRRFreeOutputInfo");
extern fn XRRCrtcInfo* xRRGetCrtcInfo(XDisplay* d, XRRScreenResources* res, RRCrtc crtc) @cname("XRRGetCrtcInfo");
extern fn int xRRFreeCrtcInfo(XRRCrtcInfo* info) @cname("XRRFreeCrtcInfo");
extern fn int xRRSetCrtcConfig(XDisplay* d, XRRScreenResources* res, RRCrtc crtc, Time_t time, int x, int y, RRMode mode, int rotation, RROutput* outputs, int noutputs) @cname("XRRSetCrtcConfig");
extern fn int xRRGetCrtcGammaSize(XDisplay* d, RRCrtc crtc) @cname("XRRGetCrtcGammaSize");
extern fn XRRCrtcGamma* xRRGetCrtcGamma(XDisplay* d, RRCrtc crtc) @cname("XRRGetCrtcGamma");
extern fn XRRCrtcGamma* xRRAllocGamma(int size) @cname("XRRAllocGamma");
extern fn int xRRSetCrtcGamma(XDisplay* d, RRCrtc crtc, XRRCrtcGamma* gamma) @cname("XRRSetCrtcGamma");
extern fn int xRRFreeGamma(XRRCrtcGamma* gamma) @cname("XRRFreeGamma");
extern fn int xRRQueryExtension(XDisplay* d, int* eventBase, int* errorBase) @cname("XRRQueryExtension");
extern fn int xRRSelectInput(XDisplay* d, XWindow w, int mask) @cname("XRRSelectInput");

alias PFN_XcursorImageCreate = fn XcursorImage*(int, int);
alias PFN_XcursorImageDestroy = fn void(XcursorImage*);
alias PFN_XcursorImageLoadCursor = fn XCursor(XDisplay*, XcursorImage*);
alias PFN_XISelectEvents = fn int(XDisplay*, XWindow, XIEventMask*, int);
alias PFN_XSyncIntToValue = fn void(XSyncValue*, int);
alias PFN_XSyncSetCounter = fn int(XDisplay*, XSyncCounter, XSyncValue);
alias PFN_XSyncCreateCounter = fn XSyncCounter(XDisplay*, XSyncValue);
alias PFN_XShapeCombineMask = fn void(XDisplay*, XWindow, int, int, int, Pixmap, int);
alias PFN_XShapeCombineRegion = fn void(XDisplay*, XWindow, int, int, int, Region, int);

PFN_XcursorImageCreate _xcursor_image_create_src;
PFN_XcursorImageDestroy _xcursor_image_destroy_src;
PFN_XcursorImageLoadCursor _xcursor_image_load_cursor_src;
PFN_XISelectEvents _xi_select_events_src;
PFN_XSyncIntToValue _xsync_int_to_value_src;
PFN_XSyncSetCounter _xsync_set_counter_src;
PFN_XSyncCreateCounter _xsync_create_counter_src;
PFN_XShapeCombineMask _xshape_combine_mask_src;
PFN_XShapeCombineRegion _xshape_combine_region_src;

void* _x11_cursor_handle;
void* _x11_xi_handle;
void* _x11_ext_handle;


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
	VkCreateSurfaceKHRProc proc = (VkCreateSurfaceKHRProc)todo_fix_getInstanceProcAddress_Vulkan(instance, "vkCreateXlibSurfaceKHR");
	if (!proc) return VULKAN_NOT_LOADED~;
	VkXlibSurfaceCreateInfoKHR create_info = {
		.sType = VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR,
		.dpy = _gfx_ctx.display,
		.window = win.src.window
	};
	if (proc(instance, &create_info, null, &surface)) return SURFACE_CREATION_FAILED~;
	return surface;
}

extern fn VisualID xVisualIDFromVisual(XVisual* visual) @cname("XVisualIDFromVisual");
alias VkGetPhysicalDeviceXlibPresentationSupportKHRProc = fn uint(VkPhysicalDevice, uint, XDisplay*, VisualID);

// public
fn bool _vulkan_get_physical_device_presentation_support_x11(VkInstance instance, VkPhysicalDevice physical_device, uint queue_family_index)
{
	VkGetPhysicalDeviceXlibPresentationSupportKHRProc proc = (VkGetPhysicalDeviceXlibPresentationSupportKHRProc)todo_fix_getInstanceProcAddress_Vulkan(instance, "vkGetPhysicalDeviceXlibPresentationSupportKHR");
	if (!proc) return false;
	XVisual* visual = xDefaultVisual(_gfx_ctx.display, xDefaultScreen(_gfx_ctx.display));
	uint out = proc(physical_device, queue_family_index, _gfx_ctx.display, xVisualIDFromVisual(visual));
	return out != 0;
}
// ---

