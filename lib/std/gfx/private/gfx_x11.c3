module std::gfx::x11 @feat(LINUX, BSD) @feat(!NO_X11) @private;
import std::math, std::collections, std::os, libc, std::time, std::time::clock, std::net, std::net::os;
import std::gfx::egl, std::gfx @public;
import std::gfx::gl @public;

struct GfxX11Ctx
{
	XDisplay* display;
	int context;
	XWindow helper_window;
	String class_name;
	XlibIM im;
	int xrandr_event_base;
	XWindow x11_source;
	int x11_version;
	int x11_format;
	GfxDataTransferType x11_transfer_type;
	XErrorEvent* x11_error;
	Atom net_current_desktop, net_wm_state_maximized_vert, net_wm_state_maximized_horz, net_wm_bypass_compositor;
	Atom net_wm_state, motif_wm_hints, wm_delete_window, net_wm_sync_request_counter, net_wm_sync_request;
	Atom atom_pair, multiple, targets, save_targets, utf8_string;
	Atom xdnd_type_list, xdnd_selection, xdnd_aware, xdnd_enter, xdnd_position, xdnd_status, xdnd_leave;
	Atom xdnd_drop, xdnd_finished, xdnd_action_copy;
	Atom wm_protocols, wm_state, net_wm_state_fullscreen, net_wm_state_above, net_wm_window_opacity;
	Atom net_wm_name, net_wm_icon, xsel_data, clipboard_atom, clipboard_manager;
	Atom xtext_uri_list, xtext_plain, net_workarea;
	bool xsync_available;
	bool resize_sync_enabled;
}

GfxX11Ctx ctx;

// --- Gfx

fn void? init_platform(String class_name, GfxInitOption flags)
{
	ctx = { .class_name = class_name };

	xInitThreads();
	ctx.display = xOpenDisplay(null);
	if (!ctx.display) return gfx::X11_DISPLAY_NOT_LOADED~;

	ctx.net_current_desktop = xInternAtom(ctx.display, "_NET_CURRENT_DESKTOP", 0);
	ctx.net_wm_state_maximized_vert = xInternAtom(ctx.display, "_NET_WM_STATE_MAXIMIZED_VERT", 0);
	ctx.net_wm_state_maximized_horz = xInternAtom(ctx.display, "_NET_WM_STATE_MAXIMIZED_HORZ", 0);
	ctx.net_wm_bypass_compositor = xInternAtom(ctx.display, "_NET_WM_BYPASS_COMPOSITOR", 0);
	ctx.net_wm_state = xInternAtom(ctx.display, "_NET_WM_STATE", 0);
	ctx.motif_wm_hints = xInternAtom(ctx.display, "_MOTIF_WM_HINTS", 0);
	ctx.wm_delete_window = xInternAtom(ctx.display, "WM_DELETE_WINDOW", 0);
	ctx.net_wm_sync_request_counter = xInternAtom(ctx.display, "_NET_WM_SYNC_REQUEST_COUNTER", 0);
	ctx.net_wm_sync_request = xInternAtom(ctx.display, "_NET_WM_SYNC_REQUEST", 0);
	ctx.atom_pair = xInternAtom(ctx.display, "ATOM_PAIR", 0);
	ctx.multiple = xInternAtom(ctx.display, "MULTIPLE", 0);
	ctx.targets = xInternAtom(ctx.display, "TARGETS", 0);
	ctx.save_targets = xInternAtom(ctx.display, "SAVE_TARGETS", 0);
	ctx.utf8_string = xInternAtom(ctx.display, "UTF8_STRING", 0);
	ctx.xdnd_type_list = xInternAtom(ctx.display, "XdndTypeList", 0);
	ctx.xdnd_selection = xInternAtom(ctx.display, "XdndSelection", 0);
	ctx.xdnd_aware = xInternAtom(ctx.display, "XdndAware", 0);
	ctx.xdnd_enter = xInternAtom(ctx.display, "XdndEnter", 0);
	ctx.xdnd_position = xInternAtom(ctx.display, "XdndPosition", 0);
	ctx.xdnd_status = xInternAtom(ctx.display, "XdndStatus", 0);
	ctx.xdnd_leave = xInternAtom(ctx.display, "XdndLeave", 0);
	ctx.xdnd_drop = xInternAtom(ctx.display, "XdndDrop", 0);
	ctx.xdnd_finished = xInternAtom(ctx.display, "XdndFinished", 0);
	ctx.xdnd_action_copy = xInternAtom(ctx.display, "XdndActionCopy", 0);
	ctx.wm_protocols = xInternAtom(ctx.display, "WM_PROTOCOLS", 0);
	ctx.wm_state = xInternAtom(ctx.display, "WM_STATE", 0);
	ctx.net_wm_state_fullscreen = xInternAtom(ctx.display, "_NET_WM_STATE_FULLSCREEN", 0);
	ctx.net_wm_state_above = xInternAtom(ctx.display, "_NET_WM_STATE_ABOVE", 0);
	ctx.net_wm_window_opacity = xInternAtom(ctx.display, "_NET_WM_WINDOW_OPACITY", 0);
	ctx.net_wm_name = xInternAtom(ctx.display, "_NET_WM_NAME", 0);
	ctx.net_wm_icon = xInternAtom(ctx.display, "_NET_WM_ICON", 0);
	ctx.xsel_data = xInternAtom(ctx.display, "XSEL_DATA", 0);
	ctx.clipboard_atom = xInternAtom(ctx.display, "CLIPBOARD", 0);
	ctx.clipboard_manager = xInternAtom(ctx.display, "CLIPBOARD_MANAGER", 0);
	ctx.net_workarea = xInternAtom(ctx.display, "_NET_WORKAREA", 0);
	ctx.xtext_uri_list = xInternAtom(ctx.display, "text/uri-list", 0);
	ctx.xtext_plain = xInternAtom(ctx.display, "text/plain", 0);

	ctx.context = xUniqueContext();
	if (!ctx.context) return gfx::X11_CTX_CREATION_FAILED~;

	XSetWindowAttributes wa;
	wa.event_mask = 1 << 22; // PropertyChangeMask
	ctx.helper_window = xCreateWindow(ctx.display, xDefaultRootWindow(ctx.display), 0, 0, 1, 1, 0, 0,
		2 /* InputOnly */, xDefaultVisual(ctx.display, xDefaultScreen(ctx.display)), 1 << 11, &wa);
	if (!ctx.helper_window) return gfx::X11_WINDOW_CREATION_FAILED~;

	dload_cursor_xi_ext();

	char[4] blk;
	gfx::_gfx.hiddenMouse = gfx::_create_mouse((void*)&blk, { 1, 1 }, RGBA8) ?? null;
	gfx::_gfx.unixClipboard = null;

	xSetErrorHandler(&_error_handler);
	xSync(ctx.display, 0);

	xSetLocaleModifiers("");
	xRegisterIMInstantiateCallback(ctx.display, null, null, null, (void*)&_x11_im_init_callback, null);

	int errorBase;
	if (xRRQueryExtension(ctx.display, &ctx.xrandr_event_base, &errorBase) != 0)
	{
		xRRSelectInput(ctx.display, xRootWindow(ctx.display, xDefaultScreen(ctx.display)), 1 << 4); // RROutputChangeNotifyMask
	}

	ctx.resize_sync_enabled = flags.enable_x11_resize_sync;
	if (ctx.resize_sync_enabled) _init_xsync();
}

fn void destroy_platform()
{
	if (ctx.display)
	{
		if (xGetSelectionOwner(ctx.display, ctx.clipboard_atom) == ctx.helper_window)
		{
			xConvertSelection(ctx.display, ctx.clipboard_manager, ctx.save_targets, 0, ctx.helper_window, 0);
			while (_handle_clipboard_selection_helper() != 0) {}
		}
		xUnregisterIMInstantiateCallback(ctx.display, null, null, null, (void*)&_x11_im_init_callback, null);
	}
	if (ctx.im) xCloseIM(ctx.im);
	if (gfx::_gfx.unixClipboard) free(gfx::_gfx.unixClipboard);
	if (gfx::_gfx.hiddenMouse) free_mouse(gfx::_gfx.hiddenMouse);
	if (ctx.helper_window) xDestroyWindow(ctx.display, ctx.helper_window);
	if (ctx.display) xCloseDisplay(ctx.display);

	dlclose_cursor_xi_ext();
	ctx = {};
}

fn void poll_events_platform()
{
	gfx::_reset_prev_state();
	xPending(ctx.display);
	while (xQLength(ctx.display)) _handle_event();
}

fn void wait_for_event(Duration duration)
{
	if (gfx::_gfx.eventWait_forceStop[0] == 0 || gfx::_gfx.eventWait_forceStop[1] == 0)
	{
		if (posix::pipe((int[2]*)&gfx::_gfx.eventWait_forceStop) != -1)
		{
			os::fcntl((NativeSocket)gfx::_gfx.eventWait_forceStop[0], os::F_GETFL, 0);
			os::fcntl((NativeSocket)gfx::_gfx.eventWait_forceStop[0], os::F_GETFD, 0);
			os::fcntl((NativeSocket)gfx::_gfx.eventWait_forceStop[1], os::F_GETFL, 0);
			os::fcntl((NativeSocket)gfx::_gfx.eventWait_forceStop[1], os::F_GETFD, 0);
		}
	}

	Posix_pollfd[2] fds;
	fds[0].fd = 0;
	fds[0].events = (short)os::POLLIN;
	fds[1].fd = gfx::_gfx.eventWait_forceStop[0];
	fds[1].events = (short)os::POLLIN;

	defer
	{
		// drain any data in the stop request
		if (gfx::_gfx.eventWait_forceStop[2])
		{
			char[64] data;
			libc::read(gfx::_gfx.eventWait_forceStop[0], &data, 64);

			gfx::_gfx.eventWait_forceStop[2] = 0;
		}
	}

	fds[0].fd = xConnectionNumber(ctx.display);

	bool infinity = duration == gfx::WAIT_FOREVER;
	bool once = true;
	Clock start = clock::now();
	while (!xPending(ctx.display))
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


// --- GfxMonitor functions

fn GfxGammaRamp? monitor_get_gamma_ramp(GfxMonitor* monitor, Allocator a)
{
	sz size = (sz)xRRGetCrtcGammaSize(ctx.display, monitor.node.crtc);
	if (!size) return gfx::CANNOT_RETRIEVE_GAMMA~;
	XRRCrtcGamma* gamma = xRRGetCrtcGamma(ctx.display, monitor.node.crtc);
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

fn GfxMonitorMode[] monitor_get_modes(GfxMonitor* monitor)
{
	XRRScreenResources* res = xRRGetScreenResourcesCurrent(ctx.display, xDefaultRootWindow(ctx.display));
	if (!res) return {};
	XRRCrtcInfo* ci = xRRGetCrtcInfo(ctx.display, res, monitor.node.crtc);
	GfxXRROutputInfo* oi = xRRGetOutputInfo(ctx.display, res, monitor.node.rr_output);
	sz count = (sz)oi.nmode;
	List{GfxMonitorMode} modes;
	for (int i = 0; i < oi.nmode; i++)
	{
		GfxMonitorMode mode;
		_get_mode(ci, res, oi.modes[i], &mode);
		modes.push(mode);
	}
	xRRFreeOutputInfo(oi);
	xRRFreeCrtcInfo(ci);
	xRRFreeScreenResources(res);
	return modes.array_view();
}

fn IntRect monitor_get_workarea(GfxMonitor* monitor)
{
	XWindow root = xDefaultRootWindow(ctx.display);
	int areaX = monitor.origin.x;
	int areaY = monitor.origin.y;
	int areaW = monitor.mode.size.x;
	int areaH = monitor.mode.size.y;

	if (ctx.net_workarea != 0 && ctx.net_current_desktop != 0)
	{
		Atom* extents = null;
		Atom* desktop = null;
		Atom actualType; int actualFormat; CULong extentCount, bytesAfter, count;
		xGetWindowProperty(ctx.display, root, ctx.net_workarea, 0, ~0, 0, 6, &actualType, &actualFormat, &extentCount, &bytesAfter, (char**)&extents);
		xGetWindowProperty(ctx.display, root, ctx.net_current_desktop, 0, ~0, 0, 6, &actualType, &actualFormat, &count, &bytesAfter, (char**)&desktop);

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

fn void monitor_node_free(GfxMonitorNode* node) {}

fn void? monitor_request_mode(GfxMonitor* mon, GfxMonitorMode* mode, GfxMonitorModeRequest request)
{
	bool output = false;
	XRRScreenResources* res = xRRGetScreenResourcesCurrent(ctx.display, xDefaultRootWindow(ctx.display));
	if (!res) return gfx::MONITOR_MODE_CHANGE_FAILED~;
	XRRCrtcInfo* ci = xRRGetCrtcInfo(ctx.display, res, mon.node.crtc);
	GfxXRROutputInfo* oi = xRRGetOutputInfo(ctx.display, res, mon.node.rr_output);
	RRMode native = 0;

	for (int i = 0; i < oi.nmode; i++)
	{
		GfxMonitorMode found_mode;
		void* mi = _get_mode(ci, res, oi.modes[i], &found_mode);
		if (mi == null) continue;
		if (mode.equals_request(&found_mode, request))
		{
			native = ((XRRModeInfoNative*)mi).id;
			output = true;
			mon.mode = found_mode;
			break;
		}
	}
	if (native != 0) xRRSetCrtcConfig(ctx.display, res, mon.node.crtc, 0, ci.x, ci.y, native, ci.rotation, ci.outputs, ci.noutput);
	xRRFreeOutputInfo(oi);
	xRRFreeCrtcInfo(ci);
	xRRFreeScreenResources(res);
	if (!output) return gfx::MONITOR_MODE_CHANGE_FAILED~;
}

fn void? monitor_set_gamma(GfxMonitor* monitor, float gamma)
{
	sz size = (sz)xRRGetCrtcGammaSize(ctx.display, monitor.node.crtc);
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
	xRRSetCrtcGamma(ctx.display, monitor.node.crtc, gamma_data);
}

fn void? monitor_set_gamma_ramp(GfxMonitor* monitor, GfxGammaRamp ramp)
{
	sz size = (sz)xRRGetCrtcGammaSize(ctx.display, monitor.node.crtc);
	if (size != ramp.red.len) return gfx::GAMMA_COULD_NOT_BE_SET~;

	XRRCrtcGamma* gamma = xRRAllocGamma((int)size);
	if (!gamma) return gfx::GAMMA_COULD_NOT_BE_SET~;
	defer xRRFreeGamma(gamma);

	gamma.red[:size] = ramp.red[..];
	gamma.green[:size] = ramp.green[..];
	gamma.blue[:size] = ramp.blue[..];
	xRRSetCrtcGamma(ctx.display, monitor.node.crtc, gamma);
}

fn void? monitor_set_mode(GfxMonitor* mon, GfxMonitorMode* mode)
{
	bool out = false;
	XRRScreenResources* res = xRRGetScreenResourcesCurrent(ctx.display, xDefaultRootWindow(ctx.display));
	XRRCrtcInfo* ci = xRRGetCrtcInfo(ctx.display, res, mon.node.crtc);
	if (xRRSetCrtcConfig(ctx.display, res, mon.node.crtc, 0, ci.x, ci.y, (RRMode)(uptr)mode.src, ci.rotation, ci.outputs, ci.noutput) == 0) out = true;
	xRRFreeCrtcInfo(ci);
	xRRFreeScreenResources(res);
	if (!out) return gfx::MONITOR_MODE_CHANGE_FAILED~;
}

fn void poll_monitors()
{
	XWindow root = xDefaultRootWindow(ctx.display);
	XRRScreenResources* res = xRRGetScreenResourcesCurrent(ctx.display, root);
	if (res == null) return;
	RROutput primary = xRRGetOutputPrimary(ctx.display, root);

	for (GfxMonitorNode* node = gfx::_gfx.monitors.list.head; node != null; node = node.next) node.disconnected = true;

	for (int i = 0; i < res.noutput; i++)
	{
		GfxMonitorNode* node = null;
		for (node = gfx::_gfx.monitors.list.head; node != null; node = node.next)
		{
			if (node.rr_output == res.outputs[i]) break;
		}
		if (node)
		{
			node.disconnected = false;
			if (node.rr_output == primary) gfx::_gfx.monitors.primary = node;
			continue;
		}

		GfxMonitor monitor;
		GfxXRROutputInfo* info = xRRGetOutputInfo(ctx.display, res, res.outputs[i]);
		if (info == null) continue;
		if (info.connection != 0 || info.crtc == 0) { xRRFreeOutputInfo(info); continue; }

		XRRCrtcInfo* ci = xRRGetCrtcInfo(ctx.display, res, info.crtc);
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
		_get_system_content_dpi(&dpi);
		monitor.scale = (Vec2)(dpi / 96.0f);
		monitor.pixelRatio = dpi >= 192.0f ? 2.0f : 1.0f;

		void* mi = _get_mode(ci, res, ci.mode, &monitor.mode);
		if (mi == null) break;

		xRRFreeCrtcInfo(ci);
		node = gfx::_monitor_add(&monitor);
		node.rr_output = res.outputs[i];
		node.crtc = info.crtc;
		if (node.rr_output == primary) gfx::_gfx.monitors.primary = node;
		xRRFreeOutputInfo(info);
		gfx::_monitor_callback(gfx::_gfx.root, &node.mon, true);
	}

	xRRFreeScreenResources(res);
	gfx::refresh_monitors();
}

// --- GfxMouse and mouse related

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
	XCursor cursor = xcursor_image_load_cursor_fn(ctx.display, native);
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
	XCursor cursor = xCreateFontCursor(ctx.display, mouseIcon);
	if (!cursor) return gfx::MOUSE_NOT_SUPPORTED~;
	return (GfxMouse*)cursor;
}

fn void free_mouse(GfxMouse* mouse)
{
	xFreeCursor(ctx.display, (XCursor)mouse);
}

fn GfxPoint get_global_mouse()
{
	int x, y, fX, fY;
	uint z;
	XWindow window1, window2;
	xQueryPointer(ctx.display, xDefaultRootWindow(ctx.display), &window1, &window2, &fX, &fY, &x, &y, &z);
	return { fX, fY };
}

// --- GfxWindow

fn void window_blit_surface(GfxWindow* win, GfxSurface* surface)
{
	GfxPoint min = math::min(win.size, surface.size);
	gfx::copy_image_data(surface.native.buffer, surface.size.x, min.y, surface.native.format, surface.data, surface.format, surface.convert_fn);
	xPutImage(ctx.display, win.src.window, win.src.gc, surface.native.bitmap, 0, 0, 0, 0, (uint)min.x, (uint)min.y);
	_handle_present(win);
}

fn void? window_create_platform(GfxWindow* win, String name, GfxWindowFlags flags)
{
	win.src.backing = {};
	if (flags.open_gl || flags.egl)
	{
		win.src.window = 0;
		if (flags.open_gl) win.create_opengl_context(gfx::get_global_opengl_hints())!;
		if (flags.egl) egl::create_context(win, gfx::get_global_opengl_hints())!;
	}
	else
	{
		XVisualInfo visual;
		_get_visual_for_window(&visual, win.internal.flags.transparent);
		_create_window(visual, name, flags, win)!;
	}
	gfx::_window_set_flags(win, flags, {});
}

fn void window_destroy_platform(GfxWindow* win)
{
	_term_resize_sync(win);
	if (win.src.ic)
	{
		xDestroyIC(win.src.ic);
		win.src.ic = null;
	}
	xFreeGC(ctx.display, win.src.gc);
	xDeleteContext(ctx.display, win.src.window, ctx.context);
	xDestroyWindow(ctx.display, win.src.window);
}

fn GfxPoint window_fetch_size(GfxWindow* win)
{
	XWindowAttributes attribs;
	xGetWindowAttributes(ctx.display, win.src.window, &attribs);
	win.size = { attribs.width, attribs.height };
	return win.get_size();
}

fn GfxMonitor*? window_find_monitor(GfxWindow* win)
{
	XWindowAttributes attrs;
	if (xGetWindowAttributes(ctx.display, win.src.window, &attrs) == 0) return NOT_FOUND~;
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

fn void window_flash(GfxWindow* win, GfxFlashRequest request)
{
	if (win.has_focus() && request != CANCEL) return;
	XWMHints* wmhints = xGetWMHints(ctx.display, win.src.window);
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
	xSetWMHints(ctx.display, win.src.window, wmhints);
	xFree(wmhints);
}

fn void window_focus(GfxWindow* win)
{
	XWindowAttributes attr;
	xGetWindowAttributes(ctx.display, win.src.window, &attr);
	if (attr.map_state != 2 /* IsViewable */) return;
	xSetInputFocus(ctx.display, win.src.window, 1 /* RevertToPointerRoot */, 0);
	xFlush(ctx.display);
}

fn void window_hide(GfxWindow* win)
{
	win.internal.flags.hide = true;
	xUnmapWindow(ctx.display, win.src.window);
	xFlush(ctx.display);
}

fn bool window_is_floating(GfxWindow* win)
{
	Atom actualType;
	int actualFormat;
	CULong nitems, bytesAfter;
	Atom* propReturn = null;
	int status = xGetWindowProperty(ctx.display, win.src.window, ctx.net_wm_state, 0, ~0, 0, 4, &actualType, &actualFormat, &nitems, &bytesAfter, (char**)&propReturn);
	if (status != 0 || actualType != 4) return false;
	for (CULong i = 0; i < nitems; i++) if (propReturn[i] == ctx.net_wm_state_above) return true;
	if (propReturn != null) xFree(propReturn);
	return false;
}

fn bool window_is_hidden(GfxWindow* win)
{
	XWindowAttributes windowAttributes;
	xGetWindowAttributes(ctx.display, win.src.window, &windowAttributes);
	return windowAttributes.map_state != 2;
}

fn bool window_is_maximized(GfxWindow* win)
{
	Atom actualType;
	int actualFormat;
	CULong nitems, bytesAfter;
	char* propData;
	int status = xGetWindowProperty(ctx.display, win.src.window, ctx.net_wm_state, 0, 1024, 0, 4, &actualType, &actualFormat, &nitems, &bytesAfter, &propData);
	if (status != 0) { if (propData != null) xFree(propData); return false; }
	for (CULong i = 0; i < nitems; i++)
	{
		if ((Atom)propData[i] == ctx.net_wm_state_maximized_vert || (Atom)propData[i] == ctx.net_wm_state_maximized_horz)
		{
			xFree(propData);
			return true;
		}
	}
	if (propData != null) xFree(propData);
	return false;
}

fn bool window_is_minimized(GfxWindow* win)
{
	Atom actualType;
	int actualFormat;
	CULong nitems, bytesAfter;
	char* propData;
	int status = xGetWindowProperty(ctx.display, win.src.window, ctx.wm_state, 0, 2, 0, 0, &actualType, &actualFormat, &nitems, &bytesAfter, &propData);
	if (status == 0 && nitems >= 1 && propData == (char*)(uptr)3 /* IconicState */)
	{
		xFree(propData);
		return true;
	}
	if (propData != null) xFree(propData);
	XWindowAttributes windowAttributes;
	xGetWindowAttributes(ctx.display, win.src.window, &windowAttributes);
	return windowAttributes.map_state != 2;
}

fn void window_maximize_platform(GfxWindow* win) => _toggle_maximized(win, true);

fn void window_minimize_platform(GfxWindow* win)
{
	if (x11::window_is_maximized(win)) return;
	win.internal.old_bounds = win.bounds;
	xIconifyWindow(ctx.display, win.src.window, xDefaultScreen(ctx.display));
	xFlush(ctx.display);
}

fn void window_move_mouse(GfxWindow* win, GfxPoint where)
{
	int rootX, rootY, win_x, win_y;
	uint mask;
	XWindow root_ret, child_ret;
	xQueryPointer(ctx.display, xDefaultRootWindow(ctx.display), &root_ret, &child_ret, &rootX, &rootY, &win_x, &win_y, &mask);
	win.internal.last_mouse = where - win.origin;
	if (win_x == where.x && win_y == where.y) return;
	xWarpPointer(ctx.display, 0, win.src.window, 0, 0, 0, 0, ...(where - win.origin));
}

fn void window_move_platform(GfxWindow* win, GfxPoint where)
{
	win.origin = where;
	xMoveWindow(ctx.display, win.src.window, ...where);
}

fn void window_raise(GfxWindow* win)
{
	xMapRaised(ctx.display, win.src.window);
	window_set_fullscreen(win, win.is_fullscreen());
}

fn void window_resize_platform(GfxWindow* win, GfxSize size)
{
	win.size = size;
	xResizeWindow(ctx.display, win.src.window, (uint)size.x, (uint)size.y);
	if (win.internal.flags.no_resize)
	{
		XSizeHints sh;
		sh.flags = (1 << 4) | (1 << 5);
		sh.min_width = sh.max_width = size.x;
		sh.min_height = sh.max_height = size.y;
		xSetWMSizeHints(ctx.display, win.src.window, &sh, 0);
	}
}

fn void window_restore_platform(GfxWindow* win)
{
	_toggle_maximized(win, false);
	xFlush(ctx.display);
}

fn void window_set_aspect_ratio(GfxWindow* win, int w, int h)
{
	if (w == 0 && h == 0) return;
	XSizeHints hints;
	CLong flags;
	xGetWMNormalHints(ctx.display, win.src.window, &hints, &flags);
	hints.flags |= (1 << 6); // PAspect
	hints.min_aspect.x = w; hints.max_aspect.x = w;
	hints.min_aspect.y = h; hints.max_aspect.y = h;
	xSetWMNormalHints(ctx.display, win.src.window, &hints);
}

fn void window_set_border(GfxWindow* win, bool border)
{
	win.internal.flags.no_border = !border;
	X11WindowHints hints;
	hints.flags = 2;
	hints.decorations = (CULong)border;
	xChangeProperty(ctx.display, win.src.window, ctx.motif_wm_hints, ctx.motif_wm_hints, 32, 0, (char*)&hints, 5);
	if (!window_is_hidden(win))
	{
		win.hide();
		win.show();
	}
}

fn void window_set_capture_mouse_platform(GfxWindow* win, bool state)
{
	if (state)
	{
		uint eventMask = (1 << 2) | (1 << 3) | (1 << 6); // Button press/release + pointer motion
		xGrabPointer(ctx.display, win.src.window, X_TRUE, eventMask, 1, 1, win.src.window, 0, 0);
	}
	else
	{
		xUngrabPointer(ctx.display, 0);
	}
}

fn void window_set_floating(GfxWindow* win, bool floating) => _window_set_atom(win, ctx.net_wm_state_above, floating);

fn void window_set_fullscreen(GfxWindow* win, bool fullscreen)
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

	xRaiseWindow(ctx.display, win.src.window);
	_window_set_atom(win, ctx.net_wm_state_fullscreen, fullscreen);

	if (!(win.internal.flags.transparent))
	{
		char value = (char)fullscreen;
		xChangeProperty(ctx.display, win.src.window, ctx.net_wm_bypass_compositor, 6 /* XA_CARDINAL */, 32, 0, &value, 1);
	}
}

fn bool window_set_icon(GfxWindow* win, char* dataSrc, int w, int h, GfxFormat format, GfxWindowIconType type)
{
	if (dataSrc == null)
	{
		return !!xChangeProperty(ctx.display, win.src.window, ctx.net_wm_icon, 6, 32, 0, null, 0);
	}
	int count = 2 + (w * h);
	CULong* data = calloc((sz)count * CULong::size);
	data[0] = (CULong)w;
	data[1] = (CULong)h;

	gfx::copy_image_data_64((char*)&data[2], w, h, BGRA8, dataSrc, format, true, null);
	bool res = true;
	if (type & TASKBAR)
	{
		res = !!xChangeProperty(ctx.display, win.src.window, ctx.net_wm_icon, 6, 32, 0, (char*)data, count);
	}

	gfx::copy_image_data_64((char*)&data[2], w, h, BGRA8, dataSrc, format, false, null);

	if (type & WINDOW)
	{
		XWMHints wmHints;
		wmHints.flags = 1 << 2; // IconPixmapHint

		int depth = xDefaultDepth(ctx.display, xDefaultScreen(ctx.display));
		void* image = xCreateImage(ctx.display, xDefaultVisual(ctx.display, xDefaultScreen(ctx.display)),
		(uint)depth, 2 /* ZPixmap */, 0, (char*)&data[2], (uint)w, (uint)h, 32, 0);

		wmHints.icon_pixmap = xCreatePixmap(ctx.display, win.src.window, (uint)w, (uint)h, (uint)depth);
		xPutImage(ctx.display, wmHints.icon_pixmap, xDefaultGC(ctx.display, xDefaultScreen(ctx.display)), image, 0, 0, 0, 0, (uint)w, (uint)h);
		((XImageHeader*)image).data = null;
		xDestroyImage(image);

		xSetWMHints(ctx.display, win.src.window, &wmHints);
	}

	free(data);
	xFlush(ctx.display);
	return res;
}

fn void window_set_max_size(GfxWindow* win, GfxSize size)
{
	CLong flags;
	XSizeHints hints;
	xGetWMNormalHints(ctx.display, win.src.window, &hints, &flags);
	hints.flags |= 1 << 5; // PMaxSize
	hints.max_width = size.x; hints.max_height = size.y;
	xSetWMNormalHints(ctx.display, win.src.window, &hints);
}

fn void window_set_min_size(GfxWindow* win, GfxSize size)
{
	CLong flags;
	XSizeHints hints;
	xGetWMNormalHints(ctx.display, win.src.window, &hints, &flags);
	hints.flags |= 1 << 4; // PMinSize
	hints.min_width = size.x; hints.min_height = size.y;
	xSetWMNormalHints(ctx.display, win.src.window, &hints);
}

fn void window_set_mouse_passthrough(GfxWindow* win, bool passthrough)
{
	if (passthrough)
	{
		Region region = xCreateRegion();
		if (xshape_combine_region_fn) xshape_combine_region_fn(ctx.display, win.src.window, 2, 0, 0, region, 0);
		xDestroyRegion(region);
		return;
	}
	if (xshape_combine_mask_fn) xshape_combine_mask_fn(ctx.display, win.src.window, 2, 0, 0, 0, 0);
}

fn void window_set_mouse_platform(GfxWindow* win, GfxMouse* mouse)
{
	xDefineCursor(ctx.display, win.src.window, (XCursor)mouse);
}

fn void window_set_name(GfxWindow* win, String name) => @pool()
{
	ZString cstring = name.zstr_copy(tmem);
	xutf8SetWMProperties(ctx.display, win.src.window, cstring, cstring, null, 0, null, null, null);
	xStoreName(ctx.display, win.src.window, cstring);
	xChangeProperty(ctx.display, win.src.window, ctx.net_wm_name, ctx.utf8_string, 8, 0, cstring, name.len);
}

fn void window_set_opacity(GfxWindow* win, float opacity)
{
	uint value = (uint)math::round(uint::max * opacity);
	xChangeProperty(ctx.display, win.src.window, ctx.net_wm_window_opacity, 6, 32, 0, (char*)&value, 1);
}

fn void window_set_raw_mouse_mode_platform(GfxWindow* win, bool state)
{
	char[4] mask;
	if (state) xi_set_mask((char*)&mask, 6 /* XI_RawMotion */);
	XIEventMask em;
	em.deviceid = 1; // XIAllMasterDevices
	em.mask_len = 4;
	em.mask = (char*)&mask;
	if (xi_select_events_fn) xi_select_events_fn(ctx.display, xDefaultRootWindow(ctx.display), &em, 1);
}

fn void window_show_platform(GfxWindow* win)
{
	win.internal.flags.hide = false;
	if (!window_is_hidden(win)) return;
	xMapWindow(ctx.display, win.src.window);
	_wait_for_show_event(win);
	win.move(win.origin);
	window_set_fullscreen(win, win.is_fullscreen());
}

// Window - opengl

fn void? window_init_opengl_context_platform(GfxWindow* win, GfxGlContext* context, GfxGlHints* hints)
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
		if (opengl_extension_is_supported_platform("GLX_ARB_framebuffer_sRGB")) stack.push_all({ GLX_FRAMEBUFFER_SRGB_CAPABLE_ARB, (int)hints.srgb });
		if (opengl_extension_is_supported_platform("GLX_EXT_framebuffer_sRGB")) stack.push_all({ GLX_FRAMEBUFFER_SRGB_CAPABLE_EXT, (int)hints.srgb });
	}

	stack.push_all({0, 0});

	int fbcount;
	GLXFBConfig* fbc = _glx_choose_fb_config_fn(ctx.display, xDefaultScreen(ctx.display), stack.array_view(), &fbcount);
	if (!fbc || !fbcount) return gfx::GLX_VISUAL_CONFIGS_NOT_FOUND~;

	int bestFbcIdx = -1;
	int bestDepth = 0;
	int bestSamples = 0;
	for (int i = 0; i < fbcount; i++)
	{
		XVisualInfo* vi = _glx_get_visual_from_fb_config_fn(ctx.display, fbc[i]);
		if (vi == null) continue;
		int sampBuf, samples;
		_glx_get_fb_config_attrib_fn(ctx.display, fbc[i], GLX_SAMPLE_BUFFERS, &sampBuf);
		_glx_get_fb_config_attrib_fn(ctx.display, fbc[i], GLX_SAMPLES, &samples);
		if (bestFbcIdx == -1) bestFbcIdx = i;
		if ((!transparent || vi.depth == 32) && bestDepth == 0) { bestFbcIdx = i; bestDepth = vi.depth; }
		if ((!transparent || vi.depth == 32) && samples <= hints.samples && samples > bestSamples) { bestFbcIdx = i; bestDepth = vi.depth; bestSamples = samples; }
		xFree(vi);
	}
	if (bestFbcIdx == -1) return gfx::GLX_NO_VALID_VISUAL~;

	GLXFBConfig bestFbc = fbc[bestFbcIdx];
	XVisualInfo* vi = _glx_get_visual_from_fb_config_fn(ctx.display, bestFbc);
	xFree(fbc);
	XVisualInfo visual = *vi;
	xFree(vi);

	_create_window(visual, "", win.internal.flags, win)!;

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

	if (opengl_extension_is_supported_platform("GLX_ARB_context_flush_control"))
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
	if (hints.robustness && opengl_extension_is_supported_platform("GLX_ARB_create_context_robustness")) glFlags |= GLX_CONTEXT_ROBUST_ACCESS_BIT_ARB;
	if (glFlags != 0) stack.push_all({ GLX_CONTEXT_FLAGS_ARB, glFlags });

	if (opengl_extension_is_supported_platform("GLX_ARB_create_context_no_error")) stack.push_all({ GLX_CONTEXT_OPENGL_NO_ERROR_ARB, (int)hints.no_error });

	stack.push_all({0, 0});

	GLXContext glx_ctx = null;
	if (hints.share) glx_ctx = hints.share.ctx;

	if (!_glx_create_context_attribs_arb_fn)
	{
		log::warn("Failed to load proc address 'glXCreateContextAttribsARB', loading a generic OpenGL context.", category: log::CATEGORY_VIDEO);
		win.src.ctx.native.ctx = _glx_create_context_fn(ctx.display, &visual, glx_ctx, X_TRUE);
	}
	else
	{
		ctx.x11_error = null;
		win.src.ctx.native.ctx = _glx_create_context_attribs_arb_fn(ctx.display, bestFbc, glx_ctx, X_TRUE, stack.array_view());
		if (ctx.x11_error || !win.src.ctx.native.ctx)
		{
			log::warn("Failed to create an OpenGL context with AttribsARB, loading a generic OpenGL context.", category: log::CATEGORY_VIDEO);
			win.src.ctx.native.ctx = _glx_create_context_fn(ctx.display, &visual, glx_ctx, X_TRUE);
		}
	}

	win.src.ctx.native.window = (void*)_glx_create_window_fn(ctx.display, bestFbc, win.src.window, null);
	_glx_make_current_fn(ctx.display, (GLXWindow)win.src.ctx.native.window, win.src.ctx.native.ctx);
	log::info("OpenGL context initalized.", category: log::CATEGORY_VIDEO);
	gfx::_window_set_opengl_swap_interval(win, 0);
}

fn void window_make_current_opengl_context(GfxWindow* win)
{
	if (!win)
	{
		_glx_make_current_fn(null, 0, null);
	}
	else
	{
		_glx_make_current_fn(ctx.display, (GLXWindow)win.src.ctx.native.window, win.src.ctx.native.ctx);
	}
}

fn void window_opengl_swap_buffers(GfxWindow* win)
{
	_glx_swap_buffers_fn(ctx.display, (GLXWindow)win.src.ctx.native.window);
	_handle_present(win);
}

fn void window_release_opengl_context_platform(GfxWindow* win, GfxGlContext* gl_ctx)
{
	if ((GLXWindow)win.src.ctx.native.window != win.src.window) _glx_destroy_window_fn(ctx.display, (GLXWindow)win.src.ctx.native.window);
	_glx_destroy_context_fn(ctx.display, gl_ctx.ctx);
	win.src.ctx.native = null;
}

fn void window_set_opengl_swap_interval(GfxWindow* win, int swapInterval)
{
	switch
	{
		case _glx_swap_interval_ext_fn != null:
			_glx_swap_interval_ext_fn(ctx.display, (GLXWindow)win.src.ctx.native.window, swapInterval);
		case _glx_swap_interval_native_fn != null:
			_glx_swap_interval_native_fn(swapInterval);
		default:
			log::warn("Failed to load swap interval function", category: log::CATEGORY_VIDEO);
	}
}

// Vulkan

fn VkSurfaceKHR? window_create_vulkan_surface(GfxWindow* win, VkInstance instance) @private
{
	VkSurfaceKHR surface;
	VkCreateSurfaceKHRProc proc = (VkCreateSurfaceKHRProc)gfx::_get_instance_proc_vulkan(instance, "vkCreateXlibSurfaceKHR");
	if (!proc) return gfx::VULKAN_NOT_LOADED~;
	VkXlibSurfaceCreateInfoKHR create_info = {
		.sType = gfx::VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR,
		.dpy = ctx.display,
		.window = win.src.window
	};
	if (proc(instance, &create_info, null, &surface)) return gfx::SURFACE_CREATION_FAILED~;
	return surface;
}

// Window WGPU

fn WGPUSurface? window_create_wgpu_surface(GfxWindow* window, WGPUInstance instance)
{
	WgpuSurfaceSourceXlibWindow fromXlib;
	fromXlib.chain.next = null;
	fromXlib.chain.sType = gfx::WGPU_STYPE_SURFACE_SOURCE_XLIB_WINDOW;
	fromXlib.display = ctx.display;
	fromXlib.window = window.src.window;
	WgpuSurfaceDescriptor surfaceDesc;
	surfaceDesc.nextInChain = (WgpuChainedStruct*)&fromXlib;
	return gfx::wgpuInstanceCreateSurface(instance, &surfaceDesc) ?: gfx::SURFACE_CREATION_FAILED~;
}

// --- Clipboard

fn GfxDataTransfer? read_clipboard(Allocator allocator, GfxDataTransferType type)
{
	if (type != TEXT) return gfx::CLIPBOARD_DID_NOT_CONTAIN_TYPE~;

	if (xGetSelectionOwner(ctx.display, ctx.clipboard_atom) == ctx.helper_window)
	{
		if (!gfx::_gfx.unixClipboard.text) return gfx::CLIPBOARD_DID_NOT_CONTAIN_TYPE~;
		return { .type = TEXT, .text = gfx::_gfx.unixClipboard.text.copy(allocator) };
	}

	xConvertSelection(ctx.display, ctx.clipboard_atom, ctx.utf8_string, ctx.xsel_data, ctx.helper_window, 0);
	xSync(ctx.display, 0);
	XEvent event;
	while (true)
	{
		xNextEvent(ctx.display, &event);
		if (event.type != 31 /* SelectionNotify */) continue;
		if (event.xselection.selection != ctx.clipboard_atom || event.xselection.property == 0) return gfx::CLIPBOARD_COULD_NOT_BE_OPENED~;
		break;
	}

	Atom target;
	int format;
	CULong n, size;
	char* data;
	xGetWindowProperty(ctx.display, event.xselection.requestor, event.xselection.property, 0, ~0, 0, 0, &target, &format, &size, &n, (char**)&data);
	defer xDeleteProperty(ctx.display, event.xselection.requestor, event.xselection.property);

	if (target == ctx.utf8_string || target == 31)
	{
		defer xFree(data);
		return { .type = TEXT, .text = (String)alloc::clone_slice(allocator, data[:size]) };
	}
	return gfx::CLIPBOARD_DID_NOT_CONTAIN_TYPE~;
}

fn void? write_clipboard(GfxDataTransfer* data)
{
	xSetSelectionOwner(ctx.display, ctx.clipboard_atom, ctx.helper_window, 0);
	if (xGetSelectionOwner(ctx.display, ctx.clipboard_atom) != ctx.helper_window)
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

// --- GfxSurface

fn void? surface_init(GfxSurface* surface, void* data, int w, int h, GfxFormat format, GfxWindow* window)
{
	if (!window) window = gfx::_gfx.root;
	surface.data = data;
	surface.size = { w, h };
	surface.format = format;

	XWindowAttributes attrs;
	if (!xGetWindowAttributes(ctx.display, window.src.window, &attrs)) return gfx::SURFACE_CREATION_FAILED~;
	defer catch surface.release();
	surface.native.bitmap = xCreateImage(ctx.display, attrs.visual, (uint)attrs.depth, 2 /* ZPixmap */, 0, null, ...(uint[<2>])surface.size, 32, 0);
	if (!surface.native.bitmap) return gfx::SURFACE_CREATION_FAILED~;
	surface.native.buffer = malloc((sz)w * h * 4);
	((XImageHeader*)surface.native.bitmap).data = surface.native.buffer;
	surface.native.format = _image_get_format(surface.native.bitmap);

}

fn void surface_release(GfxSurface* surface)
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

// --- Vulkan

fn bool get_vulkan_physical_device_presentation_support(VkInstance instance, VkPhysicalDevice physical_device, uint queue_family_index)
{
	VkGetPhysicalDeviceXlibPresentationSupportKHRProc proc = gfx::_get_instance_proc_vulkan(instance, "vkGetPhysicalDeviceXlibPresentationSupportKHR");
	if (!proc) return false;
	XVisual* visual = xDefaultVisual(ctx.display, xDefaultScreen(ctx.display));
	uint out = proc(physical_device, queue_family_index, ctx.display, xVisualIDFromVisual(visual));
	return out != 0;
}

// --- Opengl

fn void* get_current_opengl_context() => _glx_get_current_context_fn();

fn void* get_opengl_proc(ZString procname)
{
	if (!gfx::_gfx.gl.get_proc_addr_fn) return null;
	return gfx::_gfx.gl.get_proc_addr_fn(procname);
}

fn void? load_opengl()
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

fn bool opengl_extension_is_supported_platform(String extension)
{
	ZString extensions = _glx_query_extensions_string_fn(ctx.display, xDefaultScreen(ctx.display));
	return extensions && gfx::_extension_is_in_list(extensions.str_view(), extension);
}

// --- Keys

fn void init_keycodes_platform()
{
	gfx::_init_unix_keycodes();
}

fn GfxKey physical_to_mapped_key(GfxKey key)
{
	KeyCode keycode = (KeyCode)key.to_api();
	KeySym sym = xkbKeycodeToKeysym(ctx.display, keycode, 0, 0);
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

const int X_TRUE = 1;
const int X_FALSE = 0;
const int NONE = 0;
const int CURRENT_TIME = 0;
const uint XKB_USE_CORE_KBD = 0x0100;
const char SHIFT_MASK = 1 << 0;
const char LOCK_MASK = 1 << 1;
const char MOD2_MASK = 1 << 4;
const char MOD3_MASK = 1 << 5;


fn void? _create_window(XVisualInfo visual, String name, GfxWindowFlags flags, GfxWindow* win) @local
{
	if (!name) name = "C3GFX";
	// KeyPress | KeyRelease | ButtonPress | ButtonRelease | PointerMotion | StructureNotify | FocusChange |
	// LeaveWindow | EnterWindow | Exposure | VisibilityChange | PropertyChange
	CLong eventMask = (1 << 0) | (1 << 1) | (1 << 2) | (1 << 3) | (1 << 6) | (1 << 17) | (1 << 21) |
	(1 << 5) | (1 << 4) | (1 << 15) | (1 << 16) | (1 << 22);

	XSetWindowAttributes swa;
	win.src.parent = xDefaultRootWindow(ctx.display);

	Colormap cmap = xCreateColormap(ctx.display, win.src.parent, visual.visual, 0);
	swa.colormap = cmap;
	swa.event_mask = eventMask;
	swa.background_pixmap = 0;

	win.src.window = xCreateWindow(ctx.display, win.src.parent, win.x, win.y, (uint)win.width, (uint)win.height,
	0, visual.depth, 1 /* InputOutput */, visual.visual, (1 << 3) | (1 << 13) | (1 << 11), &swa);
	// CWBorderPixel | CWColormap | CWEventMask
	if (!win.src.window) return gfx::X11_WINDOW_CREATION_FAILED~;

	win.src.flashEnd = {};
	xFreeColors(ctx.display, cmap, null, 0, 0);
	xSaveContext(ctx.display, win.src.window, ctx.context, (XPointer)win);
	win.src.gc = xCreateGC(ctx.display, win.src.window, 0, null);

	if (ctx.im != null)
	{
		XImCallback callback;
		callback.callback = &_ic_callback;
		callback.client_data = (XPointer)win;

		win.src.ic = xCreateIC(ctx.im, "inputStyle", (CULong)(0x0008 | 0x0400) /* XIMPreeditNothing | XIMStatusNothing */,
		"clientWindow", win.src.window, "focusWindow", win.src.window,
		"destroyCallback", &callback, (char*)null);
	}

	@pool()
	{
		XClassHint hint;
		hint.res_name = name.zstr_copy(tmem);
		hint.res_class = ctx.class_name.zstr_copy(tmem);
		xSetClassHint(ctx.display, win.src.window, &hint);
	};

	XWMHints hints;
	hints.flags = 1 << 1; // StateHint
	hints.initial_state = 1; // NormalState
	xSetWMHints(ctx.display, win.src.window, &hints);

	xSelectInput(ctx.display, win.src.window, eventMask);
	// Only advertise _NET_WM_SYNC_REQUEST when the counter is in place: a window that
	// claims the protocol but never acks leaves the WM waiting on every resize.
	bool resize_sync = ctx.resize_sync_enabled && flags.open_gl && _init_resize_sync(win);
	Atom[2] protocols;
	int protocol_count = 0;
	protocols[protocol_count++] = ctx.wm_delete_window;
	if (resize_sync) protocols[protocol_count++] = ctx.net_wm_sync_request;
	xSetWMProtocols(ctx.display, win.src.window, &protocols, protocol_count);
	win.set_name(name);
	xMoveWindow(ctx.display, win.src.window, win.x, win.y);

	if (flags.allow_dnd)
	{
		win.internal.flags.allow_dnd = true;
		char version = 5;
		xChangeProperty(ctx.display, win.src.window, ctx.xdnd_aware, 4, 32, 0, &version, 1);
	}

	win.src.bounds = win.bounds;
	xSetWindowBackground(ctx.display, win.src.window, 0);
	xClearWindow(ctx.display, win.src.window);
	xSetWindowBackgroundPixmap(ctx.display, win.src.window, 0);
}



fn void _init_xsync() @local
{
	ctx.xsync_available = false;
	if (!xsync_initialize_fn || !xsync_create_counter_fn || !xsync_set_counter_fn || !xsync_destroy_counter_fn) return;
	int opcode, event, error;
	if (!xQueryExtension(ctx.display, "SYNC", &opcode, &event, &error)) return;
	// Seeded with the version we would like; the server writes back what it has.
	int major = 5;
	int minor = 0;
	xsync_initialize_fn(ctx.display, &major, &minor);
	if (major < 3) return;
	ctx.xsync_available = true;
}

<*
 @require win != null
 @return "true if the counter was created and published on the window"
*>
fn bool _init_resize_sync(GfxWindow* win) @local
{
	win.src.counter = 0;
	win.src.resize_id = {};
	win.src.resize_in_progress = false;
	if (!ctx.xsync_available) return false;

	XSyncCounter counter = xsync_create_counter_fn(ctx.display, (XSyncValue){});
	if (!counter) return false;
	win.src.counter = counter;

	uint counter_id = (uint)counter;
	xChangeProperty(ctx.display, win.src.window, ctx.net_wm_sync_request_counter, XA_CARDINAL, 32, 0 /* PropModeReplace */, (char*)&counter_id, 1);
	return true;
}

<*
 @require win != null
*>
fn void _term_resize_sync(GfxWindow* win) @local
{
	if (!win.src.counter) return;
	xDeleteProperty(ctx.display, win.src.window, ctx.net_wm_sync_request_counter);
	xsync_destroy_counter_fn(ctx.display, win.src.counter);
	win.src.counter = 0;
	win.src.resize_id = {};
	win.src.resize_in_progress = false;
}

fn void _handle_sync_request(GfxWindow* win, XClientMessageEvent* event) @local
{
	win.src.resize_id.lo = (uint)event.data.l[2];
	win.src.resize_id.hi = (int)event.data.l[3];
	win.src.resize_in_progress = false;
}

fn void _handle_configure(GfxWindow* win) @local
{
	if (win.src.resize_id.lo || win.src.resize_id.hi) win.src.resize_in_progress = true;
}

fn void _handle_present(GfxWindow* win) @local
{
	if (!win.src.counter || !win.src.resize_in_progress) return;
	xsync_set_counter_fn(ctx.display, win.src.counter, win.src.resize_id);
	xFlush(ctx.display);
	win.src.resize_id = {};
	win.src.resize_in_progress = false;
}

fn void _handle_event() @local
{
	static Vec2 delta = {};

	XEvent e;
	xNextEvent(ctx.display, &e);

	if (e.type != 35 /* GenericEvent */)
	{
		delta = {};
	}

	if (e.type == ctx.xrandr_event_base + 0 /* RRNotify */)
	{
		poll_monitors();
		return;
	}

	if (e.type == 30 /* SelectionRequest */)
	{
		_handle_clipboard_selection(&e);
		return;
	}

	if (e.type == 35 /* GenericEvent */)
	{
		xGetEventData(ctx.display, &e.xcookie);
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
		xFreeEventData(ctx.display, &e.xcookie);
		return;
	}

	GfxWindow* win = null;
	if (xFindContext(ctx.display, e.xany.window, ctx.context, (XPointer*)&win) != 0) return;

	if (win.src.flashEnd != (Clock)0)
	{
		if (win.src.flashEnd.to_now() > time::NANO_DURATION_ZERO || win.has_focus()) win.flash(CANCEL);
	}

	bool keyRepeat = false;
	if (e.type == 3 /* KeyRelease */ && xEventsQueued(ctx.display, 1) != 0)
	{
		XEvent ne;
		xPeekEvent(ctx.display, &ne);
		if (ne.type == 2 /* KeyPress */ && e.xkey.time == ne.xkey.time && e.xkey.keycode == ne.xkey.keycode)
		{
			xNextEvent(ctx.display, &e);
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
				xkbGetState(ctx.display, XKB_USE_CORE_KBD, &state);
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
					xQueryPointer(ctx.display, xDefaultRootWindow(ctx.display), &rootRet, &childRet, &rootX, &rootY, &winX, &winY, &mask);
					KeySym sym = xkbKeycodeToKeysym(ctx.display, (KeyCode)e.xkey.keycode, 0, (mask & SHIFT_MASK) ? 1 : 0);

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
				xkbGetState(ctx.display, XKB_USE_CORE_KBD, &state);
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
				if (e.xproperty.atom == ctx.wm_state)
				{
					if (window_is_minimized(win) && !(win.internal.flags.minimize))
					{
						gfx::_window_minimized_callback(win);
						handled = true;
					}
				}
				else if (e.xproperty.atom == ctx.net_wm_state)
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
				// Both of these arrive as WM_PROTOCOLS messages carrying the protocol
				// atom in data.l[0]; checking the payload without the message type
				// would let any unrelated client message that happens to start with
				// the same value close the window.
				if (e.xclient.message_type == ctx.wm_protocols && e.xclient.format == 32)
				{
					if ((CULong)e.xclient.data.l[0] == ctx.wm_delete_window)
					{
						gfx::_window_close_callback(win);
						break;
					}
					if ((CULong)e.xclient.data.l[0] == ctx.net_wm_sync_request)
					{
						_handle_sync_request(win, &e.xclient);
						break;
					}
				}

				if (!(win.internal.flags.allow_dnd) || ctx.x11_version > 5 /* _XDND_VERSION */) return;

				GfxPoint drag;

				if (e.xclient.message_type == ctx.xdnd_enter)
				{
					CULong count;
					Atom* formats;
					Atom[3] realFormats;
					int list = (int)(e.xclient.data.l[1] & 1);

					ctx.x11_source = (XWindow)e.xclient.data.l[0];
					ctx.x11_version = (int)(e.xclient.data.l[1] >> 24);
					ctx.x11_format = 0;
					if (list)
					{
						Atom actualType;
						int actualFormat;
						CULong bytesAfter;
						xGetWindowProperty(ctx.display, ctx.x11_source, ctx.xdnd_type_list, 0, ~0, 0, 4,
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
						switch (formats[i])
						{
							case ctx.xtext_uri_list:
								ctx.x11_transfer_type = FILE;
							case ctx.xtext_plain:
								ctx.x11_transfer_type = TEXT;	 
							default:
								continue;	
						}
						ctx.x11_format = (int)formats[i];
						break;
					}

					if (list != 0 && formats != null) xFree(formats);

					gfx::_data_drag_callback(win, ctx.x11_transfer_type, ENTER, drag);
				}
				else if (e.xclient.message_type == ctx.xdnd_position)
				{
					int xabs = (int)((e.xclient.data.l[2] >> 16) & 0xffff);
					int yabs = (int)(e.xclient.data.l[2] & 0xffff);
					XWindow dummy;
					int xpos, ypos;

					xTranslateCoordinates(ctx.display, xDefaultRootWindow(ctx.display), win.src.window,
					xabs, yabs, &xpos, &ypos, &dummy);

					drag = { xpos, ypos };

					gfx::_mouse_motion_callback(win, drag);

					XEvent reply;
					reply.type = 33; // ClientMessage
					reply.xclient.window = ctx.x11_source;
					reply.xclient.message_type = ctx.xdnd_status;
					reply.xclient.format = 32;
					reply.xclient.data.l[0] = (CLong)win.src.window;
					reply.xclient.data.l[2] = 0;
					reply.xclient.data.l[3] = 0;

					if (ctx.x11_format != 0)
					{
						reply.xclient.data.l[1] = 1;
						if (ctx.x11_version >= 2) reply.xclient.data.l[4] = (CLong)ctx.xdnd_action_copy;
					}

					xSendEvent(ctx.display, ctx.x11_source, 0, 0, &reply);
					xFlush(ctx.display);

					gfx::_data_drag_callback(win, ctx.x11_transfer_type, MOVE, drag);
				}
				else if (e.xclient.message_type == ctx.xdnd_leave)
				{
					gfx::_data_drag_callback(win, ctx.x11_transfer_type, EXIT, drag);
				}
				else if (e.xclient.message_type == ctx.xdnd_drop)
				{
					if (ctx.x11_format != 0)
					{
						Time_t time = ctx.x11_version >= 1 ?(Time_t)e.xclient.data.l[2] : 0; // CurrentTime
						xConvertSelection(ctx.display, ctx.xdnd_selection, (Atom)ctx.x11_format,
						ctx.xdnd_selection, win.src.window, time);
					}
					else if (ctx.x11_version >= 2)
					{
						XEvent reply;
						reply.type = 33; // ClientMessage
						reply.xclient.window = ctx.x11_source;
						reply.xclient.message_type = ctx.xdnd_finished;
						reply.xclient.format = 32;
						reply.xclient.data.l[0] = (CLong)win.src.window;
						reply.xclient.data.l[1] = 0;
						reply.xclient.data.l[2] = 0;

						xSendEvent(ctx.display, ctx.x11_source, 0, 0, &reply);
						xFlush(ctx.display);
					}
				}
				break;
			}
		case 31: // SelectionNotify
			{
				if (!win.internal.enabled_events.data_drop || e.xselection.property != ctx.xdnd_selection || !(win.internal.flags.allow_dnd)) return;

				ZString data;
				CULong result;
				Atom actualType;
				int actualFormat;
				CULong bytesAfter;

				xGetWindowProperty(ctx.display, e.xselection.requestor, e.xselection.property, 0, ~0, 0, e.xselection.target,
				&actualType, &actualFormat, &result, &bytesAfter, (char**)&data);

				if (result != 0)
				{
					_drop_url(win, data);
					if (data != null) xFree(data);
				}

				if (ctx.x11_version >= 2)
				{
					XEvent reply;
					reply.type = 33; // ClientMessage
					reply.xclient.window = ctx.x11_source;
					reply.xclient.message_type = ctx.xdnd_finished;
					reply.xclient.format = 32;
					reply.xclient.data.l[0] = (CLong)win.src.window;
					reply.xclient.data.l[1] = (CLong)result;
					reply.xclient.data.l[2] = (CLong)ctx.xdnd_action_copy;
					xSendEvent(ctx.display, ctx.x11_source, 0, 0, &reply);
					xFlush(ctx.display);
				}
				break;
			}
		case 22: // ConfigureNotify
			{
				_handle_configure(win);
				if (e.xconfigure.width != win.src.width || e.xconfigure.height != win.src.height)
				{
					gfx::_window_check_mode(win);
					win.src.size = { e.xconfigure.width, e.xconfigure.height };
					gfx::_window_resized_callback(win, win.src.size);
				}
				GfxPoint configure = { e.xconfigure.x, e.xconfigure.y };

				// if the event came from the server and we're not a direct child of the root window then
				// we're using local coords which need to be translated into screen coords
				XWindow root = xDefaultRootWindow(ctx.display);
				if (e.xany.send_event == 0 && win.src.parent != root)
				{
					XWindow dummy;
					xTranslateCoordinates(ctx.display, win.src.parent, root, ...configure, &configure.x, &configure.y, &dummy);
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

	xFlush(ctx.display);
}


fn bool _wait_for_show_event(GfxWindow* win) @local
{
	XEvent dummy;
	while (xCheckTypedWindowEvent(ctx.display, win.src.window, 15 /* VisibilityNotify */, &dummy) == 0)
	{
		wait_for_event(time::ms(100));
	}
	return true;
}

fn void _toggle_maximized(GfxWindow* win, bool maximized) @local
{
	XEvent xev;
	xev.type = 33; // ClientMessage
	xev.xclient.window = win.src.window;
	xev.xclient.message_type = ctx.net_wm_state;
	xev.xclient.format = 32;
	xev.xclient.data.l[0] = (int)maximized;
	xev.xclient.data.l[1] = (CLong)ctx.net_wm_state_maximized_horz;
	xev.xclient.data.l[2] = (CLong)ctx.net_wm_state_maximized_vert;
	xSendEvent(ctx.display, xDefaultRootWindow(ctx.display), 0, (1 << 20) | (1 << 19), &xev);
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

fn void _get_visual_for_window(XVisualInfo* visual, bool transparent)
{
	visual.visual = xDefaultVisual(ctx.display, xDefaultScreen(ctx.display));
	visual.depth = xDefaultDepth(ctx.display, xDefaultScreen(ctx.display));
	if (transparent)
	{
		xMatchVisualInfo(ctx.display, xDefaultScreen(ctx.display), 32, 4 /* TrueColor */, visual);
		if (visual.depth != 32)
		{
			log::warn("Failed to load a 32-bit depth.", category: log::CATEGORY_VIDEO);
		}
	}
}

fn void _window_set_atom(GfxWindow* win, Atom netAtom, bool fullscreen) @local
{
	XEvent xev;
	xev.xclient.type = 33;
	xev.xclient.send_event = 1;
	xev.xclient.message_type = ctx.net_wm_state;
	xev.xclient.window = win.src.window;
	xev.xclient.format = 32;
	xev.xclient.data.l[0] = (int)fullscreen;
	xev.xclient.data.l[1] = (CLong)netAtom;
	xSendEvent(ctx.display, xDefaultRootWindow(ctx.display), 0, (1 << 19) | (1 << 20), &xev);
}

fn void _ic_callback(void* ic, void* client_data, void* call_data) @local
{
	GfxWindow* win = client_data;
	win.src.ic = null;
}

fn void _x11_im_callback(void* im, void* client_data, void* call_data)
{
	ctx.im = null;
}

fn void _x11_im_init_callback(XDisplay* d, XPointer client_data, XPointer call_data)
{
	if (ctx.im != null) return;

	ctx.im = xOpenIM(ctx.display, null, null, null);
	if (ctx.im == null) return;

	bool found = false;
	XImStyles* styles = null;

	if (xGetIMValues(ctx.im, "queryInputStyle", &styles, (char*)null) != null)
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
		xCloseIM(ctx.im);
		ctx.im = null;
		return;
	}

	XImCallback callback;
	callback.callback = &_x11_im_callback;
	callback.client_data = null;
	xSetIMValues(ctx.im, "destroyCallback", &callback, (char*)null);
}

fn int _error_handler(XDisplay* display, XErrorEvent* ev) @local
{
	char[512] errorText;
	xGetErrorText(display, ev.error_code, (char*)&errorText, 512);
	log::warn("X Error callback :%s", (ZString)&errorText, category: log::CATEGORY_VIDEO);
	ctx.x11_error = ev;
	return 0;
}

fn void* _get_mode(XRRCrtcInfo* ci, XRRScreenResources* res, RRMode mode, GfxMonitorMode* found_mode) @local
{
	XRRModeInfoNative* mi = null;
	for (int j = 0; j < res.nmode; j++) if (res.modes[j].id == mode) mi = &res.modes[j];
	if (!mi) return null;

	found_mode.size = { (int)mi.width, (int)mi.height };
	if (ci.rotation == 4 || ci.rotation == 8) // RR_Rotate_90 / RR_Rotate_270
	{
		found_mode.size = found_mode.size.yx;
	}

	gfx::_split_bpp((uint)xDefaultDepth(ctx.display, xDefaultScreen(ctx.display)), found_mode);
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

fn void _get_system_content_dpi(float* dpi) @local
{
	if (dpi == null) return;
	float dpiOutput = 96.0f;
	ZString rms = xResourceManagerString(ctx.display);
	if (rms == null) return;
	void* db = xrmGetStringDatabase(rms);
	if (db == null) return;
	ZString type = null;
	// xrmGetResource's value-out parameter is a small {len,addr} struct; omitted here.
	xrmDestroyDatabase(db);
	*dpi = dpiOutput;
}

fn void _handle_clipboard_selection(XEvent* event) @local
{
	XSelectionRequestEvent* request = &event.xselectionrequest;
	Atom[2] formats = {ctx.utf8_string, 31 /* XA_STRING */};

	if (request.target == ctx.targets)
	{
		Atom[4] targets = {ctx.targets, ctx.multiple, ctx.utf8_string, 31};
		xChangeProperty(ctx.display, request.requestor, request.property, 4 /* XA_ATOM */, 32, 0, (char*)&targets, 4);
	}
	else if (request.target == ctx.multiple)
	{
		Atom* targets = null;
		Atom actualType;
		int actualFormat;
		CULong count, bytesAfter;
		xGetWindowProperty(ctx.display, request.requestor, request.property, 0, ~0, 0, ctx.atom_pair,
		&actualType, &actualFormat, &count, &bytesAfter, (char**)&targets);

		for (CULong i = 0; i < count; i += 2)
		{
			if (targets[i] == ctx.utf8_string || targets[i] == 31 /* XA_STRING */)
			{
				xChangeProperty(ctx.display, request.requestor, targets[i + 1], targets[i],
				8, 0, gfx::_gfx.unixClipboard.text, (int)gfx::_gfx.unixClipboard.text.len);
			}
			else
			{
				targets[i + 1] = 0; // None
			}
		}

		xChangeProperty(ctx.display, request.requestor, request.property, ctx.atom_pair, 32, 0, (char*)targets, (int)count);
		xFlush(ctx.display);
		xFree(targets);
	}
	else if (request.target == ctx.save_targets)
	{
		xChangeProperty(ctx.display, request.requestor, request.property, 0, 32, 0, null, 0);
	}
	else
	{
		for (sz i = 0; i < 2; i++)
		{
			if (request.target != formats[i]) continue;
			xChangeProperty(ctx.display, request.requestor, request.property, request.target, 8, 0, gfx::_gfx.unixClipboard.text, (int)gfx::_gfx.unixClipboard.text.len);
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
	xSendEvent(ctx.display, request.requestor, 0, 0, &reply);
	xFlush(ctx.display);
}

fn int _handle_clipboard_selection_helper() @local
{
	XEvent event;
	xPending(ctx.display);
	if (xQLength(ctx.display) != 0 || xEventsQueued(ctx.display, 0) + xEventsQueued(ctx.display, 1) != 0)
	{
		xNextEvent(ctx.display, &event);
	}
	else
	{
		return 0;
	}

	if (event.type == 30) { _handle_clipboard_selection(&event); return 0; } // SelectionRequest
	return 0;
}

fn GfxFormat _image_get_format(void* imagePtr) @local
{
	// XImage*: bits_per_pixel/red_mask/green_mask/blue_mask live at a fixed
	// offset in the real struct; here we only ever set the format ourselves
	// right after xCreateImage, so this mirrors the original's fallback path.
	return BGRA8;
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


alias VkGetPhysicalDeviceXlibPresentationSupportKHRProc = fn uint(VkPhysicalDevice, uint, XDisplay*, VisualID);

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
