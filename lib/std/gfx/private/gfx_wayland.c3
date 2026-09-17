module std::gfx::wl @feat(BSD, LINUX) @feat(WAYLAND) @private;
import std::math, std::collections, libc, std::os::posix, std::net::os, std::os::env, std::time::clock, std::time;
import std::gfx::egl, std::gfx @public;

// --- Gfx

fn void destroy_platform()
{
	if (gfx::_gfx.unixClipboard) free(gfx::_gfx.unixClipboard);
	if (gfx::_gfx.wlPointer) wl_pointer_release(gfx::_gfx.wlPointer);
	if (gfx::_gfx.wlKeyboard) wl_keyboard_release(gfx::_gfx.wlKeyboard);
	if (gfx::_gfx.registry) wl_registry_destroy(gfx::_gfx.registry);
	if (gfx::_gfx.decorationManager) zxdg_decoration_manager_v1_destroy(gfx::_gfx.decorationManager);
	if (gfx::_gfx.relativePointerManager) zwp_relative_pointer_manager_v1_destroy(gfx::_gfx.relativePointerManager);
	if (gfx::_gfx.relativePointer) zwp_relative_pointer_v1_destroy(gfx::_gfx.relativePointer);
	if (gfx::_gfx.constraint_manager) zwp_pointer_constraints_v1_destroy(gfx::_gfx.constraint_manager);
	if (gfx::_gfx.iconManager) xdg_toplevel_icon_manager_v1_destroy(gfx::_gfx.iconManager);
	if (gfx::_gfx.xdgOutputManager) zxdg_output_manager_v1_destroy(gfx::_gfx.xdgOutputManager);
	if (gfx::_gfx.dataDeviceManager) wl_data_device_manager_destroy(gfx::_gfx.dataDeviceManager);
	if (gfx::_gfx.dataDevice) wl_data_device_destroy(gfx::_gfx.dataDevice);
	if (gfx::_gfx.wlCursorTheme) wl_cursor_theme_destroy(gfx::_gfx.wlCursorTheme);
	if (gfx::_gfx.wp_pointer_warp) wp_pointer_warp_v1_destroy(gfx::_gfx.wp_pointer_warp);
	if (gfx::_gfx.hiddenMouse) free_mouse(gfx::_gfx.hiddenMouse);
	if (gfx::_gfx.cursorSurface) wl_surface_destroy(gfx::_gfx.cursorSurface);
	if (gfx::_gfx.shm) wl_shm_destroy(gfx::_gfx.shm);
	if (gfx::_gfx.seat) wl_seat_destroy(gfx::_gfx.seat);
	if (gfx::_gfx.xdgWmBase) xdg_wm_base_destroy(gfx::_gfx.xdgWmBase);
	if (gfx::_gfx.compositor) wl_compositor_destroy(gfx::_gfx.compositor);
	if (gfx::_gfx.wlDisplay) wl_display_disconnect(gfx::_gfx.wlDisplay);
}

fn void poll_events_platform()
{
	gfx::_reset_prev_state();

	while (wl_display_flush(gfx::_gfx.wlDisplay) == -1)
	{
		if (libc::errno() != EAGAIN) return;
		if (wl_display_dispatch_pending(gfx::_gfx.wlDisplay) == -1) return;
	}

	if (gfx::_gfx.wlRepeatInfoRate != 0 && gfx::_gfx.lastKey != 0)
	{
		if (gfx::_gfx.lastKeyTime.to_now() >= time::NANO_DURATION_ZERO)
		{
			_send_key_event(gfx::_gfx.lastKey);
			gfx::_gfx.lastKeyTime = clock::now() + time::from_float(1.0 / gfx::_gfx.wlRepeatInfoRate).to_nano();
		}
	}

	Posix_pollfd fds;
	fds.fd = wl_display_get_fd(gfx::_gfx.wlDisplay);
	fds.events = (short)os::POLLIN;

	while (true)
	{
		while (wl_display_prepare_read(gfx::_gfx.wlDisplay) != 0)
		{
			if (wl_display_dispatch_pending(gfx::_gfx.wlDisplay) > 0) return;
		}

		if (os::poll(&fds, 1, 0) == 0)
		{
			wl_display_cancel_read(gfx::_gfx.wlDisplay);
			return;
		}

		if (fds.revents & os::POLLIN)
		{
			wl_display_read_events(gfx::_gfx.wlDisplay);
			if (wl_display_dispatch_pending(gfx::_gfx.wlDisplay) > 0) return;
		}
		else
		{
			wl_display_cancel_read(gfx::_gfx.wlDisplay);
		}
	}
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
	fds[0].fd = wl_display_get_fd(gfx::_gfx.wlDisplay);

	// empty the queue
	while (wl_display_prepare_read(gfx::_gfx.wlDisplay))
	{
		// error occured when dispatching the queue
		if (wl_display_dispatch_pending(gfx::_gfx.wlDisplay) == -1) return;
	}

	// send any pending requests to the compositor
	while (wl_display_flush(gfx::_gfx.wlDisplay) == -1)
	{
		// queue is full dispatch them
		if (libc::errno() == EAGAIN)
		{
			if (wl_display_dispatch_pending(gfx::_gfx.wlDisplay) == -1) return;
		}
		else
		{
			return;
		}
	}

	bool infinity = duration == gfx::WAIT_FOREVER;
	bool once = true;
	Clock start = clock::now();
	while (!wl_display_dispatch_pending(gfx::_gfx.wlDisplay))
	{
		Duration diff = duration - start.to_now().to_duration();
		long ms = diff.to_ms();
		if (ms < 0 && once)
		{
			once = false;
			ms = 0;
		}
		if (ms < 0 || os::poll(&fds[0], 1, infinity || ms > CInt::max ? -1 : (CInt)ms) <= 0)
		{
			wl_display_cancel_read(gfx::_gfx.wlDisplay);
			break;
		}
		once = false;
		if (wl_display_read_events(gfx::_gfx.wlDisplay) == -1) return;
	}

	// queue contains events from read, dispatch them
	if (wl_display_dispatch_pending(gfx::_gfx.wlDisplay) == -1) return;


}

// --- GfxMonitor functions

fn GfxGammaRamp monitor_get_gamma_ramp(GfxMonitor* monitor, Allocator a) => {};

fn GfxMonitorMode[] monitor_get_modes(GfxMonitor* monitor)
{
	List{GfxMonitorMode} modes;
	for (int i = 0; i < monitor.node.mode_count; i++)
	{
		modes.push(monitor.node.modes[i]);
	}
	return modes.array_view();
}

fn IntRect monitor_get_workarea(GfxMonitor* monitor)
{
	return { ...monitor.origin, ...monitor.mode.size };
}

fn void monitor_node_free(GfxMonitorNode* node)
{
	if (node.output) wl_output_release(node.output);
	if (node.xdg_output) zxdg_output_v1_destroy(node.xdg_output);
	if (node.modes)
	{
		free(node.modes);
		node.mode_count = 0;
	}
}

fn void? monitor_request_mode(GfxMonitor* mon, GfxMonitorMode* mode, GfxMonitorModeRequest request)
{
	for (sz i = 0; i < mon.node.mode_count; i++)
	{
		if (!mode.equals_request(&mon.node.modes[i], request)) continue;
		(void)monitor_set_mode(mon, &mon.node.modes[i]);
		return;
	}
	return gfx::MONITOR_MODE_CHANGE_FAILED~;
}

fn void? monitor_set_gamma(GfxMonitor* monitor, float gamma) => NOT_IMPLEMENTED~;

fn void? monitor_set_gamma_ramp(GfxMonitor* monitor, GfxGammaRamp ramp) => NOT_IMPLEMENTED~;

fn void? monitor_set_mode(GfxMonitor* mon, GfxMonitorMode* mode) => NOT_IMPLEMENTED~;

fn void poll_monitors()
{
	gfx::_gfx.monitors.primary = gfx::_gfx.monitors.list.head;
}

// --- GfxMouse and mouse related

fn GfxMouse*? create_mouse(char* data, GfxSize size, GfxFormat format)
{
	GfxSurface* surface = gfx::create_surface(data, ...size, format)!;
	surface.native.wlBuffer = wl_shm_pool_create_buffer(surface.native.pool, 0, ...surface.size, surface.size.x * 4, WL_SHM_FORMAT_ARGB8888);
	GfxSize min = math::min(size, surface.size);
	gfx::copy_image_data(surface.native.buffer, min.x, min.y, surface.native.format, surface.data, surface.format, null);
	return (GfxMouse*)surface;
}

fn GfxMouse*? create_standard_mouse(GfxMouseIcon mouse)
{
	ZString cursorName;
	switch (mouse)
	{
		case NORMAL: cursorName = "left_ptr";
		case ARROW: cursorName = "left_ptr";
		case IBEAM: cursorName = "xterm";
		case CROSSHAIR: cursorName = "crosshair";
		case POINTING_HAND: cursorName = "hand2";
		case RESIZE_EW: cursorName = "sb_h_double_arrow";
		case RESIZE_NS: cursorName = "sb_v_double_arrow";
		case RESIZE_NWSE: cursorName = "top_left_corner";
		case RESIZE_NESW: cursorName = "top_right_corner";
		case RESIZE_NW: cursorName = "top_left_corner";
		case RESIZE_N: cursorName = "top_side";
		case RESIZE_NE: cursorName = "top_right_corner";
		case RESIZE_E: cursorName = "right_side";
		case RESIZE_SE: cursorName = "bottom_right_corner";
		case RESIZE_S: cursorName = "bottom_side";
		case RESIZE_SW: cursorName = "bottom_left_corner";
		case RESIZE_W: cursorName = "left_side";
		case RESIZE_ALL: cursorName = "fleur";
		case NOT_ALLOWED: cursorName = "not-allowed";
		case WAIT: cursorName = "watch";
		case PROGRESS: cursorName = "watch";
		default: return gfx::MOUSE_NOT_SUPPORTED~;
	}

	WlCursor* wlcursor = wl_cursor_theme_get_cursor(gfx::_gfx.wlCursorTheme, cursorName);
	if (!wlcursor) return gfx::MOUSE_NOT_SUPPORTED~;
	WlCursorImage* cursorImage = wlcursor.images[0];
	WlProxy* cursorBuffer = wl_cursor_image_get_buffer(cursorImage);

	GfxSurface* surface = mem::new(GfxSurface);
	surface.size = { (int)cursorImage.width, (int)cursorImage.height };
	surface.native.wlBuffer = cursorBuffer;
	return (GfxMouse*)surface;
}

fn void free_mouse(GfxMouse* mouse)
{
	GfxSurface* surface = (GfxSurface*)mouse;
	if (surface.native.buffer && surface.native.wlBuffer) wl_buffer_destroy(surface.native.wlBuffer);
	surface.destroy();
}

fn GfxPoint get_global_mouse()
{
	return { -1, -1 };
}

// --- GfxWindow

fn void window_blit_surface(GfxWindow* win, GfxSurface* surface)
{
	GfxPoint min = math::min(win.size, surface.size);
	surface.native.wlBuffer = wl_shm_pool_create_buffer(surface.native.pool, 0, ...min, surface.size.x * 4, WL_SHM_FORMAT_ARGB8888);
	gfx::copy_image_data(surface.native.buffer, min.x, min.y, surface.native.format, surface.data, surface.format, surface.convert_fn);

	wl_surface_attach(win.src.surface, surface.native.wlBuffer, 0, 0);
	wl_surface_damage(win.src.surface, 0, 0, ...min);
	wl_surface_commit(win.src.surface);
	wl_buffer_destroy(surface.native.wlBuffer);
	surface.native.wlBuffer = null;
}

fn void? window_create_platform(GfxWindow* win, String name, GfxWindowFlags flags) => @pool()
{
	win.src.surface = wl_compositor_create_surface(gfx::_gfx.compositor);
	static WlSurfaceListener listener = {
		.enter = &_surface_enter_cb,
		.leave = &_surface_leave_cb,
		.preferredBufferScale = &_surface_pref_scale_cb,
		.preferredBufferTransform = &_surface_pref_transform_cb
	};
	wl_surface_add_listener(win.src.surface, &listener, win);
	wl_surface_set_user_data(win.src.surface, win);

	win.src.customCursorSurface = wl_compositor_create_surface(gfx::_gfx.compositor);

	win.src.xdgSurface = xdg_wm_base_get_xdg_surface(gfx::_gfx.xdgWmBase, win.src.surface);
	static XdgSurfaceListener surface_listener = { .configure = &_xdg_surface_conf_cb };

	xdg_surface_add_listener(win.src.xdgSurface, &surface_listener, win);
	xdg_wm_base_set_user_data(gfx::_gfx.xdgWmBase, win);

	win.src.xdgToplevel = xdg_surface_get_toplevel(win.src.xdgSurface);
	xdg_toplevel_set_app_id(win.src.xdgToplevel, name.zstr_copy(tmem));
	xdg_surface_set_window_geometry(win.src.xdgSurface, 0, 0, win.width, win.height);

	if (!win.internal.flags.transparent) _window_set_opaque(win);

	static XdgToplevelListener toplevel_listener = { .configure = &_xdg_toplevel_conf_cb, .close = &_xdg_toplevel_close_cb };

	xdg_toplevel_add_listener(win.src.xdgToplevel, &toplevel_listener, win);

	if (gfx::_gfx.decorationManager)
	{
		uint decorationMode = ZXDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE;
		win.src.decoration = zxdg_decoration_manager_v1_get_toplevel_decoration(gfx::_gfx.decorationManager, win.src.xdgToplevel);
		static ZxdgToplevelDecorationV1Listener decoration_listener = { .configure = &_xdg_dec_conf_cb };

		zxdg_toplevel_decoration_v1_add_listener(win.src.decoration, &decoration_listener, win);
		if (flags.no_border) decorationMode = ZXDG_TOPLEVEL_DECORATION_V1_MODE_CLIENT_SIDE;
		zxdg_toplevel_decoration_v1_set_mode(win.src.decoration, decorationMode);
	}

	if (gfx::_gfx.iconManager) xdg_toplevel_icon_manager_v1_set_icon(gfx::_gfx.iconManager, win.src.xdgToplevel, null);

	wl_surface_commit(win.src.surface);

	while (!win.src.configured) wl_display_dispatch(gfx::_gfx.wlDisplay);

	gfx::_window_set_flags(win, flags, {});
	win.src.backing = {};
	if (flags.open_gl) win.create_opengl_context(gfx::get_global_opengl_hints())!;
	if (flags.egl) egl::create_context(win, gfx::get_global_opengl_hints())!;
	if (!flags.open_gl && !flags.egl)
	{
		sz blank_size = (sz)win.width * win.height * 3;
		char* blank = calloc(blank_size);
		GfxSurface* blank_surface = gfx::create_surface(blank, ...win.size, BGR8, win)!;
		win.blit_surface(blank_surface);
		free(blank);
		blank_surface.release();
	}

}

fn void window_destroy_platform(GfxWindow* win)
{
	if (win.src.decoration) zxdg_toplevel_decoration_v1_destroy(win.src.decoration);
	if (win.src.xdgToplevel) xdg_toplevel_destroy(win.src.xdgToplevel);
	wl_surface_destroy(win.src.customCursorSurface);
	if (win.src.locked_pointer) zwp_locked_pointer_v1_destroy(win.src.locked_pointer);
	if (win.src.icon) xdg_toplevel_icon_v1_destroy(win.src.icon);
	xdg_surface_destroy(win.src.xdgSurface);
	wl_surface_destroy(win.src.surface);
}

fn GfxPoint window_fetch_size(GfxWindow* win) => win.get_size();

fn GfxMonitor*? window_find_monitor(GfxWindow* win)
{
	if (!win.src.activeMonitor) return gfx::get_primary_monitor();
	return &win.src.activeMonitor.mon;
}

fn void window_flash(GfxWindow* win, GfxFlashRequest request) {}

fn void window_focus(GfxWindow* win) {}

fn void window_hide(GfxWindow* win)
{
	wl_surface_attach(win.src.surface, null, 0, 0);
	wl_surface_commit(win.src.surface);
	win.internal.flags.hide = true;
}

fn bool window_is_floating(GfxWindow* win) => !win.is_fullscreen() && !window_is_maximized(win);

fn bool window_is_hidden(GfxWindow* win) => false;

fn bool window_is_maximized(GfxWindow* win) => win.src.maximized;

fn bool window_is_minimized(GfxWindow* win) => win.src.minimized;

fn void window_maximize_platform(GfxWindow* win)
{
	_toggle_maximized(win, true);
}

fn void window_minimize_platform(GfxWindow* win)
{
	if (window_is_maximized(win)) return;
	win.internal.old_bounds = win.bounds;
	win.src.minimized = true;
	xdg_toplevel_set_minimized(win.src.xdgToplevel);
}

fn void window_move_mouse(GfxWindow* win, GfxPoint where)
{
	if (!gfx::_gfx.wp_pointer_warp) return;
	wp_pointer_warp_v1_warp_pointer(gfx::_gfx.wp_pointer_warp, win.src.surface, gfx::_gfx.wlPointer, wlfixed_from_int(where.x), wlfixed_from_int(where.y), gfx::_gfx.mouseEnterSerial);
}

fn void window_move_platform(GfxWindow* win, GfxPoint where)
{
	win.origin = where;
}

fn void window_raise(GfxWindow* win) {}

fn void window_resize_platform(GfxWindow* win, GfxSize size)
{
	win.size = size;
	if (gfx::_gfx.compositor != null)
	{
		xdg_surface_set_window_geometry(win.src.xdgSurface, 0, 0, ...size);
		if (win.src.ctx.egl && win.src.ctx.egl.egl_window) wl_egl_window_resize((WlEglWindow*)win.src.ctx.egl.egl_window, ...size, 0, 0);
	}
}

fn void window_restore_platform(GfxWindow* win)
{
	_toggle_maximized(win, false);
}

fn void window_set_aspect_ratio(GfxWindow* win, int w, int h)
{
	if (w == 0 && h == 0) return;
	xdg_toplevel_set_max_size(win.src.xdgToplevel, w, h);
}

fn void window_set_border(GfxWindow* win, bool border)
{
	win.internal.flags.no_border = !border;
	if (gfx::_gfx.decorationManager)
	{
		zxdg_toplevel_decoration_v1_set_mode(win.src.decoration, border ? ZXDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE : ZXDG_TOPLEVEL_DECORATION_V1_MODE_CLIENT_SIDE);
	}
}

fn void window_set_capture_mouse_platform(GfxWindow* win, bool capture)
{
	if (!gfx::_gfx.constraint_manager) return;

	if (!capture)
	{
		if (win.src.locked_pointer) zwp_locked_pointer_v1_destroy(win.src.locked_pointer);
		win.src.locked_pointer = null;
		return;
	}

	if (win.src.locked_pointer) return;

	win.src.locked_pointer = zwp_pointer_constraints_v1_lock_pointer(gfx::_gfx.constraint_manager, win.src.surface, gfx::_gfx.wlPointer, null, ZWP_POINTER_CONSTRAINTS_V1_LIFETIME_PERSISTENT);
	static ZwpLockedPointerV1Listener listener = { .locked = &_pointer_locked_cb, .unlocked = &_unlocked_noop_cb };
	zwp_locked_pointer_v1_add_listener(win.src.locked_pointer, &listener, &gfx::_gfx);
}

fn void window_set_floating(GfxWindow* win, bool floating) {}

fn void window_set_fullscreen(GfxWindow* win, bool fullscreen)
{
	if (fullscreen)
	{
		win.internal.flags.fullscreen = true;
		win.internal.old_bounds = win.bounds;
		xdg_toplevel_set_fullscreen(win.src.xdgToplevel, null);
	}
	else
	{
		win.internal.flags.fullscreen = false;
		xdg_toplevel_unset_fullscreen(win.src.xdgToplevel);
	}
}

fn bool window_set_icon(GfxWindow* win, char* data, int w, int h, GfxFormat format, GfxWindowIconType type)
{
	if (!gfx::_gfx.iconManager || w != h) return false;

	if (win.src.icon != null) { xdg_toplevel_icon_v1_destroy(win.src.icon); win.src.icon = null; }

	GfxSurface*? surface = gfx::create_surface(data, w, h, format);
	if (catch surface) return false;
	defer surface.destroy();
	int minW = w < surface.size.x ? w : surface.size.x;
	int minH = h < surface.size.y ? h : surface.size.y;
	gfx::copy_image_data(surface.native.buffer, minW, minH, surface.native.format, surface.data, surface.format, null);

	win.src.icon = xdg_toplevel_icon_manager_v1_create_icon(gfx::_gfx.iconManager);
	surface.native.wlBuffer = wl_shm_pool_create_buffer(surface.native.pool, 0, w, h, w * 4, WL_SHM_FORMAT_ARGB8888);
	xdg_toplevel_icon_v1_add_buffer(win.src.icon, surface.native.wlBuffer, 1);
	xdg_toplevel_icon_manager_v1_set_icon(gfx::_gfx.iconManager, win.src.xdgToplevel, win.src.icon);

	return true;
}

fn void window_set_max_size(GfxWindow* win, GfxSize size)
{
	xdg_toplevel_set_max_size(win.src.xdgToplevel, ...size);
}

fn void window_set_min_size(GfxWindow* win, GfxSize size)
{
	xdg_toplevel_set_min_size(win.src.xdgToplevel, ...size);
}

fn void window_set_mouse_passthrough(GfxWindow* win, bool passthrough) {}

fn void window_set_mouse_platform(GfxWindow* win, GfxMouse* mouse)
{
	GfxSurface* surface = (GfxSurface*)mouse;
	win.src.usingCustomCursor = true;
	wl_surface_attach(win.src.customCursorSurface, surface.native.wlBuffer, 0, 0);
	wl_surface_damage(win.src.customCursorSurface, 0, 0, ...surface.size);
	wl_surface_commit(win.src.customCursorSurface);
}

fn void window_set_name(GfxWindow* win, String name)
{
	if (gfx::_gfx.compositor) @pool()
	{
		xdg_toplevel_set_title(win.src.xdgToplevel, name.zstr_copy(tmem));
	};
}

fn void window_set_opacity(GfxWindow* win, float opacity) {}

fn void window_set_raw_mouse_mode_platform(GfxWindow* win, bool state)
{
	if (!gfx::_gfx.relativePointerManager) return;

	if (!state)
	{
		if (gfx::_gfx.relativePointer) zwp_relative_pointer_v1_destroy(gfx::_gfx.relativePointer);
		gfx::_gfx.relativePointer = null;
		return;
	}
	if (gfx::_gfx.relativePointer) return;

	gfx::_gfx.relativePointer = zwp_relative_pointer_manager_v1_get_relative_pointer(gfx::_gfx.relativePointerManager, gfx::_gfx.wlPointer);
	static ZwpRelativePointerV1Listener listener = { .relativeMotion = &_rel_pointer_motion_cb };
	zwp_relative_pointer_v1_add_listener(gfx::_gfx.relativePointer, &listener, &gfx::_gfx);
}

fn void window_show_platform(GfxWindow* win)
{
	win.internal.flags.hide = false;
	wl_surface_commit(win.src.surface);
}

// Window - opengl

fn void? window_init_opengl_context_platform(GfxWindow* win, GfxGlContext* context, GfxGlHints* hints)
{
	egl::init_window_context(win, (GfxEglContext*)context, hints, gfx::_gfx.wlDisplay)!;
	win.src.backing = { .opengl };
	gfx::_window_set_opengl_swap_interval(win, 0);
}

fn void window_make_current_opengl_context(GfxWindow* win)
{
	egl::set_current_window(win);
}

fn void window_opengl_swap_buffers(GfxWindow* win)
{
	egl::swap_window_buffers(win);
}

fn void window_release_opengl_context_platform(GfxWindow* win, GfxGlContext* ctx)
{
	egl::destroy_context_in_window((GfxEglContext*)ctx, null);
	win.src.ctx.native = null;
}

fn void window_set_opengl_swap_interval(GfxWindow* win, int swapInterval)
{
	egl::set_swap_interval(win, swapInterval);
}

// Vulkan

fn VkSurfaceKHR? window_create_vulkan_surface(GfxWindow* win, VkInstance instance) @private
{
	VkSurfaceKHR surface;
	VkCreateSurfaceKHRProc proc = (VkCreateSurfaceKHRProc)gfx::_get_instance_proc_vulkan(instance, "vkCreateWaylandSurfaceKHR");
	if (!proc) return gfx::VULKAN_NOT_LOADED~;
	VkWaylandSurfaceCreateInfoKHR create_info = {
		.sType = gfx::VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR,
		.display = gfx::_gfx.wlDisplay,
		.surface = win.src.surface
	};
	if (proc(instance, &create_info, null, &surface)) return gfx::SURFACE_CREATION_FAILED~;
	return surface;
}

// Window WGPU

fn WGPUSurface? window_create_wgpu_surface(GfxWindow* window, WGPUInstance instance)
{
	WgpuSurfaceSourceWaylandSurface fromWl;
	fromWl.chain.next = null;
	fromWl.chain.sType = gfx::WGPU_STYPE_SURFACE_SOURCE_WAYLAND_SURFACE;
	fromWl.display = gfx::_gfx.wlDisplay;
	fromWl.surface = window.src.surface;
	WgpuSurfaceDescriptor surfaceDesc;
	surfaceDesc.nextInChain = (WgpuChainedStruct*)&fromWl;
	return gfx::wgpuInstanceCreateSurface(instance, &surfaceDesc) ?: gfx::SURFACE_CREATION_FAILED~;
}

// --- Clipboard

fn GfxDataTransfer? read_clipboard(Allocator allocator, GfxDataTransferType type)
{
	if (type != TEXT || !gfx::_gfx.unixClipboard || !gfx::_gfx.unixClipboard.text) return gfx::CLIPBOARD_DID_NOT_CONTAIN_TYPE~;

	return { .type = TEXT, .text = gfx::_gfx.unixClipboard.text.copy(allocator) };
}

fn void? write_clipboard(GfxDataTransfer* data)
{
	if (!gfx::_gfx.dataDeviceManager) return gfx::CLIPBOARD_COULD_NOT_BE_OPENED~;
	if (gfx::_gfx.unixClipboard)
	{
		free(gfx::_gfx.unixClipboard);
		gfx::_gfx.unixClipboard = null;
	}

	sz length = data.text.len;
	gfx::_gfx.unixClipboard = mem::new_with_padding(GfxDataTransfer, length + 1);
	char* data_ptr = (char*)gfx::_gfx.unixClipboard + GfxDataTransfer::size - 1;
	String s = (String)data_ptr[:length];
	s[..] = data.text[..];
	gfx::_gfx.unixClipboard.text = s;
	gfx::_gfx.unixClipboard.type = TEXT;

	GfxWindow* win = gfx::_gfx.kbOwner;
	if (win.src.dataSource)
	{
		wl_data_source_destroy(win.src.dataSource);
		win.src.dataSource = null;
	}

	win.src.dataSource = wl_data_device_manager_create_data_source(gfx::_gfx.dataDeviceManager);
	if (!win.src.dataSource) return gfx::CLIPBOARD_COULD_NOT_BE_OPENED~;

	static WlDataSourceListener listener = {
		.target = &_data_source_nothing_string_cb,
		.action = &_data_source_nothing_uint_cb,
		.dndDropPerformed = &_data_source_nothing_cb,
		.dndFinished = &_data_source_nothing_cb,
		.send = &_data_source_send_cb,
		.cancelled = &_data_source_cancelled_cb
	};

	wl_data_source_add_listener(win.src.dataSource, &listener, &gfx::_gfx);
	wl_data_source_offer(win.src.dataSource, "text/plain;charset=utf-8");
}

// --- GfxSurface

fn void? surface_init(GfxSurface* surface, void* data, int w, int h, GfxFormat format, GfxWindow* window)
{
	surface.data = data;
	surface.size = { w, h };
	surface.format = format;

	int size = w * h * 4;
	int fd = _create_shm_file((sz)size);
	if (fd < 0) return gfx::SURFACE_CREATION_FAILED~;
	defer catch surface.release();
	surface.native.pool = wl_shm_create_pool(gfx::_gfx.shm, fd, (int)size);
	surface.native.buffer = posix::mmap(null, (usz)size, posix::PROT_READ | posix::PROT_WRITE, posix::MAP_SHARED, fd, 0);
	surface.native.fd = fd;
	surface.native.format = BGRA8;

	if (surface.native.buffer == posix::MAP_FAILED)
	{
		surface.native.buffer = null;
		return gfx::SURFACE_CREATION_FAILED~;
	}
}

fn void surface_release(GfxSurface* surface)
{
	if (!surface.data) return;
	if (surface.native.pool) wl_shm_pool_destroy(surface.native.pool);
	if (surface.native.fd) libc::close(surface.native.fd);
	if (surface.native.buffer) posix::munmap(surface.native.buffer, (usz)(surface.size.x * surface.size.y * 4));
	*surface = {};
}

// --- Vulkan

fn bool vulkan_get_physical_device_presentation_support(VkInstance instance, VkPhysicalDevice physical_device, uint queue_family_index)
{
	GfxFn raw = gfx::_get_instance_proc_vulkan(instance, "vkGetPhysicalDeviceWaylandPresentationSupportKHR");
	if (raw == null) return false;
	uint out = ((VkGetPhysicalDeviceWaylandPresentationSupportKHRProc)raw)(physical_device, queue_family_index, gfx::_gfx.wlDisplay);
	return out != 0;
}

// --- Opengl

fn void* get_current_opengl_context() => egl::get_current_context();

fn void? load_opengl()
{
	// Wayland only ever renders through EGL; "OpenGL" here just means EGL is ready.
	return gfx::load_egl();
}

fn bool opengl_extension_is_supported_platform(String extension)
{
	return gfx::egl_extension_is_supported(extension);
}

// --- Keys

fn void init_keycodes_platform()
{
	// Wayland keycodes come directly from the evdev-based xkb keymap (key+8);
	// no separate keycode table walk is needed the way X11's is.
}

fn GfxKey physical_to_mapped_key(GfxKey key)
{
	XkbKeycode kc = key.to_api() + 8;
	XkbKeysym sym = xkb_state_key_get_one_sym(gfx::_gfx.xkbState, kc);
	if (sym < 256) return (GfxKey)sym;

	switch (sym)
	{
		case XKB_KEY_F1:            return F1;
		case XKB_KEY_F2:            return F2;
		case XKB_KEY_F3:            return F3;
		case XKB_KEY_F4:            return F4;
		case XKB_KEY_F5:            return F5;
		case XKB_KEY_F6:            return F6;
		case XKB_KEY_F7:            return F7;
		case XKB_KEY_F8:            return F8;
		case XKB_KEY_F9:            return F9;
		case XKB_KEY_F10:           return F10;
		case XKB_KEY_F11:           return F11;
		case XKB_KEY_F12:           return F12;
		case XKB_KEY_F13:           return F13;
		case XKB_KEY_F14:           return F14;
		case XKB_KEY_F15:           return F15;
		case XKB_KEY_F16:           return F16;
		case XKB_KEY_F17:           return F17;
		case XKB_KEY_F18:           return F18;
		case XKB_KEY_F19:           return F19;
		case XKB_KEY_F20:           return F20;
		case XKB_KEY_F21:           return F21;
		case XKB_KEY_F22:           return F22;
		case XKB_KEY_F23:           return F23;
		case XKB_KEY_F24:           return F24;
		case XKB_KEY_F25:           return F25;
		case XKB_KEY_SHIFT_L:       return SHIFT_L;
		case XKB_KEY_SHIFT_R:       return SHIFT_R;
		case XKB_KEY_CONTROL_L:     return CONTROL_L;
		case XKB_KEY_CONTROL_R:     return CONTROL_R;
		case XKB_KEY_ALT_L:         return ALT_L;
		case XKB_KEY_ALT_R:         return ALT_R;
		case XKB_KEY_SUPER_L:       return SUPER_L;
		case XKB_KEY_SUPER_R:       return SUPER_R;
		case XKB_KEY_CAPS_LOCK:     return CAPS_LOCK;
		case XKB_KEY_NUM_LOCK:      return NUM_LOCK;
		case XKB_KEY_SCROLL_LOCK:   return SCROLL_LOCK;
		case XKB_KEY_UP:            return UP;
		case XKB_KEY_DOWN:          return DOWN;
		case XKB_KEY_LEFT:          return LEFT;
		case XKB_KEY_RIGHT:         return RIGHT;
		case XKB_KEY_HOME:          return HOME;
		case XKB_KEY_END:           return END;
		case XKB_KEY_PAGE_UP:       return PAGE_UP;
		case XKB_KEY_PAGE_DOWN:     return PAGE_DOWN;
		case XKB_KEY_INSERT:        return INSERT;
		case XKB_KEY_MENU:          return MENU;
		case XKB_KEY_KP_ADD:        return PAD_PLUS;
		case XKB_KEY_KP_SUBTRACT:   return PAD_MINUS;
		case XKB_KEY_KP_MULTIPLY:   return PAD_MULTIPLY;
		case XKB_KEY_KP_DIVIDE:     return PAD_SLASH;
		case XKB_KEY_KP_EQUAL:      return PAD_EQUAL;
		case XKB_KEY_KP_ENTER:      return PAD_RETURN;
		case XKB_KEY_KP_DECIMAL:    return PAD_PERIOD;
		case XKB_KEY_KP_0:          return PAD0;
		case XKB_KEY_KP_1:          return PAD1;
		case XKB_KEY_KP_2:          return PAD2;
		case XKB_KEY_KP_3:          return PAD3;
		case XKB_KEY_KP_4:          return PAD4;
		case XKB_KEY_KP_5:          return PAD5;
		case XKB_KEY_KP_6:          return PAD6;
		case XKB_KEY_KP_7:          return PAD7;
		case XKB_KEY_KP_8:          return PAD8;
		case XKB_KEY_KP_9:          return PAD9;
		case XKB_KEY_PRINT:         return PRINT_SCREEN;
		case XKB_KEY_PAUSE:         return PAUSE;
		default: break;
	}
	return NULL;
}

fn void? _init_wayland(String class_name, GfxInitOption flags)
{
	gfx::_gfx.wlDisplay = wl_display_connect(null);
	if (!gfx::_gfx.wlDisplay) return gfx::WAYLAND_DISPLAY_NOT_LOADED~;

	gfx::_gfx.compositor = null;
	gfx::_gfx.registry = wl_display_get_registry(gfx::_gfx.wlDisplay);
	static WlRegistryListener listener = { .global = &_global_registry_cb, .globalRemove = &_global_registry_remove_cb };

	wl_registry_add_listener(gfx::_gfx.registry, &listener, &gfx::_gfx);
	wl_display_roundtrip(gfx::_gfx.wlDisplay);

	if (!gfx::_gfx.compositor) return gfx::WAYLAND_COMPOSITOR_NOT_FOUND~;

	if (!gfx::_gfx.wlCursorTheme)
	{
		gfx::_gfx.wlCursorTheme = wl_cursor_theme_load(null, 24, gfx::_gfx.shm);
		gfx::_gfx.cursorSurface = wl_compositor_create_surface(gfx::_gfx.compositor);
	}

	char[4] blk;
	gfx::_gfx.hiddenMouse = gfx::_create_mouse(&blk, { 1, 1 }, RGBA8) ?? null;

	static XdgWmBaseListener wmBaseListener = { .ping = &_xdg_wm_base_ping_cb };
	xdg_wm_base_add_listener(gfx::_gfx.xdgWmBase, &wmBaseListener, null);

	gfx::_gfx.xkbContext = xkb_context_new(XKB_CONTEXT_NO_FLAGS);

	static WlDataDeviceListener dataDeviceListener = {
		.dataOffer = &_data_device_data_offer_cb, .enter = &_data_device_nothing_enter_cb, .leave = &_data_device_nothing_cb,
		.motion = &_data_device_nothing_motion_cb, .drop = &_data_device_nothing_cb, .selection = &_data_device_selection_cb
	};

	if (gfx::_gfx.seat && gfx::_gfx.dataDeviceManager)
	{
		gfx::_gfx.dataDevice = wl_data_device_manager_get_data_device(gfx::_gfx.dataDeviceManager, gfx::_gfx.seat);
		wl_data_device_add_listener(gfx::_gfx.dataDevice, &dataDeviceListener, null);
	}
}

// --- Helpers

fn void _toggle_maximized(GfxWindow* win, bool maximized) @local
{
	win.src.maximized = maximized;
	if (maximized)
	{
		xdg_toplevel_set_maximized(win.src.xdgToplevel);
	}
	else
	{
		xdg_toplevel_unset_maximized(win.src.xdgToplevel);
	}
}

fn int _anonymous_shm_open() @local
{
	for (int i = 0; i < 100; i++) @pool()
	{
		ZString filename = (ZString)string::tformat("/c3gfx-wayland-%x\0\0", rand(0xFFFFFFF));
		int fd = libc::shm_open(filename, libc::O_RDWR | libc::O_CREAT | libc::O_EXCL, 384);
		if (fd >= 0)
		{
			libc::shm_unlink(filename);
			return fd;
		}
	};
	return -1;
}

fn XkbKeysym _compose_symbol(GfxContext* info, XkbKeysym sym) @local
{
	if (sym == XKB_KEY_NO_SYMBOL || !info.composeState) return sym;
	if (xkb_compose_state_feed(info.composeState, sym) != XKB_COMPOSE_FEED_ACCEPTED) return sym;
	switch (xkb_compose_state_get_status(info.composeState))
	{
		case XKB_COMPOSE_COMPOSED:
			return xkb_compose_state_get_one_sym(info.composeState);
		case XKB_COMPOSE_COMPOSING:
		case XKB_COMPOSE_CANCELLED:
			return XKB_KEY_NO_SYMBOL;
		default:
			return sym;	
	}
}

fn void _create_outputs(WlProxy* registry, uint id) @local => @pool()
{
	// wl_output name/description events require version 4+.
	// If wl_seat hasn't been bound yet (compositor announced wl_output first), fall back to
	// version 4 instead of calling wl_proxy_get_version(null), which would crash.
	uint fallbackVersion = 4;
	WlProxy* output = wl_registry_bind(registry, id, &wlOutputInterface, gfx::_gfx.seat ? wl_proxy_get_version(gfx::_gfx.seat) : fallbackVersion);
	if (!output) return;

	GfxMonitor mon;
	ZString s = string::tformat_zstr("monitor-%02d", gfx::_gfx.monitors.count);
	mem::copy(&mon.name, s, s.len() + 1);

	mon.scale = (Vec2)1;
	mon.pixelRatio = 1.0f;

	GfxMonitorNode* node = gfx::_monitor_add(&mon);
	
	*node = { .disconnected = true, .id = id, .output = output };

	wl_output_set_user_data(output, node);

	static WlOutputListener wl_output_listener =
	{
		.geometry = &_output_set_geometry_cb,
		.mode = &_output_handle_mode_cb,
		.done = &_output_handle_done_cb,
		.scale = &_output_set_scale_cb,
		.name = &_output_set_name_cb,
		.description = &_output_desc_cb
	};

	wl_output_add_listener(output, &wl_output_listener, node);

	if (!gfx::_gfx.xdgOutputManager) return;

	node.xdg_output = zxdg_output_manager_v1_get_xdg_output(gfx::_gfx.xdgOutputManager, node.output);
	
	static ZxdgOutputV1Listener xdg_output_listener = {
		.name = &_xdg_output_nothing_string_cb,
		.done = &_xdg_output_nothing_cb, 
		.description = &_xdg_output_nothing_string_cb,
		.logicalPosition = &_xdg_output_logical_pos_cb, 
		.logicalSize = &_xdg_output_logical_size_cb
	};

	zxdg_output_v1_add_listener(node.xdg_output, &xdg_output_listener, node);
}


fn int _create_shm_file(sz size) @local
{
	int fd = _anonymous_shm_open();
	if (fd < 0) return fd;
	if (libc::ftruncate(fd, size) < 0)
	{
		libc::close(fd);
		return -1;
	}
	return fd;
}

fn void _send_key_event(uint key) @local
{
	XkbKeysym* keysyms;
	if (xkb_state_key_get_syms(gfx::_gfx.xkbState, key + 8, &keysyms) == 1)
	{
		XkbKeysym keysym = _compose_symbol(&gfx::_gfx, keysyms[0]);
		uint codepoint = xkb_keysym_to_utf32(keysym);
		if (codepoint != 0) gfx::_key_char_callback(gfx::_gfx.kbOwner, codepoint);
	}
}

fn void _window_set_opaque(GfxWindow* win) @local
{
	WlProxy* region = wl_compositor_create_region(gfx::_gfx.compositor);
	if (!region) return;
	wl_region_add(region, 0, 0, win.width, win.height);
	wl_surface_set_opaque_region(win.src.surface, region);
	wl_region_destroy(region);
}

// wl callbacks
fn void _data_device_data_offer_cb(void* data, WlProxy* device, WlProxy* offer) @local
{
	static WlDataOfferListener listener = {
		.offer = &_data_offer_nothing_string_cb,
		.sourceActions = &_data_offer_nothing_uint_cb,
		.action = &_data_offer_nothing_uint_cb };
	wl_data_offer_add_listener(offer, &listener, null);
}

fn void _data_device_nothing_cb(void* data, WlProxy* device) @local {}
fn void _data_device_nothing_enter_cb(void* data, WlProxy* device, uint serial, WlProxy* surface, WlFixed x, WlFixed y, WlProxy* offer) @local {}
fn void _data_device_nothing_motion_cb(void* data, WlProxy* device, uint time, WlFixed x, WlFixed y) @local {}
fn void _data_offer_nothing_string_cb(void* data, WlProxy* offer, ZString s) @local {}
fn void _data_offer_nothing_uint_cb(void* data, WlProxy* offer, uint u) @local {}
fn void _data_source_nothing_cb(void* data, WlProxy* source) @local {}
fn void _data_source_nothing_string_cb(void* data, WlProxy* source, ZString s) @local {}
fn void _data_source_nothing_uint_cb(void* data, WlProxy* source, uint u) @local {}
fn void _xdg_output_nothing_cb(void* data, WlProxy* xdgOutput) @local {}
fn void _xdg_output_nothing_string_cb(void* data, WlProxy* xdgOutput, ZString s) @local {}

fn void _seat_name_cb(void* data, WlProxy* seat, ZString name) @local {}
fn void _surface_leave_cb(void* data, WlProxy* surface, WlProxy* output) @local {}
fn void _surface_pref_scale_cb(void* data, WlProxy* surface, int scale) @local {}
fn void _surface_pref_transform_cb(void* data, WlProxy* surface, uint transform) @local {}
fn void _unlocked_noop_cb(void* data, WlProxy* p) @local {}
fn void _output_desc_cb(void* data, WlProxy* output, ZString description) @local {}
fn void _shm_format_cb(void* data, WlProxy* shm, uint format) @local {}


fn void _data_source_cancelled_cb(void* data, WlProxy* source) @local
{
	GfxContext* info = data;
	if (info.kbOwner && info.kbOwner.src.dataSource == source) info.kbOwner.src.dataSource = null;
	wl_data_source_destroy(source);
}

fn void _data_source_send_cb(void* data, WlProxy* source, ZString mime_type, int fd) @local
{
	if (mime_type && mime_type.str_view().contains("text/plain"))
	{
		String s = gfx::_gfx.unixClipboard.text;
		libc::write(fd, s, s.len);
	}
	libc::close(fd);
}

fn void _global_registry_cb(void* data, WlProxy* registry, uint id, ZString iface_str, uint version) @local
{
	GfxContext* info = data;
	String iface = iface_str.str_view();
	switch (iface)
	{
		case "wl_compositor":
			info.compositor = wl_registry_bind(registry, id, &wlCompositorInterface, 4);
		case "xdg_wm_base":
			info.xdgWmBase = wl_registry_bind(registry, id, &xdgWmBaseInterface, 1);
		case zxdgDecorationManagerV1Interface.name.str_view():
			info.decorationManager = wl_registry_bind(registry, id, &zxdgDecorationManagerV1Interface, 1);
		case zwpPointerConstraintsV1Interface.name.str_view():
			info.constraint_manager = wl_registry_bind(registry, id, &zwpPointerConstraintsV1Interface, 1);
		case zwpRelativePointerManagerV1Interface.name.str_view():
			info.relativePointerManager = wl_registry_bind(registry, id, &zwpRelativePointerManagerV1Interface, 1);
		case xdgToplevelIconManagerV1Interface.name.str_view():
			info.iconManager = wl_registry_bind(registry, id, &xdgToplevelIconManagerV1Interface, 1);
		case "wl_shm":
			info.shm = wl_registry_bind(registry, id, &wlShmInterface, 1);
			static WlShmListener listener = { .format = &_shm_format_cb };
			wl_shm_add_listener(info.shm, &listener, info);
		case "wl_seat":
			info.seat = wl_registry_bind(registry, id, &wlSeatInterface, version < 4 ? 3 : 4);
			static WlSeatListener listener = { .capabilities = &_seat_capabilities_cb, .name = &_seat_name_cb };
			wl_seat_add_listener(info.seat, &listener, info);
		case zxdgOutputManagerV1Interface.name.str_view():
			info.xdgOutputManager = wl_registry_bind(registry, id, &zxdgOutputManagerV1Interface, 1);
		case "wl_output":
			_create_outputs(registry, id);
		case wpPointerWarpV1Interface.name.str_view():
			info.wp_pointer_warp = wl_registry_bind(registry, id, &wpPointerWarpV1Interface, 1);
		case "wl_data_device_manager":
			info.dataDeviceManager = wl_registry_bind(registry, id, &wlDataDeviceManagerInterface, 1);
	}
}

fn void _global_registry_remove_cb(void* data, WlProxy* registry, uint id) @local
{
	GfxContext* info = data;
	GfxMonitorNode* prev = null;
	GfxMonitorNode* node = info.monitors.list.head;
	while (node)
	{
		if (node.id == id)
		{
			gfx::_monitor_callback(gfx::_gfx.root, &node.mon, false);
			gfx::_remove_monitor(node, prev);
			return;
		}
		prev = node;
		node = node.next;
	}
}

fn void _keyboard_enter_cb(void* data, WlProxy* keyboard, uint serial, WlProxy* surface, WlArray* keys) @local
{
	GfxContext* info = data;
	GfxWindow* win = (GfxWindow*)wl_surface_get_user_data(surface);
	info.kbOwner = win;

	if (info.dataDevice && win.src.dataSource) wl_data_device_set_selection(info.dataDevice, win.src.dataSource, serial);
	if (win.src.minimized) win.src.minimized = false;
	gfx::_window_focus_callback(win, true);
}

fn void _keyboard_key_cb(void* data, WlProxy* keyboard, uint serial, uint time, uint key, uint state) @local
{
	GfxContext* info = data;
	if (!info.kbOwner) return;

	GfxWindow* win = info.kbOwner;
	GfxKey gfx_key = gfx::key_from_api(key + 8);

	gfx::_key_update_key_mods(win, xkb_keymap_mod_get_index(info.keymap, "Lock") != 0, xkb_keymap_mod_get_index(info.keymap, "Mod2") != 0, xkb_keymap_mod_get_index(info.keymap, "ScrollLock") != 0);
	gfx::_key_callback(win, gfx_key, win.internal.mod, gfx::is_key_down(gfx_key) && state, !!state);

	info.lastKey = state ? key : 0;
	info.lastKeyTime = clock::now() + time::ms(gfx::_gfx.wlRepeatInfoDelay);
	if (state) _send_key_event(gfx::_gfx.lastKey);
}

fn void _keyboard_keymap_cb(void* data, WlProxy* keyboard, uint format, int fd, uint size) @local => @pool()
{
	GfxContext* info = data;
	ZString keymapString = (ZString)posix::mmap(null, size, posix::PROT_READ, posix::MAP_SHARED, fd, 0);
	xkb_keymap_unref(info.keymap);
	info.keymap = xkb_keymap_new_from_string(info.xkbContext, keymapString, XKB_KEYMAP_FORMAT_TEXT_V1, XKB_KEYMAP_COMPILE_NO_FLAGS);
	posix::munmap(keymapString, size);
	libc::close(fd);
	xkb_state_unref(info.xkbState);
	info.xkbState = xkb_state_new(info.keymap);

	String locale = env::tget_var("LC_ALL") ?? env::tget_var("LC_CTYPE") ?? env::tget_var("LANG") ?? "C";

	XkbComposeTable* composeTable = xkb_compose_table_new_from_locale(info.xkbContext, (ZString)locale, XKB_COMPOSE_COMPILE_NO_FLAGS);
	if (composeTable)
	{
		info.composeState = xkb_compose_state_new(composeTable, XKB_COMPOSE_STATE_NO_FLAGS);
		xkb_compose_table_unref(composeTable);
	}
}

fn void _keyboard_leave_cb(void* data, WlProxy* keyboard, uint serial, WlProxy* surface) @local
{
	GfxContext* info = data;
	GfxWindow* win = (GfxWindow*)wl_surface_get_user_data(surface);
	if (info.kbOwner == win) info.kbOwner = null;
	gfx::_window_focus_callback(win, false);
}

fn void _keyboard_mods_cb(void* data, WlProxy* keyboard, uint serial, uint modsDepressed, uint modsLatched, uint modsLocked, uint group) @local
{
	GfxContext* info = data;
	xkb_state_update_mask(info.xkbState, modsDepressed, modsLatched, modsLocked, 0, 0, group);
}

fn void _keyboard_repeat_info_cb(void* data, WlProxy* keyboard, int rate, int delay) @local
{
	gfx::_gfx.wlRepeatInfoRate = rate;
	gfx::_gfx.wlRepeatInfoDelay = delay;
}


fn void _output_handle_done_cb(void* data, WlProxy* output) @local
{
	GfxMonitorNode* node = (GfxMonitorNode*)data;
	GfxMonitor* mon = &node.mon;

	if (mon.phys_size.x <= 0 || mon.phys_size.y <= 0)
	{
		mon.phys_size = (Vec2)mon.mode.size / 96.0f;
	}

	if (!node.disconnected) return;
	node.disconnected = false;
	gfx::_monitor_callback(gfx::_gfx.root, mon, true);
}

fn void _output_handle_mode_cb(void* data, WlProxy* output, uint flags, int width, int height, int refresh) @local
{
	GfxMonitorNode* node = (GfxMonitorNode*)data;
	GfxMonitor* mon = &node.mon;

	GfxMonitorMode mode;
	mode.size = { width, height };
	mode.refresh_rate = refresh / 1000.0f;
	mode.src = output;

	node.mode_count += 1;
	GfxMonitorMode* modes = malloc(node.mode_count * GfxMonitorMode::size);
	if (node.mode_count > 1)
	{
		modes[:node.mode_count - 1] = node.modes[:node.mode_count - 1];
		free(node.modes);
	}
	modes[node.mode_count - 1] = mode;
	node.modes = modes;

	if (flags & WL_OUTPUT_MODE_CURRENT) mon.mode = mode;
}

fn void _output_set_geometry_cb(void* data, WlProxy* output, int x, int y, int physW, int physH, int subpixel, ZString make, ZString model, int transform) @local
{
	GfxMonitor* mon = &((GfxMonitorNode*)data).mon;
	mon.origin = { x, y };
	mon.phys_size = { physW / 25.4f, physH / 25.4f };
}

fn void _output_set_name_cb(void* data, WlProxy* output, ZString name) @local
{
	GfxMonitor* mon = &((GfxMonitorNode*)data).mon;
	sz n = name.len();
	if (n > 127) n = 127;
	mem::copy(&mon.name, name, n);
	mon.name[n] = 0;
}

fn void _output_set_scale_cb(void* data, WlProxy* output, int factor) @local
{
	GfxMonitor* mon = &((GfxMonitorNode*)data).mon;
	mon.scale = (Vec2)factor;
}

fn void _pointer_axis_cb(void* data, WlProxy* pointer, uint time, uint axis, WlFixed value) @local
{
	GfxContext* info = data;
	GfxWindow* win = info.mouseOwner;
	if (!win) return;
	Vec2 scroll = {};
	if (!(win.internal.enabled_events.mouse_scroll)) return;
	if (axis == WL_POINTER_AXIS_HORIZONTAL_SCROLL)
	{
		scroll.x = (float)(-value.to_double() / 10.0);
	}
	else if (axis == WL_POINTER_AXIS_VERTICAL_SCROLL)
	{
		scroll.y = (float)(-value.to_double() / 10.0);
	}
	gfx::_mouse_scroll_callback(win, scroll);
}

fn void _pointer_button_cb(void* data, WlProxy* pointer, uint serial, uint time, uint button, uint state) @local
{
	GfxContext* info = data;
	GfxWindow* win = info.mouseOwner;
	if (!win) return;
	uint b = button - 0x110;
	switch (b)
	{
		case 1: b = 2;
		case 2: b = 1;
	}
	gfx::_mouse_button_callback(win, (GfxMouseButton)b, !!state);
}

fn void _pointer_enter_cb(void* data, WlProxy* pointer, uint serial, WlProxy* surface, WlFixed surface_x, WlFixed surface_y) @local
{
	GfxContext* info = data;
	GfxWindow* win = (GfxWindow*)wl_surface_get_user_data(surface);
	info.mouseEnterSerial = serial;
	win.internal.mouse_inside = true;
	info.window_state.mouse_enter = true;
	info.mouseOwner = win;

	if (win.src.usingCustomCursor)
	{
		wl_pointer_set_cursor(pointer, serial, win.src.customCursorSurface, 0, 0);
	}
	else
	{
		win.set_mouse_default();
	}

	int x = (int)surface_x.to_double();
	int y = (int)surface_y.to_double();
	gfx::_mouse_notify_callback(win, { x, y }, true);
}

fn void _pointer_leave_cb(void* data, WlProxy* pointer, uint serial, WlProxy* surface) @local
{
	GfxWindow* win = (GfxWindow*)wl_surface_get_user_data(surface);
	GfxContext* info = data;
	if (info.mouseOwner == win) info.mouseOwner = null;
	gfx::_mouse_notify_callback(win, win.internal.last_mouse, false);
}

fn void _pointer_locked_cb(void* data, WlProxy* locked_pointer) @local
{
	GfxContext* info = data;
	wl_pointer_set_cursor(info.wlPointer, info.mouseEnterSerial, null, 0, 0);
}

fn void _pointer_motion_cb(void* data, WlProxy* pointer, uint time, WlFixed x, WlFixed y) @local
{
	GfxContext* info = data;
	GfxWindow* win = info.mouseOwner;
	if (!win) return;
	int cx = (int)x.to_double();
	int cy = (int)y.to_double();
	gfx::_mouse_motion_callback(win, { cx, cy });
}

fn void _rel_pointer_motion_cb(void* data, WlProxy* relPointer, uint timeHi, uint timeLo, WlFixed dx, WlFixed dy, WlFixed dxUnaccel, WlFixed dyUnaccel) @local
{
	GfxContext* info = data;
	GfxWindow* win = info.mouseOwner;
	if (!win) return;
	gfx::_raw_motion_callback(win, { (float)dx.to_double(), (float)dy.to_double() });
}

fn void _seat_capabilities_cb(void* data, WlProxy* seat, uint capabilities) @local
{
	GfxContext* info = data;

	if (capabilities & WL_SEAT_CAPABILITY_POINTER && !info.wlPointer)
	{
		info.wlPointer = wl_seat_get_pointer(seat);
		static WlPointerListener listener = {
			.enter = &_pointer_enter_cb,
			.leave = &_pointer_leave_cb,
			.motion = &_pointer_motion_cb,
			.button = &_pointer_button_cb,
			.axis = &_pointer_axis_cb
		};
		wl_pointer_add_listener(info.wlPointer, &listener, info);
	}
	if (capabilities & WL_SEAT_CAPABILITY_KEYBOARD && !info.wlKeyboard)
	{
		info.wlKeyboard = wl_seat_get_keyboard(seat);
		static WlKeyboardListener listener = {
			.keymap = &_keyboard_keymap_cb,
			.enter = &_keyboard_enter_cb,
			.leave = &_keyboard_leave_cb,
			.key = &_keyboard_key_cb,
			.modifiers = &_keyboard_mods_cb,
			.repeatInfo = &_keyboard_repeat_info_cb
		};
		wl_keyboard_add_listener(info.wlKeyboard, &listener, info);
	}
	if (!(capabilities & WL_SEAT_CAPABILITY_POINTER) && info.wlPointer) wl_pointer_release(info.wlPointer);
	if (!(capabilities & WL_SEAT_CAPABILITY_KEYBOARD) && info.wlKeyboard) wl_keyboard_release(info.wlKeyboard);
}

fn void _surface_enter_cb(void* data, WlProxy* surface, WlProxy* output) @local
{
	GfxWindow* win = (GfxWindow*)data;
	GfxMonitorNode* node = (GfxMonitorNode*)wl_output_get_user_data(output);
	if (!node) return;
	win.src.activeMonitor = node;
}

fn void _xdg_dec_conf_cb(void* data, WlProxy* decoration, uint mode) @local
{
	GfxWindow* win = (GfxWindow*)data;
	if (mode != win.src.decorationMode) win.src.decorationMode = mode;
}


fn void _xdg_surface_conf_cb(void* data, WlProxy* xdgSurface, uint serial) @local
{
	xdg_surface_ack_configure(xdgSurface, serial);
	GfxWindow* win = (GfxWindow*)data ?: gfx::_gfx.kbOwner;
	if (!win) return;

	if (win.src.activated != win.src.pendingActivated) win.src.activated = win.src.pendingActivated;

	if (win.src.maximized != win.src.pendingMaximized)
	{
		_toggle_maximized(win, win.src.pendingMaximized);
		gfx::_window_check_mode(win);
	}

	if (win.src.resizing)
	{
		gfx::_window_resized_callback(win, win.size);
		win.resize(win.size);
		if (!(win.internal.flags.transparent)) _window_set_opaque(win);
	}

	win.src.configured = true;
}

fn void _xdg_toplevel_close_cb(void* data, WlProxy* toplevel) @local
{
	GfxWindow* win = (GfxWindow*)data;
	if (!win.internal.should_close) gfx::_window_close_callback(win);
}

fn void _xdg_toplevel_conf_cb(void* data, WlProxy* toplevel, int width, int height, WlArray* states) @local
{
	GfxWindow* win = (GfxWindow*)data;
	win.src.pendingActivated = false;
	win.src.pendingMaximized = false;
	win.src.resizing = false;

	if (states)
	{
		uint* state = (uint*)states.data;
		sz count = states.size / uint::size;
		for (sz i = 0; i < count; i++)
		{
			switch (state[i])
			{
				case 4: // XDG_TOPLEVEL_STATE_ACTIVATED
					win.src.pendingActivated = true;
				case 1: // XDG_TOPLEVEL_STATE_MAXIMIZED
					win.src.pendingMaximized = true;
				default:
					break;
			}
		}
	}

	if (width != 0 && height != 0 && (win.width != width || win.height != height))
	{
		win.src.resizing = true;
		win.size = { width, height };
		win.src.size = win.size;
	}
}

fn void _xdg_wm_base_ping_cb(void* data, WlProxy* wmBase, uint serial) @local
{
	xdg_wm_base_pong(wmBase, serial);
}

fn void _xdg_output_logical_pos_cb(void* data, WlProxy* xdgOutput, int x, int y) @local
{
	GfxMonitor* mon = &((GfxMonitorNode*)data).mon;
	mon.origin = { x, y };
}

fn void _xdg_output_logical_size_cb(void* data, WlProxy* xdgOutput, int width, int height) @local
{
	GfxMonitor* mon = &((GfxMonitorNode*)data).mon;
	float monFloatWidth = (float)mon.mode.size.x;
	float scaleX = monFloatWidth / (float)width;
	float dpi = scaleX * 96.0f;
	mon.pixelRatio = dpi >= 192.0f ? 2.0f : 1.0f;
	mon.mode.size = { width, height };
}

fn void _data_device_selection_cb(void* data, WlProxy* device, WlProxy* offer) @local
{
	if (!offer) return;

	int[2] pfds;
	posix::pipe(&pfds);
	wl_data_offer_receive(offer, "text/plain", pfds[1]);
	libc::close(pfds[1]);
	wl_display_roundtrip(gfx::_gfx.wlDisplay);

	char[1024] buf;
	sz n = libc::read(pfds[0], &buf, 1024);
	if (n <= 0)
	{
		libc::close(pfds[0]);
		wl_data_offer_destroy(offer);
		return;
	}

	sz length = (sz)n;
	if (buf[n - 1] != 0) length += 1;

	if (gfx::_gfx.unixClipboard != null) free(gfx::_gfx.unixClipboard);
	gfx::_gfx.unixClipboard = mem::new_with_padding(GfxDataTransfer, length);
	char* data_ptr = (char*)gfx::_gfx.unixClipboard + GfxDataTransfer::size - 1;
	String s = (String)data_ptr[:length - 1];
	data_ptr[:s.len] = buf[:s.len];
	gfx::_gfx.unixClipboard.text = s;
	gfx::_gfx.unixClipboard.type = TEXT;

	libc::close(pfds[0]);
	wl_data_offer_destroy(offer);
}
// ---



// -- wgpu
struct WgpuSurfaceSourceWaylandSurface
{
	WgpuChainedStruct chain;
	WlDisplay* display;
	WlProxy* surface;
}

// --- Vulkan


alias VkGetPhysicalDeviceWaylandPresentationSupportKHRProc = fn uint(VkPhysicalDevice, uint, WlDisplay*);


struct VkWaylandSurfaceCreateInfoKHR
{
	int sType;
	void* pNext;
	uint flags;
	WlDisplay* display;
	WlProxy* surface;
}
