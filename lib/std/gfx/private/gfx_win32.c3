module std::gfx::win32 @feat(WIN32) @private;
import std::os::win32, std::collections, std::time, std::math, std::os;
import std::gfx::egl @public;

// --- Gfx

fn void? init_platform(String class_name, GfxInitOption flags) => @pool()
{
	win32::setProcessDPIAware();
	win32::timeBeginPeriod(1);

	Win32_HMODULE instance = gfx::_gfx.instance = win32::getModuleHandleW(null);

	{
		assert(class_name.len < 254);
		WString str = class_name.to_temp_wstring()!;
		sz len = str.len();
		mem::copy(&gfx::_gfx.wideClassName, str, len);
		gfx::_gfx.wideClassName[len] = 0;
	};

	gfx::_gfx.wndClass = {
		.lpszClassName = (WString)&gfx::_gfx.wideClassName,
		.hInstance = instance,
		.hCursor = win32::loadCursorA(null, (Win32_LPCSTR)win32::IDC_ARROW),
		.lpfnWndProc = &gfx::_wnd_proc,
		.cbClsExtra = sz::size,
		.hIcon = (Win32_HICON)win32::loadImageA(instance, "C3GFX", 1 /* IMAGE_ICON */, 0, 0, 0x00000040 | 0x00008000 /* LR_DEFAULTSIZE|LR_SHARED */)
	};

	if (!gfx::_gfx.wndClass.hIcon)
	{
		gfx::_gfx.wndClass.hIcon = (Win32_HICON)win32::loadImageA(null, (Win32_LPCSTR)win32::IDI_APPLICATION, 1, 0, 0, 0x00000040 | 0x00008000);
	}

	win32::registerClassW(&gfx::_gfx.wndClass);
	gfx::_gfx.helper_window = win32::createWindowExW(0, gfx::_gfx.wndClass.lpszClassName, null, 0, 0, 0, 0, 0, null, null, instance, null);

	char[4] blk;
	gfx::_gfx.hiddenMouse = win32::create_mouse(&blk, { 1, 1 }, RGBA8) ?? null;
}

fn void destroy_platform()
{
	win32::timeEndPeriod(1);
	if (gfx::_gfx.helper_window) win32::destroyWindow(gfx::_gfx.helper_window);
	if (gfx::_gfx.instance) win32::unregisterClassW(gfx::_gfx.wndClass.lpszClassName, gfx::_gfx.instance);
	if (gfx::_gfx.hiddenMouse != null) free_mouse(gfx::_gfx.hiddenMouse);
}

fn void poll_events_platform()
{
	gfx::_reset_prev_state();
	Win32_MSG msg;
	while (win32::peekMessageA(&msg, null, 0, 0, win32::PM_REMOVE))
	{
		win32::translateMessage(&msg);
		win32::dispatchMessageA(&msg);
	}
}

fn void stop_check_events()
{
	win32::postMessageW(gfx::_gfx.helper_window, 0, 0, 0);
}

fn void wait_for_event(Duration duration)
{
	if (duration < time::DURATION_ZERO) duration = time::DURATION_ZERO;
	long duration_ms = duration.to_ms();
	Win32_DWORD ms = duration_ms > Win32_DWORD::max ? win32::INFINITE : (uint)duration_ms;
	win32::msgWaitForMultipleObjects(0, null, 0, ms, 0x04FF /* QS_ALLINPUT */);
}

// --- GfxMonitor functions

fn GfxGammaRamp? monitor_get_gamma_ramp(GfxMonitor* monitor, Allocator a)
{
	ushort[] colors = mem::alloc::alloc_array(a, ushort, 768);
	Win32_HDC dc = win32::createDCW(gfx::WIDE_DISPLAY_NAME, (WString)&monitor.node.adapter_name, null, null);
	if (!dc) return gfx::CANNOT_RETRIEVE_GAMMA~;
	defer win32::deleteDC(dc);
	if (!win32::getDeviceGammaRamp(dc, colors)) return gfx::CANNOT_RETRIEVE_GAMMA~;
	return { colors[:256], colors[256:256], colors[512:256], a };
}

fn GfxMonitorMode[] monitor_get_modes(GfxMonitor* monitor)
{
	uint mode_index = 0;
	List{GfxMonitorMode} modes;
	for OUTER: (;;)
	{
		Win32_DEVMODEW dm = { .dmSize = (ushort)Win32_DEVMODEW::size };
		if (win32::enumDisplaySettingsW((WString)&monitor.node.adapter_name, mode_index, &dm) == 0) break;
		if (win32::changeDisplaySettingsExW((WString)&monitor.node.adapter_name, &dm, null, win32::CDS_TEST, null) != win32::DISP_CHANGE_SUCCESSFUL) continue;
		mode_index++;
		if (dm.dmBitsPerPel < 15) continue;
		GfxMonitorMode mode;
		gfx::_win32_get_mode(&dm, &mode);
		foreach (&mode2 : modes)
		{
			if (mode2.equals_request(&mode, ALL)) continue OUTER;
		}
		modes.push(mode);
	}
	return modes.array_view();
}

fn IntRect monitor_get_workarea(GfxMonitor* monitor)
{
	Win32_MONITORINFOEXW mi;
	mi.cbSize = Win32_MONITORINFOEXW::size;
	win32::getMonitorInfoW(monitor.node.monitor, &mi);
	return { mi.rcWork.left, mi.rcWork.top, mi.rcWork.right - mi.rcWork.left, mi.rcWork.bottom - mi.rcWork.top };
}

fn void monitor_node_free(GfxMonitorNode* node)
{}

fn void? monitor_request_mode(GfxMonitor* mon, GfxMonitorMode* mode, GfxMonitorModeRequest request)
{
	Win32_DEVMODEW dm = { .dmSize = Win32_DEVMODEW::size };
	uint index = 0;
	for (;;)
	{
		if (win32::enumDisplaySettingsW((ushort*)&mon.node.adapter_name, index, &dm) == 0) break;
		index += 1;

		if (request & SCALE) { dm.dmFields |= win32::DM_PELSWIDTH | win32::DM_PELSHEIGHT; dm.dmPelsWidth = (uint)mode.size.x; dm.dmPelsHeight = (uint)mode.size.y; }
		if (request & REFRESH) { dm.dmFields |= win32::DM_DISPLAYFREQUENCY; dm.dmDisplayFrequency = (uint)mode.refresh_rate; }
		if (request & RGB) { dm.dmFields |= win32::DM_BITSPERPEL; dm.dmBitsPerPel = (uint)(mode.rgb.sum()); }

		if (win32::changeDisplaySettingsExW((ushort*)&mon.node.adapter_name, &dm, null, win32::CDS_TEST, null) == win32::DISP_CHANGE_SUCCESSFUL)
		{
			if (win32::changeDisplaySettingsExW((ushort*)&mon.node.adapter_name, &dm, null, win32::CDS_UPDATEREGISTRY, null) == win32::DISP_CHANGE_SUCCESSFUL)
			{
				gfx::_win32_get_mode(&dm, &mon.mode);
				return;
			}
			return gfx::MONITOR_MODE_CHANGE_FAILED~;
		}
	}
	return gfx::MONITOR_MODE_CHANGE_FAILED~;
}

fn void? monitor_set_gamma(GfxMonitor* monitor, float gamma)
{
	ushort[768] g;
	for (int i = 0; i < 256; i++)
	{
		ushort v = (ushort)gfx::_gamma_for_index(gamma, i, 256);
		g[i] = g[i + 256] = g[i + 512] = v;
	}
	return gfx::_gamma_set(monitor, &g);
}

fn void? monitor_set_gamma_ramp(GfxMonitor* monitor, GfxGammaRamp ramp)
{
	if (ramp.red.len != 256) return gfx::GAMMA_COULD_NOT_BE_SET~;
	return gfx::_gamma_set(monitor, ramp.red);
}

fn void? monitor_set_mode(GfxMonitor* mon, GfxMonitorMode* mode)
{
	Win32_DEVMODEW dm = {
		.dmSize = Win32_DEVMODEW::size,
		.dmFields = win32::DM_PELSWIDTH | win32::DM_PELSHEIGHT | win32::DM_DISPLAYFREQUENCY | win32::DM_BITSPERPEL,
		.dmPelsWidth = (uint)mode.size.x,
		.dmPelsHeight = (uint)mode.size.y,
		.dmDisplayFrequency = (uint)mode.refresh_rate,
		.dmBitsPerPel = (uint)(mode.rgb.sum())
	};
	if (win32::changeDisplaySettingsExW((WString)&mon.node.adapter_name, &dm, null, win32::CDS_TEST, null) != win32::DISP_CHANGE_SUCCESSFUL)
	{
		return gfx::MONITOR_MODE_CHANGE_FAILED~;
	}
	if (win32::changeDisplaySettingsExW((WString)&mon.node.adapter_name, &dm, null, win32::CDS_UPDATEREGISTRY, null) != win32::DISP_CHANGE_SUCCESSFUL)
	{
		return gfx::MONITOR_MODE_CHANGE_FAILED~;
	}
	gfx::_win32_get_mode(&dm, &mon.mode);
}

fn void poll_monitors()
{
	for (GfxMonitorNode* node = gfx::_gfx.monitors.list.head; node != null; node = node.next) node.disconnected = true;

	for (uint adapterNum = 0;; adapterNum++)
	{
		Win32_DISPLAY_DEVICEW adapter = { .cb = Win32_DISPLAY_DEVICEW::size };
		if (!win32::enumDisplayDevicesW(null, adapterNum, &adapter, 0)) break;
		if (!(adapter.stateFlags & win32::DISPLAY_DEVICE_ACTIVE)) continue;

		Win32_DWORD device_num;
		for (device_num = 0;; device_num++)
		{
			Win32_DISPLAY_DEVICEW dd = { .cb = Win32_DISPLAY_DEVICEW::size };
			if (win32::enumDisplayDevicesW((WString)&adapter.deviceName, device_num, &dd, 0) == 0) break;
			if (!(dd.stateFlags & win32::DISPLAY_DEVICE_ACTIVE)) continue;

			GfxMonitorNode* node;
			for (node = gfx::_gfx.monitors.list.head; node != null; node = node.next)
			{
				sz i;
				for (i = 0; dd.deviceName[i] != 0 && node.device_name[i] != 0 && dd.deviceName[i] == node.device_name[i]; i++) {}
				if (node.disconnected == true && dd.deviceName[i] == 0 && node.device_name[i] == 0)
				{
					node.disconnected = false;
					win32::enumDisplayMonitors(null, null, (void*)&gfx::_get_monitor_handle_callback, (iptr)(void*)&node.mon);
					break;
				}
			}
			if (node) continue;
			gfx::_create_monitor(&adapter, &dd);
		}

		if (!device_num)
		{
			GfxMonitorNode* node;
			for (node = gfx::_gfx.monitors.list.head; node != null; node = node.next)
			{
				sz i;
				for (i = 0; adapter.deviceName[i] != 0 && node.adapter_name[i] != 0 && adapter.deviceName[i] == node.adapter_name[i]; i++)
				{}
				if (node.disconnected == true && adapter.deviceName[i] == 0 && node.adapter_name[i] == 0)
				{
					node.disconnected = false;
					break;
				}
			}
			if (node) continue;
			gfx::_create_monitor(&adapter, null);
		}
	}
	gfx::refresh_monitors();
}

// --- GfxMouse and mouse related

fn GfxMouse*? create_mouse(char* data, GfxSize size, GfxFormat format)
{
	return (GfxMouse*)_load_handle_image(data, ...size, format, 0);
}

fn GfxMouse*? create_standard_mouse(GfxMouseIcon mouse)
{
	iptr mouseIcon;
	switch (mouse)
	{
		case NORMAL:
		case ARROW:          mouseIcon = win32::IDC_ARROW;
		case IBEAM:          mouseIcon = win32::IDC_IBEAM;
		case WAIT:           mouseIcon = win32::IDC_WAIT;
		case CROSSHAIR:      mouseIcon = win32::IDC_CROSS;
		case PROGRESS:       mouseIcon = win32::IDC_APPSTARTING;
		case RESIZE_NW:
		case RESIZE_SE:
		case RESIZE_NWSE:    mouseIcon = win32::IDC_SIZENWSE;
		case RESIZE_NE:
		case RESIZE_SW:
		case RESIZE_NESW:    mouseIcon = win32::IDC_SIZENESW;
		case RESIZE_E:
		case RESIZE_W:
		case RESIZE_EW:      mouseIcon = win32::IDC_SIZEWE;
		case RESIZE_N:
		case RESIZE_S:
		case RESIZE_NS:      mouseIcon = win32::IDC_SIZENS;
		case RESIZE_ALL:     mouseIcon = win32::IDC_SIZEALL;
		case NOT_ALLOWED:    mouseIcon = win32::IDC_NO;
		case POINTING_HAND:  mouseIcon = win32::IDC_HAND;
		default: return gfx::MOUSE_NOT_SUPPORTED~;
	}
	void* cursor = win32::loadCursorA(null, (Win32_LPCSTR)mouseIcon);
	if (!cursor) return gfx::MOUSE_NOT_SUPPORTED~;
	return cursor;
}

fn void free_mouse(GfxMouse* mouse)
{
	win32::destroyCursor(mouse);
}

fn GfxPoint get_global_mouse()
{
	Win32_POINT p;
	win32::getCursorPos(&p);
	return { ...p };
}

// --- GfxWindow

fn void window_blit_surface(GfxWindow* win, GfxSurface* surface)
{
	GfxPoint blit = math::min(win.size, surface.size);
	gfx::copy_image_data(surface.native.buffer, surface.size.x, blit.y, surface.native.format, surface.data, surface.format, surface.convert_fn);
	win32::bitBlt(win.src.hdc, 0, 0, ...blit, surface.native.hdc_mem, 0, 0, win32::SRCCOPY);
}

fn void? window_create_platform(GfxWindow* win, String name, GfxWindowFlags flags) => @pool()
{
	if (!name.len) name = " ";
	win.src.hIconSmall = null; win.src.hIconBig = null;
	win.src.max_size = {};
	win.src.min_size = {};
	win.src.aspectRatioW = 0; win.src.aspectRatioH = 0;

	uint window_style = win32::WS_CLIPSIBLINGS | win32::WS_CLIPCHILDREN;
	if (!flags.no_border)
	{
		window_style |= win32::WS_CAPTION | win32::WS_SYSMENU | win32::WS_BORDER | win32::WS_MINIMIZEBOX;
		if (!flags.no_resize) window_style |= win32::WS_SIZEBOX | win32::WS_MAXIMIZEBOX;
	}
	else
	{
		window_style |= win32::WS_POPUP | win32::WS_VISIBLE | win32::WS_SYSMENU;
	}

	Win32_RECT rect = {0, 0, win.width, win.height};
	uint style = gfx::_winapi_window_get_style(flags);
	uint exStyle = gfx::_winapi_window_get_style_ex(win, flags);
	win32::adjustWindowRectEx(&rect, style, 0, exStyle);

	win.src.window = win32::createWindowExW(0, gfx::_gfx.wndClass.lpszClassName, name.to_wstring(tmem)!!, window_style, win.x + rect.left, win.y + rect.top, rect.right - rect.left, rect.bottom - rect.top, null, null, gfx::_gfx.instance, null);
	win32::setPropW(win.src.window, gfx::RENAME_PROP_NAME, win);
	win.resize(win.size);

	if (flags.allow_dnd)
	{
		win.internal.flags.allow_dnd = true;
		win.set_dnd(true);
	}
	win.src.hdc = win32::getDC(win.src.window);

	gfx::_win32_window_set_dark_mode(win, gfx::_get_dark_mode_state());
	gfx::_make_window_transparent(win);

	gfx::_window_set_flags(win, flags, {});
	win.src.backing = {};
	if (flags.open_gl) win.create_opengl_context(gfx::get_global_opengl_hints())!;
	if (flags.egl) egl::create_context(win, gfx::get_global_opengl_hints())!;
}

fn void window_destroy_platform(GfxWindow* win)
{
	win32::removePropW(win.src.window, gfx::RENAME_PROP_NAME);
	win32::releaseDC(win.src.window, win.src.hdc);
	win32::destroyWindow(win.src.window);
	if (win.src.hIconSmall) win32::destroyIcon(win.src.hIconSmall);
	if (win.src.hIconBig) win32::destroyIcon(win.src.hIconBig);
}

fn GfxPoint window_fetch_size(GfxWindow* win)
{
	Win32_RECT area;
	win32::getClientRect(win.src.window, &area);
	win.width = area.right;
	win.height = area.bottom;
	return win.get_size();
}

fn GfxMonitor*? window_find_monitor(GfxWindow* win)
{
	Win32_HMONITOR src = win32::monitorFromWindow(win.src.window, 1 /* MONITOR_DEFAULTTOPRIMARY */);
	for (GfxMonitorNode* node = gfx::_gfx.monitors.list.head; node != null; node = node.next)
	{
		if (node.monitor == src) return &node.mon;
	}
	return gfx::get_primary_monitor();
}

fn void window_flash(GfxWindow* win, GfxFlashRequest request)
{
	if (win.has_focus() && request != CANCEL) return;
	Win32_FLASHWINFO desc = { .cbSize = Win32_FLASHWINFO::size, .hwnd = win.src.window };
	switch (request)
	{
		case CANCEL:
			desc.dwFlags = win32::FLASHW_STOP;
		case BRIEFLY:
			desc.dwFlags = win32::FLASHW_TRAY;
			desc.uCount = 1;
		case UNTIL_FOCUSED:
			desc.dwFlags = win32::FLASHW_TRAY | win32::FLASHW_TIMERNOFG;
		default:
			break;
	}
	win32::flashWindowEx(&desc);
}

fn void window_focus(GfxWindow* win)
{
	win32::setForegroundWindow(win.src.window);
	win32::setFocus(win.src.window);
}

fn void window_hide(GfxWindow* win)
{
	win32::showWindow(win.src.window, win32::SW_HIDE);
}

fn bool window_is_floating(GfxWindow* win)
{
	return win32::getWindowLongPtr(win.src.window, -20) & 0x00000008 != 0;
}

fn bool window_is_hidden(GfxWindow* win)
{
	return !win32::isWindowVisible(win.src.window) && !window_is_minimized(win);
}

fn bool window_is_maximized(GfxWindow* win)
{
	Win32_WINDOWPLACEMENT placement;
	win32::getWindowPlacement(win.src.window, &placement);
	return placement.showCmd == win32::SW_SHOWMAXIMIZED || win32::isZoomed(win.src.window);
}

fn bool window_is_minimized(GfxWindow* win)
{
	Win32_WINDOWPLACEMENT placement;
	win32::getWindowPlacement(win.src.window, &placement);
	return placement.showCmd == win32::SW_SHOWMINIMIZED;
}

fn void window_maximize_platform(GfxWindow* win)
{
	win.hide();
	win32::showWindow(win.src.window, win32::SW_MAXIMIZE);
}

fn void window_minimize_platform(GfxWindow* win)
{
	win32::showWindow(win.src.window, win32::SW_MINIMIZE);
}

fn void window_move_mouse(GfxWindow* win, GfxPoint where)
{
	win.internal.last_mouse = where - win.origin;
	win32::setCursorPos(...where);
}

fn void window_move_platform(GfxWindow* win, GfxPoint where)
{
	win.origin = where;
	win32::setWindowPos(win.src.window, (Win32_HWND){}, ...where, 0, 0, 0x0001 /* SWP_NOSIZE */);
}

fn void window_raise(GfxWindow* win)
{
	win32::bringWindowToTop(win.src.window);
	win32::setWindowPos(win.src.window, null, win.x, win.y, win.width, win.height, 0x0001 | 0x0002 | 0x0200 | 0x0020); // |NOMOVE|NOOWNERZORDER|FRAMECHANGED
}

fn void window_resize_platform(GfxWindow* win, GfxSize size)
{
	win.size = size;
	Win32_RECT rect = { 0, 0, ...size };
	uint style = gfx::_winapi_window_get_style(win.internal.flags);
	uint exStyle = gfx::_winapi_window_get_style_ex(win, win.internal.flags);
	win32::adjustWindowRectEx(&rect, style, 0, exStyle);
	win32::setWindowPos(win.src.window, (Win32_HWND*)(uptr)0, 0, 0, rect.right - rect.left, rect.bottom - rect.top, 0x0010 | 0x0200 | 0x0002 | 0x0004);
}

fn void window_restore_platform(GfxWindow* win) {}

fn void window_set_aspect_ratio(GfxWindow* win, int w, int h) @private
{
	win.src.aspectRatioW = w;
	win.src.aspectRatioH = h;
}

fn void window_set_border(GfxWindow* win, bool border)
{
	win.internal.flags.no_border = !border;
	Win32_RECT rect;
	win32::getClientRect(win.src.window, &rect);
	Win32_LONG style = win32::getWindowLongW(win.src.window, win32::GWL_STYLE);
	style |= (Win32_LONG)gfx::_winapi_window_get_style(win.internal.flags);

	if (!border)
	{
		style &= ~(Win32_LONG)win32::WS_OVERLAPPEDWINDOW;
	}
	else
	{
		if (win.internal.flags.no_resize) style &= ~(Win32_LONG)win32::WS_MAXIMIZEBOX;
		style |= win32::WS_OVERLAPPEDWINDOW;
	}

	Win32_DWORD exStyle = gfx::_winapi_window_get_style_ex(win, win.internal.flags);
	win32::clientToScreen(win.src.window, (Win32_POINT*)&rect.left);
	win32::clientToScreen(win.src.window, (Win32_POINT*)&rect.right);
	win32::adjustWindowRectEx(&rect, (Win32_DWORD)style, 0, exStyle);
	win32::setWindowLongW(win.src.window, -16, style);
	win32::setWindowPos(win.src.window, win32::HWND_TOP, rect.left, rect.top, rect.right - rect.left, rect.bottom - rect.top, 0x0020 | 0x0010 | 0x0004); // SWP_FRAMECHANGED|NOACTIVATE|NOZORDER
}

fn void window_set_capture_mouse_platform(GfxWindow* win, bool state)
{
	if (!state)
	{
		win32::clipCursor(null);
		return;
	}

	Win32_RECT clip_rect;
	win32::getClientRect(win.src.window, &clip_rect);
	win32::clientToScreen(win.src.window, (Win32_POINT*)&clip_rect.left);
	win32::clientToScreen(win.src.window, (Win32_POINT*)&clip_rect.right);
	win32::clipCursor(&clip_rect);
}

fn void window_set_floating(GfxWindow* win, bool floating)
{
	if (floating)
	{
		win32::setWindowPos(win.src.window, (Win32_HWND*)(uptr)(-1), 0, 0, 0, 0, 0x0002 | 0x0001 | 0x0040);
	}
	else
	{
		win32::setWindowPos(win.src.window, (Win32_HWND*)(uptr)(-2) /* HWND_NOTOPMOST */, 0, 0, 0, 0, 0x0002 | 0x0001 | 0x0040);
	}
}

fn void window_set_fullscreen(GfxWindow* win, bool fullscreen)
{
	GfxMonitor*? mon = window_find_monitor(win);
	if (catch mon) return;

	if (!fullscreen)
	{
		// Ignore errors
		(void)monitor_set_mode(mon, &win.internal.oldMode);
		window_set_border(win, true);

		Win32_RECT rect = {0, 0, ...win.internal.old_bounds.size};
		uint style = gfx::_winapi_window_get_style(win.internal.flags);
		uint exStyle = gfx::_winapi_window_get_style_ex(win, win.internal.flags);
		win32::adjustWindowRectEx(&rect, style, 0, exStyle);
		win32::setWindowPos(win.src.window, null, ...win.internal.old_bounds.origin, rect.right - rect.left, rect.bottom - rect.top, 0x0010 | 0x0200 | 0x0004);

		win.internal.flags.fullscreen = false;
		win.bounds = win.internal.old_bounds;
		return;
	}

	win.internal.old_bounds = win.bounds;
	win.internal.oldMode = mon.mode;
	win.internal.flags.fullscreen = true;
	window_set_border(win, false);

	win32::setWindowPos(win.src.window, win32::HWND_TOPMOST, ...mon.origin, 0, 0, 0x0200 | 0x0020 | 0x0040 | 0x0001);
	win.origin = mon.origin;

	(void)mon.scale_to_window(win);
	win32::setWindowPos(win.src.window, (Win32_HWND*)(uptr)(-1), 0, 0, ...mon.mode.size, 0x0200 | 0x0020 | 0x0040 | 0x0002);
	win.size = mon.mode.size;
}

fn bool window_set_icon(GfxWindow* win, char* data, int w, int h, GfxFormat format, GfxWindowIconType type)
{
	if (win.src.hIconSmall && type & WINDOW) win32::destroyIcon(win.src.hIconSmall);
	if (win.src.hIconBig && type & TASKBAR) win32::destroyIcon(win.src.hIconBig);

	if (!data)
	{
		Win32_HICON defaultIcon = win32::loadIconA(null, (Win32_LPCSTR)win32::IDI_APPLICATION);
		if (type & WINDOW) win32::sendMessageA(win.src.window, win32::WM_SETICON, 0, (iptr)defaultIcon);
		if (type & TASKBAR) win32::sendMessageA(win.src.window, win32::WM_SETICON, 1, (iptr)defaultIcon);
		return true;
	}

	if (type & WINDOW)
	{
		win.src.hIconSmall = _load_handle_image(data, w, h, format, 1);
		win32::sendMessageA(win.src.window, win32::WM_SETICON, 0, (iptr)win.src.hIconSmall);
	}
	if (type & TASKBAR)
	{
		win.src.hIconBig = _load_handle_image(data, w, h, format, 1);
		win32::sendMessageA(win.src.window, win32::WM_SETICON, 1, (iptr)win.src.hIconBig);
	}
	return true;
}

fn void window_set_max_size(GfxWindow* win, GfxSize size)
{
	win.src.max_size = size;
}

fn void window_set_min_size(GfxWindow* win, GfxSize size)
{
	win.src.min_size = size;
}

fn void window_set_mouse_passthrough(GfxWindow* win, bool passthrough)
{
	uint key = 0;
	char alpha = 0;
	uint flags = 0;
	CLong exStyle = win32::getWindowLongW(win.src.window, win32::GWL_EXSTYLE);
	if (exStyle & (CLong)win32::WS_EX_LAYERED) win32::getLayeredWindowAttributes(win.src.window, &key, &alpha, &flags);

	if (passthrough)
	{
		exStyle |= (CLong)(win32::WS_EX_TRANSPARENT | win32::WS_EX_LAYERED);
	}
	else
	{
		exStyle &= ~(CLong)win32::WS_EX_TRANSPARENT;
		if (exStyle & (CLong)win32::WS_EX_LAYERED && flags & 2 == 0) exStyle &= ~(CLong)win32::WS_EX_LAYERED;
	}

	win32::setWindowLongW(win.src.window, win32::GWL_EXSTYLE, exStyle);
	if (passthrough) win32::setLayeredWindowAttributes(win.src.window, key, alpha, flags);
}

fn void window_set_mouse_platform(GfxWindow* win, GfxMouse* mouse)
{
	win32::setClassLongPtrA(win.src.window, win32::GCLP_HCURSOR, (iptr)mouse);
	win32::setCursor(mouse);
}

fn void window_set_name(GfxWindow* win, String name) => @pool()
{
	win32::setWindowTextW(win.src.window, name.to_wstring(tmem)!!);
}

fn void window_set_opacity(GfxWindow* win, float opacity)
{
	win32::setWindowLongA(win.src.window, -20, 0x00080000);
	win32::setLayeredWindowAttributes(win.src.window, 0, (char)math::round(opacity * 255), 2);
}

fn void window_set_raw_mouse_mode_platform(GfxWindow* win, bool state)
{
	Win32_RAWINPUTDEVICE id = {0x01, 0x02, 0, win.src.window};
	id.dwFlags = state ? 0 : win32::RIDEV_REMOVE;
	win32::registerRawInputDevices(&id, 1, Win32_RAWINPUTDEVICE::size);
}

fn void window_show_platform(GfxWindow* win)
{
	if (!win.internal.flags.maximize) win32::showWindow(win.src.window, win32::SW_RESTORE);
}

// Window - opengl

fn void? window_init_opengl_context_platform(GfxWindow* win, GfxGlContext* ctx, GfxGlHints* hints)
{
	win.src.ctx.native = ctx;
	win.src.backing = { .opengl };

	Win32_PIXELFORMATDESCRIPTOR pfd = {
		.nSize = Win32_PIXELFORMATDESCRIPTOR::size,
		.nVersion = 1,
		.dwFlags = win32::PFD_DRAW_TO_WINDOW | win32::PFD_SUPPORT_OPENGL | win32::PFD_DOUBLEBUFFER,
		.iPixelType = win32::PFD_TYPE_RGBA,
		.iLayerType = win32::PFD_MAIN_PLANE,
		.cColorBits = 32,
		.cAlphaBits = 8,
		.cDepthBits = 24,
		.cStencilBits = (char)hints.stencil,
		.cAuxBuffers = (char)hints.auxBuffers,
	};
	if (hints.stereo) pfd.dwFlags |= win32::PFD_STEREO;
	if (hints.renderer == SOFTWARE) pfd.dwFlags |= win32::PFD_GENERIC_FORMAT | win32::PFD_GENERIC_ACCELERATED;

	int pixelFormat = win32::choosePixelFormat(win.src.hdc, &pfd);
	if (gfx::wgl_choose_pixel_format_arb_fn)
	{
		FixedList{int, 50} stack;
		stack.push_all({
			WGL_ACCELERATION_ARB, WGL_FULL_ACCELERATION_ARB,
			WGL_DRAW_TO_WINDOW_ARB, 1,
			WGL_PIXEL_TYPE_ARB, WGL_TYPE_RGBA_ARB,
			WGL_SUPPORT_OPENGL_ARB, 1,
			WGL_COLOR_BITS_ARB, 32,
			WGL_DOUBLE_BUFFER_ARB, 1,
			WGL_ALPHA_BITS_ARB,         hints.rgba_size.a,
			WGL_DEPTH_BITS_ARB,         hints.depth,
			WGL_STENCIL_BITS_ARB,       hints.stencil,
			WGL_STEREO_ARB,             hints.stereo,
			WGL_AUX_BUFFERS_ARB,        hints.auxBuffers,
			WGL_RED_BITS_ARB,           hints.rgba_size.r,
			WGL_GREEN_BITS_ARB,         hints.rgba_size.g,
			WGL_BLUE_BITS_ARB,          hints.rgba_size.b,
			WGL_ACCUM_RED_BITS_ARB,     hints.accum_size.r,
			WGL_ACCUM_GREEN_BITS_ARB,   hints.accum_size.g,
			WGL_ACCUM_BLUE_BITS_ARB,    hints.accum_size.b,
			WGL_ACCUM_ALPHA_BITS_ARB,   hints.accum_size.a,
		});
		if (hints.srgb)
		{
			if (hints.profile != ES)
			{
				stack.push_all({WGL_FRAMEBUFFER_SRGB_CAPABLE_ARB, 1});
			}
			else
			{
				stack.push_all({WGL_COLORSPACE_SRGB_EXT, (int)hints.srgb});
			}
		}
		stack.push_all({ WGL_COVERAGE_SAMPLES_NV, hints.samples, 0, 0 });

		int newPixelFormat;
		uint numFormats;
		gfx::wgl_choose_pixel_format_arb_fn(win.src.hdc, stack.array_view(), null, 1, &newPixelFormat, &numFormats);
		if (!numFormats) return gfx::FAILED_TO_CREATE_WGL_PIXEL_FORMAT~;
		pixelFormat = newPixelFormat;
	}

	Win32_PIXELFORMATDESCRIPTOR suggested;
	if (!win32::describePixelFormat(win.src.hdc, pixelFormat, Win32_PIXELFORMATDESCRIPTOR::size, &suggested)
		|| !win32::setPixelFormat(win.src.hdc, pixelFormat, &pfd)) return gfx::FAILED_TO_SET_WGL_PIXEL_FORMAT~;

	if (gfx::wgl_create_context_attribs_arb_fn != null)
	{
		FixedList{int, 40} stack;

		int mask = 0;
		switch (hints.profile)
		{
			case ES: mask |= WGL_CONTEXT_ES_PROFILE_BIT_EXT;
			case COMPATIBILITY: mask |= WGL_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB;
			case FORWARD_COMPATIBILITY: mask |= WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB;
			default: mask |= WGL_CONTEXT_CORE_PROFILE_BIT_ARB;
		}
		stack.push_all({ WGL_CONTEXT_PROFILE_MASK_ARB, mask });

		if (hints.minor || hints.major)
		{
			stack.push_all({
				WGL_CONTEXT_MAJOR_VERSION_ARB, hints.major,
				WGL_CONTEXT_MINOR_VERSION_ARB, hints.minor
			});
		}

		if (opengl_extension_is_supported_platform("WGL_ARB_create_context_no_error")) stack.push_all({ WGL_CONTEXT_OPENGL_NO_ERROR_ARB, (int)hints.no_error });

		if (opengl_extension_is_supported_platform("WGL_ARB_context_flush_control"))
		{
			stack.push(WGL_CONTEXT_RELEASE_BEHAVIOR_ARB);
			stack.push(hints.release_behavior == FLUSH
				? WGL_CONTEXT_RELEASE_BEHAVIOR_FLUSH_ARB
				: WGL_CONTEXT_RELEASE_BEHAVIOR_NONE_ARB);
		}

		int flags = 0;
		if (hints.debug) flags |= WGL_CONTEXT_DEBUG_BIT_ARB;
		if (hints.robustness && opengl_extension_is_supported_platform("WGL_ARB_create_context_robustness")) flags |= WGL_CONTEXT_ROBUST_ACCESS_BIT_ARB;
		if (flags) stack.push_all({ WGL_CONTEXT_FLAGS_ARB, flags });

		stack.push_all({ 0, 0 });
		win.src.ctx.native.ctx = gfx::wgl_create_context_attribs_arb_fn(win.src.hdc, null, stack.array_view());
	}

	if (gfx::wgl_create_context_attribs_arb_fn == null || win.src.ctx.native.ctx == null)
	{
		log::warn("Failed to create accelerated OpenGL context", category: log::CATEGORY_VIDEO);
		win.src.ctx.native.ctx = gfx::wgl_create_context_fn(win.src.hdc);
	}

	win32::releaseDC(win.src.window, win.src.hdc);
	win.src.hdc = win32::getDC(win.src.window);

	if (hints.share != null) gfx::wgl_share_lists_fn(get_current_opengl_context(), hints.share.ctx);

	gfx::wgl_make_current_fn(win.src.hdc, win.src.ctx.native.ctx);
	log::info("Failed to create accelerated OpenGL context", category: log::CATEGORY_VIDEO);
}

fn void window_make_current_opengl_context(GfxWindow* win)
{
	if (!win)
	{
		gfx::wgl_make_current_fn(null, null);
		return;
	}
	gfx::wgl_make_current_fn(win.src.hdc, win.src.ctx.native.ctx);
}

fn void window_opengl_swap_buffers(GfxWindow* win)
{
	win32::swapBuffers(win.src.hdc);
}

fn void window_release_opengl_context_platform(GfxWindow* win, GfxGlContext* ctx)
{
	gfx::wgl_delete_context_fn(ctx.ctx);
	win.src.ctx.native.ctx = null;
}

fn void window_set_opengl_swap_interval(GfxWindow* win, int swap_interval)
{
	if (!gfx::wgl_swap_interval_ext_fn || !gfx::wgl_swap_interval_ext_fn(swap_interval))
	{
		log::error("Failed to set OpenGL swap interval", category: log::CATEGORY_VIDEO);
	}
}

// Vulkan

fn VkSurfaceKHR? window_create_vulkan_surface(GfxWindow* win, VkInstance instance)
{
	VkSurfaceKHR surface;
	VkCreateSurfaceKHRProc proc = (VkCreateSurfaceKHRProc)gfx::_get_instance_proc_vulkan(instance, "vkCreateWin32SurfaceKHR");
	if (!proc) return gfx::VULKAN_NOT_LOADED~;
	VkWin32SurfaceCreateInfoKHR create_info;
	create_info.sType = gfx::VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR;
	create_info.pNext = null;
	create_info.flags = 0;
	create_info.hinstance = gfx::_gfx.instance;
	create_info.hwnd = win.src.window;
	if (proc(instance, &create_info, null, &surface)) return gfx::SURFACE_CREATION_FAILED~;
	return surface;
}

// Window WGPU

fn WGPUSurface? window_create_wgpu_surface(GfxWindow* window, WGPUInstance instance)
{
	WgpuSurfaceSourceWindowsHWND fromHwnd;
	fromHwnd.chain.next = null;
	fromHwnd.chain.sType = gfx::WGPU_STYPE_SURFACE_SOURCE_WINDOWS_HWND;
	fromHwnd.hwnd = window.src.window;
	fromHwnd.hinstance = gfx::_gfx.instance;
	WgpuSurfaceDescriptor surfaceDesc;
	surfaceDesc.nextInChain = (WgpuChainedStruct*)&fromHwnd;
	return gfx::wgpuInstanceCreateSurface(instance, &surfaceDesc) ?: gfx::SURFACE_CREATION_FAILED~;
}

// --- Clipboard

fn GfxDataTransfer? read_clipboard(Allocator allocator, GfxDataTransferType requested_type) => @pool()
{
	if (requested_type != TEXT) return gfx::CLIPBOARD_DID_NOT_CONTAIN_TYPE~;

	gfx::_open_clipboard()!;
	defer win32::closeClipboard();
	void* hdata = win32::getClipboardData(13 /* CF_UNICODETEXT */);
	if (!hdata) return gfx::CLIPBOARD_COULD_NOT_BE_OPENED~;

	WString data = (WString)win32::globalLock(hdata);
	defer win32::globalUnlock(hdata);
	return { .text = string::from_wstring(allocator, data), .type = TEXT };
}

fn void? write_clipboard(GfxDataTransfer* data) => @pool()
{
	WString wstring = data.text.to_wstring(tmem)!;
	void* object = win32::globalAlloc(0x0002 /* GMEM_MOVEABLE */, (Win32_SIZE_T)data.text.len * 2);
	if (!object) return mem::OUT_OF_MEMORY~;
	defer catch win32::globalFree(object);
	Char16* buffer = (ushort*)win32::globalLock(object);
	if (!buffer) return gfx::CLIPBOARD_COULD_NOT_BE_OPENED~;
	defer win32::globalUnlock(object);
	sz len = data.text.len + 1;
	buffer[:len] = wstring[:len];
	gfx::_open_clipboard()!;
	win32::emptyClipboard();
	win32::setClipboardData(13, object);
	win32::closeClipboard();
}

// --- GfxSurface

fn void? surface_init(GfxSurface* surface, void* data, int w, int h, GfxFormat format, GfxWindow* window)
{
	surface.data = data;
	surface.size = { w, h };
	surface.format = format;

	Win32_BITMAPV5HEADER bi = {
	    .bV5Size = Win32_BITMAPV5HEADER::size,
	    .bV5Width = w,
	    .bV5Height = -h,
	    .bV5Planes = 1,
	    .bV5BitCount = (ushort)format.channels * 8,
	    .bV5Compression = win32::BI_RGB,
	};
	surface.native.format = format.channels > 3 ? BGRA8 : BGR8;
	surface.native.bitmap = win32::createDIBSection(gfx::_gfx.root.src.hdc, &bi, 0, (void**)&surface.native.buffer, null, 0);
	if (!surface.native.bitmap) return gfx::EGL_SURFACE_CREATION_FAILED~;
	defer catch surface.release();
	surface.native.hdc_mem = win32::createCompatibleDC(gfx::_gfx.root.src.hdc);
	win32::selectObject(surface.native.hdc_mem, surface.native.bitmap);
}

fn void surface_release(GfxSurface* surface)
{
	if (!surface.data) return;
	if (surface.native.hdc_mem)
	{
		win32::deleteDC(surface.native.hdc_mem);
		win32::deleteObject(surface.native.bitmap);
	}
	*surface = {};
}

// --- Vulkan

fn bool get_vulkan_physical_device_presentation_support(VkInstance instance, VkPhysicalDevice physical_device, uint queue_family_index)
{
	VkGetPhysicalDeviceWin32PresentationSupportKHRProc raw = gfx::_get_instance_proc_vulkan(instance, "vkGetPhysicalDeviceWin32PresentationSupportKHR");
	return raw && raw(physical_device, queue_family_index) != 0;
}

// --- Opengl

fn void* get_current_opengl_context()
{
	return gfx::wgl_get_current_context_fn();
}

fn void* get_opengl_proc(ZString procname)
{
	void* proc = gfx::_gfx.gl.get_proc_addr_fn ? gfx::_gfx.gl.get_proc_addr_fn(procname) : null;
	if (proc) return proc;
	return os::dlsym(gfx::_gfx.gl.native_handle, procname);
}

fn void? load_opengl()
{
	if (gfx::_gfx.gl.native_handle && gfx::_gfx.gl.get_proc_addr_fn) return;
	gfx::_gfx.gl.native_handle = os::dlopen("opengl32.dll") ?: gfx::MISSING_OPENGL_LIB~!;
	gfx::_gfx.gl.get_proc_addr_fn = os::dlsym(gfx::_gfx.gl.native_handle, "wglGetProcAddress") ?: gfx::MISSING_GL_GET_PROC_ADDR~!;
	gfx::wgl_create_context_fn = os::dlsym_try(gfx::_gfx.gl.native_handle, "wglCreateContext")!;
	gfx::wgl_delete_context_fn = os::dlsym_try(gfx::_gfx.gl.native_handle, "wglDeleteContext")!;
	gfx::wgl_make_current_fn = os::dlsym_try(gfx::_gfx.gl.native_handle, "wglMakeCurrent")!;
	gfx::wgl_get_current_dc_fn = os::dlsym_try(gfx::_gfx.gl.native_handle, "wglGetCurrentDC")!;
	gfx::wgl_get_current_context_fn = os::dlsym_try(gfx::_gfx.gl.native_handle, "wglGetCurrentContext")!;
	gfx::wgl_share_lists_fn = os::dlsym_try(gfx::_gfx.gl.native_handle, "wglShareLists")!;

	Win32_HWND dummyWin = win32::createWindowExW(0, gfx::_gfx.wndClass.lpszClassName, null, 0, 0, 0, 0, 0, null, null, gfx::_gfx.instance, null);
	defer win32::destroyWindow(dummyWin);
	Win32_HDC dummy_dc = win32::getDC(dummyWin);
	defer win32::releaseDC(dummyWin, dummy_dc);
	uint pfdFlags = win32::PFD_DRAW_TO_WINDOW | win32::PFD_SUPPORT_OPENGL | win32::PFD_DOUBLEBUFFER;

	Win32_PIXELFORMATDESCRIPTOR pfd = {
		.nSize = Win32_PIXELFORMATDESCRIPTOR::size,
		.nVersion = 1,
		.dwFlags = pfdFlags,
		.iPixelType = win32::PFD_TYPE_RGBA,
		.cColorBits = 32,
		.cAlphaBits = 8,
		.iLayerType = win32::PFD_MAIN_PLANE,
		.cDepthBits = 32,
		.cStencilBits = 8
	};

	int dummyPixelFormat = win32::choosePixelFormat(dummy_dc, &pfd);
	win32::setPixelFormat(dummy_dc, dummyPixelFormat, &pfd);

	void* dummy_context = gfx::wgl_create_context_fn(dummy_dc);
	void* cur = gfx::wgl_get_current_context_fn();
	gfx::wgl_make_current_fn(dummy_dc, dummy_context);
	defer gfx::wgl_delete_context_fn(dummy_context);

	gfx::wgl_create_context_attribs_arb_fn = get_opengl_proc("wglCreateContextAttribsARB");
	gfx::wgl_choose_pixel_format_arb_fn = get_opengl_proc("wglChoosePixelFormatARB");
	gfx::wgl_swap_interval_ext_fn = get_opengl_proc("wglSwapIntervalEXT");

	if (!gfx::wgl_swap_interval_ext_fn) return gfx::FAILED_TO_LOAD_SWAP_INTERVAL_FN~;

	gfx::wgl_make_current_fn(dummy_dc, cur);
}

fn bool opengl_extension_is_supported_platform(String extension)
{
	ZString extensions = null;
	if (GetExtStringArbFn proc = get_opengl_proc("wglGetExtensionsStringARB"))
	{
		extensions = proc(gfx::wgl_get_current_dc_fn());
	}
	else if (GetExtStringExtFn proc2 = get_opengl_proc("wglGetExtensionsStringEXT"))
	{
		extensions = proc2();
	}
	return extensions && gfx::_extension_is_in_list(extensions.str_view(), extension);
}

// --- Keys

fn void init_keycodes_platform()
{
	gfx::_gfx.keycodes = {
		[0x00B] = NUM0,          [0x002] = NUM1,          [0x003] = NUM2,
		[0x004] = NUM3,          [0x005] = NUM4,          [0x006] = NUM5,
		[0x007] = NUM6,          [0x008] = NUM7,          [0x009] = NUM8,
		[0x00A] = NUM9,          [0x01E] = A,             [0x030] = B,
		[0x02E] = C,             [0x020] = D,             [0x012] = E,
		[0x021] = F,             [0x022] = G,             [0x023] = H,
		[0x017] = I,             [0x024] = J,             [0x025] = K,
		[0x026] = L,             [0x032] = M,             [0x031] = N,
		[0x018] = O,             [0x019] = P,             [0x010] = Q,
		[0x013] = R,             [0x01F] = S,             [0x014] = T,
		[0x016] = U,             [0x02F] = V,             [0x011] = W,
		[0x02D] = X,             [0x015] = Y,             [0x02C] = Z,
		[0x028] = APOSTROPHE,    [0x02B] = BACK_SLASH,    [0x033] = COMMA,
		[0x00D] = EQUALS,        [0x029] = BACKTICK,      [0x01A] = BRACKET,
		[0x00C] = MINUS,         [0x034] = PERIOD,        [0x01B] = CLOSE_BRACKET,
		[0x027] = SEMICOLON,     [0x035] = SLASH,         [0x056] = WORLD2,
		[0x00E] = BACK_SPACE,    [0x153] = DELETE,        [0x14F] = END,
		[0x01C] = ENTER,         [0x001] = ESCAPE,        [0x147] = HOME,
		[0x152] = INSERT,        [0x15D] = MENU,          [0x151] = PAGE_DOWN,
		[0x149] = PAGE_UP,       [0x045] = PAUSE,         [0x039] = SPACE,
		[0x00F] = TAB,           [0x03A] = CAPS_LOCK,     [0x145] = NUM_LOCK,
		[0x046] = SCROLL_LOCK,   [0x03B] = F1,            [0x03C] = F2,
		[0x03D] = F3,            [0x03E] = F4,            [0x03F] = F5,
		[0x040] = F6,            [0x041] = F7,            [0x042] = F8,
		[0x043] = F9,            [0x044] = F10,           [0x057] = F11,
		[0x058] = F12,           [0x064] = F13,           [0x065] = F14,
		[0x066] = F15,           [0x067] = F16,           [0x068] = F17,
		[0x069] = F18,           [0x06A] = F19,           [0x06B] = F20,
		[0x06C] = F21,           [0x06D] = F22,           [0x06E] = F23,
		[0x076] = F24,           [0x038] = ALT_L,         [0x01D] = CONTROL_L,
		[0x02A] = SHIFT_L,       [0x15B] = SUPER_L,       [0x137] = PRINT_SCREEN,
		[0x138] = ALT_R,         [0x11D] = CONTROL_R,     [0x036] = SHIFT_R,
		[0x15C] = SUPER_R,       [0x150] = DOWN,          [0x14B] = LEFT,
		[0x14D] = RIGHT,         [0x148] = UP,            [0x052] = PAD0,
		[0x04F] = PAD1,          [0x050] = PAD2,          [0x051] = PAD3,
		[0x04B] = PAD4,          [0x04C] = PAD5,          [0x04D] = PAD6,
		[0x047] = PAD7,          [0x048] = PAD8,          [0x049] = PAD9,
		[0x04E] = PAD_PLUS,      [0x053] = PAD_PERIOD,    [0x135] = PAD_SLASH,
		[0x11C] = PAD_RETURN,    [0x059] = PAD_EQUAL,     [0x037] = PAD_MULTIPLY,
		[0x04A] = PAD_MINUS,
	};
}

fn GfxKey physical_to_mapped_key(GfxKey key)
{
	uint vsc = key.to_api();
	char[256] keyboardState;
	if (win32::getKeyboardState((char*)&keyboardState) == 0) return key;

	uint vk = win32::mapVirtualKeyW(vsc, 1 /* MAPVK_VSC_TO_VK */);
	void* layout = win32::getKeyboardLayout(0);

	ushort[4] charBuffer;
	int result = win32::toUnicodeEx(vk, vsc, (char*)&keyboardState, &charBuffer, 1, 0, layout);
	if (result == 1 && charBuffer[0] < 256) return (GfxKey)charBuffer[0];

	switch (vk)
	{
		case 0x70: return F1; case 0x71: return F2; case 0x72: return F3;
		case 0x73: return F4; case 0x74: return F5; case 0x75: return F6;
		case 0x76: return F7; case 0x77: return F8; case 0x78: return F9;
		case 0x79: return F10; case 0x7A: return F11; case 0x7B: return F12;
		case 0x7C: return F13; case 0x7D: return F14; case 0x7E: return F15;
		case 0x7F: return F16; case 0x80: return F17; case 0x81: return F18;
		case 0x82: return F19; case 0x83: return F20; case 0x84: return F21;
		case 0x85: return F22; case 0x86: return F23; case 0x87: return F24;
		case 0xA0: return SHIFT_L; case 0xA1: return SHIFT_R;
		case 0xA2: return CONTROL_L; case 0xA3: return CONTROL_R;
		case 0xA4: return ALT_L; case 0xA5: return ALT_R;
		case 0x5B: return SUPER_L; case 0x5C: return SUPER_R;
		case 0x14: return CAPS_LOCK; case 0x90: return NUM_LOCK; case 0x91: return SCROLL_LOCK;
		case 0x26: return UP; case 0x28: return DOWN; case 0x25: return LEFT; case 0x27: return RIGHT;
		case 0x24: return HOME; case 0x23: return END; case 0x21: return PAGE_UP; case 0x22: return PAGE_DOWN;
		case 0x2D: return INSERT; case 0x5D: return MENU;
		case 0x6B: return PAD_PLUS; case 0x6D: return PAD_MINUS; case 0x6A: return PAD_MULTIPLY;
		case 0x6F: return PAD_SLASH; case 0x0D: return PAD_RETURN; case 0x6E: return PAD_PERIOD;
		case 0x60: return PAD0; case 0x61: return PAD1; case 0x62: return PAD2;
		case 0x63: return PAD3; case 0x64: return PAD4; case 0x65: return PAD5;
		case 0x66: return PAD6; case 0x67: return PAD7; case 0x68: return PAD8; case 0x69: return PAD9;
		case 0x2C: return PRINT_SCREEN; case 0x13: return PAUSE;
		default: return NULL;
	}
}

// ---- WGL pixel-format / context attributes ----
const int WGL_ACCELERATION_ARB = 0x2003;
const int WGL_FULL_ACCELERATION_ARB = 0x2027;
const int WGL_DRAW_TO_WINDOW_ARB = 0x2001;
const int WGL_PIXEL_TYPE_ARB = 0x2013;
const int WGL_TYPE_RGBA_ARB = 0x202b;
const int WGL_SUPPORT_OPENGL_ARB = 0x2010;
const int WGL_COLOR_BITS_ARB = 0x2014;
const int WGL_DOUBLE_BUFFER_ARB = 0x2011;
const int WGL_ALPHA_BITS_ARB = 0x201b;
const int WGL_DEPTH_BITS_ARB = 0x2022;
const int WGL_STENCIL_BITS_ARB = 0x2023;
const int WGL_STEREO_ARB = 0x2012;
const int WGL_AUX_BUFFERS_ARB = 0x2024;
const int WGL_RED_BITS_ARB = 0x2015;
const int WGL_GREEN_BITS_ARB = 0x2017;
const int WGL_BLUE_BITS_ARB = 0x2019;
const int WGL_ACCUM_RED_BITS_ARB = 0x201e;
const int WGL_ACCUM_GREEN_BITS_ARB = 0x201f;
const int WGL_ACCUM_BLUE_BITS_ARB = 0x2020;
const int WGL_ACCUM_ALPHA_BITS_ARB = 0x2021;
const int WGL_COLORSPACE_SRGB_EXT = 0x3089;
const int WGL_CONTEXT_OPENGL_NO_ERROR_ARB = 0x31b3;
const int WGL_CONTEXT_RELEASE_BEHAVIOR_ARB = 0x2097;
const int WGL_CONTEXT_RELEASE_BEHAVIOR_NONE_ARB = 0x0000;
const int WGL_CONTEXT_RELEASE_BEHAVIOR_FLUSH_ARB = 0x2098;
const int WGL_CONTEXT_FLAGS_ARB = 0x2094;
const int WGL_COVERAGE_SAMPLES_NV = 0x2042;
const int WGL_CONTEXT_ES_PROFILE_BIT_EXT = 0x00000004;
const int WGL_CONTEXT_PROFILE_MASK_ARB = 0x9126;
const int WGL_CONTEXT_CORE_PROFILE_BIT_ARB = 0x00000001;
const int WGL_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB = 0x00000002;
const int WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB = 0x00000002;
const int WGL_CONTEXT_MAJOR_VERSION_ARB = 0x2091;
const int WGL_CONTEXT_MINOR_VERSION_ARB = 0x2092;
const int WGL_FRAMEBUFFER_SRGB_CAPABLE_ARB = 0x20A9;
const int WGL_CONTEXT_DEBUG_BIT_ARB = 0x00000001;
const int WGL_CONTEXT_ROBUST_ACCESS_BIT_ARB = 0x00000004;

// --- helpers

fn Win32_HICON _load_handle_image(char* data, int w, int h, GfxFormat format, int icon) @local
{
	Win32_BITMAPV5HEADER bi = {
		.bV5Size = Win32_BITMAPV5HEADER::size,
		.bV5Width = w,
		.bV5Height = -h,
		.bV5Planes = 1,
		.bV5BitCount = 32,
		.bV5Compression = win32::BI_RGB,
	};

	Win32_HDC dc = win32::getDC(null);
	char* target;
	Win32_HBITMAP color = win32::createDIBSection(dc, &bi, 0, &target, null, 0);
	gfx::copy_image_data(target, w, h, BGRA8, data, format, null);
	win32::releaseDC(null, dc);

	Win32_HBITMAP mask = win32::createBitmap(w, h, 1, 1, null);

	Win32_ICONINFO ii;
	ii.fIcon = icon;
	ii.xHotspot = (uint)w / 2;
	ii.yHotspot = (uint)h / 2;
	ii.hbmMask = mask;
	ii.hbmColor = color;

	Win32_HICON handle = win32::createIconIndirect(&ii);
	win32::deleteObject(color);
	win32::deleteObject(mask);
	return handle;
}

module std::gfx @feat(WIN32) @private;
import std::os::win32, std::collections, std::time, std::math, std::os;
import std::gfx::egl @public;
import std::gfx::win32 @public;


// --- Event handling

// Timer id for the modal-loop redraw pump. Scoped to our own window, so any fixed
// value is unique enough; WM_TIMER still checks it so other timers pass through.
const Win32_UINT_PTR MODAL_REDRAW_TIMER = 1;

fn Win32_LRESULT _wnd_proc(Win32_HWND hWnd, Win32_UINT message, Win32_WPARAM wParam, Win32_LPARAM lParam)
{
	if (message == win32::WM_DISPLAYCHANGE) win32::poll_monitors();

	if (hWnd == _gfx.helper_window) return win32::defWindowProcW(hWnd, message, wParam, lParam);

	GfxWindow* win = win32::getPropW(hWnd, RENAME_PROP_NAME);
	if (!win) return win32::defWindowProcW(hWnd, message, wParam, lParam);

	Win32_RECT frame;
	uint style = _winapi_window_get_style(win.internal.flags);
	uint exStyle = _winapi_window_get_style_ex(win, win.internal.flags);
	win32::adjustWindowRectEx(&frame, style, 0, exStyle);

	switch (message)
	{
		case win32::WM_CLOSE:
		case win32::WM_QUIT:
			_window_close_callback(win);
			return 0;
		case win32::WM_ACTIVATEAPP:
			bool in_focus = (wParam & 0xffff) != 0;
			_window_focus_callback(win, in_focus);
			return win32::defWindowProcW(hWnd, message, wParam, lParam);
		case win32::WM_MOVE:
			if (win.internal.capture_mouse) win32::window_set_capture_mouse_platform(win, true);
			_window_moved_callback(win, _param_to_point(lParam));
			return win32::defWindowProcW(hWnd, message, wParam, lParam);
		case win32::WM_SIZE:
			if (win.internal.capture_mouse) win32::window_set_capture_mouse_platform(win, true);
			_window_resized_callback(win, _param_to_point(lParam));
			_window_check_mode(win);
			return win32::defWindowProcW(hWnd, message, wParam, lParam);
		case win32::WM_MOUSEACTIVATE:
			if ((lParam >> 16) & 0xffff == win32::WM_LBUTTONDOWN)
			{
				if ((lParam & 0xffff) != win32::HTCLIENT) win.src.actionFrame = true;
			}
		case win32::WM_CAPTURECHANGED:
			if (!lParam && win.src.actionFrame)
			{
				win32::window_set_capture_mouse_platform(win, win.internal.capture_mouse);
				win.src.actionFrame = false;
			}
		case win32::WM_DPICHANGED:
			float scaleX = (float)((wParam >> 16) & 0xffff) / 96.0f;
			float scaleY = (float)(wParam & 0xffff) / 96.0f;
			_scale_updated_callback(win, { scaleX, scaleY});
			return win32::defWindowProcW(hWnd, message, wParam, lParam);
		case win32::WM_SIZING:
			if (!win.src.aspectRatioW && !win.src.aspectRatioH) break;
			Win32_RECT* area = (Win32_RECT*)(uptr)lParam;
			int edge = (int)wParam;
			double ratio = (double)win.src.aspectRatioW / (double)win.src.aspectRatioH;
			switch (edge)
			{
				case win32::WMSZ_LEFT:
				case win32::WMSZ_BOTTOMLEFT:
				case win32::WMSZ_RIGHT:
				case win32::WMSZ_BOTTOMRIGHT:
					area.bottom = area.top + (frame.bottom - frame.top) + (int)(((double)((area.right - area.left) - (frame.right - frame.left))) / ratio);
				case win32::WMSZ_TOPLEFT:
				case win32::WMSZ_TOPRIGHT:
					area.top = area.bottom - (frame.bottom - frame.top) - (int)(((double)((area.right - area.left) - (frame.right - frame.left))) / ratio);
				case win32::WMSZ_TOP:
				case win32::WMSZ_BOTTOM:
					area.right = area.left + (frame.right - frame.left) + (int)(((double)((area.bottom - area.top) - (frame.bottom - frame.top))) * ratio);
			}
			return 1;
		case win32::WM_GETMINMAXINFO:
			Win32_MINMAXINFO* mmi = (Win32_MINMAXINFO*)(uptr)lParam;
			bool resize = win.src.min_size == win.src.max_size;
			win.internal.flags.no_resize = resize;

			mmi.ptMinTrackSize.x = win.src.min_size.x + (frame.right - frame.left);
			mmi.ptMinTrackSize.y = win.src.min_size.y + (frame.bottom - frame.top);
			if (win.src.max_size == { 0, 0 }) return win32::defWindowProcW(hWnd, message, wParam, lParam);

			mmi.ptMaxTrackSize.x = win.src.max_size.x + (frame.right - frame.left);
			mmi.ptMaxTrackSize.y = win.src.max_size.y + (frame.bottom - frame.top);
			return win32::defWindowProcW(hWnd, message, wParam, lParam);
		case win32::WM_ENTERSIZEMOVE:
		case win32::WM_ENTERMENULOOP:
			if (win.src.actionFrame) win32::window_set_capture_mouse_platform(win, win.internal.capture_mouse);
			if (++win.src.modal_loop_depth == 1)
			{
				win32::setTimer(win.src.window, MODAL_REDRAW_TIMER, win32::USER_TIMER_MINIMUM, null);
				// No key-up arrives while the loop owns the pump, so held keys would stick.
				_release_all_keys(win);
			}
		case win32::WM_EXITSIZEMOVE:
		case win32::WM_EXITMENULOOP:
			if (win.src.actionFrame) win32::window_set_capture_mouse_platform(win, win.internal.capture_mouse);
			if (win.src.modal_loop_depth > 0 && --win.src.modal_loop_depth == 0)
			{
				win32::killTimer(win.src.window, MODAL_REDRAW_TIMER);
			}
		case win32::WM_TIMER:
			// Leave any other timer on this window to DefWindowProc.
			if (wParam != MODAL_REDRAW_TIMER) break;
			_window_refresh_callback(win, { 0, 0, ...win.size });
		case win32::WM_NCLBUTTONDOWN:
		// workaround for half-second pause when starting to move window
			Win32_POINT point = {};
			if (win32::sendMessageA(win.src.window, win32::WM_NCHITTEST, wParam, lParam) != win32::HTCAPTION || !win32::getCursorPos(&point)) break;
			win32::screenToClient(win.src.window, &point);
			win32::postMessage(win.src.window, win32::WM_MOUSEMOVE, 0, (iptr)((uint)point.x | ((uint)point.y << 16)));
		case win32::WM_PAINT:
			Win32_RECT rect;
			if (win32::getUpdateRect(hWnd, &rect, 0))
			{
				_window_refresh_callback(win, { rect.left, rect.top, rect.right - rect.left, rect.bottom - rect.top });
			}
			else
			{
				Win32_PAINTSTRUCT ps;
				win32::beginPaint(hWnd, &ps);
				_window_refresh_callback(win, { 0, 0, ...win.size });
				win32::endPaint(hWnd, &ps);
			}
			return win32::defWindowProcW(hWnd, message, wParam, lParam);
		case win32::WM_DWMCOMPOSITIONCHANGED:
		case win32::WM_DWMCOLORIZATIONCOLORCHANGED:
			_make_window_transparent(win);
		case win32::WM_CHAR:
		case win32::WM_SYSCHAR:
			if (wParam >= 0xd800 && wParam <= 0xdbff)
			{
				win.src.highSurrogate = (ushort)wParam;
				return 0;
			}
			uint codepoint = 0;
			if (wParam >= 0xdc00 && wParam <= 0xdfff)
			{
				if (win.src.highSurrogate != 0)
				{
					codepoint += (uint)((win.src.highSurrogate - 0xd800) << 10);
					codepoint += (uint)((ushort)wParam - 0xdc00);
					codepoint += 0x10000;
				}
			}
			else
			{
				codepoint = (ushort)wParam;
			}
			win.src.highSurrogate = 0;
			_key_char_callback(win, codepoint);
			return 0;
		case win32::WM_UNICHAR:
			if (wParam == 0xFFFF /* UNICODE_NOCHAR */) return 1;
			_key_char_callback(win, (uint)wParam);
			return 0;
		case win32::WM_KEYUP:
		case win32::WM_SYSKEYUP:
			if (!(win.internal.enabled_events.key_released)) return win32::defWindowProcW(hWnd, message, wParam, lParam);
			int scancode = (int)((lParam >> 16) & (0x0100 | 0xff));
			if (scancode == 0) scancode = (int)win32::mapVirtualKeyW((uint)wParam, 0 /* MAPVK_VK_TO_VSC */);
			switch (scancode)
			{
				case 0x54: scancode = 0x137;
				case 0x146: scancode = 0x45;
				case 0x136: scancode = 0x36;
				default: break;
			}
			GfxKey value = key_from_api((uint)scancode);
			if (wParam == 0x11 /* VK_CONTROL */) { value = ((lParam >> 16) & 0x0100) != 0 ? CONTROL_R : CONTROL_L; }
			_key_update_key_mods(win, win32::getKeyState(0x14) & 1 != 0, win32::getKeyState(0x90) & 1 != 0, win32::getKeyState(0x91) & 1 != 0);
			_key_callback(win, value, win.internal.mod, false, false);
		case win32::WM_KEYDOWN:
		case win32::WM_SYSKEYDOWN:
			{
				if (!win.internal.enabled_events.key_pressed) return win32::defWindowProcW(hWnd, message, wParam, lParam);
				int scancode = (int)((lParam >> 16) & (0x0100 | 0xff));
				if (scancode == 0) scancode = (int)win32::mapVirtualKeyW((uint)wParam, 0);
				switch (scancode)
				{
					case 0x54: scancode = 0x137;
					case 0x146: scancode = 0x45;
					case 0x136: scancode = 0x36;
					default: break;
				}
				GfxKey value = key_from_api((uint)scancode);
				if (wParam == 0x11) { value = ((lParam >> 16) & 0x0100) != 0 ? CONTROL_R : CONTROL_L; }
				bool repeat = is_key_down(value);
				_key_update_key_mods(win, win32::getKeyState(0x14) & 1 != 0, win32::getKeyState(0x90) & 1 != 0, win32::getKeyState(0x91) & 1 != 0);
				_key_callback(win, value, win.internal.mod, repeat, true);
				break;
			}
		case win32::WM_MOUSEMOVE:
			GfxPoint where = _param_to_point(lParam);
			if (win.internal.mouse_inside == false) _mouse_notify_callback(win, where, true);
			_mouse_motion_callback(win, where);
		case win32::WM_INPUT:
			if (!(win.internal.raw_mouse || _gfx.raw_mouse)) return win32::defWindowProcW(hWnd, message, wParam, lParam);

			Win32_RAWINPUT raw;
			uint raw_size = Win32_RAWINPUTHEADER::size + Win32_RAWMOUSE::size;
			win32::getRawInputData((Win32_HRAWINPUT)lParam, win32::RID_INPUT, &raw, &raw_size, Win32_RAWINPUTHEADER::size);

			if (raw.header.dwType != win32::RIM_TYPEMOUSE || (!raw.mouse.lLastX && !raw.mouse.lLastY)) break;

			float vecX = 0.0f;
			float vecY = 0.0f;

			if (raw.mouse.usFlags & win32::MOUSE_MOVE_ABSOLUTE)
			{
				Win32_POINT pos = {0, 0};
				int width, height;
				if (raw.mouse.usFlags & win32::MOUSE_VIRTUAL_DESKTOP)
				{
					pos.x += win32::getSystemMetrics(win32::SM_XVIRTUALSCREEN);
					pos.y += win32::getSystemMetrics(win32::SM_YVIRTUALSCREEN);
					width  = win32::getSystemMetrics(win32::SM_CXVIRTUALSCREEN);
					height = win32::getSystemMetrics(win32::SM_CYVIRTUALSCREEN);
				}
				else
				{
					width  = win32::getSystemMetrics(win32::SM_CXSCREEN);
					height = win32::getSystemMetrics(win32::SM_CYSCREEN);
				}

				pos.x += (int)(((float)raw.mouse.lLastX / 65535.0f) * (float)width);
				pos.y += (int)(((float)raw.mouse.lLastY / 65535.0f) * (float)height);
				win32::screenToClient(win.src.window, &pos);

				vecX = (float)(pos.x - win.internal.last_mouse.x);
				vecY = (float)(pos.y - win.internal.last_mouse.y);
			}
			else
			{
				vecX = (float)raw.mouse.lLastX;
				vecY = (float)raw.mouse.lLastY;
			}

			_raw_motion_callback(win, { vecX, vecY });
		case win32::WM_MOUSELEAVE:
			_mouse_notify_callback(win, win.internal.last_mouse, false);
		case 0x0201: _mouse_button_callback(win, LEFT, true);
		case 0x0204: _mouse_button_callback(win, RIGHT, true);
		case 0x0207: _mouse_button_callback(win, MIDDLE, true);
		case 0x020B: _mouse_button_callback(win, (GfxMouseButton)(GfxMouseButton.MISC1.ordinal + (((wParam >> 16) & 0xffff) == 2 ? 1 : 0)), true);
		case 0x0202: _mouse_button_callback(win, LEFT, false);
		case 0x0205: _mouse_button_callback(win, RIGHT, false);
		case 0x0208: _mouse_button_callback(win, MIDDLE, false);
		case 0x020C: _mouse_button_callback(win, (GfxMouseButton)(GfxMouseButton.MISC1.ordinal + (((wParam >> 16) & 0xffff) == 2 ? 1 : 0)), false);
		case win32::WM_MOUSEWHEEL:
			float scrollY = (float)((short)((wParam >> 16) & 0xffff)) / 120.0f; // WHEEL_DELTA
			_mouse_scroll_callback(win, { 0.0f, scrollY });
		case win32::WM_MOUSEHWHEEL:
			float scrollX = -(float)((short)((wParam >> 16) & 0xffff)) / 120.0f;
			_mouse_scroll_callback(win, { scrollX, 0.0f });
		case win32::WM_DROPFILES:
			void* drop = (void*)wParam;
			Win32_POINT pt;
			win32::dragQueryPoint(drop, &pt);
			_data_drag_callback(win, FILE, MOVE, { pt.x, pt.y });
			if (!win.internal.enabled_events.data_drop) return win32::defWindowProcW(hWnd, message, wParam, lParam);
			uint count = win32::dragQueryFileW(drop, 0xffffffff, null, 0);
			for (uint i = 0; i < count; i++) @pool()
			{
				uint length = win32::dragQueryFileW(drop, i, null, 0);
				if (!length) continue;
				Char16* buffer = mem::temp_array(Char16, (sz)length + 1);
				win32::dragQueryFileW(drop, i, buffer, length + 1);
				String? s = string::tfrom_wstring((WString)buffer);
				if (catch e = s)
				{
					log::error("Failed to convert drag %s", e, category: log::CATEGORY_VIDEO);
					continue;
				}
				_data_drop_callback(win, s, s.len + 1, FILE);
			};
			win32::dragFinish(drop);
		default:
			break;
	}

	return win32::defWindowProcW(hWnd, message, wParam, lParam);
}

// --- Helpers

fn void _create_monitor(Win32_DISPLAY_DEVICEW* adapter, Win32_DISPLAY_DEVICEW* dd)
{
	Win32_DEVMODEW dm = { .dmSize = Win32_DEVMODEW::size };
	if (!win32::enumDisplaySettingsW((WString)&adapter.deviceName, (uint)win32::ENUM_CURRENT_SETTINGS, &dm)) return;

	GfxMonitorNode* node = _monitor_add(null);
	node.adapter_name = adapter.deviceName;
	if (dd) @pool()
	{
		node.device_name = dd.deviceName;
		String s = string::tfrom_wstring((WString)&dd.deviceString) ?? "<invalid>";
		if (s.len > 127) s = s[:127];
		node.mon.name[:s.len] = s[..];
		node.mon.name[s.len] = 0;
	};
	node.mon.name[127] = 0;

	Win32_RECT rect;
	rect.left = dm.dmPosition.x;
	rect.top = dm.dmPosition.y;
	rect.right = dm.dmPosition.x + (int)dm.dmPelsWidth;
	rect.bottom = dm.dmPosition.y + (int)dm.dmPelsHeight;
	win32::enumDisplayMonitors(null, &rect, (void*)&_get_monitor_handle_callback, (iptr)(void*)node);

	_win32_get_mode(&dm, &node.mon.mode);

	Win32_MONITORINFOEXW monitorInfo;
	monitorInfo.cbSize = Win32_MONITORINFOEXW::size;
	win32::getMonitorInfoW(node.monitor, &monitorInfo);

	node.mon.origin = { monitorInfo.rcMonitor.left, monitorInfo.rcMonitor.top};

	Win32_HDC hdc = win32::createDCW((WString)&monitorInfo.szDevice, null, null, null);
	float dpiX = win32::getDeviceCaps(hdc, 88 /* LOGPIXELSX */);
	float dpiY = win32::getDeviceCaps(hdc, 90 /* LOGPIXELSY */);
	node.mon.scale = { dpiX / 96.0f, dpiY / 96.0f };
	node.mon.pixelRatio = dpiX >= 192.0f ? 2.0f : 1.0f;
	node.mon.phys_size = {
		win32::getDeviceCaps(hdc, 4 /* HORZSIZE */) / 25.4f,
		win32::getDeviceCaps(hdc, 6 /* VERTSIZE */) / 25.4f
	};
	win32::deleteDC(hdc);

	if (monitorInfo.dwFlags & win32::MONITORINFOF_PRIMARY) _gfx.monitors.primary = node;
	_monitor_callback(_gfx.root, &node.mon, true);
}

fn int _get_monitor_handle_callback(void* monitor, void* hdcMonitor, Win32_RECT* lprcMonitor, iptr dwData)
{
	Win32_MONITORINFOEXW mi;
	mi.cbSize = Win32_MONITORINFOEXW::size;
	if (win32::getMonitorInfoW(monitor, &mi))
	{
		GfxMonitorNode* node = (void*)dwData;
		sz i;
		for (i = 0; mi.szDevice[i] && node.adapter_name[i] && mi.szDevice[i] == node.adapter_name[i]; i++) {}
		if (mi.szDevice[i] == 0 && node.adapter_name[i] == 0) node.monitor = monitor;
	}
	return 1;
}

macro GfxPoint _param_to_point(iptr lParam)
{
	return { (int)(lParam & 0xffff), (int)((lParam >> 16) & 0xffff) };
}

fn uint _winapi_window_get_style(GfxWindowFlags flags)
{
	uint style = win32::WS_CLIPSIBLINGS | win32::WS_CLIPCHILDREN;
	if (flags.fullscreen)
	{
		style |= win32::WS_POPUP;
		return style;
	}
	style |= win32::WS_SYSMENU | win32::WS_MINIMIZEBOX;
	if (!flags.no_border)
	{
		style |= win32::WS_CAPTION;
		if (!flags.no_resize) style |= win32::WS_MAXIMIZEBOX | win32::WS_THICKFRAME;
	}
	else
	{
		style |= win32::WS_POPUP;
	}
	return style;
}

fn void _win32_get_mode(Win32_DEVMODEW* dm, GfxMonitorMode* mode)
{
	mode.size = { (int)dm.dmPelsWidth, (int)dm.dmPelsHeight };
	_split_bpp(dm.dmBitsPerPel, mode);
	switch (dm.dmDisplayFrequency)
	{
		case 119: case 59: case 29: mode.refresh_rate = ((float)(dm.dmDisplayFrequency + 1) * 1000.0f) / 1001.0f;
		default: mode.refresh_rate = (float)dm.dmDisplayFrequency;
	}
}

const WString WIDE_DISPLAY_NAME = @wstring("DISPLAY\0");

fn void? _gamma_set(GfxMonitor* monitor, ushort* gamma)
{
	Win32_HDC dc = win32::createDCW(WIDE_DISPLAY_NAME, (WString)&monitor.node.adapter_name, null, null);
	if (!dc) return GAMMA_COULD_NOT_BE_SET~;
	defer win32::deleteDC(dc);
	if (!win32::setDeviceGammaRamp(dc, gamma)) return GAMMA_COULD_NOT_BE_SET~;
}

fn uint _winapi_window_get_style_ex(GfxWindow* win, GfxWindowFlags flags)
{
	uint style = win32::WS_EX_APPWINDOW;
	if (flags.fullscreen || flags.floating || win.is_floating()) style |= win32::WS_EX_TOPMOST;
	return style;
}

// --- OpenGL

alias WglCreateContextProc = fn Win32_HGLRC(Win32_HDC*);
alias WglDeleteContextProc = fn int(Win32_HGLRC);
alias WglMakeCurrentProc = fn int(Win32_HDC*, Win32_HGLRC);
alias WglGetCurrentDcProc = fn Win32_HDC*();
alias WglGetCurrentContextProc = fn Win32_HGLRC();
alias WglShareListsProc = fn int(Win32_HGLRC, Win32_HGLRC);
alias WglCreateContextAttribsArbProc = fn Win32_HGLRC(Win32_HDC*, Win32_HGLRC, int*);
alias WglChoosePixelFormatArbProc = fn int(Win32_HDC*, int*, float*, uint, int*, uint*);
alias WglSwapIntervalExtProc = fn int(int);

WglCreateContextProc           wgl_create_context_fn;
WglDeleteContextProc           wgl_delete_context_fn;
WglMakeCurrentProc             wgl_make_current_fn;
WglGetCurrentDcProc            wgl_get_current_dc_fn;
WglGetCurrentContextProc       wgl_get_current_context_fn;
WglShareListsProc              wgl_share_lists_fn;
WglCreateContextAttribsArbProc wgl_create_context_attribs_arb_fn;
WglChoosePixelFormatArbProc    wgl_choose_pixel_format_arb_fn;
WglSwapIntervalExtProc         wgl_swap_interval_ext_fn;

alias GetExtStringArbFn = fn ZString(Win32_HDC*);
alias GetExtStringExtFn = fn ZString();

// --- DirectX

fn void? _window_create_directx_swap_chain_native(GfxWindow* win, Win32_IDXGIFactory* factory, void* device, void** swapchain)
{
	Win32_DXGI_SWAP_CHAIN_DESC swap_chain_desc = {
		.bufferCount = 2,
		.bufferDesc.width = (uint)win.width,
		.bufferDesc.height = (uint)win.height,
		.bufferDesc.format = R8G8B8A8_UNORM,
		.bufferUsage = win32::DXGI_USAGE_RENDER_TARGET_OUTPUT,
		.outputWindow = win.src.window,
		.sampleDesc.count = 1,
		.sampleDesc.quality = 0,
		.windowed = 1,
		.flags = ALLOW_MODE_SWITCH,
	};
	Win32_HRESULT hr = factory.lpVtbl.createSwapChain(factory, device, &swap_chain_desc, swapchain);
	if (hr < 0) return DIRECTX_SWAP_CHAIN_CREATION_FAILED~;
}


// --- Vulkan

alias VkGetPhysicalDeviceWin32PresentationSupportKHRProc = fn uint(VkPhysicalDevice, uint);

struct VkWin32SurfaceCreateInfoKHR
{
	int sType;
	void* pNext;
	uint flags;
	void* hinstance;
	void* hwnd;
}


const WString RENAME_PROP_NAME = @wstring("C3GFX\0");

// wgpu

struct WgpuSurfaceSourceWindowsHWND
{
	WgpuChainedStruct chain;
	void* hinstance;
	void* hwnd;
}

// --- helpers


fn void? _open_clipboard()
{
	for (int i = 0; i < 3; i++)
	{
		if (win32::openClipboard(null)) return;
	}
	return CLIPBOARD_COULD_NOT_BE_OPENED~;
}

fn void _make_window_transparent(GfxWindow* win)
{
	if (!win.internal.flags.transparent) return;

	Win32_DWM_BLURBEHIND bb = { .dwFlags = win32::DWM_BB_ENABLE, .fEnable = 1, .hRgnBlur = null };
	if (win32::dwmEnableBlurBehindWindow(win.src.window, &bb))
	{
		win32::setWindowLongA(win.src.window, win32::GWL_EXSTYLE, win32::WS_EX_LAYERED);
		win32::setLayeredWindowAttributes(win.src.window, 0, (char)128, win32::LWA_ALPHA);
	}
}

fn bool _get_dark_mode_state()
{
	uint light_mode = 1;
	uint len = uint::size;
	win32::regGetValueA(win32::HKEY_CURRENT_USER, `Software\Microsoft\Windows\CurrentVersion\Themes\Personalize`, "AppsUseLightTheme", win32::RRF_RT_REG_DWORD, null, &light_mode, &len);
	return light_mode == 0;
}

fn void _win32_window_set_dark_mode(GfxWindow* win, bool dark)
{
	int value = (int)dark;
	win32::dwmSetWindowAttribute(win.src.window,USE_IMMERSIVE_DARK_MODE, &value, int::size);
}
