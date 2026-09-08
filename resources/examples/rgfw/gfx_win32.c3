module std::gfx @feat(WIN32) @private;

fn void _window_show_native(GfxWindow* win)
{
	if (!win.internal.flags.maximize) showWindow(win.src.window, (int)SW_RESTORE);
}

fn bool _window_set_icon_native(GfxWindow* win, char* data, int w, int h, GfxFormat format, GfxWindowIconType type)
{
	if (win.src.hIconSmall && type & WINDOW) destroyIcon(win.src.hIconSmall);
	if (win.src.hIconBig && type & TASKBAR) destroyIcon(win.src.hIconBig);

	if (!data)
	{
		HIcon defaultIcon = loadIconA(null, (char*)(uptr)32512 /* IDI_APPLICATION */);
		if (type & WINDOW) sendMessage(win.src.window, 0x0080 /* WM_SETICON */, 0, (iptr)defaultIcon);
		if (type & TASKBAR) sendMessage(win.src.window, 0x0080, 1, (iptr)defaultIcon);
		return true;
	}

	if (type & WINDOW)
	{
		win.src.hIconSmall = rgfw_loadHandleImage(data, w, h, format, 1);
		sendMessage(win.src.window, 0x0080, 0, (iptr)(uptr)win.src.hIconSmall);
	}
	if (type & TASKBAR)
	{
		win.src.hIconBig = rgfw_loadHandleImage(data, w, h, format, 1);
		sendMessage(win.src.window, 0x0080, 1, (iptr)(uptr)win.src.hIconBig);
	}
	return true;
}

fn void _window_set_min_size_native(GfxWindow* win, int w, int h)
{
	win.src.min_size = { w, h };
}

fn bool _window_is_floating_native(GfxWindow* win)
{
	return getWindowLongPtr(win.src.window, -20) & 0x00000008 != 0;
}

fn void _window_set_fullscreen_native(GfxWindow* win, bool fullscreen)
{
	GfxMonitor* mon = find_monitor(win);

	if (!fullscreen)
	{
		rgfw_monitor_setMode(mon, &win.internal.oldMode);
		_window_set_border_native(win, true);

		Rect rect = {0, 0, ...win.internal.old_bounds.size};
		uint style = rgfw_winapiWindowGetStyle(win, win.internal.flags);
		uint exStyle = rgfw_winapiWindowGetExStyle(win, win.internal.flags);
		adjustWindowRectEx(&rect, style, 0, exStyle);
		setWindowPos(win.src.window, (HWnd*)(uptr)0, ...win.internal.old_bounds.origin, rect.right - rect.left, rect.bottom - rect.top, 0x0010 | 0x0200 | 0x0004);

		win.internal.flags.fullscreen = false;
		win.bounds = win.internal.old_bounds;
		return;
	}

	win.internal.old_bounds = win.bounds;
	win.internal.oldMode = mon.mode;
	win.internal.flags.fullscreen = true;
	_window_set_border_native(win, false);

	setWindowPos(win.src.window, (HWnd*)(uptr)(-1) /* HWND_TOPMOST */, ...mon.origin, 0, 0, 0x0200 | 0x0020 | 0x0040 | 0x0001);
	win.origin = mon.origin;

	rgfw_monitor_scaleToWindow(mon, win);
	setWindowPos(win.src.window, (HWnd*)(uptr)(-1), 0, 0, mon.mode.w, mon.mode.h, 0x0200 | 0x0020 | 0x0040 | 0x0002);
	win.width = mon.mode.w; win.height = mon.mode.h;
}

fn void _window_resize_native(GfxWindow* win, int w, int h)
{
	win.size = { w, h };
	Rect rect = {0, 0, w, h};
	uint style = rgfw_winapiWindowGetStyle(win, win.internal.flags);
	uint exStyle = rgfw_winapiWindowGetExStyle(win, win.internal.flags);
	adjustWindowRectEx(&rect, style, 0, exStyle);
	setWindowPos(win.src.window, (HWnd*)(uptr)0, 0, 0, rect.right - rect.left, rect.bottom - rect.top, 0x0010 | 0x0200 | 0x0002 | 0x0004);
}


module std::gfx @feat(WIN32) @link("user32", "gdi32", "shell32", "advapi32", "dwmapi", "opengl32", "winmm");
