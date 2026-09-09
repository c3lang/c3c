module std::gfx @feat(WIN32) @private;
import std::os::win32, std::collections::list;

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
		Win32_HICON defaultIcon = loadIconA(null, (char*)(uptr)32512 /* IDI_APPLICATION */);
		if (type & WINDOW) sendMessage(win.src.window, 0x0080 /* WM_SETICON */, 0, (iptr)defaultIcon);
		if (type & TASKBAR) sendMessage(win.src.window, 0x0080, 1, (iptr)defaultIcon);
		return true;
	}

	if (type & WINDOW)
	{
		win.src.hIconSmall = todo_fix_loadHandleImage(data, w, h, format, 1);
		sendMessage(win.src.window, 0x0080, 0, (iptr)(uptr)win.src.hIconSmall);
	}
	if (type & TASKBAR)
	{
		win.src.hIconBig = todo_fix_loadHandleImage(data, w, h, format, 1);
		sendMessage(win.src.window, 0x0080, 1, (iptr)(uptr)win.src.hIconBig);
	}
	return true;
}

fn void _window_set_min_size_native(GfxWindow* win, GfxSize size)
{
	win.src.min_size = size;
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
		todo_fix_monitor_setMode(mon, &win.internal.oldMode);
		_window_set_border_native(win, true);

		Rect rect = {0, 0, ...win.internal.old_bounds.size};
		uint style = todo_fix_winapiWindowGetStyle(win, win.internal.flags);
		uint exStyle = todo_fix_winapiWindowGetExStyle(win, win.internal.flags);
		adjustWindowRectEx(&rect, style, 0, exStyle);
		setWindowPos(win.src.window, (Win32_HWND*)(uptr)0, ...win.internal.old_bounds.origin, rect.right - rect.left, rect.bottom - rect.top, 0x0010 | 0x0200 | 0x0004);

		win.internal.flags.fullscreen = false;
		win.bounds = win.internal.old_bounds;
		return;
	}

	win.internal.old_bounds = win.bounds;
	win.internal.oldMode = mon.mode;
	win.internal.flags.fullscreen = true;
	_window_set_border_native(win, false);

	setWindowPos(win.src.window, (Win32_HWND*)(uptr)(-1) /* HWND_TOPMOST */, ...mon.origin, 0, 0, 0x0200 | 0x0020 | 0x0040 | 0x0001);
	win.origin = mon.origin;

	todo_fix_monitor_scaleToWindow(mon, win);
	setWindowPos(win.src.window, (Win32_HWND*)(uptr)(-1), 0, 0, ...mon.mode.size, 0x0200 | 0x0020 | 0x0040 | 0x0002);
	win.size = mon.mode.size;
}

fn void _window_resize_native(GfxWindow* win, GfxSize size)
{
	win.size = size;
	Rect rect = { 0, 0, ...size };
	uint style = todo_fix_winapiWindowGetStyle(win, win.internal.flags);
	uint exStyle = todo_fix_winapiWindowGetExStyle(win, win.internal.flags);
	adjustWindowRectEx(&rect, style, 0, exStyle);
	setWindowPos(win.src.window, (Win32_HWND*)(uptr)0, 0, 0, rect.right - rect.left, rect.bottom - rect.top, 0x0010 | 0x0200 | 0x0002 | 0x0004);
}

fn GfxMonitorMode[] _monitor_get_modes_native(GfxMonitor* monitor)
{
	uint mode_index = 0;
	List{GfxMonitorMode} modes;
	for OUTER: (;;)
	{
		DevModeW dm = { .size = (ushort)DevModeW::size };
		if (enumDisplaySettingsW((WString)&monitor.node.adapterName, mode_index, &dm) == 0) break;
		if (changeDisplaySettingsExW((WString)&monitor.node.adapterName, &dm, null, CDS_TEST, null) != DISP_CHANGE_SUCCESSFUL) continue;
		mode_index++;
		if (dm.bitsPerPel < 15) continue;
		GfxMonitorMode mode;
		todo_fix_win32GetMode(&dm, &mode);
		foreach (&mode2 : modes)
		{
			if (mode2.equals_request(&mode, ALL)) continue OUTER;
		}
		modes.push(mode);
	}
	return modes.array_view();
}

fn GfxPoint _get_global_mouse_native()
{
	Win32_POINT p;
	getCursorPos(&p);
	return { ...p };
}


// --- Vulkan

alias VkGetPhysicalDeviceWin32PresentationSupportKHRProc = fn uint(VkPhysicalDevice, uint);

fn bool _vulkan_get_physical_device_presentation_support_native(VkInstance instance, VkPhysicalDevice physical_device, uint queue_family_index)
{
	GfxFn raw = todo_fix_getInstanceProcAddress_Vulkan(instance, "vkGetPhysicalDeviceWin32PresentationSupportKHR");
	if (raw == null) return false;
	uint out = ((VkGetPhysicalDeviceWin32PresentationSupportKHRProc)raw)(physical_device, queue_family_index);
	return out != 0;
}

fn VkSurfaceKHR? _window_create_vulkan_surface_native(GfxWindow* win, VkInstance instance)
{
	VkSurfaceKHR surface;
	VkCreateSurfaceKHRProc proc = (VkCreateSurfaceKHRProc)todo_fix_getInstanceProcAddress_Vulkan(instance, "vkCreateWin32SurfaceKHR");
	if (!proc) return VULKAN_NOT_LOADED~;
	VkWin32SurfaceCreateInfoKHR create_info;
	create_info.sType = VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR;
	create_info.pNext = null;
	create_info.flags = 0;
	create_info.hinstance = _gfx_ctx.instance;
	create_info.hwnd = win.src.window;
	if (proc(instance, &create_info, null, &surface)) return SURFACE_CREATION_FAILED~;
	return surface;
}



module std::gfx @feat(WIN32) @link("user32", "gdi32", "shell32", "advapi32", "dwmapi", "opengl32", "winmm");
