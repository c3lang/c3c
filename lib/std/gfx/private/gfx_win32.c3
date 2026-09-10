module std::gfx @feat(WIN32) @private;
import std::os::win32, std::collections::list, std::math;

struct GfxGlContext
{
	void* ctx; // HGLRC
}

struct GfxEglContext
{
	void* ctx;
	void* surface;
	void* egl_window;
}

struct GfxWindowSrc
{
	inline IntRect bounds;
	void* window; // HWND
	void* hdc; // HDC
	void* hIconSmall; // HICON
	void* hIconBig; // HICON
	GfxPoint min_size;
	GfxPoint max_size;
	int aspectRatioW, aspectRatioH;
	bool actionFrame;
	ushort highSurrogate;
	GfxBackingContext ctx;
	TodoRenameGfxContextType gfxType;
}

fn void _window_move_mouse(GfxWindow* win, GfxPoint where)
{
	win.internal.last_mouse = where - win.origin;
	win32::setCursorPos(...where);
}

fn void _window_show_native(GfxWindow* win)
{
	if (!win.internal.flags.maximize) win32::showWindow(win.src.window, win32::SW_RESTORE);
}

fn bool _window_set_mouse_native(GfxWindow* win, TodoRenameMouse* mouse)
{
	win32::setClassLongPtrA(win.src.window, win32::GCLP_HCURSOR, (iptr)mouse);
	win32::setCursor(mouse);
	return false;
}

fn void _window_flash_native(GfxWindow* win, GfxFlashRequest request) @private
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

fn void _window_set_capture_mouse_native(GfxWindow* win, bool state)
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

fn void _window_set_raw_mouse_mode_native(GfxWindow* win, bool state)
{
	Win32_RAWINPUTDEVICE id = {0x01, 0x02, 0, win.src.window};
	id.dwFlags = state ? 0 : win32::RIDEV_REMOVE;
	win32::registerRawInputDevices(&id, 1, Win32_RAWINPUTDEVICE::size);
}

fn bool _window_set_icon_native(GfxWindow* win, char* data, int w, int h, GfxFormat format, GfxWindowIconType type)
{
	if (win.src.hIconSmall && type & WINDOW) win32::destroyIcon(win.src.hIconSmall);
	if (win.src.hIconBig && type & TASKBAR) win32::destroyIcon(win.src.hIconBig);

	if (!data)
	{
		Win32_HICON defaultIcon = win32::loadIconA(null, (Win32_LPCSTR)(uptr)32512 /* IDI_APPLICATION */);
		if (type & WINDOW) win32::sendMessageA(win.src.window, win32::WM_SETICON, 0, (iptr)defaultIcon);
		if (type & TASKBAR) win32::sendMessageA(win.src.window, win32::WM_SETICON, 1, (iptr)defaultIcon);
		return true;
	}

	if (type & WINDOW)
	{
		win.src.hIconSmall = todo_fix_loadHandleImage(data, w, h, format, 1);
		win32::sendMessageA(win.src.window, win32::WM_SETICON, 0, (iptr)win.src.hIconSmall);
	}
	if (type & TASKBAR)
	{
		win.src.hIconBig = todo_fix_loadHandleImage(data, w, h, format, 1);
		win32::sendMessageA(win.src.window, win32::WM_SETICON, 1, (iptr)win.src.hIconBig);
	}
	return true;
}

fn void _window_set_min_size_native(GfxWindow* win, GfxSize size)
{
	win.src.min_size = size;
}

fn bool _window_is_floating_native(GfxWindow* win)
{
	return win32::getWindowLongPtr(win.src.window, -20) & 0x00000008 != 0;
}

fn bool _window_is_hidden_native(GfxWindow* win)
{
	return !win32::isWindowVisible(win.src.window) && !_window_is_minimized_native(win);
}

fn void _window_set_fullscreen_native(GfxWindow* win, bool fullscreen)
{
	GfxMonitor* mon = find_monitor(win);

	if (!fullscreen)
	{
		todo_fix_monitor_setMode(mon, &win.internal.oldMode);
		_window_set_border_native(win, true);

		Win32_RECT rect = {0, 0, ...win.internal.old_bounds.size};
		uint style = _winapi_window_get_style(win.internal.flags);
		uint exStyle = _winapi_window_get_style_ex(win, win.internal.flags);
		win32::adjustWindowRectEx(&rect, style, 0, exStyle);
		win32::setWindowPos(win.src.window, null, ...win.internal.old_bounds.origin, rect.right - rect.left, rect.bottom - rect.top, 0x0010 | 0x0200 | 0x0004);

		win.internal.flags.fullscreen = false;
		win.bounds = win.internal.old_bounds;
		return;
	}

	win.internal.old_bounds = win.bounds;
	win.internal.oldMode = mon.mode;
	win.internal.flags.fullscreen = true;
	_window_set_border_native(win, false);

	win32::setWindowPos(win.src.window, win32::HWND_TOPMOST, ...mon.origin, 0, 0, 0x0200 | 0x0020 | 0x0040 | 0x0001);
	win.origin = mon.origin;

	todo_fix_monitor_scaleToWindow(mon, win);
	win32::setWindowPos(win.src.window, (Win32_HWND*)(uptr)(-1), 0, 0, ...mon.mode.size, 0x0200 | 0x0020 | 0x0040 | 0x0002);
	win.size = mon.mode.size;
}

fn void _window_hide_native(GfxWindow* win)
{
	win32::showWindow(win.src.window, win32::SW_HIDE);
}

fn void _window_focus_native(GfxWindow* win)
{
	win32::setForegroundWindow(win.src.window);
	win32::setFocus(win.src.window);
}

fn bool _window_is_minimized_native(GfxWindow* win)
{
	Win32_WINDOWPLACEMENT placement;
	win32::getWindowPlacement(win.src.window, &placement);
	return placement.showCmd == win32::SW_SHOWMINIMIZED;
}

fn void _window_resize_native(GfxWindow* win, GfxSize size)
{
	win.size = size;
	Win32_RECT rect = { 0, 0, ...size };
	uint style = _winapi_window_get_style(win.internal.flags);
	uint exStyle = _winapi_window_get_style_ex(win, win.internal.flags);
	win32::adjustWindowRectEx(&rect, style, 0, exStyle);
	win32::setWindowPos(win.src.window, (Win32_HWND*)(uptr)0, 0, 0, rect.right - rect.left, rect.bottom - rect.top, 0x0010 | 0x0200 | 0x0002 | 0x0004);
}

fn GfxPoint _window_fetch_size_native(GfxWindow* win)
{
	Win32_RECT area;
	win32::getClientRect(win.src.window, &area);
	win.width = area.right;
	win.height = area.bottom;
	return win.get_size();
}
fn bool _window_is_maximized_native(GfxWindow* win)
{
	Win32_WINDOWPLACEMENT placement;
	win32::getWindowPlacement(win.src.window, &placement);
	return placement.showCmd == win32::SW_SHOWMAXIMIZED || win32::isZoomed(win.src.window);
}

fn GfxMonitorMode[] _monitor_get_modes_native(GfxMonitor* monitor)
{
	uint mode_index = 0;
	List{GfxMonitorMode} modes;
	for OUTER: (;;)
	{
		Win32_DEVMODEW dm = { .dmSize = (ushort)Win32_DEVMODEW::size };
		if (win32::enumDisplaySettingsW((WString)&monitor.node.adapterName, mode_index, &dm) == 0) break;
		if (win32::changeDisplaySettingsExW((WString)&monitor.node.adapterName, &dm, null, CDS_TEST, null) != DISP_CHANGE_SUCCESSFUL) continue;
		mode_index++;
		if (dm.dmBitsPerPel < 15) continue;
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
	win32::getCursorPos(&p);
	return { ...p };
}

fn void _free_mouse(TodoRenameMouse* mouse)
{
	win32::destroyCursor(mouse);
}

fn void _destroy_native()
{
	win32::timeEndPeriod(1);
	if (_gfx_ctx.helper_window) win32::destroyWindow(_gfx_ctx.helper_window);
	if (_gfx_ctx.instance) win32::unregisterClassW(_gfx_ctx.wndClass.lpszClassName, _gfx_ctx.instance);
	if (_gfx_ctx.hiddenMouse != null) _free_mouse(_gfx_ctx.hiddenMouse);
}

// --- Helpers


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

fn uint _winapi_window_get_style_ex(GfxWindow* win, GfxWindowFlags flags)
{
	uint style = win32::WS_EX_APPWINDOW;
	if (flags.fullscreen || flags.floating || win.is_floating()) style |= win32::WS_EX_TOPMOST;
	return style;
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

const Char16[] RENAME_PROP_NAME = {'C', '3', 'G', 'F', 'X', 0};


module std::gfx @feat(WIN32) @link("user32", "gdi32", "shell32", "advapi32", "dwmapi", "opengl32", "winmm");
