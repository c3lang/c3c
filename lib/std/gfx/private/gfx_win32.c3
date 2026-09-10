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
		Win32_HICON defaultIcon = win32::loadIconA(null, (Win32_LPCSTR)win32::IDI_APPLICATION);
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

fn void? _write_clipboard_native(GfxDataTransfer* data)
{
	void* object = win32::globalAlloc(0x0002 /* GMEM_MOVEABLE */, (Win32_SIZE_T)data.length * 2);
	if (!object) return mem::OUT_OF_MEMORY~;
	defer catch win32::globalFree(object);
	ushort* buffer = (ushort*)win32::globalLock(object);
	if (!buffer) return CLIPBOARD_COULD_NOT_BE_OPENED~;

	win32::multiByteToWideChar(65001, 0, (void*)data.data, -1, buffer, (int)data.length);
	win32::globalUnlock(object);

	int is_open = 0;
	for (int i = 0; i < 3 && !is_open; i++)
	{
		is_open = win32::openClipboard(null);
	}
	if (!is_open) return CLIPBOARD_COULD_NOT_BE_OPENED~;

	win32::emptyClipboard();
	win32::setClipboardData(13, object);
	win32::closeClipboard();
}

fn GfxPoint _get_global_mouse_native()
{
	Win32_POINT p;
	win32::getCursorPos(&p);
	return { ...p };
}

fn void? _init_native(String class_name, GfxInitOption flags)
{
	win32::setProcessDPIAware();
	win32::timeBeginPeriod(1);

	_gfx_ctx.instance = win32::getModuleHandleW(null);
	@pool()
	{
		assert(class_name.len < 254);
		WString str = class_name.to_temp_wstring()!;
		sz len = str.len();
		mem::copy(&_gfx_ctx.wideClassName, str, len);
		_gfx_ctx.wideClassName[len] = 0;
	};

	_gfx_ctx.wndClass = {};
	_gfx_ctx.wndClass.lpszClassName = (WString)&_gfx_ctx.wideClassName;
	_gfx_ctx.wndClass.hInstance = _gfx_ctx.instance;
	_gfx_ctx.wndClass.hCursor = win32::loadCursorA(null, (Win32_LPCSTR)win32::IDC_ARROW);
	_gfx_ctx.wndClass.lpfnWndProc = &_wnd_proc;
	_gfx_ctx.wndClass.cbClsExtra = sz::size;

	_gfx_ctx.wndClass.hIcon = (Win32_HICON)win32::loadImageA(_gfx_ctx.instance, "C3GFX", 1 /* IMAGE_ICON */, 0, 0, 0x00000040 | 0x00008000 /* LR_DEFAULTSIZE|LR_SHARED */);
	if (!_gfx_ctx.wndClass.hIcon)
	{
		_gfx_ctx.wndClass.hIcon = (Win32_HICON)win32::loadImageA(null, (Win32_LPCSTR)win32::IDI_APPLICATION, 1, 0, 0, 0x00000040 | 0x00008000);
	}

	win32::registerClassW(&_gfx_ctx.wndClass);
	_gfx_ctx.helper_window = win32::createWindowExW(0, _gfx_ctx.wndClass.lpszClassName, null, 0, 0, 0, 0, 0, null, null, _gfx_ctx.instance, null);

	char[4] blk;
	_gfx_ctx.hiddenMouse = todo_fix_createMouse(&blk, 1, 1, RGBA8);
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

// --- OpenGL

fn void? _window_init_opengl_context_native(GfxWindow* win, GfxGlContext* ctx, GfxGlHints* hints)
{
	win.src.ctx.native = ctx;
	win.src.gfxType = TODO_RENAME_GFX_NATIVE_OPEN_GL;

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
	if (hints.renderer == TODO_RENAME_GL_SOFTWARE) pfd.dwFlags |= win32::PFD_GENERIC_FORMAT | win32::PFD_GENERIC_ACCELERATED;

	int pixelFormat = win32::choosePixelFormat(win.src.hdc, &pfd);
	if (todo_fix_wglChoosePixelFormatARB)
	{
		int[50] pixelFormatAttribs;
		TodoRenameAttribStack stack;
		todo_fix_attribStack_init(&stack, (int*)&pixelFormatAttribs, 50);

		todo_fix_attribStack_pushAttribs(&stack, WGL_ACCELERATION_ARB, WGL_FULL_ACCELERATION_ARB);
		todo_fix_attribStack_pushAttribs(&stack, WGL_DRAW_TO_WINDOW_ARB, 1);
		todo_fix_attribStack_pushAttribs(&stack, WGL_PIXEL_TYPE_ARB, WGL_TYPE_RGBA_ARB);
		todo_fix_attribStack_pushAttribs(&stack, WGL_SUPPORT_OPENGL_ARB, 1);
		todo_fix_attribStack_pushAttribs(&stack, WGL_COLOR_BITS_ARB, 32);
		todo_fix_attribStack_pushAttribs(&stack, WGL_DOUBLE_BUFFER_ARB, 1);
		todo_fix_attribStack_pushAttribs(&stack, WGL_ALPHA_BITS_ARB, hints.alpha);
		todo_fix_attribStack_pushAttribs(&stack, WGL_DEPTH_BITS_ARB, hints.depth);
		todo_fix_attribStack_pushAttribs(&stack, WGL_STENCIL_BITS_ARB, hints.stencil);
		todo_fix_attribStack_pushAttribs(&stack, WGL_STEREO_ARB, hints.stereo);
		todo_fix_attribStack_pushAttribs(&stack, WGL_AUX_BUFFERS_ARB, hints.auxBuffers);
		todo_fix_attribStack_pushAttribs(&stack, WGL_RED_BITS_ARB, hints.red);
		todo_fix_attribStack_pushAttribs(&stack, WGL_GREEN_BITS_ARB, hints.blue);
		todo_fix_attribStack_pushAttribs(&stack, WGL_BLUE_BITS_ARB, hints.green);
		todo_fix_attribStack_pushAttribs(&stack, WGL_ACCUM_RED_BITS_ARB, hints.accumRed);
		todo_fix_attribStack_pushAttribs(&stack, WGL_ACCUM_GREEN_BITS_ARB, hints.accumGreen);
		todo_fix_attribStack_pushAttribs(&stack, WGL_ACCUM_BLUE_BITS_ARB, hints.accumBlue);
		todo_fix_attribStack_pushAttribs(&stack, WGL_ACCUM_ALPHA_BITS_ARB, hints.accumAlpha);

		if (hints.sRGB)
		{
			if (hints.profile != TODO_RENAME_GL_ES)
			{
				todo_fix_attribStack_pushAttribs(&stack, WGL_FRAMEBUFFER_SRGB_CAPABLE_ARB, 1);
			}
			else
			{
				todo_fix_attribStack_pushAttribs(&stack, WGL_COLORSPACE_SRGB_EXT, (int)hints.sRGB);
			}
		}
		todo_fix_attribStack_pushAttribs(&stack, WGL_COVERAGE_SAMPLES_NV, hints.samples);
		todo_fix_attribStack_pushAttribs(&stack, 0, 0);

		int newPixelFormat;
		uint numFormats;
		todo_fix_wglChoosePixelFormatARB(win.src.hdc, (int*)&pixelFormatAttribs, null, 1, &newPixelFormat, &numFormats);
		if (numFormats == 0)
		{
			todo_fix_debugCallback(TODO_RENAME_TYPE_ERROR, TODO_RENAME_ERR_OPEN_GL_CONTEXT, "Failed to create a pixel format for WGL");
		}
		else
		{
			pixelFormat = newPixelFormat;
		}
	}

	Win32_PIXELFORMATDESCRIPTOR suggested;
	if (!win32::describePixelFormat(win.src.hdc, pixelFormat, Win32_PIXELFORMATDESCRIPTOR::size, &suggested)
	|| !win32::setPixelFormat(win.src.hdc, pixelFormat, &pfd))
	{
		todo_fix_debugCallback(TODO_RENAME_TYPE_ERROR, TODO_RENAME_ERR_OPEN_GL_CONTEXT, "Failed to set the WGL pixel format");
	}

	if (todo_fix_wglCreateContextAttribsARB != null)
	{
		int[40] attribs;
		TodoRenameAttribStack stack;
		todo_fix_attribStack_init(&stack, (int*)&attribs, 40);

		int mask = 0;
		switch (hints.profile)
		{
			case TODO_RENAME_GL_ES: mask |= WGL_CONTEXT_ES_PROFILE_BIT_EXT;
			case TODO_RENAME_GL_COMPATIBILITY: mask |= WGL_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB;
			case TODO_RENAME_GL_FORWARD_COMPATIBILITY: mask |= WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB;
			default: mask |= WGL_CONTEXT_CORE_PROFILE_BIT_ARB;
		}
		todo_fix_attribStack_pushAttribs(&stack, WGL_CONTEXT_PROFILE_MASK_ARB, mask);

		if (hints.minor != 0 || hints.major != 0)
		{
			todo_fix_attribStack_pushAttribs(&stack, WGL_CONTEXT_MAJOR_VERSION_ARB, hints.major);
			todo_fix_attribStack_pushAttribs(&stack, WGL_CONTEXT_MINOR_VERSION_ARB, hints.minor);
		}

		char[32] noError = "WGL_ARB_create_context_no_error";
		if (todo_fix_extensionSupportedPlatform_OpenGL((ZString)&noError, 31)) todo_fix_attribStack_pushAttribs(&stack, WGL_CONTEXT_OPENGL_NO_ERROR_ARB, (int)hints.noError);

		char[30] flushControl = "WGL_ARB_context_flush_control";
		if (todo_fix_extensionSupportedPlatform_OpenGL((ZString)&flushControl, 30))
		{
			if (hints.releaseBehavior == TODO_RENAME_GL_RELEASE_FLUSH) { todo_fix_attribStack_pushAttribs(&stack, WGL_CONTEXT_RELEASE_BEHAVIOR_ARB, WGL_CONTEXT_RELEASE_BEHAVIOR_FLUSH_ARB); }
			else if (hints.releaseBehavior == TODO_RENAME_GL_RELEASE_NONE) { todo_fix_attribStack_pushAttribs(&stack, WGL_CONTEXT_RELEASE_BEHAVIOR_ARB, WGL_CONTEXT_RELEASE_BEHAVIOR_NONE_ARB); }
		}

		int flags = 0;
		if (hints.debug) flags |= WGL_CONTEXT_DEBUG_BIT_ARB;
		char[34] robustness = "WGL_ARB_create_context_robustness";
		if (hints.robustness && todo_fix_extensionSupportedPlatform_OpenGL((ZString)&robustness, 33)) flags |= WGL_CONTEXT_ROBUST_ACCESS_BIT_ARB;
		if (flags != 0) todo_fix_attribStack_pushAttribs(&stack, WGL_CONTEXT_FLAGS_ARB, flags);

		todo_fix_attribStack_pushAttribs(&stack, 0, 0);
		win.src.ctx.native.ctx = todo_fix_wglCreateContextAttribsARB(win.src.hdc, null, (int*)&attribs);
	}

	if (todo_fix_wglCreateContextAttribsARB == null || win.src.ctx.native.ctx == null)
	{
		todo_fix_debugCallback(TODO_RENAME_TYPE_ERROR, TODO_RENAME_ERR_OPEN_GL_CONTEXT, "Failed to create an accelerated OpenGL Context.");
		win.src.ctx.native.ctx = todo_fix_wglCreateContext(win.src.hdc);
	}

	win32::releaseDC(win.src.window, win.src.hdc);
	win.src.hdc = win32::getDC(win.src.window);

	if (hints.share != null) todo_fix_wglShareLists(todo_fix_getCurrentContext_OpenGL(), hints.share.ctx);

	todo_fix_wglMakeCurrent(win.src.hdc, win.src.ctx.native.ctx);
	todo_fix_debugCallback(TODO_RENAME_TYPE_INFO, TODO_RENAME_INFO_OPEN_GL, "OpenGL context initalized.");
}

fn void _window_release_opengl_context_native(GfxWindow* win, GfxGlContext* ctx)
{
	wgl_delete_context_fn(ctx.ctx);
	win.src.ctx.native.ctx = null;
}

fn void? _load_opengl_native() @private
{
	if (_gfx_ctx.nativeGL_handle && _gfx_ctx.glGetProcAddress) return;
	if (!(_gfx_ctx.nativeGL_handle = win32::loadLibraryA("opengl32.dll"))) return MISSING_OPENGL_LIB~;
	_gfx_ctx.glGetProcAddress = win32::getProcAddress(_gfx_ctx.nativeGL_handle, "wglGetProcAddress");
	if (!_gfx_ctx.glGetProcAddress) return MISSING_GL_GET_PROC_ADDR~;
	todo_fix_wglCreateContext = win32::getProcAddress(_gfx_ctx.nativeGL_handle, "wglCreateContext");
	wgl_delete_context_fn = win32::getProcAddress(_gfx_ctx.nativeGL_handle, "wglDeleteContext");
	todo_fix_wglMakeCurrent = win32::getProcAddress(_gfx_ctx.nativeGL_handle, "wglMakeCurrent");
	todo_fix_wglGetCurrentDC = win32::getProcAddress(_gfx_ctx.nativeGL_handle, "wglGetCurrentDC");
	todo_fix_wglGetCurrentContext = win32::getProcAddress(_gfx_ctx.nativeGL_handle, "wglGetCurrentContext");
	todo_fix_wglShareLists = win32::getProcAddress(_gfx_ctx.nativeGL_handle, "wglShareLists");

	Win32_HWND dummyWin = win32::createWindowExW(0, _gfx_ctx.wndClass.lpszClassName, null, 0, 0, 0, 0, 0, null, null, _gfx_ctx.instance, null);
	Win32_HDC dummy_dc = win32::getDC(dummyWin);
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

	void* dummy_context = todo_fix_wglCreateContext(dummy_dc);
	void* cur = todo_fix_wglGetCurrentContext();
	todo_fix_wglMakeCurrent(dummy_dc, dummy_context);

	todo_fix_wglCreateContextAttribsARB = (TodoRenameWglCreateContextAttribsArbProc)_get_opengl_proc_native("wglCreateContextAttribsARB");
	todo_fix_wglChoosePixelFormatARB = (TodoRenameWglChoosePixelFormatArbProc)_get_opengl_proc_native("wglChoosePixelFormatARB");
	todo_fix_wglSwapIntervalEXT = (TodoRenameWglSwapIntervalExtProc)_get_opengl_proc_native("wglSwapIntervalEXT");

	if (todo_fix_wglSwapIntervalEXT == null) todo_fix_debugCallback(TODO_RENAME_TYPE_ERROR, TODO_RENAME_ERR_OPEN_GL_CONTEXT, "Failed to load swap interval function");

	todo_fix_wglMakeCurrent(dummy_dc, cur);
	wgl_delete_context_fn(dummy_context);
	win32::releaseDC(dummyWin, dummy_dc);
	win32::destroyWindow(dummyWin);
}

fn GfxFn _get_opengl_proc_native(ZString procname)
{
	GfxFn proc = _gfx_ctx.glGetProcAddress ? ((GlxGetProcAddressProc)_gfx_ctx.glGetProcAddress)(procname) : null;
	if (proc) return proc;
	return win32::getProcAddress(_gfx_ctx.nativeGL_handle, (Win32_LPCSTR)procname);
}

// --- Egl

fn GfxEglContext*? _window_create_egl_context_native(GfxWindow* win, GfxGlHints* hints)
{
	return NOT_IMPLEMENTED~;
}

fn void _window_release_egl_context_native(GfxWindow* win, GfxEglContext* ctx) {}

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


// --- helpers

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