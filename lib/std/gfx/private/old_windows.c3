module std::gfx @feat(WIN32);
import std::collections::list, std::math, std::os::win32;




// private
fn bool todo_fix_createUTF8FromWideStringWin32(ushort* source, char* output, sz max)
{
	if (source == null) return false;
	int size = win32::wideCharToMultiByte(65001 /* CP_UTF8 */, 0, source, -1, null, 0, null, null);
	if (size == 0) return false;
	if (size > (int)max) size = (int)max;
	if (win32::wideCharToMultiByte(65001, 0, source, -1, (void*)output, size, null, null) == 0) return false;
	output[size] = 0;
	return true;
}


// public
fn void _wait_for_event_native(int waitMS) { win32::msgWaitForMultipleObjects(0, null, 0, (uint)waitMS, 0x04FF /* QS_ALLINPUT */); }


const WString WIDE_DISPLAY_NAME = @wstring("DISPLAY\0");

fn sz todo_fix_monitor_getGammaRampPtr(GfxMonitor* monitor, TodoRenameGammaRamp* ramp)
{
	ushort[768] values; // 3 x 256
	Win32_HDC dc = win32::createDCW(WIDE_DISPLAY_NAME, (WString)&monitor.node.adapterName, null, null);
	win32::getDeviceGammaRamp(dc, &values);
	win32::deleteDC(dc);
	if (ramp != null)
	{
		mem::copy(ramp.red, &values[0], 512);
		mem::copy(ramp.green, &values[256], 512);
		mem::copy(ramp.blue, &values[512], 512);
	}
	return 256;
}

// public
fn bool todo_fix_monitor_setGammaRamp(GfxMonitor* monitor, TodoRenameGammaRamp* ramp)
{
	ushort[768] values;
	if (ramp.count != 256)
	{
		todo_fix_debugCallback(TODO_RENAME_TYPE_ERROR, TODO_RENAME_ERR_PLATFORM, "Win32: Gamma ramp size must be 256");
		return false;
	}
	mem::copy(&values[0], ramp.red, 512);
	mem::copy(&values[256], ramp.green, 512);
	mem::copy(&values[512], ramp.blue, 512);
	Win32_HDC dc = win32::createDCW(WIDE_DISPLAY_NAME, (WString)&monitor.node.adapterName, null, null);
	win32::setDeviceGammaRamp(dc, &values);
	win32::deleteDC(dc);
	return true;
}

// private
fn int todo_fix_win32GetMonitorHandleCallback(void* hMonitor, void* hdcMonitor, Win32_RECT* lprcMonitor, iptr dwData)
{
	Win32_MONITORINFOEXW mi;
	mi.cbSize = Win32_MONITORINFOEXW::size;
	if (win32::getMonitorInfoW(hMonitor, &mi) != 0)
	{
		GfxMonitorNode* node = (GfxMonitorNode*)(void*)dwData;
		sz i;
		for (i = 0; mi.szDevice[i] != 0 && node.adapterName[i] != 0 && mi.szDevice[i] == node.adapterName[i]; i++) {}
		if (mi.szDevice[i] == 0 && node.adapterName[i] == 0) node.hMonitor = hMonitor;
	}
	return 1;
}

// public
fn void todo_fix_win32CreateMonitor(Win32_DISPLAY_DEVICEW* adapter, Win32_DISPLAY_DEVICEW* dd)
{
	Win32_DEVMODEW dm;
	dm.dmSize = (ushort)Win32_DEVMODEW::size;
	if (win32::enumDisplaySettingsW((ushort*)&adapter.deviceName, (uint)win32::ENUM_CURRENT_SETTINGS, &dm) == 0) return;

	GfxMonitorNode* node = _monitor_add(null);
	mem::copy(&node.adapterName, &adapter.deviceName, 64);
	if (dd != null) mem::copy(&node.deviceName, &dd.deviceName, 64);

	if (dd != null) todo_fix_createUTF8FromWideStringWin32((ushort*)&dd.deviceString, (char*)&node.mon.name, 127);
	node.mon.name[127] = 0;

	Win32_RECT rect;
	rect.left = dm.dmPosition.x;
	rect.top = dm.dmPosition.y;
	rect.right = dm.dmPosition.x + (int)dm.dmPelsWidth;
	rect.bottom = dm.dmPosition.y + (int)dm.dmPelsHeight;
	win32::enumDisplayMonitors(null, &rect, (void*)&todo_fix_win32GetMonitorHandleCallback, (iptr)(void*)node);

	_win32_get_mode(&dm, &node.mon.mode);

	Win32_MONITORINFOEXW monitorInfo;
	monitorInfo.cbSize = Win32_MONITORINFOEXW::size;
	win32::getMonitorInfoW(node.hMonitor, &monitorInfo);

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

	if (monitorInfo.dwFlags & win32::MONITORINFOF_PRIMARY) _gfx_ctx.monitors.primary = node;
	_monitor_callback(_gfx_ctx.root, &node.mon, true);
}

// public
fn void todo_fix_pollMonitors()
{
	for (GfxMonitorNode* node = _gfx_ctx.monitors.list.head; node != null; node = node.next) node.disconnected = true;

	for (uint adapterNum = 0;; adapterNum++)
	{
		Win32_DISPLAY_DEVICEW adapter = { .cb = Win32_DISPLAY_DEVICEW::size };
		if (win32::enumDisplayDevicesW(null, adapterNum, &adapter, 0) == 0) break;
		if (!(adapter.stateFlags & win32::DISPLAY_DEVICE_ACTIVE)) continue;

		Win32_DWORD deviceNum;
		for (deviceNum = 0;; deviceNum++)
		{
			Win32_DISPLAY_DEVICEW dd = { .cb = Win32_DISPLAY_DEVICEW::size };
			if (win32::enumDisplayDevicesW((WString)&adapter.deviceName, deviceNum, &dd, 0) == 0) break;
			if (!(dd.stateFlags & win32::DISPLAY_DEVICE_ACTIVE)) continue;

			GfxMonitorNode* node;
			for (node = _gfx_ctx.monitors.list.head; node != null; node = node.next)
			{
				sz i;
				for (i = 0; dd.deviceName[i] != 0 && node.deviceName[i] != 0 && dd.deviceName[i] == node.deviceName[i]; i++) {}
				if (node.disconnected == true && dd.deviceName[i] == 0 && node.deviceName[i] == 0)
				{
					node.disconnected = false;
					win32::enumDisplayMonitors(null, null, (void*)&todo_fix_win32GetMonitorHandleCallback, (iptr)(void*)&node.mon);
					break;
				}
			}
			if (node != null) continue;
			todo_fix_win32CreateMonitor(&adapter, &dd);
		}

		if (deviceNum == 0)
		{
			GfxMonitorNode* node;
			for (node = _gfx_ctx.monitors.list.head; node != null; node = node.next)
			{
				sz i;
				for (i = 0; adapter.deviceName[i] != 0 && node.adapterName[i] != 0 && adapter.deviceName[i] == node.adapterName[i]; i++) {}
				if (node.disconnected == true && adapter.deviceName[i] == 0 && node.adapterName[i] == 0) { node.disconnected = false; break; }
			}
			if (node != null) continue;
			todo_fix_win32CreateMonitor(&adapter, null);
		}
	}
	refresh_monitors();
}

// public
fn void todo_fix_monitorNode_free(GfxMonitorNode* node) {}

// public

// public



// private
fn Win32_HICON todo_fix_loadHandleImage(char* data, int w, int h, GfxFormat format, int icon)
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
	copy_image_data(target, w, h, BGRA8, data, format, null);
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

// public
fn TodoRenameMouse* todo_fix_createMouseStandard(GfxMouseIcon mouse)
{
	iptr mouseIcon;
	switch (mouse)
	{
		case NORMAL:
		case ARROW: mouseIcon = 32512; // OCR_NORMAL
		case IBEAM: mouseIcon = 32513;
		case WAIT: mouseIcon = 32514;
		case CROSSHAIR: mouseIcon = 32515;
		case PROGRESS: mouseIcon = 32650;
		case RESIZE_NW:
		case RESIZE_SE:
		case RESIZE_NWSE: mouseIcon = 32642;
		case RESIZE_NE:
		case RESIZE_SW:
		case RESIZE_NESW: mouseIcon = 32643;
		case RESIZE_E:
		case RESIZE_W:
		case RESIZE_EW: mouseIcon = 32644;
		case RESIZE_N:
		case RESIZE_S:
		case RESIZE_NS: mouseIcon = 32645;
		case RESIZE_ALL: mouseIcon = 32646;
		case NOT_ALLOWED: mouseIcon = 32648;
		case POINTING_HAND: mouseIcon = 32649;
		default: return null;
	}
	return (TodoRenameMouse*)win32::loadCursorA(null, (Win32_LPCSTR)mouseIcon);
}

// public
fn TodoRenameMouse* todo_fix_createMouse(char* data, int w, int h, GfxFormat format) => (TodoRenameMouse*)todo_fix_loadHandleImage(data, w, h, format, 0);




// public

// public
fn void _window_move_native(GfxWindow* win, GfxPoint where)
{
	win.origin = where;
	win32::setWindowPos(win.src.window, (Win32_HWND){}, ...where, 0, 0, 0x0001 /* SWP_NOSIZE */);
}

// public

// public


// public


// public
fn bool todo_fix_readClipboardPtr(GfxDataTransferType requestedType, void* buffer, sz capacity, GfxDataTransfer* data)
{
	if (requestedType != TEXT) return false;

	sz retry = 0;
	int isOpen = 0;
	while (isOpen == 0 && retry < 3) { isOpen = win32::openClipboard(null); retry += 1; }
	if (isOpen == 0) return false;

	void* hData = win32::getClipboardData(13 /* CF_UNICODETEXT */);
	if (hData == null) { win32::closeClipboard(); return false; }

	ushort* wstr = (ushort*)win32::globalLock(hData);
	bool ret = true;
	int length = win32::wideCharToMultiByte(65001, 0, wstr, -1, null, 0, null, null);
	if (length <= 0) return false;

	data.length = (sz)length;
	data.type = TEXT;

	if (buffer != null && capacity < data.length) { ret = false; }
	else if (buffer != null && data.length != 0)
	{
		if (win32::wideCharToMultiByte(65001, 0, wstr, -1, buffer, length, null, null) <= 0) return false;
		data.data = buffer;
	}

	win32::globalUnlock(hData);
	win32::closeClipboard();
	return ret;
}

// public

// public

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

alias GetExtStringArbFn = fn ZString(Win32_HDC*);
alias GetExtStringExtFn = fn ZString();
alias GlxGetProcAddressProc = fn GfxFn(ZString);

// public
fn bool todo_fix_extensionSupportedPlatform_OpenGL(ZString extension, sz len)
{
	ZString extensions = null;
	GfxFn proc = _get_opengl_proc_native("wglGetExtensionsStringARB");
	GfxFn proc2 = _get_opengl_proc_native("wglGetExtensionsStringEXT");
	if (proc != null) { extensions = ((GetExtStringArbFn)proc)(wgl_get_current_dc_fn()); }
	else if (proc2 != null) { extensions = ((GetExtStringExtFn)proc2)(); }
	return extensions && todo_fix_extensionSupportedStr(extensions, extension, len);
}

// public
fn void _window_make_opengl_current_context_native(GfxWindow* win)
{
	if (win == null) { wgl_make_current_fn(null, null); }
	else { wgl_make_current_fn(win.src.hdc, win.src.ctx.native.ctx); }
}

// public
fn void* _get_current_opengl_context_native() => wgl_get_current_context_fn();
// public
fn void _window_swap_opengl_buffers_native(GfxWindow* win)
{
	win32::swapBuffers(win.src.hdc);
}


// public


// public
