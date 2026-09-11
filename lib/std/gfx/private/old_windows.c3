module std::gfx @feat(WIN32);
import std::collections::list, std::math, std::os::win32;


struct BitmapV5Header
{
	uint size;
	int width, height;
	ushort planes, bitCount;
	uint compression, sizeImage;
	int xPelsPerMeter, yPelsPerMeter;
	uint clrUsed, clrImportant;
	uint redMask, greenMask, blueMask, alphaMask;
	uint csType;
	int[9] endpoints;
	uint gammaRed, gammaGreen, gammaBlue;
	uint intent, profileData, profileSize, reserved;
}
const uint BI_RGB = 0;

const uint MONITORINFOF_PRIMARY = 1;

const uint DM_PELSWIDTH = 0x80000;
const uint DM_PELSHEIGHT = 0x100000;
const uint DM_DISPLAYFREQUENCY = 0x400000;
const uint DM_BITSPERPEL = 0x40000;
const int CDS_TEST = 0x2;
const int CDS_UPDATEREGISTRY = 0x1;
const int DISP_CHANGE_SUCCESSFUL = 0;
const int ENUM_CURRENT_SETTINGS = -1;

const uint DISPLAY_DEVICE_ACTIVE = 0x1;

struct MinMaxInfo { Win32_POINT ptReserved, ptMaxSize, ptMaxPosition, ptMinTrackSize, ptMaxTrackSize; }


alias TodoRenameWglCreateContextProc = fn Win32_HGLRC(Win32_HDC*);
alias TodoRenameWglDeleteContextProc = fn int(Win32_HGLRC);
alias TodoRenameWglMakeCurrentProc = fn int(Win32_HDC*, Win32_HGLRC);
alias TodoRenameWglGetCurrentDcProc = fn Win32_HDC*();
alias TodoRenameWglGetCurrentContextProc = fn Win32_HGLRC();
alias TodoRenameWglShareListsProc = fn int(Win32_HGLRC, Win32_HGLRC);
alias TodoRenameWglCreateContextAttribsArbProc = fn Win32_HGLRC(Win32_HDC*, Win32_HGLRC, int*);
alias TodoRenameWglChoosePixelFormatArbProc = fn int(Win32_HDC*, int*, float*, uint, int*, uint*);
alias TodoRenameWglSwapIntervalExtProc = fn int(int);

TodoRenameWglCreateContextProc todo_fix_wglCreateContext;
TodoRenameWglDeleteContextProc wgl_delete_context_fn;
TodoRenameWglMakeCurrentProc todo_fix_wglMakeCurrent;
TodoRenameWglGetCurrentDcProc todo_fix_wglGetCurrentDC;
TodoRenameWglGetCurrentContextProc todo_fix_wglGetCurrentContext;
TodoRenameWglShareListsProc todo_fix_wglShareLists;
TodoRenameWglCreateContextAttribsArbProc todo_fix_wglCreateContextAttribsARB;
TodoRenameWglChoosePixelFormatArbProc todo_fix_wglChoosePixelFormatARB;
TodoRenameWglSwapIntervalExtProc todo_fix_wglSwapIntervalEXT;


struct GfxNativeImage
{
	GfxFormat format;
	char* buffer; // bitmapBits
	Win32_HBITMAP bitmap; // HBITMAP
	Win32_HDC hdcMem; // HDC
}

struct WgpuSurfaceSourceWindowsHWND { WgpuChainedStruct chain; void* hinstance; void* hwnd; }

// public

alias HResult = int;
const int DXGI_FORMAT_R8G8B8A8_UNORM = 28;
const uint DXGI_USAGE_RENDER_TARGET_OUTPUT = 0x20;
const uint DXGI_SWAP_CHAIN_FLAG_ALLOW_MODE_SWITCH = 2;
const int DXGI_SWAP_EFFECT_DISCARD = 0;

struct DxgiRational { uint numerator; uint denominator; }
struct DxgiSampleDesc { uint count; uint quality; }
struct DxgiModeDesc
{
	uint width;
	uint height;
	DxgiRational refresh_rate;
	int format;
	int scanlineOrdering;
	int scaling;
}
struct DxgiSwapChainDesc
{
	DxgiModeDesc bufferDesc;
	DxgiSampleDesc sampleDesc;
	uint bufferUsage;
	uint bufferCount;
	void* outputWindow;
	int windowed;
	int swapEffect;
	uint flags;
}

alias DxgiCreateSwapChainFn = fn HResult(void* self, void* pDevice, DxgiSwapChainDesc* pDesc, void** ppSwapChain);
struct DxgiFactoryVtbl
{
	void* queryInterface;
	void* addRef;
	void* release;
	void* setPrivateData;
	void* setPrivateDataInterface;
	void* getPrivateData;
	void* getParent;
	void* enumAdapters;
	void* makeWindowAssociation;
	void* getWindowAssociation;
	DxgiCreateSwapChainFn createSwapChain;
}
struct DxgiFactory { DxgiFactoryVtbl* lpVtbl; }

// public
fn int todo_fix_window_createSwapChain_DirectX(GfxWindow* win, DxgiFactory* pFactory, void* pDevice, void** swapchain)
{
	if (win == null || pFactory == null || pDevice == null || swapchain == null) return -1;

	DxgiSwapChainDesc swapChainDesc;
	swapChainDesc.bufferCount = 2;
	swapChainDesc.bufferDesc.width = (uint)win.width;
	swapChainDesc.bufferDesc.height = (uint)win.height;
	swapChainDesc.bufferDesc.format = DXGI_FORMAT_R8G8B8A8_UNORM;
	swapChainDesc.bufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
	swapChainDesc.outputWindow = win.src.window;
	swapChainDesc.sampleDesc.count = 1;
	swapChainDesc.sampleDesc.quality = 0;
	swapChainDesc.windowed = 1;
	swapChainDesc.flags = DXGI_SWAP_CHAIN_FLAG_ALLOW_MODE_SWITCH;

	HResult hr = pFactory.lpVtbl.createSwapChain(pFactory, pDevice, &swapChainDesc, swapchain);
	if (hr < 0)
	{

		todo_fix_debugCallback(TODO_RENAME_TYPE_ERROR, TODO_RENAME_ERR_DIRECT_X_CONTEXT, "Failed to create DirectX swap chain!");
		return -2;
	}
	return 0;
}



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



macro GfxPoint _param_to_point(iptr lParam)
{
	return { (int)(lParam & 0xffff), (int)((lParam >> 16) & 0xffff) };
}

// private
fn Win32_LRESULT _wnd_proc(Win32_HWND hWnd, Win32_UINT message, Win32_WPARAM wParam, Win32_LPARAM lParam)
{
	if (message == win32::WM_DISPLAYCHANGE) todo_fix_pollMonitors();

	if (hWnd == _gfx_ctx.helper_window) return win32::defWindowProcW(hWnd, message, wParam, lParam);

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
			bool inFocus = (wParam & 0xffff) != 0;
			_window_focus_callback(win, inFocus);
			return win32::defWindowProcW(hWnd, message, wParam, lParam);
		case win32::WM_MOVE:
			if (win.internal.capture_mouse) _window_set_capture_mouse_native(win, true);
			_window_moved_callback(win, _param_to_point(lParam));
			return win32::defWindowProcW(hWnd, message, wParam, lParam);
		case win32::WM_SIZE:
			if (win.internal.capture_mouse) _window_set_capture_mouse_native(win, true);
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
				_window_set_capture_mouse_native(win, win.internal.capture_mouse);
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
			MinMaxInfo* mmi = (MinMaxInfo*)(uptr)lParam;
			bool resize = win.src.min_size == win.src.max_size;
			win.internal.flags.no_resize = resize;

			mmi.ptMinTrackSize.x = win.src.min_size.x + (frame.right - frame.left);
			mmi.ptMinTrackSize.y = win.src.min_size.y + (frame.bottom - frame.top);
			if (win.src.max_size == { 0, 0 }) return win32::defWindowProcW(hWnd, message, wParam, lParam);

			mmi.ptMaxTrackSize.x = win.src.max_size.x + (frame.right - frame.left);
			mmi.ptMaxTrackSize.y = win.src.max_size.y + (frame.bottom - frame.top);
			return win32::defWindowProcW(hWnd, message, wParam, lParam);
		case win32::WM_ENTERSIZEMOVE:
		case win32::WM_EXITSIZEMOVE:
			if (win.src.actionFrame) _window_set_capture_mouse_native(win, win.internal.capture_mouse);
		case win32::WM_TIMER:
			_window_refresh_callback(win, { 0, 0, ...win.size });
		case win32::WM_NCLBUTTONDOWN:
			// workaround for half-second pause when starting to move window
			Win32_POINT point = {};
			if (win32::sendMessageA(win.src.window, win32::WM_NCHITTEST, wParam, lParam) != win32::HTCAPTION || !win32::getCursorPos(&point)) break;
			win32::screenToClient(win.src.window, &point);
			win32::postMessage(win.src.window, win32::WM_MOUSEMOVE, 0, (iptr)((uint)point.x | ((uint)point.y << 16)));
		case win32::WM_PAINT:
			Win32_RECT rect;
			if (!win32::getUpdateRect(hWnd, &rect, 0))
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
			if (!(win.internal.enabled_events & TODO_RENAME_KEY_RELEASED_FLAG)) return win32::defWindowProcW(hWnd, message, wParam, lParam);
			int scancode = (int)((lParam >> 16) & (0x0100 | 0xff));
			if (scancode == 0) scancode = (int)win32::mapVirtualKeyW((uint)wParam, 0 /* MAPVK_VK_TO_VSC */);
			switch (scancode)
			{
				case 0x54: scancode = 0x137;
				case 0x146: scancode = 0x45;
				case 0x136: scancode = 0x36;
				default: break;
			}
			GfxKey value = (GfxKey)todo_fix_apiKeyToRGFW((uint)scancode);
			if (wParam == 0x11 /* VK_CONTROL */) { value = ((lParam >> 16) & 0x0100) != 0 ? CONTROL_R : CONTROL_L; }
			_key_update_key_mods(win, win32::getKeyState(0x14) & 1 != 0, win32::getKeyState(0x90) & 1 != 0, win32::getKeyState(0x91) & 1 != 0);
			_key_callback(win, value, win.internal.mod, false, false);
		case win32::WM_KEYDOWN:
		case win32::WM_SYSKEYDOWN:
		{
			if (!(win.internal.enabled_events & TODO_RENAME_KEY_PRESSED_FLAG)) return win32::defWindowProcW(hWnd, message, wParam, lParam);
			int scancode = (int)((lParam >> 16) & (0x0100 | 0xff));
			if (scancode == 0) scancode = (int)win32::mapVirtualKeyW((uint)wParam, 0);
			switch (scancode)
			{
				case 0x54: scancode = 0x137;
				case 0x146: scancode = 0x45;
				case 0x136: scancode = 0x36;
				default: break;
			}
			GfxKey value = (GfxKey)todo_fix_apiKeyToRGFW((uint)scancode);
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
			if (!(win.internal.raw_mouse || _gfx_ctx.raw_mouse)) return win32::defWindowProcW(hWnd, message, wParam, lParam);

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
			if (!(win.internal.enabled_events & TODO_RENAME_DATA_DROP_FLAG)) return win32::defWindowProcW(hWnd, message, wParam, lParam);
			uint count = win32::dragQueryFileW(drop, 0xffffffff, null, 0);
			for (uint i = 0; i < count; i++)
			{
				uint length = win32::dragQueryFileW(drop, i, null, 0);
				if (length == 0) continue;
				ushort* buffer = (ushort*)malloc((sz)(length + 1) * 2);
				char* cbuffer = (char*)malloc((sz)length + 1);
				win32::dragQueryFileW(drop, i, buffer, length + 1);
				todo_fix_createUTF8FromWideStringWin32(buffer, cbuffer, length);
				_data_drop_callback(win, cbuffer, (sz)length + 1, FILE);
				free(buffer);
				free(cbuffer);
			}
			win32::dragFinish(drop);
		default:
			break;
	}

	return win32::defWindowProcW(hWnd, message, wParam, lParam);
}


// public
fn void _wait_for_event_native(int waitMS) { win32::msgWaitForMultipleObjects(0, null, 0, (uint)waitMS, 0x04FF /* QS_ALLINPUT */); }

ushort[8] todo_fix_wideDisplayName = {(ushort)'D', (ushort)'I', (ushort)'S', (ushort)'P', (ushort)'L', (ushort)'A', (ushort)'Y', 0};

// public


// public



// public
fn sz todo_fix_monitor_getGammaRampPtr(GfxMonitor* monitor, TodoRenameGammaRamp* ramp)
{
	ushort[768] values; // 3 x 256
	Win32_HDC dc = win32::createDCW(&todo_fix_wideDisplayName, (WString)&monitor.node.adapterName, null, null);
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
	Win32_HDC dc = win32::createDCW(&todo_fix_wideDisplayName, (ushort*)&monitor.node.adapterName, null, null);
	win32::setDeviceGammaRamp(dc, &values);
	win32::deleteDC(dc);
	return true;
}

// public
fn void todo_fix_win32GetMode(Win32_DEVMODEW* dm, GfxMonitorMode* mode)
{
	mode.size = { (int)dm.dmPelsWidth, (int)dm.dmPelsHeight };
	_split_bpp(dm.dmBitsPerPel, mode);
	switch (dm.dmDisplayFrequency)
	{
		case 119: case 59: case 29: mode.refresh_rate = ((float)(dm.dmDisplayFrequency + 1) * 1000.0f) / 1001.0f;
		default: mode.refresh_rate = (float)dm.dmDisplayFrequency;
	}
}

// public

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
	if (win32::enumDisplaySettingsW((ushort*)&adapter.deviceName, (uint)ENUM_CURRENT_SETTINGS, &dm) == 0) return;

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

	todo_fix_win32GetMode(&dm, &node.mon.mode);

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

	if (monitorInfo.dwFlags & MONITORINFOF_PRIMARY) _gfx_ctx.monitors.primary = node;
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
		if (!(adapter.stateFlags & DISPLAY_DEVICE_ACTIVE)) continue;

		Win32_DWORD deviceNum;
		for (deviceNum = 0;; deviceNum++)
		{
			Win32_DISPLAY_DEVICEW dd = { .cb = Win32_DISPLAY_DEVICEW::size };
			if (win32::enumDisplayDevicesW((WString)&adapter.deviceName, deviceNum, &dd, 0) == 0) break;
			if (!(dd.stateFlags & DISPLAY_DEVICE_ACTIVE)) continue;

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
fn bool _monitor_set_mode_native(GfxMonitor* mon, GfxMonitorMode* mode)
{
	Win32_DEVMODEW dm;
	dm.dmSize = (ushort)Win32_DEVMODEW::size;
	dm.dmFields |= DM_PELSWIDTH | DM_PELSHEIGHT;
	dm.dmPelsWidth = (uint)mode.size.x;
	dm.dmPelsHeight = (uint)mode.size.y;
	dm.dmFields |= DM_DISPLAYFREQUENCY;
	dm.dmDisplayFrequency = (uint)mode.refresh_rate;
	dm.dmFields |= DM_BITSPERPEL;
	dm.dmBitsPerPel = (uint)(mode.rgb.sum());

	if (win32::changeDisplaySettingsExW((WString)&mon.node.adapterName, &dm, null, CDS_TEST, null) == DISP_CHANGE_SUCCESSFUL)
	{
		if (win32::changeDisplaySettingsExW((WString)&mon.node.adapterName, &dm, null, CDS_UPDATEREGISTRY, null) == DISP_CHANGE_SUCCESSFUL)
		{
			todo_fix_win32GetMode(&dm, &mon.mode);
			return true;
		}
		return false;
	}
	return false;
}


// private
fn Win32_HICON todo_fix_loadHandleImage(char* data, int w, int h, GfxFormat format, int icon)
{
	BitmapV5Header bi;
	bi.size = BitmapV5Header::size;
	bi.width = w;
	bi.height = -h;
	bi.planes = 1;
	bi.bitCount = 32;
	bi.compression = BI_RGB;

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
	if (proc != null) { extensions = ((GetExtStringArbFn)proc)(todo_fix_wglGetCurrentDC()); }
	else if (proc2 != null) { extensions = ((GetExtStringExtFn)proc2)(); }
	return extensions && todo_fix_extensionSupportedStr(extensions, extension, len);
}

// public
fn void _window_make_opengl_current_context_native(GfxWindow* win)
{
	if (win == null) { todo_fix_wglMakeCurrent(null, null); }
	else { todo_fix_wglMakeCurrent(win.src.hdc, win.src.ctx.native.ctx); }
}

// public
fn void* _get_current_opengl_context_native() => todo_fix_wglGetCurrentContext();
// public
fn void _window_swap_opengl_buffers_native(GfxWindow* win)
{
	win32::swapBuffers(win.src.hdc);
}


// public


// public
