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
fn WGPUSurface todo_fix_window_createSurface_WebGPU(GfxWindow* window, WGPUInstance instance)
{
	WgpuSurfaceSourceWindowsHWND fromHwnd;
	fromHwnd.chain.next = null;
	fromHwnd.chain.sType = WGPU_STYPE_SURFACE_SOURCE_WINDOWS_HWND;
	fromHwnd.hwnd = window.src.window;
	fromHwnd.hinstance = _gfx_ctx.instance;
	WgpuSurfaceDescriptor surfaceDesc;
	surfaceDesc.nextInChain = (WgpuChainedStruct*)&fromHwnd;
	return wgpuInstanceCreateSurface(instance, &surfaceDesc);
}

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

fn void todo_fix_win32MakeWindowDarkMode(GfxWindow* win, bool state)
{
	int value = (state == true) ? 1 : 0;
	win32::dwmSetWindowAttribute(win.src.window,USE_IMMERSIVE_DARK_MODE, &value, (uint)int::size);
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
			todo_fix_windowFocusCallback(win, inFocus);
			return win32::defWindowProcW(hWnd, message, wParam, lParam);
		case win32::WM_MOVE:
			if (win.internal.capture_mouse) _window_set_capture_mouse_native(win, true);
			_window_moved_callback(win, _param_to_point(lParam));
			return win32::defWindowProcW(hWnd, message, wParam, lParam);
		case win32::WM_SIZE:
			if (win.internal.capture_mouse) _window_set_capture_mouse_native(win, true);
			_window_resized_callback(win, _param_to_point(lParam));
			todo_fix_window_checkMode(win);
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
			todo_fix_scaleUpdatedCallback(win, { scaleX, scaleY});
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
			todo_fix_windowRefreshCallback(win, { 0, 0, ...win.size });
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
				todo_fix_windowRefreshCallback(win, { rect.left, rect.top, rect.right - rect.left, rect.bottom - rect.top });
			}
			else
			{
				Win32_PAINTSTRUCT ps;
				win32::beginPaint(hWnd, &ps);
				todo_fix_windowRefreshCallback(win, { 0, 0, ...win.size });
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
			todo_fix_keyCharCallback(win, codepoint);
			return 0;
		case win32::WM_UNICHAR:
			if (wParam == 0xFFFF /* UNICODE_NOCHAR */) return 1;
			todo_fix_keyCharCallback(win, (uint)wParam);
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
			todo_fix_keyCallback(win, value, win.internal.mod, false, false);
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
			todo_fix_keyCallback(win, value, win.internal.mod, repeat, true);
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
		case 0x0201: todo_fix_mouseButtonCallback(win, LEFT, true);
		case 0x0204: todo_fix_mouseButtonCallback(win, RIGHT, true);
		case 0x0207: todo_fix_mouseButtonCallback(win, MIDDLE, true);
		case 0x020B: todo_fix_mouseButtonCallback(win, (GfxMouseButton)(GfxMouseButton.MISC1.ordinal + (((wParam >> 16) & 0xffff) == 2 ? 1 : 0)), true);
		case 0x0202: todo_fix_mouseButtonCallback(win, LEFT, false);
		case 0x0205: todo_fix_mouseButtonCallback(win, RIGHT, false);
		case 0x0208: todo_fix_mouseButtonCallback(win, MIDDLE, false);
		case 0x020C: todo_fix_mouseButtonCallback(win, (GfxMouseButton)(GfxMouseButton.MISC1.ordinal + (((wParam >> 16) & 0xffff) == 2 ? 1 : 0)), false);
		case win32::WM_MOUSEWHEEL:
			float scrollY = (float)((short)((wParam >> 16) & 0xffff)) / 120.0f; // WHEEL_DELTA
			todo_fix_mouseScrollCallback(win, { 0.0f, scrollY });
		case win32::WM_MOUSEHWHEEL:
			float scrollX = -(float)((short)((wParam >> 16) & 0xffff)) / 120.0f;
			todo_fix_mouseScrollCallback(win, { scrollX, 0.0f });
		case win32::WM_DROPFILES:
			void* drop = (void*)wParam;
			Win32_POINT pt;
			win32::dragQueryPoint(drop, &pt);
			todo_fix_dataDragCallback(win, FILE, MOVE, { pt.x, pt.y });
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
				todo_fix_dataDropCallback(win, cbuffer, (sz)length + 1, FILE);
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
fn void todo_fix_initKeycodesPlatform()
{
	_gfx_ctx.keycodes[0x00B] = NUM0; _gfx_ctx.keycodes[0x002] = NUM1; _gfx_ctx.keycodes[0x003] = NUM2;
	_gfx_ctx.keycodes[0x004] = NUM3; _gfx_ctx.keycodes[0x005] = NUM4; _gfx_ctx.keycodes[0x006] = NUM5;
	_gfx_ctx.keycodes[0x007] = NUM6; _gfx_ctx.keycodes[0x008] = NUM7; _gfx_ctx.keycodes[0x009] = NUM8;
	_gfx_ctx.keycodes[0x00A] = NUM9;
	_gfx_ctx.keycodes[0x01E] = A; _gfx_ctx.keycodes[0x030] = B; _gfx_ctx.keycodes[0x02E] = C;
	_gfx_ctx.keycodes[0x020] = D; _gfx_ctx.keycodes[0x012] = E; _gfx_ctx.keycodes[0x021] = F;
	_gfx_ctx.keycodes[0x022] = G; _gfx_ctx.keycodes[0x023] = H; _gfx_ctx.keycodes[0x017] = I;
	_gfx_ctx.keycodes[0x024] = J; _gfx_ctx.keycodes[0x025] = K; _gfx_ctx.keycodes[0x026] = L;
	_gfx_ctx.keycodes[0x032] = M; _gfx_ctx.keycodes[0x031] = N; _gfx_ctx.keycodes[0x018] = O;
	_gfx_ctx.keycodes[0x019] = P; _gfx_ctx.keycodes[0x010] = Q; _gfx_ctx.keycodes[0x013] = R;
	_gfx_ctx.keycodes[0x01F] = S; _gfx_ctx.keycodes[0x014] = T; _gfx_ctx.keycodes[0x016] = U;
	_gfx_ctx.keycodes[0x02F] = V; _gfx_ctx.keycodes[0x011] = W; _gfx_ctx.keycodes[0x02D] = X;
	_gfx_ctx.keycodes[0x015] = Y; _gfx_ctx.keycodes[0x02C] = Z;
	_gfx_ctx.keycodes[0x028] = APOSTROPHE; _gfx_ctx.keycodes[0x02B] = BACK_SLASH;
	_gfx_ctx.keycodes[0x033] = COMMA; _gfx_ctx.keycodes[0x00D] = EQUALS;
	_gfx_ctx.keycodes[0x029] = BACKTICK; _gfx_ctx.keycodes[0x01A] = BRACKET;
	_gfx_ctx.keycodes[0x00C] = MINUS; _gfx_ctx.keycodes[0x034] = PERIOD;
	_gfx_ctx.keycodes[0x01B] = CLOSE_BRACKET; _gfx_ctx.keycodes[0x027] = SEMICOLON;
	_gfx_ctx.keycodes[0x035] = SLASH; _gfx_ctx.keycodes[0x056] = WORLD2;
	_gfx_ctx.keycodes[0x00E] = BACK_SPACE; _gfx_ctx.keycodes[0x153] = DELETE;
	_gfx_ctx.keycodes[0x14F] = END; _gfx_ctx.keycodes[0x01C] = ENTER;
	_gfx_ctx.keycodes[0x001] = ESCAPE; _gfx_ctx.keycodes[0x147] = HOME;
	_gfx_ctx.keycodes[0x152] = INSERT; _gfx_ctx.keycodes[0x15D] = MENU;
	_gfx_ctx.keycodes[0x151] = PAGE_DOWN; _gfx_ctx.keycodes[0x149] = PAGE_UP;
	_gfx_ctx.keycodes[0x045] = PAUSE; _gfx_ctx.keycodes[0x039] = SPACE;
	_gfx_ctx.keycodes[0x00F] = TAB; _gfx_ctx.keycodes[0x03A] = CAPS_LOCK;
	_gfx_ctx.keycodes[0x145] = NUM_LOCK; _gfx_ctx.keycodes[0x046] = SCROLL_LOCK;
	_gfx_ctx.keycodes[0x03B] = F1; _gfx_ctx.keycodes[0x03C] = F2; _gfx_ctx.keycodes[0x03D] = F3;
	_gfx_ctx.keycodes[0x03E] = F4; _gfx_ctx.keycodes[0x03F] = F5; _gfx_ctx.keycodes[0x040] = F6;
	_gfx_ctx.keycodes[0x041] = F7; _gfx_ctx.keycodes[0x042] = F8; _gfx_ctx.keycodes[0x043] = F9;
	_gfx_ctx.keycodes[0x044] = F10; _gfx_ctx.keycodes[0x057] = F11; _gfx_ctx.keycodes[0x058] = F12;
	_gfx_ctx.keycodes[0x064] = F13; _gfx_ctx.keycodes[0x065] = F14; _gfx_ctx.keycodes[0x066] = F15;
	_gfx_ctx.keycodes[0x067] = F16; _gfx_ctx.keycodes[0x068] = F17; _gfx_ctx.keycodes[0x069] = F18;
	_gfx_ctx.keycodes[0x06A] = F19; _gfx_ctx.keycodes[0x06B] = F20; _gfx_ctx.keycodes[0x06C] = F21;
	_gfx_ctx.keycodes[0x06D] = F22; _gfx_ctx.keycodes[0x06E] = F23; _gfx_ctx.keycodes[0x076] = F24;
	_gfx_ctx.keycodes[0x038] = ALT_L; _gfx_ctx.keycodes[0x01D] = CONTROL_L; _gfx_ctx.keycodes[0x02A] = SHIFT_L;
	_gfx_ctx.keycodes[0x15B] = SUPER_L; _gfx_ctx.keycodes[0x137] = PRINT_SCREEN;
	_gfx_ctx.keycodes[0x138] = ALT_R; _gfx_ctx.keycodes[0x11D] = CONTROL_R;
	_gfx_ctx.keycodes[0x036] = SHIFT_R; _gfx_ctx.keycodes[0x15C] = SUPER_R;
	_gfx_ctx.keycodes[0x150] = DOWN; _gfx_ctx.keycodes[0x14B] = LEFT;
	_gfx_ctx.keycodes[0x14D] = RIGHT; _gfx_ctx.keycodes[0x148] = UP;
	_gfx_ctx.keycodes[0x052] = PAD0; _gfx_ctx.keycodes[0x04F] = PAD1; _gfx_ctx.keycodes[0x050] = PAD2;
	_gfx_ctx.keycodes[0x051] = PAD3; _gfx_ctx.keycodes[0x04B] = PAD4; _gfx_ctx.keycodes[0x04C] = PAD5;
	_gfx_ctx.keycodes[0x04D] = PAD6; _gfx_ctx.keycodes[0x047] = PAD7; _gfx_ctx.keycodes[0x048] = PAD8;
	_gfx_ctx.keycodes[0x049] = PAD9; _gfx_ctx.keycodes[0x04E] = PAD_PLUS; _gfx_ctx.keycodes[0x053] = PAD_PERIOD;
	_gfx_ctx.keycodes[0x135] = PAD_SLASH; _gfx_ctx.keycodes[0x11C] = PAD_RETURN;
	_gfx_ctx.keycodes[0x059] = PAD_EQUAL; _gfx_ctx.keycodes[0x037] = PAD_MULTIPLY; _gfx_ctx.keycodes[0x04A] = PAD_MINUS;
}

// public
fn int todo_fix_initPlatform(String class_name, GfxInitOption flags)
{
	win32::setProcessDPIAware();
	win32::timeBeginPeriod(1);

	_gfx_ctx.instance = win32::getModuleHandleW(null);
	@pool()
	{
		win32::multiByteToWideChar(65001, 0, (void*)class_name.zstr_copy(tmem), -1, (ushort*)&_gfx_ctx.wideClassName, 255);
	};

	_gfx_ctx.wndClass = {};
	_gfx_ctx.wndClass.lpszClassName = (WString)&_gfx_ctx.wideClassName;
	_gfx_ctx.wndClass.hInstance = _gfx_ctx.instance;
	_gfx_ctx.wndClass.hCursor = win32::loadCursorA(null, (Win32_LPCSTR)(uptr)32512 /* IDC_ARROW */);
	_gfx_ctx.wndClass.lpfnWndProc = &_wnd_proc;
	_gfx_ctx.wndClass.cbClsExtra = sz::size;

	_gfx_ctx.wndClass.hIcon = (Win32_HICON)win32::loadImageA(_gfx_ctx.instance, "C3GFX", 1 /* IMAGE_ICON */, 0, 0, 0x00000040 | 0x00008000 /* LR_DEFAULTSIZE|LR_SHARED */);
	if (_gfx_ctx.wndClass.hIcon == null)
	{
		_gfx_ctx.wndClass.hIcon = (Win32_HICON)win32::loadImageA(null, (Win32_LPCSTR)(uptr)32512 /* IDI_APPLICATION */, 1, 0, 0, 0x00000040 | 0x00008000);
	}

	win32::registerClassW(&_gfx_ctx.wndClass);
	_gfx_ctx.helper_window = win32::createWindowExW(0, _gfx_ctx.wndClass.lpszClassName, null, 0, 0, 0, 0, 0, null, null, _gfx_ctx.instance, null);

	char[4] blk;
	_gfx_ctx.hiddenMouse = todo_fix_createMouse(&blk, 1, 1, RGBA8);
	return 0;
}

// public
fn void? todo_fix_createWindowPlatform(String name, GfxWindowFlags flags, GfxWindow* win) => @pool()
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
	uint style = _winapi_window_get_style(flags);
	uint exStyle = _winapi_window_get_style_ex(win, flags);
	win32::adjustWindowRectEx(&rect, style, 0, exStyle);

	win.src.window = win32::createWindowExW(0, _gfx_ctx.wndClass.lpszClassName, name.to_wstring(tmem)!!, window_style, win.x + rect.left, win.y + rect.top, rect.right - rect.left, rect.bottom - rect.top, null, null, _gfx_ctx.instance, null);
	win32::setPropW(win.src.window, RENAME_PROP_NAME, win);
	win.resize(win.size);

	if (flags.allow_dnd)
	{
		win.internal.flags.allow_dnd = true;
		win.set_dnd(true);
	}
	win.src.hdc = win32::getDC(win.src.window);

	todo_fix_win32MakeWindowDarkMode(win, _get_dark_mode_state());
	_make_window_transparent(win);
}

// public
fn void _window_set_border_native(GfxWindow* win, bool border)
{
	win.internal.flags.no_border = !border;
	Win32_RECT rect;
	win32::getClientRect(win.src.window, &rect);
	Win32_LONG style = win32::getWindowLongW(win.src.window, win32::GWL_STYLE);
	style |= (Win32_LONG)_winapi_window_get_style(win.internal.flags);

	if (!border) { style &= ~(Win32_LONG)0x00CF0000; } // ~WS_OVERLAPPEDWINDOW
	else
	{
		if (win.internal.flags.no_resize) style &= ~(Win32_LONG)win32::WS_MAXIMIZEBOX;
		style |= win32::WS_OVERLAPPEDWINDOW;
	}

	Win32_DWORD exStyle = _winapi_window_get_style_ex(win, win.internal.flags);
	win32::clientToScreen(win.src.window, (Win32_POINT*)&rect.left);
	win32::clientToScreen(win.src.window, (Win32_POINT*)&rect.right);
	win32::adjustWindowRectEx(&rect, (Win32_DWORD)style, 0, exStyle);
	win32::setWindowLongW(win.src.window, -16, style);
	win32::setWindowPos(win.src.window, win32::HWND_TOP, rect.left, rect.top, rect.right - rect.left, rect.bottom - rect.top, 0x0020 | 0x0010 | 0x0004); // SWP_FRAMECHANGED|NOACTIVATE|NOZORDER
}


// public

// public
fn void _window_set_aspect_ratio_native(GfxWindow* win, int w, int h) @private
{
	win.src.aspectRatioW = w;
	win.src.aspectRatioH = h;
}
// public
// public
fn void _window_set_max_size_native(GfxWindow* win, GfxSize size)
{
	win.src.max_size = size;
}

// public
// public
fn void todo_fix_window_raise(GfxWindow* win)
{
	win32::bringWindowToTop(win.src.window);
	win32::setWindowPos(win.src.window, null, win.x, win.y, win.width, win.height, 0x0001 | 0x0002 | 0x0200 | 0x0020); // |NOMOVE|NOOWNERZORDER|FRAMECHANGED
}

// public

// public
fn void _window_maximize_native(GfxWindow* win)
{
	win.hide();
	win32::showWindow(win.src.window, win32::SW_MAXIMIZE);
}
// public
fn void todo_fix_window_minimizePlatform(GfxWindow* win)
{
	win32::showWindow(win.src.window, win32::SW_MINIMIZE);
}
// public
fn void todo_fix_window_setFloating(GfxWindow* win, bool floating)
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
// public
fn void todo_fix_window_setOpacity(GfxWindow* win, char opacity)
{
	win32::setWindowLongA(win.src.window, -20, 0x00080000);
	win32::setLayeredWindowAttributes(win.src.window, 0, opacity, 2);
}
// public
fn void _window_restore_native(GfxWindow* win) {}
// public
// public
fn void todo_fix_stopCheckEvents()
{
	win32::postMessageW(_gfx_ctx.helper_window, 0, 0, 0);
}
// public
fn void todo_fix_waitForEvent(int waitMS) { win32::msgWaitForMultipleObjects(0, null, 0, (uint)waitMS, 0x04FF /* QS_ALLINPUT */); }

ushort[8] todo_fix_wideDisplayName = {(ushort)'D', (ushort)'I', (ushort)'S', (ushort)'P', (ushort)'L', (ushort)'A', (ushort)'Y', 0};

struct TodoRenameAttribStack
{
	int* attribs;
	sz count;
	sz max;
}

// public
fn GfxKey todo_fix_physicalToMappedKey(GfxKey key)
{
	uint vsc = todo_fix_rgfwToApiKey(key);
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


// public
fn void todo_fix_pollEvents()
{
	todo_fix_resetPrevState();
	Win32_MSG msg;
	while (win32::peekMessageA(&msg, null, 0, 0, win32::PM_REMOVE))
	{
		win32::translateMessage(&msg);
		win32::dispatchMessageA(&msg);
	}
}



// public
fn bool todo_fix_monitor_getWorkarea(GfxMonitor* monitor, int* x, int* y, int* width, int* height)
{
	Win32_MONITORINFOEXW mi;
	mi.cbSize = Win32_MONITORINFOEXW::size;
	win32::getMonitorInfoW(monitor.node.hMonitor, &mi);
	if (x != null) *x = mi.rcWork.left;
	if (y != null) *y = mi.rcWork.top;
	if (width != null) *width = mi.rcWork.right - mi.rcWork.left;
	if (height != null) *height = mi.rcWork.bottom - mi.rcWork.top;
	return true;
}

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
	todo_fix_splitBPP(dm.dmBitsPerPel, mode);
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
	todo_fix_monitorCallback(_gfx_ctx.root, &node.mon, true);
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
fn GfxMonitor* find_monitor(GfxWindow* win)
{
	Win32_HMONITOR src = win32::monitorFromWindow(win.src.window, 1 /* MONITOR_DEFAULTTOPRIMARY */);
	for (GfxMonitorNode* node = _gfx_ctx.monitors.list.head; node != null; node = node.next)
	{
		if (node.hMonitor == src) return &node.mon;
	}
	return get_primary_monitor() ?? null;
}

// public
fn bool todo_fix_monitor_setMode(GfxMonitor* mon, GfxMonitorMode* mode)
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

// public
fn bool todo_fix_monitor_requestMode(GfxMonitor* mon, GfxMonitorMode* mode, GfxMonitorModeRequest request)
{
	Win32_DEVMODEW dm = { .dmSize = Win32_DEVMODEW::size };
	uint index = 0;
	for (;;)
	{
		if (win32::enumDisplaySettingsW((ushort*)&mon.node.adapterName, index, &dm) == 0) break;
		index += 1;

		if (request & SCALE) { dm.dmFields |= DM_PELSWIDTH | DM_PELSHEIGHT; dm.dmPelsWidth = (uint)mode.size.x; dm.dmPelsHeight = (uint)mode.size.y; }
		if (request & REFRESH) { dm.dmFields |= DM_DISPLAYFREQUENCY; dm.dmDisplayFrequency = (uint)mode.refresh_rate; }
		if (request & RGB) { dm.dmFields |= DM_BITSPERPEL; dm.dmBitsPerPel = (uint)(mode.rgb.sum()); }

		if (win32::changeDisplaySettingsExW((ushort*)&mon.node.adapterName, &dm, null, CDS_TEST, null) == DISP_CHANGE_SUCCESSFUL)
		{
			if (win32::changeDisplaySettingsExW((ushort*)&mon.node.adapterName, &dm, null, CDS_UPDATEREGISTRY, null) == DISP_CHANGE_SUCCESSFUL)
			{
				todo_fix_win32GetMode(&dm, &mon.mode);
				return true;
			}
			return false;
		}
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
fn void todo_fix_window_closePlatform(GfxWindow* win)
{
	win32::removePropW(win.src.window, RENAME_PROP_NAME);
	win32::releaseDC(win.src.window, win.src.hdc);
	win32::destroyWindow(win.src.window);
	if (win.src.hIconSmall) win32::destroyIcon(win.src.hIconSmall);
	if (win.src.hIconBig) win32::destroyIcon(win.src.hIconBig);
}

// public
fn void _window_move_native(GfxWindow* win, GfxPoint where)
{
	win.origin = where;
	win32::setWindowPos(win.src.window, (Win32_HWND){}, ...where, 0, 0, 0x0001 /* SWP_NOSIZE */);
}

// public

// public
fn void GfxWindow.set_name(&self, String name)
{
	win32::setWindowTextW(self.src.window, name.to_wstring(tmem)!!);
}

// public
fn void todo_fix_window_setMousePassthrough(GfxWindow* win, bool passthrough)
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
fn void todo_fix_window_makeCurrentContext_OpenGL(GfxWindow* win)
{
	if (win == null) { todo_fix_wglMakeCurrent(null, null); }
	else { todo_fix_wglMakeCurrent(win.src.hdc, win.src.ctx.native.ctx); }
}

// public
fn void* todo_fix_getCurrentContext_OpenGL() => todo_fix_wglGetCurrentContext();
// public
fn void todo_fix_window_swapBuffers_OpenGL(GfxWindow* win)
{
	win32::swapBuffers(win.src.hdc);
}

// public
fn void? GfxSurface.init(&self, void* data, int w, int h, GfxFormat format, GfxWindow* window = null)
{
	self.data = data;
	self.size = { w, h };
	self.format = format;

	BitmapV5Header bi;
	bi.size = BitmapV5Header::size;
	bi.width = w;
	bi.height = -h;
	bi.planes = 1;
	bi.bitCount = (ushort)format.channels * 8;
	bi.compression = BI_RGB;
	self.native.format = format.channels > 3 ? BGRA8 : BGR8;
	self.native.bitmap = win32::createDIBSection(_gfx_ctx.root.src.hdc, &bi, 0, (void**)&self.native.buffer, null, 0);

	if (self.native.bitmap == null)
	{
		log::error("Failed to create DIB section.", category: log::CATEGORY_VIDEO);
		return SURFACE_CREATION_FAILED~;
	}
	defer catch self.release();
	self.native.hdcMem = win32::createCompatibleDC(_gfx_ctx.root.src.hdc);
	win32::selectObject(self.native.hdcMem, self.native.bitmap);
}

// public
fn void GfxSurface.release(&self)
{
	if (!self.data) return;
	win32::deleteDC(self.native.hdcMem);
	win32::deleteObject(self.native.bitmap);
	*self = {};
}

// public
fn void todo_fix_window_blitSurface(GfxWindow* win, GfxSurface* surface)
{
	GfxPoint blit = math::min(win.size, surface.size);
	copy_image_data(surface.native.buffer, surface.size.x, blit.y, surface.native.format, surface.data, surface.format, surface.convert_fn);
	win32::bitBlt(win.src.hdc, 0, 0, ...blit, surface.native.hdcMem, 0, 0, 0x00CC0020 /* SRCCOPY */);
}
