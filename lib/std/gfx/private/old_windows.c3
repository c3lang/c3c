module std::gfx @feat(WIN32);
import std::collections::list, std::math, std::os::win32;

// public
fn void _wait_for_event_native(int waitMS) { win32::msgWaitForMultipleObjects(0, null, 0, (uint)waitMS, 0x04FF /* QS_ALLINPUT */); }




// private

fn void todo_fix_monitorNode_free(GfxMonitorNode* node) {}


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
