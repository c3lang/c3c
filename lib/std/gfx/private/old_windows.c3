module std::gfx @feat(WIN32);
import std::collections::list, std::math, std::os::win32;

// public
fn void _wait_for_event_native(int waitMS)
{
	win32::msgWaitForMultipleObjects(0, null, 0, (uint)waitMS, 0x04FF /* QS_ALLINPUT */);
}


// private

fn void todo_fix_monitorNode_free(GfxMonitorNode* node)
{}


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



alias GetExtStringArbFn = fn ZString(Win32_HDC*);
alias GetExtStringExtFn = fn ZString();
alias GlxGetProcAddressProc = fn GfxFn(ZString);

// public
fn bool todo_fix_extensionSupportedPlatform_OpenGL(ZString extension, sz len)
{
	ZString extensions = null;
	GfxFn proc = _get_opengl_proc_native("wglGetExtensionsStringARB");
	GfxFn proc2 = _get_opengl_proc_native("wglGetExtensionsStringEXT");
	if (proc != null)
	{
		extensions = ((GetExtStringArbFn)proc)(wgl_get_current_dc_fn());
	}
	else if (proc2 != null)
	{
		extensions = ((GetExtStringExtFn)proc2)();
	}
	return extensions && todo_fix_extensionSupportedStr(extensions, extension, len);
}


