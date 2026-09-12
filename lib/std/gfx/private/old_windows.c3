module std::gfx @feat(WIN32);
import std::collections::list, std::math, std::os::win32;

fn void todo_fix_monitorNode_free(GfxMonitorNode* node)
{}

// public
fn GfxMouse* _create_standard_mouse_native(GfxMouseIcon mouse)
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
	return (GfxMouse*)win32::loadCursorA(null, (Win32_LPCSTR)mouseIcon);
}

// public
fn GfxMouse* todo_fix_createMouse(char* data, int w, int h, GfxFormat format) => (GfxMouse*)_load_handle_image(data, w, h, format, 0);


alias GetExtStringArbFn = fn ZString(Win32_HDC*);
alias GetExtStringExtFn = fn ZString();
alias GlxGetProcAddressProc = fn GfxFn(ZString);


