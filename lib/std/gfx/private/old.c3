module std::gfx @feat(MACOS) @link("Cocoa.framework", "CoreGraphics.framework", "ApplicationServices.framework", "QuartzCore.framework", "Carbon.framework", "CoreFoundation.framework", "objc");

import std::collections, std::math, libc, std::os::macos, std::io, std::os::darwin;
// C3 port of the shared RGFW implementation from /Users/lerno/Downloads/RGFW.h.
// This file intentionally imports no C3 standard-library modules. It only
// declares the small C runtime hooks RGFW's C header also expects from the host.

excuse NSWINDOW_CREATION_FAILED;


struct VkMetalSurfaceCreateInfoEXT { int sType; void* pNext; uint flags; void* pLayer; }
alias TodoRenameVkCreateMetalSurfaceEXTProc = fn VkResult(VkInstance instance, VkMetalSurfaceCreateInfoEXT* create_info, void* allocator, VkSurfaceKHR* surface);


// public

typedef UCKeyboardLayout = void;

extern fn char lmGetKbdType() @cname("LMGetKbdType");
extern fn int ucKeyTranslate(UCKeyboardLayout* layout, ushort virtualKeyCode, ushort keyAction, uint modifierKeyState, uint keyboardType, uint keyTranslateOptions, uint* deadKeyState, uint maxStringLength, uint* actualStringLength, ushort* unicodeString) @cname("UCKeyTranslate");
const uint UC_KEY_ACTION_DOWN = 0;
const uint UC_KEY_TRANSLATE_NO_DEAD_KEYS_BIT = 0;

// public
fn GfxKey _physical_to_mapped_key_native(GfxKey key)
{
	ushort keycode = (ushort)todo_fix_rgfwToApiKey(key);
	TISInputSourceRef source = macos::tisCopyCurrentKeyboardLayoutInputSource();
	if (!source) return key;

	CFDataRef layoutData = source.getProperty(macos::kTISPropertyUnicodeKeyLayoutData);
	defer source.release();
	if (!layoutData) return key;

	UCKeyboardLayout* layout = (UCKeyboardLayout*)layoutData.getBytePtr();

	uint deadKeyState = 0;
	ushort[4] chars;
	uint len = 0;
	uint type = (uint)lmGetKbdType();
	int status = ucKeyTranslate(layout, keycode, (ushort)UC_KEY_ACTION_DOWN, 0, type, UC_KEY_TRANSLATE_NO_DEAD_KEYS_BIT, &deadKeyState, 4, &len, &chars[0]);

	if (status == 0 && len == 1 && chars[0] < 256) return (GfxKey)chars[0];

	return key;
}

alias TodoRenameGetProcAddressFunc = fn void*(ZString procname);

// public
fn bool todo_fix_extensionSupported_base(ZString extension, sz len, TodoRenameGetProcAddressFunc getProcAddress)
{
	GfxFn todo_fix_gl_get_string = getProcAddress("glGetString");
	if (todo_fix_gl_get_string == null) return false;
	return false;
}

const int NS_OPEN_GL_PFA_DOUBLE_BUFFER = 5;
const int NS_OPEN_GL_PFA_AUX_BUFFERS = 7;
const int NS_OPEN_GL_PFA_COLOR_SIZE = 8;
const int NS_OPEN_GL_PFA_ALPHA_SIZE = 11;
const int NS_OPEN_GL_PFA_DEPTH_SIZE = 12;
const int NS_OPEN_GL_PFA_STENCIL_SIZE = 13;
const int NS_OPEN_GL_PFA_ACCUM_SIZE = 14;
const int NS_OPEN_GL_PFA_CLOSEST_POLICY = 74;
const int NS_OPEN_GL_PFA_ACCELERATED = 73;
const int NS_OPEN_GL_PFA_SAMPLE_BUFFERS = 55;
const int NS_OPEN_GL_PFA_SAMPLES = 56;
const int NS_OPEN_GL_PFA_RENDERER_ID = 70;
const int NS_OPEN_GL_PFA_OPEN_GL_PROFILE = 99;
const int NS_OPEN_GL_PROFILE_LEGACY = 0x1000;
const int NS_OPEN_GL_PROFILE_3_2_CORE = 0x3200;
const int NS_OPEN_GL_PROFILE_4_1_CORE = 0x3200;
const int NS_OPEN_GL_CP_SURFACE_OPACITY = 236;
const int NS_OPEN_GL_CONTEXT_PARAMETER_SWAP_INTERVAL = 222;
const int K_CGL_RENDERER_GENERIC_FLOAT_ID = 0x00020400;

fn void nsOpenGLContext_setValues(ObjId context, int* vals, NSInteger param)
{
	context.send_void("setValues:forParameter:", vals, param);
}


// public
fn bool todo_fix_extensionSupported_OpenGL(ZString extension, sz len)
{
	if (todo_fix_extensionSupported_base(extension, len, &_get_opengl_proc_native)) return true;
	return todo_fix_extensionSupportedPlatform_OpenGL(extension, len);
}

// public
fn void todo_fix_window_makeCurrentWindow_OpenGL(GfxWindow* win)
{
	if (win != null) _gfx_ctx.current = win;
	_window_make_opengl_current_context_native(win);
}

// public
fn GfxWindow* todo_fix_getCurrentWindow_OpenGL() => _gfx_ctx.current;

// public
fn bool todo_fix_extensionSupportedPlatform_OpenGL(ZString extension, sz len) => false;



const NSUInteger NS_BACKING_STORE_BUFFERED = 2;
const NSUInteger NS_APP_ACTIVATION_POLICY_REGULAR = 0;
const NSUInteger NS_EVENT_MASK_ANY = 0xffffffffffffffff;





fn ObjId ns_imageWithRep(ObjId rep, int w, int h)
{
	CGSize size;
	size.width = (CGFloat)w;
	size.height = (CGFloat)h;
	ObjId image = objc::alloc_by_name("NSImage").send(ObjId, "initWithSize:", size);
	if (rep) image.send_void("addRepresentation:", rep);
	return image;
}

fn CGRect ns_rect(double x, double y, double w, double h)
{
	CGRect rect;
	rect.origin.x = x; rect.origin.y = y;
	rect.size.width = w; rect.size.height = h;
	return rect;
}


// private
fn CGDirectDisplayID todo_fix_monitorDisplay(GfxMonitor* monitor)
{
	if (monitor != null && monitor.node != null && monitor.node.nativeId0 != 0) return (CGDirectDisplayID)monitor.node.nativeId0;
	return cg::mainDisplayID();
}

// public
fn void todo_fix_monitorNode_free(GfxMonitorNode* node)
{
	if (node.mon.mode.src) CFType.release(node.mon.mode.src);
}


fn TodoRenameMouse* todo_fix_createMouse(char* data, int w, int h, GfxFormat format)
{
	if (data == null) return null;
	ObjId rep = gfx::_new_nsbitmap_image_rep(w, h);
	defer objc::release(rep);
	char* pixels = rep.send(char*, "bitmapData");
	if (!pixels) return null;
	copy_image_data(pixels, w, h, RGBA8, data, format, null);
	ObjId image = ns_imageWithRep(rep, w, h);
	CGPoint hot;
	hot.x = 0.0; hot.y = 0.0;
	ObjId cursor = objc::alloc_by_name("NSCursor").send(ObjId, "initWithImage:hotSpot:", image, hot);
	objc::release(image);
	return (TodoRenameMouse*)cursor;
}

// public
fn TodoRenameMouse* todo_fix_createMouseStandard(GfxMouseIcon mouse)
{
	ObjClass cls = objc::class("NSCursor");
	if (!cls) return null;
	ObjId cursor = null;
	switch (mouse)
	{
		case NORMAL:        cursor = objc::send(cls, ObjId, "arrowCursor");
		case ARROW:         cursor = objc::send(cls, ObjId, "arrowCursor");
		case IBEAM:         cursor = objc::send(cls, ObjId, "IBeamCursor");
		case CROSSHAIR:     cursor = objc::send(cls, ObjId, "crosshairCursor");
		case POINTING_HAND: cursor = objc::send(cls, ObjId, "pointingHandCursor");
		case RESIZE_EW:     cursor = objc::send(cls, ObjId, "resizeLeftRightCursor");
		case RESIZE_E:      cursor = objc::send(cls, ObjId, "resizeLeftRightCursor");
		case RESIZE_W:      cursor = objc::send(cls, ObjId, "resizeLeftRightCursor");
		case RESIZE_NS:     cursor = objc::send(cls, ObjId, "resizeUpDownCursor");
		case RESIZE_N:      cursor = objc::send(cls, ObjId, "resizeUpDownCursor");
		case RESIZE_S:      cursor = objc::send(cls, ObjId, "resizeUpDownCursor");
		case RESIZE_NWSE:   cursor = objc::send(cls, ObjId, "_windowResizeNorthWestSouthEastCursor");
		case RESIZE_NW:     cursor = objc::send(cls, ObjId, "_windowResizeNorthWestSouthEastCursor");
		case RESIZE_SE:     cursor = objc::send(cls, ObjId, "_windowResizeNorthWestSouthEastCursor");
		case RESIZE_NESW:   cursor = objc::send(cls, ObjId, "_windowResizeNorthEastSouthWestCursor");
		case RESIZE_NE:     cursor = objc::send(cls, ObjId, "_windowResizeNorthEastSouthWestCursor");
		case RESIZE_SW:     cursor = objc::send(cls, ObjId, "_windowResizeNorthEastSouthWestCursor");
		case RESIZE_ALL:    cursor = objc::send(cls, ObjId, "openHandCursor");
		case NOT_ALLOWED:   cursor = objc::send(cls, ObjId, "operationNotAllowedCursor");
		case WAIT:          cursor = objc::send(cls, ObjId, "arrowCursor");
		case PROGRESS:      cursor = objc::send(cls, ObjId, "arrowCursor");
		default:            cursor = objc::send(cls, ObjId, "arrowCursor");
	}
	return (TodoRenameMouse*)objc::retain(cursor);
}




// public

// public
fn bool todo_fix_extensionSupported_EGL(ZString extension, sz len)
{
	if (todo_fix_extensionSupported_base(extension, len, &todo_fix_getProcAddress_EGL)) return true;
	return todo_fix_extensionSupportedPlatform_EGL(extension, len);
}

// public
fn void todo_fix_window_makeCurrentWindow_EGL(GfxWindow* win) { _gfx_ctx.current = win; }


struct WgpuSurfaceSourceMetalLayer { WgpuChainedStruct chain; ObjId layer; }

