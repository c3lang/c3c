module std::gfx @feat(MACOS) @link("Cocoa.framework", "CoreGraphics.framework", "ApplicationServices.framework", "QuartzCore.framework", "Carbon.framework", "CoreFoundation.framework", "objc");

import std::collections, std::math, libc, std::os::macos, std::io, std::os::darwin;
// C3 port of the shared RGFW implementation from /Users/lerno/Downloads/RGFW.h.
// This file intentionally imports no C3 standard-library modules. It only
// declares the small C runtime hooks RGFW's C header also expects from the host.

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


fn GfxMouse* todo_fix_createMouse(char* data, int w, int h, GfxFormat format)
{
	if (data == null) return null;
	ObjId rep = _new_nsbitmap_image_rep(w, h);
	defer objc::release(rep);
	char* pixels = rep.send(char*, "bitmapData");
	if (!pixels) return null;
	copy_image_data(pixels, w, h, RGBA8, data, format, null);
	ObjId image = _new_nsimage(w, h, rep);
	CGPoint hot;
	hot.x = 0.0; hot.y = 0.0;
	ObjId cursor = objc::alloc_by_name("NSCursor").send(ObjId, "initWithImage:hotSpot:", image, hot);
	objc::release(image);
	return (GfxMouse*)cursor;
}

// public
fn GfxMouse* _create_standard_mouse_native(GfxMouseIcon mouse)
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
	return (GfxMouse*)objc::retain(cursor);
}


// public
fn void todo_fix_window_makeCurrentWindow_EGL(GfxWindow* win) { _gfx_ctx.current = win; }



