module std::gfx @feat(MACOS) @link("Cocoa.framework", "CoreGraphics.framework", "ApplicationServices.framework", "QuartzCore.framework", "Carbon.framework", "CoreFoundation.framework", "objc");

import std::collections, std::math, libc, std::os::macos, std::io, std::os::darwin;
// C3 port of the shared RGFW implementation from /Users/lerno/Downloads/RGFW.h.
// This file intentionally imports no C3 standard-library modules. It only
// declares the small C runtime hooks RGFW's C header also expects from the host.

excuse NSWINDOW_CREATION_FAILED;


alias CFBundleRef = void*;

struct VkMetalSurfaceCreateInfoEXT { int sType; void* pNext; uint flags; void* pLayer; }
alias TodoRenameVkCreateMetalSurfaceEXTProc = fn VkResult(VkInstance instance, VkMetalSurfaceCreateInfoEXT* create_info, void* allocator, VkSurfaceKHR* surface);

extern fn void objc_msgSendRaw(...) @cname("objc_msgSend") @builtin;
extern fn ObjSel sel_registerName(ZString name);
extern fn ObjClass objc_allocateClassPair(ObjClass superclass, ZString name, sz extraBytes);
extern fn void* object_setInstanceVariable(ObjId obj, ZString name, void* value);
extern fn void* object_getInstanceVariable(ObjId obj, ZString name, void* outValue);

extern fn CFStringRef cfStringCreateWithCString(void* alloc, ZString str, uint encoding) @cname("CFStringCreateWithCString");
extern fn CFBundleRef cfBundleGetBundleWithIdentifier(CFStringRef bundleID) @cname("CFBundleGetBundleWithIdentifier");
extern fn void* cfBundleGetFunctionPointerForName(CFBundleRef bundle, CFStringRef functionName) @cname("CFBundleGetFunctionPointerForName");

macro objc_send(id, $FunctionType, ZString $selector, ...)
{
	return (($FunctionType)&objc_msgSendRaw)((ObjId)id, sel_registerName($selector), ...$vaarg);
}

alias ObjcSendId = fn ObjId(ObjId, ObjSel);
alias ObjcSendCharPtr = fn ZString(ObjId, ObjSel);
alias ObjcSendVoidId = fn void(ObjId, ObjSel, ObjId);

alias ObjcSendVoidNSUInteger = fn void(ObjId, ObjSel, NSUInteger);
alias ObjcSendVoidNSInteger = fn void(ObjId, ObjSel, NSInteger);
alias ObjcSendVoidCGFloat = fn void(ObjId, ObjSel, CGFloat);
alias ObjcSendVoidSize = fn void(ObjId, ObjSel, CGSize);
alias ObjcSendVoidPoint = fn void(ObjId, ObjSel, CGPoint);

alias ObjcSendIdRect = fn ObjId(ObjId, ObjSel, CGRect);
alias ObjcSendIdSize = fn ObjId(ObjId, ObjSel, CGSize);
alias ObjcSendBitmapRepInit = fn ObjId(ObjId, ObjSel, ZString*, NSInteger, NSInteger, NSInteger, NSInteger, bool, bool, ObjId, NSUInteger, NSInteger, NSInteger);
alias ObjcSendIdEvent = fn ObjId(ObjId, ObjSel, NSUInteger, ObjId, ObjId, bool);
alias ObjcSendIdDouble = fn ObjId(ObjId, ObjSel, double);
alias ObjcSendVoidIdBool = fn void(ObjId, ObjSel, ObjId, bool);
alias ObjcSendIdWindowInit = fn ObjId(ObjId, ObjSel, CGRect, NSUInteger, NSUInteger, bool);
alias ObjcSendIdU32Ptr = fn ObjId(ObjId, ObjSel, uint*);
alias ObjcSendIdIdId = fn ObjId(ObjId, ObjSel, ObjId, ObjId);
alias ObjcSendPoint = fn CGPoint(ObjId, ObjSel);
alias ObjcSendRect = fn CGRect(ObjId, ObjSel);
alias ObjcSendRectRect = fn CGRect(ObjId, ObjSel, CGRect);
alias ObjcSendBool = fn bool(ObjId, ObjSel);
alias ObjcSendBoolId = fn bool(ObjId, ObjSel, ObjId);
alias ObjcSendIntegerNSUInteger = fn NSInteger(ObjId, ObjSel, NSUInteger);
alias ObjcSendInteger = fn NSInteger(ObjId, ObjSel);
alias ObjcSendCGFloat = fn CGFloat(ObjId, ObjSel);
alias ObjcSendUShort = fn ushort(ObjId, ObjSel);
alias ObjcSendNSUInteger = fn NSUInteger(ObjId, ObjSel);
alias ObjcSendVoidIntPtrNSInteger = fn void(ObjId, ObjSel, int*, NSInteger);




// public

typedef UCKeyboardLayout = void;

extern fn char lmGetKbdType() @cname("LMGetKbdType");
extern fn int ucKeyTranslate(UCKeyboardLayout* layout, ushort virtualKeyCode, ushort keyAction, uint modifierKeyState, uint keyboardType, uint keyTranslateOptions, uint* deadKeyState, uint maxStringLength, uint* actualStringLength, ushort* unicodeString) @cname("UCKeyTranslate");
const uint UC_KEY_ACTION_DOWN = 0;
const uint UC_KEY_TRANSLATE_NO_DEAD_KEYS_BIT = 0;

// public
fn GfxKey todo_fix_physicalToMappedKey(GfxKey key)
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

alias TodoRenameGetProcAddressFunc = fn GfxFn(ZString procname);

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
	objc_send(context, ObjcSendVoidIntPtrNSInteger, "setValues:forParameter:", vals, param);
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
	_window_make_opengl_current_context_platform(win);
}

// public
fn GfxWindow* todo_fix_getCurrentWindow_OpenGL() => _gfx_ctx.current;

// public
fn bool todo_fix_extensionSupportedPlatform_OpenGL(ZString extension, sz len) => false;

// public
fn GfxFn _get_opengl_proc_native(ZString procname)
{
	if (!_gfx_ctx.nativeGL_handle || !procname) return null;
	CFStringRef symbol_name = cfStringCreateWithCString(null, procname, 0x0600);
	defer { if (symbol_name) symbol_name.release(); }
	return cfBundleGetFunctionPointerForName((CFBundleRef)_gfx_ctx.nativeGL_handle, symbol_name);
}




const NSUInteger NS_BACKING_STORE_BUFFERED = 2;
const NSUInteger NS_APP_ACTIVATION_POLICY_REGULAR = 0;
const NSUInteger NS_EVENT_MASK_ANY = 0xffffffffffffffff;



fn ObjId ns_bitmapRep(int w, int h)
{
	return objc_send(objc::alloc_by_name("NSBitmapImageRep"), ObjcSendBitmapRepInit,
		"initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bitmapFormat:bytesPerRow:bitsPerPixel:",
		(ZString*)null, (NSInteger)w, (NSInteger)h, 8, 4, true, false, darwin::string("NSCalibratedRGBColorSpace"), (NSUInteger)2, (NSInteger)(w * 4), 32);
}

fn char* ns_bitmapData(ObjId rep) => rep ? objc_send(rep, ObjcSendCharPtr, "bitmapData") : null;

fn ObjId ns_imageWithRep(ObjId rep, int w, int h)
{
	CGSize size;
	size.width = (CGFloat)w;
	size.height = (CGFloat)h;
	ObjId image = objc::send(objc::alloc_by_name("NSImage"), ObjId, "initWithSize:", size);
	if (rep) objc::send_void(image, "addRepresentation:", rep);
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

// private
fn GfxWindow* todo_fix_osxDelegateWindow(ObjId self)
{
	void* out = null;
	object_getInstanceVariable(self, "gfx_window", &out);
	return (GfxWindow*)out;
}



// private
fn GfxWindow* todo_fix_osxViewWindow(ObjId self)
{
	void* out = null;
	object_getInstanceVariable(self, "gfx_window", &out);
	return (GfxWindow*)out;
}


// private

// public
fn void todo_fix_monitorNode_free(GfxMonitorNode* node)
{
	if (node.mon.mode.src) CFType.release(node.mon.mode.src);
}

// public
fn void todo_fix_initKeycodesPlatform()
{
	_gfx_ctx.keycodes[0x00] = A;
	_gfx_ctx.keycodes[0x01] = S;
	_gfx_ctx.keycodes[0x02] = D;
	_gfx_ctx.keycodes[0x03] = F;
	_gfx_ctx.keycodes[0x04] = H;
	_gfx_ctx.keycodes[0x05] = G;
	_gfx_ctx.keycodes[0x06] = Z;
	_gfx_ctx.keycodes[0x07] = X;
	_gfx_ctx.keycodes[0x08] = C;
	_gfx_ctx.keycodes[0x09] = V;
	_gfx_ctx.keycodes[0x0B] = B;
	_gfx_ctx.keycodes[0x0C] = Q;
	_gfx_ctx.keycodes[0x0D] = W;
	_gfx_ctx.keycodes[0x0E] = E;
	_gfx_ctx.keycodes[0x0F] = R;
	_gfx_ctx.keycodes[0x10] = Y;
	_gfx_ctx.keycodes[0x11] = T;
	_gfx_ctx.keycodes[0x12] = NUM1;
	_gfx_ctx.keycodes[0x13] = NUM2;
	_gfx_ctx.keycodes[0x14] = NUM3;
	_gfx_ctx.keycodes[0x15] = NUM4;
	_gfx_ctx.keycodes[0x16] = NUM6;
	_gfx_ctx.keycodes[0x17] = NUM5;
	_gfx_ctx.keycodes[0x18] = EQUAL;
	_gfx_ctx.keycodes[0x19] = NUM9;
	_gfx_ctx.keycodes[0x1A] = NUM7;
	_gfx_ctx.keycodes[0x1B] = MINUS;
	_gfx_ctx.keycodes[0x1C] = NUM8;
	_gfx_ctx.keycodes[0x1D] = NUM0;
	_gfx_ctx.keycodes[0x1E] = CLOSE_BRACKET;
	_gfx_ctx.keycodes[0x1F] = O;
	_gfx_ctx.keycodes[0x20] = U;
	_gfx_ctx.keycodes[0x21] = BRACKET;
	_gfx_ctx.keycodes[0x22] = I;
	_gfx_ctx.keycodes[0x23] = P;
	_gfx_ctx.keycodes[0x24] = RETURN;
	_gfx_ctx.keycodes[0x25] = L;
	_gfx_ctx.keycodes[0x26] = J;
	_gfx_ctx.keycodes[0x27] = APOSTROPHE;
	_gfx_ctx.keycodes[0x28] = K;
	_gfx_ctx.keycodes[0x29] = SEMICOLON;
	_gfx_ctx.keycodes[0x2A] = BACK_SLASH;
	_gfx_ctx.keycodes[0x2B] = COMMA;
	_gfx_ctx.keycodes[0x2C] = SLASH;
	_gfx_ctx.keycodes[0x2D] = N;
	_gfx_ctx.keycodes[0x2E] = M;
	_gfx_ctx.keycodes[0x2F] = PERIOD;
	_gfx_ctx.keycodes[0x31] = SPACE;
	_gfx_ctx.keycodes[0x32] = BACKTICK;
	_gfx_ctx.keycodes[0x33] = BACK_SPACE;
	_gfx_ctx.keycodes[0x35] = ESCAPE;
	_gfx_ctx.keycodes[0x30] = TAB;
	_gfx_ctx.keycodes[0x7B] = LEFT;
	_gfx_ctx.keycodes[0x7C] = RIGHT;
	_gfx_ctx.keycodes[0x7D] = DOWN;
	_gfx_ctx.keycodes[0x7E] = UP;
	_gfx_ctx.keycodes[0x38] = SHIFT_L;
	_gfx_ctx.keycodes[0x3B] = CONTROL_L;
	_gfx_ctx.keycodes[0x3A] = ALT_L;
	_gfx_ctx.keycodes[0x37] = SUPER_L;
	_gfx_ctx.keycodes[0x3C] = SHIFT_R;
	_gfx_ctx.keycodes[0x3E] = CONTROL_R;
	_gfx_ctx.keycodes[0x3D] = ALT_R;
	_gfx_ctx.keycodes[0x36] = SUPER_R;
	_gfx_ctx.keycodes[0x7A] = F1;
	_gfx_ctx.keycodes[0x78] = F2;
	_gfx_ctx.keycodes[0x63] = F3;
	_gfx_ctx.keycodes[0x76] = F4;
	_gfx_ctx.keycodes[0x60] = F5;
	_gfx_ctx.keycodes[0x61] = F6;
	_gfx_ctx.keycodes[0x62] = F7;
	_gfx_ctx.keycodes[0x64] = F8;
	_gfx_ctx.keycodes[0x65] = F9;
	_gfx_ctx.keycodes[0x6D] = F10;
	_gfx_ctx.keycodes[0x67] = F11;
	_gfx_ctx.keycodes[0x6F] = F12;
	_gfx_ctx.keycodes[0x69] = PRINT_SCREEN;
	_gfx_ctx.keycodes[0x6B] = F14;
	_gfx_ctx.keycodes[0x71] = F15;
	_gfx_ctx.keycodes[0x6A] = F16;
	_gfx_ctx.keycodes[0x40] = F17;
	_gfx_ctx.keycodes[0x4F] = F18;
	_gfx_ctx.keycodes[0x50] = F19;
	_gfx_ctx.keycodes[0x5A] = F20;
	_gfx_ctx.keycodes[0x0A] = WORLD1;
	_gfx_ctx.keycodes[0x39] = CAPS_LOCK;
	_gfx_ctx.keycodes[0x75] = DELETE;
	_gfx_ctx.keycodes[0x77] = END;
	_gfx_ctx.keycodes[0x73] = HOME;
	_gfx_ctx.keycodes[0x72] = INSERT;
	_gfx_ctx.keycodes[0x6E] = MENU;
	_gfx_ctx.keycodes[0x47] = NUM_LOCK;
	_gfx_ctx.keycodes[0x79] = PAGE_DOWN;
	_gfx_ctx.keycodes[0x74] = PAGE_UP;
	_gfx_ctx.keycodes[0x52] = PAD0;
	_gfx_ctx.keycodes[0x53] = PAD1;
	_gfx_ctx.keycodes[0x54] = PAD2;
	_gfx_ctx.keycodes[0x55] = PAD3;
	_gfx_ctx.keycodes[0x56] = PAD4;
	_gfx_ctx.keycodes[0x57] = PAD5;
	_gfx_ctx.keycodes[0x58] = PAD6;
	_gfx_ctx.keycodes[0x59] = PAD7;
	_gfx_ctx.keycodes[0x5B] = PAD8;
	_gfx_ctx.keycodes[0x5C] = PAD9;
	_gfx_ctx.keycodes[0x45] = PAD_SLASH;
	_gfx_ctx.keycodes[0x41] = PAD_PERIOD;
	_gfx_ctx.keycodes[0x4B] = PAD_SLASH;
	_gfx_ctx.keycodes[0x4C] = PAD_RETURN;
	_gfx_ctx.keycodes[0x51] = PAD_EQUAL;
	_gfx_ctx.keycodes[0x43] = PAD_MULTIPLY;
	_gfx_ctx.keycodes[0x4E] = PAD_MINUS;
}

// public
fn void todo_fix_pollEvents()
{
	todo_fix_resetPrevState();
	if (!_gfx_ctx.ns_app) return;
	ObjId distantPast = objc_send(objc::@class("NSDate"), ObjcSendId, "distantPast");
	ObjId mode = darwin::string("kCFRunLoopDefaultMode");
	while (true)
	{
		ObjId ev = objc::send(_gfx_ctx.ns_app, ObjId, "nextEventMatchingMask:untilDate:inMode:dequeue:", NSEventMask.ANY, distantPast, mode, true);
		if (!ev) break;
		objc_send(_gfx_ctx.ns_app, ObjcSendVoidId, "sendEvent:", ev);
	}
}


// public
fn void todo_fix_stopCheckEvents() { _gfx_ctx.stopCheckEvents_bool = true; }
// public
fn void todo_fix_waitForEvent(int waitMS)
{
	if (!_gfx_ctx.ns_app) return;
	ObjId date = objc_send(objc::@class("NSDate"), ObjcSendIdDouble, "dateWithTimeIntervalSinceNow:", (double)waitMS / 1000.0);
	ObjId mode = darwin::string("kCFRunLoopDefaultMode");
	ObjId ev = objc::send(_gfx_ctx.ns_app, ObjId, "nextEventMatchingMask:untilDate:inMode:dequeue:", NSEventMask.ANY, date, mode, true);
	if (ev) objc::send_void(_gfx_ctx.ns_app, "postEvent:atStart:", ev, true);
}
// public
fn bool todo_fix_usingWayland() => _gfx_ctx.wl_display != null;
// public
// public
fn void* todo_fix_getDisplay_X11() => _gfx_ctx.x11_display;
// public
fn void* todo_fix_getDisplay_Wayland() => _gfx_ctx.wl_display;
// public
fn void todo_fix_moveToMacOSResourceDir()
{
	ObjId path = objc::send(objc::send(objc::class("NSBundle"), ObjId, "mainBundle"), ObjId, "resourcePath");
	ZString raw = darwin::string_to_zstr(path);
	if (raw) (void)path::chdir(raw.str_view());
}

// public



// public

// public
// public

// public
fn TodoRenameMouse* todo_fix_createMouse(char* data, int w, int h, GfxFormat format)
{
	if (data == null) return null;
	ObjId rep = ns_bitmapRep(w, h);
	defer objc::release(rep);
	char* pixels = ns_bitmapData(rep);
	if (!pixels) return null;
	copy_image_data(pixels, w, h, RGBA8, data, format, null);
	ObjId image = ns_imageWithRep(rep, w, h);
	CGPoint hot;
	hot.x = 0.0; hot.y = 0.0;
	ObjId cursor = objc::send(objc::alloc_by_name("NSCursor"), ObjId, "initWithImage:hotSpot:", image, hot);
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
fn sz todo_fix_monitor_getGammaRampPtr(GfxMonitor* monitor, TodoRenameGammaRamp* ramp)
{
	CGDirectDisplayID display = todo_fix_monitorDisplay(monitor);
	uint count = display.gammaTableCapacity();
	if (ramp == null) return (sz)count;
	float* red = mem::alloc_array(float, count);
	float* green = mem::alloc_array(float, count);
	float* blue = mem::alloc_array(float, count);
	uint actual = 0;
	if (red == null || green == null || blue == null || display.transferByTable(count, red, green, blue, &actual) != 0) { free(red); free(green); free(blue); return 0; }
	for (uint i = 0; i < actual; i++)
	{
		ramp.red[i] = (ushort)(red[i] * 65535.0);
		ramp.green[i] = (ushort)(green[i] * 65535.0);
		ramp.blue[i] = (ushort)(blue[i] * 65535.0);
	}
	ramp.count = (sz)actual;
	free(red); free(green); free(blue);
	return (sz)actual;
}

// public
fn bool todo_fix_monitor_setGammaRamp(GfxMonitor* monitor, TodoRenameGammaRamp* ramp)
{
	if (ramp == null || ramp.count == 0) return false;
	float* red = mem::alloc_array(float, ramp.count);
	float* green = mem::alloc_array(float, ramp.count);
	float* blue = mem::alloc_array(float, ramp.count);
	if (red == null || green == null || blue == null) { free(red); free(green); free(blue); return false; }
	for (sz i = 0; i < ramp.count; i++)
	{
		red[i] = (float)ramp.red[i] / 65535.0;
		green[i] = (float)ramp.green[i] / 65535.0;
		blue[i] = (float)ramp.blue[i] / 65535.0;
	}
	int err = todo_fix_monitorDisplay(monitor).setTransferByTable((uint)ramp.count, red, green, blue);
	free(red); free(green); free(blue);
	return err == 0;
}

// public
fn bool todo_fix_monitor_requestMode(GfxMonitor* mon, GfxMonitorMode* mode, GfxMonitorModeRequest request) => todo_fix_monitor_setMode(mon, mode);
// public
fn bool todo_fix_monitor_setMode(GfxMonitor* mon, GfxMonitorMode* mode)
{
	if (mode == null || mode.src == null) return false;
	return todo_fix_monitorDisplay(mon).setDisplayMode((CGDisplayModeRef)mode.src, null) == 0;
}
// public
fn GfxMonitor* find_monitor(GfxWindow* win) => get_primary_monitor() ?? null;
// public


// public
// public

// public
fn void todo_fix_window_setMousePassthrough(GfxWindow* win, bool passthrough)
{
	if (win.src.window) objc::send_void(win.src.window, "setIgnoresMouseEvents:", passthrough);
}


fn void GfxWindow.set_name(&self, String name)
{
	if (self.src.window) objc::send_void(self.src.window, "setTitle:", name.to_nsstring());
}


// public
fn bool todo_fix_readClipboardPtr(GfxDataTransferType requestedType, char* buffer, sz capacity, GfxDataTransfer* data)
{
	if (requestedType != TEXT || data == null) return false;
	ObjId pb = objc::send(objc::@class("NSPasteboard"), ObjId, "generalPasteboard");
	ObjId str = objc::send(pb, ObjId, "stringForType:", "public.utf8-plain-text".to_nsstring());
	ZString raw = darwin::string_to_zstr(str);
	if (raw == null) return false;
	sz len = raw.len();
	if (buffer != null && capacity > 0)
	{
		sz copyLen = len < capacity - 1 ? len : capacity - 1;
		mem::copy(buffer, raw, copyLen);
		buffer[copyLen] = 0;
		data.data = buffer;
	}
	else
	{
		data.data = raw;
	}
	data.length = len;
	data.type = TEXT;
	return true;
}

// public

// public
fn void* todo_fix_eglContext_getSourceContext(GfxEglContext* ctx) => ctx != null ? ctx.ctx : null;
// public
fn void* todo_fix_eglContext_getSurface(GfxEglContext* ctx) => ctx != null ? ctx.surface : null;
// public
fn void* todo_fix_eglContext_wlEGLWindow(GfxEglContext* ctx) => ctx != null ? ctx.egl_window : null;


// public

// public
fn bool todo_fix_extensionSupported_EGL(ZString extension, sz len)
{
	if (todo_fix_extensionSupported_base(extension, len, &todo_fix_getProcAddress_EGL)) return true;
	return todo_fix_extensionSupportedPlatform_EGL(extension, len);
}




// public

// public





// public
fn void todo_fix_window_makeCurrentWindow_EGL(GfxWindow* win) { _gfx_ctx.current = win; }


struct WgpuSurfaceSourceMetalLayer { WgpuChainedStruct chain; ObjId layer; }

// public
fn WGPUSurface todo_fix_window_createSurface_WebGPU(GfxWindow* window, WGPUInstance instance)
{
	if (!window || !window.src.view)
	{
		log::error("NSView is NULL for macOS window", category: log::CATEGORY_VIDEO);
		return null;
	}
	objc::send_void(window.src.view, "setWantsLayer:", true);
	ObjId layer = _get_layer_macos();
	if (!layer) return null;
	_window_set_layer_macos(window, layer);

	WgpuSurfaceSourceMetalLayer fromMetal;
	fromMetal.chain.next = null;
	fromMetal.chain.sType = WGPU_STYPE_SURFACE_SOURCE_METAL_LAYER;
	fromMetal.layer = layer;
	WgpuSurfaceDescriptor surfaceDesc;
	surfaceDesc.nextInChain = (WgpuChainedStruct*)&fromMetal;
	return wgpuInstanceCreateSurface(instance, &surfaceDesc);
}
// public

// public

// public
fn float todo_fix_cocoaYTransform(float y) => (float)(cg::mainDisplayID().bounds().size.height - (double)y - 1.0);

// public
fn GfxDataTransfer* todo_fix_readClipboard(GfxDataTransferType requestedType)
{
	return _gfx_ctx.clipboard;
}

// public
fn GfxDataTransfer* todo_fix_readClipboardString()
{
	return todo_fix_readClipboard(TEXT);
}
// public
fn bool todo_fix_readClipboardStringPtr(char* buffer, sz capacity, GfxDataTransfer* data) { return todo_fix_readClipboardPtr(TEXT, buffer, capacity, data); }
