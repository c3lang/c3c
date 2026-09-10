module std::gfx @feat(MACOS) @link("Cocoa.framework", "CoreGraphics.framework", "ApplicationServices.framework", "QuartzCore.framework", "Carbon.framework", "CoreFoundation.framework", "objc");

import std::collections, std::math, libc, std::os::macos, std::io;
// C3 port of the shared RGFW implementation from /Users/lerno/Downloads/RGFW.h.
// This file intentionally imports no C3 standard-library modules. It only
// declares the small C runtime hooks RGFW's C header also expects from the host.

excuse NSWINDOW_CREATION_FAILED;


typedef ObjcClass = void*;
typedef ObjcSelector = void*;
alias CFBundleRef = void*;

struct VkMetalSurfaceCreateInfoEXT { int sType; void* pNext; uint flags; void* pLayer; }
alias TodoRenameVkCreateMetalSurfaceEXTProc = fn VkResult(VkInstance instance, VkMetalSurfaceCreateInfoEXT* create_info, void* allocator, VkSurfaceKHR* surface);

extern fn void objc_msgSendRaw(...) @cname("objc_msgSend") @builtin;
extern fn ObjcSelector sel_registerName(ZString name);
extern fn ObjcClass objc_getClass(ZString name);
extern fn ObjcClass objc_allocateClassPair(ObjcClass superclass, ZString name, sz extraBytes);
extern fn void objc_registerClassPair(ObjcClass cls);
extern fn bool class_addIvar(ObjcClass cls, ZString name, sz size, char alignment, char* types);
extern fn bool class_addMethod(ObjcClass cls, ObjcSelector name, void* imp, char* types);
extern fn void* object_setInstanceVariable(ObjcId obj, ZString name, void* value);
extern fn void* object_getInstanceVariable(ObjcId obj, ZString name, void* outValue);

extern fn void cfRelease(void* ref) @cname("CFRelease");
extern fn CFStringRef cfStringCreateWithCString(void* alloc, ZString str, uint encoding) @cname("CFStringCreateWithCString");
extern fn CFBundleRef cfBundleGetBundleWithIdentifier(CFStringRef bundleID) @cname("CFBundleGetBundleWithIdentifier");
extern fn void* cfBundleGetFunctionPointerForName(CFBundleRef bundle, CFStringRef functionName) @cname("CFBundleGetFunctionPointerForName");

macro objc_send(id, $FunctionType, ZString $selector, ...)
{
	return (($FunctionType)&objc_msgSendRaw)((ObjcId)id, sel_registerName($selector), ...$vaarg);
}

alias ObjcSendId = fn ObjcId(ObjcId, ObjcSelector);
alias ObjcSendCharPtr = fn ZString(ObjcId, ObjcSelector);
alias ObjcSendVoid = fn void(ObjcId, ObjcSelector);
alias ObjcSendVoidId = fn void(ObjcId, ObjcSelector, ObjcId);

alias ObjcSendVoidNSUInteger = fn void(ObjcId, ObjcSelector, NSUInteger);
alias ObjcSendVoidNSInteger = fn void(ObjcId, ObjcSelector, NSInteger);
alias ObjcSendVoidCGFloat = fn void(ObjcId, ObjcSelector, CGFloat);
alias ObjcSendVoidSize = fn void(ObjcId, ObjcSelector, CGSize);
alias ObjcSendVoidRectBool = fn void(ObjcId, ObjcSelector, CGRect, bool);
alias ObjcSendVoidPoint = fn void(ObjcId, ObjcSelector, CGPoint);
alias ObjcSendIdId = fn ObjcId(ObjcId, ObjcSelector, ObjcId);
alias ObjcSendIdNSUInteger = fn ObjcId(ObjcId, ObjcSelector, NSUInteger);
alias ObjcSendIdRect = fn ObjcId(ObjcId, ObjcSelector, CGRect);
alias ObjcSendIdSize = fn ObjcId(ObjcId, ObjcSelector, CGSize);
alias ObjcSendIdImagePoint = fn ObjcId(ObjcId, ObjcSelector, ObjcId, CGPoint);
alias ObjcSendBitmapRepInit = fn ObjcId(ObjcId, ObjcSelector, ZString*, NSInteger, NSInteger, NSInteger, NSInteger, bool, bool, ObjcId, NSUInteger, NSInteger, NSInteger);
alias ObjcSendIdEvent = fn ObjcId(ObjcId, ObjcSelector, NSUInteger, ObjcId, ObjcId, bool);
alias ObjcSendIdDouble = fn ObjcId(ObjcId, ObjcSelector, double);
alias ObjcSendVoidIdBool = fn void(ObjcId, ObjcSelector, ObjcId, bool);
alias ObjcSendIdWindowInit = fn ObjcId(ObjcId, ObjcSelector, CGRect, NSUInteger, NSUInteger, bool);
alias ObjcSendIdU32Ptr = fn ObjcId(ObjcId, ObjcSelector, uint*);
alias ObjcSendIdIdId = fn ObjcId(ObjcId, ObjcSelector, ObjcId, ObjcId);
alias ObjcSendPoint = fn CGPoint(ObjcId, ObjcSelector);
alias ObjcSendRect = fn CGRect(ObjcId, ObjcSelector);
alias ObjcSendRectRect = fn CGRect(ObjcId, ObjcSelector, CGRect);
alias ObjcSendBool = fn bool(ObjcId, ObjcSelector);
alias ObjcSendBoolId = fn bool(ObjcId, ObjcSelector, ObjcId);
alias ObjcSendIntegerNSUInteger = fn NSInteger(ObjcId, ObjcSelector, NSUInteger);
alias ObjcSendInteger = fn NSInteger(ObjcId, ObjcSelector);
alias ObjcSendCGFloat = fn CGFloat(ObjcId, ObjcSelector);
alias ObjcSendUShort = fn ushort(ObjcId, ObjcSelector);
alias ObjcSendNSUInteger = fn NSUInteger(ObjcId, ObjcSelector);
alias ObjcSendVoidIntPtrNSInteger = fn void(ObjcId, ObjcSelector, int*, NSInteger);


ObjcClass todo_fix_osxWindowDelegateClass;
ObjcClass todo_fix_osxViewClass;


// public
fn void todo_fix_window_blitSurface(GfxWindow* win, GfxSurface* surface)
{
	if (win == null || surface == null || surface.data == null) return;
	GfxPoint min = math::min(win.size, surface.size);
	GfxMonitor* mon = find_monitor(win);
	if (mon) min = (GfxPoint)((Vec2)min * mon.pixelRatio);
	ObjcId rep = ns_bitmapRep(...min);
	char* pixels = ns_bitmapData(rep);
	defer objc::release(rep);
	if (!pixels) return;
	copy_image_data(pixels, ...min, RGBA8, surface.data, surface.format, surface.convert_fn);
	ObjcId image = ns_imageWithRep(rep, ...min);
	objc::send_void(win.src.view, "setWantsLayer:", true);
	ObjcId layer = objc_send((ObjcId)win.src.view, ObjcSendId, "layer");
	if (layer) objc_send(layer, ObjcSendVoidId, "setContents:", image);
	objc::release(image);
}

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


// public
fn void* todo_fix_glContext_getSourceContext(GfxGlContext* ctx) { return ctx.ctx; }

struct TodoRenameAttribStack
{
	int* attribs;
	sz count;
	sz max;
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

fn void nsOpenGLContext_setValues(ObjcId context, int* vals, NSInteger param)
{
	objc_send(context, ObjcSendVoidIntPtrNSInteger, "setValues:forParameter:", vals, param);
}


// public
fn GfxGlContext* todo_fix_window_getContext_OpenGL(GfxWindow* win)
{
	if (win == null || (win.src.gfxType & TODO_RENAME_GFX_EGL)) return null;
	return win.src.ctx.native;
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
	todo_fix_window_makeCurrentContext_OpenGL(win);
}

// public
fn GfxWindow* todo_fix_getCurrentWindow_OpenGL() => _gfx_ctx.current;

// public
fn bool todo_fix_extensionSupportedPlatform_OpenGL(ZString extension, sz len) => false;

// public
fn GfxFn _get_opengl_proc_native(ZString procname)
{
	if (!_gfx_ctx.nativeGL_handle || !procname) return null;
	CFStringRef symbolName = cfStringCreateWithCString(null, procname, 0x0600);
	GfxFn symbol = (GfxFn)cfBundleGetFunctionPointerForName((CFBundleRef)_gfx_ctx.nativeGL_handle, symbolName);
	if (symbolName != null) cfRelease(symbolName);
	return symbol;
}

// public

// public

// public
fn void todo_fix_window_makeCurrentContext_OpenGL(GfxWindow* win)
{
	if (win != null && win.src.ctx.native != null && win.src.ctx.native.ctx != null) { objc_send((ObjcId)win.src.ctx.native.ctx, ObjcSendVoid, "makeCurrentContext"); }
	else { objc_send(objc_class("NSOpenGLContext"), ObjcSendId, "clearCurrentContext"); }
}

// public
fn void* todo_fix_getCurrentContext_OpenGL() => objc_send(objc_class("NSOpenGLContext"), ObjcSendId, "currentContext");
// public
fn void todo_fix_window_swapBuffers_OpenGL(GfxWindow* win) { if (win != null && win.src.ctx.native != null && win.src.ctx.native.ctx != null) objc_send((ObjcId)win.src.ctx.native.ctx, ObjcSendVoid, "flushBuffer"); }

const NSUInteger NS_WINDOW_STYLE_TITLED = 1;
const NSUInteger NS_WINDOW_STYLE_CLOSABLE = 2;
const NSUInteger NS_WINDOW_STYLE_MINIATURIZABLE = 4;
const NSUInteger NS_WINDOW_STYLE_RESIZABLE = 8;
const NSUInteger NS_WINDOW_STYLE_BORDERLESS = 0;
const NSUInteger NS_WINDOW_STYLE_FULL_SIZE_CONTENT_VIEW = 1 << 15;
const NSUInteger NS_WINDOW_MINIATURIZE_BUTTON = 1;
const NSUInteger NS_BACKING_STORE_BUFFERED = 2;
const NSUInteger NS_APP_ACTIVATION_POLICY_REGULAR = 0;
const NSUInteger NS_EVENT_MASK_ANY = 0xffffffffffffffff;
const NSInteger NS_WINDOW_LEVEL_NORMAL = 0;
const NSInteger NS_WINDOW_LEVEL_FLOATING = 3;

fn ObjcId objc_class(ZString name) => (ObjcId)objc_getClass(name);
fn ObjcId ns_alloc(ZString className) => objc_send(objc_class(className), ObjcSendId, "alloc");

fn ObjcId ns_string(ZString str) => objc::send(objc_class("NSString"), ObjcId, "stringWithUTF8String:", str);
fn ZString ns_utf8(ObjcId str) { if (str == null) return null; return objc_send(str, ObjcSendCharPtr, "UTF8String"); }
fn ObjcId String.to_nsstring(self) => @pool()
{
	return ns_string(self.zstr_copy(tmem));
}

fn ObjcId ns_bitmapRep(int w, int h)
{
	return objc_send(ns_alloc("NSBitmapImageRep"), ObjcSendBitmapRepInit,
		"initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bitmapFormat:bytesPerRow:bitsPerPixel:",
		(ZString*)null, (NSInteger)w, (NSInteger)h, 8, 4, true, false, ns_string("NSCalibratedRGBColorSpace"), (NSUInteger)2, (NSInteger)(w * 4), 32);
}

fn char* ns_bitmapData(ObjcId rep) => rep == null ? null : objc_send(rep, ObjcSendCharPtr, "bitmapData");

fn ObjcId ns_imageWithRep(ObjcId rep, int w, int h)
{
	CGSize size;
	size.width = (CGFloat)w;
	size.height = (CGFloat)h;
	ObjcId image = objc::send(ns_alloc("NSImage"), ObjcId, "initWithSize:", size);
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
fn GfxWindow* todo_fix_osxDelegateWindow(ObjcId self)
{
	void* out = null;
	object_getInstanceVariable(self, "gfx_window", &out);
	return (GfxWindow*)out;
}



// private
fn GfxWindow* todo_fix_osxViewWindow(ObjcId self)
{
	void* out = null;
	object_getInstanceVariable(self, "gfx_window", &out);
	return (GfxWindow*)out;
}


// private

// public
fn void todo_fix_monitorNode_free(GfxMonitorNode* node) { if (node != null && node.mon.mode.src != null) cfRelease(node.mon.mode.src); }


// public


// public
fn int todo_fix_initPlatform(String class_name, GfxInitOption flags)
{
	_gfx_ctx.ns_pool = objc::pool_push();
	_gfx_ctx.ns_app = objc::send(objc::class_by_name("NSApplication")!!, ObjcId, "sharedApplication");
	if (!_gfx_ctx.ns_app)
	{
		log::error("NSApplication sharedApplication failed.", category: log::CATEGORY_VIDEO);
		return -1;
	}
	objc::send_void(_gfx_ctx.ns_app, "setActivationPolicy:", (NSUInteger)NS_APP_ACTIVATION_POLICY_REGULAR);
	objc::send_void(_gfx_ctx.ns_app, "finishLaunching");
	_osx_register_classes();
	todo_fix_pollMonitors();
	return 0;
}

// public


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
	ObjcId distantPast = objc_send(objc_class("NSDate"), ObjcSendId, "distantPast");
	ObjcId mode = ns_string("kCFRunLoopDefaultMode");
	while (true)
	{
		ObjcId ev = objc_send(_gfx_ctx.ns_app, ObjcSendIdEvent, "nextEventMatchingMask:untilDate:inMode:dequeue:", NS_EVENT_MASK_ANY, distantPast, mode, true);
		if (ev == null) break;
		objc_send(_gfx_ctx.ns_app, ObjcSendVoidId, "sendEvent:", ev);
	}
}


// public
fn void todo_fix_stopCheckEvents() { _gfx_ctx.stopCheckEvents_bool = true; }
// public
fn void todo_fix_waitForEvent(int waitMS)
{
	if (!_gfx_ctx.ns_app) return;
	ObjcId date = objc_send(objc_class("NSDate"), ObjcSendIdDouble, "dateWithTimeIntervalSinceNow:", (double)waitMS / 1000.0);
	ObjcId mode = ns_string("kCFRunLoopDefaultMode");
	ObjcId ev = objc_send(_gfx_ctx.ns_app, ObjcSendIdEvent, "nextEventMatchingMask:untilDate:inMode:dequeue:", NS_EVENT_MASK_ANY, date, mode, true);
	if (ev != null) objc_send(_gfx_ctx.ns_app, ObjcSendVoidIdBool, "postEvent:atStart:", ev, true);
}
// public
fn bool todo_fix_usingWayland() => _gfx_ctx.wl_display != null;
// public
fn void* todo_fix_getLayer_OSX() { if (_gfx_ctx.current == null || _gfx_ctx.current.src.view == null) return null; return objc_send((ObjcId)_gfx_ctx.current.src.view, ObjcSendId, "layer"); }
// public
fn void* todo_fix_getDisplay_X11() => _gfx_ctx.x11_display;
// public
fn void* todo_fix_getDisplay_Wayland() => _gfx_ctx.wl_display;
// public
fn void todo_fix_moveToMacOSResourceDir()
{
	ObjcId bundle = objc_send(objc_class("NSBundle"), ObjcSendId, "mainBundle");
	if (bundle == null) return;
	ObjcId path = objc_send(bundle, ObjcSendId, "resourcePath");
	ZString raw = ns_utf8(path);
	if (raw) (void)path::chdir(raw.str_view());
}
// public
fn void* todo_fix_window_getView_OSX(GfxWindow* win) => win != null ? win.src.view : null;
// public
fn void todo_fix_window_setLayer_OSX(GfxWindow* win, void* layer) { if (win != null && win.src.view != null) objc_send((ObjcId)win.src.view, ObjcSendVoidId, "setLayer:", (ObjcId)layer); }
// public
fn void* todo_fix_window_getWindow_OSX(GfxWindow* win) => win != null ? win.src.window : null;

// public
fn void? todo_fix_createWindowPlatform(String name, GfxWindowFlags flags, GfxWindow* win)
{
	NSUInteger style = NS_WINDOW_STYLE_TITLED | NS_WINDOW_STYLE_CLOSABLE | NS_WINDOW_STYLE_MINIATURIZABLE;
	if (!flags.no_resize) style |= NS_WINDOW_STYLE_RESIZABLE;
	if (flags.no_border) style = NS_WINDOW_STYLE_BORDERLESS;
	CGRect rect = ns_rect((double)win.x, (double)todo_fix_cocoaYTransform((float)(win.y + win.height - 1)), (double)win.width, (double)win.height);
	ObjcId nswin = objc_send(ns_alloc("NSWindow"), ObjcSendIdWindowInit, "initWithContentRect:styleMask:backing:defer:", rect, style, NS_BACKING_STORE_BUFFERED, false);
	if (!nswin) return NSWINDOW_CREATION_FAILED~;
	ObjcId view = todo_fix_osxViewClass != (ObjcClass)null ? objc_send((ObjcId)todo_fix_osxViewClass, ObjcSendId, "alloc") : ns_alloc("NSView");
	view = objc_send(view, ObjcSendIdRect, "initWithFrame:", ns_rect(0.0, 0.0, (double)win.width, (double)win.height));
	if (todo_fix_osxViewClass != (ObjcClass)null) object_setInstanceVariable(view, "gfx_window", win);
	ObjcId delegate = null;
	if (todo_fix_osxWindowDelegateClass != (ObjcClass)null)
	{
		delegate = objc_send((ObjcId)todo_fix_osxWindowDelegateClass, ObjcSendId, "alloc");
		delegate = objc_send(delegate, ObjcSendId, "init");
		object_setInstanceVariable(delegate, "gfx_window", win);
		objc_send(nswin, ObjcSendVoidId, "setDelegate:", delegate);
	}
	win.src.window = objc::retain(nswin);
	win.src.view = objc::retain(view);
	win.src.delegate = delegate;
	win.src.bounds = win.bounds;
	objc_send(nswin, ObjcSendVoidId, "setContentView:", view);
	objc_send(nswin, ObjcSendVoidId, "makeFirstResponder:", view);
	objc::send_void(nswin, "setAcceptsMouseMovedEvents:", true);
	win.set_name(name);
	todo_fix_window_setFloating(win, flags.floating);
	if (!(flags.hide)) _window_show_native(win);
	if (flags.maximize) _window_maximize_native(win);
	if (flags.minimize) todo_fix_window_minimizePlatform(win);
	if (flags.fullscreen) _window_set_fullscreen_native(win, true);
}

// public
fn void todo_fix_window_closePlatform(GfxWindow* win)
{
	if (win == null) return;
	if (win.src.window) objc_send((ObjcId)win.src.window, ObjcSendVoid, "close");
	objc::release(win.src.delegate);
	objc::release(win.src.view);
	objc::release(win.src.window);
	win.src.delegate = null;
	win.src.view = null;
	win.src.window = null;
}

// public


// public

// public
fn void todo_fix_window_minimizePlatform(GfxWindow* win) { if (win != null && win.src.window != null) objc_send((ObjcId)win.src.window, ObjcSendVoidId, "miniaturize:", null); }
// public

// public

fn void? GfxSurface.init(&self, void* data, int w, int h, GfxFormat format, GfxWindow* window = null)
{
	self.data = data;
	self.size = { w, h };
	self.format = format;
	self.native.format = RGBA8;
	self.native.buffer = malloc((sz)w * h * 4);
}

// public
fn void GfxSurface.release(&self)
{
	if (!self.data) return;
	if (self.native.native0) objc::release(self.native.native0);
	if (self.native.buffer) free(self.native.buffer);
	*self = {};
}

// public
fn TodoRenameMouse* todo_fix_createMouse(char* data, int w, int h, GfxFormat format)
{
	if (data == null) return null;
	ObjcId rep = ns_bitmapRep(w, h);
	defer objc::release(rep);
	char* pixels = ns_bitmapData(rep);
	if (!pixels) return null;
	copy_image_data(pixels, w, h, RGBA8, data, format, null);
	ObjcId image = ns_imageWithRep(rep, w, h);
	CGPoint hot;
	hot.x = 0.0; hot.y = 0.0;
	ObjcId cursor = objc_send(ns_alloc("NSCursor"), ObjcSendIdImagePoint, "initWithImage:hotSpot:", image, hot);
	objc::release(image);
	return (TodoRenameMouse*)cursor;
}

// public
fn TodoRenameMouse* todo_fix_createMouseStandard(GfxMouseIcon mouse)
{
	ObjcId cls = objc_class("NSCursor");
	ObjcId cursor = null;
	switch (mouse)
	{
		case NORMAL:        cursor = objc_send(cls, ObjcSendId, "arrowCursor");
		case ARROW:         cursor = objc_send(cls, ObjcSendId, "arrowCursor");
		case IBEAM:         cursor = objc_send(cls, ObjcSendId, "IBeamCursor");
		case CROSSHAIR:     cursor = objc_send(cls, ObjcSendId, "crosshairCursor");
		case POINTING_HAND: cursor = objc_send(cls, ObjcSendId, "pointingHandCursor");
		case RESIZE_EW:     cursor = objc_send(cls, ObjcSendId, "resizeLeftRightCursor");
		case RESIZE_E:      cursor = objc_send(cls, ObjcSendId, "resizeLeftRightCursor");
		case RESIZE_W:      cursor = objc_send(cls, ObjcSendId, "resizeLeftRightCursor");
		case RESIZE_NS:     cursor = objc_send(cls, ObjcSendId, "resizeUpDownCursor");
		case RESIZE_N:      cursor = objc_send(cls, ObjcSendId, "resizeUpDownCursor");
		case RESIZE_S:      cursor = objc_send(cls, ObjcSendId, "resizeUpDownCursor");
		case RESIZE_NWSE:   cursor = objc_send(cls, ObjcSendId, "_windowResizeNorthWestSouthEastCursor");
		case RESIZE_NW:     cursor = objc_send(cls, ObjcSendId, "_windowResizeNorthWestSouthEastCursor");
		case RESIZE_SE:     cursor = objc_send(cls, ObjcSendId, "_windowResizeNorthWestSouthEastCursor");
		case RESIZE_NESW:   cursor = objc_send(cls, ObjcSendId, "_windowResizeNorthEastSouthWestCursor");
		case RESIZE_NE:     cursor = objc_send(cls, ObjcSendId, "_windowResizeNorthEastSouthWestCursor");
		case RESIZE_SW:     cursor = objc_send(cls, ObjcSendId, "_windowResizeNorthEastSouthWestCursor");
		case RESIZE_ALL:    cursor = objc_send(cls, ObjcSendId, "openHandCursor");
		case NOT_ALLOWED:   cursor = objc_send(cls, ObjcSendId, "operationNotAllowedCursor");
		case WAIT:          cursor = objc_send(cls, ObjcSendId, "arrowCursor");
		case PROGRESS:      cursor = objc_send(cls, ObjcSendId, "arrowCursor");
		default:            cursor = objc_send(cls, ObjcSendId, "arrowCursor");
	}
	return (TodoRenameMouse*)objc::retain(cursor);
}

// public
fn bool todo_fix_monitor_getWorkarea(GfxMonitor* monitor, int* x, int* y, int* width, int* height)
{
	CGRect bounds = todo_fix_monitorDisplay(monitor).bounds();
	if (x != null) *x = (int)bounds.origin.x;
	if (y != null) *y = (int)bounds.origin.y;
	if (width != null) *width = (int)bounds.size.width;
	if (height != null) *height = (int)bounds.size.height;
	return true;
}

// public
fn GfxMonitorMode[] _monitor_get_modes_native(GfxMonitor* monitor)
{
	CFArrayRef list = todo_fix_monitorDisplay(monitor).copyAllDisplayModes(null);
	if (!list) return {};

	List{GfxMonitorMode} modes;
	foreach (CGDisplayModeRef mode : list)
	{
		modes.push({
			.size = { (int)mode.getWidth(), (int)mode.getHeight() },
			.refresh_rate = (float)mode.getRefreshRate(),
			.rgb = (char[<3>])8,
			.src = mode
		});
	}
	list.release();
	return modes.array_view();
}

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
fn GfxPoint _window_fetch_size_native(GfxWindow* win)
{
	if (win.src.window != null)
	{
		CGRect frame = objc_send((ObjcId)win.src.window, ObjcSendRect, "frame");
		win.size = { (int)frame.size.width, (int)frame.size.height };
	}
	return win.size;
}

// public
fn void _window_focus_native(GfxWindow* win) { if (win != null && win.src.window != null) objc_send((ObjcId)win.src.window, ObjcSendVoidId, "makeKeyWindow", null); if (win != null) win.internal.inFocus = true; }
// public
fn void todo_fix_window_raise(GfxWindow* win) { if (win != null && win.src.window != null) objc_send((ObjcId)win.src.window, ObjcSendVoidId, "orderFrontRegardless", null); }

// public
fn void _window_set_fullscreen_native(GfxWindow* win, bool fullscreen) { if (win != null && win.src.window != null) objc_send((ObjcId)win.src.window, ObjcSendVoidId, "toggleFullScreen:", null); if (win != null) win.internal.flags.fullscreen = fullscreen; }
// public
fn void todo_fix_window_setFloating(GfxWindow* win, bool floating) { if (win != null && win.src.window != null) objc_send((ObjcId)win.src.window, ObjcSendVoidNSInteger, "setLevel:", floating ? NS_WINDOW_LEVEL_FLOATING : NS_WINDOW_LEVEL_NORMAL); if (win != null) win.internal.flags.floating = floating; }
// public
fn void todo_fix_window_setOpacity(GfxWindow* win, char opacity) { if (win != null && win.src.window != null) objc_send((ObjcId)win.src.window, ObjcSendVoidCGFloat, "setAlphaValue:", (CGFloat)opacity / 255.0); }
// public
fn void _window_set_border_native(GfxWindow* win, bool border)
{
	if (win == null || win.src.window == null) return;
	CGRect frame = objc_send((ObjcId)win.src.window, ObjcSendRect, "frame");
	CGRect content = objc_send((ObjcId)win.src.view, ObjcSendRect, "frame");
	double offset = 0;

	win.internal.flags.no_border = !border;
	NSUInteger styleMask = NS_WINDOW_STYLE_BORDERLESS | NS_WINDOW_STYLE_FULL_SIZE_CONTENT_VIEW;
	if (border) styleMask = NS_WINDOW_STYLE_TITLED | NS_WINDOW_STYLE_CLOSABLE | NS_WINDOW_STYLE_MINIATURIZABLE;
	if (!win.internal.flags.no_resize) styleMask |= NS_WINDOW_STYLE_RESIZABLE;

	objc_send((ObjcId)win.src.window, ObjcSendVoidNSUInteger, "setStyleMask:", styleMask);

	if (!border)
	{
		ObjcId miniaturizeButton = objc_send((ObjcId)win.src.window, ObjcSendIdNSUInteger, "standardWindowButton:", NS_WINDOW_MINIATURIZE_BUTTON);
		ObjcId titleBarView = objc_send(miniaturizeButton, ObjcSendId, "superview");
		objc::send_void(titleBarView, "setHidden:", true);
		offset = (double)(frame.size.height - content.size.height);
	}

	win.resize(win.size + { 0, (int)offset });
	win.height -= (int)offset;
}
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

fn void _window_hide_native(GfxWindow* win) @private
{
	if (win.src.window) objc::send_void(win.src.window, "orderOut:", null);
	win.internal.flags.hide = true;
}

fn void _window_flash_native(GfxWindow* win, GfxFlashRequest request) @private
{
	objc::send(_gfx_ctx.ns_app, NSInteger, "requestUserAttention:", (NSUInteger)request);
}

// public
fn bool _window_is_minimized_native(GfxWindow* win)
{
	if (win.src.window) return objc::send(win.src.window, bool, "isMiniaturized");
	return win.internal.flags.minimize;
}
// public
fn bool _window_is_maximized_native(GfxWindow* win) { if (win == null) return false; if (win.src.window != null) return objc_send((ObjcId)win.src.window, ObjcSendBool, "isZoomed"); return win.internal.flags.maximize; }

// public


// public
fn bool todo_fix_readClipboardPtr(GfxDataTransferType requestedType, char* buffer, sz capacity, GfxDataTransfer* data)
{
	if (requestedType != TEXT || data == null) return false;
	ObjcId pb = objc_send(objc_class("NSPasteboard"), ObjcSendId, "generalPasteboard");
	ObjcId str = objc_send(pb, ObjcSendIdId, "stringForType:", ns_string("public.utf8-plain-text"));
	ZString raw = ns_utf8(str);
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
fn bool todo_fix_writeClipboard(GfxDataTransfer* data)
{
	if (!data || data.type != TEXT || !data.data) return false;
	ObjcId pb = objc::send(objc::@class("NSPasteboard"), ObjcId, "generalPasteboard");
	objc::send_void(pb, "clearContents");
	return objc::send(pb,bool, "setString:forType:", ns_string((ZString)data.data), "public.utf8-plain-text".to_nsstring());
}

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
fn GfxEglContext* todo_fix_window_getContext_EGL(GfxWindow* win) => win != null ? win.src.ctx.egl : null;

// public
fn void todo_fix_window_deleteContext_EGL(GfxWindow* win, GfxEglContext* ctx)
{
	_window_release_egl_context_native(win, ctx);
	if (win != null && (win.src.gfxType & TODO_RENAME_GFX_OWNED_BY_RGFW)) free(ctx);
}



// public
fn void todo_fix_window_makeCurrentWindow_EGL(GfxWindow* win) { _gfx_ctx.current = win; }


struct WgpuSurfaceSourceMetalLayer { WgpuChainedStruct chain; void* layer; }

// public
fn WGPUSurface todo_fix_window_createSurface_WebGPU(GfxWindow* window, WGPUInstance instance)
{
	if (window == null || window.src.view == null)
	{
		log::error("NSView is NULL for macOS window", category: log::CATEGORY_VIDEO);
		return null;
	}
	objc::send_void(window.src.view, "setWantsLayer:", true);
	void* layer = todo_fix_getLayer_OSX();
	if (layer == null) return null;
	todo_fix_window_setLayer_OSX(window, layer);

	WgpuSurfaceSourceMetalLayer fromMetal;
	fromMetal.chain.next = null;
	fromMetal.chain.sType = WGPU_STYPE_SURFACE_SOURCE_METAL_LAYER;
	fromMetal.layer = layer;
	WgpuSurfaceDescriptor surfaceDesc;
	surfaceDesc.nextInChain = (WgpuChainedStruct*)&fromMetal;
	return wgpuInstanceCreateSurface(instance, &surfaceDesc);
}
// public
fn void* todo_fix_getNSScreenForDisplayUInt(uint uintNum)
{
	ObjcId screens = objc_send(objc_class("NSScreen"), ObjcSendId, "screens");
	if (screens == null) return null;
	NSUInteger count = objc_send(screens, ObjcSendNSUInteger, "count");
	for (NSUInteger i = 0; i < count; i++)
	{
		ObjcId screen = objc_send(screens, ObjcSendIdNSUInteger, "objectAtIndex:", i);
		ObjcId desc = objc_send(screen, ObjcSendId, "deviceDescription");
		ObjcId number = objc_send(desc, ObjcSendIdId, "objectForKey:", ns_string("NSScreenNumber"));
		uint display = (uint)objc_send(number, ObjcSendNSUInteger, "unsignedIntValue");
		if (display == uintNum) return screen;
	}
	return null;
}

// public
fn void todo_fix_osx_initView(GfxWindow* win)
{
	if (win == null || win.src.window == null || win.src.view == null) return;
	objc_send((ObjcId)win.src.window, ObjcSendVoidId, "setContentView:", (ObjcId)win.src.view);
	objc_send((ObjcId)win.src.window, ObjcSendVoidId, "makeFirstResponder:", (ObjcId)win.src.view);
}
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
