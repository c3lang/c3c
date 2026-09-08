module std::gfx @feat(MACOS) @link("Cocoa.framework", "CoreGraphics.framework", "ApplicationServices.framework", "QuartzCore.framework", "Carbon.framework", "CoreFoundation.framework", "objc");

import std::collections, std::math, libc, std::os::macos, std::io;
// C3 port of the shared RGFW implementation from /Users/lerno/Downloads/RGFW.h.
// This file intentionally imports no C3 standard-library modules. It only
// declares the small C runtime hooks RGFW's C header also expects from the host.

excuse NSWINDOW_CREATION_FAILED;

alias ObjcId = void*;
typedef ObjcClass = void*;
typedef ObjcSelector = void*;
alias CFBundleRef = void*;
alias CGDirectDisplayID = uint;
alias CGDisplayModeRef = void*;
alias CGEventRef = void*;

struct VkMetalSurfaceCreateInfoEXT { int sType; void* pNext; uint flags; void* pLayer; }
alias RgfwVkCreateMetalSurfaceEXTProc = fn VkResult(VkInstance instance, VkMetalSurfaceCreateInfoEXT* createInfo, void* allocator, VkSurfaceKHR* surface);

extern fn void objc_msgSendRaw(...) @cname("objc_msgSend") @builtin;
extern fn ObjcSelector sel_registerName(ZString name);
extern fn ObjcClass objc_getClass(ZString name);
extern fn ObjcClass objc_allocateClassPair(ObjcClass superclass, ZString name, sz extraBytes);
extern fn void objc_registerClassPair(ObjcClass cls);
extern fn bool class_addIvar(ObjcClass cls, ZString name, sz size, char alignment, char* types);
extern fn bool class_addMethod(ObjcClass cls, ObjcSelector name, void* imp, char* types);
extern fn void* object_setInstanceVariable(ObjcId obj, ZString name, void* value);
extern fn void* object_getInstanceVariable(ObjcId obj, ZString name, void* outValue);
extern fn void* objc_autoreleasePoolPush();
extern fn void objc_autoreleasePoolPop(void* context);

extern fn CGRect cgDisplayBounds(CGDirectDisplayID display) @cname("CGDisplayBounds");
extern fn CGDirectDisplayID cgMainDisplayID() @cname("CGMainDisplayID");
extern fn int cgGetActiveDisplayList(uint maxDisplays, CGDirectDisplayID* activeDisplays, uint* displayCount) @cname("CGGetActiveDisplayList");
extern fn CGDisplayModeRef cgDisplayCopyDisplayMode(CGDirectDisplayID display) @cname("CGDisplayCopyDisplayMode");
extern fn CFArrayRef cgDisplayCopyAllDisplayModes(CGDirectDisplayID display, void* options) @cname("CGDisplayCopyAllDisplayModes");
extern fn sz cgDisplayModeGetWidth(CGDisplayModeRef mode) @cname("CGDisplayModeGetWidth");
extern fn sz cgDisplayModeGetHeight(CGDisplayModeRef mode) @cname("CGDisplayModeGetHeight");
extern fn double cgDisplayModeGetRefreshRate(CGDisplayModeRef mode) @cname("CGDisplayModeGetRefreshRate");
extern fn CGSize cgDisplayScreenSize(CGDirectDisplayID display) @cname("CGDisplayScreenSize");
extern fn uint cgDisplayGammaTableCapacity(CGDirectDisplayID display) @cname("CGDisplayGammaTableCapacity");
extern fn int cgGetDisplayTransferByTable(CGDirectDisplayID display, uint capacity, float* red, float* green, float* blue, uint* sampleCount) @cname("CGGetDisplayTransferByTable");
extern fn int cgSetDisplayTransferByTable(CGDirectDisplayID display, uint count, float* red, float* green, float* blue) @cname("CGSetDisplayTransferByTable");
extern fn int cgDisplaySetDisplayMode(CGDirectDisplayID display, CGDisplayModeRef mode, void* options) @cname("CGDisplaySetDisplayMode");
extern fn CGEventRef cgEventCreate(void* source) @cname("CGEventCreate");
extern fn CGPoint cgEventGetLocation(CGEventRef event) @cname("CGEventGetLocation");
extern fn int cgDisplayShowCursor(CGDirectDisplayID display) @cname("CGDisplayShowCursor");
extern fn int cgDisplayHideCursor(CGDirectDisplayID display) @cname("CGDisplayHideCursor");
extern fn int cgAssociateMouseAndMouseCursorPosition(bool connected) @cname("CGAssociateMouseAndMouseCursorPosition");
extern fn int cgWarpMouseCursorPosition(CGPoint p) @cname("CGWarpMouseCursorPosition");
extern fn CFIndex cfArrayGetCount(CFArrayRef array) @cname("CFArrayGetCount");
extern fn void* cfArrayGetValueAtIndex(CFArrayRef array, CFIndex index) @cname("CFArrayGetValueAtIndex");
extern fn void cfRelease(void* ref) @cname("CFRelease");
extern fn CFStringRef cfStringCreateWithCString(void* alloc, ZString str, uint encoding) @cname("CFStringCreateWithCString");
extern fn CFBundleRef cfBundleGetBundleWithIdentifier(CFStringRef bundleID) @cname("CFBundleGetBundleWithIdentifier");
extern fn void* cfBundleGetFunctionPointerForName(CFBundleRef bundle, CFStringRef functionName) @cname("CFBundleGetFunctionPointerForName");

macro objc_send(id, $FunctionType, ZString $selector, ...)
{
	return (($FunctionType)&objc_msgSendRaw)((ObjcId)id, sel_registerName($selector), ...$vaarg);
}

alias ObjcSendId = fn ObjcId(ObjcId, ObjcSelector);
alias ObjcSendIdCharPtr = fn ObjcId(ObjcId, ObjcSelector, ZString);
alias ObjcSendCharPtr = fn ZString(ObjcId, ObjcSelector);
alias ObjcSendVoid = fn void(ObjcId, ObjcSelector);
alias ObjcSendVoidId = fn void(ObjcId, ObjcSelector, ObjcId);
alias ObjcSendVoidBool = fn void(ObjcId, ObjcSelector, bool);
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
alias ObjcSendBoolIdId = fn bool(ObjcId, ObjcSelector, ObjcId, ObjcId);
alias ObjcSendIntegerNSUInteger = fn NSInteger(ObjcId, ObjcSelector, NSUInteger);
alias ObjcSendInteger = fn NSInteger(ObjcId, ObjcSelector);
alias ObjcSendCGFloat = fn CGFloat(ObjcId, ObjcSelector);
alias ObjcSendUShort = fn ushort(ObjcId, ObjcSelector);
alias ObjcSendNSUInteger = fn NSUInteger(ObjcId, ObjcSelector);
alias ObjcSendVoidIntPtrNSInteger = fn void(ObjcId, ObjcSelector, int*, NSInteger);



struct RgfwGlContext { void* ctx; void* format; }
struct RgfwEglContext { void* ctx; void* surface; void* eglWindow; }

const RgfwGlReleaseBehavior RGFW_GL_RELEASE_FLUSH = 0;
const RgfwGlReleaseBehavior RGFW_GL_RELEASE_NONE = 1;

const RgfwGlProfile RGFW_GL_CORE = 0;
const RgfwGlProfile RGFW_GL_FORWARD_COMPATIBILITY = 1;
const RgfwGlProfile RGFW_GL_COMPATIBILITY = 2;
const RgfwGlProfile RGFW_GL_ES = 3;
const RgfwGlProfile RGFW_GL_WEB = 4;

const RgfwGlRenderer RGFW_GL_ACCELERATED = 0;
const RgfwGlRenderer RGFW_GL_SOFTWARE = 1;

struct RgfwGlHints
{
	int stencil;
	int samples;
	int stereo;
	int auxBuffers;
	int doubleBuffer;
	int red, green, blue, alpha;
	int depth;
	int accumRed, accumGreen, accumBlue, accumAlpha;
	bool sRGB;
	bool robustness;
	bool debug;
	bool noError;
	RgfwGlReleaseBehavior releaseBehavior;
	RgfwGlProfile profile;
	int major, minor;
	RgfwGlContext* share;
	RgfwEglContext* shareEGL;
	RgfwGlRenderer renderer;
}

union RgfwGfxContext
{
	RgfwGlContext* native;
	RgfwEglContext* egl;
}

const RgfwGfxContextType RGFW_GFX_NATIVE_OPEN_GL = 1;
const RgfwGfxContextType RGFW_GFX_EGL = 2;
const RgfwGfxContextType RGFW_GFX_OWNED_BY_RGFW = 4;


struct RgfwWindowSrc
{
	inline IntRect bounds;
	void* window;
	void* view;
	void* mouse;
	void* delegate;
	RgfwGfxContext ctx;
	RgfwGfxContextType gfxType;
}



ObjcClass rgfw_osxWindowDelegateClass;
ObjcClass rgfw_osxViewClass;




// public
fn bool rgfw_loadGL()
{
	CFStringRef bundleName = cfStringCreateWithCString(null, "com.apple.opengl", 0x0600);
	_gfx_ctx.nativeGL_handle = cfBundleGetBundleWithIdentifier(bundleName);
	if (bundleName != null) cfRelease(bundleName);
	_gfx_ctx.glGetProcAddress = (void*)&rgfw_getProcAddress_OpenGL;
	return _gfx_ctx.nativeGL_handle != null;
}


const int VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT = 1000217000;
const VkResult VK_ERROR_EXTENSION_NOT_PRESENT = -7;
ZString[2] rgfw_vulkanExtensions = {"VK_KHR_surface", "VK_EXT_metal_surface"};

// public
fn bool rgfw_loadVulkan()
{
	if (_gfx_ctx.vulkan_handle != null) return true;
	ZString[3] names = {"libvulkan.1.dylib", "libvulkan.dylib", "libMoltenVK.dylib"};
	for (sz i = 0; i < 3; i++)
	{
		_gfx_ctx.vulkan_handle = libc::dlopen(names[i], RTLD_LAZY | RTLD_LOCAL);
		if (_gfx_ctx.vulkan_handle != null) break;
	}
	if (_gfx_ctx.vulkan_handle == null) return false;
	void* proc = libc::dlsym(_gfx_ctx.vulkan_handle, "vkGetInstanceProcAddr");
	if (proc == null) return false;
	_gfx_ctx.vkGetInstanceProcAddress = (GfxVkGetInstanceProcAddressProc)proc;
	return true;
}

// public
fn VkResult rgfw_window_createSurface_Vulkan(GfxWindow* win, VkInstance instance, VkSurfaceKHR* surface)
{
	if (surface == null || win == null || win.src.view == null) return VK_ERROR_EXTENSION_NOT_PRESENT;
	*surface = 0;
	GfxFn raw = rgfw_getInstanceProcAddress_Vulkan(instance, "vkCreateMetalSurfaceEXT");
	if (raw == null) return VK_ERROR_EXTENSION_NOT_PRESENT;
	ObjcId layer = objc_send(objc_class("CAMetalLayer"), ObjcSendId, "layer");
	if (layer == null) return VK_ERROR_EXTENSION_NOT_PRESENT;
	objc_send((ObjcId)win.src.view, ObjcSendVoidBool, "setWantsLayer:", true);
	rgfw_window_setLayer_OSX(win, layer);
	VkMetalSurfaceCreateInfoEXT createInfo;
	createInfo.sType = VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT;
	createInfo.pNext = null;
	createInfo.flags = 0;
	createInfo.pLayer = layer;
	return ((RgfwVkCreateMetalSurfaceEXTProc)raw)(instance, &createInfo, null, surface);
}

// public
fn bool rgfw_getPhysicalDevicePresentationSupport_Vulkan(VkInstance instance, VkPhysicalDevice physicalDevice, uint queueFamilyIndex) => false;


// public
fn void rgfw_window_blitSurface(GfxWindow* win, GfxSurface* surface)
{
	if (win == null || surface == null || surface.data == null) return;
	IntVec2 min = math::min(win.size, surface.size);
	GfxMonitor* mon = find_monitor(win);
	if (mon) min = (IntVec2)((Vec2)min * mon.pixelRatio);
	ObjcId rep = ns_bitmapRep(...min);
	char* pixels = ns_bitmapData(rep);
	if (pixels == null) { ns_release(rep); return; }
	copy_image_data(pixels, ...min, RGBA8, surface.data, surface.format, surface.convertFunc);
	ObjcId image = ns_imageWithRep(rep, ...min);
	objc_send((ObjcId)win.src.view, ObjcSendVoidBool, "setWantsLayer:", true);
	ObjcId layer = objc_send((ObjcId)win.src.view, ObjcSendId, "layer");
	if (layer) objc_send(layer, ObjcSendVoidId, "setContents:", image);
	ns_release(image);
	ns_release(rep);
}

alias TISInputSourceRef = void*;
alias CFDataRef = void*;
typedef UCKeyboardLayout = void;
extern fn TISInputSourceRef tisCopyCurrentKeyboardLayoutInputSource() @cname("TISCopyCurrentKeyboardLayoutInputSource");
extern fn void* tisGetInputSourceProperty(TISInputSourceRef source, CFStringRef propertyKey) @cname("TISGetInputSourceProperty");
extern fn char* cfDataGetBytePtr(CFDataRef data) @cname("CFDataGetBytePtr");
extern fn char lmGetKbdType() @cname("LMGetKbdType");
extern fn int ucKeyTranslate(UCKeyboardLayout* layout, ushort virtualKeyCode, ushort keyAction, uint modifierKeyState, uint keyboardType, uint keyTranslateOptions, uint* deadKeyState, uint maxStringLength, uint* actualStringLength, ushort* unicodeString) @cname("UCKeyTranslate");
extern CFStringRef kTISPropertyUnicodeKeyLayoutData @cname("kTISPropertyUnicodeKeyLayoutData");
const uint UC_KEY_ACTION_DOWN = 0;
const uint UC_KEY_TRANSLATE_NO_DEAD_KEYS_BIT = 0;

// public
fn GfxKey rgfw_physicalToMappedKey(GfxKey key)
{
	ushort keycode = (ushort)rgfw_rgfwToApiKey(key);
	TISInputSourceRef source = tisCopyCurrentKeyboardLayoutInputSource();
	if (source == null) return key;

	CFDataRef layoutData = tisGetInputSourceProperty(source, kTISPropertyUnicodeKeyLayoutData);
	if (layoutData == null)
	{
		cfRelease(source);
		return key;
	}

	UCKeyboardLayout* layout = (UCKeyboardLayout*)cfDataGetBytePtr(layoutData);

	uint deadKeyState = 0;
	ushort[4] chars;
	uint len = 0;
	uint type = (uint)lmGetKbdType();
	int status = ucKeyTranslate(layout, keycode, (ushort)UC_KEY_ACTION_DOWN, 0, type, UC_KEY_TRANSLATE_NO_DEAD_KEYS_BIT, &deadKeyState, 4, &len, &chars[0]);

	cfRelease(source);

	if (status == 0 && len == 1 && chars[0] < 256) return (GfxKey)chars[0];

	return key;
}


// public
fn void* rgfw_glContext_getSourceContext(RgfwGlContext* ctx) { return ctx.ctx; }

struct RgfwAttribStack
{
	int* attribs;
	sz count;
	sz max;
}


alias RgfwGetProcAddressFunc = fn GfxFn(ZString procname);

// public
fn bool rgfw_extensionSupported_base(ZString extension, sz len, RgfwGetProcAddressFunc getProcAddress)
{
	GfxFn rgfw_gl_get_string = getProcAddress("glGetString");
	if (rgfw_gl_get_string == null) return false;
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
fn RgfwGlContext* rgfw_window_getContext_OpenGL(GfxWindow* win)
{
	if (win == null || (win.src.gfxType & RGFW_GFX_EGL)) return null;
	return win.src.ctx.native;
}


// public
fn bool rgfw_extensionSupported_OpenGL(ZString extension, sz len)
{
	if (rgfw_extensionSupported_base(extension, len, &rgfw_getProcAddress_OpenGL)) return true;
	return rgfw_extensionSupportedPlatform_OpenGL(extension, len);
}

// public
fn void rgfw_window_makeCurrentWindow_OpenGL(GfxWindow* win)
{
	if (win != null) _gfx_ctx.current = win;
	rgfw_window_makeCurrentContext_OpenGL(win);
}

// public
fn GfxWindow* rgfw_getCurrentWindow_OpenGL() => _gfx_ctx.current;

// public
fn bool rgfw_extensionSupportedPlatform_OpenGL(ZString extension, sz len) => false;

// public
fn GfxFn rgfw_getProcAddress_OpenGL(ZString procname)
{
	if (!_gfx_ctx.nativeGL_handle || !procname) return null;
	CFStringRef symbolName = cfStringCreateWithCString(null, procname, 0x0600);
	GfxFn symbol = (GfxFn)cfBundleGetFunctionPointerForName((CFBundleRef)_gfx_ctx.nativeGL_handle, symbolName);
	if (symbolName != null) cfRelease(symbolName);
	return symbol;
}

// public
fn bool rgfw_window_createContextPtr_OpenGL(GfxWindow* win, RgfwGlContext* ctx, RgfwGlHints* hints)
{
	if (win == null || ctx == null || hints == null || win.src.window == null) return false;
	win.src.ctx.native = ctx;
	win.src.gfxType = RGFW_GFX_NATIVE_OPEN_GL;

	int[40] attribs;
	RgfwAttribStack stack;
	rgfw_attribStack_init(&stack, &attribs, 40);
	int colorBits = (hints.red + hints.green + hints.blue + hints.alpha) / 4;
	rgfw_attribStack_pushAttribs(&stack, NS_OPEN_GL_PFA_COLOR_SIZE, colorBits);
	rgfw_attribStack_pushAttribs(&stack, NS_OPEN_GL_PFA_ALPHA_SIZE, hints.alpha);
	rgfw_attribStack_pushAttribs(&stack, NS_OPEN_GL_PFA_DEPTH_SIZE, hints.depth);
	rgfw_attribStack_pushAttribs(&stack, NS_OPEN_GL_PFA_STENCIL_SIZE, hints.stencil);
	rgfw_attribStack_pushAttribs(&stack, NS_OPEN_GL_PFA_AUX_BUFFERS, hints.auxBuffers);
	rgfw_attribStack_pushAttrib(&stack, NS_OPEN_GL_PFA_CLOSEST_POLICY);
	if (hints.samples != 0)
	{
		rgfw_attribStack_pushAttribs(&stack, NS_OPEN_GL_PFA_SAMPLE_BUFFERS, 1);
		rgfw_attribStack_pushAttribs(&stack, NS_OPEN_GL_PFA_SAMPLES, hints.samples);
	}
	else { rgfw_attribStack_pushAttribs(&stack, NS_OPEN_GL_PFA_SAMPLE_BUFFERS, 0); }
	if (hints.doubleBuffer != 0) rgfw_attribStack_pushAttrib(&stack, NS_OPEN_GL_PFA_DOUBLE_BUFFER);
	rgfw_attribStack_pushAttribs(&stack, NS_OPEN_GL_PFA_OPEN_GL_PROFILE, hints.major >= 4 ? NS_OPEN_GL_PROFILE_4_1_CORE : (hints.major >= 3 ? NS_OPEN_GL_PROFILE_3_2_CORE : NS_OPEN_GL_PROFILE_LEGACY));
	if (hints.major <= 2)
	{
		int accumSize = (hints.accumRed + hints.accumGreen + hints.accumBlue + hints.accumAlpha) / 4;
		rgfw_attribStack_pushAttribs(&stack, NS_OPEN_GL_PFA_ACCUM_SIZE, accumSize);
	}
	if (hints.renderer == RGFW_GL_SOFTWARE) { rgfw_attribStack_pushAttribs(&stack, NS_OPEN_GL_PFA_RENDERER_ID, K_CGL_RENDERER_GENERIC_FLOAT_ID); }
	else { rgfw_attribStack_pushAttrib(&stack, NS_OPEN_GL_PFA_ACCELERATED); }
	rgfw_attribStack_pushAttribs(&stack, 0, 0);

	ObjcId format = objc_send(ns_alloc("NSOpenGLPixelFormat"), ObjcSendIdU32Ptr, "initWithAttributes:", (uint*)&attribs);
	if (format == null)
	{
		log::error("Failed to load pixel format for OpenGL.", category: log::CATEGORY_VIDEO);
		return false;
	}

	ObjcId share = hints.share != null ? (ObjcId)hints.share.ctx : null;
	ctx.ctx = objc_send(ns_alloc("NSOpenGLContext"), ObjcSendIdIdId, "initWithFormat:shareContext:", format, share);
	if (ctx.ctx == null)
	{
		ns_release(format);
		log::error("Failed to create an OpenGL context", category: log::CATEGORY_VIDEO);
		return false;
	}
	ctx.format = format;
	objc_send((ObjcId)ctx.ctx, ObjcSendVoidId, "setView:", (ObjcId)win.src.view);
	if (win.internal.flags.transparent)
	{
		int opacity = 0;
		nsOpenGLContext_setValues((ObjcId)ctx.ctx, &opacity, NS_OPEN_GL_CP_SURFACE_OPACITY);
	}
	rgfw_window_makeCurrentContext_OpenGL(win);
	win.opengl_swap_interval(0);
	log::info("OpenGL context initialized", category: log::CATEGORY_VIDEO);
	return true;
}

// public
fn void rgfw_window_deleteContextPtr_OpenGL(GfxWindow* win, RgfwGlContext* ctx)
{
	if (ctx == null) return;
	ns_release((ObjcId)ctx.format);
	ns_release((ObjcId)ctx.ctx);
	ctx.format = null;
	ctx.ctx = null;
	if (win != null) win.src.ctx.native = null;
}

// public
fn void rgfw_window_makeCurrentContext_OpenGL(GfxWindow* win)
{
	if (win != null && win.src.ctx.native != null && win.src.ctx.native.ctx != null) { objc_send((ObjcId)win.src.ctx.native.ctx, ObjcSendVoid, "makeCurrentContext"); }
	else { objc_send(objc_class("NSOpenGLContext"), ObjcSendId, "clearCurrentContext"); }
}

// public
fn void* rgfw_getCurrentContext_OpenGL() => objc_send(objc_class("NSOpenGLContext"), ObjcSendId, "currentContext");
// public
fn void rgfw_window_swapBuffers_OpenGL(GfxWindow* win) { if (win != null && win.src.ctx.native != null && win.src.ctx.native.ctx != null) objc_send((ObjcId)win.src.ctx.native.ctx, ObjcSendVoid, "flushBuffer"); }

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
fn ObjcId ns_retain(ObjcId obj) { if (obj != null) objc_send(obj, ObjcSendId, "retain"); return obj; }
fn void ns_release(ObjcId obj) { if (obj != null) objc_send(obj, ObjcSendVoid, "release"); }
fn ObjcId ns_string(ZString str) => objc_send(objc_class("NSString"), ObjcSendIdCharPtr, "stringWithUTF8String:", str);
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
	ObjcId image = objc_send(ns_alloc("NSImage"), ObjcSendIdSize, "initWithSize:", size);
	if (image != null && rep != null) objc_send(image, ObjcSendVoidId, "addRepresentation:", rep);
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
fn CGDirectDisplayID rgfw_monitorDisplay(GfxMonitor* monitor)
{
	if (monitor != null && monitor.node != null && monitor.node.nativeId0 != 0) return (CGDirectDisplayID)monitor.node.nativeId0;
	return cgMainDisplayID();
}

// private
fn GfxWindow* rgfw_osxDelegateWindow(ObjcId self)
{
	void* out = null;
	object_getInstanceVariable(self, "gfx_window", &out);
	return (GfxWindow*)out;
}



// private
fn GfxWindow* rgfw_osxViewWindow(ObjcId self)
{
	void* out = null;
	object_getInstanceVariable(self, "gfx_window", &out);
	return (GfxWindow*)out;
}


// private
fn RgfwKeymod rgfw_osxMods(NSUInteger flags)
{
	RgfwKeymod mods = 0;
	if (flags & 0x00020000) mods |= RGFW_MOD_SHIFT;
	if (flags & 0x00040000) mods |= RGFW_MOD_CONTROL;
	if (flags & 0x00080000) mods |= RGFW_MOD_ALT;
	if (flags & 0x00100000) mods |= RGFW_MOD_SUPER;
	if (flags & 0x00010000) mods |= RGFW_MOD_CAPS_LOCK;
	return mods;
}

// public
fn void rgfw_monitorNode_free(GfxMonitorNode* node) { if (node != null && node.mon.mode.src != null) cfRelease(node.mon.mode.src); }

// public
fn void _window_set_max_size_native(GfxWindow* win, int w, int h)
{
	if (win == null || win.src.window == null) return;
	if (w == 0 && h == 0)
	{
		GfxMonitor* mon = find_monitor(win);
		if (mon != null)
		{
			w = mon.mode.w;
			h = mon.mode.h;
		}
	}
	CGSize size;
	size.width = (CGFloat)w; size.height = (CGFloat)h;
	objc_send((ObjcId)win.src.window, ObjcSendVoidSize, "setMaxSize:", size);
}

// public
fn void _window_set_min_size_native(GfxWindow* win, int w, int h)
{
	if (win == null || win.src.window == null) return;
	CGSize size;
	size.width = (CGFloat)w; size.height = (CGFloat)h;
	objc_send((ObjcId)win.src.window, ObjcSendVoidSize, "setMinSize:", size);
}

// public
fn int rgfw_initPlatform(String class_name, GfxInitOption flags)
{
	_gfx_ctx.nsPool = objc_autoreleasePoolPush();
	_gfx_ctx.nsApp = objc_send(objc_class("NSApplication"), ObjcSendId, "sharedApplication");
	if (_gfx_ctx.nsApp == null)
	{
		log::error("NSApplication sharedApplication failed.", category: log::CATEGORY_VIDEO);
		return -1;
	}
	objc_send(_gfx_ctx.nsApp, ObjcSendVoidNSUInteger, "setActivationPolicy:", NS_APP_ACTIVATION_POLICY_REGULAR);
	objc_send(_gfx_ctx.nsApp, ObjcSendVoid, "finishLaunching");
	_osx_register_classes();
	rgfw_pollMonitors();
	return 0;
}

// public
fn void rgfw_deinitPlatform()
{
	if (_gfx_ctx.nsPool != null)
	{
		objc_autoreleasePoolPop(_gfx_ctx.nsPool);
		_gfx_ctx.nsPool = null;
	}
}

// public
fn void rgfw_initKeycodesPlatform()
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
fn void rgfw_pollEvents()
{
	rgfw_resetPrevState();
	if (!_gfx_ctx.nsApp) return;
	ObjcId distantPast = objc_send(objc_class("NSDate"), ObjcSendId, "distantPast");
	ObjcId mode = ns_string("kCFRunLoopDefaultMode");
	while (true)
	{
		ObjcId ev = objc_send(_gfx_ctx.nsApp, ObjcSendIdEvent, "nextEventMatchingMask:untilDate:inMode:dequeue:", NS_EVENT_MASK_ANY, distantPast, mode, true);
		if (ev == null) break;
		objc_send(_gfx_ctx.nsApp, ObjcSendVoidId, "sendEvent:", ev);
	}
}


// public
fn void rgfw_stopCheckEvents() { _gfx_ctx.stopCheckEvents_bool = true; }
// public
fn void rgfw_waitForEvent(int waitMS)
{
	if (!_gfx_ctx.nsApp) return;
	ObjcId date = objc_send(objc_class("NSDate"), ObjcSendIdDouble, "dateWithTimeIntervalSinceNow:", (double)waitMS / 1000.0);
	ObjcId mode = ns_string("kCFRunLoopDefaultMode");
	ObjcId ev = objc_send(_gfx_ctx.nsApp, ObjcSendIdEvent, "nextEventMatchingMask:untilDate:inMode:dequeue:", NS_EVENT_MASK_ANY, date, mode, true);
	if (ev != null) objc_send(_gfx_ctx.nsApp, ObjcSendVoidIdBool, "postEvent:atStart:", ev, true);
}
// public
fn bool rgfw_usingWayland() => _gfx_ctx.wl_display != null;
// public
fn void* rgfw_getLayer_OSX() { if (_gfx_ctx.current == null || _gfx_ctx.current.src.view == null) return null; return objc_send((ObjcId)_gfx_ctx.current.src.view, ObjcSendId, "layer"); }
// public
fn void* rgfw_getDisplay_X11() => _gfx_ctx.x11_display;
// public
fn void* rgfw_getDisplay_Wayland() => _gfx_ctx.wl_display;
// public
fn void rgfw_moveToMacOSResourceDir()
{
	ObjcId bundle = objc_send(objc_class("NSBundle"), ObjcSendId, "mainBundle");
	if (bundle == null) return;
	ObjcId path = objc_send(bundle, ObjcSendId, "resourcePath");
	ZString raw = ns_utf8(path);
	if (raw) (void)path::chdir(raw.str_view());
}
// public
fn void* rgfw_window_getView_OSX(GfxWindow* win) => win != null ? win.src.view : null;
// public
fn void rgfw_window_setLayer_OSX(GfxWindow* win, void* layer) { if (win != null && win.src.view != null) objc_send((ObjcId)win.src.view, ObjcSendVoidId, "setLayer:", (ObjcId)layer); }
// public
fn void* rgfw_window_getWindow_OSX(GfxWindow* win) => win != null ? win.src.window : null;

// public
fn void? rgfw_createWindowPlatform(String name, GfxWindowFlags flags, GfxWindow* win)
{
	NSUInteger style = NS_WINDOW_STYLE_TITLED | NS_WINDOW_STYLE_CLOSABLE | NS_WINDOW_STYLE_MINIATURIZABLE;
	if (!flags.no_resize) style |= NS_WINDOW_STYLE_RESIZABLE;
	if (flags.no_border) style = NS_WINDOW_STYLE_BORDERLESS;
	CGRect rect = ns_rect((double)win.x, (double)rgfw_cocoaYTransform((float)(win.y + win.height - 1)), (double)win.width, (double)win.height);
	ObjcId nswin = objc_send(ns_alloc("NSWindow"), ObjcSendIdWindowInit, "initWithContentRect:styleMask:backing:defer:", rect, style, NS_BACKING_STORE_BUFFERED, false);
	if (!nswin) return NSWINDOW_CREATION_FAILED~;
	ObjcId view = rgfw_osxViewClass != (ObjcClass)null ? objc_send((ObjcId)rgfw_osxViewClass, ObjcSendId, "alloc") : ns_alloc("NSView");
	view = objc_send(view, ObjcSendIdRect, "initWithFrame:", ns_rect(0.0, 0.0, (double)win.width, (double)win.height));
	if (rgfw_osxViewClass != (ObjcClass)null) object_setInstanceVariable(view, "gfx_window", win);
	ObjcId delegate = null;
	if (rgfw_osxWindowDelegateClass != (ObjcClass)null)
	{
		delegate = objc_send((ObjcId)rgfw_osxWindowDelegateClass, ObjcSendId, "alloc");
		delegate = objc_send(delegate, ObjcSendId, "init");
		object_setInstanceVariable(delegate, "gfx_window", win);
		objc_send(nswin, ObjcSendVoidId, "setDelegate:", delegate);
	}
	win.src.window = ns_retain(nswin);
	win.src.view = ns_retain(view);
	win.src.delegate = delegate;
	win.src.bounds = win.bounds;
	objc_send(nswin, ObjcSendVoidId, "setContentView:", view);
	objc_send(nswin, ObjcSendVoidId, "makeFirstResponder:", view);
	objc_send(nswin, ObjcSendVoidBool, "setAcceptsMouseMovedEvents:", true);
	win.set_name(name);
	rgfw_window_setFloating(win, flags.floating);
	if (!(flags.hide)) _window_show_native(win);
	if (flags.maximize) rgfw_window_maximizePlatform(win);
	if (flags.minimize) rgfw_window_minimizePlatform(win);
	if (flags.fullscreen) _window_set_fullscreen_native(win, true);
}

// public
fn void rgfw_window_closePlatform(GfxWindow* win)
{
	if (win == null) return;
	if (win.src.window != null) objc_send((ObjcId)win.src.window, ObjcSendVoid, "close");
	ns_release((ObjcId)win.src.delegate);
	ns_release((ObjcId)win.src.view);
	ns_release((ObjcId)win.src.window);
	win.src.delegate = null;
	win.src.view = null;
	win.src.window = null;
}

// public
fn bool rgfw_window_setMousePlatform(GfxWindow* win, RgfwMouse* mouse)
{
	if (mouse == null) return false;
	objc_send((ObjcId)mouse, ObjcSendVoid, "set");
	return true;
}



// public
fn void _window_restore_native(GfxWindow* win)
{
	if (win == null || win.src.window == null) return;
	objc_send((ObjcId)win.src.window, ObjcSendVoidId, "deminiaturize:", null);
	win.internal.flags.minimize = false;
	win.internal.flags.maximize = false;
}

// public
fn void rgfw_window_minimizePlatform(GfxWindow* win) { if (win != null && win.src.window != null) objc_send((ObjcId)win.src.window, ObjcSendVoidId, "miniaturize:", null); }
// public
fn void _window_move_native(GfxWindow* win, int x, int y)
{
	if (win == null || win.src.window == null) return;
	CGRect content = objc_send((ObjcId)win.src.view, ObjcSendRect, "frame");
	win.x = x;
	win.y = (int)rgfw_cocoaYTransform((float)y + (float)content.size.height - 1.0f);
	CGPoint origin = { .x = (double)x, .y = (double)y };
	objc_send((ObjcId)win.src.window, ObjcSendVoidPoint, "setFrameOrigin:", origin);
}
// public
// public
fn void rgfw_window_maximizePlatform(GfxWindow* win) { if (win != null && win.src.window != null) objc_send((ObjcId)win.src.window, ObjcSendVoidId, "zoom:", null); }
// public
fn void _window_set_capture_mouse_native(GfxWindow* win, bool state) { cgAssociateMouseAndMouseCursorPosition(!state); }
// public
fn void _window_set_raw_mouse_mode_native(GfxWindow* win, bool state) { _gfx_ctx.raw_mouse = state; }
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
	if (self.native.native0) ns_release((ObjcId)self.native.native0);
	if (self.native.buffer) free(self.native.buffer);
	*self = {};
}

// public
fn RgfwMouse* rgfw_createMouse(char* data, int w, int h, GfxFormat format)
{
	if (data == null) return null;
	ObjcId rep = ns_bitmapRep(w, h);
	char* pixels = ns_bitmapData(rep);
	if (pixels == null) { ns_release(rep); return null; }
	copy_image_data(pixels, w, h, RGBA8, data, format, null);
	ObjcId image = ns_imageWithRep(rep, w, h);
	CGPoint hot;
	hot.x = 0.0; hot.y = 0.0;
	ObjcId cursor = objc_send(ns_alloc("NSCursor"), ObjcSendIdImagePoint, "initWithImage:hotSpot:", image, hot);
	ns_release(image);
	ns_release(rep);
	return (RgfwMouse*)cursor;
}

// public
fn RgfwMouse* rgfw_createMouseStandard(GfxMouseIcon mouse)
{
	ObjcId cls = objc_class("NSCursor");
	ObjcId cursor = null;
	switch (mouse)
	{
		case NORMAL: cursor = objc_send(cls, ObjcSendId, "arrowCursor");
		case ARROW: cursor = objc_send(cls, ObjcSendId, "arrowCursor");
		case IBEAM: cursor = objc_send(cls, ObjcSendId, "IBeamCursor");
		case CROSSHAIR: cursor = objc_send(cls, ObjcSendId, "crosshairCursor");
		case POINTING_HAND: cursor = objc_send(cls, ObjcSendId, "pointingHandCursor");
		case RESIZE_EW: cursor = objc_send(cls, ObjcSendId, "resizeLeftRightCursor");
		case RESIZE_E: cursor = objc_send(cls, ObjcSendId, "resizeLeftRightCursor");
		case RESIZE_W: cursor = objc_send(cls, ObjcSendId, "resizeLeftRightCursor");
		case RESIZE_NS: cursor = objc_send(cls, ObjcSendId, "resizeUpDownCursor");
		case RESIZE_N: cursor = objc_send(cls, ObjcSendId, "resizeUpDownCursor");
		case RESIZE_S: cursor = objc_send(cls, ObjcSendId, "resizeUpDownCursor");
		case RESIZE_NWSE: cursor = objc_send(cls, ObjcSendId, "_windowResizeNorthWestSouthEastCursor");
		case RESIZE_NW: cursor = objc_send(cls, ObjcSendId, "_windowResizeNorthWestSouthEastCursor");
		case RESIZE_SE: cursor = objc_send(cls, ObjcSendId, "_windowResizeNorthWestSouthEastCursor");
		case RESIZE_NESW: cursor = objc_send(cls, ObjcSendId, "_windowResizeNorthEastSouthWestCursor");
		case RESIZE_NE: cursor = objc_send(cls, ObjcSendId, "_windowResizeNorthEastSouthWestCursor");
		case RESIZE_SW: cursor = objc_send(cls, ObjcSendId, "_windowResizeNorthEastSouthWestCursor");
		case RESIZE_ALL: cursor = objc_send(cls, ObjcSendId, "openHandCursor");
		case NOT_ALLOWED: cursor = objc_send(cls, ObjcSendId, "operationNotAllowedCursor");
		case WAIT: cursor = objc_send(cls, ObjcSendId, "arrowCursor");
		case PROGRESS: cursor = objc_send(cls, ObjcSendId, "arrowCursor");
		default: cursor = objc_send(cls, ObjcSendId, "arrowCursor");
	}
	return (RgfwMouse*)ns_retain(cursor);
}

// public
fn void rgfw_freeMouse(RgfwMouse* mouse) { ns_release((ObjcId)mouse); }

// public
fn bool rgfw_monitor_getWorkarea(GfxMonitor* monitor, int* x, int* y, int* width, int* height)
{
	CGRect bounds = cgDisplayBounds(rgfw_monitorDisplay(monitor));
	if (x != null) *x = (int)bounds.origin.x;
	if (y != null) *y = (int)bounds.origin.y;
	if (width != null) *width = (int)bounds.size.width;
	if (height != null) *height = (int)bounds.size.height;
	return true;
}

// public
fn GfxMonitorMode[] rgfw_monitor_getModesPtr(GfxMonitor* monitor)
{
	CFArrayRef list = cgDisplayCopyAllDisplayModes(rgfw_monitorDisplay(monitor), null);
	if (!list) return {};

	List{GfxMonitorMode} modes;
	int count = (int)cfArrayGetCount(list);
	for (int i = 0; i < count; i++)
	{
		CGDisplayModeRef mode = (CGDisplayModeRef)cfArrayGetValueAtIndex(list, (CFIndex)i);
		modes.push({
			.w = (int)cgDisplayModeGetWidth(mode),
			.h = (int)cgDisplayModeGetHeight(mode),
			.refresh_rate = (float)cgDisplayModeGetRefreshRate(mode),
			.red = 8,
			.green = 8,
			.blue = 8,
			.src = mode
		});
	}
	cfRelease(list);
	return modes.array_view();
}

// public
fn sz rgfw_monitor_getGammaRampPtr(GfxMonitor* monitor, RgfwGammaRamp* ramp)
{
	CGDirectDisplayID display = rgfw_monitorDisplay(monitor);
	uint count = cgDisplayGammaTableCapacity(display);
	if (ramp == null) return (sz)count;
	float* red = mem::alloc_array(float, count);
	float* green = mem::alloc_array(float, count);
	float* blue = mem::alloc_array(float, count);
	uint actual = 0;
	if (red == null || green == null || blue == null || cgGetDisplayTransferByTable(display, count, red, green, blue, &actual) != 0) { free(red); free(green); free(blue); return 0; }
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
fn bool rgfw_monitor_setGammaRamp(GfxMonitor* monitor, RgfwGammaRamp* ramp)
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
	int err = cgSetDisplayTransferByTable(rgfw_monitorDisplay(monitor), (uint)ramp.count, red, green, blue);
	free(red); free(green); free(blue);
	return err == 0;
}

// public
fn bool rgfw_monitor_requestMode(GfxMonitor* mon, GfxMonitorMode* mode, GfxMonitorModeRequest request) => rgfw_monitor_setMode(mon, mode);
// public
fn bool rgfw_monitor_setMode(GfxMonitor* mon, GfxMonitorMode* mode) { if (mode == null || mode.src == null) return false; return cgDisplaySetDisplayMode(rgfw_monitorDisplay(mon), (CGDisplayModeRef)mode.src, null) == 0; }
// public
fn GfxMonitor* find_monitor(GfxWindow* win) => get_primary_monitor() ?? null;
// public
fn IntVec2 rgfw_window_fetchSize(GfxWindow* win)
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
fn void rgfw_window_raise(GfxWindow* win) { if (win != null && win.src.window != null) objc_send((ObjcId)win.src.window, ObjcSendVoidId, "orderFrontRegardless", null); }

// public
fn void _window_set_fullscreen_native(GfxWindow* win, bool fullscreen) { if (win != null && win.src.window != null) objc_send((ObjcId)win.src.window, ObjcSendVoidId, "toggleFullScreen:", null); if (win != null) win.internal.flags.fullscreen = fullscreen; }
// public
fn void rgfw_window_setFloating(GfxWindow* win, bool floating) { if (win != null && win.src.window != null) objc_send((ObjcId)win.src.window, ObjcSendVoidNSInteger, "setLevel:", floating ? NS_WINDOW_LEVEL_FLOATING : NS_WINDOW_LEVEL_NORMAL); if (win != null) win.internal.flags.floating = floating; }
// public
fn void rgfw_window_setOpacity(GfxWindow* win, char opacity) { if (win != null && win.src.window != null) objc_send((ObjcId)win.src.window, ObjcSendVoidCGFloat, "setAlphaValue:", (CGFloat)opacity / 255.0); }
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
		objc_send(titleBarView, ObjcSendVoidBool, "setHidden:", true);
		offset = (double)(frame.size.height - content.size.height);
	}

	win.resize(win.width, win.height + (int)offset);
	win.height -= (int)offset;
}
// public
fn void rgfw_window_setMousePassthrough(GfxWindow* win, bool passthrough) { if (win != null && win.src.window != null) objc_send((ObjcId)win.src.window, ObjcSendVoidBool, "setIgnoresMouseEvents:", passthrough); }


fn void GfxWindow.set_name(&self, String name)
{
	if (self.src.window) objc_send((ObjcId)self.src.window, ObjcSendVoidId, "setTitle:", name.to_nsstring());
}

// public

fn void _window_hide_native(GfxWindow* win) @private
{
	if (win.src.window) objc_send((ObjcId)win.src.window, ObjcSendVoidId, "orderOut:", null);
	win.internal.flags.hide = true;
}

fn void _window_flash_native(GfxWindow* win, GfxFlashRequest request) @private
{
	if (_gfx_ctx.nsApp) objc_send(_gfx_ctx.nsApp, ObjcSendIntegerNSUInteger, "requestUserAttention:", (NSUInteger)request);
}

// public
fn bool _window_is_minimized_native(GfxWindow* win) { if (win != null && win.src.window != null) return objc_send((ObjcId)win.src.window, ObjcSendBool, "isMiniaturized"); return win.internal.flags.minimize; }
// public
fn bool _window_is_maximized_native(GfxWindow* win) { if (win == null) return false; if (win.src.window != null) return objc_send((ObjcId)win.src.window, ObjcSendBool, "isZoomed"); return win.internal.flags.maximize; }

// public


// public
fn bool rgfw_getGlobalMouse(int* x, int* y)
{
	CGEventRef event = cgEventCreate(null);
	if (event == null) return false;
	CGPoint point = cgEventGetLocation(event);
	cfRelease(event);
	if (x != null) *x = (int)point.x;
	if (y != null) *y = (int)point.y;
	return true;
}

<*
 @param [in] win
*>
// public
fn void rgfw_window_moveMouse(GfxWindow* win, int x, int y)
{
	cgWarpMouseCursorPosition({ x, y });
	if (win) win.internal.last_mouse = { x, y };
}

// public
fn bool rgfw_readClipboardPtr(RgfwDataTransferType requestedType, char* buffer, sz capacity, GfxDataTransfer* data)
{
	if (requestedType != RGFW_DATA_TEXT || data == null) return false;
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
	else { data.data = raw; }
	data.length = len;
	data.type = RGFW_DATA_TEXT;
	return true;
}

// public
fn bool rgfw_writeClipboard(GfxDataTransfer* data)
{
	if (data == null || data.type != RGFW_DATA_TEXT || data.data == null) return false;
	ObjcId pb = objc_send(objc_class("NSPasteboard"), ObjcSendId, "generalPasteboard");
	objc_send(pb, ObjcSendVoid, "clearContents");
	return objc_send(pb, ObjcSendBoolIdId, "setString:forType:", ns_string((ZString)data.data), ns_string("public.utf8-plain-text"));
}

// public
fn void* rgfw_eglContext_getSourceContext(RgfwEglContext* ctx) => ctx != null ? ctx.ctx : null;
// public
fn void* rgfw_eglContext_getSurface(RgfwEglContext* ctx) => ctx != null ? ctx.surface : null;
// public
fn void* rgfw_eglContext_wlEGLWindow(RgfwEglContext* ctx) => ctx != null ? ctx.eglWindow : null;
const EglInt EGL_NONE = 0x3038;
const EglInt EGL_RED_SIZE = 0x3024;
const EglInt EGL_GREEN_SIZE = 0x3023;
const EglInt EGL_BLUE_SIZE = 0x3022;
const EglInt EGL_ALPHA_SIZE = 0x3021;
const EglInt EGL_DEPTH_SIZE = 0x3025;
const EglInt EGL_STENCIL_SIZE = 0x3026;
const EglInt EGL_RENDERABLE_TYPE = 0x3040;
const EglInt EGL_SURFACE_TYPE = 0x3033;
const EglInt EGL_WINDOW_BIT = 0x0004;
const EglInt EGL_OPENGL_BIT = 0x0008;
const EglInt EGL_OPENGL_ES2_BIT = 0x0004;
const EglInt EGL_OPENGL_API = 0x30a2;
const EglInt EGL_OPENGL_ES_API = 0x30a0;
const EglInt EGL_CONTEXT_CLIENT_VERSION = 0x3098;
const EglInt EGL_EXTENSIONS = 0x3055;

// public
fn bool rgfw_extensionSupportedPlatform_EGL(ZString extension, sz len)
{
	if (rgfw_eglQueryString == null || _gfx_ctx.egl_display == null) return false;
	ZString extensions = rgfw_eglQueryString((EglDisplay)_gfx_ctx.egl_display, EGL_EXTENSIONS);
	return rgfw_extensionSupportedStr(extensions, extension, len);
}

// public
fn bool rgfw_extensionSupported_EGL(ZString extension, sz len)
{
	if (rgfw_extensionSupported_base(extension, len, &rgfw_getProcAddress_EGL)) return true;
	return rgfw_extensionSupportedPlatform_EGL(extension, len);
}


// public
fn RgfwEglContext* rgfw_window_createContext_EGL(GfxWindow* win, RgfwGlHints* hints)
{
	RgfwEglContext* ctx = mem::new(RgfwEglContext);
	if (!rgfw_window_createContextPtr_EGL(win, ctx, hints))
	{
		free(ctx);
		return null;
	}
	win.src.gfxType |= RGFW_GFX_OWNED_BY_RGFW;
	return ctx;
}

// public
fn bool rgfw_window_createContextPtr_EGL(GfxWindow* win, RgfwEglContext* ctx, RgfwGlHints* hints)
{
	if (win == null || ctx == null || hints == null) return false;
	if (load_egl() == false) return false;
	if (_gfx_ctx.egl_display == null)
	{
		_gfx_ctx.egl_display = rgfw_eglGetDisplay(null);
		if (_gfx_ctx.egl_display == null) return false;
		EglInt major = 0;
		EglInt minor = 0;
		if (rgfw_eglInitialize((EglDisplay)_gfx_ctx.egl_display, &major, &minor) == 0) return false;
	}

	EglInt[32] cfg;
	RgfwAttribStack cfgStack;
	rgfw_attribStack_init(&cfgStack, &cfg, 32);
	rgfw_attribStack_pushAttribs(&cfgStack, EGL_SURFACE_TYPE, EGL_WINDOW_BIT);
	rgfw_attribStack_pushAttribs(&cfgStack, EGL_RED_SIZE, hints.red);
	rgfw_attribStack_pushAttribs(&cfgStack, EGL_GREEN_SIZE, hints.green);
	rgfw_attribStack_pushAttribs(&cfgStack, EGL_BLUE_SIZE, hints.blue);
	rgfw_attribStack_pushAttribs(&cfgStack, EGL_ALPHA_SIZE, hints.alpha);
	rgfw_attribStack_pushAttribs(&cfgStack, EGL_DEPTH_SIZE, hints.depth);
	rgfw_attribStack_pushAttribs(&cfgStack, EGL_STENCIL_SIZE, hints.stencil);
	rgfw_attribStack_pushAttribs(&cfgStack, EGL_RENDERABLE_TYPE, hints.profile == RGFW_GL_ES ? EGL_OPENGL_ES2_BIT : EGL_OPENGL_BIT);
	rgfw_attribStack_pushAttribs(&cfgStack, EGL_NONE, EGL_NONE);
	EglConfig config = null;
	EglInt num = 0;
	if (rgfw_eglChooseConfig((EglDisplay)_gfx_ctx.egl_display, &cfg, &config, 1, &num) == 0 || num == 0) return false;
	rgfw_eglBindApi(hints.profile == RGFW_GL_ES ? (uint)EGL_OPENGL_ES_API : (uint)EGL_OPENGL_API);
	EglInt[8] ctxAttribs;
	RgfwAttribStack ctxStack;
	rgfw_attribStack_init(&ctxStack, &ctxAttribs, 8);
	if (hints.profile == RGFW_GL_ES) rgfw_attribStack_pushAttribs(&ctxStack, EGL_CONTEXT_CLIENT_VERSION, hints.major != 0 ? hints.major : 2);
	rgfw_attribStack_pushAttribs(&ctxStack, EGL_NONE, EGL_NONE);
	EglContext share = hints.shareEGL != null ? (EglContext)hints.shareEGL.ctx : null;
	ctx.ctx = rgfw_eglCreateContext((EglDisplay)_gfx_ctx.egl_display, config, share, &ctxAttribs);
	if (ctx.ctx == null) return false;
	EglNativeWindowType nativeWindow = win.src.window;
	if (win.src.view != null) nativeWindow = rgfw_getLayer_OSX();
	ctx.surface = rgfw_eglCreateWindowSurface((EglDisplay)_gfx_ctx.egl_display, config, nativeWindow, null);
	if (ctx.surface == null)
	{
		rgfw_eglDestroyContext((EglDisplay)_gfx_ctx.egl_display, (EglContext)ctx.ctx);
		ctx.ctx = null;
		return false;
	}
	win.src.ctx.egl = ctx;
	win.src.gfxType = RGFW_GFX_EGL;
	rgfw_window_makeCurrentContext_EGL(win);
	return true;
}

// public
fn RgfwEglContext* rgfw_window_getContext_EGL(GfxWindow* win) => win != null ? win.src.ctx.egl : null;

// public
fn void rgfw_window_deleteContext_EGL(GfxWindow* win, RgfwEglContext* ctx)
{
	rgfw_window_deleteContextPtr_EGL(win, ctx);
	if (win != null && (win.src.gfxType & RGFW_GFX_OWNED_BY_RGFW)) free(ctx);
}

// public
fn void rgfw_window_deleteContextPtr_EGL(GfxWindow* win, RgfwEglContext* ctx)
{
	if (ctx == null || _gfx_ctx.egl_display == null) return;
	if (ctx.surface != null) rgfw_eglDestroySurface((EglDisplay)_gfx_ctx.egl_display, (EglSurface)ctx.surface);
	if (ctx.ctx != null) rgfw_eglDestroyContext((EglDisplay)_gfx_ctx.egl_display, (EglContext)ctx.ctx);
	ctx.surface = null;
	ctx.ctx = null;
	if (win != null) win.src.ctx.egl = null;
}

// public
fn void rgfw_window_makeCurrentWindow_EGL(GfxWindow* win) { _gfx_ctx.current = win; }

alias WGPUInstance = void*;
alias WGPUSurface = void*;
alias WGPUSType = int;
const WGPUSType WGPU_STYPE_SURFACE_SOURCE_METAL_LAYER = 0x00000004;
struct WgpuChainedStruct { WgpuChainedStruct* next; WGPUSType sType; }
struct WgpuSurfaceDescriptor { WgpuChainedStruct* nextInChain; }
struct WgpuSurfaceSourceMetalLayer { WgpuChainedStruct chain; void* layer; }
extern fn WGPUSurface wgpuInstanceCreateSurface(WGPUInstance instance, WgpuSurfaceDescriptor* descriptor) @cname("wgpuInstanceCreateSurface");

// public
fn WGPUSurface rgfw_window_createSurface_WebGPU(GfxWindow* window, WGPUInstance instance)
{
	if (window == null || window.src.view == null)
	{
		log::error("NSView is NULL for macOS window", category: log::CATEGORY_VIDEO);
		return null;
	}
	objc_send((ObjcId)window.src.view, ObjcSendVoidBool, "setWantsLayer:", true);
	void* layer = rgfw_getLayer_OSX();
	if (layer == null) return null;
	rgfw_window_setLayer_OSX(window, layer);

	WgpuSurfaceSourceMetalLayer fromMetal;
	fromMetal.chain.next = null;
	fromMetal.chain.sType = WGPU_STYPE_SURFACE_SOURCE_METAL_LAYER;
	fromMetal.layer = layer;
	WgpuSurfaceDescriptor surfaceDesc;
	surfaceDesc.nextInChain = (WgpuChainedStruct*)&fromMetal;
	return wgpuInstanceCreateSurface(instance, &surfaceDesc);
}
// public
fn void* rgfw_getNSScreenForDisplayUInt(uint uintNum)
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
fn void rgfw_osx_initView(GfxWindow* win)
{
	if (win == null || win.src.window == null || win.src.view == null) return;
	objc_send((ObjcId)win.src.window, ObjcSendVoidId, "setContentView:", (ObjcId)win.src.view);
	objc_send((ObjcId)win.src.window, ObjcSendVoidId, "makeFirstResponder:", (ObjcId)win.src.view);
}
// public
fn float rgfw_cocoaYTransform(float y) => (float)(cgDisplayBounds(cgMainDisplayID()).size.height - (double)y - 1.0);

// public
fn GfxDataTransfer* rgfw_readClipboard(RgfwDataTransferType requestedType)
{
	return _gfx_ctx.clipboard;
}

// public
fn GfxDataTransfer* rgfw_readClipboardString() { return rgfw_readClipboard(RGFW_DATA_TEXT); }
// public
fn bool rgfw_readClipboardStringPtr(char* buffer, sz capacity, GfxDataTransfer* data) { return rgfw_readClipboardPtr(RGFW_DATA_TEXT, buffer, capacity, data); }
