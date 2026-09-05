module std::gfx @link("Cocoa.framework", "CoreGraphics.framework", "ApplicationServices.framework", "QuartzCore.framework", "Carbon.framework", "CoreFoundation.framework", "objc");

// C3 port of the shared RGFW implementation from /Users/lerno/Downloads/RGFW.h.
// This file intentionally imports no C3 standard-library modules. It only
// declares the small C runtime hooks RGFW's C header also expects from the host.


const bool RGFW_BUILD_MACOS = true;
const bool RGFW_BUILD_X11 = false;
const bool RGFW_BUILD_WAYLAND = false;
const bool RGFW_BUILD_WINDOWS = false;
const bool RGFW_BUILD_EGL = true;

extern fn void* malloc(usz size);
extern fn void free(void* ptr);
extern fn void* memcpy(void* dest, void* src, usz count);
extern fn void* memset(void* dest, int value, usz count);
extern fn usz strlen(char* str);
extern fn char* strstr(char* haystack, char* needle);
extern fn float powf(float x, float y);
extern fn void* dlopen(char* path, int mode);
extern fn void* dlsym(void* handle, char* symbol);
extern fn int dlclose(void* handle);
extern fn int chdir(char* path);
extern fn int clock_gettime(int clock_id, Timespec* ts);

struct Timespec { long tv_sec; long tv_nsec; }

alias ObjcId = void*;
typedef ObjcClass = void*;
typedef ObjcSelector = void*;
alias CGFloat = double;
alias NSUInteger = ulong;
alias NSInteger = long;
alias CFIndex = long;
alias CFArrayRef = void*;
alias CFStringRef = void*;
alias CFBundleRef = void*;
alias CGDirectDisplayID = uint;
alias CGDisplayModeRef = void*;
alias CGEventRef = void*;
alias VkInstance = void*;
alias VkPhysicalDevice = void*;
alias VkSurfaceKHR = ulong;
alias VkResult = int;
alias RgfwVkGetInstanceProcAddressProc = fn RgfwProc(VkInstance instance, char* procname);
struct VkMetalSurfaceCreateInfoEXT { int sType; void* pNext; uint flags; void* pLayer; }
alias RgfwVkCreateMetalSurfaceEXTProc = fn VkResult(VkInstance instance, VkMetalSurfaceCreateInfoEXT* createInfo, void* allocator, VkSurfaceKHR* surface);
alias EglDisplay = void*;
alias EglContext = void*;
alias EglSurface = void*;
alias EglConfig = void*;
alias EglNativeDisplayType = void*;
alias EglNativeWindowType = void*;
alias EglBoolean = uint;
alias EglInt = int;
alias EglGetProcAddressProc = fn RgfwProc(char* procname);
alias EglGetDisplayProc = fn EglDisplay(EglNativeDisplayType display);
alias EglInitializeProc = fn EglBoolean(EglDisplay display, EglInt* major, EglInt* minor);
alias EglTerminateProc = fn EglBoolean(EglDisplay display);
alias EglBindApiProc = fn EglBoolean(uint api);
alias EglChooseConfigProc = fn EglBoolean(EglDisplay display, EglInt* attribs, EglConfig* configs, EglInt configSize, EglInt* numConfig);
alias EglCreateContextProc = fn EglContext(EglDisplay display, EglConfig config, EglContext shareContext, EglInt* attribs);
alias EglDestroyContextProc = fn EglBoolean(EglDisplay display, EglContext context);
alias EglCreateWindowSurfaceProc = fn EglSurface(EglDisplay display, EglConfig config, EglNativeWindowType window, EglInt* attribs);
alias EglDestroySurfaceProc = fn EglBoolean(EglDisplay display, EglSurface surface);
alias EglMakeCurrentProc = fn EglBoolean(EglDisplay display, EglSurface draw, EglSurface read, EglContext context);
alias EglSwapBuffersProc = fn EglBoolean(EglDisplay display, EglSurface surface);
alias EglSwapIntervalProc = fn EglBoolean(EglDisplay display, EglInt interval);
alias EglGetCurrentContextProc = fn EglContext();
alias EglQueryStringProc = fn char*(EglDisplay display, EglInt name);
alias EglGetConfigAttribProc = fn EglBoolean(EglDisplay display, EglConfig config, EglInt attribute, EglInt* value);
alias XOpenDisplayProc = fn void*(char* displayName);
alias XCloseDisplayProc = fn int(void* display);
alias XDefaultRootWindowProc = fn ulong(void* display);
alias XCreateSimpleWindowProc = fn ulong(void* display, ulong parent, int x, int y, uint width, uint height, uint borderWidth, ulong border, ulong background);
alias XStoreNameProc = fn int(void* display, ulong window, char* name);
alias XMapRaisedProc = fn int(void* display, ulong window);
alias XMapWindowProc = fn int(void* display, ulong window);
alias XUnmapWindowProc = fn int(void* display, ulong window);
alias XDestroyWindowProc = fn int(void* display, ulong window);
alias XMoveWindowProc = fn int(void* display, ulong window, int x, int y);
alias XResizeWindowProc = fn int(void* display, ulong window, uint width, uint height);
alias XFlushProc = fn int(void* display);
alias XPendingProc = fn int(void* display);
alias WlDisplayConnectProc = fn void*(char* name);
alias WlDisplayDisconnectProc = fn void(void* display);
alias WlDisplayDispatchPendingProc = fn int(void* display);
alias WlDisplayRoundtripProc = fn int(void* display);
alias WlDisplayGetFdProc = fn int(void* display);

struct CGPoint { CGFloat x, y; }
struct CGSize { CGFloat width, height; }
struct CGRect { CGPoint origin; CGSize size; }

extern fn void objc_msgSendRaw(...) @cname("objc_msgSend") @builtin;
extern fn ObjcSelector sel_registerName(char* name);
extern fn ObjcClass objc_getClass(char* name);
extern fn ObjcClass objc_allocateClassPair(ObjcClass superclass, char* name, usz extraBytes);
extern fn void objc_registerClassPair(ObjcClass cls);
extern fn bool class_addIvar(ObjcClass cls, char* name, usz size, char alignment, char* types);
extern fn bool class_addMethod(ObjcClass cls, ObjcSelector name, void* imp, char* types);
extern fn void* object_setInstanceVariable(ObjcId obj, char* name, void* value);
extern fn void* object_getInstanceVariable(ObjcId obj, char* name, void* outValue);
extern fn void* objc_autoreleasePoolPush();
extern fn void objc_autoreleasePoolPop(void* context);

extern fn CGRect cgDisplayBounds(CGDirectDisplayID display) @cname("CGDisplayBounds");
extern fn CGDirectDisplayID cgMainDisplayID() @cname("CGMainDisplayID");
extern fn int cgGetActiveDisplayList(uint maxDisplays, CGDirectDisplayID* activeDisplays, uint* displayCount) @cname("CGGetActiveDisplayList");
extern fn CGDisplayModeRef cgDisplayCopyDisplayMode(CGDirectDisplayID display) @cname("CGDisplayCopyDisplayMode");
extern fn CFArrayRef cgDisplayCopyAllDisplayModes(CGDirectDisplayID display, void* options) @cname("CGDisplayCopyAllDisplayModes");
extern fn usz cgDisplayModeGetWidth(CGDisplayModeRef mode) @cname("CGDisplayModeGetWidth");
extern fn usz cgDisplayModeGetHeight(CGDisplayModeRef mode) @cname("CGDisplayModeGetHeight");
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
extern fn CFStringRef cfStringCreateWithCString(void* alloc, char* str, uint encoding) @cname("CFStringCreateWithCString");
extern fn CFBundleRef cfBundleGetBundleWithIdentifier(CFStringRef bundleID) @cname("CFBundleGetBundleWithIdentifier");
extern fn void* cfBundleGetFunctionPointerForName(CFBundleRef bundle, CFStringRef functionName) @cname("CFBundleGetFunctionPointerForName");

macro objc_send(id, $FunctionType, char* $selector, ...)
{
	return (($FunctionType)&objc_msgSendRaw)((ObjcId)id, sel_registerName($selector), ...$vaarg);
}

alias ObjcSendId = fn ObjcId(ObjcId, ObjcSelector);
alias ObjcSendIdCharPtr = fn ObjcId(ObjcId, ObjcSelector, char*);
alias ObjcSendCharPtr = fn char*(ObjcId, ObjcSelector);
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
alias ObjcSendBitmapRepInit = fn ObjcId(ObjcId, ObjcSelector, char**, NSInteger, NSInteger, NSInteger, NSInteger, bool, bool, ObjcId, NSUInteger, NSInteger, NSInteger);
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

struct RgfwNativeImage
{
	RgfwFormat format;
	char* buffer;
	void* native0;
	void* native1;
	void* native2;
}

struct RgfwWindowSrc
{
	void* window;
	void* view;
	void* mouse;
	void* delegate;
	RgfwGfxContext ctx;
	RgfwGfxContextType gfxType;
	int x, y, w, h;
}

struct RgfwMonitorNode
{
	GfxMonitor mon;
	bool disconnected;
	RgfwMonitorNode* next;
	void* native0;
	void* native1;
	ulong nativeId0;
	ulong nativeId1;
}

struct RgfwInfo
{
	GfxWindow* root;
	int windowCount;
	RgfwMouse* hiddenMouse;
	RgfwMouse*[21] standardMice;
	RgfwDebugFunc debugCallbackSrc;
	RgfwGenericFunc[25] callbacks;
	GfxEvent[MAX_EVENTS] events;
	int eventBottom;
	int eventLen;
	bool queueEvents;
	bool polledEvents;
	uint[RGFW_KEY_LAST] apiKeycodes;
	RgfwKey[256] keycodes;
	bool stopCheckEvents_bool;
	ulong timerOffset;
	GfxDataTransfer* clipboard;
	bool dndBuild;
	GfxDataDropNode* dndRoot;
	GfxDataDropNode* dndCur;
	GfxMonitors monitors;
	GfxWindow* current;
	void* nativeGL_handle;
	void* glGetProcAddress;
	void* vulkan_handle;
	RgfwVkGetInstanceProcAddressProc vkGetInstanceProcAddress;
	void* egl_display;
	void* egl_handle;
	void* eglGetProcAddress;
	void* x11_handle;
	void* x11_display;
	ulong x11_root;
	void* wayland_handle;
	void* wl_display;
	ObjcId nsApp;
	ObjcId nsPool;
	NSUInteger windowNumberSeed;
	bool rawMouse;
	RgfwWindowState windowState;
	RgfwKeyState[8] mouseButtons;
	RgfwKeyState[RGFW_KEY_LAST] keyboard;
	float scrollX, scrollY;
	float vectorX, vectorY;
}

RgfwInfo* _rgfw;
RgfwInfo _rgfw_global;
ObjcClass rgfw_osxWindowDelegateClass;
ObjcClass rgfw_osxViewClass;
EglGetProcAddressProc rgfw_eglGetProcAddress;
EglGetDisplayProc rgfw_eglGetDisplay;
EglInitializeProc rgfw_eglInitialize;
EglTerminateProc rgfw_eglTerminate;
EglBindApiProc rgfw_eglBindApi;
EglChooseConfigProc rgfw_eglChooseConfig;
EglCreateContextProc rgfw_eglCreateContext;
EglDestroyContextProc rgfw_eglDestroyContext;
EglCreateWindowSurfaceProc rgfw_eglCreateWindowSurface;
EglDestroySurfaceProc rgfw_eglDestroySurface;
EglMakeCurrentProc rgfw_eglMakeCurrent;
EglSwapBuffersProc rgfw_eglSwapBuffers;
EglSwapIntervalProc rgfw_eglSwapInterval;
EglGetCurrentContextProc rgfw_eglGetCurrentContext;
EglQueryStringProc rgfw_eglQueryString;
EglGetConfigAttribProc rgfw_eglGetConfigAttrib;
XOpenDisplayProc rgfw_xOpenDisplay;
XCloseDisplayProc rgfw_xCloseDisplay;
XDefaultRootWindowProc rgfw_xDefaultRootWindow;
XCreateSimpleWindowProc rgfw_xCreateSimpleWindow;
XStoreNameProc rgfw_xStoreName;
XMapRaisedProc rgfw_xMapRaised;
XMapWindowProc rgfw_xMapWindow;
XUnmapWindowProc rgfw_xUnmapWindow;
XDestroyWindowProc rgfw_xDestroyWindow;
XMoveWindowProc rgfw_xMoveWindow;
XResizeWindowProc rgfw_xResizeWindow;
XFlushProc rgfw_xFlush;
XPendingProc rgfw_xPending;
WlDisplayConnectProc rgfw_wlDisplayConnect;
WlDisplayDisconnectProc rgfw_wlDisplayDisconnect;
WlDisplayDispatchPendingProc rgfw_wlDisplayDispatchPending;
WlDisplayRoundtripProc rgfw_wlDisplayRoundtrip;
WlDisplayGetFdProc rgfw_wlDisplayGetFd;

fn void rgfw_setInfo(RgfwInfo* info) { _rgfw = info; }
fn RgfwInfo* rgfw_getInfo() { return _rgfw; }

fn bool rgfw_loadGL()
{
	CFStringRef bundleName = cfStringCreateWithCString(null, "com.apple.opengl", 0x0600);
	_rgfw.nativeGL_handle = cfBundleGetBundleWithIdentifier(bundleName);
	if (bundleName != null) cfRelease(bundleName);
	_rgfw.glGetProcAddress = (void*)&rgfw_getProcAddress_OpenGL;
	return rgfw_bool(_rgfw.nativeGL_handle != null);
}

fn void rgfw_unloadGL()
{
	_rgfw.nativeGL_handle = null;
	_rgfw.glGetProcAddress = null;
}
fn void* rgfw_eglProc(char* name)
{
	if (_rgfw.egl_handle == null) return null;
	void* proc = dlsym(_rgfw.egl_handle, name);
	if (proc == null && rgfw_eglGetProcAddress != null) proc = (void*)rgfw_eglGetProcAddress(name);
	return proc;
}

fn bool rgfw_loadEGL()
{
	if (_rgfw.egl_handle != null) return true;
	char*[5] names = {"libEGL.dylib", "libEGL.1.dylib", "libEGL.so.1", "libEGL.so", "EGL.dll"};
	for (usz i = 0; i < 5; i++)
	{
		_rgfw.egl_handle = dlopen(names[i], RTLD_LAZY | RTLD_LOCAL);
		if (_rgfw.egl_handle != null) break;
	}
	if (_rgfw.egl_handle == null) return false;
	void* getProc = dlsym(_rgfw.egl_handle, "eglGetProcAddress");
	if (getProc == null) return false;
	rgfw_eglGetProcAddress = (EglGetProcAddressProc)getProc;
	_rgfw.eglGetProcAddress = getProc;
	void* proc;
	proc = rgfw_eglProc("eglGetDisplay"); if (proc == null) return false; rgfw_eglGetDisplay = (EglGetDisplayProc)proc;
	proc = rgfw_eglProc("eglInitialize"); if (proc == null) return false; rgfw_eglInitialize = (EglInitializeProc)proc;
	proc = rgfw_eglProc("eglTerminate"); if (proc == null) return false; rgfw_eglTerminate = (EglTerminateProc)proc;
	proc = rgfw_eglProc("eglBindAPI"); if (proc == null) return false; rgfw_eglBindApi = (EglBindApiProc)proc;
	proc = rgfw_eglProc("eglChooseConfig"); if (proc == null) return false; rgfw_eglChooseConfig = (EglChooseConfigProc)proc;
	proc = rgfw_eglProc("eglCreateContext"); if (proc == null) return false; rgfw_eglCreateContext = (EglCreateContextProc)proc;
	proc = rgfw_eglProc("eglDestroyContext"); if (proc == null) return false; rgfw_eglDestroyContext = (EglDestroyContextProc)proc;
	proc = rgfw_eglProc("eglCreateWindowSurface"); if (proc == null) return false; rgfw_eglCreateWindowSurface = (EglCreateWindowSurfaceProc)proc;
	proc = rgfw_eglProc("eglDestroySurface"); if (proc == null) return false; rgfw_eglDestroySurface = (EglDestroySurfaceProc)proc;
	proc = rgfw_eglProc("eglMakeCurrent"); if (proc == null) return false; rgfw_eglMakeCurrent = (EglMakeCurrentProc)proc;
	proc = rgfw_eglProc("eglSwapBuffers"); if (proc == null) return false; rgfw_eglSwapBuffers = (EglSwapBuffersProc)proc;
	proc = rgfw_eglProc("eglSwapInterval"); if (proc == null) return false; rgfw_eglSwapInterval = (EglSwapIntervalProc)proc;
	proc = rgfw_eglProc("eglGetCurrentContext"); if (proc == null) return false; rgfw_eglGetCurrentContext = (EglGetCurrentContextProc)proc;
	proc = rgfw_eglProc("eglQueryString"); if (proc == null) return false; rgfw_eglQueryString = (EglQueryStringProc)proc;
	proc = rgfw_eglProc("eglGetConfigAttrib"); if (proc == null) return false; rgfw_eglGetConfigAttrib = (EglGetConfigAttribProc)proc;
	return true;
}

fn void rgfw_unloadEGL()
{
	if (_rgfw.egl_handle == null) return;
	if (_rgfw.egl_display != null && rgfw_eglTerminate != null) rgfw_eglTerminate((EglDisplay)_rgfw.egl_display);
	dlclose(_rgfw.egl_handle);
	_rgfw.egl_handle = null;
	_rgfw.egl_display = null;
	_rgfw.eglGetProcAddress = null;
	rgfw_eglGetProcAddress = null;
}
const int RTLD_LAZY = 0x1;
const int RTLD_LOCAL = 0x4;
const int VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT = 1000217000;
const VkResult VK_ERROR_EXTENSION_NOT_PRESENT = -7;
char*[2] rgfw_vulkanExtensions = {"VK_KHR_surface", "VK_EXT_metal_surface"};

fn bool rgfw_loadVulkan()
{
	if (_rgfw.vulkan_handle != null) return true;
	char*[3] names = {"libvulkan.1.dylib", "libvulkan.dylib", "libMoltenVK.dylib"};
	for (usz i = 0; i < 3; i++)
	{
		_rgfw.vulkan_handle = dlopen(names[i], RTLD_LAZY | RTLD_LOCAL);
		if (_rgfw.vulkan_handle != null) break;
	}
	if (_rgfw.vulkan_handle == null) return false;
	void* proc = dlsym(_rgfw.vulkan_handle, "vkGetInstanceProcAddr");
	if (proc == null) return false;
	_rgfw.vkGetInstanceProcAddress = (RgfwVkGetInstanceProcAddressProc)proc;
	return true;
}

fn void rgfw_unloadVulkan()
{
	if (_rgfw.vulkan_handle == null) return;
	dlclose(_rgfw.vulkan_handle);
	_rgfw.vulkan_handle = null;
	_rgfw.vkGetInstanceProcAddress = null;
}

fn RgfwProc rgfw_getInstanceProcAddress_Vulkan(VkInstance instance, char* procname)
{
	if (_rgfw == null || _rgfw.vkGetInstanceProcAddress == null) return null;
	return _rgfw.vkGetInstanceProcAddress(instance, procname);
}

fn char** rgfw_getRequiredInstanceExtensions_Vulkan(usz* count)
{
	if (count != null) *count = 2;
	return &rgfw_vulkanExtensions;
}

fn VkResult rgfw_window_createSurface_Vulkan(GfxWindow* win, VkInstance instance, VkSurfaceKHR* surface)
{
	if (surface == null || win == null || win.src.view == null) return VK_ERROR_EXTENSION_NOT_PRESENT;
	*surface = 0;
	RgfwProc raw = rgfw_getInstanceProcAddress_Vulkan(instance, "vkCreateMetalSurfaceEXT");
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

fn bool rgfw_getPhysicalDevicePresentationSupport_Vulkan(VkInstance instance, VkPhysicalDevice physicalDevice, uint queueFamilyIndex) => false;

fn void rgfw_debugCallback(RgfwDebugType type, RgfwErrorCode code, char* msg)
{
	RgfwDebugInfo info;
	info.type = type;
	info.code = code;
	info.msg = msg;
	if (_rgfw != null && _rgfw.debugCallbackSrc != null) _rgfw.debugCallbackSrc(&info);
}

fn RgfwDebugFunc rgfw_setDebugCallback(RgfwDebugFunc func)
{
	RgfwDebugFunc prev = _rgfw.debugCallbackSrc;
	_rgfw.debugCallbackSrc = func;
	return prev;
}

fn void rgfw_initKeycodes()
{
	rgfw_memzero((void*)&_rgfw.keycodes, RgfwKey[256]::size);
	rgfw_initKeycodesPlatform();
	for (usz i = 0; i < (usz)RGFW_KEY_LAST; i++)
	{
		for (usz y = 0; y < 256; y++)
		{
			if (_rgfw.keycodes[y] == (RgfwKey)i)
			{
				_rgfw.apiKeycodes[i] = (uint)y;
				break;
			}
		}
	}
	rgfw_resetKey();
}

fn RgfwKey rgfw_apiKeyToRGFW(uint keycode)
{
	if (keycode > 256) return RGFW_KEY_NULL;
	return _rgfw.keycodes[(usz)keycode];
}

fn uint rgfw_rgfwToApiKey(RgfwKey keycode)
{
	return _rgfw.apiKeycodes[(usz)keycode];
}

fn void rgfw_resetKey()
{
	rgfw_memzero((void*)&_rgfw.keyboard, RgfwKeyState[256]::size);
}

fn RgfwGenericFunc rgfw_setEventCallback(RgfwEventType type, RgfwGenericFunc func)
{
	RgfwGenericFunc old = _rgfw.callbacks[(usz)type];
	_rgfw.callbacks[(usz)type] = func;
	return old;
}

fn void rgfw_setDualEventCallback(RgfwEventType type, RgfwGenericFunc func, RgfwGenericFunc* first, RgfwGenericFunc* second)
{
	RgfwGenericFunc func1 = rgfw_setEventCallback(type, func);
	RgfwGenericFunc func2 = rgfw_setEventCallback((RgfwEventType)(type + 1), func);
	if (first != null) *first = func1;
	if (second != null) *second = func2;
}

fn void rgfw_setAllEventCallbacks(RgfwGenericFunc func, RgfwCallbacks* callbacks)
{
	for (RgfwEventType i = RGFW_EVENT_NONE + 1; i < RGFW_EVENT_COUNT; i++)
	{
		if (callbacks != null) callbacks.arr[(usz)i] = _rgfw.callbacks[(usz)i];
		rgfw_setEventCallback(i, func);
	}
}

fn void rgfw_windowMaximizedCallback(GfxWindow* win, int x, int y, int w, int h)
{
	win.internal.flags |= RGFW_WINDOW_MAXIMIZE;
	win.x = x; win.y = y; win.w = w; win.h = h;
	if (!(win.internal.enabledEvents & RGFW_WINDOW_MAXIMIZED_FLAG)) return;
	GfxEvent event;
	event.type = RGFW_WINDOW_MAXIMIZED;
	event.update.x = x; event.update.y = y; event.update.w = w; event.update.h = h;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void rgfw_windowMinimizedCallback(GfxWindow* win)
{
	win.internal.flags |= RGFW_WINDOW_MINIMIZE;
	if (!(win.internal.enabledEvents & RGFW_WINDOW_MINIMIZED_FLAG)) return;
	GfxEvent event;
	event.type = RGFW_WINDOW_MINIMIZED;
	event.update.x = win.x; event.update.y = win.y; event.update.w = win.w; event.update.h = win.h;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void rgfw_windowRestoredCallback(GfxWindow* win, int x, int y, int w, int h)
{
	win.internal.flags &= ~((uint)RGFW_WINDOW_MINIMIZE);
	win.x = x; win.y = y; win.w = w; win.h = h;
	if (rgfw_window_isMaximized(win) == false) win.internal.flags &= ~((uint)RGFW_WINDOW_MAXIMIZE);
	if (!(win.internal.enabledEvents & RGFW_WINDOW_RESTORED_FLAG)) return;
	GfxEvent event;
	event.type = RGFW_WINDOW_RESTORED;
	event.update.x = x; event.update.y = y; event.update.w = w; event.update.h = h;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void rgfw_windowMovedCallback(GfxWindow* win, int x, int y)
{
	win.x = x; win.y = y;
	if (!(win.internal.enabledEvents & RGFW_WINDOW_MOVED_FLAG)) return;
	GfxEvent event;
	event.type = RGFW_WINDOW_MOVED;
	event.update.x = x; event.update.y = y; event.update.w = win.w; event.update.h = win.h;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void rgfw_windowResizedCallback(GfxWindow* win, int w, int h)
{
	win.w = w; win.h = h;
	if (!(win.internal.enabledEvents & RGFW_WINDOW_RESIZED_FLAG)) return;
	GfxEvent event;
	event.type = RGFW_WINDOW_RESIZED;
	event.update.x = win.x; event.update.y = win.y; event.update.w = w; event.update.h = h;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void rgfw_windowCloseCallback(GfxWindow* win)
{
	win.internal.shouldClose = true;
	GfxEvent event;
	event.type = RGFW_WINDOW_CLOSE;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void rgfw_mouseMotionCallback(GfxWindow* win, int x, int y)
{
	win.internal.lastMouseX = x;
	win.internal.lastMouseY = y;
	if (!(win.internal.enabledEvents & RGFW_MOUSE_MOTION_FLAG)) return;
	GfxEvent event;
	event.type = RGFW_MOUSE_MOTION;
	event.mouse.x = x; event.mouse.y = y; event.mouse.inWindow = win.internal.mouseInside;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void rgfw_rawMotionCallback(GfxWindow* win, float x, float y)
{
	_rgfw.vectorX = x; _rgfw.vectorY = y;
	if (!(win.internal.enabledEvents & RGFW_MOUSE_RAW_MOTION_FLAG)) return;
	GfxEvent event;
	event.type = RGFW_MOUSE_RAW_MOTION;
	event.delta.x = x; event.delta.y = y;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void rgfw_windowRefreshCallback(GfxWindow* win, int x, int y, int w, int h)
{
	if (!(win.internal.enabledEvents & RGFW_WINDOW_REFRESH_FLAG)) return;
	GfxEvent event;
	event.type = RGFW_WINDOW_REFRESH;
	event.update.x = x; event.update.y = y; event.update.w = w; event.update.h = h;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void rgfw_windowFocusCallback(GfxWindow* win, bool inFocus)
{
	win.internal.inFocus = inFocus;
	if (win.internal.captureMouse) rgfw_window_captureMousePlatform(win, inFocus);
	GfxEvent event;
	event.common.win = win;
	event.focus.state = inFocus;
	if (inFocus == true)
	{
		if (win.internal.flags & RGFW_WINDOW_FULLSCREEN) rgfw_window_raise(win);
		event.type = RGFW_WINDOW_FOCUS_IN;
	}
	else
	{
		if (win.internal.flags & RGFW_WINDOW_FULLSCREEN) rgfw_window_minimize(win);
		for (usz key = 0; key < (usz)RGFW_KEY_LAST; key++)
		{
			if (rgfw_isKeyDown((RgfwKey)key) == false) continue;
			_rgfw.keyboard[key].current = false;
			if (win.internal.enabledEvents & RGFW_KEY_RELEASED_FLAG) rgfw_keyCallback(win, (RgfwKey)key, win.internal.mod, false, false);
		}
		rgfw_resetKey();
		event.type = RGFW_WINDOW_FOCUS_OUT;
	}
	event_queue_call_push(&event);
}

fn void rgfw_mouseNotifyCallback(GfxWindow* win, int x, int y, bool status)
{
	win.internal.mouseInside = status;
	_rgfw.windowState.win = win;
	win.internal.lastMouseX = x;
	win.internal.lastMouseY = y;
	GfxEvent event;
	event.common.win = win;
	event.mouse.x = x; event.mouse.y = y; event.mouse.inWindow = win.internal.mouseInside;
	if (status)
	{
		if (!(win.internal.enabledEvents & RGFW_MOUSE_ENTER_FLAG)) return;
		_rgfw.windowState.mouseEnter = true;
		event.type = RGFW_MOUSE_ENTER;
	}
	else
	{
		if (!(win.internal.enabledEvents & RGFW_MOUSE_LEAVE_FLAG)) return;
		_rgfw.windowState.winLeave = win;
		_rgfw.windowState.mouseLeave = true;
		event.type = RGFW_MOUSE_LEAVE;
	}
	event_queue_call_push(&event);
}

fn void rgfw_dataDragCallback(GfxWindow* win, RgfwDataTransferType dataType, RgfwDndActionType action, int x, int y)
{
	if (!(win.internal.enabledEvents & RGFW_DATA_DRAG_FLAG) || !(win.internal.flags & RGFW_WINDOW_ALLOW_DND)) return;
	_rgfw.windowState.win = win;
	_rgfw.windowState.dataDragging = true;
	_rgfw.windowState.dropX = x;
	_rgfw.windowState.dropY = y;
	GfxEvent event;
	event.type = RGFW_DATA_DRAG;
	event.drag.x = x; event.drag.y = y; event.drag.action = action; event.drag.dataType = dataType;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void rgfw_dataDropCallback(GfxWindow* win, char* data, usz length, RgfwDataTransferType dataType)
{
	if (!(win.internal.enabledEvents & RGFW_DATA_DROP_FLAG) || !(win.internal.flags & RGFW_WINDOW_ALLOW_DND)) return;
	_rgfw.windowState.win = win;
	_rgfw.windowState.dataDrop = true;
	_rgfw.windowState.dataLength = length;
	GfxDataDropNode node;
	rgfw_memzero(&node, GfxDataDropNode::size);
	node.data = data;
	node.length = length;
	node.type = dataType;
	GfxEvent event;
	event.type = RGFW_DATA_DROP;
	event.drop.value = &node;
	event.common.win = win;
	if (_rgfw.callbacks[(usz)event.type] != null) _rgfw.callbacks[(usz)event.type](&event);
	if (_rgfw.queueEvents == false && _rgfw.dndBuild == false) return;
	char* dataCopy = (char*)rgfw_alloc_impl(length);
	if (dataCopy != null) { rgfw_memcpy(dataCopy, data, length); node.data = dataCopy; }
	GfxDataDropNode* heapNode = (GfxDataDropNode*)rgfw_alloc_impl(GfxDataDropNode::size);
	if (heapNode == null) return;
	*heapNode = node;
	if (_rgfw.dndRoot == null) { _rgfw.dndRoot = heapNode; _rgfw.dndCur = heapNode; }
	else { _rgfw.dndCur.next = heapNode; _rgfw.dndCur = heapNode; }
	event.drop.value = heapNode;
	event_queue_push(&event);
}

fn void rgfw_keyCharCallback(GfxWindow* win, uint codepoint)
{
	if (!(win.internal.enabledEvents & RGFW_KEY_CHAR_FLAG)) return;
	GfxEvent event;
	event.type = RGFW_KEY_CHAR;
	event.keyChar.value = codepoint;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void rgfw_keyCallback(GfxWindow* win, RgfwKey key, RgfwKeymod mod, bool repeat, bool state)
{
	GfxEvent event;
	if (state)
	{
		if (!(win.internal.enabledEvents & RGFW_KEY_PRESSED_FLAG)) return;
		event.type = RGFW_KEY_PRESSED;
	}
	else
	{
		if (!(win.internal.enabledEvents & RGFW_KEY_RELEASED_FLAG)) return;
		event.type = RGFW_KEY_RELEASED;
	}
	_rgfw.keyboard[(usz)key].prev = _rgfw.keyboard[(usz)key].current;
	_rgfw.keyboard[(usz)key].current = state;
	event.key.value = key; event.key.repeat = repeat; event.key.mod = mod; event.key.state = state;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void rgfw_mouseButtonCallback(GfxWindow* win, RgfwMouseButton button, bool press)
{
	GfxEvent event;
	if (press)
	{
		if (!(win.internal.enabledEvents & RGFW_MOUSE_BUTTON_PRESSED_FLAG)) return;
		event.type = RGFW_MOUSE_BUTTON_PRESSED;
	}
	else
	{
		if (!(win.internal.enabledEvents & RGFW_MOUSE_BUTTON_RELEASED_FLAG)) return;
		event.type = RGFW_MOUSE_BUTTON_RELEASED;
	}
	_rgfw.mouseButtons[(usz)button].prev = _rgfw.mouseButtons[(usz)button].current;
	_rgfw.mouseButtons[(usz)button].current = press;
	event.button.value = button;
	event.button.state = press;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void rgfw_mouseScrollCallback(GfxWindow* win, float x, float y)
{
	if (!(win.internal.enabledEvents & RGFW_MOUSE_SCROLL_FLAG)) return;
	_rgfw.scrollX = x; _rgfw.scrollY = y;
	GfxEvent event;
	event.type = RGFW_MOUSE_SCROLL;
	event.delta.x = x; event.delta.y = y;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void rgfw_scaleUpdatedCallback(GfxWindow* win, float scaleX, float scaleY)
{
	if (!(win.internal.enabledEvents & RGFW_SCALE_UPDATED_FLAG)) return;
	GfxEvent event;
	event.type = RGFW_SCALE_UPDATED;
	event.scale.x = scaleX; event.scale.y = scaleY;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn void rgfw_monitorCallback(GfxWindow* win, GfxMonitor* monitor, bool connected)
{
	if (win != null)
	{
		if (connected && !(win.internal.enabledEvents & RGFW_MONITOR_CONNECTED_FLAG)) return;
		if (!connected && !(win.internal.enabledEvents & RGFW_MONITOR_DISCONNECTED_FLAG)) return;
	}
	GfxEvent event;
	event.type = connected ? RGFW_MONITOR_CONNECTED : RGFW_MONITOR_DISCONNECTED;
	event.monitor.monitor = monitor;
	event.monitor.state = connected;
	event.common.win = win;
	event_queue_call_push(&event);
}

fn int rgfw_init_ptr(char* className, RgfwInitFlags flags, RgfwInfo* info)
{
	if (info == _rgfw || info == null) return 1;
	if (className == null) className = "RGFW";
	rgfw_setInfo(info);
	rgfw_memzero(_rgfw, RgfwInfo::size);
	if (flags & RGFW_INIT_OPEN_GL && rgfw_loadGL() == false) return -1;
	if (flags & RGFW_INIT_EGL && rgfw_loadEGL() == false) return -1;
	if (flags & RGFW_INIT_VULKAN && rgfw_loadVulkan() == false) return -1;
	rgfw_initKeycodes();
	int out = rgfw_initPlatform(className, flags);
	if (out != 0)
	{
		rgfw_deinitPlatform();
		rgfw_memzero(_rgfw, RgfwInfo::size);
		rgfw_setInfo(null);
		return out;
	}
	for (usz i = 0; i < (usz)RGFW_MOUSE_ICON_COUNT; i++)
	{
		_rgfw.standardMice[i] = rgfw_createMouseStandard((RgfwMouseIcon)i);
	}
	rgfw_pollMonitors();
	return out;
}

fn void rgfw_deinit_ptr(RgfwInfo* info)
{
	if (info == null) return;
	rgfw_setInfo(info);
	for (RgfwMouseIcon i = 0; i < RGFW_MOUSE_ICON_COUNT; i++)
	{
		if (_rgfw.standardMice[(usz)i] != null) rgfw_freeMouse(_rgfw.standardMice[(usz)i]);
	}
	rgfw_freeMonitors();
	rgfw_deinitPlatform();
	rgfw_unloadEGL();
	rgfw_unloadGL();
	_rgfw.root = null;
	_rgfw.windowCount = 0;
	rgfw_setInfo(null);
}



fn GfxWindow* rgfw_createWindowPtr(char* name, int x, int y, int w, int h, RgfwWindowFlags flags, GfxWindow* win)
{
	if (win == null || _rgfw == null) return null;
	if (name == null) name = "";
	rgfw_memzero(win, GfxWindow::size);
	_rgfw.windowCount++;
	if (_rgfw.root == null) rgfw_setRootWindow(win);
	win.x = x; win.y = y; win.w = w; win.h = h;
	win.internal.mouse = _rgfw.standardMice[(usz)RGFW_MOUSE_NORMAL];
	win.internal.flags = flags;
	win.internal.enabledEvents = RGFW_ALL_EVENT_FLAGS;
	RgfwWindowFlags reservedFlags = flags & RGFW_WINDOW_SCALE_TO_MONITOR;
	GfxWindow* ret = rgfw_createWindowPlatform(name, flags, win);
	if (ret == null) return null;
	flags |= reservedFlags;
	rgfw_window_setFlagsInternal(win, flags, 0);
	win.src.gfxType = 0;
	if (flags & RGFW_WINDOW_OPEN_GL) rgfw_window_createContext_OpenGL(win, rgfw_getGlobalHints_OpenGL());
	if (!(flags & RGFW_WINDOW_OPEN_GL)) win.src.gfxType = 0;
	if (flags & RGFW_WINDOW_EGL) rgfw_window_createContext_EGL(win, rgfw_getGlobalHints_OpenGL());
	if (!(flags & RGFW_WINDOW_HIDE_MOUSE)) rgfw_window_setMouseDefault(win);
	rgfw_window_setName(win, name);
	if (!(flags & RGFW_WINDOW_HIDE))
	{
		flags |= RGFW_WINDOW_HIDE;
		rgfw_window_show(win);
	}
	else if ((flags & RGFW_WINDOW_MAXIMIZE) || (flags & RGFW_WINDOW_FULLSCREEN))
	{
		rgfw_window_hide(win);
	}
	return ret;
}



fn void rgfw_window_closePtr(GfxWindow* win)
{
	if (win == null) return;
	if (win.internal.captureMouse) rgfw_window_captureMouse(win, false);
	if ((win.src.gfxType & RGFW_GFX_EGL) && win.src.ctx.egl != null)
	{
		rgfw_window_deleteContext_EGL(win, win.src.ctx.egl);
		win.src.ctx.egl = null;
	}
	if ((win.src.gfxType & RGFW_GFX_NATIVE_OPEN_GL) && win.src.ctx.native != null)
	{
		rgfw_window_deleteContext_OpenGL(win, win.src.ctx.native);
		win.src.ctx.native = null;
	}
	rgfw_window_closePlatform(win);
	if (_rgfw.clipboard != null) rgfw_free_impl(_rgfw.clipboard);
	_rgfw.clipboard = null;
	_rgfw.windowCount--;
}

fn void rgfw_setQueueEvents(bool queue) { _rgfw.queueEvents = rgfw_bool(queue); }

fn void rgfw_resetPrevState()
{
	for (usz i = 0; i < (usz)RGFW_KEY_LAST; i++) _rgfw.keyboard[i].prev = _rgfw.keyboard[i].current;
	for (usz i = 0; i < (usz)RGFW_MOUSE_FINAL; i++) _rgfw.mouseButtons[i].prev = _rgfw.mouseButtons[i].current;
	_rgfw.scrollX = 0.0f; _rgfw.scrollY = 0.0f;
	_rgfw.vectorX = 0.0f; _rgfw.vectorY = 0.0f;
	rgfw_memzero(&_rgfw.windowState, RgfwWindowState::size);
	GfxDataDropNode* node = _rgfw.dndRoot;
	while (node != null)
	{
		GfxDataDropNode* next = node.next;
		rgfw_free_impl(node);
		node = next;
	}
	_rgfw.dndRoot = null;
	_rgfw.dndCur = null;
}

fn bool rgfw_isKeyPressed(RgfwKey key) => rgfw_bool(_rgfw.keyboard[(usz)key].current && !_rgfw.keyboard[(usz)key].prev);
fn bool rgfw_isKeyDown(RgfwKey key) => _rgfw.keyboard[(usz)key].current;
fn bool rgfw_isKeyReleased(RgfwKey key) => rgfw_bool(!_rgfw.keyboard[(usz)key].current && _rgfw.keyboard[(usz)key].prev);
fn bool rgfw_isMouseDown(RgfwMouseButton button) => _rgfw.mouseButtons[(usz)button].current;
fn bool rgfw_isMouseReleased(RgfwMouseButton button) => rgfw_bool(!_rgfw.mouseButtons[(usz)button].current && _rgfw.mouseButtons[(usz)button].prev);

fn void rgfw_getMouseScroll(float* x, float* y) { if (x != null) *x = _rgfw.scrollX; if (y != null) *y = _rgfw.scrollY; }
fn void rgfw_getMouseVector(float* x, float* y) { if (x != null) *x = _rgfw.vectorX; if (y != null) *y = _rgfw.vectorY; }

fn RgfwWindowSrc* rgfw_window_getSrc(GfxWindow* win) => &win.src;
fn bool rgfw_window_getPosition(GfxWindow* win, int* x, int* y) { if (x != null) *x = win.x; if (y != null) *y = win.y; return true; }
fn bool rgfw_window_getSize(GfxWindow* win, int* w, int* h) { if (w != null) *w = win.w; if (h != null) *h = win.h; return true; }
fn uint rgfw_window_getFlags(GfxWindow* win) => win.internal.flags;
fn RgfwKey rgfw_window_getExitKey(GfxWindow* win) => win.internal.exitKey;
fn void rgfw_window_setExitKey(GfxWindow* win, RgfwKey key) { win.internal.exitKey = key; }
fn void rgfw_window_setEnabledEvents(GfxWindow* win, RgfwEventFlag events) { win.internal.enabledEvents = events; }
fn RgfwEventFlag rgfw_window_getEnabledEvents(GfxWindow* win) => win.internal.enabledEvents;
fn void rgfw_window_setDisabledEvents(GfxWindow* win, RgfwEventFlag events) { rgfw_window_setEnabledEvents(win, RGFW_ALL_EVENT_FLAGS & ~events); }
fn void rgfw_window_setEventState(GfxWindow* win, RgfwEventFlag event, bool state) { rgfw_setBit(&win.internal.enabledEvents, event, state); }
fn void* rgfw_window_getUserPtr(GfxWindow* win) => win.userPtr;
fn void rgfw_window_setUserPtr(GfxWindow* win, void* ptr) { win.userPtr = ptr; }

fn bool rgfw_window_getSizeInPixels(GfxWindow* win, int* w, int* h)
{
	GfxMonitor* mon = rgfw_window_getMonitor(win);
	if (mon == null) return false;
	if (w != null) *w = (int)((float)win.w * mon.pixelRatio);
	if (h != null) *h = (int)((float)win.h * mon.pixelRatio);
	return true;
}

fn bool rgfw_window_didMouseLeave(GfxWindow* win) => rgfw_bool(_rgfw.windowState.winLeave == win && _rgfw.windowState.mouseLeave);
fn bool rgfw_window_didMouseEnter(GfxWindow* win) => rgfw_bool(_rgfw.windowState.win == win && _rgfw.windowState.mouseEnter);
fn bool rgfw_window_isMouseInside(GfxWindow* win) => win.internal.mouseInside;
fn bool rgfw_window_isDataDragging(GfxWindow* win) => rgfw_window_getDataDrag(win, null, null);
fn bool rgfw_window_didDataDrop(GfxWindow* win) => rgfw_bool(rgfw_window_getDataDrop(win) != null);

fn bool rgfw_window_getDataDrag(GfxWindow* win, int* x, int* y)
{
	if (_rgfw.windowState.win != win || _rgfw.windowState.dataDragging == false) return false;
	if (x != null) *x = _rgfw.windowState.dropX;
	if (y != null) *y = _rgfw.windowState.dropY;
	return true;
}

fn GfxDataDropNode* rgfw_window_getDataDrop(GfxWindow* win)
{
	if (_rgfw.windowState.win != win || _rgfw.windowState.dataDrop == false) return null;
	return _rgfw.dndRoot;
}

fn void rgfw_setRootWindow(GfxWindow* win) { _rgfw.root = win; }
fn GfxWindow* rgfw_getRootWindow() { return _rgfw.root; }

fn void rgfw_setBit(uint* value, uint mask, bool set)
{
	if (set) { *value |= mask; }
	else { *value &= ~mask; }
}

fn void rgfw_window_setFlagsInternal(GfxWindow* win, RgfwWindowFlags flags, RgfwWindowFlags cmpFlags)
{
	if (flags & RGFW_WINDOW_NO_BORDER) { rgfw_window_setBorder(win, false); }
	else if (cmpFlags & RGFW_WINDOW_NO_BORDER) { rgfw_window_setBorder(win, true); }
	if (flags & RGFW_WINDOW_MAXIMIZE) { rgfw_window_maximize(win); }
	else if (cmpFlags & RGFW_WINDOW_MAXIMIZE) { rgfw_window_restore(win); }
	if (flags & RGFW_WINDOW_MINIMIZE) { rgfw_window_minimize(win); }
	else if (cmpFlags & RGFW_WINDOW_MINIMIZE) { rgfw_window_restore(win); }
	if (flags & RGFW_WINDOW_CENTER) rgfw_window_center(win);
	if (flags & RGFW_WINDOW_CENTER_CURSOR) rgfw_window_moveMouse(win, win.x + (win.w / 2), win.y + (win.h / 2));
	if (flags & RGFW_WINDOW_FULLSCREEN) { rgfw_window_setFullscreen(win, true); }
	else if (cmpFlags & RGFW_WINDOW_FULLSCREEN) { rgfw_window_setFullscreen(win, false); }
	if (flags & RGFW_WINDOW_HIDE_MOUSE) { rgfw_window_showMouse(win, false); }
	else if (cmpFlags & RGFW_WINDOW_HIDE_MOUSE) { rgfw_window_showMouse(win, true); }
	if (flags & RGFW_WINDOW_HIDE) { rgfw_window_hide(win); }
	else if (cmpFlags & RGFW_WINDOW_HIDE) { rgfw_window_show(win); }
	if (flags & RGFW_WINDOW_FLOATING) { rgfw_window_setFloating(win, true); }
	else if (cmpFlags & RGFW_WINDOW_FLOATING) { rgfw_window_setFloating(win, false); }
	if (flags & RGFW_WINDOW_RAW_MOUSE) { rgfw_window_setRawMouseMode(win, true); }
	else if (cmpFlags & RGFW_WINDOW_RAW_MOUSE) { rgfw_window_setRawMouseMode(win, false); }
	if (flags & RGFW_WINDOW_CAPTURE_MOUSE) { rgfw_window_captureRawMouse(win, true); }
	else if (cmpFlags & RGFW_WINDOW_CAPTURE_MOUSE) { rgfw_window_captureMouse(win, false); }
	if (flags & RGFW_WINDOW_FOCUS) rgfw_window_focus(win);
	if (flags & RGFW_WINDOW_SCALE_TO_MONITOR) rgfw_window_scaleToMonitor(win);
	if (flags & RGFW_WINDOW_NO_RESIZE)
	{
		rgfw_window_setMaxSize(win, win.w, win.h);
		rgfw_window_setMinSize(win, win.w, win.h);
	}
	else if (cmpFlags & RGFW_WINDOW_NO_RESIZE)
	{
		rgfw_window_setMaxSize(win, 0, 0);
		rgfw_window_setMinSize(win, 0, 0);
	}
	win.internal.flags = flags;
}

fn void rgfw_window_setFlags(GfxWindow* win, RgfwWindowFlags flags) { rgfw_window_setFlagsInternal(win, flags, win.internal.flags); }
fn bool rgfw_window_isInFocus(GfxWindow* win) => rgfw_bool(win.internal.inFocus);
fn void rgfw_setBuildDND(bool state) { _rgfw.dndBuild = state; }
fn bool rgfw_window_getMouse(GfxWindow* win, int* x, int* y) { if (x != null) *x = win.internal.lastMouseX; if (y != null) *y = win.internal.lastMouseY; return true; }
fn bool rgfw_window_isKeyPressed(GfxWindow* win, RgfwKey key) => rgfw_bool(rgfw_isKeyPressed(key) && rgfw_window_isInFocus(win));
fn bool rgfw_window_isKeyDown(GfxWindow* win, RgfwKey key) => rgfw_bool(rgfw_isKeyDown(key) && rgfw_window_isInFocus(win));
fn bool rgfw_window_isKeyReleased(GfxWindow* win, RgfwKey key) => rgfw_bool(rgfw_isKeyReleased(key) && rgfw_window_isInFocus(win));
fn bool rgfw_window_isMousePressed(GfxWindow* win, RgfwMouseButton button) => rgfw_bool(is_mouse_pressed(button) && rgfw_window_isInFocus(win));
fn bool rgfw_window_isMouseDown(GfxWindow* win, RgfwMouseButton button) => rgfw_bool(rgfw_isMouseDown(button) && rgfw_window_isInFocus(win));
fn bool rgfw_window_isMouseReleased(GfxWindow* win, RgfwMouseButton button) => rgfw_bool(rgfw_isMouseReleased(button) && rgfw_window_isInFocus(win));

fn void rgfw_window_center(GfxWindow* win)
{
	GfxMonitor* mon = rgfw_window_getMonitor(win);
	if (mon == null) return;
	rgfw_window_move(win, mon.x + ((mon.mode.w - win.w) / 2), mon.y + ((mon.mode.h - win.h) / 2));
}

fn bool rgfw_monitor_scaleToWindow(GfxMonitor* mon, GfxWindow* win)
{
	RgfwMonitorMode mode;
	mode.w = win.w;
	mode.h = win.h;
	bool ret = rgfw_monitor_requestMode(mon, &mode, RGFW_MONITOR_SCALE);
	rgfw_window_move(win, mon.x, mon.y);
	return ret;
}

fn void rgfw_splitBPP(uint bpp, RgfwMonitorMode* mode)
{
	if (bpp == 32) bpp = 24;
	mode.red = mode.green = mode.blue = (char)(bpp / 3);
	uint delta = bpp - (mode.red * 3);
	if (delta >= 1) mode.green = mode.green + 1;
	if (delta == 2) mode.red = mode.red + 1;
}

fn bool rgfw_monitorModeCompare(RgfwMonitorMode* mon, RgfwMonitorMode* mon2, RgfwModeRequest request)
{
	return rgfw_bool(((mon.w == mon2.w && mon.h == mon2.h) || !(request & RGFW_MONITOR_SCALE)) &&
		((mon.refreshRate == mon2.refreshRate) || !(request & RGFW_MONITOR_REFRESH)) &&
		((mon.red == mon2.red && mon.green == mon2.green && mon.blue == mon2.blue) || !(request & RGFW_MONITOR_RGB)));
}

fn bool rgfw_window_shouldClose(GfxWindow* win) => rgfw_bool(win == null || win.internal.shouldClose || (win.internal.exitKey && rgfw_isKeyDown(win.internal.exitKey)));
fn void rgfw_window_setShouldClose(GfxWindow* win, bool shouldClose) { if (shouldClose) { rgfw_windowCloseCallback(win); } else { win.internal.shouldClose = false; } }
fn void rgfw_window_scaleToMonitor(GfxWindow* win) { GfxMonitor* monitor = rgfw_window_getMonitor(win); if (monitor == null) return; rgfw_window_resize(win, (int)(monitor.scaleX * (float)win.w), (int)(monitor.scaleY * (float)win.h)); }
fn void rgfw_window_moveToMonitor(GfxWindow* win, GfxMonitor* m) { rgfw_window_move(win, m.x + win.x, m.y + win.y); }

RgfwColorLayout[6] rgfw_layouts = {
	{0, 1, 2, 3, 3},
	{2, 1, 0, 3, 3},
	{0, 1, 2, 3, 4},
	{1, 2, 3, 0, 4},
	{2, 1, 0, 3, 4},
	{3, 2, 1, 0, 4},
};


fn void rgfw_surface_setConvertFunc(GfxSurface* surface, RgfwConvertImageDataFunc func) { surface.convertFunc = func; }
fn RgfwNativeImage* rgfw_surface_getNativeImage(GfxSurface* surface) => &surface.native;
fn void rgfw_surface_free(GfxSurface* surface) { rgfw_surface_freePtr(surface); rgfw_free_impl(surface); }

fn GfxSurface* rgfw_window_createSurface(GfxWindow* win, char* data, int w, int h, RgfwFormat format)
{
	GfxSurface* surface = (GfxSurface*)rgfw_alloc_impl(GfxSurface::size);
	if (surface == null) return null;
	rgfw_memzero(surface, GfxSurface::size);
	rgfw_window_createSurfacePtr(win, data, w, h, format, surface);
	return surface;
}

fn bool rgfw_window_createSurfacePtr(GfxWindow* win, char* data, int w, int h, RgfwFormat format, GfxSurface* surface)
{
	return rgfw_createSurfacePtr(data, w, h, format, surface);
}

fn RgfwMonitorNode* rgfw_monitors_add(GfxMonitor* mon)
{
	RgfwMonitorNode* node = (RgfwMonitorNode*)rgfw_alloc_impl(RgfwMonitorNode::size);
	if (node == null) return null;
	rgfw_memzero(node, RgfwMonitorNode::size);
	if (_rgfw.monitors.list.head == null) { _rgfw.monitors.list.head = node; }
	else { _rgfw.monitors.list.cur.next = node; }
	_rgfw.monitors.list.cur = node;
	if (mon != null) node.mon = *mon;
	node.mon.node = node;
	node.disconnected = false;
	_rgfw.monitors.count += 1;
	return node;
}

fn void rgfw_monitors_remove(RgfwMonitorNode* node, RgfwMonitorNode* prev)
{
	_rgfw.monitors.count -= 1;
	rgfw_monitorNode_free(node);
	if (prev != node) { prev.next = node.next; }
	else { _rgfw.monitors.list.head = node.next; }
	node.next = null;
	rgfw_free_impl(node);
}

fn void rgfw_monitors_refresh()
{
	RgfwMonitorNode* prev = _rgfw.monitors.list.head;
	RgfwMonitorNode* node = _rgfw.monitors.list.head;
	while (node != null)
	{
		RgfwMonitorNode* next = node.next;
		if (node.disconnected != false)
		{
			rgfw_monitorCallback(_rgfw.root, &node.mon, false);
			rgfw_monitors_remove(node, prev);
		}
		prev = node;
		node = next;
	}
}

fn void rgfw_freeMonitors()
{
	RgfwMonitorNode* node = _rgfw.monitors.list.head;
	while (node != null)
	{
		RgfwMonitorNode* next = node.next;
		rgfw_monitors_remove(node, node);
		node = next;
	}
}

fn GfxMonitor** rgfw_getMonitors(usz* len)
{
	if (len != null) *len = 0;
	usz count = 0;
	if (rgfw_getMonitorsPtr(0, null, &count) == false || count == 0) return null;
	GfxMonitor** monitors = (GfxMonitor**)rgfw_alloc_impl(GfxMonitor*::size * count);
	if (rgfw_getMonitorsPtr(count, monitors, &count) == false)
	{
		rgfw_free_impl(monitors);
		return null;
	}
	if (len != null) *len = count;
	return monitors;
}

fn bool rgfw_getMonitorsPtr(usz max, GfxMonitor** monitors, usz* len)
{
	if (len != null) *len = _rgfw.monitors.count;
	if (monitors == null || max == 0) return true;
	if (len != null) *len = max;
	usz i = 0;
	RgfwMonitorNode* cur_node = _rgfw.monitors.list.head;
	while (cur_node != null && i < max)
	{
		monitors[i] = &cur_node.mon;
		i++;
		cur_node = cur_node.next;
	}
	return true;
}

fn GfxMonitor* rgfw_getPrimaryMonitor()
{
	if (_rgfw.monitors.primary == null) _rgfw.monitors.primary = _rgfw.monitors.list.head;
	if (_rgfw.monitors.primary == null) return null;
	return &_rgfw.monitors.primary.mon;
}

fn RgfwMonitorMode* rgfw_monitor_getModes(GfxMonitor* monitor, usz* count)
{
	usz num = rgfw_monitor_getModesPtr(monitor, null);
	RgfwMonitorMode* modes = (RgfwMonitorMode*)rgfw_alloc_impl(num * RgfwMonitorMode::size);
	num = rgfw_monitor_getModesPtr(monitor, &modes);
	if (count != null) *count = num;
	return modes;
}

fn void rgfw_freeModes(RgfwMonitorMode* modes) { rgfw_free_impl(modes); }

fn bool rgfw_monitor_findClosestMode(GfxMonitor* monitor, RgfwMonitorMode* mode, RgfwMonitorMode* closest)
{
	usz count = rgfw_monitor_getModesPtr(monitor, null);
	RgfwMonitorMode* modes = (RgfwMonitorMode*)rgfw_alloc_impl(count * RgfwMonitorMode::size);
	count = rgfw_monitor_getModesPtr(monitor, &modes);
	RgfwMonitorMode* chosen = null;
	uint topScore = 1;
	for (usz i = 0; i < count; i++)
	{
		RgfwMonitorMode* mode2 = &modes[i];
		uint score = 0;
		if (mode.w == mode2.w && mode.h == mode2.h) score += 1000;
		if (mode.red == mode2.red && mode.green == mode2.green && mode.blue == mode2.blue) score += 100;
		if (mode.refreshRate == mode.refreshRate) score += 10;
		if (score > topScore) { topScore = score; chosen = mode2; }
	}
	if (chosen != null && closest != null) *closest = *chosen;
	rgfw_free_impl(modes);
	return chosen == null ? false : true;
}

fn bool rgfw_monitor_getPosition(GfxMonitor* monitor, int* x, int* y) { if (x != null) *x = monitor.x; if (y != null) *y = monitor.y; return true; }
fn char* rgfw_monitor_getName(GfxMonitor* monitor) => &monitor.name[0];
fn bool rgfw_monitor_getScale(GfxMonitor* monitor, float* x, float* y) { if (x != null) *x = monitor.scaleX; if (y != null) *y = monitor.scaleY; return true; }
fn bool rgfw_monitor_getPhysicalSize(GfxMonitor* monitor, float* w, float* h) { if (w != null) *w = monitor.physW; if (h != null) *h = monitor.physH; return true; }
fn void rgfw_monitor_setUserPtr(GfxMonitor* monitor, void* userPtr) { monitor.userPtr = userPtr; }
fn void* rgfw_monitor_getUserPtr(GfxMonitor* monitor) => monitor.userPtr;
fn bool rgfw_monitor_getMode(GfxMonitor* monitor, RgfwMonitorMode* mode) { if (mode != null) *mode = monitor.mode; return true; }

fn RgfwGammaRamp* rgfw_monitor_getGammaRamp(GfxMonitor* monitor)
{
	RgfwGammaRamp* ramp = (RgfwGammaRamp*)rgfw_alloc_impl(RgfwGammaRamp::size);
	ramp.count = rgfw_monitor_getGammaRampPtr(monitor, null);
	ramp.red = (ushort*)rgfw_alloc_impl(ushort::size * ramp.count);
	ramp.green = (ushort*)rgfw_alloc_impl(ushort::size * ramp.count);
	ramp.blue = (ushort*)rgfw_alloc_impl(ushort::size * ramp.count);
	ramp.count = rgfw_monitor_getGammaRampPtr(monitor, ramp);
	return ramp;
}

fn void rgfw_freeGammaRamp(RgfwGammaRamp* ramp)
{
	rgfw_free_impl(ramp.red);
	rgfw_free_impl(ramp.green);
	rgfw_free_impl(ramp.blue);
	rgfw_free_impl(ramp);
}

fn bool rgfw_monitor_setGammaPtr(GfxMonitor* monitor, float gamma, ushort* ptr, usz count)
{
	for (usz i = 0; i < count; i++)
	{
		float value = (float)i / (float)(count - 1);
		value = powf(value, 1.0f / gamma) * 65535.0f + 0.5f;
		value = rgfw_min(value, 65535.0f);
		ptr[i] = (ushort)value;
	}
	RgfwGammaRamp ramp;
	ramp.red = ptr; ramp.green = ptr; ramp.blue = ptr; ramp.count = count;
	return rgfw_monitor_setGammaRamp(monitor, &ramp);
}

fn bool rgfw_monitor_setGamma(GfxMonitor* monitor, float gamma)
{
	usz count = rgfw_monitor_getGammaRampPtr(monitor, null);
	ushort* ptr = (ushort*)rgfw_alloc_impl(count * ushort::size);
	bool ret = rgfw_monitor_setGammaPtr(monitor, gamma, ptr, count);
	rgfw_free_impl(ptr);
	return ret;
}


fn void rgfw_window_captureMouse(GfxWindow* win, bool state) { win.internal.captureMouse = state; rgfw_window_captureMousePlatform(win, state); }
fn void rgfw_window_setRawMouseMode(GfxWindow* win, bool state) { win.internal.rawMouse = state; rgfw_window_setRawMouseModePlatform(win, state); }
fn void rgfw_setRawMouseMode(bool state) { _rgfw.rawMouse = state; if (_rgfw.root != null) rgfw_window_setRawMouseModePlatform(_rgfw.root, state); }
fn void rgfw_window_captureRawMouse(GfxWindow* win, bool state) { rgfw_window_captureMouse(win, state); rgfw_window_setRawMouseMode(win, state); }
fn bool rgfw_window_isRawMouseMode(GfxWindow* win) => rgfw_bool(win.internal.rawMouse);
fn bool rgfw_window_isCaptured(GfxWindow* win) => rgfw_bool(win.internal.captureMouse);

fn void rgfw_keyUpdateKeyMod(GfxWindow* win, RgfwKeymod mod, bool value) { if (value) { win.internal.mod |= mod; } else { win.internal.mod &= ~mod; } }
fn void rgfw_keyUpdateKeyModsEx(GfxWindow* win, bool capital, bool numlock, bool control, bool alt, bool shift, bool super, bool scroll)
{
	rgfw_keyUpdateKeyMod(win, RGFW_MOD_CAPS_LOCK, capital);
	rgfw_keyUpdateKeyMod(win, RGFW_MOD_NUM_LOCK, numlock);
	rgfw_keyUpdateKeyMod(win, RGFW_MOD_CONTROL, control);
	rgfw_keyUpdateKeyMod(win, RGFW_MOD_ALT, alt);
	rgfw_keyUpdateKeyMod(win, RGFW_MOD_SHIFT, shift);
	rgfw_keyUpdateKeyMod(win, RGFW_MOD_SUPER, super);
	rgfw_keyUpdateKeyMod(win, RGFW_MOD_SCROLL_LOCK, scroll);
}

fn void rgfw_keyUpdateKeyMods(GfxWindow* win, bool capital, bool numlock, bool scroll)
{
	rgfw_keyUpdateKeyModsEx(win, capital, numlock,
		rgfw_bool(rgfw_isKeyDown(RGFW_KEY_CONTROL_L) || rgfw_isKeyDown(RGFW_KEY_CONTROL_R)),
		rgfw_bool(rgfw_isKeyDown(RGFW_KEY_ALT_L) || rgfw_isKeyDown(RGFW_KEY_ALT_R)),
		rgfw_bool(rgfw_isKeyDown(RGFW_KEY_SHIFT_L) || rgfw_isKeyDown(RGFW_KEY_SHIFT_R)),
		rgfw_bool(rgfw_isKeyDown(RGFW_KEY_SUPER_L) || rgfw_isKeyDown(RGFW_KEY_SUPER_R)),
		scroll);
}

fn void rgfw_window_showMouseFlags(GfxWindow* win, bool show)
{
	if (show && (win.internal.flags & RGFW_WINDOW_HIDE_MOUSE)) { win.internal.flags ^= RGFW_WINDOW_HIDE_MOUSE; }
	else if (!show && !(win.internal.flags & RGFW_WINDOW_HIDE_MOUSE)) { win.internal.flags |= RGFW_WINDOW_HIDE_MOUSE; }
}

fn bool rgfw_window_isMouseHidden(GfxWindow* win) => rgfw_bool(win.internal.flags & RGFW_WINDOW_HIDE_MOUSE);
fn bool rgfw_window_borderless(GfxWindow* win) => rgfw_bool(win.internal.flags & RGFW_WINDOW_NO_BORDER);
fn bool rgfw_window_isFullscreen(GfxWindow* win) => rgfw_bool(win.internal.flags & RGFW_WINDOW_FULLSCREEN);
fn bool rgfw_window_allowsDND(GfxWindow* win) => rgfw_bool(win.internal.flags & RGFW_WINDOW_ALLOW_DND);
fn bool rgfw_window_setMouseDefault(GfxWindow* win) => rgfw_window_setMouseStandard(win, RGFW_MOUSE_NORMAL);
fn bool rgfw_window_setMouseStandard(GfxWindow* win, RgfwMouseIcon icon) => rgfw_window_setMouse(win, _rgfw.standardMice[(usz)icon]);
fn bool rgfw_window_setMouse(GfxWindow* win, RgfwMouse* mouse) { if (mouse != _rgfw.hiddenMouse) win.internal.mouse = mouse; return rgfw_window_setMousePlatform(win, mouse); }
fn void rgfw_window_setDND(GfxWindow* win, bool allow) { rgfw_setBit(&win.internal.flags, RGFW_WINDOW_ALLOW_DND, allow); }
fn void rgfw_window_showMouse(GfxWindow* win, bool show) { rgfw_window_showMouseFlags(win, show); if (show == false) { rgfw_window_setMouse(win, _rgfw.hiddenMouse); } else { rgfw_window_setMouse(win, win.internal.mouse); } }

fn bool rgfw_isLatin(char* string, usz length)
{
	for (usz i = 0; i < length; i++) if ((char)string[i] >= 0x80) return true;
	return false;
}

fn uint rgfw_decodeUTF8(char* string, usz* starting_index)
{
	uint[6] offsets = {0x00000000u, 0x00003080u, 0x000e2080u, 0x03c82080u, 0xfa082080u, 0x82082080u};
	uint codepoint = (char)string[*starting_index];
	usz count = 1;
	while ((string[count + *starting_index] & 0xc0) == 0x80)
	{
		codepoint = (codepoint << 6) + (char)string[count + *starting_index];
		count++;
	}
	*starting_index += count;
	return codepoint - offsets[count - 1];
}

fn void rgfw_window_show(GfxWindow* win) { if (win.internal.flags & RGFW_WINDOW_FOCUS_ON_SHOW) rgfw_window_focus(win); win.internal.flags &= ~((uint)RGFW_WINDOW_HIDE); rgfw_window_showPlatform(win); if (win.internal.flags & RGFW_WINDOW_MAXIMIZE) rgfw_window_maximize(win); }
fn void rgfw_window_restore(GfxWindow* win) { rgfw_window_restorePlatform(win); win.internal.flags &= ~((uint)RGFW_WINDOW_MAXIMIZE); rgfw_window_show(win); rgfw_window_move(win, win.internal.oldX, win.internal.oldY); rgfw_window_resize(win, win.internal.oldW, win.internal.oldH); }
fn void rgfw_window_minimize(GfxWindow* win) { rgfw_window_minimizePlatform(win); win.internal.flags &= ~((uint)RGFW_WINDOW_MAXIMIZE); }
fn void rgfw_window_maximize(GfxWindow* win) { win.internal.oldX = win.x; win.internal.oldY = win.y; win.internal.oldW = win.w; win.internal.oldH = win.h; win.internal.flags |= RGFW_WINDOW_MAXIMIZE; rgfw_window_fetchSize(win, null, null); rgfw_window_maximizePlatform(win); }
fn void rgfw_window_move(GfxWindow* win, int x, int y) { win.x = x; win.y = y; rgfw_window_movePlatform(win, x, y); win.internal.flags &= ~((uint)RGFW_WINDOW_MAXIMIZE); }
fn void rgfw_window_resize(GfxWindow* win, int w, int h) { win.w = w; win.h = h; rgfw_window_resizePlatform(win, w, h); win.internal.flags &= ~((uint)RGFW_WINDOW_MAXIMIZE); }
fn void rgfw_window_setAspectRatio(GfxWindow* win, int w, int h)
{
	if (win == null || win.src.window == null) return;
	if (w == 0 && h == 0) { w = 1; h = 1; }
	CGSize size;
	size.width = (CGFloat)w;
	size.height = (CGFloat)h;
	objc_send((ObjcId)win.src.window, ObjcSendVoidSize, "setContentAspectRatio:", size);
}

fn RgfwFormat rgfw_nativeFormat() => RGFW_FORMAT_BGRA8;

fn void rgfw_window_blitSurface(GfxWindow* win, GfxSurface* surface)
{
	if (win == null || surface == null || surface.data == null) return;
	int minW = win.w < surface.w ? win.w : surface.w;
	int minH = win.h < surface.h ? win.h : surface.h;
	GfxMonitor* mon = rgfw_window_getMonitor(win);
	if (mon != null)
	{
		minW = (int)((float)minW * mon.pixelRatio);
		minH = (int)((float)minH * mon.pixelRatio);
	}
	ObjcId rep = ns_bitmapRep(minW, minH);
	char* pixels = ns_bitmapData(rep);
	if (pixels == null) { ns_release(rep); return; }
	rgfw_copyImageData(pixels, minW, minH, RGFW_FORMAT_RGBA8, surface.data, surface.format, surface.convertFunc);
	ObjcId image = ns_imageWithRep(rep, minW, minH);
	objc_send((ObjcId)win.src.view, ObjcSendVoidBool, "setWantsLayer:", true);
	ObjcId layer = objc_send((ObjcId)win.src.view, ObjcSendId, "layer");
	if (layer != null) objc_send(layer, ObjcSendVoidId, "setContents:", image);
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

fn RgfwKey rgfw_physicalToMappedKey(RgfwKey key)
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

	if (status == 0 && len == 1 && chars[0] < 256) return (RgfwKey)chars[0];

	return key;
}

RgfwGlHints rgfw_global_hints_open_gl_src = {
	0, 0, 0, 0, 1, 8, 8, 8, 8, 24, 0, 0, 0, 0,
	false, false, false, false, RGFW_GL_RELEASE_NONE,
	RGFW_GL_CORE, 1, 0, null, null, RGFW_GL_ACCELERATED
};
RgfwGlHints* rgfw_global_hints_open_gl = &rgfw_global_hints_open_gl_src;

fn void rgfw_resetGlobalHints_OpenGL() { rgfw_global_hints_open_gl_src = {0, 0, 0, 0, 1, 8, 8, 8, 8, 24, 0, 0, 0, 0, false, false, false, false, RGFW_GL_RELEASE_NONE, RGFW_GL_CORE, 1, 0, null, null, RGFW_GL_ACCELERATED}; }
fn void rgfw_setGlobalHints_OpenGL(RgfwGlHints* hints) { rgfw_global_hints_open_gl = hints; }
fn RgfwGlHints* rgfw_getGlobalHints_OpenGL() { return rgfw_global_hints_open_gl; }
fn void* rgfw_glContext_getSourceContext(RgfwGlContext* ctx) { return ctx.ctx; }
fn void rgfw_attribStack_init(RgfwAttribStack* stack, int* attribs, usz max) { stack.attribs = attribs; stack.count = 0; stack.max = max; }
fn void rgfw_attribStack_pushAttrib(RgfwAttribStack* stack, int attrib) { stack.attribs[stack.count] = attrib; stack.count += 1; }
fn void rgfw_attribStack_pushAttribs(RgfwAttribStack* stack, int attrib1, int attrib2) { rgfw_attribStack_pushAttrib(stack, attrib1); rgfw_attribStack_pushAttrib(stack, attrib2); }

struct RgfwAttribStack
{
	int* attribs;
	usz count;
	usz max;
}

fn bool rgfw_extensionSupportedStr(char* extensions, char* ext, usz len)
{
	if (extensions == null || ext == null) return false;
	while (ext[len - 1] == 0 && len > 3) len--;
	char* start = extensions;
	char* where = strstr(extensions, ext);
	while (where != null)
	{
		char* terminator = where + len;
		if ((where == start || *(where - 1) == ' ') && (*terminator == ' ' || *terminator == 0)) return true;
		where = strstr(terminator, ext);
	}
	return false;
}

alias RgfwGetProcAddressFunc = fn RgfwProc(char* procname);

fn bool rgfw_extensionSupported_base(char* extension, usz len, RgfwGetProcAddressFunc getProcAddress)
{
	RgfwProc rgfw_gl_get_string = getProcAddress("glGetString");
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

fn RgfwGlContext* rgfw_window_createContext_OpenGL(GfxWindow* win, RgfwGlHints* hints)
{
	RgfwGlContext* ctx = (RgfwGlContext*)rgfw_alloc_impl(RgfwGlContext::size);
	if (ctx == null) return null;
	rgfw_memzero(ctx, RgfwGlContext::size);
	if (rgfw_window_createContextPtr_OpenGL(win, ctx, hints) == false)
	{
		rgfw_free_impl(ctx);
		if (win != null) win.src.ctx.native = null;
		return null;
	}
	win.src.gfxType |= RGFW_GFX_OWNED_BY_RGFW;
	return ctx;
}

fn RgfwGlContext* rgfw_window_getContext_OpenGL(GfxWindow* win)
{
	if (win == null || (win.src.gfxType & RGFW_GFX_EGL)) return null;
	return win.src.ctx.native;
}

fn void rgfw_window_deleteContext_OpenGL(GfxWindow* win, RgfwGlContext* ctx)
{
	rgfw_window_deleteContextPtr_OpenGL(win, ctx);
	if (win != null && (win.src.gfxType & RGFW_GFX_OWNED_BY_RGFW)) rgfw_free_impl(ctx);
}

fn bool rgfw_extensionSupported_OpenGL(char* extension, usz len)
{
	if (rgfw_extensionSupported_base(extension, len, &rgfw_getProcAddress_OpenGL)) return true;
	return rgfw_extensionSupportedPlatform_OpenGL(extension, len);
}

fn void rgfw_window_makeCurrentWindow_OpenGL(GfxWindow* win)
{
	if (win != null) _rgfw.current = win;
	rgfw_window_makeCurrentContext_OpenGL(win);
}

fn GfxWindow* rgfw_getCurrentWindow_OpenGL() => _rgfw.current;

fn bool rgfw_extensionSupportedPlatform_OpenGL(char* extension, usz len) => false;

fn RgfwProc rgfw_getProcAddress_OpenGL(char* procname)
{
	if (_rgfw == null || _rgfw.nativeGL_handle == null || procname == null) return null;
	CFStringRef symbolName = cfStringCreateWithCString(null, procname, 0x0600);
	RgfwProc symbol = (RgfwProc)cfBundleGetFunctionPointerForName((CFBundleRef)_rgfw.nativeGL_handle, symbolName);
	if (symbolName != null) cfRelease(symbolName);
	return symbol;
}

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
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_OPEN_GL_CONTEXT, "Failed to load pixel format for OpenGL.");
		return false;
	}

	ObjcId share = hints.share != null ? (ObjcId)hints.share.ctx : null;
	ctx.ctx = objc_send(ns_alloc("NSOpenGLContext"), ObjcSendIdIdId, "initWithFormat:shareContext:", format, share);
	if (ctx.ctx == null)
	{
		ns_release(format);
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_OPEN_GL_CONTEXT, "Failed to create an OpenGL context.");
		return false;
	}
	ctx.format = format;
	objc_send((ObjcId)ctx.ctx, ObjcSendVoidId, "setView:", (ObjcId)win.src.view);
	if (win.internal.flags & RGFW_WINDOW_TRANSPARENT)
	{
		int opacity = 0;
		nsOpenGLContext_setValues((ObjcId)ctx.ctx, &opacity, NS_OPEN_GL_CP_SURFACE_OPACITY);
	}
	rgfw_window_makeCurrentContext_OpenGL(win);
	rgfw_window_swapInterval_OpenGL(win, 0);
	rgfw_debugCallback(RGFW_TYPE_INFO, RGFW_ERR_OPEN_GL_CONTEXT, "OpenGL context initialized.");
	return true;
}

fn void rgfw_window_deleteContextPtr_OpenGL(GfxWindow* win, RgfwGlContext* ctx)
{
	if (ctx == null) return;
	ns_release((ObjcId)ctx.format);
	ns_release((ObjcId)ctx.ctx);
	ctx.format = null;
	ctx.ctx = null;
	if (win != null) win.src.ctx.native = null;
}

fn void rgfw_window_makeCurrentContext_OpenGL(GfxWindow* win)
{
	if (win != null && win.src.ctx.native != null && win.src.ctx.native.ctx != null) { objc_send((ObjcId)win.src.ctx.native.ctx, ObjcSendVoid, "makeCurrentContext"); }
	else { objc_send(objc_class("NSOpenGLContext"), ObjcSendId, "clearCurrentContext"); }
}

fn void* rgfw_getCurrentContext_OpenGL() => objc_send(objc_class("NSOpenGLContext"), ObjcSendId, "currentContext");
fn void rgfw_window_swapBuffers_OpenGL(GfxWindow* win) { if (win != null && win.src.ctx.native != null && win.src.ctx.native.ctx != null) objc_send((ObjcId)win.src.ctx.native.ctx, ObjcSendVoid, "flushBuffer"); }
fn void rgfw_window_swapInterval_OpenGL(GfxWindow* win, int swapInterval) { if (win != null && win.src.ctx.native != null && win.src.ctx.native.ctx != null) nsOpenGLContext_setValues((ObjcId)win.src.ctx.native.ctx, &swapInterval, NS_OPEN_GL_CONTEXT_PARAMETER_SWAP_INTERVAL); }

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

fn ObjcId objc_class(char* name) => (ObjcId)objc_getClass(name);
fn ObjcId ns_alloc(char* className) => objc_send(objc_class(className), ObjcSendId, "alloc");
fn ObjcId ns_retain(ObjcId obj) { if (obj != null) objc_send(obj, ObjcSendId, "retain"); return obj; }
fn void ns_release(ObjcId obj) { if (obj != null) objc_send(obj, ObjcSendVoid, "release"); }
fn ObjcId ns_string(char* str) => objc_send(objc_class("NSString"), ObjcSendIdCharPtr, "stringWithUTF8String:", str);
fn char* ns_utf8(ObjcId str) { if (str == null) return null; return objc_send(str, ObjcSendCharPtr, "UTF8String"); }

fn ObjcId ns_bitmapRep(int w, int h)
{
	return objc_send(ns_alloc("NSBitmapImageRep"), ObjcSendBitmapRepInit,
		"initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bitmapFormat:bytesPerRow:bitsPerPixel:",
		(char**)null, (NSInteger)w, (NSInteger)h, 8, 4, true, false, ns_string("NSCalibratedRGBColorSpace"), (NSUInteger)2, (NSInteger)(w * 4), 32);
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

fn void rgfw_monitorFromDisplay(CGDirectDisplayID display, GfxMonitor* mon)
{
	rgfw_memzero(mon, GfxMonitor::size);
	rgfw_memcpy(&mon.name, "Display", 8);
	CGRect bounds = cgDisplayBounds(display);
	CGDisplayModeRef mode = cgDisplayCopyDisplayMode(display);
	mon.x = (int)bounds.origin.x;
	mon.y = (int)bounds.origin.y;
	mon.mode.w = mode != null ? (int)cgDisplayModeGetWidth(mode) : (int)bounds.size.width;
	mon.mode.h = mode != null ? (int)cgDisplayModeGetHeight(mode) : (int)bounds.size.height;
	mon.mode.refreshRate = mode != null ? (float)cgDisplayModeGetRefreshRate(mode) : 60.0;
	mon.mode.red = 8; mon.mode.green = 8; mon.mode.blue = 8;
	mon.mode.src = mode;
	CGSize phys = cgDisplayScreenSize(display);
	mon.physW = (float)phys.width;
	mon.physH = (float)phys.height;
	mon.scaleX = mon.mode.w > 0 ? ((float)bounds.size.width / (float)mon.mode.w) : 1.0;
	mon.scaleY = mon.mode.h > 0 ? ((float)bounds.size.height / (float)mon.mode.h) : 1.0;
	mon.pixelRatio = mon.scaleX;
}

fn CGDirectDisplayID rgfw_monitorDisplay(GfxMonitor* monitor)
{
	if (monitor != null && monitor.node != null && monitor.node.nativeId0 != 0) return (CGDirectDisplayID)monitor.node.nativeId0;
	return cgMainDisplayID();
}

fn GfxWindow* rgfw_osxDelegateWindow(ObjcId self)
{
	void* out = null;
	object_getInstanceVariable(self, "rgfw_window", &out);
	return (GfxWindow*)out;
}

fn void rgfw_osxWindowWillClose(ObjcId self, ObjcSelector sel, ObjcId notification)
{
	GfxWindow* win = rgfw_osxDelegateWindow(self);
	if (win != null) rgfw_windowCloseCallback(win);
}

fn void rgfw_osxWindowDidResize(ObjcId self, ObjcSelector sel, ObjcId notification)
{
	GfxWindow* win = rgfw_osxDelegateWindow(self);
	if (win == null) return;
	int w = 0;
	int h = 0;
	rgfw_window_fetchSize(win, &w, &h);
	rgfw_windowResizedCallback(win, w, h);
}

fn void rgfw_osxWindowDidMove(ObjcId self, ObjcSelector sel, ObjcId notification)
{
	GfxWindow* win = rgfw_osxDelegateWindow(self);
	if (win == null || win.src.window == null) return;
	CGRect frame = objc_send((ObjcId)win.src.window, ObjcSendRect, "frame");
	CGRect content = objc_send((ObjcId)win.src.window, ObjcSendRectRect, "contentRectForFrameRect:", frame);
	float y = rgfw_cocoaYTransform((float)(content.origin.y + content.size.height - 1));
	rgfw_windowMovedCallback(win, (int)content.origin.x, (int)y);
}

fn void rgfw_osxWindowDidMiniaturize(ObjcId self, ObjcSelector sel, ObjcId notification)
{
	GfxWindow* win = rgfw_osxDelegateWindow(self);
	if (win != null) rgfw_windowMinimizedCallback(win);
}

fn void rgfw_osxWindowDidDeminiaturize(ObjcId self, ObjcSelector sel, ObjcId notification)
{
	GfxWindow* win = rgfw_osxDelegateWindow(self);
	if (win != null) rgfw_windowRestoredCallback(win, win.x, win.y, win.w, win.h);
}

fn void rgfw_osxWindowDidBecomeKey(ObjcId self, ObjcSelector sel, ObjcId notification)
{
	GfxWindow* win = rgfw_osxDelegateWindow(self);
	if (win != null) rgfw_windowFocusCallback(win, true);
}

fn void rgfw_osxWindowDidResignKey(ObjcId self, ObjcSelector sel, ObjcId notification)
{
	GfxWindow* win = rgfw_osxDelegateWindow(self);
	if (win != null) rgfw_windowFocusCallback(win, false);
}

fn GfxWindow* rgfw_osxViewWindow(ObjcId self)
{
	void* out = null;
	object_getInstanceVariable(self, "rgfw_window", &out);
	return (GfxWindow*)out;
}

fn bool rgfw_osxAcceptsFirstResponder(ObjcId self, ObjcSelector sel) => true;

fn void rgfw_osxMouseMovedCommon(ObjcId self, ObjcId event)
{
	GfxWindow* win = rgfw_osxViewWindow(self);
	if (win == null) return;
	CGPoint p = objc_send(event, ObjcSendPoint, "locationInWindow");
	rgfw_mouseMotionCallback(win, (int)p.x, win.h - (int)p.y);
}

fn void rgfw_osxMouseMoved(ObjcId self, ObjcSelector sel, ObjcId event) { rgfw_osxMouseMovedCommon(self, event); }
fn void rgfw_osxMouseDragged(ObjcId self, ObjcSelector sel, ObjcId event) { rgfw_osxMouseMovedCommon(self, event); }

fn void rgfw_osxMouseButtonCommon(ObjcId self, ObjcId event, bool down, RgfwMouseButton fallback)
{
	GfxWindow* win = rgfw_osxViewWindow(self);
	if (win == null) return;
	NSUInteger button = objc_send(event, ObjcSendNSUInteger, "buttonNumber");
	RgfwMouseButton rgfwButton = fallback;
	if (button == 0) { rgfwButton = RGFW_MOUSE_LEFT; }
	else if (button == 1) { rgfwButton = RGFW_MOUSE_RIGHT; }
	else if (button == 2) { rgfwButton = RGFW_MOUSE_MIDDLE; }
	else { rgfwButton = (RgfwMouseButton)button; }
	rgfw_mouseButtonCallback(win, rgfwButton, down);
	rgfw_osxMouseMovedCommon(self, event);
}

fn void rgfw_osxMouseDown(ObjcId self, ObjcSelector sel, ObjcId event) { rgfw_osxMouseButtonCommon(self, event, true, RGFW_MOUSE_LEFT); }
fn void rgfw_osxMouseUp(ObjcId self, ObjcSelector sel, ObjcId event) { rgfw_osxMouseButtonCommon(self, event, false, RGFW_MOUSE_LEFT); }
fn void rgfw_osxRightMouseDown(ObjcId self, ObjcSelector sel, ObjcId event) { rgfw_osxMouseButtonCommon(self, event, true, RGFW_MOUSE_RIGHT); }
fn void rgfw_osxRightMouseUp(ObjcId self, ObjcSelector sel, ObjcId event) { rgfw_osxMouseButtonCommon(self, event, false, RGFW_MOUSE_RIGHT); }
fn void rgfw_osxOtherMouseDown(ObjcId self, ObjcSelector sel, ObjcId event) { rgfw_osxMouseButtonCommon(self, event, true, RGFW_MOUSE_MIDDLE); }
fn void rgfw_osxOtherMouseUp(ObjcId self, ObjcSelector sel, ObjcId event) { rgfw_osxMouseButtonCommon(self, event, false, RGFW_MOUSE_MIDDLE); }

fn void rgfw_osxScrollWheel(ObjcId self, ObjcSelector sel, ObjcId event)
{
	GfxWindow* win = rgfw_osxViewWindow(self);
	if (win == null) return;
	CGFloat x = objc_send(event, ObjcSendCGFloat, "scrollingDeltaX");
	CGFloat y = objc_send(event, ObjcSendCGFloat, "scrollingDeltaY");
	rgfw_mouseScrollCallback(win, (float)x, (float)y);
}

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

fn void rgfw_osxKeyCommon(ObjcId self, ObjcId event, bool down)
{
	GfxWindow* win = rgfw_osxViewWindow(self);
	if (win == null) return;
	ushort keyCode = objc_send(event, ObjcSendUShort, "keyCode");
	if (keyCode >= 256) return;
	RgfwKey key = _rgfw.keycodes[(usz)keyCode];
	if (key == RGFW_KEY_NULL) return;
	bool repeat = down ? objc_send(event, ObjcSendBool, "isARepeat") : false;
	RgfwKeymod mods = rgfw_osxMods(objc_send(event, ObjcSendNSUInteger, "modifierFlags"));
	rgfw_keyCallback(win, key, mods, repeat, down);
}

fn void rgfw_osxKeyDown(ObjcId self, ObjcSelector sel, ObjcId event) { rgfw_osxKeyCommon(self, event, true); }
fn void rgfw_osxKeyUp(ObjcId self, ObjcSelector sel, ObjcId event) { rgfw_osxKeyCommon(self, event, false); }

fn void rgfw_osxMouseEntered(ObjcId self, ObjcSelector sel, ObjcId event)
{
	GfxWindow* win = rgfw_osxViewWindow(self);
	if (win == null) return;
	CGPoint p = objc_send(event, ObjcSendPoint, "locationInWindow");
	rgfw_mouseNotifyCallback(win, (int)p.x, win.h - (int)p.y, true);
}

fn void rgfw_osxMouseExited(ObjcId self, ObjcSelector sel, ObjcId event)
{
	GfxWindow* win = rgfw_osxViewWindow(self);
	if (win == null) return;
	rgfw_mouseNotifyCallback(win, win.internal.lastMouseX, win.internal.lastMouseY, false);
}

fn void rgfw_osxFlagsChanged(ObjcId self, ObjcSelector sel, ObjcId event)
{
	GfxWindow* win = rgfw_osxViewWindow(self);
	if (win == null) return;

	RgfwKey value = RGFW_KEY_NULL;
	bool pressed = false;

	NSUInteger flags = objc_send(event, ObjcSendNSUInteger, "modifierFlags");
	rgfw_keyUpdateKeyModsEx(win,
		rgfw_bool(flags & 0x00010000),
		rgfw_bool(flags & 0x00200000),
		rgfw_bool(flags & 0x00040000),
		rgfw_bool(flags & 0x00080000),
		rgfw_bool(flags & 0x00020000),
		rgfw_bool(flags & 0x00100000),
		false);

	for (int i = 0; i < 9; i++)
	{
		RgfwKey modKey = (RgfwKey)(RGFW_KEY_CAPS_LOCK + i);
		_rgfw.keyboard[(usz)modKey].prev = _rgfw.keyboard[(usz)modKey].current;
	}

	for (int i = 0; i < 5; i++)
	{
		NSUInteger shift = (NSUInteger)1 << (uint)(i + 16);
		RgfwKey key = (RgfwKey)(RGFW_KEY_CAPS_LOCK + i);
		if ((flags & shift) != 0 && !rgfw_isKeyDown(key)) { pressed = true; value = key; break; }
		if ((flags & shift) == 0 && rgfw_isKeyDown(key)) { pressed = false; value = key; break; }
	}

	rgfw_keyCallback(win, value, win.internal.mod, rgfw_bool(rgfw_isKeyDown(value) && pressed), pressed);
	if (value != RGFW_KEY_CAPS_LOCK)
	{
		RgfwKey other = (RgfwKey)(value + 4);
		rgfw_keyCallback(win, other, win.internal.mod, rgfw_bool(rgfw_isKeyDown(other) && pressed), pressed);
	}
}

fn void rgfw_osxViewDidChangeBackingProperties(ObjcId self, ObjcSelector sel)
{
	GfxWindow* win = rgfw_osxViewWindow(self);
	if (win == null) return;
	GfxMonitor* mon = rgfw_window_getMonitor(win);
	if (mon == null) return;
	rgfw_scaleUpdatedCallback(win, mon.scaleX, mon.scaleY);
}

fn bool rgfw_osxWantsUpdateLayer(ObjcId self, ObjcSelector sel) => true;

fn void rgfw_osxUpdateLayer(ObjcId self, ObjcSelector sel)
{
	GfxWindow* win = rgfw_osxViewWindow(self);
	if (win == null) return;
	rgfw_windowRefreshCallback(win, 0, 0, win.w, win.h);
}

fn void rgfw_osxDrawRect(ObjcId self, ObjcSelector sel, CGRect rect)
{
	GfxWindow* win = rgfw_osxViewWindow(self);
	if (win == null) return;
	float y = rgfw_cocoaYTransform((float)(rect.size.height - 1));
	rgfw_windowRefreshCallback(win, (int)rect.origin.x, (int)y, (int)rect.size.width, (int)rect.size.height);
}

fn NSUInteger rgfw_osxDraggingEntered(ObjcId self, ObjcSelector sel, ObjcId sender)
{
	GfxWindow* win = rgfw_osxViewWindow(self);
	if (win == null) return 0;
	CGPoint p = objc_send(sender, ObjcSendPoint, "draggingLocation");
	rgfw_dataDragCallback(win, RGFW_DATA_FILE, RGFW_DND_ACTION_ENTER, (int)p.x, win.h - (int)p.y);
	return 1;
}

fn NSUInteger rgfw_osxDraggingUpdated(ObjcId self, ObjcSelector sel, ObjcId sender)
{
	GfxWindow* win = rgfw_osxViewWindow(self);
	if (win == null) return 0;
	if (!(win.internal.enabledEvents & RGFW_DATA_DRAG_FLAG)) return 1;
	CGPoint p = objc_send(sender, ObjcSendPoint, "draggingLocation");
	rgfw_dataDragCallback(win, RGFW_DATA_FILE, RGFW_DND_ACTION_MOVE, (int)p.x, win.h - (int)p.y);
	return 1;
}

fn void rgfw_osxDraggingEnded(ObjcId self, ObjcSelector sel, ObjcId sender)
{
	GfxWindow* win = rgfw_osxViewWindow(self);
	if (win == null) return;
	if (!(win.internal.enabledEvents & RGFW_DATA_DRAG_FLAG)) return;
	CGPoint p = objc_send(sender, ObjcSendPoint, "draggingLocation");
	rgfw_dataDragCallback(win, RGFW_DATA_FILE, RGFW_DND_ACTION_EXIT, (int)p.x, win.h - (int)p.y);
}

fn bool rgfw_osxPrepareForDragOperation(ObjcId self, ObjcSelector sel, ObjcId sender)
{
	GfxWindow* win = rgfw_osxViewWindow(self);
	if (win == null || !(win.internal.enabledEvents & RGFW_DATA_DROP_FLAG)) return true;
	if (!(win.internal.flags & RGFW_WINDOW_ALLOW_DND)) return false;
	return true;
}

fn bool rgfw_osxPerformDragOperation(ObjcId self, ObjcSelector sel, ObjcId sender)
{
	GfxWindow* win = rgfw_osxViewWindow(self);
	if (win == null || !(win.internal.enabledEvents & RGFW_DATA_DROP_FLAG)) return false;

	ObjcId pasteBoard = objc_send(sender, ObjcSendId, "draggingPasteboard");
	ObjcId types = objc_send(pasteBoard, ObjcSendId, "types");
	ObjcId fileURLsType = ns_string("NSFilenamesPboardType");

	if (!objc_send(types, ObjcSendBoolId, "containsObject:", fileURLsType))
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_CLIPBOARD, "No files found on the pasteboard.");
		return false;
	}

	ObjcId propertyList = objc_send(pasteBoard, ObjcSendIdId, "propertyListForType:", fileURLsType);

	if (objc_send(propertyList, ObjcSendBoolId, "isKindOfClass:", objc_class("NSString")))
	{
		NSUInteger stringCount = objc_send(propertyList, ObjcSendNSUInteger, "length");
		char* filePath = ns_utf8(propertyList);
		rgfw_dataDropCallback(win, filePath, (usz)stringCount + 1, RGFW_DATA_FILE);
	}
	if (!objc_send(propertyList, ObjcSendBoolId, "isKindOfClass:", objc_class("NSArray"))) return false;

	NSUInteger count = objc_send(propertyList, ObjcSendNSUInteger, "count");
	if (count == 0) return false;

	for (NSUInteger i = 0; i < count; i++)
	{
		ObjcId fileURL = objc_send(propertyList, ObjcSendIdNSUInteger, "objectAtIndex:", i);
		char* filePath = ns_utf8(fileURL);
		NSUInteger stringCount = objc_send(fileURL, ObjcSendNSUInteger, "length");
		rgfw_dataDropCallback(win, filePath, (usz)stringCount + 1, RGFW_DATA_FILE);
	}

	return false;
}

fn void rgfw_registerOsxClasses()
{
	if (rgfw_osxWindowDelegateClass != (ObjcClass)null) return;
	rgfw_osxWindowDelegateClass = objc_allocateClassPair(objc_getClass("NSObject"), "RGFWWindowDelegateC3", 0);
	if (rgfw_osxWindowDelegateClass == (ObjcClass)null) return;
	class_addIvar(rgfw_osxWindowDelegateClass, "rgfw_window", void*::size, 3, "^v");
	class_addMethod(rgfw_osxWindowDelegateClass, sel_registerName("windowWillClose:"), (void*)&rgfw_osxWindowWillClose, "v@:@");
	class_addMethod(rgfw_osxWindowDelegateClass, sel_registerName("windowDidResize:"), (void*)&rgfw_osxWindowDidResize, "v@:@");
	class_addMethod(rgfw_osxWindowDelegateClass, sel_registerName("windowDidMove:"), (void*)&rgfw_osxWindowDidMove, "v@:@");
	class_addMethod(rgfw_osxWindowDelegateClass, sel_registerName("windowDidMiniaturize:"), (void*)&rgfw_osxWindowDidMiniaturize, "v@:@");
	class_addMethod(rgfw_osxWindowDelegateClass, sel_registerName("windowDidDeminiaturize:"), (void*)&rgfw_osxWindowDidDeminiaturize, "v@:@");
	class_addMethod(rgfw_osxWindowDelegateClass, sel_registerName("windowDidBecomeKey:"), (void*)&rgfw_osxWindowDidBecomeKey, "v@:@");
	class_addMethod(rgfw_osxWindowDelegateClass, sel_registerName("windowDidResignKey:"), (void*)&rgfw_osxWindowDidResignKey, "v@:@");
	objc_registerClassPair(rgfw_osxWindowDelegateClass);
	rgfw_osxViewClass = objc_allocateClassPair(objc_getClass("NSView"), "RGFWViewC3", 0);
	if (rgfw_osxViewClass == (ObjcClass)null) return;
	class_addIvar(rgfw_osxViewClass, "rgfw_window", void*::size, 3, "^v");
	class_addMethod(rgfw_osxViewClass, sel_registerName("acceptsFirstResponder"), (void*)&rgfw_osxAcceptsFirstResponder, "c@:");
	class_addMethod(rgfw_osxViewClass, sel_registerName("mouseMoved:"), (void*)&rgfw_osxMouseMoved, "v@:@");
	class_addMethod(rgfw_osxViewClass, sel_registerName("mouseDragged:"), (void*)&rgfw_osxMouseDragged, "v@:@");
	class_addMethod(rgfw_osxViewClass, sel_registerName("rightMouseDragged:"), (void*)&rgfw_osxMouseDragged, "v@:@");
	class_addMethod(rgfw_osxViewClass, sel_registerName("otherMouseDragged:"), (void*)&rgfw_osxMouseDragged, "v@:@");
	class_addMethod(rgfw_osxViewClass, sel_registerName("mouseDown:"), (void*)&rgfw_osxMouseDown, "v@:@");
	class_addMethod(rgfw_osxViewClass, sel_registerName("mouseUp:"), (void*)&rgfw_osxMouseUp, "v@:@");
	class_addMethod(rgfw_osxViewClass, sel_registerName("rightMouseDown:"), (void*)&rgfw_osxRightMouseDown, "v@:@");
	class_addMethod(rgfw_osxViewClass, sel_registerName("rightMouseUp:"), (void*)&rgfw_osxRightMouseUp, "v@:@");
	class_addMethod(rgfw_osxViewClass, sel_registerName("otherMouseDown:"), (void*)&rgfw_osxOtherMouseDown, "v@:@");
	class_addMethod(rgfw_osxViewClass, sel_registerName("otherMouseUp:"), (void*)&rgfw_osxOtherMouseUp, "v@:@");
	class_addMethod(rgfw_osxViewClass, sel_registerName("scrollWheel:"), (void*)&rgfw_osxScrollWheel, "v@:@");
	class_addMethod(rgfw_osxViewClass, sel_registerName("keyDown:"), (void*)&rgfw_osxKeyDown, "v@:@");
	class_addMethod(rgfw_osxViewClass, sel_registerName("keyUp:"), (void*)&rgfw_osxKeyUp, "v@:@");
	class_addMethod(rgfw_osxViewClass, sel_registerName("mouseEntered:"), (void*)&rgfw_osxMouseEntered, "v@:@");
	class_addMethod(rgfw_osxViewClass, sel_registerName("mouseExited:"), (void*)&rgfw_osxMouseExited, "v@:@");
	class_addMethod(rgfw_osxViewClass, sel_registerName("flagsChanged:"), (void*)&rgfw_osxFlagsChanged, "v@:@");
	class_addMethod(rgfw_osxViewClass, sel_registerName("viewDidChangeBackingProperties"), (void*)&rgfw_osxViewDidChangeBackingProperties, "v@:");
	class_addMethod(rgfw_osxViewClass, sel_registerName("wantsUpdateLayer"), (void*)&rgfw_osxWantsUpdateLayer, "c@:");
	class_addMethod(rgfw_osxViewClass, sel_registerName("updateLayer"), (void*)&rgfw_osxUpdateLayer, "v@:");
	class_addMethod(rgfw_osxViewClass, sel_registerName("drawRect:"), (void*)&rgfw_osxDrawRect, "v@:{CGRect={CGPoint=dd}{CGSize=dd}}");
	class_addMethod(rgfw_osxViewClass, sel_registerName("draggingEntered:"), (void*)&rgfw_osxDraggingEntered, "l@:@");
	class_addMethod(rgfw_osxViewClass, sel_registerName("draggingUpdated:"), (void*)&rgfw_osxDraggingUpdated, "l@:@");
	class_addMethod(rgfw_osxViewClass, sel_registerName("draggingExited:"), (void*)&rgfw_osxDraggingEnded, "v@:@");
	class_addMethod(rgfw_osxViewClass, sel_registerName("draggingEnded:"), (void*)&rgfw_osxDraggingEnded, "v@:@");
	class_addMethod(rgfw_osxViewClass, sel_registerName("prepareForDragOperation:"), (void*)&rgfw_osxPrepareForDragOperation, "c@:@");
	class_addMethod(rgfw_osxViewClass, sel_registerName("performDragOperation:"), (void*)&rgfw_osxPerformDragOperation, "c@:@");
	objc_registerClassPair(rgfw_osxViewClass);
}

fn void rgfw_monitorNode_free(RgfwMonitorNode* node) { if (node != null && node.mon.mode.src != null) cfRelease(node.mon.mode.src); }

fn void rgfw_window_setMaxSize(GfxWindow* win, int w, int h)
{
	if (win == null || win.src.window == null) return;
	if (w == 0 && h == 0)
	{
		GfxMonitor* mon = rgfw_window_getMonitor(win);
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

fn void rgfw_window_setMinSize(GfxWindow* win, int w, int h)
{
	if (win == null || win.src.window == null) return;
	CGSize size;
	size.width = (CGFloat)w; size.height = (CGFloat)h;
	objc_send((ObjcId)win.src.window, ObjcSendVoidSize, "setMinSize:", size);
}

fn int rgfw_initPlatform(char* className, RgfwInitFlags flags)
{
	_rgfw.nsPool = objc_autoreleasePoolPush();
	_rgfw.nsApp = objc_send(objc_class("NSApplication"), ObjcSendId, "sharedApplication");
	if (_rgfw.nsApp == null)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_PLATFORM, "NSApplication sharedApplication failed.");
		return -1;
	}
	objc_send(_rgfw.nsApp, ObjcSendVoidNSUInteger, "setActivationPolicy:", NS_APP_ACTIVATION_POLICY_REGULAR);
	objc_send(_rgfw.nsApp, ObjcSendVoid, "finishLaunching");
	rgfw_registerOsxClasses();
	rgfw_pollMonitors();
	return 0;
}

fn void rgfw_deinitPlatform()
{
	if (_rgfw.nsPool != null)
	{
		objc_autoreleasePoolPop(_rgfw.nsPool);
		_rgfw.nsPool = null;
	}
}

fn void rgfw_initKeycodesPlatform()
{
	_rgfw.keycodes[0x00] = RGFW_KEY_A; _rgfw.keycodes[0x01] = RGFW_KEY_S; _rgfw.keycodes[0x02] = RGFW_KEY_D; _rgfw.keycodes[0x03] = RGFW_KEY_F;
	_rgfw.keycodes[0x04] = RGFW_KEY_H; _rgfw.keycodes[0x05] = RGFW_KEY_G; _rgfw.keycodes[0x06] = RGFW_KEY_Z; _rgfw.keycodes[0x07] = RGFW_KEY_X;
	_rgfw.keycodes[0x08] = RGFW_KEY_C; _rgfw.keycodes[0x09] = RGFW_KEY_V; _rgfw.keycodes[0x0B] = RGFW_KEY_B; _rgfw.keycodes[0x0C] = RGFW_KEY_Q;
	_rgfw.keycodes[0x0D] = RGFW_KEY_W; _rgfw.keycodes[0x0E] = RGFW_KEY_E; _rgfw.keycodes[0x0F] = RGFW_KEY_R; _rgfw.keycodes[0x10] = RGFW_KEY_Y;
	_rgfw.keycodes[0x11] = RGFW_KEY_T; _rgfw.keycodes[0x12] = RGFW_KEY1; _rgfw.keycodes[0x13] = RGFW_KEY2; _rgfw.keycodes[0x14] = RGFW_KEY3;
	_rgfw.keycodes[0x15] = RGFW_KEY4; _rgfw.keycodes[0x16] = RGFW_KEY6; _rgfw.keycodes[0x17] = RGFW_KEY5; _rgfw.keycodes[0x18] = RGFW_KEY_EQUAL;
	_rgfw.keycodes[0x19] = RGFW_KEY9; _rgfw.keycodes[0x1A] = RGFW_KEY7; _rgfw.keycodes[0x1B] = RGFW_KEY_MINUS; _rgfw.keycodes[0x1C] = RGFW_KEY8;
	_rgfw.keycodes[0x1D] = RGFW_KEY0; _rgfw.keycodes[0x1E] = RGFW_KEY_CLOSE_BRACKET; _rgfw.keycodes[0x1F] = RGFW_KEY_O; _rgfw.keycodes[0x20] = RGFW_KEY_U;
	_rgfw.keycodes[0x21] = RGFW_KEY_BRACKET; _rgfw.keycodes[0x22] = RGFW_KEY_I; _rgfw.keycodes[0x23] = RGFW_KEY_P; _rgfw.keycodes[0x24] = RGFW_KEY_RETURN;
	_rgfw.keycodes[0x25] = RGFW_KEY_L; _rgfw.keycodes[0x26] = RGFW_KEY_J; _rgfw.keycodes[0x27] = RGFW_KEY_APOSTROPHE; _rgfw.keycodes[0x28] = RGFW_KEY_K;
	_rgfw.keycodes[0x29] = RGFW_KEY_SEMICOLON; _rgfw.keycodes[0x2A] = RGFW_KEY_BACK_SLASH; _rgfw.keycodes[0x2B] = RGFW_KEY_COMMA; _rgfw.keycodes[0x2C] = RGFW_KEY_SLASH;
	_rgfw.keycodes[0x2D] = RGFW_KEY_N; _rgfw.keycodes[0x2E] = RGFW_KEY_M; _rgfw.keycodes[0x2F] = RGFW_KEY_PERIOD; _rgfw.keycodes[0x31] = RGFW_KEY_SPACE;
	_rgfw.keycodes[0x32] = RGFW_KEY_BACKTICK; _rgfw.keycodes[0x33] = RGFW_KEY_BACK_SPACE; _rgfw.keycodes[0x35] = RGFW_KEY_ESCAPE; _rgfw.keycodes[0x30] = RGFW_KEY_TAB;
	_rgfw.keycodes[0x7B] = RGFW_KEY_LEFT; _rgfw.keycodes[0x7C] = RGFW_KEY_RIGHT; _rgfw.keycodes[0x7D] = RGFW_KEY_DOWN; _rgfw.keycodes[0x7E] = RGFW_KEY_UP;
	_rgfw.keycodes[0x38] = RGFW_KEY_SHIFT_L; _rgfw.keycodes[0x3B] = RGFW_KEY_CONTROL_L; _rgfw.keycodes[0x3A] = RGFW_KEY_ALT_L; _rgfw.keycodes[0x37] = RGFW_KEY_SUPER_L;
	_rgfw.keycodes[0x3C] = RGFW_KEY_SHIFT_R; _rgfw.keycodes[0x3E] = RGFW_KEY_CONTROL_R; _rgfw.keycodes[0x3D] = RGFW_KEY_ALT_R; _rgfw.keycodes[0x36] = RGFW_KEY_SUPER_R;
	_rgfw.keycodes[0x7A] = RGFW_KEY_F1; _rgfw.keycodes[0x78] = RGFW_KEY_F2; _rgfw.keycodes[0x63] = RGFW_KEY_F3; _rgfw.keycodes[0x76] = RGFW_KEY_F4;
	_rgfw.keycodes[0x60] = RGFW_KEY_F5; _rgfw.keycodes[0x61] = RGFW_KEY_F6; _rgfw.keycodes[0x62] = RGFW_KEY_F7; _rgfw.keycodes[0x64] = RGFW_KEY_F8;
	_rgfw.keycodes[0x65] = RGFW_KEY_F9; _rgfw.keycodes[0x6D] = RGFW_KEY_F10; _rgfw.keycodes[0x67] = RGFW_KEY_F11; _rgfw.keycodes[0x6F] = RGFW_KEY_F12;
	_rgfw.keycodes[0x69] = RGFW_KEY_PRINT_SCREEN; _rgfw.keycodes[0x6B] = RGFW_KEY_F14; _rgfw.keycodes[0x71] = RGFW_KEY_F15; _rgfw.keycodes[0x6A] = RGFW_KEY_F16;
	_rgfw.keycodes[0x40] = RGFW_KEY_F17; _rgfw.keycodes[0x4F] = RGFW_KEY_F18; _rgfw.keycodes[0x50] = RGFW_KEY_F19; _rgfw.keycodes[0x5A] = RGFW_KEY_F20;
	_rgfw.keycodes[0x0A] = RGFW_KEY_WORLD1; _rgfw.keycodes[0x39] = RGFW_KEY_CAPS_LOCK; _rgfw.keycodes[0x75] = RGFW_KEY_DELETE; _rgfw.keycodes[0x77] = RGFW_KEY_END;
	_rgfw.keycodes[0x73] = RGFW_KEY_HOME; _rgfw.keycodes[0x72] = RGFW_KEY_INSERT; _rgfw.keycodes[0x6E] = RGFW_KEY_MENU; _rgfw.keycodes[0x47] = RGFW_KEY_NUM_LOCK;
	_rgfw.keycodes[0x79] = RGFW_KEY_PAGE_DOWN; _rgfw.keycodes[0x74] = RGFW_KEY_PAGE_UP;
	_rgfw.keycodes[0x52] = RGFW_KEY_PAD0; _rgfw.keycodes[0x53] = RGFW_KEY_PAD1; _rgfw.keycodes[0x54] = RGFW_KEY_PAD2; _rgfw.keycodes[0x55] = RGFW_KEY_PAD3;
	_rgfw.keycodes[0x56] = RGFW_KEY_PAD4; _rgfw.keycodes[0x57] = RGFW_KEY_PAD5; _rgfw.keycodes[0x58] = RGFW_KEY_PAD6; _rgfw.keycodes[0x59] = RGFW_KEY_PAD7;
	_rgfw.keycodes[0x5B] = RGFW_KEY_PAD8; _rgfw.keycodes[0x5C] = RGFW_KEY_PAD9; _rgfw.keycodes[0x45] = RGFW_KEY_PAD_SLASH; _rgfw.keycodes[0x41] = RGFW_KEY_PAD_PERIOD;
	_rgfw.keycodes[0x4B] = RGFW_KEY_PAD_SLASH; _rgfw.keycodes[0x4C] = RGFW_KEY_PAD_RETURN; _rgfw.keycodes[0x51] = RGFW_KEY_PAD_EQUAL; _rgfw.keycodes[0x43] = RGFW_KEY_PAD_MULTIPLY;
	_rgfw.keycodes[0x4E] = RGFW_KEY_PAD_MINUS;
}

fn void rgfw_pollEvents()
{
	rgfw_resetPrevState();
	if (_rgfw == null || _rgfw.nsApp == null) return;
	ObjcId distantPast = objc_send(objc_class("NSDate"), ObjcSendId, "distantPast");
	ObjcId mode = ns_string("kCFRunLoopDefaultMode");
	while (true)
	{
		ObjcId ev = objc_send(_rgfw.nsApp, ObjcSendIdEvent, "nextEventMatchingMask:untilDate:inMode:dequeue:", NS_EVENT_MASK_ANY, distantPast, mode, true);
		if (ev == null) break;
		objc_send(_rgfw.nsApp, ObjcSendVoidId, "sendEvent:", ev);
	}
}

fn void rgfw_pollMonitors()
{
	if (_rgfw.monitors.count != 0) rgfw_freeMonitors();
	uint count = 0;
	CGDirectDisplayID[16] displays;
	if (cgGetActiveDisplayList(16, &displays, &count) != 0) return;
	for (uint i = 0; i < count; i++)
	{
		GfxMonitor mon;
		rgfw_monitorFromDisplay(displays[i], &mon);
		RgfwMonitorNode* node = rgfw_monitors_add(&mon);
		if (node == null) continue;
		node.nativeId0 = (ulong)displays[i];
		if (displays[i] == cgMainDisplayID()) _rgfw.monitors.primary = node;
	}
}
fn void rgfw_stopCheckEvents() { if (_rgfw != null) _rgfw.stopCheckEvents_bool = true; }
fn void rgfw_waitForEvent(int waitMS)
{
	if (_rgfw == null || _rgfw.nsApp == null) return;
	ObjcId date = objc_send(objc_class("NSDate"), ObjcSendIdDouble, "dateWithTimeIntervalSinceNow:", (double)waitMS / 1000.0);
	ObjcId mode = ns_string("kCFRunLoopDefaultMode");
	ObjcId ev = objc_send(_rgfw.nsApp, ObjcSendIdEvent, "nextEventMatchingMask:untilDate:inMode:dequeue:", NS_EVENT_MASK_ANY, date, mode, true);
	if (ev != null) objc_send(_rgfw.nsApp, ObjcSendVoidIdBool, "postEvent:atStart:", ev, true);
}
fn bool rgfw_usingWayland() => rgfw_bool(_rgfw != null && _rgfw.wl_display != null);
fn void* rgfw_getLayer_OSX() { if (_rgfw == null || _rgfw.current == null || _rgfw.current.src.view == null) return null; return objc_send((ObjcId)_rgfw.current.src.view, ObjcSendId, "layer"); }
fn void* rgfw_getDisplay_X11() => _rgfw != null ? _rgfw.x11_display : null;
fn void* rgfw_getDisplay_Wayland() => _rgfw != null ? _rgfw.wl_display : null;
fn void rgfw_moveToMacOSResourceDir()
{
	ObjcId bundle = objc_send(objc_class("NSBundle"), ObjcSendId, "mainBundle");
	if (bundle == null) return;
	ObjcId path = objc_send(bundle, ObjcSendId, "resourcePath");
	char* raw = ns_utf8(path);
	if (raw != null) chdir(raw);
}
fn void* rgfw_window_getView_OSX(GfxWindow* win) => win != null ? win.src.view : null;
fn void rgfw_window_setLayer_OSX(GfxWindow* win, void* layer) { if (win != null && win.src.view != null) objc_send((ObjcId)win.src.view, ObjcSendVoidId, "setLayer:", (ObjcId)layer); }
fn void* rgfw_window_getWindow_OSX(GfxWindow* win) => win != null ? win.src.window : null;
fn void* rgfw_window_getHWND(GfxWindow* win) => null;
fn void* rgfw_window_getHDC(GfxWindow* win) => null;
fn ulong rgfw_window_getWindow_X11(GfxWindow* win) => (win != null && win.src.window != null) ? (ulong)win.src.window : 0;
fn void* rgfw_window_getWindow_Wayland(GfxWindow* win) => win != null ? win.src.window : null;

fn GfxWindow* rgfw_createWindowPlatform(char* name, RgfwWindowFlags flags, GfxWindow* win)
{
	NSUInteger style = NS_WINDOW_STYLE_TITLED | NS_WINDOW_STYLE_CLOSABLE | NS_WINDOW_STYLE_MINIATURIZABLE;
	if (!(flags & RGFW_WINDOW_NO_RESIZE)) style |= NS_WINDOW_STYLE_RESIZABLE;
	if (flags & RGFW_WINDOW_NO_BORDER) style = NS_WINDOW_STYLE_BORDERLESS;
	CGRect rect = ns_rect((double)win.x, (double)rgfw_cocoaYTransform((float)(win.y + win.h - 1)), (double)win.w, (double)win.h);
	ObjcId nswin = objc_send(ns_alloc("NSWindow"), ObjcSendIdWindowInit, "initWithContentRect:styleMask:backing:defer:", rect, style, NS_BACKING_STORE_BUFFERED, false);
	if (nswin == null) return null;
	ObjcId view = rgfw_osxViewClass != (ObjcClass)null ? objc_send((ObjcId)rgfw_osxViewClass, ObjcSendId, "alloc") : ns_alloc("NSView");
	view = objc_send(view, ObjcSendIdRect, "initWithFrame:", ns_rect(0.0, 0.0, (double)win.w, (double)win.h));
	if (rgfw_osxViewClass != (ObjcClass)null) object_setInstanceVariable(view, "rgfw_window", win);
	ObjcId delegate = null;
	if (rgfw_osxWindowDelegateClass != (ObjcClass)null)
	{
		delegate = objc_send((ObjcId)rgfw_osxWindowDelegateClass, ObjcSendId, "alloc");
		delegate = objc_send(delegate, ObjcSendId, "init");
		object_setInstanceVariable(delegate, "rgfw_window", win);
		objc_send(nswin, ObjcSendVoidId, "setDelegate:", delegate);
	}
	win.src.window = ns_retain(nswin);
	win.src.view = ns_retain(view);
	win.src.delegate = delegate;
	win.src.x = win.x; win.src.y = win.y; win.src.w = win.w; win.src.h = win.h;
	objc_send(nswin, ObjcSendVoidId, "setContentView:", view);
	objc_send(nswin, ObjcSendVoidId, "makeFirstResponder:", view);
	objc_send(nswin, ObjcSendVoidBool, "setAcceptsMouseMovedEvents:", true);
	rgfw_window_setName(win, name);
	rgfw_window_setFloating(win, rgfw_bool(flags & RGFW_WINDOW_FLOATING));
	if (!(flags & RGFW_WINDOW_HIDE)) rgfw_window_showPlatform(win);
	if (flags & RGFW_WINDOW_MAXIMIZE) rgfw_window_maximizePlatform(win);
	if (flags & RGFW_WINDOW_MINIMIZE) rgfw_window_minimizePlatform(win);
	if (flags & RGFW_WINDOW_FULLSCREEN) rgfw_window_setFullscreen(win, true);
	return win;
}

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

fn bool rgfw_window_setMousePlatform(GfxWindow* win, RgfwMouse* mouse)
{
	if (mouse == null) return false;
	objc_send((ObjcId)mouse, ObjcSendVoid, "set");
	return true;
}

fn void rgfw_window_showPlatform(GfxWindow* win)
{
	if (win == null || win.src.window == null) return;
	objc_send((ObjcId)win.src.window, ObjcSendVoidId, "makeKeyAndOrderFront:", null);
	if (_rgfw.nsApp != null) objc_send(_rgfw.nsApp, ObjcSendVoidBool, "activateIgnoringOtherApps:", true);
	win.internal.flags &= ~((uint)RGFW_WINDOW_HIDE);
}

fn void rgfw_window_restorePlatform(GfxWindow* win)
{
	if (win == null || win.src.window == null) return;
	objc_send((ObjcId)win.src.window, ObjcSendVoidId, "deminiaturize:", null);
	win.internal.flags &= ~((uint)(RGFW_WINDOW_MINIMIZE | RGFW_WINDOW_MAXIMIZE));
}

fn void rgfw_window_minimizePlatform(GfxWindow* win) { if (win != null && win.src.window != null) objc_send((ObjcId)win.src.window, ObjcSendVoidId, "miniaturize:", null); }
fn void rgfw_window_movePlatform(GfxWindow* win, int x, int y)
{
	if (win == null || win.src.window == null) return;
	CGRect content = objc_send((ObjcId)win.src.view, ObjcSendRect, "frame");
	win.x = x;
	win.y = (int)rgfw_cocoaYTransform((float)y + (float)content.size.height - 1.0f);
	CGPoint origin = { .x = (double)x, .y = (double)y };
	objc_send((ObjcId)win.src.window, ObjcSendVoidPoint, "setFrameOrigin:", origin);
}
fn void rgfw_window_resizePlatform(GfxWindow* win, int w, int h)
{
	if (win == null || win.src.window == null) return;
	CGRect frame = objc_send((ObjcId)win.src.window, ObjcSendRect, "frame");
	CGRect content = objc_send((ObjcId)win.src.window, ObjcSendRectRect, "contentRectForFrameRect:", frame);
	double offset = frame.size.height - content.size.height;
	objc_send((ObjcId)win.src.window, ObjcSendVoidRectBool, "setFrame:display:", ns_rect((double)win.x, (double)win.y, (double)w, (double)h + offset), true);
}
fn void rgfw_window_maximizePlatform(GfxWindow* win) { if (win != null && win.src.window != null) objc_send((ObjcId)win.src.window, ObjcSendVoidId, "zoom:", null); }
fn void rgfw_window_captureMousePlatform(GfxWindow* win, bool state) { cgAssociateMouseAndMouseCursorPosition(!state); }
fn void rgfw_window_setRawMouseModePlatform(GfxWindow* win, bool state) { _rgfw.rawMouse = state; }
fn bool rgfw_createSurfacePtr(char* data, int w, int h, RgfwFormat format, GfxSurface* surface)
{
	if (surface == null) return false;
	surface.data = data;
	surface.w = w;
	surface.h = h;
	surface.format = format;
	surface.native.format = RGFW_FORMAT_RGBA8;
	surface.native.buffer = (char*)rgfw_alloc_impl((usz)(w * h * 4));
	return rgfw_bool(surface.native.buffer != null);
}

fn void rgfw_surface_freePtr(GfxSurface* surface)
{
	if (surface == null) return;
	if (surface.native.native0 != null) ns_release((ObjcId)surface.native.native0);
	rgfw_free_impl(surface.native.buffer);
	surface.native.buffer = null;
	surface.native.native0 = null;
}

fn RgfwMouse* rgfw_createMouse(char* data, int w, int h, RgfwFormat format)
{
	if (data == null) return null;
	ObjcId rep = ns_bitmapRep(w, h);
	char* pixels = ns_bitmapData(rep);
	if (pixels == null) { ns_release(rep); return null; }
	rgfw_copyImageData(pixels, w, h, RGFW_FORMAT_RGBA8, data, format, null);
	ObjcId image = ns_imageWithRep(rep, w, h);
	CGPoint hot;
	hot.x = 0.0; hot.y = 0.0;
	ObjcId cursor = objc_send(ns_alloc("NSCursor"), ObjcSendIdImagePoint, "initWithImage:hotSpot:", image, hot);
	ns_release(image);
	ns_release(rep);
	return (RgfwMouse*)cursor;
}

fn RgfwMouse* rgfw_createMouseStandard(RgfwMouseIcon mouse)
{
	ObjcId cls = objc_class("NSCursor");
	ObjcId cursor = null;
	switch (mouse)
	{
		case RGFW_MOUSE_NORMAL: cursor = objc_send(cls, ObjcSendId, "arrowCursor");
		case RGFW_MOUSE_ARROW: cursor = objc_send(cls, ObjcSendId, "arrowCursor");
		case RGFW_MOUSE_IBEAM: cursor = objc_send(cls, ObjcSendId, "IBeamCursor");
		case RGFW_MOUSE_CROSSHAIR: cursor = objc_send(cls, ObjcSendId, "crosshairCursor");
		case RGFW_MOUSE_POINTING_HAND: cursor = objc_send(cls, ObjcSendId, "pointingHandCursor");
		case RGFW_MOUSE_RESIZE_EW: cursor = objc_send(cls, ObjcSendId, "resizeLeftRightCursor");
		case RGFW_MOUSE_RESIZE_E: cursor = objc_send(cls, ObjcSendId, "resizeLeftRightCursor");
		case RGFW_MOUSE_RESIZE_W: cursor = objc_send(cls, ObjcSendId, "resizeLeftRightCursor");
		case RGFW_MOUSE_RESIZE_NS: cursor = objc_send(cls, ObjcSendId, "resizeUpDownCursor");
		case RGFW_MOUSE_RESIZE_N: cursor = objc_send(cls, ObjcSendId, "resizeUpDownCursor");
		case RGFW_MOUSE_RESIZE_S: cursor = objc_send(cls, ObjcSendId, "resizeUpDownCursor");
		case RGFW_MOUSE_RESIZE_NWSE: cursor = objc_send(cls, ObjcSendId, "_windowResizeNorthWestSouthEastCursor");
		case RGFW_MOUSE_RESIZE_NW: cursor = objc_send(cls, ObjcSendId, "_windowResizeNorthWestSouthEastCursor");
		case RGFW_MOUSE_RESIZE_SE: cursor = objc_send(cls, ObjcSendId, "_windowResizeNorthWestSouthEastCursor");
		case RGFW_MOUSE_RESIZE_NESW: cursor = objc_send(cls, ObjcSendId, "_windowResizeNorthEastSouthWestCursor");
		case RGFW_MOUSE_RESIZE_NE: cursor = objc_send(cls, ObjcSendId, "_windowResizeNorthEastSouthWestCursor");
		case RGFW_MOUSE_RESIZE_SW: cursor = objc_send(cls, ObjcSendId, "_windowResizeNorthEastSouthWestCursor");
		case RGFW_MOUSE_RESIZE_ALL: cursor = objc_send(cls, ObjcSendId, "openHandCursor");
		case RGFW_MOUSE_NOT_ALLOWED: cursor = objc_send(cls, ObjcSendId, "operationNotAllowedCursor");
		case RGFW_MOUSE_WAIT: cursor = objc_send(cls, ObjcSendId, "arrowCursor");
		case RGFW_MOUSE_PROGRESS: cursor = objc_send(cls, ObjcSendId, "arrowCursor");
		default: cursor = objc_send(cls, ObjcSendId, "arrowCursor");
	}
	return (RgfwMouse*)ns_retain(cursor);
}

fn void rgfw_freeMouse(RgfwMouse* mouse) { ns_release((ObjcId)mouse); }

fn bool rgfw_monitor_getWorkarea(GfxMonitor* monitor, int* x, int* y, int* width, int* height)
{
	CGRect bounds = cgDisplayBounds(rgfw_monitorDisplay(monitor));
	if (x != null) *x = (int)bounds.origin.x;
	if (y != null) *y = (int)bounds.origin.y;
	if (width != null) *width = (int)bounds.size.width;
	if (height != null) *height = (int)bounds.size.height;
	return true;
}

fn usz rgfw_monitor_getModesPtr(GfxMonitor* monitor, RgfwMonitorMode** modes)
{
	CFArrayRef list = cgDisplayCopyAllDisplayModes(rgfw_monitorDisplay(monitor), null);
	if (list == null) return 0;
	usz count = (usz)cfArrayGetCount(list);
	if (modes != null)
	{
		RgfwMonitorMode* out = *modes;
		for (usz i = 0; i < count; i++)
		{
			CGDisplayModeRef mode = (CGDisplayModeRef)cfArrayGetValueAtIndex(list, (CFIndex)i);
			out[i].w = (int)cgDisplayModeGetWidth(mode);
			out[i].h = (int)cgDisplayModeGetHeight(mode);
			out[i].refreshRate = (float)cgDisplayModeGetRefreshRate(mode);
			out[i].red = 8; out[i].green = 8; out[i].blue = 8;
			out[i].src = mode;
		}
	}
	cfRelease(list);
	return count;
}

fn usz rgfw_monitor_getGammaRampPtr(GfxMonitor* monitor, RgfwGammaRamp* ramp)
{
	CGDirectDisplayID display = rgfw_monitorDisplay(monitor);
	uint count = cgDisplayGammaTableCapacity(display);
	if (ramp == null) return (usz)count;
	float* red = (float*)rgfw_alloc_impl((usz)count * float::size);
	float* green = (float*)rgfw_alloc_impl((usz)count * float::size);
	float* blue = (float*)rgfw_alloc_impl((usz)count * float::size);
	uint actual = 0;
	if (red == null || green == null || blue == null || cgGetDisplayTransferByTable(display, count, red, green, blue, &actual) != 0) { rgfw_free_impl(red); rgfw_free_impl(green); rgfw_free_impl(blue); return 0; }
	for (uint i = 0; i < actual; i++)
	{
		ramp.red[i] = (ushort)(red[i] * 65535.0);
		ramp.green[i] = (ushort)(green[i] * 65535.0);
		ramp.blue[i] = (ushort)(blue[i] * 65535.0);
	}
	ramp.count = (usz)actual;
	rgfw_free_impl(red); rgfw_free_impl(green); rgfw_free_impl(blue);
	return (usz)actual;
}

fn bool rgfw_monitor_setGammaRamp(GfxMonitor* monitor, RgfwGammaRamp* ramp)
{
	if (ramp == null || ramp.count == 0) return false;
	float* red = (float*)rgfw_alloc_impl(ramp.count * float::size);
	float* green = (float*)rgfw_alloc_impl(ramp.count * float::size);
	float* blue = (float*)rgfw_alloc_impl(ramp.count * float::size);
	if (red == null || green == null || blue == null) { rgfw_free_impl(red); rgfw_free_impl(green); rgfw_free_impl(blue); return false; }
	for (usz i = 0; i < ramp.count; i++)
	{
		red[i] = (float)ramp.red[i] / 65535.0;
		green[i] = (float)ramp.green[i] / 65535.0;
		blue[i] = (float)ramp.blue[i] / 65535.0;
	}
	int err = cgSetDisplayTransferByTable(rgfw_monitorDisplay(monitor), (uint)ramp.count, red, green, blue);
	rgfw_free_impl(red); rgfw_free_impl(green); rgfw_free_impl(blue);
	return rgfw_bool(err == 0);
}

fn bool rgfw_monitor_requestMode(GfxMonitor* mon, RgfwMonitorMode* mode, RgfwModeRequest request) => rgfw_monitor_setMode(mon, mode);
fn bool rgfw_monitor_setMode(GfxMonitor* mon, RgfwMonitorMode* mode) { if (mode == null || mode.src == null) return false; return rgfw_bool(cgDisplaySetDisplayMode(rgfw_monitorDisplay(mon), (CGDisplayModeRef)mode.src, null) == 0); }
fn GfxMonitor* rgfw_window_getMonitor(GfxWindow* win) => rgfw_getPrimaryMonitor();
fn bool rgfw_window_fetchSize(GfxWindow* win, int* w, int* h)
{
	if (win != null && win.src.window != null)
	{
		CGRect frame = objc_send((ObjcId)win.src.window, ObjcSendRect, "frame");
		win.w = (int)frame.size.width; win.h = (int)frame.size.height;
	}
	if (w != null) *w = win.w;
	if (h != null) *h = win.h;
	return true;
}

fn void rgfw_window_focus(GfxWindow* win) { if (win != null && win.src.window != null) objc_send((ObjcId)win.src.window, ObjcSendVoidId, "makeKeyWindow", null); if (win != null) win.internal.inFocus = true; }
fn void rgfw_window_raise(GfxWindow* win) { if (win != null && win.src.window != null) objc_send((ObjcId)win.src.window, ObjcSendVoidId, "orderFrontRegardless", null); }
fn void rgfw_window_setFullscreen(GfxWindow* win, bool fullscreen) { if (win != null && win.src.window != null) objc_send((ObjcId)win.src.window, ObjcSendVoidId, "toggleFullScreen:", null); if (win != null) rgfw_setBit(&win.internal.flags, RGFW_WINDOW_FULLSCREEN, fullscreen); }
fn void rgfw_window_setFloating(GfxWindow* win, bool floating) { if (win != null && win.src.window != null) objc_send((ObjcId)win.src.window, ObjcSendVoidNSInteger, "setLevel:", floating ? NS_WINDOW_LEVEL_FLOATING : NS_WINDOW_LEVEL_NORMAL); if (win != null) rgfw_setBit(&win.internal.flags, RGFW_WINDOW_FLOATING, floating); }
fn void rgfw_window_setOpacity(GfxWindow* win, char opacity) { if (win != null && win.src.window != null) objc_send((ObjcId)win.src.window, ObjcSendVoidCGFloat, "setAlphaValue:", (CGFloat)opacity / 255.0); }
fn void rgfw_window_setBorder(GfxWindow* win, bool border)
{
	if (win == null || win.src.window == null) return;
	CGRect frame = objc_send((ObjcId)win.src.window, ObjcSendRect, "frame");
	CGRect content = objc_send((ObjcId)win.src.view, ObjcSendRect, "frame");
	double offset = 0;

	rgfw_setBit(&win.internal.flags, RGFW_WINDOW_NO_BORDER, rgfw_bool(!border));
	NSUInteger styleMask = NS_WINDOW_STYLE_BORDERLESS | NS_WINDOW_STYLE_FULL_SIZE_CONTENT_VIEW;
	if (border) styleMask = NS_WINDOW_STYLE_TITLED | NS_WINDOW_STYLE_CLOSABLE | NS_WINDOW_STYLE_MINIATURIZABLE;
	if (!(win.internal.flags & RGFW_WINDOW_NO_RESIZE)) styleMask |= NS_WINDOW_STYLE_RESIZABLE;

	objc_send((ObjcId)win.src.window, ObjcSendVoidNSUInteger, "setStyleMask:", styleMask);

	if (!border)
	{
		ObjcId miniaturizeButton = objc_send((ObjcId)win.src.window, ObjcSendIdNSUInteger, "standardWindowButton:", NS_WINDOW_MINIATURIZE_BUTTON);
		ObjcId titleBarView = objc_send(miniaturizeButton, ObjcSendId, "superview");
		objc_send(titleBarView, ObjcSendVoidBool, "setHidden:", true);
		offset = (double)(frame.size.height - content.size.height);
	}

	rgfw_window_resize(win, win.w, win.h + (int)offset);
	win.h -= (int)offset;
}
fn void rgfw_window_setMousePassthrough(GfxWindow* win, bool passthrough) { if (win != null && win.src.window != null) objc_send((ObjcId)win.src.window, ObjcSendVoidBool, "setIgnoresMouseEvents:", passthrough); }
fn void rgfw_window_setName(GfxWindow* win, char* name) { if (win != null && win.src.window != null && name != null) objc_send((ObjcId)win.src.window, ObjcSendVoidId, "setTitle:", ns_string(name)); }
fn bool GfxWindow.set_icon(&win, char* data, int w, int h, RgfwFormat format, GfxWindowIconType type = BOTH)
{
	if (_rgfw == null || _rgfw.nsApp == null) return false;
	if (data == null)
	{
		objc_send(_rgfw.nsApp, ObjcSendVoidId, "setApplicationIconImage:", null);
		return true;
	}
	ObjcId rep = ns_bitmapRep(w, h);
	char* pixels = ns_bitmapData(rep);
	if (pixels == null) { ns_release(rep); return false; }
	rgfw_copyImageData(pixels, w, h, RGFW_FORMAT_RGBA8, data, format, null);
	ObjcId image = ns_imageWithRep(rep, w, h);
	objc_send(_rgfw.nsApp, ObjcSendVoidId, "setApplicationIconImage:", image);
	ns_release(image);
	ns_release(rep);
	return true;
}
fn void rgfw_window_hide(GfxWindow* win) { if (win != null && win.src.window != null) objc_send((ObjcId)win.src.window, ObjcSendVoidId, "orderOut:", null); win.internal.flags |= RGFW_WINDOW_HIDE; }
fn void rgfw_window_flash(GfxWindow* win, GfxFlashRequest request) { if (_rgfw.nsApp != null) objc_send(_rgfw.nsApp, ObjcSendIntegerNSUInteger, "requestUserAttention:", (NSUInteger)request); }
fn bool rgfw_window_isHidden(GfxWindow* win) { if (win != null && win.src.window != null) return objc_send((ObjcId)win.src.window, ObjcSendBool, "isVisible") ? false : true; return rgfw_bool(win.internal.flags & RGFW_WINDOW_HIDE); }
fn bool rgfw_window_isMinimized(GfxWindow* win) { if (win != null && win.src.window != null) return objc_send((ObjcId)win.src.window, ObjcSendBool, "isMiniaturized"); return rgfw_bool(win.internal.flags & RGFW_WINDOW_MINIMIZE); }
fn bool rgfw_window_isMaximized(GfxWindow* win) { if (win == null) return false; if (win.src.window != null) return objc_send((ObjcId)win.src.window, ObjcSendBool, "isZoomed"); return rgfw_bool(win.internal.flags & RGFW_WINDOW_MAXIMIZE); }

fn bool GfxWindow.is_floating(&win)
{
	if (!win) return false;
	if (win.src.window) return objc_send((ObjcId)win.src.window, ObjcSendInteger, "level") > NS_WINDOW_LEVEL_NORMAL;
	return win.internal.flags & RGFW_WINDOW_FLOATING != 0;
}

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

fn void rgfw_window_moveMouse(GfxWindow* win, int x, int y)
{
	CGPoint point;
	point.x = (CGFloat)x; point.y = (CGFloat)y;
	cgWarpMouseCursorPosition(point);
	if (win != null) { win.internal.lastMouseX = x; win.internal.lastMouseY = y; }
}

fn bool rgfw_readClipboardPtr(RgfwDataTransferType requestedType, char* buffer, usz capacity, GfxDataTransfer* data)
{
	if (requestedType != RGFW_DATA_TEXT || data == null) return false;
	ObjcId pb = objc_send(objc_class("NSPasteboard"), ObjcSendId, "generalPasteboard");
	ObjcId str = objc_send(pb, ObjcSendIdId, "stringForType:", ns_string("public.utf8-plain-text"));
	char* raw = ns_utf8(str);
	if (raw == null) return false;
	usz len = strlen(raw);
	if (buffer != null && capacity > 0)
	{
		usz copyLen = len < capacity - 1 ? len : capacity - 1;
		rgfw_memcpy(buffer, raw, copyLen);
		buffer[copyLen] = 0;
		data.data = buffer;
	}
	else { data.data = raw; }
	data.length = len;
	data.type = RGFW_DATA_TEXT;
	return true;
}

fn bool rgfw_writeClipboard(GfxDataTransfer* data)
{
	if (data == null || data.type != RGFW_DATA_TEXT || data.data == null) return false;
	ObjcId pb = objc_send(objc_class("NSPasteboard"), ObjcSendId, "generalPasteboard");
	objc_send(pb, ObjcSendVoid, "clearContents");
	return objc_send(pb, ObjcSendBoolIdId, "setString:forType:", ns_string(data.data), ns_string("public.utf8-plain-text"));
}

fn void* rgfw_eglContext_getSourceContext(RgfwEglContext* ctx) => ctx != null ? ctx.ctx : null;
fn void* rgfw_eglContext_getSurface(RgfwEglContext* ctx) => ctx != null ? ctx.surface : null;
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

fn bool rgfw_extensionSupportedPlatform_EGL(char* extension, usz len)
{
	if (rgfw_eglQueryString == null || _rgfw.egl_display == null) return false;
	char* extensions = rgfw_eglQueryString((EglDisplay)_rgfw.egl_display, EGL_EXTENSIONS);
	return rgfw_extensionSupportedStr(extensions, extension, len);
}

fn bool rgfw_extensionSupported_EGL(char* extension, usz len)
{
	if (rgfw_extensionSupported_base(extension, len, &rgfw_getProcAddress_EGL)) return true;
	return rgfw_extensionSupportedPlatform_EGL(extension, len);
}

fn void* rgfw_getCurrentContext_EGL() => rgfw_eglGetCurrentContext != null ? rgfw_eglGetCurrentContext() : null;
fn GfxWindow* rgfw_getCurrentWindow_EGL() => _rgfw.current;
fn void* rgfw_getDisplay_EGL() => _rgfw.egl_display;
fn RgfwProc rgfw_getProcAddress_EGL(char* procname)
{
	if (procname == null) return null;
	void* proc = rgfw_eglProc(procname);
	return (RgfwProc)proc;
}

fn RgfwEglContext* rgfw_window_createContext_EGL(GfxWindow* win, RgfwGlHints* hints)
{
	RgfwEglContext* ctx = (RgfwEglContext*)rgfw_alloc_impl(RgfwEglContext::size);
	if (ctx == null) return null;
	rgfw_memzero(ctx, RgfwEglContext::size);
	if (rgfw_window_createContextPtr_EGL(win, ctx, hints) == false)
	{
		rgfw_free_impl(ctx);
		return null;
	}
	win.src.gfxType |= RGFW_GFX_OWNED_BY_RGFW;
	return ctx;
}

fn bool rgfw_window_createContextPtr_EGL(GfxWindow* win, RgfwEglContext* ctx, RgfwGlHints* hints)
{
	if (win == null || ctx == null || hints == null) return false;
	if (rgfw_loadEGL() == false) return false;
	if (_rgfw.egl_display == null)
	{
		_rgfw.egl_display = rgfw_eglGetDisplay(null);
		if (_rgfw.egl_display == null) return false;
		EglInt major = 0;
		EglInt minor = 0;
		if (rgfw_eglInitialize((EglDisplay)_rgfw.egl_display, &major, &minor) == 0) return false;
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
	if (rgfw_eglChooseConfig((EglDisplay)_rgfw.egl_display, &cfg, &config, 1, &num) == 0 || num == 0) return false;
	rgfw_eglBindApi(hints.profile == RGFW_GL_ES ? (uint)EGL_OPENGL_ES_API : (uint)EGL_OPENGL_API);
	EglInt[8] ctxAttribs;
	RgfwAttribStack ctxStack;
	rgfw_attribStack_init(&ctxStack, &ctxAttribs, 8);
	if (hints.profile == RGFW_GL_ES) rgfw_attribStack_pushAttribs(&ctxStack, EGL_CONTEXT_CLIENT_VERSION, hints.major != 0 ? hints.major : 2);
	rgfw_attribStack_pushAttribs(&ctxStack, EGL_NONE, EGL_NONE);
	EglContext share = hints.shareEGL != null ? (EglContext)hints.shareEGL.ctx : null;
	ctx.ctx = rgfw_eglCreateContext((EglDisplay)_rgfw.egl_display, config, share, &ctxAttribs);
	if (ctx.ctx == null) return false;
	EglNativeWindowType nativeWindow = win.src.window;
	if (win.src.view != null) nativeWindow = rgfw_getLayer_OSX();
	ctx.surface = rgfw_eglCreateWindowSurface((EglDisplay)_rgfw.egl_display, config, nativeWindow, null);
	if (ctx.surface == null)
	{
		rgfw_eglDestroyContext((EglDisplay)_rgfw.egl_display, (EglContext)ctx.ctx);
		ctx.ctx = null;
		return false;
	}
	win.src.ctx.egl = ctx;
	win.src.gfxType = RGFW_GFX_EGL;
	rgfw_window_makeCurrentContext_EGL(win);
	return true;
}

fn RgfwEglContext* rgfw_window_getContext_EGL(GfxWindow* win) => win != null ? win.src.ctx.egl : null;

fn void rgfw_window_deleteContext_EGL(GfxWindow* win, RgfwEglContext* ctx)
{
	rgfw_window_deleteContextPtr_EGL(win, ctx);
	if (win != null && (win.src.gfxType & RGFW_GFX_OWNED_BY_RGFW)) rgfw_free_impl(ctx);
}

fn void rgfw_window_deleteContextPtr_EGL(GfxWindow* win, RgfwEglContext* ctx)
{
	if (ctx == null || _rgfw.egl_display == null) return;
	if (ctx.surface != null) rgfw_eglDestroySurface((EglDisplay)_rgfw.egl_display, (EglSurface)ctx.surface);
	if (ctx.ctx != null) rgfw_eglDestroyContext((EglDisplay)_rgfw.egl_display, (EglContext)ctx.ctx);
	ctx.surface = null;
	ctx.ctx = null;
	if (win != null) win.src.ctx.egl = null;
}

fn void rgfw_window_makeCurrentContext_EGL(GfxWindow* win)
{
	if (_rgfw.egl_display == null || rgfw_eglMakeCurrent == null) return;
	if (win == null || win.src.ctx.egl == null) { rgfw_eglMakeCurrent((EglDisplay)_rgfw.egl_display, null, null, null); return; }
	RgfwEglContext* ctx = win.src.ctx.egl;
	rgfw_eglMakeCurrent((EglDisplay)_rgfw.egl_display, (EglSurface)ctx.surface, (EglSurface)ctx.surface, (EglContext)ctx.ctx);
}
fn void rgfw_window_makeCurrentWindow_EGL(GfxWindow* win) { _rgfw.current = win; }
fn void rgfw_window_swapBuffers_EGL(GfxWindow* win) { if (win != null && win.src.ctx.egl != null && rgfw_eglSwapBuffers != null) rgfw_eglSwapBuffers((EglDisplay)_rgfw.egl_display, (EglSurface)win.src.ctx.egl.surface); }
fn void rgfw_window_swapInterval_EGL(GfxWindow* win, int swapInterval) { if (rgfw_eglSwapInterval != null) rgfw_eglSwapInterval((EglDisplay)_rgfw.egl_display, swapInterval); }

alias WGPUInstance = void*;
alias WGPUSurface = void*;
alias WGPUSType = int;
const WGPUSType WGPU_STYPE_SURFACE_SOURCE_METAL_LAYER = 0x00000004;
struct WgpuChainedStruct { WgpuChainedStruct* next; WGPUSType sType; }
struct WgpuSurfaceDescriptor { WgpuChainedStruct* nextInChain; }
struct WgpuSurfaceSourceMetalLayer { WgpuChainedStruct chain; void* layer; }
extern fn WGPUSurface wgpuInstanceCreateSurface(WGPUInstance instance, WgpuSurfaceDescriptor* descriptor) @cname("wgpuInstanceCreateSurface");

fn WGPUSurface rgfw_window_createSurface_WebGPU(GfxWindow* window, WGPUInstance instance)
{
	if (window == null || window.src.view == null)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_METAL, "NSView is NULL for macOS window");
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

fn void rgfw_osx_initView(GfxWindow* win)
{
	if (win == null || win.src.window == null || win.src.view == null) return;
	objc_send((ObjcId)win.src.window, ObjcSendVoidId, "setContentView:", (ObjcId)win.src.view);
	objc_send((ObjcId)win.src.window, ObjcSendVoidId, "makeFirstResponder:", (ObjcId)win.src.view);
}
fn float rgfw_cocoaYTransform(float y) => (float)(cgDisplayBounds(cgMainDisplayID()).size.height - (double)y - 1.0);

fn GfxDataTransfer* rgfw_readClipboard(RgfwDataTransferType requestedType)
{
	return _rgfw.clipboard;
}

fn GfxDataTransfer* rgfw_readClipboardString() { return rgfw_readClipboard(RGFW_DATA_TEXT); }
fn bool rgfw_readClipboardStringPtr(char* buffer, usz capacity, GfxDataTransfer* data) { return rgfw_readClipboardPtr(RGFW_DATA_TEXT, buffer, capacity, data); }
