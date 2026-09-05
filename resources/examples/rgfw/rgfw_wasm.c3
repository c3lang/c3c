module std::gfx;

// C3 port of the WASM/WebGL RGFW backend from /Users/lerno/Downloads/AGFW.h (RGFW).
// Mechanical, line-by-line style translation of the RGFW_WASM implementation.
// Standalone alternative to rgfw.c3 (macOS) / rgfw_x11.c3 / rgfw_wayland.c3 /
// rgfw_windows.c3 - only ONE rgfw*.c3 file should be included for a given
// target, matching how the original C header only ever compiles one #ifdef
// backend at a time.
//
// The original relies on Emscripten's EM_ASM (inline JS) and emscripten_*
// runtime; C3's wasm target has no Emscripten runtime, so every DOM/JS touch
// point here is a named `extern fn ... @wasm("env", "name")` import that a
// small JS host must implement (canvas sizing, cursor style, fullscreen,
// clipboard, WebGL context, DOM event wiring), and the functions RGFW expects
// JS to call back into (key events, drag-drop) are marked @export so the host
// can find them.

const bool RGFW_BUILD_MACOS = false;
const bool RGFW_BUILD_X11 = false;
const bool RGFW_BUILD_WAYLAND = false;
const bool RGFW_BUILD_WINDOWS = false;
const bool RGFW_BUILD_EGL = false;
const bool RGFW_BUILD_WASM = true;

alias VkInstance = void*;
alias VkPhysicalDevice = void*;
alias VkSurfaceKHR = ulong;
alias VkResult = int;
alias RgfwVkGetInstanceProcAddressProc = fn RgfwProc(VkInstance instance, char* procname);

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
	int usingAsyncify;
	bool rawMouse;
	RgfwWindowState windowState;
	RgfwKeyState[8] mouseButtons;
	RgfwKeyState[RGFW_KEY_LAST] keyboard;
	float scrollX, scrollY;
	float vectorX, vectorY;
}

RgfwInfo* _rgfw;
RgfwInfo _rgfw_global;

fn void rgfw_setInfo(RgfwInfo* info) { _rgfw = info; }
fn RgfwInfo* rgfw_getInfo() { return _rgfw; }
fn void* rgfw_alloc(usz size) { return rgfw_alloc_impl(size); }
fn void rgfw_free(void* ptr) { rgfw_free_impl(ptr); }
fn usz rgfw_sizeofInfo() { return RgfwInfo::size; }
fn usz rgfw_sizeofNativeImage() { return RgfwNativeImage::size; }
fn usz rgfw_sizeofSurface() { return GfxSurface::size; }
fn usz rgfw_sizeofWindow() { return GfxWindow::size; }
fn usz rgfw_sizeofWindowSrc() { return RgfwWindowSrc::size; }

fn bool rgfw_loadGL() => true;
fn void rgfw_unloadGL() {}
fn bool rgfw_getPhysicalDevicePresentationSupport_Vulkan(VkInstance instance, VkPhysicalDevice physicalDevice, uint queueFamilyIndex) => false;

alias WGPUInstance = void*;
alias WGPUSurface = void*;
alias WGPUSType = int;
const WGPUSType WGPU_STYPE_EMSCRIPTEN_SURFACE_SOURCE_CANVAS_HTML_SELECTOR = 0x00040000;
struct WgpuChainedStruct { WgpuChainedStruct* next; WGPUSType sType; }
struct WgpuStringView { char* data; usz length; }
struct WgpuSurfaceDescriptor { WgpuChainedStruct* nextInChain; }
struct WgpuEmscriptenSurfaceSourceCanvasHTMLSelector { WgpuChainedStruct chain; WgpuStringView selector; }
extern fn WGPUSurface wgpuInstanceCreateSurface(WGPUInstance instance, WgpuSurfaceDescriptor* descriptor) @wasm("env", "wgpuInstanceCreateSurface");

fn WGPUSurface rgfw_window_createSurface_WebGPU(GfxWindow* window, WGPUInstance instance)
{
	WgpuEmscriptenSurfaceSourceCanvasHTMLSelector canvasDesc;
	canvasDesc.chain.next = null;
	canvasDesc.chain.sType = WGPU_STYPE_EMSCRIPTEN_SURFACE_SOURCE_CANVAS_HTML_SELECTOR;
	canvasDesc.selector.data = "#canvas";
	canvasDesc.selector.length = 7;
	WgpuSurfaceDescriptor surfaceDesc;
	surfaceDesc.nextInChain = (WgpuChainedStruct*)&canvasDesc;
	return wgpuInstanceCreateSurface(instance, &surfaceDesc);
}

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
	if (flags & RGFW_INIT_OPEN_GL && rgfw_loadGL() == false)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_FAILED_FUNC_LOAD, "Failed to load the OpenGL library");
		return -1;
	}
	if (flags & RGFW_INIT_EGL && rgfw_loadEGL() == false)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_FAILED_FUNC_LOAD, "Failed to load the EGL library");
		return -1;
	}
	if (flags & RGFW_INIT_VULKAN && rgfw_loadVulkan() == false)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_FAILED_FUNC_LOAD, "Failed to load the Vulkan library");
		return -1;
	}
	rgfw_initKeycodes();
	int out = rgfw_initPlatform(className, flags);
	if (out != 0)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_INFO_GLOBAL, "failed to initialize global context");
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
	rgfw_debugCallback(RGFW_TYPE_INFO, RGFW_INFO_GLOBAL, "global context initialized");
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
	rgfw_debugCallback(RGFW_TYPE_INFO, RGFW_INFO_GLOBAL, "global context deinitialized");
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
	if (ret == null)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_PLATFORM, "failed to create a window");
		return null;
	}
	flags |= reservedFlags;
	rgfw_window_setFlagsInternal(win, flags, 0);
	if (flags & RGFW_WINDOW_OPEN_GL) rgfw_window_createContext_OpenGL(win, rgfw_getGlobalHints_OpenGL());
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
	if ((win.src.gfxType & RGFW_GFX_NATIVE_OPEN_GL) && win.src.ctx.native != null)
	{
		rgfw_window_deleteContext_OpenGL(win, win.src.ctx.native);
		win.src.ctx.native = null;
	}
	rgfw_window_closePlatform(win);
	if (_rgfw.clipboard != null) rgfw_free_impl(_rgfw.clipboard);
	_rgfw.clipboard = null;
	_rgfw.windowCount--;
	rgfw_debugCallback(RGFW_TYPE_INFO, RGFW_INFO_WINDOW, "a window was freed");
}

fn void rgfw_setQueueEvents(bool queue) { _rgfw.queueEvents = queue; }

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

fn bool rgfw_isKeyPressed(RgfwKey key) => _rgfw.keyboard[(usz)key].current && !_rgfw.keyboard[(usz)key].prev;
fn bool rgfw_isKeyDown(RgfwKey key) => _rgfw.keyboard[(usz)key].current;
fn bool rgfw_isKeyReleased(RgfwKey key) => !_rgfw.keyboard[(usz)key].current && _rgfw.keyboard[(usz)key].prev;
fn bool rgfw_isMouseDown(RgfwMouseButton button) => _rgfw.mouseButtons[(usz)button].current;
fn bool rgfw_isMouseReleased(RgfwMouseButton button) => !_rgfw.mouseButtons[(usz)button].current && _rgfw.mouseButtons[(usz)button].prev;

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

fn bool rgfw_window_didMouseLeave(GfxWindow* win) => _rgfw.windowState.winLeave == win && _rgfw.windowState.mouseLeave;
fn bool rgfw_window_didMouseEnter(GfxWindow* win) => _rgfw.windowState.win == win && _rgfw.windowState.mouseEnter;
fn bool rgfw_window_isMouseInside(GfxWindow* win) => win.internal.mouseInside;
fn bool rgfw_window_isDataDragging(GfxWindow* win) => rgfw_window_getDataDrag(win, null, null);
fn bool rgfw_window_didDataDrop(GfxWindow* win) => rgfw_window_getDataDrop(win) != null;

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

fn void rgfw_window_setAspectRatio(GfxWindow* win, int w, int h) {}
fn RgfwFormat rgfw_nativeFormat() => RGFW_FORMAT_RGBA8;

// ---- JS host imports (no Emscripten runtime under C3's wasm target) ----
extern fn void rgfwJsSetCanvasSize(int w, int h) @wasm("env", "rgfw_set_canvas_size");
extern fn void rgfwJsSetWindowTitle(char* name, usz len) @wasm("env", "rgfw_set_window_title");
extern fn void rgfwJsSetupWindowListeners() @wasm("env", "rgfw_setup_window_listeners");
extern fn void rgfwJsSetupDropListeners() @wasm("env", "rgfw_setup_drop_listeners");
extern fn void rgfwJsBlitSurface(char* data, int w, int h, int minH) @wasm("env", "rgfw_blit_surface");
extern fn void rgfwJsSetCursor(char* name, usz len) @wasm("env", "rgfw_set_cursor");
extern fn void rgfwJsHideCursor() @wasm("env", "rgfw_hide_cursor");
extern fn int rgfwJsGetMouseX() @wasm("env", "rgfw_get_mouse_x");
extern fn int rgfwJsGetMouseY() @wasm("env", "rgfw_get_mouse_y");
extern fn void rgfwJsSetPointerEvents(int passthrough) @wasm("env", "rgfw_set_pointer_events");
extern fn void rgfwJsWriteClipboard(char* data, usz len) @wasm("env", "rgfw_write_clipboard");
extern fn int rgfwJsUsingAsyncify() @wasm("env", "rgfw_using_asyncify");
extern fn void rgfwJsSleep(int ms) @wasm("env", "rgfw_sleep");
extern fn double rgfwJsDevicePixelRatio() @wasm("env", "rgfw_device_pixel_ratio");
extern fn int rgfwJsInnerWidth() @wasm("env", "rgfw_inner_width");
extern fn int rgfwJsInnerHeight() @wasm("env", "rgfw_inner_height");
extern fn void rgfwJsRequestPointerLock() @wasm("env", "rgfw_request_pointer_lock");
extern fn void rgfwJsExitPointerLock() @wasm("env", "rgfw_exit_pointer_lock");
extern fn void rgfwJsRequestFullscreen() @wasm("env", "rgfw_request_fullscreen");
extern fn void rgfwJsExitFullscreen() @wasm("env", "rgfw_exit_fullscreen");
extern fn void rgfwJsSetOpacity(float opacity) @wasm("env", "rgfw_set_opacity");
extern fn int rgfwJsWebglCreateContext(RgfwGlHints* hints) @wasm("env", "rgfw_webgl_create_context");
extern fn void rgfwJsWebglDestroyContext(int ctx) @wasm("env", "rgfw_webgl_destroy_context");
extern fn void rgfwJsWebglMakeCurrent(int ctx) @wasm("env", "rgfw_webgl_make_current");
extern fn void rgfwJsWebglCommitFrame() @wasm("env", "rgfw_webgl_commit_frame");
extern fn int rgfwJsWebglGetCurrentContext() @wasm("env", "rgfw_webgl_get_current_context");
extern fn int rgfwJsExtensionSupported(char* name, usz len) @wasm("env", "rgfw_extension_supported");
extern fn RgfwProc rgfwJsGetProcAddress(char* name) @wasm("env", "rgfw_webgl_get_proc_address");
extern fn void rgfwJsMkdir(char* name) @wasm("env", "rgfw_mkdir");
extern fn void rgfwJsWriteFile(char* path, char* data, usz len) @wasm("env", "rgfw_write_file");

const int DOM_VK_BACK_QUOTE = 192; const int DOM_VK_0 = 48; const int DOM_VK_1 = 49; const int DOM_VK_2 = 50;
const int DOM_VK_3 = 51; const int DOM_VK_4 = 52; const int DOM_VK_5 = 53; const int DOM_VK_6 = 54;
const int DOM_VK_7 = 55; const int DOM_VK_8 = 56; const int DOM_VK_9 = 57; const int DOM_VK_SPACE = 32;
const int DOM_VK_A = 65; const int DOM_VK_B = 66; const int DOM_VK_C = 67; const int DOM_VK_D = 68;
const int DOM_VK_E = 69; const int DOM_VK_F = 70; const int DOM_VK_G = 71; const int DOM_VK_H = 72;
const int DOM_VK_I = 73; const int DOM_VK_J = 74; const int DOM_VK_K = 75; const int DOM_VK_L = 76;
const int DOM_VK_M = 77; const int DOM_VK_N = 78; const int DOM_VK_O = 79; const int DOM_VK_P = 80;
const int DOM_VK_Q = 81; const int DOM_VK_R = 82; const int DOM_VK_S = 83; const int DOM_VK_T = 84;
const int DOM_VK_U = 85; const int DOM_VK_V = 86; const int DOM_VK_W = 87; const int DOM_VK_X = 88;
const int DOM_VK_Y = 89; const int DOM_VK_Z = 90;
const int DOM_VK_PERIOD = 190; const int DOM_VK_COMMA = 188; const int DOM_VK_SLASH = 191;
const int DOM_VK_OPEN_BRACKET = 219; const int DOM_VK_CLOSE_BRACKET = 221; const int DOM_VK_SEMICOLON = 186;
const int DOM_VK_QUOTE = 222; const int DOM_VK_BACK_SLASH = 220; const int DOM_VK_RETURN = 13;
const int DOM_VK_DELETE = 46; const int DOM_VK_NUM_LOCK = 144; const int DOM_VK_DIVIDE = 111;
const int DOM_VK_MULTIPLY = 106; const int DOM_VK_SUBTRACT = 109;
const int DOM_VK_NUMPAD1 = 97; const int DOM_VK_NUMPAD2 = 98; const int DOM_VK_NUMPAD3 = 99;
const int DOM_VK_NUMPAD4 = 100; const int DOM_VK_NUMPAD5 = 101; const int DOM_VK_NUMPAD6 = 102;
const int DOM_VK_NUMPAD9 = 105; const int DOM_VK_NUMPAD0 = 96; const int DOM_VK_DECIMAL = 110;
const int DOM_VK_HYPHEN_MINUS = 173; const int DOM_VK_EQUALS = 61;
const int DOM_VK_BACK_SPACE = 8; const int DOM_VK_TAB = 9; const int DOM_VK_CAPS_LOCK = 20;
const int DOM_VK_SHIFT = 16; const int DOM_VK_CONTROL = 17; const int DOM_VK_ALT = 18; const int DOM_VK_META = 91;
const int DOM_VK_F1 = 112; const int DOM_VK_F2 = 113; const int DOM_VK_F3 = 114; const int DOM_VK_F4 = 115;
const int DOM_VK_F5 = 116; const int DOM_VK_F6 = 117; const int DOM_VK_F7 = 118; const int DOM_VK_F8 = 119;
const int DOM_VK_F9 = 120; const int DOM_VK_F10 = 121; const int DOM_VK_F11 = 122; const int DOM_VK_F12 = 123;
const int DOM_VK_F13 = 124; const int DOM_VK_F14 = 125; const int DOM_VK_F15 = 126; const int DOM_VK_F16 = 127;
const int DOM_VK_F17 = 128; const int DOM_VK_F18 = 129; const int DOM_VK_F19 = 130; const int DOM_VK_F20 = 131;
const int DOM_VK_F21 = 132; const int DOM_VK_F22 = 133; const int DOM_VK_F23 = 134; const int DOM_VK_F24 = 135;
const int DOM_VK_UP = 38; const int DOM_VK_DOWN = 40; const int DOM_VK_LEFT = 37; const int DOM_VK_RIGHT = 39;
const int DOM_VK_INSERT = 45; const int DOM_VK_END = 35; const int DOM_VK_PAGE_UP = 33; const int DOM_VK_PAGE_DOWN = 34;
const int DOM_VK_ESCAPE = 27; const int DOM_VK_HOME = 36; const int DOM_VK_SCROLL_LOCK = 145;
const int DOM_VK_PRINTSCREEN = 44; const int DOM_VK_PAUSE = 19;

fn RgfwKey rgfw_wasmPhysicalToRGFW(uint hash)
{
	switch (hash)
	{
		case 0x67243A2Du: return RGFW_KEY_ESCAPE;
		case 0x67251058u: return RGFW_KEY0; case 0x67251059u: return RGFW_KEY1; case 0x6725105Au: return RGFW_KEY2;
		case 0x6725105Bu: return RGFW_KEY3; case 0x6725105Cu: return RGFW_KEY4; case 0x6725105Du: return RGFW_KEY5;
		case 0x6725105Eu: return RGFW_KEY6; case 0x6725105Fu: return RGFW_KEY7; case 0x67251050u: return RGFW_KEY8;
		case 0x67251051u: return RGFW_KEY9;
		case 0x92E14DD3u: return RGFW_KEY_MINUS; case 0x92E1FBACu: return RGFW_KEY_EQUALS;
		case 0x36BF1CB5u: return RGFW_KEY_BACK_SPACE; case 0x7B8E51E2u: return RGFW_KEY_TAB;
		case 0x2C595B51u: return RGFW_KEY_Q; case 0x2C595B57u: return RGFW_KEY_W; case 0x2C595B45u: return RGFW_KEY_E;
		case 0x2C595B52u: return RGFW_KEY_R; case 0x2C595B54u: return RGFW_KEY_T; case 0x2C595B59u: return RGFW_KEY_Y;
		case 0x2C595B55u: return RGFW_KEY_U; case 0x2C595B4Fu: return RGFW_KEY_O; case 0x2C595B50u: return RGFW_KEY_P;
		case 0x45D8158Cu: return RGFW_KEY_CLOSE_BRACKET; case 0xDEEABF7Cu: return RGFW_KEY_BRACKET;
		case 0x92E1C5D2u: return RGFW_KEY_RETURN; case 0xE058958Cu: return RGFW_KEY_CONTROL_L;
		case 0x2C595B41u: return RGFW_KEY_A; case 0x2C595B53u: return RGFW_KEY_S; case 0x2C595B44u: return RGFW_KEY_D;
		case 0x2C595B46u: return RGFW_KEY_F; case 0x2C595B47u: return RGFW_KEY_G; case 0x2C595B48u: return RGFW_KEY_H;
		case 0x2C595B4Au: return RGFW_KEY_J; case 0x2C595B4Bu: return RGFW_KEY_K; case 0x2C595B4Cu: return RGFW_KEY_L;
		case 0x2707219Eu: return RGFW_KEY_SEMICOLON; case 0x92E0B58Du: return RGFW_KEY_APOSTROPHE;
		case 0x36BF358Du: return RGFW_KEY_BACKTICK; case 0x26B1958Cu: return RGFW_KEY_SHIFT_L;
		case 0x36BF2438u: return RGFW_KEY_BACK_SLASH;
		case 0x2C595B5Au: return RGFW_KEY_Z; case 0x2C595B58u: return RGFW_KEY_X; case 0x2C595B43u: return RGFW_KEY_C;
		case 0x2C595B56u: return RGFW_KEY_V; case 0x2C595B42u: return RGFW_KEY_B; case 0x2C595B4Eu: return RGFW_KEY_N;
		case 0x2C595B4Du: return RGFW_KEY_M;
		case 0x92E1A1C1u: return RGFW_KEY_COMMA; case 0x672FFAD4u: return RGFW_KEY_PERIOD; case 0x92E0A438u: return RGFW_KEY_SLASH;
		case 0xC5A6BF7Cu: return RGFW_KEY_SHIFT_R; case 0x5D64DA91u: return RGFW_KEY_PAD_MULTIPLY;
		case 0xC914958Cu: return RGFW_KEY_ALT_L; case 0x92E09CB5u: return RGFW_KEY_SPACE;
		case 0xB8FAE73Bu: return RGFW_KEY_CAPS_LOCK;
		case 0x7174B789u: return RGFW_KEY_F1; case 0x7174B78Au: return RGFW_KEY_F2; case 0x7174B78Bu: return RGFW_KEY_F3;
		case 0x7174B78Cu: return RGFW_KEY_F4; case 0x7174B78Du: return RGFW_KEY_F5; case 0x7174B78Eu: return RGFW_KEY_F6;
		case 0x7174B78Fu: return RGFW_KEY_F7; case 0x7174B780u: return RGFW_KEY_F8; case 0x7174B781u: return RGFW_KEY_F9;
		case 0x7B8E57B0u: return RGFW_KEY_F10;
		case 0xC925FCDFu: return RGFW_KEY_PAD_MULTIPLY; case 0xC925FCD0u: return RGFW_KEY_PAD8; case 0xC925FCD1u: return RGFW_KEY_PAD9;
		case 0x5EA3E8A4u: return RGFW_KEY_MINUS;
		case 0xC925FCDCu: return RGFW_KEY_PAD4; case 0xC925FCDDu: return RGFW_KEY_PAD5; case 0xC925FCDEu: return RGFW_KEY_PAD6;
		case 0xC925FCD9u: return RGFW_KEY_PAD1; case 0xC925FCDAu: return RGFW_KEY_PAD2; case 0xC925FCDBu: return RGFW_KEY_PAD3;
		case 0xC925FCD8u: return RGFW_KEY_PAD0; case 0x95852DACu: return RGFW_KEY_PERIOD;
		case 0x7B8E57B1u: return RGFW_KEY_F11; case 0x7B8E57B2u: return RGFW_KEY_F12;
		case 0x7B8E57B3u: return RGFW_KEY_F13; case 0x7B8E57B4u: return RGFW_KEY_F14; case 0x7B8E57B5u: return RGFW_KEY_F15;
		case 0x7B8E57B6u: return RGFW_KEY_F16; case 0x7B8E57B7u: return RGFW_KEY_F17; case 0x7B8E57B8u: return RGFW_KEY_F18;
		case 0x7B8E57B9u: return RGFW_KEY_F19; case 0x7B8E57A8u: return RGFW_KEY_F20; case 0x7B8E57A9u: return RGFW_KEY_F21;
		case 0x7B8E57AAu: return RGFW_KEY_F22; case 0x7B8E57ABu: return RGFW_KEY_F23;
		case 0x7393FBACu: return RGFW_KEY_PAD_RETURN; case 0xB88EBF7Cu: return RGFW_KEY_ALT_R;
		case 0xC925873Bu: return RGFW_KEY_NUM_LOCK; case 0x2C595F45u: return RGFW_KEY_HOME;
		case 0xC91BB690u: return RGFW_KEY_UP; case 0x672F9210u: return RGFW_KEY_PAGE_UP;
		case 0x3799258Cu: return RGFW_KEY_LEFT; case 0x4CE33F7Cu: return RGFW_KEY_RIGHT;
		case 0x7B8E55DCu: return RGFW_KEY_END; case 0x3799379Eu: return RGFW_KEY_DOWN; case 0xBA90179Eu: return RGFW_KEY_PAGE_DOWN;
		case 0x6723CB2Cu: return RGFW_KEY_INSERT; case 0x6725C50Du: return RGFW_KEY_DELETE;
		case 0x6723658Cu: return RGFW_KEY_SUPER_L; case 0x39643F7Cu: return RGFW_KEY_SUPER_R;
		case 0x380B9C8Cu: return RGFW_KEY_PAD_PLUS;
		default: return RGFW_KEY_NULL;
	}
}

fn void rgfwEmscriptenOnResize(int w, int h) @export("rgfw_on_resize")
{
	rgfw_windowResizedCallback(_rgfw.root, w, h);
}

fn void rgfwEmscriptenOnFullscreenChange(int screenW, int screenH, bool entering) @export("rgfw_on_fullscreenchange")
{
	if (!(_rgfw.root.internal.enabledEvents & RGFW_WINDOW_RESIZED_FLAG)) return;
	static int originalW, originalH;
	if (entering)
	{
		originalW = _rgfw.root.w;
		originalH = _rgfw.root.h;
		_rgfw.root.w = screenW;
		_rgfw.root.h = screenH;
	}
	else
	{
		_rgfw.root.w = originalW;
		_rgfw.root.h = originalH;
	}
	rgfwJsSetCanvasSize(_rgfw.root.w, _rgfw.root.h);
	rgfw_windowResizedCallback(_rgfw.root, _rgfw.root.w, _rgfw.root.h);
}

fn void rgfwEmscriptenOnFocusIn() @export("rgfw_on_focusin") { rgfw_windowFocusCallback(_rgfw.root, true); }
fn void rgfwEmscriptenOnFocusOut() @export("rgfw_on_focusout") { rgfw_windowFocusCallback(_rgfw.root, false); }

fn void rgfwEmscriptenOnMouseMove(int x, int y, int movementX, int movementY) @export("rgfw_on_mousemove")
{
	rgfw_mouseMotionCallback(_rgfw.root, x, y);
	rgfw_rawMotionCallback(_rgfw.root, (float)movementX, (float)movementY);
}

fn void rgfwEmscriptenOnMouseDown(int button) @export("rgfw_on_mousedown")
{
	if (button > 2) button += 2;
	rgfw_mouseButtonCallback(_rgfw.root, (RgfwMouseButton)button, true);
}

fn void rgfwEmscriptenOnMouseUp(int button) @export("rgfw_on_mouseup")
{
	if (button > 2) button += 2;
	rgfw_mouseButtonCallback(_rgfw.root, (RgfwMouseButton)button, false);
}

fn void rgfwEmscriptenOnWheel(float deltaX, float deltaY) @export("rgfw_on_wheel") { rgfw_mouseScrollCallback(_rgfw.root, deltaX, deltaY); }

fn void rgfwEmscriptenOnTouch(int numTouches, int* xs, int* ys, char down) @export("rgfw_on_touch")
{
	// down == 255 is the sentinel for touchmove (no button state change).
	if (down == 255)
	{
		if (!(_rgfw.root.internal.enabledEvents & RGFW_MOUSE_MOTION_FLAG)) return;
		for (int i = 0; i < numTouches; i++)
		{
			rgfw_mouseMotionCallback(_rgfw.root, xs[i], ys[i]);
			rgfw_rawMotionCallback(_rgfw.root, 0.0f, 0.0f);
		}
		return;
	}

	if (down)
	{
		if (!(_rgfw.root.internal.enabledEvents & RGFW_MOUSE_BUTTON_PRESSED_FLAG)) return;
	}
	else
	{
		if (!(_rgfw.root.internal.enabledEvents & RGFW_MOUSE_BUTTON_RELEASED_FLAG)) return;
	}

	for (int i = 0; i < numTouches; i++)
	{
		rgfw_mouseMotionCallback(_rgfw.root, xs[i], ys[i]);
		rgfw_rawMotionCallback(_rgfw.root, 0.0f, 0.0f);
		rgfw_mouseButtonCallback(_rgfw.root, RGFW_MOUSE_LEFT, (bool)down);
	}
}

fn void rgfw_handleKeyEvent(char* code, uint codepoint, bool press) @export("rgfw_handleKeyEvent")
{
	char* iCode = code;
	uint hash = 0;
	while (*iCode != 0) { hash = ((hash ^ 0x7E057D79u) << 3) ^ (uint)*iCode; iCode++; }

	RgfwKey physicalKey = rgfw_wasmPhysicalToRGFW(hash);
	rgfw_keyCallback(_rgfw.root, physicalKey, _rgfw.root.internal.mod, rgfw_bool(rgfw_isKeyDown(physicalKey) && press), press);
	if (press) rgfw_keyCharCallback(_rgfw.root, codepoint);
}

fn void rgfw_handleKeyMods(bool capital, bool numlock, bool control, bool alt, bool shift, bool super, bool scroll) @export("rgfw_handleKeyMods")
{
	rgfw_keyUpdateKeyModsEx(_rgfw.root, capital, numlock, control, alt, shift, super, scroll);
}

fn void rgfw_emscriptenOnDrop(char* file, usz size) @export("rgfw_onDrop") { rgfw_dataDropCallback(_rgfw.root, file, size, RGFW_DATA_FILE); }

fn void rgfw_stopCheckEvents() { _rgfw.stopCheckEvents_bool = true; }

fn bool rgfw_createSurfacePtr(char* data, int w, int h, RgfwFormat format, GfxSurface* surface)
{
	surface.data = data;
	surface.w = w;
	surface.h = h;
	surface.format = format;
	return true;
}

fn void rgfw_window_blitSurface(GfxWindow* win, GfxSurface* surface)
{
	int minH = win.h < surface.h ? win.h : surface.h;
	rgfw_copyImageData(surface.data, surface.w, minH, RGFW_FORMAT_RGBA8, surface.data, surface.format, surface.convertFunc);
	rgfwJsBlitSurface(surface.data, surface.w, surface.h, minH);
}

fn void rgfw_surface_freePtr(GfxSurface* surface) {}

fn void rgfw_initKeycodesPlatform()
{
	_rgfw.keycodes[DOM_VK_BACK_QUOTE] = RGFW_KEY_BACKTICK;
	_rgfw.keycodes[DOM_VK_0] = RGFW_KEY0; _rgfw.keycodes[DOM_VK_1] = RGFW_KEY1; _rgfw.keycodes[DOM_VK_2] = RGFW_KEY2;
	_rgfw.keycodes[DOM_VK_3] = RGFW_KEY3; _rgfw.keycodes[DOM_VK_4] = RGFW_KEY4; _rgfw.keycodes[DOM_VK_5] = RGFW_KEY5;
	_rgfw.keycodes[DOM_VK_6] = RGFW_KEY6; _rgfw.keycodes[DOM_VK_7] = RGFW_KEY7; _rgfw.keycodes[DOM_VK_8] = RGFW_KEY8;
	_rgfw.keycodes[DOM_VK_9] = RGFW_KEY9; _rgfw.keycodes[DOM_VK_SPACE] = RGFW_KEY_SPACE;
	_rgfw.keycodes[DOM_VK_A] = RGFW_KEY_A; _rgfw.keycodes[DOM_VK_B] = RGFW_KEY_B; _rgfw.keycodes[DOM_VK_C] = RGFW_KEY_C;
	_rgfw.keycodes[DOM_VK_D] = RGFW_KEY_D; _rgfw.keycodes[DOM_VK_E] = RGFW_KEY_E; _rgfw.keycodes[DOM_VK_F] = RGFW_KEY_F;
	_rgfw.keycodes[DOM_VK_G] = RGFW_KEY_G; _rgfw.keycodes[DOM_VK_H] = RGFW_KEY_H; _rgfw.keycodes[DOM_VK_I] = RGFW_KEY_I;
	_rgfw.keycodes[DOM_VK_J] = RGFW_KEY_J; _rgfw.keycodes[DOM_VK_K] = RGFW_KEY_K; _rgfw.keycodes[DOM_VK_L] = RGFW_KEY_L;
	_rgfw.keycodes[DOM_VK_M] = RGFW_KEY_M; _rgfw.keycodes[DOM_VK_N] = RGFW_KEY_N; _rgfw.keycodes[DOM_VK_O] = RGFW_KEY_O;
	_rgfw.keycodes[DOM_VK_P] = RGFW_KEY_P; _rgfw.keycodes[DOM_VK_Q] = RGFW_KEY_Q; _rgfw.keycodes[DOM_VK_R] = RGFW_KEY_R;
	_rgfw.keycodes[DOM_VK_S] = RGFW_KEY_S; _rgfw.keycodes[DOM_VK_T] = RGFW_KEY_T; _rgfw.keycodes[DOM_VK_U] = RGFW_KEY_U;
	_rgfw.keycodes[DOM_VK_V] = RGFW_KEY_V; _rgfw.keycodes[DOM_VK_W] = RGFW_KEY_W; _rgfw.keycodes[DOM_VK_X] = RGFW_KEY_X;
	_rgfw.keycodes[DOM_VK_Y] = RGFW_KEY_Y; _rgfw.keycodes[DOM_VK_Z] = RGFW_KEY_Z;
	_rgfw.keycodes[DOM_VK_PERIOD] = RGFW_KEY_PERIOD; _rgfw.keycodes[DOM_VK_COMMA] = RGFW_KEY_COMMA;
	_rgfw.keycodes[DOM_VK_SLASH] = RGFW_KEY_SLASH; _rgfw.keycodes[DOM_VK_OPEN_BRACKET] = RGFW_KEY_BRACKET;
	_rgfw.keycodes[DOM_VK_CLOSE_BRACKET] = RGFW_KEY_CLOSE_BRACKET; _rgfw.keycodes[DOM_VK_SEMICOLON] = RGFW_KEY_SEMICOLON;
	_rgfw.keycodes[DOM_VK_QUOTE] = RGFW_KEY_APOSTROPHE; _rgfw.keycodes[DOM_VK_BACK_SLASH] = RGFW_KEY_BACK_SLASH;
	_rgfw.keycodes[DOM_VK_RETURN] = RGFW_KEY_RETURN; _rgfw.keycodes[DOM_VK_DELETE] = RGFW_KEY_DELETE;
	_rgfw.keycodes[DOM_VK_NUM_LOCK] = RGFW_KEY_NUM_LOCK; _rgfw.keycodes[DOM_VK_DIVIDE] = RGFW_KEY_PAD_SLASH;
	_rgfw.keycodes[DOM_VK_MULTIPLY] = RGFW_KEY_PAD_MULTIPLY; _rgfw.keycodes[DOM_VK_SUBTRACT] = RGFW_KEY_PAD_MINUS;
	_rgfw.keycodes[DOM_VK_NUMPAD1] = RGFW_KEY_PAD1; _rgfw.keycodes[DOM_VK_NUMPAD2] = RGFW_KEY_PAD2;
	_rgfw.keycodes[DOM_VK_NUMPAD3] = RGFW_KEY_PAD3; _rgfw.keycodes[DOM_VK_NUMPAD4] = RGFW_KEY_PAD4;
	_rgfw.keycodes[DOM_VK_NUMPAD5] = RGFW_KEY_PAD5; _rgfw.keycodes[DOM_VK_NUMPAD6] = RGFW_KEY_PAD6;
	_rgfw.keycodes[DOM_VK_NUMPAD9] = RGFW_KEY_PAD9; _rgfw.keycodes[DOM_VK_NUMPAD0] = RGFW_KEY_PAD0;
	_rgfw.keycodes[DOM_VK_DECIMAL] = RGFW_KEY_PAD_PERIOD; _rgfw.keycodes[DOM_VK_HYPHEN_MINUS] = RGFW_KEY_MINUS;
	_rgfw.keycodes[DOM_VK_EQUALS] = RGFW_KEY_EQUALS; _rgfw.keycodes[DOM_VK_BACK_SPACE] = RGFW_KEY_BACK_SPACE;
	_rgfw.keycodes[DOM_VK_TAB] = RGFW_KEY_TAB; _rgfw.keycodes[DOM_VK_CAPS_LOCK] = RGFW_KEY_CAPS_LOCK;
	_rgfw.keycodes[DOM_VK_SHIFT] = RGFW_KEY_SHIFT_L; _rgfw.keycodes[DOM_VK_CONTROL] = RGFW_KEY_CONTROL_L;
	_rgfw.keycodes[DOM_VK_ALT] = RGFW_KEY_ALT_L; _rgfw.keycodes[DOM_VK_META] = RGFW_KEY_SUPER_L;
	_rgfw.keycodes[DOM_VK_F1] = RGFW_KEY_F1; _rgfw.keycodes[DOM_VK_F2] = RGFW_KEY_F2; _rgfw.keycodes[DOM_VK_F3] = RGFW_KEY_F3;
	_rgfw.keycodes[DOM_VK_F4] = RGFW_KEY_F4; _rgfw.keycodes[DOM_VK_F5] = RGFW_KEY_F5; _rgfw.keycodes[DOM_VK_F6] = RGFW_KEY_F6;
	_rgfw.keycodes[DOM_VK_F7] = RGFW_KEY_F7; _rgfw.keycodes[DOM_VK_F8] = RGFW_KEY_F8; _rgfw.keycodes[DOM_VK_F9] = RGFW_KEY_F9;
	_rgfw.keycodes[DOM_VK_F10] = RGFW_KEY_F10; _rgfw.keycodes[DOM_VK_F11] = RGFW_KEY_F11; _rgfw.keycodes[DOM_VK_F12] = RGFW_KEY_F12;
	_rgfw.keycodes[DOM_VK_UP] = RGFW_KEY_UP; _rgfw.keycodes[DOM_VK_DOWN] = RGFW_KEY_DOWN;
	_rgfw.keycodes[DOM_VK_LEFT] = RGFW_KEY_LEFT; _rgfw.keycodes[DOM_VK_RIGHT] = RGFW_KEY_RIGHT;
	_rgfw.keycodes[DOM_VK_INSERT] = RGFW_KEY_INSERT; _rgfw.keycodes[DOM_VK_END] = RGFW_KEY_END;
	_rgfw.keycodes[DOM_VK_PAGE_UP] = RGFW_KEY_PAGE_UP; _rgfw.keycodes[DOM_VK_PAGE_DOWN] = RGFW_KEY_PAGE_DOWN;
	_rgfw.keycodes[DOM_VK_ESCAPE] = RGFW_KEY_ESCAPE; _rgfw.keycodes[DOM_VK_HOME] = RGFW_KEY_HOME;
	_rgfw.keycodes[DOM_VK_SCROLL_LOCK] = RGFW_KEY_SCROLL_LOCK; _rgfw.keycodes[DOM_VK_PRINTSCREEN] = RGFW_KEY_PRINT_SCREEN;
	_rgfw.keycodes[DOM_VK_PAUSE] = RGFW_KEY_PAUSE;
	_rgfw.keycodes[DOM_VK_F13] = RGFW_KEY_F13; _rgfw.keycodes[DOM_VK_F14] = RGFW_KEY_F14; _rgfw.keycodes[DOM_VK_F15] = RGFW_KEY_F15;
	_rgfw.keycodes[DOM_VK_F16] = RGFW_KEY_F16; _rgfw.keycodes[DOM_VK_F17] = RGFW_KEY_F17; _rgfw.keycodes[DOM_VK_F18] = RGFW_KEY_F18;
	_rgfw.keycodes[DOM_VK_F19] = RGFW_KEY_F19; _rgfw.keycodes[DOM_VK_F20] = RGFW_KEY_F20; _rgfw.keycodes[DOM_VK_F21] = RGFW_KEY_F21;
	_rgfw.keycodes[DOM_VK_F22] = RGFW_KEY_F22; _rgfw.keycodes[DOM_VK_F23] = RGFW_KEY_F23; _rgfw.keycodes[DOM_VK_F24] = RGFW_KEY_F24;
}

fn int rgfw_initPlatform(char* className, RgfwInitFlags flags)
{
	_rgfw.usingAsyncify = -1;
	GfxMonitor monitor;
	rgfw_memzero(&monitor, GfxMonitor::size);
	monitor.x = 0;
	monitor.y = 0;
	monitor.pixelRatio = (float)rgfwJsDevicePixelRatio();
	monitor.mode.w = rgfwJsInnerWidth();
	monitor.mode.h = rgfwJsInnerHeight();
	monitor.physW = (float)(int)(monitor.mode.w * monitor.pixelRatio + 0.5f);
	monitor.physH = (float)(int)(monitor.mode.h * monitor.pixelRatio + 0.5f);

	float dpi = 96.0f * monitor.pixelRatio;
	monitor.scaleX = dpi / 96.0f;
	monitor.scaleY = dpi / 96.0f;
	rgfw_splitBPP(32, &monitor.mode);
	monitor.mode.refreshRate = 0;

	RgfwMonitorNode* node = rgfw_monitors_add(&monitor);
	if (node != null)
	{
		_rgfw.monitors.primary = node;
		rgfw_monitorCallback(_rgfw.root, &node.mon, true);
	}
	return 0;
}

fn GfxWindow* rgfw_createWindowPlatform(char* name, RgfwWindowFlags flags, GfxWindow* win)
{
	rgfwJsSetCanvasSize(win.w, win.h);
	rgfwJsSetWindowTitle(name, strlen(name));
	rgfwJsSetupWindowListeners();

	if (flags & RGFW_WINDOW_ALLOW_DND) win.internal.flags |= RGFW_WINDOW_ALLOW_DND;

	rgfwJsSetupDropListeners();
	return win;
}

fn RgfwKey rgfw_physicalToMappedKey(RgfwKey key) => key;
fn bool rgfw_window_fetchSize(GfxWindow* win, int* w, int* h) => rgfw_window_getSize(win, w, h);

fn void rgfw_pollEvents()
{
	if (_rgfw.usingAsyncify == -1) _rgfw.usingAsyncify = rgfwJsUsingAsyncify();
	rgfw_resetPrevState();
	if (_rgfw.usingAsyncify) rgfwJsSleep(0);
}

fn void rgfw_window_resizePlatform(GfxWindow* win, int w, int h) { rgfwJsSetCanvasSize(w, h); }

fn RgfwMouse* rgfw_createMouseStandard(RgfwMouseIcon mouse)
{
	char* cursorName;
	switch (mouse)
	{
		case RGFW_MOUSE_NORMAL: cursorName = "default"; case RGFW_MOUSE_ARROW: cursorName = "default";
		case RGFW_MOUSE_IBEAM: cursorName = "text";
		case RGFW_MOUSE_CROSSHAIR: cursorName = "crosshair";
		case RGFW_MOUSE_POINTING_HAND: cursorName = "pointer";
		case RGFW_MOUSE_RESIZE_EW: cursorName = "ew-resize";
		case RGFW_MOUSE_RESIZE_NS: cursorName = "ns-resize";
		case RGFW_MOUSE_RESIZE_NWSE: cursorName = "nwse-resize";
		case RGFW_MOUSE_RESIZE_NESW: cursorName = "nesw-resize";
		case RGFW_MOUSE_RESIZE_NW: cursorName = "nw-resize";
		case RGFW_MOUSE_RESIZE_N: cursorName = "n-resize";
		case RGFW_MOUSE_RESIZE_NE: cursorName = "ne-resize";
		case RGFW_MOUSE_RESIZE_E: cursorName = "e-resize";
		case RGFW_MOUSE_RESIZE_SE: cursorName = "se-resize";
		case RGFW_MOUSE_RESIZE_S: cursorName = "s-resize";
		case RGFW_MOUSE_RESIZE_SW: cursorName = "sw-resize";
		case RGFW_MOUSE_RESIZE_W: cursorName = "w-resize";
		case RGFW_MOUSE_RESIZE_ALL: cursorName = "move";
		case RGFW_MOUSE_NOT_ALLOWED: cursorName = "not-allowed";
		case RGFW_MOUSE_WAIT: cursorName = "wait";
		case RGFW_MOUSE_PROGRESS: cursorName = "progress";
		default: return null;
	}
	return (RgfwMouse*)cursorName;
}

fn void rgfw_window_moveMouse(GfxWindow* win, int x, int y) {}
fn RgfwMouse* rgfw_createMouse(char* data, int w, int h, RgfwFormat format) => null;

fn bool rgfw_window_setMousePlatform(GfxWindow* win, RgfwMouse* mouse)
{
	char* name = (char*)mouse;
	rgfwJsSetCursor(name, strlen(name));
	return true;
}

fn void rgfw_freeMouse(RgfwMouse* mouse) {}

fn void rgfw_window_showMouse(GfxWindow* win, bool show)
{
	rgfw_window_showMouseFlags(win, show);
	if (show) { rgfw_window_setMouseDefault(win); }
	else { rgfwJsHideCursor(); }
}

fn bool rgfw_getGlobalMouse(int* x, int* y)
{
	if (x != null) *x = rgfwJsGetMouseX();
	if (y != null) *y = rgfwJsGetMouseY();
	return true;
}

fn void rgfw_window_setMousePassthrough(GfxWindow* win, char passthrough) { rgfwJsSetPointerEvents(passthrough); }

fn bool rgfw_writeClipboard(GfxDataTransfer* data)
{
	rgfwJsWriteClipboard(data.data, data.length);
	return true;
}

fn bool rgfw_readClipboardPtr(RgfwDataTransferType requestedType, char* buffer, usz capacity, GfxDataTransfer* data)
{
	if (requestedType != RGFW_DATA_TEXT) return false;
	return false;
}

fn bool rgfw_window_createContextPtr_OpenGL(GfxWindow* win, RgfwGlContext* ctx, RgfwGlHints* hints)
{
	win.src.ctx.native = ctx;
	win.src.gfxType = RGFW_GFX_NATIVE_OPEN_GL;

	int handle = rgfwJsWebglCreateContext(hints);
	if (handle == 0)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_OPEN_GL_CONTEXT, "Failed to create an OpenGL Context.");
		return false;
	}
	ctx.ctx = (void*)(uptr)handle;
	rgfwJsWebglMakeCurrent(handle);
	rgfw_window_swapInterval_OpenGL(win, 0);
	return true;
}

fn void rgfw_window_deleteContextPtr_OpenGL(GfxWindow* win, RgfwGlContext* ctx)
{
	rgfwJsWebglDestroyContext((int)(uptr)ctx.ctx);
	win.src.ctx.native.ctx = null;
	rgfw_debugCallback(RGFW_TYPE_INFO, RGFW_INFO_OPEN_GL, "OpenGL context freed.");
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

fn void rgfw_window_deleteContext_OpenGL(GfxWindow* win, RgfwGlContext* ctx)
{
	rgfw_window_deleteContextPtr_OpenGL(win, ctx);
	if (win != null && (win.src.gfxType & RGFW_GFX_OWNED_BY_RGFW)) rgfw_free_impl(ctx);
}

fn void rgfw_window_makeCurrentContext_OpenGL(GfxWindow* win)
{
	if (win == null) { rgfwJsWebglMakeCurrent(0); }
	else { rgfwJsWebglMakeCurrent((int)(uptr)win.src.ctx.native.ctx); }
}

fn void rgfw_window_swapBuffers_OpenGL(GfxWindow* win) { rgfwJsWebglCommitFrame(); }
fn void* rgfw_getCurrentContext_OpenGL() => (void*)(uptr)rgfwJsWebglGetCurrentContext();
fn bool rgfw_extensionSupportedPlatform_OpenGL(char* extension, usz len) => rgfw_bool(rgfwJsExtensionSupported(extension, len) != 0);
fn RgfwProc rgfw_getProcAddress_OpenGL(char* procname) => rgfwJsGetProcAddress(procname);
fn void rgfw_window_swapInterval_OpenGL(GfxWindow* win, int swapInterval) {}

fn void rgfw_deinitPlatform() {}
fn void rgfw_window_closePlatform(GfxWindow* win) {}

fn void rgfw_window_setRawMouseModePlatform(GfxWindow* win, bool state) {}
fn void rgfw_window_captureMousePlatform(GfxWindow* win, bool state)
{
	if (state) { rgfwJsRequestPointerLock(); }
	else { rgfwJsExitPointerLock(); }
}

fn void rgfw_window_setName(GfxWindow* win, char* name)
{
	if (name == null) name = "";
	rgfwJsSetWindowTitle(name, strlen(name));
}

fn void rgfw_window_maximizePlatform(GfxWindow* win)
{
	GfxMonitor* mon = rgfw_window_getMonitor(win);
	if (mon != null) rgfw_window_resize(win, mon.mode.w, mon.mode.h);
	rgfw_window_move(win, 0, 0);
}

fn void rgfw_window_setFullscreen(GfxWindow* win, bool fullscreen)
{
	if (fullscreen)
	{
		win.internal.flags |= RGFW_WINDOW_FULLSCREEN;
		rgfwJsRequestFullscreen();
		return;
	}
	win.internal.flags &= ~(uint)RGFW_WINDOW_FULLSCREEN;
	rgfwJsExitFullscreen();
}

fn void rgfw_window_setOpacity(GfxWindow* win, char opacity) { rgfwJsSetOpacity((float)opacity / 255.0f); }

fn GfxMonitor* rgfw_window_getMonitor(GfxWindow* win) => rgfw_getPrimaryMonitor();

// ---- unsupported on WASM (match the original's no-op stubs) ----
fn void rgfw_pollMonitors() {}
fn void rgfw_monitorNode_free(RgfwMonitorNode* node) {}
fn void rgfw_window_focus(GfxWindow* win) {}
fn void rgfw_window_raise(GfxWindow* win) {}
fn bool rgfw_monitor_requestMode(GfxMonitor* mon, RgfwMonitorMode* mode, RgfwModeRequest request) => false;
fn bool rgfw_monitor_getWorkarea(GfxMonitor* monitor, int* x, int* y, int* width, int* height) => false;
fn usz rgfw_monitor_getGammaRampPtr(GfxMonitor* monitor, RgfwGammaRamp* ramp) => 0;
fn bool rgfw_monitor_setGammaRamp(GfxMonitor* monitor, RgfwGammaRamp* ramp) => false;
fn usz rgfw_monitor_getModesPtr(GfxMonitor* mon, RgfwMonitorMode** modes) => 0;
fn bool rgfw_monitor_setMode(GfxMonitor* mon, RgfwMonitorMode* mode) => false;
fn void rgfw_window_movePlatform(GfxWindow* win, int x, int y) {}
fn void rgfw_window_setMinSize(GfxWindow* win, int w, int h) {}
fn void rgfw_window_setMaxSize(GfxWindow* win, int w, int h) {}
fn void rgfw_window_minimizePlatform(GfxWindow* win) {}
fn void rgfw_window_restorePlatform(GfxWindow* win) {}
fn void rgfw_window_setFloating(GfxWindow* win, bool floating) {}
fn void rgfw_window_setBorder(GfxWindow* win, bool border) {}
fn bool GfxWindow.set_icon(&win, char* data, int w, int h, RgfwFormat format, GfxWindowIconType type = BOTH) => false;
fn void rgfw_window_hide(GfxWindow* win) {}
fn void rgfw_window_showPlatform(GfxWindow* win) {}
fn void rgfw_window_flash(GfxWindow* win, GfxFlashRequest request) {}
fn bool rgfw_window_isHidden(GfxWindow* win) => false;
fn bool rgfw_window_isMinimized(GfxWindow* win) => false;
fn bool rgfw_window_isMaximized(GfxWindow* win) => false;
fn bool GfxWindow.is_floating(&win) => false;
fn void rgfw_waitForEvent(int waitMS) {}

fn bool rgfw_loadEGL() => false;
fn void rgfw_unloadEGL() {}
fn bool rgfw_loadVulkan() => false;
fn void rgfw_unloadVulkan() {}
