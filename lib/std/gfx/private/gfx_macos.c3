module std::gfx @feat(MACOS) @private @link("Cocoa.framework", "CoreGraphics.framework", "ApplicationServices.framework", "QuartzCore.framework", "Carbon.framework", "CoreFoundation.framework", "objc");
import std::os::macos, std::math, std::os::darwin, std::collections;

struct GfxGlContext
{
	void* ctx;
	void* format;
}

struct GfxEglContext
{
	void* ctx;
	void* surface;
	void* egl_window;
}

struct GfxNativeImage
{
	GfxFormat format;
	char* buffer;
	void* native0;
	void* native1;
	void* native2;
}

struct GfxWindowSrc
{
	inline IntRect bounds;
	void* window;
	void* view;
	void* mouse;
	void* delegate;
	GfxBackingContext ctx;
	TodoRenameGfxContextType gfxType;
}

fn void? _load_opengl_native()
{
	CFStringRef bundleName = cfStringCreateWithCString(null, "com.apple.opengl", 0x0600);
	defer bundleName.release();
	_gfx_ctx.nativeGL_handle = cfBundleGetBundleWithIdentifier(bundleName);
	if (!_gfx_ctx.nativeGL_handle) return MISSING_OPENGL_LIB~;
	_gfx_ctx.glGetProcAddress = (void*)&_get_opengl_proc_native;
}

fn void? _window_init_opengl_context_native(GfxWindow* win, GfxGlContext* ctx, GfxGlHints* hints)
{
	win.src.ctx.native = ctx;
	win.src.gfxType = TODO_RENAME_GFX_NATIVE_OPEN_GL;

	FixedList{int, 40} stack;
	int colorBits = (hints.red + hints.green + hints.blue + hints.alpha) / 4;
	stack.push_all({
		NS_OPEN_GL_PFA_COLOR_SIZE, colorBits,
		NS_OPEN_GL_PFA_ALPHA_SIZE, hints.alpha,
		NS_OPEN_GL_PFA_DEPTH_SIZE, hints.depth,
		NS_OPEN_GL_PFA_STENCIL_SIZE, hints.stencil,
		NS_OPEN_GL_PFA_AUX_BUFFERS, hints.auxBuffers,
		NS_OPEN_GL_PFA_CLOSEST_POLICY});
	if (hints.samples)
	{
		stack.push_all({ NS_OPEN_GL_PFA_SAMPLE_BUFFERS, 1 , NS_OPEN_GL_PFA_SAMPLES, hints.samples });
	}
	else
	{
		stack.push_all({ NS_OPEN_GL_PFA_SAMPLE_BUFFERS, 0 });
	}
	if (hints.doubleBuffer) stack.push(NS_OPEN_GL_PFA_DOUBLE_BUFFER);
	stack.push(NS_OPEN_GL_PFA_OPEN_GL_PROFILE);
	stack.push(hints.major >= 4 ? NS_OPEN_GL_PROFILE_4_1_CORE : (hints.major >= 3 ? NS_OPEN_GL_PROFILE_3_2_CORE : NS_OPEN_GL_PROFILE_LEGACY));
	if (hints.major <= 2)
	{
		int accumSize = (hints.accumRed + hints.accumGreen + hints.accumBlue + hints.accumAlpha) / 4;
		stack.push_all({ NS_OPEN_GL_PFA_ACCUM_SIZE, accumSize });
	}
	if (hints.renderer == TODO_RENAME_GL_SOFTWARE)
	{
		stack.push_all({ NS_OPEN_GL_PFA_RENDERER_ID, K_CGL_RENDERER_GENERIC_FLOAT_ID });
	}
	else
	{
		stack.push(NS_OPEN_GL_PFA_ACCELERATED);
	}
	stack.push_all({0, 0});

	ObjcId format = objc::send(ns_alloc("NSOpenGLPixelFormat"), ObjcId, "initWithAttributes:", stack.array_view());
	if (!format) return OPENGL_PIXEL_FORMAT_LOAD_FAILED~;
	defer catch objc::release(format);

	ObjcId share = hints.share ? hints.share.ctx : null;
	ctx.ctx = objc::send(ns_alloc("NSOpenGLContext"), ObjcId, "initWithFormat:shareContext:", format, share);
	if (!ctx.ctx) return OPENGL_CTX_CREATION_FAILED~;
	ctx.format = format;

	objc::send_void(ctx.ctx, "setView:", win.src.view);
	if (win.internal.flags.transparent)
	{
		int opacity = 0;
		nsOpenGLContext_setValues((ObjcId)ctx.ctx, &opacity, NS_OPEN_GL_CP_SURFACE_OPACITY);
	}
	todo_fix_window_makeCurrentContext_OpenGL(win);
	win.opengl_swap_interval(0);
}

fn void _window_release_opengl_context_native(GfxWindow* win, GfxGlContext* ctx)
{
	objc::release(ctx.format);
	objc::release(ctx.ctx);
	ctx.format = null;
	ctx.ctx = null;
	win.src.ctx.native = null;
}


fn void? _window_init_egl_context_native(GfxWindow* win, GfxEglContext* ctx, GfxGlHints* hints)
{
	_load_egl_native()!;
	if (_gfx_ctx.egl_display == null)
	{
		_gfx_ctx.egl_display = egl_get_display_fn(null);
		if (!_gfx_ctx.egl_display) return EGL_DISPLAY_NOT_FOUND~;
		EglInt major, minor;
		if (!egl_initialize_fn(_gfx_ctx.egl_display, &major, &minor)) return EGL_DISPLAY_FAILED_TO_INITIALIZE~;
	}

	EglInt[32] cfg;
	FixedList{EglInt, 32} stack;
	stack.push_all({
		EGL_SURFACE_TYPE, EGL_WINDOW_BIT,
		EGL_RED_SIZE, hints.red,
		EGL_GREEN_SIZE, hints.green,
		EGL_BLUE_SIZE, hints.blue,
		EGL_ALPHA_SIZE, hints.alpha,
		EGL_DEPTH_SIZE, hints.depth,
		EGL_STENCIL_SIZE, hints.stencil,
		EGL_RENDERABLE_TYPE, hints.profile == TODO_RENAME_GL_ES ? EGL_OPENGL_ES2_BIT : EGL_OPENGL_BIT,
		EGL_NONE, EGL_NONE
	});

	EglConfig config = null;
	EglInt num = 0;
	if (!egl_choose_config_fn((EglDisplay)_gfx_ctx.egl_display, &cfg, &config, 1, &num) || !num) return EGL_CONFIG_NOT_FOUND~;
	egl_bind_api_fn(hints.profile == TODO_RENAME_GL_ES ? EGL_OPENGL_ES_API : EGL_OPENGL_API);

	stack.clear();
	if (hints.profile == TODO_RENAME_GL_ES) stack.push_all({EGL_CONTEXT_CLIENT_VERSION, hints.major != 0 ? hints.major : 2});
	stack.push_all({EGL_NONE, EGL_NONE});

	EglContext share = hints.shareEGL ? hints.shareEGL.ctx : null;
	ctx.ctx = egl_create_context_fn(_gfx_ctx.egl_display, config, share, stack.array_view());
	if (!ctx.ctx) return EGL_CTX_CREATION_FAILED~;
	defer catch
	{
		egl_destroy_context_fn(_gfx_ctx.egl_display, ctx.ctx);
		ctx.ctx = null;
	}

	EglNativeWindowType nativeWindow = win.src.window;
	if (win.src.view) nativeWindow = todo_fix_getLayer_OSX();
	ctx.surface = egl_create_window_surface((EglDisplay)_gfx_ctx.egl_display, config, nativeWindow, null);
	if (!ctx.surface) return EGL_SURFACE_CREATION_FAILED~;

	win.src.ctx.egl = ctx;
	win.src.gfxType = TODO_RENAME_GFX_EGL;
	todo_fix_window_makeCurrentContext_EGL(win);
}

// public
fn void _window_release_egl_context_native(GfxWindow* win, GfxEglContext* ctx)
{
	if (ctx.surface) egl_destroy_surface_fn(_gfx_ctx.egl_display, ctx.surface);
	if (ctx.ctx) egl_destroy_context_fn(_gfx_ctx.egl_display, ctx.ctx);
	ctx.surface = null;
	ctx.ctx = null;
	win.src.ctx.egl = null;
}

fn bool _vulkan_get_physical_device_presentation_support_native(VkInstance instance, VkPhysicalDevice physical_device, uint queue_family_index)
{
	return false;
}

fn VkSurfaceKHR? _window_create_vulkan_surface_native(GfxWindow* win, VkInstance instance) @private
{
	VkSurfaceKHR surface;
	VkCreateSurfaceKHRProc raw = (VkCreateSurfaceKHRProc)todo_fix_getInstanceProcAddress_Vulkan(instance, "vkCreateMetalSurfaceEXT");
	if (!raw) return VULKAN_NOT_LOADED~;
	ObjcId layer = objc::send(objc::@class("CAMetalLayer"), ObjcId, "layer");
	if (!layer) return SURFACE_CREATION_FAILED~;
	objc::send_void(win.src.view, "setWantsLayer:", true);
	todo_fix_window_setLayer_OSX(win, layer);
	VkMetalSurfaceCreateInfoEXT create_info;

	create_info.sType = VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT;
	create_info.pNext = null;
	create_info.flags = 0;
	create_info.pLayer = layer;
	if (raw(instance, &create_info, null, &surface)) return SURFACE_CREATION_FAILED~;
	return surface;
}

fn void _window_set_capture_mouse_native(GfxWindow* win, bool state)
{
	cg::associateMouseAndMouseCursorPosition(!state);
}

fn void _window_maximize_native(GfxWindow* win)
{
	if (win.src.window) objc::send_void(win.src.window, "zoom:", null);
}

fn void _window_move_mouse(GfxWindow* win, GfxPoint where)
{
	cg::warpMouseCursorPosition({ ...where });
	if (win) win.internal.last_mouse = where;
}

fn void _window_hide_native(GfxWindow* win)
{
	objc::send_void(win.src.window, "orderOut:", null);
	win.internal.flags.hide = true;
}

fn bool _window_is_minimized_native(GfxWindow* win)
{
	if (win.src.window) return objc::send(win.src.window, bool, "isMiniaturized");
	return win.internal.flags.minimize;
}

fn void _window_flash_native(GfxWindow* win, GfxFlashRequest request)
{
	objc::send(_gfx_ctx.ns_app, NSInteger, "requestUserAttention:", (NSUInteger)request);
}

fn void _window_blit_surface_native(GfxWindow* win, GfxSurface* surface)
{
	if (!surface.data) return;
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
	ObjcId layer = objc::send(win.src.view, ObjcId, "layer");
	if (layer) objc::send_void(layer, "setContents:", image);
	objc::release(image);
}


fn void _window_set_raw_mouse_mode_native(GfxWindow* win, bool state)
{
	_gfx_ctx.raw_mouse = state;
}

fn bool _window_is_floating_native(GfxWindow* win)
{
	if (!win) return false;
	if (win.src.window) return objc::send(win.src.window, NSInteger, "level") > cocoa::NS_WINDOW_LEVEL_NORMAL;
	return win.internal.flags.floating;
}

fn void _window_set_border_native(GfxWindow* win, bool border)
{
	if (!win.src.window) return;
	CGRect frame = objc::send(win.src.window, CGRect, "frame");
	CGRect content = objc::send(win.src.view, CGRect, "frame");
	double offset = 0;

	win.internal.flags.no_border = !border;
	NSUInteger styleMask = cocoa::NS_WINDOW_STYLE_BORDERLESS | cocoa::NS_WINDOW_STYLE_FULL_SIZE_CONTENT_VIEW;
	if (border) styleMask = cocoa::NS_WINDOW_STYLE_TITLED | cocoa::NS_WINDOW_STYLE_CLOSABLE | cocoa::NS_WINDOW_STYLE_MINIATURIZABLE;
	if (!win.internal.flags.no_resize) styleMask |= cocoa::NS_WINDOW_STYLE_RESIZABLE;

	objc::send_void(win.src.window, "setStyleMask:", styleMask);

	if (!border)
	{
		ObjcId miniaturizeButton = objc::send(win.src.window, ObjcId, "standardWindowButton:", cocoa::NS_WINDOW_MINIATURIZE_BUTTON);
		ObjcId titleBarView = objc::send(miniaturizeButton, ObjcId, "superview");
		objc::send_void(titleBarView, "setHidden:", true);
		offset = (double)(frame.size.height - content.size.height);
	}

	win.resize(win.size + { 0, (int)offset });
	win.height -= (int)offset;
}

fn GfxPoint _window_fetch_size_native(GfxWindow* win)
{
	if (win.src.window)
	{
		CGRect frame = objc::send(win.src.window, CGRect, "frame");
		win.size = { (int)frame.size.width, (int)frame.size.height };
	}
	return win.size;
}

fn void _window_set_fullscreen_native(GfxWindow* win, bool fullscreen)
{
	if (win.src.window) objc::send_void(win.src.window, "toggleFullScreen:", null);
	win.internal.flags.fullscreen = fullscreen;
}

fn void _window_focus_native(GfxWindow* win)
{
	if (win.src.window) objc::send_void(win.src.window, "makeKeyWindow", null);
	win.internal.inFocus = true;
}

fn void _window_resize_native(GfxWindow* win, GfxSize size)
{
	if (!win.src.window) return;
	CGRect frame = objc::send(win.src.window, CGRect, "frame");
	CGRect content = objc::send(win.src.window, CGRect, "contentRectForFrameRect:", frame);
	double offset = frame.size.height - content.size.height;
	objc::send_void((ObjcId)win.src.window, "setFrame:display:", ns_rect((double)win.x, (double)win.y, (double)size.x, (double)size.y + offset), true);
}

fn void _window_move_native(GfxWindow* win, GfxPoint where)
{
	if (!win.src.window) return;
	CGRect content = objc::send((ObjcId)win.src.view, CGRect, "frame");
	win.x = where.x;
	win.y = (int)todo_fix_cocoaYTransform((float)where.y + (float)content.size.height - 1.0f);
	CGPoint origin = { ...where };
	objc::send_void(win.src.window, "setFrameOrigin:", origin);
}

fn void _window_set_min_size_native(GfxWindow* win, GfxSize size)
{
	objc::send_void(win.src.window, "setMinSize:", (CGSize) { ...size });
}

fn bool _window_set_icon_native(GfxWindow* win, void* data, int w, int h, GfxFormat format, GfxWindowIconType type)
{
	if (!_gfx_ctx.ns_app) return false;
	if (!data)
	{
		objc::send_void(_gfx_ctx.ns_app, "setApplicationIconImage:", null);
		return true;
	}
	ObjcId rep = ns_bitmapRep(w, h);
	defer objc::release(rep);
	char* pixels = ns_bitmapData(rep);
	if (!pixels) return false;
	copy_image_data(pixels, w, h, RGBA8, data, format, null);
	ObjcId image = ns_imageWithRep(rep, w, h);
	defer objc::release(image);
	objc::send_void(_gfx_ctx.ns_app, "setApplicationIconImage:", image);
	return true;
}

fn void _window_set_aspect_ratio_native(GfxWindow* win, int w, int h) @private
{
	if (!win.src.window) return;
	if (w == 0 && h == 0)
	{
		w = 1;
		h = 1;
	}
	CGSize size = { w, h };
	objc::send_void(win.src.window, "setContentAspectRatio:", size);
}

fn bool _window_is_hidden_native(GfxWindow* win) @private
{
	if (win.src.window) return !objc::send(win.src.window, bool, "isVisible");
	return win.internal.flags.hide;
}

fn void _free_mouse(TodoRenameMouse* mouse)
{
	objc::release(mouse);
}

fn void? _init_native(String class_name, GfxInitOption flags)
{
	_gfx_ctx.ns_pool = objc::pool_push();
	_gfx_ctx.ns_app = objc::send(objc::class_by_name("NSApplication")!, ObjcId, "sharedApplication");
	if (!_gfx_ctx.ns_app) return NSAPP_CREATION_FAILED~;
	objc::send_void(_gfx_ctx.ns_app, "setActivationPolicy:", (NSUInteger)NS_APP_ACTIVATION_POLICY_REGULAR);
	objc::send_void(_gfx_ctx.ns_app, "finishLaunching");
	_osx_register_classes();
	todo_fix_pollMonitors();
}

fn void? _create_window_native(String name, GfxWindowFlags flags, GfxWindow* win)
{
	NSUInteger style = cocoa::NS_WINDOW_STYLE_TITLED | cocoa::NS_WINDOW_STYLE_CLOSABLE | cocoa::NS_WINDOW_STYLE_MINIATURIZABLE;
	if (!flags.no_resize) style |= cocoa::NS_WINDOW_STYLE_RESIZABLE;
	if (flags.no_border) style = cocoa::NS_WINDOW_STYLE_BORDERLESS;
	CGRect rect = { { win.x, todo_fix_cocoaYTransform((float)(win.y + win.height - 1)) } , { ...win.size }};
	ObjcId nswin = objc::send(ns_alloc("NSWindow"), ObjcSendIdWindowInit, "initWithContentRect:styleMask:backing:defer:", rect, style, NS_BACKING_STORE_BUFFERED, false);
	if (!nswin) return NSWINDOW_CREATION_FAILED~;
	ObjcId view = todo_fix_osxViewClass ? objc::send(todo_fix_osxViewClass, ObjcId, "alloc") : ns_alloc("NSView");
	view = objc::send(view, ObjcId, "initWithFrame:", (CGRect) { { } , { ...win.size } });
	if (todo_fix_osxViewClass) object_setInstanceVariable(view, "gfx_window", win);
	ObjcId delegate = null;
	if (todo_fix_osxWindowDelegateClass)
	{
		delegate = objc::send(todo_fix_osxWindowDelegateClass, ObjcId, "alloc");
		delegate = objc::send(delegate, ObjcId, "init");
		object_setInstanceVariable(delegate, "gfx_window", win);
		objc::send_void(nswin, "setDelegate:", delegate);
	}
	win.src.window = objc::retain(nswin);
	win.src.view = objc::retain(view);
	win.src.delegate = delegate;
	win.src.bounds = win.bounds;
	objc::send_void(nswin, "setContentView:", view);
	objc::send_void(nswin, "makeFirstResponder:", view);
	objc::send_void(nswin, "setAcceptsMouseMovedEvents:", true);
	win.set_name(name);
	todo_fix_window_setFloating(win, flags.floating);
	if (!(flags.hide)) _window_show_native(win);
	if (flags.maximize) _window_maximize_native(win);
	if (flags.minimize) todo_fix_window_minimizePlatform(win);
	if (flags.fullscreen) _window_set_fullscreen_native(win, true);
}


fn void _window_set_max_size_native(GfxWindow* win, GfxSize size)
{
	if (!win.src.window) return;
	if (size == { 0, 0 })
	{
		GfxMonitor* mon = find_monitor(win);
		if (mon) size = mon.mode.size;
	}
	objc::send_void(win.src.window, "setMaxSize:", (CGSize){ ...size });
}

fn void _window_restore_native(GfxWindow* win)
{
	if (!win.src.window) return;
	objc::send_void(win.src.window, "deminiaturize:", null);
	win.internal.flags.minimize = false;
	win.internal.flags.maximize = false;
}

fn bool _window_set_mouse_native(GfxWindow* win, TodoRenameMouse* mouse)
{
	cg::mainDisplayID().showCursor();
	objc::send_void(mouse, "set");
	win.src.mouse = mouse;
	return true;
}

fn void _window_show_native(GfxWindow* win)
{
	if (!win.src.window) return;
	objc::send_void(win.src.window, "makeKeyAndOrderFront:", null);
	if (_gfx_ctx.ns_app) objc::send_void(_gfx_ctx.ns_app, "activateIgnoringOtherApps:", true);
	win.internal.flags.hide = false;
}

fn void _destroy_native()
{
	if (_gfx_ctx.ns_pool)
	{
		objc::pool_pop(_gfx_ctx.ns_pool);
		_gfx_ctx.ns_pool = null;
	}
}

fn void? _write_clipboard_native(GfxDataTransfer* data)
{
	if (!data || data.type != TEXT || !data.data) return CLIPBOARD_COULD_NOT_BE_OPENED~;
	ObjcId pb = objc::send(objc::@class("NSPasteboard"), ObjcId, "generalPasteboard");
	objc::send_void(pb, "clearContents");
	if (!objc::send(pb,bool, "setString:forType:", ns_string((ZString)data.data), "public.utf8-plain-text".to_nsstring()))
	{
		return CLIPBOARD_COULD_NOT_BE_OPENED~;
	}
}

fn void todo_fix_pollMonitors()
{
	if (_gfx_ctx.monitors.count != 0) free_monitors();
	uint count = 0;
	CGDirectDisplayID[16] displays;
	if (cg::getActiveDisplayList(16, &displays, &count) != 0) return;
	for (uint i = 0; i < count; i++)
	{
		GfxMonitor mon;
		_monitor_from_display(displays[i], &mon);
		GfxMonitorNode* node = _monitor_add(&mon);
		if (node == null) continue;
		node.nativeId0 = (ulong)displays[i];
		if (displays[i] == cg::mainDisplayID()) _gfx_ctx.monitors.primary = node;
	}
}

fn void _monitor_from_display(CGDirectDisplayID display, GfxMonitor* mon) @local
{
	*mon = {};
	mem::copy(&mon.name, "Display", 8);
	CGRect bounds = display.bounds();
	CGDisplayModeRef mode = display.copyDisplayMode();
	mon.origin = { (int)bounds.origin.x, (int)bounds.origin.y };
	mon.mode.size.x = mode ? (int)mode.getWidth() : (int)bounds.size.width;
	mon.mode.size.y = mode ? (int)mode.getHeight() : (int)bounds.size.height;
	mon.mode.refresh_rate = mode ? (float)mode.getRefreshRate() : 60.0;
	mon.mode.rgb = { 8, 8, 8 };
	mon.mode.src = mode;
	mon.phys_size = (float[<2>])(double[<2>]){ ...display.screenSize() };
	mon.scale = {
		mon.mode.size.x > 0 ? (float)bounds.size.width / (float)mon.mode.size.x : 1.0,
		mon.mode.size.y > 0 ? (float)bounds.size.height / (float)mon.mode.size.y : 1.0
	};
	mon.pixelRatio = mon.scale.x;
}

fn GfxPoint _get_global_mouse_native()
{
	CGEventRef event = cg::eventCreate(null);
	if (!event) return { -1, -1 };
	defer event.release();
	CGPoint point = event.getLocation();
	return { (int)point.x, (int)point.y };
}

fn void? _surface_init_native(GfxSurface* surface, void* data, int w, int h, GfxFormat format, GfxWindow* window)
{
	surface.data = data;
	surface.size = { w, h };
	surface.format = format;
	surface.native.format = RGBA8;
	surface.native.buffer = malloc((sz)w * h * 4);
}

fn void _surface_release_native(GfxSurface* surface)
{
	if (!surface.data) return;
	if (surface.native.native0) objc::release(surface.native.native0);
	if (surface.native.buffer) free(surface.native.buffer);
	*surface = {};
}

// MacOS callbacks

fn void _osx_register_classes()
{
	if (todo_fix_osxWindowDelegateClass != (ObjcClass)null) return;
	todo_fix_osxWindowDelegateClass = objc_allocateClassPair(objc_getClass("NSObject"), "RGFWWindowDelegateC3", 0);
	if (todo_fix_osxWindowDelegateClass == (ObjcClass)null) return;
	class_addIvar(todo_fix_osxWindowDelegateClass, "gfx_window", void*::size, 3, "^v");
	class_addMethod(todo_fix_osxWindowDelegateClass, sel_registerName("windowWillClose:"), (void*)&_osx_window_will_close, "v@:@");
	class_addMethod(todo_fix_osxWindowDelegateClass, sel_registerName("windowDidResize:"), (void*)&_osx_window_did_resize, "v@:@");
	class_addMethod(todo_fix_osxWindowDelegateClass, sel_registerName("windowDidMove:"), (void*)&_osx_window_did_move, "v@:@");
	class_addMethod(todo_fix_osxWindowDelegateClass, sel_registerName("windowDidMiniaturize:"), (void*)&_osx_window_did_miniaturize, "v@:@");
	class_addMethod(todo_fix_osxWindowDelegateClass, sel_registerName("windowDidDeminiaturize:"), (void*)&_osx_window_did_deminiaturize, "v@:@");
	class_addMethod(todo_fix_osxWindowDelegateClass, sel_registerName("windowDidBecomeKey:"), (void*)&_osx_window_did_become_key, "v@:@");
	class_addMethod(todo_fix_osxWindowDelegateClass, sel_registerName("windowDidResignKey:"), (void*)&_osx_window_did_resign_key, "v@:@");
	objc_registerClassPair(todo_fix_osxWindowDelegateClass);
	todo_fix_osxViewClass = objc_allocateClassPair(objc_getClass("NSView"), "GfxViewC3", 0);
	if (todo_fix_osxViewClass == (ObjcClass)null) return;
	class_addIvar(todo_fix_osxViewClass, "gfx_window", void*::size, 3, "^v");
	class_addMethod(todo_fix_osxViewClass, sel_registerName("acceptsFirstResponder"), (void*)&_osx_accepts_first_responder, "c@:");
	class_addMethod(todo_fix_osxViewClass, sel_registerName("mouseMoved:"), (void*)&_osx_mouse_moved, "v@:@");
	class_addMethod(todo_fix_osxViewClass, sel_registerName("mouseDragged:"), (void*)&_osx_mouse_dragged, "v@:@");
	class_addMethod(todo_fix_osxViewClass, sel_registerName("rightMouseDragged:"), (void*)&_osx_mouse_dragged, "v@:@");
	class_addMethod(todo_fix_osxViewClass, sel_registerName("otherMouseDragged:"), (void*)&_osx_mouse_dragged, "v@:@");
	class_addMethod(todo_fix_osxViewClass, sel_registerName("mouseDown:"), (void*)&_osx_mouse_button_down, "v@:@");
	class_addMethod(todo_fix_osxViewClass, sel_registerName("mouseUp:"), (void*)&_osx_mouse_button_up, "v@:@");
	class_addMethod(todo_fix_osxViewClass, sel_registerName("rightMouseDown:"), (void*)&_osx_right_mouse_button_down, "v@:@");
	class_addMethod(todo_fix_osxViewClass, sel_registerName("rightMouseUp:"), (void*)&_osx_right_mouse_button_up, "v@:@");
	class_addMethod(todo_fix_osxViewClass, sel_registerName("otherMouseDown:"), (void*)&_osx_other_mouse_button_down, "v@:@");
	class_addMethod(todo_fix_osxViewClass, sel_registerName("otherMouseUp:"), (void*)&_osx_other_mouse_button_up, "v@:@");
	class_addMethod(todo_fix_osxViewClass, sel_registerName("scrollWheel:"), (void*)&_osx_scroll_wheel, "v@:@");
	class_addMethod(todo_fix_osxViewClass, sel_registerName("keyDown:"), (void*)&_osx_key_down, "v@:@");
	class_addMethod(todo_fix_osxViewClass, sel_registerName("keyUp:"), (void*)&_osx_key_up, "v@:@");
	class_addMethod(todo_fix_osxViewClass, sel_registerName("mouseEntered:"), (void*)&_osx_mouse_entered, "v@:@");
	class_addMethod(todo_fix_osxViewClass, sel_registerName("mouseExited:"), (void*)&_osx_mouse_exited, "v@:@");
	class_addMethod(todo_fix_osxViewClass, sel_registerName("flagsChanged:"), (void*)&_osx_flags_changed, "v@:@");
	class_addMethod(todo_fix_osxViewClass, sel_registerName("viewDidChangeBackingProperties"), (void*)&_osx_view_did_change_backing_properties, "v@:");
	class_addMethod(todo_fix_osxViewClass, sel_registerName("wantsUpdateLayer"), (void*)&_osx_wants_update_layer, "c@:");
	class_addMethod(todo_fix_osxViewClass, sel_registerName("updateLayer"), (void*)&_osx_update_layer, "v@:");
	class_addMethod(todo_fix_osxViewClass, sel_registerName("drawRect:"), (void*)&_osx_draw_rect, "v@:{CGRect={CGPoint=dd}{CGSize=dd}}");
	class_addMethod(todo_fix_osxViewClass, sel_registerName("draggingEntered:"), (void*)&_osx_dragging_entered, "l@:@");
	class_addMethod(todo_fix_osxViewClass, sel_registerName("draggingUpdated:"), (void*)&_osx_dragging_updated, "l@:@");
	class_addMethod(todo_fix_osxViewClass, sel_registerName("draggingExited:"), (void*)&_osx_dragging_ended, "v@:@");
	class_addMethod(todo_fix_osxViewClass, sel_registerName("draggingEnded:"), (void*)&_osx_dragging_ended, "v@:@");
	class_addMethod(todo_fix_osxViewClass, sel_registerName("prepareForDragOperation:"), (void*)&_osx_prepare_for_drag_operation, "c@:@");
	class_addMethod(todo_fix_osxViewClass, sel_registerName("performDragOperation:"), (void*)&_osx_perform_drag_operation, "c@:@");
	objc_registerClassPair(todo_fix_osxViewClass);
}
fn void _osx_key_updown(ObjcId self, ObjcId event, bool down)
{
	GfxWindow* win = todo_fix_osxViewWindow(self);
	// No window
	if (!win) return;
	ushort keyCode = objc::send(event, ushort, "keyCode");
	if (keyCode >= 256) return;
	GfxKey key = _gfx_ctx.keycodes[(sz)keyCode];
	// Unmapped key
	if (!key) return;
	bool repeat = down ? objc::send(event, bool, "isARepeat") : false;
	GfxKeyModifier mods = _osx_modifiers_to_gfx(objc::send(event, NSUInteger, "modifierFlags"));
	_key_callback(win, key, mods, repeat, down);
}

fn void _osx_key_down(ObjcId self, ObjcSelector sel, ObjcId event) => _osx_key_updown(self, event, true);
fn void _osx_key_up(ObjcId self, ObjcSelector sel, ObjcId event) => _osx_key_updown(self, event, false);

fn void _osx_mouse_entered(ObjcId self, ObjcSelector sel, ObjcId event)
{
	GfxWindow* win = todo_fix_osxViewWindow(self);
	if (!win) return;
	CGPoint p = objc::send(event, CGPoint, "locationInWindow");
	_mouse_notify_callback(win, { (int)p.x, win.height - (int)p.y }, true);
}

fn void _osx_mouse_exited(ObjcId self, ObjcSelector sel, ObjcId event)
{
	if (GfxWindow* win = todo_fix_osxViewWindow(self)) _mouse_notify_callback(win, win.internal.last_mouse, false);
}

fn void _osx_window_did_move(ObjcId self, ObjcSelector sel, ObjcId notification)
{
	GfxWindow* win = todo_fix_osxDelegateWindow(self);
	if (!win || !win.src.window) return;
	CGRect frame = objc::send(win.src.window, CGRect, "frame");
	CGRect content = objc::send(win.src.window, CGRect, "contentRectForFrameRect:", frame);
	float y = todo_fix_cocoaYTransform((float)(content.origin.y + content.size.height - 1));
	_window_moved_callback(win, { (int)content.origin.x, (int)y });
}

fn void _osx_window_did_miniaturize(ObjcId self, ObjcSelector sel, ObjcId notification)
{
	if (GfxWindow* win = todo_fix_osxDelegateWindow(self)) _window_minimized_callback(win);
}

fn void _osx_window_did_deminiaturize(ObjcId self, ObjcSelector sel, ObjcId notification)
{
	if (GfxWindow* win = todo_fix_osxDelegateWindow(self)) _window_restored_callback(win, win.bounds);
}

fn void _osx_window_did_become_key(ObjcId self, ObjcSelector sel, ObjcId notification)
{
	if (GfxWindow* win = todo_fix_osxDelegateWindow(self)) _window_focus_callback(win, true);
}

fn void _osx_window_will_close(ObjcId self, ObjcSelector sel, ObjcId notification)
{
	if (GfxWindow* win = todo_fix_osxDelegateWindow(self)) _window_close_callback(win);
}

fn void _osx_window_did_resize(ObjcId self, ObjcSelector sel, ObjcId notification)
{
	if (GfxWindow* win = todo_fix_osxDelegateWindow(self)) _window_resized_callback(win, _window_fetch_size_native(win));
}
fn void _osx_window_did_resign_key(ObjcId self, ObjcSelector sel, ObjcId notification)
{
	if (GfxWindow* win = todo_fix_osxDelegateWindow(self)) _window_focus_callback(win, false);
}

fn void _osx_mouse_moved_common(ObjcId self, ObjcId event)
{
	GfxWindow* win = todo_fix_osxViewWindow(self);
	if (!win) return;
	CGPoint p = objc::send(event, CGPoint, "locationInWindow");
	_mouse_motion_callback(win, { (int)p.x, win.height - (int)p.y });
}

fn void _osx_mouse_moved(ObjcId self, ObjcSelector sel, ObjcId event) => _osx_mouse_moved_common(self, event);
fn void _osx_mouse_dragged(ObjcId self, ObjcSelector sel, ObjcId event) => _osx_mouse_moved_common(self, event);

// private
fn void _osx_mouse_button_common(ObjcId self, ObjcId event, bool down, GfxMouseButton fallback)
{
	GfxWindow* win = todo_fix_osxViewWindow(self);
	if (!win) return;
	NSUInteger button = objc::send(event, NSUInteger, "buttonNumber");
	GfxMouseButton gfx_button = fallback;
	switch (button)
	{
		case 0: gfx_button = LEFT;
		case 1: gfx_button = RIGHT;
		case 2: gfx_button = MIDDLE;
		default: gfx_button = (GfxMouseButton)button;
	}
	_mouse_button_callback(win, gfx_button, down);
	_osx_mouse_moved_common(self, event);
}

fn void _osx_mouse_button_down(ObjcId self, ObjcSelector sel, ObjcId event) => _osx_mouse_button_common(self, event, true, LEFT);
fn void _osx_mouse_button_up(ObjcId self, ObjcSelector sel, ObjcId event) => _osx_mouse_button_common(self, event, false, LEFT);
fn void _osx_right_mouse_button_down(ObjcId self, ObjcSelector sel, ObjcId event) => _osx_mouse_button_common(self, event, true, RIGHT);
fn void _osx_right_mouse_button_up(ObjcId self, ObjcSelector sel, ObjcId event) => _osx_mouse_button_common(self, event, false, RIGHT);
fn void _osx_other_mouse_button_down(ObjcId self, ObjcSelector sel, ObjcId event) { _osx_mouse_button_common(self, event, true, MIDDLE); }
fn void _osx_other_mouse_button_up(ObjcId self, ObjcSelector sel, ObjcId event) { _osx_mouse_button_common(self, event, false, MIDDLE); }

fn void _osx_scroll_wheel(ObjcId self, ObjcSelector sel, ObjcId event)
{
	GfxWindow* win = todo_fix_osxViewWindow(self);
	if (!win) return;
	CGFloat x = objc::send(event, CGFloat, "scrollingDeltaX");
	CGFloat y = objc::send(event, CGFloat, "scrollingDeltaY");
	_mouse_scroll_callback(win, { (float)x, (float)y });
}

fn void _osx_flags_changed(ObjcId self, ObjcSelector sel, ObjcId event)
{
	GfxWindow* win = todo_fix_osxViewWindow(self);
	if (!win) return;

	GfxKey value = NULL;
	bool pressed = false;

	NSUInteger flags = objc::send(event, NSUInteger, "modifierFlags");
	_key_update_key_mods_ex(win,
		flags & 0x00010000 != 0,
		flags & 0x00200000 != 0,
		flags & 0x00040000 != 0,
		flags & 0x00080000 != 0,
		flags & 0x00020000 != 0,
		flags & 0x00100000 != 0,
		false);

	for (int i = 0; i < 9; i++)
	{
		GfxKey modKey = CAPS_LOCK + (GfxKey)i;
		_gfx_ctx.keyboard[modKey].prev = _gfx_ctx.keyboard[modKey].current;
	}

	for (int i = 0; i < 5; i++)
	{
		NSUInteger shift = (NSUInteger)1 << (uint)(i + 16);
		GfxKey key = CAPS_LOCK + (GfxKey)i;
		if (flags & shift && !is_key_down(key))
		{
			pressed = true;
			value = key;
			break;
		}
		if (flags & shift == 0 && is_key_down(key))
		{
			pressed = false;
			value = key;
			break;
		}
	}

	_key_callback(win, value, win.internal.mod, is_key_down(value) && pressed, pressed);
	if (value != CAPS_LOCK)
	{
		GfxKey other = (GfxKey)(value + 4);
		_key_callback(win, other, win.internal.mod, is_key_down(other) && pressed, pressed);
	}
}

fn void _osx_view_did_change_backing_properties(ObjcId self, ObjcSelector sel)
{
	GfxWindow* win = todo_fix_osxViewWindow(self);
	if (!win) return;
	GfxMonitor* mon = find_monitor(win);
	if (!mon) return;
	_scale_updated_callback(win, mon.scale);
}

fn NSUInteger _osx_dragging_entered(ObjcId self, ObjcSelector sel, ObjcId sender)
{
	GfxWindow* win = todo_fix_osxViewWindow(self);
	if (!win) return 0;
	CGPoint p = objc::send(sender, CGPoint, "draggingLocation");
	_data_drag_callback(win, FILE, ENTER, { (int)p.x, win.height - (int)p.y });
	return 1;
}

fn NSUInteger _osx_dragging_updated(ObjcId self, ObjcSelector sel, ObjcId sender)
{
	GfxWindow* win = todo_fix_osxViewWindow(self);
	if (!win) return 0;
	if (!(win.internal.enabled_events & TODO_RENAME_DATA_DRAG_FLAG)) return 1;
	CGPoint p = objc::send(sender, CGPoint, "draggingLocation");
	_data_drag_callback(win, FILE, MOVE, { (int)p.x, win.height - (int)p.y });
	return 1;
}

fn void _osx_dragging_ended(ObjcId self, ObjcSelector sel, ObjcId sender)
{
	GfxWindow* win = todo_fix_osxViewWindow(self);
	if (!win) return;
	if (!(win.internal.enabled_events & TODO_RENAME_DATA_DRAG_FLAG)) return;
	CGPoint p = objc::send(sender, CGPoint, "draggingLocation");
	_data_drag_callback(win,FILE, EXIT, { (int)p.x, win.height - (int)p.y });
}

fn bool _osx_prepare_for_drag_operation(ObjcId self, ObjcSelector sel, ObjcId sender)
{
	GfxWindow* win = todo_fix_osxViewWindow(self);
	if (!win || !(win.internal.enabled_events & TODO_RENAME_DATA_DROP_FLAG)) return true;
	return win.internal.flags.allow_dnd;
}

// private
fn bool _osx_perform_drag_operation(ObjcId self, ObjcSelector sel, ObjcId sender)
{
	GfxWindow* win = todo_fix_osxViewWindow(self);
	if (!win || !(win.internal.enabled_events & TODO_RENAME_DATA_DROP_FLAG)) return false;

	ObjcId pasteBoard = objc::send(sender, ObjcId, "draggingPasteboard");
	ObjcId types = objc::send(pasteBoard, ObjcId, "types");
	ObjcId file_urls_type = "NSFilenamesPboardType".to_nsstring();

	if (!objc::send(types, ObjcId, "containsObject:", file_urls_type))
	{
		log::warn("No files found on the pasteboard", category: log::CATEGORY_VIDEO);
		return false;
	}

	ObjcId propertyList = objc::send(pasteBoard, ObjcId, "propertyListForType:", file_urls_type);

	if (objc::send(propertyList, bool, "isKindOfClass:", objc_class("NSString")))
	{
		NSUInteger stringCount = objc::send(propertyList, NSUInteger, "length");
		ZString filePath = ns_utf8(propertyList);
		_data_drop_callback(win, filePath, (sz)stringCount + 1, FILE);
	}
	if (!objc::send(propertyList, bool, "isKindOfClass:", objc_class("NSArray"))) return false;

	NSUInteger count = objc::send(propertyList, NSUInteger, "count");
	if (!count) return false;

	for (NSUInteger i = 0; i < count; i++)
	{
		ObjcId fileURL = objc::send(propertyList, ObjcId, "objectAtIndex:", i);
		ZString filePath = ns_utf8(fileURL);
		NSUInteger stringCount = objc::send(fileURL, NSUInteger, "length");
		_data_drop_callback(win, filePath, (sz)stringCount + 1, FILE);
	}
	return false;
}

fn bool _osx_wants_update_layer(ObjcId self, ObjcSelector sel) => true;

fn void _osx_update_layer(ObjcId self, ObjcSelector sel)
{
	if (GfxWindow* win = todo_fix_osxViewWindow(self)) _window_refresh_callback(win, { 0, 0, ...win.size });
}

fn bool _osx_accepts_first_responder(ObjcId self, ObjcSelector sel) => true;

fn void _osx_draw_rect(ObjcId self, ObjcSelector sel, CGRect rect)
{
	GfxWindow* win = todo_fix_osxViewWindow(self);
	if (!win) return;
	float y = todo_fix_cocoaYTransform((float)(rect.size.height - 1));
	_window_refresh_callback(win, { (int)rect.origin.x, (int)y, (int)rect.size.width, (int)rect.size.height });
}

// --- helpers

fn GfxKeyModifier _osx_modifiers_to_gfx(NSUInteger flags)
{
	return {
		.shift     = flags & 0x00020000 != 0,
		.control   = flags & 0x00040000 != 0,
		.alt       = flags & 0x00080000 != 0,
		.super     = flags & 0x00100000 != 0,
		.caps_lock = flags & 0x00010000 != 0,
	};
}

fn ObjcId _get_nsscreen_for_display(uint display_id)
{
	ObjcId screens = objc::send(objc_class("NSScreen"), ObjcId, "screens");
	if (!screens) return null;
	NSUInteger count = objc::send(screens, NSUInteger, "count");
	for (NSUInteger i = 0; i < count; i++)
	{
		ObjcId screen = objc::send(screens, ObjcId, "objectAtIndex:", i);
		ObjcId desc = objc::send(screen, ObjcId, "deviceDescription");
		ObjcId number = objc::send(desc, ObjcId, "objectForKey:", ns_string("NSScreenNumber"));
		uint display = (uint)objc::send(number, NSUInteger, "unsignedIntValue");
		if (display == display_id) return screen;
	}
	return null;
}
