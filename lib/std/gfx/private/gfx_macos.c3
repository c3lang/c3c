module std::gfx @feat(MACOS) @private @link("Cocoa.framework", "CoreGraphics.framework", "ApplicationServices.framework", "QuartzCore.framework", "Carbon.framework", "CoreFoundation.framework", "objc");
import std::os::macos, std::math, std::os::darwin, std::collections;

struct GfxGlContext
{
	ObjId ctx;
	ObjId format;
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
	ObjId native0;
	void* native1;
	void* native2;
}

struct GfxWindowSrc
{
	inline IntRect bounds;
	ObjId window;
	ObjId view;
	ObjId mouse;
	ObjId delegate;
	GfxBackingContext ctx;
	GfxBacking backing;
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
	win.src.backing = { .opengl };

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

	ObjId format = objc::alloc_by_name("NSOpenGLPixelFormat").send(ObjId, "initWithAttributes:", stack.array_view());
	if (!format) return OPENGL_PIXEL_FORMAT_LOAD_FAILED~;
	defer catch objc::release(format);

	ObjId share = hints.share ? hints.share.ctx : null;
	ctx.ctx = objc::alloc_by_name("NSOpenGLContext").send(ObjId, "initWithFormat:shareContext:", format, share);
	if (!ctx.ctx) return OPENGL_CTX_CREATION_FAILED~;
	ctx.format = format;

	ctx.ctx.send_void("setView:", win.src.view);
	if (win.internal.flags.transparent)
	{
		int opacity = 0;
		nsOpenGLContext_setValues((ObjId)ctx.ctx, &opacity, NS_OPEN_GL_CP_SURFACE_OPACITY);
	}
	_window_make_opengl_current_context_native(win);
	win.opengl_swap_interval(0);
}

fn void* _get_current_opengl_context_native()
{
	return (void*)objc::send(objc::@class("NSOpenGLContext"), ObjId, "currentContext");
}

fn void _window_swap_opengl_buffers_native(GfxWindow* win)
{
	if (win.src.ctx.native && win.src.ctx.native.ctx) win.src.ctx.native.ctx.send_void("flushBuffer");
}

fn void _window_make_opengl_current_context_native(GfxWindow* win)
{
	if (win && win.src.ctx.native && win.src.ctx.native.ctx)
	{
		win.src.ctx.native.ctx.send_void("makeCurrentContext");
	}
	else
	{
		objc::send_void(objc::@class("NSOpenGLContext"), "clearCurrentContext");
	}
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

	ObjId nativeWindow = win.src.window;
	if (win.src.view) nativeWindow = _get_layer_macos();
	ctx.surface = egl_create_window_surface((EglDisplay)_gfx_ctx.egl_display, config, (void*)nativeWindow, null);
	if (!ctx.surface) return EGL_SURFACE_CREATION_FAILED~;

	win.src.ctx.egl = ctx;
	win.src.backing = { .egl };
	todo_fix_window_makeCurrentContext_EGL(win);
}

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
	ObjId layer = objc::send(objc::@class("CAMetalLayer"), ObjId, "layer");
	if (!layer) return SURFACE_CREATION_FAILED~;
	win.src.view.send_void("setWantsLayer:", true);
	_window_set_layer_macos(win, layer);
	VkMetalSurfaceCreateInfoEXT create_info;

	create_info.sType = VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT;
	create_info.pNext = null;
	create_info.flags = 0;
	create_info.pLayer = (void*)layer;
	if (raw(instance, &create_info, null, &surface)) return SURFACE_CREATION_FAILED~;
	return surface;
}

fn void _window_set_capture_mouse_native(GfxWindow* win, bool state)
{
	cg::associateMouseAndMouseCursorPosition(!state);
}

fn void _window_maximize_native(GfxWindow* win)
{
	win.src.window.send_void("zoom:", null);
}

fn void _window_move_mouse(GfxWindow* win, GfxPoint where)
{
	cg::warpMouseCursorPosition({ ...where });
	if (win) win.internal.last_mouse = where;
}

fn void _window_hide_native(GfxWindow* win)
{
	win.src.window.send_void("orderOut:", null);
	win.internal.flags.hide = true;
}

fn bool _window_is_minimized_native(GfxWindow* win)
{
	if (win.src.window) return win.src.window.send(bool, "isMiniaturized");
	return win.internal.flags.minimize;
}

fn void _window_flash_native(GfxWindow* win, GfxFlashRequest request)
{
	_gfx_ctx.ns_app.send(NSInteger, "requestUserAttention:", (NSUInteger)request);
}

fn void _window_blit_surface_native(GfxWindow* win, GfxSurface* surface)
{
	if (!surface.data) return;
	GfxPoint min = math::min(win.size, surface.size);
	if (try mon = _window_find_monitor_native(win))
	{
		min = (GfxPoint)((Vec2)min * mon.pixelRatio);
	}
	ObjId rep = ns_bitmapRep(...min);
	char* pixels = ns_bitmapData(rep);
	defer objc::release(rep);
	if (!pixels) return;
	copy_image_data(pixels, ...min, RGBA8, surface.data, surface.format, surface.convert_fn);
	ObjId image = ns_imageWithRep(rep, ...min);
	win.src.view.send_void("setWantsLayer:", true);
	ObjId layer = win.src.view.send(ObjId, "layer");
	layer.send_void("setContents:", image);
	objc::release(image);
}


fn void _window_set_raw_mouse_mode_native(GfxWindow* win, bool state)
{
	_gfx_ctx.raw_mouse = state;
}

fn bool _window_is_floating_native(GfxWindow* win)
{
	if (!win) return false;
	if (win.src.window) return win.src.window.send(NSInteger, "level") > NSWindowLevel.NORMAL;
	return win.internal.flags.floating;
}

fn void _window_set_border_native(GfxWindow* win, bool border)
{
	if (!win.src.window) return;
	CGRect frame = win.src.window.send(CGRect, "frame");
	CGRect content = win.src.view.send(CGRect, "frame");
	double offset = 0;

	win.internal.flags.no_border = !border;
	NSWindowStyleMask styleMask = BORDERLESS | FULL_SIZE_CONTENT_VIEW;
	if (border) styleMask = TITLED | CLOSABLE | MINIATURIZABLE;
	if (!win.internal.flags.no_resize) styleMask |= RESIZABLE;

	win.src.window.send_void("setStyleMask:", styleMask);

	if (!border)
	{
		ObjId miniaturizeButton = win.src.window.send(ObjId, "standardWindowButton:", NSWindowButton.MINIATURIZE);
		ObjId titleBarView = miniaturizeButton.send(ObjId, "superview");
		titleBarView.send_void("setHidden:", true);
		offset = (double)(frame.size.height - content.size.height);
	}

	win.resize(win.size + { 0, (int)offset });
	win.height -= (int)offset;
}

fn GfxPoint _window_fetch_size_native(GfxWindow* win)
{
	if (win.src.view)
	{
		CGRect frame = win.src.view.send(CGRect, "frame");
		win.size = { (int)frame.size.width, (int)frame.size.height };
	}
	return win.size;
}

fn void _window_set_fullscreen_native(GfxWindow* win, bool fullscreen)
{
	if (win.src.window) win.src.window.send_void("toggleFullScreen:", null);
	win.internal.flags.fullscreen = fullscreen;
}

fn void _window_focus_native(GfxWindow* win)
{
	win.src.window.send_void("makeKeyWindow", null);
	win.internal.inFocus = true;
}

fn void _window_resize_native(GfxWindow* win, GfxSize size)
{
	if (!win.src.window) return;
	CGRect frame = win.src.window.send(CGRect, "frame");
	CGRect content = win.src.window.send(CGRect, "contentRectForFrameRect:", frame);
	double offset = frame.size.height - content.size.height;
	win.src.window.send_void("setFrame:display:", ns_rect((double)win.x, (double)win.y, (double)size.x, (double)size.y + offset), true);
}

fn void _window_move_native(GfxWindow* win, GfxPoint where)
{
	if (!win.src.window) return;
	CGRect content = win.src.view.send(CGRect, "frame");
	win.x = where.x;
	win.y = (int)flip_y((float)where.y + (float)content.size.height - 1.0f);
	CGPoint origin = { ...where };
	win.src.window.send_void("setFrameOrigin:", origin);
}

fn void _window_set_min_size_native(GfxWindow* win, GfxSize size)
{
	win.src.window.send_void("setMinSize:", (CGSize) { ...size });
}

fn bool _window_set_icon_native(GfxWindow* win, void* data, int w, int h, GfxFormat format, GfxWindowIconType type)
{
	if (!_gfx_ctx.ns_app) return false;
	if (!data)
	{
		_gfx_ctx.ns_app.send_void("setApplicationIconImage:", null);
		return true;
	}
	ObjId rep = ns_bitmapRep(w, h);
	defer objc::release(rep);
	char* pixels = ns_bitmapData(rep);
	if (!pixels) return false;
	copy_image_data(pixels, w, h, RGBA8, data, format, null);
	ObjId image = ns_imageWithRep(rep, w, h);
	defer objc::release(image);
	_gfx_ctx.ns_app.send_void("setApplicationIconImage:", image);
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
	win.src.window.send_void("setContentAspectRatio:", size);
}

fn bool _window_is_hidden_native(GfxWindow* win) @private
{
	if (win.src.window) return !win.src.window.send(bool, "isVisible");
	return win.internal.flags.hide;
}

fn void _free_mouse(TodoRenameMouse* mouse)
{
	objc::release((ObjId)mouse);
}

fn void? _init_native(String class_name, GfxInitOption flags)
{
	_gfx_ctx.ns_pool = objc::pool_push();
	ObjId app = _gfx_ctx.ns_app = objc::send(objc::class("NSApplication"), ObjId, "sharedApplication");
	if (!app) return NSAPP_CREATION_FAILED~;
	app.send_void("setActivationPolicy:", (NSUInteger)NS_APP_ACTIVATION_POLICY_REGULAR);
	app.send_void("finishLaunching");
	_osx_register_classes()!;
	_poll_monitors_native();
}

fn void _poll_events_native()
{
	objc::@autoreleasepool()
	{
		todo_fix_resetPrevState();
		if (!_gfx_ctx.ns_app) return;
		ObjId distantPast = objc_send(objc::@class("NSDate"), ObjcSendId, "distantPast");
		ObjId mode = darwin::string("kCFRunLoopDefaultMode");
		while (true)
		{
			objc::@autoreleasepool()
			{
				ObjId ev = _gfx_ctx.ns_app.send(ObjId, "nextEventMatchingMask:untilDate:inMode:dequeue:", NSEventMask.ANY, distantPast, mode, true);
				if (!ev) break;
				objc_send(_gfx_ctx.ns_app, ObjcSendVoidId, "sendEvent:", ev);
			};
		}
	};

}


fn void? _create_window_native(String name, GfxWindowFlags flags, GfxWindow* win)
{
	NSWindowStyleMask style = TITLED | CLOSABLE | MINIATURIZABLE;
	if (!flags.no_resize) style |= RESIZABLE;
	if (flags.no_border) style = BORDERLESS;
	CGRect rect = { { win.x, flip_y((float)(win.y + win.height - 1)) } , { ...win.size }};
	ObjId nswin = objc::alloc_by_name("NSWindow").send(ObjId, "initWithContentRect:styleMask:backing:defer:", rect, style, NSBackingStoreType.BUFFERED, false);
	if (!nswin) return NSWINDOW_CREATION_FAILED~;
	ObjId view = _macos_view_class ? objc::send(_macos_view_class, ObjId, "alloc") : objc::alloc_by_name("NSView");
	view = view.send(ObjId, "initWithFrame:", (CGRect) { { } , { ...win.size } });
	if (_macos_view_class) view.set_ivar("gfx_window", win);
	ObjId delegate = null;
	if (_macos_window_delegate_class)
	{
		delegate = objc::alloc(_macos_window_delegate_class).send(ObjId, "init");
		delegate.set_ivar("gfx_window", win);
		nswin.send_void("setDelegate:", delegate);
	}
	win.src.window = nswin.retain();
	win.src.view = view.retain();
	win.src.delegate = delegate;
	win.src.bounds = win.bounds;
	nswin.send_void("setContentView:", view);
	nswin.send_void("makeFirstResponder:", view);
	nswin.send_void("setAcceptsMouseMovedEvents:", true);
	win.set_name(name);
	_window_set_floating_native(win, flags.floating);
	if (!(flags.hide)) _window_show_native(win);
	if (flags.maximize) _window_maximize_native(win);
	if (flags.minimize) _window_minimize_native(win);
	if (flags.fullscreen) _window_set_fullscreen_native(win, true);

	_window_set_flags(win, flags, {});
	win.src.backing = {};
	if (flags.open_gl) _window_create_opengl_context_native(win, get_global_opengl_hints())!;
	if (flags.egl) _window_create_egl_context_native(win, get_global_opengl_hints())!;
	_macos_init_view(win);
}

fn GfxMonitor*? _window_find_monitor_native(GfxWindow* win) => get_primary_monitor();

fn void? _monitor_set_gamma_ramp_native(GfxMonitor* monitor, GfxGammaRamp ramp) => @pool()
{
	if (!ramp.red) return GAMMA_COULD_NOT_BE_SET~;
	sz len = ramp.red.len;
	float[] rgb = mem::temp_array(float, len * 3);
	float[] g = rgb[len:len];
	float[] b = rgb[len * 2..];
	foreach (i, red : ramp.red)
	{
		rgb[i] = red / 65535.0f;
		g[i] = ramp.green[i] / 65535.0f;
		b[i] = ramp.blue[i] / 65535.0f;
	}
	if (!todo_fix_monitorDisplay(monitor).setTransferByTable((uint)len, rgb.ptr, g, b)) return GAMMA_COULD_NOT_BE_SET~;
}

fn void? _monitor_set_gamma_native(GfxMonitor* monitor, float gamma) => @pool()
{
	CGDirectDisplayID display = todo_fix_monitorDisplay(monitor);
	uint len = display.gammaTableCapacity();
	if (!len) return CANNOT_RETRIEVE_GAMMA~;
	float[] gamma_array = mem::temp_array(float, len);
	foreach (i, &g : gamma_array)
	{
		*g = _gamma_for_index(gamma, i, len) / 65535.0f;
	}
	if (!todo_fix_monitorDisplay(monitor).setTransferByTable((uint)len, gamma_array, gamma_array, gamma_array)) return GAMMA_COULD_NOT_BE_SET~;
}

fn GfxGammaRamp? _monitor_get_gamma_ramp_native(GfxMonitor* monitor, Allocator a) => @pool()
{
	CGDirectDisplayID display = todo_fix_monitorDisplay(monitor);
	sz count = (sz)display.gammaTableCapacity();
	if (!count) return CANNOT_RETRIEVE_GAMMA~;
	float[] rgb = mem::temp_array(float, (sz)count * 3);
	uint actual = 0;
	float[] red = rgb[:count];
	float[] green = rgb[count:count];
	float[] blue = rgb[count * 2:count];
	if (!display.transferByTable((uint)count, red, green, blue, &actual)) return CANNOT_RETRIEVE_GAMMA~;
	ushort[] ramp = alloc::alloc_array(a, ushort, (sz)actual * 3);
	ushort[] ramp_r = ramp[:actual];
	ushort[] ramp_g = ramp[actual:actual];
	ushort[] ramp_b = ramp[actual * 2:actual];
	for (sz i = 0; i < (sz)actual; i++)
	{
		ramp_r[i] = (ushort)(red[i] * 65535.0);
		ramp_g[i] = (ushort)(green[i] * 65535.0);
		ramp_b[i] = (ushort)(blue[i] * 65535.0);
	}
	return { ramp_r, ramp_g, ramp_b, a };
}

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

fn IntRect _monitor_get_workarea(GfxMonitor* monitor)
{
	CGRect bounds = todo_fix_monitorDisplay(monitor).bounds();
	return { (int)bounds.origin.x, (int)bounds.origin.y, (int)bounds.size.width, (int)bounds.size.height };
}

fn void _window_set_max_size_native(GfxWindow* win, GfxSize size)
{
	if (!win.src.window) return;
	if (size == { 0, 0 })
	{
		if (try mon = _window_find_monitor_native(win)) size = mon.mode.size;
	}
	win.src.window.send_void("setMaxSize:", (CGSize){ ...size });
}

fn void _window_set_name_native(GfxWindow* win, String name)
{
	win.src.window.send_void("setTitle:", name.to_nsstring());
}

fn void _window_restore_native(GfxWindow* win)
{
	if (!win.src.window) return;
	win.src.window.send_void("deminiaturize:", null);
	win.internal.flags.minimize = false;
	win.internal.flags.maximize = false;
}

fn void _window_raise_native(GfxWindow* win)
{
	win.src.window.send_void("orderFrontRegardless", null);
}

fn void _window_set_mouse_passthrough_native(GfxWindow* win, bool passthrough)
{
	if (win.src.window) win.src.window.send_void("setIgnoresMouseEvents:", passthrough);
}

fn bool _window_is_maximized_native(GfxWindow* win)
{
	if (win.src.window) return win.src.window.send(bool, "isZoomed");
	return win.internal.flags.maximize;
}

fn void _window_set_opacity_native(GfxWindow* win, char opacity)
{
	win.src.window.send_void("setAlphaValue:", (CGFloat)opacity / 255.0);
}

fn void _window_minimize_native(GfxWindow* win)
{
	win.src.window.send_void("miniaturize:", null);
}

fn void _window_close_native(GfxWindow* win)
{
	win.src.window.send_void("close");
	win.src.delegate.release();
	win.src.view.release();
	win.src.window.release();
	win.src.delegate = null;
	win.src.view = null;
	win.src.window = null;
}
fn bool _window_set_mouse_native(GfxWindow* win, TodoRenameMouse* mouse)
{
	ObjId mouse_native = (ObjId)mouse;
	cg::mainDisplayID().showCursor();
	mouse_native.send_void("set");
	win.src.mouse = mouse_native;
	return true;
}

fn void _window_set_floating_native(GfxWindow* win, bool floating)
{
	if (win.src.window) win.src.window.send_void("setLevel:", floating ? NSWindowLevel.FLOATING : NSWindowLevel.NORMAL);
	win.internal.flags.floating = floating;
}

fn void _window_show_native(GfxWindow* win)
{
	if (!win.src.window) return;
	win.src.window.send_void("makeKeyAndOrderFront:", null);
	if (_gfx_ctx.ns_app) _gfx_ctx.ns_app.send_void("activateIgnoringOtherApps:", true);
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
	ObjId pb = objc::send(objc::@class("NSPasteboard"), ObjId, "generalPasteboard");
	pb.send_void("clearContents");
	if (!pb.send(bool, "setString:forType:", darwin::string((ZString)data.data), "public.utf8-plain-text".to_nsstring()))
	{
		return CLIPBOARD_COULD_NOT_BE_OPENED~;
	}
}

fn void _poll_monitors_native()
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

fn void? _monitor_request_mode_native(GfxMonitor* mon, GfxMonitorMode* mode, GfxMonitorModeRequest request)
{
	return _monitor_set_mode_native(mon, mode);
}

fn void? _monitor_set_mode_native(GfxMonitor* mon, GfxMonitorMode* mode)
{
	if (!mode.src) return MONITOR_MODE_CHANGE_FAILED~;
	if (todo_fix_monitorDisplay(mon).setDisplayMode((CGDisplayModeRef)mode.src, null)) return MONITOR_MODE_CHANGE_FAILED~;
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

fn WGPUSurface _window_create_wgpu_surface_native(GfxWindow* window, WGPUInstance instance)
{
	if (!window.src.view)
	{
		log::error("NSView is NULL for macOS window", category: log::CATEGORY_VIDEO);
		return null;
	}
	window.src.view.send_void("setWantsLayer:", true);
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

fn void _stop_check_events_native()
{
	_gfx_ctx.stop_check_events = true;
}


// MacOS callbacks

fn void? _osx_register_classes()
{
	if (_macos_window_delegate_class) return;

	_macos_window_delegate_class = objc::class("NSObject").new_subclass("C3GFXWindowDelegate");
	if (!_macos_window_delegate_class) return DELEGATE_CREATION_FAILED~;
	defer catch _macos_window_delegate_class = (ObjClass)0;
	_macos_window_delegate_class.add_ivar("gfx_window", void*, "^v");
	
	_macos_window_delegate_class.add_method("windowWillClose:",        &_osx_window_will_close, "v@:@");
	_macos_window_delegate_class.add_method("windowDidResize:",        &_osx_window_did_resize, "v@:@");
	_macos_window_delegate_class.add_method("windowDidMove:",          &_osx_window_did_move, "v@:@");
	_macos_window_delegate_class.add_method("windowDidMiniaturize:",   &_osx_window_did_miniaturize, "v@:@");
	_macos_window_delegate_class.add_method("windowDidDeminiaturize:", &_osx_window_did_deminiaturize, "v@:@");
	_macos_window_delegate_class.add_method("windowDidBecomeKey:",     &_osx_window_did_become_key, "v@:@");
	_macos_window_delegate_class.add_method("windowDidResignKey:",     &_osx_window_did_resign_key, "v@:@");

	objc::register_new_subclass(_macos_window_delegate_class);

	_macos_view_class = objc::class("NSView").new_subclass("GfxViewC3");
	if (!_macos_view_class) return DELEGATE_CREATION_FAILED~;

	_macos_view_class.add_ivar("gfx_window", void*, "^v");
	_macos_view_class.add_method("acceptsFirstResponder",    &_osx_accepts_first_responder, "c@:");
	_macos_view_class.add_method("mouseMoved:",              &_osx_mouse_moved, "v@:@");
	_macos_view_class.add_method("mouseDragged:",            &_osx_mouse_dragged, "v@:@");
	_macos_view_class.add_method("rightMouseDragged:",       &_osx_mouse_dragged, "v@:@");
	_macos_view_class.add_method("otherMouseDragged:",       &_osx_mouse_dragged, "v@:@");
	_macos_view_class.add_method("mouseDown:",               &_osx_mouse_button_down, "v@:@");
	_macos_view_class.add_method("mouseUp:",                 &_osx_mouse_button_up, "v@:@");
	_macos_view_class.add_method("rightMouseDown:",          &_osx_right_mouse_button_down, "v@:@");
	_macos_view_class.add_method("rightMouseUp:",            &_osx_right_mouse_button_up, "v@:@");
	_macos_view_class.add_method("otherMouseDown:",          &_osx_other_mouse_button_down, "v@:@");
	_macos_view_class.add_method("otherMouseUp:",            &_osx_other_mouse_button_up, "v@:@");
	_macos_view_class.add_method("scrollWheel:",             &_osx_scroll_wheel, "v@:@");
	_macos_view_class.add_method("keyDown:",                 &_osx_key_down, "v@:@");
	_macos_view_class.add_method("keyUp:",                   &_osx_key_up, "v@:@");
	_macos_view_class.add_method("mouseEntered:",            &_osx_mouse_entered, "v@:@");
	_macos_view_class.add_method("mouseExited:",             &_osx_mouse_exited, "v@:@");
	_macos_view_class.add_method("flagsChanged:",            &_osx_flags_changed, "v@:@");
	_macos_view_class.add_method("viewDidChangeBackingProperties", &_osx_view_did_change_backing_properties, "v@:");
	_macos_view_class.add_method("wantsUpdateLayer",         &_osx_wants_update_layer, "c@:");
	_macos_view_class.add_method("updateLayer",              &_osx_update_layer, "v@:");
	_macos_view_class.add_method("drawRect:",                &_osx_draw_rect, "v@:{CGRect={CGPoint=dd}{CGSize=dd}}");
	_macos_view_class.add_method("draggingEntered:",         &_osx_dragging_entered, "l@:@");
	_macos_view_class.add_method("draggingUpdated:",         &_osx_dragging_updated, "l@:@");
	_macos_view_class.add_method("draggingExited:",          &_osx_dragging_ended, "v@:@");
	_macos_view_class.add_method("draggingEnded:",           &_osx_dragging_ended, "v@:@");
	_macos_view_class.add_method("prepareForDragOperation:", &_osx_prepare_for_drag_operation, "c@:@");
	_macos_view_class.add_method("performDragOperation:",    &_osx_perform_drag_operation, "c@:@");
	objc::register_new_subclass(_macos_view_class);
}
fn void _osx_key_updown(ObjId self, ObjId event, bool down)
{
	GfxWindow*? win = _fetch_win(self);
	if (catch win) return;
	ushort keyCode = event.send(ushort, "keyCode");
	if (keyCode >= 256) return;
	GfxKey key = _gfx_ctx.keycodes[(sz)keyCode];
	// Unmapped key
	if (!key) return;
	bool repeat = down ? event.send(bool, "isARepeat") : false;
	GfxKeyModifier mods = _osx_modifiers_to_gfx(event.send(NSUInteger, "modifierFlags"));
	_key_callback(win, key, mods, repeat, down);
}

fn void _osx_key_down(ObjId self, ObjSel sel, ObjId event) => _osx_key_updown(self, event, true);
fn void _osx_key_up(ObjId self, ObjSel sel, ObjId event) => _osx_key_updown(self, event, false);

fn void _osx_mouse_entered(ObjId self, ObjSel sel, ObjId event)
{
	GfxWindow*? win = _fetch_win(self);
	if (catch win) return;
	CGPoint p = event.send(CGPoint, "locationInWindow");
	_mouse_notify_callback(win, { (int)p.x, win.height - (int)p.y }, true);
}

fn void _osx_mouse_exited(ObjId self, ObjSel sel, ObjId event)
{
	if (try win = _fetch_win(self)) _mouse_notify_callback(win, win.internal.last_mouse, false);
}

fn void _osx_window_did_move(ObjId self, ObjSel sel, ObjId notification)
{
	GfxWindow*? win = _fetch_win(self);
	if (catch win) return;
	if (!win.src.window) return;
	CGRect frame = win.src.window.send(CGRect, "frame");
	CGRect content = win.src.window.send(CGRect, "contentRectForFrameRect:", frame);
	float y = flip_y((float)(content.origin.y + content.size.height - 1));
	_window_moved_callback(win, { (int)content.origin.x, (int)y });
}

fn void _osx_window_did_miniaturize(ObjId self, ObjSel sel, ObjId notification)
{
	(void)_window_minimized_callback(_fetch_win(self));
}

fn void _osx_window_did_deminiaturize(ObjId self, ObjSel sel, ObjId notification)
{
	if (try win = _fetch_win(self)) _window_restored_callback(win, win.bounds);
}

fn void _osx_window_did_become_key(ObjId self, ObjSel sel, ObjId notification)
{
	(void)_window_focus_callback(_fetch_win(self), true);
}

fn void _osx_window_will_close(ObjId self, ObjSel sel, ObjId notification)
{
	(void)_window_close_callback(_fetch_win(self));
}

fn void _osx_window_did_resize(ObjId self, ObjSel sel, ObjId notification)
{
	GfxWindow*? win = _fetch_win(self);
	if (catch win) return;
	if (!win.src.view) return;

	CGRect frame = win.src.view.send(CGRect, "frame");
	GfxSize size = { (int)frame.size.width, (int)frame.size.height };
	if (!size.x || !size.y) return;
	win.size = size;

	GfxMonitor*? mon = _window_find_monitor_native(win);
	if (catch mon) return;

	switch
	{
		case mon.mode.size.x == size.x && mon.mode.size.y - 102 <= size.y:
			_window_maximized_callback(win, { 0, 0, ...size });
		case win.internal.flags.maximize:
			_window_restored_callback(win, { ...win.origin, ...size });
	}
	_window_resized_callback(win, size);
}

fn void _osx_window_did_resign_key(ObjId self, ObjSel sel, ObjId notification)
{
	(void)_window_focus_callback(_fetch_win(self), false);
}

fn void _osx_mouse_moved_common(ObjId self, ObjId event)
{
	GfxWindow*? win = _fetch_win(self);
	if (catch win) return;
	CGPoint p = event.send(CGPoint, "locationInWindow");
	_mouse_motion_callback(win, { (int)p.x, win.height - (int)p.y });
}

fn void _osx_mouse_moved(ObjId self, ObjSel sel, ObjId event) => _osx_mouse_moved_common(self, event);
fn void _osx_mouse_dragged(ObjId self, ObjSel sel, ObjId event) => _osx_mouse_moved_common(self, event);

// private
fn void _osx_mouse_button_common(ObjId self, ObjId event, bool down, GfxMouseButton fallback)
{
	GfxWindow*? win = _fetch_win(self);
	if (catch win) return;
	NSUInteger button = event.send(NSUInteger, "buttonNumber");
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

fn void _osx_mouse_button_down(ObjId self, ObjSel sel, ObjId event) => _osx_mouse_button_common(self, event, true, LEFT);
fn void _osx_mouse_button_up(ObjId self, ObjSel sel, ObjId event) => _osx_mouse_button_common(self, event, false, LEFT);
fn void _osx_right_mouse_button_down(ObjId self, ObjSel sel, ObjId event) => _osx_mouse_button_common(self, event, true, RIGHT);
fn void _osx_right_mouse_button_up(ObjId self, ObjSel sel, ObjId event) => _osx_mouse_button_common(self, event, false, RIGHT);
fn void _osx_other_mouse_button_down(ObjId self, ObjSel sel, ObjId event) { _osx_mouse_button_common(self, event, true, MIDDLE); }
fn void _osx_other_mouse_button_up(ObjId self, ObjSel sel, ObjId event) { _osx_mouse_button_common(self, event, false, MIDDLE); }

fn void _osx_scroll_wheel(ObjId self, ObjSel sel, ObjId event)
{
	GfxWindow*? win = _fetch_win(self);
	if (catch win) return;
	CGFloat x = event.send(CGFloat, "scrollingDeltaX");
	CGFloat y = event.send(CGFloat, "scrollingDeltaY");
	_mouse_scroll_callback(win, { (float)x, (float)y });
}

fn void _osx_flags_changed(ObjId self, ObjSel sel, ObjId event)
{
	GfxWindow*? win = _fetch_win(self);
	if (catch win) return;

	GfxKey value = NULL;
	bool pressed = false;

	NSUInteger flags = event.send(NSUInteger, "modifierFlags");
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

fn void _osx_view_did_change_backing_properties(ObjId self, ObjSel sel)
{
	GfxWindow*? win = _fetch_win(self);
	if (catch win) return;
	(void)_scale_updated_callback(win, _window_find_monitor_native(win).scale);
}

fn NSUInteger _osx_dragging_entered(ObjId self, ObjSel sel, ObjId sender)
{
	GfxWindow*? win = _fetch_win(self);
	if (catch win) return 0;
	CGPoint p = sender.send(CGPoint, "draggingLocation");
	_data_drag_callback(win, FILE, ENTER, { (int)p.x, win.height - (int)p.y });
	return 1;
}

fn NSUInteger _osx_dragging_updated(ObjId self, ObjSel sel, ObjId sender)
{
	GfxWindow*? win = _fetch_win(self);
	if (catch win) return 0;
	if (!(win.internal.enabled_events & TODO_RENAME_DATA_DRAG_FLAG)) return 1;
	CGPoint p = sender.send(CGPoint, "draggingLocation");
	_data_drag_callback(win, FILE, MOVE, { (int)p.x, win.height - (int)p.y });
	return 1;
}

fn void _osx_dragging_ended(ObjId self, ObjSel sel, ObjId sender)
{
	GfxWindow*? win = _fetch_win(self);
	if (catch win) return;
	if (!(win.internal.enabled_events & TODO_RENAME_DATA_DRAG_FLAG)) return;
	CGPoint p = sender.send(CGPoint, "draggingLocation");
	_data_drag_callback(win,FILE, EXIT, { (int)p.x, win.height - (int)p.y });
}

fn bool _osx_prepare_for_drag_operation(ObjId self, ObjSel sel, ObjId sender)
{
	GfxWindow*? win = _fetch_win(self);
	if (catch win) return true;
	if (!(win.internal.enabled_events & TODO_RENAME_DATA_DROP_FLAG)) return true;
	return win.internal.flags.allow_dnd;
}

// private
fn bool _osx_perform_drag_operation(ObjId self, ObjSel sel, ObjId sender)
{
	GfxWindow*? win = _fetch_win(self);
	if (catch win) return false;
	if (!(win.internal.enabled_events & TODO_RENAME_DATA_DROP_FLAG)) return false;

	ObjId pasteBoard = sender.send(ObjId, "draggingPasteboard");
	ObjId types = pasteBoard.send(ObjId, "types");
	ObjId file_urls_type = "NSFilenamesPboardType".to_nsstring();

	if (!types.send(bool, "containsObject:", file_urls_type))
	{
		log::warn("No files found on the pasteboard", category: log::CATEGORY_VIDEO);
		return false;
	}

	ObjId propertyList = pasteBoard.send(ObjId, "propertyListForType:", file_urls_type);

	if (propertyList.send(bool, "isKindOfClass:", objc::@class("NSString")))
	{
		NSUInteger stringCount = propertyList.send(NSUInteger, "length");
		ZString filePath = darwin::string_to_zstr(propertyList);
		_data_drop_callback(win, filePath, (sz)stringCount + 1, FILE);
	}
	if (!propertyList.send(bool, "isKindOfClass:", objc::@class("NSArray"))) return false;

	NSUInteger count = propertyList.send(NSUInteger, "count");
	if (!count) return false;

	for (NSUInteger i = 0; i < count; i++)
	{
		ObjId file_url = propertyList.send(ObjId, "objectAtIndex:", i);
		ZString filePath = darwin::string_to_zstr(file_url);
		NSUInteger string_count = file_url.send(NSUInteger, "length");
		_data_drop_callback(win, filePath, (sz)string_count + 1, FILE);
	}
	return false;
}

fn bool _osx_wants_update_layer(ObjId self, ObjSel sel) => true;

fn void _osx_update_layer(ObjId self, ObjSel sel)
{
	if (try win = _fetch_win(self)) _window_refresh_callback(win, { 0, 0, ...win.size });
}

fn bool _osx_accepts_first_responder(ObjId self, ObjSel sel) => true;

fn void _osx_draw_rect(ObjId self, ObjSel sel, CGRect rect)
{
	GfxWindow*? win = _fetch_win(self);
	if (catch win) return;
	float y = flip_y((float)(rect.size.height - 1));
	_window_refresh_callback(win, { (int)rect.origin.x, (int)y, (int)rect.size.width, (int)rect.size.height });
}

// --- helpers

fn GfxWindow*? _fetch_win(ObjId self)
{
	void* out = null;
	self.get_ivar("gfx_window", &out);
	return out ?: NOT_FOUND~;
}

fn float flip_y(float y)
{
	return (float)(cg::mainDisplayID().bounds().size.height - (double)y - 1.0);
}

fn ObjId _get_layer_macos()
{
	if (!_gfx_ctx.current || !_gfx_ctx.current.src.view) return null;
	return _gfx_ctx.current.src.view.send(ObjId, "layer");
}

fn void _window_set_layer_macos(GfxWindow* win, ObjId layer)
{
	if (win.src.view) win.src.view.send_void("setLayer:", layer);
}

fn void _macos_init_view(GfxWindow* win)
{
	CGRect content_rect = { .size = { ...win.size } };

	win.src.view.send_void("setFrame:", content_rect);

	// win.src.view.send_void("setFrameAutosaveName:", <todo>);

	win.src.view.set_ivar("gfx_window", win);
	win.src.window.send_void("setContentView:", win.src.view);
	win.src.view.send_void("setWantsLayer:", true);
	win.src.view.send_void("setLayerContentsPlacement:",  4);
	ObjId tracking_area = objc::alloc_by_name("NSTrackingArea")
		.send(ObjId, "initWithRect:options:owner:userInfo:", content_rect,
		NSTrackingAreaOptions.MOUSE_ENTERED_AND_EXITED | NSTrackingAreaOptions.ACTIVE_ALWAYS | NSTrackingAreaOptions.IN_VISIBLE_RECT,
		win.src.view, null);
	win.src.view.send_void("addTrackingArea:", tracking_area);
	tracking_area.release();
}

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

fn ObjId _get_nsscreen_for_display(uint display_id)
{
	ObjId screens = objc::send(objc::@class("NSScreen"), ObjId, "screens");
	if (!screens) return null;
	NSUInteger count = screens.send(NSUInteger, "count");
	for (NSUInteger i = 0; i < count; i++)
	{
		ObjId screen = screens.send(ObjId, "objectAtIndex:", i);
		ObjId desc = screen.send(ObjId, "deviceDescription");
		ObjId number = desc.send(ObjId, "objectForKey:", darwin::string("NSScreenNumber"));
		uint display = (uint)number.send(NSUInteger, "unsignedIntValue");
		if (display == display_id) return screen;
	}
	return null;
}

fn void _init_keycodes_native()
{
	_gfx_ctx.keycodes = {
		[0x00] = A,             [0x01] = S,             [0x02] = D,
		[0x03] = F,             [0x04] = H,             [0x05] = G,
		[0x06] = Z,             [0x07] = X,             [0x08] = C,
		[0x09] = V,             [0x0B] = B,             [0x0C] = Q,
		[0x0D] = W,             [0x0E] = E,             [0x0F] = R,
		[0x10] = Y,             [0x11] = T,             [0x12] = NUM1,
		[0x13] = NUM2,          [0x14] = NUM3,          [0x15] = NUM4,
		[0x16] = NUM6,          [0x17] = NUM5,          [0x18] = EQUAL,
		[0x19] = NUM9,          [0x1A] = NUM7,          [0x1B] = MINUS,
		[0x1C] = NUM8,          [0x1D] = NUM0,          [0x1E] = CLOSE_BRACKET,
		[0x1F] = O,             [0x20] = U,             [0x21] = BRACKET,
		[0x22] = I,             [0x23] = P,             [0x24] = RETURN,
		[0x25] = L,             [0x26] = J,             [0x27] = APOSTROPHE,
		[0x28] = K,             [0x29] = SEMICOLON,     [0x2A] = BACK_SLASH,
		[0x2B] = COMMA,         [0x2C] = SLASH,         [0x2D] = N,
		[0x2E] = M,             [0x2F] = PERIOD,        [0x31] = SPACE,
		[0x32] = BACKTICK,      [0x33] = BACK_SPACE,    [0x35] = ESCAPE,
		[0x30] = TAB,           [0x7B] = LEFT,          [0x7C] = RIGHT,
		[0x7D] = DOWN,          [0x7E] = UP,            [0x38] = SHIFT_L,
		[0x3B] = CONTROL_L,     [0x3A] = ALT_L,         [0x37] = SUPER_L,
		[0x3C] = SHIFT_R,       [0x3E] = CONTROL_R,     [0x3D] = ALT_R,
		[0x36] = SUPER_R,       [0x7A] = F1,            [0x78] = F2,
		[0x63] = F3,            [0x76] = F4,            [0x60] = F5,
		[0x61] = F6,            [0x62] = F7,            [0x64] = F8,
		[0x65] = F9,            [0x6D] = F10,           [0x67] = F11,
		[0x6F] = F12,           [0x69] = PRINT_SCREEN,  [0x6B] = F14,
		[0x71] = F15,           [0x6A] = F16,           [0x40] = F17,
		[0x4F] = F18,           [0x50] = F19,           [0x5A] = F20,
		[0x0A] = WORLD1,        [0x39] = CAPS_LOCK,     [0x75] = DELETE,
		[0x77] = END,           [0x73] = HOME,          [0x72] = INSERT,
		[0x6E] = MENU,          [0x47] = NUM_LOCK,      [0x79] = PAGE_DOWN,
		[0x74] = PAGE_UP,       [0x52] = PAD0,          [0x53] = PAD1,
		[0x54] = PAD2,          [0x55] = PAD3,          [0x56] = PAD4,
		[0x57] = PAD5,          [0x58] = PAD6,          [0x59] = PAD7,
		[0x5B] = PAD8,          [0x5C] = PAD9,          [0x45] = PAD_SLASH,
		[0x41] = PAD_PERIOD,    [0x4B] = PAD_SLASH,     [0x4C] = PAD_RETURN,
		[0x51] = PAD_EQUAL,     [0x43] = PAD_MULTIPLY,  [0x4E] = PAD_MINUS,
	};
}

// -- globals

ObjClass _macos_window_delegate_class;
ObjClass _macos_view_class;
