module std::gfx::macos @feat(MACOS) @private @link("Cocoa.framework", "AppKit.framework");
import std::os::macos, std::math, std::time, std::os::darwin, std::collections, std::gfx::egl @public;
import std::gfx::gl @public;

struct GfxMacosCtx
{
	ObjId app;
	void* pool;
}

GfxMacosCtx ctx;

// --- Gfx

fn void? init_platform(String class_name, GfxInitOption flags)
{
	ctx = { .pool = objc::pool_push() };
	ObjId app = ctx.app = macos::nsapplication().send(ObjId, "sharedApplication");
	if (!app) return gfx::NSAPP_CREATION_FAILED~;
	app.send_void("setActivationPolicy:", NSApplicationActivationPolicy.REGULAR);
	app.send_void("finishLaunching");
	_osx_register_classes()!;
}

fn void destroy_platform()
{
	if (ctx.pool)
	{
		objc::pool_pop(ctx.pool);
	}
	ctx = {};
}

fn void wake() => objc::@autoreleasepool()
{
	if (!ctx.app) return;
	ObjId ev = macos::nsevent().send(ObjId,
		"otherEventWithType:location:modifierFlags:timestamp:windowNumber:context:subtype:data1:data2:",
		NSEventType.APPLICATION_DEFINED, (CGPoint){ 0, 0 }, (NSUInteger)0, (CFTimeInterval)0,
		(NSInteger)0, (ObjId)null, (short)0, (NSInteger)0, (NSInteger)0);
	if (ev) ctx.app.send_void("postEvent:atStart:", ev, true);
}

fn void poll_events_platform(bool reset_state = true)
{
	objc::@autoreleasepool()
	{
		if (reset_state) gfx::_reset_prev_state();
		if (!ctx.app) return;
		ObjId distantPast = darwin::nsdate().send(ObjId, "distantPast");

		while (true)
		{
			objc::@autoreleasepool()
			{
				ObjId ev = ctx.app.send(ObjId, "nextEventMatchingMask:untilDate:inMode:dequeue:", NSEventMask.ANY, distantPast, cf::KCF_RUNLOOP_DEFAULT_MODE, true);
				if (!ev) break;
				ctx.app.send_void("sendEvent:", ev);
			};
		}
	};

}

<* @require duration > time::DURATION_ZERO *>
fn void _wait_platform(Duration duration) => objc::@autoreleasepool()
{
	if (!ctx.app) return;
	CFTimeInterval time = duration.to_sec();
	if (duration == gfx::WAIT_FOREVER) time = CFTimeInterval::max;
	ObjId date = darwin::nsdate().send(ObjId, "dateWithTimeIntervalSinceNow:", time);
	ObjId ev = ctx.app.send(ObjId, "nextEventMatchingMask:untilDate:inMode:dequeue:", NSEventMask.ANY, date, cf::KCF_RUNLOOP_DEFAULT_MODE, true);
	if (ev)
	{
		gfx::_reset_prev_state();
		ctx.app.send_void("sendEvent:", ev);
		poll_events_platform(reset_state: false);
	}
}


// --- GfxMonitor functions

fn GfxGammaRamp? monitor_get_gamma_ramp(GfxMonitor* monitor, Allocator a) => @pool()
{
	CGDirectDisplayID display = _monitor_display(monitor);
	sz count = (sz)display.gammaTableCapacity();
	if (!count) return gfx::CANNOT_RETRIEVE_GAMMA~;
	float[] rgb = mem::temp_array(float, (sz)count * 3);
	uint actual = 0;
	float[] red = rgb[:count];
	float[] green = rgb[count:count];
	float[] blue = rgb[count * 2:count];
	if (!display.transferByTable((uint)count, red, green, blue, &actual)) return gfx::CANNOT_RETRIEVE_GAMMA~;
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

fn GfxMonitorMode[] monitor_get_modes(GfxMonitor* monitor)
{
	CFArrayRef list = _monitor_display(monitor).copyAllDisplayModes(null);
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

fn GfxRect monitor_get_workarea(GfxMonitor* monitor)
{
	ObjId screen = monitor.node ? (ObjId)monitor.node.screen : null;
	if (!screen)
	{
		CGRect bounds = _monitor_display(monitor).bounds();
		return { (int)bounds.origin.x, (int)bounds.origin.y, (int)bounds.size.width, (int)bounds.size.height };
	}
	CGRect frame = screen.send(CGRect, "visibleFrame");
	return {
		(int)frame.origin.x,
		(int)flip_y((float)(frame.origin.y + frame.size.height - 1.0)),
		(int)frame.size.width,
		(int)frame.size.height
	};
}

fn void monitor_node_free(GfxMonitorNode* node)
{
	if (node.mon.mode.src) CFType.release(node.mon.mode.src);
}

fn void? monitor_request_mode(GfxMonitor* mon, GfxMonitorMode* mode, GfxMonitorModeRequest request)
{
	return monitor_set_mode(mon, mode);
}

fn void? monitor_set_gamma(GfxMonitor* monitor, float gamma) => @pool()
{
	CGDirectDisplayID display = _monitor_display(monitor);
	uint len = display.gammaTableCapacity();
	if (!len) return gfx::CANNOT_RETRIEVE_GAMMA~;
	float[] gamma_array = mem::temp_array(float, len);
	foreach (i, &g : gamma_array)
	{
		*g = gfx::_gamma_for_index(gamma, i, len) / 65535.0f;
	}
	if (!_monitor_display(monitor).setTransferByTable((uint)len, gamma_array, gamma_array, gamma_array)) return gfx::GAMMA_COULD_NOT_BE_SET~;
}

fn void? monitor_set_gamma_ramp(GfxMonitor* monitor, GfxGammaRamp ramp) => @pool()
{
	if (!ramp.red) return gfx::GAMMA_COULD_NOT_BE_SET~;
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
	if (!_monitor_display(monitor).setTransferByTable((uint)len, rgb.ptr, g, b)) return gfx::GAMMA_COULD_NOT_BE_SET~;
}

fn void? monitor_set_mode(GfxMonitor* mon, GfxMonitorMode* mode)
{
	if (!mode.src) return gfx::MONITOR_MODE_CHANGE_FAILED~;
	if (_monitor_display(mon).setDisplayMode((CGDisplayModeRef)mode.src, null)) return gfx::MONITOR_MODE_CHANGE_FAILED~;
}

fn void poll_monitors() => @pool()
{
	uint count = 0;
	if (cg::getActiveDisplayList(0, null, &count) != 0) return;
	CGDirectDisplayID[] displays = {};
	while (count != displays.len)
	{
		displays = mem::temp_array(CGDirectDisplayID, count);
		if (cg::getActiveDisplayList(count, displays, &count) != 0) return;
	}

	for (GfxMonitorNode* node = gfx::_gfx.monitors.list.head; node; node = node.next) node.disconnected = true;

	CGDirectDisplayID primary = cg::mainDisplayID();
	foreach (display : displays)
	{
		uint unit = display.unitNumber();
		ObjId screen = _get_nsscreen_for_unit(unit);

		GfxMonitorNode* node = null;
		for (node = gfx::_gfx.monitors.list.head; node != null; node = node.next)
		{
			if (node.num == unit) break;
		}
		if (node)
		{
			node.screen = (void*)screen;
			node.display = display;
			node.disconnected = false;
			if (display == primary) gfx::_gfx.monitors.primary = node;
			continue;
		}

		GfxMonitor mon;
		_monitor_from_display(display, screen, &mon);
		node = gfx::_monitor_add(&mon);
		node.screen = (void*)screen;
		node.display = display;
		node.num = unit;
		if (display == primary) gfx::_gfx.monitors.primary = node;
		gfx::_monitor_callback(gfx::_gfx.root, &node.mon, true);
	}
	gfx::refresh_monitors();
}

// --- GfxMouse and mouse related

fn GfxMouse*? create_mouse(void* data, GfxSize size, GfxFormat format)
{
	if (!data) return gfx::IMAGE_COULD_NOT_LOAD~;
	ObjId rep = _new_nsbitmap_image_rep(...size);
	defer objc::release(rep);
	char* pixels = rep.send(char*, "bitmapData");
	if (!pixels) return gfx::IMAGE_COULD_NOT_LOAD~;
	gfx::copy_image_data(pixels, ...size, RGBA8, data, format, null);
	ObjId image = _new_nsimage(...size, rep);
	CGPoint hot;
	hot.x = 0.0; hot.y = 0.0;
	ObjId cursor = macos::nscursor().alloc().send(ObjId, "initWithImage:hotSpot:", image, hot);
	objc::release(image);
	if (!cursor) return gfx::IMAGE_COULD_NOT_LOAD~;
	return (GfxMouse*)cursor;
}

fn GfxMouse*? create_standard_mouse(GfxMouseIcon mouse)
{
	ObjClass cls = macos::nscursor();
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
	if (!cursor) return gfx::MOUSE_NOT_SUPPORTED~;
	return (GfxMouse*)objc::retain(cursor);
}

fn void free_mouse(GfxMouse* mouse)
{
	objc::release((ObjId)mouse);
}

fn GfxPoint get_global_mouse()
{
	CGEventRef event = cg::eventCreate(null);
	if (!event) return { -1, -1 };
	defer event.release();
	CGPoint point = event.getLocation();
	return { (int)point.x, (int)point.y };
}

// --- GfxWindow

fn void window_blit_surface(GfxWindow* win, GfxSurface* surface)
{
	if (!surface.data) return;
	objc::@autoreleasepool()
	{
		GfxPoint min = math::min(win.size, surface.size);
		ObjId rep = _new_nsbitmap_image_rep(...min);
		char* pixels = rep.send(char*, "bitmapData");
		defer objc::release(rep);
		if (!pixels) return;
		gfx::copy_image_data(pixels, ...min, RGBA8, surface.data, surface.format, surface.convert_fn);
		ObjId image = _new_nsimage(...min, rep);
		win.src.view.send_void("setWantsLayer:", true);
		ObjId layer = win.src.view.send(ObjId, "layer");
		layer.send_void("setContentsScale:", (CGFloat)1.0);
		layer.send_void("setContents:", image);
		objc::release(image);
	};
}

fn void? window_create_platform(GfxWindow* win, String name, GfxWindowFlags flags)
{
	NSWindowStyleMask style = TITLED | CLOSABLE | MINIATURIZABLE;
	if (!flags.no_resize) style |= RESIZABLE;
	if (flags.no_border) style = BORDERLESS;
	CGRect rect = { { win.x, flip_y((float)(win.y + win.height - 1)) } , { ...win.size }};
	ObjId nswin = macos::nswindow().alloc().send(ObjId, "initWithContentRect:styleMask:backing:defer:", rect, style, NSBackingStoreType.BUFFERED, false);
	if (!nswin) return gfx::NSWINDOW_CREATION_FAILED~;
	ObjId view = _macos_view_class ? _macos_view_class.alloc() : macos::nsview().alloc();
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
	window_set_floating(win, flags.floating);
	if (!(flags.hide)) window_show_platform(win);
	if (flags.maximize) window_maximize_platform(win);
	if (flags.minimize) window_minimize_platform(win);
	if (flags.fullscreen) window_set_fullscreen(win, true);

	gfx::_window_set_flags(win, flags, {});
	win.src.backing = {};
	if (flags.open_gl) win.create_opengl_context(gfx::get_global_opengl_hints())!;
	if (flags.egl) egl::create_context(win, gfx::get_global_opengl_hints())!;
	_macos_init_view(win);
}

fn void window_destroy_platform(GfxWindow* win)
{
	_osx_stop_resize_timer(win);
	win.src.window.send_void("close");
	win.src.delegate.release();
	win.src.view.release();
	win.src.window.release();
	win.src.delegate = null;
	win.src.view = null;
	win.src.window = null;
}

fn GfxPoint window_fetch_size(GfxWindow* win)
{
	if (win.src.view)
	{
		CGRect frame = win.src.view.send(CGRect, "frame");
		win.size = { (int)frame.size.width, (int)frame.size.height };
	}
	return win.size;
}

fn GfxMonitor*? window_find_monitor(GfxWindow* win) => gfx::get_primary_monitor();

fn void window_flash(GfxWindow* win, GfxFlashRequest request)
{
	ctx.app.send(NSInteger, "requestUserAttention:", (NSUInteger)request);
}

fn void window_focus(GfxWindow* win)
{
	win.src.window.send_void("makeKeyWindow", null);
	win.internal.in_focus = true;
}

fn void window_hide(GfxWindow* win)
{
	win.src.window.send_void("orderOut:", null);
	win.internal.flags.hide = true;
}

fn bool window_is_floating(GfxWindow* win)
{
	if (!win) return false;
	if (win.src.window) return win.src.window.send(NSInteger, "level") > NSWindowLevel.NORMAL;
	return win.internal.flags.floating;
}

fn bool window_is_hidden(GfxWindow* win) @private
{
	if (win.src.window) return !win.src.window.send(bool, "isVisible");
	return win.internal.flags.hide;
}

fn bool window_is_maximized(GfxWindow* win)
{
	if (win.src.window) return win.src.window.send(bool, "isZoomed");
	return win.internal.flags.maximize;
}

fn bool window_is_minimized(GfxWindow* win)
{
	if (win.src.window) return win.src.window.send(bool, "isMiniaturized");
	return win.internal.flags.minimize;
}

fn void window_maximize_platform(GfxWindow* win)
{
	win.src.window.send_void("zoom:", null);
}

fn void window_minimize_platform(GfxWindow* win)
{
	win.src.window.send_void("miniaturize:", null);
}

fn void window_move_mouse(GfxWindow* win, GfxPoint where)
{
	cg::warpMouseCursorPosition({ ...where });
	if (win) win.internal.last_mouse = where;
}

fn void window_move_platform(GfxWindow* win, GfxPoint where)
{
	if (!win.src.window) return;
	CGRect content = win.src.view.send(CGRect, "frame");
	win.x = where.x;
	win.y = (int)flip_y((float)where.y + (float)content.size.height - 1.0f);
	CGPoint origin = { ...where };
	win.src.window.send_void("setFrameOrigin:", origin);
}

fn void window_raise(GfxWindow* win)
{
	win.src.window.send_void("orderFrontRegardless", null);
}

fn void window_resize_platform(GfxWindow* win, GfxSize size)
{
	if (!win.src.window) return;
	CGRect frame = win.src.window.send(CGRect, "frame");
	CGRect content = win.src.window.send(CGRect, "contentRectForFrameRect:", frame);
	double offset = frame.size.height - content.size.height;
	win.src.window.send_void("setFrame:display:", (CGRect){{ ...win.origin }, { size.x, (double)size.y + offset}}, true);
}

fn void window_restore_platform(GfxWindow* win)
{
	if (!win.src.window) return;
	win.src.window.send_void("deminiaturize:", null);
	win.internal.flags.minimize = false;
	win.internal.flags.maximize = false;
}

fn void window_set_aspect_ratio(GfxWindow* win, int w, int h) @private
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

fn void window_set_border(GfxWindow* win, bool border)
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

fn void window_set_capture_mouse_platform(GfxWindow* win, bool state)
{
	cg::associateMouseAndMouseCursorPosition(!state);
}

fn void window_set_floating(GfxWindow* win, bool floating)
{
	if (win.src.window) win.src.window.send_void("setLevel:", floating ? NSWindowLevel.FLOATING : NSWindowLevel.NORMAL);
	win.internal.flags.floating = floating;
}

fn void window_set_fullscreen(GfxWindow* win, bool fullscreen)
{
	if (win.src.window) win.src.window.send_void("toggleFullScreen:", null);
	win.internal.flags.fullscreen = fullscreen;
}

fn bool window_set_icon(GfxWindow* win, void* data, int w, int h, GfxFormat format, GfxWindowIconType type)
{
	if (!ctx.app) return false;
	if (!data)
	{
		ctx.app.send_void("setApplicationIconImage:", null);
		return true;
	}
	ObjId rep = _new_nsbitmap_image_rep(w, h);
	defer objc::release(rep);
	char* pixels = rep.send(char*, "bitmapData");
	if (!pixels) return false;
	gfx::copy_image_data(pixels, w, h, RGBA8, data, format, null);
	ObjId image = _new_nsimage(w, h, rep);
	defer objc::release(image);
	ctx.app.send_void("setApplicationIconImage:", image);
	return true;
}

fn void window_set_max_size(GfxWindow* win, GfxSize size)
{
	if (!win.src.window) return;
	if (size == { 0, 0 })
	{
		if (try mon = window_find_monitor(win)) size = mon.mode.size;
	}
	win.src.window.send_void("setMaxSize:", (CGSize){ ...size });
}

fn void window_set_min_size(GfxWindow* win, GfxSize size)
{
	win.src.window.send_void("setMinSize:", (CGSize) { ...size });
}

fn void window_set_mouse_passthrough(GfxWindow* win, bool passthrough)
{
	if (win.src.window) win.src.window.send_void("setIgnoresMouseEvents:", passthrough);
}

fn void window_set_mouse_platform(GfxWindow* win, GfxMouse* mouse)
{
	ObjId mouse_native = (ObjId)mouse;
	cg::mainDisplayID().showCursor();
	mouse_native.send_void("set");
	win.src.mouse = mouse_native;
}

fn void window_set_name(GfxWindow* win, String name)
{
	win.src.window.send_void("setTitle:", name.to_nsstring());
}

fn void window_set_opacity(GfxWindow* win, float opacity)
{
	win.src.window.send_void("setAlphaValue:", (CGFloat)opacity);
}

fn void window_set_raw_mouse_mode_platform(GfxWindow* win, bool state)
{
	gfx::_gfx.raw_mouse = state;
}

fn void window_show_platform(GfxWindow* win)
{
	if (!win.src.window) return;
	win.src.window.send_void("makeKeyAndOrderFront:", null);
	if (ctx.app) ctx.app.send_void("activateIgnoringOtherApps:", true);
	win.internal.flags.hide = false;
}

// Window - opengl

fn void? window_init_opengl_context_platform(GfxWindow* win, GfxGlContext* ctx, GfxGlHints* hints)
{
	FixedList{int, 40} stack;
	int color_bits = hints.rgba_size.sum();
	stack.push_all({
		NS_OPEN_GL_PFA_COLOR_SIZE, color_bits,
		NS_OPEN_GL_PFA_ALPHA_SIZE, hints.rgba_size.a,
		NS_OPEN_GL_PFA_DEPTH_SIZE, hints.depth,
		NS_OPEN_GL_PFA_STENCIL_SIZE, hints.stencil,
		NS_OPEN_GL_PFA_AUX_BUFFERS, hints.aux_buffers,

	});
	if (hints.samples)
	{
		stack.push_all({ NS_OPEN_GL_PFA_SAMPLE_BUFFERS, 1 , NS_OPEN_GL_PFA_SAMPLES, hints.samples });
	}
	else
	{
		stack.push_all({ NS_OPEN_GL_PFA_SAMPLE_BUFFERS, 0 });
	}
	if (hints.double_buffer) stack.push(NS_OPEN_GL_PFA_DOUBLE_BUFFER);
	if (hints.stereo) stack.push(NS_OPEN_GL_PFA_STEREO);
	stack.push(NS_OPEN_GL_PFA_OPEN_GL_PROFILE);
	stack.push(hints.major >= 4 ? NS_OPEN_GL_PROFILE_4_1_CORE : (hints.major >= 3 ? NS_OPEN_GL_PROFILE_3_2_CORE : NS_OPEN_GL_PROFILE_LEGACY));
	if (hints.major <= 2)
	{
		stack.push_all({ NS_OPEN_GL_PFA_ACCUM_SIZE, hints.accum_size.sum() });
	}
	if (hints.renderer == SOFTWARE)
	{
		stack.push_all({ NS_OPEN_GL_PFA_RENDERER_ID, K_CGL_RENDERER_GENERIC_FLOAT_ID });
	}
	else
	{
		stack.push(NS_OPEN_GL_PFA_ACCELERATED);
	}
	stack.push_all({0, 0});

	ObjId format = macos::nsopengl_pixel_format().alloc().send(ObjId, "initWithAttributes:", stack.array_view());
	if (!format) return gfx::OPENGL_PIXEL_FORMAT_LOAD_FAILED~;
	defer catch objc::release(format);

	ObjId share = hints.share ? hints.share.ctx : null;
	ctx.ctx = macos::nsopengl_context().alloc().send(ObjId, "initWithFormat:shareContext:", format, share);
	if (!ctx.ctx) return gfx::OPENGL_CTX_CREATION_FAILED~;

	ctx.format = format;
	win.src.view.send_void("setWantsBestResolutionOpenGLSurface:", true);
	ctx.ctx.send_void("setView:", win.src.view);

	if (win.internal.flags.transparent)
	{
		int opacity = 0;
		_opengl_context_set_values((ObjId)ctx.ctx, &opacity, NS_OPEN_GL_CP_SURFACE_OPACITY);
	}
	win.src.ctx.native = ctx;
	win.src.backing = { .opengl };
	window_make_current_opengl_context(win);
	window_set_opengl_swap_interval(win, 0);
}

fn void window_make_current_opengl_context(GfxWindow* win)
{
	if (win && win.src.ctx.native && win.src.ctx.native.ctx)
	{
		win.src.ctx.native.ctx.send_void("makeCurrentContext");
	}
	else
	{
		objc::send_void(macos::nsopengl_context(), "clearCurrentContext");
	}
}

fn void window_opengl_swap_buffers(GfxWindow* win)
{
	if (win.src.ctx.native && win.src.ctx.native.ctx) win.src.ctx.native.ctx.send_void("flushBuffer");
}

fn void window_release_opengl_context_platform(GfxWindow* win, GfxGlContext* ctx)
{
	objc::release(ctx.format);
	objc::release(ctx.ctx);
	ctx.format = null;
	ctx.ctx = null;
	win.src.ctx.native = null;
}

fn void window_set_opengl_swap_interval(GfxWindow* win, int swap_interval)
{
	if (win.src.ctx.native && win.src.ctx.native.ctx)
	{
		_opengl_context_set_values((ObjId)win.src.ctx.native.ctx, &swap_interval, NS_OPEN_GL_CONTEXT_PARAMETER_SWAP_INTERVAL);
	}
}

// Vulkan

struct VkMetalSurfaceCreateInfoEXT { int sType; void* pNext; uint flags; void* pLayer; }
alias VkCreateMetalSurfaceExtFn = fn VkResult(VkInstance instance, VkMetalSurfaceCreateInfoEXT* create_info, void* allocator, VkSurfaceKHR* surface);
struct WgpuSurfaceSourceMetalLayer { WgpuChainedStruct chain; ObjId layer; }

fn VkSurfaceKHR? window_create_vulkan_surface(GfxWindow* win, VkInstance instance)
{
	VkSurfaceKHR surface;
	VkCreateSurfaceKHRProc raw = (VkCreateSurfaceKHRProc)gfx::_get_instance_proc_vulkan(instance, "vkCreateMetalSurfaceEXT");
	if (!raw) return gfx::VULKAN_NOT_LOADED~;
	ObjId layer = objc::send(darwin::cametal_layer(), ObjId, "layer");
	if (!layer) return gfx::SURFACE_CREATION_FAILED~;
	win.src.view.send_void("setWantsLayer:", true);
	_window_set_layer_macos(win, layer);
	VkMetalSurfaceCreateInfoEXT create_info;

	create_info.sType = gfx::VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT;
	create_info.pNext = null;
	create_info.flags = 0;
	create_info.pLayer = (void*)layer;
	if (raw(instance, &create_info, null, &surface)) return gfx::SURFACE_CREATION_FAILED~;
	return surface;
}

// Window WGPU

fn WGPUSurface? window_create_wgpu_surface(GfxWindow* window, WGPUInstance instance)
{
	if (!window.src.view)
	{
		log::error("NSView is NULL for macOS window", category: log::CATEGORY_VIDEO);
		return gfx::SURFACE_CREATION_FAILED~;
	}

	ObjId layer = objc::send(darwin::cametal_layer(), ObjId, "layer");
	if (!layer) return gfx::SURFACE_CREATION_FAILED~;
	window.src.view.send_void("setWantsLayer:", true);
	_window_set_layer_macos(window, layer);

	WgpuSurfaceSourceMetalLayer fromMetal;
	fromMetal.chain.next = null;
	fromMetal.chain.sType = gfx::WGPU_STYPE_SURFACE_SOURCE_METAL_LAYER;
	fromMetal.layer = layer;
	WgpuSurfaceDescriptor surfaceDesc;
	surfaceDesc.nextInChain = (WgpuChainedStruct*)&fromMetal;
	return gfx::wgpuInstanceCreateSurface(instance, &surfaceDesc) ?: gfx::SURFACE_CREATION_FAILED~;
}

// --- Clipboard

fn GfxDataTransfer? read_clipboard(Allocator allocator, GfxDataTransferType type)
{
	if (type != TEXT) return gfx::CLIPBOARD_DID_NOT_CONTAIN_TYPE~;

	ObjId pb = objc::send(macos::nspasteboard(), ObjId, "generalPasteboard");
	if (!pb) return gfx::CLIPBOARD_COULD_NOT_BE_OPENED~;

	ObjId str = pb.send(ObjId, "stringForType:", "public.utf8-plain-text".to_nsstring());
	if (!str) return gfx::CLIPBOARD_DID_NOT_CONTAIN_TYPE~;

	ZString raw = darwin::string_to_zstr(str);
	if (!raw) return gfx::CLIPBOARD_DID_NOT_CONTAIN_TYPE~;

	return { .text = raw.str_view().copy(allocator), .type = TEXT };
}

fn void? write_clipboard(GfxDataTransfer* data)
{
	if (data.type != TEXT || !data.text) return NOT_IMPLEMENTED~;
	ObjId pb = objc::send(macos::nspasteboard(), ObjId, "generalPasteboard");
	if (!pb) return gfx::CLIPBOARD_COULD_NOT_BE_OPENED~;
	pb.send_void("clearContents");

	if (!pb.send(bool, "setString:forType:", data.text.to_nsstring(), "public.utf8-plain-text".to_nsstring()))
	{
		return gfx::CLIPBOARD_WRITE_FAILED~;
	}
}

// --- GfxSurface

fn void? surface_init(GfxSurface* surface, void* data, int w, int h, GfxFormat format, GfxWindow* window)
{
	surface.data = data;
	surface.size = { w, h };
	surface.format = format;
	surface.native.format = RGBA8;
	surface.native.buffer = malloc((sz)w * h * 4);
}

fn void surface_release(GfxSurface* surface)
{
	if (!surface.data) return;
	if (surface.native.buffer) free(surface.native.buffer);
	*surface = {};
}

// --- Vulkan

fn bool get_vulkan_physical_device_presentation_support(VkInstance instance, VkPhysicalDevice physical_device, uint queue_family_index)
{
	return false;
}

// --- Opengl

fn void* get_current_opengl_context()
{
	return (void*)objc::send(macos::nsopengl_context(), ObjId, "currentContext");
}

fn void* get_opengl_proc(ZString procname)
{
	if (!gfx::_gfx.gl.native_handle || !procname) return null;
	CFStringRef symbol_name = cf::stringCreateWithCString(null, procname, 0x0600);
	if (!symbol_name) return null;
	defer symbol_name.release();
	return cf::bundleGetFunctionPointerForName((CFBundleRef)gfx::_gfx.gl.native_handle, symbol_name);
}

fn void? load_opengl()
{
	if (gfx::_gfx.gl.native_handle) return;
	gfx::_gfx.gl.native_handle = cf::bundle_for_name("com.apple.opengl");
	if (!gfx::_gfx.gl.native_handle) return gfx::MISSING_OPENGL_LIB~;
	gfx::_gfx.gl.get_proc_addr_fn = &get_opengl_proc;
}

fn bool opengl_extension_is_supported_platform(String extension)
{
	return false;
}

// --- Keys

fn void init_keycodes_platform()
{
	gfx::_gfx.keycodes = {
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

fn GfxKey physical_to_mapped_key(GfxKey key)
{
	ushort keycode = (ushort)key.to_api();
	TISInputSourceRef source = macos::tisCopyCurrentKeyboardLayoutInputSource();
	if (!source) return key;

	CFDataRef layoutData = source.getProperty(macos::kTISPropertyUnicodeKeyLayoutData);
	defer source.release();
	if (!layoutData) return key;

	UCKeyboardLayout* layout = (UCKeyboardLayout*)layoutData.getBytePtr();

	uint deadKeyState = 0;
	ushort[4] chars;
	uint len = 0;
	uint type = macos::lmGetKbdType();
	int status = macos::ucKeyTranslate(layout, keycode, macos::UC_KEY_ACTION_DOWN, 0, type, macos::UC_KEY_TRANSLATE_NO_DEAD_KEYS_BIT, &deadKeyState, 4, &len, &chars[0]);

	if (status == 0 && len == 1 && chars[0] < 256) return (GfxKey)chars[0];

	return key;
}

fn ObjId _get_layer_macos()
{
	if (!gfx::_gfx.current || !gfx::_gfx.current.src.view) return null;
	return gfx::_gfx.current.src.view.send(ObjId, "layer");
}
fn void _window_set_layer_macos(GfxWindow* win, ObjId layer)
{
	if (win.src.view) win.src.view.send_void("setLayer:", layer);
}

fn ObjId _get_nsscreen_for_unit(uint unit_number) @local
{
	ObjId screens = objc::send(macos::nsscreen(), ObjId, "screens");
	if (!screens) return null;
	NSUInteger count = screens.send(NSUInteger, "count");
	for (NSUInteger i = 0; i < count; i++)
	{
		ObjId screen = screens.send(ObjId, "objectAtIndex:", i);
		ObjId desc = screen.send(ObjId, "deviceDescription");
		ObjId number = desc.send(ObjId, "objectForKey:", darwin::string("NSScreenNumber"));
		CGDirectDisplayID display = (CGDirectDisplayID)number.send(NSUInteger, "unsignedIntValue");
		if (display.unitNumber() == unit_number) return screen;
	}
	return null;
}

fn void _monitor_from_display(CGDirectDisplayID display, ObjId screen, GfxMonitor* mon) @local
{
	*mon = {};
	mem::copy(&mon.name, "Display", 8);
	CGRect bounds = display.bounds();
	CGDisplayModeRef mode = display.copyDisplayMode();
	mon.origin = { (int)bounds.origin.x, (int)bounds.origin.y };
	mon.mode.size.x = mode ? (int)mode.getWidth() : (int)bounds.size.width;
	mon.mode.size.y = mode ? (int)mode.getHeight() : (int)bounds.size.height;
	mon.mode.refresh_rate = _refresh_rate(mode);
	mon.mode.rgb = { 8, 8, 8 };
	mon.mode.src = mode;
	mon.phys_size = (float[<2>])(double[<2>]){ ...display.screenSize() } / 25.4f;
	mon.pixelRatio = screen ? (float)screen.send(CGFloat, "backingScaleFactor") : 1.0f;
	mon.scale = (Vec2)mon.pixelRatio;
}

fn float _refresh_rate(CGDisplayModeRef mode) @local
{
	float rate = mode ? (float)mode.getRefreshRate() : 0;
	return rate <= 0 ? 60 : rate;
}

// -- globals

ObjClass _macos_window_delegate_class;
ObjClass _macos_view_class;

// --- helpers

fn ObjId _new_nsimage(int w, int h, ObjId rep = null) @local
{
	CGSize size = { w, h };
	ObjId image = macos::nsimage().alloc().send(ObjId, "initWithSize:", size);
	if (rep) image.send_void("addRepresentation:", rep);
	return image;
}

fn ObjId _new_nsbitmap_image_rep(int w, int h) @local
{
	return macos::nsbitmap_image_rep().alloc().send(ObjId,
		"initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bitmapFormat:bytesPerRow:bitsPerPixel:",
		(ZString*)null, (NSInteger)w, (NSInteger)h, (NSInteger)8, (NSInteger)4, true, false, macos::nsCalibratedRGBColorSpace,
		(NSUInteger)2, (NSInteger)(w * 4), (NSInteger)32);
}


fn GfxKeyModifier _osx_modifiers_to_gfx(NSUInteger flags) @local
{
	return {
		.shift     = flags & 0x00020000 != 0,
		.control   = flags & 0x00040000 != 0,
		.alt       = flags & 0x00080000 != 0,
		.super     = flags & 0x00100000 != 0,
		.caps_lock = flags & 0x00010000 != 0,
	};
}

fn float flip_y(float y)
{
	return (float)(cg::mainDisplayID().bounds().size.height - (double)y - 1.0);
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
	ObjId tracking_area = macos::nstracking_area().alloc().send(ObjId,
		"initWithRect:options:owner:userInfo:", content_rect,
		NSTrackingAreaOptions.MOUSE_ENTERED_AND_EXITED | NSTrackingAreaOptions.ACTIVE_ALWAYS | NSTrackingAreaOptions.IN_VISIBLE_RECT,
		win.src.view, null);
	win.src.view.send_void("addTrackingArea:", tracking_area);
	tracking_area.release();
}

fn GfxWindow*? _fetch_win(ObjId self) @local
{
	void* out = null;
	self.get_ivar("gfx_window", &out);
	return out ?: NOT_FOUND~;
}
// MacOS callbacks

fn void? _osx_register_classes()
{
	if (_macos_window_delegate_class) return;

	_macos_window_delegate_class = objc::nsobject().new_subclass("C3GFXWindowDelegate");
	if (!_macos_window_delegate_class) return gfx::DELEGATE_CREATION_FAILED~;
	defer catch _macos_window_delegate_class = (ObjClass)0;
	_macos_window_delegate_class.add_ivar("gfx_window", void*, "^v");

	_macos_window_delegate_class.add_method("windowWillClose:",            &_osx_window_will_close, "v@:@");
	_macos_window_delegate_class.add_method("windowDidResize:",            &_osx_window_did_resize, "v@:@");
	_macos_window_delegate_class.add_method("windowDidMove:",              &_osx_window_did_move, "v@:@");
	_macos_window_delegate_class.add_method("windowDidMiniaturize:",       &_osx_window_did_miniaturize, "v@:@");
	_macos_window_delegate_class.add_method("windowDidDeminiaturize:",     &_osx_window_did_deminiaturize, "v@:@");
	_macos_window_delegate_class.add_method("windowDidBecomeKey:",         &_osx_window_did_become_key, "v@:@");
	_macos_window_delegate_class.add_method("windowDidResignKey:",         &_osx_window_did_resign_key, "v@:@");
	_macos_window_delegate_class.add_method("windowWillStartLiveResize:",  &_osx_window_will_start_live_resize, "v@:@");
	_macos_window_delegate_class.add_method("windowDidEndLiveResize:",     &_osx_window_did_end_live_resize, "v@:@");

	_macos_window_delegate_class.add_method("resizeTimer:",                &_osx_resize_timer, "v@:@");

	objc::register_new_subclass(_macos_window_delegate_class);

	_macos_view_class = macos::nsview().new_subclass("GfxViewC3");
	if (!_macos_view_class) return gfx::DELEGATE_CREATION_FAILED~;

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
	GfxKey key = gfx::_gfx.keycodes[(sz)keyCode];
	// Unmapped key
	if (!key) return;
	bool repeat = down ? event.send(bool, "isARepeat") : false;
	GfxKeyModifier mods = _osx_modifiers_to_gfx(event.send(NSUInteger, "modifierFlags"));
	gfx::_key_callback(win, key, mods, repeat, down);
}

fn void _osx_key_down(ObjId self, ObjSel sel, ObjId event) => _osx_key_updown(self, event, true);
fn void _osx_key_up(ObjId self, ObjSel sel, ObjId event) => _osx_key_updown(self, event, false);

fn void _osx_mouse_entered(ObjId self, ObjSel sel, ObjId event)
{
	GfxWindow*? win = _fetch_win(self);
	if (catch win) return;
	CGPoint p = event.send(CGPoint, "locationInWindow");
	gfx::_mouse_notify_callback(win, { (int)p.x, win.height - (int)p.y }, true);
}

fn void _osx_mouse_exited(ObjId self, ObjSel sel, ObjId event)
{
	if (try win = _fetch_win(self)) gfx::_mouse_notify_callback(win, win.internal.last_mouse, false);
}

fn void _osx_window_did_move(ObjId self, ObjSel sel, ObjId notification)
{
	GfxWindow*? win = _fetch_win(self);
	if (catch win) return;
	if (!win.src.window) return;
	CGRect frame = win.src.window.send(CGRect, "frame");
	CGRect content = win.src.window.send(CGRect, "contentRectForFrameRect:", frame);
	float y = flip_y((float)(content.origin.y + content.size.height - 1));
	gfx::_window_moved_callback(win, { (int)content.origin.x, (int)y });
}

fn void _osx_window_did_miniaturize(ObjId self, ObjSel sel, ObjId notification)
{
	(void)gfx::_window_minimized_callback(_fetch_win(self));
}

fn void _osx_window_did_deminiaturize(ObjId self, ObjSel sel, ObjId notification)
{
	if (try win = _fetch_win(self)) gfx::_window_restored_callback(win, win.bounds);
}

fn void _osx_window_did_become_key(ObjId self, ObjSel sel, ObjId notification)
{
	(void)gfx::_window_focus_callback(_fetch_win(self), true);
}

fn void _osx_window_will_close(ObjId self, ObjSel sel, ObjId notification)
{
	(void)gfx::_window_close_callback(_fetch_win(self));
}

// AppKit runs its own event loop while the user drags a window edge, so the
// application's loop gets no control until the drag ends. A timer registered in the
// common run loop modes still fires inside that loop, which is the only chance to
// drive a redraw. Applications will get a repaint callback during resize.
fn void _osx_window_will_start_live_resize(ObjId self, ObjSel sel, ObjId notification)
{
	GfxWindow*? win = _fetch_win(self);
	if (catch win) return;
	if (!win.src.view || !win.repaint_fn) return;
	// Aim for 60 FPS while resizing, as SDL does.
	double interval = 1.0 / 60.0;
	win.src.resize_timer = darwin::nstimer().send(ObjId, "scheduledTimerWithTimeInterval:target:selector:userInfo:repeats:",
		interval, win.src.delegate, objc::sel_getUid("resizeTimer:"), null, true);
	darwin::nsrun_loop().send(ObjId, "currentRunLoop").send_void("addTimer:forMode:", win.src.resize_timer, (ObjId)cf::KCF_RUNLOOP_COMMON_MODES);
}

// A redraw only: windowDidResize: still reports the geometry changes, so this is just to ensure redraw during resize.
fn void _osx_resize_timer(ObjId self, ObjSel sel, ObjId userinfo)
{
	GfxWindow*? win = _fetch_win(self);
	if (catch win) return;
	if (!win.src.view) return;
	gfx::_window_refresh_callback(win, { 0, 0, ...win.size });
}

fn void _osx_window_did_end_live_resize(ObjId self, ObjSel sel, ObjId notification)
{
	GfxWindow*? win = _fetch_win(self);
	if (catch win) return;
	_osx_stop_resize_timer(win);
}

// The run loop owns the scheduled timer, so it is invalidated, never released.
fn void _osx_stop_resize_timer(GfxWindow* win) @local
{
	if (!win.src.resize_timer) return;
	win.src.resize_timer.send_void("invalidate");
	win.src.resize_timer = null;
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

	GfxMonitor*? mon = window_find_monitor(win);
	if (catch mon) return;

	switch
	{
		case mon.mode.size.x == size.x && mon.mode.size.y - 102 <= size.y:
			gfx::_window_maximized_callback(win, { 0, 0, ...size });
		case win.internal.flags.maximize:
			gfx::_window_restored_callback(win, { ...win.origin, ...size });
	}
	gfx::_window_resized_callback(win, size);
}

fn void _osx_window_did_resign_key(ObjId self, ObjSel sel, ObjId notification)
{
	(void)gfx::_window_focus_callback(_fetch_win(self), false);
}

fn void _osx_mouse_moved_common(ObjId self, ObjId event)
{
	GfxWindow*? win = _fetch_win(self);
	if (catch win) return;
	CGPoint p = event.send(CGPoint, "locationInWindow");
	gfx::_mouse_motion_callback(win, { (int)p.x, win.height - (int)p.y });
}

fn void _osx_mouse_moved(ObjId self, ObjSel sel, ObjId event) => _osx_mouse_moved_common(self, event);
fn void _osx_mouse_dragged(ObjId self, ObjSel sel, ObjId event) => _osx_mouse_moved_common(self, event);

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
	gfx::_mouse_button_callback(win, gfx_button, down);
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
	gfx::_mouse_scroll_callback(win, { (float)x, (float)y });
}

fn void _osx_flags_changed(ObjId self, ObjSel sel, ObjId event)
{
	GfxWindow*? win = _fetch_win(self);
	if (catch win) return;

	GfxKey value = NULL;
	bool pressed = false;

	NSUInteger flags = event.send(NSUInteger, "modifierFlags");
	gfx::_key_update_key_mods_ex(win,
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
		gfx::_gfx.keyboard[modKey].prev = gfx::_gfx.keyboard[modKey].current;
	}

	for (int i = 0; i < 5; i++)
	{
		NSUInteger shift = (NSUInteger)1 << (uint)(i + 16);
		GfxKey key = CAPS_LOCK + (GfxKey)i;
		if (flags & shift && !gfx::is_key_down(key))
		{
			pressed = true;
			value = key;
			break;
		}
		if (flags & shift == 0 && gfx::is_key_down(key))
		{
			pressed = false;
			value = key;
			break;
		}
	}

	gfx::_key_callback(win, value, win.internal.mod, gfx::is_key_down(value) && pressed, pressed);
	if (value != CAPS_LOCK)
	{
		GfxKey other = (GfxKey)(value + 4);
		gfx::_key_callback(win, other, win.internal.mod, gfx::is_key_down(other) && pressed, pressed);
	}
}

fn void _osx_view_did_change_backing_properties(ObjId self, ObjSel sel)
{
	GfxWindow*? win = _fetch_win(self);
	if (catch win) return;
	(void)gfx::_scale_updated_callback(win, window_find_monitor(win).scale);
}

fn NSUInteger _osx_dragging_entered(ObjId self, ObjSel sel, ObjId sender)
{
	GfxWindow*? win = _fetch_win(self);
	if (catch win) return 0;
	CGPoint p = sender.send(CGPoint, "draggingLocation");
	gfx::_data_drag_callback(win, FILE, ENTER, { (int)p.x, win.height - (int)p.y });
	return 1;
}

fn NSUInteger _osx_dragging_updated(ObjId self, ObjSel sel, ObjId sender)
{
	GfxWindow*? win = _fetch_win(self);
	if (catch win) return 0;
	if (!win.internal.enabled_events.data_drag) return 1;
	CGPoint p = sender.send(CGPoint, "draggingLocation");
	gfx::_data_drag_callback(win, FILE, MOVE, { (int)p.x, win.height - (int)p.y });
	return 1;
}

fn void _osx_dragging_ended(ObjId self, ObjSel sel, ObjId sender)
{
	GfxWindow*? win = _fetch_win(self);
	if (catch win) return;
	if (!win.internal.enabled_events.data_drag) return;
	CGPoint p = sender.send(CGPoint, "draggingLocation");
	gfx::_data_drag_callback(win,FILE, EXIT, { (int)p.x, win.height - (int)p.y });
}

fn bool _osx_prepare_for_drag_operation(ObjId self, ObjSel sel, ObjId sender)
{
	GfxWindow*? win = _fetch_win(self);
	if (catch win) return true;
	if (!win.internal.enabled_events.data_drop) return true;
	return win.internal.flags.allow_dnd;
}

fn bool _osx_perform_drag_operation(ObjId self, ObjSel sel, ObjId sender)
{
	GfxWindow*? win = _fetch_win(self);
	if (catch win) return false;
	if (!win.internal.enabled_events.data_drop) return false;

	ObjId pasteBoard = sender.send(ObjId, "draggingPasteboard");
	ObjId types = pasteBoard.send(ObjId, "types");
	ObjId file_urls_type = "NSFilenamesPboardType".to_nsstring();

	if (!types.send(bool, "containsObject:", file_urls_type))
	{
		log::warn("No files found on the pasteboard", category: log::CATEGORY_VIDEO);
		return false;
	}

	ObjId propertyList = pasteBoard.send(ObjId, "propertyListForType:", file_urls_type);

	if (propertyList.send(bool, "isKindOfClass:", darwin::nsstring()))
	{
		NSUInteger stringCount = propertyList.send(NSUInteger, "length");
		ZString filePath = darwin::string_to_zstr(propertyList);
		gfx::_data_drop_callback(win, filePath, (sz)stringCount + 1, FILE);
	}
	if (!propertyList.send(bool, "isKindOfClass:", darwin::nsarray())) return false;

	NSUInteger count = propertyList.send(NSUInteger, "count");
	if (!count) return false;

	for (NSUInteger i = 0; i < count; i++)
	{
		ObjId file_url = propertyList.send(ObjId, "objectAtIndex:", i);
		ZString filePath = darwin::string_to_zstr(file_url);
		NSUInteger string_count = file_url.send(NSUInteger, "length");
		gfx::_data_drop_callback(win, filePath, (sz)string_count + 1, FILE);
	}
	return false;
}

fn bool _osx_wants_update_layer(ObjId self, ObjSel sel) => true;

fn void _osx_update_layer(ObjId self, ObjSel sel)
{
	if (try win = _fetch_win(self)) gfx::_window_refresh_callback(win, { 0, 0, ...win.size });
}

fn bool _osx_accepts_first_responder(ObjId self, ObjSel sel) => true;

fn void _osx_draw_rect(ObjId self, ObjSel sel, CGRect rect)
{
	GfxWindow*? win = _fetch_win(self);
	if (catch win) return;
	float y = flip_y((float)(rect.size.height - 1));
	gfx::_window_refresh_callback(win, { (int)rect.origin.x, (int)y, (int)rect.size.width, (int)rect.size.height });
}

fn CGDirectDisplayID _monitor_display(GfxMonitor* monitor) @local
{
	if (monitor && monitor.node && monitor.node.display) return (CGDirectDisplayID)monitor.node.display;
	return cg::mainDisplayID();
}

fn void _opengl_context_set_values(ObjId context, int* vals, NSInteger param) @local
{
	context.send_void("setValues:forParameter:", vals, param);
}

const int NS_OPEN_GL_PFA_DOUBLE_BUFFER = 5;
const int NS_OPEN_GL_PFA_STEREO = 6;
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
const int NS_OPEN_GL_PROFILE_4_1_CORE = 0x4100;
const int NS_OPEN_GL_CP_SURFACE_OPACITY = 236;
const int NS_OPEN_GL_CONTEXT_PARAMETER_SWAP_INTERVAL = 222;
const int K_CGL_RENDERER_GENERIC_FLOAT_ID = 0x00020400;

module std::gfx @feat(MACOS) @private;
import std::os::macos, std::math, std::time, std::os::darwin, std::collections, std::gfx::egl @public;
import std::gfx::macos @public;



