module std::gfx::egl @feat(MACOS, BSD, LINUX, WASM, WIN32) @feat(!(BSD | LINUX) | WAYLAND | !NO_X11) @private;
import libc, std::os, std::gfx @public;
import std::gfx::wl @public;
import std::collections::fixedlist;
import std::gfx::macos @public;


fn void? load_lib()
{
	if (gfx::_gfx.egl.handle) return;
	gfx::_gfx.egl.handle = os::dlopen_any("libEGL.dylib", "libEGL.1.dylib", "libEGL.so.1", "libEGL.so", "EGL.dll", opts: { .global_symbols, .safe_search }) ?? gfx::EGL_LIBRARY_NOT_FOUND~!;
	gfx::_gfx.egl.get_proc_addr_fn = os::dlsym(gfx::_gfx.egl.handle, "eglGetProcAddress") ?: gfx::EGL_MISSING_GET_PROC_ADDR~!;
	$if $defined(bind_egl):
		if (catch bind_egl()) return gfx::EGL_BINDING_FAILED~;
	$endif
}


fn void unload_lib() @private
{
	$if !$feat(WASM):
		if (!gfx::_gfx.egl.handle) return;
		if (gfx::_gfx.egl.display != null && egl::terminate_fn != null) egl::terminate_fn((EglDisplay)gfx::_gfx.egl.display);
		gfx::_gfx.egl.handle.close();
		gfx::_gfx.egl.display = null;
		gfx::_gfx.egl.get_proc_addr_fn = null;
	$endif
}

fn void*? find_proc(ZString name)
{
	if (!gfx::_gfx.egl.handle) return gfx::EGL_PROC_LOAD_FAILED~;
	void* proc = os::dlsym(gfx::_gfx.egl.handle, name);
	if (!proc && gfx::_gfx.egl.get_proc_addr_fn) proc = gfx::_gfx.egl.get_proc_addr_fn(name);
	if (!proc) return gfx::EGL_PROC_LOAD_FAILED~;
	return proc;
}

fn void* get_proc(ZString procname)
{
	return find_proc(procname) ?? null;
}

fn bool is_extension_supported(String extension) @feat(WASM) => false;

fn bool is_extension_supported(String extension) @feat(!WASM)
{
	if (gl::extension_supported_base(extension, &get_proc)) return true;
	if (!query_string_fn || !gfx::_gfx.egl.display) return false;
	ZString extensions = query_string_fn((EglDisplay)gfx::_gfx.egl.display, EGL_EXTENSIONS);
	return extensions && gfx::_extension_is_in_list(extensions.str_view(), extension);
}

fn void* get_current_context()
{
	$if $feat(WASM):
		return null;
	$else
		return get_current_context_fn ? get_current_context_fn() : null;
	$endif
}

fn void destroy_context_in_window(GfxEglContext* ctx, GfxWindow* win)
{
	$if $feat(WASM) || gfx::X11_ONLY:
		return;
	$else
		$if gfx::X11_OR_WAYLAND:
			if (!gfx::_gfx.use_wayland) return;
		$endif
		if (ctx.surface) egl::destroy_surface_fn(gfx::_gfx.egl.display, ctx.surface);
		if (ctx.ctx) egl::destroy_context_fn(gfx::_gfx.egl.display, ctx.ctx);

		$if (gfx::WAYLAND):
			if (ctx.egl_window) wl::wl_egl_window_destroy(ctx.egl_window);
			ctx.egl_window = null;
		$endif
		ctx.surface = null;
		ctx.ctx = null;
		if (win)
		{
			win.src.ctx.egl = null;
			if (win.src.backing.owned) free(ctx);
		}
	$endif

}

fn GfxEglContext*? create_context(GfxWindow* win, GfxGlHints* hints)
{
	$if $feat(WASM) || gfx::X11_ONLY:
		return gfx::EGL_NOT_SUPPORTED~;
	$else
		$if gfx::WAYLAND || $feat(MACOS):
			GfxEglContext* context = mem::new(GfxEglContext);
			defer catch free(context);
			init_window_context(win, context, hints, gfx::WAYLAND ??? wl::ctx.wl_display : null)!;
			win.src.backing.owned = true;
			return context;
		$else
			return gfx::EGL_NOT_SUPPORTED~;
		$endif
	$endif
}

fn void set_current_window(GfxWindow* win)
{
	$if $feat(WASM):
		return;
	$else
		if (!gfx::_gfx.egl.display || !make_current_fn) return;
		if (!win || !win.src.ctx.egl)
		{
			make_current_fn((EglDisplay)gfx::_gfx.egl.display, null, null, null);
			return;
		}
		GfxEglContext* ctx = win.src.ctx.egl;
		make_current_fn((EglDisplay)gfx::_gfx.egl.display, (EglSurface)ctx.surface, (EglSurface)ctx.surface, (EglContext)ctx.ctx);
	$endif
}


fn void swap_window_buffers(GfxWindow* win)
{
	$if $feat(WASM):
		return;
	$else
		if (!win.src.ctx.egl || !swap_buffers_fn) return;
		swap_buffers_fn((EglDisplay)gfx::_gfx.egl.display, (EglSurface)win.src.ctx.egl.surface);
	$endif
}

fn void set_swap_interval(GfxWindow* win, int swap_interval)
{
	$if $feat(WASM):
		return;
	$else
		if (!swap_interval_fn) return;
		swap_interval_fn((EglDisplay)gfx::_gfx.egl.display, swap_interval);
	$endif
}

fn void? init_window_context(GfxWindow* win, GfxEglContext* ctx, GfxGlHints* hints, void* display) @feat(WAYLAND, MACOS) @feat(MACOS, BSD, LINUX)
{
	$if $feat(WAYLAND) && !$feat(NO_X11):
		if (!gfx::_gfx.use_wayland) return gfx::EGL_NOT_SUPPORTED~;
	$endif
	load_lib()!;
	if (!gfx::_gfx.egl.display)
	{
		gfx::_gfx.egl.display = get_display_fn(display);
		if (!gfx::_gfx.egl.display) return gfx::EGL_DISPLAY_NOT_FOUND~;
		EglInt major, minor;
		if (!initialize_fn(gfx::_gfx.egl.display, &major, &minor)) return gfx::EGL_FAILED_TO_INIT_DISPLAY~;
	}

	FixedList{EglInt, 32} stack;
	stack.push_all({
		EGL_SURFACE_TYPE,    EGL_WINDOW_BIT,
		EGL_RED_SIZE,        hints.rgba_size.r,
		EGL_GREEN_SIZE,      hints.rgba_size.g,
		EGL_BLUE_SIZE,       hints.rgba_size.b,
		EGL_ALPHA_SIZE,      hints.rgba_size.a,
		EGL_DEPTH_SIZE,      hints.depth,
		EGL_STENCIL_SIZE,    hints.stencil,
		EGL_RENDERABLE_TYPE, hints.profile == ES ? EGL_OPENGL_ES2_BIT : EGL_OPENGL_BIT,
		EGL_NONE, EGL_NONE
	});

	EglConfig config = null;
	EglInt num = 0;
	if (!choose_config_fn((EglDisplay)gfx::_gfx.egl.display, stack.array_view(), &config, 1, &num) || !num) return gfx::EGL_CONFIG_NOT_FOUND~;
	bind_api_fn(hints.profile == ES ? EGL_OPENGL_ES_API : EGL_OPENGL_API);

	stack.clear();
	if (hints.profile == ES) stack.push_all({EGL_CONTEXT_CLIENT_VERSION, hints.major != 0 ? hints.major : 2});
	stack.push_all({EGL_NONE, EGL_NONE});

	EglContext share = hints.share_egl ? hints.share_egl.ctx : null;
	ctx.ctx = create_context_fn(gfx::_gfx.egl.display, config, share, stack.array_view());
	if (!ctx.ctx) return gfx::EGL_CONTEXT_CREATION_FAILED~;
	defer catch
	{
		destroy_context_fn(gfx::_gfx.egl.display, ctx.ctx);
		ctx.ctx = null;
	}

	$switch:
		$case $feat(MACOS):
			ObjId native_window = win.src.window;
			if (win.src.view) native_window = macos::_get_layer_macos();
			ctx.surface = create_window_surface_fn((EglDisplay)gfx::_gfx.egl.display, config, (void*)native_window, null);
			if (!ctx.surface) return gfx::EGL_SURFACE_CREATION_FAILED~;
		$case gfx::WAYLAND:
			WlEglWindow* egl_window = wl::wl_egl_window_create(win.src.surface, win.width, win.height);
			ctx.egl_window = egl_window;
			ctx.surface = create_window_surface_fn(gfx::_gfx.egl.display, config, egl_window, null);
			if (!ctx.surface) return gfx::EGL_SURFACE_CREATION_FAILED~;
		$default:
			return gfx::EGL_NOT_SUPPORTED~;
	$endswitch
	win.src.ctx.egl = ctx;
	win.src.backing = { .egl };
	set_current_window(win);
}




