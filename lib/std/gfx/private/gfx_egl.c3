module std::gfx @feat(MACOS, BSD, LINUX, WASM, WIN32) @private;
import libc, std::os;

fn void? _load_egl_native() @private
{
	if (_gfx.egl.handle) return;
	_gfx.egl.handle = os::dlopen_any("libEGL.dylib", "libEGL.1.dylib", "libEGL.so.1", "libEGL.so", "EGL.dll", opts: { .global_symbols, .safe_search }) ?? MISSING_EGL_LIB~!;
	_gfx.egl.get_proc_addr_fn = os::dlsym(_gfx.egl.handle, "eglGetProcAddress") ?: MISSING_EGL_GET_PROC_ADDR~!;
	$if $defined(bind_egl):
		if (catch bind_egl()) return EGL_BINDING_FAILED~;
	$endif
}

module std::gfx @private @feat(WASM, WIN32);

fn void _unload_egl_native() @private { }
fn bool _egl_extension_is_supported_native(String extension) @private => false;
fn GfxEglContext*? _window_create_egl_context_native(GfxWindow* win, GfxGlHints* hints) => NOT_IMPLEMENTED~;
fn void _window_destroy_egl_context_native(GfxWindow* win, GfxEglContext* ctx) {}

module std::gfx @private @feat(MACOS, BSD, LINUX);
import libc, std::os, std::gfx::egl @public;
import std::collections::fixedlist;

fn void _window_destroy_egl_context_native(GfxWindow* win, GfxEglContext* ctx)
{
	_window_release_egl_context_native(win, ctx);
	if (win.src.backing.owned) free(ctx);
}

fn bool _egl_extension_is_supported_native(String extension) @private
{
	if (_opengl_extension_supported_base(extension, &_get_egl_proc_address)) return true;
	if (!egl::query_string_fn || !_gfx.egl.display) return false;
	ZString extensions = egl::query_string_fn((EglDisplay)_gfx.egl.display, egl::EGL_EXTENSIONS);
	return extensions && _extension_is_in_list(extensions.str_view(), extension);
}

excuse PROC_LOAD_FAILED;

fn void _unload_egl_native() @private
{
	if (!_gfx.egl.handle) return;
	if (_gfx.egl.display != null && egl::terminate_fn != null) egl::terminate_fn((EglDisplay)_gfx.egl.display);
	_gfx.egl.handle.close();
	_gfx.egl.display = null;
	_gfx.egl.get_proc_addr_fn = null;
}

fn void* _get_egl_proc_address(ZString procname)
{
	return _find_egl_proc(procname) ?? null;
}

fn void*? _find_egl_proc(ZString name)
{
	if (!_gfx.egl.handle) return PROC_LOAD_FAILED~;
	void* proc = os::dlsym(_gfx.egl.handle, name);
	if (!proc && _gfx.egl.get_proc_addr_fn) proc = _gfx.egl.get_proc_addr_fn(name);
	if (!proc) return PROC_LOAD_FAILED~;
	return proc;
}

fn void? _window_init_egl_context_native(GfxWindow* win, GfxEglContext* ctx, GfxGlHints* hints, void* display) @feat(WAYLAND, MACOS)
{
	$if $feat(WAYLAND) && !$feat(NO_X11):
		if (!_gfx.use_wayland) return NOT_IMPLEMENTED~;
	$endif
	_load_egl_native()!;
	if (!_gfx.egl.display)
	{
		_gfx.egl.display = egl::get_display_fn(display);
		if (!_gfx.egl.display) return EGL_DISPLAY_NOT_FOUND~;
		EglInt major, minor;
		if (!egl::initialize_fn(_gfx.egl.display, &major, &minor)) return EGL_DISPLAY_FAILED_TO_INITIALIZE~;
	}

	FixedList{EglInt, 32} stack;
	stack.push_all({
		egl::EGL_SURFACE_TYPE,    egl::EGL_WINDOW_BIT,
		egl::EGL_RED_SIZE,        hints.rgba_size.r,
		egl::EGL_GREEN_SIZE,      hints.rgba_size.g,
		egl::EGL_BLUE_SIZE,       hints.rgba_size.b,
		egl::EGL_ALPHA_SIZE,      hints.rgba_size.a,
		egl::EGL_DEPTH_SIZE,      hints.depth,
		egl::EGL_STENCIL_SIZE,    hints.stencil,
		egl::EGL_RENDERABLE_TYPE, hints.profile == ES ? egl::EGL_OPENGL_ES2_BIT : egl::EGL_OPENGL_BIT,
		egl::EGL_NONE, egl::EGL_NONE
	});

	EglConfig config = null;
	EglInt num = 0;
	if (!egl::choose_config_fn((EglDisplay)_gfx.egl.display, stack.array_view(), &config, 1, &num) || !num) return EGL_CONFIG_NOT_FOUND~;
	egl::bind_api_fn(hints.profile == ES ? egl::EGL_OPENGL_ES_API : egl::EGL_OPENGL_API);

	stack.clear();
	if (hints.profile == ES) stack.push_all({egl::EGL_CONTEXT_CLIENT_VERSION, hints.major != 0 ? hints.major : 2});
	stack.push_all({egl::EGL_NONE, egl::EGL_NONE});

	EglContext share = hints.share_egl ? hints.share_egl.ctx : null;
	ctx.ctx = egl::create_context_fn(_gfx.egl.display, config, share, stack.array_view());
	if (!ctx.ctx) return EGL_CTX_CREATION_FAILED~;
	defer catch
	{
		egl::destroy_context_fn(_gfx.egl.display, ctx.ctx);
		ctx.ctx = null;
	}

	$switch:
		$case $feat(MACOS):
			ObjId nativeWindow = win.src.window;
			if (win.src.view) nativeWindow = _get_layer_macos();
			ctx.surface = egl::create_window_surface_fn((EglDisplay)_gfx.egl.display, config, (void*)nativeWindow, null);
			if (!ctx.surface) return EGL_SURFACE_CREATION_FAILED~;
		$case X11_WAYLAND && $feat(WAYLAND):
			WlEglWindow* egl_window = wl_egl_window_create(win.src.surface, win.width, win.height);
			ctx.egl_window = egl_window;
			ctx.surface = egl::create_window_surface_fn(_gfx.egl.display, config, egl_window, null);
			if (!ctx.surface) return EGL_SURFACE_CREATION_FAILED~;
		$default:
			return NOT_IMPLEMENTED~;
	$endswitch
	win.src.ctx.egl = ctx;
	win.src.backing = { .egl };
	_window_make_current_egl_context(win);
}

fn GfxEglContext*? _window_create_egl_context_native(GfxWindow* win, GfxGlHints* hints)
{
	$if (X11_WAYLAND && $feat(WAYLAND)) || $feat(MACOS):
		GfxEglContext* context = mem::new(GfxEglContext);
		defer catch free(context);
		_window_init_egl_context_native(win, context, hints, X11_WAYLAND ??? _gfx.wlDisplay : null)!;
		win.src.backing.owned = true;
		return context;
	$else
		return NOT_IMPLEMENTED~;
	$endif
}



fn void* _get_current_egl_context() => egl::get_current_context_fn ? egl::get_current_context_fn() : null;

fn void _window_make_current_egl_context(GfxWindow* win)
{
	if (!_gfx.egl.display || !egl::make_current_fn) return;
	if (!win || !win.src.ctx.egl)
	{
		egl::make_current_fn((EglDisplay)_gfx.egl.display, null, null, null);
		return;
	}
	GfxEglContext* ctx = win.src.ctx.egl;
	egl::make_current_fn((EglDisplay)_gfx.egl.display, (EglSurface)ctx.surface, (EglSurface)ctx.surface, (EglContext)ctx.ctx);
}

fn void _window_swap_egl_buffers(GfxWindow* win)
{
	if (!win || !win.src.ctx.egl || !egl::swap_buffers_fn) return;
	egl::swap_buffers_fn((EglDisplay)_gfx.egl.display, (EglSurface)win.src.ctx.egl.surface);
}

fn void _window_egl_swap_interval(GfxWindow* win, int swap_interval)
{
	if (!egl::swap_interval_fn) return;
	egl::swap_interval_fn((EglDisplay)_gfx.egl.display, swap_interval);
}

