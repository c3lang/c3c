module std::gfx @private;
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
import libc, std::os;

fn void _window_destroy_egl_context_native(GfxWindow* win, GfxEglContext* ctx)
{
	_window_release_egl_context_native(win, ctx);
	if (win.src.backing.owned) free(ctx);
}

fn bool _egl_extension_is_supported_native(String extension) @private
{
	if (_opengl_extension_supported_base(extension, &_get_egl_proc_address)) return true;
	if (!egl_query_string_fn || !_gfx.egl.display) return false;
	ZString extensions = egl_query_string_fn((EglDisplay)_gfx.egl.display, EGL_EXTENSIONS);
	return extensions && _extension_is_in_list(extensions.str_view(), extension);
}



excuse PROC_LOAD_FAILED;

fn void _unload_egl_native() @private
{
	if (!_gfx.egl.handle) return;
	if (_gfx.egl.display != null && egl_terminate_fn != null) egl_terminate_fn((EglDisplay)_gfx.egl.display);
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

fn GfxEglContext*? _window_create_egl_context_native(GfxWindow* win, GfxGlHints* hints)
{
	GfxEglContext* context = mem::new(GfxEglContext);
	defer catch free(context);
	_window_init_egl_context_native(win, context, hints)!;
	win.src.backing.owned = true;
	return context;
}



fn void* _get_current_egl_context() => egl_get_current_context_fn ? egl_get_current_context_fn() : null;

fn void _window_make_current_egl_context(GfxWindow* win)
{
	if (!_gfx.egl.display || !egl_make_current_fn) return;
	if (!win || !win.src.ctx.egl)
	{
		egl_make_current_fn((EglDisplay)_gfx.egl.display, null, null, null);
		return;
	}
	GfxEglContext* ctx = win.src.ctx.egl;
	egl_make_current_fn((EglDisplay)_gfx.egl.display, (EglSurface)ctx.surface, (EglSurface)ctx.surface, (EglContext)ctx.ctx);
}

fn void _window_swap_egl_buffers(GfxWindow* win)
{
	if (!win || !win.src.ctx.egl || !egl_swap_buffers_fn) return;
	egl_swap_buffers_fn((EglDisplay)_gfx.egl.display, (EglSurface)win.src.ctx.egl.surface);
}

fn void _window_egl_swap_interval(GfxWindow* win, int swapInterval)
{
	if (!egl_swap_interval_fn) return;
	egl_swap_interval_fn((EglDisplay)_gfx.egl.display, swapInterval);
}

