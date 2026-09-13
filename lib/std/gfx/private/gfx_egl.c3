module std::gfx @private @feat(MACOS, WAYLAND, X11, X11_WAYLAND, WASM, WIN32);

fn void? _load_egl_native() @private @weak => NOT_IMPLEMENTED~;
fn void _unload_egl_native() @private @weak { }


fn void _window_destroy_egl_context_native(GfxWindow* win, GfxEglContext* ctx)
{
	_window_release_egl_context_native(win, ctx);
	if (win.src.backing.owned) free(ctx);
}

fn bool _egl_extension_is_supported_native(String extension) @private
{
	$if $feat(WASM) || $feat(WIN32):
		return false;
	$else
		if (_opengl_extension_supported_base(extension, &_get_egl_proc_address)) return true;
		if (!egl_query_string_fn || !_gfx_ctx.egl_display) return false;
		ZString extensions = egl_query_string_fn((EglDisplay)_gfx_ctx.egl_display, EGL_EXTENSIONS);
		return extensions && _extension_is_in_list(extensions.str_view(), extension);
	$endif
}

fn bool _egl_extension_is_supported_native(String extension) @private @weak => false;

module std::gfx @feat(MACOS, WAYLAND, X11, X11_WAYLAND);
import libc;

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

alias EglDisplay = void*;
alias EglContext = void*;
alias EglSurface = void*;
alias EglConfig = void*;
alias EglNativeDisplayType = void*;
alias EglNativeWindowType = void*;
alias EglBoolean = uint;
alias EglInt = int;
alias EglGetProcAddressProc = fn GfxFn(ZString procname);
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
alias EglQueryStringProc = fn ZString(EglDisplay display, EglInt name);
alias EglGetConfigAttribProc = fn EglBoolean(EglDisplay display, EglConfig config, EglInt attribute, EglInt* value);

EglGetProcAddressProc _egl_get_proc_address;
EglGetDisplayProc egl_get_display_fn;
EglInitializeProc egl_initialize_fn;
EglTerminateProc egl_terminate_fn;
EglBindApiProc egl_bind_api_fn;
EglChooseConfigProc egl_choose_config_fn;
EglCreateContextProc egl_create_context_fn;
EglDestroyContextProc egl_destroy_context_fn;
EglCreateWindowSurfaceProc egl_create_window_surface_fn;
EglDestroySurfaceProc egl_destroy_surface_fn;
EglMakeCurrentProc egl_make_current_fn;
EglSwapBuffersProc egl_swap_buffers_fn;
EglSwapIntervalProc egl_swap_interval_fn;
EglGetCurrentContextProc egl_get_current_context_fn;
EglQueryStringProc egl_query_string_fn;
EglGetConfigAttribProc egl_get_config_attrib_fn;

excuse PROC_LOAD_FAILED;


// public
fn void _unload_egl_native() @private
{
	if (_gfx_ctx.egl_handle == null) return;
	if (_gfx_ctx.egl_display != null && egl_terminate_fn != null) egl_terminate_fn((EglDisplay)_gfx_ctx.egl_display);
	libc::dlclose(_gfx_ctx.egl_handle);
	_gfx_ctx.egl_handle = null;
	_gfx_ctx.egl_display = null;
	_gfx_ctx.eglGetProcAddress = null;
	_egl_get_proc_address = null;
}

fn void* _get_egl_proc_address(ZString procname)
{
	return _find_egl_proc(procname) ?? null;
}

fn void*? _find_egl_proc(ZString name)
{
	if (!_gfx_ctx.egl_handle) return PROC_LOAD_FAILED~;
	void* proc = libc::dlsym(_gfx_ctx.egl_handle, name);
	if (!proc && _egl_get_proc_address) proc = (void*)_egl_get_proc_address(name);
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

fn void? bind_egl() @local
{
	egl_get_display_fn           = _find_egl_proc("eglGetDisplay")!;
	egl_initialize_fn            = _find_egl_proc("eglInitialize")!;
	egl_terminate_fn             = _find_egl_proc("eglTerminate")!;
	egl_bind_api_fn              = _find_egl_proc("eglBindAPI")!;
	egl_choose_config_fn         = _find_egl_proc("eglChooseConfig")!;
	egl_create_context_fn        = _find_egl_proc("eglCreateContext")!;
	egl_destroy_context_fn       = _find_egl_proc("eglDestroyContext")!;
	egl_create_window_surface_fn = _find_egl_proc("eglCreateWindowSurface")!;
	egl_destroy_surface_fn       = _find_egl_proc("eglDestroySurface")!;
	egl_make_current_fn          = _find_egl_proc("eglMakeCurrent")!;
	egl_swap_buffers_fn          = _find_egl_proc("eglSwapBuffers")!;
	egl_swap_interval_fn         = _find_egl_proc("eglSwapInterval")!;
	egl_get_current_context_fn   = _find_egl_proc("eglGetCurrentContext")!;
	egl_query_string_fn          = _find_egl_proc("eglQueryString")!;
	egl_get_config_attrib_fn     = _find_egl_proc("eglGetConfigAttrib")!;
}

// public
fn void? _load_egl_native() @private
{
	if (_gfx_ctx.egl_handle) return;
	_gfx_ctx.egl_handle = _dlopen_any("libEGL.dylib", "libEGL.1.dylib", "libEGL.so.1", "libEGL.so", "EGL.dll") ?? MISSING_EGL_LIB~!;
	void* getProc = libc::dlsym(_gfx_ctx.egl_handle, "eglGetProcAddress");
	if (!getProc) return MISSING_EGL_GET_PROC_ADDR~;
	_egl_get_proc_address = (EglGetProcAddressProc)getProc;
	_gfx_ctx.eglGetProcAddress = getProc;
	if (catch bind_egl()) return EGL_BINDING_FAILED~;
}

fn void* _get_current_egl_context() => egl_get_current_context_fn ? egl_get_current_context_fn() : null;

fn void _window_make_current_egl_context(GfxWindow* win)
{
	if (!_gfx_ctx.egl_display || !egl_make_current_fn) return;
	if (!win || !win.src.ctx.egl)
	{
		egl_make_current_fn((EglDisplay)_gfx_ctx.egl_display, null, null, null);
		return;
	}
	GfxEglContext* ctx = win.src.ctx.egl;
	egl_make_current_fn((EglDisplay)_gfx_ctx.egl_display, (EglSurface)ctx.surface, (EglSurface)ctx.surface, (EglContext)ctx.ctx);
}

fn void _window_swap_egl_buffers(GfxWindow* win)
{
	if (!win || !win.src.ctx.egl || !egl_swap_buffers_fn) return;
	egl_swap_buffers_fn((EglDisplay)_gfx_ctx.egl_display, (EglSurface)win.src.ctx.egl.surface);
}

fn void _window_egl_swap_interval(GfxWindow* win, int swapInterval)
{
	if (!egl_swap_interval_fn) return;
	egl_swap_interval_fn((EglDisplay)_gfx_ctx.egl_display, swapInterval);
}

