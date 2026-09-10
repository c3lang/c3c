module std::gfx @private @feat(MACOS, WAYLAND, X11, X11_WAYLAND, WASM, WIN32);

fn void? _load_egl_native() @private @weak => NOT_IMPLEMENTED~;
fn void _unload_egl_native() @private @weak { }


fn void _window_destroy_egl_context_native(GfxWindow* win, GfxEglContext* ctx)
{
	_window_release_egl_context_native(win, ctx);
	if (win.src.gfxType & TODO_RENAME_GFX_OWNED_BY_RGFW) free(ctx);
}

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

EglGetProcAddressProc todo_fix_eglGetProcAddress;
EglGetDisplayProc egl_get_display_fn;
EglInitializeProc egl_initialize_fn;
EglTerminateProc todo_fix_eglTerminate;
EglBindApiProc egl_bind_api_fn;
EglChooseConfigProc egl_choose_config_fn;
EglCreateContextProc egl_create_context_fn;
EglDestroyContextProc egl_destroy_context_fn;
EglCreateWindowSurfaceProc egl_create_window_surface;
EglDestroySurfaceProc egl_destroy_surface_fn;
EglMakeCurrentProc todo_fix_eglMakeCurrent;
EglSwapBuffersProc todo_fix_eglSwapBuffers;
EglSwapIntervalProc todo_fix_eglSwapInterval;
EglGetCurrentContextProc todo_fix_eglGetCurrentContext;
EglQueryStringProc todo_fix_eglQueryString;
EglGetConfigAttribProc todo_fix_eglGetConfigAttrib;

excuse PROC_LOAD_FAILED;


// public
fn void _unload_egl_native() @private
{
	if (_gfx_ctx.egl_handle == null) return;
	if (_gfx_ctx.egl_display != null && todo_fix_eglTerminate != null) todo_fix_eglTerminate((EglDisplay)_gfx_ctx.egl_display);
	libc::dlclose(_gfx_ctx.egl_handle);
	_gfx_ctx.egl_handle = null;
	_gfx_ctx.egl_display = null;
	_gfx_ctx.eglGetProcAddress = null;
	todo_fix_eglGetProcAddress = null;
}

// public
fn GfxFn todo_fix_getProcAddress_EGL(ZString procname)
{
	if (procname == null) return null;
	if (try proc = todo_fix_eglProc(procname))
	{
		return (GfxFn)proc;
	}
	return null;
}

// private
fn void*? todo_fix_eglProc(ZString name)
{
	if (_gfx_ctx.egl_handle == null) return PROC_LOAD_FAILED~;
	void* proc = libc::dlsym(_gfx_ctx.egl_handle, name);
	if (!proc && todo_fix_eglGetProcAddress) proc = (void*)todo_fix_eglGetProcAddress(name);
	if (!proc) return PROC_LOAD_FAILED~;
	return proc;
}

fn GfxEglContext*? _window_create_egl_context_native(GfxWindow* win, GfxGlHints* hints)
{
	GfxEglContext* context = mem::new(GfxEglContext);
	defer catch free(context);
	_window_init_egl_context_native(win, context, hints)!;
	win.src.gfxType |= TODO_RENAME_GFX_OWNED_BY_RGFW;
	return context;
}


// private
fn void? bind_egl() @local
{
	egl_get_display_fn = todo_fix_eglProc("eglGetDisplay")!;
	egl_initialize_fn = todo_fix_eglProc("eglInitialize")!;
	todo_fix_eglTerminate = todo_fix_eglProc("eglTerminate")!;
	egl_bind_api_fn = todo_fix_eglProc("eglBindAPI")!;
	egl_choose_config_fn = todo_fix_eglProc("eglChooseConfig")!;
	egl_create_context_fn = todo_fix_eglProc("eglCreateContext")!;
	egl_destroy_context_fn = todo_fix_eglProc("eglDestroyContext")!;
	egl_create_window_surface = todo_fix_eglProc("eglCreateWindowSurface")!;
	egl_destroy_surface_fn = todo_fix_eglProc("eglDestroySurface")!;
	todo_fix_eglMakeCurrent = todo_fix_eglProc("eglMakeCurrent")!;
	todo_fix_eglSwapBuffers = todo_fix_eglProc("eglSwapBuffers")!;
	todo_fix_eglSwapInterval = todo_fix_eglProc("eglSwapInterval")!;
	todo_fix_eglGetCurrentContext = todo_fix_eglProc("eglGetCurrentContext")!;
	todo_fix_eglQueryString = todo_fix_eglProc("eglQueryString")!;
	todo_fix_eglGetConfigAttrib = todo_fix_eglProc("eglGetConfigAttrib")!;
}
// public
fn void? _load_egl_native() @private
{
	if (_gfx_ctx.egl_handle) return;
	_gfx_ctx.egl_handle = _dlopen_any("libEGL.dylib", "libEGL.1.dylib", "libEGL.so.1", "libEGL.so", "EGL.dll") ?? MISSING_EGL_LIB~!;
	void* getProc = libc::dlsym(_gfx_ctx.egl_handle, "eglGetProcAddress");
	if (!getProc) return MISSING_EGL_GET_PROC_ADDR~;
	todo_fix_eglGetProcAddress = (EglGetProcAddressProc)getProc;
	_gfx_ctx.eglGetProcAddress = getProc;
	if (catch bind_egl()) return EGL_BINDING_FAILED~;
}

// public
fn void* todo_fix_getCurrentContext_EGL() => todo_fix_eglGetCurrentContext != null ? todo_fix_eglGetCurrentContext() : null;
// public
fn GfxWindow* todo_fix_getCurrentWindow_EGL() => _gfx_ctx.current;
// public
fn void* todo_fix_getDisplay_EGL() => _gfx_ctx.egl_display;

// public
fn void todo_fix_window_makeCurrentContext_EGL(GfxWindow* win)
{
	if (!_gfx_ctx.egl_display || !todo_fix_eglMakeCurrent) return;
	if (!win || !win.src.ctx.egl)
	{
		todo_fix_eglMakeCurrent((EglDisplay)_gfx_ctx.egl_display, null, null, null);
		return;
	}
	GfxEglContext* ctx = win.src.ctx.egl;
	todo_fix_eglMakeCurrent((EglDisplay)_gfx_ctx.egl_display, (EglSurface)ctx.surface, (EglSurface)ctx.surface, (EglContext)ctx.ctx);
}

// public
fn void todo_fix_window_swapBuffers_EGL(GfxWindow* win)
{
	if (!win || !win.src.ctx.egl || !todo_fix_eglSwapBuffers) return;
	todo_fix_eglSwapBuffers((EglDisplay)_gfx_ctx.egl_display, (EglSurface)win.src.ctx.egl.surface);
}

// public
fn void todo_fix_window_swapInterval_EGL(GfxWindow* win, int swapInterval)
{
	if (!todo_fix_eglSwapInterval) return;
	todo_fix_eglSwapInterval((EglDisplay)_gfx_ctx.egl_display, swapInterval);
}

fn bool todo_fix_extensionSupportedPlatform_EGL(ZString extension, sz len)
{
	if (todo_fix_eglQueryString == null || _gfx_ctx.egl_display == null) return false;
	ZString extensions = todo_fix_eglQueryString((EglDisplay)_gfx_ctx.egl_display, EGL_EXTENSIONS);
	return extensions && todo_fix_extensionSupportedStr(extensions, extension, len);
}
