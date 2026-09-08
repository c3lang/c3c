module std::gfx @private;

fn void? _load_egl_native() @private @weak => NOT_IMPLEMENTED~;
fn void _unload_egl_native() @private @weak { }

module std::gfx @feat(MACOS, WAYLAND, X11, X11_WAYLAND);
import libc;

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

excuse PROC_LOAD_FAILED;

// public
fn void _unload_egl_native() @private
{
	if (_gfx_ctx.egl_handle == null) return;
	if (_gfx_ctx.egl_display != null && rgfw_eglTerminate != null) rgfw_eglTerminate((EglDisplay)_gfx_ctx.egl_display);
	libc::dlclose(_gfx_ctx.egl_handle);
	_gfx_ctx.egl_handle = null;
	_gfx_ctx.egl_display = null;
	_gfx_ctx.eglGetProcAddress = null;
	rgfw_eglGetProcAddress = null;
}

// public
fn GfxFn rgfw_getProcAddress_EGL(ZString procname)
{
	if (procname == null) return null;
	if (try proc = rgfw_eglProc(procname))
	{
		return (GfxFn)proc;
	}
	return null;
}

// private
fn void*? rgfw_eglProc(ZString name)
{
	if (_gfx_ctx.egl_handle == null) return PROC_LOAD_FAILED~;
	void* proc = libc::dlsym(_gfx_ctx.egl_handle, name);
	if (!proc && rgfw_eglGetProcAddress) proc = (void*)rgfw_eglGetProcAddress(name);
	if (!proc) return PROC_LOAD_FAILED~;
	return proc;
}


// private
fn void? bind_egl() @local
{
	rgfw_eglGetDisplay = rgfw_eglProc("eglGetDisplay")!;
	rgfw_eglInitialize = rgfw_eglProc("eglInitialize")!;
	rgfw_eglTerminate = rgfw_eglProc("eglTerminate")!;
	rgfw_eglBindApi = rgfw_eglProc("eglBindAPI")!;
	rgfw_eglChooseConfig = rgfw_eglProc("eglChooseConfig")!;
	rgfw_eglCreateContext = rgfw_eglProc("eglCreateContext")!;
	rgfw_eglDestroyContext = rgfw_eglProc("eglDestroyContext")!;
	rgfw_eglCreateWindowSurface = rgfw_eglProc("eglCreateWindowSurface")!;
	rgfw_eglDestroySurface = rgfw_eglProc("eglDestroySurface")!;
	rgfw_eglMakeCurrent = rgfw_eglProc("eglMakeCurrent")!;
	rgfw_eglSwapBuffers = rgfw_eglProc("eglSwapBuffers")!;
	rgfw_eglSwapInterval = rgfw_eglProc("eglSwapInterval")!;
	rgfw_eglGetCurrentContext = rgfw_eglProc("eglGetCurrentContext")!;
	rgfw_eglQueryString = rgfw_eglProc("eglQueryString")!;
	rgfw_eglGetConfigAttrib = rgfw_eglProc("eglGetConfigAttrib")!;
}
// public
fn void? _load_egl_native() @private
{
	if (_gfx_ctx.egl_handle) return;
	_gfx_ctx.egl_handle = _dlopen_any("libEGL.dylib", "libEGL.1.dylib", "libEGL.so.1", "libEGL.so", "EGL.dll") ?? MISSING_EGL_LIB~!;
	void* getProc = libc::dlsym(_gfx_ctx.egl_handle, "eglGetProcAddress");
	if (!getProc) return MISSING_EGL_GET_PROC_ADDR~;
	rgfw_eglGetProcAddress = (EglGetProcAddressProc)getProc;
	_gfx_ctx.eglGetProcAddress = getProc;
	if (catch bind_egl()) return EGL_BINDING_FAILED~;
}

// public
fn void* rgfw_getCurrentContext_EGL() => rgfw_eglGetCurrentContext != null ? rgfw_eglGetCurrentContext() : null;
// public
fn GfxWindow* rgfw_getCurrentWindow_EGL() => _gfx_ctx.current;
// public
fn void* rgfw_getDisplay_EGL() => _gfx_ctx.egl_display;

// public
fn void rgfw_window_makeCurrentContext_EGL(GfxWindow* win)
{
	if (!_gfx_ctx.egl_display || !rgfw_eglMakeCurrent) return;
	if (!win || !win.src.ctx.egl)
	{
		rgfw_eglMakeCurrent((EglDisplay)_gfx_ctx.egl_display, null, null, null);
		return;
	}
	RgfwEglContext* ctx = win.src.ctx.egl;
	rgfw_eglMakeCurrent((EglDisplay)_gfx_ctx.egl_display, (EglSurface)ctx.surface, (EglSurface)ctx.surface, (EglContext)ctx.ctx);
}

// public
fn void rgfw_window_swapBuffers_EGL(GfxWindow* win)
{
	if (!win || !win.src.ctx.egl || !rgfw_eglSwapBuffers) return;
	rgfw_eglSwapBuffers((EglDisplay)_gfx_ctx.egl_display, (EglSurface)win.src.ctx.egl.surface);
}

// public
fn void rgfw_window_swapInterval_EGL(GfxWindow* win, int swapInterval)
{
	if (!rgfw_eglSwapInterval) return;
	rgfw_eglSwapInterval((EglDisplay)_gfx_ctx.egl_display, swapInterval);
}

