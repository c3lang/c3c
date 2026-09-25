module std::gfx::egl @weak @private @feat(MACOS, BSD, LINUX, WIN32);
import std::gfx @public;

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

EglGetDisplayProc          get_display_fn;
EglInitializeProc          initialize_fn;
EglTerminateProc           terminate_fn;
EglBindApiProc             bind_api_fn;
EglChooseConfigProc        choose_config_fn;
EglCreateContextProc       create_context_fn;
EglDestroyContextProc      destroy_context_fn;
EglCreateWindowSurfaceProc create_window_surface_fn;
EglDestroySurfaceProc      destroy_surface_fn;
EglMakeCurrentProc         make_current_fn;
EglSwapBuffersProc         swap_buffers_fn;
EglSwapIntervalProc        swap_interval_fn;
EglGetCurrentContextProc   get_current_context_fn;
EglQueryStringProc         query_string_fn;
EglGetConfigAttribProc     get_config_attrib_fn;

alias EglDisplay = void*;
alias EglContext = void*;
alias EglSurface = void*;
alias EglConfig = void*;
alias EglNativeDisplayType = void*;
alias EglNativeWindowType = void*;
alias EglBoolean = uint;
alias EglInt = int;
alias EglGetDisplayProc = fn EglDisplay(EglNativeDisplayType display);
alias EglInitializeProc = fn EglBoolean(EglDisplay display, EglInt* major, EglInt* minor);
alias EglTerminateProc = fn EglBoolean(EglDisplay display);
alias EglBindApiProc = fn EglBoolean(uint api);
alias EglChooseConfigProc = fn EglBoolean(EglDisplay display, EglInt* attribs, EglConfig* configs, EglInt config_size, EglInt* num_config);
alias EglCreateContextProc = fn EglContext(EglDisplay display, EglConfig config, EglContext share_context, EglInt* attribs);
alias EglDestroyContextProc = fn EglBoolean(EglDisplay display, EglContext context);
alias EglCreateWindowSurfaceProc = fn EglSurface(EglDisplay display, EglConfig config, EglNativeWindowType window, EglInt* attribs);
alias EglDestroySurfaceProc = fn EglBoolean(EglDisplay display, EglSurface surface);
alias EglMakeCurrentProc = fn EglBoolean(EglDisplay display, EglSurface draw, EglSurface read, EglContext context);
alias EglSwapBuffersProc = fn EglBoolean(EglDisplay display, EglSurface surface);
alias EglSwapIntervalProc = fn EglBoolean(EglDisplay display, EglInt interval);
alias EglGetCurrentContextProc = fn EglContext();
alias EglQueryStringProc = fn ZString(EglDisplay display, EglInt name);
alias EglGetConfigAttribProc = fn EglBoolean(EglDisplay display, EglConfig config, EglInt attribute, EglInt* value);

fn void? bind_egl()
{
	get_display_fn           = find_proc("eglGetDisplay")!;
	initialize_fn            = find_proc("eglInitialize")!;
	terminate_fn             = find_proc("eglTerminate")!;
	bind_api_fn              = find_proc("eglBindAPI")!;
	choose_config_fn         = find_proc("eglChooseConfig")!;
	create_context_fn        = find_proc("eglCreateContext")!;
	destroy_context_fn       = find_proc("eglDestroyContext")!;
	create_window_surface_fn = find_proc("eglCreateWindowSurface")!;
	destroy_surface_fn       = find_proc("eglDestroySurface")!;
	make_current_fn          = find_proc("eglMakeCurrent")!;
	swap_buffers_fn          = find_proc("eglSwapBuffers")!;
	swap_interval_fn         = find_proc("eglSwapInterval")!;
	get_current_context_fn   = find_proc("eglGetCurrentContext")!;
	query_string_fn          = find_proc("eglQueryString")!;
	get_config_attrib_fn     = find_proc("eglGetConfigAttrib")!;
}
