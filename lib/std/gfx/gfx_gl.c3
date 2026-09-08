module std::gfx;

fn void? _load_opengl_native() @private @weak => NOT_IMPLEMENTED~;
fn void _unload_opengl_native() @private @weak { }

// public
fn void GfxWindow.opengl_swap_interval(&self, int swap_interval)
{
	$switch:
		$case $feat(MACOS):
			if (self.src.ctx.native && self.src.ctx.native.ctx)
			{
				nsOpenGLContext_setValues((ObjcId)self.src.ctx.native.ctx, &swap_interval, NS_OPEN_GL_CONTEXT_PARAMETER_SWAP_INTERVAL);
			}
		$case $feat(WIN32):
			if (!todo_fix_wglSwapIntervalEXT || !todo_fix_wglSwapIntervalEXT(swap_interval))
			{
				log::error("Failed to set OpenGL swap interval", category: log::CATEGORY_VIDEO);
			}
		$case $feat(LINUX):
			$switch:
				$case WAYLAND_ONLY:
					return todo_fix_window_swapInterval_OpenGL_Wayland(self, swap_interval);
				$case X11_ONLY:
					return todo_fix_window_swapInterval_OpenGL_X11(self, swap_interval);
				$default:
					if (gfx_use_wayland)
					{
						return todo_fix_window_swapInterval_OpenGL_Wayland(self, swap_interval);
					}
					return todo_fix_window_swapInterval_OpenGL_X11(self, swap_interval);
			$endswitch
		$default:
		// Nothing
	$endswitch
}


module std::gfx @feat(!WASM);

// public
fn void todo_fix_attribStack_init(TodoRenameAttribStack* stack, int* attribs, sz max) { stack.attribs = attribs; stack.count = 0; stack.max = max; }
// public
fn void todo_fix_attribStack_pushAttrib(TodoRenameAttribStack* stack, int attrib) { stack.attribs[stack.count] = attrib; stack.count += 1; }
// public
fn void todo_fix_attribStack_pushAttribs(TodoRenameAttribStack* stack, int attrib1, int attrib2) { todo_fix_attribStack_pushAttrib(stack, attrib1); todo_fix_attribStack_pushAttrib(stack, attrib2); }

// public
fn void _unload_opengl_native() @private
{
	_gfx_ctx.nativeGL_handle = null;
	_gfx_ctx.glGetProcAddress = null;
}


module std::gfx @feat(WASM);


module std::gfx @feat(WASM, MACOS, WIN32);

TodoRenameGlHints todo_fix_global_hints_open_gl_src = {
	0, 0, 0, 0, 1, 8, 8, 8, 8, 24, 0, 0, 0, 0,
	false, false, false, false, TODO_RENAMEGL_RELEASE_NONE,
	TODO_RENAMEGL_CORE, 1, 0, null, null, TODO_RENAMEGL_ACCELERATED
};
TodoRenameGlHints* todo_fix_global_hints_open_gl = &todo_fix_global_hints_open_gl_src;

// public
fn TodoRenameGlHints* todo_fix_getGlobalHints_OpenGL()
{
	return todo_fix_global_hints_open_gl;
}

// public
fn void todo_fix_setGlobalHints_OpenGL(TodoRenameGlHints* hints)
{
	todo_fix_global_hints_open_gl = hints;
}

// public
fn void todo_fix_window_deleteContext_OpenGL(GfxWindow* win, TodoRenameGlContext* ctx)
{
	todo_fix_window_deleteContextPtr_OpenGL(win, ctx);
	if (win && (win.src.gfxType & TODO_RENAMEGFX_OWNED_BY_RGFW)) free(ctx);
}


// public
fn TodoRenameGlContext* todo_fix_window_createContext_OpenGL(GfxWindow* win, TodoRenameGlHints* hints)
{
	TodoRenameGlContext* ctx = mem::new(TodoRenameGlContext);
	if (!todo_fix_window_createContextPtr_OpenGL(win, ctx, hints))
	{
		free(ctx);
		if (win) win.src.ctx.native = null;
		return null;
	}
	win.src.gfxType |= TODO_RENAMEGFX_OWNED_BY_RGFW;
	return ctx;
}

module std::gfx @feat(WASM, MACOS);

// public
fn void todo_fix_resetGlobalHints_OpenGL()
{
	todo_fix_global_hints_open_gl_src = {0, 0, 0, 0, 1, 8, 8, 8, 8, 24, 0, 0, 0, 0, false, false, false, false, TODO_RENAMEGL_RELEASE_NONE, TODO_RENAMEGL_CORE, 1, 0, null, null, TODO_RENAMEGL_ACCELERATED};
}

module std::gfx @feat(WIN32);
import std::os::win32;

// public
fn void? _load_opengl_native() @private
{
	if (_gfx_ctx.nativeGL_handle && _gfx_ctx.glGetProcAddress) return;
	if (!(_gfx_ctx.nativeGL_handle = win32::loadLibraryA("opengl32.dll"))) return MISSING_OPENGL_LIB~;
	_gfx_ctx.glGetProcAddress = win32::getProcAddress((Win32_HMODULE*)_gfx_ctx.nativeGL_handle, "wglGetProcAddress");
	if (!_gfx_ctx.glGetProcAddress) return MISSING_GL_GET_PROC_ADDR~;
	todo_fix_wglCreateContext = win32::getProcAddress((Win32_HMODULE*)_gfx_ctx.nativeGL_handle, "wglCreateContext");
	todo_fix_wglDeleteContext = win32::getProcAddress((Win32_HMODULE*)_gfx_ctx.nativeGL_handle, "wglDeleteContext");
	todo_fix_wglMakeCurrent = win32::getProcAddress((Win32_HMODULE*)_gfx_ctx.nativeGL_handle, "wglMakeCurrent");
	todo_fix_wglGetCurrentDC = win32::getProcAddress((Win32_HMODULE*)_gfx_ctx.nativeGL_handle, "wglGetCurrentDC");
	todo_fix_wglGetCurrentContext = win32::getProcAddress((Win32_HMODULE*)_gfx_ctx.nativeGL_handle, "wglGetCurrentContext");
	todo_fix_wglShareLists = win32::getProcAddress((Win32_HMODULE*)_gfx_ctx.nativeGL_handle, "wglShareLists");

	Win32_HWND* dummyWin = win32::createWindowExW(0, _gfx_ctx.wndClass.className, null, 0, 0, 0, 0, 0, null, null, _gfx_ctx.instance, null);
	Win32_HDC dummyDc = getDC(dummyWin);
	uint pfdFlags = PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER;

	PixelFormatDescriptor pfd;
	pfd.size = (ushort)PixelFormatDescriptor::size;
	pfd.version = 1;
	pfd.flags = pfdFlags;
	pfd.pixelType = PFD_TYPE_RGBA;
	pfd.colorBits = 32;
	pfd.alphaBits = 8;
	pfd.layerType = PFD_MAIN_PLANE;
	pfd.depthBits = 32;
	pfd.stencilBits = 8;

	int dummyPixelFormat = choosePixelFormat(dummyDc, &pfd);
	setPixelFormat(dummyDc, dummyPixelFormat, &pfd);

	void* dummyContext = todo_fix_wglCreateContext(dummyDc);
	void* cur = todo_fix_wglGetCurrentContext();
	todo_fix_wglMakeCurrent(dummyDc, dummyContext);

	todo_fix_wglCreateContextAttribsARB = (TodoRenameWglCreateContextAttribsArbProc)_get_opengl_proc_native("wglCreateContextAttribsARB");
	todo_fix_wglChoosePixelFormatARB = (TodoRenameWglChoosePixelFormatArbProc)_get_opengl_proc_native("wglChoosePixelFormatARB");
	todo_fix_wglSwapIntervalEXT = (TodoRenameWglSwapIntervalExtProc)_get_opengl_proc_native("wglSwapIntervalEXT");

	if (todo_fix_wglSwapIntervalEXT == null) todo_fix_debugCallback(TODO_RENAMETYPE_ERROR, TODO_RENAMEERR_OPEN_GL_CONTEXT, "Failed to load swap interval function");

	todo_fix_wglMakeCurrent(dummyDc, cur);
	todo_fix_wglDeleteContext(dummyContext);
	releaseDC(dummyWin, dummyDc);
	destroyWindow(dummyWin);
}

fn GfxFn _get_opengl_proc_native(ZString procname)
{
	GfxFn proc = _gfx_ctx.glGetProcAddress ? ((GlxGetProcAddressProc)_gfx_ctx.glGetProcAddress)(procname) : null;
	if (proc) return proc;
	return win32::getProcAddress((Win32_HMODULE*)_gfx_ctx.nativeGL_handle, (Win32_LPCSTR)procname);
}
