module std::gfx @feat(!WASM);

// public
fn void rgfw_attribStack_init(RgfwAttribStack* stack, int* attribs, sz max) { stack.attribs = attribs; stack.count = 0; stack.max = max; }
// public
fn void rgfw_attribStack_pushAttrib(RgfwAttribStack* stack, int attrib) { stack.attribs[stack.count] = attrib; stack.count += 1; }
// public
fn void rgfw_attribStack_pushAttribs(RgfwAttribStack* stack, int attrib1, int attrib2) { rgfw_attribStack_pushAttrib(stack, attrib1); rgfw_attribStack_pushAttrib(stack, attrib2); }

// public
fn void unload_gl()
{
	_gfx_ctx.nativeGL_handle = null;
	_gfx_ctx.glGetProcAddress = null;
}


module std::gfx @feat(WASM);

// public
fn void unload_gl()
{}

module std::gfx @feat(WASM, MACOS, WIN32);

RgfwGlHints rgfw_global_hints_open_gl_src = {
	0, 0, 0, 0, 1, 8, 8, 8, 8, 24, 0, 0, 0, 0,
	false, false, false, false, RGFW_GL_RELEASE_NONE,
	RGFW_GL_CORE, 1, 0, null, null, RGFW_GL_ACCELERATED
};
RgfwGlHints* rgfw_global_hints_open_gl = &rgfw_global_hints_open_gl_src;

// public
fn RgfwGlHints* rgfw_getGlobalHints_OpenGL()
{
	return rgfw_global_hints_open_gl;
}

// public
fn void rgfw_setGlobalHints_OpenGL(RgfwGlHints* hints)
{
	rgfw_global_hints_open_gl = hints;
}

// public
fn void rgfw_window_deleteContext_OpenGL(GfxWindow* win, RgfwGlContext* ctx)
{
	rgfw_window_deleteContextPtr_OpenGL(win, ctx);
	if (win && (win.src.gfxType & RGFW_GFX_OWNED_BY_RGFW)) free(ctx);
}


// public
fn RgfwGlContext* rgfw_window_createContext_OpenGL(GfxWindow* win, RgfwGlHints* hints)
{
	RgfwGlContext* ctx = mem::new(RgfwGlContext);
	if (!rgfw_window_createContextPtr_OpenGL(win, ctx, hints))
	{
		free(ctx);
		if (win) win.src.ctx.native = null;
		return null;
	}
	win.src.gfxType |= RGFW_GFX_OWNED_BY_RGFW;
	return ctx;
}

module std::gfx @feat(WASM, MACOS);

// public
fn void rgfw_resetGlobalHints_OpenGL()
{
	rgfw_global_hints_open_gl_src = {0, 0, 0, 0, 1, 8, 8, 8, 8, 24, 0, 0, 0, 0, false, false, false, false, RGFW_GL_RELEASE_NONE, RGFW_GL_CORE, 1, 0, null, null, RGFW_GL_ACCELERATED};
}
