module std::gfx @feat(MACOS, X11, X11_WAYLAND, WAYLAND, WASM, WIN32);
import std::os::macos, std::os::darwin;

const TodoRenameGlReleaseBehavior TODO_RENAME_GL_RELEASE_FLUSH = 0;
const TodoRenameGlReleaseBehavior TODO_RENAME_GL_RELEASE_NONE = 1;

const TodoRenameGlProfile TODO_RENAME_GL_CORE = 0;
const TodoRenameGlProfile TODO_RENAME_GL_FORWARD_COMPATIBILITY = 1;
const TodoRenameGlProfile TODO_RENAME_GL_COMPATIBILITY = 2;
const TodoRenameGlProfile TODO_RENAME_GL_ES = 3;
const TodoRenameGlProfile TODO_RENAME_GL_WEB = 4;

const TodoRenameGlRenderer TODO_RENAME_GL_ACCELERATED = 0;
const TodoRenameGlRenderer TODO_RENAME_GL_SOFTWARE = 1;

struct GfxGlHints
{
	int stencil;
	int samples;
	int stereo;
	int auxBuffers;
	int doubleBuffer;
	int red, green, blue, alpha;
	int depth;
	int accumRed, accumGreen, accumBlue, accumAlpha;
	bool sRGB;
	bool robustness;
	bool debug;
	bool noError;
	TodoRenameGlReleaseBehavior releaseBehavior;
	TodoRenameGlProfile profile;
	int major, minor;
	GfxGlContext* share;
	GfxEglContext* shareEGL;
	TodoRenameGlRenderer renderer;
}


fn GfxGlContext*? _window_create_opengl_context_native(GfxWindow* win, GfxGlHints* hints)
{
	GfxGlContext* context = mem::new(GfxGlContext);
	defer catch free(context);
	_window_init_opengl_context_native(win, context, hints)!;
	win.src.backing.owned = true;
	return context;
}

fn void _window_destroy_opengl_context_native(GfxWindow* win, GfxGlContext* ctx)
{
	_window_release_opengl_context_native(win, ctx);
	if (win.src.backing.owned) free(ctx);
}

fn void? _load_opengl_native() @private @weak => NOT_IMPLEMENTED~;
fn void _unload_opengl_native() @private @weak { }



fn void GfxWindow.opengl_swap_interval(&self, int swap_interval)
{
	$switch:
		$case $feat(MACOS):
			if (self.src.ctx.native && self.src.ctx.native.ctx)
			{
				nsOpenGLContext_setValues((ObjId)self.src.ctx.native.ctx, &swap_interval, NS_OPEN_GL_CONTEXT_PARAMETER_SWAP_INTERVAL);
			}
		$case $feat(WIN32):
			if (!todo_fix_wglSwapIntervalEXT || !todo_fix_wglSwapIntervalEXT(swap_interval))
			{
				log::error("Failed to set OpenGL swap interval", category: log::CATEGORY_VIDEO);
			}
		$case $feat(LINUX):
			$switch:
				$case WAYLAND_ONLY:
					return _window_opengl_swap_interval_wayland(self, swap_interval);
				$case X11_ONLY:
					return todo_fix_window_swapInterval_OpenGL_X11(self, swap_interval);
				$default:
					if (gfx_use_wayland)
					{
						return _window_opengl_swap_interval_wayland(self, swap_interval);
					}
					return todo_fix_window_swapInterval_OpenGL_X11(self, swap_interval);
			$endswitch
		$default:
		// Nothing
	$endswitch
}


const GfxGlHints GLOBAL_HINTS = {
	0, 0, 0, 0, 1, 8, 8, 8, 8, 24, 0, 0, 0, 0,
	false, false, false, false, TODO_RENAME_GL_RELEASE_NONE,
	TODO_RENAME_GL_CORE, 1, 0, null, null, TODO_RENAME_GL_ACCELERATED
};

GfxGlHints _global_hints_open_gl = GLOBAL_HINTS;

module std::gfx  @feat(MACOS, X11, X11_WAYLAND, WAYLAND, WIN32) @feat(!WASM);

// public
fn void _unload_opengl_native() @private
{
	_gfx_ctx.nativeGL_handle = null;
	_gfx_ctx.glGetProcAddress = null;
}

