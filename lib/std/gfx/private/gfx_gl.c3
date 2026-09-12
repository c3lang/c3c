module std::gfx @feat(MACOS, X11, X11_WAYLAND, WAYLAND, WASM, WIN32);
import std::os::macos, std::os::darwin;

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
	GfxGlReleaseBehavior release_behavior;
	GfxGlProfile profile;
	int major, minor;
	GfxGlContext* share;
	GfxEglContext* share_egl;
	GfxGlRenderer renderer;
}

const uint GL_EXTENSIONS     = 0x1F03;
const uint GL_NUM_EXTENSIONS = 0x821D;

alias GlGetString      = fn ZString(uint name);
alias GlGetStringi     = fn ZString(uint name, uint index);
alias GlGetIntegerv    = fn void(uint pname, int* data);
alias GetProcAddressFn = fn void*(ZString procname);

fn bool _opengl_extension_supported_base(String extension, void* proc)
{
	GetProcAddressFn get_proc = proc;
	if (get_global_opengl_hints().major >= 3)
	{
		GlGetIntegerv get_integerv = get_proc("glGetIntegerv");
		GlGetStringi get_stringi = get_proc("glGetStringi");
		if (!get_integerv || !get_stringi) return false;
		int count = 0;
		get_integerv(GL_NUM_EXTENSIONS, &count);
		for (int i = 0; i < count; i++)
		{
			ZString name = get_stringi(GL_EXTENSIONS, (uint)i);
			if (name && name.str_view() == extension) return true;
		}
		return false;
	}

	GlGetString get_string = get_proc("glGetString");
	if (!get_string) return false;
	ZString extensions = get_string(GL_EXTENSIONS);
	return extensions && _extension_is_in_list(extensions.str_view(), extension);
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
			if (!wgl_swap_interval_ext_fn || !wgl_swap_interval_ext_fn(swap_interval))
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
	false, false, false, false, NONE,
	CORE, 1, 0, null, null, ACCELERATED
};

GfxGlHints _global_hints_open_gl = GLOBAL_HINTS;

module std::gfx  @feat(MACOS, X11, X11_WAYLAND, WAYLAND, WIN32) @feat(!WASM);

// public
fn void _unload_opengl_native() @private
{
	_gfx_ctx.native_gl_handle = null;
	_gfx_ctx.glGetProcAddress = null;
}

