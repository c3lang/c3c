module std::gfx @feat(MACOS, LINUX, BSD, WASM, WIN32);
import std::os::macos, std::os::darwin, std::os::win32;

const GfxGlHints GLOBAL_HINTS = {
	0, 0, 0, 0, 1,
	{ 8, 8, 8, 8 },
	24,
	{ 0, 0, 0, 0 },
	false, false, false, false, NONE,
	CORE, 1, 0, null, null, ACCELERATED
};

GfxGlHints _global_hints_open_gl = GLOBAL_HINTS;


struct GfxGlHints
{
	int stencil;
	int samples;
	int stereo;
	int auxBuffers;
	int doubleBuffer;
	int[<4>] rgba_size;
	int depth;
	int[<4>] accum_size;
	bool srgb;
	bool robustness;
	bool debug;
	bool no_error;
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

fn bool _opengl_extension_supported_base(String extension, GfxGetProcFn proc)
{
	if (get_global_opengl_hints().major >= 3)
	{
		GlGetIntegerv get_integerv = proc("glGetIntegerv");
		GlGetStringi get_stringi = proc("glGetStringi");
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

	GlGetString get_string = proc("glGetString");
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

fn void _window_set_opengl_swap_interval(GfxWindow* win, int swap_interval)
{
	$switch:
		$case $feat(MACOS):
			if (win.src.ctx.native && win.src.ctx.native.ctx)
			{
				_opengl_context_set_values((ObjId)win.src.ctx.native.ctx, &swap_interval, NS_OPEN_GL_CONTEXT_PARAMETER_SWAP_INTERVAL);
			}
		$case $feat(WIN32):
			if (!wgl_swap_interval_ext_fn || !wgl_swap_interval_ext_fn(swap_interval))
			{
				log::error("Failed to set OpenGL swap interval", category: log::CATEGORY_VIDEO);
			}
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland)
			{
				return _window_opengl_swap_interval_wayland(win, swap_interval);
			}
			return _window_opengl_swap_interval_x11(win, swap_interval);
		$case X11:
			return _window_opengl_swap_interval_x11(win, swap_interval);
		$case WAYLAND:
			return _window_opengl_swap_interval_wayland(win, swap_interval);
		$default:
		// Nothing
	$endswitch
}


