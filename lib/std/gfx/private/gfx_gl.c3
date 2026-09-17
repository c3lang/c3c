module std::gfx @feat(MACOS, LINUX, BSD, WASM, WIN32);
import std::os::macos, std::os::darwin, std::os::win32;
import std::gfx::wl @public, std::gfx::x11 @public;

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

module std::gfx::gl @feat(MACOS, LINUX, BSD, WASM, WIN32);
import std::gfx @public;

const uint GL_EXTENSIONS     = 0x1F03;
const uint GL_NUM_EXTENSIONS = 0x821D;

alias GlGetString      = fn ZString(uint name);
alias GlGetStringi     = fn ZString(uint name, uint index);
alias GlGetIntegerv    = fn void(uint pname, int* data);

fn bool extension_supported_base(String extension, GfxGetProcFn proc)
{
	if (gfx::get_global_opengl_hints().major >= 3)
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
	return extensions && gfx::_extension_is_in_list(extensions.str_view(), extension);
}


