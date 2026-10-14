module std::gfx::gl @feat(MACOS, LINUX, BSD, WASM, WIN32) @feat(!(BSD | LINUX) | WAYLAND | !NO_X11) @private;
import std::gfx @public;

const uint GL_EXTENSIONS     = 0x1F03;
const uint GL_NUM_EXTENSIONS = 0x821D;

alias GlGetString      = fn ZString(uint name);
alias GlGetStringi     = fn ZString(uint name, uint index);
alias GlGetIntegerv    = fn void(uint pname, int* data);

fn bool extension_supported_base(String extension, GfxGetProcFn proc)
{
	GlGetIntegerv get_integerv = proc("glGetIntegerv");
	GlGetStringi get_stringi = proc("glGetStringi");
	if (get_integerv && get_stringi)
	{
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


