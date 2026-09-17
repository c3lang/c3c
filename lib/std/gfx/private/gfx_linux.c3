module std::gfx @feat(LINUX, BSD) @feat(WAYLAND | !NO_X11) @private;
import std::math, std::collections::list, libc, std::time, std::os::env, std::os::posix, std::net::os;
import std::gfx::x11 @public, std::gfx::wl @public, std::gfx::egl @public;

struct GfxNativeImage
{
	GfxFormat format;
	char* buffer;
	void* bitmap; // XImage* (X11)
	struct @feat(WAYLAND)
	{
		WlProxy* pool; // wl_shm_pool* (Wayland)
		int fd; // Wayland
		WlProxy* wlBuffer; // wl_buffer* (Wayland)
	}
}




