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



fn void? _init_native(String class_name, GfxInitOption flags)
{
	$switch:
		$case WAYLAND_ONLY:
			return wl::_init_wayland(class_name, flags);
		$case X11_ONLY:
			return x11::_init_x11(class_name, flags);
		$default:
			if (!(flags & ENABLE_X11))
			{
				String wayland_display = env::tget_var("WAYLAND_DISPLAY") ?? "";
				_gfx.use_wayland = wayland_display != "";
				if (_gfx.use_wayland)
				{
					if (try wl::_init_wayland(class_name, flags)) return;
					log::info("Falling back to X11", category: log::CATEGORY_VIDEO);
					_gfx.use_wayland = false;
				}
			}
			else
			{
				_gfx.use_wayland = false;
			}
			return x11::_init_x11(class_name, flags);
	$endswitch
}

