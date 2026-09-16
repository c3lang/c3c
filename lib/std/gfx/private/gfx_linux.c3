module std::gfx @feat(LINUX, BSD) @private;
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

struct GfxGlContext
{
	struct @feat(!NO_X11)
	{
		GLXContext ctx;
		GLXWindow window;
	}
	struct @feat(WAYLAND)
	{
		GfxEglContext egl;
	}
}

struct GfxEglContext
{
	void* ctx;
	void* surface;
	void* egl_window;
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



fn void _wait_for_event_native(Duration duration)
{
	if (_gfx.eventWait_forceStop[0] == 0 || _gfx.eventWait_forceStop[1] == 0)
	{
		if (posix::pipe((int[2]*)&_gfx.eventWait_forceStop) != -1)
		{
			os::fcntl((NativeSocket)_gfx.eventWait_forceStop[0], os::F_GETFL, 0);
			os::fcntl((NativeSocket)_gfx.eventWait_forceStop[0], os::F_GETFD, 0);
			os::fcntl((NativeSocket)_gfx.eventWait_forceStop[1], os::F_GETFL, 0);
			os::fcntl((NativeSocket)_gfx.eventWait_forceStop[1], os::F_GETFD, 0);
		}
	}

	Posix_pollfd[2] fds;
	fds[0].fd = 0;
	fds[0].events = (short)os::POLLIN;
	fds[1].fd = _gfx.eventWait_forceStop[0];
	fds[1].events = (short)os::POLLIN;

	$switch:
		$case WAYLAND_ONLY:
			wl::_wait_for_event_wayland(fds[..], duration);
		$case X11_ONLY:
			x11::_wait_for_event_x11(fds[..], duration);
		$default:
			if (_gfx.use_wayland)
			{
				wl::_wait_for_event_wayland(fds[..], duration);
			}
			else
			{
				x11::_wait_for_event_x11(fds[..], duration);
			}
	$endswitch

	// drain any data in the stop request
	if (_gfx.eventWait_forceStop[2])
	{
		char[64] data;
		libc::read(_gfx.eventWait_forceStop[0], &data, 64);

		_gfx.eventWait_forceStop[2] = 0;
	}
}
