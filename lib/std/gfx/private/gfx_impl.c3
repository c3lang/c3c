module std::gfx @feat(LINUX, BSD, MACOS, WASM, WIN32) @private;
import libc, std::math, std::collections::list, std::os, std::gfx::x11 @public, std::gfx::egl @public, std::time;
import std::gfx::wl @public, std::gfx::macos @public, std::gfx::wasm @public;
import std::gfx::win32 @public;

// Note that _platform functions are not directly used, but will be used with some
// surrounding code in gfx.c3. Non _platform functions are directly forwarded.


// --- Gfx

fn void _destroy_platform()
{
	$switch:
		$case $feat(MACOS):
			return macos::destroy_platform();
		$case $feat(WASM):
			return wasm::destroy_platform();
		$case $feat(WIN32):
			return win32::destroy_platform();
		$case WAYLAND_ONLY:
			return wl::destroy_platform();
		$case X11_ONLY:
			return x11::destroy_platform();
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::destroy_platform();
			return x11::destroy_platform();
		$default:
			unreachable();
	$endswitch
}

fn void _poll_events_platform()
{
	$switch:
		$case $feat(MACOS):
			return macos::poll_events_platform();
		$case $feat(WASM):
			return wasm::poll_events_platform();
		$case $feat(WIN32):
			return win32::poll_events_platform();
		$case WAYLAND_ONLY:
			return wl::poll_events_platform();
		$case X11_ONLY:
			return x11::poll_events_platform();
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::poll_events_platform();
			return x11::poll_events_platform();
		$default:
			unreachable();
	$endswitch
}

fn void _stop_check_events()
{
	$switch:
		$case $feat(MACOS):
		$case $feat(WASM):
			_gfx.stop_check_events = true;
		$case $feat(WIN32):
			return win32::stop_check_events();
		$case WAYLAND || X11:
			_gfx.eventWait_forceStop[2] = 1;
			while (true)
			{
				char byte = 0;
				sz result = libc::write(_gfx.eventWait_forceStop[1], &byte, 1);
				if (result == 1 || result == -1) break;
			}
		$default:
			unreachable();
	$endswitch
}

// --- GfxMonitor functions

fn GfxGammaRamp? _monitor_get_gamma_ramp(GfxMonitor* monitor, Allocator a)
{
	$switch:
		$case $feat(MACOS):
			return macos::monitor_get_gamma_ramp(monitor, a);
		$case $feat(WASM):
			return wasm::monitor_get_gamma_ramp(monitor, a);
		$case $feat(WIN32):
			return win32::monitor_get_gamma_ramp(monitor, a);
		$case WAYLAND_ONLY:
			return wl::monitor_get_gamma_ramp(monitor, a);
		$case X11_ONLY:
			return x11::monitor_get_gamma_ramp(monitor, a);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::monitor_get_gamma_ramp(monitor, a);
			return x11::monitor_get_gamma_ramp(monitor, a);
		$default:
			unreachable();
	$endswitch
}

fn GfxMonitorMode[] _monitor_get_modes(GfxMonitor* monitor)
{
	$switch:
		$case $feat(MACOS):
			return macos::monitor_get_modes(monitor);
		$case $feat(WASM):
			return wasm::monitor_get_modes(monitor);
		$case $feat(WIN32):
			return win32::monitor_get_modes(monitor);
		$case WAYLAND_ONLY:
			return wl::monitor_get_modes(monitor);
		$case X11_ONLY:
			return x11::monitor_get_modes(monitor);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::monitor_get_modes(monitor);
			return x11::monitor_get_modes(monitor);
		$default:
			unreachable();
	$endswitch
}

fn IntRect _monitor_get_workarea(GfxMonitor* monitor)
{
	$switch:
		$case $feat(MACOS):
			return macos::monitor_get_workarea(monitor);
		$case $feat(WASM):
			return wasm::monitor_get_workarea(monitor);
		$case $feat(WIN32):
			return win32::monitor_get_workarea(monitor);
		$case WAYLAND_ONLY:
			return wl::monitor_get_workarea(monitor);
		$case X11_ONLY:
			return x11::monitor_get_workarea(monitor);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::monitor_get_workarea(monitor);
			return x11::monitor_get_workarea(monitor);
		$default:
			unreachable();
	$endswitch
}

fn void _monitor_node_free(GfxMonitorNode* node)
{
	$switch:
		$case $feat(MACOS):
			return macos::monitor_node_free(node);
		$case $feat(WASM):
			return wasm::monitor_node_free(node);
		$case $feat(WIN32):
			return win32::monitor_node_free(node);
		$case WAYLAND_ONLY:
			return wl::monitor_node_free(node);
		$case X11_ONLY:
			return x11::monitor_node_free(node);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::monitor_node_free(node);
			return x11::monitor_node_free(node);
		$default:
			unreachable();
	$endswitch
}

fn void? _monitor_request_mode(GfxMonitor* mon, GfxMonitorMode* mode, GfxMonitorModeRequest request)
{
	$switch:
		$case $feat(MACOS):
			return macos::monitor_request_mode(mon, mode, request);
		$case $feat(WASM):
			return wasm::monitor_request_mode(mon, mode, request);
		$case $feat(WIN32):
			return win32::monitor_request_mode(mon, mode, request);
		$case WAYLAND_ONLY:
			return wl::monitor_request_mode(mon, mode, request);
		$case X11_ONLY:
			return x11::monitor_request_mode(mon, mode, request);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::monitor_request_mode(mon, mode, request);
			return x11::monitor_request_mode(mon, mode, request);
		$default:
			unreachable();
	$endswitch
}

fn void? _monitor_set_gamma(GfxMonitor* monitor, float gamma)
{
	$switch:
		$case $feat(MACOS):
			return macos::monitor_set_gamma(monitor, gamma);
		$case $feat(WASM):
			return wasm::monitor_set_gamma(monitor, gamma);
		$case $feat(WIN32):
			return win32::monitor_set_gamma(monitor, gamma);
		$case WAYLAND_ONLY:
			return wl::monitor_set_gamma(monitor, gamma);
		$case X11_ONLY:
			return x11::monitor_set_gamma(monitor, gamma);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::monitor_set_gamma(monitor, gamma);
			return x11::monitor_set_gamma(monitor, gamma);
		$default:
			unreachable();
	$endswitch
}

fn void? _monitor_set_gamma_ramp(GfxMonitor* monitor, GfxGammaRamp ramp)
{
	$switch:
		$case $feat(MACOS):
			return macos::monitor_set_gamma_ramp(monitor, ramp);
		$case $feat(WASM):
			return wasm::monitor_set_gamma_ramp(monitor, ramp);
		$case $feat(WIN32):
			return win32::monitor_set_gamma_ramp(monitor, ramp);
		$case WAYLAND_ONLY:
			return wl::monitor_set_gamma_ramp(monitor, ramp);
		$case X11_ONLY:
			return x11::monitor_set_gamma_ramp(monitor, ramp);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::monitor_set_gamma_ramp(monitor, ramp);
			return x11::monitor_set_gamma_ramp(monitor, ramp);
		$default:
			unreachable();
	$endswitch
}

fn void? _monitor_set_mode(GfxMonitor* mon, GfxMonitorMode* mode)
{
	$switch:
		$case $feat(MACOS):
			return macos::monitor_set_mode(mon, mode);
		$case $feat(WASM):
			return wasm::monitor_set_mode(mon, mode);
		$case $feat(WIN32):
			return win32::monitor_set_mode(mon, mode);
		$case WAYLAND_ONLY:
			return wl::monitor_set_mode(mon, mode);
		$case X11_ONLY:
			return x11::monitor_set_mode(mon, mode);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::monitor_set_mode(mon, mode);
			return x11::monitor_set_mode(mon, mode);
		$default:
			unreachable();
	$endswitch
}

fn void _poll_monitors()
{
	$switch:
		$case $feat(MACOS):
			return macos::poll_monitors();
		$case $feat(WASM):
			return wasm::poll_monitors();
		$case $feat(WIN32):
			return win32::poll_monitors();
		$case WAYLAND_ONLY:
			return wl::poll_monitors();
		$case X11_ONLY:
			return x11::poll_monitors();
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::poll_monitors();
			return x11::poll_monitors();
		$default:
			unreachable();
	$endswitch
}

// --- GfxMouse and mouse related

fn GfxMouse*? _create_mouse(void* data, GfxSize size, GfxFormat format)
{
	$switch:
		$case $feat(MACOS):
			return macos::create_mouse(data, size, format);
		$case $feat(WASM):
			return wasm::create_mouse(data, size, format);
		$case $feat(WIN32):
			return win32::create_mouse(data, size, format);
		$case WAYLAND_ONLY:
			return wl::create_mouse(data, size, format);
		$case X11_ONLY:
			return x11::create_mouse(data, size, format);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::create_mouse(data, size, format);
			return x11::create_mouse(data, size, format);
		$default:
			unreachable();
	$endswitch
}

fn GfxMouse*? _create_standard_mouse(GfxMouseIcon mouse)
{
	$switch:
		$case $feat(MACOS):
			return macos::create_standard_mouse(mouse);
		$case $feat(WASM):
			return wasm::create_standard_mouse(mouse);
		$case $feat(WIN32):
			return win32::create_standard_mouse(mouse);
		$case WAYLAND_ONLY:
			return wl::create_standard_mouse(mouse);
		$case X11_ONLY:
			return x11::create_standard_mouse(mouse);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::create_standard_mouse(mouse);
			return x11::create_standard_mouse(mouse);
		$default:
			unreachable();
	$endswitch
}

fn void _free_mouse(GfxMouse* mouse)
{
	$switch:
		$case $feat(MACOS):
			return macos::free_mouse(mouse);
		$case $feat(WASM):
			return wasm::free_mouse(mouse);
		$case $feat(WIN32):
			return win32::free_mouse(mouse);
		$case WAYLAND_ONLY:
			return wl::free_mouse(mouse);
		$case X11_ONLY:
			return x11::free_mouse(mouse);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::free_mouse(mouse);
			return x11::free_mouse(mouse);
		$default:
			unreachable();
	$endswitch
}

fn GfxPoint _get_global_mouse()
{
	$switch:
		$case $feat(MACOS):
			return macos::get_global_mouse();
		$case $feat(WASM):
			return wasm::get_global_mouse();
		$case $feat(WIN32):
			return win32::get_global_mouse();
		$case WAYLAND_ONLY:
			return wl::get_global_mouse();
		$case X11_ONLY:
			return x11::get_global_mouse();
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::get_global_mouse();
			return x11::get_global_mouse();
		$default:
			unreachable();
	$endswitch
}

// --- GfxWindow

<*
 Blit the surface to the window
*>
fn void _window_blit_surface(GfxWindow* win, GfxSurface* surface)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_blit_surface(win, surface);
		$case $feat(WASM):
			return wasm::window_blit_surface(win, surface);
		$case $feat(WIN32):
			return win32::window_blit_surface(win, surface);
		$case WAYLAND_ONLY:
			return wl::window_blit_surface(win, surface);
		$case X11_ONLY:
			return x11::window_blit_surface(win, surface);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_blit_surface(win, surface);
			return x11::window_blit_surface(win, surface);
		$default:
			unreachable();
	$endswitch
}

<*
 Create the actual window on the platform itself.
*>
fn void? _window_create_platform(GfxWindow* win, String name, GfxWindowFlags flags)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_create_platform(win, name, flags);
		$case $feat(WASM):
			return wasm::window_create_platform(win, name, flags);
		$case $feat(WIN32):
			return win32::window_create_platform(win, name, flags);
		$case WAYLAND_ONLY:
			return wl::window_create_platform(win, name, flags);
		$case X11_ONLY:
			return x11::window_create_platform(win, name, flags);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_create_platform(win, name, flags);
			return x11::window_create_platform(win, name, flags);
		$default:
			unreachable();
	$endswitch
}

<*
 Destroy the platform native window.
*>
fn void _window_destroy_platform(GfxWindow* win)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_destroy_platform(win);
		$case $feat(WASM):
			return wasm::window_destroy_platform(win);
		$case $feat(WIN32):
			return win32::window_destroy_platform(win);
		$case WAYLAND_ONLY:
			return wl::window_destroy_platform(win);
		$case X11_ONLY:
			return x11::window_destroy_platform(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_destroy_platform(win);
			return x11::window_destroy_platform(win);
		$default:
			unreachable();
	$endswitch
}

<*
 Retrieve the size of the window from the underlying platform.
*>
fn GfxPoint _window_fetch_size(GfxWindow* win)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_fetch_size(win);
		$case $feat(WASM):
			return wasm::window_fetch_size(win);
		$case $feat(WIN32):
			return win32::window_fetch_size(win);
		$case WAYLAND_ONLY:
			return wl::window_fetch_size(win);
		$case X11_ONLY:
			return x11::window_fetch_size(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_fetch_size(win);
			return x11::window_fetch_size(win);
		$default:
			unreachable();
	$endswitch
}

fn GfxMonitor*? _window_find_monitor(GfxWindow* win)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_find_monitor(win);
		$case $feat(WASM):
			return wasm::window_find_monitor(win);
		$case $feat(WIN32):
			return win32::window_find_monitor(win);
		$case WAYLAND_ONLY:
			return wl::window_find_monitor(win);
		$case X11_ONLY:
			return x11::window_find_monitor(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_find_monitor(win);
			return x11::window_find_monitor(win);
		$default:
			unreachable();
	$endswitch
}

fn void _window_flash(GfxWindow* win, GfxFlashRequest request)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_flash(win, request);
		$case $feat(WASM):
			return wasm::window_flash(win, request);
		$case $feat(WIN32):
			return win32::window_flash(win, request);
		$case WAYLAND_ONLY:
			return wl::window_flash(win, request);
		$case X11_ONLY:
			return x11::window_flash(win, request);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_flash(win, request);
			return x11::window_flash(win, request);
		$default:
			unreachable();
	$endswitch
}

fn void _window_focus(GfxWindow* win)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_focus(win);
		$case $feat(WASM):
			return wasm::window_focus(win);
		$case $feat(WIN32):
			return win32::window_focus(win);
		$case WAYLAND_ONLY:
			return wl::window_focus(win);
		$case X11_ONLY:
			return x11::window_focus(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_focus(win);
			return x11::window_focus(win);
		$default:
			unreachable();
	$endswitch
}

fn void _window_hide(GfxWindow* win) @private
{
	$switch:
		$case $feat(MACOS):
			return macos::window_hide(win);
		$case $feat(WASM):
			return wasm::window_hide(win);
		$case $feat(WIN32):
			return win32::window_hide(win);
		$case WAYLAND_ONLY:
			return wl::window_hide(win);
		$case X11_ONLY:
			return x11::window_hide(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_hide(win);
			return x11::window_hide(win);
		$default:
			unreachable();
	$endswitch
}

fn bool _window_is_floating(GfxWindow* win) @private
{
	$switch:
		$case $feat(MACOS):
			return macos::window_is_floating(win);
		$case $feat(WASM):
			return wasm::window_is_floating(win);
		$case $feat(WIN32):
			return win32::window_is_floating(win);
		$case WAYLAND_ONLY:
			return wl::window_is_floating(win);
		$case X11_ONLY:
			return x11::window_is_floating(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_is_floating(win);
			return x11::window_is_floating(win);
		$default:
			unreachable();
	$endswitch
}

<*
 Return whether the underlying platform window is hidden or not.
*>
fn bool _window_is_hidden(GfxWindow* win) @private
{
	$switch:
		$case $feat(MACOS):
			return macos::window_is_hidden(win);
		$case $feat(WASM):
			return wasm::window_is_hidden(win);
		$case $feat(WIN32):
			return win32::window_is_hidden(win);
		$case WAYLAND_ONLY:
			return wl::window_is_hidden(win);
		$case X11_ONLY:
			return x11::window_is_hidden(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_is_hidden(win);
			return x11::window_is_hidden(win);
		$default:
			unreachable();
	$endswitch
}

<*
 Check if the underlying window is maximized.
*>
fn bool _window_is_maximized(GfxWindow* win)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_is_maximized(win);
		$case $feat(WASM):
			return wasm::window_is_maximized(win);
		$case $feat(WIN32):
			return win32::window_is_maximized(win);
		$case WAYLAND_ONLY:
			return wl::window_is_maximized(win);
		$case X11_ONLY:
			return x11::window_is_maximized(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_is_maximized(win);
			return x11::window_is_maximized(win);
		$default:
			unreachable();
	$endswitch
}

<*
 Check if the underlying window is minimized.
*>
fn bool _window_is_minimized(GfxWindow* win)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_is_minimized(win);
		$case $feat(WASM):
			return wasm::window_is_minimized(win);
		$case $feat(WIN32):
			return win32::window_is_minimized(win);
		$case WAYLAND_ONLY:
			return wl::window_is_minimized(win);
		$case X11_ONLY:
			return x11::window_is_minimized(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_is_minimized(win);
			return x11::window_is_minimized(win);
		$default:
			unreachable();
	$endswitch
}

fn void _window_maximize_platform(GfxWindow* win)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_maximize_platform(win);
		$case $feat(WASM):
			return wasm::window_maximize_platform(win);
		$case $feat(WIN32):
			return win32::window_maximize_platform(win);
		$case WAYLAND_ONLY:
			return wl::window_maximize_platform(win);
		$case X11_ONLY:
			return x11::window_maximize_platform(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_maximize_platform(win);
			return x11::window_maximize_platform(win);
		$default:
			unreachable();
	$endswitch
}

fn void _window_minimize_platform(GfxWindow* win)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_minimize_platform(win);
		$case $feat(WASM):
			return wasm::window_minimize_platform(win);
		$case $feat(WIN32):
			return win32::window_minimize_platform(win);
		$case WAYLAND_ONLY:
			return wl::window_minimize_platform(win);
		$case X11_ONLY:
			return x11::window_minimize_platform(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_minimize_platform(win);
			return x11::window_minimize_platform(win);
		$default:
			unreachable();
	$endswitch
}

fn void _window_move_mouse(GfxWindow* win, GfxPoint where)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_move_mouse(win, where);
		$case $feat(WASM):
			return wasm::window_move_mouse(win, where);
		$case $feat(WIN32):
			return win32::window_move_mouse(win, where);
		$case WAYLAND_ONLY:
			return wl::window_move_mouse(win, where);
		$case X11_ONLY:
			return x11::window_move_mouse(win, where);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_move_mouse(win, where);
			return x11::window_move_mouse(win, where);
		$default:
			unreachable();
	$endswitch
}

fn void _window_move_platform(GfxWindow* win, GfxPoint where)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_move_platform(win, where);
		$case $feat(WASM):
			return wasm::window_move_platform(win, where);
		$case $feat(WIN32):
			return win32::window_move_platform(win, where);
		$case WAYLAND_ONLY:
			return wl::window_move_platform(win, where);
		$case X11_ONLY:
			return x11::window_move_platform(win, where);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_move_platform(win, where);
			return x11::window_move_platform(win, where);
		$default:
			unreachable();
	$endswitch
}

fn void _window_raise(GfxWindow* win)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_raise(win);
		$case $feat(WASM):
			return wasm::window_raise(win);
		$case $feat(WIN32):
			return win32::window_raise(win);
		$case WAYLAND_ONLY:
			return wl::window_raise(win);
		$case X11_ONLY:
			return x11::window_raise(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_raise(win);
			return x11::window_raise(win);
		$default:
			unreachable();
	$endswitch
}

fn void _window_resize_platform(GfxWindow* win, GfxSize size)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_resize_platform(win, size);
		$case $feat(WASM):
			return wasm::window_resize_platform(win, size);
		$case $feat(WIN32):
			return win32::window_resize_platform(win, size);
		$case WAYLAND_ONLY:
			return wl::window_resize_platform(win, size);
		$case X11_ONLY:
			return x11::window_resize_platform(win, size);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_resize_platform(win, size);
			return x11::window_resize_platform(win, size);
		$default:
			unreachable();
	$endswitch
}

fn void _window_restore_platform(GfxWindow* win)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_restore_platform(win);
		$case $feat(WASM):
			return wasm::window_restore_platform(win);
		$case $feat(WIN32):
			return win32::window_restore_platform(win);
		$case WAYLAND_ONLY:
			return wl::window_restore_platform(win);
		$case X11_ONLY:
			return x11::window_restore_platform(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_restore_platform(win);
			return x11::window_restore_platform(win);
		$default:
			unreachable();
	$endswitch
}

fn void _window_set_aspect_ratio(GfxWindow* win, int w, int h)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_set_aspect_ratio(win, w, h);
		$case $feat(WASM):
			return wasm::window_set_aspect_ratio(win, w, h);
		$case $feat(WIN32):
			return win32::window_set_aspect_ratio(win, w, h);
		$case WAYLAND_ONLY:
			return wl::window_set_aspect_ratio(win, w, h);
		$case X11_ONLY:
			return x11::window_set_aspect_ratio(win, w, h);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_set_aspect_ratio(win, w, h);
			return x11::window_set_aspect_ratio(win, w, h);
		$default:
			unreachable();
	$endswitch
}

fn void _window_set_border(GfxWindow* win, bool border)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_set_border(win, border);
		$case $feat(WASM):
			return wasm::window_set_border(win, border);
		$case $feat(WIN32):
			return win32::window_set_border(win, border);
		$case WAYLAND_ONLY:
			return wl::window_set_border(win, border);
		$case X11_ONLY:
			return x11::window_set_border(win, border);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_set_border(win, border);
			return x11::window_set_border(win, border);
		$default:
			unreachable();
	$endswitch
}

fn void _window_set_capture_mouse_platform(GfxWindow* win, bool state)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_set_capture_mouse_platform(win, state);
		$case $feat(WASM):
			return wasm::window_set_capture_mouse_platform(win, state);
		$case $feat(WIN32):
			return win32::window_set_capture_mouse_platform(win, state);
		$case WAYLAND_ONLY:
			return wl::window_set_capture_mouse_platform(win, state);
		$case X11_ONLY:
			return x11::window_set_capture_mouse_platform(win, state);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_set_capture_mouse_platform(win, state);
			return x11::window_set_capture_mouse_platform(win, state);
		$default:
			unreachable();
	$endswitch
}

fn void _window_set_floating(GfxWindow* win, bool floating)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_set_floating(win, floating);
		$case $feat(WASM):
			return wasm::window_set_floating(win, floating);
		$case $feat(WIN32):
			return win32::window_set_floating(win, floating);
		$case WAYLAND_ONLY:
			return wl::window_set_floating(win, floating);
		$case X11_ONLY:
			return x11::window_set_floating(win, floating);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_set_floating(win, floating);
			return x11::window_set_floating(win, floating);
		$default:
			unreachable();
	$endswitch
}

fn void _window_set_fullscreen(GfxWindow* win, bool fullscreen)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_set_fullscreen(win, fullscreen);
		$case $feat(WASM):
			return wasm::window_set_fullscreen(win, fullscreen);
		$case $feat(WIN32):
			return win32::window_set_fullscreen(win, fullscreen);
		$case WAYLAND_ONLY:
			return wl::window_set_fullscreen(win, fullscreen);
		$case X11_ONLY:
			return x11::window_set_fullscreen(win, fullscreen);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_set_fullscreen(win, fullscreen);
			return x11::window_set_fullscreen(win, fullscreen);
		$default:
			unreachable();
	$endswitch
}

fn bool _window_set_icon(GfxWindow* win, void* data_src, int w, int h, GfxFormat format, GfxWindowIconType type = BOTH)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_set_icon(win, data_src, w, h, format, type);
		$case $feat(WASM):
			return wasm::window_set_icon(win, data_src, w, h, format, type);
		$case $feat(WIN32):
			return win32::window_set_icon(win, data_src, w, h, format, type);
		$case WAYLAND_ONLY:
			return wl::window_set_icon(win, data_src, w, h, format, type);
		$case X11_ONLY:
			return x11::window_set_icon(win, data_src, w, h, format, type);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_set_icon(win, data_src, w, h, format, type);
			return x11::window_set_icon(win, data_src, w, h, format, type);
		$default:
			unreachable();
	$endswitch
}

fn void _window_set_max_size(GfxWindow* win, GfxSize size)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_set_max_size(win, size);
		$case $feat(WASM):
			return wasm::window_set_max_size(win, size);
		$case $feat(WIN32):
			return win32::window_set_max_size(win, size);
		$case WAYLAND_ONLY:
			return wl::window_set_max_size(win, size);
		$case X11_ONLY:
			return x11::window_set_max_size(win, size);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_set_max_size(win, size);
			return x11::window_set_max_size(win, size);
		$default:
			unreachable();
	$endswitch
}

fn void _window_set_min_size(GfxWindow* win, GfxSize size)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_set_min_size(win, size);
		$case $feat(WASM):
			return wasm::window_set_min_size(win, size);
		$case $feat(WIN32):
			return win32::window_set_min_size(win, size);
		$case WAYLAND_ONLY:
			return wl::window_set_min_size(win, size);
		$case X11_ONLY:
			return x11::window_set_min_size(win, size);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_set_min_size(win, size);
			return x11::window_set_min_size(win, size);
		$default:
			unreachable();
	$endswitch
}

fn void _window_set_mouse_passthrough(GfxWindow* win, bool passthrough)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_set_mouse_passthrough(win, passthrough);
		$case $feat(WASM):
			return wasm::window_set_mouse_passthrough(win, passthrough);
		$case $feat(WIN32):
			return win32::window_set_mouse_passthrough(win, passthrough);
		$case WAYLAND_ONLY:
			return wl::window_set_mouse_passthrough(win, passthrough);
		$case X11_ONLY:
			return x11::window_set_mouse_passthrough(win, passthrough);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_set_mouse_passthrough(win, passthrough);
			return x11::window_set_mouse_passthrough(win, passthrough);
		$default:
			unreachable();
	$endswitch
}

fn void _window_set_mouse_platform(GfxWindow* win, GfxMouse* mouse)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_set_mouse_platform(win, mouse);
		$case $feat(WASM):
			return wasm::window_set_mouse_platform(win, mouse);
		$case $feat(WIN32):
			return win32::window_set_mouse_platform(win, mouse);
		$case WAYLAND_ONLY:
			return wl::window_set_mouse_platform(win, mouse);
		$case X11_ONLY:
			return x11::window_set_mouse_platform(win, mouse);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_set_mouse_platform(win, mouse);
			return x11::window_set_mouse_platform(win, mouse);
		$default:
			unreachable();
	$endswitch
}

fn void _window_set_name(GfxWindow* win, String name)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_set_name(win, name);
		$case $feat(WASM):
			return wasm::window_set_name(win, name);
		$case $feat(WIN32):
			return win32::window_set_name(win, name);
		$case WAYLAND_ONLY:
			return wl::window_set_name(win, name);
		$case X11_ONLY:
			return x11::window_set_name(win, name);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_set_name(win, name);
			return x11::window_set_name(win, name);
		$default:
			unreachable();
	$endswitch
}

fn void _window_set_opacity(GfxWindow* win, float opacity)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_set_opacity(win, opacity);
		$case $feat(WASM):
			return wasm::window_set_opacity(win, opacity);
		$case $feat(WIN32):
			return win32::window_set_opacity(win, opacity);
		$case WAYLAND_ONLY:
			return wl::window_set_opacity(win, opacity);
		$case X11_ONLY:
			return x11::window_set_opacity(win, opacity);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_set_opacity(win, opacity);
			return x11::window_set_opacity(win, opacity);
		$default:
			unreachable();
	$endswitch
}

fn void _window_set_raw_mouse_mode_platform(GfxWindow* win, bool state)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_set_raw_mouse_mode_platform(win, state);
		$case $feat(WASM):
			return wasm::window_set_raw_mouse_mode_platform(win, state);
		$case $feat(WIN32):
			return win32::window_set_raw_mouse_mode_platform(win, state);
		$case WAYLAND_ONLY:
			return wl::window_set_raw_mouse_mode_platform(win, state);
		$case X11_ONLY:
			return x11::window_set_raw_mouse_mode_platform(win, state);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_set_raw_mouse_mode_platform(win, state);
			return x11::window_set_raw_mouse_mode_platform(win, state);
		$default:
			unreachable();
	$endswitch
}

fn void _window_show_platform(GfxWindow* win)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_show_platform(win);
		$case $feat(WASM):
			return wasm::window_show_platform(win);
		$case $feat(WIN32):
			return win32::window_show_platform(win);
		$case WAYLAND_ONLY:
			return wl::window_show_platform(win);
		$case X11_ONLY:
			return x11::window_show_platform(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_show_platform(win);
			return x11::window_show_platform(win);
		$default:
			unreachable();
	$endswitch
}

// Window - opengl

fn void? _window_init_opengl_context_platform(GfxWindow* win, GfxGlContext* context, GfxGlHints* hints)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_init_opengl_context_platform(win, context, hints);
		$case $feat(WASM):
			return wasm::window_init_opengl_context_platform(win, context, hints);
		$case $feat(WIN32):
			return win32::window_init_opengl_context_platform(win, context, hints);
		$case WAYLAND_ONLY:
			return wl::window_init_opengl_context_platform(win, context, hints);
		$case X11_ONLY:
			return x11::window_init_opengl_context_platform(win, context, hints);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_init_opengl_context_platform(win, context, hints);
			return x11::window_init_opengl_context_platform(win, context, hints);
		$default:
			unreachable();
	$endswitch
}

fn void _window_make_current_opengl_context(GfxWindow* win)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_make_current_opengl_context(win);
		$case $feat(WASM):
			return wasm::window_make_current_opengl_context(win);
		$case $feat(WIN32):
			return win32::window_make_current_opengl_context(win);
		$case WAYLAND_ONLY:
			return wl::window_make_current_opengl_context(win);
		$case X11_ONLY:
			return x11::window_make_current_opengl_context(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_make_current_opengl_context(win);
			return x11::window_make_current_opengl_context(win);
		$default:
			unreachable();
	$endswitch
}

fn void _window_opengl_swap_buffers(GfxWindow* win)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_opengl_swap_buffers(win);
		$case $feat(WASM):
			return wasm::window_opengl_swap_buffers(win);
		$case $feat(WIN32):
			return win32::window_opengl_swap_buffers(win);
		$case WAYLAND_ONLY:
			return wl::window_opengl_swap_buffers(win);
		$case X11_ONLY:
			return x11::window_opengl_swap_buffers(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_opengl_swap_buffers(win);
			return x11::window_opengl_swap_buffers(win);
		$default:
			unreachable();
	$endswitch
}

fn void _window_release_opengl_context_platform(GfxWindow* win, GfxGlContext* ctx)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_release_opengl_context_platform(win, ctx);
		$case $feat(WASM):
			return wasm::window_release_opengl_context_platform(win, ctx);
		$case $feat(WIN32):
			return win32::window_release_opengl_context_platform(win, ctx);
		$case WAYLAND_ONLY:
			return wl::window_release_opengl_context_platform(win, ctx);
		$case X11_ONLY:
			return x11::window_release_opengl_context_platform(win, ctx);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_release_opengl_context_platform(win, ctx);
			return x11::window_release_opengl_context_platform(win, ctx);
		$default:
			unreachable();
	$endswitch
}

// Vulkan

fn VkSurfaceKHR? _window_create_vulkan_surface(GfxWindow* win, VkInstance instance) @private
{
	$switch:
		$case $feat(MACOS):
			return macos::window_create_vulkan_surface(win, instance);
		$case $feat(WASM):
			return NOT_IMPLEMENTED~;
		$case $feat(WIN32):
			return win32::window_create_vulkan_surface(win, instance);
		$case WAYLAND_ONLY:
			return wl::window_create_vulkan_surface(win, instance);
		$case X11_ONLY:
			return x11::window_create_vulkan_surface(win, instance);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_create_vulkan_surface(win, instance);
			return x11::window_create_vulkan_surface(win, instance);
		$default:
			unreachable();
	$endswitch
}

// Window WGPU

fn WGPUSurface _window_create_wgpu_surface(GfxWindow* window, WGPUInstance instance)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_create_wgpu_surface(window, instance);
		$case $feat(WASM):
			return wasm::window_create_wgpu_surface(window, instance);
		$case $feat(WIN32):
			return win32::window_create_wgpu_surface(window, instance);
		$case WAYLAND_ONLY:
			return wl::window_create_wgpu_surface(window, instance);
		$case X11_ONLY:
			return x11::window_create_wgpu_surface(window, instance);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_create_wgpu_surface(window, instance);
			return x11::window_create_wgpu_surface(window, instance);
		$default:
			unreachable();
	$endswitch
}

// --- Clipboard

fn GfxDataTransfer? _read_clipboard(Allocator allocator, GfxDataTransferType type)
{
	$switch:
		$case $feat(MACOS):
			return macos::read_clipboard(allocator, type);
		$case $feat(WASM):
			return wasm::read_clipboard(allocator, type);
		$case $feat(WIN32):
			return win32::read_clipboard(allocator, type);
		$case WAYLAND_ONLY:
			return wl::read_clipboard(allocator, type);
		$case X11_ONLY:
			return x11::read_clipboard(allocator, type);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::read_clipboard(allocator, type);
			return x11::read_clipboard(allocator, type);
		$default:
			unreachable();
	$endswitch
}

fn void? _write_clipboard(GfxDataTransfer* data)
{
	$switch:
		$case $feat(MACOS):
			return macos::write_clipboard(data);
		$case $feat(WASM):
			return wasm::write_clipboard(data);
		$case $feat(WIN32):
			return win32::write_clipboard(data);
		$case WAYLAND_ONLY:
			return wl::write_clipboard(data);
		$case X11_ONLY:
			return x11::write_clipboard(data);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::write_clipboard(data);
			return x11::write_clipboard(data);
		$default:
			unreachable();
	$endswitch
}

// --- GfxSurface

fn void? _surface_init(GfxSurface* surface, void* data, int w, int h, GfxFormat format, GfxWindow* window)
{
	$switch:
		$case $feat(MACOS):
			return macos::surface_init(surface, data, w, h, format, window);
		$case $feat(WASM):
			return wasm::surface_init(surface, data, w, h, format, window);
		$case $feat(WIN32):
			return win32::surface_init(surface, data, w, h, format, window);
		$case WAYLAND_ONLY:
			return wl::surface_init(surface, data, w, h, format, window);
		$case X11_ONLY:
			return x11::surface_init(surface, data, w, h, format, window);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::surface_init(surface, data, w, h, format, window);
			return x11::surface_init(surface, data, w, h, format, window);
		$default:
			unreachable();
	$endswitch
}

fn void _surface_release(GfxSurface* surface)
{
	$switch:
		$case $feat(MACOS):
			return macos::surface_release(surface);
		$case $feat(WASM):
			return wasm::surface_release(surface);
		$case $feat(WIN32):
			return win32::surface_release(surface);
		$case WAYLAND_ONLY:
			return wl::surface_release(surface);
		$case X11_ONLY:
			return x11::surface_release(surface);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::surface_release(surface);
			return x11::surface_release(surface);
		$default:
			unreachable();
	$endswitch
}

// --- Vulkan

fn bool _vulkan_get_physical_device_presentation_support(VkInstance instance, VkPhysicalDevice physical_device, uint queue_family_index) @private
{
	$switch:
		$case $feat(MACOS):
			return macos::vulkan_get_physical_device_presentation_support(instance, physical_device, queue_family_index);
		$case $feat(WASM):
			return wasm::vulkan_get_physical_device_presentation_support(instance, physical_device, queue_family_index);
		$case $feat(WIN32):
			return win32::vulkan_get_physical_device_presentation_support(instance, physical_device, queue_family_index);
		$case WAYLAND_ONLY:
			return wl::vulkan_get_physical_device_presentation_support(instance, physical_device, queue_family_index);
		$case X11_ONLY:
			return x11::vulkan_get_physical_device_presentation_support(instance, physical_device, queue_family_index);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::vulkan_get_physical_device_presentation_support(instance, physical_device, queue_family_index);
			return x11::vulkan_get_physical_device_presentation_support(instance, physical_device, queue_family_index);
		$default:
			unreachable();
	$endswitch
}

// --- Opengl

fn void* _get_current_opengl_context()
{
	$switch:
		$case $feat(MACOS):
			return macos::get_current_opengl_context();
		$case $feat(WASM):
			return wasm::get_current_opengl_context();
		$case $feat(WIN32):
			return win32::get_current_opengl_context();
		$case WAYLAND_ONLY:
			return wl::get_current_opengl_context();
		$case X11_ONLY:
			return x11::get_current_opengl_context();
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::get_current_opengl_context();
			return x11::get_current_opengl_context();
		$default:
			unreachable();
	$endswitch
}

fn void* _get_opengl_proc(ZString procname)
{
	$switch:
		$case $feat(MACOS):
			return macos::get_opengl_proc(procname);
		$case $feat(WASM):
			return wasm::get_opengl_proc(procname);
		$case $feat(WIN32):
			return win32::get_opengl_proc(procname);
		$case WAYLAND_ONLY:
			return egl::get_proc(procname);
		$case X11_ONLY:
			return x11::get_opengl_proc(procname);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return egl::get_proc(procname);
			return x11::get_opengl_proc(procname);
		$default:
			unreachable();
	$endswitch
}

fn bool _opengl_extension_is_supported_platform(String extension)
{
	$switch:
		$case $feat(MACOS):
			return macos::opengl_extension_is_supported_platform(extension);
		$case $feat(WASM):
			return wasm::opengl_extension_is_supported_platform(extension);
		$case $feat(WIN32):
			return win32::opengl_extension_is_supported_platform(extension);
		$case WAYLAND_ONLY:
			return wl::opengl_extension_is_supported_platform(extension);
		$case X11_ONLY:
			return x11::opengl_extension_is_supported_platform(extension);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::opengl_extension_is_supported_platform(extension);
			return x11::opengl_extension_is_supported_platform(extension);
		$default:
			unreachable();
	$endswitch
}

// --- Keys

fn void _init_keycodes_platform()
{
	$switch:
		$case $feat(MACOS):
			return macos::init_keycodes_platform();
		$case $feat(WASM):
			return wasm::init_keycodes_platform();
		$case $feat(WIN32):
			return win32::init_keycodes_platform();
		$case WAYLAND_ONLY:
			return wl::init_keycodes_platform();
		$case X11_ONLY:
			return x11::init_keycodes_platform();
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::init_keycodes_platform();
			return x11::init_keycodes_platform();
		$default:
			unreachable();
	$endswitch
}

fn GfxKey _physical_to_mapped_key(GfxKey key)
{
	$switch:
		$case $feat(MACOS):
			return macos::physical_to_mapped_key(key);
		$case $feat(WASM):
			return wasm::physical_to_mapped_key(key);
		$case $feat(WIN32):
			return win32::physical_to_mapped_key(key);
		$case WAYLAND_ONLY:
			return wl::physical_to_mapped_key(key);
		$case X11_ONLY:
			return x11::physical_to_mapped_key(key);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::physical_to_mapped_key(key);
			return x11::physical_to_mapped_key(key);
		$default:
			unreachable();
	$endswitch
}
