module std::gfx @feat(LINUX, BSD, MACOS, WASM, WIN32) @private;
import libc, std::math, std::collections::list, std::os, std::gfx::x11 @public, std::gfx::egl @public, std::time;
import std::gfx::wl @public, std::gfx::macos @public, std::gfx::wasm @public;
import std::gfx::win32 @public;

// --- GfxMonitor functions


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
			return x11::monitor_set_gamma_x11(monitor, gamma);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::monitor_set_gamma(monitor, gamma);
			return x11::monitor_set_gamma_x11(monitor, gamma);
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

fn void _window_show(GfxWindow* win)
{
	$switch:
		$case $feat(MACOS):
			return macos::window_show(win);
		$case $feat(WASM):
			return wasm::window_show(win);
		$case $feat(WIN32):
			return win32::window_show(win);
		$case WAYLAND_ONLY:
			return wl::window_show(win);
		$case X11_ONLY:
			return x11::window_show(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_show(win);
			return x11::window_show(win);
		$default:
			unreachable();
	$endswitch
}



fn void _destroy_native()
{
	$switch:
		$case $feat(MACOS):
			return macos::_destroy_native_macos();
		$case $feat(WASM):
			return wasm::_destroy_native_wasm();
		$case $feat(WIN32):
			return win32::_destroy_native_win32();
		$case WAYLAND_ONLY:
			return wl::_destroy_wayland();
		$case X11_ONLY:
			return x11::_destroy_x11();
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_destroy_wayland();
			return x11::_destroy_x11();
		$default:
			unreachable();
	$endswitch
}

fn bool _opengl_extension_is_supported_native(String extension)
{
	$switch:
		$case $feat(MACOS):
			return macos::_opengl_extension_is_supported_native_macos(extension);
		$case $feat(WASM):
			return wasm::_opengl_extension_is_supported_native_wasm(extension);
		$case $feat(WIN32):
			return win32::_opengl_extension_is_supported_native_win32(extension);
		$case WAYLAND_ONLY:
			return wl::_opengl_extension_is_supported_wayland(extension);
		$case X11_ONLY:
			return x11::_opengl_extension_is_supported_x11(extension);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_opengl_extension_is_supported_wayland(extension);
			return x11::_opengl_extension_is_supported_x11(extension);
		$default:
			unreachable();
	$endswitch
}


fn void* _get_current_opengl_context_native()
{
	$switch:
		$case $feat(MACOS):
			return macos::_get_current_opengl_context_native_macos();
		$case $feat(WASM):
			return wasm::_get_current_opengl_context_native_wasm();
		$case $feat(WIN32):
			return win32::_get_current_opengl_context_native_win32();
		$case WAYLAND_ONLY:
			return wl::_get_current_opengl_context_wayland();
		$case X11_ONLY:
			return x11::_get_current_opengl_context_x11();
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_get_current_opengl_context_wayland();
			return x11::_get_current_opengl_context_x11();
		$default:
			unreachable();
	$endswitch
}

fn void* _get_opengl_proc_native(ZString procname)
{
	$switch:
		$case $feat(MACOS):
			return macos::_get_opengl_proc_native_macos(procname);
		$case $feat(WASM):
			return wasm::_get_opengl_proc_native_wasm(procname);
		$case $feat(WIN32):
			return win32::_get_opengl_proc_native_win32(procname);
		$case WAYLAND_ONLY:
			return egl::get_proc(procname);
		$case X11_ONLY:
			return x11::_get_opengl_proc(procname);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return egl::get_proc(procname);
			return x11::_get_opengl_proc(procname);
		$default:
			unreachable();
	$endswitch
}

fn void _init_keycodes_native()
{
	$switch:
		$case $feat(MACOS):
			return macos::_init_keycodes_native_macos();
		$case $feat(WASM):
			return wasm::_init_keycodes_native_wasm();
		$case $feat(WIN32):
			return win32::_init_keycodes_native_win32();
		$case WAYLAND_ONLY:
			return wl::_init_keycodes_native_wayland();
		$case X11_ONLY:
			return x11::_init_keycodes_native_x11();
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_init_keycodes_native_wayland();
			return x11::_init_keycodes_native_x11();
		$default:
			unreachable();
	$endswitch
}

fn void? _create_window_native(String name, GfxWindowFlags flags, GfxWindow* win)
{
	$switch:
		$case $feat(MACOS):
			return macos::_create_window_native_macos(name, flags, win);
		$case $feat(WASM):
			return wasm::_create_window_native_wasm(name, flags, win);
		$case $feat(WIN32):
			return win32::_create_window_native_win32(name, flags, win);
		$case WAYLAND_ONLY:
			return wl::_create_window_wayland(name, flags, win);
		$case X11_ONLY:
			return x11::_create_window_x11(name, flags, win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_create_window_wayland(name, flags, win);
			return x11::_create_window_x11(name, flags, win);
		$default:
			unreachable();
	$endswitch
}

fn void _monitor_node_free_native(GfxMonitorNode* node)
{
	$switch:
		$case $feat(MACOS):
			return macos::_monitor_node_free_native_macos(node);
		$case $feat(WASM):
			return wasm::_monitor_node_free_native_wasm(node);
		$case $feat(WIN32):
			return win32::_monitor_node_free_native_win32(node);
		$case WAYLAND_ONLY:
			return wl::_monitor_node_free_wayland(node);
		$case X11_ONLY:
			return x11::_monitor_node_free_x11(node);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_monitor_node_free_wayland(node);
			return x11::_monitor_node_free_x11(node);
		$default:
			unreachable();
	$endswitch
}

fn IntRect _monitor_get_workarea(GfxMonitor* monitor)
{
	$switch:
		$case $feat(MACOS):
			return macos::_monitor_get_workarea_macos(monitor);
		$case $feat(WASM):
			return wasm::_monitor_get_workarea_wasm(monitor);
		$case $feat(WIN32):
			return win32::_monitor_get_workarea_win32(monitor);
		$case WAYLAND_ONLY:
			return wl::_monitor_get_workarea_wayland(monitor);
		$case X11_ONLY:
			return x11::_monitor_get_workarea_x11(monitor);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_monitor_get_workarea_wayland(monitor);
			return x11::_monitor_get_workarea_x11(monitor);
		$default:
			unreachable();
	$endswitch
}

fn void? _monitor_request_mode_native(GfxMonitor* mon, GfxMonitorMode* mode, GfxMonitorModeRequest request)
{
	$switch:
		$case $feat(MACOS):
			return macos::_monitor_request_mode_native_macos(mon, mode, request);
		$case $feat(WASM):
			return wasm::_monitor_request_mode_native_wasm(mon, mode, request);
		$case $feat(WIN32):
			return win32::_monitor_request_mode_native_win32(mon, mode, request);
		$case WAYLAND_ONLY:
			return wl::_monitor_request_mode_wayland(mon, mode, request);
		$case X11_ONLY:
			return x11::_monitor_request_mode_x11(mon, mode, request);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_monitor_request_mode_wayland(mon, mode, request);
			return x11::_monitor_request_mode_x11(mon, mode, request);
		$default:
			unreachable();
	$endswitch
}

fn GfxKey _physical_to_mapped_key_native(GfxKey key)
{
	$switch:
		$case $feat(MACOS):
			return macos::_physical_to_mapped_key_native_macos(key);
		$case $feat(WASM):
			return wasm::_physical_to_mapped_key_native_wasm(key);
		$case $feat(WIN32):
			return win32::_physical_to_mapped_key_native_win32(key);
		$case WAYLAND_ONLY:
			return wl::_physical_to_mapped_key_native_wayland(key);
		$case X11_ONLY:
			return x11::_physical_to_mapped_key_native_x11(key);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_physical_to_mapped_key_native_wayland(key);
			return x11::_physical_to_mapped_key_native_x11(key);
		$default:
			unreachable();
	$endswitch
}

fn void _poll_events_native()
{
	$switch:
		$case $feat(MACOS):
			return macos::_poll_events_native_macos();
		$case $feat(WASM):
			return wasm::_poll_events_native_wasm();
		$case $feat(WIN32):
			return win32::_poll_events_native_win32();
		$case WAYLAND_ONLY:
			return wl::_poll_events_wayland();
		$case X11_ONLY:
			return x11::_poll_events_x11();
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_poll_events_wayland();
			return x11::_poll_events_x11();
		$default:
			unreachable();
	$endswitch
}

fn void _poll_monitors_native()
{
	$switch:
		$case $feat(MACOS):
			return macos::_poll_monitors_native_macos();
		$case $feat(WASM):
			return wasm::_poll_monitors_native_wasm();
		$case $feat(WIN32):
			return win32::_poll_monitors_native_win32();
		$case WAYLAND_ONLY:
			return wl::_poll_monitors_wayland();
		$case X11_ONLY:
			return x11::_poll_monitors_x11();
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_poll_monitors_wayland();
			return x11::_poll_monitors_x11();
		$default:
			unreachable();
	$endswitch
}

fn GfxDataTransfer? _read_clipboard_native(Allocator allocator, GfxDataTransferType type)
{
	$switch:
		$case $feat(MACOS):
			return macos::_read_clipboard_native_macos(allocator, type);
		$case $feat(WASM):
			return wasm::_read_clipboard_native_wasm(allocator, type);
		$case $feat(WIN32):
			return win32::_read_clipboard_native_win32(allocator, type);
		$case WAYLAND_ONLY:
			return wl::_read_clipboard_wayland(allocator, type);
		$case X11_ONLY:
			return x11::_read_clipboard_x11(allocator, type);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_read_clipboard_wayland(allocator, type);
			return x11::_read_clipboard_x11(allocator, type);
		$default:
			unreachable();
	$endswitch
}

fn void _window_blit_surface_native(GfxWindow* win, GfxSurface* surface)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_blit_surface_native_macos(win, surface);
		$case $feat(WASM):
			return wasm::_window_blit_surface_native_wasm(win, surface);
		$case $feat(WIN32):
			return win32::_window_blit_surface_native_win32(win, surface);
		$case WAYLAND_ONLY:
			return wl::_window_blit_surface_wayland(win, surface);
		$case X11_ONLY:
			return x11::_window_blit_surface_x11(win, surface);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_blit_surface_wayland(win, surface);
			return x11::_window_blit_surface_x11(win, surface);
		$default:
			unreachable();
	$endswitch
}

fn void _window_close_native(GfxWindow* win)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_close_native_macos(win);
		$case $feat(WASM):
			return wasm::_window_close_native_wasm(win);
		$case $feat(WIN32):
			return win32::_window_close_native_win32(win);
		$case WAYLAND_ONLY:
			return wl::_window_close_wayland(win);
		$case X11_ONLY:
			return x11::_window_close_native_x11(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_close_wayland(win);
			return x11::_window_close_native_x11(win);
		$default:
			unreachable();
	$endswitch
}

fn void? _window_init_opengl_context_native(GfxWindow* win, GfxGlContext* context, GfxGlHints* hints)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_init_opengl_context_native_macos(win, context, hints);
		$case $feat(WASM):
			return wasm::_window_init_opengl_context_native_wasm(win, context, hints);
		$case $feat(WIN32):
			return win32::_window_init_opengl_context_native_win32(win, context, hints);
		$case WAYLAND_ONLY:
			return wl::_window_init_opengl_context_wayland(win, context, hints);
		$case X11_ONLY:
			return x11::_window_init_opengl_context_x11(win, context, hints);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_init_opengl_context_wayland(win, context, hints);
			return x11::_window_init_opengl_context_x11(win, context, hints);
		$default:
			unreachable();
	$endswitch
}

fn WGPUSurface _window_create_wgpu_surface_native(GfxWindow* window, WGPUInstance instance)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_create_wgpu_surface_native_macos(window, instance);
		$case $feat(WASM):
			return wasm::_window_create_wgpu_surface_native_wasm(window, instance);
		$case $feat(WIN32):
			return win32::_window_create_wgpu_surface_native_win32(window, instance);
		$case WAYLAND_ONLY:
			return wl::_window_create_wgpu_surface_wayland(window, instance);
		$case X11_ONLY:
			return x11::_window_create_wgpu_surface_x11(window, instance);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_create_wgpu_surface_wayland(window, instance);
			return x11::_window_create_wgpu_surface_x11(window, instance);
		$default:
			unreachable();
	$endswitch
}

fn void _window_release_opengl_context_native(GfxWindow* win, GfxGlContext* ctx)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_release_opengl_context_native_macos(win, ctx);
		$case $feat(WASM):
			return wasm::_window_release_opengl_context_native_wasm(win, ctx);
		$case $feat(WIN32):
			return win32::_window_release_opengl_context_native_win32(win, ctx);
		$case WAYLAND_ONLY:
			return wl::_window_release_opengl_context_wayland(win, ctx);
		$case X11_ONLY:
			return x11::_window_release_opengl_context_x11(win, ctx);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_release_opengl_context_wayland(win, ctx);
			return x11::_window_release_opengl_context_x11(win, ctx);
		$default:
			unreachable();
	$endswitch
}

fn GfxPoint _window_fetch_size_native(GfxWindow* win)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_fetch_size_native_macos(win);
		$case $feat(WASM):
			return wasm::_window_fetch_size_native_wasm(win);
		$case $feat(WIN32):
			return win32::_window_fetch_size_native_win32(win);
		$case WAYLAND_ONLY:
			return wl::_window_fetch_size_wayland(win);
		$case X11_ONLY:
			return x11::_window_fetch_size_x11(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_fetch_size_wayland(win);
			return x11::_window_fetch_size_x11(win);
		$default:
			unreachable();
	$endswitch
}

fn void _window_flash_native(GfxWindow* win, GfxFlashRequest request)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_flash_native_macos(win, request);
		$case $feat(WASM):
			return wasm::_window_flash_native_wasm(win, request);
		$case $feat(WIN32):
			return win32::_window_flash_native_win32(win, request);
		$case WAYLAND_ONLY:
			return wl::_window_flash_wayland(win, request);
		$case X11_ONLY:
			return x11::_window_flash_x11(win, request);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_flash_wayland(win, request);
			return x11::_window_flash_x11(win, request);
		$default:
			unreachable();
	$endswitch
}

fn void _window_focus_native(GfxWindow* win)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_focus_native_macos(win);
		$case $feat(WASM):
			return wasm::_window_focus_native_wasm(win);
		$case $feat(WIN32):
			return win32::_window_focus_native_win32(win);
		$case WAYLAND_ONLY:
			return wl::_window_focus_wayland(win);
		$case X11_ONLY:
			return x11::_window_focus_x11(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_focus_wayland(win);
			return x11::_window_focus_x11(win);
		$default:
			unreachable();
	$endswitch
}

fn void _window_hide_native(GfxWindow* win) @private
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_hide_native_macos(win);
		$case $feat(WASM):
			return wasm::_window_hide_native_wasm(win);
		$case $feat(WIN32):
			return win32::_window_hide_native_win32(win);
		$case WAYLAND_ONLY:
			return wl::_window_hide_wayland(win);
		$case X11_ONLY:
			return x11::_window_hide_x11(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_hide_wayland(win);
			return x11::_window_hide_x11(win);
		$default:
			unreachable();
	$endswitch
}

fn bool _window_is_hidden_native(GfxWindow* win) @private
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_is_hidden_native_macos(win);
		$case $feat(WASM):
			return wasm::_window_is_hidden_native_wasm(win);
		$case $feat(WIN32):
			return win32::_window_is_hidden_native_win32(win);
		$case WAYLAND_ONLY:
			return wl::_window_is_hidden_wayland(win);
		$case X11_ONLY:
			return x11::_window_is_hidden_x11(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_is_hidden_wayland(win);
			return x11::_window_is_hidden_x11(win);
		$default:
			unreachable();
	$endswitch
}

fn bool _window_is_maximized_native(GfxWindow* win)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_is_maximized_native_macos(win);
		$case $feat(WASM):
			return wasm::_window_is_maximized_native_wasm(win);
		$case $feat(WIN32):
			return win32::_window_is_maximized_native_win32(win);
		$case WAYLAND_ONLY:
			return wl::_window_is_maximized_wayland(win);
		$case X11_ONLY:
			return x11::_window_is_maximized_x11(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_is_maximized_wayland(win);
			return x11::_window_is_maximized_x11(win);
		$default:
			unreachable();
	$endswitch
}

fn bool _window_is_minimized_native(GfxWindow* win)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_is_minimized_native_macos(win);
		$case $feat(WASM):
			return wasm::_window_is_minimized_native_wasm(win);
		$case $feat(WIN32):
			return win32::_window_is_minimized_native_win32(win);
		$case WAYLAND_ONLY:
			return wl::_window_is_minimized_wayland(win);
		$case X11_ONLY:
			return x11::_window_is_minimized_x11(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_is_minimized_wayland(win);
			return x11::_window_is_minimized_x11(win);
		$default:
			unreachable();
	$endswitch
}

fn void _window_make_opengl_current_context_native(GfxWindow* win)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_make_opengl_current_context_native_macos(win);
		$case $feat(WASM):
			return wasm::_window_make_opengl_current_context_native_wasm(win);
		$case $feat(WIN32):
			return win32::_window_make_opengl_current_context_native_win32(win);
		$case WAYLAND_ONLY:
			return wl::_window_make_opengl_current_context_wayland(win);
		$case X11_ONLY:
			return x11::_window_make_opengl_current_x11(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_make_opengl_current_context_wayland(win);
			return x11::_window_make_opengl_current_x11(win);
		$default:
			unreachable();
	$endswitch
}

fn void _window_maximize_native(GfxWindow* win)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_maximize_native_macos(win);
		$case $feat(WASM):
			return wasm::_window_maximize_native_wasm(win);
		$case $feat(WIN32):
			return win32::_window_maximize_native_win32(win);
		$case WAYLAND_ONLY:
			return wl::_window_maximize_wayland(win);
		$case X11_ONLY:
			return x11::_window_maximize_x11(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_maximize_wayland(win);
			return x11::_window_maximize_x11(win);
		$default:
			unreachable();
	$endswitch
}

fn void _window_minimize_native(GfxWindow* win)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_minimize_native_macos(win);
		$case $feat(WASM):
			return wasm::_window_minimize_native_wasm(win);
		$case $feat(WIN32):
			return win32::_window_minimize_native_win32(win);
		$case WAYLAND_ONLY:
			return wl::_window_minimize_wayland(win);
		$case X11_ONLY:
			return x11::_window_minimize_x11(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_minimize_wayland(win);
			return x11::_window_minimize_x11(win);
		$default:
			unreachable();
	$endswitch
}

fn void _window_move_mouse(GfxWindow* win, GfxPoint where)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_move_mouse_macos(win, where);
		$case $feat(WASM):
			return wasm::_window_move_mouse_wasm(win, where);
		$case $feat(WIN32):
			return win32::_window_move_mouse_win32(win, where);
		$case WAYLAND_ONLY:
			return wl::_window_move_mouse_wayland(win, where);
		$case X11_ONLY:
			return x11::_window_move_mouse_x11(win, where);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_move_mouse_wayland(win, where);
			return x11::_window_move_mouse_x11(win, where);
		$default:
			unreachable();
	$endswitch
}

fn void _window_move_native(GfxWindow* win, GfxPoint where)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_move_native_macos(win, where);
		$case $feat(WASM):
			return wasm::_window_move_native_wasm(win, where);
		$case $feat(WIN32):
			return win32::_window_move_native_win32(win, where);
		$case WAYLAND_ONLY:
			return wl::_window_move_wayland(win, where);
		$case X11_ONLY:
			return x11::_window_move_x11(win, where);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_move_wayland(win, where);
			return x11::_window_move_x11(win, where);
		$default:
			unreachable();
	$endswitch
}

fn void _window_raise_native(GfxWindow* win)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_raise_native_macos(win);
		$case $feat(WASM):
			return wasm::_window_raise_native_wasm(win);
		$case $feat(WIN32):
			return win32::_window_raise_native_win32(win);
		$case WAYLAND_ONLY:
			return wl::_window_raise_wayland(win);
		$case X11_ONLY:
			return x11::_window_raise_x11(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_raise_wayland(win);
			return x11::_window_raise_x11(win);
		$default:
			unreachable();
	$endswitch
}

fn void _window_resize_native(GfxWindow* win, GfxSize size)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_resize_native_macos(win, size);
		$case $feat(WASM):
			return wasm::_window_resize_native_wasm(win, size);
		$case $feat(WIN32):
			return win32::_window_resize_native_win32(win, size);
		$case WAYLAND_ONLY:
			return wl::_window_resize_wayland(win, size);
		$case X11_ONLY:
			return x11::_window_resize_x11(win, size);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_resize_wayland(win, size);
			return x11::_window_resize_x11(win, size);
		$default:
			unreachable();
	$endswitch
}

fn void _window_restore_native(GfxWindow* win)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_restore_native_macos(win);
		$case $feat(WASM):
			return wasm::_window_restore_native_wasm(win);
		$case $feat(WIN32):
			return win32::_window_restore_native_win32(win);
		$case WAYLAND_ONLY:
			return wl::_window_restore_wayland(win);
		$case X11_ONLY:
			return x11::_window_restore_x11(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_restore_wayland(win);
			return x11::_window_restore_x11(win);
		$default:
			unreachable();
	$endswitch
}

fn void _window_set_aspect_ratio_native(GfxWindow* win, int w, int h)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_set_aspect_ratio_native_macos(win, w, h);
		$case $feat(WASM):
			return wasm::_window_set_aspect_ratio_native_wasm(win, w, h);
		$case $feat(WIN32):
			return win32::_window_set_aspect_ratio_native_win32(win, w, h);
		$case WAYLAND_ONLY:
			return wl::_window_set_aspect_ratio_wayland(win, w, h);
		$case X11_ONLY:
			return x11::_window_set_aspect_ratio_x11(win, w, h);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_set_aspect_ratio_wayland(win, w, h);
			return x11::_window_set_aspect_ratio_x11(win, w, h);
		$default:
			unreachable();
	$endswitch
}

fn void _window_set_border_native(GfxWindow* win, bool border)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_set_border_native_macos(win, border);
		$case $feat(WASM):
			return wasm::_window_set_border_native_wasm(win, border);
		$case $feat(WIN32):
			return win32::_window_set_border_native_win32(win, border);
		$case WAYLAND_ONLY:
			return wl::_window_set_border_wayland(win, border);
		$case X11_ONLY:
			return x11::_window_set_border_x11(win, border);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_set_border_wayland(win, border);
			return x11::_window_set_border_x11(win, border);
		$default:
			unreachable();
	$endswitch
}

fn void _window_set_floating_native(GfxWindow* win, bool floating)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_set_floating_native_macos(win, floating);
		$case $feat(WASM):
			return wasm::_window_set_floating_native_wasm(win, floating);
		$case $feat(WIN32):
			return win32::_window_set_floating_native_win32(win, floating);
		$case WAYLAND_ONLY:
			return wl::_window_set_floating_wayland(win, floating);
		$case X11_ONLY:
			return x11::_window_set_floating_x11(win, floating);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_set_floating_wayland(win, floating);
			return x11::_window_set_floating_x11(win, floating);
		$default:
			unreachable();
	$endswitch
}

fn void _window_set_fullscreen_native(GfxWindow* win, bool fullscreen)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_set_fullscreen_native_macos(win, fullscreen);
		$case $feat(WASM):
			return wasm::_window_set_fullscreen_native_wasm(win, fullscreen);
		$case $feat(WIN32):
			return win32::_window_set_fullscreen_native_win32(win, fullscreen);
		$case WAYLAND_ONLY:
			return wl::_window_set_fullscreen_wayland(win, fullscreen);
		$case X11_ONLY:
			return x11::_window_set_fullscreen_x11(win, fullscreen);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_set_fullscreen_wayland(win, fullscreen);
			return x11::_window_set_fullscreen_x11(win, fullscreen);
		$default:
			unreachable();
	$endswitch
}

fn void _window_set_max_size_native(GfxWindow* win, GfxSize size)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_set_max_size_native_macos(win, size);
		$case $feat(WASM):
			return wasm::_window_set_max_size_native_wasm(win, size);
		$case $feat(WIN32):
			return win32::_window_set_max_size_native_win32(win, size);
		$case WAYLAND_ONLY:
			return wl::_window_set_max_size_wayland(win, size);
		$case X11_ONLY:
			return x11::_window_set_max_size_x11(win, size);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_set_max_size_wayland(win, size);
			return x11::_window_set_max_size_x11(win, size);
		$default:
			unreachable();
	$endswitch
}

fn void _window_set_min_size_native(GfxWindow* win, GfxSize size)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_set_min_size_native_macos(win, size);
		$case $feat(WASM):
			return wasm::_window_set_min_size_native_wasm(win, size);
		$case $feat(WIN32):
			return win32::_window_set_min_size_native_win32(win, size);
		$case WAYLAND_ONLY:
			return wl::_window_set_min_size_wayland(win, size);
		$case X11_ONLY:
			return x11::_window_set_min_size_x11(win, size);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_set_min_size_wayland(win, size);
			return x11::_window_set_min_size_x11(win, size);
		$default:
			unreachable();
	$endswitch
}

fn void _window_set_mouse_passthrough_native(GfxWindow* win, bool passthrough)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_set_mouse_passthrough_native_macos(win, passthrough);
		$case $feat(WASM):
			return wasm::_window_set_mouse_passthrough_native_wasm(win, passthrough);
		$case $feat(WIN32):
			return win32::_window_set_mouse_passthrough_native_win32(win, passthrough);
		$case WAYLAND_ONLY:
			return wl::_window_set_mouse_passthrough_wayland(win, passthrough);
		$case X11_ONLY:
			return x11::_window_set_mouse_passthrough_x11(win, passthrough);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_set_mouse_passthrough_wayland(win, passthrough);
			return x11::_window_set_mouse_passthrough_x11(win, passthrough);
		$default:
			unreachable();
	$endswitch
}

fn bool _window_set_mouse_native(GfxWindow* win, GfxMouse* mouse)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_set_mouse_native_macos(win, mouse);
		$case $feat(WASM):
			return wasm::_window_set_mouse_native_wasm(win, mouse);
		$case $feat(WIN32):
			return win32::_window_set_mouse_native_win32(win, mouse);
		$case WAYLAND_ONLY:
			return wl::_window_set_mouse_wayland(win, mouse);
		$case X11_ONLY:
			return x11::_window_set_mouse_x11(win, mouse);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_set_mouse_wayland(win, mouse);
			return x11::_window_set_mouse_x11(win, mouse);
		$default:
			unreachable();
	$endswitch
}

fn void _window_set_opacity_native(GfxWindow* win, char opacity)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_set_opacity_native_macos(win, opacity);
		$case $feat(WASM):
			return wasm::_window_set_opacity_native_wasm(win, opacity);
		$case $feat(WIN32):
			return win32::_window_set_opacity_native_win32(win, opacity);
		$case WAYLAND_ONLY:
			return wl::_window_set_opacity_wayland(win, opacity);
		$case X11_ONLY:
			return x11::_window_set_opacity_x11(win, opacity);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_set_opacity_wayland(win, opacity);
			return x11::_window_set_opacity_x11(win, opacity);
		$default:
			unreachable();
	$endswitch
}

fn void _window_set_raw_mouse_mode_native(GfxWindow* win, bool state)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_set_raw_mouse_mode_native_macos(win, state);
		$case $feat(WASM):
			return wasm::_window_set_raw_mouse_mode_native_wasm(win, state);
		$case $feat(WIN32):
			return win32::_window_set_raw_mouse_mode_native_win32(win, state);
		$case WAYLAND_ONLY:
			return wl::_window_set_raw_mouse_mode_wayland(win, state);
		$case X11_ONLY:
			return x11::_window_set_raw_mouse_mode_x11(win, state);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_set_raw_mouse_mode_wayland(win, state);
			return x11::_window_set_raw_mouse_mode_x11(win, state);
		$default:
			unreachable();
	$endswitch
}

fn void _window_opengl_swap_buffers_native(GfxWindow* win)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_opengl_swap_buffers_native_macos(win);
		$case $feat(WASM):
			return wasm::_window_opengl_swap_buffers_native_wasm(win);
		$case $feat(WIN32):
			return win32::_window_opengl_swap_buffers_native_win32(win);
		$case WAYLAND_ONLY:
			return wl::_window_opengl_swap_buffers_wayland(win);
		$case X11_ONLY:
			return x11::_window_opengl_swap_buffers_x11(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_opengl_swap_buffers_wayland(win);
			return x11::_window_opengl_swap_buffers_x11(win);
		$default:
			unreachable();
	$endswitch
}

fn void? _write_clipboard_native(GfxDataTransfer* data)
{
	$switch:
		$case $feat(MACOS):
			return macos::_write_clipboard_native_macos(data);
		$case $feat(WASM):
			return wasm::_write_clipboard_native_wasm(data);
		$case $feat(WIN32):
			return win32::_write_clipboard_native_win32(data);
		$case WAYLAND_ONLY:
			return wl::_write_clipboard_wayland(data);
		$case X11_ONLY:
			return x11::_write_clipboard_x11(data);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_write_clipboard_wayland(data);
			return x11::_write_clipboard_x11(data);
		$default:
			unreachable();
	$endswitch
}

fn void? _surface_init_native(GfxSurface* surface, void* data, int w, int h, GfxFormat format, GfxWindow* window)
{
	$switch:
		$case $feat(MACOS):
			return macos::_surface_init_native_macos(surface, data, w, h, format, window);
		$case $feat(WASM):
			return wasm::_surface_init_native_wasm(surface, data, w, h, format, window);
		$case $feat(WIN32):
			return win32::_surface_init_native_win32(surface, data, w, h, format, window);
		$case WAYLAND_ONLY:
			return wl::_init_surface_wayland(surface, data, w, h, format, window);
		$case X11_ONLY:
			return x11::_init_surface_x11(surface, data, w, h, format, window);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_init_surface_wayland(surface, data, w, h, format, window);
			return x11::_init_surface_x11(surface, data, w, h, format, window);
		$default:
			unreachable();
	$endswitch
}

fn void _surface_release_native(GfxSurface* surface)
{
	$switch:
		$case $feat(MACOS):
			return macos::_surface_release_native_macos(surface);
		$case $feat(WASM):
			return wasm::_surface_release_native_wasm(surface);
		$case $feat(WIN32):
			return win32::_surface_release_native_win32(surface);
		$case WAYLAND_ONLY:
			return wl::_surface_release_wayland(surface);
		$case X11_ONLY:
			return x11::_surface_release_x11(surface);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_surface_release_wayland(surface);
			return x11::_surface_release_x11(surface);
		$default:
			unreachable();
	$endswitch
}

fn bool _window_is_floating_native(GfxWindow* win) @private
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_is_floating_native_macos(win);
		$case $feat(WASM):
			return wasm::_window_is_floating_native_wasm(win);
		$case $feat(WIN32):
			return win32::_window_is_floating_native_win32(win);
		$case WAYLAND_ONLY:
			return wl::_window_is_floating_wayland(win);
		$case X11_ONLY:
			return x11::_window_is_floating_x11(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_is_floating_wayland(win);
			return x11::_window_is_floating_x11(win);
		$default:
			unreachable();
	$endswitch
}

fn void _window_set_name_native(GfxWindow* win, String name)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_set_name_native_macos(win, name);
		$case $feat(WASM):
			return wasm::_window_set_name_native_wasm(win, name);
		$case $feat(WIN32):
			return win32::_window_set_name_native_win32(win, name);
		$case WAYLAND_ONLY:
			return wl::_window_set_name_wayland(win, name);
		$case X11_ONLY:
			return x11::_window_set_name_x11(win, name);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_set_name_wayland(win, name);
			return x11::_window_set_name_x11(win, name);
		$default:
			unreachable();
	$endswitch
}

fn bool _window_set_icon_native(GfxWindow* win, void* data_src, int w, int h, GfxFormat format, GfxWindowIconType type = BOTH)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_set_icon_native_macos(win, data_src, w, h, format, type);
		$case $feat(WASM):
			return wasm::_window_set_icon_native_wasm(win, data_src, w, h, format, type);
		$case $feat(WIN32):
			return win32::_window_set_icon_native_win32(win, data_src, w, h, format, type);
		$case WAYLAND_ONLY:
			return wl::_window_set_icon_wayland(win, data_src, w, h, format, type);
		$case X11_ONLY:
			return x11::_window_set_icon_x11(win, data_src, w, h, format, type);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_set_icon_wayland(win, data_src, w, h, format, type);
			return x11::_window_set_icon_x11(win, data_src, w, h, format, type);
		$default:
			unreachable();
	$endswitch
}

fn GfxMonitor*? _window_find_monitor_native(GfxWindow* win)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_find_monitor_native_macos(win);
		$case $feat(WASM):
			return wasm::_window_find_monitor_native_wasm(win);
		$case $feat(WIN32):
			return win32::_window_find_monitor_native_win32(win);
		$case WAYLAND_ONLY:
			return wl::_window_find_monitor_wayland(win);
		$case X11_ONLY:
			return x11::_window_find_monitor_x11(win);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_window_find_monitor_wayland(win);
			return x11::_window_find_monitor_x11(win);
		$default:
			unreachable();
	$endswitch
}

fn void _window_set_capture_mouse_native(GfxWindow* win, bool state)
{
	$switch:
		$case $feat(MACOS):
			return macos::_window_set_capture_mouse_native_macos(win, state);
		$case $feat(WASM):
			return wasm::_window_set_capture_mouse_native_wasm(win, state);
		$case $feat(WIN32):
			return win32::_window_set_capture_mouse_native_win32(win, state);
		$case WAYLAND_ONLY:
			return wl::window_capture_mouse(win, state);
		$case X11_ONLY:
			return x11::_window_capture_mouse_x11(win, state);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::window_capture_mouse(win, state);
			return x11::_window_capture_mouse_x11(win, state);
		$default:
			unreachable();
	$endswitch
}

fn bool _vulkan_get_physical_device_presentation_support_native(VkInstance instance, VkPhysicalDevice physical_device, uint queue_family_index) @private
{
	$switch:
		$case $feat(MACOS):
			return macos::_vulkan_get_physical_device_presentation_support_native_macos(instance, physical_device, queue_family_index);
		$case $feat(WASM):
			return wasm::_vulkan_get_physical_device_presentation_support_native_wasm(instance, physical_device, queue_family_index);
		$case $feat(WIN32):
			return win32::_vulkan_get_physical_device_presentation_support_native_win32(instance, physical_device, queue_family_index);
		$case WAYLAND_ONLY:
			return wl::_vulkan_get_physical_device_presentation_support_wayland(instance, physical_device, queue_family_index);
		$case X11_ONLY:
			return x11::_vulkan_get_physical_device_presentation_support_x11(instance, physical_device, queue_family_index);
		$case X11_OR_WAYLAND:
			if (_gfx.use_wayland) return wl::_vulkan_get_physical_device_presentation_support_wayland(instance, physical_device, queue_family_index);
			return x11::_vulkan_get_physical_device_presentation_support_x11(instance, physical_device, queue_family_index);
		$default:
			unreachable();
	$endswitch
}
