module std::gfx @feat(X11_WAYLAND, X11, WAYLAND);
import std::math, std::collections::list, libc, std::time, std::os::env;

const WAYLAND_ONLY = $feat(WAYLAND) && !$feat(X11_WAYLAND) && !$feat(X11);
const X11_ONLY = !$feat(WAYLAND) && !$feat(X11_WAYLAND) && $feat(X11);

bool gfx_use_wayland = false;

struct GfxNativeImage
{
	GfxFormat format;
	char* buffer;
	void* bitmap; // XImage* (X11)
	struct @feat(WAYLAND, X11_WAYLAND)
	{
		WlProxy* pool; // wl_shm_pool* (Wayland)
		int fd; // Wayland
		WlProxy* wlBuffer; // wl_buffer* (Wayland)
	}
}

struct GfxGlContext
{
	struct @feat(X11, X11_WAYLAND)
	{
		GLXContext ctx;
		GLXWindow window;
	}
	struct @feat(X11_WAYLAND, WAYLAND)
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

struct GfxWindowSrc
{
	inline IntRect bounds;
	GfxBackingContext ctx;
	GfxBacking backing;
	struct @feat(X11, X11_WAYLAND)
	{
		XWindow window;
		XWindow parent;
		GContext gc;
		XlibIC ic;
		Clock flashEnd;
		long counter_value;
		XSyncCounter counter;
	}
	struct @feat(X11_WAYLAND, WAYLAND)
	{
		WlProxy* surface;
		WlProxy* xdgSurface;
		WlProxy* xdgToplevel;
		WlProxy* decoration;
		WlProxy* lockedPointer;
		WlProxy* icon;
		WlProxy* dataSource;
		WlProxy* customCursorSurface;
		GfxMonitorNode* activeMonitor;
		uint decorationMode;
		bool pendingActivated;
		bool activated;
		bool resizing;
		bool pendingMaximized;
		bool maximized;
		bool minimized;
		bool configured;
		bool usingCustomCursor;
	}
}


fn GfxMonitorMode[] _monitor_get_modes_native(GfxMonitor* monitor)
{
	$switch:
		$case WAYLAND_ONLY:
			return _monitor_get_modes_wayland(monitor);
		$case X11_ONLY:
			return _monitor_get_modes_x11(monitor);
		$default:
			if (gfx_use_wayland)
			{
				return _monitor_get_modes_wayland(monitor);
			}
			return _monitor_get_modes_x11(monitor);
	$endswitch
}

fn void _window_show_native(GfxWindow* win)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_show_wayland(win);
		$case X11_ONLY:
			return todo_fix_window_showPlatform_X11(win);
		$default:
			if (gfx_use_wayland)
			{
				return _window_show_wayland(win);
			}
			return todo_fix_window_showPlatform_X11(win);
	$endswitch
}

// public
fn sz todo_fix_monitor_getGammaRampPtr(GfxMonitor* monitor, TodoRenameGammaRamp* ramp)
{
	$switch:
		$case WAYLAND_ONLY:
			return _monitor_get_gamma_ramp_wayland(monitor, ramp);
		$case X11_ONLY:
			return todo_fix_monitor_getGammaRampPtr_X11(monitor, ramp);
		$default:
			if (gfx_use_wayland)
			{
				return _monitor_get_gamma_ramp_wayland(monitor, ramp);
			}
			return todo_fix_monitor_getGammaRampPtr_X11(monitor, ramp);
	$endswitch
}

// public
fn bool todo_fix_monitor_setGammaRamp(GfxMonitor* monitor, TodoRenameGammaRamp* ramp)
{
	$switch:
		$case WAYLAND_ONLY:
			return _monitor_set_gamma_ramp_wayland(monitor, ramp);
		$case X11_ONLY:
			return todo_fix_monitor_setGammaRamp_X11(monitor, ramp);
		$default:
			if (gfx_use_wayland)
			{
				return _monitor_set_gamma_ramp_wayland(monitor, ramp);
			}
			return todo_fix_monitor_setGammaRamp_X11(monitor, ramp);
	$endswitch
}

// public
fn bool todo_fix_monitor_setMode(GfxMonitor* mon, GfxMonitorMode* mode)
{
	$switch:
		$case WAYLAND_ONLY:
			return _monitor_set_mode_wayland(mon, mode);
		$case X11_ONLY:
			return todo_fix_monitor_setMode_X11(mon, mode);
		$default:
			if (gfx_use_wayland)
			{
				return _monitor_set_mode_wayland(mon, mode);
			}
			return todo_fix_monitor_setMode_X11(mon, mode);
	$endswitch
}

// public
fn TodoRenameMouse* todo_fix_createMouse(char* data, int w, int h, GfxFormat format)
{
	$switch:
		$case WAYLAND_ONLY:
			return todo_fix_createMouse_Wayland(data, w, h, format);
		$case X11_ONLY:
			return todo_fix_createMouse_X11(data, w, h, format);
		$default:
			if (gfx_use_wayland)
			{
				return todo_fix_createMouse_Wayland(data, w, h, format);
			}
			return todo_fix_createMouse_X11(data, w, h, format);
	$endswitch
}

// public
fn TodoRenameMouse* todo_fix_createMouseStandard(GfxMouseIcon mouse)
{
	$switch:
		$case WAYLAND_ONLY:
			return todo_fix_createMouseStandard_Wayland(mouse);
		$case X11_ONLY:
			return todo_fix_createMouseStandard_X11(mouse);
		$default:
			if (gfx_use_wayland)
			{
				return todo_fix_createMouseStandard_Wayland(mouse);
			}
			return todo_fix_createMouseStandard_X11(mouse);
	$endswitch
}

// public
fn void _destroy_native()
{
	$switch:
		$case WAYLAND_ONLY:
			return _destroy_wayland();
		$case X11_ONLY:
			return _destroy_x11();
		$default:
			if (gfx_use_wayland)
			{
				return _destroy_wayland();
			}
			return _destroy_x11();
	$endswitch
}

// public
fn bool todo_fix_extensionSupportedPlatform_OpenGL(ZString extension, sz len)
{
	$switch:
		$case WAYLAND_ONLY:
			return todo_fix_extensionSupportedPlatform_OpenGL_Wayland(extension, len);
		$case X11_ONLY:
			return todo_fix_extensionSupportedPlatform_OpenGL_X11(extension, len);
		$default:
			if (gfx_use_wayland)
			{
				return todo_fix_extensionSupportedPlatform_OpenGL_Wayland(extension, len);
			}
			return todo_fix_extensionSupportedPlatform_OpenGL_X11(extension, len);
	$endswitch
}

// public
fn void _free_mouse(TodoRenameMouse* mouse)
{
	$switch:
		$case WAYLAND_ONLY:
			return _free_mouse_wayland(mouse);
		$case X11_ONLY:
			return _free_mouse_x11(mouse);
		$default:
			if (gfx_use_wayland)
			{
				return _free_mouse_wayland(mouse);
			}
			return _free_mouse_x11(mouse);
	$endswitch
}

// public
fn void* _get_current_opengl_context_native()
{
	$switch:
		$case WAYLAND_ONLY:
			return todo_fix_getCurrentContext_OpenGL_Wayland();
		$case X11_ONLY:
			return todo_fix_getCurrentContext_OpenGL_X11();
		$default:
			if (gfx_use_wayland)
			{
				return todo_fix_getCurrentContext_OpenGL_Wayland();
			}
			return todo_fix_getCurrentContext_OpenGL_X11();
	$endswitch
}

// public
fn GfxPoint _get_global_mouse_native()
{
	$switch:
		$case WAYLAND_ONLY:
			return _get_global_mouse_wayland();
		$case X11_ONLY:
			return _get_global_mouse_x11();
		$default:
			if (gfx_use_wayland)
			{
				return _get_global_mouse_wayland();
			}
			return _get_global_mouse_x11();
	$endswitch
}

// public
fn GfxFn _get_opengl_proc_native(ZString procname)
{
	$switch:
		$case WAYLAND_ONLY:
			return todo_fix_getProcAddress_OpenGL_Wayland(procname);
		$case X11_ONLY:
			return todo_fix_getProcAddress_OpenGL_X11(procname);
		$default:
			if (gfx_use_wayland)
			{
				return todo_fix_getProcAddress_OpenGL_Wayland(procname);
			}
			return todo_fix_getProcAddress_OpenGL_X11(procname);
	$endswitch
}

// public
fn void todo_fix_initKeycodesPlatform()
{
	$switch:
		$case WAYLAND_ONLY:
			return todo_fix_initKeycodesPlatform_Wayland();
		$case X11_ONLY:
			return todo_fix_initKeycodesPlatform_X11();
		$default:
			if (gfx_use_wayland)
			{
				return todo_fix_initKeycodesPlatform_Wayland();
			}
			return todo_fix_initKeycodesPlatform_X11();
	$endswitch
}

fn void? _create_window_native(String name, GfxWindowFlags flags, GfxWindow* win)
{
	$switch:
		$case WAYLAND_ONLY:
			return _create_window_wayland(name, flags, win);
		$case X11_ONLY:
			return _create_window_x11(name, flags, win);
		$default:
			if (gfx_use_wayland)
			{
				return _create_window_wayland(name, flags, win);
			}
			return _create_window_x11(name, flags, win);
	$endswitch
}


// public
fn void? _init_native(String class_name, GfxInitOption flags)
{
	$switch:
		$case WAYLAND_ONLY:
			return _init_wayland(class_name, flags);
		$case X11_ONLY:
			return _init_x11(class_name, flags);
		$default:
			if (!(flags & ENABLE_X11))
			{
				String wayland_display = env::tget_var("WAYLAND_DISPLAY") ?? "";
				gfx_use_wayland = wayland_display != "";;
				if (gfx_use_wayland)
				{
					if (try _init_wayland(class_name, flags)) return;
					todo_fix_debugCallback(TODO_RENAME_TYPE_WARNING, TODO_RENAME_WARNING_WAYLAND, "Falling back to X11");
					gfx_use_wayland = false;
				}
			}
			else
			{
				gfx_use_wayland = false;
			}
			return _init_x11(class_name, flags);
	$endswitch
}

// public
fn void? _load_opengl_native() @private
{
	$switch:
		$case WAYLAND_ONLY:
			return _load_opengl_wayland();
		$case X11_ONLY:
			return _load_opengl_x11();
		$default:
			if (gfx_use_wayland)
			{
				return _load_opengl_wayland();
			}
			return _load_opengl_x11();
	$endswitch
}

// public
fn void todo_fix_monitorNode_free(GfxMonitorNode* node)
{
	$switch:
		$case WAYLAND_ONLY:
			return _monitornode_free_wayland(node);
		$case X11_ONLY:
			return todo_fix_monitorNode_free_X11(node);
		$default:
			if (gfx_use_wayland)
			{
				return _monitornode_free_wayland(node);
			}
			return todo_fix_monitorNode_free_X11(node);
	$endswitch
}

fn IntRect _monitor_get_workarea(GfxMonitor* monitor)
{
	$switch:
		$case WAYLAND_ONLY:
			return _monitor_get_workarea_wayland(monitor);
		$case X11_ONLY:
			return _monitor_get_workarea_x11(monitor);
		$default:
			if (gfx_use_wayland)
			{
				return _monitor_get_workarea_wayland(monitor);
			}
			return _monitor_get_workarea_x11(monitor);
	$endswitch
}

// public
fn bool todo_fix_monitor_requestMode(GfxMonitor* mon, GfxMonitorMode* mode, GfxMonitorModeRequest request)
{
	$switch:
		$case WAYLAND_ONLY:
			return _monitor_request_mode_wayland(mon, mode, request);
		$case X11_ONLY:
			return todo_fix_monitor_requestMode_X11(mon, mode, request);
		$default:
			if (gfx_use_wayland)
			{
				return _monitor_request_mode_wayland(mon, mode, request);
			}
			return todo_fix_monitor_requestMode_X11(mon, mode, request);
	$endswitch
}

// public
fn GfxKey todo_fix_physicalToMappedKey(GfxKey key)
{
	$switch:
		$case WAYLAND_ONLY:
			return todo_fix_physicalToMappedKey_Wayland(key);
		$case X11_ONLY:
			return todo_fix_physicalToMappedKey_X11(key);
		$default:
			if (gfx_use_wayland)
			{
				return todo_fix_physicalToMappedKey_Wayland(key);
			}
			return todo_fix_physicalToMappedKey_X11(key);
	$endswitch
}

// public
fn void todo_fix_pollEvents()
{
	$switch:
		$case WAYLAND_ONLY:
			return todo_fix_pollEvents_Wayland();
		$case X11_ONLY:
			return todo_fix_pollEvents_X11();
		$default:
			if (gfx_use_wayland)
			{
				return todo_fix_pollEvents_Wayland();
			}
			return todo_fix_pollEvents_X11();
	$endswitch
}

// public
fn void todo_fix_pollMonitors()
{
	$switch:
		$case WAYLAND_ONLY:
			return _poll_monitors_wayland();
		$case X11_ONLY:
			return todo_fix_pollMonitors_X11();
		$default:
			if (gfx_use_wayland)
			{
				return _poll_monitors_wayland();
			}
			return todo_fix_pollMonitors_X11();
	$endswitch
}

// public
fn bool todo_fix_readClipboardPtr(GfxDataTransferType requestedType, char* buffer, sz capacity, GfxDataTransfer* dataTransfer)
{
	$switch:
		$case WAYLAND_ONLY:
			return todo_fix_readClipboardPtr_Wayland(requestedType, buffer, capacity, dataTransfer);
		$case X11_ONLY:
			return todo_fix_readClipboardPtr_X11(requestedType, buffer, capacity, dataTransfer);
		$default:
			if (gfx_use_wayland)
			{
				return todo_fix_readClipboardPtr_Wayland(requestedType, buffer, capacity, dataTransfer);
			}
			return todo_fix_readClipboardPtr_X11(requestedType, buffer, capacity, dataTransfer);
	$endswitch
}

// public
fn void _window_blit_surface_native(GfxWindow* win, GfxSurface* surface)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_blit_surface_wayland(win, surface);
		$case X11_ONLY:
			return _window_blit_surface_x11(win, surface);
		$default:
			if (gfx_use_wayland)
			{
				return _window_blit_surface_wayland(win, surface);
			}
			return _window_blit_surface_x11(win, surface);
	$endswitch
}

fn void _window_close_native(GfxWindow* win)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_close_wayland(win);
		$case X11_ONLY:
			return _window_close_native_x11(win);
		$default:
			if (gfx_use_wayland)
			{
				return _window_close_wayland(win);
			}
			return _window_close_native_x11(win);
	$endswitch
}

fn void? _window_init_opengl_context_native(GfxWindow* win, GfxGlContext* context, GfxGlHints* hints)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_init_opengl_context_wayland(win, context, hints);
		$case X11_ONLY:
			return _window_init_opengl_context_x11(win, context, hints);
		$default:
			if (gfx_use_wayland)
			{
				return _window_init_opengl_context_wayland(win, context, hints);
			}
			return _window_init_opengl_context_x11(win, context, hints);
	$endswitch
}

fn void? _window_init_egl_context_native(GfxWindow* win, GfxEglContext* context, GfxGlHints* hints)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_init_egl_context_wayland(win, context, hints);
		$case X11_ONLY:
			return NOT_IMPLEMENTED~;
		$default:
			if (gfx_use_wayland)
			{
				return _window_init_egl_context_wayland(win, context, hints);
			}
			return NOT_IMPLEMENTED~;
	$endswitch
}

// public
fn WGPUSurface todo_fix_window_createSurface_WebGPU(GfxWindow* window, WGPUInstance instance)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_wgpu_create_surface_wayland(window, instance);
		$case X11_ONLY:
			return todo_fix_window_createSurface_WebGPU_X11(window, instance);
		$default:
			if (gfx_use_wayland)
			{
				return _window_wgpu_create_surface_wayland(window, instance);
			}
			return todo_fix_window_createSurface_WebGPU_X11(window, instance);
	$endswitch
}

// public
fn void _window_release_opengl_context_native(GfxWindow* win, GfxGlContext* ctx)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_release_opengl_context_wayland(win, ctx);
		$case X11_ONLY:
			return _window_release_opengl_context_x11(win, ctx);
		$default:
			if (gfx_use_wayland)
			{
				return _window_release_opengl_context_wayland(win, ctx);
			}
			return _window_release_opengl_context_x11(win, ctx);
	$endswitch
}

fn void _window_release_egl_context_native(GfxWindow* win, GfxEglContext* ctx)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_release_egl_context_wayland(win, ctx);
		$case X11_ONLY:
			return _window_release_egl_context_x11(win, ctx);
		$default:
			if (gfx_use_wayland)
			{
				return _window_release_egl_context_wayland(win, ctx);
			}
			return _window_release_egl_context_x11(win, ctx);
	$endswitch
}

// public
fn GfxPoint _window_fetch_size_native(GfxWindow* win)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_fetch_size_wayland(win);
		$case X11_ONLY:
			return _window_fetch_size_x11(win);
		$default:
			if (gfx_use_wayland)
			{
				return _window_fetch_size_wayland(win);
			}
			return _window_fetch_size_x11(win);
	$endswitch
}

fn void _window_flash_native(GfxWindow* win, GfxFlashRequest request)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_flash_wayland(win, request);
		$case X11_ONLY:
			return _window_flash_x11(win, request);
		$default:
			if (gfx_use_wayland)
			{
				return _window_flash_wayland(win, request);
			}
			return _window_flash_x11(win, request);
	$endswitch
}

fn void _window_focus_native(GfxWindow* win)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_focus_wayland(win);
		$case X11_ONLY:
			return _window_focus_x11(win);
		$default:
			if (gfx_use_wayland)
			{
				return _window_focus_wayland(win);
			}
			return _window_focus_x11(win);
	$endswitch
}

fn void _window_hide_native(GfxWindow* win) @private
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_hide_wayland(win);
		$case X11_ONLY:
			return _window_hide_x11(win);
		$default:
			if (gfx_use_wayland)
			{
				return _window_hide_wayland(win);
			}
			return _window_hide_x11(win);
	$endswitch
}

// public
fn bool _window_is_hidden_native(GfxWindow* win) @private
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_is_hidden_wayland(win);
		$case X11_ONLY:
			return _window_is_hidden_x11(win);
		$default:
			if (gfx_use_wayland)
			{
				return _window_is_hidden_wayland(win);
			}
			return _window_is_hidden_x11(win);
	$endswitch
}

fn bool _window_is_maximized_native(GfxWindow* win)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_is_maximized_wayland(win);
		$case X11_ONLY:
			return _window_is_maximized_x11(win);
		$default:
			if (gfx_use_wayland)
			{
				return _window_is_maximized_wayland(win);
			}
			return _window_is_maximized_x11(win);
	$endswitch
}

// public
fn bool _window_is_minimized_native(GfxWindow* win)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_is_minimized_wayland(win);
		$case X11_ONLY:
			return _window_is_minimized_x11(win);
		$default:
			if (gfx_use_wayland)
			{
				return _window_is_minimized_wayland(win);
			}
			return _window_is_minimized_x11(win);
	$endswitch
}

// public
fn void _window_make_opengl_current_context_platform(GfxWindow* win)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_opengl_make_current_context_wayland(win);
		$case X11_ONLY:
			return _win_make_opengl_current_platform_X11(win);
		$default:
			if (gfx_use_wayland)
			{
				return _window_opengl_make_current_context_wayland(win);
			}
			return _win_make_opengl_current_platform_X11(win);
	$endswitch
}

// public
fn void _window_maximize_native(GfxWindow* win)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_maximize_wayland(win);
		$case X11_ONLY:
			return _window_maximize_x11(win);
		$default:
			if (gfx_use_wayland)
			{
				return _window_maximize_wayland(win);
			}
			return _window_maximize_x11(win);
	$endswitch
}

// public
fn void _window_minimize_native(GfxWindow* win)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_minimize_wayland(win);
		$case X11_ONLY:
			return _window_minimize_x11(win);
		$default:
			if (gfx_use_wayland)
			{
				return _window_minimize_wayland(win);
			}
			return _window_minimize_x11(win);
	$endswitch
}

fn void _window_move_mouse(GfxWindow* win, GfxPoint where)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_move_mouse_wayland(win, where);
		$case X11_ONLY:
			return _window_move_mouse_x11(win, where);
		$default:
			if (gfx_use_wayland)
			{
				return _window_move_mouse_wayland(win, where);
			}
			return _window_move_mouse_x11(win, where);
	$endswitch
}

// public
fn void _window_move_native(GfxWindow* win, GfxPoint where)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_move_wayland(win, where);
		$case X11_ONLY:
			return _window_move_x11(win, where);
		$default:
			if (gfx_use_wayland)
			{
				return _window_move_wayland(win, where);
			}
			return _window_move_x11(win, where);
	$endswitch
}

fn void _window_raise_native(GfxWindow* win)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_raise_wayland(win);
		$case X11_ONLY:
			return _window_raise_x11(win);
		$default:
			if (gfx_use_wayland)
			{
				return _window_raise_wayland(win);
			}
			return _window_raise_x11(win);
	$endswitch
}

// public
fn void _window_resize_native(GfxWindow* win, GfxSize size)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_resize_wayland(win, size);
		$case X11_ONLY:
			return _window_resize_x11(win, size);
		$default:
			if (gfx_use_wayland)
			{
				return _window_resize_wayland(win, size);
			}
			return _window_resize_x11(win, size);
	$endswitch
}

// public
fn void _window_restore_native(GfxWindow* win)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_restore_wayland(win);
		$case X11_ONLY:
			return _window_restore_x11(win);
		$default:
			if (gfx_use_wayland)
			{
				return _window_restore_wayland(win);
			}
			return _window_restore_x11(win);
	$endswitch
}

fn void _window_set_aspect_ratio_native(GfxWindow* win, int w, int h)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_set_aspect_ratio_wayland(win, w, h);
		$case X11_ONLY:
			return _window_set_aspect_ratio_x11(win, w, h);
		$default:
			if (gfx_use_wayland)
			{
				return _window_set_aspect_ratio_wayland(win, w, h);
			}
			return _window_set_aspect_ratio_x11(win, w, h);
	$endswitch
}

// public
fn void _window_set_border_native(GfxWindow* win, bool border)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_set_border_wayland(win, border);
		$case X11_ONLY:
			return todo_fix_window_setBorder_X11(win, border);
		$default:
			if (gfx_use_wayland)
			{
				return _window_set_border_wayland(win, border);
			}
			return todo_fix_window_setBorder_X11(win, border);
	$endswitch
}

// public
fn void _window_set_floating_native(GfxWindow* win, bool floating)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_set_floating_wayland(win, floating);
		$case X11_ONLY:
			return todo_fix_window_setFloating_X11(win, floating);
		$default:
			if (gfx_use_wayland)
			{
				return _window_set_floating_wayland(win, floating);
			}
			return todo_fix_window_setFloating_X11(win, floating);
	$endswitch
}

// public
fn void _window_set_fullscreen_native(GfxWindow* win, bool fullscreen)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_set_fullscreen_wayland(win, fullscreen);
		$case X11_ONLY:
			return _window_set_fullscreen_x11(win, fullscreen);
		$default:
			if (gfx_use_wayland)
			{
				return _window_set_fullscreen_wayland(win, fullscreen);
			}
			return _window_set_fullscreen_x11(win, fullscreen);
	$endswitch
}

// public
fn void _window_set_max_size_native(GfxWindow* win, GfxSize size)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_set_max_size_wayland(win, size);
		$case X11_ONLY:
			return _window_set_max_size_x11(win, size);
		$default:
			if (gfx_use_wayland)
			{
				return _window_set_max_size_wayland(win, size);
			}
			return _window_set_max_size_x11(win, size);
	$endswitch
}

// public
fn void _window_set_min_size_native(GfxWindow* win, GfxSize size)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_set_min_size_wayland(win, size);
		$case X11_ONLY:
			return _window_set_min_size_x11(win, size);
		$default:
			if (gfx_use_wayland)
			{
				return _window_set_min_size_wayland(win, size);
			}
			return _window_set_min_size_x11(win, size);
	$endswitch
}

// public
fn void todo_fix_window_setMousePassthrough(GfxWindow* win, bool passthrough)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_set_mouse_passthrough_wayland(win, passthrough);
		$case X11_ONLY:
			return todo_fix_window_setMousePassthrough_X11(win, passthrough);
		$default:
			if (gfx_use_wayland)
			{
				return _window_set_mouse_passthrough_wayland(win, passthrough);
			}
			return todo_fix_window_setMousePassthrough_X11(win, passthrough);
	$endswitch
}

// public
fn bool _window_set_mouse_native(GfxWindow* win, TodoRenameMouse* mouse)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_set_mouse_wayland(win, mouse);
		$case X11_ONLY:
			return _window_set_mouse_x11(win, mouse);
		$default:
			if (gfx_use_wayland)
			{
				return _window_set_mouse_wayland(win, mouse);
			}
			return _window_set_mouse_x11(win, mouse);
	$endswitch
}

// public
fn void _window_set_opacity_native(GfxWindow* win, char opacity)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_set_opacity_wayland(win, opacity);
		$case X11_ONLY:
			return _window_set_opacity_x11(win, opacity);
		$default:
			if (gfx_use_wayland)
			{
				return _window_set_opacity_wayland(win, opacity);
			}
			return _window_set_opacity_x11(win, opacity);
	$endswitch
}

// public
fn void _window_set_raw_mouse_mode_native(GfxWindow* win, bool state)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_set_raw_mouse_mode_wayland(win, state);
		$case X11_ONLY:
			return todo_fix_window_setRawMouseModePlatform_X11(win, state);
		$default:
			if (gfx_use_wayland)
			{
				return _window_set_raw_mouse_mode_wayland(win, state);
			}
			return todo_fix_window_setRawMouseModePlatform_X11(win, state);
	$endswitch
}

// public
fn void _window_swap_opengl_buffers_native(GfxWindow* win)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_opengl_swap_buffers_wayland(win);
		$case X11_ONLY:
			return todo_fix_window_swapBuffers_OpenGL_X11(win);
		$default:
			if (gfx_use_wayland)
			{
				return _window_opengl_swap_buffers_wayland(win);
			}
			return todo_fix_window_swapBuffers_OpenGL_X11(win);
	$endswitch
}

// public
fn void? _write_clipboard_native(GfxDataTransfer* data)
{
	$switch:
		$case WAYLAND_ONLY:
			return _write_clipboard_wayland(data);
		$case X11_ONLY:
			return _write_clipboard_x11(data);
		$default:
			if (gfx_use_wayland)
			{
				return _write_clipboard_wayland(data);
			}
			return _write_clipboard_x11(data);
	$endswitch
}




// public
fn void? _surface_init_native(GfxSurface* surface, void* data, int w, int h, GfxFormat format, GfxWindow* window)
{
	$switch:
		$case WAYLAND_ONLY:
			return _init_surface_wayland(surface, data, w, h, format, window);
		$case X11_ONLY:
			return _init_surface_x11(surface, data, w, h, format, window);
		$default:
			if (gfx_use_wayland)
			{
				return _init_surface_wayland(surface, data, w, h, format, window);
			}
			return _init_surface_x11(surface, data, w, h, format, window);
	$endswitch
}

// public
fn void _surface_release_native(GfxSurface* surface)
{
	$switch:
		$case WAYLAND_ONLY:
			return _surface_release_wayland(surface);
		$case X11_ONLY:
			return _surface_release_x11(surface);
		$default:
			if (gfx_use_wayland)
			{
				return _surface_release_wayland(surface);
			}
			return _surface_release_x11(surface);
	$endswitch
}

fn bool _window_is_floating_native(GfxWindow* win) @private
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_is_floating_wayland(win);
		$case X11_ONLY:
			return _window_is_floating_x11(win);
		$default:
			if (gfx_use_wayland)
			{
				return _window_is_floating_wayland(win);
			}
			return _window_is_floating_x11(win);
	$endswitch
}

// public
fn void GfxWindow.set_name(&self, String name)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_set_name_wayland(self, name);
		$case X11_ONLY:
			return todo_fix_window_setName_X11(self, name);
		$default:
			if (gfx_use_wayland)
			{
				return _window_set_name_wayland(self, name);
			}
			return todo_fix_window_setName_X11(self, name);
	$endswitch
}

// public
fn bool _window_set_icon_native(GfxWindow* win, void* data_src, int w, int h, GfxFormat format, GfxWindowIconType type = BOTH)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_set_icon_wayland(win, data_src, w, h, format, type);
		$case X11_ONLY:
			return _window_set_icon_x11(win, data_src, w, h, format, type);
		$default:
			if (gfx_use_wayland)
			{
				return _window_set_icon_wayland(win, data_src, w, h, format, type);
			}
			return _window_set_icon_x11(win, data_src, w, h, format, type);
	$endswitch
}

// public
fn GfxMonitor* find_monitor(GfxWindow* win)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_get_monitor(win);
		$case X11_ONLY:
			return todo_fix_window_getMonitor_X11(win);
		$default:
			if (gfx_use_wayland)
			{
				return _window_get_monitor(win);
			}
			return todo_fix_window_getMonitor_X11(win);
	$endswitch
}

// public
fn void _window_set_capture_mouse_native(GfxWindow* win, bool state)
{
	$switch:
		$case WAYLAND_ONLY:
			return _window_capture_mouse_wayland(win, state);
		$case X11_ONLY:
			return todo_fix_window_captureMousePlatform_X11(win, state);
		$default:
			if (gfx_use_wayland)
			{
				return _window_capture_mouse_wayland(win, state);
			}
			return todo_fix_window_captureMousePlatform_X11(win, state);
	$endswitch
}

fn VkSurfaceKHR? _window_create_vulkan_surface_native(GfxWindow* win, VkInstance instance) @private
{
	ZString name;
	int type;
	$switch:
		$case WAYLAND_ONLY:
			return _window_create_vulkan_surface_wayland(win, instance);
		$case X11_ONLY:
			return _window_create_vulkan_surface_x11(win, instance);
		$default:
			return gfx_use_wayland
				? _window_create_vulkan_surface_wayland(win, instance)
				: _window_create_vulkan_surface_x11(win, instance);
	$endswitch
}

fn bool _vulkan_get_physical_device_presentation_support_native(VkInstance instance, VkPhysicalDevice physical_device, uint queue_family_index) @private
{
	ZString name;
	int type;
	$switch:
		$case WAYLAND_ONLY:
			return _vulkan_get_physical_device_presentation_support_wayland(instance, physical_device, queue_family_index);
		$case X11_ONLY:
			return _vulkan_get_physical_device_presentation_support_x11(instance, physical_device, queue_family_index);
		$default:
			return gfx_use_wayland
			? _vulkan_get_physical_device_presentation_support_wayland(instance, physical_device, queue_family_index)
			: _vulkan_get_physical_device_presentation_support_x11(instance, physical_device, queue_family_index);
	$endswitch
}
