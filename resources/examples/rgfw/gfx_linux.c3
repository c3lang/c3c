module std::gfx @feat(X11_WAYLAND, X11, WAYLAND);
import std::math, std::collections::list, libc;

const WAYLAND_ONLY = $feat(WAYLAND) && !$feat(X11_WAYLAND) && !$feat(X11);
const X11_ONLY = !$feat(WAYLAND) && !$feat(X11_WAYLAND) && $feat(X11);

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

fn GfxMonitorMode[] rgfw_monitor_getModesPtr(GfxMonitor* monitor)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_monitor_getModesPtr_Wayland(monitor);
		$case X11_ONLY:
			return rgfw_monitor_getModesPtr_X11(monitor);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_monitor_getModesPtr_Wayland(monitor);
			}
			return rgfw_monitor_getModesPtr_X11(monitor);
	$endswitch
}

fn void _window_show_native(GfxWindow* win)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_showPlatform_Wayland(win);
		$case X11_ONLY:
			return rgfw_window_showPlatform_X11(win);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_window_showPlatform_Wayland(win);
			}
			return rgfw_window_showPlatform_X11(win);
	$endswitch
}

// public
fn sz rgfw_monitor_getGammaRampPtr(GfxMonitor* monitor, RgfwGammaRamp* ramp)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_monitor_getGammaRampPtr_Wayland(monitor, ramp);
		$case X11_ONLY:
			return rgfw_monitor_getGammaRampPtr_X11(monitor, ramp);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_monitor_getGammaRampPtr_Wayland(monitor, ramp);
			}
			return rgfw_monitor_getGammaRampPtr_X11(monitor, ramp);
	$endswitch
}

// public
fn bool rgfw_monitor_setGammaRamp(GfxMonitor* monitor, RgfwGammaRamp* ramp)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_monitor_setGammaRamp_Wayland(monitor, ramp);
		$case X11_ONLY:
			return rgfw_monitor_setGammaRamp_X11(monitor, ramp);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_monitor_setGammaRamp_Wayland(monitor, ramp);
			}
			return rgfw_monitor_setGammaRamp_X11(monitor, ramp);
	$endswitch
}

// public
fn bool rgfw_monitor_setMode(GfxMonitor* mon, GfxMonitorMode* mode)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_monitor_setMode_Wayland(mon, mode);
		$case X11_ONLY:
			return rgfw_monitor_setMode_X11(mon, mode);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_monitor_setMode_Wayland(mon, mode);
			}
			return rgfw_monitor_setMode_X11(mon, mode);
	$endswitch
}

// public
fn RgfwMouse* rgfw_createMouse(char* data, int w, int h, GfxFormat format)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_createMouse_Wayland(data, w, h, format);
		$case X11_ONLY:
			return rgfw_createMouse_X11(data, w, h, format);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_createMouse_Wayland(data, w, h, format);
			}
			return rgfw_createMouse_X11(data, w, h, format);
	$endswitch
}

// public
fn RgfwMouse* rgfw_createMouseStandard(GfxMouseIcon mouse)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_createMouseStandard_Wayland(mouse);
		$case X11_ONLY:
			return rgfw_createMouseStandard_X11(mouse);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_createMouseStandard_Wayland(mouse);
			}
			return rgfw_createMouseStandard_X11(mouse);
	$endswitch
}

// public
fn void rgfw_deinitPlatform()
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_deinitPlatform_Wayland();
		$case X11_ONLY:
			return rgfw_deinitPlatform_X11();
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_deinitPlatform_Wayland();
			}
			return rgfw_deinitPlatform_X11();
	$endswitch
}

// public
fn bool rgfw_extensionSupportedPlatform_OpenGL(ZString extension, sz len)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_extensionSupportedPlatform_OpenGL_Wayland(extension, len);
		$case X11_ONLY:
			return rgfw_extensionSupportedPlatform_OpenGL_X11(extension, len);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_extensionSupportedPlatform_OpenGL_Wayland(extension, len);
			}
			return rgfw_extensionSupportedPlatform_OpenGL_X11(extension, len);
	$endswitch
}

// public
fn void rgfw_freeMouse(RgfwMouse* mouse)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_freeMouse_Wayland(mouse);
		$case X11_ONLY:
			return rgfw_freeMouse_X11(mouse);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_freeMouse_Wayland(mouse);
			}
			return rgfw_freeMouse_X11(mouse);
	$endswitch
}

// public
fn void* rgfw_getCurrentContext_OpenGL()
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_getCurrentContext_OpenGL_Wayland();
		$case X11_ONLY:
			return rgfw_getCurrentContext_OpenGL_X11();
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_getCurrentContext_OpenGL_Wayland();
			}
			return rgfw_getCurrentContext_OpenGL_X11();
	$endswitch
}

// public
fn bool rgfw_getGlobalMouse(int* fX, int* fY)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_getGlobalMouse_Wayland(fX, fY);
		$case X11_ONLY:
			return rgfw_getGlobalMouse_X11(fX, fY);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_getGlobalMouse_Wayland(fX, fY);
			}
			return rgfw_getGlobalMouse_X11(fX, fY);
	$endswitch
}

// public
fn GfxFn _get_opengl_proc_native(ZString procname)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_getProcAddress_OpenGL_Wayland(procname);
		$case X11_ONLY:
			return rgfw_getProcAddress_OpenGL_X11(procname);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_getProcAddress_OpenGL_Wayland(procname);
			}
			return rgfw_getProcAddress_OpenGL_X11(procname);
	$endswitch
}

// public
fn void rgfw_initKeycodesPlatform()
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_initKeycodesPlatform_Wayland();
		$case X11_ONLY:
			return rgfw_initKeycodesPlatform_X11();
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_initKeycodesPlatform_Wayland();
			}
			return rgfw_initKeycodesPlatform_X11();
	$endswitch
}

// public
fn int rgfw_initPlatform(String class_name, GfxInitOption flags)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_initPlatform_Wayland(class_name, flags);
		$case X11_ONLY:
			return rgfw_initPlatform_X11(class_name, flags);
		$default:
			if (!(flags & ENABLE_X11))
			{
				ZString waylandDisplay = getenv("WAYLAND_DISPLAY");
				gfx_use_wayland = waylandDisplay != null && waylandDisplay[0] != 0;
				if (gfx_use_wayland)
				{
					int ret = rgfw_initPlatform_Wayland(class_name, flags);
					if (ret == 0) return 0;
					rgfw_debugCallback(RGFW_TYPE_WARNING, RGFW_WARNING_WAYLAND, "Falling back to X11");
					gfx_use_wayland = false;
				}
			}
			else
			{
				gfx_use_wayland = false;
			}
			return rgfw_initPlatform_X11(class_name, flags);
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
fn void rgfw_monitorNode_free(GfxMonitorNode* node)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_monitorNode_free_Wayland(node);
		$case X11_ONLY:
			return rgfw_monitorNode_free_X11(node);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_monitorNode_free_Wayland(node);
			}
			return rgfw_monitorNode_free_X11(node);
	$endswitch
}

// public
fn bool rgfw_monitor_getWorkarea(GfxMonitor* monitor, int* x, int* y, int* width, int* height)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_monitor_getWorkarea_Wayland(monitor, x, y, width, height);
		$case X11_ONLY:
			return rgfw_monitor_getWorkarea_X11(monitor, x, y, width, height);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_monitor_getWorkarea_Wayland(monitor, x, y, width, height);
			}
			return rgfw_monitor_getWorkarea_X11(monitor, x, y, width, height);
	$endswitch
}

// public
fn bool rgfw_monitor_requestMode(GfxMonitor* mon, GfxMonitorMode* mode, GfxMonitorModeRequest request)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_monitor_requestMode_Wayland(mon, mode, request);
		$case X11_ONLY:
			return rgfw_monitor_requestMode_X11(mon, mode, request);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_monitor_requestMode_Wayland(mon, mode, request);
			}
			return rgfw_monitor_requestMode_X11(mon, mode, request);
	$endswitch
}

// public
fn GfxKey rgfw_physicalToMappedKey(GfxKey key)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_physicalToMappedKey_Wayland(key);
		$case X11_ONLY:
			return rgfw_physicalToMappedKey_X11(key);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_physicalToMappedKey_Wayland(key);
			}
			return rgfw_physicalToMappedKey_X11(key);
	$endswitch
}

// public
fn void rgfw_pollEvents()
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_pollEvents_Wayland();
		$case X11_ONLY:
			return rgfw_pollEvents_X11();
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_pollEvents_Wayland();
			}
			return rgfw_pollEvents_X11();
	$endswitch
}

// public
fn void rgfw_pollMonitors()
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_pollMonitors_Wayland();
		$case X11_ONLY:
			return rgfw_pollMonitors_X11();
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_pollMonitors_Wayland();
			}
			return rgfw_pollMonitors_X11();
	$endswitch
}

// public
fn bool rgfw_readClipboardPtr(GfxDataTransferType requestedType, char* buffer, sz capacity, GfxDataTransfer* dataTransfer)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_readClipboardPtr_Wayland(requestedType, buffer, capacity, dataTransfer);
		$case X11_ONLY:
			return rgfw_readClipboardPtr_X11(requestedType, buffer, capacity, dataTransfer);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_readClipboardPtr_Wayland(requestedType, buffer, capacity, dataTransfer);
			}
			return rgfw_readClipboardPtr_X11(requestedType, buffer, capacity, dataTransfer);
	$endswitch
}

// public
fn void rgfw_window_blitSurface(GfxWindow* win, GfxSurface* surface)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_blitSurface_Wayland(win, surface);
		$case X11_ONLY:
			return rgfw_window_blitSurface_X11(win, surface);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_window_blitSurface_Wayland(win, surface);
			}
			return rgfw_window_blitSurface_X11(win, surface);
	$endswitch
}

// public
fn void rgfw_window_closePlatform(GfxWindow* win)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_closePlatform_Wayland(win);
		$case X11_ONLY:
			return rgfw_window_closePlatform_X11(win);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_window_closePlatform_Wayland(win);
			}
			return rgfw_window_closePlatform_X11(win);
	$endswitch
}

// public
fn bool rgfw_window_createContextPtr_OpenGL(GfxWindow* win, RgfwGlContext* context, RgfwGlHints* hints)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_createContextPtr_OpenGL_Wayland(win, context, hints);
		$case X11_ONLY:
			return rgfw_window_createContextPtr_OpenGL_X11(win, context, hints);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_window_createContextPtr_OpenGL_Wayland(win, context, hints);
			}
			return rgfw_window_createContextPtr_OpenGL_X11(win, context, hints);
	$endswitch
}

// public
fn WGPUSurface rgfw_window_createSurface_WebGPU(GfxWindow* window, WGPUInstance instance)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_createSurface_WebGPU_Wayland(window, instance);
		$case X11_ONLY:
			return rgfw_window_createSurface_WebGPU_X11(window, instance);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_window_createSurface_WebGPU_Wayland(window, instance);
			}
			return rgfw_window_createSurface_WebGPU_X11(window, instance);
	$endswitch
}

// public
fn void rgfw_window_deleteContextPtr_OpenGL(GfxWindow* win, RgfwGlContext* ctx)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_deleteContextPtr_OpenGL_Wayland(win, ctx);
		$case X11_ONLY:
			return rgfw_window_deleteContextPtr_OpenGL_X11(win, ctx);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_window_deleteContextPtr_OpenGL_Wayland(win, ctx);
			}
			return rgfw_window_deleteContextPtr_OpenGL_X11(win, ctx);
	$endswitch
}

// public
fn GfxPoint _window_fetch_size_native(GfxWindow* win)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_fetchSize_Wayland(win);
		$case X11_ONLY:
			return rgfw_window_fetchSize_X11(win);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_window_fetchSize_Wayland(win);
			}
			return rgfw_window_fetchSize_X11(win);
	$endswitch
}

// public
fn void _window_flash_native(GfxWindow* win, GfxFlashRequest request) @private
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_flash_Wayland(win, request);
		$case X11_ONLY:
			return rgfw_window_flash_X11(win, request);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_window_flash_Wayland(win, request);
			}
			return rgfw_window_flash_X11(win, request);
	$endswitch
}

// public
fn void _window_focus_native(GfxWindow* win)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_focus_Wayland(win);
		$case X11_ONLY:
			return rgfw_window_focus_X11(win);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_window_focus_Wayland(win);
			}
			return rgfw_window_focus_X11(win);
	$endswitch
}

fn void _window_hide_native(GfxWindow* win) @private
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_hide_Wayland(win);
		$case X11_ONLY:
			return rgfw_window_hide_X11(win);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_window_hide_Wayland(win);
			}
			return rgfw_window_hide_X11(win);
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
fn void rgfw_window_makeCurrentContext_OpenGL(GfxWindow* win)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_makeCurrentContext_OpenGL_Wayland(win);
		$case X11_ONLY:
			return rgfw_window_makeCurrentContext_OpenGL_X11(win);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_window_makeCurrentContext_OpenGL_Wayland(win);
			}
			return rgfw_window_makeCurrentContext_OpenGL_X11(win);
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
fn void rgfw_window_minimizePlatform(GfxWindow* win)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_minimizePlatform_Wayland(win);
		$case X11_ONLY:
			return rgfw_window_minimizePlatform_X11(win);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_window_minimizePlatform_Wayland(win);
			}
			return rgfw_window_minimizePlatform_X11(win);
	$endswitch
}

// public
fn void rgfw_window_moveMouse(GfxWindow* win, int x, int y)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_moveMouse_Wayland(win, x, y);
		$case X11_ONLY:
			return rgfw_window_moveMouse_X11(win, x, y);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_window_moveMouse_Wayland(win, x, y);
			}
			return rgfw_window_moveMouse_X11(win, x, y);
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

// public
fn void rgfw_window_raise(GfxWindow* win)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_raise_Wayland(win);
		$case X11_ONLY:
			return rgfw_window_raise_X11(win);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_window_raise_Wayland(win);
			}
			return rgfw_window_raise_X11(win);
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
			return rgfw_window_restorePlatform_Wayland(win);
		$case X11_ONLY:
			return rgfw_window_restorePlatform_X11(win);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_window_restorePlatform_Wayland(win);
			}
			return rgfw_window_restorePlatform_X11(win);
	$endswitch
}

// public
fn void _window_set_aspect_ratio_native(GfxWindow* win, int w, int h) @private
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_setAspectRatio_Wayland(win, w, h);
		$case X11_ONLY:
			return rgfw_window_setAspectRatio_X11(win, w, h);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_window_setAspectRatio_Wayland(win, w, h);
			}
			return rgfw_window_setAspectRatio_X11(win, w, h);
	$endswitch
}

// public
fn void _window_set_border_native(GfxWindow* win, bool border)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_setBorder_Wayland(win, border);
		$case X11_ONLY:
			return rgfw_window_setBorder_X11(win, border);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_window_setBorder_Wayland(win, border);
			}
			return rgfw_window_setBorder_X11(win, border);
	$endswitch
}

// public
fn void rgfw_window_setFloating(GfxWindow* win, bool floating)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_setFloating_Wayland(win, floating);
		$case X11_ONLY:
			return rgfw_window_setFloating_X11(win, floating);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_window_setFloating_Wayland(win, floating);
			}
			return rgfw_window_setFloating_X11(win, floating);
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
fn void rgfw_window_setMousePassthrough(GfxWindow* win, bool passthrough)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_setMousePassthrough_Wayland(win, passthrough);
		$case X11_ONLY:
			return rgfw_window_setMousePassthrough_X11(win, passthrough);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_window_setMousePassthrough_Wayland(win, passthrough);
			}
			return rgfw_window_setMousePassthrough_X11(win, passthrough);
	$endswitch
}

// public
fn bool rgfw_window_setMousePlatform(GfxWindow* win, RgfwMouse* mouse)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_setMousePlatform_Wayland(win, mouse);
		$case X11_ONLY:
			return rgfw_window_setMousePlatform_X11(win, mouse);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_window_setMousePlatform_Wayland(win, mouse);
			}
			return rgfw_window_setMousePlatform_X11(win, mouse);
	$endswitch
}

// public
fn void rgfw_window_setOpacity(GfxWindow* win, char opacity)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_setOpacity_Wayland(win, opacity);
		$case X11_ONLY:
			return rgfw_window_setOpacity_X11(win, opacity);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_window_setOpacity_Wayland(win, opacity);
			}
			return rgfw_window_setOpacity_X11(win, opacity);
	$endswitch
}

// public
fn void _window_set_raw_mouse_mode_native(GfxWindow* win, bool state)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_setRawMouseModePlatform_Wayland(win, state);
		$case X11_ONLY:
			return rgfw_window_setRawMouseModePlatform_X11(win, state);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_window_setRawMouseModePlatform_Wayland(win, state);
			}
			return rgfw_window_setRawMouseModePlatform_X11(win, state);
	$endswitch
}

// public
fn void rgfw_window_swapBuffers_OpenGL(GfxWindow* win)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_swapBuffers_OpenGL_Wayland(win);
		$case X11_ONLY:
			return rgfw_window_swapBuffers_OpenGL_X11(win);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_window_swapBuffers_OpenGL_Wayland(win);
			}
			return rgfw_window_swapBuffers_OpenGL_X11(win);
	$endswitch
}

// public
fn bool rgfw_writeClipboard(GfxDataTransfer* data)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_writeClipboard_Wayland(data);
		$case X11_ONLY:
			return rgfw_writeClipboard_X11(data);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_writeClipboard_Wayland(data);
			}
			return rgfw_writeClipboard_X11(data);
	$endswitch
}



// public
fn ulong rgfw_unix_getTimeNS()
{
	Timespec ts;
	clock_gettime(1 /* CLOCK_MONOTONIC */, &ts);
	return (ulong)ts.tv_sec * 1000000000 + (ulong)ts.tv_nsec;
}

// public
fn void? GfxSurface.init(&self, void* data, int w, int h, GfxFormat format, GfxWindow* window = null)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_createSurfacePtr_Wayland(data, w, h, format, self, window);
		$case X11_ONLY:
			return rgfw_createSurfacePtr_X11(data, w, h, format, self, window);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_createSurfacePtr_Wayland(data, w, h, format, self, window);
			}
			return rgfw_createSurfacePtr_X11(data, w, h, format, self, window);
	$endswitch
}

// public
fn void GfxSurface.release(&self)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_surface_freePtr_Wayland(self);
		$case X11_ONLY:
			return rgfw_surface_freePtr_X11(self);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_surface_freePtr_Wayland(self);
			}
			return rgfw_surface_freePtr_X11(self);
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
			return rgfw_window_setName_Wayland(self, name);
		$case X11_ONLY:
			return rgfw_window_setName_X11(self, name);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_window_setName_Wayland(self, name);
			}
			return rgfw_window_setName_X11(self, name);
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
			return rgfw_window_getMonitor_Wayland(win);
		$case X11_ONLY:
			return rgfw_window_getMonitor_X11(win);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_window_getMonitor_Wayland(win);
			}
			return rgfw_window_getMonitor_X11(win);
	$endswitch
}

// public
fn void _window_set_capture_mouse_native(GfxWindow* win, bool state)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_captureMousePlatform_Wayland(win, state);
		$case X11_ONLY:
			return rgfw_window_captureMousePlatform_X11(win, state);
		$default:
			if (gfx_use_wayland)
			{
				return rgfw_window_captureMousePlatform_Wayland(win, state);
			}
			return rgfw_window_captureMousePlatform_X11(win, state);
	$endswitch
}
