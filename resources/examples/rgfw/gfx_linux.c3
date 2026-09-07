module std::gfx @feat(X11_WAYLAND, X11, WAYLAND);
import std::math, std::collections::list, libc;

const WAYLAND_ONLY = $feat(WAYLAND) && !$feat(X11_WAYLAND) && !$feat(X11);
const X11_ONLY = !$feat(WAYLAND) && !$feat(X11_WAYLAND) && $feat(X11);

fn GfxMonitorMode[] rgfw_monitor_getModesPtr(GfxMonitor* monitor)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_monitor_getModesPtr_Wayland(monitor);
		$case X11_ONLY:
			return rgfw_monitor_getModesPtr_X11(monitor);
		$default:
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_monitor_getModesPtr_Wayland(monitor);
			}
			return rgfw_monitor_getModesPtr_X11(monitor);
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
			if (rgfw_dynamicUsingWayland)
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
			if (rgfw_dynamicUsingWayland)
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
			if (rgfw_dynamicUsingWayland)
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
			if (rgfw_dynamicUsingWayland)
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
			if (rgfw_dynamicUsingWayland)
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
			if (rgfw_dynamicUsingWayland)
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
			if (rgfw_dynamicUsingWayland)
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
			if (rgfw_dynamicUsingWayland)
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
			if (rgfw_dynamicUsingWayland)
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
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_getGlobalMouse_Wayland(fX, fY);
			}
			return rgfw_getGlobalMouse_X11(fX, fY);
	$endswitch
}

// public
fn GfxFn rgfw_getProcAddress_OpenGL(ZString procname)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_getProcAddress_OpenGL_Wayland(procname);
		$case X11_ONLY:
			return rgfw_getProcAddress_OpenGL_X11(procname);
		$default:
			if (rgfw_dynamicUsingWayland)
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
			if (rgfw_dynamicUsingWayland)
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
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_initPlatform_Wayland(class_name, flags);
			}
			return rgfw_initPlatform_X11(class_name, flags);
	$endswitch
}

// public
fn bool rgfw_loadGL()
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_loadGL_Wayland();
		$case X11_ONLY:
			return rgfw_loadGL_X11();
		$default:
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_loadGL_Wayland();
			}
			return rgfw_loadGL_X11();
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
			if (rgfw_dynamicUsingWayland)
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
			if (rgfw_dynamicUsingWayland)
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
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_monitor_requestMode_Wayland(mon, mode, request);
			}
			return rgfw_monitor_requestMode_X11(mon, mode, request);
	$endswitch
}

// public
fn RgfwKey rgfw_physicalToMappedKey(RgfwKey key)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_physicalToMappedKey_Wayland(key);
		$case X11_ONLY:
			return rgfw_physicalToMappedKey_X11(key);
		$default:
			if (rgfw_dynamicUsingWayland)
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
			if (rgfw_dynamicUsingWayland)
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
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_pollMonitors_Wayland();
			}
			return rgfw_pollMonitors_X11();
	$endswitch
}

// public
fn bool rgfw_readClipboardPtr(RgfwDataTransferType requestedType, char* buffer, sz capacity, GfxDataTransfer* dataTransfer)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_readClipboardPtr_Wayland(requestedType, buffer, capacity, dataTransfer);
		$case X11_ONLY:
			return rgfw_readClipboardPtr_X11(requestedType, buffer, capacity, dataTransfer);
		$default:
			if (rgfw_dynamicUsingWayland)
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
			if (rgfw_dynamicUsingWayland)
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
			if (rgfw_dynamicUsingWayland)
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
			if (rgfw_dynamicUsingWayland)
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
			if (rgfw_dynamicUsingWayland)
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
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_window_deleteContextPtr_OpenGL_Wayland(win, ctx);
			}
			return rgfw_window_deleteContextPtr_OpenGL_X11(win, ctx);
	$endswitch
}

// public
fn IntVec2 rgfw_window_fetchSize(GfxWindow* win)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_fetchSize_Wayland(win);
		$case X11_ONLY:
			return rgfw_window_fetchSize_X11(win);
		$default:
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_window_fetchSize_Wayland(win);
			}
			return rgfw_window_fetchSize_X11(win);
	$endswitch
}

// public
fn void rgfw_window_flash(GfxWindow* win, GfxFlashRequest request)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_flash_Wayland(win, request);
		$case X11_ONLY:
			return rgfw_window_flash_X11(win, request);
		$default:
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_window_flash_Wayland(win, request);
			}
			return rgfw_window_flash_X11(win, request);
	$endswitch
}

// public
fn void rgfw_window_focus(GfxWindow* win)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_focus_Wayland(win);
		$case X11_ONLY:
			return rgfw_window_focus_X11(win);
		$default:
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_window_focus_Wayland(win);
			}
			return rgfw_window_focus_X11(win);
	$endswitch
}

// public
fn void rgfw_window_hide(GfxWindow* win)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_hide_Wayland(win);
		$case X11_ONLY:
			return rgfw_window_hide_X11(win);
		$default:
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_window_hide_Wayland(win);
			}
			return rgfw_window_hide_X11(win);
	$endswitch
}

// public
fn bool rgfw_window_isHidden(GfxWindow* win)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_isHidden_Wayland(win);
		$case X11_ONLY:
			return rgfw_window_isHidden_X11(win);
		$default:
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_window_isHidden_Wayland(win);
			}
			return rgfw_window_isHidden_X11(win);
	$endswitch
}

// public
fn bool rgfw_window_isMaximized(GfxWindow* win)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_isMaximized_Wayland(win);
		$case X11_ONLY:
			return rgfw_window_isMaximized_X11(win);
		$default:
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_window_isMaximized_Wayland(win);
			}
			return rgfw_window_isMaximized_X11(win);
	$endswitch
}

// public
fn bool rgfw_window_isMinimized(GfxWindow* win)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_isMinimized_Wayland(win);
		$case X11_ONLY:
			return rgfw_window_isMinimized_X11(win);
		$default:
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_window_isMinimized_Wayland(win);
			}
			return rgfw_window_isMinimized_X11(win);
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
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_window_makeCurrentContext_OpenGL_Wayland(win);
			}
			return rgfw_window_makeCurrentContext_OpenGL_X11(win);
	$endswitch
}

// public
fn void rgfw_window_maximizePlatform(GfxWindow* win)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_maximizePlatform_Wayland(win);
		$case X11_ONLY:
			return rgfw_window_maximizePlatform_X11(win);
		$default:
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_window_maximizePlatform_Wayland(win);
			}
			return rgfw_window_maximizePlatform_X11(win);
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
			if (rgfw_dynamicUsingWayland)
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
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_window_moveMouse_Wayland(win, x, y);
			}
			return rgfw_window_moveMouse_X11(win, x, y);
	$endswitch
}

// public
fn void rgfw_window_movePlatform(GfxWindow* win, int x, int y)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_movePlatform_Wayland(win, x, y);
		$case X11_ONLY:
			return rgfw_window_movePlatform_X11(win, x, y);
		$default:
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_window_movePlatform_Wayland(win, x, y);
			}
			return rgfw_window_movePlatform_X11(win, x, y);
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
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_window_raise_Wayland(win);
			}
			return rgfw_window_raise_X11(win);
	$endswitch
}

// public
fn void rgfw_window_resizePlatform(GfxWindow* win, int w, int h)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_resizePlatform_Wayland(win, w, h);
		$case X11_ONLY:
			return rgfw_window_resizePlatform_X11(win, w, h);
		$default:
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_window_resizePlatform_Wayland(win, w, h);
			}
			return rgfw_window_resizePlatform_X11(win, w, h);
	$endswitch
}

// public
fn void rgfw_window_restorePlatform(GfxWindow* win)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_restorePlatform_Wayland(win);
		$case X11_ONLY:
			return rgfw_window_restorePlatform_X11(win);
		$default:
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_window_restorePlatform_Wayland(win);
			}
			return rgfw_window_restorePlatform_X11(win);
	$endswitch
}

// public
fn void rgfw_window_setAspectRatio(GfxWindow* win, int w, int h)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_setAspectRatio_Wayland(win, w, h);
		$case X11_ONLY:
			return rgfw_window_setAspectRatio_X11(win, w, h);
		$default:
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_window_setAspectRatio_Wayland(win, w, h);
			}
			return rgfw_window_setAspectRatio_X11(win, w, h);
	$endswitch
}

// public
fn void rgfw_window_setBorder(GfxWindow* win, bool border)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_setBorder_Wayland(win, border);
		$case X11_ONLY:
			return rgfw_window_setBorder_X11(win, border);
		$default:
			if (rgfw_dynamicUsingWayland)
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
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_window_setFloating_Wayland(win, floating);
			}
			return rgfw_window_setFloating_X11(win, floating);
	$endswitch
}

// public
fn void rgfw_window_setFullscreen(GfxWindow* win, bool fullscreen)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_setFullscreen_Wayland(win, fullscreen);
		$case X11_ONLY:
			return rgfw_window_setFullscreen_X11(win, fullscreen);
		$default:
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_window_setFullscreen_Wayland(win, fullscreen);
			}
			return rgfw_window_setFullscreen_X11(win, fullscreen);
	$endswitch
}

// public
fn void rgfw_window_setMaxSize(GfxWindow* win, int w, int h)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_setMaxSize_Wayland(win, w, h);
		$case X11_ONLY:
			return rgfw_window_setMaxSize_X11(win, w, h);
		$default:
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_window_setMaxSize_Wayland(win, w, h);
			}
			return rgfw_window_setMaxSize_X11(win, w, h);
	$endswitch
}

// public
fn void rgfw_window_setMinSize(GfxWindow* win, int w, int h)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_setMinSize_Wayland(win, w, h);
		$case X11_ONLY:
			return rgfw_window_setMinSize_X11(win, w, h);
		$default:
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_window_setMinSize_Wayland(win, w, h);
			}
			return rgfw_window_setMinSize_X11(win, w, h);
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
			if (rgfw_dynamicUsingWayland)
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
			if (rgfw_dynamicUsingWayland)
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
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_window_setOpacity_Wayland(win, opacity);
			}
			return rgfw_window_setOpacity_X11(win, opacity);
	$endswitch
}

// public
fn void rgfw_window_setRawMouseModePlatform(GfxWindow* win, bool state)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_setRawMouseModePlatform_Wayland(win, state);
		$case X11_ONLY:
			return rgfw_window_setRawMouseModePlatform_X11(win, state);
		$default:
			if (rgfw_dynamicUsingWayland)
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
			if (rgfw_dynamicUsingWayland)
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
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_writeClipboard_Wayland(data);
			}
			return rgfw_writeClipboard_X11(data);
	$endswitch
}

// public
fn void rgfw_unloadVulkan()
{
	if (_gfx_ctx.vulkan_handle == null) return;
	libc::dlclose(_gfx_ctx.vulkan_handle);
	_gfx_ctx.vulkan_handle = null;
	_gfx_ctx.vkGetInstanceProcAddress = null;
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
			if (rgfw_dynamicUsingWayland)
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
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_surface_freePtr_Wayland(self);
			}
			return rgfw_surface_freePtr_X11(self);
	$endswitch
}

// public
fn bool GfxWindow.is_floating(&win)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_isFloating_Wayland(win);
		$case X11_ONLY:
			return rgfw_window_isFloating_X11(win);
		$default:
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_window_isFloating_Wayland(win);
			}
			return rgfw_window_isFloating_X11(win);
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
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_window_setName_Wayland(self, name);
			}
			return rgfw_window_setName_X11(self, name);
	$endswitch
}

// public
fn bool GfxWindow.set_icon(&win, char* dataSrc, int w, int h, GfxFormat format, GfxWindowIconType type = BOTH)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_setIconEx_Wayland(win, dataSrc, w, h, format, type);
		$case X11_ONLY:
			return rgfw_window_setIconEx_X11(win, dataSrc, w, h, format, type);
		$default:
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_window_setIconEx_Wayland(win, dataSrc, w, h, format, type);
			}
			return rgfw_window_setIconEx_X11(win, dataSrc, w, h, format, type);
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
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_window_getMonitor_Wayland(win);
			}
			return rgfw_window_getMonitor_X11(win);
	$endswitch
}

// public
fn void rgfw_window_captureMousePlatform(GfxWindow* win, bool state)
{
	$switch:
		$case WAYLAND_ONLY:
			return rgfw_window_captureMousePlatform_Wayland(win, state);
		$case X11_ONLY:
			return rgfw_window_captureMousePlatform_X11(win, state);
		$default:
			if (rgfw_dynamicUsingWayland)
			{
				return rgfw_window_captureMousePlatform_Wayland(win, state);
			}
			return rgfw_window_captureMousePlatform_X11(win, state);
	$endswitch
}
