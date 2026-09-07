module std::gfx @feat(MACOS) @private @link("Cocoa.framework", "CoreGraphics.framework", "ApplicationServices.framework", "QuartzCore.framework", "Carbon.framework", "CoreFoundation.framework", "objc");
import std::os::macos;

struct GfxNativeImage
{
	GfxFormat format;
	char* buffer;
	void* native0;
	void* native1;
	void* native2;
}

fn void _window_set_aspect_ratio(GfxWindow* win, int w, int h)
{
	if (!win.src.window) return;
	if (w == 0 && h == 0)
	{
		w = 1;
		h = 1;
	}
	CGSize size = { w, h };
	objc_send((ObjcId)win.src.window, ObjcSendVoidSize, "setContentAspectRatio:", size);
}

// private
fn void _monitor_from_display(CGDirectDisplayID display, GfxMonitor* mon)
{
	*mon = {};
	mem::copy(&mon.name, "Display", 8);
	CGRect bounds = cgDisplayBounds(display);
	CGDisplayModeRef mode = cgDisplayCopyDisplayMode(display);
	mon.origin = { (int)bounds.origin.x, (int)bounds.origin.y };
	mon.mode.w = mode ? (int)cgDisplayModeGetWidth(mode) : (int)bounds.size.width;
	mon.mode.h = mode ? (int)cgDisplayModeGetHeight(mode) : (int)bounds.size.height;
	mon.mode.refresh_rate = mode ? (float)cgDisplayModeGetRefreshRate(mode) : 60.0;
	mon.mode.red = 8; mon.mode.green = 8; mon.mode.blue = 8;
	mon.mode.src = mode;
	mon.phys_size = (float[<2>])(double[<2>]){ ...cgDisplayScreenSize(display) };
	mon.scale = {
		mon.mode.w > 0 ? ((float)bounds.size.width / (float)mon.mode.w) : 1.0,
		mon.mode.h > 0 ? ((float)bounds.size.height / (float)mon.mode.h) : 1.0
	};
	mon.pixelRatio = mon.scale.x;
}

fn void rgfw_window_showPlatform(GfxWindow* win)
{
	if (win == null || win.src.window == null) return;
	objc_send((ObjcId)win.src.window, ObjcSendVoidId, "makeKeyAndOrderFront:", null);
	if (_gfx_ctx.nsApp != null) objc_send(_gfx_ctx.nsApp, ObjcSendVoidBool, "activateIgnoringOtherApps:", true);
	win.internal.flags.hide = false;
}