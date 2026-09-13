module std::gfx @feat(MACOS) @link("Cocoa.framework", "CoreGraphics.framework", "ApplicationServices.framework", "QuartzCore.framework", "Carbon.framework", "CoreFoundation.framework", "objc");

import std::collections, std::math, libc, std::os::macos, std::io, std::os::darwin;
// C3 port of the shared RGFW implementation from /Users/lerno/Downloads/RGFW.h.
// This file intentionally imports no C3 standard-library modules. It only
// declares the small C runtime hooks RGFW's C header also expects from the host.

const int NS_OPEN_GL_PFA_DOUBLE_BUFFER = 5;
const int NS_OPEN_GL_PFA_AUX_BUFFERS = 7;
const int NS_OPEN_GL_PFA_COLOR_SIZE = 8;
const int NS_OPEN_GL_PFA_ALPHA_SIZE = 11;
const int NS_OPEN_GL_PFA_DEPTH_SIZE = 12;
const int NS_OPEN_GL_PFA_STENCIL_SIZE = 13;
const int NS_OPEN_GL_PFA_ACCUM_SIZE = 14;
const int NS_OPEN_GL_PFA_CLOSEST_POLICY = 74;
const int NS_OPEN_GL_PFA_ACCELERATED = 73;
const int NS_OPEN_GL_PFA_SAMPLE_BUFFERS = 55;
const int NS_OPEN_GL_PFA_SAMPLES = 56;
const int NS_OPEN_GL_PFA_RENDERER_ID = 70;
const int NS_OPEN_GL_PFA_OPEN_GL_PROFILE = 99;
const int NS_OPEN_GL_PROFILE_LEGACY = 0x1000;
const int NS_OPEN_GL_PROFILE_3_2_CORE = 0x3200;
const int NS_OPEN_GL_PROFILE_4_1_CORE = 0x3200;
const int NS_OPEN_GL_CP_SURFACE_OPACITY = 236;
const int NS_OPEN_GL_CONTEXT_PARAMETER_SWAP_INTERVAL = 222;
const int K_CGL_RENDERER_GENERIC_FLOAT_ID = 0x00020400;

fn void nsOpenGLContext_setValues(ObjId context, int* vals, NSInteger param)
{
	context.send_void("setValues:forParameter:", vals, param);
}

// private
fn CGDirectDisplayID todo_fix_monitorDisplay(GfxMonitor* monitor)
{
	if (monitor != null && monitor.node != null && monitor.node.nativeId0 != 0) return (CGDirectDisplayID)monitor.node.nativeId0;
	return cg::mainDisplayID();
}

// public
fn void todo_fix_monitorNode_free(GfxMonitorNode* node)
{
	if (node.mon.mode.src) CFType.release(node.mon.mode.src);
}





// public
fn void todo_fix_window_makeCurrentWindow_EGL(GfxWindow* win) { _gfx_ctx.current = win; }



