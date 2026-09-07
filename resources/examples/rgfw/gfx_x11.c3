module std::gfx @feat(X11, X11_WAYLAND) @link("X11", "Xext", "Xrandr", "GL") @private;
import std::math;
excuse
{
	X11_WINDOW_CREATION_FAILED
}

// Leading fields of the real XImage struct (ABI-compatible prefix; only used to null
// out `data` before XDestroyImage, since that buffer is owned by our own allocation).
struct XImageHeader
{
	int width;
	int height;
	int xoffset;
	int format;
	char* data;
}
// X11 types

typedef GfxXDisplay = void;
typedef GfxXVisual = void;
alias GfxXWindow = CULong;
alias GfxXDrawable = CULong;
alias GfxXPointer = char*;
alias GfxRROutput = CULong;

// X11 used functions
extern fn GfxXDisplay* xOpenDisplay(ZString name) @cname("XOpenDisplay");
extern fn int xCloseDisplay(GfxXDisplay* d) @cname("XCloseDisplay");
extern fn int xInitThreads() @cname("XInitThreads");
extern fn GfxXWindow xDefaultRootWindow(GfxXDisplay* d) @cname("XDefaultRootWindow");
extern fn GfxXWindow xRootWindow(GfxXDisplay* d, int screen) @cname("XRootWindow");
extern fn int xDefaultScreen(GfxXDisplay* d) @cname("XDefaultScreen");
extern fn GfxXVisual* xDefaultVisual(GfxXDisplay* d, int screen) @cname("XDefaultVisual");
extern fn int xDefaultDepth(GfxXDisplay* d, int screen) @cname("XDefaultDepth");
extern fn GContext xDefaultGC(GfxXDisplay* d, int screen) @cname("XDefaultGC");
extern fn Colormap xCreateColormap(GfxXDisplay* d, GfxXWindow win, GfxXVisual* visual, int alloc) @cname("XCreateColormap");
extern fn int xFreeColors(GfxXDisplay* d, Colormap cm, CULong* pixels, int n, CULong planes) @cname("XFreeColors");
extern fn GfxXWindow xCreateWindow(GfxXDisplay* d, GfxXWindow parent, int x, int y, uint w, uint h, uint borderWidth, int depth, uint c_class, GfxXVisual* visual, CULong valuemask, XSetWindowAttributes* attrs) @cname("XCreateWindow");
extern fn int xDestroyWindow(GfxXDisplay* d, GfxXWindow w) @cname("XDestroyWindow");
extern fn int xMapWindow(GfxXDisplay* d, GfxXWindow w) @cname("XMapWindow");
extern fn int xMapRaised(GfxXDisplay* d, GfxXWindow w) @cname("XMapRaised");
extern fn int xUnmapWindow(GfxXDisplay* d, GfxXWindow w) @cname("XUnmapWindow");
extern fn int xMoveWindow(GfxXDisplay* d, GfxXWindow w, int x, int y) @cname("XMoveWindow");
extern fn int xResizeWindow(GfxXDisplay* d, GfxXWindow w, uint width, uint height) @cname("XResizeWindow");
extern fn int xRaiseWindow(GfxXDisplay* d, GfxXWindow w) @cname("XRaiseWindow");
extern fn GContext xCreateGC(GfxXDisplay* d, GfxXDrawable draw, CULong valuemask, void* values) @cname("XCreateGC");
extern fn int xFreeGC(GfxXDisplay* d, GContext gc) @cname("XFreeGC");
extern fn int xSaveContext(GfxXDisplay* d, XlibId xid, int context, GfxXPointer data) @cname("XSaveContext");
extern fn int xFindContext(GfxXDisplay* d, XlibId xid, int context, GfxXPointer* dataOut) @cname("XFindContext");
extern fn int xDeleteContext(GfxXDisplay* d, XlibId xid, int context) @cname("XDeleteContext");
extern fn int xUniqueContext() @cname("XrmUniqueQuark");
extern fn XlibIC xCreateIC(XlibIM im, ...) @cname("XCreateIC");
extern fn int xDestroyIC(XlibIC ic) @cname("XDestroyIC");
extern fn int xSetICFocus(XlibIC ic) @cname("XSetICFocus");
extern fn int xUnsetICFocus(XlibIC ic) @cname("XUnsetICFocus");
extern fn XlibIM xOpenIM(GfxXDisplay* d, void* db, ZString res_name, ZString res_class) @cname("XOpenIM");
extern fn int xCloseIM(XlibIM im) @cname("XCloseIM");
extern fn ZString xGetIMValues(XlibIM im, ...) @cname("XGetIMValues");
extern fn ZString xSetIMValues(XlibIM im, ...) @cname("XSetIMValues");
extern fn int xRegisterIMInstantiateCallback(GfxXDisplay* d, void* db, ZString res_name, ZString res_class, void* callback, GfxXPointer clientData) @cname("XRegisterIMInstantiateCallback");
extern fn int xUnregisterIMInstantiateCallback(GfxXDisplay* d, void* db, ZString res_name, ZString res_class, void* callback, GfxXPointer clientData) @cname("XUnregisterIMInstantiateCallback");
extern fn ZString xSetLocaleModifiers(ZString modifierList) @cname("XSetLocaleModifiers");
extern fn sz xutf8LookupString(XlibIC ic, XKeyEvent* event, char* buffer, int bytes, KeySym* keysym, int* status) @cname("Xutf8LookupString");
extern fn int xutf8SetWMProperties(GfxXDisplay* d, GfxXWindow w, ZString windowName, ZString iconName, ZString* argv, int argc, XSizeHints* hints, XWMHints* wmHints, void* classHints) @cname("Xutf8SetWMProperties");
extern fn int xSetClassHint(GfxXDisplay* d, GfxXWindow w, XClassHint* hint) @cname("XSetClassHint");
extern fn int xSetWMHints(GfxXDisplay* d, GfxXWindow w, XWMHints* hints) @cname("XSetWMHints");
extern fn XWMHints* xGetWMHints(GfxXDisplay* d, GfxXWindow w) @cname("XGetWMHints");
extern fn int xSetWMProtocols(GfxXDisplay* d, GfxXWindow w, Atom* protocols, int count) @cname("XSetWMProtocols");
extern fn int xSetWMSizeHints(GfxXDisplay* d, GfxXDrawable w, XSizeHints* hints, Atom property) @cname("XSetWMSizeHints");
extern fn int xGetWMNormalHints(GfxXDisplay* d, GfxXWindow w, XSizeHints* hints, CLong* flags) @cname("XGetWMNormalHints");
extern fn int xSetWMNormalHints(GfxXDisplay* d, GfxXWindow w, XSizeHints* hints) @cname("XSetWMNormalHints");
extern fn int xSelectInput(GfxXDisplay* d, GfxXDrawable w, CLong eventMask) @cname("XSelectInput");
extern fn int xStoreName(GfxXDisplay* d, GfxXWindow w, ZString name) @cname("XStoreName");
extern fn int xSetWindowBackground(GfxXDisplay* d, GfxXWindow w, CULong background) @cname("XSetWindowBackground");
extern fn int xSetWindowBackgroundPixmap(GfxXDisplay* d, GfxXWindow w, Pixmap pixmap) @cname("XSetWindowBackgroundPixmap");
extern fn int xClearWindow(GfxXDisplay* d, GfxXWindow w) @cname("XClearWindow");
extern fn int xFlush(GfxXDisplay* d) @cname("XFlush");
extern fn int xSync(GfxXDisplay* d, int discard) @cname("XSync");
extern fn int xPending(GfxXDisplay* d) @cname("XPending");
extern fn int xConnectionNumber(GfxXDisplay* d) @cname("XConnectionNumber");
extern fn int qLength(GfxXDisplay* d) @cname("XQLength");
extern fn int xEventsQueued(GfxXDisplay* d, int mode) @cname("XEventsQueued");
extern fn int xNextEvent(GfxXDisplay* d, XEvent* ev) @cname("XNextEvent");
extern fn int xPeekEvent(GfxXDisplay* d, XEvent* ev) @cname("XPeekEvent");
extern fn int xCheckTypedWindowEvent(GfxXDisplay* d, GfxXWindow w, int eventType, XEvent* ev) @cname("XCheckTypedWindowEvent");
extern fn int xFilterEvent(XEvent* ev, GfxXWindow w) @cname("XFilterEvent");
extern fn int xSendEvent(GfxXDisplay* d, GfxXWindow w, int propagate, CLong eventMask, XEvent* ev) @cname("XSendEvent");
extern fn int xGetEventData(GfxXDisplay* d, XGenericEventCookie* cookie) @cname("XGetEventData");
extern fn int xFreeEventData(GfxXDisplay* d, XGenericEventCookie* cookie) @cname("XFreeEventData");
extern fn int xGetWindowAttributes(GfxXDisplay* d, GfxXWindow w, XWindowAttributes* attrs) @cname("XGetWindowAttributes");
extern fn int xChangeProperty(GfxXDisplay* d, GfxXWindow w, Atom property, Atom type, int format, int mode, char* data, int nelements) @cname("XChangeProperty");
extern fn int xGetWindowProperty(GfxXDisplay* d, GfxXWindow w, Atom property, CLong offset, CLong length, int delet, Atom reqType, Atom* actualType, int* actualFormat, CULong* nitems, CULong* bytesAfter, char** propReturn) @cname("XGetWindowProperty");
extern fn int xDeleteProperty(GfxXDisplay* d, GfxXWindow w, Atom property) @cname("XDeleteProperty");
extern fn Atom xInternAtom(GfxXDisplay* d, ZString name, int onlyIfExists) @cname("XInternAtom");
extern fn int xConvertSelection(GfxXDisplay* d, Atom selection, Atom target, Atom property, GfxXWindow requestor, Time time) @cname("XConvertSelection");
extern fn GfxXWindow xGetSelectionOwner(GfxXDisplay* d, Atom selection) @cname("XGetSelectionOwner");
extern fn int xSetSelectionOwner(GfxXDisplay* d, Atom selection, GfxXWindow owner, Time time) @cname("XSetSelectionOwner");
extern fn int xFree(void* data) @cname("XFree");
extern fn int xSetErrorHandler(void* handler) @cname("XSetErrorHandler");
extern fn int xGetErrorText(GfxXDisplay* d, int code, char* buffer, int len) @cname("XGetErrorText");
extern fn int xQueryPointer(GfxXDisplay* d, GfxXWindow w, GfxXWindow* rootRet, GfxXWindow* childRet, int* rootX, int* rootY, int* winX, int* winY, uint* mask) @cname("XQueryPointer");
extern fn int xWarpPointer(GfxXDisplay* d, GfxXWindow src, GfxXWindow dest, int srcX, int srcY, uint srcW, uint srcH, int destX, int destY) @cname("XWarpPointer");
extern fn int xTranslateCoordinates(GfxXDisplay* d, GfxXWindow src, GfxXWindow dest, int srcX, int srcY, int* destXRet, int* destYRet, GfxXWindow* childRet) @cname("XTranslateCoordinates");
extern fn int xSetInputFocus(GfxXDisplay* d, GfxXWindow w, int revert, Time time) @cname("XSetInputFocus");
extern fn int xGrabPointer(GfxXDisplay* d, GfxXWindow w, int ownerEvents, uint eventMask, int pointerMode, int keyboardMode, GfxXWindow confineTo, XCursor cursor, Time time) @cname("XGrabPointer");
extern fn int xUngrabPointer(GfxXDisplay* d, Time time) @cname("XUngrabPointer");
extern fn int xIconifyWindow(GfxXDisplay* d, GfxXWindow w, int screen) @cname("XIconifyWindow");
extern fn int xDefineCursor(GfxXDisplay* d, GfxXWindow w, XCursor cursor) @cname("XDefineCursor");
extern fn int xFreeCursor(GfxXDisplay* d, XCursor cursor) @cname("XFreeCursor");
extern fn XCursor xCreateFontCursor(GfxXDisplay* d, uint shape) @cname("XCreateFontCursor");
extern fn KeySym xkbKeycodeToKeysym(GfxXDisplay* d, KeyCode kc, int group, int level) @cname("XkbKeycodeToKeysym");
extern fn int xkbGetState(GfxXDisplay* d, uint deviceSpec, void* stateRet) @cname("XkbGetState");
extern fn void* xkbGetMap(GfxXDisplay* d, uint which, uint deviceSpec) @cname("XkbGetMap");
extern fn int xkbGetNames(GfxXDisplay* d, uint which, void* xkb) @cname("XkbGetNames");
extern fn void* xkbGetKeyboardByName(GfxXDisplay* d, uint deviceSpec, void* names, uint want, uint need, int loadKeycodes) @cname("XkbGetKeyboardByName");
extern fn int xkbFreeKeyboard(void* xkb, uint which, int freeAll) @cname("XkbFreeKeyboard");
extern fn int xMatchVisualInfo(GfxXDisplay* d, int screen, int depth, int c_class, XVisualInfo* visualOut) @cname("XMatchVisualInfo");
extern fn int xChangeWindowAttributes(GfxXDisplay* d, GfxXWindow w, CULong valuemask, XSetWindowAttributes* attrs) @cname("XChangeWindowAttributes");
extern fn Region xCreateRegion() @cname("XCreateRegion");
extern fn int xDestroyRegion(Region r) @cname("XDestroyRegion");
extern fn XCursor xCreatePixmap(GfxXDisplay* d, GfxXDrawable draw, uint w, uint h, uint depth) @cname("XCreatePixmap");
extern fn void* xCreateImage(GfxXDisplay* d, GfxXVisual* visual, uint depth, int format, int offset, char* data, uint w, uint h, int bitmapPad, int bytesPerLine) @cname("XCreateImage");
extern fn int xPutImage(GfxXDisplay* d, GfxXDrawable draw, GContext gc, void* image, int srcX, int srcY, int destX, int destY, uint w, uint h) @cname("XPutImage");
extern fn int xDestroyImage(void* image) @cname("XDestroyImage");
extern fn ZString xResourceManagerString(GfxXDisplay* d) @cname("XResourceManagerString");
extern fn void* xrmGetStringDatabase(ZString data) @cname("XrmGetStringDatabase");
extern fn int xrmGetResource(void* db, ZString name, ZString c_class, ZString* typeOut, void* valueOut) @cname("XrmGetResource");
extern fn int xrmDestroyDatabase(void* db) @cname("XrmDestroyDatabase");
extern fn XRRScreenResources* xRRGetScreenResourcesCurrent(GfxXDisplay* d, GfxXWindow w) @cname("XRRGetScreenResourcesCurrent");
extern fn int xRRFreeScreenResources(XRRScreenResources* res) @cname("XRRFreeScreenResources");
extern fn GfxRROutput xRRGetOutputPrimary(GfxXDisplay* d, GfxXWindow w) @cname("XRRGetOutputPrimary");
extern fn GfxXRROutputInfo* xRRGetOutputInfo(GfxXDisplay* d, XRRScreenResources* res, GfxRROutput output) @cname("XRRGetOutputInfo");
extern fn int xRRFreeOutputInfo(GfxXRROutputInfo* info) @cname("XRRFreeOutputInfo");
extern fn XRRCrtcInfo* xRRGetCrtcInfo(GfxXDisplay* d, XRRScreenResources* res, RRCrtc crtc) @cname("XRRGetCrtcInfo");
extern fn int xRRFreeCrtcInfo(XRRCrtcInfo* info) @cname("XRRFreeCrtcInfo");
extern fn int xRRSetCrtcConfig(GfxXDisplay* d, XRRScreenResources* res, RRCrtc crtc, Time time, int x, int y, RRMode mode, int rotation, GfxRROutput* outputs, int noutputs) @cname("XRRSetCrtcConfig");
extern fn int xRRGetCrtcGammaSize(GfxXDisplay* d, RRCrtc crtc) @cname("XRRGetCrtcGammaSize");
extern fn XRRCrtcGamma* xRRGetCrtcGamma(GfxXDisplay* d, RRCrtc crtc) @cname("XRRGetCrtcGamma");
extern fn XRRCrtcGamma* xRRAllocGamma(int size) @cname("XRRAllocGamma");
extern fn int xRRSetCrtcGamma(GfxXDisplay* d, RRCrtc crtc, XRRCrtcGamma* gamma) @cname("XRRSetCrtcGamma");
extern fn int xRRFreeGamma(XRRCrtcGamma* gamma) @cname("XRRFreeGamma");
extern fn int xRRQueryExtension(GfxXDisplay* d, int* eventBase, int* errorBase) @cname("XRRQueryExtension");
extern fn int xRRSelectInput(GfxXDisplay* d, GfxXWindow w, int mask) @cname("XRRSelectInput");

module std::gfx @feat(X11, X11_WAYLAND) @private;
import std::math, std::collections::list, libc;

fn void rgfw_surface_freePtr_X11(GfxSurface* surface)
{
	if (!surface.data) return;
	if (surface.native.buffer) free(surface.native.buffer);
	if (surface.native.bitmap) xDestroyImage(surface.native.bitmap);
	*surface = {};
}

fn void rgfw_window_setBorder_X11(GfxWindow* win, bool border)
{
	win.internal.flags.no_border = !border;
	X11WindowHints hints;
	hints.flags = 2;
	hints.decorations = (CULong)border;
	xChangeProperty(_gfx_ctx.display, win.src.window, _gfx_ctx.motifWmHints, _gfx_ctx.motifWmHints, 32, 0, (char*)&hints, 5);
	if (!rgfw_window_isHidden(win))
	{
		rgfw_window_hide(win);
		show(win);
	}
}

fn void rgfw_window_setRawMouseModePlatform_X11(GfxWindow* win, bool state)
{
	char[4] mask;
	if (state) xiSetMask((char*)&mask, 6 /* XI_RawMotion */);
	XIEventMask em;
	em.deviceid = 1; // XIAllMasterDevices
	em.mask_len = 4;
	em.mask = (char*)&mask;
	if (rgfw_xiSelectEventsSRC) rgfw_xiSelectEventsSRC(_gfx_ctx.display, xDefaultRootWindow(_gfx_ctx.display), &em, 1);
}

fn void rgfw_window_captureMousePlatform_X11(GfxWindow* win, bool state)
{
	if (state)
	{
		uint eventMask = (1 << 2) | (1 << 3) | (1 << 6); // Button press/release + pointer motion
		xGrabPointer(_gfx_ctx.display, win.src.window, X_TRUE, eventMask, 1, 1, win.src.window, 0, 0);
	}
	else
	{
		xUngrabPointer(_gfx_ctx.display, 0);
	}
}

// private
fn WGPUSurface rgfw_window_createSurface_WebGPU_X11(GfxWindow* window, WGPUInstance instance)
{
	WgpuSurfaceSourceXlibWindow fromXlib;
	fromXlib.chain.next = null;
	fromXlib.chain.sType = WGPU_STYPE_SURFACE_SOURCE_XLIB_WINDOW;
	fromXlib.display = _gfx_ctx.display;
	fromXlib.window = window.src.window;
	WgpuSurfaceDescriptor surfaceDesc;
	surfaceDesc.nextInChain = (WgpuChainedStruct*)&fromXlib;
	return wgpuInstanceCreateSurface(instance, &surfaceDesc);
}

// public
fn void rgfw_window_setAspectRatio_X11(GfxWindow* win, int w, int h)
{
	if (win == null) return;
	if (w == 0 && h == 0) return;
	XSizeHints hints;
	CLong flags;
	xGetWMNormalHints(_gfx_ctx.display, win.src.window, &hints, &flags);
	hints.flags |= (1 << 6); // PAspect
	hints.min_aspect.x = w; hints.max_aspect.x = w;
	hints.min_aspect.y = h; hints.max_aspect.y = h;
	xSetWMNormalHints(_gfx_ctx.display, win.src.window, &hints);
}

// public
fn void rgfw_window_blitSurface_X11(GfxWindow* win, GfxSurface* surface)
{
	if (surface == null) return;
	// surface.native.bitmap->data = surface.native.buffer would go here in real Xlib
	// (XImage.data is a char* at a fixed offset); left to the caller's XImage helper.
	IntVec2 min = math::min(win.size, surface.size);
	xPutImage(_gfx_ctx.display, win.src.window, win.src.gc, surface.native.bitmap, 0, 0, 0, 0, (uint)min.x, (uint)min.y);
}

// public
fn bool rgfw_getGlobalMouse_X11(int* fX, int* fY)
{
	int x, y;
	uint z;
	GfxXWindow window1, window2;
	xQueryPointer(_gfx_ctx.display, xDefaultRootWindow(_gfx_ctx.display), &window1, &window2, fX, fY, &x, &y, &z);
	return true;
}

// public
fn IntVec2 rgfw_window_fetchSize_X11(GfxWindow* win)
{
	XWindowAttributes attribs;
	xGetWindowAttributes(_gfx_ctx.display, win.src.window, &attribs);
	win.size = { attribs.width, attribs.height };
	return win.get_size();
}

// public
fn void rgfw_window_setMousePassthrough_X11(GfxWindow* win, bool passthrough)
{
	if (passthrough)
	{
		Region region = xCreateRegion();
		if (rgfw_xShapeCombineRegionSRC) rgfw_xShapeCombineRegionSRC(_gfx_ctx.display, win.src.window, 2, 0, 0, region, 0);
		xDestroyRegion(region);
		return;
	}
	if (rgfw_xShapeCombineMaskSRC) rgfw_xShapeCombineMaskSRC(_gfx_ctx.display, win.src.window, 2, 0, 0, 0, 0);
}

// public
fn sz rgfw_monitor_getGammaRampPtr_X11(GfxMonitor* monitor, RgfwGammaRamp* ramp)
{
	sz size = (sz)xRRGetCrtcGammaSize(_gfx_ctx.display, monitor.node.nativeId1);
	XRRCrtcGamma* gamma = xRRGetCrtcGamma(_gfx_ctx.display, monitor.node.nativeId1);
	if (ramp != null)
	{
		mem::copy(ramp.red, gamma.red, size * ushort::size);
		mem::copy(ramp.green, gamma.green, size * ushort::size);
		mem::copy(ramp.blue, gamma.blue, size * ushort::size);
	}
	xRRFreeGamma(gamma);
	return size;
}

// public
fn bool rgfw_monitor_setGammaRamp_X11(GfxMonitor* monitor, RgfwGammaRamp* ramp)
{
	sz size = (sz)xRRGetCrtcGammaSize(_gfx_ctx.display, monitor.node.nativeId1);
	if (size != ramp.count)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_X11, "X11: Gamma ramp size must match current ramp size");
		return false;
	}
	XRRCrtcGamma* gamma = xRRAllocGamma((int)ramp.count);
	mem::copy(gamma.red, ramp.red, ramp.count * ushort::size);
	mem::copy(gamma.green, ramp.green, ramp.count * ushort::size);
	mem::copy(gamma.blue, ramp.blue, ramp.count * ushort::size);
	xRRSetCrtcGamma(_gfx_ctx.display, monitor.node.nativeId1, gamma);
	xRRFreeGamma(gamma);
	return true;
}

// private
fn RgfwMouse* rgfw_createMouse_X11(char* data, int w, int h, GfxFormat format)
{
	if (rgfw_xcursorImageCreateSRC == null) return null;
	XcursorImage* native = rgfw_xcursorImageCreateSRC(w, h);
	native.xhot = 0; native.yhot = 0;
	mem::clear(native.pixels, (sz)(w * h * 4));
	mem::copy(native.pixels, data, (sz)(w * h * 4));
	XCursor cursor = rgfw_xcursorImageLoadCursorSRC(_gfx_ctx.display, native);
	rgfw_xcursorImageDestroySRC(native);
	return (RgfwMouse*)cursor;
}

// private
fn RgfwMouse* rgfw_createMouseStandard_X11(GfxMouseIcon mouse)
{
	uint mouseIcon = 0;
	switch (mouse)
	{
		case NORMAL: mouseIcon = 68; // XC_left_ptr
		case ARROW: mouseIcon = 68;
		case IBEAM: mouseIcon = 152; // XC_xterm
		case WAIT: mouseIcon = 150; // XC_watch
		case CROSSHAIR: mouseIcon = 130; // XC_tcross
		case PROGRESS: mouseIcon = 150;
		case RESIZE_NWSE: mouseIcon = 134;
		case RESIZE_NESW: mouseIcon = 136;
		case RESIZE_EW: mouseIcon = 108;
		case RESIZE_NS: mouseIcon = 116;
		case RESIZE_NW: mouseIcon = 134;
		case RESIZE_N: mouseIcon = 138;
		case RESIZE_NE: mouseIcon = 136;
		case RESIZE_E: mouseIcon = 96;
		case RESIZE_SE: mouseIcon = 14;
		case RESIZE_S: mouseIcon = 16;
		case RESIZE_SW: mouseIcon = 12;
		case RESIZE_W: mouseIcon = 70;
		case RESIZE_ALL: mouseIcon = 52; // XC_fleur
		case NOT_ALLOWED: mouseIcon = 88; // XC_pirate
		case POINTING_HAND: mouseIcon = 60; // XC_hand2
		default: return null;
	}
	XCursor cursor = xCreateFontCursor(_gfx_ctx.display, mouseIcon);
	return (RgfwMouse*)cursor;
}

// private
fn void? rgfw_createWindowPlatform_X11(String name, GfxWindowFlags flags, GfxWindow* win)
{
	if (flags.open_gl || flags.egl)
	{
		win.src.window = 0;
		return;
	}
	XVisualInfo visual;
	rgfw_window_getVisual(&visual, win.internal.flags.transparent);
	return rgfw_xCreateWindow(visual, name, flags, win);
}

// private
fn void rgfw_deinitPlatform_X11()
{
	if (_gfx_ctx.display != null)
	{
		if (xGetSelectionOwner(_gfx_ctx.display, _gfx_ctx.clipboardAtom) == _gfx_ctx.helperWindow)
		{
			xConvertSelection(_gfx_ctx.display, _gfx_ctx.clipboardManager, _gfx_ctx.saveTargets, 0, _gfx_ctx.helperWindow, 0);
			while (rgfw_xHandleClipboardSelectionHelper() != 0) {}
		}
		xUnregisterIMInstantiateCallback(_gfx_ctx.display, null, null, null, (void*)&rgfw_x11ImInitCallback, null);
	}
	if (_gfx_ctx.im != null) { xCloseIM(_gfx_ctx.im); _gfx_ctx.im = null; }
	if (_gfx_ctx.unixClipboard != null) { free(_gfx_ctx.unixClipboard); _gfx_ctx.unixClipboard = null; }
	if (_gfx_ctx.hiddenMouse != null) { rgfw_freeMouse(_gfx_ctx.hiddenMouse); _gfx_ctx.hiddenMouse = null; }
	if (_gfx_ctx.helperWindow != 0) xDestroyWindow(_gfx_ctx.display, _gfx_ctx.helperWindow);
	if (_gfx_ctx.display != null) xCloseDisplay(_gfx_ctx.display);
	_gfx_ctx.display = null;

	if (rgfw_x11CursorHandle != null) { libc::dlclose(rgfw_x11CursorHandle); rgfw_x11CursorHandle = null; }
	if (rgfw_x11XiHandle != null) { libc::dlclose(rgfw_x11XiHandle); rgfw_x11XiHandle = null; }
	if (rgfw_x11ExtHandle != null) { libc::dlclose(rgfw_x11ExtHandle); rgfw_x11ExtHandle = null; }
}

// private
fn bool rgfw_extensionSupportedPlatform_OpenGL_X11(ZString extension, sz len)
{
	ZString extensions = rgfw_glXQueryExtensionsString(_gfx_ctx.display, xDefaultScreen(_gfx_ctx.display));
	return extensions && rgfw_extensionSupportedStr(extensions, extension, len);
}

// private
fn void rgfw_freeMouse_X11(RgfwMouse* mouse) { xFreeCursor(_gfx_ctx.display, (XCursor)mouse); }

// private
fn void* rgfw_getCurrentContext_OpenGL_X11() => rgfw_glXGetCurrentContext();

// private
fn GfxFn rgfw_getProcAddress_OpenGL_X11(ZString procname)
{
	if (_gfx_ctx.glGetProcAddress == null) return null;
	return ((GlxGetProcAddressProc)_gfx_ctx.glGetProcAddress)(procname);
}

// private
fn void rgfw_initKeycodesPlatform_X11()
{
	// A full evdev-keycode -> RGFW_KEY table walk (xkbGetKeyboardByName) is left
	// out of this mechanical pass; keycodes[] stays zeroed (RGFW_KEY_NULL).
}

// private
fn int rgfw_initPlatform_X11(String class_name, GfxInitOption flags)
{
	_gfx_ctx.class_name = class_name;

	xInitThreads();
	_gfx_ctx.display = xOpenDisplay(null);
	if (_gfx_ctx.display == null) return -1;

	_gfx_ctx.netCurrentDesktop = xInternAtom(_gfx_ctx.display, "_NET_CURRENT_DESKTOP", 0);
	_gfx_ctx.netWmStateMaximizedVert = xInternAtom(_gfx_ctx.display, "_NET_WM_STATE_MAXIMIZED_VERT", 0);
	_gfx_ctx.netWmStateMaximizedHorz = xInternAtom(_gfx_ctx.display, "_NET_WM_STATE_MAXIMIZED_HORZ", 0);
	_gfx_ctx.netWmBypassCompositor = xInternAtom(_gfx_ctx.display, "_NET_WM_BYPASS_COMPOSITOR", 0);
	_gfx_ctx.netWmState = xInternAtom(_gfx_ctx.display, "_NET_WM_STATE", 0);
	_gfx_ctx.motifWmHints = xInternAtom(_gfx_ctx.display, "_MOTIF_WM_HINTS", 0);
	_gfx_ctx.wmDeleteWindow = xInternAtom(_gfx_ctx.display, "WM_DELETE_WINDOW", 0);
	_gfx_ctx.netWmSyncRequestCounter = xInternAtom(_gfx_ctx.display, "_NET_WM_SYNC_REQUEST_COUNTER", 0);
	_gfx_ctx.netWmSyncRequest = xInternAtom(_gfx_ctx.display, "_NET_WM_SYNC_REQUEST", 0);
	_gfx_ctx.atomPair = xInternAtom(_gfx_ctx.display, "ATOM_PAIR", 0);
	_gfx_ctx.multiple = xInternAtom(_gfx_ctx.display, "MULTIPLE", 0);
	_gfx_ctx.targets = xInternAtom(_gfx_ctx.display, "TARGETS", 0);
	_gfx_ctx.saveTargets = xInternAtom(_gfx_ctx.display, "SAVE_TARGETS", 0);
	_gfx_ctx.utf8String = xInternAtom(_gfx_ctx.display, "UTF8_STRING", 0);
	_gfx_ctx.xdndTypeList = xInternAtom(_gfx_ctx.display, "XdndTypeList", 0);
	_gfx_ctx.xdndSelection = xInternAtom(_gfx_ctx.display, "XdndSelection", 0);
	_gfx_ctx.xdndAware = xInternAtom(_gfx_ctx.display, "XdndAware", 0);
	_gfx_ctx.xdndEnter = xInternAtom(_gfx_ctx.display, "XdndEnter", 0);
	_gfx_ctx.xdndPosition = xInternAtom(_gfx_ctx.display, "XdndPosition", 0);
	_gfx_ctx.xdndStatus = xInternAtom(_gfx_ctx.display, "XdndStatus", 0);
	_gfx_ctx.xdndLeave = xInternAtom(_gfx_ctx.display, "XdndLeave", 0);
	_gfx_ctx.xdndDrop = xInternAtom(_gfx_ctx.display, "XdndDrop", 0);
	_gfx_ctx.xdndFinished = xInternAtom(_gfx_ctx.display, "XdndFinished", 0);
	_gfx_ctx.xdndActionCopy = xInternAtom(_gfx_ctx.display, "XdndActionCopy", 0);
	_gfx_ctx.wmProtocols = xInternAtom(_gfx_ctx.display, "WM_PROTOCOLS", 0);
	_gfx_ctx.wmState = xInternAtom(_gfx_ctx.display, "WM_STATE", 0);
	_gfx_ctx.netWmStateFullscreen = xInternAtom(_gfx_ctx.display, "_NET_WM_STATE_FULLSCREEN", 0);
	_gfx_ctx.netWmStateAbove = xInternAtom(_gfx_ctx.display, "_NET_WM_STATE_ABOVE", 0);
	_gfx_ctx.netWmWindowOpacity = xInternAtom(_gfx_ctx.display, "_NET_WM_WINDOW_OPACITY", 0);
	_gfx_ctx.netWmName = xInternAtom(_gfx_ctx.display, "_NET_WM_NAME", 0);
	_gfx_ctx.netWmIcon = xInternAtom(_gfx_ctx.display, "_NET_WM_ICON", 0);
	_gfx_ctx.xselData = xInternAtom(_gfx_ctx.display, "XSEL_DATA", 0);
	_gfx_ctx.clipboardAtom = xInternAtom(_gfx_ctx.display, "CLIPBOARD", 0);
	_gfx_ctx.clipboardManager = xInternAtom(_gfx_ctx.display, "CLIPBOARD_MANAGER", 0);
	_gfx_ctx.netWorkarea = xInternAtom(_gfx_ctx.display, "_NET_WORKAREA", 0);
	_gfx_ctx.xtextUriList = xInternAtom(_gfx_ctx.display, "text/uri-list", 0);
	_gfx_ctx.xtextPlain = xInternAtom(_gfx_ctx.display, "text/plain", 0);

	_gfx_ctx.context = xUniqueContext();
	if (_gfx_ctx.context == 0) return -1;

	XSetWindowAttributes wa;
	wa.event_mask = 1 << 22; // PropertyChangeMask
	_gfx_ctx.helperWindow = xCreateWindow(_gfx_ctx.display, xDefaultRootWindow(_gfx_ctx.display), 0, 0, 1, 1, 0, 0,
		2 /* InputOnly */, xDefaultVisual(_gfx_ctx.display, xDefaultScreen(_gfx_ctx.display)), 1 << 11, &wa);
	if (_gfx_ctx.helperWindow == 0) return -1;

	rgfw_x11CursorHandle = libc::dlopen("libXcursor.so.1", RTLD_LAZY | RTLD_LOCAL);
	if (rgfw_x11CursorHandle != null)
	{
		rgfw_xcursorImageCreateSRC = (PFN_XcursorImageCreate)libc::dlsym(rgfw_x11CursorHandle, "XcursorImageCreate");
		rgfw_xcursorImageDestroySRC = (PFN_XcursorImageDestroy)libc::dlsym(rgfw_x11CursorHandle, "XcursorImageDestroy");
		rgfw_xcursorImageLoadCursorSRC = (PFN_XcursorImageLoadCursor)libc::dlsym(rgfw_x11CursorHandle, "XcursorImageLoadCursor");
	}
	rgfw_x11XiHandle = libc::dlopen("libXi.so.6", RTLD_LAZY | RTLD_LOCAL);
	if (rgfw_x11XiHandle != null) rgfw_xiSelectEventsSRC = (PFN_XISelectEvents)libc::dlsym(rgfw_x11XiHandle, "XISelectEvents");
	rgfw_x11ExtHandle = libc::dlopen("libXext.so.6", RTLD_LAZY | RTLD_LOCAL);
	if (rgfw_x11ExtHandle != null)
	{
		rgfw_xSyncCreateCounterSRC = (PFN_XSyncCreateCounter)libc::dlsym(rgfw_x11ExtHandle, "XSyncCreateCounter");
		rgfw_xSyncIntToValueSRC = (PFN_XSyncIntToValue)libc::dlsym(rgfw_x11ExtHandle, "XSyncIntToValue");
		rgfw_xSyncSetCounterSRC = (PFN_XSyncSetCounter)libc::dlsym(rgfw_x11ExtHandle, "XSyncSetCounter");
		rgfw_xShapeCombineRegionSRC = (PFN_XShapeCombineRegion)libc::dlsym(rgfw_x11ExtHandle, "XShapeCombineRegion");
		rgfw_xShapeCombineMaskSRC = (PFN_XShapeCombineMask)libc::dlsym(rgfw_x11ExtHandle, "XShapeCombineMask");
	}

	char[4] blk = {0, 0, 0, 0};
	_gfx_ctx.hiddenMouse = rgfw_createMouse((char*)&blk, 1, 1, RGBA8);
	_gfx_ctx.unixClipboard = null;

	xSetErrorHandler((void*)&rgfw_xErrorHandler);
	xSync(_gfx_ctx.display, 0);

	xSetLocaleModifiers("");
	xRegisterIMInstantiateCallback(_gfx_ctx.display, null, null, null, (void*)&rgfw_x11ImInitCallback, null);

	int errorBase;
	if (xRRQueryExtension(_gfx_ctx.display, &_gfx_ctx.xrandrEventBase, &errorBase) != 0)
	{
		xRRSelectInput(_gfx_ctx.display, xRootWindow(_gfx_ctx.display, xDefaultScreen(_gfx_ctx.display)), 1 << 4); // RROutputChangeNotifyMask
	}
	return 0;
}

// private
fn bool rgfw_loadGL_X11()
{
	if (_gfx_ctx.nativeGL_handle != null && _gfx_ctx.glGetProcAddress != null) return true;
	ZString[3] glxSonames = {"libGLX.so.0", "libGL.so.1", "libGL.so"};
	for (sz i = 0; i < 3; i++)
	{
		_gfx_ctx.nativeGL_handle = libc::dlopen(glxSonames[i], RTLD_LAZY | RTLD_LOCAL);
		if (_gfx_ctx.nativeGL_handle != null) break;
	}
	if (_gfx_ctx.nativeGL_handle == null) return false;
	void* ptr = libc::dlsym(_gfx_ctx.nativeGL_handle, "glXGetProcAddress");
	_gfx_ctx.glGetProcAddress = ptr;

	rgfw_glXChooseVisual = (RGFW_glXChooseVisualProc)libc::dlsym(_gfx_ctx.nativeGL_handle, "glXChooseVisual");
	rgfw_glXCreateContext = (RGFW_glXCreateContextProc)libc::dlsym(_gfx_ctx.nativeGL_handle, "glXCreateContext");
	rgfw_glXMakeCurrent = (RGFW_glXMakeCurrentProc)libc::dlsym(_gfx_ctx.nativeGL_handle, "glXMakeCurrent");
	rgfw_glXGetCurrentContext = (RGFW_glXGetCurrentContextProc)libc::dlsym(_gfx_ctx.nativeGL_handle, "glXGetCurrentContext");
	rgfw_glXSwapBuffers = (RGFW_glXSwapBuffersProc)libc::dlsym(_gfx_ctx.nativeGL_handle, "glXSwapBuffers");
	rgfw_glXGetVisualFromFBConfig = (RGFW_glXGetVisualFromFBConfigProc)libc::dlsym(_gfx_ctx.nativeGL_handle, "glXGetVisualFromFBConfig");
	rgfw_glXGetFBConfigAttrib = (RGFW_glXGetFBConfigAttribProc)libc::dlsym(_gfx_ctx.nativeGL_handle, "glXGetFBConfigAttrib");
	rgfw_glXGetProcAddressARB = (RGFW_glXGetProcAddressARBProc)libc::dlsym(_gfx_ctx.nativeGL_handle, "glXGetProcAddressARB");
	rgfw_glXChooseFBConfig = (RGFW_glXChooseFBConfigProc)libc::dlsym(_gfx_ctx.nativeGL_handle, "glXChooseFBConfig");
	rgfw_glXDestroyContext = (RGFW_glXDestroyContextProc)libc::dlsym(_gfx_ctx.nativeGL_handle, "glXDestroyContext");
	rgfw_glXCreateWindow = (RGFW_glXCreateWindowProc)libc::dlsym(_gfx_ctx.nativeGL_handle, "glXCreateWindow");
	rgfw_glXDestroyWindow = (RGFW_glXDestroyWindowProc)libc::dlsym(_gfx_ctx.nativeGL_handle, "glXDestroyWindow");
	rgfw_glXQueryExtensionsString = (RGFW_glXQueryExtensionsStringProc)libc::dlsym(_gfx_ctx.nativeGL_handle, "glXQueryExtensionsString");
	rgfw_glXCreateContextAttribsARB = (RGFW_glXCreateContextAttribsARBProc)libc::dlsym(_gfx_ctx.nativeGL_handle, "glXCreateContextAttribsARB");
	rgfw_glXSwapIntervalEXT = (RGFW_glXSwapIntervalEXTProc)libc::dlsym(_gfx_ctx.nativeGL_handle, "glXSwapIntervalEXT");
	if (rgfw_glXSwapIntervalEXT == null)
	{
		ZString[2] array = {"GLX_MESA_swap_control", "GLX_SGI_swap_control"};
		for (sz i = 0; i < 2 && rgfw_glXSwapIntervalNative == null; i++)
		{
			rgfw_glXSwapIntervalNative = (RGFW_glXSwapIntervalNativeProc)libc::dlsym(_gfx_ctx.nativeGL_handle, array[i]);
		}
	}
	return true;
}

// private
fn void rgfw_monitorNode_free_X11(GfxMonitorNode* node) {}

// private
fn GfxMonitorMode[] rgfw_monitor_getModesPtr_X11(GfxMonitor* monitor)
{
	XRRScreenResources* res = xRRGetScreenResourcesCurrent(_gfx_ctx.display, xDefaultRootWindow(_gfx_ctx.display));
	if (!res) return {};
	XRRCrtcInfo* ci = xRRGetCrtcInfo(_gfx_ctx.display, res, monitor.node.nativeId1);
	GfxXRROutputInfo* oi = xRRGetOutputInfo(_gfx_ctx.display, res, monitor.node.nativeId0);
	sz count = (sz)oi.nmode;
	List{GfxMonitorMode} modes;
	for (int i = 0; i < oi.nmode; i++)
	{
		GfxMonitorMode mode;
		rgfw_xGetMode(ci, res, oi.modes[i], &mode);
		modes.push(mode);
	}
	xRRFreeOutputInfo(oi);
	xRRFreeCrtcInfo(ci);
	xRRFreeScreenResources(res);
	return modes.array_view();
}

// private
fn bool rgfw_monitor_getWorkarea_X11(GfxMonitor* monitor, int* x, int* y, int* width, int* height)
{
	GfxXWindow root = xDefaultRootWindow(_gfx_ctx.display);
	int areaX = monitor.origin.x;
	int areaY = monitor.origin.y;
	int areaW = monitor.mode.w;
	int areaH = monitor.mode.h;

	if (_gfx_ctx.netWorkarea != 0 && _gfx_ctx.netCurrentDesktop != 0)
	{
		Atom* extents = null;
		Atom* desktop = null;
		Atom actualType; int actualFormat; CULong extentCount, bytesAfter, count;
		xGetWindowProperty(_gfx_ctx.display, root, _gfx_ctx.netWorkarea, 0, ~0, 0, 6, &actualType, &actualFormat, &extentCount, &bytesAfter, (char**)&extents);
		xGetWindowProperty(_gfx_ctx.display, root, _gfx_ctx.netCurrentDesktop, 0, ~0, 0, 6, &actualType, &actualFormat, &count, &bytesAfter, (char**)&desktop);

		if (count != 0 && extentCount >= 4 && *desktop < extentCount / 4)
		{
			int globalX = (int)extents[*desktop * 4 + 0];
			int globalY = (int)extents[*desktop * 4 + 1];
			int globalW = (int)extents[*desktop * 4 + 2];
			int globalH = (int)extents[*desktop * 4 + 3];
			if (areaX < globalX) { areaW -= globalX - areaX; areaX = globalX; }
			if (areaY < globalY) { areaH -= globalY - areaY; areaY = globalY; }
			if (areaX + areaW > globalX + globalW) areaW = globalX - areaX + globalW;
			if (areaY + areaH > globalY + globalH) areaH = globalY - areaY + globalH;
		}
		if (extents != null) xFree(extents);
		if (desktop != null) xFree(desktop);
	}

	if (x != null) *x = areaX;
	if (y != null) *y = areaY;
	if (width != null) *width = areaW;
	if (height != null) *height = areaH;
	return true;
}

// private
fn bool rgfw_monitor_requestMode_X11(GfxMonitor* mon, GfxMonitorMode* mode, GfxMonitorModeRequest request)
{
	bool output = false;
	XRRScreenResources* res = xRRGetScreenResourcesCurrent(_gfx_ctx.display, xDefaultRootWindow(_gfx_ctx.display));
	if (res == null) return false;
	XRRCrtcInfo* ci = xRRGetCrtcInfo(_gfx_ctx.display, res, mon.node.nativeId1);
	GfxXRROutputInfo* oi = xRRGetOutputInfo(_gfx_ctx.display, res, mon.node.nativeId0);
	RRMode native = 0;

	for (int i = 0; i < oi.nmode; i++)
	{
		GfxMonitorMode foundMode;
		void* mi = rgfw_xGetMode(ci, res, oi.modes[i], &foundMode);
		if (mi == null) continue;
		if (rgfw_monitorModeCompare(mode, &foundMode, request))
		{
			native = ((XRRModeInfoNative*)mi).id;
			output = true;
			mon.mode = foundMode;
			break;
		}
	}
	if (native != 0) xRRSetCrtcConfig(_gfx_ctx.display, res, mon.node.nativeId1, 0, ci.x, ci.y, native, ci.rotation, ci.outputs, ci.noutput);
	xRRFreeOutputInfo(oi);
	xRRFreeCrtcInfo(ci);
	xRRFreeScreenResources(res);
	return output;
}

// private
fn bool rgfw_monitor_setMode_X11(GfxMonitor* mon, GfxMonitorMode* mode)
{
	bool out = false;
	XRRScreenResources* res = xRRGetScreenResourcesCurrent(_gfx_ctx.display, xDefaultRootWindow(_gfx_ctx.display));
	XRRCrtcInfo* ci = xRRGetCrtcInfo(_gfx_ctx.display, res, mon.node.nativeId1);
	if (xRRSetCrtcConfig(_gfx_ctx.display, res, mon.node.nativeId1, 0, ci.x, ci.y, (RRMode)(uptr)mode.src, ci.rotation, ci.outputs, ci.noutput) == 0) out = true;
	xRRFreeCrtcInfo(ci);
	xRRFreeScreenResources(res);
	return out;
}

// private
fn RgfwKey rgfw_physicalToMappedKey_X11(RgfwKey key)
{
	KeyCode keycode = (KeyCode)rgfw_rgfwToApiKey(key);
	KeySym sym = xkbKeycodeToKeysym(_gfx_ctx.display, keycode, 0, 0);
	if (sym < 256) return (RgfwKey)sym;

	switch (sym)
	{
		case XK_F1: return RGFW_KEY_F1; case XK_F2: return RGFW_KEY_F2; case XK_F3: return RGFW_KEY_F3;
		case XK_F4: return RGFW_KEY_F4; case XK_F5: return RGFW_KEY_F5; case XK_F6: return RGFW_KEY_F6;
		case XK_F7: return RGFW_KEY_F7; case XK_F8: return RGFW_KEY_F8; case XK_F9: return RGFW_KEY_F9;
		case XK_F10: return RGFW_KEY_F10; case XK_F11: return RGFW_KEY_F11; case XK_F12: return RGFW_KEY_F12;
		case XK_F13: return RGFW_KEY_F13; case XK_F14: return RGFW_KEY_F14; case XK_F15: return RGFW_KEY_F15;
		case XK_F16: return RGFW_KEY_F16; case XK_F17: return RGFW_KEY_F17; case XK_F18: return RGFW_KEY_F18;
		case XK_F19: return RGFW_KEY_F19; case XK_F20: return RGFW_KEY_F20; case XK_F21: return RGFW_KEY_F21;
		case XK_F22: return RGFW_KEY_F22; case XK_F23: return RGFW_KEY_F23; case XK_F24: return RGFW_KEY_F24;
		case XK_F25: return RGFW_KEY_F25;
		case XK_SHIFT_L: return RGFW_KEY_SHIFT_L; case XK_SHIFT_R: return RGFW_KEY_SHIFT_R;
		case XK_CONTROL_L: return RGFW_KEY_CONTROL_L; case XK_CONTROL_R: return RGFW_KEY_CONTROL_R;
		case XK_ALT_L: return RGFW_KEY_ALT_L; case XK_ALT_R: return RGFW_KEY_ALT_R;
		case XK_SUPER_L: return RGFW_KEY_SUPER_L; case XK_SUPER_R: return RGFW_KEY_SUPER_R;
		case XK_CAPS_LOCK: return RGFW_KEY_CAPS_LOCK;
		case XK_NUM_LOCK: return RGFW_KEY_NUM_LOCK;
		case XK_SCROLL_LOCK: return RGFW_KEY_SCROLL_LOCK;
		case XK_UP: return RGFW_KEY_UP; case XK_DOWN: return RGFW_KEY_DOWN;
		case XK_LEFT: return RGFW_KEY_LEFT; case XK_RIGHT: return RGFW_KEY_RIGHT;
		case XK_HOME: return RGFW_KEY_HOME; case XK_END: return RGFW_KEY_END;
		case XK_PAGE_UP: return RGFW_KEY_PAGE_UP; case XK_PAGE_DOWN: return RGFW_KEY_PAGE_DOWN;
		case XK_INSERT: return RGFW_KEY_INSERT;
		case XK_MENU: return RGFW_KEY_MENU;
		case XK_KP_ADD: return RGFW_KEY_PAD_PLUS;
		case XK_KP_SUBTRACT: return RGFW_KEY_PAD_MINUS;
		case XK_KP_MULTIPLY: return RGFW_KEY_PAD_MULTIPLY;
		case XK_KP_DIVIDE: return RGFW_KEY_PAD_SLASH;
		case XK_KP_EQUAL: return RGFW_KEY_PAD_EQUAL;
		case XK_KP_ENTER: return RGFW_KEY_PAD_RETURN;
		case XK_KP_DECIMAL: return RGFW_KEY_PAD_PERIOD;
		case XK_KP_0: return RGFW_KEY_PAD0; case XK_KP_1: return RGFW_KEY_PAD1;
		case XK_KP_2: return RGFW_KEY_PAD2; case XK_KP_3: return RGFW_KEY_PAD3;
		case XK_KP_4: return RGFW_KEY_PAD4; case XK_KP_5: return RGFW_KEY_PAD5;
		case XK_KP_6: return RGFW_KEY_PAD6; case XK_KP_7: return RGFW_KEY_PAD7;
		case XK_KP_8: return RGFW_KEY_PAD8; case XK_KP_9: return RGFW_KEY_PAD9;
		case XK_PRINT: return RGFW_KEY_PRINT_SCREEN;
		case XK_PAUSE: return RGFW_KEY_PAUSE;
		default: break;
	}

	return RGFW_KEY_NULL;
}

// private
fn void rgfw_pollEvents_X11()
{
	rgfw_resetPrevState();
	xPending(_gfx_ctx.display);
	while (qLength(_gfx_ctx.display) != 0) rgfw_xHandleEvent();
}

// private
fn void rgfw_pollMonitors_X11()
{
	GfxXWindow root = xDefaultRootWindow(_gfx_ctx.display);
	XRRScreenResources* res = xRRGetScreenResourcesCurrent(_gfx_ctx.display, root);
	if (res == null) return;
	GfxRROutput primary = xRRGetOutputPrimary(_gfx_ctx.display, root);

	for (GfxMonitorNode* node = _gfx_ctx.monitors.list.head; node != null; node = node.next) node.disconnected = true;

	for (int i = 0; i < res.noutput; i++)
	{
		GfxMonitorNode* node = null;
		for (node = _gfx_ctx.monitors.list.head; node != null; node = node.next)
		{
			if (node.nativeId0 == res.outputs[i]) break;
		}
		if (node != null)
		{
			node.disconnected = false;
			if (node.nativeId0 == primary) _gfx_ctx.monitors.primary = node;
			continue;
		}

		GfxMonitor monitor;
		GfxXRROutputInfo* info = xRRGetOutputInfo(_gfx_ctx.display, res, res.outputs[i]);
		if (info == null) continue;
		if (info.connection != 0 || info.crtc == 0) { xRRFreeOutputInfo(info); continue; }

		XRRCrtcInfo* ci = xRRGetCrtcInfo(_gfx_ctx.display, res, info.crtc);
		if (ci == null) continue;

		float physW = (float)info.mm_width / 25.4f;
		float physH = (float)info.mm_height / 25.4f;

		sz n = info.name.len();
		if (n > 127) n = 127;
		mem::copy(&monitor.name, info.name, n);
		monitor.name[n] = 0;

		if (physW > 0.0f && physH > 0.0f)
		{
			monitor.phys_size = { physW, physH };
		}
		else
		{
			monitor.phys_size = { ci.width / 96.0f, ci.height / 96.0f };
		}

		monitor.origin = { ci.x, ci.y };

		float dpi = 96.0f;
		rgfw_xGetSystemContentDPI(&dpi);
		monitor.scale = (Vec2)(dpi / 96.0f);
		monitor.pixelRatio = dpi >= 192.0f ? 2.0f : 1.0f;

		void* mi = rgfw_xGetMode(ci, res, ci.mode, &monitor.mode);
		if (mi == null) break;

		xRRFreeCrtcInfo(ci);
		node = rgfw_monitors_add(&monitor);
		if (node == null) break;
		node.nativeId0 = res.outputs[i];
		node.nativeId1 = info.crtc;
		if (node.nativeId0 == primary) _gfx_ctx.monitors.primary = node;
		xRRFreeOutputInfo(info);
		rgfw_monitorCallback(_gfx_ctx.root, &node.mon, true);
	}

	xRRFreeScreenResources(res);
	rgfw_monitors_refresh();
}

// private
fn void rgfw_window_closePlatform_X11(GfxWindow* win)
{
	if (win.src.ic != null) { xDestroyIC(win.src.ic); win.src.ic = null; }
	xFreeGC(_gfx_ctx.display, win.src.gc);
	xDeleteContext(_gfx_ctx.display, win.src.window, _gfx_ctx.context);
	xDestroyWindow(_gfx_ctx.display, win.src.window);
}

// private
fn void rgfw_window_deleteContextPtr_OpenGL_X11(GfxWindow* win, RgfwGlContext* ctx)
{
	if (win.src.ctx.native.window != win.src.window) rgfw_glXDestroyWindow(_gfx_ctx.display, win.src.ctx.native.window);
	rgfw_glXDestroyContext(_gfx_ctx.display, ctx.ctx);
	win.src.ctx.native = null;
	rgfw_debugCallback(RGFW_TYPE_INFO, RGFW_INFO_OPEN_GL, "OpenGL context freed.");
}

// private
fn void rgfw_window_focus_X11(GfxWindow* win)
{
	XWindowAttributes attr;
	xGetWindowAttributes(_gfx_ctx.display, win.src.window, &attr);
	if (attr.map_state != 2 /* IsViewable */) return;
	xSetInputFocus(_gfx_ctx.display, win.src.window, 1 /* RevertToPointerRoot */, 0);
	xFlush(_gfx_ctx.display);
}

// private
fn void rgfw_window_hide_X11(GfxWindow* win)
{
	win.internal.flags.hide = true;
	xUnmapWindow(_gfx_ctx.display, win.src.window);
	xFlush(_gfx_ctx.display);
}

// private
fn bool rgfw_window_isHidden_X11(GfxWindow* win)
{
	XWindowAttributes windowAttributes;
	xGetWindowAttributes(_gfx_ctx.display, win.src.window, &windowAttributes);
	return windowAttributes.map_state != 2;
}

// private
fn bool rgfw_window_isMaximized_X11(GfxWindow* win)
{
	Atom actualType;
	int actualFormat;
	CULong nitems, bytesAfter;
	char* propData;
	int status = xGetWindowProperty(_gfx_ctx.display, win.src.window, _gfx_ctx.netWmState, 0, 1024, 0, 4, &actualType, &actualFormat, &nitems, &bytesAfter, &propData);
	if (status != 0) { if (propData != null) xFree(propData); return false; }
	for (CULong i = 0; i < nitems; i++)
	{
		if ((Atom)propData[i] == _gfx_ctx.netWmStateMaximizedVert || (Atom)propData[i] == _gfx_ctx.netWmStateMaximizedHorz)
		{
			xFree(propData);
			return true;
		}
	}
	if (propData != null) xFree(propData);
	return false;
}

// private
fn bool rgfw_window_isMinimized_X11(GfxWindow* win)
{
	Atom actualType;
	int actualFormat;
	CULong nitems, bytesAfter;
	char* propData;
	int status = xGetWindowProperty(_gfx_ctx.display, win.src.window, _gfx_ctx.wmState, 0, 2, 0, 0, &actualType, &actualFormat, &nitems, &bytesAfter, &propData);
	if (status == 0 && nitems >= 1 && propData == (char*)(uptr)3 /* IconicState */)
	{
		xFree(propData);
		return true;
	}
	if (propData != null) xFree(propData);
	XWindowAttributes windowAttributes;
	xGetWindowAttributes(_gfx_ctx.display, win.src.window, &windowAttributes);
	return windowAttributes.map_state != 2;
}

// private
fn void rgfw_window_makeCurrentContext_OpenGL_X11(GfxWindow* win)
{
	if (win == null) { rgfw_glXMakeCurrent(null, 0, null); }
	else { rgfw_glXMakeCurrent(_gfx_ctx.display, win.src.ctx.native.window, win.src.ctx.native.ctx); }
}

// private
fn void rgfw_window_maximizePlatform_X11(GfxWindow* win) => rgfw_toggleXMaximized(win, true);

// private
fn void rgfw_window_minimizePlatform_X11(GfxWindow* win)
{
	if (rgfw_window_isMaximized(win)) return;
	win.internal.old_bounds = win.bounds;
	xIconifyWindow(_gfx_ctx.display, win.src.window, xDefaultScreen(_gfx_ctx.display));
	xFlush(_gfx_ctx.display);
}

// private
fn void rgfw_window_moveMouse_X11(GfxWindow* win, int x, int y)
{
	int rootX, rootY, winX, winY;
	uint mask;
	GfxXWindow rootRet, childRet;
	xQueryPointer(_gfx_ctx.display, xDefaultRootWindow(_gfx_ctx.display), &rootRet, &childRet, &rootX, &rootY, &winX, &winY, &mask);
	win.internal.last_mouse = { x - win.x, y - win.y };
	if (winX == x && winY == y) return;
	xWarpPointer(_gfx_ctx.display, 0, win.src.window, 0, 0, 0, 0, x - win.x, y - win.y);
}

// private
fn void rgfw_window_movePlatform_X11(GfxWindow* win, int x, int y)
{
	win.x = x; win.y = y;
	xMoveWindow(_gfx_ctx.display, win.src.window, x, y);
}

// private
fn void rgfw_window_raise_X11(GfxWindow* win)
{
	xMapRaised(_gfx_ctx.display, win.src.window);
	rgfw_window_setFullscreen(win, win.is_fullscreen());
}

// private
fn void rgfw_window_resizePlatform_X11(GfxWindow* win, int w, int h)
{
	win.width = w; win.height = h;
	xResizeWindow(_gfx_ctx.display, win.src.window, (uint)w, (uint)h);
	if (win.internal.flags.no_resize)
	{
		XSizeHints sh;
		sh.flags = (1 << 4) | (1 << 5);
		sh.min_width = w; sh.max_width = w;
		sh.min_height = h; sh.max_height = h;
		xSetWMSizeHints(_gfx_ctx.display, win.src.window, &sh, 0);
	}
}

// private
fn void rgfw_window_restorePlatform_X11(GfxWindow* win)
{
	rgfw_toggleXMaximized(win, false);
	xFlush(_gfx_ctx.display);
}

// private
fn void rgfw_window_setFloating_X11(GfxWindow* win, bool floating) => rgfw_window_setXAtom(win, _gfx_ctx.netWmStateAbove, floating);

// private
fn void rgfw_window_setFullscreen_X11(GfxWindow* win, bool fullscreen)
{
	if (fullscreen)
	{
		win.internal.flags.fullscreen = true;
		win.internal.old_bounds = win.bounds;
	}
	else
	{
		win.internal.flags.fullscreen = false;
	}

	xRaiseWindow(_gfx_ctx.display, win.src.window);
	rgfw_window_setXAtom(win, _gfx_ctx.netWmStateFullscreen, fullscreen);

	if (!(win.internal.flags.transparent))
	{
		char value = (char)fullscreen;
		xChangeProperty(_gfx_ctx.display, win.src.window, _gfx_ctx.netWmBypassCompositor, 6 /* XA_CARDINAL */, 32, 0, &value, 1);
	}
}

// private
fn void rgfw_window_setMaxSize_X11(GfxWindow* win, int w, int h)
{
	CLong flags;
	XSizeHints hints;
	xGetWMNormalHints(_gfx_ctx.display, win.src.window, &hints, &flags);
	hints.flags |= 1 << 5; // PMaxSize
	hints.max_width = w; hints.max_height = h;
	xSetWMNormalHints(_gfx_ctx.display, win.src.window, &hints);
}

// private
fn void rgfw_window_setMinSize_X11(GfxWindow* win, int w, int h)
{
	CLong flags;
	XSizeHints hints;
	xGetWMNormalHints(_gfx_ctx.display, win.src.window, &hints, &flags);
	hints.flags |= 1 << 4; // PMinSize
	hints.min_width = w; hints.min_height = h;
	xSetWMNormalHints(_gfx_ctx.display, win.src.window, &hints);
}

// private
fn bool rgfw_window_setMousePlatform_X11(GfxWindow* win, RgfwMouse* mouse)
{
	xDefineCursor(_gfx_ctx.display, win.src.window, (XCursor)mouse);
	return true;
}

// private
fn void rgfw_window_setOpacity_X11(GfxWindow* win, char opacity)
{
	uint value = (uint)(4294967295.0 * (double)opacity);
	xChangeProperty(_gfx_ctx.display, win.src.window, _gfx_ctx.netWmWindowOpacity, 6, 32, 0, (char*)&value, 1);
}

// private
fn void rgfw_window_swapBuffers_OpenGL_X11(GfxWindow* win) { rgfw_glXSwapBuffers(_gfx_ctx.display, win.src.ctx.native.window); }

// private
fn bool rgfw_writeClipboard_X11(GfxDataTransfer* data)
{
	xSetSelectionOwner(_gfx_ctx.display, _gfx_ctx.clipboardAtom, _gfx_ctx.helperWindow, 0);
	if (xGetSelectionOwner(_gfx_ctx.display, _gfx_ctx.clipboardAtom) != _gfx_ctx.helperWindow)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_CLIPBOARD, "X11 failed to become owner of clipboard selection");
		return false;
	}
	if (_gfx_ctx.unixClipboard)
	{
		free(_gfx_ctx.unixClipboard);
		_gfx_ctx.unixClipboard = null;
	}

	sz length = data.length;
	if (data.data[data.length - 1] != 0) length += 1;

	_gfx_ctx.unixClipboard = mem::new_with_padding(GfxDataTransfer, data.length);
	char* dataPtr = (char*)_gfx_ctx.unixClipboard + GfxDataTransfer::size - 1;
	mem::copy(dataPtr, data.data, data.length);
	dataPtr[length - 1] = 0;
	_gfx_ctx.unixClipboard.data = dataPtr;
	_gfx_ctx.unixClipboard.type = RGFW_DATA_TEXT;
	_gfx_ctx.unixClipboard.length = length;
	return true;
}

// private
fn bool rgfw_readClipboardPtr_X11(RgfwDataTransferType requestedType, char* buffer, sz capacity, GfxDataTransfer* dataTransfer)
{
	if (requestedType != RGFW_DATA_TEXT) return false;
	dataTransfer.data = buffer;

	if (xGetSelectionOwner(_gfx_ctx.display, _gfx_ctx.clipboardAtom) == _gfx_ctx.helperWindow)
	{
		dataTransfer.length = _gfx_ctx.unixClipboard.length;
		if (buffer != null && _gfx_ctx.unixClipboard.data != null)
		{
			if (_gfx_ctx.unixClipboard.length > capacity) return false;
			mem::copy(buffer, (void*)_gfx_ctx.unixClipboard.data, _gfx_ctx.unixClipboard.length);
		}
		dataTransfer.type = RGFW_DATA_TEXT;
		return true;
	}

	xConvertSelection(_gfx_ctx.display, _gfx_ctx.clipboardAtom, _gfx_ctx.utf8String, _gfx_ctx.xselData, _gfx_ctx.helperWindow, 0);
	xSync(_gfx_ctx.display, 0);
	XEvent event;
	while (true)
	{
		xNextEvent(_gfx_ctx.display, &event);
		if (event.type != 31 /* SelectionNotify */) continue;
		if (event.xselection.selection != _gfx_ctx.clipboardAtom || event.xselection.property == 0) return false;
		break;
	}

	Atom target;
	int format;
	CULong n, size;
	char* data;
	xGetWindowProperty(_gfx_ctx.display, event.xselection.requestor, event.xselection.property, 0, ~0, 0, 0, &target, &format, &size, &n, (char**)&data);

	bool ret = true;
	sz length = (sz)size;
	if (data[size - 1] != 0) length += 1;

	if (size > (CULong)capacity && buffer != null) { ret = false; }
	else if ((target == _gfx_ctx.utf8String || target == 31) && buffer != null)
	{
		mem::copy(buffer, data, (sz)size);
		buffer[length - 1] = 0;
		xFree(data);
	}
	else if (buffer != null) { ret = false; }

	xDeleteProperty(_gfx_ctx.display, event.xselection.requestor, event.xselection.property);
	dataTransfer.length = length;
	dataTransfer.type = RGFW_DATA_TEXT;
	return ret;
}

// private
fn bool rgfw_window_createContextPtr_OpenGL_X11(GfxWindow* win, RgfwGlContext* context, RgfwGlHints* hints)
{
	win.src.ctx.native = context;
	win.src.gfxType = RGFW_GFX_NATIVE_OPEN_GL;

	bool showWindow = false;
	if (win.src.window != 0)
	{
		showWindow = !rgfw_window_isMinimized(win);
		rgfw_window_closePlatform(win);
	}

	bool transparent = win.internal.flags.transparent;

	int[40] visualAttribs;
	RgfwAttribStack stack;
	rgfw_attribStack_init(&stack, (int*)&visualAttribs, 40);
	rgfw_attribStack_pushAttribs(&stack, 0x22, 0x8002); // GLX_X_VISUAL_TYPE, GLX_TRUE_COLOR
	rgfw_attribStack_pushAttribs(&stack, 0x8012, 1); // GLX_X_RENDERABLE
	rgfw_attribStack_pushAttribs(&stack, 0x8011, 0x1); // GLX_RENDER_TYPE, GLX_RGBA_BIT
	rgfw_attribStack_pushAttribs(&stack, 0x8010, 0x1); // GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT
	rgfw_attribStack_pushAttribs(&stack, 5, 1); // GLX_DOUBLEBUFFER
	rgfw_attribStack_pushAttribs(&stack, 11, hints.alpha); // GLX_ALPHA_SIZE
	rgfw_attribStack_pushAttribs(&stack, 12, hints.depth); // GLX_DEPTH_SIZE
	rgfw_attribStack_pushAttribs(&stack, 13, hints.stencil); // GLX_STENCIL_SIZE
	rgfw_attribStack_pushAttribs(&stack, 6, hints.stereo); // GLX_STEREO
	rgfw_attribStack_pushAttribs(&stack, 7, hints.auxBuffers); // GLX_AUX_BUFFERS
	rgfw_attribStack_pushAttribs(&stack, 8, hints.red); // GLX_RED_SIZE
	rgfw_attribStack_pushAttribs(&stack, 9, hints.green); // GLX_GREEN_SIZE
	rgfw_attribStack_pushAttribs(&stack, 10, hints.blue); // GLX_BLUE_SIZE
	rgfw_attribStack_pushAttribs(&stack, 14, hints.accumRed); // GLX_ACCUM_RED_SIZE
	rgfw_attribStack_pushAttribs(&stack, 15, hints.accumGreen);
	rgfw_attribStack_pushAttribs(&stack, 16, hints.accumBlue);
	rgfw_attribStack_pushAttribs(&stack, 17, hints.accumAlpha);

	if (hints.sRGB)
	{
		ZString srgbArbStr = "GLX_ARB_framebuffer_sRGB";
		ZString srgbExtStr = "GLX_EXT_framebuffer_sRGB";
		if (rgfw_extensionSupportedPlatform_OpenGL_X11(srgbArbStr, srgbArbStr.len() + 1)) rgfw_attribStack_pushAttribs(&stack, 0x20B2, (int)hints.sRGB); // GLX_FRAMEBUFFER_SRGB_CAPABLE_ARB
		if (rgfw_extensionSupportedPlatform_OpenGL_X11(srgbExtStr, srgbExtStr.len() + 1)) rgfw_attribStack_pushAttribs(&stack, 0x20B2, (int)hints.sRGB); // GLX_FRAMEBUFFER_SRGB_CAPABLE_EXT
	}

	rgfw_attribStack_pushAttribs(&stack, 0, 0);

	int fbcount;
	GLXFBConfig* fbc = rgfw_glXChooseFBConfig(_gfx_ctx.display, xDefaultScreen(_gfx_ctx.display), (int*)&visualAttribs, &fbcount);
	if (fbc == null || fbcount == 0)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_OPEN_GL_CONTEXT, "Failed to find any valid GLX visual configs.");
		return false;
	}

	int bestFbcIdx = -1;
	int bestDepth = 0;
	int bestSamples = 0;
	for (int i = 0; i < fbcount; i++)
	{
		XVisualInfo* vi = rgfw_glXGetVisualFromFBConfig(_gfx_ctx.display, fbc[i]);
		if (vi == null) continue;
		int sampBuf, samples;
		rgfw_glXGetFBConfigAttrib(_gfx_ctx.display, fbc[i], 100000, &sampBuf); // GLX_SAMPLE_BUFFERS
		rgfw_glXGetFBConfigAttrib(_gfx_ctx.display, fbc[i], 100001, &samples); // GLX_SAMPLES
		if (bestFbcIdx == -1) bestFbcIdx = i;
		if ((!transparent || vi.depth == 32) && bestDepth == 0) { bestFbcIdx = i; bestDepth = vi.depth; }
		if ((!transparent || vi.depth == 32) && samples <= hints.samples && samples > bestSamples) { bestFbcIdx = i; bestDepth = vi.depth; bestSamples = samples; }
		xFree(vi);
	}
	if (bestFbcIdx == -1)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_OPEN_GL_CONTEXT, "Failed to get a valid GLX visual.");
		return false;
	}

	GLXFBConfig bestFbc = fbc[bestFbcIdx];
	XVisualInfo* vi = rgfw_glXGetVisualFromFBConfig(_gfx_ctx.display, bestFbc);
	xFree(fbc);
	XVisualInfo visual = *vi;
	xFree(vi);

	if (catch rgfw_xCreateWindow(visual, "", win.internal.flags, win)) return false;
	if (showWindow) show(win);

	int[40] contextAttribs;
	rgfw_attribStack_init(&stack, (int*)&contextAttribs, 40);
	int mask = 0;
	switch (hints.profile)
	{
		case RGFW_GL_ES: mask |= 0x4;
		case RGFW_GL_FORWARD_COMPATIBILITY: mask |= 0x2;
		case RGFW_GL_COMPATIBILITY: mask |= 0x2;
		default: mask |= 0x1; // GLX_CONTEXT_CORE_PROFILE_BIT_ARB
	}
	rgfw_attribStack_pushAttribs(&stack, 0x9126, mask); // GLX_CONTEXT_PROFILE_MASK_ARB
	if (hints.minor != 0 || hints.major != 0)
	{
		rgfw_attribStack_pushAttribs(&stack, 0x2091, hints.major);
		rgfw_attribStack_pushAttribs(&stack, 0x2092, hints.minor);
	}

	ZString flushStr = "GLX_ARB_context_flush_control";
	if (rgfw_extensionSupportedPlatform_OpenGL_X11(flushStr, flushStr.len() + 1))
	{
		if (hints.releaseBehavior == RGFW_GL_RELEASE_FLUSH) { rgfw_attribStack_pushAttribs(&stack, 0x2097, 0x2098); } // GLX_CONTEXT_RELEASE_BEHAVIOR_ARB, GLX_CONTEXT_RELEASE_BEHAVIOR_FLUSH_ARB
		else if (hints.releaseBehavior == RGFW_GL_RELEASE_NONE) { rgfw_attribStack_pushAttribs(&stack, 0x2097, 0); } // GLX_CONTEXT_RELEASE_BEHAVIOR_ARB, GLX_CONTEXT_RELEASE_BEHAVIOR_NONE_ARB
	}

	ZString robustStr = "GLX_ARB_create_context_robustness";
	int glFlags = 0;
	if (hints.debug) glFlags |= 0x00000001; // GLX_CONTEXT_DEBUG_BIT_ARB
	if (hints.robustness && rgfw_extensionSupportedPlatform_OpenGL_X11(robustStr, robustStr.len() + 1)) glFlags |= 0x00000004; // GLX_CONTEXT_ROBUST_ACCESS_BIT_ARB
	if (glFlags != 0) rgfw_attribStack_pushAttribs(&stack, 0x2094, glFlags); // GLX_CONTEXT_FLAGS_ARB

	ZString noErrorStr = "GLX_ARB_create_context_no_error";
	if (rgfw_extensionSupportedPlatform_OpenGL_X11(noErrorStr, noErrorStr.len() + 1)) rgfw_attribStack_pushAttribs(&stack, 0x31B3, (int)hints.noError); // GLX_CONTEXT_OPENGL_NO_ERROR_ARB

	rgfw_attribStack_pushAttribs(&stack, 0, 0);

	GLXContext ctx = null;
	if (hints.share != null) ctx = hints.share.ctx;

	if (rgfw_glXCreateContextAttribsARB == null)
	{
		rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_OPEN_GL_CONTEXT, "Failed to load proc address 'glXCreateContextAttribsARB', loading a generic OpenGL context.");
		win.src.ctx.native.ctx = rgfw_glXCreateContext(_gfx_ctx.display, &visual, ctx, X_TRUE);
	}
	else
	{
		_gfx_ctx.x11Error = null;
		win.src.ctx.native.ctx = rgfw_glXCreateContextAttribsARB(_gfx_ctx.display, bestFbc, ctx, X_TRUE, (int*)&contextAttribs);
		if (_gfx_ctx.x11Error != null || win.src.ctx.native.ctx == null)
		{
			rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_OPEN_GL_CONTEXT, "Failed to create an OpenGL context with AttribsARB, loading a generic OpenGL context.");
			win.src.ctx.native.ctx = rgfw_glXCreateContext(_gfx_ctx.display, &visual, ctx, X_TRUE);
		}
	}

	win.src.ctx.native.window = rgfw_glXCreateWindow(_gfx_ctx.display, bestFbc, win.src.window, null);
	rgfw_glXMakeCurrent(_gfx_ctx.display, win.src.ctx.native.window, win.src.ctx.native.ctx);
	rgfw_debugCallback(RGFW_TYPE_INFO, RGFW_INFO_OPEN_GL, "OpenGL context initalized.");
	rgfw_window_swapInterval_OpenGL(win, 0);
	return true;
}

// private
fn void rgfw_window_flash_X11(GfxWindow* win, GfxFlashRequest request)
{
	if (win.has_focus() && request != CANCEL) return;
	XWMHints* wmhints = xGetWMHints(_gfx_ctx.display, win.src.window);
	if (wmhints == null) return;
	if (request)
	{
		wmhints.flags |= 1 << 8; // XUrgencyHint
		if (request == BRIEFLY) win.src.flashEnd = rgfw_unix_getTimeNS() + (ulong)1e9;
		if (request == UNTIL_FOCUSED) win.src.flashEnd = (ulong)-1;
	}
	else
	{
		win.src.flashEnd = 0;
		wmhints.flags &= ~(1 << 8);
	}
	xSetWMHints(_gfx_ctx.display, win.src.window, wmhints);
	xFree(wmhints);
}

// private
fn void? rgfw_createSurfacePtr_X11(void* data, int w, int h, GfxFormat format, GfxSurface* self, GfxWindow* window)
{
	if (!window) window = _gfx_ctx.root;
	self.data = data;
	self.size = { w, h };
	self.format = format;

	XWindowAttributes attrs;
	if (xGetWindowAttributes(_gfx_ctx.display, window.src.window, &attrs) == 0)
	{
		log::error("Failed to get window attributes.", category: log::CATEGORY_VIDEO);
		return SURFACE_CREATION_FAILED~;
	}
	defer catch self.release();
	self.native.bitmap = xCreateImage(_gfx_ctx.display, attrs.visual, (uint)attrs.depth, 2 /* ZPixmap */, 0, null, ...(uint[<2>])self.size, 32, 0);
	self.native.buffer = malloc((sz)w * h * 4);
	self.native.format = rgfw_xImageGetFormat(self.native.bitmap);

	if (!self.native.bitmap)
	{
		log::error("Failed to create XImage.", category: log::CATEGORY_VIDEO);
		return SURFACE_CREATION_FAILED~;
	}
}

// private
fn bool rgfw_window_isFloating_X11(GfxWindow* win)
{
	Atom actualType;
	int actualFormat;
	CULong nitems, bytesAfter;
	Atom* propReturn = null;
	int status = xGetWindowProperty(_gfx_ctx.display, win.src.window, _gfx_ctx.netWmState, 0, ~0, 0, 4, &actualType, &actualFormat, &nitems, &bytesAfter, (char**)&propReturn);
	if (status != 0 || actualType != 4) return false;
	for (CULong i = 0; i < nitems; i++) if (propReturn[i] == _gfx_ctx.netWmStateAbove) return true;
	if (propReturn != null) xFree(propReturn);
	return false;
}

// private
fn void rgfw_window_setName_X11(GfxWindow* win, String name) => @pool()
{
	ZString cstring = name.zstr_copy(tmem);
	xutf8SetWMProperties(_gfx_ctx.display, win.src.window, cstring, cstring, null, 0, null, null, null);
	xStoreName(_gfx_ctx.display, win.src.window, cstring);
	xChangeProperty(_gfx_ctx.display, win.src.window, _gfx_ctx.netWmName, _gfx_ctx.utf8String, 8, 0, cstring, name.len);
}

// private
fn bool rgfw_window_setIconEx_X11(GfxWindow* win, char* dataSrc, int w, int h, GfxFormat format, GfxWindowIconType type)
{
	if (dataSrc == null)
	{
		return !!xChangeProperty(_gfx_ctx.display, win.src.window, _gfx_ctx.netWmIcon, 6, 32, 0, null, 0);
	}
	int count = 2 + (w * h);
	CULong* data = calloc((sz)count * CULong::size);
	data[0] = (CULong)w;
	data[1] = (CULong)h;

	copy_image_data_64((char*)&data[2], w, h, BGRA8, dataSrc, format, true, null);
	bool res = true;
	if (type & TASKBAR)
	{
		res = !!xChangeProperty(_gfx_ctx.display, win.src.window, _gfx_ctx.netWmIcon, 6, 32, 0, (char*)data, count);
	}

	copy_image_data_64((char*)&data[2], w, h, BGRA8, dataSrc, format, false, null);

	if (type & WINDOW)
	{
		XWMHints wmHints;
		wmHints.flags = 1 << 2; // IconPixmapHint

		int depth = xDefaultDepth(_gfx_ctx.display, xDefaultScreen(_gfx_ctx.display));
		void* image = xCreateImage(_gfx_ctx.display, xDefaultVisual(_gfx_ctx.display, xDefaultScreen(_gfx_ctx.display)),
			(uint)depth, 2 /* ZPixmap */, 0, (char*)&data[2], (uint)w, (uint)h, 32, 0);

		wmHints.icon_pixmap = xCreatePixmap(_gfx_ctx.display, win.src.window, (uint)w, (uint)h, (uint)depth);
		xPutImage(_gfx_ctx.display, wmHints.icon_pixmap, xDefaultGC(_gfx_ctx.display, xDefaultScreen(_gfx_ctx.display)), image, 0, 0, 0, 0, (uint)w, (uint)h);
		((XImageHeader*)image).data = null;
		xDestroyImage(image);

		xSetWMHints(_gfx_ctx.display, win.src.window, &wmHints);
	}

	free(data);
	xFlush(_gfx_ctx.display);
	return res;
}

// private
fn GfxMonitor* rgfw_window_getMonitor_X11(GfxWindow* win)
{
	XWindowAttributes attrs;
	if (xGetWindowAttributes(_gfx_ctx.display, win.src.window, &attrs) == 0) return null;
	for (GfxMonitorNode* node = _gfx_ctx.monitors.list.head; node != null; node = node.next)
	{
		if ((attrs.x < node.mon.origin.x + node.mon.mode.w) && (attrs.x + attrs.width > node.mon.origin.x) &&
			(attrs.y < node.mon.origin.y + node.mon.mode.h) && (attrs.y + attrs.height > node.mon.origin.y))
		{
			return &node.mon;
		}
	}
	return _gfx_ctx.monitors.list.head != null ? &_gfx_ctx.monitors.list.head.mon : null;
}

// public
fn void rgfw_window_showPlatform_X11(GfxWindow* win)
{
	win.internal.flags.hide = false;
	if (!rgfw_window_isHidden(win)) return;
	xMapWindow(_gfx_ctx.display, win.src.window);
	rgfw_window_move(win, win.x, win.y);

	rgfw_waitForShowEvent_X11(win);
	rgfw_window_setFullscreen(win, win.is_fullscreen());
}

// public
fn bool rgfw_waitForShowEvent_X11(GfxWindow* win)
{
	$if $feat(X11_WAYLAND):
	XEvent dummy;
	while (xCheckTypedWindowEvent(_gfx_ctx.display, win.src.window, 15 /* VisibilityNotify */, &dummy) == 0)
	{
		rgfw_waitForEvent(100);
	}
	return true;
	$else
	XEvent dummy;
	while (true)
	{
		// XCheckTypedWindowEvent(display, window, VisibilityNotify, &dummy)
		if (xPending(_gfx_ctx.display) == 0) break;
		xNextEvent(_gfx_ctx.display, &dummy);
		if (dummy.type == 15 /* VisibilityNotify */ && dummy.xany.window == win.src.window) break;
	}
	return true;
	$endif
}

// public
fn bool rgfw_loadVulkan()
{
	if (_gfx_ctx.vulkan_handle != null) return true;
	ZString[3] names = {"libvulkan.so.1", "libvulkan.so", "libMoltenVK.so"};
	for (sz i = 0; i < 3; i++)
	{
		_gfx_ctx.vulkan_handle = libc::dlopen(names[i], RTLD_LAZY | RTLD_LOCAL);
		if (_gfx_ctx.vulkan_handle != null) break;
	}
	if (_gfx_ctx.vulkan_handle == null) return false;
	void* proc = libc::dlsym(_gfx_ctx.vulkan_handle, "vkGetInstanceProcAddr");
	if (proc == null) return false;
	_gfx_ctx.vkGetInstanceProcAddress = (GfxVkGetInstanceProcAddressProc)proc;
	return true;
}

// private
fn void rgfw_toggleXMaximized(GfxWindow* win, bool maximized)
{
	XEvent xev;
	xev.type = 33; // ClientMessage
	xev.xclient.window = win.src.window;
	xev.xclient.message_type = _gfx_ctx.netWmState;
	xev.xclient.format = 32;
	xev.xclient.data.l[0] = (int)maximized;
	xev.xclient.data.l[1] = (CLong)_gfx_ctx.netWmStateMaximizedHorz;
	xev.xclient.data.l[2] = (CLong)_gfx_ctx.netWmStateMaximizedVert;
	xSendEvent(_gfx_ctx.display, xDefaultRootWindow(_gfx_ctx.display), 0, (1 << 20) | (1 << 19), &xev);
}

// public
fn void rgfw_unix_parseURI(GfxWindow* win, ZString data)
{
	ZString prefix = "file://";
	ZString line;
	ZString savePtr;
	while ((line = (ZString)strtokR(data, "\r\n", (char**)&savePtr)) != null)
	{
		data = null;

		if (line[0] == '#') continue;

		ZString l;
		for (l = line; true; l++)
		{
			if ((char*)l - (char*)line > 7) { break; }
			else if (l[0] != prefix[(char*)l - (char*)line]) { break; }
			else if (l[0] == 0 && prefix[(char*)l - (char*)line] == 0)
			{
				line += 7;
				while (line[0] != '/') line++;
				break;
			}
			else if (l[0] == 0) { break; }
		}

		sz len = line.len();
		ZString path = (ZString)malloc(len + 1);

		sz index = 0;
		while (line[0] != 0)
		{
			if (line[0] == '%' && line[1] != 0 && line[2] != 0)
			{
				char[3] digits = {0, 0, 0};
				digits[0] = line[1];
				digits[1] = line[2];
				digits[2] = 0;
				path[index] = (char)strtol((char*)&digits, null, 16);
				line += 2;
			}
			else
			{
				if (index >= len) break;
				path[index] = line[0];
			}
			index++;
			line++;
		}

		path[len] = 0;
		rgfw_dataDropCallback(win, path, len + 1, RGFW_DATA_FILE);
		free(path);
	}
}

// public
fn VkResult rgfw_window_createSurface_Vulkan(GfxWindow* win, VkInstance instance, VkSurfaceKHR* surface)
{
	if (surface == null || win == null) return VK_ERROR_EXTENSION_NOT_PRESENT;
	*surface = 0;
	GfxFn raw = rgfw_getInstanceProcAddress_Vulkan(instance, "vkCreateXlibSurfaceKHR");
	if (raw == null) return VK_ERROR_EXTENSION_NOT_PRESENT;
	VkXlibSurfaceCreateInfoKHR createInfo;
	createInfo.sType = VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR;
	createInfo.pNext = null;
	createInfo.flags = 0;
	createInfo.dpy = _gfx_ctx.display;
	createInfo.window = win.src.window;
	return ((RgfwVkCreateXlibSurfaceKHRProc)raw)(instance, &createInfo, null, surface);
}

// public
fn void rgfw_window_getVisual(XVisualInfo* visual, bool transparent)
{
	visual.visual = xDefaultVisual(_gfx_ctx.display, xDefaultScreen(_gfx_ctx.display));
	visual.depth = xDefaultDepth(_gfx_ctx.display, xDefaultScreen(_gfx_ctx.display));
	if (transparent)
	{
		xMatchVisualInfo(_gfx_ctx.display, xDefaultScreen(_gfx_ctx.display), 32, 4 /* TrueColor */, visual);
		if (visual.depth != 32) rgfw_debugCallback(RGFW_TYPE_WARNING, RGFW_WARNING_OPEN_GL, "Failed to load a 32-bit depth.");
	}
}

// private
fn void rgfw_window_setXAtom(GfxWindow* win, Atom netAtom, bool fullscreen)
{
	XEvent xev;
	xev.xclient.type = 33;
	xev.xclient.send_event = 1;
	xev.xclient.message_type = _gfx_ctx.netWmState;
	xev.xclient.window = win.src.window;
	xev.xclient.format = 32;
	xev.xclient.data.l[0] = (int)fullscreen;
	xev.xclient.data.l[1] = (CLong)netAtom;
	xSendEvent(_gfx_ctx.display, xDefaultRootWindow(_gfx_ctx.display), 0, (1 << 19) | (1 << 20), &xev);
}

// public
fn void rgfw_x11IcCallback(void* ic, char* clientData, char* callData)
{
	GfxWindow* win = (GfxWindow*)(void*)clientData;
	win.src.ic = null;
}

// public
fn void rgfw_x11ImCallback(void* im, char* clientData, char* callData)
{
	_gfx_ctx.im = null;
}

// public
fn void rgfw_x11ImInitCallback(GfxXDisplay* d, GfxXPointer clientData, GfxXPointer callData)
{
	if (_gfx_ctx.im != null) return;

	_gfx_ctx.im = xOpenIM(_gfx_ctx.display, null, null, null);
	if (_gfx_ctx.im == null) return;

	bool found = false;
	XImStyles* styles = null;

	if (xGetIMValues(_gfx_ctx.im, "queryInputStyle", &styles, (char*)null) != null)
	{
		found = false;
	}
	else
	{
		for (uint i = 0; i < styles.count_styles; i++)
		{
			if (styles.supported_styles[i] == (0x0008 | 0x0400) /* XIMPreeditNothing | XIMStatusNothing */)
			{
				found = true;
				break;
			}
		}
		xFree(styles);
	}

	if (found == false)
	{
		xCloseIM(_gfx_ctx.im);
		_gfx_ctx.im = null;
		return;
	}

	XImCallback callback;
	callback.callback = &rgfw_x11ImCallback;
	callback.client_data = null;
	xSetIMValues(_gfx_ctx.im, "destroyCallback", &callback, (char*)null);
}

// public
fn int rgfw_xErrorHandler(GfxXDisplay* display, XErrorEvent* ev)
{
	char[512] errorText;
	xGetErrorText(display, ev.error_code, (char*)&errorText, 512);
	rgfw_debugCallback(RGFW_TYPE_ERROR, RGFW_ERR_X11, (ZString)&errorText);
	_gfx_ctx.x11Error = ev;
	return 0;
}

// public
fn void* rgfw_xGetMode(XRRCrtcInfo* ci, XRRScreenResources* res, RRMode mode, GfxMonitorMode* foundMode)
{
	XRRModeInfoNative* mi = null;
	for (int j = 0; j < res.nmode; j++) if (res.modes[j].id == mode) mi = &res.modes[j];
	if (mi == null) return null;

	foundMode.w = (int)mi.width;
	foundMode.h = (int)mi.height;
	if (ci.rotation == 4 || ci.rotation == 8) // RR_Rotate_90 / RR_Rotate_270
	{
		foundMode.w = (int)mi.height;
		foundMode.h = (int)mi.width;
	}

	rgfw_splitBPP((uint)xDefaultDepth(_gfx_ctx.display, xDefaultScreen(_gfx_ctx.display)), foundMode);
	foundMode.src = (void*)(uptr)mode;
	foundMode.refresh_rate = 0;
	if (mi.hTotal == 0 || mi.vTotal == 0) return mi;

	uint vTotal = mi.vTotal;
	int numerator = (int)mi.dotClock;
	int denominator = (int)(mi.hTotal * vTotal);
	if (denominator <= 0) denominator = 1;
	float refresh_rate = (float)numerator / (float)denominator;
	foundMode.refresh_rate = (float)((int)(refresh_rate * 100 + 0.5f)) / 100.0f;
	return mi;
}

// public
fn void rgfw_xGetSystemContentDPI(float* dpi)
{
	if (dpi == null) return;
	float dpiOutput = 96.0f;
	ZString rms = xResourceManagerString(_gfx_ctx.display);
	if (rms == null) return;
	void* db = xrmGetStringDatabase(rms);
	if (db == null) return;
	ZString type = null;
	// xrmGetResource's value-out parameter is a small {len,addr} struct; omitted here.
	xrmDestroyDatabase(db);
	*dpi = dpiOutput;
}

// public
fn void rgfw_xHandleClipboardSelection(XEvent* event)
{
	XSelectionRequestEvent* request = &event.xselectionrequest;
	Atom[2] formats = {_gfx_ctx.utf8String, 31 /* XA_STRING */};

	if (request.target == _gfx_ctx.targets)
	{
		Atom[4] targets = {_gfx_ctx.targets, _gfx_ctx.multiple, _gfx_ctx.utf8String, 31};
		xChangeProperty(_gfx_ctx.display, request.requestor, request.property, 4 /* XA_ATOM */, 32, 0, (char*)&targets, 4);
	}
	else if (request.target == _gfx_ctx.multiple)
	{
		Atom* targets = null;
		Atom actualType;
		int actualFormat;
		CULong count, bytesAfter;
		xGetWindowProperty(_gfx_ctx.display, request.requestor, request.property, 0, ~0, 0, _gfx_ctx.atomPair,
			&actualType, &actualFormat, &count, &bytesAfter, (char**)&targets);

		for (CULong i = 0; i < count; i += 2)
		{
			if (targets[i] == _gfx_ctx.utf8String || targets[i] == 31 /* XA_STRING */)
			{
				xChangeProperty(_gfx_ctx.display, request.requestor, targets[i + 1], targets[i],
					8, 0, _gfx_ctx.unixClipboard.data, (int)_gfx_ctx.unixClipboard.length);
			}
			else
			{
				targets[i + 1] = 0; // None
			}
		}

		xChangeProperty(_gfx_ctx.display, request.requestor, request.property, _gfx_ctx.atomPair, 32, 0, (char*)targets, (int)count);
		xFlush(_gfx_ctx.display);
		xFree(targets);
	}
	else if (request.target == _gfx_ctx.saveTargets)
	{
		xChangeProperty(_gfx_ctx.display, request.requestor, request.property, 0, 32, 0, null, 0);
	}
	else
	{
		for (sz i = 0; i < 2; i++)
		{
			if (request.target != formats[i]) continue;
			xChangeProperty(_gfx_ctx.display, request.requestor, request.property, request.target,
				8, 0, _gfx_ctx.unixClipboard.data, (int)_gfx_ctx.unixClipboard.length);
		}
	}

	XEvent reply;
	reply.type = 31; // SelectionNotify
	reply.xselection.property = request.property;
	reply.xselection.display = request.display;
	reply.xselection.requestor = request.requestor;
	reply.xselection.selection = request.selection;
	reply.xselection.target = request.target;
	reply.xselection.time = request.time;
	xSendEvent(_gfx_ctx.display, request.requestor, 0, 0, &reply);
	xFlush(_gfx_ctx.display);
}

// private
fn int rgfw_xHandleClipboardSelectionHelper()
{
	XEvent event;
	xPending(_gfx_ctx.display);
	if (qLength(_gfx_ctx.display) != 0 || xEventsQueued(_gfx_ctx.display, 0) + xEventsQueued(_gfx_ctx.display, 1) != 0)
	{
		xNextEvent(_gfx_ctx.display, &event);
	}
	else
	{
		return 0;
	}

	if (event.type == 30) { rgfw_xHandleClipboardSelection(&event); return 0; } // SelectionRequest
	return 0;
}

// public
fn GfxFormat rgfw_xImageGetFormat(void* imagePtr)
{
	// XImage*: bits_per_pixel/red_mask/green_mask/blue_mask live at a fixed
	// offset in the real struct; here we only ever set the format ourselves
	// right after xCreateImage, so this mirrors the original's fallback path.
	return BGRA8;
}
