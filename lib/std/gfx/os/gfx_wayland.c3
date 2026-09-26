module std::gfx  @feat(BSD, LINUX) @feat(WAYLAND);
import std::gfx::wl @public;

enum WlLayer : char
{
	BACKGROUND,
	BOTTOM,
	TOP,
	OVERLAY,
}

bitstruct WlAnchor : char
{
	bool top;
	bool bottom;
	bool left;
	bool right;
}

enum WlKeyboardInteractivity : char
{
	NONE,
	EXCLUSIVE,
	ON_DEMAND,
}

struct WlMargin
{
	int top;
	int right;
	int bottom;
	int left;
}

struct WlLayerConfig
{
	WlLayer layer;
	WlAnchor anchor;
	int exclusive_zone;
	WlMargin margin;
	WlKeyboardInteractivity keyboard;
	GfxMonitor* monitor;
	String namespace;
}

<*
 Create a layer-shell surface (dock, bar, wallpaper, notification, overlay).

 @param name : "Window title / identifier"
 @param w : "Initial width, 0 to let anchors dictate width"
 @param h : "Initial height, 0 to let anchors dictate height"
 @param config : "Layer-shell positioning and behavior parameters"
 @param flags : "Optional standard window flags"
 @return "The created layer-shell window"
 @return? GFX_CONTEXT_NOT_INITIALIZED : "If gfx::init has not been called"
 @return? WAYLAND_LAYER_SHELL_NOT_SUPPORTED : "If compositor lacks zwlr_layer_shell_v1"
 @return? SURFACE_CREATION_FAILED : "If surface could not be created"

 @require [flags] !flags.maximize && !flags.minimize && !flags.fullscreen_exclusive && !flags.fullscreen_borderless && !flags.no_border : "Maximize, minimize, fullscreen and no-border options may not be used for layers"
*>
fn GfxWindow*? create_layer_window(String name, int w, int h, WlLayerConfig config, GfxWindowFlags flags = {}) => @pool()
{
	$if X11_OR_WAYLAND:
		if (!_gfx.use_wayland) return WAYLAND_LAYER_SHELL_NOT_SUPPORTED~;
	$endif
	return _create_window(name, 0, 0, w, h, flags, &config);
}

<*
 Set anchor edges for a layer surface.
*>
fn void GfxWindow.set_layer_anchor(&self, WlAnchor anchor)
{
	wl::window_set_layer_anchor(self, anchor);
}

<*
 Set exclusive zone in pixels (>0 reserves space, 0 is normal, -1 ignores other zones).
*>
fn void GfxWindow.set_layer_exclusive_zone(&self, int zone)
{
	wl::window_set_layer_exclusive_zone(self, zone);
}

<*
 Set margin offsets around the layer surface.
*>
fn void GfxWindow.set_layer_margin(&self, WlMargin margin)
{
	wl::window_set_layer_margin(self, margin);
}

<*
 Set keyboard focus interactivity (NONE, EXCLUSIVE, ON_DEMAND).
*>
fn void GfxWindow.set_layer_keyboard_interactivity(&self, WlKeyboardInteractivity mode)
{
	wl::window_set_layer_keyboard_interactivity(self, mode);
}

<*
 Move the layer surface to another z-order layer.
*>
fn void GfxWindow.set_layer(&self, WlLayer layer)
{
	wl::window_set_layer(self, layer);
}

<*
 Disambiguate which edge exclusive zone applies to when multiple anchors are set (protocol v5+).
*>
fn void GfxWindow.set_layer_exclusive_edge(&self, WlAnchor edge)
{
	wl::window_set_layer_exclusive_edge(self, edge);
}
