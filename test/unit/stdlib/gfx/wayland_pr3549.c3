module test::gfx_wayland_pr3549 @test @feat(BSD, LINUX) @feat(WAYLAND) @feat(NO_X11);
import std::gfx;

fn void icon_buffer_must_outlive_its_icon()
{
	if (catch gfx::init("pr3549-icon", {})) return;
	defer gfx::finalize();
	GfxWindow*? win = gfx::create_window("icon", 100, 100, 64, 64, {});
	if (catch win) return;
	defer win.destroy();

	char[32 * 32 * 4] pixels;
	if (!win.set_icon(&pixels, 32, 32, RGBA8)) return; // No xdg-toplevel-icon support.

	void* display = wl_proxy_get_display(win.src.surface);
	assert(wl_display_roundtrip(display) >= 0, "the connection was closed after set_icon");
	assert(wl_display_get_error(display) == 0, "protocol error after set_icon (no_buffer = 3 on xdg_toplevel_icon_v1)");
}

// `window_hide` clears `configured` for every window, but `window_show_platform` only waits for the re-map
// configure when `xdg_surface` is set. A layer-shell surface has to be re-mapped the same way (commit without a
// buffer, wait for configure), so show() must not return before the configure has arrived.
fn void layer_surface_show_waits_for_configure_after_hide()
{
	if (catch gfx::init("pr3549-layer", {})) return;
	defer gfx::finalize();
	WlLayerConfig config = { .layer = TOP, .namespace = "pr3549" };
	GfxWindow*? win = gfx::create_layer_window("pr3549-layer", 100, 40, config);
	if (catch win) return; // No zwlr_layer_shell_v1.
	defer win.destroy();

	win.hide();
	win.show();
	assert(win.src.configured, "show() returned before the layer surface was configured again");
}

// Control for the test above: the same hide/show on an xdg toplevel does wait, so this one passes with the PR.
fn void xdg_window_show_waits_for_configure_after_hide()
{
	if (catch gfx::init("pr3549-xdg", {})) return;
	defer gfx::finalize();
	GfxWindow*? win = gfx::create_window("xdg", 100, 100, 100, 40, {});
	if (catch win) return;
	defer win.destroy();

	win.hide();
	win.show();
	assert(win.src.configured, "show() returned before the xdg surface was configured again");
}

// Wayland has no request to undo xdg_toplevel.set_minimized, so restore() cannot actually restore a minimized
// window. The PR makes `window_restore_platform` (and `window_show_platform`) set `minimized = false` anyway, so
// is_minimized() reports a window as restored while the compositor still has it minimized. Before the PR the flag
// was only cleared when the keyboard focus returned to the window.
fn void restore_does_not_claim_a_minimized_window_is_restored()
{
	if (catch gfx::init("pr3549-minimize", {})) return;
	defer gfx::finalize();
	GfxWindow*? win = gfx::create_window("minimize", 100, 100, 100, 40, {});
	if (catch win) return;
	defer win.destroy();

	win.minimize();
	assert(win.is_minimized());
	win.restore(); // No events are pumped in between, so nothing can have returned focus to the window.
	assert(win.is_minimized(), "restore() on a minimized Wayland window must not report it as restored");
}

module test::gfx_wayland_pr3549 @feat(BSD, LINUX) @feat(WAYLAND) @feat(NO_X11);

extern fn void* wl_proxy_get_display(void* proxy) @cname("wl_proxy_get_display");
extern fn int wl_display_roundtrip(void* display) @cname("wl_display_roundtrip");
extern fn int wl_display_get_error(void* display) @cname("wl_display_get_error");
