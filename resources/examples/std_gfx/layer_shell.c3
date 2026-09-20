<* 
 https://wayland.app/protocols/wlr-layer-shell-unstable-v1 
 needs feature WAYLAND -> `-D WAYLAND`
*>
module layer_shell;
import std;

fn void redraw(GfxWindow* win, uint color)
{
	if (win.width <= 0 || win.height <= 0) return;
	uint[] pixels = mem::alloc_array(uint, (sz)win.width * win.height);
	defer free(pixels);
	foreach (&p : pixels) *p = color;

	GfxSurface* surf = gfx::create_surface((char*)pixels.ptr, win.width, win.height, BGRA8, win) ?? null;
	if (!surf) return;
	defer surf.destroy();
	win.blit_surface(surf);
}

fn int main()
{
	gfx::init("LayerShellTest", { .enable_x11 = false })!!;
	defer gfx::finalize();

	GfxWlLayerConfig config = {
		.layer = TOP,
		.anchor = { .top = true, .left = true, .right = true },
		.exclusive_zone = 36,
		.keyboard = ON_DEMAND,
		.namespace = "c3-bar-test",
	};

	GfxWindow* win = gfx::create_layer_window("C3 Layer Bar", 800, 36, config)!!;
	defer win.destroy();

	uint[4] colors = { 0xFF282A36, 0xFF44475A, 0xFF6272A4, 0xFFBD93F9 };
	sz color_idx = 0;
	bool anchored_top = true;
	int exclusive = 36;
	bool has_margin = false;
	GfxWlKeyboardInteractivity kbd = ON_DEMAND;

	redraw(win, colors[color_idx]);

	io::printn("--- Layer Shell Interactive Test ---");
	io::printn(" [1-4]          : Layer (1: Background, 2: Bottom, 3: Top, 4: Overlay)");
	io::printn(" [Space]        : Toggle exclusive zone (0 <-> 36px)");
	io::printn(" [A]            : Toggle anchor edge (Top <-> Bottom)");
	io::printn(" [M]            : Toggle margins (0 <-> 8px)");
	io::printn(" [K / R-Click]  : Cycle keyboard interactivity (R-Click recovers from NONE)");
	io::printn(" [L-Click]      : Cycle bar color");
	io::printn(" [M-Click / Esc]: Exit");

	while MAIN_LOOP: (!win.should_close())
	{
		GfxEvent? initial_ev = gfx::wait(gfx::WAIT_FOREVER);
		GfxEvent ev;
		if (try ev_ok = initial_ev)
		{
			ev = ev_ok;
		}
		else
		{
			continue;
		}

		while (true)
		{
			switch (ev.type)
			{
				case WINDOW_CLOSE:
					break MAIN_LOOP;
				case WINDOW_RESIZED:
					redraw(win, colors[color_idx]);
				case MOUSE_BUTTON_PRESSED:
					switch (ev.button.value)
					{
						case LEFT:
							color_idx = (color_idx + 1) % colors.len;
							redraw(win, colors[color_idx]);
						case RIGHT:
							kbd = (GfxWlKeyboardInteractivity)(((int)kbd + 1) % 3);
							gfx::set_layer_keyboard_interactivity(win, kbd);
							io::printfn("Keyboard interactivity -> %s", kbd.description);
						case MIDDLE:
							break MAIN_LOOP;
						default:
							break;
					}
				case KEY_PRESSED:
					switch (ev.key.value)
					{
						case ESCAPE:
							break MAIN_LOOP;
						case NUM1:
							gfx::set_layer(win, BACKGROUND);
							io::printn("Layer -> BACKGROUND");
						case NUM2:
							gfx::set_layer(win, BOTTOM);
							io::printn("Layer -> BOTTOM");
						case NUM3:
							gfx::set_layer(win, TOP);
							io::printn("Layer -> TOP");
						case NUM4:
							gfx::set_layer(win, OVERLAY);
							io::printn("Layer -> OVERLAY");
						case SPACE:
							exclusive = exclusive == 36 ? 0 : 36;
							gfx::set_layer_exclusive_zone(win, exclusive);
							io::printfn("Exclusive zone -> %d", exclusive);
						case A:
							anchored_top = !anchored_top;
							GfxWlAnchor anchor = anchored_top
								? (GfxWlAnchor){ .top = true, .left = true, .right = true }
								: (GfxWlAnchor){ .bottom = true, .left = true, .right = true };
							gfx::set_layer_anchor(win, anchor);
							io::printfn("Anchor -> %s", anchored_top ? "TOP" : "BOTTOM");
						case M:
							has_margin = !has_margin;
							GfxWlMargin m = has_margin ? { 8, 16, 8, 16 } : {};
							gfx::set_layer_margin(win, m);
							io::printfn("Margin -> %s", has_margin ? "8,16,8,16" : "0");
						case K:
							kbd = (GfxWlKeyboardInteractivity)(((int)kbd + 1) % 3);
							gfx::set_layer_keyboard_interactivity(win, kbd);
							io::printfn("Keyboard interactivity -> %s", kbd.description);
						default:
							break;
					}
				case KEY_CHAR:
					io::printfn("Char received: %c (%d)", (char)ev.keyChar.value, ev.keyChar.value);
				default:
					break;
			}

			GfxEvent? next = gfx::peep(win);
			if (catch next) break;
			ev = next;
		}
	}
	return 0;
}
