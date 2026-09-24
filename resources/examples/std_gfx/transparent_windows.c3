module transparent_windows;
import std;

fn int main()
{
	gfx::init("TransparentWindows", {})!!;
	defer gfx::finalize();

	Canvas c1;
	c1.init(gfx::create_window("Opaque Window", 100, 100, 640, 480, { .allow_dnd = true })!!, "Opaque Window");
	defer c1.destroy();

	Canvas c2;
	c2.init(gfx::create_window("Transparent Window", 760, 100, 640, 480, { .transparent = true, .allow_dnd = true })!!, "Transparent Window");
	defer c2.destroy();

	while (!c1.win.should_close() && !c2.win.should_close())
	{
		if (try e = gfx::wait(gfx::WAIT_FOREVER))
		{
			if (!e.common.win) continue;
			Canvas* c = (Canvas*)e.common.win.get_user_ptr();

			switch (e.type)
			{
				case WINDOW_CLOSE:
					return 0;
				case KEY_PRESSED:
					if (e.key.value == ESCAPE) return 0;
					if (e.key.mod.control && e.key.value == V)
					{
						if (try clip = gfx::read_clipboard(tmem, TEXT))
						{
							io::printfn("Clipboard: %s", clip.text);
						}
					}
				case MOUSE_MOTION:
					c.cursor = e.mouse.pos;
					nextcase;
				case WINDOW_RESIZED:
					c.draw();
					c.update_title();
				case MOUSE_BUTTON_PRESSED:
					switch (e.button.value)
					{
						case LEFT:
							c.circle_idx = (c.circle_idx + 1) % CIRCLE_COLORS.len;
							c.set_icon();
							c.draw();
						case RIGHT:
							c.bg_idx = (c.bg_idx + 1) % BG_COLORS.len;
							c.draw();
						default:
							break;
					}
				case MOUSE_SCROLL:
					if (e.delta.vector.y != 0)
					{
						c.radius = math::clamp(c.radius + (int)(e.delta.vector.y * 5.0f), 10, 300);
						c.draw();
						c.update_title();
					}
				case DATA_DROP:
					for (GfxDataDropNode* node = e.drop.value; node; node = node.next)
					{
						io::printfn("Dropped [%s]: %s", node.type, (ZString)node.data);
					}
				default:
					break;
			}
		}
	}
	return 0;
}

const uint[*] BG_COLORS = {
	0x30181820,
	0x30101830,
	0x30102818,
	0x30281028,
};

const uint[*] CIRCLE_COLORS = {
	0xE000E5FF,
	0xE050FA7B,
	0xE0FF5588,
	0xE0FFB86C,
};

struct Canvas
{
	GfxWindow* win;
	GfxSurface* surf;
	uint[] pixels;
	int width;
	int height;
	GfxPoint cursor;
	int radius;
	sz bg_idx;
	sz circle_idx;
	String title;
}

fn void Canvas.init(&self, GfxWindow* win, String title)
{
	self.win = win;
	self.title = title;
	self.win.set_user_ptr(self);
	self.win.show_mouse(false);
	self.cursor = { win.width / 2, win.height / 2 };
	self.radius = 60;
	self.set_icon();
	self.draw();
	self.update_title();
}

fn void Canvas.destroy(&self)
{
	if (self.surf) self.surf.destroy();
	free(self.pixels);
	self.win.destroy();
}

fn void Canvas.set_icon(&self)
{
	uint[16 * 16] icon_px;
	icon_px[..] = CIRCLE_COLORS[self.circle_idx];
	self.win.set_icon(&icon_px, 16, 16, BGRA8);
}

fn void Canvas.update_title(&self) => @pool()
{
	self.win.set_name(string::tformat("%s | Pos: %d,%d | Radius: %d",
		self.title, self.cursor.x, self.cursor.y, self.radius));
}

fn void Canvas.draw(&self)
{
	int w = self.win.width;
	int h = self.win.height;
	if (w <= 0 || h <= 0) return;

	if (w != self.width || h != self.height)
	{
		if (self.surf) self.surf.destroy();
		free(self.pixels);
		self.width = w;
		self.height = h;
		self.pixels = mem::alloc_array(uint, (sz)w * h);
		self.surf = gfx::create_surface((char*)self.pixels.ptr, w, h, BGRA8, self.win)!!;
	}

	self.pixels[..] = BG_COLORS[self.bg_idx];

	int r = self.radius;
	int r2 = r * r;
	int min_x = math::max(0, self.cursor.x - r);
	int max_x = math::min(w - 1, self.cursor.x + r);
	int min_y = math::max(0, self.cursor.y - r);
	int max_y = math::min(h - 1, self.cursor.y + r);

	uint color = CIRCLE_COLORS[self.circle_idx];
	for (int y = min_y; y <= max_y; y++)
	{
		int dy = y - self.cursor.y;
		sz row = (sz)y * w;
		for (int x = min_x; x <= max_x; x++)
		{
			int dx = x - self.cursor.x;
			if (dx * dx + dy * dy <= r2)
			{
				self.pixels[row + x] = color;
			}
		}
	}

	self.win.blit_surface(self.surf);
}