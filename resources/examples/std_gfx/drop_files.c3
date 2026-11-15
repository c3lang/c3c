<*
 Prints the path, size and first bytes of every file dropped on the window.

 Native:
   c3c compile-run drop_files.c3
 Emscripten:
   c3c compile drop_files.c3 -o build/index.html --target emscripten
*>
module drop_files;
import std;

alias EmCallback @feat(EMSCRIPTEN) = fn void();
extern fn void emscripten_set_main_loop(
	EmCallback func,
	int fps,
	int simulate_infinite_loop
) @cname("emscripten_set_main_loop") @feat(EMSCRIPTEN);

GfxWindow* win;

fn void step() => @pool()
{
	while (try event = gfx::poll(win))
	{
		switch (event.type)
		{
			case DATA_DROP:
				for (GfxDataDropNode* node = event.drop.value; node; node = node.next)
				{
					String path = (String)node.data;
					char[]? data = file::load_temp(path);
					if (catch excuse = data)
					{
						io::printfn("Dropped [%s] %s: %s", node.type, path, excuse);
						continue;
					}
					sz preview = data.len < 32 ? data.len : 32;
					io::printfn("Dropped [%s] %s: %d bytes, starts with \"%s\"", node.type, path, data.len, (String)data[:preview]);
				}
			default:
				break;
		}
	}
}

fn void main() @winmain
{
	gfx::init("Drop files", {})!!;
	win = gfx::create_window("Drop files", 100, 100, 640, 480, { .allow_dnd = true })!!;

	$if $feat(EMSCRIPTEN):
		emscripten_set_main_loop(&step, 0, 1);
	$else
		while (!win.should_close())
		{
			step();
		}
		win.destroy();
		gfx::finalize();
	$endif
}
