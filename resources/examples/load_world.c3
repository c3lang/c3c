module load_world;
import std::io;
fn void! main()
{
	Path path = path::new("examples/hello_world.txt")!!;
	io::printfn("%s ... %s", path.as_str(), path::exists(path) ? "exists" : "doesn't exist");
	File f = file::open("examples/hello_world.txt", "rb")!;
	defer f.close()!!;
	while (!f.eof())
	{
		@pool() { io::printn(f.tgetline()); };
	}
}