module load_world;
import std::io;
fn void! main()
{
	File f;
	f.open("examples/hello_world.txt", "rb")?;
	defer f.close()!!;
	while (!f.eof())
	{
		@pool() { io::printn(f.tgetline()); };
	}
}