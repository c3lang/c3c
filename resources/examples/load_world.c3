module load_world;
import std::io;
fn void main()
{
	File f = file::open("examples/hello_world.txt", "rb")!!;
	defer f.close()!!;
	while (!f.eof())
	{
		@pool() { io::printn(io::treadline(&f)!!); };
	}
}