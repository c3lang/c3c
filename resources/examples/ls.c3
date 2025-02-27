import std::io;

fn void main()
{
	Path path = path::cwd(mem)!!;
	foreach (i, p : path::ls(mem, path)!!)
	{
		io::printfn("%02d %s", i, p.str_view());
	}
}