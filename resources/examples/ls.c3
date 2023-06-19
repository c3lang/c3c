import std::io;

fn void main()
{
	Path path = path::getcwd()!!;
	foreach (i, p : path::ls(path)!!)
	{
		io::printfn("%02d %s", i, p.as_str());
	}
}