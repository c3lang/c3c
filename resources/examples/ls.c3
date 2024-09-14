import std::io;

fn void main()
{
	Path path = path::new_cwd()!!;
	foreach (i, p : path::new_ls(path)!!)
	{
		io::printfn("%02d %s", i, p.str_view());
	}
}