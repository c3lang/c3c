import std::io::path;

fn void! process_dir(String dir_name)
{
	path::Path p = path::temp_new(dir_name)!;
	path::PathWalker fnwalk = fn bool!(Path p, bool is_dir, void*) { // #error: issing return statement at the end
		io::printfn("path is %s", p);
    };
}