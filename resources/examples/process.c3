module test;
import std::os::process;
import std::io;

fn void! main()
{
	String command = env::WIN32 ? "dir" : "ls";
	SubProcess x = process::create({ command }, { .search_user_path = true })!!;
	x.join()!;
	Stream stream = x.stdout().as_stream();
	while (try char b = stream.read_byte())
	{
		io::printf("%c", b);
	}
	io::printn("...Done");
}
