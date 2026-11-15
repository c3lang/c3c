module my_program;
import std;

fn void my_panic(String summary, String file, String function, int line, String fmt, any[] args)
{
	io::eprintfn("My Panic");
	io::eprintfn("  summary:  '%s'", summary);
	io::eprintfn("  location: %s:%d in %s()", file, line, function);
	if (fmt.len)
	{
		io::eprint("  details:  ");
		io::eprintfn(fmt, ...args);
	}
	breakpoint();
}

fn void main(String[] args)
{
    builtin::panic = &my_panic;

    int[] values = { 1, 2 };
    io::printn(values[args.len + 5]);
}
