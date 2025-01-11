module test;
import std::io;

fn void main()
{
	int x = run()!!; // #error: You cannot cast
	io::printfn("x=%d", x);
}

macro run()
{
	int i;
	while (true)
	{
		i++;
		if (i > 10) break;
		if (i > 100) return IoError.EOF?;
	}
}