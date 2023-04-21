module test;
import std::time;
import std::io;

def foo = write;

def Int = int;

macro @timeit(#call)
{
    Clock t = clock::now();
    var result = #call;
    io::printfn("'%s' took %d ns", $stringify(#call), t.mark());
    return result;
}

fn int write()
{
	for (int i = 0; i < 1000; i++) io::printf("%d", i);
	io::printn();
	return 0;
}

fn void main()
{
	@timeit(write());
}