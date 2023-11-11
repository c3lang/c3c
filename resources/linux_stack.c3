module test;
import std::io;
import std::collections::map;
import std::os;

fn void! test2()
{
	BacktraceList list = linux::backtrace_load(mem::heap())!;
    foreach (Backtrace trace : list)
    {
		io::printfn("%s", trace);
	}
}

fn void test1()
{
	(void)test2();
}
fn void main()
{
	test1();
}