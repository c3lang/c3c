module test;
import std::io;
import std::collections::map;
import std::os;

fn void! test2()
{
	builtin::print_backtrace("hello", 1);
}

fn void test1()
{
	(void)test2();
}
fn void main()
{
	test1();
}