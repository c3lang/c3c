module test;
import std::io;
import std::collections::map;
import std::os;

fn void? test2()
{
	builtin::print_backtrace("hello", 1);
}

fn void test1()
{
	(void)test2();
}


fn void foo()
{
	baz();
}

macro bar()
{
	builtin::print_backtrace("bar", 1);
}

macro baz()
{
	bar();
}


fn void main()
{
	test1();
	foo();
}