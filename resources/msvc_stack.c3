module test;
import std::io;
import std::collections::map;
import std::os;
import libc;

fn void bye() @finalizer
{
	libc::puts("Bye from finalizer!");
}

fn void test1()
{
	builtin::print_backtrace("hello", 1);


	int x = 1;
	return;
}

fn void main()
{
	test1();
}