module test;
import std::io;

macro int foo(int y) // #error: Missing return
{
	if (y > 5) return y;
}

fn void main()
{
	int x = foo(3);
}