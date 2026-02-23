module test;
import std;

extern const void* FOO @cname("malloc");

macro foo($x) {}

fn void test()
{
	var $x = FOO; // #error: $x must be assigned a compile-time constant. 'extern' constants are resolved
}

fn void test2()
{
	foo(FOO); // #error: A compile-time parameter requires a value known at compile time. Did
}
