module test;
import std::io;

macro @foo(;@body)
{
	var i = 1.0;
	@body();
}

fn void main()
{
	@foo() { int j = 1; };
	@foo() { var j = 1.0; }; // #error: is only allowed inside
}

fn void test()
{
	var g = 1; // #error: is only allowed inside
}

macro void test2m()
{
	var h = { 1, 2 }; // #error: cannot be inferred
}

fn void test2()
{
	test2m();
}
struct Foo { int a; }
macro void test3m()
{
	var $foo = Foo.membersof;
	var h = $foo[0]; // #error: compile time type
}
fn void test3()
{
	test3m();
}