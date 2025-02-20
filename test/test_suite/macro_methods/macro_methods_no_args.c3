import std::io;
// Issue #1990
struct Foo
{
	uint field;
}

macro Foo.bar() // #error: A method must start with an argument
{
	io.printn("UwU");
}

fn void main()
{
	Foo foo = {};
	foo.bar();
}