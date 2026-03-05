import std::io;

fn Foo foo()
{
	return (Foo){};
}

fn Bar bar()
{
	return (Bar){};
}

struct Bar
{
	$typeof(foo().foo) bar; // #error: Recursive definition of 'bar'
}

struct Foo
{
	$typeof(bar().bar) foo;
}

fn void main()
{
	io::printn("Hello, World!");
}
