import std::io;

enum Foo
{
    BAR
}

fn void test()
{
	int x = BAR; // #error: type cannot be inferred correctly,
}

fn void main()
{
    io::printn(BAR); // #error: type cannot be inferred,
}