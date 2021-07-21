module foo;

struct Bar
{
    int x;
}

module baz;
import foo;
import std::io;

func void foo::Bar.test(Bar *bar)
{
    io::println("Inside of baz::Bar.test");
}

func void Bar.test(Bar *bar) // #error: This method is already defined in this module
{
    io::println("Inside of baz::Bar.test");
}

module abc;
import foo;
import baz;

func void main()
{
    Bar bar;
    bar.test();
}

