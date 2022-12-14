module foo;

struct Bar
{
    int x;
}

module baz;
import foo;
import std::io;

fn void foo::Bar.test(Bar *bar)
{
    io::println("Inside of baz::Bar.test");
}

fn void Bar.test(Bar *bar) // #error: This method is already defined for
{
    io::println("Inside of baz::Bar.test");
}

module abc;
import foo;
import baz;

fn void main()
{
    Bar bar;
    bar.test();
}

