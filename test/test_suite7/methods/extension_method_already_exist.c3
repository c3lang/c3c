module foo;

fn void Bar.test(Bar *bar)
{
    io::printn("Inside of baz::Bar.test");
}

struct Bar
{
    int x;
}

module baz;
import foo;
import std::io;

fn void foo::Bar.test(Bar *bar) // #error: This method is already defined for 'Bar'
{
    io::printn("Inside of baz::Bar.test");
}

module abc;
import foo;
import baz;

fn void main()
{
    Bar bar;
    bar.test();
}