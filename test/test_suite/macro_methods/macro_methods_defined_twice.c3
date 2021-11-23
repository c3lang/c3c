module foo;

struct Bar
{
    int x;
}

module baz;
import foo;
import std::io;

macro void foo::Bar.test(Bar &bar)
{
    io::println("Inside of baz::Bar.test");
}

macro void Bar.test(Bar &bar) // #error: This macro method is already defined in this module
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

