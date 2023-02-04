module foo;

struct Bar
{
    int x;
}

module baz;
import foo;
import std::io;

macro void foo::Bar.@test(Bar &bar)
{
    io::printn("Inside of baz::Bar.test");
}

module bad;
import foo;
macro void Bar.@test(Bar &bar) // #error: This macro method is already defined for 'Bar'.
{
    io::printn("Inside of baz::Bar.test");
}

module abc;
import foo;

fn void main()
{
    Bar bar;
    bar.@test();
}

