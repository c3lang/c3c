module repro;

import std::io;

interface VeryOptional
{
    fn void do_something() @optional;
}

struct Foo (VeryOptional)
{
    String name;
}

Foo foo = { "Foo" };

fn VeryOptional? get_very_optional()
{
    return &foo;
}

fn void main()
{
    VeryOptional? v = get_very_optional();
    if (&v.do_something) // #error: expression may not be an optional
    {
        v.do_something();
    }
    else
    {
        io::printfn("do_something is not implemented for Foo");
    }
}