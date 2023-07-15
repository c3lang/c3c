module test;

import std::io;

macro @foo(int* &hello)
{
    hello = hello; // #error: You cannot assign to a ref
}

macro @bar(Foo* &f)
{
    f.a = 1;
    int* x = &f.a;
    Foo **ff = &f; // #error: You may not take the address
}

struct Foo
{
    int a;
}

fn void main()
{
    int a;
    @foo(a);
    Foo x;
    @bar(x);
}