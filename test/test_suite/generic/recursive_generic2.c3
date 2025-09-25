module foo { Type };

import std::collections::list;

struct Foo { // #error: Recursive definition of 'Foo'
    Type foo;
    Allocator allocator;
    List { Foo } children;
}

module test;
import std::collections::list;
import foo;

fn int main(String[] args)
{
    Foo { int } test;
    // assume we add children here but it's irrelevant for the repro
    Foo {int} [] range = test.children[4:20];

    return 0;
}