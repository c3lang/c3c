module baz::foo;

import bar;

public struct Foo
{
    int a;
}

extern func void printf(char *hello);

local func void ofke()
{}

func void exple() {}

public func void test()
{
    printf("Hello from baz::foo::test()!\n");
}