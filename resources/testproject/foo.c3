module baz::foo;

import bar;

struct Foo
{
    int a;
}

extern fn void printf(char *hello);

private fn void ofke()
{}

private fn void exple() {}

fn void test()
{
    printf("Hello from baz::foo::test()!\n");
}