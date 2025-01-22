module baz::foo;

import bar;

struct Foo
{
    int a;
}

extern fn void printf(char *hello);

fn void ofke() @private
{}

fn void exple() @private{}

fn void test()
{
    printf("Hello from baz::foo::test()!\n");
}