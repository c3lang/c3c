module baz::foo;

import bar;

struct Foo
{
    int a;
}

extern func void printf(char *hello);

private func void ofke()
{}

private func void exple() {}

func void test()
{
    printf("Hello from baz::foo::test()!\n");
}