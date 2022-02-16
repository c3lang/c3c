module abc;

struct Foo
{
    int a;
    int b;
}

fn void test() {}

module dde;

fn void test()
{
    abc::test();
    Foo f;
}