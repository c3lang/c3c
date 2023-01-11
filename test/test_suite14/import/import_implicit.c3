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
    abc::test(); // #error: Did you mean the function 'abc::test'
    Foo f; // #error: Did you mean the struct 'Foo'
}