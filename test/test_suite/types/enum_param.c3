enum Foo
{
    A, B
}

fn void test(Foo f)
{}

fn void test2()
{
    test(Foo.A);
}