struct Foo
{
    int a;
    float b;
}

struct Bar
{
    int b;
    float c;
}

struct Baz
{
    int b;
    int c;
}

struct BazTwo
{
    int[2] d;
    int e;
}

fn void test1()
{
    Foo x;
    Bar z = (Baz)(x); // #error: 'Foo' to 'Baz'
}
fn void test2()
{
    Baz x;
    BazTwo z = (BazTwo)(x); // #error: 'Baz' to 'BazTwo'
}

