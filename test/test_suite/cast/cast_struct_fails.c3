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

func void test1()
{
    Foo x;
    Bar z = (Baz)(x); // #error: Cannot cast 'Foo' to 'Baz'
}
func void test2()
{
    Baz x;
    BazTwo z = (BazTwo)(x); // #error: Cannot cast 'Baz' to 'BazTwo'
}

