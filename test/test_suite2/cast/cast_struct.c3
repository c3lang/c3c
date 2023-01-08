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
    int[1] d;
    int e;
}

fn void test()
{
    Foo x;
    Bar y = (Bar)(x); // #error: The cast 'Foo' to 'Bar' is not allowed

    Baz z;
    int[2] w = (int[2])(z); // #error: The cast 'Baz' to 'int[2]' is not allowed
    z = (Baz)(w);
    BazTwo v = (BazTwo)(z); // #error: The cast 'Baz' to 'BazTwo' is not allowed
    v = (BazTwo)(w);
    z = (Baz)(v);
    w = (int[2])(v);
}
