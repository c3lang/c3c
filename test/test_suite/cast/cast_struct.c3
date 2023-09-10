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
    Bar y = (Bar)(x); // #error: 'Foo' to 'Bar'

    Baz z;
    int[2] w = (int[2])(z); // #error: 'Baz' to 'int[2]'
    z = (Baz)(w);
    BazTwo v = (BazTwo)(z); // #error: 'Baz' to 'BazTwo'
    v = (BazTwo)(w);
    z = (Baz)(v);
    w = (int[2])(v);
}
