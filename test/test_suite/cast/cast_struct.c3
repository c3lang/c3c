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

func void test()
{
    Foo x;
    Bar y = (Bar)(x);

    Baz z;
    int[2] w = (int[2])(z);
    z = (Baz)(w);
    BazTwo v = (BazTwo)(z);
    v = (BazTwo)(w);
    z = (Baz)(v);
    w = (int[2])(v);
}
