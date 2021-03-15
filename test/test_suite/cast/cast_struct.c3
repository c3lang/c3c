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
    Bar y = cast(x as Bar);

    Baz z;
    int[2] w = cast(z as int[2]);
    z = cast(w as Baz);
    BazTwo v = cast(z as BazTwo);
    v = cast(w as BazTwo);
    z = cast(v as Baz);
    w = cast(v as int[2]);
}
