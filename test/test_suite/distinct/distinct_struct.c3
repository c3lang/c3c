
struct Struct
{
    int x;
    double y;
}

typedef Struct as distinct Foo;

struct Struct2
{
    Foo f;
    int d;
    struct bar
    {
        Foo x;
    }
}
Foo f = { 1, 1.0 };

func void test(int x)
{
    Struct s = { 1, 2.0 };
    Foo f2 = (Foo)(s);
    Foo f3 = { .x = 1 };
    Struct2 s2 = { .f = { 1, 2.0 } };
    Struct2 s3 = { .bar.x.y = 3.0 };
}