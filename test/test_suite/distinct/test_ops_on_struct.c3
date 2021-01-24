module test;

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
}
Foo f = { 1, 1.0 };

func void test(int x)
{
    Struct s = { 1, 2.0 };
    Foo f2 = cast(s as Foo);
    Foo f3 = { .x = 1 };
    Struct2 s2 = { .f = { 1, 2.0 } };
}