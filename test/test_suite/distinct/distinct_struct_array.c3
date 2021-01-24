module test;

typedef int as distinct Foo;

struct Struct
{
    Foo x;
    int y;
}

typedef Struct as distinct Struct2;
typedef Struct2[3] as distinct StructArr;

func void test(int x)
{
    StructArr z = { { .x = 1 }, { .y = x }, { 1, 2 }};
    usize xr = z.sizeof;
    usize len = z.len;
    Foo zz = z[2].x;
}
