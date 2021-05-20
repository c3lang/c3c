module test;

define Foo = distinct int;

struct Struct
{
    Foo x;
    int y;
}

define Struct2 = distinct Struct;
define StructArr = distinct Struct2[3];

func void test(int x)
{
    StructArr z = { { .x = 1 }, { .y = x }, { 1, 2 }};
    usize xr = z.sizeof;
    usize len = z.len;
    Foo zz = z[2].x;
}
