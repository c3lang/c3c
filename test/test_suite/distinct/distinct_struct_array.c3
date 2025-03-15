module test;

typedef Foo = int;

struct Struct
{
    Foo x;
    int y;
}

typedef Struct2 = Struct;
typedef StructArr = Struct2[3];

fn void test(int x)
{
    StructArr z = { { .x = 1 }, { .y = x }, { 1, 2 }};
    usz len = z.len;
    Foo zz = z[2].x;
}
