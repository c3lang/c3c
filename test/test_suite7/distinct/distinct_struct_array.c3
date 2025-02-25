module test;

distinct Foo = int;

struct Struct
{
    Foo x;
    int y;
}

distinct Struct2 = Struct;
distinct StructArr = Struct2[3];

fn void test(int x)
{
    StructArr z = { { .x = 1 }, { .y = x }, { 1, 2 }};
    usz len = z.len;
    Foo zz = z[2].x;
}
