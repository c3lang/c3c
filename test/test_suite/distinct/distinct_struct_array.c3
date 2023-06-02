module test;

def Foo = distinct int;

struct Struct
{
    Foo x;
    int y;
}

def Struct2 = distinct Struct;
def StructArr = distinct Struct2[3];

fn void test(int x)
{
    StructArr z = { { .x = 1 }, { .y = x }, { 1, 2 }};
    usz len = z.len;
    Foo zz = z[2].x;
}
