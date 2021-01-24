module test;

union Union
{
    int x;
    double y;
}

typedef Union as distinct Foo;

union Union2
{
    Foo f;
    int d;
    struct bar
    {
        Foo x;
    }
}
Foo f = { .x = 1 };

typedef Union2 as distinct Union3;

typedef Union3[3] as distinct UnionArr;

func void test(int x)
{
    Union s = { .y = 2.0 };
    Foo f2 = cast(s as Foo);
    Foo f3 = { .x = 1 };
    Union2 s2 = { .f = { .y = 1 } };
    Union2 s3 = { .bar.x.y = 3.0 };
    UnionArr a = { [0].bar.x.x = 100 };
}
