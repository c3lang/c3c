module test;

union Union
{
    int x;
    double y;
}

define Foo = distinct Union;

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

define Union3 = distinct Union2;

define UnionArr = distinct Union3[3];

fn void test(int x)
{
    Union s = { .y = 2.0 };
    Foo f2 = (Foo)(s);
    Foo f3 = { .x = 1 };
    Union2 s2 = { .f = { .y = 1 } };
    Union2 s3 = { .bar.x.y = 3.0 };
    UnionArr a = { [0].bar.x.x = 100 };
}
