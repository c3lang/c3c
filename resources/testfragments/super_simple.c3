module bar;

func void test(int x)
{
    int i = x;
    switch (i + 1)
    {
        case 1:
            i = i + 1;
            next;
        case 3:
        case 2:
            i = i + 2;
        case 5:
        default:
            i = i * 100;
        case 7:
    }
    return;
}

func int test3()
{
    return 1;
}
typedef func void(int) as Foo;
//typedef int as Foo;

func void test2(int* x, int y, int z)
{
    int i = 3;
    uint ui = 2;
    int j = 129;
    float f = 23.2;
    f = f + 1.0;
    f = f - 1.0;
    f = f * 2.0;
    f = f / 3.0;
    i = i * 2;
    ui = ui * 2;
    i = i / 2;
    ui = ui / 2;
    i = i + 1;
    ui = ui + 1;
    i = i - 1;
    ui = ui - 1;
    // TODO x + 1;
}