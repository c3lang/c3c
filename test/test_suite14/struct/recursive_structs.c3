struct Test1
{
    Test1 *x;
}

struct Test2
{
    Test2[] y;
}

struct Test3 // #error: Recursive definition
{
    Test3[4] z;
}

struct Test4
{
    Test4[3]* w;
}


struct Test5
{
    Test5[3][] w;
}
