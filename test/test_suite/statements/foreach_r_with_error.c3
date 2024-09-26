module test;

fn void test()
{
    int[3]! x;
    int g;
    foreach_r (z : x) // #error: The foreach iterable expression may not be
    {
        g += z;
        x[0] = 1;
    }
}
