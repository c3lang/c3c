module test;

fn void test()
{
    int[3]! x;
    int g;
    foreach_r (z : x) // #error: The expression may not be optional.
    {
        g += z;
        x[0] = 1;
    }
}
