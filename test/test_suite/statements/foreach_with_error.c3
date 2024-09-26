module test;

fn void test()
{
    int[3]! x;
    int g;
    foreach (z : x) // #error: The foreach iterable expression
    {
        g += z;
        x[0] = 1;
    }
}
