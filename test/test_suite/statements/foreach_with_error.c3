module test;

func void test()
{
    int[3]! x;
    int g;
    foreach (z : x) // #error: It's not possible to enumerate an expression of type 'int[3]!'
    {
        g += z;
        x[0] = 1;
    }
}
