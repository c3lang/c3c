module test;

func void test1()
{
    bool! x = 0;
    if (x) // #error: 'bool!' cannot be converted to 'bool'
    {
        x = 100;
    }
}

func void test2()
{
    bool! x = 0;
    while (x) // #error: 'bool!' cannot be converted to 'bool'
    {
        x = false;
    }
}

func void test3()
{
    bool! x = 0;
    double y = 1;
    do
    {
        y = y + 1;
    } while (y);
    do
    {
        x = !x;
    }
    while (x); // #error: 'bool!' cannot be converted to 'bool'
}