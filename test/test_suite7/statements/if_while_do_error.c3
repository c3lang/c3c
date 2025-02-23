module test;

fn void test1()
{
    bool! x = false;
    if (x) // #error: optional, but was 'bool!
    {
        x = 100;
    }
}

fn void test2()
{
    bool! x = false;
    while (x) // #error: optional, but was 'bool!
    {
        x = false;
    }
}

fn void test3()
{
    bool! x = false;
    double y = 1;
    do
    {
        y = y + 1;
    } while (y);
    do
    {
        x = !x;
    }
    while (x); // #error: 'bool!' cannot be implicitly converted to a regular boolean value
}