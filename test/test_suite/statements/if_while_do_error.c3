module test;

fn void test1()
{
    bool! x = 0;
    if (x) // #error: The expression may not be a failable, but was 'bool!'
    {
        x = 100;
    }
}

fn void test2()
{
    bool! x = 0;
    while (x) // #error: The expression may not be a failable, but was 'bool!'
    {
        x = false;
    }
}

fn void test3()
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
    while (x); // #error: A failable 'bool!' cannot be implicitly converted to a regular boolean value
}