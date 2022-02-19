fn void test1()
{

    int! a;
    int b;
    int*! x;
    if (try int &a = a) {} // #error: A new variable was expected.
}

fn void test2()
{
    int! a;
    int b;
    int*! x;
    if (try foo::z = a, b += 1) {} // #error: The 'try' must be placed last, can you change it?
}