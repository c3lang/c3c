func void test1()
{

    int! a;
    int b;
    int*! x;
    if (try int &a = a) {} // #error: Expected a variable name after the type.
}

func void test2()
{
    int! a;
    int b;
    int*! x;
    if (try foo::z = a, b += 1) {} // #error: The 'try' must be placed last, can you change it?
}