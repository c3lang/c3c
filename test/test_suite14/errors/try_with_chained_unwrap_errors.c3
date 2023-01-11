fn void test()
{
    int! a;
    int b;
    if (try int b = a) {} // #error: 'b' would shadow a previous declaration.
}
fn void test2()
{
    int! a;
    int b;
    if (try b = a) {}
    if (try test2 = a) {} // #error: You cannot assign a value to a function
}

const int BAZ = 1;

fn void test3()
{

    int! a;
    int b;
    if (try BAZ = a) {} // #error: You cannot assign to a constant
}


fn void test4()
{

    int! a;
    int b;
    if (try b = 1) {} // #error: remove 'try'
}

fn void test5()
{

    int! a;
    int b;
    if (try a = a) {} // #error: you use 'try' without '='
}

fn void test6()
{

    int! a;
    int b;
    int*! x;
    if (try *x = a) {} // #error: You cannot assign to a dereferenced optional.
}


fn void test7()
{
    int! a;
    int b;
    int*! x;
    if (try foo::z = a) {} // #error: The variable may not have a path.
}

fn void test8()
{
    int! a;
    int b;
    if (b == 0, try b = a && try b = a && b > 0) {}
    if (try b = a && try b = a && b > 0) {}
    if (try c = a && try c = a) { c++; }
}

fn void test9()
{
    int! a = 11;
    if (try z = a)
    {
        int g = z++;
    }
    else
    {
        z++; // #error: 'z' could not be found, did you spell it right?
    }
}

fn void test10()
{
    int! a = 11;
    if (try a)
    {
        int g = a;
    }
    else
    {
        int g = a; // #error: 'int!' to 'int'
    }
}