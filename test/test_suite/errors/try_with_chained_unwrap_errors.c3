func void test()
{
    int! a;
    int b;
    if (try int b = a) {} // #error: 'b' would shadow a previous declaration.
}
func void test2()
{
    int! a;
    int b;
    if (try b = a) {}
    if (try test2 = a) {} // #error: 'try' expected an assignable variable or expression here, did you make a mistake?
}

const int BAZ = 1;

func void test3()
{

    int! a;
    int b;
    if (try BAZ = a) {} // #error: 'try' expected an assignable variable or expression here, did you make a mistake?
}


func void test4()
{

    int! a;
    int b;
    if (try b = 1) {} // #error: Expected a failable expression to 'try' here. If it isn't a failable, remove 'try'
}

func void test5()
{

    int! a;
    int b;
    if (try a = a) {} // #error: This is a failable variable, you should only have non-failable variables on the left side unless you use 'try' without '='
}

func void test6()
{

    int! a;
    int b;
    int*! x;
    if (try *x = a) {} // #error: This is a failable expression, it can't go on the left hand side of a 'try'.
}


func void test7()
{
    int! a;
    int b;
    int*! x;
    if (try foo::z = a) {} // #error: The variable may not have a path.
}

func void test8()
{
    int! a;
    int b;
    if (b == 0, try b = a && try b = a && b > 0) {}
    if (try b = a && try b = a && b > 0) {}
    if (try c = a && try c = a) { c++; }
}

func void test9()
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

func void test10()
{
    int! a = 11;
    if (try a)
    {
        int g = a;
    }
    else
    {
        int g = a; // #error: 'int!' cannot be converted to 'int'
    }
}