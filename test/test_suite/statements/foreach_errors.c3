module test;

extern func void foo();
int[3] z;

func void test1()
{
    int x;
    foreach (a : x) // #error: It's not possible to enumerate an expression of type 'int'.
    {
        foo();
    }
}

func void test2()
{
    foreach (a : z) foo();
    foreach (i, a : z) foo();
    foreach (double i, a : z); // #error: Index must be an integer type, 'double' is not valid.
}

func void test3()
{
    foreach (&a : z) foo();
    foreach (&i, &a : z) foo(); // #error: The index cannot be held by reference, did you accidentally add a '&'?
}

func void test4()
{
    foreach (&a : z) foo();
    foreach (&i, a : z) foo(); // #error: The index cannot be held by reference, did you accidentally add a '&'?
}

func void test5()
{
    foreach (int! y : z) foo(); // #error: The variable may not be a failable.
}

func void test6()
{
    foreach (int! i, y : z) foo(); // #error: The index may not be a failable.
}

func void test7()
{
    foreach (int a : { 1, 2, 3 }) foo();
    foreach (a : { 1, 2, 3 }) foo(); // #error: Add the type of your variable here if you want to iterate over
}

func void test8()
{
    foreach (int a : { z }) foo(); // #error: 'int[3]' into 'int'
}

