module test;

extern fn void foo();
int[3] z;

fn void test1()
{
    int x;
    foreach (a : x) // #error: It's not possible to enumerate an expression of type 'int'.
    {
        foo();
    }
}

fn void test2()
{
    foreach (a : z) foo();
    foreach (i, a : z) foo();
    foreach (double i, a : z); // #error: Index must be an integer type, 'double' is not valid.
}

fn void test3()
{
    foreach (&a : z) foo();
    foreach (&i, &a : z) foo(); // #error: The index cannot be held by reference, did you accidentally add a '&'?
}

fn void test4()
{
    foreach (&a : z) foo();
    foreach (&i, a : z) foo(); // #error: The index cannot be held by reference, did you accidentally add a '&'?
}

fn void test5()
{
    foreach (int! y : z) foo();
}

fn void test6()
{
    foreach (int! i, y : z) foo(); // #error: The index may not be an optional.
}

fn void test7()
{
    foreach (int a : { 1, 2, 3 }) foo();
    foreach (a : { 1, 2, 3 }) foo(); // #error: Add an explicit type to the variable
}

fn void test8()
{
    foreach (int a : { z }) foo(); // #error: 'int[3]' to 'int'
}

