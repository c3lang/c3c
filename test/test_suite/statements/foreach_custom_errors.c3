// #skip
fn void test1()
{
    int x;
    foreach (a : x) { }; // #error: It's not possible to enumerate an expression of type 'int'
}

define Test1 = distinct int;

fn void test2()
{
    Test1 x;
    foreach (a : x) { }; // #error: This type cannot be iterated over, implement a method or method macro called 'iterator'
}

struct Test3
{}

fn void Test3.iterator(Test3* x, int y) { }

fn void test3()
{
    Test3 x;
    foreach (a : x) { }; // #error: 'iterator()' takes parameters and can't be used for 'foreach'.
}

struct Test4
{}

macro Test4.iterator(Test4* x) { }

fn void test4()
{
    Test4 x;
    foreach (a : x) { }; // #error: This type has an iterator without a declared result type, this can't be used with 'foreach'
}

struct Test5
{}

fn int Test5.iterator(Test5* x) { return 1; }

fn void test5()
{
    Test5 x;
    foreach (a : x) { }; // #error: This type has an implementation of 'iterator()' that doesn't return a struct, so it can't be used with 'foreach'
}

struct Test6
{}

fn TestIterator6 Test6.iterator(Test6* x) { return TestIterator6{}; }

struct TestIterator6
{}

fn void test6()
{
    Test6 x;
    foreach (a : x) { }; // #error: The iterator 'TestIterator6' is missing a definition for 'next()'
}

struct Test7
{}

fn TestIterator7 Test7.iterator(Test7* x) { return TestIterator7{}; }

struct TestIterator7
{}

fn bool TestIterator7.next(TestIterator7* x, int a, int b) { return true; }

fn void test7()
{
    Test7 x;
    foreach (a : x) { }; // #error: An iterator with a 'next()' that take takes 2 parameters can't be used for 'foreach', it should have 1.
}

