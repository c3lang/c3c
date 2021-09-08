func void test1()
{
    int x;
    foreach (a : x) { }; // #error: It's not possible to enumerate an expression of type 'int'
}

define Test1 = distinct int;

func void test2()
{
    Test1 x;
    foreach (a : x) { }; // #error: This type cannot be iterated over, implement a method or method macro called 'iterator'
}

struct Test3
{}

func void Test3.iterator(Test3* x, int y) { }

func void test3()
{
    Test3 x;
    foreach (a : x) { }; // #error: 'iterator()' takes parameters and can't be used for 'foreach'.
}

struct Test4
{}

macro Test4.iterator(Test4* x) { }

func void test4()
{
    Test4 x;
    foreach (a : x) { }; // #error: This type has an iterator without a declared result type, this can't be used with 'foreach'
}

struct Test5
{}

func int Test5.iterator(Test5* x) { return 1; }

func void test5()
{
    Test5 x;
    foreach (a : x) { }; // #error: This type has an implementation of 'iterator()' that doesn't return a struct, so it can't be used with 'foreach'
}

struct Test6
{}

func TestIterator6 Test6.iterator(Test6* x) { return TestIterator6{}; }

struct TestIterator6
{}

func void test6()
{
    Test6 x;
    foreach (a : x) { }; // #error: The iterator 'TestIterator6' is missing a definition for 'next()'
}

struct Test7
{}

func TestIterator7 Test7.iterator(Test7* x) { return TestIterator7{}; }

struct TestIterator7
{}

func bool TestIterator7.next(TestIterator7* x, int a, int b) { return true; }

func void test7()
{
    Test7 x;
    foreach (a : x) { }; // #error: An iterator with a 'next()' that take takes 2 parameters can't be used for 'foreach', it should have 1.
}

