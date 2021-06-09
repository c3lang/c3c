module test;

extern func int printf(char *, ...);

macro foo(x; y)
{
    yield x;
}

func void test()
{
    @foo(10); // #error: Expected call to have a trailing statement, did you forget to add it?
}

func void test3()
{
    @foo(10) // #error: Not enough parameters
    {
    };
}

func void test5()
{
    @foo(10; int a, int b) // #error: Too many parameters for the macro body, expected
    {
    };
}
macro foo_no(x)
{
    return x;
}

func void test2()
{
    @foo_no(10) // #error: This macro does not support trailing statements
    {
        printf("foek");
    };
}

macro foo2(x)
{
    yield x; // #error: 'yield' can only be used in macros that takes trailing bodies, use ';' after the regular parameters.
}

func void test4()
{
    @foo2(10);
}