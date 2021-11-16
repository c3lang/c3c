module test;

extern fn int printf(char *, ...);

macro foo(x; @body(y))
{
    @body(x);
}

fn void test()
{
    @foo(10); // #error: Expected call to have a trailing statement, did you forget to add it?
}

fn void test3()
{
    @foo(10) // #error: Not enough parameters
    {
    };
}

fn void test5()
{
    @foo(10; int a, int b) // #error: Too many parameters for the macro body, expected
    {
    };
}
macro foo_no(x)
{
    return x;
}

fn void test2()
{
    @foo_no(10) // #error: This macro does not support trailing statements
    {
        printf("foek");
    };
}

macro foo2(x)
{
    @body(x); // #error: 'body' could not be found, did you spell it right?
}

fn void test4()
{
    @foo2(10);
}

