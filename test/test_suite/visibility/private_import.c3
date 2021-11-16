module foo;

private fn void hidden()
{
}

module bar;
import foo;

fn void test()
{
    foo::hidden(); // #error: The function 'hidden' is not visible from this module.
}

module baz;
import private foo;

fn void test()
{
    foo::hidden();
}