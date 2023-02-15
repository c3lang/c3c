module foo;

fn void hidden() @private
{
}

module bar;
import foo;

fn void test()
{
    foo::hidden(); // #error: The function 'foo::hidden' is not visible from this module.
}

module baz;
import foo @public;

fn void test()
{
    foo::hidden();
}