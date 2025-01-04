module foo;

fn void hidden() @private
{
}

module bar;
import foo;

fn void test()
{
    foo::hidden(); // #error: 'foo::hidden' is '@private'
}

module baz;
import foo @public;

fn void test()
{
    foo::hidden();
}