module foo;

private func void hidden()
{
}

module bar;
import foo;

func void test()
{
    foo::hidden(); // #error: The function 'hidden' is not visible from this module.
}

module baz;
import private foo;

func void test()
{
    foo::hidden();
}