module private foo;

func void hidden()
{
}

module bar;
import foo; // #error: Importing a private module is not allowed (unless 'import private' is used).


module baz;
import private foo;

func void test()
{
    foo::hidden();
}