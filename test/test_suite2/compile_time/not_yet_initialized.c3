macro foo($Foo)
{
    $Foo a;
    return a;
}

fn void test1()
{
    var $Bar;
    foo($Bar); // #error: '$Bar' is not defined yet
}

fn void test2()
{
    var $Bar;
    $Bar z; // #error: '$Bar' is not defined yet
}
