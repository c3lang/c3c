macro foo($Foo)
{
    $Foo a;
    return a;
}

fn void test1()
{
    var $Bar;
    foo($Bar); // #error: You need to assign a type to
}

fn void test2()
{
    var $Bar;
    $Bar z; // #error: You need to assign a type to
}
