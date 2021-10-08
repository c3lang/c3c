macro foo($Foo)
{
    $Foo a;
    return a;
}

func void test1()
{
    var $Bar;
    @foo($Bar); // #error: '$Bar' is not defined yet
}

func void test2()
{
    var $Bar;
    $Bar z; // #error: '$Bar' is not defined yet
}
