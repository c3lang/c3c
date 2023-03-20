
fn void test()
{
    $switch (int.typeid)
        $case int:
            return;
        $default:
        $default: // #error: More than one $default is not allowed
            return;
    $endswitch
}

fn void test1()
{
    $switch (int.typeid)
        $case int:
            return;
        $case int: // #error: 'int' appears more than once
            return;
        $default:
            return;
    $endswitch
}

typedef Foo = int;
typedef Bar = double;
fn void test2()
{
    $switch (int.typeid)
        $case int:
            return;
        $case Bar:
            return;
        $case Foo: // #error: 'int' appears more than once
            return;
        $default:
            return;
    $endswitch
}

fn void test3()
{
    $switch (int.typeid)
        $case int:
            return;
        $case 123: // #error: A type was expected here not 'int'
            return;
        $default:
            return;
    $endswitch
}