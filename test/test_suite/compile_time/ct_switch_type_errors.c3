
fn void test()
{
    $switch (int.typeid):
        $case int:
            return;
        $default:
        $default: // #error: More than one $default is not allowed
            return;
    $endswitch;
}

fn void test1()
{
    $switch (int.typeid):
        $case int:
            return;
        $case int: // #error: The type 'int' appears more than once
            return;
        $default:
            return;
    $endswitch;
}

define Foo = int;
define Bar = double;
fn void test2()
{
    $switch (int.typeid):
        $case int:
            return;
        $case Bar:
            return;
        $case Foo: // #error: The type 'int' appears more than once
            return;
        $default:
            return;
    $endswitch;
}

fn void test3()
{
    $switch (int.typeid):
        $case int:
            return;
        $case 123: // #error: A type is expected here not 'int'
            return;
        $default:
            return;
    $endswitch;
}