
fn void test()
{
    $switch int.typeid:
        $case int:
            return;
        $default: // #error: must be last in a $switch
        $default:
            return;
    $endswitch
}

fn void test1()
{
    $switch int.typeid:
        $case int:
            return;
        $case int: // #error: 'int' appears more than once
            return;
        $default:
            return;
    $endswitch
}

alias Foo = int;
alias Bar = double;
fn void test2()
{
    $switch int.typeid:
        $case Foo:
            return;
        $case Bar:
            return;
        $case int: // #error: 'int' appears more than once
            return;
        $default:
            return;
    $endswitch
}

fn void test3()
{
    $switch int.typeid:
        $case int:
            return;
        $case 123: // #error: A type was expected here not 'int'
            return;
        $default:
            return;
    $endswitch
}