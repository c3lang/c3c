
fn void test()
{
    $switch (3)
        $case 2:
            return;
        $default: // #error: $default must be last in a $switch
        $default:
            return;
    $endswitch
}

fn void test1()
{
    $switch (1)
        $case -1:
            return;
        $case -1: // #error: '-1' appears more than once
            return;
        $default:
            return;
    $endswitch
}


fn void test3()
{
    $switch (3)
        $case 3:
            return;
        $case 123.0: // #error: 'int'
            return;
        $default:
            return;
    $endswitch
}