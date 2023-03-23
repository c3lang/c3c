int x = 3;

fn void test()
{
    $assert(x == 3); // #error: Compile time evaluation requires a compile time constant value.
}

fn void test2()
{
    int i = 0;
    $assert(1);
    $assert(i == 0); // #error: Compile time evaluation requires a compile time constant value.
}

extern fn int foo();
fn void test3()
{
    int i = 0;
    $assert(foo() == 0); // #error: Compile time evaluation requires a compile time constant value.
}
