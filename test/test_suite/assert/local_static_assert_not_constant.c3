int x = 3;

func void test()
{
    $assert(x == 3); // #error: Compile time evaluation requires a compile time constant value.
}

func void test2()
{
    int i = 0;
    $assert(1);
    $assert(i == 0); // #error: Compile time evaluation requires a compile time constant value.
}

func int foo();
func void test3()
{
    int i = 0;
    $assert(foo() == 0); // #error: Compile time evaluation requires a compile time constant value.
}
