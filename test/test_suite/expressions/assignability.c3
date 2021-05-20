define Number = int;

func void test1()
{
    10 = 20;        // #error: Expression is not assignable.
}

func void test2()
{
    "foo" = "bar";  // #error: Expression is not assignable.
}

func void test3()
{
    true = false;   // #error: Expression is not assignable.
}

func void test4()
{
    'c' = 'd';      // #error: Expression is not assignable.
}

func void test5()
{
    3.14 = 2.14;    // #error: Expression is not assignable.
}

func void test21()
{
    int a = 0;
    int b = 2;
    a++ = b++;      // #error: Expression is not assignable
}
