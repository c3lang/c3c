func void test1()
{
    int a = (i ? 1 : 1); // #error: Identifier 'i' could not be found.
}

func void test2()
{
    int a = (1 ? i : 2); // #error: Identifier 'i' could not be found.
}

func void test3()
{
    int a = (1 ? 2 : i); // #error: Identifier 'i' could not be found.
}
