func void test1()
{
    int a = (i ? 1 : 1); // #error: 'i' could not be found, did you spell it right
}

func void test2()
{
    int a = (1 ? i : 2); // #error: 'i' could not be found, did you spell it right
}

func void test3()
{
    int a = (1 ? 2 : i); // #error: 'i' could not be found, did you spell it right
}
