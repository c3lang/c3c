fn void test1()
{
    int x;
    foreach (a : x) { }; // #error: It's not possible to enumerate an expression of type 'int'
}

distinct Test1 = int;

fn void test2()
{
    Test1 x;
    foreach (a : x) { }; // #error: It's not possible to enumerate an expression of type 'Test1'
}


