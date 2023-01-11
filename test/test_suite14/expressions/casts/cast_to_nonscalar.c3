struct Struct
{
    int x;
}

fn void test1()
{
    int a = (Struct)(200);    // #error: 'int' to 'Struct'
}
