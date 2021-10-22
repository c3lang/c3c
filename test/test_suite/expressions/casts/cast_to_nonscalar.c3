struct Struct
{
    int x;
}

func void test1()
{
    int a = (Struct)(200);    // #error: 'int' to 'Struct'
}
