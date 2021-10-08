struct Struct
{
    int x;
}

func void test1()
{
    int a = (Struct)(200);    // #error: Cannot cast 'int' to 'Struct'
}
