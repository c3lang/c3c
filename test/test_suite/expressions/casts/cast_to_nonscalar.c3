struct Struct
{
    int x;
}

func void test1()
{
    int a = cast(200 as Struct);    // #error: Cannot cast 'compint' to 'Struct'
}
