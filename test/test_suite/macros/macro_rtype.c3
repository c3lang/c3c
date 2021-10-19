macro int frob()
{
    return 0.0; // #error: Expected 'int', not 'double'.
}

func void test1()
{
    @frob();
}