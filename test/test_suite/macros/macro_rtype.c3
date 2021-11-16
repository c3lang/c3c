macro int frob()
{
    return 0.0; // #error: Expected 'int', not 'double'.
}

fn void test1()
{
    @frob();
}