macro int frob()
{
    return 0.0; // #error: int
}

fn void test1()
{
    frob();
}