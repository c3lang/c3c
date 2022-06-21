macro int frob()
{
    return 0.0; // #error: Implicitly casting 'double' to 'int' is not permitted
}

fn void test1()
{
    frob();
}