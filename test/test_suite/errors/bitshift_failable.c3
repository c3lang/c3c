func void test()
{
    int! x = 0;
    int! z = x << 100; // #error: shift exceeds bitsize of 'int'
}