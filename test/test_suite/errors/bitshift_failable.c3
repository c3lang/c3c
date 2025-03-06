fn void test()
{
    int? x = 0;
    int? z = x << 100; // #error: the bitsize of 'int'
}