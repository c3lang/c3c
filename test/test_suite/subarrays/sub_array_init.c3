func void test2()
{
    int[2] a = { 1, 2 };

    int[2] b = 30;   // #error: 'compint' into 'int[2]'
    int[2] c = a;
}
