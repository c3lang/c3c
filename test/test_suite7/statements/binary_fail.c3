fn void test()
{
    float a;
    float b;
    int *c;
    int *d;
    if (a + c) return; // #error: A value of type 'float' cannot be added to 'int*', an integer was expected here
}