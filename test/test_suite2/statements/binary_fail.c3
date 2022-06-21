fn void test()
{
    float a;
    float b;
    int *c;
    int *d;
    if ((a + c) // #error: A value of type 'float' cannot be added to 'int*', an integer was expected here
      + (b + d)) return; // #error: A value of type 'float' cannot be added to 'int*', an integer was expected here
}