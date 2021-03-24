func void test1()
{
    int a;
    while (1) a++;
    while (1)
    {
      a++;
    }
    while (1)
    a++; // #error: must be placed on the same line
}