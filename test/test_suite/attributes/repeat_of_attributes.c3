fn void test()
{
    for (int a @align(16) @align(16) = 0 ; a < 10; a++) // #error: Repeat of attribute '@align'
    {
    }
}
