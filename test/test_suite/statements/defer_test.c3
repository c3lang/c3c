fn void test()
{
    int i = 0;
    defer
    {
        i = i + 1;
        break;
    }
    return;
}