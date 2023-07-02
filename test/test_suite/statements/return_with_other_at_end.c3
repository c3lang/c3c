fn int testReturnWithOtherAtEnd()
{
    int i = 0;
    return i;
    if (i == 10) i++; // #error: This code will
    i = i + 2;
}
