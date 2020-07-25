func void test1(char* cp)
{
    int a = 10;

    char* cp2 = cp - 10;
    char* cp3 = cp + 10;
    cp2 -= 10;
    cp2 -= a;
    cp3 += 10;
    cp3 += a;
    char* cp4 = cp - a;
}