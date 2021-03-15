func void test1(ichar* cp)
{
    int a = 10;

    ichar* cp2 = cp - 10;
    ichar* cp3 = cp + 10;
    cp2 -= 10;
    cp2 -= a;
    cp3 += 10;
    cp3 += a;
    ichar* cp4 = cp - a;
    cp2 - cp3;
}
func void test2(ichar* cp)
{
    cp + 1;
    cp * 1.0; // #error: Cannot multiply 'ichar*' by 'compfloat'
}