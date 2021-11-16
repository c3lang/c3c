fn void test2(ichar a)
{}

fn void test1()
{
    test2(100);
    ichar c = 1;
    test2(c);

    int a = 1;
    test2(a);           // #error: 'int' to 'ichar'
    test2(100 + a);     // #error: 'int' to 'ichar'

    const int X = 120;
    test2(X);           // #error: 'int' to 'ichar'

    test2(100 + 100);   // #error: '200' is out of range for 'ichar'
}

