func void test2(ichar a)
{}

func void test1()
{
    test2(100);
    ichar c = 1;
    test2(c);

    int a = 1;
    test2(a);           // #error: Cannot implicitly cast 'int' to 'ichar'.
    test2(100 + a);     // #error: Cannot implicitly cast 'int' to 'ichar'.

    const int X = 120;
    test2(X);           // #error: Cannot implicitly cast 'int' to 'ichar'.

    test2(100 + 100);   // #error: Cannot fit '200' into type 'ichar'.
}

