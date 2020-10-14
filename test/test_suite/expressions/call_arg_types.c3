func void test2(char a)
{}

func void test1()
{
    test2(100);
    char c = 1;
    test2(c);

    int a = 1;
    test2(a);           // #error: Cannot implicitly cast 'int' to 'char'.
    test2(100 + a);     // #error: Cannot implicitly cast 'int' to 'char'.

    const int X = 120;
    test2(X);           // #error: Cannot implicitly cast 'int' to 'char'.

    test2(100 + 100);   // #error: Cannot fit '200' into type 'char'.
}

