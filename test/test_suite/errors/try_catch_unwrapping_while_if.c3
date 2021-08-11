
extern func int printf(char* fmt, ...);

extern func int! err();
errtype FooErr { int x; }
func void test1()
{
    int! z = 234;
    if (try z)
    {
        int y = z;
        z = 12;
        z = FooErr({1})!; // #error: The variable is unwrapped in this context
    }
}

func void test2()
{
    int! z = 234;
    if (try z || 1)
    {
        z = 12;
        z = FooErr({1})!;
    }
}
func void test3()
{
    int! z = 234;
    int! w = 123;
    if (try z || try w)
    {
        int y = z; // #error: 'int!' cannot be converted into 'int'
        y = w;
    }
}

func void test4()
{
    int! z = 234;
    int! w = 123;
    if (try z && ((try w) && 1))
    {
        int y = z;
        y = w;
    }
}

func void test5()
{
    int! z = 234;
    int! w = 123;
    if (try z && try z)
    {
        int y = z;
    }
}

func void test6()
{
    int! z = 234;
    int! w = 123;
    while (try z)
    {
        int y = z;
        break;
    }
}

func void test7()
{
    int! z = 234;
    int! w = 123;
    if (catch z)
    {
    }
    else
    {
        int y = z;
    }
}

func void test8()
{
    int! z = 234;
    int! w = 123;
    if (catch z && 1)
    {
    }
    else
    {
        int y = z; // #error: 'int!' cannot be converted into 'int'
    }
}
func void test9()
{
    int! z = 234;
    int! w = 123;
    anyerr e;
    if (catch e = z)
    {
    }
    else
    {
        int y = z;
    }
}
