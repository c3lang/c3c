extern func int printf(char* fmt, ...);

extern func int! err();
errtype FooErr
{
    X
}
func void test1()
{
    int! z = 234;
    if (try z)
    {
        int y = z;
        z = 12;
        z = FooErr.X!; // #error: The variable is unwrapped in this context
    }
}


func void test4()
{
    int! z = 234;
    int! w = 123;
    if (try z && try w && 1)
    {
        int y = z;
        y = w;
    }
}

func void test5()
{
    int! z = 234;
    int! w = 123;
    if (try z && try w)
    {
        int y = z;
    }
}
func void test5b()
{
    int! z = 234;
    int! w = 123;
    if (try z && try z) // #error: This variable is already unwrapped, so you cannot use 'try' on it again, please remove the 'try'.
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
    if (catch z)
    {
    }
    else
    {
        int y = z;
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
