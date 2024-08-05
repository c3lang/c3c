extern fn int printf(char* fmt, ...);

extern fn int! err();
fault FooErr
{
    X
}
fn void test1()
{
    int! z = 234;
    if (try z)
    {
        int y = z;
        z = 12;
        z = FooErr.X?; // #error: The variable is unwrapped in this context
    }
}


fn void test4()
{
    int! z = 234;
    int! w = 123;
    if (try z && try w && 1)
    {
        int y = z;
        y = w;
    }
}

fn void test5()
{
    int! z = 234;
    int! w = 123;
    if (try z && try w)
    {
        int y = z;
    }
}
fn void test5b()
{
    int! z = 234;
    int! w = 123;
    if (try z && try z) // #error: This variable is already unwrapped, so you cannot use 'try' on it again, please remove the 'try'.
    {
        int y = z;
    }
}

fn void test6()
{
    int! z = 234;
    int! w = 123;
    while (try z)
    {
        int y = z;
        break;
    }
}

fn void test7()
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

fn void test8()
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
fn void test9()
{
    int! z = 234;
    int! w = 123;
    if (catch e = z)
    {
    }
    else
    {
        int y = z;
    }
}
