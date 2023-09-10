fault MyError
{
    FOO
}

fn void test()
{
    int x = 0;
    switch (x)
    {
        case MyError.FOO? : // #error: 'int!' to 'int'
            x = x + 1;
    }
}