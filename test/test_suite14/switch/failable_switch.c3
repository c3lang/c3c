fault MyError
{
    FOO
}

fn void test()
{
    int x = 0;
    switch (x)
    {
        case MyError.FOO!: // #error: cannot be converted
            x = x + 1;
    }
}