errtype MyError
{
    FOO
}

fn void test()
{
    int x = 0;
    switch (x)
    {
        case MyError.FOO!: // #error: You cannot have a failable here
            x = x + 1;
    }
}