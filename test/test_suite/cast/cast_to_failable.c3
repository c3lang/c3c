fault MyErr
{
    FOO
}

fn void test()
{
    int! x;
    int! d = ($typeof(MyErr.FOO!))(x); // #error: Casting to an optional type is not allowed
}


