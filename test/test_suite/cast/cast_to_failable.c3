optenum MyErr
{
    FOO
}

fn void test()
{
    int! x;
    int! d = ($typeof(MyErr.FOO!))(x); // #error: Casting to a failable type is not allowed
}


