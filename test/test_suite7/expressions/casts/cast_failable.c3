fault MyErr
{
    FOO
}

fn void test()
{
    int! x = (int!)(MyErr.FOO?); // #error: Casting to an optional type is not
    int! y = MyErr.FOO?;
}