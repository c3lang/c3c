errtype MyErr
{
    FOO
}

func void test()
{
    int! x = (int!)(MyErr.FOO!); // #error: Casting to a failable type is not allowed
    int! y = MyErr.FOO!;
}