fault MyErr
{
    FOO
}

fn void test()
{
    int! x;
    double! y;
    int! d = ($typeof(MyErr.FOO?))(x); // #error: optional expression lacks a concrete
    int! df = ($typeof(y))(x); // #error: Casting to an optional type is not allowed
    double! df2 = ($typeof(y!!))(x);
}

