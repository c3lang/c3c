faultdef FOO;

fn void test()
{
    int? x;
    double? y;
    double? df2 = ($typeof(y!!))(x);
    int? d = ($typeof(FOO~))(x); // #error: Casting to an optional
}

fn void test2()
{
    int? x;
    double? y;
    int? df = ($typeof(y))(x); // #error: Casting to an optional type is not allowed
}

