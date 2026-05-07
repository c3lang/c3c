faultdef FOO;

fn void test()
{
    int? x;
    double? y;
    double? df2 = ($Typeof(y!!))(x);
    int? d = ($Typeof(FOO~))(x); // #error: Casting to an optional
}

fn void test2()
{
    int? x;
    double? y;
    int? df = ($Typeof(y))(x); // #error: Casting to an optional type is not allowed
}

