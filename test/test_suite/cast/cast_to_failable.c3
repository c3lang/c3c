faultdef FOO;

fn void test()
{
    int? x;
    double? y;
    int? d = ($typeof(FOO~))(x); // #error: Casting to an optional
    int? df = ($typeof(y))(x); // #error: Casting to an optional type is not allowed
    double? df2 = ($typeof(y!!))(x);
}

