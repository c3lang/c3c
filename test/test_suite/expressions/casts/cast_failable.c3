faultdef FOO;

fn void test()
{
    int? x = (int?)(FOO~); // #error: Casting to an optional type is not
    int? y = FOO~;
}