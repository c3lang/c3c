fn void main()
{
    int a;
    $extnameof(a); // #error: 'a' does not have an external name.
}
fn void main2()
{
    int a;
    $extnameof($eval("a")); // #error: 'a' does not have an external name.
}

fn void main3()
{
    $extnameof($evaltype("int")); // #error: Only user defined types have an external name.
}

fn void main4()
{
    $extnameof($evaltype("foo::int")); // #error: Built in types cannot have a path prefix.
}
