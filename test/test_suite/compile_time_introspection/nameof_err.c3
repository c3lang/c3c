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
    $typefrom("int").extnameof; // #error: 'int' does not have a property or method 'extnameof'
}

fn void main4()
{
    $typefrom("foo::int").extnameof; // #error: Unexpected path in
}
