fn void main()
{
    int a;
    $cnameof(a); // #error: 'a' does not have an external name.
}
fn void main2()
{
    int a;
    $cnameof($eval("a")); // #error: 'a' does not have an external name.
}

fn void main3()
{
    $typefrom("int").cnameof; // #error: 'int' does not have a property or method 'cnameof'
}

fn void main4()
{
    $typefrom("foo::int").cnameof; // #error: Unexpected path in
}
