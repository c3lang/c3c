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
    $evaltype("int").extnameof; // #error: 'int' does not have a property or method 'extnameof'
}

fn void main4()
{
    $evaltype("foo::int").extnameof; // #error: Only valid types may be resolved with $evaltype.
}
