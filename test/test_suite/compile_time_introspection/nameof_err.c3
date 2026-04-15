fn void main()
{
    int a;
    $reflect(a).cname(a); // #error: There is no property 'cname' available for the reflected expression
}
fn void main2()
{
    int a;
    $reflect($eval("a")).cname; // #error: There is no property 'cname' available for the reflected expression
}

fn void main3()
{
    $typefrom("int")::cname; // #error: 'cname' is not a valid property for 'int'
}

fn void main4()
{
    $typefrom("foo::int")::cname; // #error: Unexpected path in
}
