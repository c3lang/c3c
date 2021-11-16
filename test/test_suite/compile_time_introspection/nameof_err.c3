fn void main()
{
    int a;
    $extnameof(a); // #error: 'a' does not have an external name.
}
fn void main2()
{
    int a;
    $extnameof("a"); // #error: 'a' does not have an external name.
}

fn void main3()
{
    $extnameof("int"); // #error: Only user defined types have an external name.
}
