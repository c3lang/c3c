
macro void m(&x) // #error: Self parameters are only allowed on methods
{
    $echo $typeof(x);
}

fn void main()
{
    int x;
    m(x);
}