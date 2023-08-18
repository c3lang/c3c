int[8][8] a;
fn void main()
{
   a[2..3];
    a[2:1];
    a[..3];
    a[2..];
    a[..];
    a[2:1];
    a[:4];
}

fn void test1()
{
    a[:]; // #error: not elide
}

fn void test2()
{
    a[2:]; // #error: not elide
}