struct Test
{
    int a;
}
fn int main()
{
    bool x = test(Test, );
    return 0;
}
macro test($Type, )
{
    return $defined($Type);
}