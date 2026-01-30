macro test(x)
{
}
fn int main()
{
    test($typeof({})?); // #error: You cannot take the typeid of a compile time type
    return 0;
}