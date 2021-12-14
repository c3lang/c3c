struct Foo
{ int j; }

fn int main()
{
    int a = 0;
    Foo f;
    f - a; // #error: The subtraction 'Foo' - 'int' is not possible
    return 1;
}