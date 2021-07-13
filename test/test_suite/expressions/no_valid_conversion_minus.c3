struct Foo
{}

func int main()
{
    int a = 0;
    Foo f;
    f - a; // #error: There is no implicit conversion available for 'Foo' - 'int'
    return 1;
}