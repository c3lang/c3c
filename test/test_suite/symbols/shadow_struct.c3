struct Foo
{
    Foo *x;
    int y;
}

alias Foo = float; // #error: shadow a previous declaration

enum Bar
{
    TEST1,
    TEST2
}

alias Bar = float; // #error: shadow a previous declaration