struct Foo
{
    Foo *x;
    int y;
}

define Foo = float; // #error: shadow a previous declaration

enum Bar
{
    TEST1,
    TEST2
}

define Bar = float; // #error: shadow a previous declaration