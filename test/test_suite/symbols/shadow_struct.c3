struct Foo
{
    Foo *x;
    int y;
}

typedef Foo = float; // #error: shadow a previous declaration

enum Bar
{
    TEST1,
    TEST2
}

typedef Bar = float; // #error: shadow a previous declaration