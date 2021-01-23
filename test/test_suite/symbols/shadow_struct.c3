struct Foo
{
    Foo *x;
    int y;
}

typedef float as Foo; // #error: shadow a previous declaration

enum Bar
{
    TEST1,
    TEST2
}

typedef float as Bar; // #error: shadow a previous declaration