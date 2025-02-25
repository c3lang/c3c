struct Foo
{
    Foo *x;
    int y;
}

def Foo = float; // #error: shadow a previous declaration

enum Bar
{
    TEST1,
    TEST2
}

def Bar = float; // #error: shadow a previous declaration