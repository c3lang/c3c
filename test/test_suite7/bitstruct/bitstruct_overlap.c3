bitstruct Foo1 : char
{
    int a : 2..5;
    int b : 5..6; // #error: Overlapping members
}

bitstruct Foo2 : char
{
    int a : 2..5;
    int b : 4..6; // #error: Overlapping members
}

bitstruct Foo3 : char
{
    int a : 2..5;
    int b : 2..6; // #error: Overlapping members
}

bitstruct Foo4 : char
{
    int a : 2..5;
    int b : 1..6; // #error: Overlapping members
}

bitstruct Foo5 : char
{
    int a : 2..5;
    int b : 1..3; // #error: Overlapping members
}

bitstruct Foo6 : char
{
    int a : 2..5;
    int b : 1..1;
}

bitstruct Foo7 : char @overlap
{
    int a : 2..5;
    int b : 1..3;
}

bitstruct Foo8 : char
{
    int a : 2..5;
    bool b : 3; // #error: Overlapping members
}
