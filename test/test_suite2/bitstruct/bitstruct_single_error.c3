bitstruct Foo1 : char
{
    char x : 1..1;
}

bitstruct Foo2 : char
{
    char x : 1; // #error: Only booleans may use non-range indices
}

bitstruct Foo3 : char
{
    bool x : 0..2; // #error: The bit width of 'bool'
}

bitstruct Foo4 : int
{
    char x : 0..15; // #error: The bit width of 'char'
}