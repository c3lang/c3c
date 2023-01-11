
bitstruct Test : int
{
    bool x : 0..0;
    float a : 1..3; // #error: 'float' is not supported in a bitstruct, only enums, integer and boolean values may be used.
}

define Baz = distinct float;
define Foo = distinct bool;
enum Boo
{
    BAR
}
bitstruct Test2 : int
{
    Foo x : 0..0;
    Boo y : 4..6;
    Baz a : 1..3; // #error: 'Baz' is not supported in a bitstruct, only enums, integer and boolean values may be used.
}


bitstruct Test3 : int
{
    Foo x : 0..0;
    Brob a : 1..3; // #error: 'Brob' could not be found, did you spell it right?
}
