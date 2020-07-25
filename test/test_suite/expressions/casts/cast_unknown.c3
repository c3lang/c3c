typedef int as Number;

func void test1()
{
    int a = 10;

    int b = cast(a as Number);

    int c = cast(a as Foo);         // #error: Unknown type 'Foo'.
}

func void test2()
{
    int d = cast(bar as Number);;    // #error: Identifier 'bar' could not be found.
}

func void test3()
{
    int e = cast(faa as             // #error: Identifier 'faa' could not be found.
        Bar);                       // #error: Unknown type 'Bar'.
}
