define Number = int;

func void test1()
{
    int a = 10;

    int b = (Number)(a);

    int c = (Foo)(a);         // #error: Unknown type 'Foo'.
}

func void test2()
{
    int d = (Number)(bar);;    // #error: Identifier 'bar' could not be found.
}

func void test3()
{
    int e = (Bar)( // #error: Unknown type 'Bar'.
        faa);             // #error: Identifier 'faa' could not be found.
}
