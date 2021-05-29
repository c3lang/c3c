define Number = int;

func void test1()
{
    int a = 10;

    int b = (Number)(a);

    int c = (Foo)(a);         // #error: type 'Foo' could not be found
}

func void test2()
{
    int d = (Number)(bar);;    // #error: identifier 'bar' could not be found
}

func void test3()
{
    int e = (Bar)( // #error: type 'Bar' could not be found
        faa);             // #error: identifier 'faa' could not be found
}
