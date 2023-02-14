typedef Number = int;

fn void test1()
{
    int a = 10;

    int b = (Number)(a);

    int c = (Foo)(a);         // #error: 'Foo' could not be found
}

fn void test2()
{
    int d = (Number)(bar);;    // #error: 'bar' could not be found
}

fn void test3()
{
    int e = (Bar)( // #error: 'Bar' could not be found
        faa);             // #error:  'faa' could not be found
}
