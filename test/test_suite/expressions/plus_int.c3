fn void test1()
{
    short! a = 1;
    @ok(+a);
    short b = +a; // #error: 'short!' to 'short'.
}

fn void test2()
{
    int! a = 1;
    @ok(+a);
    int b = +a; // #error: 'int!' to 'int'
}

fn void test3()
{
    long! a = 1;
    @ok(+a);
    long b = +a; // #error: 'long!' to 'long'
}
