fn void test1()
{
    short! a = 1;
    try? -a;
    short b = -a; // #error: 'int!' to 'short'.
}

fn void test2()
{
    int! a = 1;
    try? -a;
    int b = -a; // #error: 'int!' to 'int'
}

fn void test3()
{
    long! a = 1;
    try? -a;
    long b = -a; // #error: 'long!' to 'long'
}

fn void test4()
{
    short! a = 1;
    try? ~a;
    short b = ~a; // #error: 'short!' to 'short'
}

fn void test5()
{
    int! a = 1;
    try? ~a;
    int b = ~a; // #error: 'int!' to 'int'
}

fn void test6()
{
    long! a = 1;
    try? ~a;
    long b = ~a; // #error: 'long!' to 'long'
}
