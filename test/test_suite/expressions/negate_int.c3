func void test1()
{
    short! a = 1;
    try(-a);
    short b = -a; // #error: 'int!' cannot be converted to 'short'
}

func void test2()
{
    int! a = 1;
    try(-a);
    int b = -a; // #error: 'int!' to 'int'
}

func void test3()
{
    long! a = 1;
    try(-a);
    long b = -a; // #error: 'long!' to 'long'
}

func void test4()
{
    short! a = 1;
    try(~a);
    short b = ~a; // #error: 'short!' to 'short'
}

func void test5()
{
    int! a = 1;
    try(~a);
    int b = ~a; // #error: 'int!' to 'int'
}

func void test6()
{
    long! a = 1;
    try(~a);
    long b = ~a; // #error: 'long!' to 'long'
}
