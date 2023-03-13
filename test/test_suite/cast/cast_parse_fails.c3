struct Foo { int a; }

fn void test2()
{
    int x = int{ 32 };    // #error: 'int' cannot use compound literal initialization, did you intend to use a cast
}

fn void test3()
{
    int x = int(32);    // #error: A type cannot be followed by (), if you intended a cast, use '(type) expression'
}


