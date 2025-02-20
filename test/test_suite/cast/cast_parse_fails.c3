struct Foo { int a; }

fn void test2()
{
    int x = (int){ 32 };    // #error: 'int' with a non-empty initializer list
}

fn void test3()
{
    int x = int(32);    // #error: A type cannot be followed by (), if you intended a cast, use '(type) expression'
}


