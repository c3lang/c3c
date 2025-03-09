fn void test1()
{
    bool x = $defined(1);
}

struct Foo
{
    int x;
}

fn void test2()
{
    $assert $defined(int[1]);
    bool x = $defined(int[y]); // #error: 'y' could not be found, did you spell it right?
}

fn void test3()
{
    $assert $defined(Foo[1]);
    $assert $defined(Foo*);
    $assert $defined(Foo[]);
    $assert $defined(Foo[*]);
    bool x = $defined(Foo[y]); // #error: 'y' could not be found, did you spell it right?
}

