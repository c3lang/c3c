
alias Func = fn int(int);

alias Func2 = fn int(Foo*, int);

struct Foo
{
    int a;
    Func callback;
}

fn int Foo.func2(Foo* f, int i)
{
    return f.a + i;
}

fn void test_unknown_member()
{
   int a = Foo.b;               // #error: No method or inner struct/union 'Foo.b' found.
}


fn void test_nonstatic_struct_func1()
{
    Func2 a = &Foo.func2;
}

fn void test_nonstatic_struct_func2()
{
    int b = Foo.func2(null, 2);
}
