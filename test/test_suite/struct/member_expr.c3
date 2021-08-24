
define Func = func int(int);

define Func2 = func int(Foo*, int);

struct Foo
{
    int a;
    Func callback;
}

func int Foo.func2(Foo* f, int i)
{
    return f.a + i;
}

func void test_unknown_member()
{
   int a = Foo.b;               // #error: No method or inner struct/union 'Foo.b' found.
}


func void test_nonstatic_stuct_func1()
{
    Func2 a = &Foo.func2;
}

func void test_nonstatic_stuct_func2()
{
    int b = Foo.func2(null, 2);
}
