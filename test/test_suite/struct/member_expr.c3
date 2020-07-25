
typedef func int(int) as Func;

typedef func int(Foo*, int) as Func2;

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
   int a = Foo.b;               // #error: No function or member 'Foo.b' found.
}

/*
func void test_regular_member1()
{
   int a = Foo.a.sizeof;
   int b = Foo.a;              // @error{member 'a' cannot be used without instance}

}

func void test_regular_member2()
{
    int a = Foo.a();            // @error{member 'a' cannot be used without instance}
}

func void test_function_member1()
{
    Func a = Foo.callback;          // @error{member 'callback' cannot be used without instance}
}

func void test_function_member2()
{
    int a = Foo.callback.sizeof;
    int b = Foo.callback(1);     // @error{member 'callback' cannot be used without instance}
}

func void test_static_struct_func()
{
    Func a = Foo.func1;
    int b = Foo.func1(1);
    int c = Foo.func1.sizeof;
}

func void test_nonstatic_stuct_func1()
{
    Func2 a = Foo.func2;
}

func void test_nonstatic_stuct_func2()
{
    int a = Foo.func2.sizeof;
    int b = Foo.func2(nil, 2);
}

*/