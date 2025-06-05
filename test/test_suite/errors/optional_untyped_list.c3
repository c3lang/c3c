module test;
struct Foo
{
    int x, y;
}

faultdef FOO;

extern fn int printf(char *c, ...);

fn void foo()
{
    int z = 2;
    ushort* y = &&4;
    Foo*? w2 = &&{ z, 0 };
    void*? w = &&{ z, 0 }; // #error: An untyped list can only have constant elements
}

fn void foo2()
{
    int? z = 2;
    void*? w = &&{ z, 0 }; // #error: An untyped list can only have constant elements
}

fn void test()
{
    int? z = 2;
    Foo*? w = &&(Foo?){ z, 0 }; // #error: please remove the '?'
}