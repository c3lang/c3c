module test;
struct Foo
{
    int x, y;
}

errtype MyErr
{
    FOO
}

extern func int printf(char *c, ...);

func void main()
{
    int! z = 2;
    Foo*! w = &&{ z, 0 }; // #error: A failable 'int[2]*!' cannot be converted to 'Foo*'
}

func void test()
{
    int! z = 2;
    Foo*! w = &&Foo!{ z, 0 }; // #error: please remove the '!'
}