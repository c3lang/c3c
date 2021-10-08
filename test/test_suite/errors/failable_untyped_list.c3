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
    Foo*! w = &&{ z, 0 }; // #error: casting 'int[2]*' to 'Foo*' is not permitted
}

func void test()
{
    int! z = 2;
    Foo*! w = &&Foo!{ z, 0 }; // #error: please remove the '!'
}