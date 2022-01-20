module test;
struct Foo
{
    int x, y;
}

optenum MyErr
{
    FOO
}

extern fn int printf(char *c, ...);

fn void main()
{
    int! z = 2;
    Foo*! w = &&{ z, 0 }; // #error: A failable 'int[2]*!' cannot be converted to 'Foo*'
}

fn void test()
{
    int! z = 2;
    Foo*! w = &&Foo!{ z, 0 }; // #error: please remove the '!'
}