fn void test()
{
    int f;
    int* x = &(((f)));
    int* h = &&(f++);
    int* z = &(f++); // #error: To take the address of a temporary value, use '&&' instead of '&'
}


fn void test2()
{
    int f;
    var $foo;
    int* x = &$foo;    // #error: It's not possible to take the address of a compile time value
}

int he;
macro int hello()
{
    return he;
}

fn void test3()
{
    int* x = &hello();    // #error: To take the address of a temporary value, use '&&' instead of '&'
}

fn void test3b()
{
    int* x = &hello;    // #error: It is not possible to take the address of a macro.
}

const X = 2;
const int XX = 3;
fn void test4()
{
    int* w = &XX;
}

fn void test5()
{
    int* z = &X;    // #error: The constant is not typed, either type it or use && to take the reference to a temporary.
}

struct Foo
{
    int x;
    int y;
}

alias heh = he;

fn void test6()
{
    int* hee = &heh;
    Foo h;
    int* z = &h.x;
    int[3] arr;
    int* d = &arr[2];
    int[]* e = &arr[1..2]; // #error: To take the address of a temporary value, use '&&' instead of '&'
}

alias Baz = Foo;
distinct Bar = int;
fault FOO;
union Un { int x; }
enum MyEnum { BAR }

fn void test7()
{
    &Baz; // #error: It is not possible to take the address of a type.
    &Bar; // #error: It is not possible to take the address of a type.
    &Un; // #error: It is not possible to take the address of a type.
    &FOO; // #error: To take the address of a temporary value, use '&&'
    &MyEnum; // #error: It is not possible to take the address of a type.
    &MyEnum.BAR; // #error: To take the address of a temporary value, use '&&' instead of '&'
}
