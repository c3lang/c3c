module test;

fn void test1()
{
    char a = 1;
    int b = 2;
    char c = b > a ? 1 : 0;
}

fn void test2()
{
    char a = 1;
    char b = 2;
    char c = a + b;
}

fn void test3()
{
    ichar a = 1;
    int b = 2;
    ichar c = a + b; // #error: 'int' to 'ichar'
}

fn void test4()
{
    char a = 1;
    char b = 2;
    int c = a + b;
}

fn void test5()
{
    char a = 1;
    int b = 2;
    int c = a + b;
}


fn void test6()
{
    char a = 1;
    char b = 2;
    char c = (b > a) ? 1 : 0 + a + b;
}

fn void test7()
{
    int[100] array = { };
    int v = array[1];
}

define Number = int;

fn void test8()
{
    Number a = 10;
    ichar c = a;         // #error: 'Number' (int) to 'ichar'
}


fn void test9()
{
    const char A = 1;
    char b = A;
    A = b;              // #error: This expression is not assignable, did you make
}

fn void test10()
{
    const char B = 1;
    char* c = &B;
    const A = 1;
    char* b = &A;       // #error: either type it or use && to take the reference to a temporary
}

enum Enum : int
{
    A = 127,
    B,
}

fn void test11()
{
    int a = Enum.A;
    ichar b = Enum.B;     // #error: 'Enum' to 'ichar'
}

fn void test12()
{
    float f = 3.14;
    ichar a = f;         // #error: 'float' to 'ichar'
}

fn void test13()
{
    int a = 1;
    ichar b = a;         // #error: 'int' to 'ichar'
}

fn void test14()
{
    char a = 1;
    ichar b = a;
}

fn void test15()
{
    float f = 3.14;
    ichar c = 1;
    ichar* a = &f;       // #error: 'float*' to 'ichar*'
    ichar* b = &c;
}

fn void test16()
{
    float f = 3.14;
    int i = 1;

    ichar c = 1 ? 'c' : 'd';
    ichar d = 1 ? 'c' : i;
    ichar e = 1 ? i : 0;         // #error: 'int' to 'ichar'
    int g = 1 ? i : f;          // #error: 'float' to 'int'
    int a = f ? 1 : 0;
}

fn void test17()
{
    int a = "test";             // #error: 'char[4]*' into 'int'
}

fn void test18()
{
    char b = 1;
    int a = b;
}

fn void test19()
{
    uint a = 1;
    int b = a;
}

/*
const i32 Num = 200;

func void test1() {
    i8 a = test.Num;  // @error{constant value 200 out-of-bounds for type 'i8', range [-128, 127]}
}*/

fn void test21()
{
    int a = 1;
    uint b = a;
}

fn void foo() {}

fn void test22()
{
    ichar a = foo();             // #error: 'void' into 'ichar'
    short b = foo();            // #error: 'void' into 'short'
    int c = foo();              // #error: 'void' into 'int'
    long d = foo();             // #error: 'void' into 'long'
    char e = foo();             // #error: 'void' into 'char'
    ushort f = foo();           // #error: 'void' into 'ushort'
    uint g = foo();             // #error: 'void' into 'uint'
    ulong h = foo();            // #error: 'void' into 'ulong'
    bool i = foo();             // #error: 'void' into 'bool'
}


int num = 10;

fn void test23()
{
    int a = num;
    int b = test::num;
    char c = test::num;         // #error: 'int' to 'char'
}

int[2][3] b123;

fn void test24()
{
    int a = b123;  // #error: 'int[2][3]' into 'int'
}

fn void test25()
{
    const A = void; // #error: Constants cannot be undefined.
}

fn void test26()
{
    const int A = void; // #error: Constants cannot be undefined.
}



