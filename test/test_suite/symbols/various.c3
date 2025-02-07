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
    ichar c = a + b; // #error: 'ichar'
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

def Number = int;

fn void test8()
{
    Number a = 10;
    ichar c = a;         // #error: 'Number' (int)
}


fn void test9()
{
    const char A = 1;
    char b = A;
    A = b;              // #error: You cannot assign to a constant
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
    A,
    B,
}

fn void test11()
{
    int a = Enum.A; // #error: It is not possible to cast
    ichar b = Enum.B; // #error: It is not possible to cast
}

fn void test12()
{
    float f = 3.14;
    ichar a = f;         // #error: 'ichar'
}

fn void test13()
{
    int a = 1;
    ichar b = a;         // #error: 'ichar'
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
    ichar e = 1 ? i : 0;         // #error: 'ichar'
    int g = 1 ? i : f;          // #error: 'float'
    int a = f ? 1 : 0;
}

fn void test17()
{
    int a = "test";             // #error: 'String' to 'int'
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

fn void test1() {
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
    ichar a = foo();             // #error: 'void' to 'ichar'
    short b = foo();            // #error: 'void' to 'short'
    int c = foo();              // #error: 'void' to 'int'
    long d = foo();             // #error: 'void' to 'long'
    char e = foo();             // #error: 'void' to 'char'
    ushort f = foo();           // #error: 'void' to 'ushort'
    uint g = foo();             // #error: 'void' to 'uint'
    ulong h = foo();            // #error: 'void' to 'ulong'
    bool i = foo();             // #error: 'void' to 'bool'
}


int num = 10;

fn void test23()
{
    int a = num;
    int b = test::num;
    char c = test::num;         // #error: 'char'
}

int[2][3] b123;

fn void test24()
{
    int a = b123;  // #error: 'int[2][3]' to 'int'
}



