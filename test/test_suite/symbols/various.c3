module test;

func void test1()
{
    char a = 1;
    int b = 2;
    char c = b > a ? 1 : 0;
}

func void test2()
{
    char a = 1;
    char b = 2;
    char c = a + b;
}

func void test3()
{
    ichar a = 1;
    int b = 2;
    ichar c = a + b; // #error: 'int' to 'ichar'
}

func void test4()
{
    char a = 1;
    char b = 2;
    int c = a + b;
}

func void test5()
{
    char a = 1;
    int b = 2;
    int c = a + b;
}


func void test6()
{
    char a = 1;
    char b = 2;
    char c = (b > a) ? 1 : 0 + a + b;
}

func void test7()
{
    int[100] array = { };
    int v = array[1];
}

define Number = int;

func void test8()
{
    Number a = 10;
    ichar c = a;         // #error: 'Number' (int) to 'ichar'
}


func void test9()
{
    const char A = 1;
    char b = A;
    A = b;              // #error: This expression is not assignable, did you make
}

func void test10()
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

func void test11()
{
    int a = Enum.A;
    ichar b = Enum.B;     // #error: 'Enum' to 'ichar'
}

func void test12()
{
    float f = 3.14;
    ichar a = f;         // #error: 'float' to 'ichar'
}

func void test13()
{
    int a = 1;
    ichar b = a;         // #error: 'int' to 'ichar'
}

func void test14()
{
    char a = 1;
    ichar b = a;
}

func void test15()
{
    float f = 3.14;
    ichar c = 1;
    ichar* a = &f;       // #error: 'float*' to 'ichar*'
    ichar* b = &c;
}

func void test16()
{
    float f = 3.14;
    int i = 1;

    ichar c = 1 ? 'c' : 'd';
    ichar d = 1 ? 'c' : i;       // #error: 'int' to 'ichar'
    ichar e = 1 ? i : 0;         // #error: 'int' to 'ichar'
    int g = 1 ? i : f;          // #error: 'float' to 'int'
    int a = f ? 1 : 0;
}

func void test17()
{
    int a = "test";             // #error: 'string literal' into 'int'
}

func void test18()
{
    char b = 1;
    int a = b;
}

func void test19()
{
    uint a = 1;
    int b = a;
}

/*
const i32 Num = 200;

func void test1() {
    i8 a = test.Num;  // @error{constant value 200 out-of-bounds for type 'i8', range [-128, 127]}
}*/

func void test21()
{
    int a = 1;
    uint b = a;
}

func void foo() {}

func void test22()
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

func void test23()
{
    int a = num;
    int b = test::num;
    char c = test::num;         // #error: 'int' to 'char'
}

int[2][3] b123;

func void test24()
{
    int a = b123;  // #error: 'int[2][3]' into 'int'
}

func void test25()
{
    const A = void; // #error: Constants cannot be undefined.
}

func void test26()
{
    const int A = void; // #error: Constants cannot be undefined.
}



