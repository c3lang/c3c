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
    char a = 1;
    int b = 2;
    char c = a + b; // #error: Cannot implicitly cast 'int' to 'char'
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

typedef int as Number;

func void test8()
{
    Number a = 10;
    char c = a;         // #error: implicitly cast 'Number' (int) to 'char'
}


func void test9()
{
    const char A = 1;
    char b = A;
    A = b;              // #error: Expression is not assignable
}

func void test10()
{
    const char B = 1;
    char* c = &B;
    const A = 1;
    char* b = &A;       // #error: To take the address of a temporary value, use '&&' instead of '&'
}

enum Enum : int
{
    A = 127,
    B,
}

func void test11()
{
    int a = Enum.A;
    char b = Enum.B;     // #error: Cannot implicitly convert 'Enum' with underlying type of 'int' to 'char'
}

func void test12()
{
    float f = 3.14;
    char a = f;         // #error: cast 'float' to 'char'
}

func void test13()
{
    int a = 1;
    char b = a;         // #error: cast 'int' to 'char'
}

func void test14()
{
    byte a = 1;
    char b = a;         // #error: cast 'byte' to 'char'
}

func void test15()
{
    float f = 3.14;
    char c = 1;
    char* a = &f;       // #error: cast 'float*' to 'char*'
    char* b = &c;
}

func void test16()
{
    float f = 3.14;
    int i = 1;

    char c = 1 ? 'c' : 'd';
    char d = 1 ? 'c' : i;       // #error: cast 'int' to 'char'
    char e = 1 ? i : 0;         // #error: cast 'int' to 'char'
    int g = 1 ? i : f;          // #error: cast 'float' to 'int'
    int a = f ? 1 : 0;
}

func void test17()
{
    int a = "test";             // #error: cast 'string' to 'int'
}

func void test18()
{
    char b = 1;
    int a = b;
}

func void test19()
{
    uint a = 1;
    int b = a;                  // #error: cast 'uint' to 'int'
}

/*
const i32 Num = 200;

func void test1() {
    i8 a = test.Num;  // @error{constant value 200 out-of-bounds for type 'i8', range [-128, 127]}
}*/

func void test21()
{
    int a = 1;
    uint b = a;                 // #error: cast 'int' to 'uint'
}

func void foo() {}

func void test22()
{
    char a = foo();             // #error: cast 'void' to 'char'
    short b = foo();            // #error: cast 'void' to 'short'
    int c = foo();              // #error: cast 'void' to 'int'
    long d = foo();             // #error: cast 'void' to 'long'
    byte e = foo();             // #error: cast 'void' to 'byte'
    ushort f = foo();           // #error: cast 'void' to 'ushort'
    uint g = foo();             // #error: cast 'void' to 'uint'
    ulong h = foo();            // #error: cast 'void' to 'ulong'
    bool i = foo();             // #error: cast 'void' to 'bool'
}


int num = 10;

func void test23()
{
    int a = num;
    int b = test::num;
    char c = test::num;         // #error: cast 'int' to 'char'
}

int[2][3] b123;

func void test24()
{
    int a = b123;  // #error: cast 'int[2][3]' to 'int'
}

func void test25()
{
    const A = void; // #error: Constants cannot be undefined.
}

func void test26()
{
    const int A = void; // #error: Constants cannot be undefined.
}



