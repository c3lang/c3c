func void test1()
{
    double x = 2.3 +% 2; // #error: only valid for integer addition
}

func void test2()
{
    double x = 0;
    int y = x +% 4; // #error: only valid for integer addition
}

func void test3()
{
    double x = 2.3 -% 2; // #error: only valid for integer subtraction
}

func void test4()
{
    double x = 0;
    int y = x -% 4; // #error: only valid for integer subtraction
}

func void test5()
{
    double x = 2.3 *% 2; // #error: try * instead
}

func void test6()
{
    double x = 0;
    int y = x *% 4; // #error: try * instead
}

func void test7()
{
    double x = 1.2 / 0; // #error: division by zero is not allowed
}

func void test8()
{
    int y = 0 / 0; // #error: division by zero is not allowed
}

func void test9()
{
    int y = 0;
    int x = y / 0; // #error: division by zero is not allowed
}

func void test10()
{
    10 = 20;        // #error: Expression is not assignable
}

func void test11()
{
    '10' = '20';        // #error: Expression is not assignable
}

func void test12()
{
    true = false;        // #error: Expression is not assignable
}

func void test13()
{
    "a" = "b";        // #error: Expression is not assignable
}

func void test14()
{
    1.2 = 1.3;        // #error: Expression is not assignable
}

func void test15()
{
    null = null;      // #error: Expression is not assignable
}

func void test16()
{
    int a = 0;
    uint b = 2;
    ushort c = 3;
    a = a + c;
    int g = a + b;      // #error: Cannot add 'int' to 'uint'
}

func void test17()
{
    char a = 100 + 300; // #error: '300' does not fit in type 'char'
}

func void test18()
{
    char b = 100 + 156; // #error: Cannot fit '256' into type 'char'
}

func void test19()
{
    ichar b = (-40) - 126; // #error: Cannot fit '-166' into type 'ichar'
}



func void test20()
{
    ichar d = ((-128 - 10) + 10) - 2; // #error: Cannot fit '-130' into type 'ichar'
    ichar c = 100 * 100; // #error: Cannot fit '10000' into type 'ichar'
    ichar e = (-138 + 30);
    ichar f = -138 + 30; // #error: '-138' does not fit in type 'ichar'
    ichar g = -(128);
    check(128); // #error: '128' does not fit in type 'ichar'
}

func void check(ichar x) {}


func char test22()
{
    return 300;     // #error: '300' does not fit in type 'char'
}

