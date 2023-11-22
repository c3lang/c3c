
fn void test7()
{
    double x = 1.2 / 0; // This is ok! NaN
}

fn void test8()
{
    int y = 0 / 0; // #error: division by zero is not allowed
}

fn void test9()
{
    int y = 0;
    int x = y / 0; // #error: division by zero is not allowed
}

fn void test10()
{
    10 = 20;        // #error: to a constant expression
}

fn void test11()
{
    '10' = '20';        // #error: to a constant expression
}

fn void test12()
{
    true = false;        // #error: to a constant expression
}

fn void test13()
{
    "a" = "b";        // #error: to a constant expression
}

fn void test14()
{
    1.2 = 1.3;        // #error: to a constant expression
}

fn void test15()
{
    null = null;      // #error: to a constant expression
}

fn void test16()
{
    int a = 0;
    uint b = 2;
    ushort c = 3;
    a = a + c;
    int g = a + b;
}

fn void test17()
{
    char a = 100 + 300; // #error: '400' is out of range for 'char'
}

fn void test18()
{
    char b = 100 + 156; // #error: '256' is out of range for 'char'
}

fn void test19()
{
    ichar b = (-40) - 126; // #error: '-166' is out of range for 'ichar'
}



fn void test20()
{
    ichar d = ((-128 - 10) + 10) - 2; // #error: '-130' is out of range for 'ichar'
    ichar c = 100 * 100; // #error: '10000' is out of range for 'ichar'
    ichar e = (-138 + 30);
    ichar f = -138 + 30;
    ichar g = -(128);
    check(128); // #error: '128' is out of range for 'ichar'
}

fn void check(ichar x) {}


fn char test22()
{
    return 300;     // #error: '300' is out of range for 'char'
}

