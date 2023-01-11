module arithmetics;

fn void testAdd(int a, int b)
{
    a = a + b;
    a += b;
    a += 1;
}

fn void testSub(int a, int b)
{
    a = a - b;
    a -= b;
    a -= 1;
}

fn void testMult(int a, int b)
{
    a = a * b;
    a *= b;
    a *= 1;
}

fn void testDiv(int a, int b)
{
    a = a / b;
    a /= b;
    a /= 1;
}

fn void testAssignment()
{
    ichar x = -3 - 5;
    ichar c = -128;
}

fn char test22()
{
    return 100;
}