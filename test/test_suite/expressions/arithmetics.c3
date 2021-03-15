module arithmetics;

func void testAdd(int a, int b)
{
    a = a + b;
    a += b;
    a += 1;
}

func void testSub(int a, int b)
{
    a = a - b;
    a -= b;
    a -= 1;
}

func void testMult(int a, int b)
{
    a = a * b;
    a *= b;
    a *= 1;
}

func void testDiv(int a, int b)
{
    a = a / b;
    a /= b;
    a /= 1;
}

func void testAssignment()
{
    ichar x = -3 - 5;
    ichar c = -128;
}

func char test22()
{
    return 100;
}