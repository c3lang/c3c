module arithmetics;

func void testAdd(int a, int b)
{
    a = a + b;
    a = a +% b;
    a +%= b;
    a += b;
    a += 1;
    a +%= 1;
}

func void testSub(int a, int b)
{
    a = a - b;
    a = a -% b;
    a -%= b;
    a -= b;
    a -%= 1;
    a -= 1;
}

func void testMult(int a, int b)
{
    a = a * b;
    a = a *% b;
    a *%= b;
    a *= b;
    a *%= 1;
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
    char x = -3 - 5;
    char c = -128;
}

func byte test22()
{
    return 100;
}