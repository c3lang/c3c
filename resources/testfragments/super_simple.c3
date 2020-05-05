module bar;

typedef int as Bob;

struct Test
{
    int a;
}

struct Test2
{
    Test t;
    int b;
}

union Test3
{
    long eo;
    Test t;
    int b;
}

struct Teob
{
    int x;
    double y;
    int xy;
    int oekfeo;
}

enum EnumWithData : ushort (int a, char[] x, long b = 4)
{
    // Currently the args are ignored TODO!
    TEST1(42, "hello", 328) = 3,
    TEST2(12, "world")
}

/*
enum EnumTestNoOverflowAfterULong : ulong
{
    VALUE = 0xFFFF_FFFF_FFFF_FFFE,
    VALUE_NO_EXCEED
}



enum EnumTestOverflowAfterLong : long
{
    VALUE = 0x7FFF_FFFF_FFFF_FFFF,
    VALUE_EXCEED
}

enum EnumTestOverflowAfterULong : ulong
{
    VALUE = 0xFFFF_FFFF_FFFF_FFFF,
    VALUE_EXCEED
}

enum EnumTestOverflowAfter
{
    VALUE = 0x80000000 - 1,
    VALUE_EXCEED
}*/



errset Error
{
    BLURB,
    NO_SUCH_FILE,
}

errset OtherError
{
    FOO_BAR
}

enum Inf
{
    A,
    B,
    C = 10000
}

enum Inf2 : byte
{
    A,
    B,
    C = 129,
}

typedef Inf as BooInf;

struct TestStruct
{
    int a;
}

struct TestStruct2
{
    TestStruct a;
    char xx;
    TestStruct b;
    int c;
}

union TestUnion
{
    int a;
    double f;
    TestStruct2 e;
}
union SimpleUnion
{
    int a;
    double f;
}

func void testPointer()
{
    int i = 0;
    int *x = &i;
    int y = x[2];
    *x;
}
func void testUnion()
{
    int wdw = 123;
    cast(wdw, long);
    wdw = 222;
    SimpleUnion s;
    s.a = 1;
    s.f = 1.0;
    s = { 1 };
    int x = 2;
    s = { (x = 2) };
    s = { f = 1.0 };
    TestUnion tu = { e = TestStruct2 { c = 1 } };
    tu.e = TestStruct2 {  c = 1 };


    SimpleUnion y = s = { 2 };
    double yval = SimpleUnion{ f = 3.333 }.f;
    printf("Union y: %d\n", y.a);
    printf("Union simple: %f\n", yval);
}

func TestStruct2 structTest(int i)
{
    TestStruct foo = { i };
    TestStruct foo2 = { a = i };
    TestStruct foo3 = TestStruct { i };
    TestStruct2 bar = { c = 2 };
    int x = 3 * i;
    TestStruct2 bar2 = { b.a = x, a.a = x + 1 };
    return bar2;
}

func void enumInferenceTest()
{
//    OtherError e = OtherError.FOO_BAR;
    Inf x = Inf.A;
    x = BooInf.B;
    x = A;
    int x1 = 0;
    bool y = x1 == x1;
    Inf2 z = C;
    if (z == Inf2.A) return;
    if (z == 1) return;
    z = 2;
    switch (z)
    {
        case Inf2.A:
            x1++;
            return;
        case B:
            return;
        case 111:
            x1 += 1;
            return;
        default:
            return;
    }
}

func int jumptest()
{
    if (1) goto LABELX;
    return 1;
    LABELX:
    return 2;
}
/*
func int borok() throws
{
    return 1;
}*/
func void testNoReturn()
{
    int i = 0;
    i = -i;
}

func int testReturn()
{
    int i = 0;
    return i;
}

func int testReturnWithOtherAtEnd()
{
    int i = 0;
    return i;
    if (i == 10) i++;
    i = i + 2;
}
/*
func int testReturnWithError() throws Error
{
    int i = 0;
    throw Error.NO_SUCH_FILE;
    i = i + 1;
}*/

func int testReturnWithConditional()
{
    int i = 0;
    if (i > 0)
    {
        return 1;
    }
    else
    {
        return 2;
    }
}
/*
func int testReturnWithCondThrow() throws Error
{
    int i = 0;
    if (i > 0)
    {
        throw Error.NO_SUCH_FILE;
    }
    else
    {
        throw Error.NO_SUCH_FILE;
    }
}
*/
func int testReturnSwitch()
{
    int i = 0;
    switch (i)
    {
        case 0:
        case 3:
        case 1:
            return 2;
        case 2:
            return 3;
        default:
            return 4;
    }
}

/*
func int barok() throws Error, OtherError
{
    if (true)
    {
        throw Error.NO_SUCH_FILE;
    }
    return 100;
}
*/

struct SimpleStruct
{
    int a;
    int b;
    double c;
    double d;
    char z1;
    char z2;
}
func void testSimpleStruct(int x)
{
    SimpleStruct snoinit;
    SimpleStruct sinit = { b = 1, d = 3.0, z1 = 1 };
    sinit.a = 1;
    sinit.b = 2;
    printf("a = %d, b = %d (1), c = %f, d = %f (3.0), z1 = %d (1), z2 = %d\n", sinit.a, sinit.b, sinit.c, sinit.d, cast(sinit.z1, int), cast(sinit.z2, int));
    snoinit.b = 1;
    snoinit.a = 100;
    snoinit.d = 3.0;
    snoinit.c = 2.0;
    snoinit.z1 = 1;
    snoinit.z2 = 2;
    printf("b = %d (1), d = %f (3.0), z1 = %d (1)\n", snoinit.b, snoinit.d, snoinit.z1);
}

struct AnonStruct
{
    int a;
    struct xx
    {
        int b;
        int c;
    }
    struct
    {
        int b1;
        int c1;
    }
    union
    {
        int b2;
        int c2;
    }
    int x;
}

func AnonStruct sendAnonStruct()
{
    AnonStruct foo = AnonStruct { a = 1 };
    // Foo { a = 1 }
    foo.b2 = 123;
    return foo;
}

func void testAnonStruct2()
{
    sendAnonStruct();
}


func void testAnonStruct()
{
    AnonStruct s = { b2 = 3, b1 = 7, xx.b = 1 };
    AnonStruct foo;

    printf("a = %d, b = %d (1), c = %d, b1 = %d (7), c1 = %d, b2 = %d (3), c2 = %d (3), x = %d\n", s.a, s.xx.b, s.xx.c, s.b1, s.c1, s.b2, s.c2, s.x);

    s.xx.b = 100;
    s.xx.c = 99;
    s.b1 = 3;
    s.b2 = 5;
    s.c2 = 7;

    printf("a = %d, b = %d (100), c = %d (99), b1 = %d (3), c1 = %d, b2 = %d (7), c2 = %d (7), x = %d\n", s.a, s.xx.b, s.xx.c, s.b1, s.c1, s.b2, s.c2, s.x);

    s = { xx = { c = 2 }};

    printf("a = %d, b = %d, c = %d (2), b1 = %d, c1 = %d, b2 = %d, c2 = %d, x = %d\n", s.a, s.xx.b, s.xx.c, s.b1, s.c1, s.b2, s.c2, s.x);

    s.xx.c = 3;
    s.x = 1212;
    s.a = 29183;
    s = AnonStruct { xx = { c = 2 }};

    printf("a = %d, b = %d, c = %d (2), b1 = %d, c1 = %d, b2 = %d, c2 = %d, x = %d\n", s.a, s.xx.b, s.xx.c, s.b1, s.c1, s.b2, s.c2, s.x);

    s = sendAnonStruct();

    printf("Got it sent: a = %d, b = %d (1), c = %d, b1 = %d (7), c1 = %d, b2 = %d (3), c2 = %d (3), x = %d\n", s.a, s.xx.b, s.xx.c, s.b1, s.c1, s.b2, s.c2, s.x);

}
func int boba(int y, int j)
{
//    hello();
    //$e = type(Teob);
    //Teob xbd = type(Teob);
    //Teob xb = { 1, 1.0, 100, 1000 };
    //Test2 tee = { { 3 }, 4 };
    //Test3 xc = { eo = 1, t.a = 1 };
   // throw Error.NO_SUCH_FILE;

    for (int i = 0; i < 10; i++)
    {

    }
    for (int i = 0, int foo = 0; i < 10; i++)
    {

    }

    for (int i = 0, j = 1; i < 10; i++, j++)
    {

    }

    Test2 bar;
    bar.b = 1;
    //int w = y ? y : j;
    int x = y * 2;
    int z = j;
    while (j > 10)
    {
        z--;
        x = x + z * 2;
    }
    return x;
}
func int test(int x = 100)
{
    x = x + 1;
    x = x +% 1;
    x += 1;
    x +%= 1;
    Test3 foekf;
    Test oef;
    Test2 foek;
    int i = x;
    Bob foo = x;
    Bob fe = 0;
    fe++;
    switch (fe + 1)
    {
        case 1:
            i = i + 1;
            next;
        case 3:
        case 2:
            i = i + 2;
        case 5:
        default:
            i = i * 100;
        case 7:
    }
    i++;
    int y = i--;
    return y;
}

func int* elvis(int *x, int *y)
{
    return x ?: y;
}

func int test3()
{
    if (test() < 0) return -1;
    return 5;
}

typedef func void() as FEok;

typedef func void(int) as Foo;
//typedef int as Foo;
extern func void printf(char *hello, ...);

macro hello()
{
    printf("Hello world!\n");
}

func void bob()
{

    byte a = 2;
    short b = 3;
    int c = 4;
    bool eok = true;
    long deee = (eok ? a : b) + c;
}

func int if_test(int x)
{
    switch (x)
    {
        case 1:
            x += 1;
            if (x < 10)
            {
                defer x += 5;
                if (x < 7)
                {
                    defer x += 100;
                    next;
                }
                x += 99;
            }
            next;
        default:
            x += 2;
            break;
    }
    return 1;
}
func int yyyy(int x)
{
    defer printf("A");
    if (x > 0) return 2;
    defer printf("B");
    printf("C");
    return 1;
}

func void zzzz()
{
    int x = 0;
    defer
    {
        x += 1;
        printf("A");
    }
    defer
    {
        x += 2;
        printf("B");
    }
    printf("C");
    x += 3;
}

func int jumpback(int x)
{
    {
        defer x += 1;
        {
            defer x += 2;
            LABELX:
            x += 3;
        }
    }

    if (x < 100) goto LABELX;
    return x + 1;
}

func void test_expr_block(int x)
{
    int a = ({
        if (x > 0) return x * 2;
        if (x == 0) return 100;
        return -x;
    });
    //printf("The result was %d\n", a);
}

func int expr_block()
{
    int fok = ({ return ({ return 10; }); });
    int y = 2;
    int x = ({
        if (y < 10) return 10;
        return 2;
    });

  /*  ({
        return 3;
    });*/
    return x;

}
func int xxxx(int x)
{


    {
        x += 10;
        defer printf("XXX");
        if (x < 100) goto LABELD;
        defer printf("EODfe");
    }
    {
        defer printf("Defer says hello!\n");
        LABELD:
        x--;
    }
    if (x > 0) goto LABELD;
    return 1;
}

func int testPointers(int x, int j = 0, double foo = 3.2)
{
    1 ? 1 : 2;
    int y = 0;
    int* z = &y;
    int d = *(z + y);
    isize e = z - &y;
    int* eff = &y + 1;
    short x1 = 2;
    float f = x1 +% x1 + 1.0;
    float f2 = x1 -% x1 + 1.0;
    usize ef = z - &y > 0 ? 1 : z - &y;
    z - &y > 0 ? 1 : z - &y;
    return 1;
}

func void testDefault(int x = 2, int y = 100, int z = -100)
{
    printf("x = %d, y = %d, z = %d\n", x, y, z);
}

func int testReturnDefer()
{
    int i = 0;
    i++;
    defer ++i;
    return i;
}

struct WithArray
{
    int[4] x;
}

errset Err
{
    TEST_ERR1
}

errset Err2
{
    TEST_ERR2
}

func int testThrow1() throws Err
{
    throw Err.TEST_ERR1;
}

func int testThrow2() throws Err, Err2
{
    throw Err.TEST_ERR1;
}

func int testThrow3() throws
{
    throw Err.TEST_ERR1;
}

func int testThrow4(int x) throws
{
    throw x > 4 ? Err.TEST_ERR1 : Err2.TEST_ERR2;
}

func int testThrow5(int x) throws
{
    throw x > 4 ? Err.TEST_ERR1 : Err.TEST_ERR1;
}

func int testThrow6(int x) throws Err, Err2
{
    if (x > 4) throw Err.TEST_ERR1;
    throw Err2.TEST_ERR2;
}


func int testThrow(int x) throws Err
{
    if (x < 0) throw Err.TEST_ERR1;
    return x * x;
}

func int testThrowAny(int x) throws
{
    if (x < 0) throw Err.TEST_ERR1;
    return x * x;
}

func int oekt() throws
{
    int z = try testThrow(-1) else goto NEXT;
    printf("Skipped.\n");
    NEXT:
    int x = try testThrow(-3);
    return x;
}

func void testErrorMulti()
{
    defer printf("End defer\n");
    printf("Test error multi\n");
   {
        defer printf("Defer\n");
        printf("Will call\n");
        int z = try oekt();
        printf("Got here\n");
    }
    catch (Err e)
    {
        defer printf("Left error\n");
        printf("Expected particular error.\n");
    }
    catch (error e)
    {
        defer printf("Left any error\n");
        printf("Unexpected any error error.\n");
    }
    printf("End\n");
}

func void throwAOrB(int i) throws Err, Err2
{
    printf("AB\n");
    defer printf("A-Err\n");
    if (i == 1) throw Err.TEST_ERR1;
    printf("B\n");
    defer printf("B-Err\n");
    if (i == 2) throw Err2.TEST_ERR2;
    printf("None\n");
}

func void testErrors()
{

    int x = try testThrow(20) else 1;
    printf("Value was %d, expected 400.\n", x);


    x = try testThrow(-1) else 20;
    printf("Value was %d, expected 20.\n", x);

    printf("Begin\n");
    int y = try testThrow(-1);

    printf("Value was %d, expected 9.\n", y);

    printf("Didn't expect this one.\n");
    catch (error e)
    {
        printf("Expected this catch.\n");
    }
    y = try testThrow(-1);

    catch (Err e)
    {
        printf("Particular error.\n");
    }
    testErrorMulti();

    try throwAOrB(1);
    catch (Err e)
    {
        printf("A1\n");
    }
    catch (Err2 e)
    {
        printf("A2\n");
    }
    catch (error e)
    {
        printf("Wut\n");
    }
    try throwAOrB(2);
    catch (Err e)
    {
        printf("B1\n");
    }
    catch (Err2 e)
    {
        printf("B2\n");
    }
    catch (error e)
    {
        printf("Wut\n");
    }
    printf("End of errors\n");
}

func void testArray()
{
    int[4] zebra = { [2] = 1 };
    int[4] deok = { 1, 2, 3, 4 };
    WithArray boo;
    boo.x[0] = 2;
    printf("boo.x[0] = %d\n", boo.x[0]);
    printf("boo.x[2] = %d\n", boo.x[2]);
    int[4] x;
    x[1] = 1;
    x[0] = 3;
    int y = x[1];
    int z = 1;
    int* b = &z;
    printf("b: %d\n", *b);
    *b = 3;
    printf("b: %d\n", *b);
    printf("z: %d\n", z);

    for (int i = 0; i < 4; i++)
    {
        printf("x[%d] = %d\n", i, x[i]);
    }

}
func void testDefer()
{
    printf("1 == %d\n", testReturnDefer());
    printf("1");
    defer printf("8\n");
    printf("2");
    {
        printf("3");
        defer printf("5");
        printf("4");
    }
    int i = 0;
    goto JUMP;
    defer printf("ERROR");
    int r = 0;
JUMP:
    defer printf("-JUMPDEFER-");
    if (i++ < 2) goto JUMP;
    switch (int boo = 2)
    {
        case 0:
            printf("\n0\n");
        case 2:
            defer printf("*CaseDefer*");
            printf("-Case2-");
        default:
            break;
    }
    defer printf("7");
    printf("6");
}

func void testType()
{
    printf("Test type\n");
    typeid x = WithArray.typeid;
    int y = 0;
    Teob b;
    typeid structSize = typeof(b);
}

func int main(int x)
{

    printf("Helo!\n");
    testErrors();
    testDefault(y = 99);
    testPointers(2, 3);
    testDefer();
    testArray();
    testAnonStruct();
    testSimpleStruct(0);
    testUnion();
    testType();
    int efd = 9;
    uint fefoek = 1;
    printf("Helo: %d\n", efd + cast(fefoek, int));
    //long fefoek = -fefoek;
    int okfe = 1;
    return 1;
    switch (int bobe = okfe > 0 ? 1 : 0)
    {
        case 0:
            defer printf("case0-\n");
        case 1:
            printf("case 1\n");
            defer printf("case1-\n");
            if (efd < 10)
            {
                {
                defer printf("ef < 10\n");
                if (efd < 7)
                {
                    defer printf("ef < 7\n");
                    next;
                }
                }
            }
            next;
        case 1000 >> 2:
            printf("case 1000 >> 2\n");
        case (1 << 200) >> 197:
            printf("case 1 << 3\n");
        default:
            printf("default\n");
    }
    int aa = x++;
    int bb = x--;
    int cc = ++x;
    for (int ok = 0; ok < 10; ok++)
    {
        printf("ok");
    }
    printf("\n");
    for (int ok = 0, int ko = 0, ok = 2; ok + ko < 10; ok++, ko++)
    {
        printf(":okko");
    }
    printf("\n");
    while (int ok = 0; int j = ok++, ok < 10)
    {
        printf("foo");
    }
    printf("\n");
    x = 3;
    if (int odk = x, x > 0)
    {
        printf("helo\n");
    }
    Test baok = { 1 };
    Test2 efe;
    efe.t.a = 3;
    if (efe.t.a > 2) printf("Works!\n");
    int ef = 3;
    int *eff = &ef;
    eff[0] = 4;
    byte *ex = cast(eff, byte*);
    ex[0] = 5;
    if (eff[0] == 5) printf("Works-5!\n");
    ex[1] = 5;
    if (eff[0] == 5 + 5 * 256) printf("Works-5*256!\n");
    if (ef == 4) printf("Works5!\n");
    if (ef == 4) printf("Works1!\n");
        ef = 0;

    byte a = 2;
    short b = 3;
    int c = 4;
    bool eok = true;
    long deee = (eok ? a : b) + (eok ? b : c);
int i = 0;

    JUMP:
    i = i + 1;
    //@hello();
    printf("Hello worldABC" "D" "E\u2701\n");
    float f = 3.0;
    float* pf = &f;
    switch (i)
    {
        case 0:
            printf("c0\n");
        case 1:
            printf("c1\n");
        case 2:
            printf("c2\n");
        case 3:
            printf("c3\n");
        default:
            printf("default\n");
    }
    if (*pf > i) goto JUMP;
    goto EX;
    YEF:
    return 4;
    EX:
    printf("EX\n");
    goto YEF;
    return 1;
}

func void test2(int* x, int y, int z)
{
    *(&(&x)[0]);
    float cheat = cast(x, int);

    x++;
    z = 0;
    z ? y : z;
    x += 1;
    y += z;
    x -= 1;
    y -= z;
    y *= 2;
    y /= 2;
    y /= *x;
    y |= 2;
    y ^= 2;
    y &= 2;
    z ^ y;
    z | y;
    int g = z & y;
    g <<= 2;
    g <<= z;
    g >>= 2;
    g >>= z;
    int fz = 100;
    y && z;
    y || z;
    y >> z;
    z << y;
    ~z;
    !z;
    -z;

    int i = 3;
    uint ui = 2;
    int j = 129;
    float f = 23.2;
    f = f + 1.0;
    f = f - 1.0;
    f = f * 2.0;
    f = f / 3.0;
    i = i * 2;
    ui = ui * 2;
    i = i / 2;
    ui = ui / 2;
    i = i + 1;
    ui = ui + 1;
    i = i - 1;
    ui = ui - 1;
    x + 1;
    int j1 = x[0];
    j1 = *x;
}

