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

func int boba(int y, int j)
{
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
func int test(int x)
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


typedef func void(int) as Foo;
//typedef int as Foo;
extern func void printf(char *hello);

macro @hello()
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



func int main(int x)
{
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
    byte *ex = cast(byte *, eff);
    ex[0] = 5;
    if (eff[0] == 5) printf("Works-5!\n");
    ex[1] = 5;
    if (eff[0] == 5 + 5 * 256) printf("Works-5*256!\n");
    if (ef == 4) printf("Works5!\n");
    if (ef == 4) printf("Works1!\n");
        ef = 0;
/*
    byte a = 2;
    short b = 3;
    int c = 4;
    bool eok = true;
    long deee = (eok ? a : b) + (eok ? b : c);*/
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
}

func void test2(int* x, int y, int z)
{
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