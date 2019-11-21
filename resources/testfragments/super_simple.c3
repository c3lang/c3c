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

func int test3()
{
    if (test() < 0) return -1;
    return 5;
}
typedef func void(int) as Foo;
//typedef int as Foo;

func void printf(char *hello);

func int main(int x)
{
    printf("Hello worldABC" "D" "E\u2701\n");
    return 4 * test3();
}

func void test2(int* x, int y, int z)
{
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
    // TODO x + 1;
}