struct Foo
{
    int x;
    struct b
    {
        int y;
        int z;
    }
    struct
    {
        int g;
    }
    union
    {
        usize x1;
        int z2;
    }
}

func void Foo.xy(Foo* f, int y)
{
    f.b.y = y;
}

extern func void printf(char*c , ...);

struct SimpleStruct
{
    int a;
    double b;
}
func void main()
{
    Foo z;
    z.x = 2;
    z.b.z = 3;
    z.g = 4;
    printf("z.x = %d\n", z.x);
    printf("z.b.z = %d\n", z.b.z);
    printf("z.g = %d\n", z.g);
    z.xy(100);
    printf("z.b.y = %d\n", z.b.y);
    SimpleStruct s = { a = 3, b = 33.3 };
    int[2] fe = { 1, 2 };
    //Foo f = { x = 2, b.z = 3, z2 = -2 };
    Foo f = { x = 2, b.z = 3, z2 = -2, b = { 1, 4 } };
    int[3] fo = { [1] = 2 };


    //byte[7] fo = { [1..4] = 3 };
}