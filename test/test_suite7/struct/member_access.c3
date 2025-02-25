module test;


struct Multi
{
    int a, b, c;
}

struct Point
{
    int a;
    struct bb
    {
        int b;
    }
    struct
    {
        int b;
        int c;
        struct
        {
            Point* p;
        }
    }
    union
    {
        int d;
        short e;
    }
    union uu {
        int d;
        ushort e;
    }
}


fn void tester()
{
    Multi m;
    m.a = 1;
    Point p;
    p.a = 1;
    p.bb.b = 2;
    p.b = 3;
    p.c = 4;
    p.p = null;
    p.d = 5;
    p.e = 6;
    p.uu.d = 7;
    p.uu.e = 8;
    Point *p2 = &p;
    p2.bb.b = 3;
    p = { .a = 1, .bb.b = 3, .e = 2 };

    Point* pp = &p;
    pp.a = 20;
}

struct Aa1
{
    struct bb
    {
        int b;
    }
    struct
    {
        int c;
    }
}

fn void test_conversion_struct()
{
    Aa1 a1;
    int aa = a1.bb;    // #error: 'Aa1.bb' to 'int'
}

struct Struct
{
    int a;
}

fn void myfunc()
{
    Struct s;
    s.b = 10;       // #error: There is no field or method 'Struct.b'
}

