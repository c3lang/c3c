module test;

struct Foo
{
  char p;
  short q;
  char r;
  int x;
  short y, z;
  int q2;
}

extern func int test(Foo, float);
extern func int testE(char,short,char,int,int,float);

func void test3(Foo *x)
{
    x.q = 1;
}

func void test2(Foo y)
{
    testE(y.p, y.q, y.r, y.x, y.y, 0.1);
    test(y, 0.1);
    test2(y);
    test3(&y);
}
