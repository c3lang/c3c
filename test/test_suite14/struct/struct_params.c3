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

extern fn int test(Foo, float);
extern fn int testE(char,short,char,int,int,float);

fn void test3(Foo *x)
{
    x.q = 1;
}

fn void test2(Foo y)
{
    testE(y.p, y.q, y.r, y.x, y.y, 0.1);
    test(y, 0.1);
    test2(y);
    test3(&y);
}
