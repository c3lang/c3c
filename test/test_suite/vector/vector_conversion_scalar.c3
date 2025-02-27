// #deprecation: no
fn void test2(int[<2>] x) {}
fn void main()
{
  int[<2>] y = 1;
  y[..] = 3;
  y.xy = 3;       // #error: cannot use swizzling
  y *= 2;
  y = 3;          // #error: explicit cast
  test2(3);       // #error: explicit cast
}