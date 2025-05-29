struct Abc
{
  int a;
  int[*] b;
}
fn void test()
{
	Abc y = {};
	foreach (x : y.b) {} // #error: It is not possible to enumerate over a flexible
}
fn int main()
{
  foreach (x : (Abc){}.b) {} // #error: it's a flexible array member
  return 0;
}