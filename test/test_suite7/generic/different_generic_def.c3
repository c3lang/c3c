module test;
import test2;
fn int main()
{
  Foo1(<int>) a;
  return 0;
}

module test2(<Type>);

struct Foo
{
   Type f;
}

module test2(<Type, FOO>); // #error: declarations of the generic

struct Foo1
{
  Type a;
}
