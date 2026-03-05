module test;
import test2;
fn int main()
{
  Foo1{int} a; // #error: 'Foo1' expects 2 arguments, but you supplied 1, did you make a mistake?
  return 0;
}

module test2 <Type>;

struct Foo
{
   Type f;
}

module test2 <Type, FOO>;

struct Foo1
{
  Type a;
}
