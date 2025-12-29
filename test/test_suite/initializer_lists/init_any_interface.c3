module test;

interface Foo{}

struct Bar (Foo)
{
  int baz;
}

fn int main() {
  Test{Foo} a = {{}};
  Test{any} b = {{}};
  Test{Foo} a2 = {(Bar){}}; // #error: You can only convert pointers to an interface

  return 0;
}

module test::generic{Type};

struct Test
{
  Type test;
}