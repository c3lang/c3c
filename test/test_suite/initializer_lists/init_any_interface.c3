module test;

interface Foo{}

struct Bar (Foo)
{
  int baz;
}

fn int main() {
  Test(<Foo>) a = {{}};
  Test(<any>) b = {{}};
  Test(<Foo>) a2 = {Bar{}}; // #error: It is not possible to cast

  return 0;
}

module test::generic(<Type>);

struct Test
{
  Type test;
}