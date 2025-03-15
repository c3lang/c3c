enum Test @tag("test", Test.FOO) {FOO} // #error: Recursive definition of 'Test'

attrdef @Test(Foo val);

enum Foo @tag("test", Foo.BAR) // #error: Recursive definition of 'Foo'
{
  BAR,
  BAZ
}

fn void main() @Test(BAR)
{
}
