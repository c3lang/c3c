module test;

attrdef @Align(y) = @Align16(y / 2);
attrdef @Align16(x) = @align(4), @Align(8 * x); // #error: Recursive declaration of attribute
attrdef @Test = @noinline;
struct Foo
{
	int z;
	int xy @Align16(8);
}

Foo f;

fn void testme() @Test
{
	int x;
}
