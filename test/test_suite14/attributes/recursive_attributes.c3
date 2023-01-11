module test;

define @Align(y) = @Align16(y / 2);
// This one is directly recursive
define @Align16(x) = @align(4), @Align(8 * x); // #error: Recursive declaration of attribute
// This one uses a recursive attribute
define @AlignIndirect(x) = @Align16(x); // #error: Recursive declaration of attribute
define @Test = @noinline;
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

struct Bar
{
	int abc @AlignIndirect(8);
}