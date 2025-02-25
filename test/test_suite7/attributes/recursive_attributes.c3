module test;

def @Align(y) = { @Align16(y / 2) };
def @Align16(x) = { @align(4) @Align(8 * x) }; // #error: Recursive declaration of attribute
def @Test = { @noinline };
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
