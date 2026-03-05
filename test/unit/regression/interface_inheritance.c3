module inherit_interface;
import std;
interface Test
{
	fn int x();
}

struct Foo (Test)
{
	int z;
}

fn int Foo.x(&self) @dynamic
{
	return 42;
}

struct Baz
{
	inline Bar b;
}

struct Bar
{
	inline Foo f;
}

fn void test_inheritance() @test
{
	Baz x;
	Test t = &x;
	test::eq(t.x(), 42);
}