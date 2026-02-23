module foo;
import std;
interface Test
{
	fn void x();
}

typedef Foo (Test) = int;

fn void Foo.x(&self) @dynamic
{
	io::printn("Foo!");
}

constdef Tester : inline Foo
{
	ABC = 1
}

fn int main()
{
	Tester x;
	Test t = &x; // #error: but you can use an explicit cast to
	t.x();
	return 0;
}