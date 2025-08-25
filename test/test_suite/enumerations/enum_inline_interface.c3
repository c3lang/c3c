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

enum Tester : inline Foo
{
	ABC
}

fn int main()
{
	Tester x;
	Test t = &x; // #error: but you can use an explicit cast to
	t.x();
	return 0;
}

