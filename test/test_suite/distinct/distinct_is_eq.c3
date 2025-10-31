module test;
import std;
struct Boo { int a; }
typedef Foo = Boo;

fn bool Foo.eq(Foo a, Foo b) @operator(==) => false;

fn int main()
{
	$assert Foo.is_eq;
	return 0;
}