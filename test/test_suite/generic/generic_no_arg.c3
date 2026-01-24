module foo;
import std, bar;
struct Foo
{
	int i;
}
fn void foo(Foo f) {}

fn void a()
{
	List{} a; // #error: 'List' requires generic arguments inside the '{}', did you forget them
}
fn void b()
{
	foo(Foo{}); // #error: 'Foo' is not a generic type
}
fn void c()
{
	bar::test{}(); // #error: 'test' requires generic arguments inside the '{}', did you forget them
}

fn int main()
{
	return 0;
}

module bar <Type>;
fn void test()
{}