module foo;
import std, bar;
struct Foo
{
	int i;
}
fn void foo(Foo f) {}

fn void a()
{
	List{} a; // #error: must be instantiatied with generic module arguments
}
fn void b()
{
	foo(Foo{}); // #error: 'Foo' is not a generic type
}
fn void c()
{
	bar::test{}(); // #error: must be instantiatied with generic module arguments
}

fn int main()
{

	return 0;
}

module bar{Type};
fn void test()
{}