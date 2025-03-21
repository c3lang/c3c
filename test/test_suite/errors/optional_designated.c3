module foo;

struct Foo { int a; }
struct Bar { int b; Foo f; }
fn void main()
{
	(Bar) { .f = (Foo) { foo() } }; // #error: An optional value was discarded
}

fn int? foo() => 1;