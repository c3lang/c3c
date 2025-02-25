struct Foo { int a; }
fn Foo Foo.foo() @operator(construct)
{
	return { 1 };
}
