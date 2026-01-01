interface Foo { }
fn int main()
{
	Foo? v;
	v.nope(); // #error: The 'Foo' interface has no method 'nope', did you spell it correctly
	return 0;
}
