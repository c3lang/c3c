constdef Foo
{
	<* @deprecated *>
	ABC
}

<* @deprecated *>
Foo a;
fn int main()
{

	Foo f = Foo.ABC; // #warning: 'ABC' is deprecated.
	Foo b = a;       // #warning: 'a' is deprecated.
	return 0;
}
