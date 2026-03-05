bitstruct Foo : long
{
	int a : 2..4;
	uint b: 5..10;
}
fn int main()
{
	Foo? d = { 1, 11 };
	Foo.membersof[0].set(d, 3); // #error: You cannot assign to an optional value.
	return 0;
}