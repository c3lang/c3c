bitstruct Foo : long
{
	int a : 2..4;
	uint b: 5..10;
}
fn int main()
{
	Foo? d = { 1, 11 };
	var $member = Foo::members[0];
	d.$member = 3; // #error: You cannot assign to an optional value.
	return 0;
}