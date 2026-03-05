struct Abc
{
	int x;
	int[*] y;
}
fn void test()
{
	Abc y;
	bool same = Abc.y == y.y; // #error: 'member_ref' and 'int[*]' are different types and cannot be compared
}