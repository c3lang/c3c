module std::test;
fn int main()
{
	if ($$veccompne.type == int::typeid) // #error:  Builtin functions have neither methods nor fields
	return 0;
}