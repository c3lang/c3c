bitstruct Foo2 : int[2] // #error: The type of the bitstruct cannot be 'int[2]' but must be an
{
    bool enable_version;
}
fn void main()
{
	Foo2 a2 = { };
	Foo2 b2 = { .enable_version, };
	if (!(a2 & b2)) return;
}