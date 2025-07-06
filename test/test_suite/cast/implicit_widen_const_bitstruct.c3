bitstruct Foo : ushort
{
	bool bar;
}

fn void main()
{
	ushort foo = (ushort)((Foo) {.bar}) & 1;
}