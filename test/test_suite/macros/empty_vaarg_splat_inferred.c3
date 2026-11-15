macro usz @m(...)
{
	int[*] x = { ...$vaarg }; // #error: Zero length arrays
	return x.len;
}

fn void main()
{
	@m();
}
