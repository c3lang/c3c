fn int! abc()
{
	return 1;
}
macro test()
{
	abc()!;
}

fn void main()
{
	test() ?? 2; // No longer an error!
}