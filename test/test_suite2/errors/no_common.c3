fn int! abc()
{
	return 1;
}
macro test()
{
	abc()?;
}

fn void main()
{
	test() ?? 2; // #error: Cannot find a common type for 'void' and 'int'
}