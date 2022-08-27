
fn void test()
{
	for FOO: (int i = 0; i < 100; i++)
	{
		{|
			if (i == 10) break FOO; // #error: You cannot break out of an expression block
		|};
	}
}