module any_tests @test;

fn void any_compare()
{
	int x;
	any a = &x;
	any b = &x;
	assert(a == b);
	assert(a == a);
}

alias AnyAlias = any;

fn void test_aliasing()
{
	int x;
	AnyAlias z = &x;
	switch (z.type)
	{
		case int:
			assert(true);
		default:
			assert(false);
	}
}