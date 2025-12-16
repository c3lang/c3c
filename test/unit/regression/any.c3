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

fn void any_to_as()
{
	int x = 3;
	any a = &x;
	test::eq(a.as(int), 3);
	test::eq(a.to(int)!!, 3);
}

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