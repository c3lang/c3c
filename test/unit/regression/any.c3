module any_tests @test;

fn void any_compare()
{
	int x;
	any a = &x;
	any b = &x;
	assert(a == b);
	assert(a == a);
}

def AnyAlias = any;

fn void test_aliasing()
{
	int x;
	AnyAlias z = &x;
	switch (z)
	{
		case int:
			assert(true);
		default:
			assert(false);
	}
}