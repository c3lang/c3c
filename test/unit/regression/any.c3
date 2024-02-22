module any_tests @test;

fn void any_compare()
{
	int x;
	any a = &x;
	any b = &x;
	void* v = &x;
	any c = v;
	assert(a == b);
	assert(a == a);
	assert(a.ptr == c.ptr);
	assert(a != c);
	bool aisc = a == c;
	assert(!aisc);
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