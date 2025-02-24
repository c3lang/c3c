module core_builtin_tests;

fn void test_likely() @test
{
	assert(@likely(2 > 1));
	assert(@likely(2 > 1, 0.5));
}

fn void test_unlikely() @test
{
	assert(!@unlikely(2 < 1));
	assert(!@unlikely(2 < 1, 0.5));
}

fn void test_expect() @test
{
	assert(@expect(2 > 1, true));
	assert(!@expect(2 < 1, false));

	assert(@expect(2 > 1, true, 0.5));
	assert(!@expect(2 < 1, false, 0.5));
}

