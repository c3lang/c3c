module int128_test;

fn void check(uint128 a, uint128 b)
{
	uint128 div = a / b;
	uint128 mod = a % b;
	assert(div * b + mod == a);
}

fn void test_big() @test
{
	uint128 a = 12345678901234567890012u128;
	uint128 b = 1234567890123456789001u128;
	for (int i = 0; i < 10; i++)
	{
		for (int j = 0; j < 10; j++)
		{
			check(a + i, b + j);
		}
	}
}
