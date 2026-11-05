module int128_test;
import compiler_rt;

fn void check(uint128 a, uint128 b)
{
	uint128 div = a / b;
	uint128 mod = a % b;
	test::eq(div * b + mod, a);
}

fn void test_big() @test
{
	uint128 a = 12345678901234567890012ULL;
	uint128 b = 1234567890123456789001ULL;
	for (uint i = 0; i < 10; i++)
	{
		for (uint j = 0; j < 10; j++)
		{
			check(a + i, b + j);
		}
	}
}

fn void check_smaller(uint128 n, uint128 d)
{
	test::eq(n / d, (uint128)0);
	test::eq(n % d, n);
	test::eq(compiler_rt::__udivti3(n, d), (uint128)0);
	test::eq(compiler_rt::__umodti3(n, d), n);
	int128 sn = (int128)n;
	int128 sd = (int128)d;
	test::eq(sn / sd, 0);
	test::eq(sn % sd, sn);
	test::eq((-sn) / sd, 0);
	test::eq((-sn) % sd, -sn);
	test::eq(compiler_rt::__divti3(-sn, sd), 0);
	test::eq(compiler_rt::__modti3(-sn, sd), -sn);
}

// Dividing by a larger value where both high words are set, and the divisor has the larger high word.
fn void test_div_by_larger_high_word() @test
{
	// The divisor has a non zero low word.
	check_smaller(0x7fffffffffffffffffULL, 0x2bc759170fe375837da840d9ecULL);
	check_smaller(((uint128)1 << 64) | 1, ((uint128)3 << 64) | 1);
	// The divisor has a zero low word and a high word that isn't a power of two.
	check_smaller(((uint128)0x7f << 64) | 5, (uint128)0x81 << 64);
	check_smaller(((uint128)0xffff << 64) | 0x1234, (uint128)0x10001 << 64);
}

fn void test_div_remainder_is_smaller() @test
{
	uint128[*] values = {
		1, 3, 0xffffffffffffffffULL, (uint128)1 << 64, ((uint128)1 << 64) | 1, 0x7fffffffffffffffffULL,
		0x2bc759170fe375837da840d9ecULL, (uint128)0x81 << 64, ((uint128)1 << 127) + 7, ~(uint128)0
	};
	foreach (n : values)
	{
		foreach (d : values)
		{
			uint128 q = n / d;
			uint128 r = n % d;
			test::eq(q * d + r, n);
			assert(r < d, "Remainder %s not smaller than divisor %s", r, d);
		}
	}
}

// The high word of an arithmetic shift right must be sign extended.
fn void test_ashr_negative() @test
{
	test::eq(compiler_rt::__ashrti3(-2, 1), -1);
	test::eq(compiler_rt::__ashrti3(-1, 127), -1);
	test::eq(compiler_rt::__ashrti3(-(int128)1 << 100, 100), -1);
	test::eq(compiler_rt::__ashrti3(-(int128)1 << 100, 64), -(int128)1 << 36);
	int128 x = -(int128)0x123456789abcdef00fedcba987654321ULL;
	for (uint b = 0; b < 128; b++)
	{
		test::eq(compiler_rt::__ashrti3(x, b), x >> b);
		test::eq(compiler_rt::__lshrti3((uint128)x, b), (uint128)x >> b);
		test::eq(compiler_rt::__ashlti3(x, b), x << b);
	}
}
