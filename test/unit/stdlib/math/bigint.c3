module std::math::bigint @test;

fn void test_plus()
{
	BigInt a = bigint::from_int(123);
	BigInt b = bigint::from_int(234);
	assert(a.add(b).equals(bigint::from_int(234 + 123)));

	a = bigint::from_int(12323400012311213314141414i128);
	b = bigint::from_int(23400012311213314141414i128);
	assert(a.add(b).equals(bigint::from_int(12323400012311213314141414i128 + 23400012311213314141414i128)));
}

fn void test_mult()
{
	BigInt a = bigint::from_int(123);
	BigInt b = bigint::from_int(234);
	assert(a.mult(b).equals(bigint::from_int(234 * 123)));

	a = bigint::from_int(1232311213314141414i128);
	b = bigint::from_int(234000123112414i128);
	assert(a.mult(b).equals(bigint::from_int(1232311213314141414i128 * 234000123112414i128)));
}

fn void test_minus()
{
	BigInt a = bigint::from_int(123);
	BigInt b = bigint::from_int(234);
	assert(a.sub(b).equals(bigint::from_int(123 - 234)));

	a = bigint::from_int(12323400012311213314141414i128);
	b = bigint::from_int(23400012311213314141414i128);
	assert(a.sub(b).equals(bigint::from_int(12323400012311213314141414i128 - 23400012311213314141414i128)));
}

fn void test_init_string_radix()
{
	BigInt a;
	a.init_string_radix("123", 10)!!;
	assert(a.equals(bigint::from_int(123)));
	a.init_string_radix("123", 8)!!;
	assert(a.equals(bigint::from_int(0o123)));
	a.init_string_radix("123", 16)!!;
	assert(a.equals(bigint::from_int(0x123)));
}