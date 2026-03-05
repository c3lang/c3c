module std::math::bigint @test;

fn void test_init_with_u128()
{
	BigInt bi @noinit;
	assert(!bi.init_with_u128(uint128.max).is_negative());
}

fn void init_with_array()
{
	BigInt bi @noinit;
	assert(bi.init_with_array({}).equals(ZERO));
	assert(bi.init_with_array({0, 0, 0, 1}).equals(bigint::from_int(1)));
	assert("100000000" == bi.init_with_array({1, 0}).to_string_with_radix(16, tmem));
}

fn void test_parse16()
{
	BigInt bi @noinit;
    assert(bi.init_string_radix("c", 16)!!.equals(bigint::from_int(12)));
}

fn void test_zero()
{
    assert(bigint::from_int(0).to_string(tmem) == "0");
    BigInt bi;
    bi.init_string_radix("00", 16)!!;
    assert(bi.to_string(tmem) == "0");
}

fn void test_plus()
{
	BigInt a = bigint::from_int(123);
	BigInt b = bigint::from_int(234);
	assert(a.add(b).equals(bigint::from_int(234 + 123)));

	a = bigint::from_int(12323400012311213314141414LL);
	b = bigint::from_int(23400012311213314141414LL);
	assert(a.add(b).equals(bigint::from_int(12323400012311213314141414LL + 23400012311213314141414LL)));
}

fn void test_mult()
{
	BigInt a = bigint::from_int(123);
	BigInt b = bigint::from_int(234);
	assert(a.mult(b).equals(bigint::from_int(234 * 123)));

	a = bigint::from_int(1232311213314141414LL);
	b = bigint::from_int(234000123112414LL);
	assert(a.mult(b).equals(bigint::from_int(1232311213314141414LL * 234000123112414LL)));
}

fn void test_minus()
{
	BigInt a = bigint::from_int(123);
	BigInt b = bigint::from_int(234);
	assert(a.sub(b).equals(bigint::from_int(123 - 234)));

	a = bigint::from_int(12323400012311213314141414LL);
	b = bigint::from_int(23400012311213314141414LL);
	assert(a.sub(b).equals(bigint::from_int(12323400012311213314141414LL - 23400012311213314141414LL)));
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

fn void test_gcd()
{
	BigInt a = bigint::from_int(15);
	BigInt b = bigint::from_int(20);
	assert(a.gcd(b).equals(bigint::from_int(5)));
	assert(math::gcd(a,b).equals(bigint::from_int(5)));
}

fn void test_lcm()
{
	BigInt a = bigint::from_int(11);
	BigInt b = bigint::from_int(17);
	assert(a.lcm(b).equals(bigint::from_int(11*17)));
	assert(math::lcm(a,b).equals(bigint::from_int(11*17)));
}
