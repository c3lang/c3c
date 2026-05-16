module std::math::bigint @test;

fn void test_init_with_u128()
{
	BigInt bi @noinit;
	assert(!bi.init_with_u128(uint128::max).is_negative());
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

fn void test_negative_print()
{
	assert(bigint::from_int(-1).to_string(tmem) == "-1");
}

fn void test_multi_byte_divide()
{
	// Test 1: exact division, quotient fits in one limb
	// (2^32+1) * (2^32-1) = 2^64-1
	// So (2^64-1) / (2^32+1) = 2^32-1, remainder 0
	BigInt a; a.init_with_u128(18446744073709551615ull); // 2^64 - 1
	BigInt b; b.init_with_u128(4294967297ull);           // 2^32 + 1

	BigInt q = a;
	q.div_this(b);
	BigInt r = a;
	r.mod_this(b);

	assert(q.equals(from_int(4294967295)));  // 2^32 - 1
	assert(r.is_zero());

	// Test 2: 2^64 / (2^32+1)
	// 2^64 = (2^32+1)(2^32-1) + 1, so quotient = 2^32-1, remainder = 1
	BigInt a2; a2.init_with_u128(1ull << 64);  // 2^64
	BigInt b2; b2.init_with_u128(4294967297ull);

	BigInt q2 = a2;
	q2.div_this(b2);
	BigInt r2 = a2;
	r2.mod_this(b2);

	assert(q2.equals(from_int(4294967295)));  // 2^32 - 1
	assert(r2.equals(from_int(1)));

	// Test 3: small exact multiple — 2*(2^32+1) / (2^32+1) = 2
	BigInt a3; a3.init_with_u128(8589934594ull);  // 2 * (2^32+1)
	BigInt b3; b3.init_with_u128(4294967297ull);

	BigInt q3 = a3;
	q3.div_this(b3);

	assert(q3.equals(from_int(2)));
	BigInt r3 = a3;
	r3.mod_this(b3);
	assert(r3.is_zero());

	// Test 4: self-consistency — (a * b) / b == a
	BigInt x; x.init_with_u128(123456789012345ull);
	BigInt divisor; divisor.init_with_u128(4294967311ull); // prime > 2^32
	BigInt product = x.mult(divisor);

	BigInt qc = product;
	qc.div_this(divisor);
	assert(qc.equals(x));

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
