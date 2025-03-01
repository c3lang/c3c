module std::math @test;

fn void test_frexp()
{
	int a;
	double z = math::frexp(231.23, &a);
	assert((z - 0.903242187) < 0.0000001 && a == 8);
	float z2 = math::frexp(231.23f, &a);
	assert((z2 - 0.903242187) < 0.0000001 && a == 8);
}

fn void test_signbit()
{
	assert(math::signbit(-231.3) == 1);
	assert(math::signbit(231.3) == 0);
	assert(math::signbit(float.inf) == 0);
	assert(math::signbit(-float.inf) == 1);
}
