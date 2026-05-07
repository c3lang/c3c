module signed_unsigned_comparisons @test;

fn void test_signed()
{
    int a = 0;
    int b = 1;

    bool ab_gt = a > b;
    bool ab_ge = a >= b;
    bool ab_le = a <= b;
    bool ab_lt = a < b;
    bool ab_ne = a != b;
    bool ab_eq = a == b;

	assert(!ab_gt);
	assert(!ab_ge);
	assert(ab_le);
	assert(ab_lt);
	assert(ab_ne);
	assert(!ab_eq);
}

fn void test_unsigned()
{
    uint a = 0;
    uint b = 1;

    bool ab_gt = a > b;
    bool ab_ge = a >= b;
    bool ab_le = a <= b;
    bool ab_lt = a < b;
    bool ab_ne = a != b;
    bool ab_eq = a == b;

	assert(!ab_gt);
	assert(!ab_ge);
	assert(ab_le);
	assert(ab_lt);
	assert(ab_ne);
	assert(!ab_eq);

}
