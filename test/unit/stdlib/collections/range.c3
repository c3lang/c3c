module range_test @test;
import std::collections::range;

def IntRange = Range(<int>);
def IntExRange = ExclusiveRange(<int>);

fn void test_range()
{
	IntRange range = { -4, 2 };
	int sum = 0;
	foreach (int z : range)
	{
		assert(z >= -4 && z < 3);
		sum += z * z;
	}
	assert(sum == 35);
	assert(range.contains(-4));
	assert(range.contains(2));
	assert(!range.contains(3));
}

fn void test_exrange()
{
	IntExRange range = { -4, 2 };
	int sum = 0;
	foreach (int z : range)
	{
		assert(z >= -4 && z < 2);
		sum += z * z;
	}
	assert(sum == 31);
	assert(range.contains(-4));
	assert(range.contains(1));
	assert(!range.contains(2));
}

