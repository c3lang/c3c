module range_test @test;
import std::collections::range;

def IntRange = Range<int>;
fn void! test_range()
{
	IntRange range = { -4, 2 };
	int sum = 0;
	foreach (int z : range)
	{
		assert(z >= -4 && z < 2);
		sum += z * z;
	}
	assert(sum == 31);
}