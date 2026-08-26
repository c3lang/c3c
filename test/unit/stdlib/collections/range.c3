module range_test @test;
import std::collections::range;

alias IntRange = Range{int};
alias IntExRange = ExclusiveRange{int};

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
	assert(range.to_array(tmem) == (int[]){ -4, -3, -2, -1, 0, 1, 2 });
	assert((IntRange){ 2, -4 }.to_array(tmem) == (int[]){});

	range = { 2, 4 };
	int[] array = { 5, 6, 9, 1, 2, 8, 4 };
	assert(range::slice(array, range) == { 9, 1, 2 });

	// Full-array and single-element slices
	assert(range::slice(array, (IntRange){ 0, 6 }) == array);
	assert(range::slice(array, (IntRange){ 3, 3 }) == { 1 });

	// Canonical empty range (end == start - 1) is valid and slices to nothing
	assert((IntRange){ 2, 1 }.is_valid());
	assert(range::slice(array, (IntRange){ 2, 1 }) == {});

	// Deeper reversed ranges are not valid
	assert(!(IntRange){ 5, 2 }.is_valid());
	assert(!(IntRange){ 2, 0 }.is_valid());
	assert((IntRange){ 1, 2 }.is_valid());
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
	assert(range.to_array(tmem) == (int[]){ -4, -3, -2, -1, 0, 1 });
	assert((IntExRange){ 2, 2 }.to_array(tmem) == (int[]){});

	range = { 2, 4 };
	int[] array = { 5, 6, 9, 1, 2, 8, 4 };
	assert(range::slice(array, range) == { 9, 1 });

	// Full-array and single-element slices
	assert(range::slice(array, (IntExRange){ 0, 7 }) == array);
	assert(range::slice(array, (IntExRange){ 3, 4 }) == { 1 });

	// Empty range (start == end) is valid and slices to nothing
	assert((IntExRange){ 3, 3 }.is_valid());
	assert(range::slice(array, (IntExRange){ 3, 3 }) == {});

	// Reversed ranges are not valid
	assert(!(IntExRange){ 3, 2 }.is_valid());
	assert((IntExRange){ 1, 2 }.is_valid());
}
