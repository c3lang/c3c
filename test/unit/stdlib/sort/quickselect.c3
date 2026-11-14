module sort_test @test;
import std::sort;

struct TestCase @local
{
	int[] list;
	sz k;
	int want;
}

fn void quickselect()
{
    TestCase[] tcases = {
		{
			.list = {3, 4, 1},
			.k = 0,
			.want = 1,
		},
		{
			.list = {3, 4, 1},
			.k = 1,
			.want = 3,
		},
		{
			.list = {3, 4, 1},
			.k = 2,
			.want = 4,
		},
		{
			.list = {3, 2, 4, 1},
			.k = 1,
			.want = 2,
		},
		{
			.list = {3, 2, 1, 2},
			.k = 1,
			.want = 2,
		},
		{
			.list = {3, 2, 1, 2},
			.k = 2,
			.want = 2,
		},
		{
			.list = {3, 2, 1, 2},
			.k = 3,
			.want = 3,
		},
		{
			// single element list
			.list = {5},
			.k = 0,
			.want = 5,
		},
		{
			// sorted list longer than 64 elements
			.list = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69},
			.k = 69,
			.want = 69,
		},
	};

    foreach (i, tc : tcases)
    {
		int got = sort::select_nth(tc.list, tc.k);
		assert(got == tc.want, "test %d: got %d, want %d", i, got, tc.want);
    }
}

