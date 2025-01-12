module sort_test @test;
import std::sort;

struct TestCase @local
{
	int[] list;
	isz k;
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
	};

    foreach (i, tc : tcases)
    {
        if (try got = sort::quickselect(tc.list, tc.k))
	{
		assert(got == tc.want, "got: %d, want %d", got, tc.want);
	}
	else
	{
		assert(false, "test %d failed", i);
	}
    }
}

