module sort_test @test;
import std::sort;
import std::collections::list;

struct TestCase @local
{
	int[] input;
	bool want;
}

fn void sorted()
{
	TestCase[] tcases = {
		{
		    .input = {},
		    .want = true,
		},
		{
		    .input = {1},
		    .want = true,
		},
		{
		    .input = {1,2},
		    .want = true,
		},
		{
		    .input = {2,1},
		    .want = true,
		},
		{
		    .input = {1,2,3},
		    .want = true,
		},
		{
		    .input = {1,2,1},
		    .want = false,
		},
		{
		    .input = {2,1,2},
		    .want = false,
		},
		{
		    .input = {3,2,1},
		    .want = true,
		},
		{
		    .input = {1,1,1,1,1,2},
		    .want = true,
		},
		{
		    .input = {2,2,2,2,2,1},
		    .want = true,
		},
		{
		    .input = {1,1,1,1,2,1},
		    .want = false,
		},
	};

	bool got;
	foreach (tc : tcases)
	{
		// default
		got = is_sorted(tc.input);
		assert(got == tc.want, "default: %s, got: %s, want: %s",
			tc.input, got, tc.want);
	
		// with list
		List(<int>) list;
		list.temp_init();
		list.add_array(tc.input);

		got = is_sorted(list);
		assert(got == tc.want, "list: %s, got: %s, want: %s",
			list.to_tstring(), got, tc.want);

		// with lambda
		got = is_sorted(tc.input, fn int(int a, int b) => a - b);
		assert(got == tc.want, "lambda: %s, got: %s, want: %s",
			tc.input, got, tc.want);

		// with value
		got = is_sorted(tc.input, &sort::cmp_int_value);
		assert(got == tc.want, "value: %s, got: %s, want: %s",
			tc.input, got, tc.want);

		// with ref
		got = is_sorted(tc.input, &sort::cmp_int_ref);
		assert(got == tc.want, "ref: %s, got: %s, want: %s",
			tc.input, got, tc.want);
	}
}
