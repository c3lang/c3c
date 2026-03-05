module sort_test @test;
import std::math;
import std::sort;
import sort::check;
import std::collections::list;

fn void countingsort()
{
	int[][] tcases = {
	    {},
	    {[0..128] = 10, [129..192] = 3},
	    {[0..128] = 3, [129..192] = 2, [193..200] = 1},
	    {[0..128] = 1, [129..192] = 2, [193..200] = 3},
	    {[0..128] = 2, [129..192] = 1, [193..200] = 3},
	};

	foreach (tc : tcases)
	{
		sort::countingsort(tc);
		assert(check::int_ascending_sort(tc));
	}
}

fn void countingsort_with_ref()
{
	int[][] tcases = {
	    {},
	    {[0..128] = 10, [129..192] = 3},
	    {[0..128] = 3, [129..192] = 2, [193..200] = 1},
	    {[0..128] = 1, [129..192] = 2, [193..200] = 3},
	    {[0..128] = 2, [129..192] = 1, [193..200] = 3},
	};

    foreach (tc : tcases)
    {
		sort::countingsort(tc, &sort::key_int_ref);
		assert(check::int_ascending_sort(tc));
    }
}

fn void countingsort_with_value()
{
	 int[][] tcases = {
		 {},
		 {[0..128] = 10, [129..192] = 3},
		 {[0..128] = 3, [129..192] = 2, [193..200] = 1},
		 {[0..128] = 1, [129..192] = 2, [193..200] = 3},
		 {[0..128] = 2, [129..192] = 1, [193..200] = 3},
	 };

	 foreach (tc : tcases)
	 {
		 sort::countingsort(tc, &sort::key_int_value);
		 assert(check::int_ascending_sort(tc));
	 }
}

fn void countingsort_with_lambda()
{
	int[][] tcases =
	{
	    {},
	    {[0..128] = 10, [129..192] = 3},
	    {[0..128] = 3, [129..192] = 2, [193..200] = 1},
	    {[0..128] = 1, [129..192] = 2, [193..200] = 3},
	    {[0..128] = 2, [129..192] = 1, [193..200] = 3},
	};

	foreach (tc : tcases)
	{
		sort::countingsort(tc, fn uint(int a) => ((uint)(a + int.min)));
		assert(check::int_ascending_sort(tc));
	}
}

alias CountingSortTestList = List{int};

fn void countingsort_list()
{
	CountingSortTestList list;
	list.push_all({ 2, 1, 3});
	sort::countingsort(&list, &sort::key_int_value);
	assert(check::int_ascending_sort(list.array_view()));
}

fn void countingsort_random_large_list()
{
	Lcg128Random random;
	random::seed_entropy(&random);

	CountingSortTestList list;
	for (usz i = 0; i < 2048; i++)
	{
		list.push(random.next_int());
	}

	sort::countingsort(&list, &sort::key_int_value);
	assert(check::int_ascending_sort(list.array_view()));
	list.free();
}