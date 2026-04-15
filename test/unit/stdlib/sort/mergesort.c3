module sort_test @test;
import sort::check;
import std::sort;
import std::collections::list;

fn void mergesort()
{
	int[][] tcases = {
		{},
		{10, 3},
		{3, 2, 1},
		{1, 2, 3},
		{2, 1, 3},
	};

	foreach (tc : tcases)
	{
		sort::mergesort(tc);
		assert(check::int_sort(tc));
	}
}

fn void mergesort_with_ref()
{
	int[][] tcases = {
		{},
		{10, 3},
		{3, 2, 1},
		{1, 2, 3},
		{2, 1, 3},
	};

	foreach (tc : tcases)
	{
		sort::mergesort(tc, &sort::cmp_int_ref);
		assert(check::int_sort(tc));
	}
}

fn void mergesort_with_value()
{
	int[][] tcases = {
		{},
		{10, 3},
		{3, 2, 1},
		{1, 2, 3},
		{2, 1, 3},
	};

	foreach (tc : tcases)
	{
		sort::mergesort(tc, &sort::cmp_int_value);
		assert(check::int_sort(tc));
	}
}

fn void mergesort_with_array()
{
	int[*] a = { 4, 8, 100, 1, 2 };
	sort::mergesort(&a);
	assert(a == { 1, 2, 4, 8, 100 });
}

fn void mergesort_with_lambda()
{
	int[][] tcases = {
		{},
		{10, 3},
		{3, 2, 1},
		{1, 2, 3},
		{2, 1, 3},
	};

	foreach (tc : tcases)
	{
		sort::mergesort(tc, fn int(int a, int b) => a - b);
		assert(check::int_sort(tc));
	}
}

alias MergeSortTestList = List{int};

fn void mergesort_list()
{
	MergeSortTestList list;
	list.push_all({ 2, 1, 3});
	sort::mergesort(&list, &sort::cmp_int_value);
	assert(check::int_sort(list.array_view()));
}