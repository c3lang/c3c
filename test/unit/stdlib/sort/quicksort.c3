module sort_test @test;
import std::sort;
import sort::check;
import std::collections::list;

fn void quicksort()
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
        sort::quicksort(tc);
        assert(check::int_sort(tc));
    }
}

fn void quicksort_with_ref()
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
        sort::quicksort(tc, &sort::cmp_int_ref);
        assert(check::int_sort(tc));
    }
}

fn void quicksort_with_array()
{
	int[*] a = { 4, 8, 100, 1, 2 };
	sort::quicksort(&a);
	assert(a == { 1, 2, 4, 8, 100 });
}

fn void quicksort_with_value()
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
        sort::quicksort(tc, &sort::cmp_int_value);
        assert(check::int_sort(tc));
    }
}

fn void quicksort_with_lambda()
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
        sort::quicksort(tc, fn int(int a, int b) => a - b);
        assert(check::int_sort(tc));
    }
}

def List = List(<int>);

fn void quicksort_list()
{
    List list;
    list.temp_init();
    list.add_array({ 2, 1, 3});
    sort::quicksort(list, &sort::cmp_int_value);
    assert(check::int_sort(list.array_view()));
}

module sort::check;

fn bool int_sort(int[] list)
{
    int prev = int.min;
    foreach (x : list)
    {
        if (prev > x) return false;
        prev = x;
    }
    return true;
}