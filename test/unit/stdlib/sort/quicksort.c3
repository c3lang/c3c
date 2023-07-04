module sort_test @test;
import std::sort;
import std::sort::quicksort;

def qs_int = quicksort::sort<int[]>;

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
        qs_int(tc);
        assert(sort::check_int_sort(tc));
    }
}

def Cmp = fn int(int*, int*);
def qs_int_ref = quicksort::sort_ref_fn<int[]>;

fn void quicksort_with()
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
        qs_int_ref(tc, (Cmp)&sort::cmp_int);
        assert(sort::check_int_sort(tc));
    }
}

def qs_int_fn = quicksort::sort_fn<int[]>;

fn void quicksort_with2()
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
        qs_int_fn(tc, &sort::cmp_int2);
        assert(sort::check_int_sort(tc));
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
        qs_int_fn(tc, fn int(int a, int b) => a - b);
        assert(sort::check_int_sort(tc));
    }
}

module std::sort;

fn bool check_int_sort(int[] list)
{
    int prev = int.min;
    foreach (x : list)
    {
        if (prev > x) return false;
        prev = x;
    }
    return true;
}