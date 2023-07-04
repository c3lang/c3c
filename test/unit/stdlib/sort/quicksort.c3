module sort_test @test;
import std::sort;
import std::sort::quicksort;

def QSInt = quicksort::Quicksort<int, void*>;

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
        sort::quicksort(tc, QSInt);
        assert(sort::check_int_sort(tc));
    }
}

def Cmp = fn int (void*, void*);
def QSIntCmp = quicksort::Quicksort<int, Cmp>;

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
        sort::quicksort_with(tc, QSIntCmp, &sort::cmp_int);
        assert(sort::check_int_sort(tc));
    }
}

def Cmp2 = fn int (int, int);
def QSIntCmp2 = quicksort::Quicksort<int, Cmp2>;

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
        sort::quicksort_with(tc, QSIntCmp2, &sort::cmp_int2);
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
        sort::quicksort_with(tc, QSIntCmp2, fn int(int a, int b) => a - b);
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