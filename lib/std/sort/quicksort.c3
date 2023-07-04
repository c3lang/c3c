module std::sort;

/**
 * @require is_searchable(list) "The list must be indexable and support .len or .len()"
 **/
macro quicksort(list, $Type)
{
    (($Type)(list)).sort(null);
}

/**
 * @require is_searchable(list) "The list must be indexable and support .len or .len()"
 * @require is_comparer(cmp, list) "Expected a comparison function which compares values"
 **/
macro quicksort_with(list, $Type, cmp)
{
    (($Type)(list)).sort(cmp);
}

module std::sort::quicksort<Type, Comparer>;
import std::sort;

def Quicksort = distinct Type[];

fn void Quicksort.sort(qs, Comparer cmp)
{
    usz len = sort::@len_from_list(qs);
    qs.qsort(0, (isz)len - 1, cmp);
}

fn void Quicksort.qsort(Quicksort qs, isz low, isz high, Comparer cmp) @private
{
    if (low < high)
    {
        isz p = qs.partition(low, high, cmp);
        qs.qsort(low, p - 1, cmp);
        qs.qsort(p + 1, high, cmp);
    }
}

fn isz Quicksort.partition(qs, isz low, isz high, Comparer cmp) @inline @private
{
    Type pivot = qs[high];
    isz i = low - 1;
    for (isz j = low; j < high; j++)
    {
        $if $checks(cmp(qs[0], qs[0])):
            int res = cmp(qs[j], pivot);
        $else
            $if $checks(cmp(&qs[0], &qs[0])):
                int res = cmp(&qs[j], &pivot);
            $else
                int res;
                if (greater(qs[j], pivot)) res = 1;
            $endif
        $endif
        if (res <= 0)
        {
            i++;
            @swap(qs[i], qs[j]);
        }
    }
    i++;
    @swap(qs[i], qs[high]);
    return i;
}