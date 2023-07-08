module std::sort;
import std::sort::qs;

macro quicksort(list, cmp = null)
{
    var $Type = $typeof(list);
    var $CmpType = $typeof(cmp);
    usz len = sort::@len_from_list(list);
    qs::qsort(<$Type, $CmpType>)(list, 0, (isz)len - 1, cmp);
}

module std::sort::qs(<Type, Comparer>);

def ElementType = $typeof(Type{}[0]);

fn void qsort(Type list, isz low, isz high, Comparer cmp)
{
    if (low < high)
    {
        isz p = partition(list, low, high, cmp);
        qsort(list, low, p - 1, cmp);
        qsort(list, p + 1, high, cmp);
    }
}

fn isz partition(Type list, isz low, isz high, Comparer cmp) @inline @local
{
    ElementType pivot = list[high];
    isz i = low - 1;
    for (isz j = low; j < high; j++)
    {
        $if $checks(cmp(list[0], list[0])):
            int res = cmp(list[j], pivot);
        $else
            $if $checks(cmp(&list[0], &list[0])):
                int res = cmp(&list[j], &pivot);
            $else
                int res;
                if (greater(list[j], pivot)) continue;
            $endif
        $endif
        if (res <= 0)
        {
            i++;
            @swap(list[i], list[j]);
        }
    }
    i++;
    @swap(list[i], list[high]);
    return i;
}