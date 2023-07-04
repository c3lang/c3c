module std::sort::quicksort<Type>;
import std::sort;

def ElementType = $typeof(Type{}[0]);
def Comparer = fn int(ElementType, ElementType);
def ComparerRef = fn int(ElementType*, ElementType*);

const bool ELEMENT_COMPARABLE = $checks(ElementType x, greater(x, x));

fn void sort_fn(Type list, Comparer cmp)
{
    usz len = sort::@len_from_list(list);
    qsort_value(list, 0, (isz)len - 1, cmp);
}

fn void sort_ref_fn(Type list, ComparerRef cmp)
{
    usz len = sort::@len_from_list(list);
    qsort_ref(list, 0, (isz)len - 1, cmp);
}

fn void sort(Type list) @if(ELEMENT_COMPARABLE)
{
    usz len = sort::@len_from_list(list);
    qsort(list, 0, (isz)len - 1);
}

fn void qsort(Type list, isz low, isz high) @local @if(ELEMENT_COMPARABLE)
{
    if (low < high)
    {
        isz p = partition(list, low, high);
        qsort(list, low, p - 1);
        qsort(list, p + 1, high);
    }
}

fn void qsort_value(Type list, isz low, isz high, Comparer cmp) @local
{
    if (low < high)
    {
        isz p = partition_value(list, low, high, cmp);
        qsort_value(list, low, p - 1, cmp);
        qsort_value(list, p + 1, high, cmp);
    }
}

fn void qsort_ref(Type list, isz low, isz high, ComparerRef cmp) @local
{
    if (low < high)
    {
        isz p = partition_ref(list, low, high, cmp);
        qsort_ref(list, low, p - 1, cmp);
        qsort_ref(list, p + 1, high, cmp);
    }
}

fn isz partition(Type list, isz low, isz high) @inline @local
{
    ElementType pivot = list[high];
    isz i = low - 1;
    for (isz j = low; j < high; j++)
    {
        if (greater(list[j], pivot)) continue;
        i++;
        @swap(list[i], list[j]);
    }
    i++;
    @swap(list[i], list[high]);
    return i;
}

fn isz partition_value(Type list, isz low, isz high, Comparer cmp) @inline @private
{
    ElementType pivot = list[high];
    isz i = low - 1;
    for (isz j = low; j < high; j++)
    {
        if (cmp(list[j], pivot) <= 0)
        {
            i++;
            @swap(list[i], list[j]);
        }
    }
    i++;
    @swap(list[i], list[high]);
    return i;
}

fn isz partition_ref(Type list, isz low, isz high, ComparerRef cmp) @inline @private
{
    ElementType* pivot = &list[high];
    isz i = low - 1;
    for (isz j = low; j < high; j++)
    {
        if (cmp(&list[j], pivot) <= 0)
        {
            i++;
            @swap(list[i], list[j]);
        }
    }
    i++;
    @swap(list[i], list[high]);
    return i;
}