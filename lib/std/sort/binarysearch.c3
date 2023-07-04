module std::sort;

/**
 * Perform a binary search over the sorted array and return the smallest index
 * in [0, array.len) where cmp(i) is true and cmp(j) is true for j in [i, array.len).
 * @require is_searchable(list) "The list must be indexable and support .len or .len()"
 * @require is_comparer(cmp, list) "Expected a comparison function which compares values"
 **/
macro usz binarysearch_with(list, x, cmp)
{
    usz i;
    usz len = @len_from_list(list);
    for (usz j = len; i < j;)
    {
        usz half = (i + j) / 2;
        $if $checks(cmp(list[0], list[0])):
            int res = cmp(list[half], x);
        $else
            int res = cmp(&list[half], &x);
        $endif
        switch
        {
            case res > 0: j = half;
            case res < 0: i = half + 1;
            default: return half;
        }
    }
    return i;
}

/**
 * Perform a binary search over the sorted array and return the index
 * in [0, array.len) where x would be inserted.
 * @require is_searchable(list) "The list must be indexable and support .len or .len()"
 * @checked less(list[0], x) "The values must be comparable"
 **/
macro usz binarysearch(list, x)
{
    usz i;
    usz len = @len_from_list(list);
    for (usz j = len; i < j;)
    {
        usz half = (i + j) / 2;
        switch {
            case greater(list[half], x): j = half;
            case less(list[half], x): i = half + 1;
            default: return half;
        }
    }
    return i;
}