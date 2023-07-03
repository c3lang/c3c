module std::sort::binarysearch;

/*
 * Compare x and y and return one of the following values:
 *  -1 if x < y
 *  0 if x == y
 *  1 if x > y
 */
def Comparer = fn int (void *x, void *y);

/**
 * Perform a binary search over the sorted array and return the smallest index
 * in [0, array.len) where cmp(i) is true and cmp(j) is true for j in [i, array.len).
 * @require is_array_or_subarray(array) "array must be an array or subarray"
 **/
macro usz cmp_search(array, x, Comparer cmp)
{
    usz i;
    for (usz j = array.len; i < j;)
    {
        usz half = (i + j) / 2;
        int res = cmp(&array[half], &x);
        switch {
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
 * @require is_array_or_subarray(array) "array must be an array or subarray"
 **/
macro usz search(array, x)
{
    usz i;
    for (usz j = array.len; i < j;)
    {
        usz half = (i + j) / 2;
        switch {
            case greater(array[half], x): j = half;
            case less(array[half], x): i = half + 1;
            default: return half;
        }
    }
    return i;
}

macro bool is_array_or_subarray(slice) @private
{
    $switch ($typeof(slice).kindof)
    $case POINTER:
        var $Inner = $typefrom($typeof(bytes).inner);
        $if $Inner.kindof == ARRAY:
            return true;
        $endif
    $case ARRAY:
    $case SUBARRAY:
        return true;
    $default:
	    return false;
    $endswitch
}