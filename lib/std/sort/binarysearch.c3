module std::sort;

/**
 * Perform a binary search over the sorted array and return the index
 * in [0, array.len) where x would be inserted or cmp(i) is true and cmp(j) is true for j in [i, array.len).
 * @require $defined(list[0]) && $defined(list.len) "The list must be indexable"
 * @require $or($typeof(cmp).typeid == void*.typeid, @is_comparer(cmp, list)) "Expected a comparison function which compares values"
 **/
macro usz binarysearch(list, x, cmp = null) @builtin
{
	usz i;
	usz len = @len_from_list(list);
	for (usz j = len; i < j;)
	{
		usz half = i + (j - i) / 2;
		$if $typeof(cmp).typeid == void*.typeid:
			switch
			{
				case greater(list[half], x): j = half;
				case less(list[half], x): i = half + 1;
				default: return half;
			}
		$else
			$switch
				$case $typeof(cmp).params[0] == $typeof(list[0]).typeid:
					int res = cmp(list[half], x);
				$default:
					int res = cmp(&list[half], &x);
			$endswitch
			switch
			{
				case res > 0: j = half;
				case res < 0: i = half + 1;
				default: return half;
			}
		$endif
	}
	return i;
}