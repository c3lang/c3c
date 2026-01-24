module std::sort;

<*
Perform a binary search over the sorted array and return the index
in [0, array.len) where x would be inserted or cmp(i) is true and cmp(j) is true for j in [i, array.len).

 @require @list_is_by_ref(list) : "Expected a list passed by reference or a slice"
 @require @is_sortable(list) : "The list must be sortable"
 @require @is_valid_cmp_fn(#cmp: ...cmp, #list: list, #context: ...context) : "Expected a comparison function which compares values"
 @require @is_valid_context(...cmp, ...context) : "Expected a valid context"
*>
macro usz binarysearch(list, x, cmp = ..., context = ...) @builtin
{
	var used_cmp = $defined(cmp) ??? cmp : (TypeNotSet)null;
	var used_ctx = $defined(context) ??? context : (TypeNotSet)null;
	return _binarysearch{$typeof(list), $typeof(x), $typeof(used_cmp), $typeof(used_ctx)}(list, x, used_cmp, used_ctx);
}

fn usz _binarysearch(ListType list, XType x, CmpFnType cmp, ContextType context) <ListType, XType, CmpFnType, ContextType> @noinline @local
{
	usz i;
	var $no_cmp = $typeof(cmp) == TypeNotSet;
	var $has_context = $typeof(context) != TypeNotSet;

	$if $kindof(list) == SLICE:
		usz len = lengthof(list);
		for (usz j = len; i < j;)
		{
			usz half = i + (j - i) / 2;
			$if $no_cmp:
				switch
				{
					case greater(list[half], x): j = half;
					case less(list[half], x): i = half + 1;
					default: return half;
				}
			$else

				$switch:
					$case $defined(cmp(list[0], list[0], context)):
						int res = cmp(list[half], x, context);
					$case $defined(cmp(list[0], list[0])):
						assert(!$has_context);
						int res = cmp(list[half], x);
					$case $defined(cmp(&list[0], &list[0], context)):
						int res = cmp(&list[half], &x, context);
					$case $defined(cmp(&list[0], &list[0])):
						assert(!$has_context);
						int res = cmp(&list[half], &x);
					$default:
						assert(false, "Invalid comparison function");
				$endswitch
				switch
				{
					case res > 0: j = half;
					case res < 0: i = half + 1;
					default: return half;
				}
			$endif
		}
	$else
		usz len = lengthof(*list);
		for (usz j = len; i < j;)
		{
			usz half = i + (j - i) / 2;
			$if $no_cmp:
				switch
				{
					case greater((*list)[half], x): j = half;
					case less((*list)[half], x): i = half + 1;
					default: return half;
				}
			$else
				$switch:
					$case $defined(cmp((*list)[0], (*list)[0], context)):
						int res = cmp(list[half], x, context);
					$case $defined(cmp((*list)[0], (*list)[0])):
						assert(!$has_context);
						int res = cmp((*list)[half], x);
					$case $defined(cmp(&(*list)[0], &(*list)[0], context)):
						int res = cmp(&(*list)[half], &x, context);
					$case $defined(cmp(&(*list)[0], &(*list)[0])):
						assert(!$has_context);
						int res = cmp(&(*list)[half], &x);
					$default:
						assert(false, "Invalid comparison function");
				$endswitch
				switch
				{
					case res > 0: j = half;
					case res < 0: i = half + 1;
					default: return half;
				}
			$endif
		}
	$endif
	return i;
}