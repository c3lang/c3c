module std::sort;

<*
 Returns true if list is sorted in either ascending or descending order.

 @require @is_any_sortable(list) : "The list must be indexable and support .len or .len()"
 @require @is_any_valid_cmp_fn(#cmp: ...cmp, #list: list, #context: ...ctx) : "Expected a comparison function which compares values"
 @require @is_valid_context(...cmp, ...ctx) : "Expected a valid context"
*>
macro bool is_sorted(list, cmp = ..., ctx = ...) @builtin
{
	// When the context or cmp functions are not defined, we can simply use a dummy/default type.
	const LIST_IS_REF = $kindof(list) == POINTER;
	var $Type = !LIST_IS_REF ??? $typeof(list) : $typeof(*list);

	usz len = !LIST_IS_REF ??? lengthof(list) : lengthof(*list);
	if (len <= 1) return true;

	bool is_sorted = false;
	if CHECK_SORT: (true)
	{
		usz i = 0;
		int sort_order;
		// determine sort order (ascending or descending)
		for (; i < (len - 1) && sort_order == 0; i++)
		{
			sort_order = @sort_cmp(list, i, ...cmp, ...ctx);
		}
		// no sort order found, all elements are the same, consider list sorted
		if (sort_order == 0)
		{
			is_sorted = true;
			break CHECK_SORT;
		}
		// compare adjacent elements to the sort order
		for (; i < (len - 1); i++)
		{
			if (sort_order * @sort_cmp(list, i, ...cmp, ...ctx) < 0) break CHECK_SORT;
		}
		is_sorted = true;
	}

	return is_sorted;
}

macro int @sort_cmp(list_maybe_ref, pos, cmp = ..., ctx = ...) @local
{
	var list = $kindof(list_maybe_ref) != POINTER ??? list_maybe_ref : *list_maybe_ref;
	var $has_cmp = $defined(cmp);
	var $has_context = $defined(ctx);
	var $cmp_by_value = $has_cmp &&& $defined($typefrom($typeof(cmp).paramsof[0].type) v = list[0]);

	var a = list[pos];
	var b = list[pos+1];

	$switch:
		$case $cmp_by_value && $has_context:
			return cmp(a, b);
		$case $cmp_by_value:
			return cmp(a, b);
		$case $has_cmp && $has_context:
			return cmp(&a, &b, ctx);
		$case $has_cmp:
			return cmp(&a, &b);
		$default:
			return compare_to(a,b);
	$endswitch
}
