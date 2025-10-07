module std::sort;

<*
 Returns true if list is sorted in either ascending or descending order.

 @require @is_any_sortable(list) : "The list must be indexable and support .len or .len()"
 @require @is_any_valid_cmp_fn(cmp, list, ctx) : "Expected a comparison function which compares values"
 @require @is_valid_context(cmp, ctx) : "Expected a valid context"
*>
macro bool is_sorted(list, cmp = EMPTY_MACRO_SLOT, ctx = EMPTY_MACRO_SLOT) @builtin
{
	$if ($kindof(list) != POINTER):
	var $Type = $typeof(list);
	usz len = lengthof(list);
	if (len <= 1) return true;
	var check_sort = fn bool($Type list, usz start, usz end, $typeof(cmp) cmp, $typeof(ctx) ctx)
	{
		usz i;
		int sort_order;

		// determine sort order (ascending or descending)
		for (i = start; i < end && sort_order == 0; i++)
		{
			sort_order = @sort_cmp_slice(list, i, cmp, ctx);
		}

		// no sort order found, all elements are the same, consider list sorted
		if (sort_order == 0) return true;

		// compare adjacent elements to the sort order
		for (; i < end; i++)
		{
			if (sort_order * @sort_cmp_slice(list, i, cmp, ctx) < 0) return false;
		}
		return true;
	};

	return check_sort(list, 0, len - 1, cmp, ctx);
	$else
	var $Type = $typeof(*list);
	usz len = lengthof(*list);
	if (len <= 1) return true;
	var check_sort = fn bool($Type* list, usz start, usz end, $typeof(cmp) cmp, $typeof(ctx) ctx)
	{
		usz i;
		int sort_order;

		// determine sort order (ascending or descending)
		for (i = start; i < end && sort_order == 0; i++)
		{
			sort_order = @sort_cmp(list, i, cmp, ctx);
		}

		// no sort order found, all elements are the same, consider list sorted
		if (sort_order == 0) return true;

		// compare adjacent elements to the sort order
		for (; i < end; i++)
		{
			if (sort_order * @sort_cmp(list, i, cmp, ctx) < 0) return false;
		}
		return true;
	};
	return check_sort(list, 0, len - 1, cmp, ctx);
	$endif
}

macro int @sort_cmp(list, pos, cmp, ctx) @local
{
	var $has_cmp = @is_valid_macro_slot(cmp);
	var $has_context = @is_valid_macro_slot(ctx);
	var $cmp_by_value = $has_cmp &&& $defined($typefrom($typeof(cmp).paramsof[0].type) v = (*list)[0]);

	var a = (*list)[pos];
	var b = (*list)[pos+1];

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

macro int @sort_cmp_slice(list, pos, cmp, ctx) @local
{
	var $has_cmp = @is_valid_macro_slot(cmp);
	var $has_context = @is_valid_macro_slot(ctx);
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
