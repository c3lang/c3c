module std::sort;
import std::sort::sorted;

<*
Returns true if list is sorted in either ascending or descending order.
 @require @is_sortable(list) "The list must be indexable and support .len or .len()"
 @require @is_valid_cmp_fn(cmp, list, context) "Expected a comparison function which compares values"
 @require @is_valid_context(cmp, context) "Expected a valid context"
*>
macro bool is_sorted(list, cmp = EMPTY_MACRO_SLOT, context = EMPTY_MACRO_SLOT) @builtin
{
	usz len = sort::@len_from_list(list);
	if (len <= 1) return true;
	return sorted::is_sorted(<$typeof(list), $typeof(cmp), $typeof(context)>)(list, 0, (isz)len - 1, cmp, context);
}

module std::sort::sorted(<Type, CmpFn, Context>);

macro int @cmp_fn(a, b, list, cmp, context)
{
	var $has_cmp = @is_valid_macro_slot(cmp);
	var $has_context = @is_valid_macro_slot(context);
	var $cmp_by_value = $has_cmp &&& $assignable(list[0], $typefrom(CmpFn.paramsof[0].type));

	$switch
		$case $cmp_by_value && $has_context:
			return cmp(a, b);
		$case $cmp_by_value:
			return cmp(a, b);
		$case $has_cmp && $has_context:
			return cmp(&a, &b, context);
		$case $has_cmp:
			return cmp(&a, &b);
		$default:
			return compare_to(a,b);
	$endswitch
}

fn bool is_sorted(Type list, isz start, isz end, CmpFn cmp, Context ctx)
{
	if (start < 0 || end < 0 || start >= end)
	{
		return false;
	}

	// determine sort order (ascending or descending)
	int sort_order;
	for (usz i = start; i < end && sort_order == 0; i++)
	{
		sort_order = @cmp_fn(list[i], list[i+1], list, cmp, ctx);
	}

	// no sort order found, all elements are the same, consider list sorted
	if (sort_order == 0) return true;

	// compare adjacent elements to the determined sort order
	for (usz i = start; i < end; i++)
	{
		switch (@cmp_fn(list[i], list[i+1], list, cmp, ctx))
		{
			case 0:
			case sort_order:
				continue;
			default:
				return false;
		}
	}
	return true;
}
