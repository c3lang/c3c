module std::sort;

<*
 Sort list using the quick sort algorithm.

 @require @list_is_by_ref(list) : "Expected a list passed by reference or be a slice"
 @require @is_sortable(list) : "The list must be indexable and support .len or .len()"
 @require @is_valid_cmp_fn(#cmp: ...cmp, #list: list, #context: ...context) : "Expected a comparison function which compares values"
 @require @is_valid_context(...cmp, ...context) : "Expected a valid context"
*>
macro void quicksort(list, cmp = ..., context = ...) @builtin
{
	var used_cmp = $defined(cmp) ??? cmp : (TypeNotSet)null;
	var used_ctx = $defined(context) ??? context : (TypeNotSet)null;
	$if @kindof(list) == SLICE:
		qsort{$typeof(list), $typeof(used_cmp), $typeof(used_ctx)}(list, 0, (sz)list.len - 1, used_cmp, used_ctx);
	$else
		qsort{$typeof(*list), $typeof(used_cmp), $typeof(used_ctx)}(list, 0, (sz)lengthof(*list) - 1, used_cmp, used_ctx);
	$endif
}

<*
 Select the (k+1)th smallest element in an unordered list using Hoare's
 selection algorithm (Quickselect). k should be between 0 and len-1. The data
 list will be partially sorted.

 @require @list_is_by_ref(list) : "Expected a list passed by reference or be a slice"
 @require @is_sortable(list) : "The list must be indexable and support .len or .len()"
 @require @is_valid_cmp_fn(#cmp: ...cmp, #list: list, #context: ...context) : "expected a comparison function which compares values"
 @require @is_valid_context(...cmp, ...context) : "Expected a valid context"
*>
macro quickselect(list, sz k, cmp = ..., context = ...) @builtin
{
	var used_cmp = $defined(cmp) ??? cmp : (TypeNotSet)null;
	var used_ctx = $defined(context) ??? context : (TypeNotSet)null;
	$if @kindof(list) == SLICE:
		return qselect{$typeof(list), $typeof(used_cmp), $typeof(used_ctx)}(list, 0, (sz)list.len - 1, k, used_cmp, used_ctx);
	$else
		return qselect{$typeof(*list), $typeof(used_cmp), $typeof(used_ctx)}(list, 0, (sz)lengthof(*list) - 1, k, used_cmp, used_ctx);
	$endif
}


module std::sort <Type, CmpFn, Context> @private;

struct StackElementItem @private
{
	sz low;
	sz high;
}

alias Stack @private = StackElementItem[64];

// Based on https://alienryderflex.com/quicksort by Darel Rex Finley, Public Domain.

fn void qsort(ListType list, sz low, sz high, CmpFn cmp, Context context) @noinline
{
	if (low >= 0 && high >= 0 && low < high)
	{
		Stack stack;
		stack[0].low = low;
		stack[0].high = high;
		sz i;
		sz l;
		sz h;
		while (i >= 0)
		{
			l = stack[i].low;
			h = stack[i].high;

			if (l < h)
			{
				l = @partition(list, l, h, cmp, context);
				stack[i + 1].low = l + 1;
				stack[i + 1].high = stack[i].high;
				stack[i++].high = l;
				if (stack[i].high - stack[i].low > stack[i - 1].high - stack[i - 1].low)
				{
					@swap(stack[i], stack[i - 1]);
				}
			}
			else
			{
				i--;
			}
		}
	}
}

<*
@require low <= k : "kth smallest element is smaller than lower bounds"
@require k <= high : "kth smallest element is larger than upper bounds"
*>
fn ElementType? qselect(ListType list, sz low, sz high, sz k, CmpFn cmp, Context context) @noinline
{
	if (low >= 0 && high >= 0 && low < high)
	{
		sz l = low;
		sz h = high;
		sz pivot;

		sz max_retries = 64;
		while (l <= h && max_retries--)
		{
			pivot = @partition(list, l, h, cmp, context);
			if (k == pivot) return list_get(list, k);
			if (k < pivot)
			{
				h = pivot - 1;
			}
			else
			{
				l = pivot + 1;
			}
		}
	}
	return NOT_FOUND~;
}

macro sz @partition(ListType list, sz l, sz h, CmpFn cmp, Context context)
{
	var $has_cmp = $typeof(cmp) != sort::TypeNotSet;
	var $has_context = $typeof(context) != sort::TypeNotSet;
	var $cmp_by_value = $has_cmp &&& $defined($typefrom(CmpFn::params[0].type) v = list_get(list, 0));

	ElementType pivot = list_get(list, l);
	while (l < h)
	{
		$switch:
			$case $cmp_by_value && $has_context:
				while (cmp(list_get(list, h), pivot, context) >= 0 && l < h) h--;
				if (l < h) list_set(list, l++, list_get(list, h));
				while (cmp(list_get(list, l), pivot, context) <= 0 && l < h) l++;
			$case $cmp_by_value:
				while (cmp(list_get(list, h), pivot) >= 0 && l < h) h--;
				if (l < h) list_set(list, l++, list_get(list, h));
				while (cmp(list_get(list, l), pivot) <= 0 && l < h) l++;
			$case $has_cmp && $has_context:
				while (cmp(list_get_ref(list, h), &pivot, context) >= 0 && l < h) h--;
				if (l < h) list_set(list, l++, list_get(list, h));
				while (cmp(list_get_ref(list, l), &pivot, context) <= 0 && l < h) l++;
			$case $has_cmp:
				while (cmp(list_get_ref(list, h), &pivot) >= 0 && l < h) h--;
				if (l < h) list_set(list, l++, list_get(list, h));
				while (cmp(list_get_ref(list, l), &pivot) <= 0 && l < h) l++;
			$default:
				while (greater_eq(list_get(list, h), pivot) && l < h) h--;
				if (l < h) list_set(list, l++, list_get(list, h));
				while (less_eq(list_get(list, l), pivot) && l < h) l++;
		$endswitch
		if (l < h) list_set(list, h--, list_get(list, l));
	}
	list_set(list, l, pivot);

	return l;
}
