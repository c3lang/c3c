module std::sort;
import std::sort::qs;

<*
 Sort list using the quick sort algorithm.

 @require @list_is_by_ref(list) : "Expected a list passed by reference or be a slice"
 @require @is_sortable(list) : "The list must be indexable and support .len or .len()"
 @require @is_valid_cmp_fn(cmp, list, context) : "Expected a comparison function which compares values"
 @require @is_valid_context(cmp, context) : "Expected a valid context"
*>
macro void quicksort(list, cmp = EMPTY_MACRO_SLOT, context = EMPTY_MACRO_SLOT) @builtin
{
	$if $kindof(list) == SLICE:
		qs::qsort{$typeof(list), $typeof(cmp), $typeof(context)}(list, 0, (isz)list.len - 1, cmp, context);
	$else
		qs::qsort{$typeof(*list), $typeof(cmp), $typeof(context)}(list, 0, (isz)lengthof(*list) - 1, cmp, context);
	$endif
}

<*
 Select the (k+1)th smallest element in an unordered list using Hoare's
 selection algorithm (Quickselect). k should be between 0 and len-1. The data
 list will be partially sorted.

 @require @list_is_by_ref(list) : "Expected a list passed by reference or be a slice"
 @require @is_sortable(list) : "The list must be indexable and support .len or .len()"
 @require @is_valid_cmp_fn(cmp, list, context) : "expected a comparison function which compares values"
 @require @is_valid_context(cmp, context) : "Expected a valid context"
*>
macro quickselect(list, isz k, cmp = EMPTY_MACRO_SLOT, context = EMPTY_MACRO_SLOT) @builtin
{
	$if $kindof(list) == SLICE:
		return qs::qselect{$typeof(list), $typeof(cmp), $typeof(context)}(list, 0, (isz)list.len - 1, k, cmp, context);
	$else
		return qs::qselect{$typeof(*list), $typeof(cmp), $typeof(context)}(list, 0, (isz)lengthof(*list) - 1, k, cmp, context);
	$endif
}

module std::sort::qs{Type, CmpFn, Context};

alias ElementType = $typeof(((Type){})[0]);
const bool IS_SLICE = Type.kindof == SLICE;
alias ListType = $typefrom(IS_SLICE ??? Type : Type*);
macro list_get(ListType l, i) @if(!IS_SLICE) => (*l)[i];
macro list_get(ListType l, i) @if(IS_SLICE) => l[i];
macro list_get_ref(ListType l, i) @if(!IS_SLICE) => &(*l)[i];
macro list_get_ref(ListType l, i) @if(IS_SLICE) => &l[i];
macro list_set(ListType l, i, v) @if(!IS_SLICE) => (*l)[i] = v;
macro list_set(ListType l, i, v) @if(IS_SLICE) => l[i] = v;

struct StackElementItem @private
{
	isz low;
	isz high;
}

alias Stack @private = StackElementItem[64];

// Based on https://alienryderflex.com/quicksort by Darel Rex Finley, Public Domain.

fn void qsort(ListType list, isz low, isz high, CmpFn cmp, Context context)
{
	if (low >= 0 && high >= 0 && low < high)
	{
		Stack stack;
		stack[0].low = low;
		stack[0].high = high;
		isz i;
		isz l;
		isz h;
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
fn ElementType? qselect(ListType list, isz low, isz high, isz k, CmpFn cmp, Context context)
{
	if (low >= 0 && high >= 0 && low < high)
	{
		isz l = low;
		isz h = high;
		isz pivot;

		usz max_retries = 64;
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
	return NOT_FOUND?;
}

macro isz @partition(ListType list, isz l, isz h, CmpFn cmp, Context context)
{
	var $has_cmp = @is_valid_macro_slot(cmp);
	var $has_context = @is_valid_macro_slot(context);
	var $cmp_by_value = $has_cmp &&& $defined($typefrom(CmpFn.paramsof[0].type) v = list_get(list, 0));

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
