module std::sort;

<*
 Sort list using the mergesort algorithm.

 @require @list_is_by_ref(list) : "Expected a list passed by reference, or slice passed by value"
 @require @is_sortable(list) : "The list must be indexable and support .len or .len()"
 @require @is_valid_cmp_fn(#cmp: ...cmp, #list: list, #context: ...context) : "Expected a comparison function which compares values"
 @require @is_valid_context(...cmp, ...context) : "Expected a valid context"

 @param list : "The list to sort"
 @param cmp : "The comparison function"
 @param context : "The context to use with the comparion function as needed"
 @param buffer_allocator : "The allocator to use for allocating a buffer used by merge. Defaults to tmem."
*>
macro void mergesort(list, cmp = ..., context = ..., buffer_allocator = tmem) @builtin
{
	var used_cmp = $defined(cmp) ??? cmp : (TypeNotSet)null;
	var used_ctx = $defined(context) ??? context : (TypeNotSet)null;
	$if @kindof(list) == SLICE:
		msort{$typeof(list), $typeof(used_cmp), $typeof(used_ctx)}(list, used_cmp, used_ctx, buffer_allocator);
	$else
		msort{$typeof(*list), $typeof(used_cmp), $typeof(used_ctx)}(list, used_cmp, used_ctx, buffer_allocator);
	$endif
}

module std::sort <Type, CmpFn, Context> @private;

fn void msort(ListType list, CmpFn cmp, Context context, Allocator allocator)
{
	const LIST_IS_REF = @kindof(list) == POINTER;
	sz len = !LIST_IS_REF ??? lengthof(list) : lengthof(*list);
	if (len <= 1) return;
	ElementType[] buff = alloc::new_array(allocator, ElementType, len);
	msort_split(list, buff, 0, len, cmp, context);
	alloc::free(allocator, buff);
	return;
}

<*
@require left <= right : "Indexing has to be done from lower to higher index"
*>
fn void msort_split(ListType list, ElementType[] buff, sz left, sz right, CmpFn cmp, Context context)
{
	if (right - left <= 1) return;
	sz mid = left + (right - left) / 2;
	msort_split(list, buff, left, mid, cmp, context);
	msort_split(list, buff, mid, right, cmp, context);
	merge(list, buff, left, mid, right, cmp, context);
}

<*
@require left <= mid : "Indexing has to be done from lower to higher index"
@require mid <= right : "Indexing has to be done from lower to higher index"
*>
fn void merge(ListType list, ElementType[] buff, sz left, sz mid, sz right, CmpFn cmp, Context context)
{
	var $has_cmp = $typeof(cmp) != TypeNotSet;
	var $has_context = $typeof(context) != TypeNotSet;
	var $cmp_by_value = $has_cmp &&& $defined($typefrom(CmpFn::params[0].type) v = list_get(list, 0));

	sz i = left;
	sz j = mid;
	sz k = left;
	while(i < mid && j < right){
		$switch:
			$case $cmp_by_value && $has_context:
				bool cmp_result = cmp(list_get(list, i), list_get(list, j), context) <= 0;
			$case $cmp_by_value:
				bool cmp_result = cmp(list_get(list, i), list_get(list, j)) <= 0;
			$case $has_cmp && $has_context:
				bool cmp_result = cmp(list_get_ref(list, i), list_get_ref(list, j), context) <= 0;
			$case $has_cmp:
				bool cmp_result = cmp(list_get_ref(list, i), list_get_ref(list, j)) <= 0;
			$default:
				bool cmp_result = less_eq(list_get(list, i), list_get(list, j));
		$endswitch
		if (cmp_result)
		{
			buff[k++] = list_get(list, i++);
		} else
		{
			buff[k++] = list_get(list, j++);
		}
	}

	while (i < mid) buff[k++] = list_get(list, i++);
	while (j < right) buff[k++] = list_get(list, j++);
	mem::copy(list_get_ref(list, left), &buff[left], (right - left) * @sizeof(list_get(list, 0)));
}
