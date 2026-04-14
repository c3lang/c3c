module std::sort;

<*
 Sort list using the mergesort algorithm.

 @require @list_is_by_ref(list) : "Expected a list passed by reference, or slice passed by value"
 @require @is_sortable(list) : "The list must be indexable and support .len or .len()"
 @require @is_valid_cmp_fn(#cmp: ...cmp, #list: list, #context: ...context) : "Expected a comparison function which compares values"
 @require @is_valid_context(...cmp, ...context) : "Expected a valid context"
*>
macro void mergesort(list, cmp = ..., context = ...) @builtin
{
	var used_cmp = $defined(cmp) ??? cmp : (TypeNotSet)null;
	var used_ctx = $defined(context) ??? context : (TypeNotSet)null;
	$if $kindof(list) == SLICE:
		msort{$typeof(list), $typeof(used_cmp), $typeof(used_ctx)}(list, used_cmp, used_ctx);
	$else
		msort{$typeof(*list), $typeof(used_cmp), $typeof(used_ctx)}(list, used_cmp, used_ctx);
	$endif
}

module std::sort <Type, CmpFn, Context> @private;

fn void msort(ListType list, CmpFn cmp, Context context)
{
	if (list.len <= 1) return;
	ListType buff = mem::new_array(Type.inner, list.len);
	msort_split(list, buff, 0, list.len, cmp, context);
	free(buff);
	return;
}

<*
@require left <= right : "Indexing has to be done from lower to higher index"
*>
fn void msort_split(ListType list, Type buff, sz left, sz right, CmpFn cmp, Context context)
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
fn void merge(ListType list, ListType buff, sz left, sz mid, sz right, CmpFn cmp, Context context)
{
	var $has_cmp = $typeof(cmp) != sort::TypeNotSet;
	var $has_context = $typeof(context) != sort::TypeNotSet;
	var $cmp_by_value = $has_cmp &&& $defined($typefrom(CmpFn.paramsof[0].type) v = list_get(list, 0));
	var $has_get_ref = IS_SLICE ||| $defined(&(*list)[0]);

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
				bool cmp_result = cmp(*list_get_ref(list, i), *list_get_ref(list, j), context) <= 0;
			$case $has_cmp:
				bool cmp_result = cmp(*list_get_ref(list, i), *list_get_ref(list, j)) <= 0;
			$default:
				bool cmp_result = less_eq(list_get(list, i), list_get(list, j));
		$endswitch
		if (cmp_result)
		{
			$switch:
				$case $has_cmp:
					list_set(buff, k++, *list_get_ref(list, i++));
				$default:
					list_set(buff, k++, list_get(list, i++));
			$endswitch
		} else
		{
			$switch:
				$case $has_cmp:
					list_set(buff, k++, *list_get_ref(list, j++));
				$default:
					list_set(buff, k++, list_get(list, j++));
			$endswitch
		}
	}

	$switch:
		$case $has_cmp:
			while (i < mid) list_set(buff, k++, *list_get_ref(list, i++));
			while (j < right) list_set(buff, k++, *list_get_ref(list, j++));
		$default:
			while (i < mid) list_set(buff, k++, list_get(list, i++));
			while (j < right) list_set(buff, k++, list_get(list, j++));	
	$endswitch

	mem::copy(&list[left], &buff[left], (right - left) * $sizeof(list[0]));
}
