module std::sort;

<*
 Sort list using the mergesort algorithm.

 @require @list_is_by_ref(list) : "Expected a list passed by reference, or slice passed by value"
 @require @is_sortable(list) : "The list must be indexable and support .len or .len()"
*>
macro void mergesort(list) @builtin
{
	$if $kindof(list) == SLICE:
		msort{$typeof(list)}(list);
	$else
		msort{$typeof(*list)}(list);
	$endif
}

module std::sort <Type> @private;

fn void msort(Type list)
{
	if (list.len <= 1) return;
	Type buff = mem::new_array(Type.inner, list.len);
	msort_split(list, buff, 0, list.len);
	free(buff);
	return;
}

fn void msort_split(Type list, Type buff, sz left, sz right)
{
	if (right - left <= 1) return;
	sz mid = left + (right - left) / 2;
	msort_split(list, buff, left, mid);
	msort_split(list, buff, mid, right);
	merge(list, buff, left, mid, right);
}

fn void merge(Type list, Type buff, sz left, sz mid, sz right){
	sz i = left;
	sz j = mid;
	sz k = left;
	while(i < mid && j < right){
		if(list[i] <= list[j]) 
		{
			buff[k++] = list[i++];
		} else
		{
			buff[k++] = list[j++];
		}
	}
	while (i < mid) buff[k++] = list[i++];
	while (j < right) buff[k++] = list[j++];
	mem::copy(&list[left], &buff[left], (right - left) * $sizeof(Type.inner));
}
