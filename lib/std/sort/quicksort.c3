module std::sort;
import std::sort::qs;

/**
 * Sort list using the quick sort algorithm.
 * @require is_searchable(list) "The list must be indexable and support .len or .len()"
 * @require !cmp || is_comparer(cmp, list) "Expected a comparison function which compares values"
 **/
macro quicksort(list, cmp = null) @builtin
{
	var $Type = $typeof(list);
	var $CmpType = $typeof(cmp);
	usz len = sort::@len_from_list(list);
	qs::qsort(<$Type, $CmpType>)(list, 0, (isz)len - 1, cmp);
}

module std::sort::qs(<Type, Comparer>);

def ElementType = $typeof(Type{}[0]);

fn void qsort(Type list, isz low, isz high, Comparer cmp)
{
	if (low >= 0 && high >= 0 && low < high)
	{
		isz p = partition(list, low, high, cmp);
		qsort(list, low, p, cmp);
		qsort(list, p + 1, high, cmp);
	}
}

fn isz partition(Type list, isz low, isz high, Comparer cmp) @inline @local
{
	ElementType pivot = list[low + (high - low) / 2];
	isz i = low - 1;
	isz j = high + 1;
	bool ok;
	while (true)
	{
		do {
			i++;
			$switch
				$case $checks(cmp(list[0], list[0])):
					ok = cmp(list[i], pivot) < 0;
				$case $checks(cmp(&list[0], &list[0])):
					ok = cmp(&list[i], &pivot) < 0;
				$default:
					ok = less(list[i], pivot);
			$endswitch
		} while (ok);
		do {
			j--;
			$switch
				$case $checks(cmp(list[0], list[0])):
					ok = cmp(list[j], pivot) > 0;
				$case $checks(cmp(&list[0], &list[0])):
					ok = cmp(&list[j], &pivot) > 0;
				$default:
					ok = greater(list[j], pivot);
			$endswitch
		} while (ok);
		if (i >= j) return j;
		$if $checks(list.swap(0, 0)):
			list.swap(i, j);
		$else
			@swap(list[i], list[j]);
		$endif
	}
}