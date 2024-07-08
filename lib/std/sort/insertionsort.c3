module std::sort;
import std::sort::is;

/**
 * Sort list using the quick sort algorithm.
 * @require @is_sortable(list) "The list must be indexable and support .len or .len()"
 * @require $or(@typeid(cmp) == void*.typeid, @is_cmp_fn(cmp, list)) "Expected a comparison function which compares values"
 **/
macro insertionsort(list, cmp = null) @builtin
{
	usz len = sort::@len_from_list(list);
	is::isort(<$typeof(list), $typeof(cmp)>)(list, 0, (isz)len, cmp);
}

module std::sort::is(<Type, Comparer>);

def ElementType = $typeof(Type{}[0]);

fn void isort(Type list, usz low, usz high, Comparer comp)
{
	var $no_cmp = Comparer.typeid == void*.typeid;
	var $cmp_by_value = $and(!$no_cmp, Comparer.params[0] == @typeid(list[0]));
	var $has_get_ref =  $defined(&list[0]);
	assert(sort::@is_sortable(list));
	for (usz i = low; i < high; ++i)
	{
		usz j = i;
		for (;j > low;)
		{
			$if $has_get_ref:
				ElementType *rhs = &list[j];
				ElementType *lhs = &list[--j];
				$switch
					$case $cmp_by_value:
						if (comp(*rhs, *lhs) >= 0) break;
					$case !$no_cmp:
						if (comp(rhs, lhs) >= 0) break;
					$default:
						if (!less(*rhs, *lhs)) break;
				$endswitch
				@swap(*rhs, *lhs);
			$else
				usz r = j;
				--j;

				$switch
					$case $cmp_by_value:
						if (comp(list[r], list[j]) >= 0) break;
					$case !$no_cmp:
						if (comp(&list[r], &list[j]) >= 0) break;
					$default:
						if (!less(list[r], list[j])) break;
				$endswitch
				@swap(list[r], list[j]);
			$endif
		}
	}
}
