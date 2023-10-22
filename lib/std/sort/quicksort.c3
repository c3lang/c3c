module std::sort;
import std::sort::qs;

/**
 * Sort list using the quick sort algorithm.
 * @require $defined(list[0]) && $defined(list.len) "The list must be indexable and support .len or .len()"
 * @require $or($typeof(cmp).typeid == void*.typeid, @is_comparer(cmp, list)) "Expected a comparison function which compares values"
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

struct StackElementItem @private
{
	isz low;
	isz high;
}

def Stack = StackElementItem[64] @private;

// Based on https://alienryderflex.com/quicksort by Darel Rex Finley, Public Domain.

fn void qsort(Type list, isz low, isz high, Comparer cmp)
{
	var $no_cmp = Comparer.typeid == void*.typeid;
	var $cmp_by_value = $and(!$no_cmp, Comparer.params[0] == $typeof(list[0]).typeid);
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
				ElementType pivot = list[l];
				while (l < h)
				{
					$switch
						$case $cmp_by_value:
							while (cmp(list[h], pivot) >= 0 && l < h) h--;
						$case !$no_cmp:
							while (cmp(&list[h], &pivot) >= 0 && l < h) h--;
						$default:
							while (greater_eq(list[h], pivot) && l < h) h--;
					$endswitch
					if (l < h) list[l++] = list[h];
					$switch
						$case $cmp_by_value:
							while (cmp(list[l], pivot) <= 0 && l < h) l++;
						$case !$no_cmp:
							while (cmp(&list[l], &pivot) <= 0 && l < h) l++;
						$default:
							while (less_eq(list[l], pivot) && l < h) l++;
					$endswitch
					if (l < h) list[h--] = list[l];
				}
				list[l] = pivot;
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
