module std::sort;
import std::sort::is;

<*
Sort list using the quick sort algorithm.
 @require @is_sortable(list) : "The list must be indexable and support .len or .len()"
 @require @is_valid_cmp_fn(cmp, list, context) : "Expected a comparison function which compares values"
*>
macro insertionsort(list, cmp = EMPTY_MACRO_SLOT, context = EMPTY_MACRO_SLOT) @builtin @safemacro
{
	$if @typekind(list) == POINTER &&& (@typekind(*list) == ARRAY || @typekind(*list) == VECTOR):
		$typeof((*list)[0])[] list2 = list;
		is::isort{$typeof(list2), $typeof(cmp), $typeof(context)}(list2, 0, list.len, cmp, context);
	$else
		usz len = sort::len_from_list(list);
		is::isort{$typeof(list), $typeof(cmp), $typeof(context)}(list, 0, (isz)len, cmp, context);
	$endif
}

module std::sort::is{Type, CmpFn, Context};

alias ElementType = $typeof(((Type){})[0]);

fn void isort(Type list, usz low, usz high, CmpFn comp, Context context)
{
	var $has_cmp = @is_valid_macro_slot(comp);
	var $has_context = @is_valid_macro_slot(context);
	var $cmp_by_value = $has_cmp &&& $assignable(list[0], $typefrom(CmpFn.paramsof[0].type));
	var $has_get_ref =  $defined(&list[0]);
	for (usz i = low; i < high; ++i)
	{
		usz j = i;
		for (;j > low;)
		{
			$if $has_get_ref:
				ElementType *rhs = &list[j];
				ElementType *lhs = &list[--j];
				$switch:
					$case $cmp_by_value && $has_context:
						if (comp(*rhs, *lhs, context) >= 0) break;
					$case $cmp_by_value:
						if (comp(*rhs, *lhs) >= 0) break;
					$case $has_cmp && $has_context:
						if (comp(rhs, lhs, context) >= 0) break;
					$case $has_cmp:
						if (comp(rhs, lhs) >= 0) break;
					$default:
						if (!less(*rhs, *lhs)) break;
				$endswitch
				@swap(*rhs, *lhs);
			$else
				usz r = j;
				--j;
				$switch:
					$case $cmp_by_value && $has_context:
						if (comp(list[r], list[j], context) >= 0) break;
					$case $cmp_by_value:
						if (comp(list[r], list[j]) >= 0) break;
					$case $has_cmp && $has_context:
						if (comp(&list[r], &list[j], context) >= 0) break;
					$case $has_cmp:
						if (comp(&list[r], &list[j]) >= 0) break;
					$default:
						if (!less(list[r], list[j])) break;
				$endswitch
				@swap(list[r], list[j]);
			$endif
		}
	}
}
