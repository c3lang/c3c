module std::sort;
import std::sort::is;

<*
 Sort list using the quick sort algorithm.

 @require @list_is_by_ref(list) : "Expected a list passed by reference, or slice passed by value"
 @require @is_sortable(list) : "The list must be indexable and support .len or .len()"
 @require @is_valid_cmp_fn(#cmp: ...cmp, #list: list, #context: ...context) : "Expected a comparison function which compares values"
*>
macro void insertionsort(list, cmp = ..., context = ...) @builtin @safemacro
{
	// When the context or cmp functions are not defined, we can simply use a dummy/default type.
	var $CmpFnType = $defined(cmp) ??? $typeof(cmp) : uint;
	var $ContextType = $defined(context) ??? $typeof(context) : uint;
	$if $kindof(list) == SLICE:
		is::isort{$typeof(list), $CmpFnType, $ContextType}(list, 0, lengthof(list), ...cmp, ...context);
	$else
		is::isort{$typeof(*list), $CmpFnType, $ContextType}(list, 0, lengthof(*list), ...cmp, ...context);
	$endif
}

module std::sort::is <Type, CmpFn, Context>;

alias ElementType = $typeof(((Type){})[0]);
const bool IS_SLICE = Type.kindof == SLICE;
alias ListType = $typefrom(IS_SLICE ??? Type : Type*);
macro ElementType list_get(ListType l, i) => IS_SLICE ??? l[i] : (*l)[i];
macro ElementType* list_get_ref(ListType l, i) => IS_SLICE ??? &l[i] : &(*l)[i];

macro void isort(ListType list, usz low, usz high, CmpFn comp = ..., Context context = ...)
{
	var $has_cmp = $defined(comp);
	var $has_context = $defined(context);
	var $cmp_by_value = $has_cmp &&& $defined($typefrom(CmpFn.paramsof[0].type) p = list_get(list, 0));
	var $has_get_ref = IS_SLICE ||| $defined(&(*list)[0]);
	for (usz i = low; i < high; ++i)
	{
		usz j = i;
		for (;j > low;)
		{
			$if $has_get_ref:
				ElementType *rhs = list_get_ref(list, j);
				ElementType *lhs = list_get_ref(list, --j);
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
						if (comp(list_get(list, r), list_get(j), context) >= 0) break;
					$case $cmp_by_value:
						if (comp(list_get(list, r), list_get(j)) >= 0) break;
					$case $has_cmp && $has_context:
						if (comp(list_get_ref(list, r), list_get_ref(j), context) >= 0) break;
					$case $has_cmp:
						if (comp(list_get_ref(list, r), list_get_ref(j)) >= 0) break;
					$default:
						if (!less(list_get(list, r), list_get(j))) break;
				$endswitch
				$if IS_SLICE:
					@swap(list[r], list[j]);
				$else
					@swap((*list)[r], (*list)[j]);
				$endif
			$endif
		}
	}
}
