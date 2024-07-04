module std::sort;
import std::sort::is;

macro insertionsort(list, cmp = null) @builtin
{
	var $Type = $typeof(list);
	var $CmpType = $typeof(cmp);
	usz len = sort::@len_from_list(list);
	is::isort(<$Type, $CmpType>)(list, 0, (isz)len, cmp);
}

module std::sort::is(<Type, Comparer>);

def ElementType = $typeof(Type{}[0]);

fn void isort(Type list, usz low, usz high, Comparer comp) {
	var $no_cmp = Comparer.typeid == void*.typeid;
	var $cmp_by_value = $and(!$no_cmp, Comparer.params[0] == @typeid(list[0]));

	for (usz i = low; i < high; ++i) {
		usz j = i;
		for (;j > low;) {
			ElementType *rhs = list.get_ref(j);
			ElementType *lhs = list.get_ref(--j);

			$switch
				$case $cmp_by_value:
					if (!comp(*rhs, *lhs)) {
						break;
					}
				$case !$no_cmp:
					if (!comp(rhs, lhs)) {
						break;
					}
				$default:
					if (!less(*rhs, *lhs)) {
						break;
					}
			$endswitch

			@swap(*rhs, *lhs);
		}
	}
}
